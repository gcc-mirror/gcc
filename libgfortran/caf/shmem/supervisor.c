/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "config.h"

#include "../caf_error.h"
#include "supervisor.h"
#include "teams_mgmt.h"
#include "thread_support.h"

#include <assert.h>
#include <unistd.h>
#include <string.h>
#ifdef HAVE_WAIT_H
#include <wait.h>
#elif HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#define GFORTRAN_ENV_NUM_IMAGES "GFORTRAN_NUM_IMAGES"
#define GFORTRAN_ENV_SHARED_MEMORY_SIZE "GFORTRAN_SHARED_MEMORY_SIZE"
#define GFORTRAN_ENV_IMAGE_NUM "GFORTRAN_IMAGE_NUM"

image_local *local = NULL;

image this_image = {-1, NULL};

/* Get image number from environment or sysconf.  */

static int
get_image_num_from_envvar (void)
{
  char *num_images_char;
  int nimages;
  num_images_char = getenv (GFORTRAN_ENV_NUM_IMAGES);
  if (!num_images_char)
    return sysconf (_SC_NPROCESSORS_ONLN); /* TODO: Make portable.  */
  /* TODO: Error checking.  */
  nimages = atoi (num_images_char);
  return nimages;
}

/* Get the amount of memory for the shared memory block.  This is picked from
   an environment variable.  If that is not there, pick a reasonable default.
   Note that on a 64-bit system which allows overcommit, there is no penalty in
   reserving a large space and then not using it.  */

static size_t
get_memory_size_from_envvar (void)
{
  char *e;
  size_t sz = 0;
  e = getenv (GFORTRAN_ENV_SHARED_MEMORY_SIZE);
  if (e)
    {
      char suffix[2];
      int rv;
      rv = sscanf (e, "%zu%1s", &sz, suffix);
      if (rv == 2)
	{
	  switch (suffix[0])
	    {
	    case 'k':
	    case 'K':
	      sz *= ((size_t) 1) << 10;
	      break;
	    case 'm':
	    case 'M':
	      sz *= ((size_t) 1) << 20;
	      break;
	    case 'g':
	    case 'G':
	      sz *= ((size_t) 1) << 30;
	      break;
	    default:
	      sz = 0;
	    }
	}
    }
  if (sz == 0)
    {
      /* Use 256 MB for 32-bit systems and 4 GB for 64-bit systems.  */
      if (sizeof (size_t) == 4)
	sz = ((size_t) 1) << 28;
      else
	sz = ((size_t) 1) << 34;
    }
  return sz;
}

/* Get a supervisor.  */

static supervisor *
get_supervisor (void)
{
  supervisor *sv;
  sv = SHMPTR_AS (supervisor *,
		  shared_memory_get_master (&local->sm,
					    sizeof (supervisor)
					      + sizeof (image_tracker)
						  * local->total_num_images,
					    __alignof__ (supervisor)),
		  &local->sm);
  sv->failed_images = 0;
  sv->finished_images = 0;
  return sv;
}

/* Defined in shmem.c, but we need it here.  */

extern memid next_memid;

#define SUPERVISOR_MAGIC_NUM 0x12345678

/* Ensure things are initialized.  */

void
ensure_shmem_initialization (void)
{
  size_t shmem_size;
  char *image_num;

  if (local)
    return;

  local = malloc (sizeof (image_local));
  pagesize = sysconf (_SC_PAGE_SIZE);
  shmem_size = round_to_pagesize (get_memory_size_from_envvar ());
  local->total_num_images = get_image_num_from_envvar ();
  shared_memory_init (&local->sm, shmem_size);
  shared_memory_prepare (&local->sm);

  /* Shared memory needs to be present, before master can be initialized/linked
     to.  */
  image_num = getenv (GFORTRAN_ENV_IMAGE_NUM);
  if (image_num)
    {
      bool created;
      this_image = (image) {atoi (image_num), get_supervisor ()};
      assert (this_image.supervisor->magic_number == SUPERVISOR_MAGIC_NUM);

      alloc_init (&local->ai, &local->sm);

      caf_initial_team = caf_current_team
	= (caf_shmem_team_t) calloc (1, sizeof (struct caf_shmem_team));
      allocator_lock (&local->ai.alloc);
      *caf_initial_team = (struct caf_shmem_team) {
	NULL,
	-1,
	this_image.image_num,
	0,
	NULL,
	{alloc_get_memory_by_id_created (&local->ai,
					 local->total_num_images * sizeof (int)
					   + sizeof (struct shmem_image_info),
					 next_memid++, &created)}};
      if (created)
	{
	  counter_barrier_init (&caf_initial_team->u.image_info->image_count,
				local->total_num_images);
	  collsub_init_supervisor (&caf_initial_team->u.image_info->collsub,
				   alloc_get_allocator (&local->ai),
				   local->total_num_images);
	  caf_initial_team->u.image_info->team_parent_id = 0;
	  caf_initial_team->u.image_info->team_id = -1;
	  caf_initial_team->u.image_info->image_map_size
	    = local->total_num_images;
	  caf_initial_team->u.image_info->num_term_images = 0;
	  caf_initial_team->u.image_info->lastmemid = 0;
	  for (int i = 0; i < local->total_num_images; ++i)
	    caf_initial_team->u.image_info->image_map[i] = i;
	}
      allocator_unlock (&local->ai.alloc);
      sync_init (&local->si, &local->sm);
    }
  else
    {
      this_image = (image) {-1, get_supervisor ()};
      this_image.supervisor->magic_number = SUPERVISOR_MAGIC_NUM;
      counter_barrier_init (&this_image.supervisor->num_active_images,
			    local->total_num_images);
      alloc_init_supervisor (&local->ai, &local->sm);
      sync_init_supervisor (&local->si, &local->ai);
    }
}

extern char **environ;

int
supervisor_main_loop (int *argc __attribute__ ((unused)), char ***argv,
		      int *exit_code)
{
  supervisor *m;
  pid_t new_pid, finished_pid;
  image im;
  int chstatus;

  *exit_code = 0;
  shared_memory_set_env (getpid ());
  m = this_image.supervisor;

  for (im.image_num = 0; im.image_num < local->total_num_images; im.image_num++)
    {
      if ((new_pid = fork ()))
	{
	  if (new_pid == -1)
	    caf_runtime_error ("error spawning child\n");
	  m->images[im.image_num] = (image_tracker) {new_pid, IMAGE_OK};
	}
      else
	{
	  static char **new_env;
	  static char num_image[32];
	  size_t n = 2; /* Add one env-var and one for the term NULL.  */

	  /* Count the number of entries in the current environment.  */
	  for (char **e = environ; *e; ++e, ++n)
	    ;
	  new_env = (char **) malloc (sizeof (char *) * n);
	  memcpy (new_env, environ, sizeof (char *) * (n - 2));
	  snprintf (num_image, 32, "%s=%d", GFORTRAN_ENV_IMAGE_NUM,
		    im.image_num);
	  new_env[n - 2] = num_image;
	  new_env[n - 1] = NULL;
	  execve ((*argv)[0], *argv, new_env);
	  return 1;
	}
    }
  for (int j, i = 0; i < local->total_num_images; i++)
    {
      finished_pid = wait (&chstatus);
      if (WIFEXITED (chstatus) && !WEXITSTATUS (chstatus))
	{
	  for (j = 0;
	       j < local->total_num_images && m->images[j].pid != finished_pid;
	       j++)
	    ;
	  /* Only set the status, when it has not been set by the (failing)
	     image already.  */
	  if (m->images[j].status == IMAGE_OK)
	    {
	      m->images[j].status = IMAGE_SUCCESS;
	      atomic_fetch_add (&m->finished_images, 1);
	    }
	}
      else if (!WIFEXITED (chstatus) || WEXITSTATUS (chstatus))
	{
	  for (j = 0;
	       j < local->total_num_images && m->images[j].pid != finished_pid;
	       j++)
	    ;
	  dprintf (2, "ERROR: Image %d(pid: %d) failed with %d.\n", j + 1,
		   finished_pid, WTERMSIG (chstatus));
	  if (j == local->total_num_images)
	    {
	      if (finished_pid == getpid ())
		{
		  dprintf (2,
			   "WARNING: Supervisor process got signal %d. Killing "
			   "childs and exiting.\n",
			   WTERMSIG (chstatus));
		  for (j = 0; j < local->total_num_images; j++)
		    {
		      if (m->images[j].status == IMAGE_OK)
			kill (m->images[j].pid, SIGKILL);
		    }
		  exit (1);
		}
	      dprintf (2,
		       "WARNING: Got signal %d for unknown process %d. "
		       "Ignoring and trying to continue.\n",
		       WTERMSIG (chstatus), finished_pid);
	      continue;
	    }
	  m->images[j].status = IMAGE_FAILED;
	  atomic_fetch_add (&m->failed_images, 1);
	  if (*exit_code < WTERMSIG (chstatus))
	    *exit_code = WTERMSIG (chstatus);
	  else if (*exit_code == 0)
	    *exit_code = 1;
	}
      /* Trigger waiting sync images aka sync_table.  */
      for (j = 0; j < local->total_num_images; j++)
	pthread_cond_signal (&SHMPTR_AS (pthread_cond_t *,
					 m->sync_shared.sync_images_cond_vars,
					 &local->sm)[j]);
      counter_barrier_add (&m->num_active_images, -1);
    }
  return 0;
}
