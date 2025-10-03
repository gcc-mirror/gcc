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

#include "../caf_error.h"
#include "supervisor.h"
#include "teams_mgmt.h"
#include "thread_support.h"

#include <assert.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#ifdef HAVE_WAIT_H
#include <wait.h>
#elif HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#if !defined(_SC_PAGE_SIZE) && defined(WIN32)
#include <windows.h>
#endif

#define GFORTRAN_ENV_NUM_IMAGES "GFORTRAN_NUM_IMAGES"
#define GFORTRAN_ENV_SHARED_MEMORY_SIZE "GFORTRAN_SHARED_MEMORY_SIZE"
#define GFORTRAN_ENV_IMAGE_NUM "GFORTRAN_IMAGE_NUM"
#define GFORTRAN_ENV_IMAGE_RESTARTS_LIMITS "GFORTRAN_IMAGE_RESTARTS_LIMIT"

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
#ifdef _SC_NPROCESSORS_ONLN
    return sysconf (_SC_NPROCESSORS_ONLN);
#elif defined(WIN32)
    num_images_char = getenv ("NUMBER_OF_PROCESSORS");
#else
#error "Unsupported system: No known way to get number of cores!"
#endif
  nimages = atoi (num_images_char);
  return nimages;
}

/* Get the number of restarts allowed when the shared memory could not be placed
at the same location in each image.  This is mostly important for MacOS, because
this OS acts somewhat arbitrary/indeterministic.  */

static unsigned
get_image_restarts_limit (void)
{
  char *limit_chars;
  unsigned limit = 4000;
  limit_chars = getenv (GFORTRAN_ENV_IMAGE_RESTARTS_LIMITS);
  if (limit_chars)
    limit = atoi (limit_chars);
  return limit;
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
#ifndef WIN32
	sz = ((size_t) 1) << 34;
#else
	/* Use 1GB on Windows.  */
	sz = ((size_t) 1) << 30;
#endif
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
  if (!local)
    {
      caf_runtime_error ("can not initialize memory for local cache");
      exit (1);
    }
#if defined(_SC_PAGE_SIZE)
  pagesize = sysconf (_SC_PAGE_SIZE);
#elif defined(WIN32)
  {
    SYSTEM_INFO si;
    GetNativeSystemInfo (&si);
    pagesize = si.dwAllocationGranularity;
  }
#else
#warning                                                                       \
  "Unsupported system: No known way to get memory page size. Assuming 4k!"
  pagesize = 4096;
#endif
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
      thread_support_init_supervisor ();
      counter_barrier_init (&this_image.supervisor->num_active_images,
			    local->total_num_images);
      alloc_init_supervisor (&local->ai, &local->sm);
      sync_init_supervisor (&local->si, &local->ai);
    }
}

#if !defined(environ)
extern char **environ;
#endif

static bool
startWorker (image *im __attribute__ ((unused)),
	     char ***argv __attribute__ ((unused)))
{
#ifdef HAVE_FORK
  caf_shmem_pid new_pid;
  if ((new_pid = fork ()))
    {
      im->supervisor->images[im->image_num]
	= (image_tracker) {new_pid, IMAGE_OK};
      return false;
    }
  else
    {
      if (new_pid == -1)
	caf_runtime_error ("error spawning child\n");
      static char **new_env;
      static char num_image[32];
      size_t n = 2; /* Add one env-var and one for the term NULL.  */

      /* Count the number of entries in the current environment.  */
      for (char **e = environ; *e; ++e, ++n)
	;
      new_env = (char **) malloc (sizeof (char *) * n);
      memcpy (new_env, environ, sizeof (char *) * (n - 2));
      snprintf (num_image, 32, "%s=%d", GFORTRAN_ENV_IMAGE_NUM, im->image_num);
      new_env[n - 2] = num_image;
      new_env[n - 1] = NULL;
      if (execve ((*argv)[0], *argv, new_env) == -1)
	{
	  perror ("execve failed");
	}
      exit (255);
    }
#endif
  return true;
}

#ifndef WIN32
static void
kill_all_images (supervisor *m)
{
  for (int j = 0; j < local->total_num_images; j++)
    if (m->images[j].status == IMAGE_OK)
      kill (m->images[j].pid, SIGKILL);
}
#endif

/* argc and argv may not be used on certain OSes.  Flag them unused therefore.
 */
int
supervisor_main_loop (int *argc __attribute__ ((unused)),
		      char ***argv __attribute__ ((unused)), int *exit_code)
{
  supervisor *m;
  image im;
#if defined(WIN32) && !defined(HAVE_FORK)
  HANDLE *process_handles = malloc (sizeof (HANDLE) * local->total_num_images),
	 *thread_handles = malloc (sizeof (HANDLE) * local->total_num_images),
	 *waiting_handles = malloc (sizeof (HANDLE) * local->total_num_images);
  int count_waiting = local->total_num_images;
  LPTCH *envs = malloc (sizeof (LPTCH) * local->total_num_images);
  LPTSTR currentDir;
  DWORD cdLen = GetCurrentDirectory (0, NULL);
  currentDir = malloc (cdLen);
  GetCurrentDirectory (cdLen, currentDir);
#else
  int chstatus;
  unsigned restarts = 0, restarts_limit;
  restarts_limit = get_image_restarts_limit ();
#endif

  *exit_code = 0;
  shared_memory_set_env (getpid ());
  im.supervisor = m = this_image.supervisor;

  for (im.image_num = 0; im.image_num < local->total_num_images; im.image_num++)
    {
#ifdef HAVE_FORK
      if (startWorker (&im, argv))
	return 1;
#elif defined(WIN32)
      LPTCH new_env;
      size_t n = 0, es;
      STARTUPINFO si;
      DWORD dwFlags = 0;
      PROCESS_INFORMATION pi;
      LPTCH env = GetEnvironmentStrings ();

      ZeroMemory (&si, sizeof (si));
      si.cb = sizeof (si);
      ZeroMemory (&pi, sizeof (pi));

      /* Count the number of characters in the current environment.  */
      for (LPTSTR e = (LPTSTR) env; *e; es = lstrlen (e) + 1, e += es, n += es)
	;
      new_env = (LPCH) malloc (n + 32 * sizeof (TCHAR));
      memcpy (new_env, env, n);
      snprintf (&((TCHAR *) new_env)[n], 32, "%s=%d%c", GFORTRAN_ENV_IMAGE_NUM,
		im.image_num, (char) 0);
      if (!CreateProcessA (NULL, GetCommandLine (), NULL, NULL, FALSE, dwFlags,
			   new_env, currentDir, &si, &pi))
	{
	  LPVOID lpMsgBuf;
	  DWORD dw = GetLastError ();

	  if (FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER
			       | FORMAT_MESSAGE_FROM_SYSTEM
			       | FORMAT_MESSAGE_IGNORE_INSERTS,
			     NULL, dw,
			     MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
			     (LPTSTR) &lpMsgBuf, 0, NULL)
	      == 0)
	    {
	      fprintf (stderr, "formatting the error message failed.\n");
	      ExitProcess (dw);
	    }

	  fprintf (stderr, "error spawning child: %ld, %s\n", dw,
		   (LPCTSTR) lpMsgBuf);

	  LocalFree (lpMsgBuf);
	  exit (1);
	}
      m->images[im.image_num] = (image_tracker) {pi.hProcess, IMAGE_OK};
      process_handles[im.image_num] = waiting_handles[im.image_num]
	= pi.hProcess;
      thread_handles[im.image_num] = pi.hThread;
      envs[im.image_num] = new_env;
#else
#error "no way known to start child processes."
#endif
    }
  for (int i = 0; i < local->total_num_images; i++)
    {
#ifdef HAVE_FORK
      caf_shmem_pid finished_pid = wait (&chstatus);
      int j;

      if (finished_pid == -1)
	{
	  /* Skip wait having an issue.  */
	  perror ("wait failed");
	  --i;
	  continue;
	}
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
	  if (WEXITSTATUS (chstatus) == 210)
	    {
	      --i;
	      im.image_num = j;
	      ++restarts;
	      if (restarts > restarts_limit)
		{
		  kill_all_images (m);
		  caf_runtime_error (
		    "After restarting images %d times, no common state on "
		    "shared memory could be reached. Giving up...",
		    restarts);
		  exit (1);
		}
	      if (startWorker (&im, argv))
		return 1;
	      continue;
	    }
	  else
	    {
	      dprintf (2,
		       "ERROR: Image %d(pid: %d) failed with signal %d, "
		       "exitstatus %d.\n",
		       j + 1, finished_pid, WTERMSIG (chstatus),
		       WEXITSTATUS (chstatus));
	      if (j == local->total_num_images)
		{
		  if (finished_pid == getpid ())
		    {
		      dprintf (
			2,
			"WARNING: Supervisor process got signal %d. Killing "
			"childs and exiting.\n",
			WTERMSIG (chstatus));
		      kill_all_images (m);
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
	}
      /* Trigger waiting sync images aka sync_table.  */
      for (j = 0; j < local->total_num_images; j++)
	caf_shmem_cond_signal (&SHMPTR_AS (caf_shmem_condvar *,
					   m->sync_shared.sync_images_cond_vars,
					   &local->sm)[j]);
      counter_barrier_add (&m->num_active_images, -1);
#elif defined(WIN32)
      DWORD res = WaitForMultipleObjects (count_waiting, waiting_handles, FALSE,
					  INFINITE);
      HANDLE cand;
      bool progress = false;
      DWORD process_exit_code;
      if (res == WAIT_FAILED)
	caf_runtime_error ("waiting for process termination failed.");
      int index = res - WAIT_OBJECT_0, finished_process;
      bool fail;

      do
	{
	  cand = waiting_handles[index];
	  for (finished_process = 0;
	       finished_process < local->total_num_images
	       && cand != process_handles[finished_process];
	       ++finished_process)
	    ;

	  GetExitCodeProcess (cand, &process_exit_code);
	  fail = process_exit_code != 0;
	  fprintf (stderr, "terminating process %d with fail status %d (%ld)\n",
		   finished_process, fail, process_exit_code);
	  if (finished_process < local->total_num_images)
	    {
	      CloseHandle (process_handles[finished_process]);
	      process_handles[finished_process] = NULL;
	      CloseHandle (thread_handles[finished_process]);
	      FreeEnvironmentStrings (envs[finished_process]);
	      if (fail)
		{
		  m->images[finished_process].status = IMAGE_FAILED;
		  atomic_fetch_add (&m->failed_images, 1);
		  if (*exit_code < process_exit_code)
		    *exit_code = process_exit_code;
		}
	      else
		{
		  m->images[finished_process].status = IMAGE_SUCCESS;
		  atomic_fetch_add (&m->finished_images, 1);
		}
	    }
	  memmove (&waiting_handles[index], &waiting_handles[index + 1],
		   sizeof (HANDLE) * (count_waiting - index - 1));
	  --count_waiting;
	  counter_barrier_add (&m->num_active_images, -1);

	  /* Check if more than one process has terminated already.  */
	  progress = false;
	  for (index = 0; index < count_waiting; ++index)
	    if (WaitForSingleObject (waiting_handles[index], 0)
		== WAIT_OBJECT_0)
	      {
		progress = true;
		++i;
		break;
	      }
	}
      while (progress && count_waiting > 0);
#endif
    }

#if defined(WIN32) && !defined(HAVE_FORK)
  free (process_handles);
  free (thread_handles);
  free (envs);
#endif
  return 0;
}
