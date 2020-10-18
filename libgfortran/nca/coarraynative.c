/* Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

This file is part of the GNU Fortran Native Coarray Library (libnca).

Libnca is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libnca is distributed in the hope that it will be useful,
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


#include "libgfortran.h"
#include "libcoarraynative.h"
#include "allocator.h"
#include "hashmap.h"
#include "util.h"
#include "lock.h"
#include "collective_subroutine.h"

#include <unistd.h>
#include <sys/mman.h>
// #include <stdlib.h>
#include <sys/wait.h>

#define GFORTRAN_ENV_NUM_IMAGES "GFORTRAN_NUM_IMAGES"

nca_local_data *local = NULL;

image this_image;

static int
get_environ_image_num (void)
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

void 
ensure_initialization(void)
{
  if (local)
    return;

  local = malloc(sizeof(nca_local_data)); // Is malloc already init'ed at that
  					// point? Maybe use mmap(MAP_ANON) 
					// instead
  pagesize = sysconf (_SC_PAGE_SIZE); 
  local->num_images = get_environ_image_num ();
  shared_memory_init (&local->sm);
  shared_memory_prepare (&local->sm);
  alloc_iface_init (&local->ai, &local->sm);
  collsub_iface_init (&local->ci, &local->ai, &local->sm);
  sync_iface_init (&local->si, &local->ai, &local->sm);
}

static void   __attribute__((noreturn))
image_main_wrapper (void (*image_main) (void), image *this)
{
  this_image = *this;

  sync_all(&local->si);

  image_main ();

  exit (0);
}

static master *
get_master (void) {
  master *m;
  m = SHMPTR_AS (master *,
        shared_memory_get_mem_with_alignment
		 (&local->sm,
		  sizeof (master) + sizeof(image_status) * local->num_images,
		  __alignof__(master)), &local->sm);
  m->has_failed_image = 0;
  return m;
}

/* This is called from main, with a pointer to the user's program as
   argument.  It forks the images and waits for their completion.  */

void
cas_master (void (*image_main) (void)) {
  master *m;
  int i, j;
  pid_t new;
  image im;
  int exit_code = 0;
  int chstatus;
  ensure_initialization();  
  m = get_master();

  im.m = m;

  for (im.image_num = 0; im.image_num < local->num_images; im.image_num++)
    {
      if ((new = fork()))
        {
	  if (new == -1)
	    {
	      dprintf(2, "error spawning child\n");
	      exit_code = 1;
	    }
	  m->images[im.image_num].pid = new;
	  m->images[im.image_num].status = IMAGE_OK;
        }
      else
        image_main_wrapper(image_main, &im);
    }
  for (i = 0; i < local->num_images; i++)
    {
      new = wait (&chstatus);
      if (WIFEXITED (chstatus) && !WEXITSTATUS (chstatus))
	{
	  j = 0;
	  for (; j < local->num_images && m->images[j].pid != new; j++);
	  m->images[j].status = IMAGE_SUCCESS;
	  m->finished_images++; /* FIXME: Needs to be atomic, probably.  */
	}
      else if (!WIFEXITED (chstatus) || WEXITSTATUS (chstatus))
	{
	  j = 0;
	  for (; j < local->num_images && m->images[j].pid != new; j++);
	  m->images[j].status = IMAGE_FAILED;
	  m->has_failed_image++; /* FIXME: Needs to be atomic, probably.  */
	  dprintf (2, "ERROR: Image %d(%#x) failed\n", j, new);
	  exit_code = 1;
	}
    }
  exit (exit_code);
}
