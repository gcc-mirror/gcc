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

#include "thread_support.h"

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#define ERRCHECK(a)                                                            \
  do                                                                           \
    {                                                                          \
      int rc = a;                                                              \
      if (rc)                                                                  \
	{                                                                      \
	  errno = rc;                                                          \
	  perror (#a " failed");                                               \
	  exit (1);                                                            \
	}                                                                      \
    }                                                                          \
  while (0)

void
initialize_shared_mutex (pthread_mutex_t *mutex)
{
  pthread_mutexattr_t mattr;
  ERRCHECK (pthread_mutexattr_init (&mattr));
  ERRCHECK (pthread_mutexattr_setpshared (&mattr, PTHREAD_PROCESS_SHARED));
  ERRCHECK (pthread_mutex_init (mutex, &mattr));
  ERRCHECK (pthread_mutexattr_destroy (&mattr));
}

void
initialize_shared_errorcheck_mutex (pthread_mutex_t *mutex)
{
  pthread_mutexattr_t mattr;
  ERRCHECK (pthread_mutexattr_init (&mattr));
  ERRCHECK (pthread_mutexattr_setpshared (&mattr, PTHREAD_PROCESS_SHARED));
  ERRCHECK (pthread_mutexattr_settype (&mattr, PTHREAD_MUTEX_ERRORCHECK));
  ERRCHECK (pthread_mutex_init (mutex, &mattr));
  ERRCHECK (pthread_mutexattr_destroy (&mattr));
}

void
initialize_shared_condition (pthread_cond_t *cond)
{
  pthread_condattr_t cattr;
  ERRCHECK (pthread_condattr_init (&cattr));
  ERRCHECK (pthread_condattr_setpshared (&cattr, PTHREAD_PROCESS_SHARED));
  ERRCHECK (pthread_cond_init (cond, &cattr));
  ERRCHECK (pthread_condattr_destroy (&cattr));
}
