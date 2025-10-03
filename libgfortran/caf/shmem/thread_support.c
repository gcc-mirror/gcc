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

#if !defined(WIN32) && !defined(__CYGWIN__)
#include <pthread.h>

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
initialize_shared_mutex (caf_shmem_mutex *mutex)
{
  pthread_mutexattr_t mattr;
  ERRCHECK (pthread_mutexattr_init (&mattr));
  ERRCHECK (pthread_mutexattr_setpshared (&mattr, PTHREAD_PROCESS_SHARED));
#ifdef PTHREAD_MUTEX_ROBUST
  ERRCHECK (pthread_mutexattr_setrobust (&mattr, PTHREAD_MUTEX_ROBUST));
#endif
  ERRCHECK (pthread_mutex_init (mutex, &mattr));
  ERRCHECK (pthread_mutexattr_destroy (&mattr));
}

void
initialize_shared_errorcheck_mutex (caf_shmem_mutex *mutex)
{
  pthread_mutexattr_t mattr;
  ERRCHECK (pthread_mutexattr_init (&mattr));
  ERRCHECK (pthread_mutexattr_settype (&mattr, PTHREAD_MUTEX_ERRORCHECK));
  ERRCHECK (pthread_mutexattr_setpshared (&mattr, PTHREAD_PROCESS_SHARED));
#ifdef PTHREAD_MUTEX_ROBUST
  ERRCHECK (pthread_mutexattr_setrobust (&mattr, PTHREAD_MUTEX_ROBUST));
#endif
  ERRCHECK (pthread_mutex_init (mutex, &mattr));
  ERRCHECK (pthread_mutexattr_destroy (&mattr));
}

void
initialize_shared_condition (caf_shmem_condvar *cond, const int)
{
  pthread_condattr_t cattr;
  ERRCHECK (pthread_condattr_init (&cattr));
  ERRCHECK (pthread_condattr_setpshared (&cattr, PTHREAD_PROCESS_SHARED));
  ERRCHECK (pthread_cond_init (cond, &cattr));
  ERRCHECK (pthread_condattr_destroy (&cattr));
}
#else
#include "../caf_error.h"
#include "supervisor.h"
#include "teams_mgmt.h"
#include <windows.h>
#include <assert.h>

static HANDLE *handles = NULL;
static size_t cap_handles = 0;

static const int ULONGBITS = sizeof (unsigned long) << 3; // *8

static size_t
smax (size_t a, size_t b)
{
  return a < b ? b : a;
}

static HANDLE
get_handle (const size_t id, const char t)
{
  const int add = t == 'c' ? 1 : 0;
  while (id + add >= cap_handles)
    {
      cap_handles += 1024;
      if (handles)
	handles = realloc (handles, sizeof (HANDLE) * cap_handles);
      else
	handles = malloc (sizeof (HANDLE) * cap_handles);
      if (!handles)
	caf_runtime_error (
	  "can not get buffer for synchronication objects, aborting");

      memset (&handles[cap_handles - 1024], 0, sizeof (HANDLE) * 1024);
    }
  if (!handles[id])
    {
      static char *pid = NULL;
      char name[MAX_PATH];

      if (!pid)
	pid = shared_memory_get_env ();
      snprintf (name, MAX_PATH, "Global_gfortran-%s-%c-%zd", pid, t, id);
      switch (t)
	{
	case 'm':
	  handles[id] = CreateMutex (NULL, false, name);
	  break;
	case 'c':
	  {
	    handles[id] = CreateSemaphore (NULL, 0, __INT_MAX__, name);
	    snprintf (name, MAX_PATH, "Global_gfortran-%s-%c-%zd_lock", pid, t,
		      id);
	    handles[id + 1] = CreateSemaphore (NULL, 1, 1, name);
	    this_image.supervisor->global_used_handles
	      = smax (this_image.supervisor->global_used_handles, id + 2);
	    break;
	  }
	default:
	  caf_runtime_error ("Unknown handle type %c", t);
	  exit (1);
	}
      if (handles[id] == NULL)
	{
	  caf_runtime_error (
	    "Could not create synchronisation object, error: %d",
	    GetLastError ());
	  return NULL;
	}

      this_image.supervisor->global_used_handles
	= smax (this_image.supervisor->global_used_handles, id + 1);
    }

  return handles[id];
}

static HANDLE
get_mutex (caf_shmem_mutex *m)
{
  return get_handle (m->id, 'm');
}

static HANDLE
get_condvar (caf_shmem_condvar *cv)
{
  return get_handle (cv->id, 'c');
}

void
thread_support_init_supervisor (void)
{
  if (local->total_num_images > ULONGBITS * MAX_NUM_SIGNALED)
    caf_runtime_error ("Maximum number of supported images is %zd.",
		       ULONGBITS * MAX_NUM_SIGNALED);
  this_image.supervisor->global_used_handles = 0;
}

int
caf_shmem_mutex_lock (caf_shmem_mutex *m)
{
  HANDLE mutex = get_mutex (m);
  DWORD res = WaitForSingleObject (mutex, INFINITE);

  /* Return zero on success.  */
  return res != WAIT_OBJECT_0;
}

int
caf_shmem_mutex_trylock (caf_shmem_mutex *m)
{
  HANDLE mutex = get_mutex (m);
  DWORD res = WaitForSingleObject (mutex, 0);

  return res == WAIT_OBJECT_0 ? 0 : EBUSY;
}

int
caf_shmem_mutex_unlock (caf_shmem_mutex *m)
{
  HANDLE mutex = get_mutex (m);
  BOOL res = ReleaseMutex (mutex);

  if (!res)
    {
      LPVOID lpMsgBuf;
      DWORD dw = GetLastError ();

      if (FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER
			   | FORMAT_MESSAGE_FROM_SYSTEM
			   | FORMAT_MESSAGE_IGNORE_INSERTS,
			 NULL, dw, MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
			 (LPTSTR) &lpMsgBuf, 0, NULL)
	  == 0)
	{
	  fprintf (stderr, "%d: formatting the error message failed.\n",
		   this_image.image_num);
	  ExitProcess (dw);
	}

      fprintf (stderr, "%d: unlock mutex failed: %d, %s\n",
	       this_image.image_num, dw, (LPCTSTR) lpMsgBuf);

      LocalFree (lpMsgBuf);
    }
  return res ? 0 : EPERM;
}

static bool
bm_is_set (volatile unsigned long mask[], const int b)
{
  return (mask[b / ULONGBITS] & (1UL << (b % ULONGBITS))) != 0;
}

static void
bm_clear_bit (volatile unsigned long mask[], const int b)
{
  mask[b / ULONGBITS] &= ~(1UL << (b % ULONGBITS));
}

static void
bm_set_mask (volatile unsigned long mask[], const int size)
{
  const int entries = size / ULONGBITS;
  const int rem = size % ULONGBITS;
  int i = 0;
  assert (entries >= 0);

  for (; i < entries; ++i)
    mask[i] = ~0UL;
  if (rem != 0)
    mask[i] = ~0UL >> (ULONGBITS - rem);
}

__attribute__ ((used)) static bool
bm_is_none (volatile unsigned long mask[], const int size)
{
  const int entries = size / ULONGBITS;
  const int rem = size % ULONGBITS;
  int i = 0;
  for (; i < entries; ++i)
    if (mask[i] != 0)
      return false;

  return rem == 0 || ((mask[i] & (~0UL >> (ULONGBITS - rem))) == 0);
}

void
caf_shmem_cond_wait (caf_shmem_condvar *cv, caf_shmem_mutex *m)
{
  HANDLE mutex = get_mutex (m), condvar = get_condvar (cv),
	 lock = get_handle (cv->id + 1, 'c');
  HANDLE entry[3] = {mutex, condvar, lock};
  int res;

  WaitForSingleObject (lock, INFINITE);
  for (;;)
    {
      if (bm_is_set (cv->signaled, this_image.image_num) || cv->any)
	{
	  break;
	}
      ReleaseMutex (mutex);
      ReleaseSemaphore (lock, 1, NULL);
      res = WaitForMultipleObjects (3, entry, true, INFINITE);
      if (res != WAIT_OBJECT_0)
	{
	  fprintf (stderr, "%d: failed to get all wait for: %d\n",
		   this_image.image_num, res);
	  fflush (stderr);
	}
      ReleaseSemaphore (condvar, 1, NULL);
    }
  res = WaitForSingleObject (condvar, INFINITE);
  if (res != WAIT_OBJECT_0)
    {
      fprintf (stderr, "%d: failed to get condvar: %d\n", this_image.image_num,
	       res);
      fflush (stderr);
    }

  bm_clear_bit (cv->signaled, this_image.image_num);
  cv->any = 0;
  ReleaseSemaphore (lock, 1, NULL);
}

void
caf_shmem_cond_broadcast (caf_shmem_condvar *cv)
{
  HANDLE condvar = get_condvar (cv), lock = get_handle (cv->id + 1, 'c');

  WaitForSingleObject (lock, INFINITE);
  bm_set_mask (cv->signaled, cv->size);
  bm_clear_bit (cv->signaled, this_image.image_num);

  ReleaseSemaphore (condvar, cv->size, NULL);
  ReleaseSemaphore (lock, 1, NULL);
}

void
caf_shmem_cond_signal (caf_shmem_condvar *cv)
{
  HANDLE condvar = get_condvar (cv), lock = get_handle (cv->id + 1, 'c');

  if (caf_current_team)
    {
      WaitForSingleObject (lock, INFINITE);
    }
  else
    return;
  /* The first image is zero, which wouldn't allow it to signal.  */
  cv->any = this_image.image_num + 1;
  ReleaseSemaphore (condvar, 1, NULL);
  ReleaseSemaphore (lock, 1, NULL);
}

void
caf_shmem_cond_update_count (caf_shmem_condvar *cv, int val)
{
  cv->size += val;
}

void
initialize_shared_mutex (caf_shmem_mutex *m)
{
  *m = (caf_shmem_mutex) {this_image.supervisor->global_used_handles};

  get_mutex (m);
}

void
initialize_shared_errorcheck_mutex (caf_shmem_mutex *m)
{
  *m = (caf_shmem_mutex) {this_image.supervisor->global_used_handles};

  get_mutex (m);
}

void
initialize_shared_condition (caf_shmem_condvar *cv, const int size)
{
  *cv = (caf_shmem_condvar) {this_image.supervisor->global_used_handles,
			     0,
			     size,
			     {}};

  memset ((void *) cv->signaled, 0, sizeof (unsigned long) * MAX_NUM_SIGNALED);
  get_condvar (cv);
  assert (bm_is_none (cv->signaled, cv->size));
}

void
thread_support_cleanup (void)
{
  for (size_t i = 0; i < this_image.supervisor->global_used_handles; ++i)
    if (handles[i])
      CloseHandle (handles[i]);
}
#endif
