/* GNU Objective C Runtime Thread Implementation
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
GNU CC; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include <objc/thr.h>
#include "runtime.h"

/* Thread local storage for a single thread */
static void *thread_local_storage = NULL;

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
  /* No thread support available */
  return -1;
}

/* Close the threads subsystem. */
int
__objc_close_thread_system(void)
{
  /* No thread support available */
  return -1;
}

/* Backend thread functions */

/* Create a new thread of execution. */
objc_thread_t
__objc_thread_detach(void (*func)(void *arg), void *arg)
{
  /* No thread support available */
  return NULL;
}

/* Set the current thread's priority. */
int
__objc_thread_set_priority(int priority)
{
  /* No thread support available */
  return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  return OBJC_THREAD_INTERACTIVE_PRIORITY;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  return;
}

/* Terminate the current thread. */
int
__objc_thread_exit(void)
{
  /* No thread support available */
  /* Should we really exit the program */
  /* exit(&__objc_thread_exit_status); */
  return -1;
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id(void)
{
  /* No thread support, use 1. */
  return (objc_thread_t)1;
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  thread_local_storage = value;
  return 0;
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  return thread_local_storage;
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  return 0;
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  /* There can only be one thread, so we always get the lock */
  return 0;
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  /* There can only be one thread, so we always get the lock */
  return 0;
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  return 0;
}

/* Backend condition mutex functions */

/* Allocate a condition. */
int
__objc_condition_allocate(objc_condition_t condition)
{
  return 0;
}

/* Deallocate a condition. */
int
__objc_condition_deallocate(objc_condition_t condition)
{
  return 0;
}

/* Wait on the condition */
int
__objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  return 0;
}

/* Wake up all threads waiting on this condition. */
int
__objc_condition_broadcast(objc_condition_t condition)
{
  return 0;
}

/* Wake up one thread waiting on this condition. */
int
__objc_condition_signal(objc_condition_t condition)
{
  return 0;
}

/* End of File */
