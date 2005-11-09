/* GNU Objective C Runtime Thread Interface.
   Copyright (C) 1999 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#define _LIBOBJC
/* The line below is needed for declarations of functions such as
   pthread_mutexattr_settype, without which gthr-posix.h may fail to
   compile within libobjc.  */
#define _XOPEN_SOURCE 500
#include "config.h"
#include "tconfig.h"
#include "coretypes.h"
#include "tm.h"
#include "defaults.h"
#include "objc/thr.h"
#include "objc/runtime.h"
#include <gthr.h>

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
  return __gthread_objc_init_thread_system ();
}

/* Close the threads subsystem. */
int
__objc_close_thread_system(void)
{
  return __gthread_objc_close_thread_system ();
}

/* Backend thread functions */

/* Create a new thread of execution. */
objc_thread_t
__objc_thread_detach(void (*func)(void *), void *arg)
{
  return __gthread_objc_thread_detach (func, arg);
}

/* Set the current thread's priority. */
int
__objc_thread_set_priority(int priority)
{
  return __gthread_objc_thread_set_priority (priority);
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  return __gthread_objc_thread_get_priority ();
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  __gthread_objc_thread_yield ();
}

/* Terminate the current thread. */
int
__objc_thread_exit(void)
{
  return __gthread_objc_thread_exit ();
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id(void)
{
  return __gthread_objc_thread_id ();
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  return __gthread_objc_thread_set_data (value);
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  return __gthread_objc_thread_get_data ();
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  return __gthread_objc_mutex_allocate (mutex);
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  return __gthread_objc_mutex_deallocate (mutex);
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  return __gthread_objc_mutex_lock (mutex);
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  return __gthread_objc_mutex_trylock (mutex);
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  return __gthread_objc_mutex_unlock (mutex);
}

/* Backend condition mutex functions */

/* Allocate a condition. */
int
__objc_condition_allocate(objc_condition_t condition)
{
  return __gthread_objc_condition_allocate (condition);
}

/* Deallocate a condition. */
int
__objc_condition_deallocate(objc_condition_t condition)
{
  return __gthread_objc_condition_deallocate (condition);
}

/* Wait on the condition */
int
__objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  return __gthread_objc_condition_wait (condition, mutex);
}

/* Wake up all threads waiting on this condition. */
int
__objc_condition_broadcast(objc_condition_t condition)
{
  return __gthread_objc_condition_broadcast (condition);
}

/* Wake up one thread waiting on this condition. */
int
__objc_condition_signal(objc_condition_t condition)
{
  return __gthread_objc_condition_signal (condition);
}

/* End of File */
