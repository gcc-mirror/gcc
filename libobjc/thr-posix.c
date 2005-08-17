/* GNU Objective C Runtime Thread Interface for POSIX compliant threads
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)
   Modified for Linux/Pthreads by Kai-Uwe Sattler (kus@iti.cs.uni-magdeburg.de)
   Modified for posix compliance by Chris Ball (cball@fmco.com)

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

#include "objc/thr.h"
#include "objc/runtime.h"
#include <pthread.h>

/* Key structure for maintaining thread specific storage */
static pthread_key_t _objc_thread_storage;
static pthread_attr_t _objc_thread_attribs;

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
  /* Initialize the thread storage key */
  if (pthread_key_create(&_objc_thread_storage, NULL) == 0)
    {
      /*
       * The normal default detach state for threads is PTHREAD_CREATE_JOINABLE
       * which causes threads to not die when you think they should.
	   */
      if (pthread_attr_init(&_objc_thread_attribs) == 0)
        {
          if (pthread_attr_setdetachstate(&_objc_thread_attribs, 
                                          PTHREAD_CREATE_DETACHED) == 0)
            return 0;
        }
    }

  return -1;
}

/* Close the threads subsystem. */
int
__objc_close_thread_system(void)
{
  if (pthread_key_delete(_objc_thread_storage) == 0)
    {
      if (pthread_attr_destroy(&_objc_thread_attribs) == 0)
        return 0;
    }

  return -1;
}

/* Backend thread functions */

/* Create a new thread of execution. */
objc_thread_t
__objc_thread_detach(void (*func)(void *arg), void *arg)
{
  objc_thread_t thread_id;
  pthread_t new_thread_handle;
  
  if (!(pthread_create(&new_thread_handle, &_objc_thread_attribs, 
                       (void *)func, arg)))
    thread_id = *(objc_thread_t *)&new_thread_handle;
  else
    thread_id = NULL;
  
  return thread_id;
}

/* Set the current thread's priority.
 *
 * Be aware that the default schedpolicy often disallows thread priorities.
 */
int
__objc_thread_set_priority(int priority)
{
  pthread_t thread_id = pthread_self();
  int policy;
  struct sched_param params;
  int priority_min, priority_max;

  if (pthread_getschedparam(thread_id, &policy, &params) == 0)
    {
      if ((priority_max = sched_get_priority_max(policy)) != 0)
        return -1;

      if ((priority_min = sched_get_priority_min(policy)) != 0)
        return -1;

      if (priority > priority_max)
        priority = priority_max;
      else if (priority < priority_min)
        priority = priority_min;
      params.sched_priority = priority;

      /*
       * The solaris 7 and several other man pages incorrectly state that
       * this should be a pointer to policy but pthread.h is universally
       * at odds with this.
       */
      if (pthread_setschedparam(thread_id, policy, &params) == 0)
        return 0;
    }
  return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  int policy;
  struct sched_param params;

  if (pthread_getschedparam(pthread_self(), &policy, &params) == 0)
    return params.sched_priority;
  else
    return -1;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  sched_yield();
}

/* Terminate the current thread. */
int
__objc_thread_exit(void)
{
  /* exit the thread */
  pthread_exit(&__objc_thread_exit_status);

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id(void)
{
  pthread_t self = pthread_self();

  return *(objc_thread_t *)&self;
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  if (pthread_setspecific(_objc_thread_storage, value) == 0)
    return 0;
  else
    return -1;
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  return pthread_getspecific(_objc_thread_storage);
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  mutex->backend = objc_malloc(sizeof(pthread_mutex_t));

  if (pthread_mutex_init((pthread_mutex_t *)mutex->backend, NULL))
    {
      objc_free(mutex->backend);
      mutex->backend = NULL;
      return -1;
    }

  return 0;
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  int count = 1;

  /*
   * Posix Threads specifically require that the thread be unlocked for
   * pthread_mutex_destroy to work.
   */

  while (count)
    {
      if ((count = pthread_mutex_unlock((pthread_mutex_t*)mutex->backend)) < 0)
        return -1;
    }

  if (pthread_mutex_destroy((pthread_mutex_t *)mutex->backend))
    return -1;

  objc_free(mutex->backend);
  mutex->backend = NULL;
  return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  if (pthread_mutex_lock((pthread_mutex_t *)mutex->backend) == 0)
    return 0;
  else
    return -1;
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  if (pthread_mutex_trylock((pthread_mutex_t *)mutex->backend) == 0)
    return 0;
  else
    return -1;
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  if (pthread_mutex_unlock((pthread_mutex_t *)mutex->backend) == 0)
    return 0;
  else
    return -1;
}

/* Backend condition mutex functions */

/* Allocate a condition. */
int
__objc_condition_allocate(objc_condition_t condition)
{
  condition->backend = objc_malloc(sizeof(pthread_cond_t));

  if (pthread_cond_init((pthread_cond_t *)condition->backend, NULL))
    {
      objc_free(condition->backend);
      condition->backend = NULL;
      return -1;
    }

  return 0;
}

/* Deallocate a condition. */
int
__objc_condition_deallocate(objc_condition_t condition)
{
  if (pthread_cond_destroy((pthread_cond_t *)condition->backend))
    return -1;

  objc_free(condition->backend);
  condition->backend = NULL;
  return 0;
}

/* Wait on the condition */
int
__objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  if (pthread_cond_wait((pthread_cond_t *)condition->backend,
                        (pthread_mutex_t *)mutex->backend) == 0)
    return 0;
  else
    return -1;
}

/* Wake up all threads waiting on this condition. */
int
__objc_condition_broadcast(objc_condition_t condition)
{
  if (pthread_cond_broadcast((pthread_cond_t *)condition->backend) == 0)
    return 0;
  else
    return -1;
}

/* Wake up one thread waiting on this condition. */
int
__objc_condition_signal(objc_condition_t condition)
{
  if (pthread_cond_signal((pthread_cond_t *)condition->backend) == 0)
    return 0;
  else
    return -1;
}
