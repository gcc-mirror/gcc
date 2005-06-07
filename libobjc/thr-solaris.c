/* GNU Objective C Runtime Thread Interface
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)
   Conditions added by Mircea Oancea (mircea@first.elcom.pub.ro)
      
This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
GCC; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include "objc/thr.h"
#include "objc/runtime.h"

#include <thread.h>
#include <synch.h>
#include <errno.h>

/* Key structure for maintaining thread specific storage */
static thread_key_t     __objc_thread_data_key;

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
  /* Initialize the thread storage key */
  if (thr_keycreate(&__objc_thread_data_key, NULL) == 0)
    return 0;
  else
    return -1;
}

/* Close the threads subsystem. */
int
__objc_close_thread_system(void)
{
  return 0;
}

/* Backend thread functions */

/* Create a new thread of execution. */
objc_thread_t
__objc_thread_detach(void (*func)(void *arg), void *arg)
{
  objc_thread_t thread_id;
  thread_t new_thread_id = 0;

  if (thr_create(NULL, 0, (void *)func, arg,
                 THR_DETACHED | THR_NEW_LWP,
                 &new_thread_id) == 0)
    thread_id = *(objc_thread_t *)&new_thread_id;
  else
    thread_id = NULL;
  
  return thread_id;
}

/* Set the current thread's priority. */
int
__objc_thread_set_priority(int priority)
{
  int sys_priority = 0;

  switch (priority)
    {
    case OBJC_THREAD_INTERACTIVE_PRIORITY:
      sys_priority = 300;
      break;
    default:
    case OBJC_THREAD_BACKGROUND_PRIORITY:
      sys_priority = 200;
      break;
    case OBJC_THREAD_LOW_PRIORITY:
      sys_priority = 1000;
      break;
    }

  /* Change priority */
  if (thr_setprio(thr_self(), sys_priority) == 0)
    return 0;
  else
    return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  int sys_priority;
                                                   
  if (thr_getprio(thr_self(), &sys_priority) == 0)
    {
      if (sys_priority >= 250)
	return OBJC_THREAD_INTERACTIVE_PRIORITY;
      else if (sys_priority >= 150)
	return OBJC_THREAD_BACKGROUND_PRIORITY;
      return OBJC_THREAD_LOW_PRIORITY;
    }

  /* Couldn't get priority. */
  return -1;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  thr_yield();
}

/* Terminate the current thread. */
int
__objc_thread_exit(void)
{
  /* exit the thread */
  thr_exit(&__objc_thread_exit_status);

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id(void)
{
  return (objc_thread_t)thr_self();
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  if (thr_setspecific(__objc_thread_data_key, value) == 0)
    return 0;
  else
    return -1;
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  void *value = NULL;

  if (thr_getspecific(__objc_thread_data_key, &value) == 0)
    return value;

  return NULL;
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  if (mutex_init( (mutex_t *)(&(mutex->backend)), USYNC_THREAD, 0))
    return -1;
  else
    return 0;
}


/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  mutex_destroy((mutex_t *)(&(mutex->backend)));
  return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  if (mutex_lock((mutex_t *)(&(mutex->backend))) != 0)
    return -1;
  else
    return 0;
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  if (mutex_trylock((mutex_t *)(&(mutex->backend))) != 0)
    return -1;
  else
    return 0;
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  if (mutex_unlock((mutex_t *)(&(mutex->backend))) != 0)
    return -1;
  else
    return 0;
}

/* Backend condition mutex functions */

/* Allocate a condition. */
int
__objc_condition_allocate(objc_condition_t condition)
{
  return cond_init((cond_t *)(&(condition->backend)), USYNC_THREAD, NULL);
}

/* Deallocate a condition. */
int
__objc_condition_deallocate(objc_condition_t condition)
{
  return cond_destroy((cond_t *)(&(condition->backend)));
}

/* Wait on the condition */
int
__objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  return cond_wait((cond_t *)(&(condition->backend)),
		   (mutex_t *)(&(mutex->backend)));
}

/* Wake up all threads waiting on this condition. */
int
__objc_condition_broadcast(objc_condition_t condition)
{
  return cond_broadcast((cond_t *)(&(condition->backend)));
}

/* Wake up one thread waiting on this condition. */
int
__objc_condition_signal(objc_condition_t condition)
{
  return cond_signal((cond_t *)(&(condition->backend)));
}

/* End of File */
