/* GNU Objective C Runtime Thread Interface
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)

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

#include <pthread.h>
#include <thr.h>
#include "runtime.h"

/* Key structure for maintaining thread specific storage */
static pthread_key_t _objc_thread_storage;

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
  /* Initialize the thread storage key */
  return pthread_keycreate(&_objc_thread_storage, NULL);
}

/* Close the threads subsystem. */
int
__objc_close_thread_system(void)
{
  /* Destroy the thread storage key */
  /* Not implemented yet */
  /* return pthread_key_delete(&_objc_thread_storage); */
  return 0;
}

/* Backend thread functions */

/* Create a new thread of execution. */
objc_thread_t
__objc_thread_detach(void (*func)(void *arg), void *arg)
{
  objc_thread_t thread_id;
  pthread_t new_thread_handle;

  if (pthread_create(&new_thread_handle, pthread_attr_default,
		     (void *)func, arg) == 0)
    {
      /* ??? May not work! (64bit) */
      thread_id = *(objc_thread_t *)&new_thread_handle; 
      pthread_detach(&new_thread_handle);     /* Fully detach thread.     */
    }
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
      sys_priority = (PRI_FG_MIN_NP + PRI_FG_MAX_NP) / 2;
      break;
    default:
    case OBJC_THREAD_BACKGROUND_PRIORITY:
      sys_priority = (PRI_BG_MIN_NP + PRI_BG_MAX_NP) / 2;
      break;
    case OBJC_THREAD_LOW_PRIORITY:
      sys_priority = (PRI_BG_MIN_NP + PRI_BG_MAX_NP) / 2;
      break;
    }
    
  /* Change the priority. */
  if (pthread_setprio(pthread_self(), sys_priority) >= 0)
    return 0;
  else
    /* Failed */
    return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  int sys_priority;
    
  if ((sys_priority = pthread_getprio(pthread_self())) >= 0) {
    if (sys_priority >= PRI_FG_MIN_NP && sys_priority <= PRI_FG_MAX_NP)
      return OBJC_THREAD_INTERACTIVE_PRIORITY;
    if (sys_priority >= PRI_BG_MIN_NP && sys_priority <= PRI_BG_MAX_NP)
      return OBJC_THREAD_BACKGROUND_PRIORITY;
    return OBJC_THREAD_LOW_PRIORITY;
  }

  /* Failed */
  return -1;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  pthread_yield();
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

  return (objc_thread_t) pthread_getunique_np (&self);
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  return pthread_setspecific(_objc_thread_storage, value);
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  void *value = NULL;

  if ( !(pthread_getspecific(_objc_thread_storage, &value)) )
    return value;

  return NULL;
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  if (pthread_mutex_init((pthread_mutex_t *)(&(mutex->backend)), 
			 pthread_mutexattr_default))
    return -1;
  else
    return 0;
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  if (pthread_mutex_destroy((pthread_mutex_t *)(&(mutex->backend))))
    return -1;
  else
    return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  return pthread_mutex_lock((pthread_mutex_t *)(&(mutex->backend)));
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  if (pthread_mutex_trylock((pthread_mutex_t *)(&(mutex->backend))) != 1)
    return -1;
  else
    return 0;
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  return pthread_mutex_unlock((pthread_mutex_t *)(&(mutex->backend)));
}

/* Backend condition mutex functions */

/* Allocate a condition. */
int
__objc_condition_allocate(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;

  /*
  if (pthread_cond_init((pthread_cond_t *)(&(condition->backend)), NULL))
    return -1;
  else
    return 0;
    */
}

/* Deallocate a condition. */
int
__objc_condition_deallocate(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;

  /*
  return pthread_cond_destroy((pthread_cond_t *)(&(condition->backend)));
  */
}

/* Wait on the condition */
int
__objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  /* Unimplemented. */
  return -1;

  /*
  return pthread_cond_wait((pthread_cond_t *)(&(condition->backend)),
			   (pthread_mutex_t *)(&(mutex->backend)));
			   */
}

/* Wake up all threads waiting on this condition. */
int
__objc_condition_broadcast(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;

  /*
  return pthread_cond_broadcast((pthread_cond_t *)(&(condition->backend)));
  */
}

/* Wake up one thread waiting on this condition. */
int
__objc_condition_signal(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;

  /*
  return pthread_cond_signal((pthread_cond_t *)(&(condition->backend)));
  */
}

/* End of File */
