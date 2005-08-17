/* GNU Objective C Runtime Thread Implementation
   Copyright (C) 1996, 1997, 2002 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)
   Modified for Mach threads by Bill Bumgarner <bbum@friday.com>
   Condition functions added by Mircea Oancea <mircea@first.elcom.pub.ro>

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

#include <mach/mach.h>
#include <mach/cthreads.h>
#include "objc/thr.h"
#include "objc/runtime.h"

/*
  Obtain the maximum thread priority that can set for t.  Under the
  mach threading model, it is possible for the developer to adjust the
  maximum priority downward only-- cannot be raised without superuser
  privileges.  Once lowered, it cannot be raised.
  */
static int
__mach_get_max_thread_priority (cthread_t t, int *base)
{
  thread_t threadP;
  kern_return_t error;
  struct thread_sched_info info;
  unsigned int info_count=THREAD_SCHED_INFO_COUNT;
    
  if (t == NULL)
    return -1;

  threadP  = cthread_thread (t); 	/* get thread underlying */

  error = thread_info (threadP, THREAD_SCHED_INFO, 
		       (thread_info_t) &info, &info_count);

  if (error != KERN_SUCCESS)
    return -1;

  if (base != NULL)
    *base = info.base_priority;

  return info.max_priority;
}
	
/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system (void)
{
  return 0;
}

/* Close the threads subsystem. */
int
__objc_close_thread_system (void)
{
  return 0;
}

/* Backend thread functions */

/* Create a new thread of execution. */
objc_thread_t
__objc_thread_detach (void (*func) (void *arg), void *arg)
{
  objc_thread_t thread_id;
  cthread_t new_thread_handle;

  /* create thread */
  new_thread_handle = cthread_fork ((cthread_fn_t) func, arg);

  if (new_thread_handle)
    {
      /* this is not terribly portable */
      thread_id = *(objc_thread_t *) &new_thread_handle; 
      cthread_detach (new_thread_handle);
    }
  else
    thread_id = NULL;
  
  return thread_id;
}

/* Set the current thread's priority. */
int
__objc_thread_set_priority (int priority)
{
  objc_thread_t *t = objc_thread_id ();
  cthread_t cT = (cthread_t) t; 
  int maxPriority = __mach_get_max_thread_priority (cT, NULL);
  int sys_priority = 0;

  if (maxPriority == -1)
    return -1;

  switch (priority)
    {
    case OBJC_THREAD_INTERACTIVE_PRIORITY:
      sys_priority = maxPriority;
      break;
    case OBJC_THREAD_BACKGROUND_PRIORITY:
      sys_priority = (maxPriority * 2) / 3;
      break;
    case OBJC_THREAD_LOW_PRIORITY:
      sys_priority = maxPriority / 3;
      break;
    default:
      return -1;
    }

  if (sys_priority == 0)
    return -1;

  /* Change the priority */
  if (cthread_priority (cT, sys_priority, 0) == KERN_SUCCESS)
    return 0;
  else
    return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority (void)
{
  objc_thread_t *t = objc_thread_id ();
  cthread_t cT = (cthread_t) t; /* see objc_thread_id () */
  int basePriority;
  int maxPriority;
  int sys_priority = 0;

  int interactiveT, backgroundT, lowT; /* thresholds */

  maxPriority = __mach_get_max_thread_priority (cT, &basePriority);

  if (maxPriority == -1)
    return -1;

  if (basePriority > ( (maxPriority * 2) / 3))
    return OBJC_THREAD_INTERACTIVE_PRIORITY;

  if (basePriority > ( maxPriority / 3))
    return OBJC_THREAD_BACKGROUND_PRIORITY;

  return OBJC_THREAD_LOW_PRIORITY;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield (void)
{
  cthread_yield ();
}

/* Terminate the current thread. */
int
__objc_thread_exit (void)
{
  /* exit the thread */
  cthread_exit (&__objc_thread_exit_status);

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id (void)
{
  cthread_t self = cthread_self ();

  return *(objc_thread_t *) &self;
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data (void *value)
{
  cthread_set_data (cthread_self (), (any_t) value);
  return 0;
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data (void)
{
  return (void *) cthread_data (cthread_self ());
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate (objc_mutex_t mutex)
{
  int err = 0;
  mutex->backend = objc_malloc (sizeof (struct mutex));

  err = mutex_init ((mutex_t) (mutex->backend));

  if (err != 0)
    {
      objc_free (mutex->backend);
      return -1;
    }
  else
    return 0;
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate (objc_mutex_t mutex)
{
  mutex_clear ((mutex_t) (mutex->backend));

  objc_free (mutex->backend);
  mutex->backend = NULL;
  return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock (objc_mutex_t mutex)
{
  mutex_lock ((mutex_t) (mutex->backend));
  return 0;
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock (objc_mutex_t mutex)
{
  if (mutex_try_lock ((mutex_t) (mutex->backend)) == 0)
    return -1;
  else
    return 0;
}

/* Unlock the mutex */
int
__objc_mutex_unlock (objc_mutex_t mutex)
{
  mutex_unlock ((mutex_t) (mutex->backend));
  return 0;
}

/* Backend condition mutex functions */

/* Allocate a condition. */
int
__objc_condition_allocate (objc_condition_t condition)
{
  condition->backend = objc_malloc (sizeof (struct condition));
  condition_init ((condition_t) (condition->backend));
  return 0;
}

/* Deallocate a condition. */
int
__objc_condition_deallocate (objc_condition_t condition)
{
  condition_clear ((condition_t) (condition->backend));
  objc_free (condition->backend);
  condition->backend = NULL;
  return 0;
}

/* Wait on the condition */
int
__objc_condition_wait (objc_condition_t condition, objc_mutex_t mutex)
{
  condition_wait ((condition_t) (condition->backend),
		  (mutex_t) (mutex->backend));
  return 0;
}

/* Wake up all threads waiting on this condition. */
int
__objc_condition_broadcast (objc_condition_t condition)
{
  condition_broadcast ((condition_t) (condition->backend));
  return 0;
}

/* Wake up one thread waiting on this condition. */
int
__objc_condition_signal (objc_condition_t condition)
{
  condition_signal ((condition_t) (condition->backend));
  return 0;
}

/* End of File */
