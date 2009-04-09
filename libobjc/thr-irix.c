/* GNU Objective C Runtime Thread Interface - SGI IRIX Implementation
   Copyright (C) 1996, 1997, 2009 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#include <stdlib.h>
#include <sys/types.h>
#include <sys/sysmp.h>
#include <sys/prctl.h>
#include <ulocks.h>
#include "objc/thr.h"
#include "objc/runtime.h"

/* Key structure for maintaining thread specific storage */
static void * __objc_shared_arena_handle = NULL;

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
  /* Name of IRIX arena. */
  char arena_name[64];

  DEBUG_PRINTF("__objc_init_thread_system\n");

  /* Construct a temporary name for arena. */
  sprintf(arena_name, "/usr/tmp/objc_%05u", (unsigned)getpid());

  /* Up to 256 threads.  Arena only for threads. */
  usconfig(CONF_INITUSERS, 256);
  usconfig(CONF_ARENATYPE, US_SHAREDONLY);

  /* Initialize the arena */
  if (!(__objc_shared_arena_handle = usinit(arena_name)))
    /* Failed */
    return -1;

  return 0;
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
  int sys_id;

  if ((sys_id = sproc((void *)func, PR_SALL, arg)) >= 0)
    thread_id = (objc_thread_t)sys_id;
  else
    thread_id = NULL;
  
  return thread_id;
}

/* Set the current thread's priority. */
int
__objc_thread_set_priority(int priority)
{
  /* Not implemented yet */
  return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  /* Not implemented yet */
  return OBJC_THREAD_INTERACTIVE_PRIORITY;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  sginap(0);
}

/* Terminate the current thread. */
int
__objc_thread_exit(void)
{
  /* IRIX only has exit. */
  exit(__objc_thread_exit_status);

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id(void)
{
  /* Threads are processes. */
  return (objc_thread_t)get_pid();
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  *((void **)&PRDA->usr_prda) = value;
  return 0;
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  return *((void **)&PRDA->usr_prda);
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  if (!( (ulock_t)(mutex->backend) = usnewlock(__objc_shared_arena_handle) ))
    return -1;
  else
    return 0;
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  usfreelock((ulock_t)(mutex->backend), __objc_shared_arena_handle);
  return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  if (ussetlock((ulock_t)(mutex->backend)) == 0)
    return -1;
  else
    return 0;
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  if (ustestlock((ulock_t)(mutex->backend)) == 0)
    return -1;
  else
    return 0;
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  usunsetlock((ulock_t)(mutex->backend));
  return 0;
}

/* Backend condition mutex functions */

/* Allocate a condition. */
int
__objc_condition_allocate(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;
}

/* Deallocate a condition. */
int
__objc_condition_deallocate(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;
}

/* Wait on the condition */
int
__objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  /* Unimplemented. */
  return -1;
}

/* Wake up all threads waiting on this condition. */
int
__objc_condition_broadcast(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;
}

/* Wake up one thread waiting on this condition. */
int
__objc_condition_signal(objc_condition_t condition)
{
  /* Unimplemented. */
  return -1;
}

/* End of File */
