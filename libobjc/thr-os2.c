/* GNU Objective C Runtime Thread Interface - OS/2 emx Implementation
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Thomas Baier (baier@ci.tuwien.ac.at)

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

#define INCL_DOSSEMAPHORES
#define INCL_DOSPROCESS

/*
 * conflicts with objc.h:       SEL, BOOL, id
 * solution:  prefixing those with _OS2_ before including <os2.h>
 */
#define SEL _OS2_SEL
#define BOOL _OS2_BOOL
#define id _OS2_id
#include <os2.h>
#undef id
#undef SEL
#undef BOOL

#include <stdlib.h>

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
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
  int thread_id = 0;

  if ((thread_id = _beginthread (func,NULL,32768,arg)) < 0)
    thread_id = 0;
  
  return (objc_thread_t)thread_id;
}

/* Set the current thread's priority. */
int
__objc_thread_set_priority(int priority)
{
  ULONG sys_class = 0;
  ULONG sys_priority = 0;

  /* OBJC_THREAD_INTERACTIVE_PRIORITY -> PRTYC_FOREGROUNDSERVER
   * OBJC_THREAD_BACKGROUND_PRIORITY  -> PRTYC_REGULAR
   * OBJC_THREAD_LOW_PRIORITY         -> PRTYC_IDLETIME */
  
  switch (priority) {
  case OBJC_THREAD_INTERACTIVE_PRIORITY:
    sys_class = PRTYC_REGULAR;
    sys_priority = 10;
    break;
  default:
  case OBJC_THREAD_BACKGROUND_PRIORITY:
    sys_class = PRTYC_IDLETIME;
    sys_priority = 25;
    break;
  case OBJC_THREAD_LOW_PRIORITY:
    sys_class = PRTYC_IDLETIME;
    sys_priority = 0;
    break;
  }

  /* Change priority */
  if (!DosSetPriority (PRTYS_THREAD,sys_class,sys_priority,*_threadid))
    return 0;
  else
    return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  PTIB ptib;
  PPIB ppib;

  /* get information about current thread */
  DosGetInfoBlocks (&ptib,&ppib);

  switch (ptib->tib_ptib2->tib2_ulpri)
    {
    case PRTYC_IDLETIME:
    case PRTYC_REGULAR:
    case PRTYC_TIMECRITICAL:
    case PRTYC_FOREGROUNDSERVER:
    default:
      return OBJC_THREAD_INTERACTIVE_PRIORITY;
    }

  return -1;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  DosSleep (0);
}

/* Terminate the current thread. */
int
__objc_thread_exit(void)
{
  /* terminate the thread, NEVER use DosExit () */
  _endthread ();

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id(void)
{
  return (objc_thread_t) *_threadid;
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  *_threadstore () = value;

  return 0;
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  return *_threadstore ();
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  if (DosCreateMutexSem (NULL, (HMTX)(&(mutex->backend)),0L,0) > 0)
    return -1;
  else
    return 0;
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  DosCloseMutexSem ((HMTX)(mutex->backend));
  return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  if (DosRequestMutexSem ((HMTX)(mutex->backend),-1L) != 0)
    return -1;
  else
    return 0;
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  if (DosRequestMutexSem ((HMTX)(mutex->backend),0L) != 0)
    return -1;
  else
    return 0;
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  if (DosReleaseMutexSem((HMTX)(mutex->backend)) != 0)
    return -1;
  else
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
