/* GNU Objective C Runtime Thread Interface - Win32 Implementation
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

#ifndef __OBJC__
#define __OBJC__
#endif
#include <windows.h>

/* Key structure for maintaining thread specific storage */
static DWORD	__objc_data_tls = (DWORD)-1;

/* Backend initialization functions */

/* Initialize the threads subsystem. */
int
__objc_init_thread_system(void)
{
  /* Initialize the thread storage key */
  if ((__objc_data_tls = TlsAlloc()) != (DWORD)-1)
    return 0;
  else
    return -1;
}

/* Close the threads subsystem. */
int
__objc_close_thread_system(void)
{
  if (__objc_data_tls != (DWORD)-1)
    TlsFree(__objc_data_tls);
  return 0;
}

/* Backend thread functions */

/* Create a new thread of execution. */
objc_thread_t
__objc_thread_detach(void (*func)(void *arg), void *arg)
{
  DWORD	thread_id = 0;
  HANDLE win32_handle;

  if (!(win32_handle = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)func,
                                   arg, 0, &thread_id)))
    thread_id = 0;
  
  return (objc_thread_t)thread_id;
}

/* Set the current thread's priority. */
int
__objc_thread_set_priority(int priority)
{
  int sys_priority = 0;

  switch (priority)
    {
    case OBJC_THREAD_INTERACTIVE_PRIORITY:
      sys_priority = THREAD_PRIORITY_NORMAL;
      break;
    default:
    case OBJC_THREAD_BACKGROUND_PRIORITY:
      sys_priority = THREAD_PRIORITY_BELOW_NORMAL;
      break;
    case OBJC_THREAD_LOW_PRIORITY:
      sys_priority = THREAD_PRIORITY_LOWEST;
      break;
    }

  /* Change priority */
  if (SetThreadPriority(GetCurrentThread(), sys_priority))
    return 0;
  else
    return -1;
}

/* Return the current thread's priority. */
int
__objc_thread_get_priority(void)
{
  int sys_priority;

  sys_priority = GetThreadPriority(GetCurrentThread());
  
  switch (sys_priority)
    {
    case THREAD_PRIORITY_HIGHEST:
    case THREAD_PRIORITY_TIME_CRITICAL:
    case THREAD_PRIORITY_ABOVE_NORMAL:
    case THREAD_PRIORITY_NORMAL:
      return OBJC_THREAD_INTERACTIVE_PRIORITY;

    default:
    case THREAD_PRIORITY_BELOW_NORMAL:
      return OBJC_THREAD_BACKGROUND_PRIORITY;
    
    case THREAD_PRIORITY_IDLE:
    case THREAD_PRIORITY_LOWEST:
      return OBJC_THREAD_LOW_PRIORITY;
    }

  /* Couldn't get priority. */
  return -1;
}

/* Yield our process time to another thread. */
void
__objc_thread_yield(void)
{
  Sleep(0);
}

/* Terminate the current thread. */
int
__objc_thread_exit(void)
{
  /* exit the thread */
  ExitThread(__objc_thread_exit_status);

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread. */
objc_thread_t
__objc_thread_id(void)
{
  return (objc_thread_t)GetCurrentThreadId();
}

/* Sets the thread's local storage pointer. */
int
__objc_thread_set_data(void *value)
{
  if (TlsSetValue(__objc_data_tls, value))
    return 0;
  else
    return -1;
}

/* Returns the thread's local storage pointer. */
void *
__objc_thread_get_data(void)
{
  return TlsGetValue(__objc_data_tls);          /* Return thread data.      */
}

/* Backend mutex functions */

/* Allocate a mutex. */
int
__objc_mutex_allocate(objc_mutex_t mutex)
{
  if ((mutex->backend = (void *)CreateMutex(NULL, 0, NULL)) == NULL)
    return -1;
  else
    return 0;
}

/* Deallocate a mutex. */
int
__objc_mutex_deallocate(objc_mutex_t mutex)
{
  CloseHandle((HANDLE)(mutex->backend));
  return 0;
}

/* Grab a lock on a mutex. */
int
__objc_mutex_lock(objc_mutex_t mutex)
{
  int status;

  status = WaitForSingleObject((HANDLE)(mutex->backend), INFINITE);
  if (status != WAIT_OBJECT_0 && status != WAIT_ABANDONED)
    return -1;
  else
    return 0;
}

/* Try to grab a lock on a mutex. */
int
__objc_mutex_trylock(objc_mutex_t mutex)
{
  int status;

  status = WaitForSingleObject((HANDLE)(mutex->backend), 0);
  if (status != WAIT_OBJECT_0 && status != WAIT_ABANDONED)
    return -1;
  else
    return 0;
}

/* Unlock the mutex */
int
__objc_mutex_unlock(objc_mutex_t mutex)
{
  if (ReleaseMutex((HANDLE)(mutex->backend)) == 0)
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
