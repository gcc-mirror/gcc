/* Copyright (C) 2002-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Threads compatibility routines for libgcc2 for VxWorks.

   This file implements the GTHREAD_CXX0X part of the interface
   exposed by gthr-vxworks.h, using APIs exposed by regular (!AE/653)
   VxWorks kernels.  */

#include "gthr.h"

#if __GTHREADS_CXX0X

#include <taskLib.h>
#include <stdlib.h>

#define __TIMESPEC_TO_NSEC(timespec) \
  ((long long)timespec.tv_sec * 1000000000 + (long long)timespec.tv_nsec)

#define __TIMESPEC_TO_TICKS(timespec) \
  ((long long)(sysClkRateGet() * __TIMESPEC_TO_NSEC(timespec) + 999999999) \
    / 1000000000)

#ifdef __RTP__
  void tls_delete_hook (void);
  #define __CALL_DELETE_HOOK(tcb) tls_delete_hook()
#else
  /* In kernel mode, we need to pass the TCB to task_delete_hook. The TCB is
     the pointer to the WIND_TCB structure and is the ID of the task.  */
  void tls_delete_hook (void *TCB);
  #define __CALL_DELETE_HOOK(tcb) tls_delete_hook((WIND_TCB *) ((tcb)->task_id))
#endif

int
__gthread_cond_signal (__gthread_cond_t *cond)
{
  if (!cond)
    return ERROR;

  /* If nobody is waiting, skip the semGive altogether: no one can get
     in line while we hold the mutex associated with *COND.  We could
     skip this test altogether, but it's presumed cheaper than going
     through the give and take below, and that a signal without a
     waiter occurs often enough for the test to be worth it.  */
  SEM_INFO info;
  memset (&info, 0, sizeof (info));
  __RETURN_ERRNO_IF_NOT_OK (semInfoGet (*cond, &info));
  if (info.numTasks == 0)
    return OK;

  int ret = __CHECK_RESULT (semGive (*cond));

  /* It might be the case, however, that when we called semInfo, there
     was a waiter just about to timeout, and by the time we called
     semGive, it had already timed out, so our semGive would leave the
     *cond semaphore full, so the next caller of wait would pass
     through.  We don't want that.  So, make sure we leave the
     semaphore empty.  Despite the window in which the semaphore will
     be full, this works because:

     - we're holding the mutex, so nobody else can semGive, and any
       pending semTakes are actually within semExchange.  there might
       be others blocked to acquire the mutex, but those are not
       relevant for the analysis.

     - if there was another non-timed out waiter, semGive will wake it
       up immediately instead of leaving the semaphore full, so the
       semTake below will time out, and the semantics are as expected

     - otherwise, if all waiters timed out before the semGive (or if
       there weren't any to begin with), our semGive completed leaving
       the semaphore full, and our semTake below will consume it
       before any other waiter has a change to reach the semExchange,
       because we're holding the mutex.  */
  if (ret == OK)
    semTake (*cond, NO_WAIT);

  return ret;
}

/* -------------------- Timed Condition Variables --------------------- */

int
__gthread_cond_timedwait (__gthread_cond_t *cond,
			  __gthread_mutex_t *mutex,
			  const __gthread_time_t *abs_timeout)
{
  if (!cond)
    return ERROR;

  if (!mutex)
    return ERROR;

  if (!abs_timeout)
    return ERROR;

  struct timespec current;
  if (clock_gettime (CLOCK_REALTIME, &current) == ERROR)
    /* CLOCK_REALTIME is not supported.  */
    return ERROR;

  const long long abs_timeout_ticks = __TIMESPEC_TO_TICKS ((*abs_timeout));
  const long long current_ticks = __TIMESPEC_TO_TICKS (current);

  long long waiting_ticks;

  if (current_ticks < abs_timeout_ticks)
    waiting_ticks = abs_timeout_ticks - current_ticks;
  else
    /* The point until we would need to wait is in the past,
       no need to wait at all.  */
    waiting_ticks = 0;

  /* We check that waiting_ticks can be safely casted as an int.  */
  if (waiting_ticks > INT_MAX)
    waiting_ticks = INT_MAX;

  int ret = __CHECK_RESULT (semExchange (*mutex, *cond, waiting_ticks));

  __RETURN_ERRNO_IF_NOT_OK (semTake (*mutex, WAIT_FOREVER));

  return ret;
}

/* --------------------------- Timed Mutexes ------------------------------ */

int
__gthread_mutex_timedlock (__gthread_mutex_t *m,
			   const __gthread_time_t *abs_time)
{
  if (!m)
    return ERROR;

  if (!abs_time)
    return ERROR;

  struct timespec current;
  if (clock_gettime (CLOCK_REALTIME, &current) == ERROR)
    /* CLOCK_REALTIME is not supported.  */
    return ERROR;

  const long long abs_timeout_ticks = __TIMESPEC_TO_TICKS ((*abs_time));
  const long long current_ticks = __TIMESPEC_TO_TICKS (current);
  long long waiting_ticks;

  if (current_ticks < abs_timeout_ticks)
    waiting_ticks = abs_timeout_ticks - current_ticks;
  else
    /* The point until we would need to wait is in the past,
       no need to wait at all.  */
    waiting_ticks = 0;

  /* Make sure that waiting_ticks can be safely casted as an int.  */
  if (waiting_ticks > INT_MAX)
    waiting_ticks = INT_MAX;

  return __CHECK_RESULT (semTake (*m, waiting_ticks));
}

int
__gthread_recursive_mutex_timedlock (__gthread_recursive_mutex_t *mutex,
				     const __gthread_time_t *abs_timeout)
{
  return __gthread_mutex_timedlock ((__gthread_mutex_t *)mutex, abs_timeout);
}

/* ------------------------------ Threads --------------------------------- */

/* Task control block initialization and destruction functions.  */

int
__init_gthread_tcb (__gthread_t __tcb)
{
  if (!__tcb)
    return ERROR;

  __gthread_mutex_init (&(__tcb->return_value_available));
  if (__tcb->return_value_available == SEM_ID_NULL)
    return ERROR;

  __gthread_mutex_init (&(__tcb->delete_ok));
  if (__tcb->delete_ok == SEM_ID_NULL)
    goto return_sem_delete;

  /* We lock the two mutexes used for signaling.  */
  if (__gthread_mutex_lock (&(__tcb->delete_ok)) != OK)
    goto delete_sem_delete;

  if (__gthread_mutex_lock (&(__tcb->return_value_available)) != OK)
    goto delete_sem_delete;

  __tcb->task_id = TASK_ID_NULL;
  return OK;

delete_sem_delete:
  semDelete (__tcb->delete_ok);
return_sem_delete:
  semDelete (__tcb->return_value_available);
  return ERROR;
}

/* Here, we pass a pointer to a tcb to allow calls from
   cleanup attributes.  */
void
__delete_gthread_tcb (__gthread_t* __tcb)
{
  semDelete ((*__tcb)->return_value_available);
  semDelete ((*__tcb)->delete_ok);
  free (*__tcb);
}

/* This __gthread_t stores the address of the TCB malloc'ed in
   __gthread_create.  It is then accessible via __gthread_self().  */
__thread __gthread_t __local_tcb = NULL;

__gthread_t
__gthread_self (void)
{
  if (!__local_tcb)
    {
      /* We are in the initial thread, we need to initialize the TCB.  */
      __local_tcb = malloc (sizeof (*__local_tcb));
      if (!__local_tcb)
	return NULL;

      if (__init_gthread_tcb (__local_tcb) != OK)
	{
	  __delete_gthread_tcb (&__local_tcb);
	  return NULL;
	}
      /* We do not set the mutexes in the structure as a thread is not supposed
         to join or detach himself.  */
      __local_tcb->task_id = taskIdSelf ();
    }
  return __local_tcb;
}

int
__task_wrapper (__gthread_t tcb, FUNCPTR __func, _Vx_usr_arg_t __args)
{
  if (!tcb)
    return ERROR;

  __local_tcb = tcb;

  /* We use this variable to avoid memory leaks in the case where
     the underlying function throws an exception.  */
  __attribute__ ((cleanup (__delete_gthread_tcb))) __gthread_t __tmp = tcb;

  void *return_value = (void *) __func (__args);
  tcb->return_value = return_value;

  /* Call the destructors.  */
  __CALL_DELETE_HOOK (tcb);

  /* Future calls of join() will be able to retrieve the return value.  */
  __gthread_mutex_unlock (&tcb->return_value_available);

  /* We wait for the thread to be joined or detached.  */
  __gthread_mutex_lock (&(tcb->delete_ok));
  __gthread_mutex_unlock (&(tcb->delete_ok));

  /* Memory deallocation is done by the cleanup attribute of the tmp variable.  */

  return OK;
}

/* Proper gthreads API.  */

int
__gthread_create (__gthread_t * __threadid, void *(*__func) (void *),
		  void *__args)
{
  if (!__threadid)
    return ERROR;

  int priority;
  __RETURN_ERRNO_IF_NOT_OK (taskPriorityGet (taskIdSelf (), &priority));

  int options;
  __RETURN_ERRNO_IF_NOT_OK (taskOptionsGet (taskIdSelf (), &options));

#if defined (__SPE__)
  options |= VX_SPE_TASK;
#else
  options |= VX_FP_TASK;
#endif
  options &= VX_USR_TASK_OPTIONS;

  int stacksize = 20 * 1024;

  __gthread_t tcb = malloc (sizeof (*tcb));
  if (!tcb)
    return ERROR;

  if (__init_gthread_tcb (tcb) != OK)
    {
      free (tcb);
      return ERROR;
    }

  TASK_ID task_id = taskCreate (NULL,
				priority, options, stacksize,
				(FUNCPTR) & __task_wrapper,
				(_Vx_usr_arg_t) tcb,
				(_Vx_usr_arg_t) __func,
				(_Vx_usr_arg_t) __args,
				0, 0, 0, 0, 0, 0, 0);

  /* If taskCreate succeeds, task_id will be a valid TASK_ID and not zero.  */
  __RETURN_ERRNO_IF_NOT_OK (!task_id);

  tcb->task_id = task_id;
  *__threadid = tcb;

  return __CHECK_RESULT (taskActivate (task_id));
}

int
__gthread_equal (__gthread_t __t1, __gthread_t __t2)
{
  return (__t1 == __t2) ? OK : ERROR;
}

int
__gthread_yield (void)
{
  return taskDelay (0);
}

int
__gthread_join (__gthread_t __threadid, void **__value_ptr)
{
  if (!__threadid)
    return ERROR;

  /* A thread cannot join itself.  */
  if (__threadid->task_id == taskIdSelf ())
    return ERROR;

  /* Waiting for the task to set the return value.  */
  __gthread_mutex_lock (&__threadid->return_value_available);
  __gthread_mutex_unlock (&__threadid->return_value_available);

  if (__value_ptr)
    *__value_ptr = __threadid->return_value;

  /* The task will be safely be deleted.  */
  __gthread_mutex_unlock (&(__threadid->delete_ok));

  __RETURN_ERRNO_IF_NOT_OK (taskWait (__threadid->task_id, WAIT_FOREVER));

  return OK;
}

int
__gthread_detach (__gthread_t __threadid)
{
  if (!__threadid)
    return ERROR;

  if (taskIdVerify (__threadid->task_id) != OK)
    return ERROR;

  /* The task will be safely be deleted.  */
  __gthread_mutex_unlock (&(__threadid->delete_ok));

  return OK;
}

#endif
