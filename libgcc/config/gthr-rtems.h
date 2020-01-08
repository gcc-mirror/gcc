/* RTEMS threads compatibility routines for libgcc2 and libobjc.
   by: Rosimildo da Silva( rdasilva@connecttel.com ) */
/* Compile this one with gcc.  */
/* Copyright (C) 1997-2020 Free Software Foundation, Inc.

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

#ifndef GCC_GTHR_RTEMS_H
#define GCC_GTHR_RTEMS_H

#include <sys/lock.h>
#include <pthread.h>
#include <sched.h>

#ifdef __cplusplus
extern "C" {
#endif

#define __GTHREADS 1
#define __GTHREADS_CXX0X 1
#define __GTHREAD_HAS_COND 1

typedef pthread_t __gthread_t;
typedef pthread_key_t __gthread_key_t;
typedef pthread_once_t __gthread_once_t;
typedef struct _Mutex_Control __gthread_mutex_t;
typedef struct _Mutex_recursive_Control __gthread_recursive_mutex_t;
typedef struct _Condition_Control __gthread_cond_t;
typedef struct timespec __gthread_time_t;

#define __GTHREAD_ONCE_INIT PTHREAD_ONCE_INIT
#define __GTHREAD_MUTEX_INIT _MUTEX_INITIALIZER
#define __GTHREAD_MUTEX_INIT_FUNCTION _Mutex_Initialize
#define __GTHREAD_RECURSIVE_MUTEX_INIT _MUTEX_RECURSIVE_INITIALIZER
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION _Mutex_recursive_Initialize
#define __GTHREAD_COND_INIT _CONDITION_INITIALIZER
#define __GTHREAD_COND_INIT_FUNCTION _Condition_Initialize
#define __GTHREAD_TIME_INIT {0, 0}

static inline int
__gthread_active_p (void)
{
  return 1;
}

static inline int
__gthread_create (__gthread_t *__threadid, void *(*__func) (void *),
		  void *__args)
{
  return pthread_create (__threadid, NULL, __func, __args);
}

static inline int
__gthread_join (__gthread_t __threadid, void **__value_ptr)
{
  return pthread_join (__threadid, __value_ptr);
}

static inline int
__gthread_detach (__gthread_t __threadid)
{
  return pthread_detach (__threadid);
}

static inline int
__gthread_equal (__gthread_t __t1, __gthread_t __t2)
{
  return pthread_equal (__t1, __t2);
}

static inline __gthread_t
__gthread_self (void)
{
  return pthread_self ();
}

static inline int
__gthread_yield (void)
{
  return sched_yield ();
}

static inline int
__gthread_once (__gthread_once_t *__once, void (*__func) (void))
{
   return pthread_once (__once, __func);
}

static inline int
__gthread_key_create (__gthread_key_t *__key, void (*__dtor) (void *))
{
  return pthread_key_create (__key, __dtor);
}

static inline int
__gthread_key_delete (__gthread_key_t __key)
{
  return pthread_key_delete (__key);
}

static inline void *
__gthread_getspecific (__gthread_key_t __key)
{
  return pthread_getspecific (__key);
}

static inline int
__gthread_setspecific (__gthread_key_t __key, const void *__ptr)
{
  return pthread_setspecific (__key, __ptr);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
  _Mutex_Acquire (__mutex);
  return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
  return _Mutex_Try_acquire (__mutex);
}

static inline int
__gthread_mutex_timedlock (__gthread_mutex_t *__mutex,
			   const __gthread_time_t *__abs_timeout)
{
  return _Mutex_Acquire_timed (__mutex, __abs_timeout);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
  _Mutex_Release (__mutex);
  return 0;
}

static inline int
__gthread_mutex_destroy (__gthread_mutex_t *__mutex)
{
  _Mutex_Destroy (__mutex);
  return 0;
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex)
{
  _Mutex_recursive_Acquire (__mutex);
  return 0;
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
  return _Mutex_recursive_Try_acquire (__mutex);
}

static inline int
__gthread_recursive_mutex_timedlock (__gthread_recursive_mutex_t *__mutex,
				     const __gthread_time_t *__abs_timeout)
{
  return _Mutex_recursive_Acquire_timed (__mutex, __abs_timeout);
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex)
{
  _Mutex_recursive_Release (__mutex);
  return 0;
}

static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex)
{
  _Mutex_recursive_Destroy (__mutex);
  return 0;
}

static inline int
__gthread_cond_broadcast (__gthread_cond_t *__cond)
{
  _Condition_Broadcast (__cond);
  return 0;
}

static inline int
__gthread_cond_signal (__gthread_cond_t *__cond)
{
  _Condition_Signal (__cond);
  return 0;
}

static inline int
__gthread_cond_wait (__gthread_cond_t *__cond, __gthread_mutex_t *__mutex)
{
  _Condition_Wait (__cond, __mutex);
  return 0;
}

static inline int
__gthread_cond_timedwait (__gthread_cond_t *__cond, __gthread_mutex_t *__mutex,
			  const __gthread_time_t *__abs_timeout)
{
  return _Condition_Wait_timed (__cond, __mutex, __abs_timeout);
}

static inline int
__gthread_cond_wait_recursive (__gthread_cond_t *__cond,
			       __gthread_recursive_mutex_t *__mutex)
{
  _Condition_Wait_recursive (__cond, __mutex);
  return 0;
}

static inline int
__gthread_cond_destroy (__gthread_cond_t *__cond)
{
  _Condition_Destroy (__cond);
  return 0;
}

#ifdef __cplusplus
}
#endif

#endif /* ! GCC_GTHR_RTEMS_H */
