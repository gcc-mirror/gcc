/* Threads compatibility routines for libgcc2 and libobjc.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997-2024 Free Software Foundation, Inc.

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

#ifndef GCC_GTHR_DCE_H
#define GCC_GTHR_DCE_H

/* If _DCE_THREADS is not defined, then we're building the single
   threaded version of the libraries and do not want to reference
   anything related to pthreads or dce.  */
#ifndef _DCE_THREADS
#include "gthr-single.h"
#else
/* DCE threads interface.
   DCE threads are based on POSIX threads draft 4, and many things
   have changed since then.  */

/* Make sure CONST_CAST2 (original in system.h) is defined.  */
#ifndef CONST_CAST2
#ifdef __cplusplus
#define CONST_CAST2(TOTYPE,FROMTYPE,X) (const_cast<TOTYPE> (X))
#else
#define CONST_CAST2(TOTYPE,FROMTYPE,X) ((__extension__(union {FROMTYPE _q; TOTYPE _nq;})(X))._nq)
#endif
#endif

#define __GTHREADS 1

#include <pthread.h>

typedef pthread_key_t __gthread_key_t;
typedef pthread_once_t __gthread_once_t;
typedef pthread_mutex_t __gthread_mutex_t;
typedef pthread_mutex_t __gthread_recursive_mutex_t;

#define __GTHREAD_ONCE_INIT pthread_once_init

#define __GTHREAD_MUTEX_INIT_FUNCTION __gthread_mutex_init_function
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION __gthread_recursive_mutex_init_function

#define __GTHREAD_MUTEX_INIT_DEFAULT pthread_once_init

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK
# define __gthrw(name) \
  static __typeof(name) __gthrw_ ## name __attribute__ ((__weakref__(#name)));
# define __gthrw_(name) __gthrw_ ## name
#else
# define __gthrw(name)
# define __gthrw_(name) name
#endif

__gthrw(pthread_once)
__gthrw(pthread_keycreate)
__gthrw(pthread_getspecific)
__gthrw(pthread_setspecific)
__gthrw(pthread_create)
__gthrw(pthread_mutex_init)
__gthrw(pthread_mutex_destroy)
__gthrw(pthread_mutex_lock)
__gthrw(pthread_mutex_trylock)
__gthrw(pthread_mutex_unlock)
__gthrw(pthread_mutexattr_create)
__gthrw(pthread_mutexattr_setkind_np)
__gthrw(pthread_mutexattr_delete)

#ifdef _LIBOBJC
/* Objective-C.  */
__gthrw(pthread_cond_broadcast)
__gthrw(pthread_cond_destroy)
__gthrw(pthread_cond_init)
__gthrw(pthread_cond_signal)
__gthrw(pthread_cond_wait)
__gthrw(pthread_exit)

#ifdef pthread_getunique_np
# define __gthrw_pthread_getunique_np pthread_getunique_np
#else
__gthrw(pthread_getunique_np)
# define __gthrw_pthread_getunique_np __gthrw_(pthread_getunique_np)
#endif

__gthrw(pthread_mutex_destroy)
__gthrw(pthread_self)
__gthrw(pthread_yield)
#endif

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK

static inline int
__gthread_active_p (void)
{
  static void *const __gthread_active_ptr = (void *) &__gthrw_(pthread_create);
  return __gthread_active_ptr != 0;
}

#else /* not SUPPORTS_WEAK */

static inline int
__gthread_active_p (void)
{
  return 1;
}

#endif /* SUPPORTS_WEAK */

#ifdef _LIBOBJC

/* Key structure for maintaining thread specific storage */
static pthread_key_t _objc_thread_storage;

/* Thread local storage for a single thread */
static void *thread_local_storage = NULL;

/* Backend initialization functions */

/* Initialize the threads subsystem.  */
static inline int
__gthread_objc_init_thread_system (void)
{
  if (__gthread_active_p ())
    /* Initialize the thread storage key.  */
    return __gthrw_(pthread_keycreate) (&_objc_thread_storage, NULL);
  else
    return -1;
}

/* Close the threads subsystem.  */
static inline int
__gthread_objc_close_thread_system (void)
{
  if (__gthread_active_p ())
    return 0;
  else
    return -1;
}

/* Backend thread functions */

/* Create a new thread of execution.  */
static inline objc_thread_t
__gthread_objc_thread_detach (void (*func)(void *), void *arg)
{
  objc_thread_t thread_id;
  pthread_t new_thread_handle;

  if (!__gthread_active_p ())
    return NULL;

  if (!(__gthrw_(pthread_create) (&new_thread_handle, pthread_attr_default,
			(void *) func, arg)))
    {
      /* ??? May not work! (64bit) */
      thread_id = *(objc_thread_t *) &new_thread_handle;
      pthread_detach (&new_thread_handle); /* Fully detach thread.  */
    }
  else
    thread_id = NULL;

  return thread_id;
}

/* Set the current thread's priority.  */
static inline int
__gthread_objc_thread_set_priority (int priority)
{
  int sys_priority = 0;

  if (!__gthread_active_p ())
    return -1;

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

  /* Change the priority.  */
  if (pthread_setprio (__gthrw_(pthread_self) (), sys_priority) >= 0)
    return 0;
  else
    /* Failed */
    return -1;
}

/* Return the current thread's priority.  */
static inline int
__gthread_objc_thread_get_priority (void)
{
  int sys_priority;

  if (__gthread_active_p ())
    {
      if ((sys_priority = pthread_getprio (__gthrw_(pthread_self) ())) >= 0)
	{
	  if (sys_priority >= PRI_FG_MIN_NP
	      && sys_priority <= PRI_FG_MAX_NP)
	    return OBJC_THREAD_INTERACTIVE_PRIORITY;
	  if (sys_priority >= PRI_BG_MIN_NP
	      && sys_priority <= PRI_BG_MAX_NP)
	    return OBJC_THREAD_BACKGROUND_PRIORITY;
	  return OBJC_THREAD_LOW_PRIORITY;
	}

      /* Failed */
      return -1;
    }
  else
    return OBJC_THREAD_INTERACTIVE_PRIORITY;
}

/* Yield our process time to another thread.  */
static inline void
__gthread_objc_thread_yield (void)
{
  if (__gthread_active_p ())
    __gthrw_(pthread_yield) ();
}

/* Terminate the current thread.  */
static inline int
__gthread_objc_thread_exit (void)
{
  if (__gthread_active_p ())
    /* exit the thread */
    __gthrw_(pthread_exit) (&__objc_thread_exit_status);

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread.  */
static inline objc_thread_t
__gthread_objc_thread_id (void)
{
  if (__gthread_active_p ())
    {
      pthread_t self = __gthrw_(pthread_self) ();

      return (objc_thread_t) __gthrw_pthread_getunique_np (&self);
    }
  else
    return (objc_thread_t) 1;
}

/* Sets the thread's local storage pointer.  */
static inline int
__gthread_objc_thread_set_data (void *value)
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_setspecific) (_objc_thread_storage, value);
  else
    {
      thread_local_storage = value;
      return 0;
    }
}

/* Returns the thread's local storage pointer.  */
static inline void *
__gthread_objc_thread_get_data (void)
{
  void *value = NULL;

  if (__gthread_active_p ())
    {
      if (!(__gthrw_(pthread_getspecific) (_objc_thread_storage, &value)))
	return value;

      return NULL;
    }
  else
    return thread_local_storage;
}

/* Backend mutex functions */

/* Allocate a mutex.  */
static inline int
__gthread_objc_mutex_allocate (objc_mutex_t mutex)
{
  if (__gthread_active_p ())
    {
      mutex->backend = objc_malloc (sizeof (pthread_mutex_t));

      if (__gthrw_(pthread_mutex_init) ((pthread_mutex_t *) mutex->backend,
			      pthread_mutexattr_default))
	{
	  objc_free (mutex->backend);
	  mutex->backend = NULL;
	  return -1;
	}
    }

  return 0;
}

/* Deallocate a mutex.  */
static inline int
__gthread_objc_mutex_deallocate (objc_mutex_t mutex)
{
  if (__gthread_active_p ())
    {
      if (__gthrw_(pthread_mutex_destroy) ((pthread_mutex_t *) mutex->backend))
	return -1;

      objc_free (mutex->backend);
      mutex->backend = NULL;
    }

  return 0;
}

/* Grab a lock on a mutex.  */
static inline int
__gthread_objc_mutex_lock (objc_mutex_t mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_mutex_lock) ((pthread_mutex_t *) mutex->backend);
  else
    return 0;
}

/* Try to grab a lock on a mutex.  */
static inline int
__gthread_objc_mutex_trylock (objc_mutex_t mutex)
{
  if (__gthread_active_p ()
      && __gthrw_(pthread_mutex_trylock) ((pthread_mutex_t *) mutex->backend) != 1)
    return -1;

  return 0;
}

/* Unlock the mutex */
static inline int
__gthread_objc_mutex_unlock (objc_mutex_t mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_mutex_unlock) ((pthread_mutex_t *) mutex->backend);
  else
    return 0;
}

/* Backend condition mutex functions */

/* Allocate a condition.  */
static inline int
__gthread_objc_condition_allocate (objc_condition_t condition
				   __attribute__ ((__unused__)))
{
  if (__gthread_active_p ())
    /* Unimplemented.  */
    return -1;
  else
    return 0;
}

/* Deallocate a condition.  */
static inline int
__gthread_objc_condition_deallocate (objc_condition_t condition
				     __attribute__ ((__unused__)))
{
  if (__gthread_active_p ())
    /* Unimplemented.  */
    return -1;
  else
    return 0;
}

/* Wait on the condition */
static inline int
__gthread_objc_condition_wait (objc_condition_t condition
			       __attribute__ ((__unused__)),
			       objc_mutex_t mutex __attribute__ ((__unused__)))
{
  if (__gthread_active_p ())
    /* Unimplemented.  */
    return -1;
  else
    return 0;
}

/* Wake up all threads waiting on this condition.  */
static inline int
__gthread_objc_condition_broadcast (objc_condition_t condition
				    __attribute__ ((__unused__)))
{
  if (__gthread_active_p ())
    /* Unimplemented.  */
    return -1;
  else
    return 0;
}

/* Wake up one thread waiting on this condition.  */
static inline int
__gthread_objc_condition_signal (objc_condition_t condition
				 __attribute__ ((__unused__)))
{
  if (__gthread_active_p ())
    /* Unimplemented.  */
    return -1;
  else
    return 0;
}

#else /* _LIBOBJC */

static inline int
__gthread_once (__gthread_once_t *__once, void (*__func) (void))
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_once) (__once, __func);
  else
    return -1;
}

static inline int
__gthread_key_create (__gthread_key_t *__key, void (*__dtor) (void *))
{
  return __gthrw_(pthread_keycreate) (__key, __dtor);
}

static inline int
__gthread_key_delete (__gthread_key_t __key __attribute__ ((__unused__)))
{
  /* Operation is not supported.  */
  return -1;
}

static inline void *
__gthread_getspecific (__gthread_key_t __key)
{
  void *__ptr;
  if (__gthrw_(pthread_getspecific) (__key, &__ptr) == 0)
    return __ptr;
  else
    return 0;
}

static inline int
__gthread_setspecific (__gthread_key_t __key, const void *__ptr)
{
  return __gthrw_(pthread_setspecific)
    (__key, CONST_CAST2(void *, const void *, __ptr));
}

static inline void
__gthread_mutex_init_function (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    __gthrw_(pthread_mutex_init) (__mutex, pthread_mutexattr_default);
}

static inline int
__gthread_mutex_destroy (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_mutex_destroy) (__mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_mutex_lock) (__mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_mutex_trylock) (__mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(pthread_mutex_unlock) (__mutex);
  else
    return 0;
}

static inline int
__gthread_recursive_mutex_init_function (__gthread_recursive_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    {
      pthread_mutexattr_t __attr;
      int __r;

      __r = __gthrw_(pthread_mutexattr_create) (&__attr);
      if (!__r)
	__r = __gthrw_(pthread_mutexattr_setkind_np) (&__attr,
						      MUTEX_RECURSIVE_NP);
      if (!__r)
	__r = __gthrw_(pthread_mutex_init) (__mutex, __attr);
      if (!__r)
	__r = __gthrw_(pthread_mutexattr_delete) (&__attr);
      return __r;
    }
  return 0;
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_lock (__mutex);
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_trylock (__mutex);
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_unlock (__mutex);
}

static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_destroy (__mutex);
}

#endif /* _LIBOBJC */

#endif
#endif /* ! GCC_GTHR_DCE_H */
