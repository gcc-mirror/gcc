/* MIPS SDE threads compatibility routines for libgcc2 and libobjc.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2006-2025 Free Software Foundation, Inc.
   Contributed by Nigel Stephens <nigel@mips.com>

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

#ifndef GCC_GTHR_MIPSSDE_H
#define GCC_GTHR_MIPSSDE_H

/* MIPS SDE threading API specific definitions.
   Easy, since the interface is pretty much one-to-one.  */

#define __GTHREADS 1

#include <sdethread.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef __sdethread_key_t __gthread_key_t;
typedef __sdethread_once_t __gthread_once_t;
typedef __sdethread_mutex_t __gthread_mutex_t;

typedef struct {
  long depth;
  __sdethread_t owner;
  __sdethread_mutex_t actual;
} __gthread_recursive_mutex_t;

#define __GTHREAD_MUTEX_INIT __SDETHREAD_MUTEX_INITIALIZER("gthr")
#define __GTHREAD_ONCE_INIT __SDETHREAD_ONCE_INIT
static inline int
__gthread_recursive_mutex_init_function(__gthread_recursive_mutex_t *__mutex);
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION __gthread_recursive_mutex_init_function

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK
# define __gthrw(name) \
  static __typeof(name) __gthrw_ ## name __attribute__ ((__weakref__(#name)));
# define __gthrw_(name) __gthrw_ ## name
#else
# define __gthrw(name)
# define __gthrw_(name) name
#endif

__gthrw(__sdethread_once)
__gthrw(__sdethread_key_create)
__gthrw(__sdethread_key_delete)
__gthrw(__sdethread_getspecific)
__gthrw(__sdethread_setspecific)

__gthrw(__sdethread_self)

__gthrw(__sdethread_mutex_lock)
__gthrw(__sdethread_mutex_trylock)
__gthrw(__sdethread_mutex_unlock)

__gthrw(__sdethread_mutex_init)

__gthrw(__sdethread_threading)

#if SUPPORTS_WEAK && GTHREAD_USE_WEAK

static inline int
__gthread_active_p (void)
{
  return !!(void *)&__sdethread_threading;
}

#else /* not SUPPORTS_WEAK */

static inline int
__gthread_active_p (void)
{
  return 1;
}

#endif /* SUPPORTS_WEAK */

static inline int
__gthread_once (__gthread_once_t *__once, void (*__func) (void))
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_once) (__once, __func);
  else
    return -1;
}

static inline int
__gthread_key_create (__gthread_key_t *__key, void (*__dtor) (void *))
{
  return __gthrw_(__sdethread_key_create) (__key, __dtor);
}

static inline int
__gthread_key_delete (__gthread_key_t __key)
{
  return __gthrw_(__sdethread_key_delete) (__key);
}

static inline void *
__gthread_getspecific (__gthread_key_t __key)
{
  return __gthrw_(__sdethread_getspecific) (__key);
}

static inline int
__gthread_setspecific (__gthread_key_t __key, const void *__ptr)
{
  return __gthrw_(__sdethread_setspecific) (__key, __ptr);
}

static inline int
__gthread_mutex_destroy (__gthread_mutex_t * UNUSED(__mutex))
{
  return 0;
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_mutex_lock) (__mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_mutex_trylock) (__mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthrw_(__sdethread_mutex_unlock) (__mutex);
  else
    return 0;
}

static inline int
__gthread_recursive_mutex_init_function (__gthread_recursive_mutex_t *__mutex)
{
  __mutex->depth = 0;
  __mutex->owner = __gthrw_(__sdethread_self) ();
  return __gthrw_(__sdethread_mutex_init) (&__mutex->actual, NULL);
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    {
      __sdethread_t __me = __gthrw_(__sdethread_self) ();

      if (__mutex->owner != __me)
	{
	  __gthrw_(__sdethread_mutex_lock) (&__mutex->actual);
	  __mutex->owner = __me;
	}

      __mutex->depth++;
    }
  return 0;
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    {
      __sdethread_t __me = __gthrw_(__sdethread_self) ();

      if (__mutex->owner != __me)
	{
	  if (__gthrw_(__sdethread_mutex_trylock) (&__mutex->actual))
	    return 1;
	  __mutex->owner = __me;
	}

      __mutex->depth++;
    }
  return 0;
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    {
      if (--__mutex->depth == 0)
	{
	   __mutex->owner = (__sdethread_t) 0;
	   __gthrw_(__sdethread_mutex_unlock) (&__mutex->actual);
	}
    }
  return 0;
}

static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t
                                   * UNUSED(__mutex))
{
  return 0;
}

#ifdef __cplusplus
}
#endif

#endif /* ! GCC_GTHR_MIPSSDE_H */
