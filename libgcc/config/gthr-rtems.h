/* RTEMS threads compatibility routines for libgcc2 and libobjc.
   by: Rosimildo da Silva( rdasilva@connecttel.com ) */
/* Compile this one with gcc.  */
/* Copyright (C) 1997-2013 Free Software Foundation, Inc.

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

#ifdef __cplusplus
extern "C" {
#endif

#define __GTHREADS 1

#define __GTHREAD_ONCE_INIT  0
#define __GTHREAD_MUTEX_INIT_FUNCTION  rtems_gxx_mutex_init
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION  rtems_gxx_recursive_mutex_init

/* Avoid dependency on rtems specific headers.  */
typedef void *__gthread_key_t;
typedef int   __gthread_once_t;
typedef void *__gthread_mutex_t;
typedef void *__gthread_recursive_mutex_t;

/*
 * External functions provided by RTEMS. They are very similar to their POSIX
 * counterparts. A "Wrapper API" is being use to avoid dependency on any RTEMS
 * header files.
 */

/* generic per task variables */
extern int rtems_gxx_once (__gthread_once_t *__once, void (*__func) (void));
extern int rtems_gxx_key_create (__gthread_key_t *__key, void (*__dtor) (void *));
extern int rtems_gxx_key_delete (__gthread_key_t __key);
extern void *rtems_gxx_getspecific (__gthread_key_t __key);
extern int rtems_gxx_setspecific (__gthread_key_t __key, const void *__ptr);

/* mutex support */
extern void rtems_gxx_mutex_init (__gthread_mutex_t *__mutex);
extern int rtems_gxx_mutex_destroy (__gthread_mutex_t *__mutex);
extern int rtems_gxx_mutex_lock (__gthread_mutex_t *__mutex);
extern int rtems_gxx_mutex_trylock (__gthread_mutex_t *__mutex);
extern int rtems_gxx_mutex_unlock (__gthread_mutex_t *__mutex);

/* recursive mutex support */
extern void rtems_gxx_recursive_mutex_init (__gthread_recursive_mutex_t *__mutex);
extern int rtems_gxx_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex);
extern int rtems_gxx_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex);
extern int rtems_gxx_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex);

/* RTEMS threading is always active */
static inline int
__gthread_active_p (void)
{
  return 1;
}

/* Wrapper calls */
static inline int
__gthread_once (__gthread_once_t *__once, void (*__func) (void))
{
   return rtems_gxx_once( __once, __func );
}

static inline int
__gthread_key_create (__gthread_key_t *__key, void (*__dtor) (void *))
{
  return rtems_gxx_key_create( __key, __dtor );
}

static inline int
__gthread_key_delete (__gthread_key_t __key)
{
  return rtems_gxx_key_delete (__key);
}

static inline void *
__gthread_getspecific (__gthread_key_t __key)
{
  return rtems_gxx_getspecific (__key);
}

static inline int
__gthread_setspecific (__gthread_key_t __key, const void *__ptr)
{
  return rtems_gxx_setspecific (__key, __ptr);
}

static inline int
__gthread_mutex_destroy (__gthread_mutex_t *__mutex)
{
  return rtems_gxx_mutex_destroy (__mutex);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
    return rtems_gxx_mutex_lock (__mutex);
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
    return rtems_gxx_mutex_trylock (__mutex);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
    return rtems_gxx_mutex_unlock( __mutex );
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex)
{
    return rtems_gxx_recursive_mutex_lock (__mutex);
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
    return rtems_gxx_recursive_mutex_trylock (__mutex);
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex)
{
    return rtems_gxx_recursive_mutex_unlock( __mutex );
}

static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex)
{
  /* This requires that recursive and non-recursive mutexes have the same
     representation.  */
    return rtems_gxx_mutex_destroy (__mutex );
}

#ifdef __cplusplus
}
#endif

#endif /* ! GCC_GTHR_RTEMS_H */
