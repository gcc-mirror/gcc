/* Threads compatibility routines for libgcc2 and libobjc for VxWorks.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997-2019 Free Software Foundation, Inc.
   Contributed by Mike Stump <mrs@wrs.com>.

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

#ifndef GCC_GTHR_VXWORKS_H
#define GCC_GTHR_VXWORKS_H

#ifdef _LIBOBJC

/* libobjc requires the optional pthreads component.  */
#include "gthr-posix.h"

#else
#ifdef __cplusplus
#define UNUSED(x)
#else
#define UNUSED(x) x __attribute__((__unused__))
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define __GTHREADS 1
#define __gthread_active_p() 1

/* Mutexes are easy, except that they need to be initialized at runtime.  */

#include <semLib.h>

typedef SEM_ID __gthread_mutex_t;
/* All VxWorks mutexes are recursive.  */
typedef SEM_ID __gthread_recursive_mutex_t;
#define __GTHREAD_MUTEX_INIT_FUNCTION __gthread_mutex_init_function
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION __gthread_recursive_mutex_init_function

static inline void
__gthread_mutex_init_function (__gthread_mutex_t *mutex)
{
  *mutex = semMCreate (SEM_Q_PRIORITY | SEM_INVERSION_SAFE | SEM_DELETE_SAFE);
}

static inline int
__gthread_mutex_destroy (__gthread_mutex_t *mutex)
{
  semDelete(*mutex);
  return 0;
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *mutex)
{
  return semTake (*mutex, WAIT_FOREVER);
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *mutex)
{
  return semTake (*mutex, NO_WAIT);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *mutex)
{
  return semGive (*mutex);
}

static inline void
__gthread_recursive_mutex_init_function (__gthread_recursive_mutex_t *mutex)
{
  __gthread_mutex_init_function (mutex);
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *mutex)
{
  return __gthread_mutex_lock (mutex);
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *mutex)
{
  return __gthread_mutex_trylock (mutex);
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *mutex)
{
  return __gthread_mutex_unlock (mutex);
}

static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_destroy (__mutex);
}

/* pthread_once is complicated enough that it's implemented
   out-of-line.  See config/vxlib.c.  */

typedef struct
{
#if !defined(__RTP__)
#if defined(__PPC__)
  __attribute ((aligned (__alignof (unsigned))))
#endif
  volatile unsigned char busy;
#endif
  volatile unsigned char done;
#if !defined(__RTP__) && defined(__PPC__)
  /* PPC's test-and-set implementation requires a 4 byte aligned
     object, of which it only sets the first byte.  We use padding
     here, in order to maintain some amount of backwards
     compatibility.  Without this padding, gthread_once objects worked
     by accident because they happen to be static objects and the ppc
     port automatically increased their alignment to 4 bytes.  */
  unsigned char pad1;
  unsigned char pad2;
#endif
}
__gthread_once_t;

#if defined (__RTP__)
# define __GTHREAD_ONCE_INIT { 0 }
#elif defined (__PPC__)
# define __GTHREAD_ONCE_INIT { 0, 0, 0, 0 }
#else
# define __GTHREAD_ONCE_INIT { 0, 0 }
#endif

extern int __gthread_once (__gthread_once_t *__once, void (*__func)(void));

/* Thread-specific data requires a great deal of effort, since VxWorks
   is not really set up for it.  See config/vxlib.c for the gory
   details.  All the TSD routines are sufficiently complex that they
   need to be implemented out of line.  */

typedef unsigned int __gthread_key_t;

extern int __gthread_key_create (__gthread_key_t *__keyp, void (*__dtor)(void *));
extern int __gthread_key_delete (__gthread_key_t __key);

extern void *__gthread_getspecific (__gthread_key_t __key);
extern int __gthread_setspecific (__gthread_key_t __key, void *__ptr);

#undef UNUSED

#ifdef __cplusplus
}
#endif

#endif /* not _LIBOBJC */

#endif /* gthr-vxworks.h */
