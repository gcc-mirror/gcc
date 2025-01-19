/* Threads compatibility routines for libgcc2 and libobjc.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2019-2025 Free Software Foundation, Inc.

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

/* AMD GCN does not support dynamic creation of threads.  There may be many
   hardware threads, but they're all created simultaneously at launch time.

   This implementation is intended to provide mutexes for libgfortran, etc.
   It is not intended to provide a TLS implementation at this time,
   although that may be added later if needed.

   __gthread_active_p returns "1" to ensure that mutexes are used, and that
   programs attempting to use emutls will fail with the appropriate abort.
   It is expected that the TLS tests will fail.  */

#ifndef GCC_GTHR_GCN_H
#define GCC_GTHR_GCN_H

#define __GTHREADS 1

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _LIBOBJC
#error "Objective C is not supported on AMD GCN"
#else

static inline int
__gthread_active_p (void)
{
  return 1;
}

typedef int __gthread_key_t;
typedef int __gthread_once_t;
typedef int __gthread_mutex_t;
typedef int __gthread_recursive_mutex_t;

#define __GTHREAD_ONCE_INIT 0
#define __GTHREAD_MUTEX_INIT 0
#define __GTHREAD_RECURSIVE_MUTEX_INIT 0

static inline int
__gthread_once (__gthread_once_t *__once __attribute__((unused)),
		void (*__func) (void) __attribute__((unused)))
{
  return 0;
}

static inline int
__gthread_key_create (__gthread_key_t *__key __attribute__((unused)),
		      void (*__dtor) (void *) __attribute__((unused)))
{
  /* Operation is not supported.  */
  return -1;
}

static inline int
__gthread_key_delete (__gthread_key_t __key __attribute__ ((__unused__)))
{
  /* Operation is not supported.  */
  return -1;
}

static inline void *
__gthread_getspecific (__gthread_key_t __key __attribute__((unused)))
{
  return 0;
}

static inline int
__gthread_setspecific (__gthread_key_t __key __attribute__((unused)),
		       const void *__ptr __attribute__((unused)))
{
  /* Operation is not supported.  */
  return -1;
}

static inline int
__gthread_mutex_destroy (__gthread_mutex_t *__mutex __attribute__((unused)))
{
  return 0;
}

static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex __attribute__((unused)))
{
  return 0;
}


static inline int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
  while (__sync_lock_test_and_set (__mutex, 1))
    asm volatile ("s_sleep\t1" ::: "memory");

  return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
  return __sync_lock_test_and_set (__mutex, 1);
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
  __sync_lock_release (__mutex);

  return 0;
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex __attribute__((unused)))
{
  /* Operation is not supported.  */
  return -1;
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex __attribute__((unused)))
{
  /* Operation is not supported.  */
  return -1;
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex __attribute__((unused)))
{
  /* Operation is not supported.  */
  return -1;
}
#endif /* _LIBOBJC */

#ifdef __cplusplus
}
#endif

#endif /* ! GCC_GTHR_GCN_H */
