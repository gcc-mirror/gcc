/* libc-internal interface for mutex locks.  LinuxThreads version.
   Copyright (C) 1996 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef _LIBC_LOCK_H
#define _LIBC_LOCK_H 1

#include <pthread.h>
#define __libc_lock_t pthread_mutex_t

/* Define a lock variable NAME with storage class CLASS.  The lock must be
   initialized with __libc_lock_init before it can be used (or define it
   with __libc_lock_define_initialized, below).  Use `extern' for CLASS to
   declare a lock defined in another module.  In public structure
   definitions you must use a pointer to the lock structure (i.e., NAME
   begins with a `*'), because its storage size will not be known outside
   of libc.  */
#define __libc_lock_define(CLASS,NAME) \
  CLASS __libc_lock_t NAME;

/* Define an initialized lock variable NAME with storage class CLASS.  */
#define __libc_lock_define_initialized(CLASS,NAME) \
  CLASS __libc_lock_t NAME = PTHREAD_MUTEX_INITIALIZER;

/* Initialize the named lock variable, leaving it in a consistent, unlocked
   state.  */
#define __libc_lock_init(NAME) \
  (__pthread_mutex_init != NULL ? __pthread_mutex_init (&(NAME), NULL) : 0);

/* Same as last but this time we initialize a recursive mutex.  */
#define __libc_lock_init_recursive(NAME) \
  do {                                                                      \
    if (__pthread_mutex_init != NULL)                                       \
      {                                                                     \
      pthread_mutexattr_t __attr;                                           \
      __pthread_mutexattr_init (&__attr);                                   \
      __pthread_mutexattr_setkind_np (&__attr, PTHREAD_MUTEX_RECURSIVE_NP); \
      __pthread_mutex_init (&(NAME), &__attr);                              \
      __pthread_mutexattr_destroy (&__attr);                                \
      }                                                                             \
  } while (0);

/* Finalize the named lock variable, which must be locked.  It cannot be
   used again until __libc_lock_init is called again on it.  This must be
   called on a lock variable before the containing storage is reused.  */
#define __libc_lock_fini(NAME) \
  (__pthread_mutex_destroy != NULL ? __pthread_mutex_destroy (&(NAME)) : 0);

/* Lock the named lock variable.  */
#define __libc_lock_lock(NAME) \
  (__pthread_mutex_lock != NULL ? __pthread_mutex_lock (&(NAME)) : 0);

/* Try to lock the named lock variable.  */
#define __libc_lock_trylock(NAME) \
  (__pthread_mutex_trylock != NULL ? __pthread_mutex_trylock (&(NAME)) : 0);

/* Unlock the named lock variable.  */
#define __libc_lock_unlock(NAME) \
  (__pthread_mutex_unlock != NULL ? __pthread_mutex_unlock (&(NAME)) : 0);

/* Start critical region with cleanup.  */
#define __libc_cleanup_region_start(FCT, ARG) \
  { struct _pthread_cleanup_buffer _buffer;                                 \
    if (_pthread_cleanup_push_defer != NULL) {                              \
      _pthread_cleanup_push_defer (&_buffer, (FCT), (ARG));                 \
    }

/* End critical region with cleanup.  */
#define __libc_cleanup_region_end(DOIT) \
    if (_pthread_cleanup_push_defer != NULL) {                              \
      _pthread_cleanup_pop_restore (&_buffer, (DOIT));                      \
    }                                                                       \
  }

/* Make the pthread functions weak so that we can elide them from
   single-threaded processes.  */
#pragma weak __pthread_mutex_init
#pragma weak __pthread_mutex_destroy
#pragma weak __pthread_mutex_lock
#pragma weak __pthread_mutex_trylock
#pragma weak __pthread_mutex_unlock
#pragma weak __pthread_mutexattr_init
#pragma weak __pthread_mutexattr_destroy
#pragma weak __pthread_mutexattr_setkind_np
#pragma weak __pthread_key_create
#pragma weak __pthread_setspecific
#pragma weak __pthread_getspecific
#pragma weak __pthread_initialize
#pragma weak _pthread_cleanup_push_defer
#pragma weak _pthread_cleanup_pop_restore

/* We need portable names for some functions.  E.g., when they are
   used as argument to __libc_cleanup_region_start.  */
#define __libc_mutex_unlock __pthread_mutex_unlock

#endif        /* libc-lock.h */

