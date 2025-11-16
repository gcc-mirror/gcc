/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef THREAD_SUPPORT_H
#define THREAD_SUPPORT_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef WIN32
#include <sys/types.h>

typedef pid_t caf_shmem_pid;
typedef int caf_shmem_fd;
#else
#include <handleapi.h>

typedef HANDLE caf_shmem_pid;
typedef HANDLE caf_shmem_fd;
#endif

#if !defined(WIN32) && !defined(__CYGWIN__)
#include <pthread.h>

typedef pthread_mutex_t caf_shmem_mutex;
typedef pthread_cond_t caf_shmem_condvar;

#define CAF_SHMEM_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
#define CAF_SHMEM_COND_INITIALIZER PTHREAD_COND_INITIALIZER

#define thread_support_init_supervisor() (void) 0

#define caf_shmem_mutex_lock pthread_mutex_lock
#define caf_shmem_mutex_trylock pthread_mutex_trylock
#define caf_shmem_mutex_unlock pthread_mutex_unlock

#define caf_shmem_cond_wait pthread_cond_wait
#define caf_shmem_cond_broadcast pthread_cond_broadcast
#define caf_shmem_cond_signal pthread_cond_signal
#define caf_shmem_cond_update_count(c, v) (void) 0

#define thread_support_cleanup() (void) 0
#else
#include <synchapi.h>
#include <stddef.h>

typedef struct caf_shmem_mutex
{
  size_t id;
} caf_shmem_mutex;

#define MAX_NUM_SIGNALED 8

typedef struct caf_shmem_condvar
{
  size_t id;
  volatile int any;
  int size;
  volatile unsigned long signaled[MAX_NUM_SIGNALED];
} caf_shmem_condvar;

#define CAF_SHMEM_MUTEX_INITIALIZER (caf_shmem_mutex){0}
#define CAF_SHMEM_COND_INITIALIZER                                             \
  (caf_shmem_condvar)                                                          \
  {                                                                            \
    0, 0, 0, {}                                                                \
  }

void thread_support_init_supervisor (void);

int caf_shmem_mutex_lock (caf_shmem_mutex *);
int caf_shmem_mutex_trylock (caf_shmem_mutex *);
int caf_shmem_mutex_unlock (caf_shmem_mutex *);

void caf_shmem_cond_wait (caf_shmem_condvar *, caf_shmem_mutex *);
void caf_shmem_cond_broadcast (caf_shmem_condvar *);
void caf_shmem_cond_signal (caf_shmem_condvar *);
void caf_shmem_cond_update_count (caf_shmem_condvar *, int);

void thread_support_cleanup (void);
#endif

/* Support routines to setup pthread structs in shared memory.  */

void initialize_shared_mutex (caf_shmem_mutex *);

void initialize_shared_errorcheck_mutex (caf_shmem_mutex *);

void initialize_shared_condition (caf_shmem_condvar *, const int size);

#endif
