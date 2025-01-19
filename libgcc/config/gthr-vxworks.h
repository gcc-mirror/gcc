/* Threads compatibility routines for libgcc2 and libobjc for VxWorks.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997-2025 Free Software Foundation, Inc.
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

#include <vxWorks.h>
#include <_vxworks-versions.h>

/* Some VxWorks headers profusely use typedefs of a pointer to a function with
   undefined number of arguments.  Arrange to ignore declaration errors in C++,
   which is achievable by ignoring Wstrict-prototypes diagnostics even when the
   option is registered as only valid for c/objc.  */
#pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wpragmas"
  #pragma GCC diagnostic ignored "-Wstrict-prototypes"
  #include <semLib.h>
#pragma GCC diagnostic pop

#include <errnoLib.h>


/* --------------------- Test & Set/Swap internal API --------------------- */

/* We use a bare atomic primitive with busy loops to handle mutual exclusion.
   Inefficient, but reliable.  The actual primitive used depends on the mode
   (RTP vs Kernel) and the version of VxWorks.  We define a macro and a type
   here, for reuse without conditionals cluttering in the code afterwards.  */

/* RTP, pre 6.9.  */

#if defined(__RTP__) && _VXWORKS_PRE(6,9)

#define __TAS(x) vxCas ((x), 0, 1)
typedef volatile unsigned char __vx_tas_t;

#endif

/* RTP, 6.9 and beyond.  */

#if defined(__RTP__) && !_VXWORKS_PRE(6,9)

#define __TAS(x) vxAtomicCas ((x), 0, 1)
typedef atomic_t __vx_tas_t;

/* Our implementation will need the system headers to use the vxAtomic
   primitives.  Other includers won't and could actually be incompatible
   with this inclusion, for instance libstdc++ sources compiled in C++
   98 mode while AtomicLib for C++ requires C++ 11 at least.  */

#if defined(IN_LIBGCC2)
#include <vxAtomicLib.h>
#endif

#endif

/* Kernel */

#if !defined(__RTP__)

#define __TAS(x) vxTas (x)
typedef volatile unsigned char __vx_tas_t;

#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ------------------------ Base __GTHREADS support ----------------------- */

#define __GTHREADS 1
#define __gthread_active_p() 1

/* Mutexes are easy, except that they need to be initialized at runtime.  */

/* All VxWorks mutexes are recursive.  */
typedef SEM_ID __gthread_mutex_t;
typedef SEM_ID __gthread_recursive_mutex_t;
#define __GTHREAD_MUTEX_INIT_FUNCTION __gthread_mutex_init
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION __gthread_recursive_mutex_init

#define __CHECK_RESULT(result) (((result) == OK) ? OK : errnoGet())

/* If a call to the VxWorks API fails, we must propagate the errno value.  */
#define __RETURN_ERRNO_IF_NOT_OK(exp) if ((exp) != OK) return errnoGet()

/* Non re-entrant mutex implementation. Libstdc++ expects the default
   gthread mutex to be non reentrant.  */

static inline void
__gthread_mutex_init (__gthread_mutex_t * __mutex)
{
  if (!__mutex)
    return;
  *__mutex = semBCreate (SEM_Q_PRIORITY, SEM_FULL);
}

static inline int
__gthread_mutex_destroy (__gthread_mutex_t * __mutex)
{
  if (!__mutex)
    return ERROR;
  return __CHECK_RESULT (semDelete (*__mutex));
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t * __mutex)
{
  if (!__mutex)
    return ERROR;
  return __CHECK_RESULT (semTake(*__mutex, WAIT_FOREVER));
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t * __mutex)
{
  if (!__mutex)
    return ERROR;
  return __CHECK_RESULT (semTake (*__mutex, NO_WAIT));
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t * __mutex)
{
  if (!__mutex)
    return ERROR;
  return __CHECK_RESULT (semGive (*__mutex));
}

/* Recursive mutex implementation. The only change is that we use semMCreate()
   instead of semBCreate().  */

static inline void
__gthread_recursive_mutex_init (__gthread_recursive_mutex_t * __mutex)
{
  if (!__mutex)
    return;
  *__mutex =
    semMCreate (SEM_Q_PRIORITY | SEM_INVERSION_SAFE | SEM_DELETE_SAFE);
}

static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t * __mutex)
{
  return __gthread_mutex_destroy (__mutex);
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t * __mutex)
{
  return __gthread_mutex_lock (__mutex);
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t * __mutex)
{
  return __gthread_mutex_trylock (__mutex);
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t * __mutex)
{
  return __gthread_mutex_unlock (__mutex);
}

typedef struct
{
  /* PPC's test-and-set kernel mode implementation requires a pointer aligned
     object, of which it only sets the first byte.  We use padding in addition
     to an alignment request here to maxmise the factors leading to the
     desired actual alignment choice by the compiler.  */
#if defined(__PPC__)
  __attribute ((aligned (__alignof__ (void *))))
#endif

  __vx_tas_t busy;
  volatile unsigned char done;

#if !defined(__RTP__) && defined(__PPC__)
  unsigned char pad1;
  unsigned char pad2;
#endif
#if !defined(__RTP__) && defined(__PPC64__)
  unsigned char pad3;
  unsigned char pad4;
  unsigned char pad5;
  unsigned char pad6;
#endif
} __gthread_once_t;

#define __GTHREAD_ONCE_INIT {}

extern int __gthread_once (__gthread_once_t *__once, void (*__func)(void));

/* All the TSD routines are sufficiently complex that they
   need to be implemented out of line.  */

typedef unsigned int __gthread_key_t;

extern int __gthread_key_create (__gthread_key_t *__keyp,
				 void (*__dtor)(void *));
extern int __gthread_key_delete (__gthread_key_t __key);

extern void *__gthread_getspecific (__gthread_key_t __key);
extern int __gthread_setspecific (__gthread_key_t __key, void *__ptr);

/* ------------------ Base condition variables support ------------------- */

/* VxWorks prio to 6 misses a few services key to a correct
   implementation of condition variables with reasonable complexity.
   semExchange in particular.  */

#if _VXWORKS_MAJOR_GE(6)

#define __GTHREAD_HAS_COND 1

typedef SEM_ID __gthread_cond_t;

#define __GTHREAD_COND_INIT_FUNCTION __gthread_cond_init

/* Condition variable declarations.  */

extern void __gthread_cond_init (__gthread_cond_t *cond);

extern int __gthread_cond_destroy (__gthread_cond_t *cond);

extern int __gthread_cond_broadcast (__gthread_cond_t *cond);

extern int __gthread_cond_wait (__gthread_cond_t *cond,
				__gthread_mutex_t *mutex);

extern int __gthread_cond_wait_recursive (__gthread_cond_t *cond,
					  __gthread_recursive_mutex_t *mutex);

#endif

/* -----------------------  C++0x thread support ------------------------- */

/* We do not support C++0x threads on that VxWorks 653, which we can
   recognize by VTHREADS being defined.  */

#if _VXWORKS_MAJOR_GE(6) && !defined(VTHREADS)

#define __GTHREADS_CXX0X 1

#include <limits.h>
#include <time.h>
#include <tickLib.h>
#include <sysLib.h>
#include <version.h>

typedef struct
{
  TASK_ID task_id;
  void *return_value;

  /* This mutex is used to block in join() while the return value is
     unavailable.  */
  __gthread_mutex_t return_value_available;

  /* Before freeing the structure in the task wrapper, we need to wait until
     join() or detach() are called on that thread.   */
  __gthread_mutex_t delete_ok;
} __gthread_tcb;

typedef __gthread_tcb *__gthread_t;

/* Typedefs specific to different vxworks versions.  */
#if _VXWORKS_PRE(6,9)
  typedef int _Vx_usr_arg_t;
  #define TASK_ID_NULL ((TASK_ID)NULL)
  #define SEM_ID_NULL ((SEM_ID)NULL)
#endif

typedef struct timespec __gthread_time_t;

/* Timed mutex lock declarations.  */

extern int __gthread_mutex_timedlock (__gthread_mutex_t *m,
				      const __gthread_time_t *abs_time);

extern int __gthread_recursive_mutex_timedlock
  (__gthread_recursive_mutex_t *mutex,
   const __gthread_time_t *abs_timeout);

/* Timed condition variable declarations.  */

extern int __gthread_cond_signal (__gthread_cond_t *cond);
extern int __gthread_cond_timedwait (__gthread_cond_t *cond,
				     __gthread_mutex_t *mutex,
				     const __gthread_time_t *abs_timeout);

/* gthreads declarations.  */

extern int __gthread_equal (__gthread_t t1, __gthread_t t2);
extern int __gthread_yield (void);
extern int __gthread_create (__gthread_t *__threadid,
			     void *(*__func) (void*),
			     void *__args);
extern int __gthread_join (__gthread_t thread, void **value_ptr);
extern int __gthread_detach (__gthread_t thread);

extern __gthread_t __gthread_self (void);

#endif /* _VXWORKS_MAJOR_GE(6) && !defined(VTHREADS) */

#ifdef __cplusplus
}
#endif

#endif /* not _LIBOBJC */

#endif /* gthr-vxworks.h */
