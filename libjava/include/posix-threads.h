// -*- c++ -*-
// posix-threads.h - Defines for using POSIX threads.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_POSIX_THREADS__
#define __JV_POSIX_THREADS__

// NOTE: This file may only reference those pthread functions which
// are known not to be overridden by the Boehm GC.  If in doubt, scan
// boehm-gc/gc.h.  This is yucky but lets us avoid including gc.h
// everywhere (which would be truly yucky).

#include <pthread.h>
#include <sched.h>

#if defined (HAVE_PTHREAD_MUTEXATTR_SETTYPE) || defined (HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)
#  define HAVE_RECURSIVE_MUTEX 1
#endif


//
// Typedefs.
//

typedef pthread_cond_t _Jv_ConditionVariable_t;

#if defined (PTHREAD_MUTEX_HAVE_M_COUNT) || defined (PTHREAD_MUTEX_HAVE___M_COUNT)

// On Linux we use implementation details of mutexes in order to get
// faster results.
typedef pthread_mutex_t _Jv_Mutex_t;

#else /* LINUX_THREADS */

#define PTHREAD_MUTEX_IS_STRUCT

typedef struct
{
  // Mutex used when locking this structure transiently.
  pthread_mutex_t mutex;
#ifndef HAVE_RECURSIVE_MUTEX
  // Some systems do not have recursive mutexes, so we must simulate
  // them.  Solaris is one such system.

  // Mutex the thread holds the entire time this mutex is held.  This
  // is used to make condition variables work properly.
  pthread_mutex_t mutex2;
  // Condition variable used when waiting for this lock.
  pthread_cond_t cond;
  // Thread holding this mutex.  If COUNT is 0, no thread is holding.
  pthread_t thread;
#endif /* HAVE_RECURSIVE_MUTEX */

  // Number of times mutex is held.  If 0, the lock is not held.  We
  // do this even if we have a native recursive mutex so that we can
  // keep track of whether the lock is held; this lets us do error
  // checking.  FIXME it would be nice to optimize this; on some
  // systems we could do so by relying on implementation details of
  // recursive mutexes.
  int count;
} _Jv_Mutex_t;

#endif

typedef struct
{
  // Flag values are defined in implementation.
  int flags;

  // Actual thread id.
  pthread_t thread;
} _Jv_Thread_t;
typedef void _Jv_ThreadStartFunc (java::lang::Thread *);


// This convenience function is used to return the POSIX mutex
// corresponding to our mutex.
inline pthread_mutex_t *
_Jv_PthreadGetMutex (_Jv_Mutex_t *mu)
{
#if ! defined (PTHREAD_MUTEX_IS_STRUCT)
  return mu;
#elif defined (HAVE_RECURSIVE_MUTEX)
  return &mu->mutex;
#else
  return &mu->mutex2;
#endif
}

#include <stdio.h>

// This is a convenience function used only by the pthreads thread
// implementation.  This is slow, but that's too bad -- we need to do
// the checks for correctness.  It might be nice to be able to compile
// this out.  Returns 0 if the lock is held by the current thread, and
// 1 otherwise.
inline int
_Jv_PthreadCheckMonitor (_Jv_Mutex_t *mu)
{
  pthread_mutex_t *pmu;
#ifdef HAVE_RECURSIVE_MUTEX
  pmu = _Jv_PthreadGetMutex (mu);
  // See if the mutex is locked by this thread.
  if (pthread_mutex_trylock (pmu))
    return 1;

#if defined (PTHREAD_MUTEX_HAVE_M_COUNT)
  // On Linux we exploit knowledge of the implementation.
  int r = pmu->m_count == 1;
#elif defined (PTHREAD_MUTEX_HAVE___M_COUNT)
  // In glibc 2.1, the first time the mutex is grabbed __m_count is
  // set to 0 and __m_owner is set to pthread_self().
  int r = ! pmu->__m_count;
#else
  int r = mu->count == 0;
#endif

#else /* HAVE_RECURSIVE_MUTEX */
  // In this case we must lock our structure and then see if this
  // thread owns the mutex.
  pmu = &mu->mutex;
  if (pthread_mutex_lock (pmu))
    return 1;

  int r = mu->thread != pthread_self () || mu->count == 0;
#endif /* HAVE_RECURSIVE_MUTEX */

  pthread_mutex_unlock (pmu);
  return r;
}

//
// Condition variables.
//

inline void
_Jv_CondInit (_Jv_ConditionVariable_t *cv)
{
  pthread_cond_init (cv, 0);
}

#ifndef LINUX_THREADS

// pthread_cond_destroy does nothing on Linux and it is a win to avoid
// defining this macro.

#define _Jv_HaveCondDestroy

inline void
_Jv_CondDestroy (_Jv_ConditionVariable_t *cv)
{
  pthread_cond_destroy (cv);
}

#endif /* LINUX_THREADS */

int _Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
		  jlong millis, jint nanos);

inline int
_Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu)
{
  return _Jv_PthreadCheckMonitor (mu) || pthread_cond_signal (cv);
}

inline int
_Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu)
{
  return _Jv_PthreadCheckMonitor (mu) || pthread_cond_broadcast (cv);
}


//
// Mutexes.
//

#ifdef RECURSIVE_MUTEX_IS_DEFAULT
inline void
_Jv_MutexInit (_Jv_Mutex_t *mu)
{
  pthread_mutex_init (_Jv_PthreadGetMutex (mu), NULL);
#ifdef PTHREAD_MUTEX_IS_STRUCT
  mu->count = 0;
#endif
}
#else
void _Jv_MutexInit (_Jv_Mutex_t *mu);
#endif

#ifndef LINUX_THREADS

// pthread_mutex_destroy does nothing on Linux and it is a win to avoid
// defining this macro.

#define _Jv_HaveMutexDestroy

#ifdef HAVE_RECURSIVE_MUTEX

inline void
_Jv_MutexDestroy (_Jv_Mutex_t *mu)
{
  pthread_mutex_destroy (_Jv_PthreadGetMutex (mu));
}

#else /* HAVE_RECURSIVE_MUTEX */

extern void _Jv_MutexDestroy (_Jv_Mutex_t *mu);

#endif /* HAVE_RECURSIVE_MUTEX */
#endif /* LINUX_THREADS */

#ifdef HAVE_RECURSIVE_MUTEX

inline int
_Jv_MutexLock (_Jv_Mutex_t *mu)
{
  int r = pthread_mutex_lock (_Jv_PthreadGetMutex (mu));
#ifdef PTHREAD_MUTEX_IS_STRUCT
  if (! r)
    ++mu->count;
#endif
  return r;
}

inline int
_Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  int r = pthread_mutex_unlock (_Jv_PthreadGetMutex (mu));
#ifdef PTHREAD_MUTEX_IS_STRUCT
  if (! r)
    --mu->count;
#endif
  return r;
}

#else /* HAVE_RECURSIVE_MUTEX */

extern int _Jv_MutexLock (_Jv_Mutex_t *mu);
extern int _Jv_MutexUnlock (_Jv_Mutex_t *mu);

#endif /* HAVE_RECURSIVE_MUTEX */


//
// Thread creation and manipulation.
//

void _Jv_InitThreads (void);

void _Jv_ThreadInitData (_Jv_Thread_t **data, java::lang::Thread *thread);

inline java::lang::Thread *
_Jv_ThreadCurrent (void)
{
  extern pthread_key_t _Jv_ThreadKey;
  return (java::lang::Thread *) pthread_getspecific (_Jv_ThreadKey);
}

inline _Jv_Thread_t *
_Jv_ThreadCurrentData (void)
{
  extern pthread_key_t _Jv_ThreadDataKey;
  return (_Jv_Thread_t *) pthread_getspecific (_Jv_ThreadDataKey);
}

inline void
_Jv_ThreadYield (void)
{
#ifdef HAVE_SCHED_YIELD
  sched_yield ();
#endif /* HAVE_SCHED_YIELD */
}

void _Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio);

void _Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		      _Jv_ThreadStartFunc *meth);

void _Jv_ThreadWait (void);

void _Jv_ThreadInterrupt (_Jv_Thread_t *data);

#endif /* __JV_POSIX_THREADS__ */
