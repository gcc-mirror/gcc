// -*- c++ -*-
// posix-threads.h - Defines for using POSIX threads.

/* Copyright (C) 1998, 1999, 2001  Free Software Foundation

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

//
// Typedefs.
//

typedef struct _Jv_Thread_t
{
  // Flag values are defined in implementation.
  int flags;

  // Actual thread id.
  pthread_t thread;
  
  // Java Thread object.
  java::lang::Thread *thread_obj;
  
  // Condition variable and corresponding mutex, used to implement the
  // interruptable wait/notify mechanism.
  pthread_cond_t wait_cond;
  pthread_mutex_t wait_mutex;

  // Next thread for Condition Variable wait-list chain.
  _Jv_Thread_t *next;
  
} _Jv_Thread_t;

typedef void _Jv_ThreadStartFunc (java::lang::Thread *);


// Condition Variables used to implement wait/notify/sleep/interrupt.
typedef struct
{
  // Linked list of Threads that are waiting to be notified.
  _Jv_Thread_t *first;

} _Jv_ConditionVariable_t;

typedef struct
{
  // For compatibility, simplicity, and correctness, we do not use the native
  // pthreads recursive mutex implementation, but simulate them instead.

  // Mutex the thread holds the entire time this mutex is held. 
  pthread_mutex_t mutex;

  // Thread holding this mutex.
  pthread_t owner;

  // Number of times mutex is held (lock depth).  If 0, the lock is not held.
  int count;
} _Jv_Mutex_t;

// This is a convenience function used only by the pthreads thread
// implementation.  This is slow, but that's too bad -- we need to do
// the checks for correctness.  It might be nice to be able to compile
// this out.  Returns 0 if the lock is held by the current thread, and
// 1 otherwise.
inline int
_Jv_PthreadCheckMonitor (_Jv_Mutex_t *mu)
{
  pthread_t self = pthread_self();
  if (mu->owner == self)
    return 0;
  else return 1;
}

//
// Condition variables.
//

int _Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
		  jlong millis, jint nanos);
		  
int _Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu);

int _Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu);

inline void
_Jv_CondInit (_Jv_ConditionVariable_t *cv)
{
  cv->first = 0;
}

//
// Mutexes.
//

#ifdef LOCK_DEBUG
# include <stdio.h>
#endif

inline void
_Jv_MutexInit (_Jv_Mutex_t *mu)
{
# ifdef LOCK_DEBUG /* Assumes Linuxthreads */
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  pthread_mutex_init (&mu->mutex, &attr);
# else
  pthread_mutex_init (&mu->mutex, 0);
# endif

  mu->count = 0;
  mu->owner = 0;
}

inline int
_Jv_MutexLock (_Jv_Mutex_t *mu)
{
  pthread_t self = pthread_self ();
  if (mu->owner == self)
    {
      mu->count++;
    }
  else
    {
#     ifdef LOCK_DEBUG
	int result = pthread_mutex_lock (&mu->mutex);
	if (0 != result)
	  {
	    fprintf(stderr, "Pthread_mutex_lock returned %d\n", result);
	    for (;;) {}
	  }
#     else
        pthread_mutex_lock (&mu->mutex);
#     endif
      mu->count = 1;
      mu->owner = self;
    }
  return 0;
}

inline int
_Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  if (_Jv_PthreadCheckMonitor (mu))
    {
#     ifdef LOCK_DEBUG
	fprintf(stderr, "_Jv_MutexUnlock: Not owner\n");
	for (;;) {}
#     endif
      return 1;
    }
    
  mu->count--;

  if (mu->count == 0)
    {
      mu->owner = 0;
#     ifdef LOCK_DEBUG
	int result = pthread_mutex_unlock (&mu->mutex);
	if (0 != result)
	  {
	    fprintf(stderr, "Pthread_mutex_unlock returned %d\n", result);
	    for (;;) {}
	  }
#     else
        pthread_mutex_unlock (&mu->mutex);
#     endif
    }
  return 0;
}

#ifndef LINUX_THREADS

// pthread_mutex_destroy does nothing on Linux and it is a win to avoid
// defining this macro.

#define _Jv_HaveMutexDestroy

inline void 
_Jv_MutexDestroy (_Jv_Mutex_t *mu)
{
  pthread_mutex_destroy (&mu->mutex);
}

#endif /* LINUX_THREADS */

//
// Thread creation and manipulation.
//

void _Jv_InitThreads (void);

_Jv_Thread_t *_Jv_ThreadInitData (java::lang::Thread *thread);
void _Jv_ThreadDestroyData (_Jv_Thread_t *data);

inline java::lang::Thread *
_Jv_ThreadCurrent (void)
{
  extern pthread_key_t _Jv_ThreadKey;
  return (java::lang::Thread *) pthread_getspecific (_Jv_ThreadKey);
}

#ifdef JV_HASH_SYNCHRONIZATION
// Should be specialized to just load the "current thread" register
// on platforms that support it.   Speed is of the essence.  The value
// of the descriptor is not, so long as there is a one-to-one correspondence
// to threads.


#ifdef __ia64__

typedef size_t _Jv_ThreadId_t;

register size_t _Jv_self __asm__("r13");
	// For linux_threads this is really a pointer to its thread data
	// structure.  We treat it as opaque.  That should also work
	// on other operating systems that follow the ABI standard.

// This should become the prototype for machines that maintain a thread
// pointer in a register.
inline _Jv_ThreadId_t
_Jv_ThreadSelf (void)
{
  return _Jv_self;
}

#define JV_SELF_DEFINED

#endif /* __ia64__ */

#ifdef __alpha__

#ifdef __FreeBSD__
#include <machine/pal.h>
#define PAL_rduniq PAL_rdunique
#else
#include <asm/pal.h>
#endif

typedef unsigned long _Jv_ThreadId_t;

inline _Jv_ThreadId_t
_Jv_ThreadSelf (void)
{
  unsigned long id;
  __asm__ ("call_pal %1\n\tmov $0, %0" : "=r"(id) : "i"(PAL_rduniq) : "$0");
  return id;
}

#define JV_SELF_DEFINED

#endif /* __alpha__ */

#if defined(SLOW_PTHREAD_SELF)

#include "sysdep/locks.h"

typedef pthread_t _Jv_ThreadId_t;

// E.g. on X86 Linux, pthread_self() is too slow for our purpose.
// Instead we maintain a cache based on the current sp value.
// This is similar to what's done for thread local allocation in the
// GC, only far simpler.
// This code should probably go away when Linux/X86 starts using a
// segment register to hold the thread id.
# define LOG_THREAD_SPACING 12
			// If two thread pointer values are closer than
			// 1 << LOG_THREAD_SPACING, we assume they belong
			// to the same thread.
# define SELF_CACHE_SIZE 1024
# define SC_INDEX(sp) (((unsigned long)(sp) >> 19) & (SELF_CACHE_SIZE-1))
		        // Mapping from sp value to cache index.
			// Note that this is not in any real sense a hash
			// function, since we need to be able to clear
			// all possibly matching slots on thread startup.
			// Thus all entries that might correspond to
			// a given thread are intentionally contiguous.
			// Works well with anything that allocates at least
			// 512KB stacks.
# define SC_CLEAR_MIN (-16)	// When starting a new thread, we clear
# define SC_CLEAR_MAX 0		// all self cache entries between
				// SC_INDEX(sp)+SC_CLEAR_MIN and
				// SC_INDEX(sp)+SC_CLEAR_MAX to ensure
				// we never see stale values.  The
				// current values assume a downward
				// growing stack of size <= 7.5 MB.
# define BAD_HIGH_SP_VALUE ((size_t)(-1))

extern volatile
struct self_cache_entry {
  size_t high_sp_bits;	// sp value >> LOG_THREAD_SPACING
  pthread_t self;	// Corresponding thread
} _Jv_self_cache[];

void _Jv_Self_Cache_Init();

_Jv_ThreadId_t
_Jv_ThreadSelf_out_of_line(volatile self_cache_entry *sce,
			   size_t high_sp_bits);
  
inline _Jv_ThreadId_t
_Jv_ThreadSelf (void)
{
  int dummy;
  size_t sp = (size_t)(&dummy);
  unsigned h = SC_INDEX(sp);
  volatile self_cache_entry *sce = _Jv_self_cache + h;
  pthread_t candidate_self = sce -> self;  // Read must precede following one.
  read_barrier();
  if (sce -> high_sp_bits == sp >> LOG_THREAD_SPACING)
    {
      // The sce -> self value we read must be valid.  An intervening
      // cache replacement by another thread would have first replaced
      // high_sp_bits by something else, and it can't possibly change
      // back without our intervention.
      return candidate_self;
    }
  else
    return _Jv_ThreadSelf_out_of_line(sce, sp >> LOG_THREAD_SPACING);
}

#define JV_SELF_DEFINED

#endif /* SLOW_PTHREAD_SELF */

#ifndef JV_SELF_DEFINED /* If all else fails, call pthread_self directly */

typedef pthread_t _Jv_ThreadId_t;

inline _Jv_ThreadId_t
_Jv_ThreadSelf (void)
{
  return pthread_self();
}

#endif /* !JV_SELF_DEFINED */

#endif /* JV_HASH_SYNCHRONIZATION */

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

void _Jv_ThreadRegister (_Jv_Thread_t *data);
void _Jv_ThreadUnRegister ();

void _Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio);

void _Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		      _Jv_ThreadStartFunc *meth);

void _Jv_ThreadWait (void);

void _Jv_ThreadInterrupt (_Jv_Thread_t *data);

#endif /* __JV_POSIX_THREADS__ */
