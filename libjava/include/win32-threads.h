// -*- c++ -*-
// win32-threads.h - Defines for using Win32 threads.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 Free Software
   Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_WIN32_THREADS__
#define __JV_WIN32_THREADS__

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

//
// Typedefs.
//

typedef struct
{
  // ev[0] (signal) is a Win32 auto-reset event for _Jv_CondNotify
  // ev[1] (broadcast) is a Win32 manual-reset event for _Jv_CondNotifyAll
  HANDLE ev[2];

  // Number of threads waiting on this condition variable
  int blocked_count;

  // Protects access to the blocked_count variable
  CRITICAL_SECTION count_mutex;

} _Jv_ConditionVariable_t;

typedef struct
{
  // The thread-id of the owner thread if any, 0 otherwise
  DWORD owner;

  // Track nested mutex acquisitions by the same thread
  int refcount;

  // The actual Windows construct used to implement this mutex
  CRITICAL_SECTION cs;

} _Jv_Mutex_t;

typedef struct
{
  int flags;            // Flags are defined in implementation.
  HANDLE handle;        // Actual handle to the thread

  // Protects access to the thread's interrupt_flag and
  // interrupt_event variables within this module.
  CRITICAL_SECTION interrupt_mutex;
  
  // A Win32 auto-reset event for thread interruption
  HANDLE interrupt_event;

  java::lang::Thread *thread_obj;
} _Jv_Thread_t;

typedef DWORD _Jv_ThreadId_t;

inline _Jv_ThreadId_t
_Jv_ThreadSelf (void)
{
  return GetCurrentThreadId();
}

typedef void _Jv_ThreadStartFunc (java::lang::Thread *);

//
// Condition variables.
//

#define _Jv_HaveCondDestroy
int _Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu, jlong millis, jint nanos);
void _Jv_CondInit (_Jv_ConditionVariable_t *cv);
void _Jv_CondDestroy (_Jv_ConditionVariable_t *cv);
int _Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *);
int _Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *);

//
// Mutexes.
// We use CRITICAL_SECTIONs instead of CreateMutex() for better performance
//

// Returns 0 if the mutex lock is held by the current thread, and 1 otherwise.
inline int _Jv_MutexCheckMonitor (_Jv_Mutex_t *mu)
{
  return (mu->owner != GetCurrentThreadId ( ));
}

inline void _Jv_MutexInit (_Jv_Mutex_t *mu)
{
  mu->owner = 0UL;
  mu->refcount = 0;
  InitializeCriticalSection (&(mu->cs));
}

#define _Jv_HaveMutexDestroy
inline void _Jv_MutexDestroy (_Jv_Mutex_t *mu)
{
  mu->owner = 0UL;
  mu->refcount = 0;
  DeleteCriticalSection (&(mu->cs));
  mu = NULL;
}

inline int _Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  if (mu->owner == GetCurrentThreadId ( ))
    {
      mu->refcount--;
      if (mu->refcount == 0)
        mu->owner = 0UL;
      LeaveCriticalSection (&(mu->cs));
      return 0;
    }
  else
    return 1;
}

inline int _Jv_MutexLock (_Jv_Mutex_t *mu)
{
  EnterCriticalSection (&(mu->cs));
  mu->owner = GetCurrentThreadId ( );
  mu->refcount++;
  return 0;
}

//
// Thread creation and manipulation.
//

void _Jv_InitThreads (void);
_Jv_Thread_t *_Jv_ThreadInitData (java::lang::Thread *thread);
void _Jv_ThreadDestroyData (_Jv_Thread_t *data);

inline java::lang::Thread* _Jv_ThreadCurrent (void)
{
  extern DWORD _Jv_ThreadKey;
  return (java::lang::Thread *) TlsGetValue(_Jv_ThreadKey);
}

inline _Jv_Thread_t *_Jv_ThreadCurrentData(void)
{
  extern DWORD _Jv_ThreadDataKey;
  return (_Jv_Thread_t *) TlsGetValue(_Jv_ThreadDataKey);
}

inline void _Jv_ThreadYield (void)
{
  Sleep (0);
}

void _Jv_ThreadRegister (_Jv_Thread_t *data);
void _Jv_ThreadUnRegister ();

void _Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio);
void _Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		      _Jv_ThreadStartFunc *meth);
void _Jv_ThreadWait (void);
void _Jv_ThreadInterrupt (_Jv_Thread_t *data);

//
// Thread interruption support
//

// Gets the auto-reset event for the current thread which is
// signalled by _Jv_ThreadInterrupt. The caller can wait on this
// event in addition to other waitable objects.
//
// NOTE: After waiting on this event with WaitForMultipleObjects,
// you should ALWAYS use the return value of WaitForMultipleObjects
// to test whether this event was signalled and whether thread
// interruption has occurred. You should do this instead of checking
// the thread's interrupted_flag, because someone could have reset
// this flag in the interval of time between the return of
// WaitForMultipleObjects and the time you query interrupted_flag.
// See java/lang/natWin32Process.cc (waitFor) for an example.
HANDLE _Jv_Win32GetInterruptEvent (void);

// Remove defines from <windows.h> that conflict with various things in libgcj code

#undef TRUE
#undef FALSE
#undef MAX_PRIORITY
#undef MIN_PRIORITY
#undef min
#undef max
#undef interface
#undef STRICT
#undef VOID

#endif /* __JV_WIN32_THREADS__ */
