// -*- c++ -*-
// win32-threads.h - Defines for using Win32 threads.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_WIN32_THREADS__
#define __JV_WIN32_THREADS__

#include <windows.h>

//
// Typedefs.
//

typedef HANDLE _Jv_ConditionVariable_t;
typedef HANDLE _Jv_Mutex_t;

typedef struct
{
  int flags;            // Flags are defined in implementation.
  HANDLE handle;        // Actual handle to the thread
} _Jv_Thread_t;

typedef void _Jv_ThreadStartFunc (java::lang::Thread *);

//
// Condition variables.
//

inline void
_Jv_CondInit (_Jv_ConditionVariable_t *cv)
{
  *cv = CreateEvent (NULL, 0, 0, NULL);
}

#define _Jv_HaveCondDestroy

inline void
_Jv_CondDestroy (_Jv_ConditionVariable_t *cv)
{
  CloseHandle (*cv);
  cv = NULL;
}

int _Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
		  jlong millis, jint nanos);

inline int
_Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *)
{
  return PulseEvent (*cv) ? 0 : GetLastError ();        // FIXME: Map error code?
}

inline int
_Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *)
{
  return PulseEvent (*cv) ? 0 : GetLastError ();        // FIXME: Map error code?
}

//
// Mutexes.
//

inline void
_Jv_MutexInit (_Jv_Mutex_t *mu)
{
  *mu = CreateMutex (NULL, 0, NULL);
}

#define _Jv_HaveMutexDestroy

inline void
_Jv_MutexDestroy (_Jv_Mutex_t *mu)
{
  CloseHandle (*mu);
  mu = NULL;
}

int _Jv_MutexLock (_Jv_Mutex_t *mu);

inline int
_Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  return ReleaseMutex(*mu) ? 0 : GetLastError();        // FIXME: Map error code?
}

//
// Thread creation and manipulation.
//

void _Jv_InitThreads (void);
void _Jv_ThreadInitData (_Jv_Thread_t **data, java::lang::Thread *thread);

inline java::lang::Thread *
_Jv_ThreadCurrent (void)
{
  extern DWORD _Jv_ThreadKey;
  return (java::lang::Thread *) TlsGetValue(_Jv_ThreadKey);
}

inline _Jv_Thread_t *
_Jv_ThreadCurrentData (void)
{
  extern DWORD _Jv_ThreadDataKey;
  return (_Jv_Thread_t *) TlsGetValue(_Jv_ThreadDataKey);
}

inline void
_Jv_ThreadYield (void)
{
  Sleep (0);
}

void _Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio);
void _Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		      _Jv_ThreadStartFunc *meth);
void _Jv_ThreadWait (void);
void _Jv_ThreadInterrupt (_Jv_Thread_t *data);

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
