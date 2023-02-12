/* Threads compatibility routines for libgcc2 and libobjc.  */
/* Compile this one with gcc.  */

/* Copyright (C) 1999-2023 Free Software Foundation, Inc.
   Contributed by Mumit Khan <khan@xraylith.wisc.edu>.

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

#ifndef GCC_GTHR_WIN32_H
#define GCC_GTHR_WIN32_H

/* So we can test Windows version numbers.  */
#include <stdlib.h>

/* The Windows threading model does not map well into the POSIX inspired
   GCC threading model, so there are caveats one needs to be aware of.

   1. The destructor supplied to __gthread_key_create is ignored for
      generic Windows ports.  This will certainly cause memory leaks
      due to unreclaimed EH contexts (sizeof (eh_context) is at least
      24 bytes for x86 currently).

      This memory leak may be significant for long-running applications
      that make heavy use of C++ EH.

      However, Mingw runtime (version 0.3 or newer) provides a mechanism
      to emulate pthreads key dtors; the runtime provides a special DLL,
      linked in if -mthreads option is specified, that runs the dtors in
      the reverse order of registration when each thread exits. If
      -mthreads option is not given, a stub is linked in instead of the
      DLL, which results in memory leak.  Other Windows ports can use
      the same technique of course to avoid the leak.

   2. The error codes returned are non-POSIX like, and cast into ints.
      This may cause incorrect error return due to truncation values on
      hw where sizeof (DWORD) > sizeof (int).

   3. POSIX-like condition variables are supported, but only on Vista and
      Server 2008 or later versions.

   4. Timed lock primitives are not supported.  */

#define __GTHREADS 1

/* Condition variables are supported on Vista and Server 2008 or later.  */
#if _WIN32_WINNT >= 0x0600
#define __GTHREAD_HAS_COND 1
#define __GTHREADS_CXX0X 1
#endif

#if _GTHREAD_USE_MUTEX_TIMEDLOCK
#error Timed lock primitives are not supported on Windows targets
#endif

/* Make sure CONST_CAST2 (origin in system.h) is declared.  */
#ifndef CONST_CAST2
#ifdef __cplusplus
#define CONST_CAST2(TOTYPE,FROMTYPE,X) (const_cast<TOTYPE> (X))
#else
#define CONST_CAST2(TOTYPE,FROMTYPE,X) ((__extension__(union {FROMTYPE _q; TOTYPE _nq;})(X))._nq)
#endif
#endif

#ifndef ATTRIBUTE_UNUSED
#define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif

#ifdef _LIBOBJC

/* This is necessary to prevent windef.h (included from windows.h) from
   defining its own BOOL as a typedef.  */
#ifndef __OBJC__
#define __OBJC__
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
/* Now undef the windows BOOL and CC_NONE */
#undef BOOL
#undef CC_NONE

/* Key structure for maintaining thread specific storage */
static DWORD __gthread_objc_data_tls = TLS_OUT_OF_INDEXES;

/* Backend initialization functions */

/* Initialize the threads subsystem.  */
int
__gthread_objc_init_thread_system (void)
{
  /* Initialize the thread storage key.  */
  if ((__gthread_objc_data_tls = TlsAlloc ()) != TLS_OUT_OF_INDEXES)
    return 0;
  else
    return -1;
}

/* Close the threads subsystem.  */
int
__gthread_objc_close_thread_system (void)
{
  if (__gthread_objc_data_tls != TLS_OUT_OF_INDEXES)
    TlsFree (__gthread_objc_data_tls);
  return 0;
}

/* Backend thread functions */

/* Create a new thread of execution.  */
objc_thread_t
__gthread_objc_thread_detach (void (*func)(void *arg), void *arg)
{
  DWORD	thread_id = 0;
  HANDLE win32_handle;

  if (!(win32_handle = CreateThread (NULL, 0, (LPTHREAD_START_ROUTINE) func,
				     arg, 0, &thread_id)))
    thread_id = 0;

  return (objc_thread_t) (INT_PTR) thread_id;
}

/* Set the current thread's priority.  */
int
__gthread_objc_thread_set_priority (int priority)
{
  int sys_priority = 0;

  switch (priority)
    {
    case OBJC_THREAD_INTERACTIVE_PRIORITY:
      sys_priority = THREAD_PRIORITY_NORMAL;
      break;
    default:
    case OBJC_THREAD_BACKGROUND_PRIORITY:
      sys_priority = THREAD_PRIORITY_BELOW_NORMAL;
      break;
    case OBJC_THREAD_LOW_PRIORITY:
      sys_priority = THREAD_PRIORITY_LOWEST;
      break;
    }

  /* Change priority */
  if (SetThreadPriority (GetCurrentThread (), sys_priority))
    return 0;
  else
    return -1;
}

/* Return the current thread's priority.  */
int
__gthread_objc_thread_get_priority (void)
{
  int sys_priority;

  sys_priority = GetThreadPriority (GetCurrentThread ());

  switch (sys_priority)
    {
    case THREAD_PRIORITY_HIGHEST:
    case THREAD_PRIORITY_TIME_CRITICAL:
    case THREAD_PRIORITY_ABOVE_NORMAL:
    case THREAD_PRIORITY_NORMAL:
      return OBJC_THREAD_INTERACTIVE_PRIORITY;

    default:
    case THREAD_PRIORITY_BELOW_NORMAL:
      return OBJC_THREAD_BACKGROUND_PRIORITY;

    case THREAD_PRIORITY_IDLE:
    case THREAD_PRIORITY_LOWEST:
      return OBJC_THREAD_LOW_PRIORITY;
    }

  /* Couldn't get priority.  */
  return -1;
}

/* Yield our process time to another thread.  */
void
__gthread_objc_thread_yield (void)
{
  Sleep (0);
}

/* Terminate the current thread.  */
int
__gthread_objc_thread_exit (void)
{
  /* exit the thread */
  ExitThread (__objc_thread_exit_status);

  /* Failed if we reached here */
  return -1;
}

/* Returns an integer value which uniquely describes a thread.  */
objc_thread_t
__gthread_objc_thread_id (void)
{
  return (objc_thread_t) (INT_PTR) GetCurrentThreadId ();
}

/* Sets the thread's local storage pointer.  */
int
__gthread_objc_thread_set_data (void *value)
{
  if (TlsSetValue (__gthread_objc_data_tls, value))
    return 0;
  else
    return -1;
}

/* Returns the thread's local storage pointer.  */
void *
__gthread_objc_thread_get_data (void)
{
  DWORD lasterror = GetLastError ();
  void * ptr = TlsGetValue (__gthread_objc_data_tls);
  SetLastError (lasterror);
  return ptr;
}

/* Backend mutex functions */

/* Allocate a mutex.  */
int
__gthread_objc_mutex_allocate (objc_mutex_t mutex)
{
  if ((mutex->backend = (void *) CreateMutex (NULL, 0, NULL)) == NULL)
    return -1;
  else
    return 0;
}

/* Deallocate a mutex.  */
int
__gthread_objc_mutex_deallocate (objc_mutex_t mutex)
{
  CloseHandle ((HANDLE) (mutex->backend));
  return 0;
}

/* Grab a lock on a mutex.  */
int
__gthread_objc_mutex_lock (objc_mutex_t mutex)
{
  int status;

  status = WaitForSingleObject ((HANDLE) (mutex->backend), INFINITE);
  if (status != WAIT_OBJECT_0 && status != WAIT_ABANDONED)
    return -1;
  else
    return 0;
}

/* Try to grab a lock on a mutex.  */
int
__gthread_objc_mutex_trylock (objc_mutex_t mutex)
{
  int status;

  status = WaitForSingleObject ((HANDLE) (mutex->backend), 0);
  if (status != WAIT_OBJECT_0 && status != WAIT_ABANDONED)
    return -1;
  else
    return 0;
}

/* Unlock the mutex */
int
__gthread_objc_mutex_unlock (objc_mutex_t mutex)
{
  if (ReleaseMutex ((HANDLE) (mutex->backend)) == 0)
    return -1;
  else
    return 0;
}

/* Backend condition mutex functions */

/* Allocate a condition.  */
int
__gthread_objc_condition_allocate (objc_condition_t condition ATTRIBUTE_UNUSED)
{
  /* Unimplemented.  */
  return -1;
}

/* Deallocate a condition.  */
int
__gthread_objc_condition_deallocate (objc_condition_t condition ATTRIBUTE_UNUSED)
{
  /* Unimplemented.  */
  return -1;
}

/* Wait on the condition */
int
__gthread_objc_condition_wait (objc_condition_t condition ATTRIBUTE_UNUSED,
			       objc_mutex_t mutex ATTRIBUTE_UNUSED)
{
  /* Unimplemented.  */
  return -1;
}

/* Wake up all threads waiting on this condition.  */
int
__gthread_objc_condition_broadcast (objc_condition_t condition ATTRIBUTE_UNUSED)
{
  /* Unimplemented.  */
  return -1;
}

/* Wake up one thread waiting on this condition.  */
int
__gthread_objc_condition_signal (objc_condition_t condition ATTRIBUTE_UNUSED)
{
  /* Unimplemented.  */
  return -1;
}

#else /* _LIBOBJC */

/* For struct timespec.  Do not include <sys/time.h> here since Gnulib provides
   its own version which drags the Win32 API definitions.  */
#include <sys/timeb.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned int __gthr_win32_DWORD;
typedef void *__gthr_win32_HANDLE;

typedef struct {
  void *DebugInfo;
  int LockCount;
  int RecursionCount;
  __gthr_win32_HANDLE OwningThread;
  __gthr_win32_HANDLE LockSemaphore;
  void *SpinCount;
} __gthr_win32_CRITICAL_SECTION;

typedef struct {
  void *Ptr;
} __gthr_win32_CONDITION_VARIABLE;

typedef __gthr_win32_HANDLE __gthread_t;
typedef __gthr_win32_DWORD __gthread_key_t;
typedef struct { int done; long started; } __gthread_once_t;
typedef __gthr_win32_CRITICAL_SECTION __gthread_mutex_t;
typedef __gthr_win32_CRITICAL_SECTION __gthread_recursive_mutex_t;
#if __GTHREAD_HAS_COND
typedef __gthr_win32_CONDITION_VARIABLE __gthread_cond_t;
#endif
typedef struct timespec __gthread_time_t;

#define __GTHREAD_ONCE_INIT {0, -1}
#define __GTHREAD_MUTEX_INIT_FUNCTION __gthread_mutex_init_function
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION \
  __gthread_recursive_mutex_init_function
#define __GTHREAD_COND_INIT_FUNCTION __gthread_cond_init_function
#define __GTHREAD_TIME_INIT {0, 0}

// Libstdc++ std::basic_filebuf needs the old definition of __gthread_mutex_t
// for layout purposes, but doesn't actually use it.
typedef struct {
  long __unused1;
  void *__unused2;
} __gthr_win32_legacy_mutex_t;
#define __GTHREAD_LEGACY_MUTEX_T __gthr_win32_legacy_mutex_t

#if defined (_WIN32) && !defined(__CYGWIN__)
#define MINGW32_SUPPORTS_MT_EH 1
/* Mingw runtime >= v0.3 provides a magic variable that is set to nonzero
   if -mthreads option was specified, or 0 otherwise. This is to get around
   the lack of weak symbols in PE-COFF.  */
extern int _CRT_MT;
extern int __mingwthr_key_dtor (unsigned long, void (*) (void *));
#endif /* _WIN32 && !__CYGWIN__ */

/* __GTHR_W32_InterlockedCompareExchange is left over from win95,
   which did not support InterlockedCompareExchange. */
#define __GTHR_W32_InterlockedCompareExchange InterlockedCompareExchange

static inline int
__gthread_active_p (void)
{
#ifdef MINGW32_SUPPORTS_MT_EH
  return _CRT_MT;
#else
  return 1;
#endif
}

extern int __gthr_win32_create (__gthread_t *, void *(*) (void*), void *);
extern int __gthr_win32_join (__gthread_t, void **);
extern __gthread_t __gthr_win32_self (void);
extern int __gthr_win32_once (__gthread_once_t *, void (*) (void));
extern int __gthr_win32_detach (__gthread_t);
extern int __gthr_win32_equal (__gthread_t, __gthread_t);
extern int __gthr_win32_yield (void);
extern int __gthr_win32_key_create (__gthread_key_t *, void (*) (void*));
extern int __gthr_win32_key_delete (__gthread_key_t);
extern void * __gthr_win32_getspecific (__gthread_key_t);
extern int __gthr_win32_setspecific (__gthread_key_t, const void *);
extern void __gthr_win32_mutex_init_function (__gthread_mutex_t *);
extern void __gthr_win32_mutex_destroy (__gthread_mutex_t *);
extern int __gthr_win32_mutex_lock (__gthread_mutex_t *);
extern int __gthr_win32_mutex_trylock (__gthread_mutex_t *);
extern int __gthr_win32_mutex_unlock (__gthread_mutex_t *);
extern int __gthr_win32_recursive_mutex_trylock (__gthread_recursive_mutex_t *);
#if __GTHREAD_HAS_COND
extern void __gthr_win32_cond_init_function (__gthread_cond_t *);
extern int __gthr_win32_cond_broadcast (__gthread_cond_t *);
extern int __gthr_win32_cond_signal (__gthread_cond_t *);
extern int __gthr_win32_cond_wait (__gthread_cond_t *, __gthread_mutex_t *);
extern int __gthr_win32_cond_timedwait (__gthread_cond_t *, __gthread_mutex_t *,
					const __gthread_time_t *);
#endif

static inline int
__gthread_create (__gthread_t *__thr, void *(*__func) (void*),
		  void *__args)
{
  return __gthr_win32_create (__thr, __func, __args);
}

static inline int
__gthread_join (__gthread_t __thr, void **__value_ptr)
{
  return __gthr_win32_join (__thr, __value_ptr);
}

static inline __gthread_t
__gthread_self (void)
{
  return __gthr_win32_self ();
}

#if __GTHREAD_HIDE_WIN32API

/* The implementations are in config/i386/gthr-win32.c in libgcc.a.
   Only stubs are exposed to avoid polluting the C++ namespace with
   Win32 API definitions.  */

static inline int
__gthread_detach (__gthread_t __thr)
{
  return __gthr_win32_detach (__thr);
}

static inline int
__gthread_equal (__gthread_t __thr1, __gthread_t __thr2)
{
  return __gthr_win32_equal (__thr1, __thr2);
}

static inline int
__gthread_yield (void)
{
  return __gthr_win32_yield ();
}

static inline int
__gthread_once (__gthread_once_t *__once, void (*__func) (void))
{
  if (__gthread_active_p ())
    return __gthr_win32_once (__once, __func);
  else
    return -1;
}

static inline int
__gthread_key_create (__gthread_key_t *__key, void (*__dtor) (void *))
{
  return __gthr_win32_key_create (__key, __dtor);
}

static inline int
__gthread_key_delete (__gthread_key_t __key)
{
  return __gthr_win32_key_delete (__key);
}

static inline void *
__gthread_getspecific (__gthread_key_t __key)
{
  return __gthr_win32_getspecific (__key);
}

static inline int
__gthread_setspecific (__gthread_key_t __key, const void *__ptr)
{
  return __gthr_win32_setspecific (__key, __ptr);
}

static inline void
__gthread_mutex_init_function (__gthread_mutex_t *__mutex)
{
  __gthr_win32_mutex_init_function (__mutex);
}

static inline void
__gthread_mutex_destroy (__gthread_mutex_t *__mutex)
{
  __gthr_win32_mutex_destroy (__mutex);
}

static inline int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthr_win32_mutex_lock (__mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthr_win32_mutex_trylock (__mutex);
  else
    return 0;
}

static inline int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthr_win32_mutex_unlock (__mutex);
  else
    return 0;
}

static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
  if (__gthread_active_p ())
    return __gthr_win32_recursive_mutex_trylock (__mutex);
  else
    return 0;
}

#if __GTHREAD_HAS_COND

static inline void
__gthread_cond_init_function (__gthread_cond_t *__cond)
{
  __gthr_win32_cond_init_function (__cond);
}

static inline int
__gthread_cond_broadcast (__gthread_cond_t *__cond)
{
  return __gthr_win32_cond_broadcast (__cond);
}

static inline int
__gthread_cond_signal (__gthread_cond_t *__cond)
{
  return __gthr_win32_cond_signal (__cond);
}

static inline int
__gthread_cond_wait (__gthread_cond_t *__cond, __gthread_mutex_t *__mutex)
{
  return __gthr_win32_cond_wait (__cond, __mutex);
}

static inline int
__gthread_cond_timedwait (__gthread_cond_t *__cond, __gthread_mutex_t *__mutex,
			  const __gthread_time_t *__abs_time)
{
  return __gthr_win32_cond_timedwait (__cond, __mutex, __abs_time);
}

#endif /* __GTHREAD_HAS_COND */

#else /* ! __GTHREAD_HIDE_WIN32API */

#ifndef __GTHREAD_WIN32_INLINE
#define __GTHREAD_WIN32_INLINE static inline
#endif

#ifndef __GTHREAD_WIN32_COND_INLINE
#define __GTHREAD_WIN32_COND_INLINE static inline
#endif

#ifndef __GTHREAD_WIN32_ACTIVE_P
#define __GTHREAD_WIN32_ACTIVE_P __gthread_active_p
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef CC_NONE

__GTHREAD_WIN32_INLINE int
__gthread_detach (__gthread_t __thr)
{
  CloseHandle ((HANDLE) __thr);
  return 0;
}

__GTHREAD_WIN32_INLINE int
__gthread_equal (__gthread_t __t1, __gthread_t __t2)
{
  return GetThreadId ((HANDLE) __t1) == GetThreadId ((HANDLE) __t2);
}

__GTHREAD_WIN32_INLINE int
__gthread_yield (void)
{
  Sleep (0);
  return 0;
}

__GTHREAD_WIN32_INLINE int
__gthread_once (__gthread_once_t *__once, void (*__func) (void))
{
  if (!__GTHREAD_WIN32_ACTIVE_P ())
    return -1;

  if (__builtin_expect (!__once->done, 0))
    {
      /* We rely on the memory model of the x86 architecture where every load
	 has acquire semantics and every store has release semantics.  */
      if (__atomic_add_fetch (&__once->started, 1, __ATOMIC_ACQ_REL) == 0)
	{
	  (*__func) ();
	  __once->done = 1;
	}
      else
	{
	  /* Another thread is currently executing the code, so wait for it
	     to finish and yield the CPU in the meantime.  If performance
	     does become an issue, the solution is to use an Event that
	     we wait on here (and set above), but that implies a place to
	     create the event before this routine is called.  */
	  while (!__once->done)
	    __gthread_yield ();
	}
    }

  return 0;
}

/* Windows thread local keys don't support destructors; this leads to
   leaks, especially in threaded applications making extensive use of
   C++ EH. Mingw uses a thread-support DLL to work-around this problem.  */
__GTHREAD_WIN32_INLINE int
__gthread_key_create (__gthread_key_t *__key,
		      void (*__dtor) (void *) ATTRIBUTE_UNUSED)
{
  DWORD __tls_index = TlsAlloc ();
  if (__tls_index != TLS_OUT_OF_INDEXES)
    {
      *__key = __tls_index;
#ifdef MINGW32_SUPPORTS_MT_EH
      /* Mingw runtime will run the dtors in reverse order for each thread
         when the thread exits.  */
      return __mingwthr_key_dtor (*__key, __dtor);
#else
      return 0;
#endif
    }
  else
    return (int) GetLastError ();
}

__GTHREAD_WIN32_INLINE int
__gthread_key_delete (__gthread_key_t __key)
{
  if (TlsFree (__key))
    return 0;
  else
    return (int) GetLastError ();
}

__GTHREAD_WIN32_INLINE void *
__gthread_getspecific (__gthread_key_t __key)
{
  DWORD __lasterror = GetLastError ();
  void *__ptr = TlsGetValue (__key);
  SetLastError (__lasterror);
  return __ptr;
}

__GTHREAD_WIN32_INLINE int
__gthread_setspecific (__gthread_key_t __key, const void *__ptr)
{
  if (TlsSetValue (__key, CONST_CAST2(void *, const void *, __ptr)))
    return 0;
  else
    return (int) GetLastError ();
}

__GTHREAD_WIN32_INLINE void
__gthread_mutex_init_function (__gthread_mutex_t *__mutex)
{
  InitializeCriticalSection ((LPCRITICAL_SECTION) __mutex);
}

__GTHREAD_WIN32_INLINE void
__gthread_mutex_destroy (__gthread_mutex_t *__mutex)
{
  DeleteCriticalSection ((LPCRITICAL_SECTION) __mutex);
}

__GTHREAD_WIN32_INLINE int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
  if (__GTHREAD_WIN32_ACTIVE_P ())
    EnterCriticalSection ((LPCRITICAL_SECTION) __mutex);
  return 0;
}

__GTHREAD_WIN32_INLINE int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
  if (__GTHREAD_WIN32_ACTIVE_P ())
    {
      BOOL __ret = TryEnterCriticalSection ((LPCRITICAL_SECTION) __mutex);
      if (__ret)
	{
	  if (__mutex->RecursionCount > 1)
	    {
	      LeaveCriticalSection ((LPCRITICAL_SECTION) __mutex);
	      return 1;
	    }
	  else
	    return 0;
	}
      else
	return 1;
    }
  else
    return 0;
}

__GTHREAD_WIN32_INLINE int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
  if (__GTHREAD_WIN32_ACTIVE_P ())
    LeaveCriticalSection ((LPCRITICAL_SECTION) __mutex);
  return 0;
}

__GTHREAD_WIN32_INLINE int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
  if (__GTHREAD_WIN32_ACTIVE_P ())
    return TryEnterCriticalSection ((LPCRITICAL_SECTION) __mutex) ? 0 : 1;
  else
    return 0;
}

#if __GTHREAD_HAS_COND

__GTHREAD_WIN32_COND_INLINE void
__gthread_cond_init_function (__gthread_cond_t *__cond)
{
  InitializeConditionVariable ((PCONDITION_VARIABLE) __cond);
}

__GTHREAD_WIN32_COND_INLINE int
__gthread_cond_broadcast (__gthread_cond_t *__cond)
{
  WakeAllConditionVariable ((PCONDITION_VARIABLE) __cond);
  return 0;
}

__GTHREAD_WIN32_COND_INLINE int
__gthread_cond_signal (__gthread_cond_t *__cond)
{
  WakeConditionVariable ((PCONDITION_VARIABLE) __cond);
  return 0;
}

__GTHREAD_WIN32_COND_INLINE int
__gthread_cond_wait (__gthread_cond_t *__cond, __gthread_mutex_t *__mutex)
{
  if (SleepConditionVariableCS ((PCONDITION_VARIABLE) __cond,
				(PCRITICAL_SECTION) __mutex,
				INFINITE))
    return 0;
  else
    return (int) GetLastError ();
}

extern DWORD __gthr_win32_abs_to_rel_time (const __gthread_time_t *);

__GTHREAD_WIN32_COND_INLINE int
__gthread_cond_timedwait (__gthread_cond_t *__cond,
			  __gthread_mutex_t *__mutex,
			  const __gthread_time_t *__abs_time)
{
  DWORD __rel_time = __gthr_win32_abs_to_rel_time (__abs_time);
  if (SleepConditionVariableCS ((PCONDITION_VARIABLE) __cond,
				(PCRITICAL_SECTION) __mutex,
				__rel_time))
    return 0;
  else
    return (int) GetLastError ();
}

#endif /* __GTHREAD_HAS_COND */

#endif /*  __GTHREAD_HIDE_WIN32API */

static inline void
__gthread_recursive_mutex_init_function (__gthread_recursive_mutex_t *__mutex)
{
  __gthread_mutex_init_function (__mutex);
}

static inline void
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex)
{
  __gthread_mutex_destroy (__mutex);
}

static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_lock (__mutex);
}

static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_unlock (__mutex);
}

#if __GTHREAD_HAS_COND

static inline int
__gthread_cond_destroy (__gthread_cond_t *__cond ATTRIBUTE_UNUSED)
{
  return 0;
}

static inline int
__gthread_cond_wait_recursive (__gthread_cond_t *__cond,
			       __gthread_recursive_mutex_t *__mutex)
{
  return __gthread_cond_wait (__cond, __mutex);
}

#endif

#ifdef __cplusplus
}
#endif

#endif /* _LIBOBJC */

#endif /* ! GCC_GTHR_WIN32_H */
