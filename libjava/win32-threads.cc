// win32-threads.cc - interface between libjava and Win32 threads.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

// If we're using the Boehm GC, then we need to override some of the
// thread primitives.  This is fairly gross.
#ifdef HAVE_BOEHM_GC
extern "C"
{
#include <boehm-config.h>
#include <gc.h>
};
#endif /* HAVE_BOEHM_GC */

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Thread.h>
#include <java/lang/System.h>

#include <errno.h>

#ifndef ETIMEDOUT
#define ETIMEDOUT 116
#endif

// This is used to implement thread startup.
struct starter
{
  _Jv_ThreadStartFunc *method;
  java::lang::Thread *object;
  _Jv_Thread_t *data;
};

// Controls access to the variable below
static HANDLE daemon_mutex;
static HANDLE daemon_cond;
// Number of non-daemon threads - _Jv_ThreadWait returns when this is 0
static int non_daemon_count;

// TLS key get Java object representing the thread
DWORD _Jv_ThreadKey;
// TLS key to get _Jv_Thread_t* representing the thread
DWORD _Jv_ThreadDataKey;

//
// These are the flags that can appear in _Jv_Thread_t.
//

// Thread started.
#define FLAG_START   0x01
// Thread is daemon.
#define FLAG_DAEMON  0x02

//
// Condition variables.
//

int
_Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu, jlong millis, jint nanos)
{
  DWORD time;
  DWORD rval;

  // FIXME: check for mutex ownership?

  _Jv_MutexUnlock (mu);

  if((millis == 0) && (nanos > 0))
    time = 1;
  else if(millis == 0)
    time = INFINITE;
  else
    time = millis;

  rval = WaitForSingleObject (*cv, time);
  _Jv_MutexLock (mu);

  if (rval == WAIT_FAILED)
    return _JV_NOT_OWNER;       // FIXME?
  else
    return 0;
}

//
// Mutexes.
//

int
_Jv_MutexLock (_Jv_Mutex_t *mu)
{
  DWORD rval;

  // FIXME: Are Win32 mutexs recursive? Should we use critical section objects
  rval = WaitForSingleObject (*mu, INFINITE);

  if (rval == WAIT_FAILED)
    return GetLastError ();       // FIXME: Map to errno?
  else if (rval == WAIT_TIMEOUT)
    return ETIMEDOUT;
  else
    return 0;
}

//
// Threads.
//

void
_Jv_InitThreads (void)
{
  _Jv_ThreadKey = TlsAlloc();
  _Jv_ThreadDataKey = TlsAlloc();
  daemon_mutex = CreateMutex(NULL, 0, NULL);
  daemon_cond = CreateEvent(NULL, 0, 0, NULL);
  non_daemon_count = 0;
}

void
_Jv_ThreadInitData (_Jv_Thread_t **data, java::lang::Thread *)
{
  _Jv_Thread_t *info = new _Jv_Thread_t;
  info->flags = 0;

  // FIXME register a finalizer for INFO here.
  // FIXME also must mark INFO somehow.

  *data = info;
}

void
_Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio)
{
  int actual = THREAD_PRIORITY_NORMAL;

  if (data->flags & FLAG_START)
    {
      switch (prio)
        {
          case 10:
            actual = THREAD_PRIORITY_TIME_CRITICAL;
            break;
          case 9:
            actual = THREAD_PRIORITY_HIGHEST;
            break;
          case 8:
          case 7:
            actual = THREAD_PRIORITY_ABOVE_NORMAL;
            break;
          case 6:
          case 5:
            actual = THREAD_PRIORITY_NORMAL;
            break;
          case 4:
          case 3:
            actual = THREAD_PRIORITY_BELOW_NORMAL;
            break;
          case 2:
            actual = THREAD_PRIORITY_LOWEST;
            break;
          case 1:
            actual = THREAD_PRIORITY_IDLE;
            break;
        }
      SetThreadPriority(data->handle, actual);
    }
}

// This function is called when a thread is started.  We don't arrange
// to call the `run' method directly, because this function must
// return a value.
static DWORD __stdcall
really_start (void* x)
{
  struct starter *info = (struct starter *) x;

  TlsSetValue (_Jv_ThreadKey, info->object);
  TlsSetValue (_Jv_ThreadDataKey, info->data);
  info->method (info->object);

  if (! (info->data->flags & FLAG_DAEMON))
    {
      WaitForSingleObject (daemon_mutex, INFINITE);
      non_daemon_count--;
      if (! non_daemon_count)
          PulseEvent (daemon_cond);
      ReleaseMutex (daemon_mutex);
    }

  return 0;
}

void
_Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data, _Jv_ThreadStartFunc *meth)
{
  DWORD id;
  struct starter *info;

  // Do nothing if thread has already started
  if (data->flags & FLAG_START)
    return;
  data->flags |= FLAG_START;

  // FIXME: handle marking the info object for GC.
  info = (struct starter *) _Jv_AllocBytes (sizeof (struct starter));
  info->method = meth;
  info->object = thread;
  info->data = data;

  if (! thread->isDaemon ())
    {
      WaitForSingleObject (daemon_mutex, INFINITE);
      non_daemon_count++;
      ReleaseMutex (daemon_mutex);
    }
  else
    data->flags |= FLAG_DAEMON;

  HANDLE h = CreateThread(NULL, 0, really_start, info, 0, &id);
  _Jv_ThreadSetPriority(data, thread->getPriority());

  //if (!h)
    //JvThrow ();
}

void
_Jv_ThreadWait (void)
{
  WaitForSingleObject(daemon_mutex, INFINITE);
  if(non_daemon_count)
      SignalObjectAndWait(daemon_mutex, daemon_cond, INFINITE, 0);
  ReleaseMutex(daemon_mutex);
}

void
_Jv_ThreadInterrupt (_Jv_Thread_t *data)
{
  MessageBox(NULL, "Unimplemented", "win32-threads.cc:_Jv_ThreadInterrupt", MB_OK);
  // FIXME:
}
