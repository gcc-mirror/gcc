// posix-threads.cc - interface between libjava and POSIX threads.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// TO DO:
// * Document signal handling limitations

#include <config.h>

// If we're using the Boehm GC, then we need to override some of the
// thread primitives.  This is fairly gross.
#ifdef HAVE_BOEHM_GC
extern "C"
{
#include <gcconfig.h>
#include <gc.h>
};
#endif /* HAVE_BOEHM_GC */

#include <stdlib.h>
#include <time.h>
#include <signal.h>
#include <errno.h>
#include <limits.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Thread.h>
#include <java/lang/System.h>
#include <java/lang/Long.h>
#include <java/lang/OutOfMemoryError.h>

// This is used to implement thread startup.
struct starter
{
  _Jv_ThreadStartFunc *method;
  java::lang::Thread *object;
  _Jv_Thread_t *data;
};

// This is the key used to map from the POSIX thread value back to the
// Java object representing the thread.  The key is global to all
// threads, so it is ok to make it a global here.
pthread_key_t _Jv_ThreadKey;

// This is the key used to map from the POSIX thread value back to the
// _Jv_Thread_t* representing the thread.
pthread_key_t _Jv_ThreadDataKey;

// We keep a count of all non-daemon threads which are running.  When
// this reaches zero, _Jv_ThreadWait returns.
static pthread_mutex_t daemon_mutex;
static pthread_cond_t daemon_cond;
static int non_daemon_count;

// The signal to use when interrupting a thread.
#ifdef LINUX_THREADS
  // LinuxThreads (prior to glibc 2.1) usurps both SIGUSR1 and SIGUSR2.
#  define INTR SIGHUP
#else /* LINUX_THREADS */
#  define INTR SIGUSR2
#endif /* LINUX_THREADS */

//
// These are the flags that can appear in _Jv_Thread_t.
//

// Thread started.
#define FLAG_START   0x01
// Thread is daemon.
#define FLAG_DAEMON  0x02



int
_Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
	      jlong millis, jint nanos)
{
  if (_Jv_PthreadCheckMonitor (mu))
    return 1;

  int r;
  pthread_mutex_t *pmu = _Jv_PthreadGetMutex (mu);
  struct timespec ts;
  jlong m, m2, startTime;
  bool done_sleeping = false;

  if (millis == 0 && nanos == 0)
    {
#ifdef LINUX_THREADS
      // pthread_cond_timedwait can be interrupted by a signal on linux, while
      // pthread_cond_wait can not. So pthread_cond_timedwait() forever.
      m = java::lang::Long::MAX_VALUE;
      ts.tv_sec = LONG_MAX;
      ts.tv_nsec = 0;
#endif
    }
  else
    {
      startTime = java::lang::System::currentTimeMillis();
      m = millis + startTime;
      ts.tv_sec = m / 1000; 
      ts.tv_nsec = ((m % 1000) * 1000000) + nanos; 
    }

  java::lang::Thread *current = _Jv_ThreadCurrent();

  do
    {
      r = EINTR;
      // Check to ensure the thread hasn't already been interrupted.
      if (!(current->isInterrupted ()))
        {
#ifdef LINUX_THREADS	
	  // FIXME: in theory, interrupt() could be called on this thread
	  // between the test above and the wait below, resulting in the 
	  // interupt() call failing. I don't see a way to fix this 
	  // without significant changes to the implementation.
	  r = pthread_cond_timedwait (cv, pmu, &ts);
#else
	  if (millis == 0 && nanos == 0)
	    r = pthread_cond_wait (cv, pmu);
	  else	  
	    r = pthread_cond_timedwait (cv, pmu, &ts);	  
#endif
	}
      
      if (r == EINTR)
	{
	  /* We were interrupted by a signal.  Either this is
	     because we were interrupted intentionally (i.e. by
	     Thread.interrupt()) or by the GC if it is
	     signal-based.  */
	  if (current->isInterrupted ())
	    {
	      r = 0;
              done_sleeping = true;
            }
	  else
            {
	      /* We were woken up by the GC or another signal.  */
	      m2 = java::lang::System::currentTimeMillis ();
	      if (m2 >= m)
		{
		  r = 0;
		  done_sleeping = true;
		}
	    }
	}
      else if (r == ETIMEDOUT)
	{
	  /* A timeout is a normal result.  */
	  r = 0;
	  done_sleeping = true;
	}
      else
	done_sleeping = true;
    }
  while (! done_sleeping);

  return r != 0;
}

#ifndef RECURSIVE_MUTEX_IS_DEFAULT

void
_Jv_MutexInit (_Jv_Mutex_t *mu)
{
#ifdef HAVE_RECURSIVE_MUTEX
  pthread_mutexattr_t *val = NULL;

#if defined (HAVE_PTHREAD_MUTEXATTR_SETTYPE)
  pthread_mutexattr_t attr;

  // If this is slow, then allocate it statically and only initialize
  // it once.
  pthread_mutexattr_init (&attr);
  pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
  val = &attr;
#elif defined (HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)
  pthread_mutexattr_t attr;
  pthread_mutexattr_init (&attr);
  pthread_mutexattr_setkind_np (&attr, PTHREAD_MUTEX_RECURSIVE_NP);
  val = &attr;
#endif

  pthread_mutex_init (_Jv_PthreadGetMutex (mu), val);
#ifdef PTHREAD_MUTEX_IS_STRUCT
  mu->count = 0;
#endif

#if defined (HAVE_PTHREAD_MUTEXATTR_SETTYPE) || defined (HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)
  pthread_mutexattr_destroy (&attr);
#endif

#else /* HAVE_RECURSIVE_MUTEX */

  // No recursive mutex, so simulate one.
  pthread_mutex_init (&mu->mutex, NULL);
  pthread_mutex_init (&mu->mutex2, NULL);
  pthread_cond_init (&mu->cond, 0);
  mu->count = 0;

#endif /* HAVE_RECURSIVE_MUTEX */
}

#endif /* not RECURSIVE_MUTEX_IS_DEFAULT */

#if ! defined (LINUX_THREADS) && ! defined (HAVE_RECURSIVE_MUTEX)

void
_Jv_MutexDestroy (_Jv_Mutex_t *mu)
{
  pthread_mutex_destroy (&mu->mutex);
  pthread_mutex_destroy (&mu->mutex2);
  pthread_cond_destroy (&mu->cond);
}

int
_Jv_MutexLock (_Jv_Mutex_t *mu)
{
  if (pthread_mutex_lock (&mu->mutex))
    return -1;
  while (1)
    {
      if (mu->count == 0)
	{
	  // Grab the lock.
	  mu->thread = pthread_self ();
	  mu->count = 1;
	  pthread_mutex_lock (&mu->mutex2);
	  break;
	}
      else if (pthread_self () == mu->thread)
	{
	  // Already have the lock.
	  mu->count += 1;
	  break;
	}
      else
	{
	  // Try to acquire the lock.
	  pthread_cond_wait (&mu->cond, &mu->mutex);
	}
    }
  pthread_mutex_unlock (&mu->mutex);
  return 0;
}

int
_Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  if (pthread_mutex_lock (&mu->mutex))
    return -1;
  int r = 0;
  if (mu->count == 0 || pthread_self () != mu->thread)
    r = -1;
  else
    {
      mu->count -= 1;
      if (! mu->count)
	{
	  pthread_mutex_unlock (&mu->mutex2);
	  pthread_cond_signal (&mu->cond);
	}
    }
  pthread_mutex_unlock (&mu->mutex);
  return r;
}

#endif /* not LINUX_THREADS and not HAVE_RECURSIVE_MUTEX */

static void
handle_intr (int)
{
  // Do nothing.
}

void
_Jv_InitThreads (void)
{
  pthread_key_create (&_Jv_ThreadKey, NULL);
  pthread_key_create (&_Jv_ThreadDataKey, NULL);
  pthread_mutex_init (&daemon_mutex, NULL);
  pthread_cond_init (&daemon_cond, 0);
  non_daemon_count = 0;

  // Arrange for the interrupt signal to interrupt system calls.
  struct sigaction act;
  act.sa_handler = handle_intr;
  sigemptyset (&act.sa_mask);
  act.sa_flags = 0;
  sigaction (INTR, &act, NULL);
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
  if (data->flags & FLAG_START)
    {
      struct sched_param param;

      param.sched_priority = prio;
      pthread_setschedparam (data->thread, SCHED_RR, &param);
    }
}

// This function is called when a thread is started.  We don't arrange
// to call the `run' method directly, because this function must
// return a value.
static void *
really_start (void *x)
{
  struct starter *info = (struct starter *) x;

  pthread_setspecific (_Jv_ThreadKey, info->object);
  pthread_setspecific (_Jv_ThreadDataKey, info->data);
  info->method (info->object);

  if (! (info->data->flags & FLAG_DAEMON))
    {
      pthread_mutex_lock (&daemon_mutex);
      --non_daemon_count;
      if (! non_daemon_count)
	pthread_cond_signal (&daemon_cond);
      pthread_mutex_unlock (&daemon_mutex);
    }

  return NULL;
}

void
_Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		 _Jv_ThreadStartFunc *meth)
{
  struct sched_param param;
  pthread_attr_t attr;
  struct starter *info;

  if (data->flags & FLAG_START)
    return;
  data->flags |= FLAG_START;

  param.sched_priority = thread->getPriority();

  pthread_attr_init (&attr);
  pthread_attr_setschedparam (&attr, &param);

  // FIXME: handle marking the info object for GC.
  info = (struct starter *) _Jv_AllocBytes (sizeof (struct starter));
  info->method = meth;
  info->object = thread;
  info->data = data;

  if (! thread->isDaemon())
    {
      pthread_mutex_lock (&daemon_mutex);
      ++non_daemon_count;
      pthread_mutex_unlock (&daemon_mutex);
    }
  else
    data->flags |= FLAG_DAEMON;
  int r = pthread_create (&data->thread, &attr, really_start, (void *) info);
  
  pthread_attr_destroy (&attr);

  if (r)
    {
      const char* msg = "Cannot create additional threads";
      JvThrow (new java::lang::OutOfMemoryError (JvNewStringUTF (msg)));
    }
}

void
_Jv_ThreadWait (void)
{
  pthread_mutex_lock (&daemon_mutex);
  if (non_daemon_count)
    pthread_cond_wait (&daemon_cond, &daemon_mutex);
  pthread_mutex_unlock (&daemon_mutex);
}

void
_Jv_ThreadInterrupt (_Jv_Thread_t *data)
{
  pthread_kill (data->thread, INTR);
}
