// posix-threads.cc - interface between libjava and POSIX threads.

/* Copyright (C) 1998, 1999, 2000, 2001, 2004  Free Software Foundation

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
#include <gc.h>
#endif /* HAVE_BOEHM_GC */

#include <stdlib.h>
#include <time.h>
#include <signal.h>
#include <errno.h>
#include <limits.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>	// To test for _POSIX_THREAD_PRIORITY_SCHEDULING
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Thread.h>
#include <java/lang/System.h>
#include <java/lang/Long.h>
#include <java/lang/OutOfMemoryError.h>
#include <java/lang/InternalError.h>

// This is used to implement thread startup.
struct starter
{
  _Jv_ThreadStartFunc *method;
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
#if defined(LINUX_THREADS) || defined(FREEBSD_THREADS)
  // LinuxThreads (prior to glibc 2.1) usurps both SIGUSR1 and SIGUSR2.
  // GC on FreeBSD uses both SIGUSR1 and SIGUSR2.
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



// Wait for the condition variable "CV" to be notified. 
// Return values:
// 0: the condition was notified, or the timeout expired.
// _JV_NOT_OWNER: the thread does not own the mutex "MU".   
// _JV_INTERRUPTED: the thread was interrupted. Its interrupted flag is set.   
int
_Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
	      jlong millis, jint nanos)
{
  pthread_t self = pthread_self();
  if (mu->owner != self)
    return _JV_NOT_OWNER;

  struct timespec ts;
  jlong m, startTime;

  if (millis > 0 || nanos > 0)
    {
      startTime = java::lang::System::currentTimeMillis();
      m = millis + startTime;
      ts.tv_sec = m / 1000; 
      ts.tv_nsec = ((m % 1000) * 1000000) + nanos; 
    }

  _Jv_Thread_t *current = _Jv_ThreadCurrentData ();
  java::lang::Thread *current_obj = _Jv_ThreadCurrent ();

  pthread_mutex_lock (&current->wait_mutex);

  // Now that we hold the wait mutex, check if this thread has been 
  // interrupted already.
  if (current_obj->interrupt_flag)
    {
      pthread_mutex_unlock (&current->wait_mutex);
      return _JV_INTERRUPTED;
    }

  // Add this thread to the cv's wait set.
  current->next = NULL;

  if (cv->first == NULL)
    cv->first = current;
  else
    for (_Jv_Thread_t *t = cv->first;; t = t->next)
      {
        if (t->next == NULL)
          {
            t->next = current;
            break;
          }
      }

  // Record the current lock depth, so it can be restored when we re-aquire it.
  int count = mu->count;

  // Release the monitor mutex.
  mu->count = 0;
  mu->owner = 0;
  pthread_mutex_unlock (&mu->mutex);
  
  int r = 0;
  bool done_sleeping = false;

  while (! done_sleeping)
    {
      if (millis == 0 && nanos == 0)
	r = pthread_cond_wait (&current->wait_cond, &current->wait_mutex);
      else
	r = pthread_cond_timedwait (&current->wait_cond, &current->wait_mutex, 
				    &ts);

      // In older glibc's (prior to 2.1.3), the cond_wait functions may 
      // spuriously wake up on a signal. Catch that here.
      if (r != EINTR)
        done_sleeping = true;
    }
  
  // Check for an interrupt *before* releasing the wait mutex.
  jboolean interrupted = current_obj->interrupt_flag;
  
  pthread_mutex_unlock (&current->wait_mutex);

  //  Reaquire the monitor mutex, and restore the lock count.
  pthread_mutex_lock (&mu->mutex);
  mu->owner = self;
  mu->count = count;

  // If we were interrupted, or if a timeout occurred, remove ourself from
  // the cv wait list now. (If we were notified normally, notify() will have
  // already taken care of this)
  if (r == ETIMEDOUT || interrupted)
    {
      _Jv_Thread_t *prev = NULL;
      for (_Jv_Thread_t *t = cv->first; t != NULL; t = t->next)
        {
	  if (t == current)
	    {
	      if (prev != NULL)
		prev->next = t->next;
	      else
	        cv->first = t->next;
	      t->next = NULL;
	      break;
	    }
	  prev = t;
	}
      if (interrupted)
	return _JV_INTERRUPTED;
    }
  
  return 0;
}

int
_Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu)
{
  if (_Jv_MutexCheckMonitor (mu))
    return _JV_NOT_OWNER;

  _Jv_Thread_t *target;
  _Jv_Thread_t *prev = NULL;

  for (target = cv->first; target != NULL; target = target->next)
    {
      pthread_mutex_lock (&target->wait_mutex);

      if (target->thread_obj->interrupt_flag)
        {
	  // Don't notify a thread that has already been interrupted.
	  pthread_mutex_unlock (&target->wait_mutex);
          prev = target;
	  continue;
	}

      pthread_cond_signal (&target->wait_cond);
      pthread_mutex_unlock (&target->wait_mutex);

      // Two concurrent notify() calls must not be delivered to the same 
      // thread, so remove the target thread from the cv wait list now.
      if (prev == NULL)
	cv->first = target->next;
      else
        prev->next = target->next;
		
      target->next = NULL;
      
      break;
    }

  return 0;
}

int
_Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu)
{
  if (_Jv_MutexCheckMonitor (mu))
    return _JV_NOT_OWNER;

  _Jv_Thread_t *target;
  _Jv_Thread_t *prev = NULL;

  for (target = cv->first; target != NULL; target = target->next)
    {
      pthread_mutex_lock (&target->wait_mutex);
      pthread_cond_signal (&target->wait_cond);
      pthread_mutex_unlock (&target->wait_mutex);

      if (prev != NULL)
	prev->next = NULL;
      prev = target;
    }
  if (prev != NULL)
    prev->next = NULL;
    
  cv->first = NULL;

  return 0;
}

void
_Jv_ThreadInterrupt (_Jv_Thread_t *data)
{
  pthread_mutex_lock (&data->wait_mutex);

  // Set the thread's interrupted flag *after* aquiring its wait_mutex. This
  // ensures that there are no races with the interrupt flag being set after 
  // the waiting thread checks it and before pthread_cond_wait is entered.
  data->thread_obj->interrupt_flag = true;

  // Interrupt blocking system calls using a signal.
  pthread_kill (data->thread, INTR);
  
  pthread_cond_signal (&data->wait_cond);
  
  pthread_mutex_unlock (&data->wait_mutex);
}

static void
handle_intr (int)
{
  // Do nothing.
}

static void
block_sigchld()
{
  sigset_t mask;
  sigemptyset (&mask);
  sigaddset (&mask, SIGCHLD);
  int c = pthread_sigmask (SIG_BLOCK, &mask, NULL);
  if (c != 0)
    JvFail (strerror (c));
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

  // Block SIGCHLD here to ensure that any non-Java threads inherit the new 
  // signal mask.
  block_sigchld();
}

_Jv_Thread_t *
_Jv_ThreadInitData (java::lang::Thread *obj)
{
  _Jv_Thread_t *data = (_Jv_Thread_t *) _Jv_Malloc (sizeof (_Jv_Thread_t));
  data->flags = 0;
  data->thread_obj = obj;

  pthread_mutex_init (&data->wait_mutex, NULL);
  pthread_cond_init (&data->wait_cond, NULL);

  return data;
}

void
_Jv_ThreadDestroyData (_Jv_Thread_t *data)
{
  pthread_mutex_destroy (&data->wait_mutex);
  pthread_cond_destroy (&data->wait_cond);
  _Jv_Free ((void *)data);
}

void
_Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio)
{
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
  if (data->flags & FLAG_START)
    {
      struct sched_param param;

      param.sched_priority = prio;
      pthread_setschedparam (data->thread, SCHED_RR, &param);
    }
#endif
}

void
_Jv_ThreadRegister (_Jv_Thread_t *data)
{
  pthread_setspecific (_Jv_ThreadKey, data->thread_obj);
  pthread_setspecific (_Jv_ThreadDataKey, data);

  // glibc 2.1.3 doesn't set the value of `thread' until after start_routine
  // is called. Since it may need to be accessed from the new thread, work 
  // around the potential race here by explicitly setting it again.
  data->thread = pthread_self ();

# ifdef SLOW_PTHREAD_SELF
    // Clear all self cache slots that might be needed by this thread.
    int dummy;
    int low_index = SC_INDEX(&dummy) + SC_CLEAR_MIN;
    int high_index = SC_INDEX(&dummy) + SC_CLEAR_MAX;
    for (int i = low_index; i <= high_index; ++i) 
      {
        int current_index = i;
	if (current_index < 0)
	  current_index += SELF_CACHE_SIZE;
	if (current_index >= SELF_CACHE_SIZE)
	  current_index -= SELF_CACHE_SIZE;
	_Jv_self_cache[current_index].high_sp_bits = BAD_HIGH_SP_VALUE;
      }
# endif
  // Block SIGCHLD which is used in natPosixProcess.cc.
  block_sigchld();
}

void
_Jv_ThreadUnRegister ()
{
  pthread_setspecific (_Jv_ThreadKey, NULL);
  pthread_setspecific (_Jv_ThreadDataKey, NULL);
}

// This function is called when a thread is started.  We don't arrange
// to call the `run' method directly, because this function must
// return a value.
static void *
really_start (void *x)
{
  struct starter *info = (struct starter *) x;

  _Jv_ThreadRegister (info->data);

  info->method (info->data->thread_obj);

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

  // Block SIGCHLD which is used in natPosixProcess.cc.
  // The current mask is inherited by the child thread.
  block_sigchld();

  param.sched_priority = thread->getPriority();

  pthread_attr_init (&attr);
  pthread_attr_setschedparam (&attr, &param);
  pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);

  info = (struct starter *) _Jv_AllocBytes (sizeof (struct starter));
  info->method = meth;
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
      throw new java::lang::OutOfMemoryError (JvNewStringUTF (msg));
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

#if defined(SLOW_PTHREAD_SELF)

#include "sysdep/locks.h"

// Support for pthread_self() lookup cache.
volatile self_cache_entry _Jv_self_cache[SELF_CACHE_SIZE];

_Jv_ThreadId_t
_Jv_ThreadSelf_out_of_line(volatile self_cache_entry *sce, size_t high_sp_bits)
{
  pthread_t self = pthread_self();
  sce -> high_sp_bits = high_sp_bits;
  write_barrier();
  sce -> self = self;
  return self;
}

#endif /* SLOW_PTHREAD_SELF */
