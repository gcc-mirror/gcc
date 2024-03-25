/* RTco.cc provides minimal access to thread primitives.

Copyright (C) 2019-2022 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include <unistd.h>
#include <pthread.h>
#include <sys/select.h>
#include <stdlib.h>
#include <m2rts.h>
#include <cstdio>

#define EXPORT(FUNC) m2iso ## _RTco_ ## FUNC
#define M2EXPORT(FUNC) m2iso ## _M2_RTco_ ## FUNC
#define M2LIBNAME "m2iso"

/* This implementation of RTco.cc uses a single lock for mutex across
   the whole module.  It also forces context switching between threads
   in transfer by combining an implementation of wait and signal.

   All semaphores are implemented using the same mutex lock and
   separate condition variables.  */

#undef TRACEON

#define POOL
#define SEM_POOL 10000
#define THREAD_POOL 10000

#define _GTHREAD_USE_COND_INIT_FUNC
#include "gthr.h"

/* Ensure that ANSI conform stdio is used.  This needs to be set
   before any system header file is included.  */
#if defined __MINGW32__
#define _POSIX 1
#define gm2_printf gnu_printf
#else
#define gm2_printf __printf__
#endif

#if defined(TRACEON)
#define tprintf printf
#else
#define tprintf(...)
#endif


typedef struct threadCB_s
{
  void (*proc) (void);
  pthread_t p;
  int tid;   /* The thread id.  */
  unsigned int interruptLevel;
  __gthread_cond_t run_counter;  /* Used to block the thread and force
				    a context switch.  */
  int value;    /* Count 0 or 1.  */
  bool waiting; /* Is this thread waiting on the run_counter?  */
} threadCB;


typedef struct threadSem_s
{
  __gthread_cond_t counter;
  bool waiting;
  int sem_value;
} threadSem;

static unsigned int nThreads = 0;
static threadCB *threadArray = NULL;
static unsigned int nSemaphores = 0;
static threadSem **semArray = NULL;

/* These are used to lock the above module data structures.  */
static __gthread_mutex_t lock;  /* This is the only mutex for
				   the whole module.  */
static bool initialized = false;

extern "C" int EXPORT(init) (void);

extern "C" void
M2EXPORT(dep) (void)
{
}

extern "C" void
M2EXPORT(init) (int argc, char *argv[], char *envp[])
{
}

extern "C" void
M2EXPORT(fini) (int argc, char *argv[], char *envp[])
{
}


static void
initSem (threadSem *sem, int value)
{
  __GTHREAD_COND_INIT_FUNCTION (&sem->counter);
  sem->waiting = false;
  sem->sem_value = value;
}

static void
waitSem (threadSem *sem)
{
  __gthread_mutex_lock (&lock);
  if (sem->sem_value == 0)
    {
      sem->waiting = true;
      __gthread_cond_wait (&sem->counter, &lock);
      sem->waiting = false;
    }
  else
    sem->sem_value--;
  __gthread_mutex_unlock (&lock);
}

static void
signalSem (threadSem *sem)
{
  __gthread_mutex_lock (&lock);
  if (sem->waiting)
    __gthread_cond_signal (&sem->counter);
  else
    sem->sem_value++;
  __gthread_mutex_unlock (&lock);
}

extern "C" void
EXPORT(wait) (int sid)
{
  EXPORT(init) ();
  tprintf ("wait %d\n", sid);
  waitSem (semArray[sid]);
}

extern "C" void
EXPORT(signal) (int sid)
{
  EXPORT(init) ();
  tprintf ("signal %d\n", sid);
  signalSem (semArray[sid]);
}

static int
newSem (void)
{
#if defined(POOL)
  semArray[nSemaphores]
      = (threadSem *)malloc (sizeof (threadSem));
  nSemaphores += 1;
  if (nSemaphores == SEM_POOL)
    m2iso_M2RTS_HaltC ("too many semaphores created",
		       __FILE__, __FUNCTION__, __LINE__);
#else
  threadSem *sem
      = (threadSem *)malloc (sizeof (threadSem));

  /* We need to be careful when using realloc as the lock (semaphore)
     operators use the semaphore address.  So we keep an array of pointer
     to semaphores.  */
  if (nSemaphores == 0)
    {
      semArray = (threadSem **)malloc (sizeof (sem));
      nSemaphores = 1;
    }
  else
    {
      nSemaphores += 1;
      semArray = (threadSem **)realloc (semArray,
					sizeof (sem) * nSemaphores);
    }
  semArray[nSemaphores - 1] = sem;
#endif
  return nSemaphores - 1;
}

static int
initSemaphore (int value)
{
  int sid = newSem ();

  initSem (semArray[sid], value);
  tprintf ("%d = initSemaphore (%d)\n", sid, value);
  return sid;
}

extern "C" int
EXPORT(initSemaphore) (int value)
{
  int sid;

  tprintf ("initSemaphore (%d) called\n", value);
  EXPORT(init) ();
  tprintf ("about to access lock\n");
  __gthread_mutex_lock (&lock);
  sid = initSemaphore (value);
  __gthread_mutex_unlock (&lock);
  return sid;
}

static int
currentThread (void)
{
  int tid;

  for (tid = 0; tid < nThreads; tid++)
    if (pthread_self () == threadArray[tid].p)
      return tid;
  m2iso_M2RTS_HaltC ("failed to find currentThread",
		     __FILE__, __FUNCTION__, __LINE__);
}

extern "C" int
EXPORT(currentThread) (void)
{
  int tid;

  EXPORT(init) ();
  __gthread_mutex_lock (&lock);
  tid = currentThread ();
  tprintf ("currentThread %d\n", tid);
  __gthread_mutex_unlock (&lock);
  return tid;
}

/* currentInterruptLevel returns the interrupt level of the current thread.  */

extern "C" unsigned int
EXPORT(currentInterruptLevel) (void)
{
  EXPORT(init) ();
  __gthread_mutex_lock (&lock);
  int current = currentThread ();
  tprintf ("currentInterruptLevel %d\n",
           threadArray[current].interruptLevel);
  int level = threadArray[current].interruptLevel;
  __gthread_mutex_unlock (&lock);
  return level;
}

/* turninterrupts returns the old interrupt level and assigns the
   interrupt level to newLevel.  */

extern "C" unsigned int
EXPORT(turnInterrupts) (unsigned int newLevel)
{
  EXPORT(init) ();
  __gthread_mutex_lock (&lock);
  int current = currentThread ();
  unsigned int old = threadArray[current].interruptLevel;
  tprintf ("turnInterrupts from %d to %d\n", old, newLevel);
  threadArray[current].interruptLevel = newLevel;
  __gthread_mutex_unlock (&lock);
  return old;
}

static void
never (void)
{
  m2iso_M2RTS_HaltC ("the main thread should never call here",
		     __FILE__, __FUNCTION__, __LINE__);
}

static void *
execThread (void *t)
{
  threadCB *tp = (threadCB *)t;

  tprintf ("exec thread tid = %d coming to life\n", tp->tid);
  __gthread_mutex_lock (&lock);
  tprintf ("exec thread tid = %d  function = 0x%p  arg = 0x%p\n", tp->tid,
           tp->proc, t);
  /* Has the thread been signalled?  */
  if (tp->value == 0)
    {
      /* Not been signalled therefore we force ourselves to block.  */
      tprintf ("%s: forcing thread tid = %d to wait\n",
	       __FUNCTION__, tp->tid);
      tp->waiting = true;  /* We are waiting.  */
      __gthread_cond_wait (&tp->run_counter, &lock);
      tp->waiting = false; /* Running again.  */
    }
  else
    {
      /* Yes signalled, therefore just take the recorded signal and continue.  */
      tprintf ("%s: no need for thread tid = %d to wait\n",
	       __FUNCTION__, tp->tid);
      tp->value--;
    }
  tprintf ("  running exec thread [%d]  function = 0x%p  arg = 0x%p\n", tp->tid,
           tp->proc, t);
  __gthread_mutex_unlock (&lock);
  tp->proc (); /* Now execute user procedure.  */
#if 0
  m2iso_M2RTS_CoroutineException ( __FILE__, __LINE__, __COLUMN__, __FUNCTION__, "coroutine finishing");
#endif
  m2iso_M2RTS_HaltC ("execThread should never finish",
		     __FILE__, __FUNCTION__, __LINE__);
  return NULL;
}

static int
newThread (void)
{
#if defined(POOL)
  nThreads += 1;
  if (nThreads == THREAD_POOL)
    m2iso_M2RTS_HaltC ("too many threads created",
		       __FILE__, __FUNCTION__, __LINE__);
  return nThreads - 1;
#else
  if (nThreads == 0)
    {
      threadArray = (threadCB *)malloc (sizeof (threadCB));
      nThreads = 1;
    }
  else
    {
      nThreads += 1;
      threadArray
          = (threadCB *)realloc (threadArray, sizeof (threadCB) * nThreads);
    }
  return nThreads - 1;
#endif
}

static int
initThread (void (*proc) (void), unsigned int stackSize,
            unsigned int interrupt)
{
  int tid = newThread ();
  pthread_attr_t attr;
  int result;

  threadArray[tid].proc = proc;
  threadArray[tid].tid = tid;
  /* Initialize the thread run_counter used to force a context switch.  */
  __GTHREAD_COND_INIT_FUNCTION (&threadArray[tid].run_counter);
  threadArray[tid].interruptLevel = interrupt;
  threadArray[tid].waiting = false;     /* The thread is running.  */
  threadArray[tid].value = 0;  /* No signal has been seen yet.  */

  /* Set thread creation attributes.  */
  result = pthread_attr_init (&attr);
  if (result != 0)
    m2iso_M2RTS_HaltC ("failed to create thread attribute",
		       __FILE__, __FUNCTION__, __LINE__);

  if (stackSize > 0)
    {
      result = pthread_attr_setstacksize (&attr, stackSize);
      if (result != 0)
        m2iso_M2RTS_HaltC ("failed to set stack size attribute",
			   __FILE__, __FUNCTION__, __LINE__);
    }

  tprintf ("initThread [%d]  function = 0x%p  (arg = 0x%p)\n", tid, proc,
           (void *)&threadArray[tid]);
  result = pthread_create (&threadArray[tid].p, &attr, execThread,
                           (void *)&threadArray[tid]);
  if (result != 0)
    m2iso_M2RTS_HaltC ("thread_create failed",
		       __FILE__, __FUNCTION__, __LINE__);
  tprintf ("  created thread [%d]  function = 0x%p  0x%p\n", tid, proc,
           (void *)&threadArray[tid]);
  return tid;
}

extern "C" int
EXPORT(initThread) (void (*proc) (void), unsigned int stackSize,
		    unsigned int interrupt)
{
  int tid;

  EXPORT(init) ();
  __gthread_mutex_lock (&lock);
  tid = initThread (proc, stackSize, interrupt);
  __gthread_mutex_unlock (&lock);
  return tid;
}

/* transfer unlocks thread p2 and locks the current thread.  p1 is
   updated with the current thread id.
   The implementation of transfer uses a combined wait/signal.  */

extern "C" void
EXPORT(transfer) (int *p1, int p2)
{
  __gthread_mutex_lock (&lock);
  {
    int current = currentThread ();
    if (!initialized)
      m2iso_M2RTS_HaltC ("cannot transfer to a process before the process has been created",
			 __FILE__, __FUNCTION__, __LINE__);
    if (current == p2)
      {
	/* Error.  */
	m2iso_M2RTS_HaltC ("attempting to transfer to ourself",
			   __FILE__, __FUNCTION__, __LINE__);
    }
    else
      {
	*p1 = current;
	int old = current;
	tprintf ("start, context switching from: %d to %d\n", current, p2);
	/* Perform signal (p2 sem).  Without the mutex lock as we have
	   already obtained it above.  */
	if (threadArray[p2].waiting)
	  {
	    /* p2 is blocked on the condition variable, release it.  */
	    tprintf ("p1 = %d cond_signal to p2 (%d)\n", current, p2);
	  __gthread_cond_signal (&threadArray[p2].run_counter);
	  tprintf ("after p1 = %d cond_signal to p2 (%d)\n", current, p2);
	  }
	else
	  {
	    /* p2 hasn't reached the condition variable, so bump value
	       ready for p2 to test.  */
	    tprintf ("no need for thread %d to cond_signal - bump %d value (pre) = %d\n",
		     current, p2, threadArray[p2].value);
	    threadArray[p2].value++;
	  }
	/* Perform wait (old sem).  Again without obtaining mutex as
	   we've already claimed it.  */
	if (threadArray[old].value == 0)
	  {
	    /* Record we are about to wait on the condition variable.  */
	    threadArray[old].waiting = true;
	    __gthread_cond_wait (&threadArray[old].run_counter, &lock);
	    threadArray[old].waiting = false;
	    /* We are running again.  */
	  }
	else
	  {
	    tprintf ("(currentThread = %d) no need for thread %d to cond_wait - taking value (pre) = %d\n",
		     current, old, threadArray[old].value);
	    /* No need to block as we have been told a signal has
               effectively already been recorded.  We remove the signal
               notification without blocking.  */
	    threadArray[old].value--;
	  }
	tprintf ("end, context back to %d\n", current);
	if (current != old)
	  m2iso_M2RTS_HaltC ("wrong process id",
			     __FILE__, __FUNCTION__, __LINE__);
      }
  }
  __gthread_mutex_unlock (&lock);
}

extern "C" int
EXPORT(select) (int p1, fd_set *p2, fd_set *p3, fd_set *p4, const timespec *p5)
{
  EXPORT(init) ();
  tprintf ("[%x]  RTco.select (...)\n", pthread_self ());
  return pselect (p1, p2, p3, p4, p5, NULL);
}

extern "C" int
EXPORT(init) (void)
{
  tprintf ("checking init\n");
  if (! initialized)
    {
      initialized = true;

      tprintf ("RTco initialized\n");
      __GTHREAD_MUTEX_INIT_FUNCTION (&lock);
      __gthread_mutex_lock (&lock);
      /* Create initial thread container.  */
#if defined(POOL)
      threadArray = (threadCB *)malloc (sizeof (threadCB) * THREAD_POOL);
      semArray = (threadSem **)malloc (sizeof (threadSem *) * SEM_POOL);
#endif
      /* Create a thread control block for the main program (or process).  */
      int tid = newThread ();  /* For the current initial thread.  */
      threadArray[tid].p = pthread_self ();
      threadArray[tid].tid = tid;
      __GTHREAD_COND_INIT_FUNCTION (&threadArray[tid].run_counter);
      threadArray[tid].interruptLevel = 0;
      /* The line below shouldn't be necessary as we are already running.  */
      threadArray[tid].proc = never;
      threadArray[tid].waiting = false;   /* We are running.  */
      threadArray[tid].value = 0;   /* No signal from anyone yet.  */
      tprintf ("RTco initialized completed\n");
      __gthread_mutex_unlock (&lock);
    }
  return 0;
}

extern "C" void __attribute__((__constructor__))
M2EXPORT(ctor) (void)
{
  m2iso_M2RTS_RegisterModule ("RTco", M2LIBNAME,
			      M2EXPORT(init), M2EXPORT(fini),
			      M2EXPORT(dep));
}
