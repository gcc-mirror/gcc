/* RTco.c provides minimal access to thread primitives.

Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

extern void M2RTS_Halt (const char *, int, const char *, const char *);
int RTco_init (void);

// #define TRACEON

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

#if !defined(TRUE)
#define TRUE (1 == 1)
#endif

#if !defined(FALSE)
#define FALSE (1 == 0)
#endif

#if defined(TRACEON)
#define tprintf printf
#else
/* sizeof is not evaluated.  */
#define tprintf (void)sizeof
#endif

typedef struct threadCB_s
{
  void (*proc) (void);
  int execution;
  pthread_t p;
  int tid;
  unsigned int interruptLevel;
} threadCB;


typedef struct threadSem_s
{
  __gthread_mutex_t mutex;
  __gthread_cond_t counter;
  int waiting;
  int sem_value;
} threadSem;

static unsigned int nThreads = 0;
static threadCB *threadArray = NULL;
static unsigned int nSemaphores = 0;
static threadSem **semArray = NULL;

/* used to lock the above module data structures.  */
static threadSem lock;
static int initialized = FALSE;

static void
initSem (threadSem *sem, int value)
{
  __GTHREAD_COND_INIT_FUNCTION (&sem->counter);
  __GTHREAD_MUTEX_INIT_FUNCTION (&sem->mutex);
  sem->waiting = FALSE;
  sem->sem_value = value;
}

static void
waitSem (threadSem *sem)
{
  __gthread_mutex_lock (&sem->mutex);
  if (sem->sem_value == 0)
    {
      sem->waiting = TRUE;
      __gthread_cond_wait (&sem->counter, &sem->mutex);
      sem->waiting = FALSE;
    }
  else
    sem->sem_value--;
  __gthread_mutex_unlock (&sem->mutex);
}

static void
signalSem (threadSem *sem)
{
  __gthread_mutex_unlock (&sem->mutex);
  if (sem->waiting)
    __gthread_cond_signal (&sem->counter);
  else
    sem->sem_value++;
  __gthread_mutex_unlock (&sem->mutex);  
}

void stop (void) {}

void
RTco_wait (int sid)
{
  RTco_init ();
  tprintf ("wait %d\n", sid);
  waitSem (semArray[sid]);
}

void
RTco_signal (int sid)
{
  RTco_init ();
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
    M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__,
                "too many semaphores created");
#else
  threadSem *sem
      = (threadSem *)malloc (sizeof (threadSem));

  /* we need to be careful when using realloc as the lock (semaphore)
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

int
RTco_initSemaphore (int value)
{
  int sid;

  RTco_init ();
  waitSem (&lock);
  sid = initSemaphore (value);
  signalSem (&lock);
  return sid;
}

/* signalThread signal the semaphore associated with thread tid.  */

void
RTco_signalThread (int tid)
{
  int sem;
  RTco_init ();
  tprintf ("signalThread %d\n", tid);
  waitSem (&lock);
  sem = threadArray[tid].execution;
  signalSem (&lock);
  RTco_signal (sem);
}

/* waitThread wait on the semaphore associated with thread tid.  */

void
RTco_waitThread (int tid)
{
  RTco_init ();
  tprintf ("waitThread %d\n", tid);
  RTco_wait (threadArray[tid].execution);
}

int
currentThread (void)
{
  int tid;

  for (tid = 0; tid < nThreads; tid++)
    if (pthread_self () == threadArray[tid].p)
      return tid;
  M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__,
              "failed to find currentThread");
}

int
RTco_currentThread (void)
{
  int tid;

  RTco_init ();
  waitSem (&lock);
  tid = currentThread ();
  tprintf ("currentThread %d\n", tid);
  signalSem (&lock);
  return tid;
}

/* currentInterruptLevel returns the interrupt level of the current thread.  */

unsigned int
RTco_currentInterruptLevel (void)
{
  RTco_init ();
  tprintf ("currentInterruptLevel %d\n",
           threadArray[RTco_currentThread ()].interruptLevel);
  return threadArray[RTco_currentThread ()].interruptLevel;
}

/* turninterrupts returns the old interrupt level and assigns the
   interrupt level to newLevel.  */

unsigned int
RTco_turnInterrupts (unsigned int newLevel)
{
  int tid = RTco_currentThread ();
  unsigned int old = RTco_currentInterruptLevel ();

  tprintf ("turnInterrupts from %d to %d\n", old, newLevel);
  waitSem (&lock);
  threadArray[tid].interruptLevel = newLevel;
  signalSem (&lock);
  return old;
}

static void
never (void)
{
  M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__,
              "the main thread should never call here");
}

static void *
execThread (void *t)
{
  threadCB *tp = (threadCB *)t;

  tprintf ("exec thread tid = %d  function = 0x%p  arg = 0x%p\n", tp->tid,
           tp->proc, t);
  RTco_waitThread (
      tp->tid); /* forcing this thread to block, waiting to be scheduled.  */
  tprintf ("  exec thread [%d]  function = 0x%p  arg = 0x%p\n", tp->tid,
           tp->proc, t);
  tp->proc (); /* now execute user procedure.  */
#if 0
  M2RTS_CoroutineException ( __FILE__, __LINE__, __COLUMN__, __FUNCTION__, "coroutine finishing");
#endif
  M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__, "execThread should never finish");
  return NULL;
}

static int
newThread (void)
{
#if defined(POOL)
  nThreads += 1;
  if (nThreads == THREAD_POOL)
    M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__, "too many threads created");
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
  threadArray[tid].execution = initSemaphore (0);
  threadArray[tid].interruptLevel = interrupt;

  /* set thread creation attributes.  */
  result = pthread_attr_init (&attr);
  if (result != 0)
    M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__,
                "failed to create thread attribute");

  if (stackSize > 0)
    {
      result = pthread_attr_setstacksize (&attr, stackSize);
      if (result != 0)
        M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__,
                    "failed to set stack size attribute");
    }

  tprintf ("initThread [%d]  function = 0x%p  (arg = 0x%p)\n", tid, proc,
           (void *)&threadArray[tid]);
  result = pthread_create (&threadArray[tid].p, &attr, execThread,
                           (void *)&threadArray[tid]);
  if (result != 0)
    M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__, "thread_create failed");
  tprintf ("  created thread [%d]  function = 0x%p  0x%p\n", tid, proc,
           (void *)&threadArray[tid]);
  return tid;
}

int
RTco_initThread (void (*proc) (void), unsigned int stackSize,
                 unsigned int interrupt)
{
  int tid;

  RTco_init ();
  waitSem (&lock);
  tid = initThread (proc, stackSize, interrupt);
  signalSem (&lock);
  return tid;
}

/* transfer unlocks thread p2 and locks the current thread.  p1 is
   updated with the current thread id.  */

void
RTco_transfer (int *p1, int p2)
{
  int tid = currentThread ();

  if (!initialized)
    M2RTS_Halt (
        __FILE__, __LINE__, __FUNCTION__,
        "cannot transfer to a process before the process has been created");
  if (tid == p2)
    {
      /* error.  */
      M2RTS_Halt (__FILE__, __LINE__, __FUNCTION__,
	      "attempting to transfer to ourself");
    }
  else
    {
      *p1 = tid;
      tprintf ("start, context switching from: %d to %d\n", tid, p2);
      RTco_signalThread (p2);
      RTco_waitThread (tid);
      tprintf ("end, context back to %d\n", tid);
    }
}

int
RTco_select (int p1, void *p2, void *p3, void *p4, void *p5)
{
  tprintf ("[%x]  RTco.select (...)\n", pthread_self ());
  return pselect (p1, p2, p3, p4, p5, NULL);
}

int
RTco_init (void)
{
  if (!initialized)
    {
      int tid;

      tprintf ("RTco initialized\n");
      initSem (&lock, 0);
      /* create initial thread container.  */
#if defined(POOL)
      threadArray = (threadCB *)malloc (sizeof (threadCB) * THREAD_POOL);
      semArray = (threadSem **)malloc (sizeof (threadSem *) * SEM_POOL);
#endif
      tid = newThread (); /* for the current initial thread.  */
      threadArray[tid].tid = tid;
      threadArray[tid].execution = initSemaphore (0);
      threadArray[tid].p = pthread_self ();
      threadArray[tid].interruptLevel = 0;
      threadArray[tid].proc
          = never; /* this shouldn't happen as we are already running.  */
      initialized = TRUE;
      tprintf ("RTco initialized completed\n");
      signalSem (&lock);
    }
  return 0;
}

void
_M2_RTco_init ()
{
}

void
_M2_RTco_finish ()
{
}
