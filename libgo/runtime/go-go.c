/* go-go.c -- the go function.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>

#include "config.h"
#include "go-assert.h"
#include "go-panic.h"
#include "go-alloc.h"
#include "runtime.h"
#include "malloc.h"

#ifdef USING_SPLIT_STACK
/* FIXME: This is not declared anywhere.  */
extern void *__splitstack_find (void *, void *, size_t *, void **, void **,
				void **);
#endif

/* We stop the threads by sending them the signal GO_SIG_STOP and we
   start them by sending them the signal GO_SIG_START.  */

#define GO_SIG_START (SIGRTMIN + 1)
#define GO_SIG_STOP (SIGRTMIN + 2)

#ifndef SA_RESTART
  #define SA_RESTART 0
#endif

/* A doubly linked list of the threads we have started.  */

struct __go_thread_id
{
  /* Links.  */
  struct __go_thread_id *prev;
  struct __go_thread_id *next;
  /* True if the thread ID has not yet been filled in.  */
  _Bool tentative;
  /* Thread ID.  */
  pthread_t id;
  /* Thread's M structure.  */
  struct M *m;
  /* If the thread ID has not been filled in, the function we are
     running.  */
  void (*pfn) (void *);
  /* If the thread ID has not been filled in, the argument to the
     function.  */
  void *arg;
};

static struct __go_thread_id *__go_all_thread_ids;

/* A lock to control access to ALL_THREAD_IDS.  */

static pthread_mutex_t __go_thread_ids_lock = PTHREAD_MUTEX_INITIALIZER;

/* A semaphore used to wait until all the threads have stopped.  */

static sem_t __go_thread_ready_sem;

/* A signal set used to wait until garbage collection is complete.  */

static sigset_t __go_thread_wait_sigset;

/* Remove the current thread from the list of threads.  */

static void
remove_current_thread (void)
{
  struct __go_thread_id *list_entry;
  MCache *mcache;
  int i;
  
  list_entry = m->list_entry;
  mcache = m->mcache;

  i = pthread_mutex_lock (&__go_thread_ids_lock);
  __go_assert (i == 0);

  if (list_entry->prev != NULL)
    list_entry->prev->next = list_entry->next;
  else
    __go_all_thread_ids = list_entry->next;
  if (list_entry->next != NULL)
    list_entry->next->prev = list_entry->prev;

  runtime_MCache_ReleaseAll (mcache);

  /* As soon as we release this look, a GC could run.  Since this
     thread is no longer on the list, the GC will not find our M
     structure, so it could get freed at any time.  That means that
     any code from here to thread exit must not assume that the m is
     valid.  */
  m = NULL;

  i = pthread_mutex_unlock (&__go_thread_ids_lock);
  __go_assert (i == 0);

  runtime_lock (&runtime_mheap);
  mstats.heap_alloc += mcache->local_alloc;
  mstats.heap_objects += mcache->local_objects;
  __builtin_memset (mcache, 0, sizeof (struct MCache));
  runtime_FixAlloc_Free (&runtime_mheap.cachealloc, mcache);
  runtime_unlock (&runtime_mheap);

  free (list_entry);
}

/* Start the thread.  */

static void *
start_go_thread (void *thread_arg)
{
  struct M *newm = (struct M *) thread_arg;
  void (*pfn) (void *);
  void *arg;
  struct __go_thread_id *list_entry;
  int i;

#ifdef __rtems__
  __wrap_rtems_task_variable_add ((void **) &m);
  __wrap_rtems_task_variable_add ((void **) &__go_panic_defer);
#endif

  m = newm;

  list_entry = newm->list_entry;

  pfn = list_entry->pfn;
  arg = list_entry->arg;

#ifndef USING_SPLIT_STACK
  /* If we don't support split stack, record the current stack as the
     top of the stack.  There shouldn't be anything relevant to the
     garbage collector above this point.  */
  m->gc_sp = (void *) &arg;
#endif

  /* Finish up the entry on the thread list.  */

  i = pthread_mutex_lock (&__go_thread_ids_lock);
  __go_assert (i == 0);

  list_entry->id = pthread_self ();
  list_entry->pfn = NULL;
  list_entry->arg = NULL;
  list_entry->tentative = 0;

  i = pthread_mutex_unlock (&__go_thread_ids_lock);
  __go_assert (i == 0);

  (*pfn) (arg);

  remove_current_thread ();

  return NULL;
}

/* The runtime.Goexit function.  */

void Goexit (void) asm ("libgo_runtime.runtime.Goexit");

void
Goexit (void)
{
  remove_current_thread ();
  pthread_exit (NULL);
  abort ();
}

/* Implement the go statement.  */

void
__go_go (void (*pfn) (void*), void *arg)
{
  int i;
  pthread_attr_t attr;
  struct M *newm;
  struct __go_thread_id *list_entry;
  pthread_t tid;

  i = pthread_attr_init (&attr);
  __go_assert (i == 0);
  i = pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);
  __go_assert (i == 0);

#ifdef LINKER_SUPPORTS_SPLIT_STACK
  /* The linker knows how to handle calls between code which uses
     -fsplit-stack and code which does not.  That means that we can
     run with a smaller stack and rely on the -fsplit-stack support to
     save us.  The GNU/Linux glibc library won't let us have a very
     small stack, but we make it as small as we can.  */
#ifndef PTHREAD_STACK_MIN
#define PTHREAD_STACK_MIN 8192
#endif
  i = pthread_attr_setstacksize (&attr, PTHREAD_STACK_MIN);
  __go_assert (i == 0);
#endif

  newm = __go_alloc (sizeof (M));

  list_entry = malloc (sizeof (struct __go_thread_id));
  list_entry->prev = NULL;
  list_entry->next = NULL;
  list_entry->tentative = 1;
  list_entry->m = newm;
  list_entry->pfn = pfn;
  list_entry->arg = arg;

  newm->list_entry = list_entry;

  newm->mcache = runtime_allocmcache ();

  /* Add the thread to the list of all threads, marked as tentative
     since it is not yet ready to go.  */
  i = pthread_mutex_lock (&__go_thread_ids_lock);
  __go_assert (i == 0);

  if (__go_all_thread_ids != NULL)
    __go_all_thread_ids->prev = list_entry;
  list_entry->next = __go_all_thread_ids;
  __go_all_thread_ids = list_entry;

  i = pthread_mutex_unlock (&__go_thread_ids_lock);
  __go_assert (i == 0);

  /* Start the thread.  */
  i = pthread_create (&tid, &attr, start_go_thread, newm);
  __go_assert (i == 0);

  i = pthread_attr_destroy (&attr);
  __go_assert (i == 0);
}

/* This is the signal handler for GO_SIG_START.  The garbage collector
   will send this signal to a thread when it wants the thread to
   start.  We don't have to actually do anything here, but we need a
   signal handler since ignoring the signal will mean that the
   sigsuspend will never see it.  */

static void
gc_start_handler (int sig __attribute__ ((unused)))
{
}

/* Tell the garbage collector that we are ready, and wait for the
   garbage collector to tell us that it is done.  This may be called
   by a signal handler, so it is restricted to using functions which
   are async cancel safe.  */

static void
stop_for_gc (void)
{
  int i;

  /* Tell the garbage collector about our stack.  */
#ifdef USING_SPLIT_STACK
  m->gc_sp = __splitstack_find (NULL, NULL, &m->gc_len,
				&m->gc_next_segment, &m->gc_next_sp,
				&m->gc_initial_sp);
#else
  {
    uintptr_t top = (uintptr_t) m->gc_sp;
    uintptr_t bottom = (uintptr_t) &top;
    if (top < bottom)
      {
	m->gc_next_sp = m->gc_sp;
	m->gc_len = bottom - top;
      }
    else
      {
	m->gc_next_sp = (void *) bottom;
	m->gc_len = top - bottom;
      }
  }
#endif

  /* FIXME: Perhaps we should just move __go_panic_defer into M.  */
  m->gc_panic_defer = __go_panic_defer;

  /* Tell the garbage collector that we are ready by posting to the
     semaphore.  */
  i = sem_post (&__go_thread_ready_sem);
  __go_assert (i == 0);

  /* Wait for the garbage collector to tell us to continue.  */
  sigsuspend (&__go_thread_wait_sigset);
}

/* This is the signal handler for GO_SIG_STOP.  The garbage collector
   will send this signal to a thread when it wants the thread to
   stop.  */

static void
gc_stop_handler (int sig __attribute__ ((unused)))
{
  struct M *pm = m;

  if (__sync_bool_compare_and_swap (&pm->holds_finlock, 1, 1))
    {
      /* We can't interrupt the thread while it holds the finalizer
	 lock.  Otherwise we can get into a deadlock when mark calls
	 runtime_walkfintab.  */
      __sync_bool_compare_and_swap (&pm->gcing_for_finlock, 0, 1);
      return;
    }

  if (__sync_bool_compare_and_swap (&pm->mallocing, 1, 1))
    {
      /* m->mallocing was already non-zero.  We can't interrupt the
	 thread while it is running an malloc.  Instead, tell it to
	 call back to us when done.  */
      __sync_bool_compare_and_swap (&pm->gcing, 0, 1);
      return;
    }

  if (__sync_bool_compare_and_swap (&pm->nomemprof, 1, 1))
    {
      /* Similarly, we can't interrupt the thread while it is building
	 profiling information.  Otherwise we can get into a deadlock
	 when sweepspan calls MProf_Free.  */
      __sync_bool_compare_and_swap (&pm->gcing_for_prof, 0, 1);
      return;
    }

  stop_for_gc ();
}

/* This is called by malloc when it gets a signal during the malloc
   call itself.  */

int
__go_run_goroutine_gc (int r)
{
  /* Force callee-saved registers to be saved on the stack.  This is
     not needed if we are invoked from the signal handler, but it is
     needed if we are called directly, since otherwise we might miss
     something that a function somewhere up the call stack is holding
     in a register.  */
  __builtin_unwind_init ();

  stop_for_gc ();

  /* This avoids tail recursion, to make sure that the saved registers
     are on the stack.  */
  return r;
}

/* Stop all the other threads for garbage collection.  */

void
runtime_stoptheworld (void)
{
  int i;
  pthread_t me;
  int c;
  struct __go_thread_id *p;

  i = pthread_mutex_lock (&__go_thread_ids_lock);
  __go_assert (i == 0);

  me = pthread_self ();
  c = 0;
  p = __go_all_thread_ids;
  while (p != NULL)
    {
      if (p->tentative || pthread_equal (me, p->id))
	p = p->next;
      else
	{
	  i = pthread_kill (p->id, GO_SIG_STOP);
	  if (i == 0)
	    {
	      ++c;
	      p = p->next;
	    }
	  else if (i == ESRCH)
	    {
	      struct __go_thread_id *next;

	      /* This thread died somehow.  Remove it from the
		 list.  */
	      next = p->next;
	      if (p->prev != NULL)
		p->prev->next = next;
	      else
		__go_all_thread_ids = next;
	      if (next != NULL)
		next->prev = p->prev;
	      free (p);
	      p = next;
	    }
	  else
	    abort ();
	}
    }

  /* Wait for each thread to receive the signal and post to the
     semaphore.  If a thread receives the signal but contrives to die
     before it posts to the semaphore, then we will hang forever
     here.  */

  while (c > 0)
    {
      i = sem_wait (&__go_thread_ready_sem);
      if (i < 0 && errno == EINTR)
	continue;
      __go_assert (i == 0);
      --c;
    }

  /* The gc_panic_defer field should now be set for all M's except the
     one in this thread.  Set this one now.  */
  m->gc_panic_defer = __go_panic_defer;

  /* Leave with __go_thread_ids_lock held.  */
}

/* Scan all the stacks for garbage collection.  This should be called
   with __go_thread_ids_lock held.  */

void
__go_scanstacks (void (*scan) (byte *, int64))
{
  pthread_t me;
  struct __go_thread_id *p;

  /* Make sure all the registers for this thread are on the stack.  */
  __builtin_unwind_init ();

  me = pthread_self ();
  for (p = __go_all_thread_ids; p != NULL; p = p->next)
    {
      if (p->tentative)
	{
	  /* The goroutine function and argument can be allocated on
	     the heap, so we have to scan them for a thread that has
	     not yet started.  */
	  scan ((void *) &p->pfn, sizeof (void *));
	  scan ((void *) &p->arg, sizeof (void *));
	  scan ((void *) &p->m, sizeof (void *));
	  continue;
	}

#ifdef USING_SPLIT_STACK

      void *sp;
      size_t len;
      void *next_segment;
      void *next_sp;
      void *initial_sp;

      if (pthread_equal (me, p->id))
	{
	  next_segment = NULL;
	  next_sp = NULL;
	  initial_sp = NULL;
	  sp = __splitstack_find (NULL, NULL, &len, &next_segment,
				  &next_sp, &initial_sp);
	}
      else
	{
	  sp = p->m->gc_sp;
	  len = p->m->gc_len;
	  next_segment = p->m->gc_next_segment;
	  next_sp = p->m->gc_next_sp;
	  initial_sp = p->m->gc_initial_sp;
	}

      while (sp != NULL)
	{
	  scan (sp, len);
	  sp = __splitstack_find (next_segment, next_sp, &len,
				  &next_segment, &next_sp, &initial_sp);
	}

#else /* !defined(USING_SPLIT_STACK) */

      if (pthread_equal (me, p->id))
	{
	  uintptr_t top = (uintptr_t) m->gc_sp;
	  uintptr_t bottom = (uintptr_t) &top;
	  if (top < bottom)
	    scan (m->gc_sp, bottom - top);
	  else
	    scan ((void *) bottom, top - bottom);
	}
      else
	{
	  scan (p->m->gc_next_sp, p->m->gc_len);
	}
	
#endif /* !defined(USING_SPLIT_STACK) */

      /* Also scan the M structure while we're at it.  */

      scan ((void *) &p->m, sizeof (void *));
    }
}

/* Release all the memory caches.  This is called with
   __go_thread_ids_lock held.  */

void
__go_stealcache (void)
{
  struct __go_thread_id *p;

  for (p = __go_all_thread_ids; p != NULL; p = p->next)
    runtime_MCache_ReleaseAll (p->m->mcache);
}

/* Gather memory cache statistics.  This is called with
   __go_thread_ids_lock held.  */

void
__go_cachestats (void)
{
  struct __go_thread_id *p;

  for (p = __go_all_thread_ids; p != NULL; p = p->next)
    {
      MCache *c;

      c = p->m->mcache;
      mstats.heap_alloc += c->local_alloc;
      c->local_alloc = 0;
      mstats.heap_objects += c->local_objects;
      c->local_objects = 0;
    }
}

/* Start the other threads after garbage collection.  */

void
runtime_starttheworld (void)
{
  int i;
  pthread_t me;
  struct __go_thread_id *p;

  /* Here __go_thread_ids_lock should be held.  */

  me = pthread_self ();
  p = __go_all_thread_ids;
  while (p != NULL)
    {
      if (p->tentative || pthread_equal (me, p->id))
	p = p->next;
      else
	{
	  i = pthread_kill (p->id, GO_SIG_START);
	  if (i == 0)
	    p = p->next;
	  else
	    abort ();
	}
    }

  i = pthread_mutex_unlock (&__go_thread_ids_lock);
  __go_assert (i == 0);
}

/* Initialize the interaction between goroutines and the garbage
   collector.  */

void
__go_gc_goroutine_init (void *sp __attribute__ ((unused)))
{
  struct __go_thread_id *list_entry;
  int i;
  sigset_t sset;
  struct sigaction act;

  /* Add the initial thread to the list of all threads.  */

  list_entry = malloc (sizeof (struct __go_thread_id));
  list_entry->prev = NULL;
  list_entry->next = NULL;
  list_entry->tentative = 0;
  list_entry->id = pthread_self ();
  list_entry->m = m;
  list_entry->pfn = NULL;
  list_entry->arg = NULL;
  __go_all_thread_ids = list_entry;

  /* Initialize the semaphore which signals when threads are ready for
     GC.  */

  i = sem_init (&__go_thread_ready_sem, 0, 0);
  __go_assert (i == 0);

  /* Fetch the current signal mask.  */

  i = sigemptyset (&sset);
  __go_assert (i == 0);
  i = sigprocmask (SIG_BLOCK, NULL, &sset);
  __go_assert (i == 0);

  /* Make sure that GO_SIG_START is not blocked and GO_SIG_STOP is
     blocked, and save that set for use with later calls to sigsuspend
     while waiting for GC to complete.  */

  i = sigdelset (&sset, GO_SIG_START);
  __go_assert (i == 0);
  i = sigaddset (&sset, GO_SIG_STOP);
  __go_assert (i == 0);
  __go_thread_wait_sigset = sset;

  /* Block SIG_SET_START and unblock SIG_SET_STOP, and use that for
     the process signal mask.  */

  i = sigaddset (&sset, GO_SIG_START);
  __go_assert (i == 0);
  i = sigdelset (&sset, GO_SIG_STOP);
  __go_assert (i == 0);
  i = sigprocmask (SIG_SETMASK, &sset, NULL);
  __go_assert (i == 0);

  /* Install the signal handlers.  */
  memset (&act, 0, sizeof act);
  i = sigemptyset (&act.sa_mask);
  __go_assert (i == 0);

  act.sa_handler = gc_start_handler;
  act.sa_flags = SA_RESTART;
  i = sigaction (GO_SIG_START, &act, NULL);
  __go_assert (i == 0);

  /* We could consider using an alternate signal stack for this.  The
     function does not use much stack space, so it may be OK.  */
  act.sa_handler = gc_stop_handler;
  i = sigaction (GO_SIG_STOP, &act, NULL);
  __go_assert (i == 0);

#ifndef USING_SPLIT_STACK
  /* If we don't support split stack, record the current stack as the
     top of the stack.  */
  m->gc_sp = sp;
#endif
}
