/* Mudflap: narrow-pointer bounds-checking by tree rewriting.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Frank Ch. Eigler <fche@redhat.com>
   and Graydon Hoare <graydon@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"

#ifndef HAVE_SOCKLEN_T
#define socklen_t int
#endif

/* These attempt to coax various unix flavours to declare all our
   needed tidbits in the system headers.  */
#if !defined(__FreeBSD__) && !defined(__APPLE__)
#define _POSIX_SOURCE
#endif /* Some BSDs break <sys/socket.h> if this is defined. */
#define _GNU_SOURCE 
#define _XOPEN_SOURCE
#define _BSD_TYPES
#define __EXTENSIONS__
#define _ALL_SOURCE
#define _LARGE_FILE_API
#define _XOPEN_SOURCE_EXTENDED 1

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <sched.h>
#include <fcntl.h>

#include "mf-runtime.h"
#include "mf-impl.h"

#ifdef _MUDFLAP
#error "Do not compile this file with -fmudflap!"
#endif


/* Multithreading support hooks.  */



#ifndef LIBMUDFLAPTH
#error "pthreadstuff is to be included only in libmudflapth"
#endif



/* Describe a thread (dead or alive). */
struct pthread_info
{
  short used_p;  /* Is this slot in use?  */
  short dead_p;  /* Is this thread dead?  */
  pthread_t self; /* The thread id.  */

  /* If libmudflapth allocated the stack, store its adjusted base/size.  */
  void *stack;
  size_t stack_size;
  /* The _alloc fields store unadjusted values from the moment of allocation.  */
  void *stack_alloc;
  size_t stack_size_alloc;

  int *thread_errno;
  enum __mf_state_enum state;
};


/* Describe the startup information for a new user thread.  */
struct pthread_start_info
{
  /* The user's thread entry point and argument.  */
  void * (*user_fn)(void *);
  void *user_arg;

  /* Set by user thread when this startup struct may be disposed of.  */
  struct pthread_info *thread_info;
};




/* To avoid dynamic memory allocation, use static array to store these
   thread description structs.  The second (_idx) array is used as a
   simple caching hash table, mapping PTHREAD_HASH(thread) to its
   index in __mf_pthread_info[]. */

#define LIBMUDFLAPTH_THREADS_MAX 1024
static struct pthread_info __mf_pthread_info[LIBMUDFLAPTH_THREADS_MAX];
static unsigned __mf_pthread_info_idx[LIBMUDFLAPTH_THREADS_MAX];
#define PTHREAD_HASH(p) ((unsigned) (p) % LIBMUDFLAPTH_THREADS_MAX)


/* Find any old empty entry in __mf_pthread_info; mark it used and
   return it.  Return NULL if there are no more available slots.  */
struct pthread_info* 
__mf_allocate_blank_threadinfo (unsigned* idx)
{
  static unsigned probe = LIBMUDFLAPTH_THREADS_MAX-1;
  unsigned probe_at_start = probe;
  static pthread_mutex_t mutex =
#ifdef PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP
    PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP;
#else
    PTHREAD_MUTEX_INITIALIZER;
#endif
  int rc;

  rc = pthread_mutex_lock (& mutex);
  assert (rc == 0);

  /* Look for a blank spot starting one past the last one we found. */
  do
    {
      probe = (probe + 1) % LIBMUDFLAPTH_THREADS_MAX;
      struct pthread_info* pi = & __mf_pthread_info [probe];
      if (! pi->used_p)
	{
	  /* memset (pi, 0, sizeof (*pi)); */
	  pi->used_p = 1;
	  if (idx != NULL) *idx = probe;
	  /* VERBOSE_TRACE ("allocated threadinfo slot %u\n", probe); */
	  rc = pthread_mutex_unlock (& mutex);
	  assert (rc == 0);
	  return pi;
	}
    }
  while (probe != probe_at_start);
  
  rc = pthread_mutex_unlock (& mutex);
  assert (rc == 0);
  return NULL;
}


/* Find and return the pthread_info struct for the current thread.
   There might already be one in __mf_pthread_info for this thread, in
   which case return it.  There may not be one (if this is a main
   thread, an auxiliary -lpthread manager, or an actual user thread
   making an early call into libmudflap.  In these cases, create a new
   entry.  If not it's not the main thread, put it into reentrant
   initial state.

   NB: VERBOSE_TRACE type functions are not generally safe to call
   from this context, since a new thread might just be "booting up",
   making printf unsafe to call.
*/
static struct pthread_info* 
__mf_find_threadinfo ()
{
  pthread_t it = pthread_self ();
  unsigned *hash = & __mf_pthread_info_idx [PTHREAD_HASH (it)];
  struct pthread_info *result = NULL;
  static pthread_t last;
  static int main_thread_seen_p;

  /* Check out the lookup cache; failing that, do a linear search
     around the table.  */
  {
    struct pthread_info* pi = & __mf_pthread_info [*hash];
    unsigned i;

    if (pi->used_p && pi->self == it)
      result = pi;
    else for (i = 0; i < LIBMUDFLAPTH_THREADS_MAX; i++)
      {
	struct pthread_info* pi2 = & __mf_pthread_info [i];
	if (pi2->used_p && pi2->self == it) 
	  {
	    *hash = i;
	    result = pi2;
	    break;
	  }
      }
  }    

  if (result == NULL)
    {
      /* Create a __mf_pthread_info record for the main thread.  It's
	 different from the auto-recognized worker bees because for
	 example we can assume that it's a fully stack/errno-equipped
	 thread. */

      /* This must be the main thread, until now unseen in libmudflap.  */
      unsigned *hash = & __mf_pthread_info_idx [PTHREAD_HASH (it)];
      struct pthread_info* pi = __mf_allocate_blank_threadinfo (hash);
      assert (pi != NULL);
      assert (pi->used_p);
      result = pi;
      result->self = it;

      if (! main_thread_seen_p)
	{
	  result->state = active;
	  /* NB: leave result->thread_errno unset, as main thread's errno
	     has already been registered in __mf_init.  */
	  /* NB: leave stack-related fields unset, to avoid
	     deallocation.  */
	  main_thread_seen_p = 1;
	  /* VERBOSE_TRACE ("identified self as main thread\n"); */
	}
      else
	{
	  result->state = reentrant;
	  /* NB: leave result->thread_errno unset, as worker thread's
	     errno is unlikely to be used, and user threads fill them
	     in during __mf_pthread_spawn().  */
	  /* NB: leave stack-related fields unset, leaving pthread_create
	     to fill them in for user threads, leaving them empty for
	     other threads.  */
	  /* VERBOSE_TRACE ("identified self as new aux or user thread\n"); */
	}
    }

  if (last != it)
    {
      /*
      VERBOSE_TRACE ("found threadinfo for %u, slot %u\n", 
		     (unsigned) it,
		     (unsigned) *hash);
      */
      last = it;
    }

  assert (result != NULL);
  assert (result->self == it);

  return result;
}



/* Return a pointer to the per-thread __mf_state variable.  */
enum __mf_state_enum *
__mf_state_perthread ()
{
  assert (! __mf_starting_p);
  return & (__mf_find_threadinfo()->state);
}


static void 
__mf_pthread_cleanup (void *arg)
{
  struct pthread_info *pi = arg;

  /* XXX: This unregistration is not safe on platforms where distinct
     threads share errno (or at least its virtual address).  */
  if (pi->thread_errno != NULL)
    __mf_unregister (pi->thread_errno, sizeof (int), __MF_TYPE_GUESS);

  /* XXX: Only detached threads should designate themselves as dead
     here.  Non-detached threads are marked dead after their
     personalized pthread_join() call.  */
  pi->state = reentrant;
  pi->dead_p = 1;

  VERBOSE_TRACE ("thread pi %p exiting\n", pi);
}


static void *
__mf_pthread_spawner (void *arg)
{
  struct pthread_info *pi = __mf_find_threadinfo ();
  void *result = NULL;

  /* Turn off reentrancy indications.  */
  assert (pi->state == reentrant);
  pi->state = active;

  VERBOSE_TRACE ("new user thread\n");
  
  if (__mf_opts.heur_std_data)
    {
      pi->thread_errno = & errno;
      __mf_register (pi->thread_errno, sizeof (int), 
		     __MF_TYPE_GUESS, "errno area (thread)");
      /* NB: we could use __MF_TYPE_STATIC above, but we guess that
	 the thread errno is coming out of some dynamically allocated
	 pool that we already know of as __MF_TYPE_HEAP. */
    }

  /* We considered using pthread_key_t objects instead of these
     cleanup stacks, but they were less cooperative with the
     interposed malloc hooks in libmudflap.  */
  pthread_cleanup_push (& __mf_pthread_cleanup, pi);

  /* Call user thread */
  {
    /* Extract given entry point and argument.  */
    struct pthread_start_info *psi = arg;
    void * (*user_fn)(void *) = psi->user_fn;
    void *user_arg = psi->user_arg;

    /* Signal the main thread to resume.  */
    psi->thread_info = pi;
      
    result = (*user_fn)(user_arg);
  }

  pthread_cleanup_pop (1 /* execute */);

  /* NB: there is a slight race here.  The pthread_info field will now
     say this thread is dead, but it may still be running .. right
     here.  We try to check for this possibility using the
     pthread_kill test below. */

  return result;
}


#if PIC
/* A special bootstrap variant. */
int
__mf_0fn_pthread_create (pthread_t *thr, const pthread_attr_t *attr, 
			 void * (*start) (void *), void *arg)
{
  return -1;
}
#endif


#undef pthread_create
WRAPPER(int, pthread_create, pthread_t *thr, const pthread_attr_t *attr, 
	 void * (*start) (void *), void *arg)
{
  DECLARE(int, munmap, void *p, size_t l);
  DECLARE(void *, mmap, void *p, size_t l, int prot, int flags, int fd, off_t of);
  DECLARE(int, pthread_create, pthread_t *thr, const pthread_attr_t *attr, 
	  void * (*start) (void *), void *arg);
  int result;
  pthread_attr_t override_attr;
  void *override_stack;
  size_t override_stacksize;
  void *override_stack_alloc = (void *) 0;
  size_t override_stacksize_alloc = 0;
  unsigned i;

  TRACE ("pthread_create\n");

  /* Garbage-collect dead threads' stacks.  */
  LOCKTH ();
  for (i = 0; i < LIBMUDFLAPTH_THREADS_MAX; i++)
    {
      struct pthread_info *pi = & __mf_pthread_info [i];
      if (! pi->used_p)
	continue;
      if (! pi->dead_p)
	continue;

      /* VERBOSE_TRACE ("thread %u pi %p stack cleanup deferred (%u)\n",
	 (unsigned) pi->self, pi, pi->dead_p); */
	      
      /* Delay actual deallocation by a few cycles, try to discourage the
	 race mentioned at the end of __mf_pthread_spawner().  */
      if (pi->dead_p)
	pi->dead_p ++;
      if (pi->dead_p >= 10 /* XXX */)
	{
	  if (pi->stack)
	    CALL_REAL (munmap, pi->stack_alloc, pi->stack_size_alloc);

	  VERBOSE_TRACE ("slot %u freed, stack %p\n", i, pi->stack_alloc);
	  memset (pi, 0, sizeof (*pi));

	  /* One round of garbage collection is enough.  */
	  break;
	}
    }
  UNLOCKTH ();

  /* Let's allocate a stack for this thread, if one is not already
     supplied by the caller.  We don't want to let e.g. the
     linuxthreads manager thread do this allocation.  */
  if (attr != NULL)
    override_attr = *attr;
  else
    pthread_attr_init (& override_attr);

  /* Get supplied attributes, if any.  */
  /* XXX: consider using POSIX2K attr_getstack() */
  if (pthread_attr_getstackaddr (& override_attr, & override_stack) != 0 ||
      pthread_attr_getstacksize (& override_attr, & override_stacksize) != 0)
    {
      override_stack = NULL;
      override_stacksize = 0;
    }

  /* Do we need to allocate the new thread's stack?  */
  if (__mf_opts.thread_stack && override_stack == NULL)
    {
      uintptr_t alignment = 256; /* power of two */

      /* Perturb the initial stack addresses slightly, to encourage
	 threads to have nonconflicting entries in the lookup cache
	 for their tracked stack objects.  */
      static unsigned perturb = 0;
      const unsigned perturb_delta = 32;
      const unsigned perturb_count = 16;
      perturb += perturb_delta;
      if (perturb > perturb_delta*perturb_count) perturb = 0;

      /* Use glibc x86 defaults */
/* Should have been defined in <limits.h> */
#ifndef PTHREAD_STACK_MIN
#define PTHREAD_STACK_MIN 65536
#endif
      override_stacksize = max (PTHREAD_STACK_MIN, __mf_opts.thread_stack * 1024);


#if defined(MAP_ANONYMOUS)
#define MF_MAP_ANON MAP_ANONYMOUS
#elif defined(MAP_ANON)
#define MF_MAP_ANON MAP_ANON
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

#ifdef MF_MAP_ANON
      override_stack = CALL_REAL (mmap, NULL, override_stacksize, 
				  PROT_READ|PROT_WRITE, 
				  MAP_PRIVATE|MF_MAP_ANON,
				  0, 0);
#else
      /* Try mapping /dev/zero instead.  */
      {
        static int zerofd = -1;
        if (zerofd == -1)
          zerofd = open ("/dev/zero", O_RDWR);
        if (zerofd == -1)
          override_stack = MAP_FAILED;
        else
          override_stack = CALL_REAL (mmap, NULL, override_stacksize, 
                                      PROT_READ|PROT_WRITE, 
                                      MAP_PRIVATE, zerofd, 0);
      }
#endif

      if (override_stack == 0 || override_stack == MAP_FAILED)
	{
	  errno = EAGAIN;
	  return -1;
	}

      VERBOSE_TRACE ("thread stack alloc %p size %lu\n", 
		     override_stack, (unsigned long) override_stacksize);

      /* Save the original allocated values for later deallocation.  */
      override_stack_alloc = override_stack;
      override_stacksize_alloc = override_stacksize;

      /* The stackaddr pthreads attribute is a candidate stack pointer.
	 It must point near the top or the bottom of this buffer, depending
	 on whether stack grows downward or upward, and suitably aligned.
	 On the x86, it grows down, so we set stackaddr near the top.  */
      /* XXX: port logic */
      override_stack = (void *)
	(((uintptr_t) override_stack + override_stacksize - alignment - perturb)
	 & (~(uintptr_t)(alignment-1)));
      
      /* XXX: consider using POSIX2K attr_setstack() */
      if (pthread_attr_setstackaddr (& override_attr, override_stack) != 0 ||
	  pthread_attr_setstacksize (& override_attr, 
				     override_stacksize - alignment - perturb) != 0)
	{
	  /* This should not happen.  */
	  CALL_REAL (munmap, override_stack, override_stacksize);
	  errno = EAGAIN;
	  return -1;
	}
  }

  /* Actually start the child thread.  */
  {
    struct pthread_start_info psi;
    struct pthread_info *pi = NULL;
    
    /* Fill in startup-control fields.  */
    psi.user_fn = start;
    psi.user_arg = arg;
    psi.thread_info = NULL;
    
    /* Actually create the thread.  */
    __mf_state = reentrant;
    result = CALL_REAL (pthread_create, thr, & override_attr,
			& __mf_pthread_spawner, (void *) & psi);
    __mf_state = active;
    /* We also hook pthread_join/pthread_exit to get into reentrant
       mode during thread shutdown/cleanup.  */

    /* Wait until child thread has progressed far enough into its
       __mf_pthread_spawner() call.  */
    while (1) /* XXX: timeout? */
      {
	volatile struct pthread_start_info *psip = & psi;
	pi = psip->thread_info;
	if (pi != NULL) 
	  break;
	sched_yield ();
      }

    /* Fill in remaining fields in pthread_info. */
    pi->stack = override_stack;
    pi->stack_size = override_stacksize;
    pi->stack_alloc = override_stack_alloc;
    pi->stack_size_alloc = override_stacksize_alloc;
    /* XXX: this might be too late for future heuristics that attempt
       to use thread stack bounds.  We may need to put the new thread
       to sleep. */
  }


  /* May need to clean up if we created a pthread_attr_t of our own.  */
  if (attr == NULL)
    pthread_attr_destroy (& override_attr); /* NB: this shouldn't deallocate stack */

  return result;
}



#if PIC
/* A special bootstrap variant. */
int
__mf_0fn_pthread_join (pthread_t thr, void **rc)
{
  return -1;
}
#endif


#undef pthread_join
WRAPPER(int, pthread_join, pthread_t thr, void **rc)
{
  DECLARE(int, pthread_join, pthread_t thr, void **rc);
  int result;

  TRACE ("pthread_join\n");
  __mf_state = reentrant;
  result = CALL_REAL (pthread_join, thr, rc);
  __mf_state = active;
  
  return result;
}


#if PIC
/* A special bootstrap variant. */
void
__mf_0fn_pthread_exit (void *rc)
{
}
#endif


#undef pthread_exit
WRAPPER(void, pthread_exit, void *rc)
{
  DECLARE(void, pthread_exit, void *rc);

  TRACE ("pthread_exit\n");
  /* __mf_state = reentrant; */
  CALL_REAL (pthread_exit, rc);
  /* NOTREACHED */
  exit (0);  /* Satisfy noreturn attribute of pthread_exit.  */
}
