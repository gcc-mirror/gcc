/* Thread library support for -fsplit-stack.  */
/* Copyright (C) 2009-2016 Free Software Foundation, Inc.
   Contributed by Ian Lance Taylor <iant@google.com>.

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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

/* If inhibit_libc is defined, we can not compile this file.  The
   effect is that people will not be able to use -fsplit-stack.  That
   is much better than failing the build particularly since people
   will want to define inhibit_libc while building a compiler which
   can build glibc.  */

#ifndef inhibit_libc

#include <errno.h>
#include <pthread.h>

#include "generic-morestack.h"

/* We declare the pthread functions we need as weak, so that
   libgcc_s.so does not need to be linked against -lpthread.  */

extern int pthread_once (pthread_once_t *, void (*) (void))
  __attribute__ ((weak));

extern int pthread_key_create (pthread_key_t *, void (*) (void *))
  __attribute__ ((weak));

extern int pthread_setspecific (pthread_key_t, const void *)
  __attribute__ ((weak));

/* The key for the list of stack segments to free when the thread
   exits.  This is created by pthread_key_create.  */

static pthread_key_t segment_list_key;

/* Used to only run create_key once.  */

static pthread_once_t create_key_once = PTHREAD_ONCE_INIT;

/* Release all the segments for a thread.  This is the destructor
   function used by pthread_key_create, and is called when a thread
   exits.  */

static void
free_segments (void* arg)
{
  __morestack_release_segments ((struct stack_segment **) arg, 1);
}

/* Set up the key for the list of segments.  This is called via
   pthread_once.  */

static void
create_key (void)
{
  int err;

  err = pthread_key_create (&segment_list_key, free_segments);
  if (err != 0)
    {
      static const char msg[] = "pthread_key_create failed: errno ";
      __morestack_fail (msg, sizeof msg - 1, err);
    }
}

/* Pass information from the pthread_create wrapper to
   stack_split_initialize_thread.  */

struct pthread_create_args
{
  void *(*start_routine) (void *);
  void *arg;
};

/* Initialize a thread.  This is called via pthread_create.  It calls
   a target dependent function to set up any required stack guard.  */

static void* stack_split_initialize_thread (void *)
  __attribute__ ((no_split_stack));

static void *
stack_split_initialize_thread (void *varg)
{
  struct pthread_create_args *args = (struct pthread_create_args *) varg;
  int err;
  void *(*start_routine) (void *);
  void *arg;

  __stack_split_initialize ();

  err = pthread_setspecific (segment_list_key, (void *) &__morestack_segments);
  if (err != 0)
    {
      static const char msg[] = "pthread_setspecific failed: errno ";
      __morestack_fail (msg, sizeof msg - 1, err);
    }

  start_routine = args->start_routine;
  arg = args->arg;
  free (args);
  return (*start_routine) (arg);
}

/* This function wraps calls to pthread_create to make sure that the
   stack guard is initialized for new threads.  FIXME: This hack will
   not be necessary if glibc supports -fsplit-stack directly.  */

int __wrap_pthread_create (pthread_t *, const pthread_attr_t *,
			   void *(*start_routine) (void *), void *)
  __attribute__ ((visibility ("hidden")));

extern int __real_pthread_create (pthread_t *, const pthread_attr_t *,
				  void *(*start_routine) (void *), void *)
  __attribute__ ((weak));

int
__wrap_pthread_create (pthread_t *tid, const pthread_attr_t *attr,
		       void *(*start_routine) (void *), void *arg)
{
  int err;
  struct pthread_create_args* args;

  err = pthread_once (&create_key_once, create_key);
  if (err != 0)
    {
      static const char msg[] = "pthread_once failed: errno ";
      __morestack_fail (msg, sizeof msg - 1, err);
    }

  args = malloc (sizeof (struct pthread_create_args));
  if (args == NULL)
    return EAGAIN;
  args->start_routine = start_routine;
  args->arg = arg;
  return __real_pthread_create (tid, attr, stack_split_initialize_thread, args);
}

#endif /* !defined (inhibit_libc) */
