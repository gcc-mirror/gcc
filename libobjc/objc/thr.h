/* Thread and mutex controls for Objective C.
   Copyright (C) 1996-2018 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef __thread_INCLUDE_GNU
#define __thread_INCLUDE_GNU

#include "objc.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*************************************************************************
 *  Universal static variables:
 */
extern int __objc_thread_exit_status;      /* Global exit status.   */

/********
 *  Thread safe implementation types and functions.  
 */

/* Thread priorities */
#define OBJC_THREAD_INTERACTIVE_PRIORITY        2
#define OBJC_THREAD_BACKGROUND_PRIORITY         1
#define OBJC_THREAD_LOW_PRIORITY                0

/* A thread */
typedef void * objc_thread_t;

/* This structure represents a single mutual exclusion lock. */
struct objc_mutex
{
  volatile objc_thread_t owner;     /* Id of thread that owns. */
  volatile int depth;               /* # of acquires. */
  void * backend;                   /* Specific to backend */
};
typedef struct objc_mutex *objc_mutex_t;

/* This structure represents a single condition mutex */
struct objc_condition
{
  void * backend;                   /* Specific to backend */
};
typedef struct objc_condition *objc_condition_t;

/* Frontend mutex functions */
objc_mutex_t objc_mutex_allocate (void);
int objc_mutex_deallocate (objc_mutex_t mutex);
int objc_mutex_lock (objc_mutex_t mutex);
int objc_mutex_unlock (objc_mutex_t mutex);
int objc_mutex_trylock (objc_mutex_t mutex);

/* Frontend condition mutex functions */
objc_condition_t objc_condition_allocate (void);
int objc_condition_deallocate (objc_condition_t condition);
int objc_condition_wait (objc_condition_t condition, objc_mutex_t mutex);
int objc_condition_signal (objc_condition_t condition);
int objc_condition_broadcast (objc_condition_t condition);

/* Frontend thread functions */
objc_thread_t objc_thread_detach (SEL selector, id object, id argument);
void objc_thread_yield (void);
int objc_thread_exit (void);
int objc_thread_set_priority (int priority);
int objc_thread_get_priority (void);
void * objc_thread_get_data (void);
int objc_thread_set_data (void *value);
objc_thread_t objc_thread_id (void);
void objc_thread_add (void);
void objc_thread_remove (void);

/*
  Use this to set the hook function that will be called when the 
  runtime initially becomes multi threaded.
  The hook function is only called once, meaning only when the 
  2nd thread is spawned, not for each and every thread.

  It returns the previous hook function or NULL if there is none.

  A program outside of the runtime could set this to some function so
  it can be informed; for example, the GNUstep Base Library sets it 
  so it can implement the NSBecomingMultiThreaded notification.
  */
typedef void (*objc_thread_callback) (void);
objc_thread_callback objc_set_thread_callback (objc_thread_callback func);

/* Backend initialization functions */
int __objc_init_thread_system (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* not __thread_INCLUDE_GNU */
