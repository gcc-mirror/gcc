/* GNU Objective C Runtime Thread Interface
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Galen C. Hunt (gchunt@cs.rochester.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
GNU CC; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include <stdlib.h>
#include "runtime.h"

/* Global exit status. */
int __objc_thread_exit_status = 0;

/* Flag which lets us know if we ever became multi threaded */
int __objc_is_multi_threaded = 0;

/* The hook function called when the runtime becomes multi threaded */
objc_thread_callback _objc_became_multi_threaded = NULL;

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
objc_thread_callback objc_set_thread_callback(objc_thread_callback func)
{
  objc_thread_callback temp = _objc_became_multi_threaded;
  _objc_became_multi_threaded = func;
  return temp;
}

/*
  Private functions

  These functions are utilized by the frontend, but they are not
  considered part of the public interface.
  */

/*
  First function called in a thread, starts everything else.

  This function is passed to the backend by objc_thread_detach
  as the starting function for a new thread.
 */
struct __objc_thread_start_state
{
  SEL selector;
  id object;
  id argument;
};

static volatile void
__objc_thread_detach_function(struct __objc_thread_start_state *istate)
{
  /* Valid state? */
  if (istate) {
    id (*imp)(id,SEL,id);
    SEL selector = istate->selector;
    id object   = istate->object;
    id argument = istate->argument;

    /* Don't need anymore so free it */
    objc_free(istate);

    /* Clear out the thread local storage */
    objc_thread_set_data(NULL);

    /* Check to see if we just became multi threaded */
    if (!__objc_is_multi_threaded)
      {
	__objc_is_multi_threaded = 1;

	/* Call the hook function */
	if (_objc_became_multi_threaded != NULL)
	  (*_objc_became_multi_threaded)();
      }

    /* Call the method */
    if ((imp = (id(*)(id, SEL, id))objc_msg_lookup(object, selector)))
	(*imp)(object, selector, argument);
    else
      objc_error(object, OBJC_ERR_UNIMPLEMENTED,
		 "objc_thread_detach called with bad selector.\n");
  }
  else
    objc_error(nil, OBJC_ERR_BAD_STATE,
	       "objc_thread_detach called with NULL state.\n");

  /* Exit the thread */
  objc_thread_exit();
}

/*
  Frontend functions

  These functions constitute the public interface to the Objective-C thread
  and mutex functionality.
  */

/* Frontend thread functions */

/*
  Detach a new thread of execution and return its id.  Returns NULL if fails.
  Thread is started by sending message with selector to object.  Message
  takes a single argument.
  */
objc_thread_t
objc_thread_detach(SEL selector, id object, id argument)
{
  struct __objc_thread_start_state *istate;
  objc_thread_t        thread_id = NULL;

  /* Allocate the state structure */
  if (!(istate = (struct __objc_thread_start_state *)
	objc_malloc(sizeof(*istate))))
    return NULL;

  /* Initialize the state structure */
  istate->selector = selector;
  istate->object = object;
  istate->argument = argument;

  /* lock access */
  objc_mutex_lock(__objc_runtime_mutex);

  /* Call the backend to spawn the thread */
  if ((thread_id = __objc_thread_detach((void *)__objc_thread_detach_function,
					istate)) == NULL)
    {
      /* failed! */
      objc_mutex_unlock(__objc_runtime_mutex);
      objc_free(istate);
      return NULL;
    }

  /* Increment our thread counter */
  __objc_runtime_threads_alive++;
  objc_mutex_unlock(__objc_runtime_mutex);

  return thread_id;
}

/* Set the current thread's priority. */
int
objc_thread_set_priority(int priority)
{
  /* Call the backend */
  return __objc_thread_set_priority(priority);
}

/* Return the current thread's priority. */
int
objc_thread_get_priority(void)
{
  /* Call the backend */
  return __objc_thread_get_priority();
}

/*
  Yield our process time to another thread.  Any BUSY waiting that is done
  by a thread should use this function to make sure that other threads can
  make progress even on a lazy uniprocessor system.
  */
void
objc_thread_yield(void)
{
  /* Call the backend */
  __objc_thread_yield();
}

/*
  Terminate the current tread.  Doesn't return.
  Actually, if it failed returns -1.
  */
int
objc_thread_exit(void)
{
  /* Decrement our counter of the number of threads alive */
  objc_mutex_lock(__objc_runtime_mutex);
  __objc_runtime_threads_alive--;
  objc_mutex_unlock(__objc_runtime_mutex);

  /* Call the backend to terminate the thread */
  return __objc_thread_exit();
}

/*
  Returns an integer value which uniquely describes a thread.  Must not be
  NULL which is reserved as a marker for "no thread".
  */
objc_thread_t
objc_thread_id(void)
{
  /* Call the backend */
  return __objc_thread_id();
}

/*
  Sets the thread's local storage pointer. 
  Returns 0 if successful or -1 if failed.
  */
int
objc_thread_set_data(void *value)
{
  /* Call the backend */
  return __objc_thread_set_data(value);
}

/*
  Returns the thread's local storage pointer.  Returns NULL on failure.
  */
void *
objc_thread_get_data(void)
{
  /* Call the backend */
  return __objc_thread_get_data();
}

/* Frontend mutex functions */

/*
  Allocate a mutex.  Return the mutex pointer if successful or NULL if the
  allocation failed for any reason.
  */
objc_mutex_t
objc_mutex_allocate(void)
{
  objc_mutex_t mutex;

  /* Allocate the mutex structure */
  if (!(mutex = (objc_mutex_t)objc_malloc(sizeof(struct objc_mutex))))
    return NULL;

  /* Call backend to create the mutex */
  if (__objc_mutex_allocate(mutex))
    {
      /* failed! */
      objc_free(mutex);
      return NULL;
    }

  /* Initialize mutex */
  mutex->owner = NULL;
  mutex->depth = 0;
  return mutex;
}

/*
  Deallocate a mutex.  Note that this includes an implicit mutex_lock to
  insure that no one else is using the lock.  It is legal to deallocate
  a lock if we have a lock on it, but illegal to deallocate a lock held
  by anyone else.
  Returns the number of locks on the thread.  (1 for deallocate).
  */
int
objc_mutex_deallocate(objc_mutex_t mutex)
{
  int depth;

  /* Valid mutex? */
  if (!mutex)
    return -1;

  /* Acquire lock on mutex */
  depth = objc_mutex_lock(mutex);

  /* Call backend to destroy mutex */
  if (__objc_mutex_deallocate(mutex))
    return -1;

  /* Free the mutex structure */
  objc_free(mutex);

  /* Return last depth */
  return depth;
}

/*
  Grab a lock on a mutex.  If this thread already has a lock on this mutex
  then we increment the lock count.  If another thread has a lock on the 
  mutex we block and wait for the thread to release the lock.
  Returns the lock count on the mutex held by this thread.
  */
int
objc_mutex_lock(objc_mutex_t mutex)
{
  objc_thread_t thread_id;
  int status;

  /* Valid mutex? */
  if (!mutex)
    return -1;

  /* If we already own the lock then increment depth */
  thread_id = __objc_thread_id();
  if (mutex->owner == thread_id)
    return ++mutex->depth;

  /* Call the backend to lock the mutex */
  status = __objc_mutex_lock(mutex);

  /* Failed? */
  if (status)
    return status;

  /* Successfully locked the thread */
  mutex->owner = thread_id;
  return mutex->depth = 1;
}

/*
  Try to grab a lock on a mutex.  If this thread already has a lock on
  this mutex then we increment the lock count and return it.  If another
  thread has a lock on the mutex returns -1.
  */
int
objc_mutex_trylock(objc_mutex_t mutex)
{
  objc_thread_t thread_id;
  int status;

  /* Valid mutex? */
  if (!mutex)
    return -1;

  /* If we already own the lock then increment depth */ 
  thread_id = __objc_thread_id();
  if (mutex->owner == thread_id)
    return ++mutex->depth;
    
  /* Call the backend to try to lock the mutex */
  status = __objc_mutex_trylock(mutex);

  /* Failed? */
  if (status)
    return status;

  /* Successfully locked the thread */
  mutex->owner = thread_id;
  return mutex->depth = 1;
}

/* 
  Unlocks the mutex by one level.
  Decrements the lock count on this mutex by one.
  If the lock count reaches zero, release the lock on the mutex.
  Returns the lock count on the mutex.
  It is an error to attempt to unlock a mutex which this thread 
  doesn't hold in which case return -1 and the mutex is unaffected.
  */
int
objc_mutex_unlock(objc_mutex_t mutex)
{
  objc_thread_t thread_id;
  int status;

  /* Valid mutex? */
  if (!mutex)
    return -1;

  /* If another thread owns the lock then abort */
  thread_id = __objc_thread_id();
  if (mutex->owner != thread_id)
    return -1;

  /* Decrement depth and return */
  if (mutex->depth > 1)
    return --mutex->depth;

  /* Depth down to zero so we are no longer the owner */
  mutex->depth = 0;
  mutex->owner = NULL;

  /* Have the backend unlock the mutex */
  status = __objc_mutex_unlock(mutex);

  /* Failed? */
  if (status)
    return status;

  return 0;
}

/* Frontend condition mutex functions */

/*
  Allocate a condition.  Return the condition pointer if successful or NULL
  if the allocation failed for any reason.
  */
objc_condition_t 
objc_condition_allocate(void)
{
  objc_condition_t condition;
    
  /* Allocate the condition mutex structure */
  if (!(condition = 
	(objc_condition_t)objc_malloc(sizeof(struct objc_condition))))
    return NULL;

  /* Call the backend to create the condition mutex */
  if (__objc_condition_allocate(condition))
    {
      /* failed! */
      objc_free(condition);
      return NULL;
    }

  /* Success! */
  return condition;
}

/*
  Deallocate a condition. Note that this includes an implicit 
  condition_broadcast to insure that waiting threads have the opportunity
  to wake.  It is legal to dealloc a condition only if no other
  thread is/will be using it. Here we do NOT check for other threads
  waiting but just wake them up.
  */
int
objc_condition_deallocate(objc_condition_t condition)
{
  /* Broadcast the condition */
  if (objc_condition_broadcast(condition))
    return -1;

  /* Call the backend to destroy */
  if (__objc_condition_deallocate(condition))
    return -1;

  /* Free the condition mutex structure */
  objc_free(condition);

  return 0;
}

/*
  Wait on the condition unlocking the mutex until objc_condition_signal()
  or objc_condition_broadcast() are called for the same condition. The
  given mutex *must* have the depth set to 1 so that it can be unlocked
  here, so that someone else can lock it and signal/broadcast the condition.
  The mutex is used to lock access to the shared data that make up the
  "condition" predicate.
  */
int
objc_condition_wait(objc_condition_t condition, objc_mutex_t mutex)
{
  objc_thread_t thread_id;

  /* Valid arguments? */
  if (!mutex || !condition)
    return -1;

  /* Make sure we are owner of mutex */
  thread_id = __objc_thread_id();
  if (mutex->owner != thread_id)
    return -1;

  /* Cannot be locked more than once */
  if (mutex->depth > 1)
    return -1;

  /* Virtually unlock the mutex */
  mutex->depth = 0;
  mutex->owner = (objc_thread_t)NULL;

  /* Call the backend to wait */
  __objc_condition_wait(condition, mutex);

  /* Make ourselves owner of the mutex */
  mutex->owner = thread_id;
  mutex->depth = 1;

  return 0;
}

/*
  Wake up all threads waiting on this condition. It is recommended that 
  the called would lock the same mutex as the threads in objc_condition_wait
  before changing the "condition predicate" and make this call and unlock it
  right away after this call.
  */
int
objc_condition_broadcast(objc_condition_t condition)
{
  /* Valid condition mutex? */
  if (!condition)
    return -1;

  return __objc_condition_broadcast(condition);
}

/*
  Wake up one thread waiting on this condition. It is recommended that 
  the called would lock the same mutex as the threads in objc_condition_wait
  before changing the "condition predicate" and make this call and unlock it
  right away after this call.
  */
int
objc_condition_signal(objc_condition_t condition)
{
  /* Valid condition mutex? */
  if (!condition)
    return -1;

  return __objc_condition_signal(condition);
}

/* Make the objc thread system aware that a thread which is managed
   (started, stopped) by external code could access objc facilities
   from now on.  This is used when you are interfacing with some
   external non-objc-based environment/system - you must call
   objc_thread_add() before an alien thread makes any calls to
   Objective-C.  Do not cause the _objc_became_multi_threaded hook to
   be executed. */
void 
objc_thread_add(void)
{
  objc_mutex_lock(__objc_runtime_mutex);
  __objc_is_multi_threaded = 1;
  __objc_runtime_threads_alive++;
  objc_mutex_unlock(__objc_runtime_mutex);  
}

/* Make the objc thread system aware that a thread managed (started,
   stopped) by some external code will no longer access objc and thus
   can be forgotten by the objc thread system.  Call
   objc_thread_remove() when your alien thread is done with making
   calls to Objective-C. */
void
objc_thread_remove(void)
{
  objc_mutex_lock(__objc_runtime_mutex);
  __objc_runtime_threads_alive--;
  objc_mutex_unlock(__objc_runtime_mutex);  
}

/* End of File */
