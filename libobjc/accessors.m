/* GNU Objective C Runtime accessors functions
   Copyright (C) 2010-2019 Free Software Foundation, Inc.
   Contributed by Nicola Pero

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "objc-private/common.h"
#include "objc/objc.h"
#include "objc/thr.h"
#include <string.h>                    /* For memcpy */

/* This file contains functions that the compiler uses when
   synthesizing accessors (getters/setters) for properties.  The
   functions are part of the ABI, but are meant to be used by the
   compiler and not by users; for this reason, they are not declared
   in public header files.  The compiler automatically generates
   declarations for these functions.  */

/* Properties can be "atomic", which requires protecting them from
   concurrency issues using a lock.  Unfortunately, we can't have a
   lock for each property, so we'll go with a small pool of locks.
   Any time a property is accessed in an "atomic" way, we pick a
   random lock from the pool (random, but always the same one for the
   same property of the same object) and use it to protect access to
   the property.

   The size of the pool is currently 16.  A bigger pool can help
   reduce contention, ie, reduce the chances that two threads,
   operating on unrelated properties, will have to wait for each other
   because the properties use the same lock.  16 seems big enough at
   the moment.  */
#define ACCESSORS_NUMBER_OF_LOCKS 16

#define ACCESSORS_HASH(POINTER) ((((size_t)POINTER >> 8) ^ (size_t)POINTER) & (ACCESSORS_NUMBER_OF_LOCKS - 1))

static objc_mutex_t accessors_locks[ACCESSORS_NUMBER_OF_LOCKS];

/* This is called at startup to setup the locks.  */
void
__objc_accessors_init (void)
{
  int i;

  for (i = 0; i < ACCESSORS_NUMBER_OF_LOCKS; i++)
    accessors_locks[i] = objc_mutex_allocate ();
}

/* The property accessors automatically call various methods from the
   Foundation library (eg, GNUstep-base).  These methods are not
   implemented here, but we need to declare them so we can compile the
   runtime.  The Foundation library will need to provide
   implementations of these methods (most likely in the root class,
   eg, NSObject) as the accessors only work with objects of classes
   that implement these methods.  */
@interface _libobjcNSObject
- (id) copyWithZone: (void *)zone;
- (id) mutableCopyWithZone: (void *)zone;
@end
#define COPY(X)         [((_libobjcNSObject *)(X)) copyWithZone: NULL]
#define MUTABLE_COPY(X) [((_libobjcNSObject *)(X)) mutableCopyWithZone: NULL]


#if OBJC_WITH_GC

#  define AUTORELEASE(X)  (X)
#  define RELEASE(X)
#  define RETAIN(X)       (X)

#else

@interface _libobjcNSObject (RetainReleaseMethods)
- (id) autorelease;
- (oneway void) release;
- (id) retain;
@end
#  define AUTORELEASE(X)  [((_libobjcNSObject *)(X)) autorelease]
#  define RELEASE(X)      [((_libobjcNSObject *)(X)) release]
#  define RETAIN(X)       [((_libobjcNSObject *)(X)) retain]

#endif

/* The compiler uses this function when implementing some synthesized
   getters for properties of type 'id'.  */
id
objc_getProperty (id self, SEL __attribute__((unused)) _cmd, ptrdiff_t offset, BOOL is_atomic)
{
  if (self != nil)
    {
      id *pointer_to_ivar = (id *)((char *)self + offset);


      if (is_atomic == NO)
	{
	  /* Note that in this case, we do not RETAIN/AUTORELEASE the
	     returned value.  The programmer should do it if it is
	     needed.  Since access is non-atomic, other threads can be
	     ignored and the caller has full control of what happens
	     to the object and whether it needs to be RETAINed or not,
	     so it makes sense to leave the decision to him/her.  This
	     is also what the Apple/NeXT runtime does.  */
	  return *pointer_to_ivar;
	}
      else
	{
	  objc_mutex_t lock = accessors_locks[ACCESSORS_HASH (pointer_to_ivar)];
	  id result;
	  
	  objc_mutex_lock (lock);
	  result = RETAIN (*(pointer_to_ivar));
	  objc_mutex_unlock (lock);
	  
	  return AUTORELEASE (result);
	}
    }

  return nil;
}

/* The compiler uses this function when implementing some synthesized
   setters for properties of type 'id'.

   PS: Note how 'should_copy' is declared 'BOOL' but then actually
   takes values from 0 to 2.  This hack was introduced by Apple; we
   do the same for compatibility reasons.  */
void
objc_setProperty (id self, SEL __attribute__((unused)) _cmd, ptrdiff_t offset, id new_value, BOOL is_atomic, BOOL should_copy)
{
  if (self != nil)
    {
      id *pointer_to_ivar = (id *)((char *)self + offset);
      id retained_value;
#if !OBJC_WITH_GC
      id old_value;
#endif

      switch (should_copy)
	{
	case 0: /* retain */
	  {
	    if (*pointer_to_ivar == new_value)
	      return;
	    retained_value = RETAIN (new_value);
	    break;
	  }
	case 2: /* mutable copy */
	  {
	    retained_value = MUTABLE_COPY (new_value);
	    break;
	  }
	case 1: /* copy */
	default:
	  {
	    retained_value = COPY (new_value);
	    break;
	  }
	}

      if (is_atomic == NO)
	{
#if !OBJC_WITH_GC
	  old_value = *pointer_to_ivar;
#endif
	  *pointer_to_ivar = retained_value;
	}
      else
	{
	  objc_mutex_t lock = accessors_locks[ACCESSORS_HASH (pointer_to_ivar)];

	  objc_mutex_lock (lock);
#if !OBJC_WITH_GC
	  old_value = *pointer_to_ivar;
#endif
	  *pointer_to_ivar = retained_value;
	  objc_mutex_unlock (lock);
	}
#if !OBJC_WITH_GC
      RELEASE (old_value);
#endif
    }
}

/* The compiler uses this function when implementing some synthesized
   getters for properties of arbitrary C types.  The data is just
   copied.  Compatibility Note: this function does not exist in the
   Apple/NeXT runtime.  */
void
objc_getPropertyStruct (void *destination, const void *source, ptrdiff_t size, BOOL is_atomic, BOOL __attribute__((unused)) has_strong)
{
  if (is_atomic == NO)
    memcpy (destination, source, size);
  else
    {
      objc_mutex_t lock = accessors_locks[ACCESSORS_HASH (source)];

      objc_mutex_lock (lock);
      memcpy (destination, source, size);
      objc_mutex_unlock (lock);
    }
}

/* The compiler uses this function when implementing some synthesized
   setters for properties of arbitrary C types.  The data is just
   copied.  Compatibility Note: this function does not exist in the
   Apple/NeXT runtime.  */
void
objc_setPropertyStruct (void *destination, const void *source, ptrdiff_t size, BOOL is_atomic, BOOL __attribute__((unused)) has_strong)
{
  if (is_atomic == NO)
    memcpy (destination, source, size);
  else
    {
      objc_mutex_t lock = accessors_locks[ACCESSORS_HASH (destination)];

      objc_mutex_lock (lock);
      memcpy (destination, source, size);
      objc_mutex_unlock (lock);
    }
}

/* This is the function that the Apple/NeXT runtime has instead of
   objc_getPropertyStruct and objc_setPropertyStruct.  We include it
   for API compatibility (just for people who may have used
   objc_copyStruct on the NeXT runtime thinking it was a public API);
   the compiler never generates calls to it with the GNU runtime.
   This function is clumsy because it requires two locks instead of
   one.  */
void
objc_copyStruct (void *destination, const void *source, ptrdiff_t size, BOOL is_atomic, BOOL __attribute__((unused)) has_strong)
{
  if (is_atomic == NO)
    memcpy (destination, source, size);
  else
    {
      /* We don't know which one is the property, so we have to lock
	 both.  One of them is most likely a temporary buffer in the
	 local stack and we really wouldn't want to lock it (our
	 objc_getPropertyStruct and objc_setPropertyStruct functions
	 don't lock it).  Note that if we're locking more than one
	 accessor lock at once, we need to always lock them in the
	 same order to avoid deadlocks.  */
      objc_mutex_t first_lock;
      objc_mutex_t second_lock;

      if (ACCESSORS_HASH (source) == ACCESSORS_HASH (destination))
	{
	  /* A lucky collision.  */
	  first_lock = accessors_locks[ACCESSORS_HASH (source)];
	  objc_mutex_lock (first_lock);
	  memcpy (destination, source, size);
	  objc_mutex_unlock (first_lock);
	  return;
	}

      if (ACCESSORS_HASH (source) > ACCESSORS_HASH (destination))
	{
	  first_lock = accessors_locks[ACCESSORS_HASH (source)];
	  second_lock = accessors_locks[ACCESSORS_HASH (destination)];
	}
      else
	{
	  first_lock = accessors_locks[ACCESSORS_HASH (destination)];
	  second_lock = accessors_locks[ACCESSORS_HASH (source)];	  
	}

      objc_mutex_lock (first_lock);
      objc_mutex_lock (second_lock);
      memcpy (destination, source, size);
      objc_mutex_unlock (second_lock);
      objc_mutex_unlock (first_lock);
    }
}
