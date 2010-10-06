/* GNU Objective-C Runtime API.
   Copyright (C) 2010 Free Software Foundation, Inc.
   Contributed by Nicola Pero <nicola.pero@meta-innovation.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef __objc_runtime_INCLUDE_GNU
#define __objc_runtime_INCLUDE_GNU

#include "objc.h"

/* The following is temporary, until all code from objc-api.h has been
   moved into this file and objc-api.h will include runtime.h.  */
#include "objc-api.h"

/* 'objc_enumerationMutation()' is called when a collection is
   mutated while being "fast enumerated".  That is a hard error, and
   objc_enumerationMutation is called to deal with it.  'collection'
   is the collection object that was mutated during an enumeration.

   objc_enumerationMutation() will invoke the mutation handler if any
   is set.  Then, it will abort the program.

   Compatibility note: the Apple runtime will not abort the program
   after calling the mutation handler.
 */
objc_EXPORT void objc_enumerationMutation (id collection);

/* 'objc_set_enumeration_mutation_handler' can be used to set a
   function that will be called (instead of aborting) when a fast
   enumeration is mutated during enumeration.  The handler will be
   called with the 'collection' being mutated as the only argument and
   it should not return; it should either exit the program, or could
   throw an exception.  The recommended implementation is to throw an
   exception - the user can then use exception handlers to deal with
   it.

   This function is not thread safe (other threads may be trying to
   invoke the enumeration mutation handler while you are changing it!)
   and should be called during during the program initialization
   before threads are started.  It is mostly reserved for "Foundation"
   libraries; in the case of GNUstep, GNUstep Base may be using this
   function to improve the standard enumeration mutation handling.
   You probably shouldn't use this function unless you are writing
   your own Foundation library.
*/
objc_EXPORT void objc_set_enumeration_mutation_handler (void (*handler)(id));

/* This structure (used during fast enumeration) is automatically
   defined by the compiler (it is as if this definition was always
   included in all Objective-C files).  Note that it is usually
   defined again with the name of NSFastEnumeration by "Foundation"
   libraries such as GNUstep Base.  And if NSFastEnumeration is
   defined, the compiler will use it instead of
   __objcFastEnumerationState when doing fast enumeration.
*/
/*
struct __objcFastEnumerationState
{
  unsigned long state;
  id            *itemsPtr;
  unsigned long *mutationsPtr;
  unsigned long extra[5];
};
*/
/* For compatibility with the Apple/NeXT runtime.  */
#define objc_setEnumerationMutationHandler objc_set_enumeration_mutation_handler

#endif
