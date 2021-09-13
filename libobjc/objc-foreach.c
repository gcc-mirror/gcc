/* GNU Objective C Runtime 'fast enumeration' implementation
   Copyright (C) 2010-2021 Free Software Foundation, Inc.
   Contributed by Nicola Pero <nicola.pero@meta-innovation.com>

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

/* This file implements objc_enumeration_mutation() and
   objc_set_enumeration_mutation_handler(), the two functions required
   to handle mutations during a fast enumeration.  */
#include "objc-private/common.h"
#include "objc-private/error.h"     /* For _objc_abort() */
#include "objc/runtime.h"           /* For objc_enumerationMutation() and objc_set_enumeration_mutation_handler() */

/* The enumeration mutation handler currently in use.  */
static void (*__objc_enumeration_mutation_handler)(id) = NULL;

void
objc_setEnumerationMutationHandler (void (*handler)(id))
{
  __objc_enumeration_mutation_handler = handler;
}

void
objc_enumerationMutation (id collection)
{
  if (__objc_enumeration_mutation_handler != NULL)
    (*__objc_enumeration_mutation_handler) (collection);

  /* We always abort if we get here; there is no point in going on as
     the next iteration in the fast enumeration would probably go
     deeply wrong.  */
  _objc_abort ("Collection %p mutated during fast enumeration", collection);
}  
