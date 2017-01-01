/* GNU Objective C Runtime @synchronized implementation
   Copyright (C) 2010-2017 Free Software Foundation, Inc.
   Contributed by Nicola Pero <nicola.pero@meta-innovation.com>

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

#ifndef __objc_sync_INCLUDE_GNU
#define __objc_sync_INCLUDE_GNU

#include "objc.h"
#include "objc-decls.h"

#ifdef __cplusplus
extern "C" {
#endif

/* These functions are automatically called by @synchronized().  */

/* 'objc_sync_enter' is automatically called when entering a
   @synchronized() block.  It locks the recursive lock associated with
   'object'.  If 'object' is nil, it does nothing.  It returns
   OBJC_SYNC_SUCCESS on success; see the enumeration below for error
   values.
 
   Note that you should not rely on the behaviour when 'object' is nil
   because it could change.  */
objc_EXPORT int objc_sync_enter (id object);

/* 'objc_sync_exit' is automatically called when exiting from a
   @synchronized() block.  It unlocks the recursive lock associated
   with 'object'.  If 'object' is nil, it does nothing.  It returns
   OBJC_SYNC_SUCCESS on success; see the enumeration below for error
   values.  */
objc_EXPORT int objc_sync_exit (id object);

/* All the possible return values for objc_sync_enter() and
   objc_sync_exit().
 */
enum {
  OBJC_SYNC_SUCCESS = 0,
  OBJC_SYNC_NOT_OWNING_THREAD_ERROR = -1,
  OBJC_SYNC_TIMED_OUT = -2,
  OBJC_SYNC_NOT_INITIALIZED = -3
};

#ifdef __cplusplus
}
#endif

#endif /* not __objc_sync_INCLUDE_GNU */
