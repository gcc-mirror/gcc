/* GNU Objective C Runtime messaging declarations
   Copyright (C) 1993, 1995, 1996, 2004, 2009, 
   2010 Free Software Foundation, Inc.

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

#ifndef __objc_message_INCLUDE_GNU
#define __objc_message_INCLUDE_GNU

#ifdef __cplusplus
extern "C" {
#endif

#include "objc.h"
#include "objc-decls.h"

/* This file includes declarations of the messaging functions and
   types.
*/

/* Compatibility note: the messaging function is one area where the
   GNU runtime and the Apple/NeXT runtime differ significantly.  If
   you can, it is recommended that you use higher-level facilities
   (provided by a Foundation library such as GNUstep Base) to perform
   forwarding or other advanced messaging tricks.
*/

typedef void* retval_t;		/* return value */
typedef void(*apply_t)(void);	/* function pointer */
typedef union arglist {
  char *arg_ptr;
  char arg_regs[sizeof (char*)];
} *arglist_t;			/* argument frame */

objc_EXPORT IMP objc_msg_lookup(id receiver, SEL op);

/*
 * Structure used when a message is send to a class's super class.
 * The compiler generates one of these structures and passes it to
 * objc_msg_lookup_super.
 */
typedef struct objc_super {
  id      self;       /* Id of the object sending the message. */
#ifdef __cplusplus
  /* The new version of the API will always use 'super_class'.  */
  Class super_class;
#else
  Class class;        /* Object's super class. */
#endif
} Super, *Super_t;

objc_EXPORT IMP objc_msg_lookup_super(Super_t super, SEL sel);

objc_EXPORT retval_t objc_msg_sendv(id, SEL, arglist_t);

/*
 * Hooks for method forwarding. This makes it easy to substitute a
 * library, such as ffcall, that implements closures, thereby avoiding
 * gcc's __builtin_apply problems.  __objc_msg_forward2's result will
 * be preferred over that of __objc_msg_forward if both are set and
 * return non-NULL.
 *
 * TODO: The API should define objc_set_msg_forward_handler () or
 * similar instead of these hooks.
 */
objc_EXPORT IMP (*__objc_msg_forward)(SEL);
objc_EXPORT IMP (*__objc_msg_forward2)(id, SEL);

#ifdef __cplusplus
}
#endif

#endif /* not __objc_message_INCLUDE_GNU */
