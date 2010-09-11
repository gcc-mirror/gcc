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

/* This file includes declarations of the messaging functions and types.  */

typedef void* retval_t;		/* return value */
typedef void(*apply_t)(void);	/* function pointer */
typedef union arglist {
  char *arg_ptr;
  char arg_regs[sizeof (char*)];
} *arglist_t;			/* argument frame */

IMP objc_msg_lookup(id receiver, SEL op);

/* TODO: Add the remaining messaging declarations from objc-api.h.  */

#ifdef __cplusplus
}
#endif

#endif /* not __objc_message_INCLUDE_GNU */
