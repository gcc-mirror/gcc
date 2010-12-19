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

#include "objc.h"
#include "objc-decls.h"

#ifdef __cplusplus
extern "C" {
#endif

/* This file includes declarations of the messaging functions and
   types.  */

/* Compatibility note: the messaging function is one area where the
   GNU runtime and the Apple/NeXT runtime differ significantly.  If
   you can, it is recommended that you use higher-level facilities
   (provided by a Foundation library such as GNUstep Base) to perform
   forwarding or other advanced messaging tricks.  */

/* This function returns the IMP (C function implementing a method) to
   use to invoke the method with selector 'op' of receiver 'receiver'.

   This is the function used by the compiler when compiling method
   invocations with the GNU runtime.  For example, the method call

     result = [receiver method];

   is compiled by the compiler (with the GNU runtime) into the
   equivalent of:

   {
     IMP function = objc_msg_lookup (receiver, @selector (method));
     result = function (receiver, @selector (method));
   }

   so, a call to objc_msg_lookup() determines the IMP (the C function
   implementing the method) to call.  Then, the function is called.
   If the method takes or returns different arguments, the compiler
   will cast 'function' to the right type before invoking it, making
   sure arguments and return value are handled correctly.

   objc_msg_lookup() must always return a valid function that can be
   called with the required method signature (otherwise the
   compiler-generated code shown above could segfault).  If 'receiver'
   is NULL, objc_msg_lookup() returns a C function that does nothing,
   ignores all its arguments, and returns NULL (see nil_method.c).  If
   'receiver' does not respond to the selector 'op', objc_msg_lookup()
   will try to call +resolveClassMethod: or resolveInstanceMethod: as
   appropriate, and if they return YES, it will try the lookup again
   (+resolveClassMethod: and +resolveInstanceMethod: can thus install
   dynamically methods as they are requested).  If
   +resolveClassMethod: or +resolveInstanceMethod: are either not
   available, or return NO, or return YES but 'receiver' still doesn't
   implement the 'selector' after calling them, the runtime returns a
   generic "forwarding" function that can be called with the required
   method signature and which can process the method invocation
   according to the forwarding API.  There are two runtime hooks that
   allow Foundation libraries (such as GNUstep-Base) to return their
   own forwarding function in preference to the runtime ones.  When
   that happens, the Foundation library effectively takes complete
   control of the forwarding process; any method invocation where the
   selector is not implemented by the receiver will end up calling a
   forwarding function chosen by the Foundation library.  */
objc_EXPORT IMP objc_msg_lookup (id receiver, SEL op);

/* Structure used when a message is send to a class's super class.
   The compiler generates one of these structures and passes it to
   objc_msg_lookup_super() when a [super method] call is compiled.  */

/* In the traditional API, the super class field is called 'class' in
   Objective-C and 'super_class' in Objective-C++.  In the new API
   (objc/runtime.h) it is always called 'super_class'.  We detect the
   "traditional API" by the fact that the objc/objc-api.h header
   include guards are defined, which means objc/objc-api.h has been
   included.  This works because objc/message.h does not exist in the
   Traditional API and is only read because objc-api.h itself includes
   it.  */
#ifdef __objc_api_INCLUDE_GNU
/* Traditional API.  */
typedef struct objc_super
{
  id    self;       /* Id of the object sending the message. */
#ifdef __cplusplus
  Class super_class;
#else
  Class class;        /* Object's super class. */
#endif
} Super, *Super_t;
#else
/* Modern API.  */
struct objc_super
{
  id    self;        /* The receiver of the message.  */
  Class super_class; /* The superclass of the receiver.  */
};
#endif

/* This is used by the compiler instead of objc_msg_lookup () when
   compiling a call to 'super', such as [super method].  This requires
   sending a message to super->self, but looking up the method as if
   super->self was in class super->super_class.  */
objc_EXPORT IMP objc_msg_lookup_super (struct objc_super *super, SEL sel);

/* Hooks for method forwarding.  They make it easy to substitute the
   built-in forwarding with one based on a library, such as ffi, that
   implement closures, thereby avoiding gcc's __builtin_apply
   problems.  __objc_msg_forward2's result will be preferred over that
   of __objc_msg_forward if both are set and return non-NULL.  */   
objc_EXPORT IMP (*__objc_msg_forward)(SEL);
objc_EXPORT IMP (*__objc_msg_forward2)(id, SEL);

#ifdef __cplusplus
}
#endif

#endif /* not __objc_message_INCLUDE_GNU */
