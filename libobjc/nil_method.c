/* GNU Objective C Runtime nil receiver function
   Copyright (C) 1993-2023 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

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


/* This is the nil method, the function that is called when the receiver
   of a method is nil */

#include "objc-private/common.h"
#include "objc/objc.h"

/* When the receiver of a method invocation is nil, the runtime
   returns nil_method() as the method implementation.  This function
   will be casted to whatever function was supposed to be executed to
   execute that method (that function will take an id, followed by a
   SEL, followed by who knows what arguments, depends on the method),
   and executed.
   
   For this reason, nil_method() should be a function which can be
   called in place of any function taking an 'id' argument followed by
   a 'SEL' argument, followed by zero, or one, or any number of
   arguments (both a fixed number, or a variable number !).
   
   There is no "proper" implementation of such a nil_method function
   in C, however in all existing implementations it does not matter
   when extra arguments are present, so we can simply create a function
   taking a receiver and a selector, and all other arguments will be
   ignored. :-)
*/

id
nil_method (id receiver, SEL op __attribute__ ((__unused__)))
{
  return receiver;
}
