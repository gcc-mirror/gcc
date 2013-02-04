/* GNU Objective C Runtime class related functions
   Copyright (C) 1993-2013 Free Software Foundation, Inc.
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

#include "objc-private/common.h"
#include "objc/runtime.h"
#include "objc/thr.h"                   /* Required by objc-private/runtime.h.  */
#include "objc-private/module-abi-8.h"  /* For CLS_ISCLASS and similar.  */
#include "objc-private/runtime.h"	/* the kitchen sink */

#include <string.h>                     /* For memcpy()  */

#if OBJC_WITH_GC
# include <gc.h>
# include <gc_typed.h>
#endif

/* FIXME: The semantics of extraBytes are not really clear.  */
inline
id
class_createInstance (Class class, size_t extraBytes)
{
  id new = nil;

#if OBJC_WITH_GC
  if (CLS_ISCLASS (class))
    new = (id) GC_malloc_explicitly_typed (class->instance_size + extraBytes,
					   (GC_descr)class->gc_object_type);
#else
  if (CLS_ISCLASS (class))
    new = (id) objc_calloc (class->instance_size + extraBytes, 1);
#endif

  if (new != nil)
    {
      /* There is no need to zero the memory, since both
	 GC_malloc_explicitly_typed and objc_calloc return zeroed
	 memory.  */
      new->class_pointer = class;
    }

  /* TODO: Invoke C++ constructors on all appropriate C++ instance
     variables of the new object.  */

  return new;
}

/* Traditional GNU Objective-C Runtime API.  */
id
object_copy (id object, size_t extraBytes)
{
  if ((object != nil) && CLS_ISCLASS (object->class_pointer))
    {
      /* TODO: How should it work with C++ constructors ? */
      id copy = class_createInstance (object->class_pointer, extraBytes);
      memcpy (copy, object, object->class_pointer->instance_size + extraBytes);
      return copy;
    }
  else
    return nil;
}

id
object_dispose (id object)
{
  if ((object != nil) && CLS_ISCLASS (object->class_pointer))
    {
      /* TODO: Invoke C++ destructors on all appropriate C++ instance
	 variables.  But what happens with the garbage collector ?
	 Would object_dispose() be ever called in that case ?  */

      objc_free (object);
    }
  return nil;
}

const char *
object_getClassName (id object)
{
  if (object != nil)
    return object->class_pointer->name;
  else
    return "Nil";
}

Class
object_setClass (id object, Class class_)
{
  if (object == nil)
    return Nil;
  else
    {
      Class old_class = object->class_pointer;

      object->class_pointer = class_;
      return old_class;
    }
}
