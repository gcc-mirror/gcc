/* GNU Objective C Runtime class related functions
   Copyright (C) 1993, 1995, 1996 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

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

#include "tconfig.h"         /* include defs of bzero for target */
#include "objc.h"
#include "runtime.h"		/* the kitchen sink */

#if OBJC_WITH_GC
# include <gc.h>
#endif

id __objc_object_alloc(Class);
id __objc_object_dispose(id);
id __objc_object_copy(id);

id (*_objc_object_alloc)(Class)   = __objc_object_alloc;   /* !T:SINGLE */ 
id (*_objc_object_dispose)(id)    = __objc_object_dispose; /* !T:SINGLE */
id (*_objc_object_copy)(id)       = __objc_object_copy;    /* !T:SINGLE */

id
class_create_instance(Class class)
{
  id new = nil;

#if OBJC_WITH_GC
  if (CLS_ISCLASS(class))
    new = (id)GC_malloc_explicitly_typed (class->instance_size,
					  class->gc_object_type);
#else
  if (CLS_ISCLASS(class))
    new = (*_objc_object_alloc)(class);
#endif

  if (new!=nil)
    {
      memset (new, 0, class->instance_size);
      new->class_pointer = class;
    }
  return new;
}

id
object_copy(id object)
{
  if ((object!=nil)&&CLS_ISCLASS(object->class_pointer))
    return (*_objc_object_copy)(object);
  else
    return nil;
}

id
object_dispose(id object)
{
  if ((object!=nil)&&CLS_ISCLASS(object->class_pointer))
    {
      if (_objc_object_dispose)
        (*_objc_object_dispose)(object);
      else
        objc_free(object);
    }
  return nil;
}

id __objc_object_alloc(Class class)
{
  return (id)objc_malloc(class->instance_size);
}

id __objc_object_dispose(id object) 
{
  objc_free(object);
  return 0;
}

id __objc_object_copy(id object)
{
  id copy = class_create_instance(object->class_pointer);
  memcpy(copy, object, object->class_pointer->instance_size);
  return copy;
}


