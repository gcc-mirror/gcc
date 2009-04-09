/* GNU Objective C Runtime class related functions
   Copyright (C) 1993, 1995, 1996, 2009 Free Software Foundation, Inc.
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


#include "tconfig.h"         /* include defs of bzero for target */
#include "objc/objc.h"
#include "objc/runtime.h"		/* the kitchen sink */

#if OBJC_WITH_GC
# include <gc.h>
#endif

id __objc_object_alloc (Class);
id __objc_object_dispose (id);
id __objc_object_copy (id);

id (*_objc_object_alloc) (Class)   = __objc_object_alloc;   /* !T:SINGLE */ 
id (*_objc_object_dispose) (id)    = __objc_object_dispose; /* !T:SINGLE */
id (*_objc_object_copy) (id)       = __objc_object_copy;    /* !T:SINGLE */

id
class_create_instance (Class class)
{
  id new = nil;

#if OBJC_WITH_GC
  if (CLS_ISCLASS (class))
    new = (id) GC_malloc_explicitly_typed (class->instance_size,
					   class->gc_object_type);
#else
  if (CLS_ISCLASS (class))
    new = (*_objc_object_alloc) (class);
#endif

  if (new != nil)
    {
      memset (new, 0, class->instance_size);
      new->class_pointer = class;
    }
  return new;
}

id
object_copy (id object)
{
  if ((object != nil) && CLS_ISCLASS (object->class_pointer))
    return (*_objc_object_copy) (object);
  else
    return nil;
}

id
object_dispose (id object)
{
  if ((object != nil) && CLS_ISCLASS (object->class_pointer))
    {
      if (_objc_object_dispose)
        (*_objc_object_dispose) (object);
      else
        objc_free (object);
    }
  return nil;
}

id __objc_object_alloc (Class class)
{
  return (id) objc_malloc (class->instance_size);
}

id __objc_object_dispose (id object) 
{
  objc_free (object);
  return 0;
}

id __objc_object_copy (id object)
{
  id copy = class_create_instance (object->class_pointer);
  memcpy (copy, object, object->class_pointer->instance_size);
  return copy;
}
