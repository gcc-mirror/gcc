/* GNU Objective C Runtime ivar related functions.
   Copyright (C) 2010 Free Software Foundation, Inc.
   Contributed by Nicola Pero

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
#include "objc-private/module-abi-8.h" /* For runtime structures  */
#include "objc/thr.h"
#include "objc-private/runtime.h"		/* the kitchen sink */
#include <string.h> /* For strcmp */

struct objc_ivar *
class_getInstanceVariable (Class class_, const char *name)
{
  if (class_ != Nil  &&  name != NULL)
    {
      objc_mutex_lock (__objc_runtime_mutex);
      while (class_ != Nil)
	{
	  struct objc_ivar_list *ivars = class_->ivars;
	  if (ivars != NULL)
	    {
	      int i;
	      
	      for (i = 0; i < ivars->ivar_count; i++)
		{
		  struct objc_ivar *ivar = &(ivars->ivar_list[i]);
		  
		  if (!strcmp (ivar->ivar_name, name))
		    {
		      objc_mutex_unlock (__objc_runtime_mutex);
		      return ivar;
		    }
		}
	    }
	  class_ = class_->super_class;
	}
      objc_mutex_unlock (__objc_runtime_mutex);
    }
  return NULL;
}

void *
object_getIndexedIvars (id object)
{
  if (object == nil)
    return NULL;
  else
    {
      return (void *)(((char *)object) 
		      + object->class_pointer->instance_size);
    }
}

struct objc_ivar *
object_getInstanceVariable (id object, const char *name, void **returnValue)
{
  if (object == nil  ||  name == NULL)
    return NULL;
  else
    {
      struct objc_ivar * variable = class_getInstanceVariable (object->class_pointer, name);

      if (variable != NULL  &&  returnValue != NULL)
	{
	  char *location = (char *)object + variable->ivar_offset;
	 
	  *returnValue = *((id *)location);
	}

      return variable;
    }
}

struct objc_ivar *
object_setInstanceVariable (id object, const char *name, void *newValue)
{
  if (object == nil  ||  name == NULL)
    return NULL;
  else
    {
      struct objc_ivar * variable = class_getInstanceVariable (object->class_pointer, name);

      if (variable != NULL)
	{
	  char *location = (char *)object + variable->ivar_offset;
	  
	  *((id *)location) = (id)newValue;
	}

      return variable;
    }
}

id object_getIvar (id object, struct objc_ivar * variable)
{
  if (object == nil  ||  variable == NULL)
    return nil;
  else
    {
      char *location = (char *)object + variable->ivar_offset;

      return *((id *)location);
    }
}

void object_setIvar (id object, struct objc_ivar * variable, id value)
{
  if (object == nil  ||  variable == NULL)
    return;
  else
    {
      char *location = (char *)object + variable->ivar_offset;

      *((id *)location) = value;
    }
}

const char * ivar_getName (struct objc_ivar * variable)
{
  return variable->ivar_name;
}

ptrdiff_t ivar_getOffset (struct objc_ivar * variable)
{
  return (ptrdiff_t)(variable->ivar_offset);
}

const char * ivar_getTypeEncoding (struct objc_ivar * variable)
{
  return variable->ivar_type;
}
