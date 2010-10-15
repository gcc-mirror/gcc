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
#include "objc-private/runtime.h"      /* the kitchen sink */
#include <string.h>                    /* For strcmp */

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

struct objc_ivar *
class_getClassVariable (Class class_, const char *name)
{
  if (class_ == Nil)
    return NULL;

  /* Logically, since a class is an instance of its meta-class, and
     since its class methods are the instance methods of the
     meta-class, class variables should be instance variables of the
     meta-class.  That is different from the normal use of having
     'static' variables in the class implementation file, because
     every class would have its own variables.

     Anyway, it is all speculative at this stage, but if we get class
     variables in Objective-C, it is conceivable that this
     implementation should work.  */
  return class_getInstanceVariable (class_->class_pointer, name);
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
  if (variable == NULL)
    return NULL;

  return variable->ivar_name;
}

ptrdiff_t ivar_getOffset (struct objc_ivar * variable)
{
  if (variable == NULL)
    return 0;

  return (ptrdiff_t)(variable->ivar_offset);
}

const char * ivar_getTypeEncoding (struct objc_ivar * variable)
{
  if (variable == NULL)
    return NULL;

  return variable->ivar_type;
}

struct objc_ivar ** class_copyIvarList (Class class_, unsigned int *numberOfReturnedIvars)
{
  unsigned int count = 0;
  struct objc_ivar **returnValue = NULL;
  struct objc_ivar_list* ivar_list;

  if (class_ == Nil)
    {
      if (numberOfReturnedIvars)
	*numberOfReturnedIvars = 0;
      return NULL;
    }

  /* TODO: We do not need to lock the runtime mutex if the class has
     been registered with the runtime, since the instance variable
     list can not change after the class is registered.  The only case
     where the lock may be useful if the class is still being created
     using objc_allocateClassPair(), but has not been registered using
     objc_registerClassPair() yet.  I'm not even sure that is
     allowed.  */
  objc_mutex_lock (__objc_runtime_mutex);

  /* Count how many ivars we have.  */
  ivar_list = class_->ivars;
  count = ivar_list->ivar_count;

  if (count != 0)
    {
      unsigned int i = 0;
      
      /* Allocate enough memory to hold them.  */
      returnValue = (struct objc_ivar **)(malloc (sizeof (struct objc_ivar *) * (count + 1)));
      
      /* Copy the ivars.  */
      for (i = 0; i < count; i++)
	{
	  returnValue[i] = &(ivar_list->ivar_list[i]);
	}
      
      returnValue[i] = NULL;
    }
  
  objc_mutex_unlock (__objc_runtime_mutex);

  if (numberOfReturnedIvars)
    *numberOfReturnedIvars = count;

  return returnValue;
}

const char *
property_getName (struct objc_property * property __attribute__ ((__unused__)))
{
  if (property == NULL)
    return NULL;

  /* TODO: New ABI.  */
  /* The current ABI does not have any information on properties.  */
  return NULL;
}

const char *
property_getAttributes (struct objc_property * property __attribute__ ((__unused__)))
{
  if (property == NULL)
    return NULL;

  /* TODO: New ABI.  */
  /* The current ABI does not have any information on properties.  */
  return NULL;
}

struct objc_property *
class_getProperty (Class class_ __attribute__ ((__unused__)),
		   const char *propertyName __attribute__ ((__unused__)))
{
  if (class_ == NULL  ||  propertyName == NULL)
    return NULL;

  /* TODO: New ABI.  */
  /* The current ABI does not have any information on class properties.  */
  return NULL;
}

struct objc_property ** 
class_copyPropertyList (Class class_ __attribute__ ((__unused__)), 
			unsigned int *numberOfReturnedProperties __attribute__ ((__unused__)))
{
  if (class_ == Nil)
    {
      if (numberOfReturnedProperties)
	*numberOfReturnedProperties = 0;
      return NULL;
    }

  /* TODO: New ABI.  */
  /* The current ABI does not have any information on class properties.  */
  if (numberOfReturnedProperties)
    *numberOfReturnedProperties = 0;

  return NULL;
}
