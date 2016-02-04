/* GNU Objective C Runtime ivar related functions.
   Copyright (C) 2010-2016 Free Software Foundation, Inc.
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
#include <string.h>                    /* For strcmp.  */
#include <stdlib.h>                    /* For malloc.  */

struct objc_ivar *
class_getInstanceVariable (Class class_, const char *name)
{
  if (class_ != Nil  &&  name != NULL  &&  ! CLS_IS_IN_CONSTRUCTION (class_))
    {
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
		    return ivar;
		}
	    }
	  class_ = class_getSuperclass (class_);
	}
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
    return (void *)(((char *)object) 
		    + object->class_pointer->instance_size);
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

  if (class_ == Nil  ||  CLS_IS_IN_CONSTRUCTION (class_) || !class_->ivars)
    {
      if (numberOfReturnedIvars)
	*numberOfReturnedIvars = 0;
      return NULL;
    }
    
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
	returnValue[i] = &(ivar_list->ivar_list[i]);
      
      returnValue[i] = NULL;
    }
  
  if (numberOfReturnedIvars)
    *numberOfReturnedIvars = count;

  return returnValue;
}

BOOL
class_addIvar (Class class_, const char * ivar_name, size_t size,
	       unsigned char log_2_of_alignment, const char *type)
{
  struct objc_ivar_list *ivars;

  if (class_ == Nil
      || (! CLS_IS_IN_CONSTRUCTION (class_))  
      || ivar_name == NULL  
      || (strcmp (ivar_name, "") == 0)
      || size == 0
      || type == NULL)
    return NO;

  /* Check if the class has an instance variable with that name
     already.  */
  ivars = class_->ivars;

  if (ivars != NULL)
    {
      int i;
      
      for (i = 0; i < ivars->ivar_count; i++)
	{
	  struct objc_ivar *ivar = &(ivars->ivar_list[i]);
	  
	  if (strcmp (ivar->ivar_name, ivar_name) == 0)
	    return NO;
	}
    }

  /* Ok, no direct ivars.  Check superclasses.  */
  if (class_getInstanceVariable (objc_getClass ((char *)(class_->super_class)),
				 ivar_name))
    return NO;

  /* Good.  Create space for the new instance variable.  */
  if (ivars)
    {
      int ivar_count = ivars->ivar_count + 1;
      int new_size = sizeof (struct objc_ivar_list) 
	+ (ivar_count - 1) * sizeof (struct objc_ivar);
      
      ivars = (struct objc_ivar_list*) objc_realloc (ivars, new_size);
      ivars->ivar_count = ivar_count;
      class_->ivars = ivars;
    }
  else
    {
      int new_size = sizeof (struct objc_ivar_list);
      
      ivars = (struct objc_ivar_list*) objc_malloc (new_size);
      ivars->ivar_count = 1;
      class_->ivars = ivars;
    }
    
  /* Now ivars is set to a list of instance variables of the right
     size. */
  {
    struct objc_ivar *ivar = &(ivars->ivar_list[ivars->ivar_count - 1]);
    unsigned int alignment = 1 << log_2_of_alignment;
    int misalignment;
    
    ivar->ivar_name = objc_malloc (strlen (ivar_name) + 1);
    strcpy ((char *)ivar->ivar_name, ivar_name);

    ivar->ivar_type = objc_malloc (strlen (type) + 1);
    strcpy ((char *)ivar->ivar_type, type);

    /* The new instance variable is placed at the end of the existing
       instance_size, at the first byte that is aligned with
       alignment.  */
    misalignment = class_->instance_size % alignment;
    
    if (misalignment == 0)
      ivar->ivar_offset = class_->instance_size;
    else
      ivar->ivar_offset = class_->instance_size - misalignment + alignment;
    
    class_->instance_size = ivar->ivar_offset + size;
  }
  
  return YES;
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

const char *
class_getIvarLayout (Class class_ __attribute__ ((__unused__)))
{
  return NULL;
}

const char *
class_getWeakIvarLayout (Class class_ __attribute__ ((__unused__)))
{
  return NULL;
}

void
class_setIvarLayout (Class class_ __attribute__ ((__unused__)),
		     const char *layout __attribute__ ((__unused__)))
{
  return;
}

void
class_setWeakIvarLayout (Class class_ __attribute__ ((__unused__)),
			 const char *layout __attribute__ ((__unused__)))
{
  return;
}
