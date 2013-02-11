/* GNU Objective C Runtime method related functions.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
#include "objc-private/module-abi-8.h" /* For runtime structures.   */
#include "objc/thr.h"
#include "objc-private/runtime.h"      /* For __objc_runtime_mutex.  */
#include <stdlib.h>                    /* For malloc.  */

SEL
method_getName (struct objc_method * method)
{
  if (method == NULL)
    return NULL;

  return method->method_name;
}

const char *
method_getTypeEncoding (struct objc_method * method)
{
  if (method == NULL)
    return NULL;

  return method->method_types;
}

IMP
method_getImplementation (struct objc_method * method)
{
  if (method == NULL)
    return NULL;

  return method->method_imp;
}

struct objc_method_description *
method_getDescription (struct objc_method * method)
{
  /* Note that the following returns NULL if method is NULL, which is
     fine.  */
  return (struct objc_method_description *)method;
}

struct objc_method **
class_copyMethodList (Class class_, unsigned int *numberOfReturnedMethods)
{
  unsigned int count = 0;
  struct objc_method **returnValue = NULL;
  struct objc_method_list* method_list;

  if (class_ == Nil)
    {
      if (numberOfReturnedMethods)
	*numberOfReturnedMethods = 0;
      return NULL;
    }

  /* Lock the runtime mutex because the class methods may be
     concurrently modified.  */
  objc_mutex_lock (__objc_runtime_mutex);

  /* Count how many methods we have.  */
  method_list = class_->methods;

  while (method_list)
    {
      count = count + method_list->method_count;
      method_list = method_list->method_next;
    }

  if (count != 0)
    {
      unsigned int i = 0;
      
      /* Allocate enough memory to hold them.  */
      returnValue 
	= (struct objc_method **)(malloc (sizeof (struct objc_method *) 
					  * (count + 1)));
      
      /* Copy the methods.  */
      method_list = class_->methods;
      
      while (method_list)
	{
	  int j;
	  for (j = 0; j < method_list->method_count; j++)
	    {
	      returnValue[i] = &(method_list->method_list[j]);
	      i++;
	    }
	  method_list = method_list->method_next;
	}
      
      returnValue[i] = NULL;
    }

  objc_mutex_unlock (__objc_runtime_mutex);

  if (numberOfReturnedMethods)
    *numberOfReturnedMethods = count;

  return returnValue;
}

IMP
method_setImplementation (struct objc_method * method, IMP implementation)
{
  IMP old_implementation;

  if (method == NULL  ||  implementation == NULL)
    return NULL;

  /* We lock the runtime mutex so that concurrent calls to change the
     same method won't conflict with each other.  */
  objc_mutex_lock (__objc_runtime_mutex);

  old_implementation = method->method_imp;
  method->method_imp = implementation;

  /* That was easy :-).  But now we need to find all classes that use
     this method, and update the IMP in the dispatch tables.  */
  __objc_update_classes_with_methods (method, NULL);

  objc_mutex_unlock (__objc_runtime_mutex);

  return old_implementation;
}

void
method_exchangeImplementations (struct objc_method * method_a, struct objc_method * method_b)
{
  IMP old_implementation_a;
  IMP old_implementation_b;

  if (method_a == NULL  ||  method_b == NULL)
    return;

  /* We lock the runtime mutex so that concurrent calls to exchange
     similar methods won't conflict with each other.  Each of them
     should be atomic.  */
  objc_mutex_lock (__objc_runtime_mutex);

  old_implementation_a = method_a->method_imp;
  old_implementation_b = method_b->method_imp;

  method_a->method_imp = old_implementation_b;
  method_b->method_imp = old_implementation_a;

  /* That was easy :-).  But now we need to find all classes that use
     these methods, and update the IMP in the dispatch tables.  */
  __objc_update_classes_with_methods (method_a, method_b);

  objc_mutex_unlock (__objc_runtime_mutex);
}
