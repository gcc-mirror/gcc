/* GNU Objective C Runtime message lookup 
   Copyright (C) 1993-2020 Free Software Foundation, Inc.
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

/* Uncommented the following line to enable debug logging.  Use this
   only while debugging the runtime.  */
/* #define DEBUG 1 */

/* FIXME: This should be using libffi instead of __builtin_apply
   and friends.  */

#include "objc-private/common.h"
#include "objc-private/error.h"
#include "tconfig.h"
#include "coretypes.h"
#include "objc/runtime.h"
#include "objc/message.h"          /* For objc_msg_lookup(), objc_msg_lookup_super().  */
#include "objc/thr.h"
#include "objc-private/module-abi-8.h"
#include "objc-private/runtime.h"
#include "objc-private/hash.h"
#include "objc-private/sarray.h"
#include "objc-private/selector.h" /* For sel_is_mapped() */
#include "runtime-info.h"
#include <assert.h> /* For assert */
#include <string.h> /* For strlen */

#define INVISIBLE_STRUCT_RETURN 1

/* The uninstalled dispatch table.  If a class' dispatch table points
   to __objc_uninstalled_dtable then that means it needs its dispatch
   table to be installed.  */
struct sarray *__objc_uninstalled_dtable = 0;   /* !T:MUTEX */

/* Two hooks for method forwarding. If either is set, it is invoked to
 * return a function that performs the real forwarding.  If both are
 * set, the result of __objc_msg_forward2 will be preferred over that
 * of __objc_msg_forward.  If both return NULL or are unset, the
 * libgcc based functions (__builtin_apply and friends) are used.  */
IMP (*__objc_msg_forward) (SEL) = NULL;
IMP (*__objc_msg_forward2) (id, SEL) = NULL;

/* Send +initialize to class.  */
static void __objc_send_initialize (Class);

/* Forward declare some functions */
static void __objc_install_dtable_for_class (Class cls);
static void __objc_prepare_dtable_for_class (Class cls);
static void __objc_install_prepared_dtable_for_class (Class cls);

static struct sarray *__objc_prepared_dtable_for_class (Class cls);
static IMP __objc_get_prepared_imp (Class cls,SEL sel);
  

/* Various forwarding functions that are used based upon the
   return type for the selector.
   __objc_block_forward for structures.
   __objc_double_forward for floats/doubles.
   __objc_word_forward for pointers or types that fit in registers.  */
static double __objc_double_forward (id, SEL, ...);
static id __objc_word_forward (id, SEL, ...);
typedef struct { id many[8]; } __big;
#if INVISIBLE_STRUCT_RETURN 
static __big 
#else
static id
#endif
__objc_block_forward (id, SEL, ...);
static struct objc_method * search_for_method_in_hierarchy (Class class, SEL sel);
struct objc_method * search_for_method_in_list (struct objc_method_list * list, SEL op);
id nil_method (id, SEL);

/* Make sure this inline function is exported regardless of GNU89 or C99
   inlining semantics as it is part of the libobjc ABI.  */
extern IMP __objc_get_forward_imp (id, SEL);

/* Given a selector, return the proper forwarding implementation.  */
inline
IMP
__objc_get_forward_imp (id rcv, SEL sel)
{
  /* If a custom forwarding hook was registered, try getting a
     forwarding function from it. There are two forward routine hooks,
     one that takes the receiver as an argument and one that does
     not.  */
  if (__objc_msg_forward2)
    {
      IMP result;
      if ((result = __objc_msg_forward2 (rcv, sel)) != NULL)
       return result;
    }
  if (__objc_msg_forward)
    {
      IMP result;
      if ((result = __objc_msg_forward (sel)) != NULL) 
	return result;
    }

  /* In all other cases, use the default forwarding functions built
     using __builtin_apply and friends.  */
    {
      const char *t = sel->sel_types;
      
      if (t && (*t == '[' || *t == '(' || *t == '{')
#ifdef OBJC_MAX_STRUCT_BY_VALUE
          && objc_sizeof_type (t) > OBJC_MAX_STRUCT_BY_VALUE
#endif
          )
        return (IMP)__objc_block_forward;
      else if (t && (*t == 'f' || *t == 'd'))
        return (IMP)__objc_double_forward;
      else
        return (IMP)__objc_word_forward;
    }
}

/* Selectors for +resolveClassMethod: and +resolveInstanceMethod:.
   These are set up at startup.  */
static SEL selector_resolveClassMethod = NULL;
static SEL selector_resolveInstanceMethod = NULL;

/* Internal routines use to resolve a class method using
   +resolveClassMethod:.  'class' is always a non-Nil class (*not* a
   meta-class), and 'sel' is the selector that we are trying to
   resolve.  This must be called when class is not Nil, and the
   dispatch table for class methods has already been installed.

   This routine tries to call +resolveClassMethod: to give an
   opportunity to resolve the method.  If +resolveClassMethod: returns
   YES, it tries looking up the method again, and if found, it returns
   it.  Else, it returns NULL.  */
static inline
IMP
__objc_resolve_class_method (Class class, SEL sel)
{
  /* We need to lookup +resolveClassMethod:.  */
  BOOL (*resolveMethodIMP) (id, SEL, SEL);

  /* The dispatch table for class methods is already installed and we
     don't want any forwarding to happen when looking up this method,
     so we just look it up directly.  Note that if 'sel' is precisely
     +resolveClassMethod:, this would look it up yet again and find
     nothing.  That's no problem and there's no recursion.  */
  resolveMethodIMP = (BOOL (*) (id, SEL, SEL))sarray_get_safe
    (class->class_pointer->dtable, (size_t) selector_resolveClassMethod->sel_id);

  if (resolveMethodIMP && resolveMethodIMP ((id)class, selector_resolveClassMethod, sel))
    {
      /* +resolveClassMethod: returned YES.  Look the method up again.
	 We already know the dtable is installed.  */
      
      /* TODO: There is the case where +resolveClassMethod: is buggy
	 and returned YES without actually adding the method.  We
	 could maybe print an error message.  */
      return sarray_get_safe (class->class_pointer->dtable, (size_t) sel->sel_id);
    }

  return NULL;
}

/* Internal routines use to resolve a instance method using
   +resolveInstanceMethod:.  'class' is always a non-Nil class, and
   'sel' is the selector that we are trying to resolve.  This must be
   called when class is not Nil, and the dispatch table for instance
   methods has already been installed.

   This routine tries to call +resolveInstanceMethod: to give an
   opportunity to resolve the method.  If +resolveInstanceMethod:
   returns YES, it tries looking up the method again, and if found, it
   returns it.  Else, it returns NULL.  */
static inline
IMP
__objc_resolve_instance_method (Class class, SEL sel)
{
  /* We need to lookup +resolveInstanceMethod:.  */
  BOOL (*resolveMethodIMP) (id, SEL, SEL);

  /* The dispatch table for class methods may not be already installed
     so we have to install it if needed.  */
  resolveMethodIMP = sarray_get_safe (class->class_pointer->dtable,
				      (size_t) selector_resolveInstanceMethod->sel_id);
  if (resolveMethodIMP == 0)
    {
      /* Try again after installing the dtable.  */
      if (class->class_pointer->dtable == __objc_uninstalled_dtable)
	{
	  objc_mutex_lock (__objc_runtime_mutex);
	  if (class->class_pointer->dtable == __objc_uninstalled_dtable)
	    __objc_install_dtable_for_class (class->class_pointer);
	  objc_mutex_unlock (__objc_runtime_mutex);
	}
      resolveMethodIMP = sarray_get_safe (class->class_pointer->dtable,
					  (size_t) selector_resolveInstanceMethod->sel_id);	      
    }

  if (resolveMethodIMP && resolveMethodIMP ((id)class, selector_resolveInstanceMethod, sel))
    {
      /* +resolveInstanceMethod: returned YES.  Look the method up
	 again.  We already know the dtable is installed.  */
      
      /* TODO: There is the case where +resolveInstanceMethod: is
	 buggy and returned YES without actually adding the method.
	 We could maybe print an error message.  */
      return sarray_get_safe (class->dtable, (size_t) sel->sel_id);	
    }

  return NULL;
}

/* Given a CLASS and selector, return the implementation corresponding
   to the method of the selector.

   If CLASS is a class, the instance method is returned.
   If CLASS is a meta class, the class method is returned.

   Since this requires the dispatch table to be installed, this function
   will implicitly invoke +initialize for CLASS if it hasn't been
   invoked yet.  This also insures that +initialize has been invoked
   when the returned implementation is called directly.

   The forwarding hooks require the receiver as an argument (if they are to
   perform dynamic lookup in proxy objects etc), so this function has a
   receiver argument to be used with those hooks.  */
static inline
IMP
get_implementation (id receiver, Class class, SEL sel)
{
  void *res;

  if (class->dtable == __objc_uninstalled_dtable)
    {
      /* The dispatch table needs to be installed.  */
      objc_mutex_lock (__objc_runtime_mutex);

      /* Double-checked locking pattern: Check
	 __objc_uninstalled_dtable again in case another thread
	 installed the dtable while we were waiting for the lock to be
	 released.  */
      if (class->dtable == __objc_uninstalled_dtable)
	__objc_install_dtable_for_class (class);

      /* If the dispatch table is not yet installed, we are still in
	 the process of executing +initialize.  But the implementation
	 pointer should be available in the prepared ispatch table if
	 it exists at all.  */
      if (class->dtable == __objc_uninstalled_dtable)
	{
	  assert (__objc_prepared_dtable_for_class (class) != 0);
	  res = __objc_get_prepared_imp (class, sel);
	}
      else
	res = 0;

      objc_mutex_unlock (__objc_runtime_mutex);
      /* Call ourselves with the installed dispatch table and get the
	 real method.  */
      if (!res)
	res = get_implementation (receiver, class, sel);
    }
  else
    {
      /* The dispatch table has been installed.  */
      res = sarray_get_safe (class->dtable, (size_t) sel->sel_id);
      if (res == 0)
	{
	  /* The dispatch table has been installed, and the method is
	     not in the dispatch table.  So the method just doesn't
	     exist for the class.  */

	  /* Try going through the +resolveClassMethod: or
	     +resolveInstanceMethod: process.  */
	  if (CLS_ISMETA (class))
	    {
	      /* We have the meta class, but we need to invoke the
		 +resolveClassMethod: method on the class.  So, we
		 need to obtain the class from the meta class, which
		 we do using the fact that both the class and the
		 meta-class have the same name.  */
	      Class realClass = objc_lookUpClass (class->name);
	      if (realClass)
		res = __objc_resolve_class_method (realClass, sel);
	    }
	  else
	    res = __objc_resolve_instance_method (class, sel);

	  if (res == 0)
	    res = __objc_get_forward_imp (receiver, sel);
	}
    }
  return res;
}

/* Make sure this inline function is exported regardless of GNU89 or C99
   inlining semantics as it is part of the libobjc ABI.  */
extern IMP get_imp (Class, SEL);

inline
IMP
get_imp (Class class, SEL sel)
{
  /* In a vanilla implementation we would first check if the dispatch
     table is installed.  Here instead, to get more speed in the
     standard case (that the dispatch table is installed) we first try
     to get the imp using brute force.  Only if that fails, we do what
     we should have been doing from the very beginning, that is, check
     if the dispatch table needs to be installed, install it if it's
     not installed, and retrieve the imp from the table if it's
     installed.  */
  void *res = sarray_get_safe (class->dtable, (size_t) sel->sel_id);
  if (res == 0)
    {
      res = get_implementation(nil, class, sel);
    }
  return res;
}

/* The new name of get_imp().  */
IMP
class_getMethodImplementation (Class class_, SEL selector)
{
  if (class_ == Nil  ||  selector == NULL)
    return NULL;

  /* get_imp is inlined, so we're good.  */
  return get_imp (class_, selector);
}

/* Given a method, return its implementation.  This has been replaced
   by method_getImplementation() in the modern API.  */
IMP
method_get_imp (struct objc_method * method)
{
  return (method != (struct objc_method *)0) ? method->method_imp : (IMP)0;
}

/* Query if an object can respond to a selector, returns YES if the
   object implements the selector otherwise NO.  Does not check if the
   method can be forwarded.  Since this requires the dispatch table to
   installed, this function will implicitly invoke +initialize for the
   class of OBJECT if it hasn't been invoked yet.  */
inline
BOOL
__objc_responds_to (id object, SEL sel)
{
  void *res;
  struct sarray *dtable;

  /* Install dispatch table if need be */
  dtable = object->class_pointer->dtable;
  if (dtable == __objc_uninstalled_dtable)
    {
      objc_mutex_lock (__objc_runtime_mutex);
      if (object->class_pointer->dtable == __objc_uninstalled_dtable)
        __objc_install_dtable_for_class (object->class_pointer);

      /* If the dispatch table is not yet installed, we are still in
         the process of executing +initialize.  Yet the dispatch table
         should be available.  */
      if (object->class_pointer->dtable == __objc_uninstalled_dtable)
        {
          dtable = __objc_prepared_dtable_for_class (object->class_pointer);
          assert (dtable);
        }
      else
        dtable = object->class_pointer->dtable;

      objc_mutex_unlock (__objc_runtime_mutex);
    }

  /* Get the method from the dispatch table.  */
  res = sarray_get_safe (dtable, (size_t) sel->sel_id);
  return (res != 0) ? YES : NO;
}

BOOL
class_respondsToSelector (Class class_, SEL selector)
{
  struct sarray *dtable;
  void *res;

  if (class_ == Nil  ||  selector == NULL)
    return NO;

  /* Install dispatch table if need be.  */
  dtable = class_->dtable;
  if (dtable == __objc_uninstalled_dtable)
    {
      objc_mutex_lock (__objc_runtime_mutex);
      if (class_->dtable == __objc_uninstalled_dtable)
	__objc_install_dtable_for_class (class_);

      /* If the dispatch table is not yet installed,
         we are still in the process of executing +initialize.
         Yet the dispatch table should be available.  */
      if (class_->dtable == __objc_uninstalled_dtable)
        {
          dtable = __objc_prepared_dtable_for_class (class_);
          assert (dtable);
        }
      else
        dtable = class_->dtable;

      objc_mutex_unlock (__objc_runtime_mutex);
    }

  /* Get the method from the dispatch table.  */
  res = sarray_get_safe (dtable, (size_t) selector->sel_id);
  return (res != 0) ? YES : NO;
}

/* This is the lookup function.  All entries in the table are either a
   valid method *or* zero.  If zero then either the dispatch table
   needs to be installed or it doesn't exist and forwarding is
   attempted.  */
IMP
objc_msg_lookup (id receiver, SEL op)
{
  IMP result;
  if (receiver)
    {
      /* First try a quick lookup assuming the dispatch table exists.  */
      result = sarray_get_safe (receiver->class_pointer->dtable, 
				(sidx)op->sel_id);
      if (result == 0)
	{
	  /* Not found ... call get_implementation () to install the
             dispatch table and call +initialize as required,
             providing the method implementation or a forwarding
             function.  */
	  result = get_implementation (receiver, receiver->class_pointer, op);
	}
      return result;
    }
  else
    return (IMP)nil_method;
}

IMP
objc_msg_lookup_super (struct objc_super *super, SEL sel)
{
  if (super->self)
    return get_imp (super->super_class, sel);
  else
    return (IMP)nil_method;
}

void
__objc_init_dispatch_tables ()
{
  __objc_uninstalled_dtable = sarray_new (200, 0);

  /* TODO: It would be cool to register typed selectors here.  */
  selector_resolveClassMethod = sel_registerName ("resolveClassMethod:");
  selector_resolveInstanceMethod = sel_registerName ("resolveInstanceMethod:");
}


/* Install dummy table for class which causes the first message to
   that class (or instances hereof) to be initialized properly.  */
void
__objc_install_premature_dtable (Class class)
{
  assert (__objc_uninstalled_dtable);
  class->dtable = __objc_uninstalled_dtable;
}   

/* Send +initialize to class if not already done.  */
static void
__objc_send_initialize (Class class)
{
  /* This *must* be a class object.  */
  assert (CLS_ISCLASS (class));
  assert (! CLS_ISMETA (class));

  /* class_add_method_list/__objc_update_dispatch_table_for_class may
     have reset the dispatch table.  The canonical way to insure that
     we send +initialize just once, is this flag.  */
  if (! CLS_ISINITIALIZED (class))
    {
      DEBUG_PRINTF ("+initialize: need to initialize class '%s'\n", class->name);
      CLS_SETINITIALIZED (class);
      CLS_SETINITIALIZED (class->class_pointer);

      /* Create the garbage collector type memory description.  */
      __objc_generate_gc_type_description (class);

      if (class->super_class)
	__objc_send_initialize (class->super_class);

      {
	SEL op = sel_registerName ("initialize");
        struct objc_method *method = search_for_method_in_hierarchy (class->class_pointer, 
								     op);

	if (method)
	  {
	    DEBUG_PRINTF (" begin of [%s +initialize]\n", class->name);
	    (*method->method_imp) ((id)class, op);
	    DEBUG_PRINTF (" end of [%s +initialize]\n", class->name);
	  }
#ifdef DEBUG
	else
	  {
	    DEBUG_PRINTF (" class '%s' has no +initialize method\n", class->name);	    
	  }
#endif
      }
    }
}

/* Walk on the methods list of class and install the methods in the
   reverse order of the lists.  Since methods added by categories are
   before the methods of class in the methods list, this allows
   categories to substitute methods declared in class.  However if
   more than one category replaces the same method nothing is
   guaranteed about what method will be used.  Assumes that
   __objc_runtime_mutex is locked down.  */
static void
__objc_install_methods_in_dtable (struct sarray *dtable, struct objc_method_list * method_list)
{
  int i;
  
  if (! method_list)
    return;
  
  if (method_list->method_next)
    __objc_install_methods_in_dtable (dtable, method_list->method_next);
  
  for (i = 0; i < method_list->method_count; i++)
    {
      struct objc_method * method = &(method_list->method_list[i]);
      sarray_at_put_safe (dtable,
			  (sidx) method->method_name->sel_id,
			  method->method_imp);
    }
}

void
__objc_update_dispatch_table_for_class (Class class)
{
  Class next;
  struct sarray *arr;

  DEBUG_PRINTF (" _objc_update_dtable_for_class (%s)\n", class->name);

  objc_mutex_lock (__objc_runtime_mutex);

  /* Not yet installed -- skip it unless in +initialize.  */
  if (class->dtable == __objc_uninstalled_dtable) 
    {
      if (__objc_prepared_dtable_for_class (class))
	{
	  /* There is a prepared table so we must be initialising this
	     class ... we must re-do the table preparation.  */
	  __objc_prepare_dtable_for_class (class);
	}
      objc_mutex_unlock (__objc_runtime_mutex);
      return;
    }

  arr = class->dtable;
  __objc_install_premature_dtable (class); /* someone might require it... */
  sarray_free (arr);			   /* release memory */
  
  /* Could have been lazy...  */
  __objc_install_dtable_for_class (class); 

  if (class->subclass_list)	/* Traverse subclasses.  */
    for (next = class->subclass_list; next; next = next->sibling_class)
      __objc_update_dispatch_table_for_class (next);

  objc_mutex_unlock (__objc_runtime_mutex);
}

/* This function adds a method list to a class.  This function is
   typically called by another function specific to the run-time.  As
   such this function does not worry about thread safe issues.

   This one is only called for categories. Class objects have their
   methods installed right away, and their selectors are made into
   SEL's by the function __objc_register_selectors_from_class.  */
void
class_add_method_list (Class class, struct objc_method_list * list)
{
  /* Passing of a linked list is not allowed.  Do multiple calls.  */
  assert (! list->method_next);

  __objc_register_selectors_from_list(list);

  /* Add the methods to the class's method list.  */
  list->method_next = class->methods;
  class->methods = list;

  /* Update the dispatch table of class.  */
  __objc_update_dispatch_table_for_class (class);
}

struct objc_method *
class_getInstanceMethod (Class class_, SEL selector)
{
  struct objc_method *m;

  if (class_ == Nil  ||  selector == NULL)
    return NULL;

  m = search_for_method_in_hierarchy (class_, selector);
  if (m)
    return m;

  /* Try going through +resolveInstanceMethod:, and do the search
     again if successful.  */
  if (__objc_resolve_instance_method (class_, selector))
    return search_for_method_in_hierarchy (class_, selector);

  return NULL;
}

struct objc_method *
class_getClassMethod (Class class_, SEL selector)
{
  struct objc_method *m;

  if (class_ == Nil  ||  selector == NULL)
    return NULL;
  
  m = search_for_method_in_hierarchy (class_->class_pointer, 
				      selector);
  if (m)
    return m;

  /* Try going through +resolveClassMethod:, and do the search again
     if successful.  */
  if (__objc_resolve_class_method (class_, selector))
    return search_for_method_in_hierarchy (class_->class_pointer, 
					   selector);    

  return NULL;
}

BOOL
class_addMethod (Class class_, SEL selector, IMP implementation,
		 const char *method_types)
{
  struct objc_method_list *method_list;
  struct objc_method *method;
  const char *method_name;

  if (class_ == Nil  ||  selector == NULL  ||  implementation == NULL  
      || method_types == NULL  || (strcmp (method_types, "") == 0))
    return NO;

  method_name = sel_getName (selector);
  if (method_name == NULL)
    return NO;

  /* If the method already exists in the class, return NO.  It is fine
     if the method already exists in the superclass; in that case, we
     are overriding it.  */
  if (CLS_IS_IN_CONSTRUCTION (class_))
    {
      /* The class only contains a list of methods; they have not been
	 registered yet, ie, the method_name of each of them is still
	 a string, not a selector.  Iterate manually over them to
	 check if we have already added the method.  */
      struct objc_method_list * method_list = class_->methods;
      while (method_list)
	{
	  int i;
	  
	  /* Search the method list.  */
	  for (i = 0; i < method_list->method_count; ++i)
	    {
	      struct objc_method * method = &method_list->method_list[i];
	      
	      if (method->method_name
		  && strcmp ((char *)method->method_name, method_name) == 0)
		return NO;
	    }
	  
	  /* The method wasn't found.  Follow the link to the next list of
	     methods.  */
	  method_list = method_list->method_next;
	}
      /* The method wasn't found.  It's a new one.  Go ahead and add
	 it.  */
    }
  else
    {
      /* Do the standard lookup.  This assumes the selectors are
	 mapped.  */
      if (search_for_method_in_list (class_->methods, selector))
	return NO;
    }

  method_list = (struct objc_method_list *)objc_calloc (1, sizeof (struct objc_method_list));
  method_list->method_count = 1;

  method = &(method_list->method_list[0]);
  method->method_name = objc_malloc (strlen (method_name) + 1);
  strcpy ((char *)method->method_name, method_name);

  method->method_types = objc_malloc (strlen (method_types) + 1);
  strcpy ((char *)method->method_types, method_types);
  
  method->method_imp = implementation;
  
  if (CLS_IS_IN_CONSTRUCTION (class_))
    {
      /* We only need to add the method to the list.  It will be
	 registered with the runtime when the class pair is registered
	 (if ever).  */
      method_list->method_next = class_->methods;
      class_->methods = method_list;
    }
  else
    {
      /* Add the method to a live class.  */
      objc_mutex_lock (__objc_runtime_mutex);
      class_add_method_list (class_, method_list);
      objc_mutex_unlock (__objc_runtime_mutex);
    }

  return YES;
}

IMP
class_replaceMethod (Class class_, SEL selector, IMP implementation,
		     const char *method_types)
{
  struct objc_method * method;

  if (class_ == Nil  ||  selector == NULL  ||  implementation == NULL
      || method_types == NULL)
    return NULL;

  method = search_for_method_in_hierarchy (class_, selector);

  if (method)
    {
      return method_setImplementation (method, implementation);
    }
  else
    {
      class_addMethod (class_, selector, implementation, method_types);
      return NULL;
    }
}

/* Search for a method starting from the current class up its
   hierarchy.  Return a pointer to the method's method structure if
   found.  NULL otherwise.  */
static struct objc_method *
search_for_method_in_hierarchy (Class cls, SEL sel)
{
  struct objc_method * method = NULL;
  Class class;

  if (! sel_is_mapped (sel))
    return NULL;

  /* Scan the method list of the class.  If the method isn't found in
     the list then step to its super class.  */
  for (class = cls; ((! method) && class); class = class->super_class)
    method = search_for_method_in_list (class->methods, sel);

  return method;
}



/* Given a linked list of method and a method's name.  Search for the
   named method's method structure.  Return a pointer to the method's
   method structure if found.  NULL otherwise.  */  
struct objc_method *
search_for_method_in_list (struct objc_method_list * list, SEL op)
{
  struct objc_method_list * method_list = list;

  if (! sel_is_mapped (op))
    return NULL;

  /* If not found then we'll search the list.  */
  while (method_list)
    {
      int i;

      /* Search the method list.  */
      for (i = 0; i < method_list->method_count; ++i)
        {
          struct objc_method * method = &method_list->method_list[i];

          if (method->method_name)
            if (method->method_name->sel_id == op->sel_id)
              return method;
        }

      /* The method wasn't found.  Follow the link to the next list of
         methods.  */
      method_list = method_list->method_next;
    }

  return NULL;
}

typedef void * retval_t;
typedef void * arglist_t;

static retval_t __objc_forward (id object, SEL sel, arglist_t args);

/* Forwarding pointers/integers through the normal registers.  */
static id
__objc_word_forward (id rcv, SEL op, ...)
{
  void *args, *res;

  args = __builtin_apply_args ();
  res = __objc_forward (rcv, op, args);
  if (res)
    __builtin_return (res);
  else
    return res;
}

/* Specific routine for forwarding floats/double because of
   architectural differences on some processors.  i386s for example
   which uses a floating point stack versus general registers for
   floating point numbers.  This forward routine makes sure that GCC
   restores the proper return values.  */
static double
__objc_double_forward (id rcv, SEL op, ...)
{
  void *args, *res;

  args = __builtin_apply_args ();
  res = __objc_forward (rcv, op, args);
  __builtin_return (res);
}

#if INVISIBLE_STRUCT_RETURN
static __big
#else
static id
#endif
__objc_block_forward (id rcv, SEL op, ...)
{
  void *args, *res;

  args = __builtin_apply_args ();
  res = __objc_forward (rcv, op, args);
  if (res)
    __builtin_return (res);
  else
#if INVISIBLE_STRUCT_RETURN
    return (__big) {{0, 0, 0, 0, 0, 0, 0, 0}};
#else
    return nil;
#endif
}


/* This function is called for methods which are not implemented,
   unless a custom forwarding routine has been installed.  Please note
   that most serious users of libobjc (eg, GNUstep base) do install
   their own forwarding routines, and hence this is never actually
   used.  But, if no custom forwarding routine is installed, this is
   called when a selector is not recognized.  */
static retval_t
__objc_forward (id object, SEL sel, arglist_t args)
{
  IMP imp;
  static SEL frwd_sel = 0;                      /* !T:SAFE2 */
  SEL err_sel;

  /* First try if the object understands forward::.  */
  if (! frwd_sel)
    frwd_sel = sel_get_any_uid ("forward::");

  if (__objc_responds_to (object, frwd_sel))
    {
      imp = get_implementation (object, object->class_pointer, frwd_sel);
      return (*imp) (object, frwd_sel, sel, args);
    }

  /* If the object recognizes the doesNotRecognize: method then we're
     going to send it.  */
  err_sel = sel_get_any_uid ("doesNotRecognize:");
  if (__objc_responds_to (object, err_sel))
    {
      imp = get_implementation (object, object->class_pointer, err_sel);
      return (*imp) (object, err_sel, sel);
    }
  
  /* The object doesn't recognize the method.  Check for responding to
     error:.  If it does then sent it.  */
  {
    char msg[256 + strlen ((const char *) sel_getName (sel))
             + strlen ((const char *) object->class_pointer->name)];

    sprintf (msg, "(%s) %s does not recognize %s",
	     (CLS_ISMETA (object->class_pointer)
	      ? "class"
	      : "instance" ),
             object->class_pointer->name, sel_getName (sel));

    /* The object doesn't respond to doesNotRecognize:.  Therefore, a
       default action is taken.  */
    _objc_abort ("%s\n", msg);

    return 0;
  }
}

void
__objc_print_dtable_stats (void)
{
  int total = 0;

  objc_mutex_lock (__objc_runtime_mutex);

#ifdef OBJC_SPARSE2
  printf ("memory usage: (%s)\n", "2-level sparse arrays");
#else
  printf ("memory usage: (%s)\n", "3-level sparse arrays");
#endif

  printf ("arrays: %d = %ld bytes\n", narrays, 
	  (long) ((size_t) narrays * sizeof (struct sarray)));
  total += narrays * sizeof (struct sarray);
  printf ("buckets: %d = %ld bytes\n", nbuckets, 
	  (long) ((size_t) nbuckets * sizeof (struct sbucket)));
  total += nbuckets * sizeof (struct sbucket);

  printf ("idxtables: %d = %ld bytes\n",
	  idxsize, (long) ((size_t) idxsize * sizeof (void *)));
  total += idxsize * sizeof (void *);
  printf ("-----------------------------------\n");
  printf ("total: %d bytes\n", total);
  printf ("===================================\n");

  objc_mutex_unlock (__objc_runtime_mutex);
}

static cache_ptr prepared_dtable_table = 0;

/* This function is called by: objc_msg_lookup, get_imp and
   __objc_responds_to (and the dispatch table installation functions
   themselves) to install a dispatch table for a class.

   If CLS is a class, it installs instance methods.
   If CLS is a meta class, it installs class methods.

   In either case +initialize is invoked for the corresponding class.

   The implementation must insure that the dispatch table is not
   installed until +initialize completes.  Otherwise it opens a
   potential race since the installation of the dispatch table is used
   as gate in regular method dispatch and we need to guarantee that
   +initialize is the first method invoked an that no other thread my
   dispatch messages to the class before +initialize completes.  */
static void
__objc_install_dtable_for_class (Class cls)
{
  /* If the class has not yet had its class links resolved, we must
     re-compute all class links.  */
  if (! CLS_ISRESOLV (cls))
    __objc_resolve_class_links ();

  /* Make sure the super class has its dispatch table installed or is
     at least preparing.  We do not need to send initialize for the
     super class since __objc_send_initialize will insure that.  */
  if (cls->super_class
      && cls->super_class->dtable == __objc_uninstalled_dtable
      && !__objc_prepared_dtable_for_class (cls->super_class))
    {
      __objc_install_dtable_for_class (cls->super_class);
      /* The superclass initialisation may have also initialised the
         current class, in which case there is no more to do.  */
      if (cls->dtable != __objc_uninstalled_dtable)
	return;
    }

  /* We have already been prepared but +initialize hasn't completed.
     The +initialize implementation is probably sending 'self'
     messages.  We rely on _objc_get_prepared_imp to retrieve the
     implementation pointers.  */
  if (__objc_prepared_dtable_for_class (cls))
    return;

  /* We have this function cache the implementation pointers for
     _objc_get_prepared_imp but the dispatch table won't be initilized
     until __objc_send_initialize completes.  */
  __objc_prepare_dtable_for_class (cls);

  /* We may have already invoked +initialize but
     __objc_update_dispatch_table_for_class invoked by
     class_add_method_list may have reset dispatch table.  */

  /* Call +initialize.  If we are a real class, we are installing
     instance methods.  If we are a meta class, we are installing
     class methods.  The __objc_send_initialize itself will insure
     that the message is called only once per class.  */
  if (CLS_ISCLASS (cls))
    __objc_send_initialize (cls);
  else
    {
      /* Retrieve the class from the meta class.  */
      Class c = objc_getClass (cls->name);
      assert (CLS_ISMETA (cls));
      assert (c);
      __objc_send_initialize (c);
    }

  /* We install the dispatch table correctly when +initialize completed.  */
  __objc_install_prepared_dtable_for_class (cls);
}

/* Builds the dispatch table for the class CLS and stores it in a
   place where it can be retrieved by __objc_get_prepared_imp until
   __objc_install_prepared_dtable_for_class installs it into the
   class.  The dispatch table should not be installed into the class
   until +initialize has completed.  */
static void
__objc_prepare_dtable_for_class (Class cls)
{
  struct sarray *dtable;
  struct sarray *super_dtable;

  /* This table could be initialized in init.c.  We cannot use the
     class name since the class maintains the instance methods and the
     meta class maintains the the class methods yet both share the
     same name.  Classes should be unique in any program.  */
  if (! prepared_dtable_table)
    prepared_dtable_table 
      = objc_hash_new (32,
		       (hash_func_type) objc_hash_ptr,
		       (compare_func_type) objc_compare_ptrs);
  
  /* If the class has not yet had its class links resolved, we must
     re-compute all class links.  */
  if (! CLS_ISRESOLV (cls))
    __objc_resolve_class_links ();

  assert (cls);
  assert (cls->dtable == __objc_uninstalled_dtable);

  /* If there is already a prepared dtable for this class, we must
     replace it with a new version (since there must have been methods
     added to or otherwise modified in the class while executing
     +initialize, and the table needs to be recomputed.  */
  dtable = __objc_prepared_dtable_for_class (cls);
  if (dtable != 0)
    {
      objc_hash_remove (prepared_dtable_table, cls);
      sarray_free (dtable);
    }

  /* Now prepare the dtable for population.  */
  assert (cls != cls->super_class);
  if (cls->super_class)
    {
      /* Inherit the method list from the super class.  Yet the super
         class may still be initializing in the case when a class
         cluster sub class initializes its super classes.  */
      if (cls->super_class->dtable == __objc_uninstalled_dtable)
	__objc_install_dtable_for_class (cls->super_class);

      super_dtable = cls->super_class->dtable;
      /* If the dispatch table is not yet installed, we are still in
	 the process of executing +initialize.  Yet the dispatch table
	 should be available.  */
      if (super_dtable == __objc_uninstalled_dtable)
	super_dtable = __objc_prepared_dtable_for_class (cls->super_class);

      assert (super_dtable);
      dtable = sarray_lazy_copy (super_dtable);
    }
  else
    dtable = sarray_new (__objc_selector_max_index, 0);

  __objc_install_methods_in_dtable (dtable, cls->methods);

  objc_hash_add (&prepared_dtable_table,
		 cls,
		 dtable);
}

/* This wrapper only exists to allow an easy replacement of the lookup
   implementation and it is expected that the compiler will optimize
   it away.  */
static struct sarray *
__objc_prepared_dtable_for_class (Class cls)
{
  struct sarray *dtable = 0;
  assert (cls);
  if (prepared_dtable_table)
    dtable = objc_hash_value_for_key (prepared_dtable_table, cls);
  /* dtable my be nil, since we call this to check whether we are
     currently preparing before we start preparing.  */
  return dtable;
}

/* Helper function for messages sent to CLS or implementation pointers
   retrieved from CLS during +initialize before the dtable is
   installed.  When a class implicitly initializes another class which
   in turn implicitly invokes methods in this class, before the
   implementation of +initialize of CLS completes, this returns the
   expected implementation.  Forwarding remains the responsibility of
   objc_msg_lookup.  This function should only be called under the
   global lock.  */
static IMP
__objc_get_prepared_imp (Class cls,SEL sel)
{
  struct sarray *dtable;
  IMP imp;

  assert (cls);
  assert (sel);
  assert (cls->dtable == __objc_uninstalled_dtable);
  dtable = __objc_prepared_dtable_for_class (cls);

  assert (dtable);
  assert (dtable != __objc_uninstalled_dtable);
  imp = sarray_get_safe (dtable, (size_t) sel->sel_id);

  /* imp may be Nil if the method does not exist and we may fallback
     to the forwarding implementation later.  */
  return imp;  
}

/* When this function is called +initialize should be completed.  So
   now we are safe to install the dispatch table for the class so that
   they become available for other threads that may be waiting in the
   lock.  */
static void
__objc_install_prepared_dtable_for_class (Class cls)
{
  assert (cls);
  assert (cls->dtable == __objc_uninstalled_dtable);
  cls->dtable = __objc_prepared_dtable_for_class (cls);

  assert (cls->dtable);
  assert (cls->dtable != __objc_uninstalled_dtable);
  objc_hash_remove (prepared_dtable_table, cls);
}
