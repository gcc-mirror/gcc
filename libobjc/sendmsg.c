/* GNU Objective C Runtime message lookup 
   Copyright (C) 1993, 1995, 1996, 1997, 1998,
   2001, 2002, 2004, 2009, 2010 Free Software Foundation, Inc.
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

/* FIXME: This file has no business including tm.h.  */
/* FIXME: This should be using libffi instead of __builtin_apply
   and friends.  */

#include "objc-private/common.h"
#include "objc-private/error.h"
#include "tconfig.h"
#include "coretypes.h"
#include "tm.h"
#include "objc/runtime.h"
#include "objc/message.h"          /* For objc_msg_lookup(), objc_msg_lookup_super().  */
#include "objc/thr.h"
#include "objc-private/module-abi-8.h"
#include "objc-private/runtime.h"
#include "objc-private/sarray.h"
#include "objc-private/selector.h" /* For sel_is_mapped() */
#include "runtime-info.h"
#include <assert.h> /* For assert */
#include <string.h> /* For strlen */

/* This is how we hack STRUCT_VALUE to be 1 or 0.   */
#define gen_rtx(args...) 1
#define gen_rtx_MEM(args...) 1
#define gen_rtx_REG(args...) 1
/* Already defined in gcc/coretypes.h. So prevent double definition warning.  */
#undef rtx
#define rtx int

#if ! defined (STRUCT_VALUE) || STRUCT_VALUE == 0
#define INVISIBLE_STRUCT_RETURN 1
#else
#define INVISIBLE_STRUCT_RETURN 0
#endif

/* The uninstalled dispatch table.  */
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

static void __objc_install_dispatch_table_for_class (Class);

/* Forward declare some functions.  */
static void __objc_init_install_dtable (id, SEL);

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
	    __objc_install_dispatch_table_for_class (class->class_pointer);
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

/* Given a class and selector, return the selector's
   implementation.  */
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
      /* Not a valid method.  */
      if (class->dtable == __objc_uninstalled_dtable)
	{
	  /* The dispatch table needs to be installed.  */
	  objc_mutex_lock (__objc_runtime_mutex);

	   /* Double-checked locking pattern: Check
	      __objc_uninstalled_dtable again in case another thread
	      installed the dtable while we were waiting for the lock
	      to be released.  */
         if (class->dtable == __objc_uninstalled_dtable)
           {
             __objc_install_dispatch_table_for_class (class);
           }

	  objc_mutex_unlock (__objc_runtime_mutex);
	  /* Call ourselves with the installed dispatch table and get
	     the real method.  */
	  res = get_imp (class, sel);
	}
      else
	{
	  /* The dispatch table has been installed.  */

         /* Get the method from the dispatch table (we try to get it
	    again in case another thread has installed the dtable just
	    after we invoked sarray_get_safe, but before we checked
	    class->dtable == __objc_uninstalled_dtable).  */
	  res = sarray_get_safe (class->dtable, (size_t) sel->sel_id);
	  if (res == 0)
	    {
	      /* The dispatch table has been installed, and the method
		 is not in the dispatch table.  So the method just
		 doesn't exist for the class.  */

	      /* Try going through the +resolveClassMethod: or
		 +resolveInstanceMethod: process.  */
	      if (CLS_ISMETA (class))
		{
		  /* We have the meta class, but we need to invoke the
		     +resolveClassMethod: method on the class.  So, we
		     need to obtain the class from the meta class,
		     which we do using the fact that both the class
		     and the meta-class have the same name.  */
		  Class realClass = objc_lookUpClass (class->name);
		  if (realClass)
		    res = __objc_resolve_class_method (realClass, sel);
		}
	      else
		res = __objc_resolve_instance_method (class, sel);

	      if (res == 0)
		{
		  /* If that fails, then return the forwarding
		     implementation.  We don't know the receiver (only
		     its class), so we have to pass 'nil' as the first
		     argument.  Passing the class as first argument is
		     wrong because the class is not the receiver; it
		     can result in us calling a class method when we
		     want an instance method of the same name.  */
		  res = __objc_get_forward_imp (nil, sel);
		}
	    }
	}
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
   method can be forwarded.  */
inline
BOOL
__objc_responds_to (id object, SEL sel)
{
  void *res;

  /* Install dispatch table if need be.  */
  if (object->class_pointer->dtable == __objc_uninstalled_dtable)
    {
      objc_mutex_lock (__objc_runtime_mutex);
      if (object->class_pointer->dtable == __objc_uninstalled_dtable)
	{
	  __objc_install_dispatch_table_for_class (object->class_pointer);
	}
      objc_mutex_unlock (__objc_runtime_mutex);
    }

  /* Get the method from the dispatch table.  */
  res = sarray_get_safe (object->class_pointer->dtable, (size_t) sel->sel_id);
  return (res != 0);
}

BOOL
class_respondsToSelector (Class class_, SEL selector)
{
  void *res;

  if (class_ == Nil  ||  selector == NULL)
    return NO;

  /* Install dispatch table if need be.  */
  if (class_->dtable == __objc_uninstalled_dtable)
    {
      objc_mutex_lock (__objc_runtime_mutex);
      if (class_->dtable == __objc_uninstalled_dtable)
	{
	  __objc_install_dispatch_table_for_class (class_);
	}
      objc_mutex_unlock (__objc_runtime_mutex);
    }

  /* Get the method from the dispatch table.  */
  res = sarray_get_safe (class_->dtable, (size_t) selector->sel_id);
  return (res != 0);
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
      result = sarray_get_safe (receiver->class_pointer->dtable, 
				(sidx)op->sel_id);
      if (result == 0)
	{
	  /* Not a valid method.  */
	  if (receiver->class_pointer->dtable == __objc_uninstalled_dtable)
	    {
	      /* The dispatch table needs to be installed.  This
		 happens on the very first method call to the
		 class.  */
	      __objc_init_install_dtable (receiver, op);

	      /* Get real method for this in newly installed
		 dtable.  */
	      result = get_imp (receiver->class_pointer, op);
	    }
	  else
	    {
	      /* The dispatch table has been installed.  Check again
		 if the method exists (just in case the dispatch table
		 has been installed by another thread after we did the
		 previous check that the method exists).  */
	      result = sarray_get_safe (receiver->class_pointer->dtable,
					(sidx)op->sel_id);
	      if (result == 0)
		{
		  /* Try going through the +resolveClassMethod: or
		     +resolveInstanceMethod: process.  */
		  if (CLS_ISMETA (receiver->class_pointer))
		    result = __objc_resolve_class_method ((Class)receiver, op);
		  else
		    result = __objc_resolve_instance_method (receiver->class_pointer,
							     op);

		  if (result == 0)
		    {
		      /* If the method still just doesn't exist for
			 the class, attempt to forward the method.  */
		      result = __objc_get_forward_imp (receiver, op);
		    }
		}
	    }
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

/* Temporarily defined here until objc_msg_sendv() goes away.  */
char *method_get_first_argument (struct objc_method *,
				 arglist_t argframe, 
				 const char **type);
char *method_get_next_argument (arglist_t argframe, 
				const char **type);
int method_get_sizeof_arguments (struct objc_method *);

struct objc_method *
class_get_instance_method (Class class, SEL op);

retval_t
objc_msg_sendv (id object, SEL op, arglist_t arg_frame)
{
  struct objc_method *m = class_get_instance_method (object->class_pointer, op);
  const char *type;
  *((id *) method_get_first_argument (m, arg_frame, &type)) = object;
  *((SEL *) method_get_next_argument (arg_frame, &type)) = op;
  return __builtin_apply ((apply_t) m->method_imp, 
			  arg_frame,
			  method_get_sizeof_arguments (m));
}

void
__objc_init_dispatch_tables ()
{
  __objc_uninstalled_dtable = sarray_new (200, 0);

  /* TODO: It would be cool to register typed selectors here.  */
  selector_resolveClassMethod = sel_registerName ("resolveClassMethod:");
  selector_resolveInstanceMethod  =sel_registerName ("resolveInstanceMethod:");
}

/* This function is called by objc_msg_lookup when the dispatch table
   needs to be installed; thus it is called once for each class,
   namely when the very first message is sent to it.  */
static void
__objc_init_install_dtable (id receiver, SEL op __attribute__ ((__unused__)))
{
  objc_mutex_lock (__objc_runtime_mutex);
  
  /* This may happen, if the programmer has taken the address of a
     method before the dtable was initialized... too bad for him!  */
  if (receiver->class_pointer->dtable != __objc_uninstalled_dtable)
    {
      objc_mutex_unlock (__objc_runtime_mutex);
      return;
    }
  
  if (CLS_ISCLASS (receiver->class_pointer))
    {
      /* receiver is an ordinary object.  */
      assert (CLS_ISCLASS (receiver->class_pointer));

      /* Install instance methods table.  */
      __objc_install_dispatch_table_for_class (receiver->class_pointer);

      /* Call +initialize -- this will in turn install the factory
	 dispatch table if not already done. :-)  */
      __objc_send_initialize (receiver->class_pointer);
    }
  else
    {
      /* receiver is a class object.  */
      assert (CLS_ISCLASS ((Class)receiver));
      assert (CLS_ISMETA (receiver->class_pointer));

      /* Install real dtable for factory methods.  */
      __objc_install_dispatch_table_for_class (receiver->class_pointer);

      __objc_send_initialize ((Class)receiver);
    }
  objc_mutex_unlock (__objc_runtime_mutex);
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
	IMP imp = 0;
        struct objc_method_list * method_list = class->class_pointer->methods;
	
        while (method_list)
	  {
	    int i;
	    struct objc_method * method;
	    
	    for (i = 0; i < method_list->method_count; i++)
	      {
		method = &(method_list->method_list[i]);
		if (method->method_name
		    && method->method_name->sel_id == op->sel_id)
		  {
		    imp = method->method_imp;
		    break;
		  }
	      }
	    
	    if (imp)
	      break;
	    
	    method_list = method_list->method_next;
	  }
	if (imp)
	  {
	    DEBUG_PRINTF (" begin of [%s +initialize]\n", class->name);
	    (*imp) ((id) class, op);
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
__objc_install_methods_in_dtable (Class class, struct objc_method_list * method_list)
{
  int i;
  
  if (! method_list)
    return;
  
  if (method_list->method_next)
    __objc_install_methods_in_dtable (class, method_list->method_next);
  
  for (i = 0; i < method_list->method_count; i++)
    {
      struct objc_method * method = &(method_list->method_list[i]);
      sarray_at_put_safe (class->dtable,
			  (sidx) method->method_name->sel_id,
			  method->method_imp);
    }
}

/* Assumes that __objc_runtime_mutex is locked down.  */
static void
__objc_install_dispatch_table_for_class (Class class)
{
  Class super;

  /* If the class has not yet had its class links resolved, we must
     re-compute all class links.  */
  if (! CLS_ISRESOLV (class))
    __objc_resolve_class_links ();

  DEBUG_PRINTF ("__objc_install_dispatch_table_for_class (%s)\n", class->name);
  
  super = class->super_class;

  if (super != 0 && (super->dtable == __objc_uninstalled_dtable))
    __objc_install_dispatch_table_for_class (super);

  /* Allocate dtable if necessary.  */
  if (super == 0)
    {
      objc_mutex_lock (__objc_runtime_mutex);
      class->dtable = sarray_new (__objc_selector_max_index, 0);
      objc_mutex_unlock (__objc_runtime_mutex);
    }
  else
    class->dtable = sarray_lazy_copy (super->dtable);

  __objc_install_methods_in_dtable (class, class->methods);
}

void
__objc_update_dispatch_table_for_class (Class class)
{
  Class next;
  struct sarray *arr;

  /* Not yet installed -- skip it.  */
  if (class->dtable == __objc_uninstalled_dtable) 
    return;

  DEBUG_PRINTF (" _objc_update_dispatch_table_for_class (%s)\n", class->name);

  objc_mutex_lock (__objc_runtime_mutex);

  arr = class->dtable;
  __objc_install_premature_dtable (class); /* someone might require it... */
  sarray_free (arr);			   /* release memory */
  
  /* Could have been lazy...  */
  __objc_install_dispatch_table_for_class (class); 

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
class_get_instance_method (Class class, SEL op)
{
  return search_for_method_in_hierarchy (class, op);
}

struct objc_method *
class_get_class_method (MetaClass class, SEL op)
{
  return search_for_method_in_hierarchy (class, op);
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


/* This function is installed in the dispatch table for all methods
   which are not implemented.  Thus, it is called when a selector is
   not recognized.  */
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
      imp = get_imp (object->class_pointer, frwd_sel);
      return (*imp) (object, frwd_sel, sel, args);
    }

  /* If the object recognizes the doesNotRecognize: method then we're
     going to send it.  */
  err_sel = sel_get_any_uid ("doesNotRecognize:");
  if (__objc_responds_to (object, err_sel))
    {
      imp = get_imp (object->class_pointer, err_sel);
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

    /* TODO: support for error: is surely deprecated ? */
    err_sel = sel_get_any_uid ("error:");
    if (__objc_responds_to (object, err_sel))
      {
	imp = get_imp (object->class_pointer, err_sel);
	return (*imp) (object, sel_get_any_uid ("error:"), msg);
      }

    /* The object doesn't respond to doesNotRecognize: or error:;
       Therefore, a default action is taken.  */
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

/* Returns the uninstalled dispatch table indicator.  If a class'
   dispatch table points to __objc_uninstalled_dtable then that means
   it needs its dispatch table to be installed.  */
struct sarray *
objc_get_uninstalled_dtable (void)
{
  return __objc_uninstalled_dtable;
}
