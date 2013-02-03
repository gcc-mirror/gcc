/* GNU Objective C Runtime class related functions
   Copyright (C) 1993-2013 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup and Dennis Glatting.

   Lock-free class table code designed and written from scratch by
   Nicola Pero, 2001.

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

/* The code in this file critically affects class method invocation
  speed.  This long preamble comment explains why, and the issues
  involved.

  One of the traditional weaknesses of the GNU Objective-C runtime is
  that class method invocations are slow.  The reason is that when you
  write
  
  array = [NSArray new];
  
  this gets basically compiled into the equivalent of 
  
  array = [(objc_get_class ("NSArray")) new];
  
  objc_get_class returns the class pointer corresponding to the string
  `NSArray'; and because of the lookup, the operation is more
  complicated and slow than a simple instance method invocation.
  
  Most high performance Objective-C code (using the GNU Objc runtime)
  I had the opportunity to read (or write) work around this problem by
  caching the class pointer:
  
  Class arrayClass = [NSArray class];
  
  ... later on ...
  
  array = [arrayClass new];
  array = [arrayClass new];
  array = [arrayClass new];
  
  In this case, you always perform a class lookup (the first one), but
  then all the [arrayClass new] methods run exactly as fast as an
  instance method invocation.  It helps if you have many class method
  invocations to the same class.
  
  The long-term solution to this problem would be to modify the
  compiler to output tables of class pointers corresponding to all the
  class method invocations, and to add code to the runtime to update
  these tables - that should in the end allow class method invocations
  to perform precisely as fast as instance method invocations, because
  no class lookup would be involved.  I think the Apple Objective-C
  runtime uses this technique.  Doing this involves synchronized
  modifications in the runtime and in the compiler.
  
  As a first medicine to the problem, I [NP] have redesigned and
  rewritten the way the runtime is performing class lookup.  This
  doesn't give as much speed as the other (definitive) approach, but
  at least a class method invocation now takes approximately 4.5 times
  an instance method invocation on my machine (it would take approx 12
  times before the rewriting), which is a lot better.

  One of the main reason the new class lookup is so faster is because
  I implemented it in a way that can safely run multithreaded without
  using locks - a so-called `lock-free' data structure.  The atomic
  operation is pointer assignment.  The reason why in this problem
  lock-free data structures work so well is that you never remove
  classes from the table - and the difficult thing with lock-free data
  structures is freeing data when is removed from the structures.  */

#include "objc-private/common.h"
#include "objc-private/error.h"
#include "objc/runtime.h"
#include "objc/thr.h"
#include "objc-private/module-abi-8.h"  /* For CLS_ISCLASS and similar.  */
#include "objc-private/runtime.h"       /* the kitchen sink */
#include "objc-private/sarray.h"        /* For sarray_put_at_safe.  */
#include "objc-private/selector.h"      /* For sarray_put_at_safe.  */
#include <string.h>                     /* For memset */

/* We use a table which maps a class name to the corresponding class
   pointer.  The first part of this file defines this table, and
   functions to do basic operations on the table.  The second part of
   the file implements some higher level Objective-C functionality for
   classes by using the functions provided in the first part to manage
   the table. */

/**
 ** Class Table Internals
 **/

/* A node holding a class */
typedef struct class_node
{
  struct class_node *next;      /* Pointer to next entry on the list.
                                   NULL indicates end of list. */
  
  const char *name;             /* The class name string */
  int length;                   /* The class name string length */
  Class pointer;                /* The Class pointer */
  
} *class_node_ptr;

/* A table containing classes is a class_node_ptr (pointing to the
   first entry in the table - if it is NULL, then the table is
   empty). */

/* We have 1024 tables.  Each table contains all class names which
   have the same hash (which is a number between 0 and 1023).  To look
   up a class_name, we compute its hash, and get the corresponding
   table.  Once we have the table, we simply compare strings directly
   till we find the one which we want (using the length first).  The
   number of tables is quite big on purpose (a normal big application
   has less than 1000 classes), so that you shouldn't normally get any
   collisions, and get away with a single comparison (which we can't
   avoid since we need to know that you have got the right thing).  */
#define CLASS_TABLE_SIZE 1024
#define CLASS_TABLE_MASK 1023

static class_node_ptr class_table_array[CLASS_TABLE_SIZE];

/* The table writing mutex - we lock on writing to avoid conflicts
   between different writers, but we read without locks.  That is
   possible because we assume pointer assignment to be an atomic
   operation.  TODO: This is only true under certain circumstances,
   which should be clarified.  */
static objc_mutex_t __class_table_lock = NULL;

/* CLASS_TABLE_HASH is how we compute the hash of a class name.  It is
   a macro - *not* a function - arguments *are* modified directly.

   INDEX should be a variable holding an int;
   HASH should be a variable holding an int;
   CLASS_NAME should be a variable holding a (char *) to the class_name.  

   After the macro is executed, INDEX contains the length of the
   string, and HASH the computed hash of the string; CLASS_NAME is
   untouched.  */

#define CLASS_TABLE_HASH(INDEX, HASH, CLASS_NAME)          \
  HASH = 0;                                                  \
  for (INDEX = 0; CLASS_NAME[INDEX] != '\0'; INDEX++)        \
    {                                                        \
      HASH = (HASH << 4) ^ (HASH >> 28) ^ CLASS_NAME[INDEX]; \
    }                                                        \
                                                             \
  HASH = (HASH ^ (HASH >> 10) ^ (HASH >> 20)) & CLASS_TABLE_MASK;

/* Setup the table.  */
static void
class_table_setup (void)
{
  /* Start - nothing in the table.  */
  memset (class_table_array, 0, sizeof (class_node_ptr) * CLASS_TABLE_SIZE);

  /* The table writing mutex.  */
  __class_table_lock = objc_mutex_allocate ();
}


/* Insert a class in the table (used when a new class is
   registered).  */
static void 
class_table_insert (const char *class_name, Class class_pointer)
{
  int hash, length;
  class_node_ptr new_node;

  /* Find out the class name's hash and length.  */
  CLASS_TABLE_HASH (length, hash, class_name);
  
  /* Prepare the new node holding the class.  */
  new_node = objc_malloc (sizeof (struct class_node));
  new_node->name = class_name;
  new_node->length = length;
  new_node->pointer = class_pointer;

  /* Lock the table for modifications.  */
  objc_mutex_lock (__class_table_lock);
  
  /* Insert the new node in the table at the beginning of the table at
     class_table_array[hash].  */
  new_node->next = class_table_array[hash];
  class_table_array[hash] = new_node;
  
  objc_mutex_unlock (__class_table_lock);
}

/* Get a class from the table.  This does not need mutex protection.
   Currently, this function is called each time you call a static
   method, this is why it must be very fast.  */
static inline Class 
class_table_get_safe (const char *class_name)
{
  class_node_ptr node;  
  int length, hash;

  /* Compute length and hash.  */
  CLASS_TABLE_HASH (length, hash, class_name);
  
  node = class_table_array[hash];
  
  if (node != NULL)
    {
      do
        {
          if (node->length == length)
            {
              /* Compare the class names.  */
              int i;

              for (i = 0; i < length; i++)
                {
                  if ((node->name)[i] != class_name[i]) 
		    break;
                }
              
              if (i == length)
                {
                  /* They are equal!  */
                  return node->pointer;
                }
            }
        }
      while ((node = node->next) != NULL);
    }

  return Nil;
}

/* Enumerate over the class table.  */
struct class_table_enumerator
{
  int hash;
  class_node_ptr node;
};


static Class
class_table_next (struct class_table_enumerator **e)
{
  struct class_table_enumerator *enumerator = *e;
  class_node_ptr next;
  
  if (enumerator == NULL)
    {
       *e = objc_malloc (sizeof (struct class_table_enumerator));
      enumerator = *e;
      enumerator->hash = 0;
      enumerator->node = NULL;

      next = class_table_array[enumerator->hash];
    }
  else
    next = enumerator->node->next;
  
  if (next != NULL)
    {
      enumerator->node = next;
      return enumerator->node->pointer;
    }
  else 
    {
      enumerator->hash++;
     
      while (enumerator->hash < CLASS_TABLE_SIZE)
        {
          next = class_table_array[enumerator->hash];
          if (next != NULL)
            {
              enumerator->node = next;
              return enumerator->node->pointer;
            }
          enumerator->hash++;
        }
      
      /* Ok - table finished - done.  */
      objc_free (enumerator);
      return Nil;
    }
}

#if 0 /* DEBUGGING FUNCTIONS */
/* Debugging function - print the class table.  */
void
class_table_print (void)
{
  int i;
  
  for (i = 0; i < CLASS_TABLE_SIZE; i++)
    {
      class_node_ptr node;
      
      printf ("%d:\n", i);
      node = class_table_array[i];
      
      while (node != NULL)
        {
          printf ("\t%s\n", node->name);
          node = node->next;
        }
    }
}

/* Debugging function - print an histogram of number of classes in
   function of hash key values.  Useful to evaluate the hash function
   in real cases.  */
void
class_table_print_histogram (void)
{
  int i, j;
  int counter = 0;
  
  for (i = 0; i < CLASS_TABLE_SIZE; i++)
    {
      class_node_ptr node;
      
      node = class_table_array[i];
      
      while (node != NULL)
        {
          counter++;
          node = node->next;
        }
      if (((i + 1) % 50) == 0)
        {
          printf ("%4d:", i + 1);
          for (j = 0; j < counter; j++)
	    printf ("X");

          printf ("\n");
          counter = 0;
        }
    }
  printf ("%4d:", i + 1);
  for (j = 0; j < counter; j++)
    printf ("X");

  printf ("\n");
}
#endif /* DEBUGGING FUNCTIONS */

/**
 ** Objective-C runtime functions
 **/

/* From now on, the only access to the class table data structure
   should be via the class_table_* functions.  */

/* This is a hook which is called by objc_get_class and
   objc_lookup_class if the runtime is not able to find the class.
   This may e.g. try to load in the class using dynamic loading.

   This hook was a public, global variable in the Traditional GNU
   Objective-C Runtime API (objc/objc-api.h).  The modern GNU
   Objective-C Runtime API (objc/runtime.h) provides the
   objc_setGetUnknownClassHandler() function instead.
*/
Class (*_objc_lookup_class) (const char *name) = 0;      /* !T:SAFE */

/* The handler currently in use.  PS: if both
   __obj_get_unknown_class_handler and _objc_lookup_class are defined,
   __objc_get_unknown_class_handler is called first.  */
static objc_get_unknown_class_handler
__objc_get_unknown_class_handler = NULL;

objc_get_unknown_class_handler
objc_setGetUnknownClassHandler (objc_get_unknown_class_handler 
				new_handler)
{
  objc_get_unknown_class_handler old_handler 
    = __objc_get_unknown_class_handler;
  __objc_get_unknown_class_handler = new_handler;
  return old_handler;
}


/* True when class links has been resolved.  */     
BOOL __objc_class_links_resolved = NO;                  /* !T:UNUSED */


void
__objc_init_class_tables (void)
{
  /* Allocate the class hash table.  */
  
  if (__class_table_lock)
    return;
  
  objc_mutex_lock (__objc_runtime_mutex);
  
  class_table_setup ();

  objc_mutex_unlock (__objc_runtime_mutex);
}  

/* This function adds a class to the class hash table, and assigns the
   class a number, unless it's already known.  Return 'YES' if the
   class was added.  Return 'NO' if the class was already known.  */
BOOL
__objc_add_class_to_hash (Class class)
{
  Class existing_class;

  objc_mutex_lock (__objc_runtime_mutex);

  /* Make sure the table is there.  */
  assert (__class_table_lock);

  /* Make sure it's not a meta class.  */
  assert (CLS_ISCLASS (class));

  /* Check to see if the class is already in the hash table.  */
  existing_class = class_table_get_safe (class->name);

  if (existing_class)
    {
      objc_mutex_unlock (__objc_runtime_mutex);
      return NO;      
    }
  else
    {
      /* The class isn't in the hash table.  Add the class and assign
         a class number.  */
      static unsigned int class_number = 1;
      
      CLS_SETNUMBER (class, class_number);
      CLS_SETNUMBER (class->class_pointer, class_number);

      ++class_number;
      class_table_insert (class->name, class);

      objc_mutex_unlock (__objc_runtime_mutex);
      return YES;
    }
}

Class
objc_getClass (const char *name)
{
  Class class;

  if (name == NULL)
    return Nil;

  class = class_table_get_safe (name);
  
  if (class)
    return class;

  if (__objc_get_unknown_class_handler)
    return (*__objc_get_unknown_class_handler) (name);

  if (_objc_lookup_class)
    return (*_objc_lookup_class) (name);

  return Nil;
}

Class
objc_lookUpClass (const char *name)
{
  if (name == NULL)
    return Nil;
  else
    return class_table_get_safe (name);
}

Class
objc_getMetaClass (const char *name)
{
  Class class = objc_getClass (name);

  if (class)
    return class->class_pointer;
  else
    return Nil;
}

Class
objc_getRequiredClass (const char *name)
{
  Class class = objc_getClass (name);

  if (class)
    return class;
  else
    _objc_abort ("objc_getRequiredClass ('%s') failed: class not found\n", name);
}

int
objc_getClassList (Class *returnValue, int maxNumberOfClassesToReturn)
{
  /* Iterate over all entries in the table.  */
  int hash, count = 0;

  for (hash = 0; hash < CLASS_TABLE_SIZE; hash++)
    {
      class_node_ptr node = class_table_array[hash];
      
      while (node != NULL)
	{
	  if (returnValue)
	    {
	      if (count < maxNumberOfClassesToReturn)
		returnValue[count] = node->pointer;
	      else
		return count;
	    }
	  count++;
	  node = node->next;
	}
    }
  
  return count;
}

Class
objc_allocateClassPair (Class super_class, const char *class_name, size_t extraBytes)
{
  Class new_class;
  Class new_meta_class;

  if (class_name == NULL)
    return Nil;

  if (objc_getClass (class_name))
    return Nil;

  if (super_class)
    {
      /* If you want to build a hierarchy of classes, you need to
	 build and register them one at a time.  The risk is that you
	 are able to cause confusion by registering a subclass before
	 the superclass or similar.  */
      if (CLS_IS_IN_CONSTRUCTION (super_class))
	return Nil;
    }

  /* Technically, we should create the metaclass first, then use
     class_createInstance() to create the class.  That complication
     would be relevant if we had class variables, but we don't, so we
     just ignore it and create everything directly and assume all
     classes have the same size.  */
  new_class = objc_calloc (1, sizeof (struct objc_class) + extraBytes);
  new_meta_class = objc_calloc (1, sizeof (struct objc_class) + extraBytes);

  /* We create an unresolved class, similar to one generated by the
     compiler.  It will be resolved later when we register it.

     Note how the metaclass details are not that important; when the
     class is resolved, the ones that matter will be fixed up.  */
  new_class->class_pointer = new_meta_class;
  new_meta_class->class_pointer = 0;

  if (super_class)
    {
      /* Force the name of the superclass in place of the link to the
	 actual superclass, which will be put there when the class is
	 resolved.  */
      const char *super_class_name = class_getName (super_class);
      new_class->super_class = (void *)super_class_name;
      new_meta_class->super_class = (void *)super_class_name;
    }
  else
    {
      new_class->super_class = (void *)0;
      new_meta_class->super_class = (void *)0;
    }

  new_class->name = objc_malloc (strlen (class_name) + 1);
  strcpy ((char*)new_class->name, class_name);
  new_meta_class->name = new_class->name;

  new_class->version = 0;
  new_meta_class->version = 0;

  new_class->info = _CLS_CLASS | _CLS_IN_CONSTRUCTION;
  new_meta_class->info = _CLS_META | _CLS_IN_CONSTRUCTION;

  if (super_class)
    new_class->instance_size = super_class->instance_size;
  else
    new_class->instance_size = 0;
  new_meta_class->instance_size = sizeof (struct objc_class);

  return new_class;
}

void
objc_registerClassPair (Class class_)
{
  if (class_ == Nil)
    return;

  if ((! CLS_ISCLASS (class_)) || (! CLS_IS_IN_CONSTRUCTION (class_)))
    return;

  if ((! CLS_ISMETA (class_->class_pointer)) || (! CLS_IS_IN_CONSTRUCTION (class_->class_pointer)))
    return;

  objc_mutex_lock (__objc_runtime_mutex);

  if (objc_getClass (class_->name))
    {
      objc_mutex_unlock (__objc_runtime_mutex);
      return;
    }

  CLS_SET_NOT_IN_CONSTRUCTION (class_);
  CLS_SET_NOT_IN_CONSTRUCTION (class_->class_pointer);

  __objc_init_class (class_);

  /* Resolve class links immediately.  No point in waiting.  */
  __objc_resolve_class_links ();

  objc_mutex_unlock (__objc_runtime_mutex);
}

void
objc_disposeClassPair (Class class_)
{
  if (class_ == Nil)
    return;

  if ((! CLS_ISCLASS (class_)) || (! CLS_IS_IN_CONSTRUCTION (class_)))
    return;

  if ((! CLS_ISMETA (class_->class_pointer)) || (! CLS_IS_IN_CONSTRUCTION (class_->class_pointer)))
    return;

  /* Undo any class_addIvar().  */
  if (class_->ivars)
    {
      int i;
      for (i = 0; i < class_->ivars->ivar_count; i++)
	{
	  struct objc_ivar *ivar = &(class_->ivars->ivar_list[i]);

	  objc_free ((char *)ivar->ivar_name);
	  objc_free ((char *)ivar->ivar_type);
	}
      
      objc_free (class_->ivars);
    }

  /* Undo any class_addMethod().  */
  if (class_->methods)
    {
      struct objc_method_list *list = class_->methods;
      while (list)
	{
	  int i;
	  struct objc_method_list *next = list->method_next;

	  for (i = 0; i < list->method_count; i++)
	    {
	      struct objc_method *method = &(list->method_list[i]);

	      objc_free ((char *)method->method_name);
	      objc_free ((char *)method->method_types);
	    }

	  objc_free (list);
	  list = next;
	}
    }

  /* Undo any class_addProtocol().  */
  if (class_->protocols)
    {
      struct objc_protocol_list *list = class_->protocols;
      while (list)
	{
	  struct objc_protocol_list *next = list->next;

	  objc_free (list);
	  list = next;
	}
    }
  
  /* Undo any class_addMethod() on the meta-class.  */
  if (class_->class_pointer->methods)
    {
      struct objc_method_list *list = class_->class_pointer->methods;
      while (list)
	{
	  int i;
	  struct objc_method_list *next = list->method_next;

	  for (i = 0; i < list->method_count; i++)
	    {
	      struct objc_method *method = &(list->method_list[i]);

	      objc_free ((char *)method->method_name);
	      objc_free ((char *)method->method_types);
	    }

	  objc_free (list);
	  list = next;
	}
    }

  /* Undo objc_allocateClassPair().  */
  objc_free ((char *)(class_->name));
  objc_free (class_->class_pointer);
  objc_free (class_);
}

/* Traditional GNU Objective-C Runtime API.  Important: this method is
   called automatically by the compiler while messaging (if using the
   traditional ABI), so it is worth keeping it fast; don't make it
   just a wrapper around objc_getClass().  */
/* Note that this is roughly equivalent to objc_getRequiredClass().  */
/* Get the class object for the class named NAME.  If NAME does not
   identify a known class, the hook _objc_lookup_class is called.  If
   this fails, an error message is issued and the system aborts.  */
Class
objc_get_class (const char *name)
{
  Class class;

  class = class_table_get_safe (name);

  if (class)
    return class;

  if (__objc_get_unknown_class_handler)
    class = (*__objc_get_unknown_class_handler) (name);

  if ((!class)  &&  _objc_lookup_class)
    class = (*_objc_lookup_class) (name);

  if (class)
    return class;
  
  _objc_abort ("objc runtime: cannot find class %s\n", name);

  return 0;
}

/* This is used by the compiler too.  */
Class
objc_get_meta_class (const char *name)
{
  return objc_get_class (name)->class_pointer;
}

/* This is not used by GCC, but the clang compiler seems to use it
   when targetting the GNU runtime.  That's wrong, but we have it to
   be compatible.  */
Class
objc_lookup_class (const char *name)
{
  return objc_getClass (name);
}

/* This is used when the implementation of a method changes.  It goes
   through all classes, looking for the ones that have these methods
   (either method_a or method_b; method_b can be NULL), and reloads
   the implementation for these.  You should call this with the
   runtime mutex already locked.  */
void
__objc_update_classes_with_methods (struct objc_method *method_a, struct objc_method *method_b)
{
  int hash;

  /* Iterate over all classes.  */
  for (hash = 0; hash < CLASS_TABLE_SIZE; hash++)
    {
      class_node_ptr node = class_table_array[hash];
      
      while (node != NULL)
	{
	  /* We execute this loop twice: the first time, we iterate
	     over all methods in the class (instance methods), while
	     the second time we iterate over all methods in the meta
	     class (class methods).  */
	  Class class = Nil;
	  BOOL done = NO;

	  while (done == NO)
	    {
	      struct objc_method_list * method_list;

	      if (class == Nil)
		{
		  /* The first time, we work on the class.  */
		  class = node->pointer;
		}
	      else
		{
		  /* The second time, we work on the meta class.  */
		  class = class->class_pointer;
		  done = YES;
		}

	      method_list = class->methods;

	      while (method_list)
		{
		  int i;
		  
		  for (i = 0; i < method_list->method_count; ++i)
		    {
		      struct objc_method *method = &method_list->method_list[i];
		      
		      /* If the method is one of the ones we are
			 looking for, update the implementation.  */
		      if (method == method_a)
			sarray_at_put_safe (class->dtable,
					    (sidx) method_a->method_name->sel_id,
					    method_a->method_imp);
		      
		      if (method == method_b)
			{
			  if (method_b != NULL)
			    sarray_at_put_safe (class->dtable,
						(sidx) method_b->method_name->sel_id,
						method_b->method_imp);
			}
		    }
		  
		  method_list = method_list->method_next;
		}
	    }
	  node = node->next;
	}
    }
}

/* Resolve super/subclass links for all classes.  The only thing we
   can be sure of is that the class_pointer for class objects point to
   the right meta class objects.  */
void
__objc_resolve_class_links (void)
{
  struct class_table_enumerator *es = NULL;
  Class object_class = objc_get_class ("Object");
  Class class1;

  assert (object_class);

  objc_mutex_lock (__objc_runtime_mutex);

  /* Assign subclass links.  */
  while ((class1 = class_table_next (&es)))
    {
      /* Make sure we have what we think we have.  */
      assert (CLS_ISCLASS (class1));
      assert (CLS_ISMETA (class1->class_pointer));

      /* The class_pointer of all meta classes point to Object's meta
         class.  */
      class1->class_pointer->class_pointer = object_class->class_pointer;

      if (! CLS_ISRESOLV (class1))
        {
          CLS_SETRESOLV (class1);
          CLS_SETRESOLV (class1->class_pointer);
              
          if (class1->super_class)
            {   
              Class a_super_class 
                = objc_get_class ((char *) class1->super_class);
              
              assert (a_super_class);
              
              DEBUG_PRINTF ("making class connections for: %s\n",
                            class1->name);
              
              /* Assign subclass links for superclass.  */
              class1->sibling_class = a_super_class->subclass_list;
              a_super_class->subclass_list = class1;
              
              /* Assign subclass links for meta class of superclass.  */
              if (a_super_class->class_pointer)
                {
                  class1->class_pointer->sibling_class
                    = a_super_class->class_pointer->subclass_list;
                  a_super_class->class_pointer->subclass_list 
                    = class1->class_pointer;
                }
            }
          else /* A root class, make its meta object be a subclass of
                  Object.  */
            {
              class1->class_pointer->sibling_class 
                = object_class->subclass_list;
              object_class->subclass_list = class1->class_pointer;
            }
        }
    }

  /* Assign superclass links.  */
   es = NULL;
   while ((class1 = class_table_next (&es)))
    {
      Class sub_class;
      for (sub_class = class1->subclass_list; sub_class;
           sub_class = sub_class->sibling_class)
        {
          sub_class->super_class = class1;
          if (CLS_ISCLASS (sub_class))
            sub_class->class_pointer->super_class = class1->class_pointer;
        }
    }

  objc_mutex_unlock (__objc_runtime_mutex);
}

const char *
class_getName (Class class_)
{
  if (class_ == Nil)
    return "nil";

  return class_->name;
}

BOOL
class_isMetaClass (Class class_)
{
  /* CLS_ISMETA includes the check for Nil class_.  */
  return CLS_ISMETA (class_);
}

/* Even inside libobjc it may be worth using class_getSuperclass
   instead of accessing class_->super_class directly because it
   resolves the class links if needed.  If you access
   class_->super_class directly, make sure to deal with the situation
   where the class is not resolved yet!  */
Class
class_getSuperclass (Class class_)
{
  if (class_ == Nil)
    return Nil;

  /* Classes that are in construction are not resolved, and still have
     the class name (instead of a class pointer) in the
     class_->super_class field.  In that case we need to lookup the
     superclass name to return the superclass.  We can not resolve the
     class until it is registered.  */
  if (CLS_IS_IN_CONSTRUCTION (class_))
    {
      if (CLS_ISMETA (class_))
	return object_getClass ((id)objc_lookUpClass ((const char *)(class_->super_class)));
      else
	return objc_lookUpClass ((const char *)(class_->super_class));
    }

  /* If the class is not resolved yet, super_class would point to a
     string (the name of the super class) as opposed to the actual
     super class.  In that case, we need to resolve the class links
     before we can return super_class.  */
  if (! CLS_ISRESOLV (class_))
    __objc_resolve_class_links ();
  
  return class_->super_class;
}

int
class_getVersion (Class class_)
{
  if (class_ == Nil)
    return 0;

  return (int)(class_->version);
}

void
class_setVersion (Class class_, int version)
{
  if (class_ == Nil)
    return;

  class_->version = version;
}

size_t
class_getInstanceSize (Class class_)
{
  if (class_ == Nil)
    return 0;

  return class_->instance_size;
}

