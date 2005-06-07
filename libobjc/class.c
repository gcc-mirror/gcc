/* GNU Objective C Runtime class related functions
   Copyright (C) 1993, 1995, 1996, 1997, 2001, 2002
     Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup and Dennis Glatting.

   Lock-free class table code designed and written from scratch by
   Nicola Pero, 2001.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
GCC; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

/*
  The code in this file critically affects class method invocation
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

#include "objc/runtime.h"            /* the kitchen sink */
#include "objc/sarray.h"

#include "objc/objc.h"
#include "objc/objc-api.h"
#include "objc/thr.h"

/* We use a table which maps a class name to the corresponding class
 * pointer.  The first part of this file defines this table, and
 * functions to do basic operations on the table.  The second part of
 * the file implements some higher level Objective-C functionality for
 * classes by using the functions provided in the first part to manage
 * the table. */

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
   operation.  */
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


/* Insert a class in the table (used when a new class is registered).  */
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

/* Replace a class in the table (used only by poseAs:).  */
static void 
class_table_replace (Class old_class_pointer, Class new_class_pointer)
{
  int hash;
  class_node_ptr node;

  objc_mutex_lock (__class_table_lock);
  
  hash = 0;
  node = class_table_array[hash];
  
  while (hash < CLASS_TABLE_SIZE)
    {
      if (node == NULL)
        {
          hash++;
          if (hash < CLASS_TABLE_SIZE)
            {
              node = class_table_array[hash];
            }
        }
      else
        {
          Class class1 = node->pointer;

          if (class1 == old_class_pointer)
            {
              node->pointer = new_class_pointer;
            }
          node = node->next;
        }
    }

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
                    {
                      break;
                    }
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
    {
      next = enumerator->node->next;
    }
  
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
            {
              printf ("X");
            }
          printf ("\n");
          counter = 0;
        }
    }
  printf ("%4d:", i + 1);
  for (j = 0; j < counter; j++)
    {
      printf ("X");
    }
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
   This may e.g. try to load in the class using dynamic loading.  */
Class (*_objc_lookup_class) (const char *name) = 0;      /* !T:SAFE */


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
   class a number, unless it's already known.  */
void
__objc_add_class_to_hash (Class class)
{
  Class h_class;

  objc_mutex_lock (__objc_runtime_mutex);

  /* Make sure the table is there.  */
  assert (__class_table_lock);

  /* Make sure it's not a meta class.  */
  assert (CLS_ISCLASS (class));

  /* Check to see if the class is already in the hash table.  */
  h_class = class_table_get_safe (class->name);
  if (! h_class)
    {
      /* The class isn't in the hash table.  Add the class and assign a class
         number.  */
      static unsigned int class_number = 1;

      CLS_SETNUMBER (class, class_number);
      CLS_SETNUMBER (class->class_pointer, class_number);

      ++class_number;
      class_table_insert (class->name, class);
    }

  objc_mutex_unlock (__objc_runtime_mutex);
}

/* Get the class object for the class named NAME.  If NAME does not
   identify a known class, the hook _objc_lookup_class is called.  If
   this fails, nil is returned.  */
Class
objc_lookup_class (const char *name)
{
  Class class;

  class = class_table_get_safe (name);

  if (class)
    return class;

  if (_objc_lookup_class)
    return (*_objc_lookup_class) (name);
  else
    return 0;
}

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

  if (_objc_lookup_class)
    class = (*_objc_lookup_class) (name);

  if (class)
    return class;
  
  objc_error (nil, OBJC_ERR_BAD_CLASS, 
              "objc runtime: cannot find class %s\n", name);
  return 0;
}

MetaClass
objc_get_meta_class (const char *name)
{
  return objc_get_class (name)->class_pointer;
}

/* This function provides a way to enumerate all the classes in the
   executable.  Pass *ENUM_STATE == NULL to start the enumeration.  The
   function will return 0 when there are no more classes.  
   For example: 
       id class; 
       void *es = NULL;
       while ((class = objc_next_class (&es)))
         ... do something with class; 
*/
Class
objc_next_class (void **enum_state)
{
  Class class;

  objc_mutex_lock (__objc_runtime_mutex);
  
  /* Make sure the table is there.  */
  assert (__class_table_lock);

  class = class_table_next ((struct class_table_enumerator **) enum_state);

  objc_mutex_unlock (__objc_runtime_mutex);
  
  return class;
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



#define CLASSOF(c) ((c)->class_pointer)

Class
class_pose_as (Class impostor, Class super_class)
{
  if (! CLS_ISRESOLV (impostor))
    __objc_resolve_class_links ();

  /* Preconditions */
  assert (impostor);
  assert (super_class);
  assert (impostor->super_class == super_class);
  assert (CLS_ISCLASS (impostor));
  assert (CLS_ISCLASS (super_class));
  assert (impostor->instance_size == super_class->instance_size);

  {
    Class *subclass = &(super_class->subclass_list);

    /* Move subclasses of super_class to impostor.  */
    while (*subclass)
      {
        Class nextSub = (*subclass)->sibling_class;

        if (*subclass != impostor)
          {
            Class sub = *subclass;

            /* Classes */
            sub->sibling_class = impostor->subclass_list;
            sub->super_class = impostor;
            impostor->subclass_list = sub;

            /* It will happen that SUB is not a class object if it is
               the top of the meta class hierarchy chain (root
               meta-class objects inherit their class object).  If
               that is the case... don't mess with the meta-meta
               class.  */
            if (CLS_ISCLASS (sub))
              {
                /* Meta classes */
                CLASSOF (sub)->sibling_class = 
                  CLASSOF (impostor)->subclass_list;
                CLASSOF (sub)->super_class = CLASSOF (impostor);
                CLASSOF (impostor)->subclass_list = CLASSOF (sub);
              }
          }

        *subclass = nextSub;
      }

    /* Set subclasses of superclass to be impostor only.  */
    super_class->subclass_list = impostor;
    CLASSOF (super_class)->subclass_list = CLASSOF (impostor);
    
    /* Set impostor to have no sibling classes.  */
    impostor->sibling_class = 0;
    CLASSOF (impostor)->sibling_class = 0;
  }
  
  /* Check relationship of impostor and super_class is kept.  */
  assert (impostor->super_class == super_class);
  assert (CLASSOF (impostor)->super_class == CLASSOF (super_class));

  /* This is how to update the lookup table.  Regardless of what the
     keys of the hashtable is, change all values that are superclass
     into impostor.  */

  objc_mutex_lock (__objc_runtime_mutex);

  class_table_replace (super_class, impostor);

  objc_mutex_unlock (__objc_runtime_mutex);

  /* Next, we update the dispatch tables...  */
  __objc_update_dispatch_table_for_class (CLASSOF (impostor));
  __objc_update_dispatch_table_for_class (impostor);

  return impostor;
}
