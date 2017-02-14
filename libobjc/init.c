/* GNU Objective C Runtime initialization 
   Copyright (C) 1993-2017 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup
   +load support contributed by Ovidiu Predescu <ovidiu@net-community.com>

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

#include "objc-private/common.h"
#include "objc-private/error.h"
#include "objc/runtime.h"
#include "objc/thr.h"
#include "objc-private/hash.h"
#include "objc-private/objc-list.h" 
#include "objc-private/module-abi-8.h" 
#include "objc-private/runtime.h"   /* For __objc_resolve_class_links().  */
#include "objc-private/selector.h"  /* For __sel_register_typed_name().  */
#include "objc-private/objc-sync.h" /* For __objc_sync_init() */
#include "objc-private/protocols.h" /* For __objc_protocols_init(),
				       __objc_protocols_add_protocol()
				       __objc_protocols_register_selectors() */
#include "objc-private/accessors.h" /* For __objc_accessors_init() */

/* The version number of this runtime.  This must match the number
   defined in gcc (objc-act.c).  */
#define OBJC_VERSION 8
#define PROTOCOL_VERSION 2

/* This list contains modules currently loaded into the runtime and
   for which the +load method (and the load callback, if any) has not
   been called yet.  */
static struct objc_list *__objc_module_list = 0; 	/* !T:MUTEX */

/* This list contains all proto_list's not yet assigned class
   links.  */
static struct objc_list *unclaimed_proto_list = 0; 	/* !T:MUTEX */

/* List of unresolved static instances.  */
static struct objc_list *uninitialized_statics = 0; 	/* !T:MUTEX */

/* List of duplicated classes found while loading modules.  If we find
   a class twice, we ignore it the second time.  On some platforms,
   where the order in which modules are loaded is well defined, this
   allows you to replace a class in a shared library by linking in a
   new implementation which is loaded in in the right order, and which
   overrides the existing one.

   Protected by __objc_runtime_mutex.  */
static cache_ptr duplicate_classes = NULL;

/* Global runtime "write" mutex.  Having a single mutex prevents
   deadlocks, but reduces concurrency.  To improve concurrency, some
   groups of functions in the runtime have their own separate mutex
   (eg, __class_table_lock in class.c); to avoid deadlocks, these
   routines must make sure that they never acquire any other lock
   while holding their own local lock.  Ie, they should lock, execute
   some C code that does not perform any calls to other runtime
   functions which may potentially lock different locks, then unlock.
   If they need to perform any calls to other runtime functions that
   may potentially lock other locks, then they should use the global
   __objc_runtime_mutex.  */
objc_mutex_t __objc_runtime_mutex = 0;

/* Number of threads that are alive.  */
int __objc_runtime_threads_alive = 1;			/* !T:MUTEX */

/* Check compiler vs runtime version.  */
static void init_check_module_version (struct objc_module *);

/* Assign isa links to protos.  */
static void __objc_init_protocols (struct objc_protocol_list *protos);

/* Assign isa link to a protocol, and register it.  */
static void __objc_init_protocol (struct objc_protocol *protocol);

/* Add protocol to class.  */
static void __objc_class_add_protocols (Class, struct objc_protocol_list *);

/* Load callback hook.  */
void (*_objc_load_callback) (Class class, struct objc_category *category) = 0; /* !T:SAFE */

/* Are all categories/classes resolved ?  */
BOOL __objc_dangling_categories = NO;           /* !T:UNUSED */

/* Sends +load to all classes and categories in certain
   situations.  */
static void objc_send_load (void);

/* Inserts all the classes defined in module in a tree of classes that
   resembles the class hierarchy.  This tree is traversed in preorder
   and the classes in its nodes receive the +load message if these
   methods were not executed before.  The algorithm ensures that when
   the +load method of a class is executed all the superclasses have
   been already received the +load message.  */
static void __objc_create_classes_tree (struct objc_module *module);

/* Calls the _objc_load_callback for each class and category in the
   module (if _objc_load_callback is not NULL).  */
static void __objc_call_load_callback (struct objc_module *module);

/* A special version that works only before the classes are completely
   installed in the runtime.  */
static BOOL class_is_subclass_of_class (Class class, Class superclass);

/* This is a node in the class tree hierarchy used to send +load
   messages.  */
typedef struct objc_class_tree
{
  /* The class corresponding to the node.  */
  Class class;

  /* This is a linked list of all the direct subclasses of this class.
     'head' points to a subclass node; 'tail' points to the next
     objc_list node (whose 'head' points to another subclass node,
     etc).  */
  struct objc_list *subclasses;
} objc_class_tree;

/* This is a linked list of objc_class_tree trees.  The head of these
   trees are root classes (their super class is Nil).  These different
   trees represent different class hierarchies.  */
static struct objc_list *__objc_class_tree_list = NULL;

/* Keeps the +load methods who have been already executed. This hash
   should not be destroyed during the execution of the program.  */
static cache_ptr __objc_load_methods = NULL;

/* This function is used when building the class tree used to send
   ordinately the +load message to all classes needing it.  The tree
   is really needed so that superclasses will get the message before
   subclasses.

   This tree may contain classes which are being loaded (or have just
   being loaded), and whose super_class pointers have not yet been
   resolved.  This implies that their super_class pointers point to a
   string with the name of the superclass; when the first message is
   sent to the class (/an object of that class) the class links will
   be resolved, which will replace the super_class pointers with
   pointers to the actual superclasses.

   Unfortunately, the tree might also contain classes which had been
   loaded previously, and whose class links have already been
   resolved.

   This function returns the superclass of a class in both cases, and
   can be used to build the determine the class relationships while
   building the tree.  */
static Class  class_superclass_of_class (Class class)
{
  char *super_class_name;

  /* If the class links have been resolved, use the resolved
     links.  */
  if (CLS_ISRESOLV (class))
    return class->super_class;
  
  /* Else, 'class' has not yet been resolved.  This means that its
     super_class pointer is really the name of the super class (rather
     than a pointer to the actual superclass).  */
  super_class_name = (char *)class->super_class;

  /* Return Nil for a root class.  */
  if (super_class_name == NULL)
    return Nil;

  /* Lookup the superclass of non-root classes.  */
  return objc_getClass (super_class_name);
}


/* Creates a tree of classes whose topmost class is directly inherited
   from `upper' and the bottom class in this tree is `bottom_class'.
   If `upper' is Nil, creates a class hierarchy up to a root class.
   The classes in this tree are super classes of `bottom_class'.  The
   `subclasses' member of each tree node point to the list of
   subclasses for the node.  */
static objc_class_tree *
create_tree_of_subclasses_inherited_from (Class bottom_class, Class upper)
{
  Class superclass;
  objc_class_tree *tree, *prev;

  DEBUG_PRINTF ("create_tree_of_subclasses_inherited_from:");
  DEBUG_PRINTF (" bottom_class = %s, upper = %s\n",
		(bottom_class ? bottom_class->name : NULL),
		(upper ? upper->name : NULL));

  superclass = class_superclass_of_class (bottom_class);

  prev = objc_calloc (1, sizeof (objc_class_tree));
  prev->class = bottom_class;

  if (superclass == upper)
    return prev;

  while (superclass != upper)
    {
      tree = objc_calloc (1, sizeof (objc_class_tree));
      tree->class = superclass;
      tree->subclasses = list_cons (prev, tree->subclasses);
      superclass = class_superclass_of_class (superclass);
      prev = tree;
    }

  return tree;
}

/* Insert the `class' into the proper place in the `tree' class
   hierarchy.  This function returns a new tree if the class has been
   successfully inserted into the tree or NULL if the class is not
   part of the classes hierarchy described by `tree'.  This function
   is private to objc_tree_insert_class (), you should not call it
   directly.  */
static objc_class_tree *
__objc_tree_insert_class (objc_class_tree *tree, Class class)
{
  DEBUG_PRINTF ("__objc_tree_insert_class: tree = %p (root: %s), class = %s\n",
		tree, ((tree && tree->class) ? tree->class->name : "Nil"), class->name);

  if (tree == NULL)
    return create_tree_of_subclasses_inherited_from (class, NULL);
  else if (class == tree->class)
    {
      /* `class' has been already inserted.  */
      DEBUG_PRINTF (" 1. class %s was previously inserted\n", class->name);
      return tree;
    }
  else if (class_superclass_of_class (class) == tree->class)
    {
      /* If class is a direct subclass of tree->class then add class
	 to the list of subclasses. First check to see if it wasn't
	 already inserted.  */
      struct objc_list *list = tree->subclasses;
      objc_class_tree *node;

      while (list)
	{
	  /* Class has been already inserted; do nothing just return
	     the tree.  */
	  if (((objc_class_tree *) list->head)->class == class)
	    {
	      DEBUG_PRINTF (" 2. class %s was previously inserted\n",
			    class->name);
	      return tree;
	    }
	  list = list->tail;
	}

      /* Create a new node class and insert it into the list of
	 subclasses.  */
      node = objc_calloc (1, sizeof (objc_class_tree));
      node->class = class;
      tree->subclasses = list_cons (node, tree->subclasses);
      DEBUG_PRINTF (" 3. class %s inserted\n", class->name);
      return tree;
    }
  else
    {
      /* The class is not a direct subclass of tree->class.  Search
         for class's superclasses in the list of subclasses.  */
      struct objc_list *subclasses = tree->subclasses;

      /* Precondition: the class must be a subclass of tree->class;
         otherwise return NULL to indicate our caller that it must
         take the next tree.  */
      if (! class_is_subclass_of_class (class, tree->class))
	return NULL;

      for (; subclasses != NULL; subclasses = subclasses->tail)
	{
	  Class aClass = ((objc_class_tree *) (subclasses->head))->class;

	  if (class_is_subclass_of_class (class, aClass))
	    {
	      /* If we found one of class's superclasses we insert the
	         class into its subtree and return the original tree
	         since nothing has been changed.  */
	      subclasses->head
		  = __objc_tree_insert_class (subclasses->head, class);
 	      DEBUG_PRINTF (" 4. class %s inserted\n", class->name);
	      return tree;
	    }
	}

      /* We haven't found a subclass of `class' in the `subclasses'
         list.  Create a new tree of classes whose topmost class is a
         direct subclass of tree->class.  */
      {
	objc_class_tree *new_tree
	  = create_tree_of_subclasses_inherited_from (class, tree->class);
	tree->subclasses = list_cons (new_tree, tree->subclasses);
 	DEBUG_PRINTF (" 5. class %s inserted\n", class->name);
	return tree;
      }
    }
}

/* This function inserts `class' in the right tree hierarchy classes.  */
static void
objc_tree_insert_class (Class class)
{
  struct objc_list *list_node;
  objc_class_tree *tree;
  
  list_node = __objc_class_tree_list;
  while (list_node)
    {
      /* Try to insert the class in this class hierarchy.  */
      tree = __objc_tree_insert_class (list_node->head, class);
      if (tree)
	{
	  list_node->head = tree;
	  return;
	}
      else
	list_node = list_node->tail;
    }
  
  /* If the list was finished but the class hasn't been inserted, we
     don't have an existing class hierarchy that can accommodate it.
     Create a new one.  */
  __objc_class_tree_list = list_cons (NULL, __objc_class_tree_list);
  __objc_class_tree_list->head = __objc_tree_insert_class (NULL, class);
}

/* Traverse tree in preorder. Used to send +load.  */
static void
objc_preorder_traverse (objc_class_tree *tree,
			int level,
			void (*function) (objc_class_tree *, int))
{
  struct objc_list *node;

  (*function) (tree, level);
  for (node = tree->subclasses; node; node = node->tail)
    objc_preorder_traverse (node->head, level + 1, function);
}

/* Traverse tree in postorder. Used to destroy a tree.  */
static void
objc_postorder_traverse (objc_class_tree *tree,
			 int level,
			 void (*function) (objc_class_tree *, int))
{
  struct objc_list *node;

  for (node = tree->subclasses; node; node = node->tail)
    objc_postorder_traverse (node->head, level + 1, function);
  (*function) (tree, level);
}

/* Used to print a tree class hierarchy.  */
#ifdef DEBUG
static void
__objc_tree_print (objc_class_tree *tree, int level)
{
  int i;

  for (i = 0; i < level; i++)
    printf ("  ");
  printf ("%s\n", tree->class->name);
}
#endif

/* Walks on a linked list of methods in the reverse order and executes
   all the methods corresponding to the `+load' selector.  Walking in
   the reverse order assures the +load of class is executed first and
   then +load of categories because of the way in which categories are
   added to the class methods.  This function needs to be called with
   the objc_runtime_mutex locked.  */
static void
__objc_send_load_using_method_list (struct objc_method_list *method_list, Class class)
{
  static SEL load_selector = 0;
  int i;

  if (!method_list)
    return;

  /* This needs no lock protection because we are called with the
     objc_runtime_mutex locked.  */
  if (!load_selector)
    load_selector = sel_registerName ("load");

  /* method_list is a linked list of method lists; since we're
     executing in reverse order, we need to do the next list before we
     do this one.  */
  __objc_send_load_using_method_list (method_list->method_next, class);

  /* Search the method list.  */
  for (i = 0; i < method_list->method_count; i++)
    {
      struct objc_method *mth = &method_list->method_list[i];

      /* We are searching for +load methods that we haven't executed
	 yet.  */
      if (mth->method_name && sel_eq (mth->method_name, load_selector)
	  && ! objc_hash_is_key_in_hash (__objc_load_methods, mth->method_imp))
	{
	  /* Add this method into the +load hash table, so we won't
	     execute it again next time.  */
	  objc_hash_add (&__objc_load_methods,
			 mth->method_imp,
			 mth->method_imp);
	  
	  /* Call +load.  */
	  DEBUG_PRINTF (" begin of [%s +load]\n", class->name);
	  (*mth->method_imp) ((id)class, mth->method_name);
	  DEBUG_PRINTF (" end of [%s +load]\n", class->name);

	  break;
	}
    }
}

/* This function needs to be called with the objc_runtime_mutex
   locked.  */
static void
__objc_send_load (objc_class_tree *tree,
		  int level __attribute__ ((__unused__)))
{
  Class class = tree->class;
  struct objc_method_list *method_list = class->class_pointer->methods;

  DEBUG_PRINTF ("+load: need to send load to class '%s'\n", class->name);
  __objc_send_load_using_method_list (method_list, class);
}

static void
__objc_destroy_class_tree_node (objc_class_tree *tree,
				int level __attribute__ ((__unused__)))
{
  objc_free (tree);
}

/* This is used to check if the relationship between two classes
   before the runtime completely installs the classes.  */
static BOOL
class_is_subclass_of_class (Class class, Class superclass)
{
  for (; class != Nil;)
    {
      if (class == superclass)
	return YES;
      class = class_superclass_of_class (class);
    }

  return NO;
}

/* This list contains all the classes in the runtime system for whom
   their superclasses are not yet known to the runtime.  */
static struct objc_list *unresolved_classes = 0;

/* Extern function used to reference the Object class.  */
extern void __objc_force_linking (void);

void
__objc_force_linking (void)
{
  extern void __objc_linking (void);
  __objc_linking ();
}

/* Run through the statics list, removing modules as soon as all its
   statics have been initialized.  */
static void
objc_init_statics (void)
{
  struct objc_list **cell = &uninitialized_statics;
  struct objc_static_instances **statics_in_module;

  objc_mutex_lock (__objc_runtime_mutex);

  while (*cell)
    {
      int module_initialized = 1;

      for (statics_in_module = (*cell)->head;
	   *statics_in_module; statics_in_module++)
	{
	  struct objc_static_instances *statics = *statics_in_module;
	  Class class = objc_getClass (statics->class_name);

	  if (! class)
	    {
	      /* It is unfortunate that this will cause all the
		 statics initialization to be done again (eg, if we
		 already initialized constant strings, and are now
		 initializing protocols, setting module_initialized to
		 0 would cause constant strings to be initialized
		 again).  It would be good to be able to track if we
		 have already initialized some of them.  */
	      module_initialized = 0;
	    }
	  else
	    {
	      /* Note that if this is a list of Protocol objects, some
		 of them may have been initialized already (because
		 they were attached to classes or categories, and the
		 class/category loading code automatically fixes them
		 up), and some of them may not.  We really need to go
		 through the whole list to be sure!  Protocols are
		 also special because we want to register them and
		 register all their selectors.  */
	      id *inst;

	      if (strcmp (statics->class_name, "Protocol") == 0)
		{
		  /* Protocols are special, because not only we want
		     to fix up their class pointers, but we also want
		     to register them and their selectors with the
		     runtime.  */
		  for (inst = &statics->instances[0]; *inst; inst++)
		    __objc_init_protocol ((struct objc_protocol *)*inst);
		}
	      else
		{
		  /* Other static instances (typically constant
		     strings) are easier as we just fix up their class
		     pointers.  */
		  for (inst = &statics->instances[0]; *inst; inst++)		  
		    (*inst)->class_pointer = class;
		}
	    }
	}
      if (module_initialized)
	{
	  /* Remove this module from the uninitialized list.  */
	  struct objc_list *this = *cell;
	  *cell = this->tail;
	  objc_free (this);
	}
      else
	cell = &(*cell)->tail;
    }

  objc_mutex_unlock (__objc_runtime_mutex);
}

/* This function is called by constructor functions generated for each
   module compiled.  (_GLOBAL_$I$...) The purpose of this function is
   to gather the module pointers so that they may be processed by the
   initialization routines as soon as possible.  */
void
__objc_exec_class (struct objc_module *module)
{
  /* Have we processed any constructors previously?  This flag is used
     to indicate that some global data structures need to be
     built.  */
  static BOOL previous_constructors = 0;

  static struct objc_list *unclaimed_categories = 0;

  /* The symbol table (defined in objc-private/module-abi-8.h)
     generated by gcc.  */
  struct objc_symtab *symtab = module->symtab;

  /* The statics in this module.  */
  struct objc_static_instances **statics
    = symtab->defs[symtab->cls_def_cnt + symtab->cat_def_cnt];

  /* Entry used to traverse hash lists.  */
  struct objc_list **cell;

  /* The table of selector references for this module.  */
  struct objc_selector *selectors = symtab->refs;

  int i;

  DEBUG_PRINTF ("\n__objc_exec_class (%p) - start processing module...\n", module);

  /* Check gcc version.  */
  init_check_module_version (module);

  /* On the first call of this routine, initialize some data
     structures.  */
  if (! previous_constructors)
    {
	/* Initialize thread-safe system.  */
      __objc_init_thread_system ();
      __objc_runtime_threads_alive = 1;
      __objc_runtime_mutex = objc_mutex_allocate ();

      __objc_init_selector_tables ();
      __objc_init_class_tables ();
      __objc_init_dispatch_tables ();
      duplicate_classes = objc_hash_new (8,
					 (hash_func_type)objc_hash_ptr,
					 objc_compare_ptrs);
      __objc_load_methods = objc_hash_new (128, 
					   (hash_func_type)objc_hash_ptr,
					   objc_compare_ptrs);
      __objc_protocols_init ();
      __objc_accessors_init ();
      __objc_sync_init ();
      previous_constructors = 1;
    }

  /* Save the module pointer so that later we remember to call +load
     on all classes and categories on it.  */
  objc_mutex_lock (__objc_runtime_mutex);
  __objc_module_list = list_cons (module, __objc_module_list);

  /* Replace referenced selectors from names to SELs.  */
  if (selectors)
    {
      DEBUG_PRINTF (" registering selectors\n");
      __objc_register_selectors_from_module (selectors);
    }

  /* Parse the classes in the load module and gather selector
     information.  */
  for (i = 0; i < symtab->cls_def_cnt; ++i)
    {
      Class class = (Class) symtab->defs[i];
      const char *superclass = (char *) class->super_class;

      /* Make sure we have what we think.  */
      assert (CLS_ISCLASS (class));
      assert (CLS_ISMETA (class->class_pointer));
      DEBUG_PRINTF (" installing class '%s'\n", class->name);

      /* Workaround for a bug in clang: Clang may set flags other than
	 _CLS_CLASS and _CLS_META even when compiling for the
	 traditional ABI (version 8), confusing our runtime.  Try to
	 wipe these flags out.  */
      if (CLS_ISCLASS (class))
	__CLS_INFO (class) = _CLS_CLASS;
      else
	__CLS_INFO (class) = _CLS_META;

      /* Initialize the subclass list to be NULL.  In some cases it
	 isn't and this crashes the program.  */
      class->subclass_list = NULL;

      if (__objc_init_class (class))
	{
	  /* Check to see if the superclass is known in this point. If
	     it's not add the class to the unresolved_classes list.  */
	  if (superclass && ! objc_getClass (superclass))
	    unresolved_classes = list_cons (class, unresolved_classes);
	}
    }

  /* Process category information from the module.  */
  for (i = 0; i < symtab->cat_def_cnt; ++i)
    {
      struct objc_category *category = symtab->defs[i + symtab->cls_def_cnt];
      Class class = objc_getClass (category->class_name);
      
      /* If the class for the category exists then append its
	 methods.  */
      if (class)
	{
	  DEBUG_PRINTF (" installing category '%s (%s)'\n", category->class_name, category->category_name);
	  /* Do instance methods.  */
	  if (category->instance_methods)
	    class_add_method_list (class, category->instance_methods);

	  /* Do class methods.  */
	  if (category->class_methods)
	    class_add_method_list ((Class) class->class_pointer, 
				   category->class_methods);

	  if (category->protocols)
	    {
	      __objc_init_protocols (category->protocols);
	      __objc_class_add_protocols (class, category->protocols);
	    }

          /* Register the instance methods as class methods, this is
             only done for root classes.  */
          __objc_register_instance_methods_to_class (class);
	}
      else
	{
	  DEBUG_PRINTF (" delaying installation of category '%s (%s)'\n", category->class_name, category->category_name);
	  /* The object to which the category methods belong can't be
	     found.  Save the information.  */
	  unclaimed_categories = list_cons (category, unclaimed_categories);
	}
    }

  if (statics)
    uninitialized_statics = list_cons (statics, uninitialized_statics);
  if (uninitialized_statics)
    objc_init_statics ();

  /* Scan the unclaimed category hash.  Attempt to attach any
     unclaimed categories to objects.  */
  for (cell = &unclaimed_categories; *cell; )
    {
      struct objc_category *category = (*cell)->head;
      Class class = objc_getClass (category->class_name);
      
      if (class)
	{
	  DEBUG_PRINTF (" installing (delayed) category '%s (%s)'\n", category->class_name, category->category_name);
	  list_remove_head (cell);
	  
	  if (category->instance_methods)
	    class_add_method_list (class, category->instance_methods);
	  
	  if (category->class_methods)
	    class_add_method_list ((Class) class->class_pointer,
				   category->class_methods);

	  if (category->protocols)
	    {
	      __objc_init_protocols (category->protocols);
	      __objc_class_add_protocols (class, category->protocols);
	    }

          /* Register the instance methods as class methods, this is
             only done for root classes.  */
          __objc_register_instance_methods_to_class (class);
	}
      else
	cell = &(*cell)->tail;
    }
  
  if (unclaimed_proto_list && objc_getClass ("Protocol"))
    {
      list_mapcar (unclaimed_proto_list,
		   (void (*) (void *))__objc_init_protocols);
      list_free (unclaimed_proto_list);
      unclaimed_proto_list = 0;
    }

  objc_send_load ();

  /* Check if there are no unresolved classes (ie, classes whose
     superclass has not been loaded yet) and that the 'Object' class,
     used as the class of classes, exist.  If so, it is worth
     "resolving the class links" at this point, which will setup all
     the class/superclass pointers.  */
  if (!unresolved_classes && objc_getClass ("Object"))
    {
      DEBUG_PRINTF (" resolving class links\n");
      __objc_resolve_class_links ();
    }

  objc_mutex_unlock (__objc_runtime_mutex);

  DEBUG_PRINTF ("__objc_exec_class (%p) - finished processing module...\n\n", module);
}

/* This function needs to be called with the objc_runtime_mutex
   locked.  */
static void
objc_send_load (void)
{
  if (!__objc_module_list)
    return;
 
  /* Try to find out if all the classes loaded so far also have their
     superclasses known to the runtime.  We suppose that the objects
     that are allocated in the +load method are in general of a class
     declared in the same module.  */
  if (unresolved_classes)
    {
      Class class = unresolved_classes->head;

      while (objc_getClass ((char *) class->super_class))
	{
	  list_remove_head (&unresolved_classes);
	  if (unresolved_classes)
	    class = unresolved_classes->head;
	  else
	    break;
	}

      /* If we still have classes for whom we don't have yet their
         super classes known to the runtime we don't send the +load
         messages (and call the load callback) yet.  */
      if (unresolved_classes)
	return;
    }

  /* Special check.  If 'Object', which is used by meta-classes, has
     not been loaded yet, delay sending of +load.  */
  if (! objc_getClass ("Object"))
    return;

  /* Iterate over all modules in the __objc_module_list and call on
     them the __objc_create_classes_tree function.  This function
     creates a tree of classes that resembles the class hierarchy.  */
  list_mapcar (__objc_module_list,
	       (void (*) (void *)) __objc_create_classes_tree);

  while (__objc_class_tree_list)
    {
#ifdef DEBUG
      objc_preorder_traverse (__objc_class_tree_list->head,
			      0, __objc_tree_print);
#endif
      objc_preorder_traverse (__objc_class_tree_list->head,
			      0, __objc_send_load);
      objc_postorder_traverse (__objc_class_tree_list->head,
			      0, __objc_destroy_class_tree_node);
      list_remove_head (&__objc_class_tree_list);
    }

  /* For each module, call the _objc_load_callback if any is
     defined.  */
  list_mapcar (__objc_module_list, (void (*) (void *)) __objc_call_load_callback);

  /* Empty the list of modules.  */
  list_free (__objc_module_list);
  __objc_module_list = NULL;
}

static void
__objc_create_classes_tree (struct objc_module *module)
{
  /* The runtime mutex is locked at this point */
  struct objc_symtab *symtab = module->symtab;
  int i;

  /* Iterate through classes defined in this module and insert them in
     the classes tree hierarchy.  */
  for (i = 0; i < symtab->cls_def_cnt; i++)
    {
      Class class = (Class) symtab->defs[i];

      if (!objc_hash_is_key_in_hash (duplicate_classes, class))
	objc_tree_insert_class (class);
    }

  /* Now iterate over "claimed" categories too (ie, categories that
     extend a class that has already been loaded by the runtime), and
     insert them in the classes tree hiearchy too.  Otherwise, if you
     add a category, its +load method would not be called if the class
     is already loaded in the runtime.  It the category is
     "unclaimed", ie, we haven't loaded the main class yet, postpone
     sending +load as we want to execute +load from the class before
     we execute the one from the category.  */
  for (i = 0; i < symtab->cat_def_cnt; ++i)
    {
      struct objc_category *category = symtab->defs[i + symtab->cls_def_cnt];
      Class class = objc_getClass (category->class_name);
      
      /* If the class for the category exists then append its
	 methods.  */
      if (class)
	objc_tree_insert_class (class);
    }
}

static void
__objc_call_load_callback (struct objc_module *module)
{
  if (_objc_load_callback)
    {
      /* The runtime mutex is locked at this point.  */
      struct objc_symtab *symtab = module->symtab;
      int i;
      
      /* Iterate through classes defined in this module and call the callback
	 for each one.  */
      for (i = 0; i < symtab->cls_def_cnt; i++)
	{
	  Class class = (Class) symtab->defs[i];
	
	  if (!objc_hash_is_key_in_hash (duplicate_classes, class))
	    {
	      /* Call the _objc_load_callback for this class.  */
	      DEBUG_PRINTF (" calling the load callback for class '%s'\n", class->name);
	      _objc_load_callback (class, 0);
	    }
	}
      
      /* Call the _objc_load_callback for categories.  Don't register
	 the instance methods as class methods for categories to root
	 classes since they were already added in the class.  */
      for (i = 0; i < symtab->cat_def_cnt; i++)
	{
	  struct objc_category *category = symtab->defs[i + symtab->cls_def_cnt];
	  Class class = objc_getClass (category->class_name);
	  
	  DEBUG_PRINTF (" calling the load callback for category '%s (%s)'\n",
			category->class_name, category->category_name);
	  _objc_load_callback (class, category);
	}
    }
}

/* Sanity check the version of gcc used to compile `module'.  */
static void
init_check_module_version (struct objc_module *module)
{
  if ((module->version != OBJC_VERSION) || (module->size != sizeof (struct objc_module)))
    {
      _objc_abort ("Module %s version %d doesn't match runtime %d\n",
		   module->name, (int)module->version, OBJC_VERSION);
    }
}

/* __objc_init_class must be called with __objc_runtime_mutex already
   locked.  Return YES if the class could be setup; return NO if the
   class could not be setup because a class with the same name already
   exists.  */
BOOL
__objc_init_class (Class class)
{
  /* Store the class in the class table and assign class numbers.  */
  if (__objc_add_class_to_hash (class))
    {
      /* Register all of the selectors in the class and meta class.  */
      __objc_register_selectors_from_class (class);
      __objc_register_selectors_from_class ((Class) class->class_pointer);
      
      /* Install the fake dispatch tables.  */
      __objc_install_premature_dtable (class);
      __objc_install_premature_dtable (class->class_pointer);
      
      /* Register the instance methods as class methods, this is only
	 done for root classes.  */
      __objc_register_instance_methods_to_class (class);
      
      if (class->protocols)
	__objc_init_protocols (class->protocols);

      return YES;
    }
  else
    {
      /* The module contains a duplicate class.  Remember it so that
	 we will ignore it later.  */
      DEBUG_PRINTF (" duplicate class '%s' - will be ignored\n", class->name);
      objc_hash_add (&duplicate_classes, class, class);
      return NO;
    }
}

/* __objc_init_protocol must be called with __objc_runtime_mutex
   already locked, and the "Protocol" class already registered.  */
static void
__objc_init_protocol (struct objc_protocol *protocol)
{
  static Class proto_class = 0;

  if (! proto_class)
    proto_class = objc_getClass ("Protocol");

  if (((size_t)protocol->class_pointer) == PROTOCOL_VERSION)
    {
      /* Assign class pointer.  */
      protocol->class_pointer = proto_class;
      
      /* Register all the selectors in the protocol with the runtime.
	 This both registers the selectors with the right types, and
	 it also fixes up the 'struct objc_method' structures inside
	 the protocol so that each method_name (a char * as compiled
	 by the compiler) is replaced with the appropriate runtime
	 SEL.  */
      if (protocol->class_methods)
	__objc_register_selectors_from_description_list (protocol->class_methods);

      if (protocol->instance_methods)
	__objc_register_selectors_from_description_list (protocol->instance_methods);

      /* Register the protocol in the hashtable or protocols by
	 name.  */
      __objc_protocols_add_protocol (protocol->protocol_name, protocol);
      
      /* Init super protocols.  */
      __objc_init_protocols (protocol->protocol_list);
    }
  else if (protocol->class_pointer != proto_class)
    {
      _objc_abort ("Version %d doesn't match runtime protocol version %d\n",
		   (int) ((char *) protocol->class_pointer
			  - (char *) 0),
		   PROTOCOL_VERSION);
    }
}

static void
__objc_init_protocols (struct objc_protocol_list *protos)
{
  size_t i;
  static Class proto_class = 0;

  if (! protos)
    return;

  objc_mutex_lock (__objc_runtime_mutex);

  if (! proto_class)
    proto_class = objc_getClass ("Protocol");

  if (! proto_class)
    {
      unclaimed_proto_list = list_cons (protos, unclaimed_proto_list);
      objc_mutex_unlock (__objc_runtime_mutex);
      return;
    }

#if 0
  assert (protos->next == 0); /* Only single ones allowed.  */
#endif

  for (i = 0; i < protos->count; i++)
    {
      struct objc_protocol *aProto = protos->list[i];
      __objc_init_protocol (aProto);
    }

  objc_mutex_unlock (__objc_runtime_mutex);
}

static void
__objc_class_add_protocols (Class class, struct objc_protocol_list *protos)
{
  if (! protos)
    return;

  protos->next = class->protocols;
  class->protocols = protos;
}
