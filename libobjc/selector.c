/* GNU Objective C Runtime selector related functions
   Copyright (C) 1993-2014 Free Software Foundation, Inc.
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

#include "objc-private/common.h"
#include "objc/runtime.h"
#include "objc/thr.h"
#include "objc-private/hash.h"
#include "objc-private/objc-list.h"
#include "objc-private/module-abi-8.h"
#include "objc-private/runtime.h"
#include "objc-private/sarray.h"
#include "objc-private/selector.h"
#include <stdlib.h>                    /* For malloc.  */

/* Initial selector hash table size. Value doesn't matter much.  */
#define SELECTOR_HASH_SIZE 128

/* Tables mapping selector names to uid and opposite.  */
static struct sarray *__objc_selector_array = 0; /* uid -> sel  !T:MUTEX */
static struct sarray *__objc_selector_names = 0; /* uid -> name !T:MUTEX */
static cache_ptr      __objc_selector_hash  = 0; /* name -> uid !T:MUTEX */

/* Number of selectors stored in each of the above tables.  */
unsigned int __objc_selector_max_index = 0;     /* !T:MUTEX */

/* Forward-declare an internal function.  */
static SEL
__sel_register_typed_name (const char *name, const char *types,
			   struct objc_selector *orig, BOOL is_const);

void __objc_init_selector_tables (void)
{
  __objc_selector_array = sarray_new (SELECTOR_HASH_SIZE, 0);
  __objc_selector_names = sarray_new (SELECTOR_HASH_SIZE, 0);
  __objc_selector_hash
    = objc_hash_new (SELECTOR_HASH_SIZE,
		     (hash_func_type) objc_hash_string,
		     (compare_func_type) objc_compare_strings);
}  

/* Register a bunch of selectors from the table of selectors in a
   module.  'selectors' should not be NULL.  The list is terminated by
   a selectors with a NULL sel_id.  The selectors are assumed to
   contain the 'name' in the sel_id field; this is replaced with the
   final selector id after they are registered.  */
void
__objc_register_selectors_from_module (struct objc_selector *selectors)
{
  int i;

  for (i = 0; selectors[i].sel_id; ++i)
    {
      const char *name, *type;
      name = (char *) selectors[i].sel_id;
      type = (char *) selectors[i].sel_types;
      /* Constructors are constant static data and we can safely store
	 pointers to them in the runtime structures, so we set
	 is_const == YES.  */
      __sel_register_typed_name (name, type, (struct objc_selector *) &(selectors[i]),
				 /* is_const */ YES);
    }
}

/* This routine is given a class and records all of the methods in its
   class structure in the record table.  */
void
__objc_register_selectors_from_class (Class class)
{
  struct objc_method_list * method_list;

  method_list = class->methods;
  while (method_list)
    {
      __objc_register_selectors_from_list (method_list);
      method_list = method_list->method_next;
    }
}


/* This routine is given a list of methods and records each of the
   methods in the record table.  This is the routine that does the
   actual recording work.

   The name and type pointers in the method list must be permanent and
   immutable.  */
void
__objc_register_selectors_from_list (struct objc_method_list *method_list)
{
  int i = 0;

  objc_mutex_lock (__objc_runtime_mutex);
  while (i < method_list->method_count)
    {
      Method method = &method_list->method_list[i];
      if (method->method_name)
	{
	  method->method_name
	    = __sel_register_typed_name ((const char *) method->method_name,
					 method->method_types, 0, YES);
	}
      i += 1;
    }
  objc_mutex_unlock (__objc_runtime_mutex);
}

/* The same as __objc_register_selectors_from_list, but works on a
   struct objc_method_description_list* instead of a struct
   objc_method_list*.  This is only used for protocols, which have
   lists of method descriptions, not methods.  */
void
__objc_register_selectors_from_description_list 
(struct objc_method_description_list *method_list)
{
  int i = 0;
  
  objc_mutex_lock (__objc_runtime_mutex);
  while (i < method_list->count)
    {
      struct objc_method_description *method = &method_list->list[i];
      if (method->name)
	{
	  method->name
	    = __sel_register_typed_name ((const char *) method->name,
					 method->types, 0, YES);
	}
      i += 1;
    }
  objc_mutex_unlock (__objc_runtime_mutex);
}

/* Register instance methods as class methods for root classes.  */
void __objc_register_instance_methods_to_class (Class class)
{
  struct objc_method_list *method_list;
  struct objc_method_list *class_method_list;
  int max_methods_no = 16;
  struct objc_method_list *new_list;
  Method curr_method;

  /* Only if a root class. */
  if (class->super_class)
    return;

  /* Allocate a method list to hold the new class methods.  */
  new_list = objc_calloc (sizeof (struct objc_method_list)
			  + sizeof (struct objc_method[max_methods_no]), 1);
  method_list = class->methods;
  class_method_list = class->class_pointer->methods;
  curr_method = &new_list->method_list[0];
  
  /* Iterate through the method lists for the class.  */
  while (method_list)
    {
      int i;
      
      /* Iterate through the methods from this method list.  */
      for (i = 0; i < method_list->method_count; i++)
	{
	  Method mth = &method_list->method_list[i];
	  if (mth->method_name
	      && ! search_for_method_in_list (class_method_list,
					      mth->method_name))
	    {
	      /* This instance method isn't a class method.  Add it
		 into the new_list. */
	      *curr_method = *mth;
	      
	      /* Reallocate the method list if necessary.  */
	      if (++new_list->method_count == max_methods_no)
		new_list =
		  objc_realloc (new_list, sizeof (struct objc_method_list)
				+ sizeof (struct 
					  objc_method[max_methods_no += 16]));
	      curr_method = &new_list->method_list[new_list->method_count];
	    }
	}

      method_list = method_list->method_next;
    }

  /* If we created any new class methods then attach the method list
     to the class.  */
  if (new_list->method_count)
    {
      new_list =
 	objc_realloc (new_list, sizeof (struct objc_method_list)
		      + sizeof (struct objc_method[new_list->method_count]));
      new_list->method_next = class->class_pointer->methods;
      class->class_pointer->methods = new_list;
    }
  else
    objc_free(new_list);
  
  __objc_update_dispatch_table_for_class (class->class_pointer);
}

BOOL
sel_isEqual (SEL s1, SEL s2)
{
  if (s1 == 0 || s2 == 0)
    return s1 == s2;
  else
    return s1->sel_id == s2->sel_id;
}

/* Return YES iff t1 and t2 have same method types.  Ignore the
   argframe layout.  */
static BOOL
sel_types_match (const char *t1, const char *t2)
{
  if (! t1 || ! t2)
    return NO;
  while (*t1 && *t2)
    {
      if (*t1 == '+') t1++;
      if (*t2 == '+') t2++;
      while (isdigit ((unsigned char) *t1)) t1++;
      while (isdigit ((unsigned char) *t2)) t2++;
      /* xxx Remove these next two lines when qualifiers are put in
	 all selectors, not just Protocol selectors.  */
      t1 = objc_skip_type_qualifiers (t1);
      t2 = objc_skip_type_qualifiers (t2);
      if (! *t1 && ! *t2)
	return YES;
      if (*t1 != *t2)
	return NO;
      t1++;
      t2++;
    }
  return NO;
}

/* Return selector representing name.  */
SEL
sel_get_any_uid (const char *name)
{
  struct objc_list *l;
  sidx i;

  objc_mutex_lock (__objc_runtime_mutex);

  i = (sidx) objc_hash_value_for_key (__objc_selector_hash, name);
  if (soffset_decode (i) == 0)
    {
      objc_mutex_unlock (__objc_runtime_mutex);
      return 0;
    }

  l = (struct objc_list *) sarray_get_safe (__objc_selector_array, i);
  objc_mutex_unlock (__objc_runtime_mutex);

  if (l == 0)
    return 0;

  return (SEL) l->head;
}

SEL
sel_getTypedSelector (const char *name)
{
  sidx i;

  if (name == NULL)
    return NULL;
  
  objc_mutex_lock (__objc_runtime_mutex);
  
  /* Look for a typed selector.  */
  i = (sidx) objc_hash_value_for_key (__objc_selector_hash, name);
  if (i != 0)
    {
      struct objc_list *l;
      SEL returnValue = NULL;

      for (l = (struct objc_list *) sarray_get_safe (__objc_selector_array, i);
	   l; l = l->tail)
	{
	  SEL s = (SEL) l->head;
	  if (s->sel_types)
	    {
	      if (returnValue == NULL)
		{
		  /* First typed selector that we find.  Keep it in
		     returnValue, but keep checking as we want to
		     detect conflicts.  */
		  returnValue = s;
		}
	      else
		{
		  /* We had already found a typed selectors, so we
		     have multiple ones.  Double-check that they have
		     different types, just in case for some reason we
		     got duplicates with the same types.  If so, it's
		     OK, we'll ignore the duplicate.  */
		  if (returnValue->sel_types == s->sel_types)
		    continue;
		  else if (sel_types_match (returnValue->sel_types, s->sel_types))
		    continue;
		  else
		    {
		      /* The types of the two selectors are different;
			 it's a conflict.  Too bad.  Return NULL.  */
		      objc_mutex_unlock (__objc_runtime_mutex);
		      return NULL;
		    }
		}
	    }
	}

      if (returnValue != NULL)
	{
	  objc_mutex_unlock (__objc_runtime_mutex);
	  return returnValue;
	}
    }

  /* No typed selector found.  Return NULL.  */
  objc_mutex_unlock (__objc_runtime_mutex);
  return 0;
}

SEL *
sel_copyTypedSelectorList (const char *name, unsigned int *numberOfReturnedSelectors)
{
  unsigned int count = 0;
  SEL *returnValue = NULL;
  sidx i;
  
  if (name == NULL)
    {
      if (numberOfReturnedSelectors)
	*numberOfReturnedSelectors = 0;
      return NULL;
    }

  objc_mutex_lock (__objc_runtime_mutex);

  /* Count how many selectors we have.  */
  i = (sidx) objc_hash_value_for_key (__objc_selector_hash, name);
  if (i != 0)
    {
      struct objc_list *selector_list = NULL;
      selector_list = (struct objc_list *) sarray_get_safe (__objc_selector_array, i);

      /* Count how many selectors we have.  */
      {
	struct objc_list *l;
	for (l = selector_list; l; l = l->tail)
	  count++;
      }

      if (count != 0)
	{
	  /* Allocate enough memory to hold them.  */
	  returnValue = (SEL *)(malloc (sizeof (SEL) * (count + 1)));
	  
	  /* Copy the selectors.  */
	  {
	    unsigned int j;
	    for (j = 0; j < count; j++)
	      {
		returnValue[j] = (SEL)(selector_list->head);
		selector_list = selector_list->tail;
	      }
	    returnValue[j] = NULL;
	  }
	}
    }      

  objc_mutex_unlock (__objc_runtime_mutex);
  
  if (numberOfReturnedSelectors)
    *numberOfReturnedSelectors = count;
  
  return returnValue;
}

/* Get the name of a selector.  If the selector is unknown, the empty
   string "" is returned.  */ 
const char *sel_getName (SEL selector)
{
  const char *ret;

  if (selector == NULL)
    return "<null selector>";
  
  objc_mutex_lock (__objc_runtime_mutex);
  if ((soffset_decode ((sidx)selector->sel_id) > 0)
      && (soffset_decode ((sidx)selector->sel_id) <= __objc_selector_max_index))
    ret = sarray_get_safe (__objc_selector_names, (sidx) selector->sel_id);
  else
    ret = 0;
  objc_mutex_unlock (__objc_runtime_mutex);
  return ret;
}

BOOL
sel_is_mapped (SEL selector)
{
  unsigned int idx = soffset_decode ((sidx)selector->sel_id);
  return ((idx > 0) && (idx <= __objc_selector_max_index));
}

const char *sel_getTypeEncoding (SEL selector)
{
  if (selector)
    return selector->sel_types;
  else
    return 0;
}

/* The uninstalled dispatch table.  */
extern struct sarray *__objc_uninstalled_dtable;

/* __sel_register_typed_name allocates lots of struct objc_selector:s
   of 8 (16, if pointers are 64 bits) bytes at startup. To reduce the
   number of malloc calls and memory lost to malloc overhead, we
   allocate objc_selector:s in blocks here. This is only called from
   __sel_register_typed_name, and __sel_register_typed_name may only
   be called when __objc_runtime_mutex is locked.

   Note that the objc_selector:s allocated from
   __sel_register_typed_name are never freed.

   62 because 62 * sizeof (struct objc_selector) = 496 (992). This
   should let malloc add some overhead and use a nice, round 512
   (1024) byte chunk.  */
#define SELECTOR_POOL_SIZE 62
static struct objc_selector *selector_pool;
static int selector_pool_left;

static struct objc_selector *
pool_alloc_selector(void)
{
  if (!selector_pool_left)
    {
      selector_pool = objc_malloc (sizeof (struct objc_selector)
				   * SELECTOR_POOL_SIZE);
      selector_pool_left = SELECTOR_POOL_SIZE;
    }
  return &selector_pool[--selector_pool_left];
}

/* Store the passed selector name in the selector record and return
   its selector value (value returned by sel_get_uid).  Assume that
   the calling function has locked down __objc_runtime_mutex.  The
   'is_const' parameter tells us if the name and types parameters are
   really constant or not.  If YES then they are constant and we can
   just store the pointers.  If NO then we need to copy name and types
   because the pointers may disappear later on.  If the 'orig'
   parameter is not NULL, then we are registering a selector from a
   module, and 'orig' is that selector.  In this case, we can put the
   selector in the tables if needed, and orig->sel_id is updated with
   the selector ID of the registered selector, and 'orig' is
   returned.  */
static SEL
__sel_register_typed_name (const char *name, const char *types, 
			   struct objc_selector *orig, BOOL is_const)
{
  struct objc_selector *j;
  sidx i;
  struct objc_list *l;

  i = (sidx) objc_hash_value_for_key (__objc_selector_hash, name);
  if (soffset_decode (i) != 0)
    {
      /* There are already selectors with that name.  Examine them to
	 see if the one we're registering already exists.  */
      for (l = (struct objc_list *)sarray_get_safe (__objc_selector_array, i);
	   l; l = l->tail)
	{
	  SEL s = (SEL)l->head;
	  if (types == 0 || s->sel_types == 0)
	    {
	      if (s->sel_types == types)
		{
		  if (orig)
		    {
		      orig->sel_id = (void *)i;
		      return orig;
		    }
		  else
		    return s;
		}
	    }
	  else if (sel_types_match (s->sel_types, types))
	    {
	      if (orig)
		{
		  orig->sel_id = (void *)i;
		  return orig;
		}
	      else
		return s;
	    }
	}
      /* A selector with this specific name/type combination does not
	 exist yet.  We need to register it.  */
      if (orig)
	j = orig;
      else
	j = pool_alloc_selector ();
      
      j->sel_id = (void *)i;
      /* Can we use the pointer or must we copy types ?  Don't copy if
	 NULL.  */
      if ((is_const) || (types == 0))
	j->sel_types = types;
      else
	{
	  j->sel_types = (char *)objc_malloc (strlen (types) + 1);
	  strcpy ((char *)j->sel_types, types);
	}
      l = (struct objc_list *)sarray_get_safe (__objc_selector_array, i);
    }
  else
    {
      /* There are no other selectors with this name registered in the
	 runtime tables.  */
      const char *new_name;

      /* Determine i.  */
      __objc_selector_max_index += 1;
      i = soffset_encode (__objc_selector_max_index);

      /* Prepare the selector.  */
      if (orig)
	j = orig;
      else
	j = pool_alloc_selector ();
      
      j->sel_id = (void *)i;
      /* Can we use the pointer or must we copy types ?  Don't copy if
	 NULL.  */
      if (is_const || (types == 0))
	j->sel_types = types;
      else
	{
	  j->sel_types = (char *)objc_malloc (strlen (types) + 1);
	  strcpy ((char *)j->sel_types, types);
	}

      /* Since this is the first selector with this name, we need to
	 register the correspondence between 'i' (the sel_id) and
	 'name' (the actual string) in __objc_selector_names and
	 __objc_selector_hash.  */
      
      /* Can we use the pointer or must we copy name ?  Don't copy if
	 NULL.  (FIXME: Can the name really be NULL here ?)  */
      if (is_const || (name == 0))
	new_name = name;
      else
	{
	  new_name = (char *)objc_malloc (strlen (name) + 1);
	  strcpy ((char *)new_name, name);
	}
      
      /* This maps the sel_id to the name.  */
      sarray_at_put_safe (__objc_selector_names, i, (void *)new_name);

      /* This maps the name to the sel_id.  */
      objc_hash_add (&__objc_selector_hash, (void *)new_name, (void *)i);

      l = 0;
    }

  DEBUG_PRINTF ("Record selector %s[%s] as: %ld\n", name, types, 
		(long)soffset_decode (i));

  /* Now add the selector to the list of selectors with that id.  */
  l = list_cons ((void *)j, l);
  sarray_at_put_safe (__objc_selector_array, i, (void *)l);

  sarray_realloc (__objc_uninstalled_dtable, __objc_selector_max_index + 1);
  
  return (SEL)j;
}

SEL
sel_registerName (const char *name)
{
  SEL ret;

  if (name == NULL)
    return NULL;
    
  objc_mutex_lock (__objc_runtime_mutex);
  /* Assume that name is not constant static memory and needs to be
     copied before put into a runtime structure.  is_const == NO.  */
  ret = __sel_register_typed_name (name, 0, 0, NO);
  objc_mutex_unlock (__objc_runtime_mutex);
  
  return ret;
}

SEL
sel_registerTypedName (const char *name, const char *type)
{
  SEL ret;

  if (name == NULL)
    return NULL;

  objc_mutex_lock (__objc_runtime_mutex);
  /* Assume that name and type are not constant static memory and need
     to be copied before put into a runtime structure.  is_const ==
     NO.  */
  ret = __sel_register_typed_name (name, type, 0, NO);
  objc_mutex_unlock (__objc_runtime_mutex);
  
  return ret;
}

/* Return the selector representing name.  */
SEL
sel_getUid (const char *name)
{
  return sel_registerTypedName (name, 0);
}
