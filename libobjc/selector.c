/* GNU Objective C Runtime selector related functions
   Copyright (C) 1993, 1995, 1996, 1997, 2002, 2004 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include "objc/runtime.h"
#include "objc/sarray.h"
#include "objc/encoding.h"

/* Initial selector hash table size. Value doesn't matter much */
#define SELECTOR_HASH_SIZE 128

/* Tables mapping selector names to uid and opposite */
static struct sarray *__objc_selector_array = 0; /* uid -> sel  !T:MUTEX */
static struct sarray *__objc_selector_names = 0; /* uid -> name !T:MUTEX */
static cache_ptr      __objc_selector_hash  = 0; /* name -> uid !T:MUTEX */

/* Number of selectors stored in each of the above tables */
unsigned int __objc_selector_max_index = 0;     /* !T:MUTEX */

void __objc_init_selector_tables (void)
{
  __objc_selector_array = sarray_new (SELECTOR_HASH_SIZE, 0);
  __objc_selector_names = sarray_new (SELECTOR_HASH_SIZE, 0);
  __objc_selector_hash
    = objc_hash_new (SELECTOR_HASH_SIZE,
		     (hash_func_type) objc_hash_string,
		     (compare_func_type) objc_compare_strings);
}  

/* This routine is given a class and records all of the methods in its class
   structure in the record table.  */
void
__objc_register_selectors_from_class (Class class)
{
  MethodList_t method_list;

  method_list = class->methods;
  while (method_list)
    {
      __objc_register_selectors_from_list (method_list);
      method_list = method_list->method_next;
    }
}


/* This routine is given a list of methods and records each of the methods in
   the record table.  This is the routine that does the actual recording
   work.

   The name and type pointers in the method list must be permanent and
   immutable.
   */
void
__objc_register_selectors_from_list (MethodList_t method_list)
{
  int i = 0;

  objc_mutex_lock (__objc_runtime_mutex);
  while (i < method_list->method_count)
    {
      Method_t method = &method_list->method_list[i];
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


/* Register instance methods as class methods for root classes */
void __objc_register_instance_methods_to_class (Class class)
{
  MethodList_t method_list;
  MethodList_t class_method_list;
  int max_methods_no = 16;
  MethodList_t new_list;
  Method_t curr_method;

  /* Only if a root class. */
  if (class->super_class)
    return;

  /* Allocate a method list to hold the new class methods */
  new_list = objc_calloc (sizeof (struct objc_method_list)
			    + sizeof (struct objc_method[max_methods_no]), 1);
  method_list = class->methods;
  class_method_list = class->class_pointer->methods;
  curr_method = &new_list->method_list[0];

  /* Iterate through the method lists for the class */
  while (method_list)
    {
      int i;

      /* Iterate through the methods from this method list */
      for (i = 0; i < method_list->method_count; i++)
	{
	  Method_t mth = &method_list->method_list[i];
	  if (mth->method_name
	      && ! search_for_method_in_list (class_method_list,
					      mth->method_name))
	    {
	      /* This instance method isn't a class method. 
		  Add it into the new_list. */
	      *curr_method = *mth;
  
	      /* Reallocate the method list if necessary */
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

  /* If we created any new class methods
     then attach the method list to the class */
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


/* Returns YES iff t1 and t2 have same method types, but we ignore
   the argframe layout */
BOOL
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
	 all selectors, not just Protocol selectors. */
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

/* return selector representing name */
SEL
sel_get_typed_uid (const char *name, const char *types)
{
  struct objc_list *l;
  sidx i;

  objc_mutex_lock (__objc_runtime_mutex);

  i = (sidx) objc_hash_value_for_key (__objc_selector_hash, name);
  if (i == 0)
    {
      objc_mutex_unlock (__objc_runtime_mutex);
      return 0;
    }

  for (l = (struct objc_list *) sarray_get_safe (__objc_selector_array, i);
       l; l = l->tail)
    {
      SEL s = (SEL) l->head;
      if (types == 0 || s->sel_types == 0)
	{
	  if (s->sel_types == types)
	    {
	      objc_mutex_unlock (__objc_runtime_mutex);
	      return s;
	    }
	}
      else if (sel_types_match (s->sel_types, types))
	{
	  objc_mutex_unlock (__objc_runtime_mutex);
	  return s;
	}
    }

  objc_mutex_unlock (__objc_runtime_mutex);
  return 0;
}

/* Return selector representing name; prefer a selector with non-NULL type */
SEL
sel_get_any_typed_uid (const char *name)
{
  struct objc_list *l;
  sidx i;
  SEL s = NULL;

  objc_mutex_lock (__objc_runtime_mutex);

  i = (sidx) objc_hash_value_for_key (__objc_selector_hash, name);
  if (i == 0)
    {
      objc_mutex_unlock (__objc_runtime_mutex);
      return 0;
    }

  for (l = (struct objc_list *) sarray_get_safe (__objc_selector_array, i);
       l; l = l->tail)
    {
      s = (SEL) l->head;
      if (s->sel_types)
	{
	    objc_mutex_unlock (__objc_runtime_mutex);
	    return s;
	}
    }

  objc_mutex_unlock (__objc_runtime_mutex);
  return s;
}

/* return selector representing name */
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

/* return selector representing name */
SEL
sel_get_uid (const char *name)
{
  return sel_register_typed_name (name, 0);
}

/* Get name of selector.  If selector is unknown, the empty string "" 
   is returned */ 
const char *sel_get_name (SEL selector)
{
  const char *ret;

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


const char *sel_get_type (SEL selector)
{
  if (selector)
    return selector->sel_types;
  else
    return 0;
}

/* The uninstalled dispatch table */
extern struct sarray *__objc_uninstalled_dtable;

/* __sel_register_typed_name allocates lots of struct objc_selector:s
   of 8 (16, if pointers are 64 bits) bytes at startup. To reduce the number
   of malloc calls and memory lost to malloc overhead, we allocate
   objc_selector:s in blocks here. This is only called from
   __sel_register_typed_name, and __sel_register_typed_name may only be
   called when __objc_runtime_mutex is locked.

   Note that the objc_selector:s allocated from __sel_register_typed_name
   are never freed.

   62 because 62 * sizeof (struct objc_selector) = 496 (992). This should
   let malloc add some overhead and use a nice, round 512 (1024) byte chunk.
   */
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

/* Store the passed selector name in the selector record and return its
   selector value (value returned by sel_get_uid).
   Assumes that the calling function has locked down __objc_runtime_mutex. */
/* is_const parameter tells us if the name and types parameters
   are really constant or not.  If YES then they are constant and
   we can just store the pointers.  If NO then we need to copy
   name and types because the pointers may disappear later on. */
SEL
__sel_register_typed_name (const char *name, const char *types, 
			   struct objc_selector *orig, BOOL is_const)
{
  struct objc_selector *j;
  sidx i;
  struct objc_list *l;

  i = (sidx) objc_hash_value_for_key (__objc_selector_hash, name);
  if (soffset_decode (i) != 0)
    {
      for (l = (struct objc_list *) sarray_get_safe (__objc_selector_array, i);
	   l; l = l->tail)
	{
	  SEL s = (SEL) l->head;
	  if (types == 0 || s->sel_types == 0)
	    {
	      if (s->sel_types == types)
		{
		  if (orig)
		    {
		      orig->sel_id = (void *) i;
		      return orig;
		    }
		  else
		    return s;
		}
	    }
	  else if (! strcmp (s->sel_types, types))
	    {
	      if (orig)
		{
		  orig->sel_id = (void *) i;
		  return orig;
		}
	      else
		return s;
	    }
	}
      if (orig)
	j = orig;
      else
	j = pool_alloc_selector ();

      j->sel_id = (void *) i;
      /* Can we use the pointer or must copy types?  Don't copy if NULL */
      if ((is_const) || (types == 0))
	j->sel_types = (const char *) types;
      else {
	j->sel_types = (char *) objc_malloc (strlen (types) + 1);
	strcpy ((char *) j->sel_types, types);
      }
      l = (struct objc_list *) sarray_get_safe (__objc_selector_array, i);
    }
  else
    {
      __objc_selector_max_index += 1;
      i = soffset_encode (__objc_selector_max_index);
      if (orig)
	j = orig;
      else
	j = pool_alloc_selector ();
	
      j->sel_id = (void *) i;
      /* Can we use the pointer or must copy types?  Don't copy if NULL */
      if ((is_const) || (types == 0))
	j->sel_types = (const char *) types;
      else {
	j->sel_types = (char *) objc_malloc (strlen (types) + 1);
	strcpy ((char *) j->sel_types, types);
      }
      l = 0;
    }

  DEBUG_PRINTF ("Record selector %s[%s] as: %ld\n", name, types, 
		(long) soffset_decode (i));
  
  {
    int is_new = (l == 0);
    const char *new_name;

    /* Can we use the pointer or must copy name?  Don't copy if NULL */
    if ((is_const) || (name == 0))
      new_name = name;
    else {
      new_name = (char *) objc_malloc (strlen (name) + 1);
      strcpy ((char *) new_name, name);
    }

    l = list_cons ((void *) j, l);
    sarray_at_put_safe (__objc_selector_names, i, (void *) new_name);
    sarray_at_put_safe (__objc_selector_array, i, (void *) l);
    if (is_new)
      objc_hash_add (&__objc_selector_hash, (void *) new_name, (void *) i);
  }

  sarray_realloc (__objc_uninstalled_dtable, __objc_selector_max_index + 1);

  return (SEL) j;
}

SEL
sel_register_name (const char *name)
{
  SEL ret;
    
  objc_mutex_lock (__objc_runtime_mutex);
  /* Assume that name is not constant static memory and needs to be
     copied before put into a runtime structure.  is_const == NO */
  ret = __sel_register_typed_name (name, 0, 0, NO);
  objc_mutex_unlock (__objc_runtime_mutex);
  
  return ret;
}

SEL
sel_register_typed_name (const char *name, const char *type)
{
  SEL ret;

  objc_mutex_lock (__objc_runtime_mutex);
  /* Assume that name and type are not constant static memory and need to
     be copied before put into a runtime structure.  is_const == NO */
  ret = __sel_register_typed_name (name, type, 0, NO);
  objc_mutex_unlock (__objc_runtime_mutex);
  
  return ret;
}
