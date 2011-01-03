/* Basic data types for Objective C.
   Copyright (C) 1998, 2002, 2004, 2005, 2006, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Ovidiu Predescu.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "objc-private/common.h"
#include "objc/objc.h"

#if OBJC_WITH_GC

#include "tconfig.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "objc/encoding.h"

#include <gc.h>
#include <limits.h>

/* gc_typed.h uses the following but doesn't declare them */
typedef GC_word word;
typedef GC_signed_word signed_word;
#define BITS_PER_WORD (CHAR_BIT * sizeof (word))

#include <gc_typed.h>

/* The following functions set up in `mask` the corresponding pointers.
   The offset is incremented with the size of the type.  */

#define ROUND(V, A) \
  ({ typeof (V) __v = (V); typeof (A) __a = (A); \
     __a * ((__v+__a - 1)/__a); })

#define SET_BIT_FOR_OFFSET(mask, offset) \
  GC_set_bit (mask, offset / sizeof (void *))

/* Some prototypes */
static void
__objc_gc_setup_struct (GC_bitmap mask, const char *type, int offset);
static void
__objc_gc_setup_union (GC_bitmap mask, const char *type, int offset);


static void
__objc_gc_setup_array (GC_bitmap mask, const char *type, int offset)
{
  int i, len = atoi (type + 1);

  while (isdigit (*++type))
    /* do nothing */;		/* skip the size of the array */

  switch (*type) {
  case _C_ARY_B:
    for (i = 0; i < len; i++)
      __objc_gc_setup_array (mask, type, offset);
    break;

  case _C_STRUCT_B:
    for (i = 0; i < len; i++)
      __objc_gc_setup_struct (mask, type, offset);
    break;

  case _C_UNION_B:
    for (i = 0; i < len; i++)
      __objc_gc_setup_union (mask, type, offset);
    break;

  default:
    break;
  }
}

static void
__objc_gc_setup_struct (GC_bitmap mask, const char *type, int offset)
{
  struct objc_struct_layout layout;
  unsigned int position;
  const char *mtype;

  objc_layout_structure (type, &layout);

  while (objc_layout_structure_next_member (&layout))
    {
      BOOL gc_invisible = NO;

      objc_layout_structure_get_info (&layout, &position, NULL, &mtype);

      /* Skip the variable name */
      if (*mtype == '"')
	{
	  for (mtype++; *mtype++ != '"';)
	    /* do nothing */;
	}

      if (*mtype == _C_GCINVISIBLE)
	{
	  gc_invisible = YES;
	  mtype++;
	}

      /* Add to position the offset of this structure */
      position += offset;

      switch (*mtype) {
      case _C_ID:
      case _C_CLASS:
      case _C_SEL:
      case _C_PTR:
      case _C_CHARPTR:
      case _C_ATOM:
	if (! gc_invisible)
	  SET_BIT_FOR_OFFSET (mask, position);
	break;

      case _C_ARY_B:
	__objc_gc_setup_array (mask, mtype, position);
	break;

      case _C_STRUCT_B:
	__objc_gc_setup_struct (mask, mtype, position);
	break;

      case _C_UNION_B:
	__objc_gc_setup_union (mask, mtype, position);
	break;

      default:
        break;
      }
    }
}

static void
__objc_gc_setup_union (GC_bitmap mask, const char *type, int offset)
{
  /* Sub-optimal, quick implementation: assume the union is made of
     pointers, set up the mask accordingly. */

  int i, size, align;

  /* Skip the variable name */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  size = objc_sizeof_type (type);
  align = objc_alignof_type (type);

  offset = ROUND (offset, align);
  for (i = 0; i < size; i += sizeof (void *))
    {
      SET_BIT_FOR_OFFSET (mask, offset);
      offset += sizeof (void *);
    }
}


/* Iterates over the types in the structure that represents the class
   encoding and sets the bits in mask according to each ivar type.  */
static void
__objc_gc_type_description_from_type (GC_bitmap mask, const char *type)
{
  struct objc_struct_layout layout;
  unsigned int offset, align;
  const char *ivar_type;

  objc_layout_structure (type, &layout);

  while (objc_layout_structure_next_member (&layout))
    {
      BOOL gc_invisible = NO;

      objc_layout_structure_get_info (&layout, &offset, &align, &ivar_type);

      /* Skip the variable name */
      if (*ivar_type == '"')
	{
	  for (ivar_type++; *ivar_type++ != '"';)
	    /* do nothing */;
	}

      if (*ivar_type == _C_GCINVISIBLE)
	{
	  gc_invisible = YES;
	  ivar_type++;
	}

      switch (*ivar_type) {
      case _C_ID:
      case _C_CLASS:
      case _C_SEL:
      case _C_PTR:
      case _C_CHARPTR:
        if (! gc_invisible)
          SET_BIT_FOR_OFFSET (mask, offset);
	break;

      case _C_ARY_B:
	__objc_gc_setup_array (mask, ivar_type, offset);
	break;

      case _C_STRUCT_B:
	__objc_gc_setup_struct (mask, ivar_type, offset);
	break;

      case _C_UNION_B:
	__objc_gc_setup_union (mask, ivar_type, offset);
	break;

      default:
        break;
      }
    }
}

/* Computes in *type the full type encoding of this class including
   its super classes. '*size' gives the total number of bytes allocated
   into *type, '*current' the number of bytes used so far by the
   encoding. */
static void
__objc_class_structure_encoding (Class class, char **type, int *size,
                                 int *current)
{
  int i, ivar_count;
  struct objc_ivar_list *ivars;

  if (! class)
    {
      strcat (*type, "{");
      (*current)++;
      return;
    }

  /* Add the type encodings of the super classes */
  __objc_class_structure_encoding (class->super_class, type, size, current);

  ivars = class->ivars;
  if (! ivars)
    return;

  ivar_count = ivars->ivar_count;

  for (i = 0; i < ivar_count; i++)
    {
      struct objc_ivar *ivar = &(ivars->ivar_list[i]);
      const char *ivar_type = ivar->ivar_type;
      int len = strlen (ivar_type);

      if (*current + len + 1 >= *size)
        {
          /* Increase the size of the encoding string so that it
             contains this ivar's type. */
          *size = ROUND (*current + len + 1, 10);
          *type = objc_realloc (*type, *size);
        }
      strcat (*type + *current, ivar_type);
      *current += len;
    }
}


/* Allocates the memory that will hold the type description for class
   and calls the __objc_class_structure_encoding that generates this
   value. */
void
__objc_generate_gc_type_description (Class class)
{
  GC_bitmap mask;
  int bits_no, size;
  int type_size = 10, current;
  char *class_structure_type;

  if (! CLS_ISCLASS (class))
    return;

  /* We have to create a mask in which each bit counts for a pointer member.
     We take into consideration all the non-pointer instance variables and we
     round them up to the alignment. */

  /* The number of bits in the mask is the size of an instance in bytes divided
     by the size of a pointer. */
  bits_no = (ROUND (class_get_instance_size (class), sizeof (void *))
             / sizeof (void *));
  size = ROUND (bits_no, BITS_PER_WORD) / BITS_PER_WORD;
  mask = objc_atomic_malloc (size * sizeof (int));
  memset (mask, 0, size * sizeof (int));

  class_structure_type = objc_atomic_malloc (type_size);
  *class_structure_type = current = 0;
  __objc_class_structure_encoding (class, &class_structure_type,
                                   &type_size, &current);
  if (current + 1 == type_size)
    class_structure_type = objc_realloc (class_structure_type, ++type_size);
  strcat (class_structure_type + current, "}");
#ifdef DEBUG
  printf ("type description for '%s' is %s\n", class->name, class_structure_type);
#endif
  
  __objc_gc_type_description_from_type (mask, class_structure_type);
  objc_free (class_structure_type);

#ifdef DEBUG
  printf ("  mask for '%s', type '%s' (bits %d, mask size %d) is:",
	  class_structure_type, class->name, bits_no, size);
  {
    int i;
    for (i = 0; i < size; i++)
      printf (" %lx", mask[i]);
  }
  puts ("");
#endif

  class->gc_object_type = (void *) GC_make_descriptor (mask, bits_no);
}


/* Returns YES if type denotes a pointer type, NO otherwise */
static inline BOOL
__objc_ivar_pointer (const char *type)
{
  type = objc_skip_type_qualifiers (type);

  return (*type == _C_ID
          || *type == _C_CLASS
          || *type == _C_SEL
          || *type == _C_PTR
          || *type == _C_CHARPTR
          || *type == _C_ATOM);
}


/* Mark the instance variable whose name is given by ivarname as a
   weak pointer (a pointer hidden to the garbage collector) if
   gc_invisible is true. If gc_invisible is false it unmarks the
   instance variable and makes it a normal pointer, visible to the
   garbage collector.

   This operation only makes sense on instance variables that are
   pointers.  */
void
class_ivar_set_gcinvisible (Class class, const char *ivarname,
                            BOOL gc_invisible)
{
  int i, ivar_count;
  struct objc_ivar_list *ivars;

  if (! class || ! ivarname)
    return;

  ivars = class->ivars;
  if (! ivars)
    return;

  ivar_count = ivars->ivar_count;

  for (i = 0; i < ivar_count; i++)
    {
      struct objc_ivar *ivar = &(ivars->ivar_list[i]);
      const char *type;

      if (! ivar->ivar_name || strcmp (ivar->ivar_name, ivarname))
	continue;

      assert (ivar->ivar_type);
      type = ivar->ivar_type;

      /* Skip the variable name */
      if (*type == '"')
	{
	  for (type++; *type++ != '"';)
	    /* do nothing */;
	}

      if (*type == _C_GCINVISIBLE)
	{
	  char *new_type;
	  size_t len;

	  if (gc_invisible || ! __objc_ivar_pointer (type))
	    return;	/* The type of the variable already matches the
			   requested gc_invisible type */

	  /* The variable is gc_invisible so we make it gc visible.  */
	  new_type = objc_atomic_malloc (strlen(ivar->ivar_type));
	  len = (type - ivar->ivar_type);
	  memcpy (new_type, ivar->ivar_type, len);
	  new_type[len] = 0;
	  strcat (new_type, type + 1);
	  ivar->ivar_type = new_type;
	}
      else
	{
	  char *new_type;
	  size_t len;

	  if (! gc_invisible || ! __objc_ivar_pointer (type))
	    return;	/* The type of the variable already matches the
			   requested gc_invisible type */

	  /* The variable is gc visible so we make it gc_invisible.  */
	  new_type = objc_malloc (strlen(ivar->ivar_type) + 2);
	  len = (type - ivar->ivar_type);
	  memcpy (new_type, ivar->ivar_type, len);
	  new_type[len] = 0;
	  strcat (new_type, "!");
	  strcat (new_type, type);
	  ivar->ivar_type = new_type;
	}

      __objc_generate_gc_type_description (class);
      return;
    }

  /* Search the instance variable in the superclasses */
  class_ivar_set_gcinvisible (class->super_class, ivarname, gc_invisible);
}

#else /* !OBJC_WITH_GC */

void
__objc_generate_gc_type_description (Class class __attribute__ ((__unused__)))
{
}

void class_ivar_set_gcinvisible (Class class __attribute__ ((__unused__)),
				 const char *ivarname __attribute__ ((__unused__)),
				 BOOL gc_invisible __attribute__ ((__unused__)))
{
}

#endif /* OBJC_WITH_GC */
