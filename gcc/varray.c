/* Virtual array support.
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */

#include "config.h"
#include "errors.h"
#include "system.h"
#include "varray.h"
#include "ggc.h"

#define VARRAY_HDR_SIZE (sizeof (struct varray_head_tag) - sizeof (varray_data))

static const size_t element_size[NUM_VARRAY_DATA] = {
  sizeof (char),
  sizeof (unsigned char),
  sizeof (short),
  sizeof (unsigned short),
  sizeof (int),
  sizeof (unsigned int),
  sizeof (long),
  sizeof (unsigned long),
  sizeof (HOST_WIDE_INT),
  sizeof (unsigned HOST_WIDE_INT),
  sizeof (PTR),
  sizeof (char *),
  sizeof (struct rtx_def *),
  sizeof (struct rtvec_def *),
  sizeof (union tree_node *),
  sizeof (struct bitmap_head_def *),
  sizeof (struct reg_info_def *),
  sizeof (struct const_equiv_data),
  sizeof (struct basic_block_def *),
  sizeof (struct elt_list *)
};

static const int uses_ggc[NUM_VARRAY_DATA] = {
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* unsigned HOST_WIDE_INT */
  1, /* PTR */
  1, 1, 1, 1, 1, /* bitmap_head_def */
  0, 0, 0, 1
};

/* Allocate a virtual array with NUM_ELEMENT elements, each of which is
   ELEMENT_SIZE bytes long, named NAME.  Array elements are zeroed.  */
varray_type
varray_init (num_elements, element_kind, name)
     size_t num_elements;
     enum varray_data_enum element_kind;
     const char *name;
{
  size_t data_size = num_elements * element_size[element_kind];
  varray_type ptr;
  if (uses_ggc [element_kind])
    ptr = (varray_type) ggc_alloc_cleared (VARRAY_HDR_SIZE + data_size);
  else
    ptr = (varray_type) xcalloc (VARRAY_HDR_SIZE + data_size, 1);

  ptr->num_elements = num_elements;
  ptr->elements_used = 0;
  ptr->type = element_kind;
  ptr->name = name;
  return ptr;
}

/* Grow/shrink the virtual array VA to N elements.  Zero any new elements
   allocated.  */
varray_type
varray_grow (va, n)
     varray_type va;
     size_t n;
{
  size_t old_elements = va->num_elements;

  if (n != old_elements)
    {
      size_t elem_size = element_size[va->type];
      size_t old_data_size = old_elements * elem_size;
      size_t data_size = n * elem_size;

      if (uses_ggc[va->type])
	va = (varray_type) ggc_realloc (va, VARRAY_HDR_SIZE + data_size);
      else
	va = (varray_type) xrealloc ((char *) va, VARRAY_HDR_SIZE + data_size);
      va->num_elements = n;
      if (n > old_elements)
	memset (&va->data.c[old_data_size], 0, data_size - old_data_size);
    }

  return va;
}

/* Reset a varray to its original state.  */
void
varray_clear (va)
     varray_type va;
{
  size_t data_size = element_size[va->type] * va->num_elements;

  memset (va->data.c, 0, data_size);
  va->elements_used = 0;
}

/* Check the bounds of a varray access.  */

#if defined ENABLE_CHECKING && (GCC_VERSION >= 2007)

extern void error PARAMS ((const char *, ...))	ATTRIBUTE_PRINTF_1;

void
varray_check_failed (va, n, file, line, function)
     varray_type va;
     size_t n;
     const char *file;
     int line;
     const char *function;
{
  internal_error ("virtual array %s[%lu]: element %lu out of bounds in %s, at %s:%d",
		  va->name, (unsigned long) va->num_elements, (unsigned long) n,
		  function, trim_filename (file), line);
}

#endif
