/* Virtual array support.
   Copyright (C) 1998 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "bitmap.h"
#include "varray.h"

#define VARRAY_HDR_SIZE (sizeof (struct varray_head_tag) - sizeof (varray_data))

/* Allocate a virtual array with NUM_ELEMENT elements, each of which is
   ELEMENT_SIZE bytes long, named NAME.  Array elements are zeroed.  */
varray_type
varray_init (num_elements, element_size, name)
     size_t num_elements;
     size_t element_size;
     const char *name;
{
  size_t data_size = num_elements * element_size;
  varray_type ptr = (varray_type) xcalloc (VARRAY_HDR_SIZE + data_size, 1);

  ptr->num_elements = num_elements;
  ptr->element_size = element_size;
  ptr->name	    = name;
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
      size_t element_size = va->element_size;
      size_t old_data_size = old_elements * element_size;
      size_t data_size = n * element_size;

      va = (varray_type) xrealloc ((char *)va, VARRAY_HDR_SIZE + data_size);
      va->num_elements = n;
      if (n > old_elements)
	bzero (&va->data.c[old_data_size], data_size - old_data_size);
    }

  return va;
}
