/* A representation of vector permutation indices.
   Copyright (C) 2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "vec-perm-indices.h"
#include "tree.h"
#include "backend.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"

/* Switch to a new permutation vector that selects the same input elements
   as ORIG, but with each element split into FACTOR pieces.  For example,
   if ORIG is { 1, 2, 0, 3 } and FACTOR is 2, the new permutation is
   { 2, 3, 4, 5, 0, 1, 6, 7 }.  */

void
vec_perm_indices::new_expanded_vector (const vec_perm_indices &orig,
				       unsigned int factor)
{
  truncate (0);
  reserve (orig.length () * factor);
  for (unsigned int i = 0; i < orig.length (); ++i)
    {
      element_type base = orig[i] * factor;
      for (unsigned int j = 0; j < factor; ++j)
	quick_push (base + j);
    }
}

/* Return true if all elements of the permutation vector are in the range
   [START, START + SIZE).  */

bool
vec_perm_indices::all_in_range_p (element_type start, element_type size) const
{
  for (unsigned int i = 0; i < length (); ++i)
    if ((*this)[i] < start || ((*this)[i] - start) >= size)
      return false;
  return true;
}

/* Try to read the contents of VECTOR_CST CST as a constant permutation
   vector.  Return true and add the elements to BUILDER on success,
   otherwise return false without modifying BUILDER.  */

bool
tree_to_vec_perm_builder (vec_perm_builder *builder, tree cst)
{
  unsigned int nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (cst));
  for (unsigned int i = 0; i < nelts; ++i)
    if (!tree_fits_shwi_p (vector_cst_elt (cst, i)))
      return false;

  builder->reserve (nelts);
  for (unsigned int i = 0; i < nelts; ++i)
    builder->quick_push (tree_to_shwi (vector_cst_elt (cst, i))
			 & (2 * nelts - 1));
  return true;
}

/* Return a CONST_VECTOR of mode MODE that contains the elements of
   INDICES.  */

rtx
vec_perm_indices_to_rtx (machine_mode mode, const vec_perm_indices &indices)
{
  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	      && GET_MODE_NUNITS (mode) == indices.length ());
  unsigned int nelts = indices.length ();
  rtvec v = rtvec_alloc (nelts);
  for (unsigned int i = 0; i < nelts; ++i)
    RTVEC_ELT (v, i) = gen_int_mode (indices[i], GET_MODE_INNER (mode));
  return gen_rtx_CONST_VECTOR (mode, v);
}
