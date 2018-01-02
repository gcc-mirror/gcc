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
#include "fold-const.h"
#include "tree-vector-builder.h"
#include "backend.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"

/* Switch to a new permutation vector that selects between NINPUTS vector
   inputs that have NELTS_PER_INPUT elements each.  Take the elements of the
   new permutation vector from ELEMENTS, clamping each one to be in range.  */

void
vec_perm_indices::new_vector (const vec_perm_builder &elements,
			      unsigned int ninputs,
			      unsigned int nelts_per_input)
{
  m_ninputs = ninputs;
  m_nelts_per_input = nelts_per_input;
  /* Expand the encoding and clamp each element.  E.g. { 0, 2, 4, ... }
     might wrap halfway if there is only one vector input.  */
  unsigned int full_nelts = elements.full_nelts ();
  m_encoding.new_vector (full_nelts, full_nelts, 1);
  for (unsigned int i = 0; i < full_nelts; ++i)
    m_encoding.quick_push (clamp (elements.elt (i)));
  m_encoding.finalize ();
}

/* Switch to a new permutation vector that selects the same input elements
   as ORIG, but with each element split into FACTOR pieces.  For example,
   if ORIG is { 1, 2, 0, 3 } and FACTOR is 2, the new permutation is
   { 2, 3, 4, 5, 0, 1, 6, 7 }.  */

void
vec_perm_indices::new_expanded_vector (const vec_perm_indices &orig,
				       unsigned int factor)
{
  m_ninputs = orig.m_ninputs;
  m_nelts_per_input = orig.m_nelts_per_input * factor;
  m_encoding.new_vector (orig.m_encoding.full_nelts () * factor,
			 orig.m_encoding.npatterns () * factor,
			 orig.m_encoding.nelts_per_pattern ());
  unsigned int encoded_nelts = orig.m_encoding.encoded_nelts ();
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    {
      element_type base = orig.m_encoding[i] * factor;
      for (unsigned int j = 0; j < factor; ++j)
	m_encoding.quick_push (base + j);
    }
  m_encoding.finalize ();
}

/* Rotate the inputs of the permutation right by DELTA inputs.  This changes
   the values of the permutation vector but it doesn't change the way that
   the elements are encoded.  */

void
vec_perm_indices::rotate_inputs (int delta)
{
  element_type element_delta = delta * m_nelts_per_input;
  for (unsigned int i = 0; i < m_encoding.length (); ++i)
    m_encoding[i] = clamp (m_encoding[i] + element_delta);
}

/* Return true if all elements of the permutation vector are in the range
   [START, START + SIZE).  */

bool
vec_perm_indices::all_in_range_p (element_type start, element_type size) const
{
  /* Check the first two elements of each pattern.  */
  unsigned int npatterns = m_encoding.npatterns ();
  unsigned int nelts_per_pattern = m_encoding.nelts_per_pattern ();
  unsigned int base_nelts = npatterns * MIN (nelts_per_pattern, 2);
  for (unsigned int i = 0; i < base_nelts; ++i)
    if (m_encoding[i] < start || (m_encoding[i] - start) >= size)
      return false;

  /* For stepped encodings, check the full range of the series.  */
  if (nelts_per_pattern == 3)
    {
      element_type limit = input_nelts ();

      /* The number of elements in each pattern beyond the first two
	 that we checked above.  */
      unsigned int step_nelts = (m_encoding.full_nelts () / npatterns) - 2;
      for (unsigned int i = 0; i < npatterns; ++i)
	{
	  /* BASE1 has been checked but BASE2 hasn't.   */
	  element_type base1 = m_encoding[i + npatterns];
	  element_type base2 = m_encoding[i + base_nelts];

	  /* The step to add to get from BASE1 to each subsequent value.  */
	  element_type step = clamp (base2 - base1);

	  /* STEP has no inherent sign, so a value near LIMIT can
	     act as a negative step.  The series is in range if it
	     is in range according to one of the two interpretations.

	     Since we're dealing with clamped values, ELEMENT_TYPE is
	     wide enough for overflow not to be a problem.  */
	  element_type headroom_down = base1 - start;
	  element_type headroom_up = size - headroom_down - 1;
	  if (headroom_up < step * step_nelts
	      && headroom_down < (limit - step) * step_nelts)
	    return false;
	}
    }
  return true;
}

/* Try to read the contents of VECTOR_CST CST as a constant permutation
   vector.  Return true and add the elements to BUILDER on success,
   otherwise return false without modifying BUILDER.  */

bool
tree_to_vec_perm_builder (vec_perm_builder *builder, tree cst)
{
  unsigned int encoded_nelts = vector_cst_encoded_nelts (cst);
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    if (!tree_fits_shwi_p (VECTOR_CST_ENCODED_ELT (cst, i)))
      return false;

  builder->new_vector (TYPE_VECTOR_SUBPARTS (TREE_TYPE (cst)),
		       VECTOR_CST_NPATTERNS (cst),
		       VECTOR_CST_NELTS_PER_PATTERN (cst));
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    builder->quick_push (tree_to_shwi (VECTOR_CST_ENCODED_ELT (cst, i)));
  return true;
}

/* Return a VECTOR_CST of type TYPE for the permutation vector in INDICES.  */

tree
vec_perm_indices_to_tree (tree type, const vec_perm_indices &indices)
{
  gcc_assert (TYPE_VECTOR_SUBPARTS (type) == indices.length ());
  tree_vector_builder sel (type, indices.encoding ().npatterns (),
			   indices.encoding ().nelts_per_pattern ());
  unsigned int encoded_nelts = sel.encoded_nelts ();
  for (unsigned int i = 0; i < encoded_nelts; i++)
    sel.quick_push (build_int_cst (TREE_TYPE (type), indices[i]));
  return sel.build ();
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
