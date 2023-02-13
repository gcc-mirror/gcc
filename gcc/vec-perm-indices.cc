/* A representation of vector permutation indices.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.

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
#include "selftest.h"
#include "rtx-vector-builder.h"

/* Switch to a new permutation vector that selects between NINPUTS vector
   inputs that have NELTS_PER_INPUT elements each.  Take the elements of the
   new permutation vector from ELEMENTS, clamping each one to be in range.  */

void
vec_perm_indices::new_vector (const vec_perm_builder &elements,
			      unsigned int ninputs,
			      poly_uint64 nelts_per_input)
{
  m_ninputs = ninputs;
  m_nelts_per_input = nelts_per_input;
  /* If the vector has a constant number of elements, expand the
     encoding and clamp each element.  E.g. { 0, 2, 4, ... } might
     wrap halfway if there is only one vector input, and we want
     the wrapped form to be the canonical one.

     If the vector has a variable number of elements, just copy
     the encoding.  In that case the unwrapped form is canonical
     and there is no way of representing the wrapped form.  */
  poly_uint64 full_nelts = elements.full_nelts ();
  unsigned HOST_WIDE_INT copy_nelts;
  if (full_nelts.is_constant (&copy_nelts))
    m_encoding.new_vector (full_nelts, copy_nelts, 1);
  else
    {
      copy_nelts = elements.encoded_nelts ();
      m_encoding.new_vector (full_nelts, elements.npatterns (),
			     elements.nelts_per_pattern ());
    }
  unsigned int npatterns = m_encoding.npatterns ();
  for (unsigned int i = 0; i < npatterns; ++i)
    m_encoding.quick_push (clamp (elements.elt (i)));
  /* Use the fact that:

	(a + b) % c == ((a % c) + (b % c)) % c

     to simplify the clamping of variable-length vectors.  */
  for (unsigned int i = npatterns; i < copy_nelts; ++i)
    {
      element_type step = clamp (elements.elt (i)
				 - elements.elt (i - npatterns));
      m_encoding.quick_push (clamp (m_encoding[i - npatterns] + step));
    }
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

/* Check whether we can switch to a new permutation vector that
   selects the same input elements as ORIG, but with each element
   built up from FACTOR pieces.  Return true if yes, otherwise
   return false.  Every FACTOR permutation indexes should be
   continuous separately and the first one of each batch should
   be able to exactly modulo FACTOR.  For example, if ORIG is
   { 2, 3, 4, 5, 0, 1, 6, 7 } and FACTOR is 2, the new permutation
   is { 1, 2, 0, 3 }.  */

bool
vec_perm_indices::new_shrunk_vector (const vec_perm_indices &orig,
				     unsigned int factor)
{
  gcc_assert (factor > 0);

  if (maybe_lt (orig.m_nelts_per_input, factor))
    return false;

  poly_uint64 nelts;
  /* Invalid if vector units number isn't multiple of factor.  */
  if (!multiple_p (orig.m_nelts_per_input, factor, &nelts))
    return false;

  /* Only handle the case that npatterns is multiple of factor.
     FIXME: Try to see whether we can reshape it by factor npatterns.  */
  if (orig.m_encoding.npatterns () % factor != 0)
    return false;

  unsigned int encoded_nelts = orig.m_encoding.encoded_nelts ();
  auto_vec<element_type, 32> encoding (encoded_nelts);
  /* Separate all encoded elements into batches by size factor,
     then ensure the first element of each batch is multiple of
     factor and all elements in each batch is consecutive from
     the first one.  */
  for (unsigned int i = 0; i < encoded_nelts; i += factor)
    {
      element_type first = orig.m_encoding[i];
      element_type new_index;
      if (!multiple_p (first, factor, &new_index))
	return false;
      for (unsigned int j = 1; j < factor; ++j)
	if (maybe_ne (first + j, orig.m_encoding[i + j]))
	  return false;
      encoding.quick_push (new_index);
    }

  m_ninputs = orig.m_ninputs;
  m_nelts_per_input = nelts;
  poly_uint64 full_nelts = exact_div (orig.m_encoding.full_nelts (), factor);
  unsigned int npatterns = orig.m_encoding.npatterns () / factor;

  m_encoding.new_vector (full_nelts, npatterns,
			 orig.m_encoding.nelts_per_pattern ());
  m_encoding.splice (encoding);
  m_encoding.finalize ();

  return true;
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

/* Return true if index OUT_BASE + I * OUT_STEP selects input
   element IN_BASE + I * IN_STEP.  For example, the call to test
   whether a permute reverses a vector of N elements would be:

     series_p (0, 1, N - 1, -1)

   which would return true for { N - 1, N - 2, N - 3, ... }.
   The calls to test for an interleaving of elements starting
   at N1 and N2 would be:

     series_p (0, 2, N1, 1) && series_p (1, 2, N2, 1).

   which would return true for { N1, N2, N1 + 1, N2 + 1, ... }.  */

bool
vec_perm_indices::series_p (unsigned int out_base, unsigned int out_step,
			    element_type in_base, element_type in_step) const
{
  /* Check the base value.  */
  if (maybe_ne (clamp (m_encoding.elt (out_base)), clamp (in_base)))
    return false;

  element_type full_nelts = m_encoding.full_nelts ();
  unsigned int npatterns = m_encoding.npatterns ();

  /* Calculate which multiple of OUT_STEP elements we need to get
     back to the same pattern.  */
  unsigned int cycle_length = least_common_multiple (out_step, npatterns);

  /* Check the steps.  */
  in_step = clamp (in_step);
  out_base += out_step;
  unsigned int limit = 0;
  for (;;)
    {
      /* Succeed if we've checked all the elements in the vector.  */
      if (known_ge (out_base, full_nelts))
	return true;

      if (out_base >= npatterns)
	{
	  /* We've got to the end of the "foreground" values.  Check
	     2 elements from each pattern in the "background" values.  */
	  if (limit == 0)
	    limit = out_base + cycle_length * 2;
	  else if (out_base >= limit)
	    return true;
	}

      element_type v0 = m_encoding.elt (out_base - out_step);
      element_type v1 = m_encoding.elt (out_base);
      if (maybe_ne (clamp (v1 - v0), in_step))
	return false;

      out_base += out_step;
    }
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
    if (!known_in_range_p (m_encoding[i], start, size))
      return false;

  /* For stepped encodings, check the full range of the series.  */
  if (nelts_per_pattern == 3)
    {
      element_type limit = input_nelts ();

      /* The number of elements in each pattern beyond the first two
	 that we checked above.  */
      poly_int64 step_nelts = exact_div (m_encoding.full_nelts (),
					 npatterns) - 2;
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
	  HOST_WIDE_INT diff;
	  if ((!step.is_constant (&diff)
	       || maybe_lt (headroom_up, diff * step_nelts))
	      && (!(limit - step).is_constant (&diff)
		  || maybe_lt (headroom_down, diff * step_nelts)))
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
    if (!tree_fits_poly_int64_p (VECTOR_CST_ENCODED_ELT (cst, i)))
      return false;

  builder->new_vector (TYPE_VECTOR_SUBPARTS (TREE_TYPE (cst)),
		       VECTOR_CST_NPATTERNS (cst),
		       VECTOR_CST_NELTS_PER_PATTERN (cst));
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    builder->quick_push (tree_to_poly_int64 (VECTOR_CST_ENCODED_ELT (cst, i)));
  return true;
}

/* Return a VECTOR_CST of type TYPE for the permutation vector in INDICES.  */

tree
vec_perm_indices_to_tree (tree type, const vec_perm_indices &indices)
{
  gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (type), indices.length ()));
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
	      && known_eq (GET_MODE_NUNITS (mode), indices.length ()));
  rtx_vector_builder sel (mode, indices.encoding ().npatterns (),
			  indices.encoding ().nelts_per_pattern ());
  unsigned int encoded_nelts = sel.encoded_nelts ();
  for (unsigned int i = 0; i < encoded_nelts; i++)
    sel.quick_push (gen_int_mode (indices[i], GET_MODE_INNER (mode)));
  return sel.build ();
}

#if CHECKING_P

namespace selftest {

/* Test a 12-element vector.  */

static void
test_vec_perm_12 (void)
{
  vec_perm_builder builder (12, 12, 1);
  for (unsigned int i = 0; i < 4; ++i)
    {
      builder.quick_push (i * 5);
      builder.quick_push (3 + i);
      builder.quick_push (2 + 3 * i);
    }
  vec_perm_indices indices (builder, 1, 12);
  ASSERT_TRUE (indices.series_p (0, 3, 0, 5));
  ASSERT_FALSE (indices.series_p (0, 3, 3, 5));
  ASSERT_FALSE (indices.series_p (0, 3, 0, 8));
  ASSERT_TRUE (indices.series_p (1, 3, 3, 1));
  ASSERT_TRUE (indices.series_p (2, 3, 2, 3));

  ASSERT_TRUE (indices.series_p (0, 4, 0, 4));
  ASSERT_FALSE (indices.series_p (1, 4, 3, 4));

  ASSERT_TRUE (indices.series_p (0, 6, 0, 10));
  ASSERT_FALSE (indices.series_p (0, 6, 0, 100));

  ASSERT_FALSE (indices.series_p (1, 10, 3, 7));
  ASSERT_TRUE (indices.series_p (1, 10, 3, 8));

  ASSERT_TRUE (indices.series_p (0, 12, 0, 10));
  ASSERT_TRUE (indices.series_p (0, 12, 0, 11));
  ASSERT_TRUE (indices.series_p (0, 12, 0, 100));
}

/* Run selftests for this file.  */

void
vec_perm_indices_cc_tests ()
{
  test_vec_perm_12 ();
}

} // namespace selftest

#endif
