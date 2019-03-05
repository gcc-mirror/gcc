/* A class for building vector tree constants.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.

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
#include "tree.h"
#include "fold-const.h"
#include "tree-vector-builder.h"

/* Try to start building a new vector of type TYPE that holds the result of
   a unary operation on VECTOR_CST T.  ALLOW_STEPPED_P is true if the
   operation can handle stepped encodings directly, without having to
   expand the full sequence.

   Return true if the operation is possible, which it always is when
   ALLOW_STEPPED_P is true.  Leave the builder unchanged otherwise.  */

bool
tree_vector_builder::new_unary_operation (tree type, tree t,
					  bool allow_stepped_p)
{
  poly_uint64 full_nelts = TYPE_VECTOR_SUBPARTS (type);
  gcc_assert (known_eq (full_nelts, TYPE_VECTOR_SUBPARTS (TREE_TYPE (t))));
  unsigned int npatterns = VECTOR_CST_NPATTERNS (t);
  unsigned int nelts_per_pattern = VECTOR_CST_NELTS_PER_PATTERN (t);
  if (!allow_stepped_p && nelts_per_pattern > 2)
    {
      if (!full_nelts.is_constant ())
	return false;
      npatterns = full_nelts.to_constant ();
      nelts_per_pattern = 1;
    }
  new_vector (type, npatterns, nelts_per_pattern);
  return true;
}

/* Try to start building a new vector of type TYPE that holds the result of
   a binary operation on VECTOR_CSTs T1 and T2.  ALLOW_STEPPED_P is true if
   the operation can handle stepped encodings directly, without having to
   expand the full sequence.

   Return true if the operation is possible.  Leave the builder unchanged
   otherwise.  */

bool
tree_vector_builder::new_binary_operation (tree type, tree t1, tree t2,
					   bool allow_stepped_p)
{
  poly_uint64 full_nelts = TYPE_VECTOR_SUBPARTS (type);
  gcc_assert (known_eq (full_nelts, TYPE_VECTOR_SUBPARTS (TREE_TYPE (t1)))
	      && known_eq (full_nelts, TYPE_VECTOR_SUBPARTS (TREE_TYPE (t2))));
  /* Conceptually we split the patterns in T1 and T2 until we have
     an equal number for both.  Each split pattern requires the same
     number of elements per pattern as the original.  E.g. splitting:

       { 1, 2, 3, ... }

     into two gives:

       { 1, 3, 5, ... }
       { 2, 4, 6, ... }

     while splitting:

       { 1, 0, ... }

     into two gives:

       { 1, 0, ... }
       { 0, 0, ... }.  */
  unsigned int npatterns = least_common_multiple (VECTOR_CST_NPATTERNS (t1),
						  VECTOR_CST_NPATTERNS (t2));
  unsigned int nelts_per_pattern = MAX (VECTOR_CST_NELTS_PER_PATTERN (t1),
					VECTOR_CST_NELTS_PER_PATTERN (t2));
  if (!allow_stepped_p && nelts_per_pattern > 2)
    {
      if (!full_nelts.is_constant ())
	return false;
      npatterns = full_nelts.to_constant ();
      nelts_per_pattern = 1;
    }
  new_vector (type, npatterns, nelts_per_pattern);
  return true;
}

/* Return the number of elements that the caller needs to operate on in
   order to handle a binary operation on VECTOR_CSTs T1 and T2.  This static
   function is used instead of new_binary_operation if the result of the
   operation is not a VECTOR_CST.  */

unsigned int
tree_vector_builder::binary_encoded_nelts (tree t1, tree t2)
{
  poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (t1));
  gcc_assert (known_eq (nelts, TYPE_VECTOR_SUBPARTS (TREE_TYPE (t2))));
  /* See new_binary_operation for details.  */
  unsigned int npatterns = least_common_multiple (VECTOR_CST_NPATTERNS (t1),
						  VECTOR_CST_NPATTERNS (t2));
  unsigned int nelts_per_pattern = MAX (VECTOR_CST_NELTS_PER_PATTERN (t1),
					VECTOR_CST_NELTS_PER_PATTERN (t2));
  unsigned HOST_WIDE_INT const_nelts;
  if (nelts.is_constant (&const_nelts))
    return MIN (npatterns * nelts_per_pattern, const_nelts);
  return npatterns * nelts_per_pattern;
}

/* Return a vector element with the value BASE + FACTOR * STEP.  */

tree
tree_vector_builder::apply_step (tree base, unsigned int factor,
				 const wide_int &step) const
{
  return wide_int_to_tree (TREE_TYPE (base),
			   wi::to_wide (base) + factor * step);
}

/* Return a VECTOR_CST for the current constant.  */

tree
tree_vector_builder::build ()
{
  finalize ();
  gcc_assert (pow2p_hwi (npatterns ()));
  tree v = make_vector (exact_log2 (npatterns ()), nelts_per_pattern ());
  TREE_TYPE (v) = m_type;
  memcpy (VECTOR_CST_ENCODED_ELTS (v), address (),
	  encoded_nelts () * sizeof (tree));
  return v;
}
