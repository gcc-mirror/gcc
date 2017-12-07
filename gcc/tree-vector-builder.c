/* A class for building vector tree constants.
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
  unsigned int full_nelts = TYPE_VECTOR_SUBPARTS (type);
  gcc_assert (full_nelts == TYPE_VECTOR_SUBPARTS (TREE_TYPE (t)));
  unsigned int npatterns = VECTOR_CST_NPATTERNS (t);
  unsigned int nelts_per_pattern = VECTOR_CST_NELTS_PER_PATTERN (t);
  if (!allow_stepped_p && nelts_per_pattern > 2)
    {
      npatterns = full_nelts;
      nelts_per_pattern = 1;
    }
  new_vector (type, npatterns, nelts_per_pattern);
  return true;
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
