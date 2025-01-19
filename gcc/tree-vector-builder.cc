/* A class for building vector tree constants.
   Copyright (C) 2017-2025 Free Software Foundation, Inc.

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
