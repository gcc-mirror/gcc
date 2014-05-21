/* tree-upc.c: UPC language-specific tree node support.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-upc.h"

/* Hash table for UPC block factor lookups when the block factor
   is not 0 (the indefinite block factor) or 1 (the default).  */
static GTY ((if_marked ("tree_map_marked_p"),
           param_is (struct tree_map)))
     htab_t upc_block_factor_for_type;

/* Return the blocking factor of the UPC shared type, TYPE.
   If the blocking factor is NULL, then return the default blocking
   factor of 1.  */

tree
upc_get_block_factor (const tree type)
{
  tree block_factor = size_one_node;
  const tree elt_type = strip_array_types (type);
  if (elt_type && (TREE_CODE (elt_type) != ERROR_MARK)
      && TYPE_HAS_UPC_BLOCK_FACTOR (elt_type))
    block_factor = TYPE_UPC_BLOCK_FACTOR (elt_type);
  return block_factor;
}

/* Return a variant of TYPE, where all UPC qualifiers
   have been removed.  */

tree
build_upc_unshared_type (tree type)
{
  tree u_type = type;
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      const tree elem_type = TREE_TYPE(type);
      const tree u_elem_type = build_upc_unshared_type (elem_type);
      if (u_elem_type != elem_type)
        {
          for (u_type = TYPE_MAIN_VARIANT (type);
               u_type && TREE_TYPE(u_type) != u_elem_type;
               u_type = TYPE_NEXT_VARIANT (u_type)) /* loop */;
          if (!u_type)
            {
              u_type = build_variant_type_copy (type);
              TREE_TYPE (u_type) = u_elem_type;
            }
        }
    }
  else
    {
      const int quals = TYPE_QUALS (type);
      const int u_quals = quals & ~(TYPE_QUAL_UPC_SHARED
                                    | TYPE_QUAL_UPC_RELAXED
                                    | TYPE_QUAL_UPC_STRICT);
      u_type = build_qualified_type (type, u_quals);
    }
  return u_type;
}

/* Lookup the UPC block size of TYPE, and return it if we find one.  */

tree
upc_block_factor_lookup (const_tree type)
{
  struct tree_map *h, in;
  union
    {
      const_tree ct;
      tree t;
    } ct_to_t;
  ct_to_t.ct = type;
  /* Drop the const qualifier, avoid the warning.  */
  in.base.from = ct_to_t.t;

  h = (struct tree_map *)
      htab_find_with_hash (upc_block_factor_for_type, &in, TYPE_HASH (type));
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping TYPE->BLOCK_FACTOR in the UPC block factor  hashtable.  */

void
upc_block_factor_insert (tree type,
                     tree block_factor)
{
  struct tree_map *h;
  void **loc;

  gcc_assert (type && TYPE_P (type));
  gcc_assert (block_factor && INTEGRAL_TYPE_P (TREE_TYPE (block_factor)));
  gcc_assert (!(integer_zerop (block_factor) || integer_onep (block_factor)));
  h = ggc_alloc<tree_map> ();
  h->base.from = type;
  h->to = (tree) block_factor;
  loc = htab_find_slot_with_hash (upc_block_factor_for_type,
                                  h, TYPE_HASH (type), INSERT);
  *(struct tree_map **) loc = h;
}

void
upc_block_factor_lookup_init (void)
{
  upc_block_factor_for_type = htab_create_ggc (512, tree_map_hash,
                                               tree_map_eq, 0);
}

#include "gt-tree-upc.h"
