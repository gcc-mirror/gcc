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
#include "hashtab.h"

struct tree_map_hasher : ggc_cache_hasher<tree_map *>
{
  static inline hashval_t hash (tree_map *m) { return m->hash; }
  static inline bool
  equal (tree_map *a, tree_map *b)
  {
    return a->base.from == b->base.from;
  }

  static void
  handle_cache_entry (tree_map *&m)
  {
    extern void gt_ggc_mx (tree_map *&);
    if (m == HTAB_EMPTY_ENTRY || m == HTAB_DELETED_ENTRY)
      return;
    else if (ggc_marked_p (m->base.from))
      gt_ggc_mx (m);
    else
      m = static_cast<tree_map *> (HTAB_DELETED_ENTRY);
  }
};

/* Hash table for UPC block factor lookups when the block factor
   is not 0 (the indefinite block factor) or 1 (the default).  */
static GTY((cache)) hash_table<tree_map_hasher> *upc_block_factor_htab;

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
  struct tree_map in;
  union
    {
      const_tree ct;
      tree t;
    } ct_to_t;
  ct_to_t.ct = type;
  /* Drop the const qualifier, avoid the warning.  */
  in.base.from = ct_to_t.t;
  in.hash = TYPE_HASH (in.base.from);
  struct tree_map **loc = upc_block_factor_htab->
                            find_slot_with_hash (&in, in.hash, NO_INSERT);
  gcc_assert (loc != NULL);
  struct tree_map *h = *loc;
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping TYPE->BLOCK_FACTOR in the UPC block factor  hashtable.  */

void
upc_block_factor_insert (tree type,
                         tree block_factor)
{

  gcc_assert (type && TYPE_P (type));
  gcc_assert (block_factor && INTEGRAL_TYPE_P (TREE_TYPE (block_factor)));
  gcc_assert (!(integer_zerop (block_factor) || integer_onep (block_factor)));
  tree_map *h = ggc_alloc<tree_map> ();
  h->base.from = type;
  h->hash = TYPE_HASH (type);
  h->to = block_factor;
  tree_map **loc = upc_block_factor_htab->
                     find_slot_with_hash (h, h->hash, INSERT);
  *loc = h;
}

void
upc_block_factor_lookup_init (void)
{
  upc_block_factor_htab = hash_table<tree_map_hasher>::create_ggc (17);
}

#include "gt-tree-upc.h"
