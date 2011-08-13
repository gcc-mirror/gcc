/* Miscellaneous utilities for tree streaming.  Things that are used
   in both input and output are here.

   Copyright 2011 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@google.com>

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
#include "streamer-hooks.h"
#include "tree-streamer.h"

/* Check that all the TS_* structures handled by the streamer_write_* and
   streamer_read_* routines are exactly ALL the structures defined in
   treestruct.def.  */

void
streamer_check_handled_ts_structures (void)
{
  bool handled_p[LAST_TS_ENUM];
  unsigned i;

  memset (&handled_p, 0, sizeof (handled_p));

  /* These are the TS_* structures that are either handled or
     explicitly ignored by the streamer routines.  */
  handled_p[TS_BASE] = true;
  handled_p[TS_TYPED] = true;
  handled_p[TS_COMMON] = true;
  handled_p[TS_INT_CST] = true;
  handled_p[TS_REAL_CST] = true;
  handled_p[TS_FIXED_CST] = true;
  handled_p[TS_VECTOR] = true;
  handled_p[TS_STRING] = true;
  handled_p[TS_COMPLEX] = true;
  handled_p[TS_IDENTIFIER] = true;
  handled_p[TS_DECL_MINIMAL] = true;
  handled_p[TS_DECL_COMMON] = true;
  handled_p[TS_DECL_WRTL] = true;
  handled_p[TS_DECL_NON_COMMON] = true;
  handled_p[TS_DECL_WITH_VIS] = true;
  handled_p[TS_FIELD_DECL] = true;
  handled_p[TS_VAR_DECL] = true;
  handled_p[TS_PARM_DECL] = true;
  handled_p[TS_LABEL_DECL] = true;
  handled_p[TS_RESULT_DECL] = true;
  handled_p[TS_CONST_DECL] = true;
  handled_p[TS_TYPE_DECL] = true;
  handled_p[TS_FUNCTION_DECL] = true;
  handled_p[TS_TYPE_COMMON] = true;
  handled_p[TS_TYPE_WITH_LANG_SPECIFIC] = true;
  handled_p[TS_TYPE_NON_COMMON] = true;
  handled_p[TS_LIST] = true;
  handled_p[TS_VEC] = true;
  handled_p[TS_EXP] = true;
  handled_p[TS_SSA_NAME] = true;
  handled_p[TS_BLOCK] = true;
  handled_p[TS_BINFO] = true;
  handled_p[TS_STATEMENT_LIST] = true;
  handled_p[TS_CONSTRUCTOR] = true;
  handled_p[TS_OMP_CLAUSE] = true;
  handled_p[TS_OPTIMIZATION] = true;
  handled_p[TS_TARGET_OPTION] = true;
  handled_p[TS_TRANSLATION_UNIT_DECL] = true;

  /* Anything not marked above will trigger the following assertion.
     If this assertion triggers, it means that there is a new TS_*
     structure that should be handled by the streamer.  */
  for (i = 0; i < LAST_TS_ENUM; i++)
    gcc_assert (handled_p[i]);
}


/* Helper for streamer_tree_cache_insert_1.  Add T to CACHE->NODES at
   slot IX.  */

static void
streamer_tree_cache_add_to_node_array (struct streamer_tree_cache_d *cache,
				       unsigned ix, tree t)
{
  /* Make sure we're either replacing an old element or
     appending consecutively.  */
  gcc_assert (ix <= VEC_length (tree, cache->nodes));

  if (ix == VEC_length (tree, cache->nodes))
    VEC_safe_push (tree, heap, cache->nodes, t);
  else
    VEC_replace (tree, cache->nodes, ix, t);
}


/* Helper for streamer_tree_cache_insert and streamer_tree_cache_insert_at.
   CACHE, T, and IX_P are as in streamer_tree_cache_insert.

   If INSERT_AT_NEXT_SLOT_P is true, T is inserted at the next available
   slot in the cache.  Otherwise, T is inserted at the position indicated
   in *IX_P.

   If T already existed in CACHE, return true.  Otherwise,
   return false.  */

static bool
streamer_tree_cache_insert_1 (struct streamer_tree_cache_d *cache,
			      tree t, unsigned *ix_p,
			      bool insert_at_next_slot_p)
{
  void **slot;
  unsigned ix;
  bool existed_p;

  gcc_assert (t);

  slot = pointer_map_insert (cache->node_map, t);
  if (!*slot)
    {
      /* Determine the next slot to use in the cache.  */
      if (insert_at_next_slot_p)
	ix = VEC_length (tree, cache->nodes);
      else
	ix = *ix_p;
       *slot = (void *)(size_t) (ix + 1);

      streamer_tree_cache_add_to_node_array (cache, ix, t);

      /* Indicate that the item was not present in the cache.  */
      existed_p = false;
    }
  else
    {
      ix = (size_t) *slot - 1;

      if (!insert_at_next_slot_p && ix != *ix_p)
	{
	  /* If the caller wants to insert T at a specific slot
	     location, and ENTRY->TO does not match *IX_P, add T to
	     the requested location slot.  */
	  ix = *ix_p;
	  streamer_tree_cache_add_to_node_array (cache, ix, t);
	}

      /* Indicate that T was already in the cache.  */
      existed_p = true;
    }

  if (ix_p)
    *ix_p = ix;

  return existed_p;
}


/* Insert tree node T in CACHE.  If T already existed in the cache
   return true.  Otherwise, return false.

   If IX_P is non-null, update it with the index into the cache where
   T has been stored.  */

bool
streamer_tree_cache_insert (struct streamer_tree_cache_d *cache, tree t,
			    unsigned *ix_p)
{
  return streamer_tree_cache_insert_1 (cache, t, ix_p, true);
}


/* Insert tree node T in CACHE at slot IX.  If T already
   existed in the cache return true.  Otherwise, return false.  */

bool
streamer_tree_cache_insert_at (struct streamer_tree_cache_d *cache,
			       tree t, unsigned ix)
{
  return streamer_tree_cache_insert_1 (cache, t, &ix, false);
}


/* Appends tree node T to CACHE, even if T already existed in it.  */

void
streamer_tree_cache_append (struct streamer_tree_cache_d *cache, tree t)
{
  unsigned ix = VEC_length (tree, cache->nodes);
  streamer_tree_cache_insert_1 (cache, t, &ix, false);
}

/* Return true if tree node T exists in CACHE, otherwise false.  If IX_P is
   not NULL, write to *IX_P the index into the cache where T is stored
   ((unsigned)-1 if T is not found).  */

bool
streamer_tree_cache_lookup (struct streamer_tree_cache_d *cache, tree t,
			    unsigned *ix_p)
{
  void **slot;
  bool retval;
  unsigned ix;

  gcc_assert (t);

  slot = pointer_map_contains  (cache->node_map, t);
  if (slot == NULL)
    {
      retval = false;
      ix = -1;
    }
  else
    {
      retval = true;
      ix = (size_t) *slot - 1;
    }

  if (ix_p)
    *ix_p = ix;

  return retval;
}


/* Return the tree node at slot IX in CACHE.  */

tree
streamer_tree_cache_get (struct streamer_tree_cache_d *cache, unsigned ix)
{
  gcc_assert (cache);

  /* Make sure we're not requesting something we don't have.  */
  gcc_assert (ix < VEC_length (tree, cache->nodes));

  return VEC_index (tree, cache->nodes, ix);
}


/* Record NODE in CACHE.  */

static void
record_common_node (struct streamer_tree_cache_d *cache, tree node)
{
  /* We have to make sure to fill exactly the same number of
     elements for all frontends.  That can include NULL trees.
     As our hash table can't deal with zero entries we'll simply stream
     a random other tree.  A NULL tree never will be looked up so it
     doesn't matter which tree we replace it with, just to be sure
     use error_mark_node.  */
  if (!node)
    node = error_mark_node;

  streamer_tree_cache_append (cache, node);

  if (POINTER_TYPE_P (node)
      || TREE_CODE (node) == COMPLEX_TYPE
      || TREE_CODE (node) == ARRAY_TYPE)
    record_common_node (cache, TREE_TYPE (node));
  else if (TREE_CODE (node) == RECORD_TYPE)
    {
      /* The FIELD_DECLs of structures should be shared, so that every
	 COMPONENT_REF uses the same tree node when referencing a field.
	 Pointer equality between FIELD_DECLs is used by the alias
	 machinery to compute overlapping memory references (See
	 nonoverlapping_component_refs_p).  */
      tree f;
      for (f = TYPE_FIELDS (node); f; f = TREE_CHAIN (f))
	record_common_node (cache, f);
    }
}


/* Preload common nodes into CACHE and make sure they are merged
   properly according to the gimple type table.  */

static void
preload_common_nodes (struct streamer_tree_cache_d *cache)
{
  unsigned i;

  for (i = 0; i < itk_none; i++)
    /* Skip itk_char.  char_type_node is dependent on -f[un]signed-char.  */
    if (i != itk_char)
      record_common_node (cache, integer_types[i]);

  for (i = 0; i < TYPE_KIND_LAST; i++)
    record_common_node (cache, sizetype_tab[i]);

  for (i = 0; i < TI_MAX; i++)
    /* Skip boolean type and constants, they are frontend dependent.  */
    if (i != TI_BOOLEAN_TYPE
	&& i != TI_BOOLEAN_FALSE
	&& i != TI_BOOLEAN_TRUE)
      record_common_node (cache, global_trees[i]);
}


/* Create a cache of pickled nodes.  */

struct streamer_tree_cache_d *
streamer_tree_cache_create (void)
{
  struct streamer_tree_cache_d *cache;

  cache = XCNEW (struct streamer_tree_cache_d);

  cache->node_map = pointer_map_create ();

  /* Load all the well-known tree nodes that are always created by
     the compiler on startup.  This prevents writing them out
     unnecessarily.  */
  preload_common_nodes (cache);

  return cache;
}


/* Delete the streamer cache C.  */

void
streamer_tree_cache_delete (struct streamer_tree_cache_d *c)
{
  if (c == NULL)
    return;

  pointer_map_destroy (c->node_map);
  VEC_free (tree, heap, c->nodes);
  free (c);
}
