/* Miscellaneous utilities for tree streaming.  Things that are used
   in both input and output are here.

   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
				       unsigned ix, tree t, hashval_t hash)
{
  /* Make sure we're either replacing an old element or
     appending consecutively.  */
  gcc_assert (ix <= cache->nodes.length ());

  if (ix == cache->nodes.length ())
    {
      cache->nodes.safe_push (t);
      if (cache->hashes.exists ())
	cache->hashes.safe_push (hash);
    }
  else
    {
      cache->nodes[ix] = t;
      if (cache->hashes.exists ())
	cache->hashes[ix] = hash;
    }
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
			      tree t, hashval_t hash, unsigned *ix_p,
			      bool insert_at_next_slot_p)
{
  unsigned *slot;
  unsigned ix;
  bool existed_p;

  gcc_assert (t);

  slot = cache->node_map->insert (t, &existed_p);
  if (!existed_p)
    {
      /* Determine the next slot to use in the cache.  */
      if (insert_at_next_slot_p)
	ix = cache->nodes.length ();
      else
	ix = *ix_p;
       *slot = ix;

      streamer_tree_cache_add_to_node_array (cache, ix, t, hash);
    }
  else
    {
      ix = *slot;

      if (!insert_at_next_slot_p && ix != *ix_p)
	{
	  /* If the caller wants to insert T at a specific slot
	     location, and ENTRY->TO does not match *IX_P, add T to
	     the requested location slot.  */
	  ix = *ix_p;
	  streamer_tree_cache_add_to_node_array (cache, ix, t, hash);
	  *slot = ix;
	}
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
			    hashval_t hash, unsigned *ix_p)
{
  return streamer_tree_cache_insert_1 (cache, t, hash, ix_p, true);
}


/* Replace the tree node with T in CACHE at slot IX.  */

void
streamer_tree_cache_replace_tree (struct streamer_tree_cache_d *cache,
				  tree t, unsigned ix)
{
  hashval_t hash = 0;
  if (cache->hashes.exists ())
    hash = streamer_tree_cache_get_hash (cache, ix);
  if (!cache->node_map)
    streamer_tree_cache_add_to_node_array (cache, ix, t, hash);
  else
    streamer_tree_cache_insert_1 (cache, t, hash, &ix, false);
}


/* Appends tree node T to CACHE, even if T already existed in it.  */

void
streamer_tree_cache_append (struct streamer_tree_cache_d *cache,
			    tree t, hashval_t hash)
{
  unsigned ix = cache->nodes.length ();
  if (!cache->node_map)
    streamer_tree_cache_add_to_node_array (cache, ix, t, hash);
  else
    streamer_tree_cache_insert_1 (cache, t, hash, &ix, false);
}

/* Return true if tree node T exists in CACHE, otherwise false.  If IX_P is
   not NULL, write to *IX_P the index into the cache where T is stored
   ((unsigned)-1 if T is not found).  */

bool
streamer_tree_cache_lookup (struct streamer_tree_cache_d *cache, tree t,
			    unsigned *ix_p)
{
  unsigned *slot;
  bool retval;
  unsigned ix;

  gcc_assert (t);

  slot = cache->node_map->contains (t);
  if (slot == NULL)
    {
      retval = false;
      ix = -1;
    }
  else
    {
      retval = true;
      ix = *slot;
    }

  if (ix_p)
    *ix_p = ix;

  return retval;
}


/* Record NODE in CACHE.  */

static void
record_common_node (struct streamer_tree_cache_d *cache, tree node)
{
  /* If we recursively end up at nodes we do not want to preload simply don't.
     ???  We'd want to verify that this doesn't happen, or alternatively
     do not recurse at all.  */
  if (node == char_type_node)
    return;

  gcc_checking_assert (node != boolean_type_node
		       && node != boolean_true_node
		       && node != boolean_false_node);

  /* We have to make sure to fill exactly the same number of
     elements for all frontends.  That can include NULL trees.
     As our hash table can't deal with zero entries we'll simply stream
     a random other tree.  A NULL tree never will be looked up so it
     doesn't matter which tree we replace it with, just to be sure
     use error_mark_node.  */
  if (!node)
    node = error_mark_node;

  /* ???  FIXME, devise a better hash value.  But the hash needs to be equal
     for all frontend and lto1 invocations.  So just use the position
     in the cache as hash value.  */
  streamer_tree_cache_append (cache, node, cache->nodes.length ());

  if (POINTER_TYPE_P (node)
      || TREE_CODE (node) == COMPLEX_TYPE
      || TREE_CODE (node) == ARRAY_TYPE)
    record_common_node (cache, TREE_TYPE (node));
  else if (TREE_CODE (node) == RECORD_TYPE)
    {
      /* The FIELD_DECLs of structures should be shared, so that every
	 COMPONENT_REF uses the same tree node when referencing a field.
	 Pointer equality between FIELD_DECLs is used by the alias
	 machinery to compute overlapping component references (see
	 nonoverlapping_component_refs_p and
	 nonoverlapping_component_refs_of_decl_p).  */
      for (tree f = TYPE_FIELDS (node); f; f = TREE_CHAIN (f))
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

  for (i = 0; i < stk_type_kind_last; i++)
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
streamer_tree_cache_create (bool with_hashes, bool with_map)
{
  struct streamer_tree_cache_d *cache;

  cache = XCNEW (struct streamer_tree_cache_d);

  if (with_map)
    cache->node_map = new pointer_map<unsigned>;
  cache->nodes.create (165);
  if (with_hashes)
    cache->hashes.create (165);

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

  if (c->node_map)
    delete c->node_map;
  c->nodes.release ();
  c->hashes.release ();
  free (c);
}
