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

/* Check that all the TS_* structures handled by the lto_output_* and
   lto_input_* routines are exactly ALL the structures defined in
   treestruct.def.  */

void
check_handled_ts_structures (void)
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


/* Helper for lto_streamer_cache_insert_1.  Add T to CACHE->NODES at
   slot IX.  */

static void
lto_streamer_cache_add_to_node_array (struct lto_streamer_cache_d *cache,
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


/* Helper for lto_streamer_cache_insert and lto_streamer_cache_insert_at.
   CACHE, T, and IX_P are as in lto_streamer_cache_insert.

   If INSERT_AT_NEXT_SLOT_P is true, T is inserted at the next available
   slot in the cache.  Otherwise, T is inserted at the position indicated
   in *IX_P.

   If T already existed in CACHE, return true.  Otherwise,
   return false.  */

static bool
lto_streamer_cache_insert_1 (struct lto_streamer_cache_d *cache,
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

      lto_streamer_cache_add_to_node_array (cache, ix, t);

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
	  lto_streamer_cache_add_to_node_array (cache, ix, t);
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
lto_streamer_cache_insert (struct lto_streamer_cache_d *cache, tree t,
			   unsigned *ix_p)
{
  return lto_streamer_cache_insert_1 (cache, t, ix_p, true);
}


/* Insert tree node T in CACHE at slot IX.  If T already
   existed in the cache return true.  Otherwise, return false.  */

bool
lto_streamer_cache_insert_at (struct lto_streamer_cache_d *cache,
			      tree t, unsigned ix)
{
  return lto_streamer_cache_insert_1 (cache, t, &ix, false);
}


/* Appends tree node T to CACHE, even if T already existed in it.  */

void
lto_streamer_cache_append (struct lto_streamer_cache_d *cache, tree t)
{
  unsigned ix = VEC_length (tree, cache->nodes);
  lto_streamer_cache_insert_1 (cache, t, &ix, false);
}

/* Return true if tree node T exists in CACHE, otherwise false.  If IX_P is
   not NULL, write to *IX_P the index into the cache where T is stored
   ((unsigned)-1 if T is not found).  */

bool
lto_streamer_cache_lookup (struct lto_streamer_cache_d *cache, tree t,
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
lto_streamer_cache_get (struct lto_streamer_cache_d *cache, unsigned ix)
{
  gcc_assert (cache);

  /* Make sure we're not requesting something we don't have.  */
  gcc_assert (ix < VEC_length (tree, cache->nodes));

  return VEC_index (tree, cache->nodes, ix);
}

/* Create a cache of pickled nodes.  */

struct lto_streamer_cache_d *
lto_streamer_cache_create (void)
{
  struct lto_streamer_cache_d *cache;

  cache = XCNEW (struct lto_streamer_cache_d);

  cache->node_map = pointer_map_create ();

  /* Load all the well-known tree nodes that are always created by
     the compiler on startup.  This prevents writing them out
     unnecessarily.  */
  streamer_hooks.preload_common_nodes (cache);

  return cache;
}


/* Delete the streamer cache C.  */

void
lto_streamer_cache_delete (struct lto_streamer_cache_d *c)
{
  if (c == NULL)
    return;

  pointer_map_destroy (c->node_map);
  VEC_free (tree, heap, c->nodes);
  free (c);
}
