/* Data structures and functions for streaming trees.

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

#ifndef GCC_TREE_STREAMER_H
#define GCC_TREE_STREAMER_H

#include "tree.h"
#include "streamer-hooks.h"
#include "lto-streamer.h"

/* Cache of pickled nodes.  Used to avoid writing the same node more
   than once.  The first time a tree node is streamed out, it is
   entered in this cache.  Subsequent references to the same node are
   resolved by looking it up in this cache.

   This is used in two ways:

   - On the writing side, the first time T is added to STREAMER_CACHE,
     a new reference index is created for T and T is emitted on the
     stream.  If T needs to be emitted again to the stream, instead of
     pickling it again, the reference index is emitted.

   - On the reading side, the first time T is read from the stream, it
     is reconstructed in memory and a new reference index created for
     T.  The reconstructed T is inserted in some array so that when
     the reference index for T is found in the input stream, it can be
     used to look up into the array to get the reconstructed T.  */
struct lto_streamer_cache_d
{
  /* The mapping between tree nodes and slots into the nodes array.  */
  struct pointer_map_t *node_map;

  /* The nodes pickled so far.  */
  VEC(tree,heap) *nodes;
};

/* Return true if tree node EXPR should be streamed as a builtin.  For
   these nodes, we just emit the class and function code.  */
static inline bool
lto_stream_as_builtin_p (tree expr)
{
  return (TREE_CODE (expr) == FUNCTION_DECL
	  && DECL_IS_BUILTIN (expr)
	  && (DECL_BUILT_IN_CLASS (expr) == BUILT_IN_NORMAL
	      || DECL_BUILT_IN_CLASS (expr) == BUILT_IN_MD));
}

/* In tree-streamer-in.c.  */
tree input_string_cst (struct data_in *, struct lto_input_block *);
void lto_streamer_read_tree (struct lto_input_block *, struct data_in *, tree);
tree lto_materialize_tree (struct lto_input_block *, struct data_in *,
			   enum LTO_tags);
void lto_input_tree_pointers (struct lto_input_block *, struct data_in *, tree);
tree lto_get_pickled_tree (struct lto_input_block *, struct data_in *);
tree lto_get_builtin_tree (struct lto_input_block *, struct data_in *);
tree lto_input_integer_cst (struct lto_input_block *, struct data_in *);
struct bitpack_d tree_read_bitfields (struct lto_input_block *, tree);

/* In tree-streamer-out.c.  */
void lto_output_chain (struct output_block *, tree, bool);
void lto_output_tree_header (struct output_block *, tree);
void pack_value_fields (struct bitpack_d *, tree);
void lto_output_tree_pointers (struct output_block *, tree, bool);
void lto_output_integer_cst (struct output_block *, tree, bool);
void lto_output_builtin_tree (struct output_block *, tree);

/* In tree-streamer.c.  */
void check_handled_ts_structures (void);
bool lto_streamer_cache_insert (struct lto_streamer_cache_d *, tree,
 			        unsigned *);
bool lto_streamer_cache_insert_at (struct lto_streamer_cache_d *, tree,
 				   unsigned);
void lto_streamer_cache_append (struct lto_streamer_cache_d *, tree);
bool lto_streamer_cache_lookup (struct lto_streamer_cache_d *, tree,
 			        unsigned *);
tree lto_streamer_cache_get (struct lto_streamer_cache_d *, unsigned);
struct lto_streamer_cache_d *lto_streamer_cache_create (void);
void lto_streamer_cache_delete (struct lto_streamer_cache_d *);

#endif  /* GCC_TREE_STREAMER_H  */
