/* A splay-tree datatype.  
   Copyright 1998, 1999, 2000, 2002, 2004 Free Software Foundation, Inc.
   Contributed by Mark Mitchell (mark@markmitchell.com).
   Adapted for libmudflap from libiberty by Frank Ch. Eigler <fche@redhat.com>.

This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* For an easily readable description of splay-trees, see:

     Lewis, Harry R. and Denenberg, Larry.  Data Structures and Their
     Algorithms.  Harper-Collins, Inc.  1991.  

   The major feature of splay trees is that all basic tree operations
   are amortized O(log n) time for a tree with n nodes.  

   This version has been further modified to periodically rebalance
   the entire tree, should degenerate access patterns result in a very
   lopsided tree.
*/

#ifndef _SPLAY_TREE_H
#define _SPLAY_TREE_H

/* Use typedefs for the key and data types to facilitate changing
   these types, if necessary.  These types should be sufficiently wide
   that any pointer or scalar can be cast to these types, and then
   cast back, without loss of precision.  */
typedef uintptr_t splay_tree_key;
typedef void *splay_tree_value;

/* Forward declaration for a node in the tree.  */
typedef struct splay_tree_node_s *splay_tree_node;

/* The type of a function used to iterate over the tree.  */
typedef int (*splay_tree_foreach_fn) (splay_tree_node, void *);

/* The nodes in the splay tree.  */
struct splay_tree_node_s
{
  /* Data.  */
  splay_tree_key key;
  splay_tree_value value;
  /* Children.  */
  splay_tree_node left;
  splay_tree_node right;
};

/* The splay tree itself.  */
struct splay_tree_s
{
  /* The root of the tree.  */
  splay_tree_node root;

  /* The last key value for which the tree has been splayed, but not
     since modified.  */
  splay_tree_key last_splayed_key;
  int last_splayed_key_p;

  /* Statistics.  */
  unsigned num_keys;

  /* Traversal recursion control flags.  */
  unsigned max_depth;
  unsigned depth;
  unsigned rebalance_p;
};
typedef struct splay_tree_s *splay_tree;

extern splay_tree splay_tree_new (void);
extern splay_tree_node splay_tree_insert (splay_tree, splay_tree_key, splay_tree_value);
extern void splay_tree_remove (splay_tree, splay_tree_key);
extern splay_tree_node splay_tree_lookup (splay_tree, splay_tree_key);
extern splay_tree_node splay_tree_predecessor (splay_tree, splay_tree_key);
extern splay_tree_node splay_tree_successor (splay_tree, splay_tree_key);
extern splay_tree_node splay_tree_max (splay_tree);
extern splay_tree_node splay_tree_min (splay_tree);
extern int splay_tree_foreach (splay_tree, splay_tree_foreach_fn, void *);
extern void splay_tree_rebalance (splay_tree sp);


#endif /* _SPLAY_TREE_H */
