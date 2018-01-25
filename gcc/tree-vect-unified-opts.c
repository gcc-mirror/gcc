/* lOOP Vectorization using unified representation
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

/* Loop autovectorization using unified representation for permute
   instructions.  */
#if 1

#ifndef GENERATOR_FILE
#include "config.h"
#else
#include "bconfig.h"
#endif

#include "system.h"
#include "coretypes.h"

#ifndef GENERATOR_FILE
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-ssa-loop-manip.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "tree-ssa-propagate.h"
#include "dbgcnt.h"
#include "tree-scalar-evolution.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "target.h"
#include "rtl.h"
#include "tm_p.h"
#include "optabs-tree.h"
#include "dumpfile.h"
#include "alias.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop.h"
#include "expr.h"
#include "builtins.h"
#include "params.h"
#include "pretty-print.h"
#else
# include "errors.h"
#include "machmode.h"
#include "signop.h"
#include "wide-int.h"
#include "double-int.h"
#include "real.h"
#include "fixed-value.h"
#include "statistics.h"
#include "vec.h"
#include "hash-table.h"
#include "hash-set.h"
#include "input.h"
#include "is-a.h"
#include "target.h"
#include "tree-core.h"
#endif

#include "tree-vect-unified.h"

/* Function ILV_arity_reduction.

	ILV_k^N					    ILV_m^Nk/m
      /    |    \        =>                         /   |    \
     /     |     \                               /      |       \
    T1    T2 ... Tk                          /          |   ...     \
	                                 /              |              \
	                             ILV_k/m^N      ILV_k/m^N      ILV_k/m^N
	                              /  |  \        /  |  \        /  |  \
	                             /   |...\      /   |...\      /   |...\
	                           T1 Tm+1 Tk-m+1  T2 Tm+2 Tk-m+2 Tm  T2m  Tk

*/

struct primop_tree *
ILV_arity_reduction (struct primop_tree *root, int from_arity, int to_arity)
{
  struct primop_tree *new_root, *new_child, *tmp;
  tree new_iter_count;
  int i, j;

#ifndef GENERATOR_FILE
  gcc_assert (from_arity == PT_DIVISION (root));

  new_iter_count = size_binop (MULT_EXPR,
			fold_convert (ssizetype, PT_ITER_COUNT (root)),
			ssize_int (from_arity / to_arity));
#else
  new_iter_count = NULL;
#endif

  new_root = create_primTree_combine (POP_ILV, NULL, to_arity,
	 new_iter_count, PT_PARENT (root), PT_VEC_TYPE (root));

  for (i = 0; i < to_arity; i++)
    {
      new_child = create_primTree_combine (POP_ILV, NULL,
	 from_arity / to_arity, PT_ITER_COUNT (root), new_root,
	 PT_VEC_TYPE (root));

      for (j = 0; j < from_arity / to_arity; j++)
	{
	  add_child_at_index (new_child, PT_CHILD (root, j * to_arity + i), j);
	}

      tmp = k_arity_promotion_reduction (new_child, to_arity);

      if (tmp != NULL)
	{
	  add_child_at_index (new_root, tmp, i);
	}
      else
	return NULL;
    }

  return new_root;
}

/*Function EXTR_arity_reduction.

       EXTR Arity Reduction

	               EXTR_k,i^N                     EXTR_m,(i/(k/m))%m^Nm/k
	                    |          =>                      |
	                    |                                  |
	                    T                         EXTR_k/m,i%(k/m)^N
	                                                       |
	                                                       |
	                                                       T
*/

struct primop_tree *
EXTR_arity_reduction (struct primop_tree *root, int from_arity, int to_arity)
{
  struct primop_tree *new_root, *new_child, *tmp;
  tree new_iter_count;
  int i, j;

#ifndef GENERATOR_FILE
  gcc_assert (from_arity == PT_DIVISION (root));

  new_iter_count = size_binop (MULT_EXPR,
			fold_convert (ssizetype, PT_ITER_COUNT (root)),
			ssize_int (to_arity / from_arity));
#else
  new_iter_count = NULL;
#endif

  new_root = create_primTree_partition (POP_EXTR, NULL, to_arity,
		(PT_OPERAND_SELECTOR (root) * to_arity / from_arity) % to_arity,
		new_iter_count, PT_PARENT (root), PT_VEC_TYPE (root));

  new_child = create_primTree_partition (POP_EXTR, NULL, from_arity/to_arity,
		PT_OPERAND_SELECTOR (root) % (from_arity / to_arity),
		PT_ITER_COUNT (root), new_root, PT_VEC_TYPE (root));

  add_child_at_index (new_child, PT_CHILD (root, 0), 0);

  tmp = k_arity_promotion_reduction (new_child, to_arity);

  if (tmp != NULL)
    {
      add_child_at_index (new_root, tmp, 0);
    }
  else
    return NULL;


  return new_root;
}

/* Function k_arity_reduction.

   Driver function for arity reduction of EXTR, ILV, CONCAT and SPLT.  CONCAT
   and SPLT need not be implemented due to phase ordering.  */

struct primop_tree *
k_arity_reduction (struct primop_tree *root, int from_arity, int to_arity)
{
  if (PT_NODE_OP (root) == POP_ILV)
    return ILV_arity_reduction (root, from_arity, to_arity);

  if (PT_NODE_OP (root) == POP_EXTR)
    return EXTR_arity_reduction (root, from_arity, to_arity);

  /* For leaf nodes - like const or memory, return root.  */
  if (PT_ARITY (root) == 0)
    return root;

  return NULL;
}

/* Function ILV_arity_promotion.

		ILV_k^N
		 / | \
		/  |  \
	       /   |   \
	      T1  T2   Tk

	         _||_
	 	 \  /
	          \/

	                   	  ILV_m^Nk/m
	   +-----------------+-----------+-+---------------------+
	  /                 /           /   \                     \
       /                 /           /        \                     \
    /                 /           /             \                     \
EXTR_m/k^N,0 ... EXTR_m/k^N,0 EXTR_m/k^N,1 ... EXTR_m/k^N,m/k-1 ... EXTR_m/k^N,m/k-1
     |                  |          |                   |                     |
    T1                  Tk         T1                  T1                    Tk

*/

struct primop_tree *
ILV_arity_promotion (struct primop_tree *root, int from_arity, int to_arity)
{
  struct primop_tree *new_root, *new_child, *tmp;
  tree new_iter_count;
  int i, j;

#ifndef GENERATOR_FILE
  gcc_assert (from_arity == PT_DIVISION (root));

  new_iter_count = size_binop (EXACT_DIV_EXPR, size_binop (MULT_EXPR,
						 fold_convert (ssizetype,
							 PT_ITER_COUNT (root)),
						 ssize_int (from_arity)),
			       ssize_int (to_arity));
#else
  new_iter_count = NULL;
#endif

  new_root = create_primTree_combine (POP_ILV, NULL, to_arity,
		new_iter_count, PT_PARENT (root), PT_VEC_TYPE (root));
  for (i = 0; i < to_arity / from_arity; i++)
    {
      for (j = 0; j < from_arity; j++)
	{
	  new_child = create_primTree_partition (POP_EXTR, NULL,
			 to_arity / from_arity, i, PT_ITER_COUNT (root),
			 new_root, PT_VEC_TYPE (root));
	  add_child_at_index (new_child, PT_CHILD (root, j), 0);
	  tmp = k_arity_promotion_reduction (new_child, to_arity);
	  if (tmp != NULL)
	    add_child_at_index (new_root, tmp,  i * from_arity + j);
	  else
	    return NULL;
	}
    }

  return new_root;
}

/* Function merge_EXTR_nodes.

   Multiple EXTR nodes can be there in primop_tree, which need to be merged,
   before promoting/reducing EXTR node for better optimization.  If merged EXTR
   node needs further promotion, the vectorization cannot be applied, as EXTR
   promotion can introduce new ILV node.
  TODO - Add stricter check for final promotion/reduction to target specific
  arity.  Currently, we do not block vectorization if EXTR arity cannot be
  promoted.
*/

struct primop_tree *
merge_EXTR_nodes (struct primop_tree *root, int to_arity)
{
  int from_arity, parts, selector;
  struct primop_tree *iter_node, *tmp;
  tree iter_count;

  from_arity = PT_DIVISION (root);
  parts = 1;
  iter_node = root;
  selector = 0;

  while (PT_NODE_OP (iter_node) == POP_EXTR)
    {
      iter_count = PT_ITER_COUNT (iter_node);
      parts = parts * PT_DIVISION (iter_node);
      selector = selector * PT_DIVISION (iter_node)
			    + PT_OPERAND_SELECTOR (iter_node);
      iter_node = PT_CHILD (iter_node, 0);
    }

  if (iter_node != PT_CHILD (root, 0))
    {
      tmp = create_primTree_partition (POP_EXTR, NULL, parts, selector,
			     	       iter_count, PT_PARENT (root),
				       PT_VEC_TYPE (root));
      add_child_at_index (tmp, iter_node, 0);
      return tmp;
    }

  return root;
}

/* Function k_arity_promotion.

   Driver function for arity promotion of EXTR, ILV, CONCAT and SPLT.  CONCAT
   and SPLIT need not be implemented due to phase ordering.  Implementation of
   ILV-promotion introduces new EXTR nodes whereas implementation of
   EXTR-promotion introduce ILV node.  To avoid the infinite loop, only
   ILV-promotion or EXTR-promotion can be implemented.  As the
   k_arity_promotion_reduction is top-down algorithm, ILV-promotion is
   implemented.  For EXTR-promotion, merge newly created EXPR node with child
   EXTR node if any, and hope that arity promotion/reduction is applicable
   to new EXTR node.  */

struct primop_tree *
k_arity_promotion (struct primop_tree *root, int from_arity, int to_arity)
{
  struct primop_tree *tmp_root;

  if (PT_NODE_OP (root) == POP_ILV)
    return ILV_arity_promotion (root, from_arity, to_arity);

  if (PT_NODE_OP (root) == POP_EXTR)
    {
      tmp_root = merge_EXTR_nodes (root, to_arity);
      if (tmp_root != root)
	return k_arity_promotion_reduction (tmp_root, to_arity);
      else
	return root;
    }
/* For leaf nodes - like const or memory, return root.  */
  if (PT_ARITY (root) == 0)
    return root;

  return NULL;
}

/* Function k_arity_promotion_reduction.

   Driver function for promoting/reducing arity of the tree rooted at ROOT from
   FROM_ARITY to TO_ARITY.  */

struct primop_tree *
k_arity_promotion_reduction (struct primop_tree *root, int to_arity)
{
  struct primop_tree *retval = root;
  int from_arity, i;

#ifndef GENERATOR_FILE
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
			"\n k_arity_promotion_reduction: ");
      dump_primtree_node (MSG_NOTE, root);
    }
#endif

  if (PT_NODE_OP (root) == POP_EXTR || PT_NODE_OP (root) == POP_SPLT)
    from_arity = PT_DIVISION (root);
  else
    from_arity = PT_ARITY (root);

  if (PT_NODE_OP (root) >= MAX_TREE_CODES && PT_NODE_OP (root) < POP_COLLAPSE)
    {
      if (from_arity > to_arity)
	{
	  /* Arity reduction.  */
	  if (from_arity % to_arity == 0)
    	    {
    	      retval = k_arity_reduction (root, from_arity, to_arity);
	      return retval;
    	    }
	  else
    	    return NULL;
	}
      else if (from_arity < to_arity)
	{
	  /* Arity promotion.  */
	  if (to_arity % from_arity == 0)
    	    {
    	      retval = k_arity_promotion (root, from_arity, to_arity);
	      return retval;
    	    }
	  else
    	    return NULL;
	}
      else
	{
	  retval = duplicate_prim_node (root);
	  //return retval;
	}
    }

  if (retval != NULL)
    {
      /* The tree node is compute-node.  Hence, no action to be taken for arity
	 promotion/reduction.  However, the subtrees below this root may need
	 arity adjustment.  Hence, invoke k_arity_promotion_reduction algorithm
	 recursively on children of root.  */
      for (i = 0; i < retval->children.length (); i++)
	{
	  struct primop_tree *tmp;
	  tmp = k_arity_promotion_reduction (PT_CHILD (retval, i), to_arity);
	  if (tmp == NULL)
	    return NULL;

	  PT_CHILD (retval, i) = tmp;
	}

      PT_ARITY (retval) = i;
    }

  return retval;


}

struct primtree_hash_table
{
  /* Assuming the operation has maximum 3 children.  The key is created as
     PRIMOP_CODE # <opd0 idx> # <opd1 idx> # <opd2 idx> where
     PRIMOP_CODE is 10 bits, and <opdn idx> is 12 bit each.  */
  long long int key;

  /* List of leaf-nodes in the subtree.  */
  vec<struct primop_tree *> leaves;
};

vec<struct primtree_hash_table *> primop_hash;

bool
compare_leaf_nodes (struct primop_tree *n1, struct primop_tree *n2)
{
  if (PT_NODE_OP (n1) != PT_NODE_OP (n2))
    return false;

  switch (PT_NODE_OP (n1))
    {
#ifndef GENERATOR_FILE
      case POP_MEMREF:
	if (operand_equal_p (PT_MEMVAL_BASE (n1), PT_MEMVAL_BASE (n2), 0)
	    && operand_equal_p (PT_MEMVAL_MULT_IDX (n1),
		 PT_MEMVAL_MULT_IDX (n2), 0)
	    && PT_MEMVAL_IS_READ (n1) == PT_MEMVAL_IS_READ (n2))
	  return true;
	break;
#endif
      case POP_PH:
	if (PT_PH_IDX (n1) == PT_PH_IDX (n2)
	    && PT_PH_TYPE (n1) == PT_PH_TYPE (n1))
	      return true;
	break;

      case POP_CONST:
	gcc_assert (!"CONST is not supported for now.");
	break;

      default:
	gcc_assert (0);
    }

  return false;
}

int
lookup_key_in_table (long long int key, struct primop_tree **leaves, int length)
{
  int i, j;

  for (i = 0; i < primop_hash.length (); i++)
    {
      if (key == primop_hash[i]->key
	  && length == primop_hash[i]->leaves.length ())
	{
	  for (j = 0; j < length; j++)
	    {
	      if (!compare_leaf_nodes (primop_hash[i]->leaves[j], leaves[j]))
		break;
	    }

	  if (j == length)
	    break;
	}
    }

  if (i < primop_hash.length ())
    return i;

  return -1;
}

/* Function annotate_tree_nodes.

   Add each subtree matching to

*/

int
annotate_tree_nodes (struct primop_tree *ptree, int *end,
				struct primop_tree **leaves)
{
  long long int key;
  int idx;
  int length = 0;
  int i, j;
  struct primop_tree *temp[150];
  struct primtree_hash_table *new_hash;

  if (PT_NODE_OP (ptree) == POP_MEMREF
      || PT_NODE_OP (ptree) == POP_CONST
      || PT_NODE_OP (ptree) == POP_PH)
    {
      leaves[(*end)++] = ptree;
      return 0xfff;
    }

  key = PT_NODE_OP (ptree) << 10;

  gcc_assert (ptree->children.length () < 4);

  for (i = 0; i < ptree->children.length (); i++)
    {
      key = (key << 12)
	     | annotate_tree_nodes (PT_CHILD (ptree, i), &length, temp);
      for (j = 0; j < length; j++)
	{
	  leaves[(*end)++] = temp[j];
	}
    }

  idx = lookup_key_in_table (key, leaves, *end);

  if (idx == -1)
    {
      // Create new entry.
      new_hash = (struct primtree_hash_table *) xcalloc (1,
      sizeof (struct primtree_hash_table));
      new_hash->key = key;
      new_hash->leaves = vNULL;
      for (i = 0; i < *end; i++)
	new_hash->leaves.safe_insert (new_hash->leaves.length (), leaves[i]);
      idx = primop_hash.length ();
      primop_hash.safe_insert (idx, new_hash);
    }

  PT_AUX (ptree) = idx;
  return idx;
}

bool
unity_redundancy_elimination_2 (struct primop_tree *ptree,
				struct primop_tree **new_ptree)
{
  bool changed = false;
  int to_be_matched;
  struct primop_tree *temp_ptree;
  int i, j;
  long long int key;
  int idx, end;
  struct primop_tree *leaves[150];
  struct primtree_hash_table *new_hash;

  *new_ptree = ptree;

  if (PT_NODE_OP (ptree) == POP_EXTR
      && PT_NODE_OP (PT_CHILD (ptree, 0)) == POP_ILV
      && PT_DIVISION (ptree) == PT_DIVISION (PT_CHILD (ptree,
					PT_OPERAND_SELECTOR (ptree))))
    {
      /* EXTR nodes have single child - and if it is ILV node, eliminate
	 EXTR and ILV nodes, and replace it with operand_selector^th
	 child of ILV node.  */
      changed = true;
      *new_ptree = PT_CHILD (PT_CHILD (ptree, 0), PT_OPERAND_SELECTOR (ptree));
    }

  if (PT_NODE_OP (ptree) == POP_ILV)
    {
      for (i = 0; i < ptree->children.length (); i++)
	{
	  if (PT_NODE_OP (PT_CHILD (ptree, i)) != POP_EXTR
	      || PT_DIVISION (ptree) != PT_DIVISION (PT_CHILD (ptree, i))
	      || PT_OPERAND_SELECTOR (PT_CHILD (ptree, i)) != i)
	    break;
	  if (i == 0)
	    to_be_matched = (int) PT_AUX (PT_CHILD (PT_CHILD (ptree, i), 0));
	  if (to_be_matched != (int) PT_AUX (PT_CHILD (PT_CHILD (ptree, i), 0)))
	    break;
	}

      if (i == ptree->children.length ())
	{
	  changed = true;
	  *new_ptree = PT_CHILD (PT_CHILD (ptree, 0), 0);
	}
    }

  key = PT_NODE_OP (ptree) << 10;
  end = 0;
  for (i = 0; i < (*new_ptree)->children.length (); i++)
    {
      changed |= unity_redundancy_elimination_2 (PT_CHILD (*new_ptree, i),
			 &temp_ptree);
      PT_CHILD (*new_ptree, i) = temp_ptree;
      PT_PARENT (temp_ptree) = *new_ptree;
      if (PT_NODE_OP (temp_ptree) == POP_MEMREF
	  || PT_NODE_OP (temp_ptree) == POP_PH
	  || PT_NODE_OP (temp_ptree) == POP_CONST)
	{
	  key = (key | 0xfff) << 12;
	  leaves[end++] = temp_ptree; 
	}
      else
	{
      	  key = (key << 12) | PT_AUX(temp_ptree);
      
      	  for (j = 0;
	       j < primop_hash[PT_AUX(temp_ptree)]->leaves.length ();
	       j++)
	    {
      	      leaves[end + j] = primop_hash[PT_AUX(temp_ptree)]->leaves[j];
	    }

          end = end + j;
	}

    }

  idx = lookup_key_in_table (key, leaves, end);

  if (idx == -1)
    {
      // Create new entry.
      new_hash = (struct primtree_hash_table *) xcalloc (1,
      sizeof (struct primtree_hash_table));
      new_hash->key = key;
      new_hash->leaves = vNULL;
      for (i = 0; i < end; i++)
	new_hash->leaves.safe_insert (new_hash->leaves.length (), leaves[i]);
      idx = primop_hash.length ();
      primop_hash.safe_insert (idx, new_hash);
    }
  PT_AUX (*new_ptree) = idx;

    return changed;
}

/* Function unity_redundancy_elimination.

   Perform unity reduction as shown in following rules:
   - ILV_m (EXTR_0 (S), EXTR_1 (S),...EXTR_m-1 (S)) => S
   - EXTR_m,x (ILV_M(S1, S2, ... Sm)) => Sx

*/

struct primop_tree *
unity_redundancy_elimination (struct primop_tree *ptree)
{
  struct primop_tree *dummy[150];
  struct primop_tree *new_ptree;
  int end = 0;
  bool changed;

  annotate_tree_nodes (ptree, &end, dummy);
  changed = false;

  do {
    changed = unity_redundancy_elimination_2 (ptree, &new_ptree);
    //if (ptree == new_ptree)
    //  break;
    ptree = new_ptree;
  } while (changed == true);

  return new_ptree;
}

#endif
