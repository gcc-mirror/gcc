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
#include "errors.h"
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

/***** Helper functions for prim-tree creation *****/

/* Function init_primop_node.

   This function creates PRIMOP_TREE node and initializes all its fields to 0.
*/

struct primop_tree *
init_primop_node (void)
{
  static int pid = 0;
  struct primop_tree *ptree;
  ptree = (struct primop_tree *) xcalloc (1, sizeof (struct primop_tree));

  PT_PID (ptree) = pid++;
  PT_NODE_OP (ptree) = 0;
  PT_ARITY (ptree) = 0;
  ptree->children = vNULL;
  PT_PARENT (ptree) = NULL;
  PT_ITER_COUNT (ptree) = NULL;
  PT_VEC_SIZE (ptree) = 0;
  PT_VEC_TYPE (ptree) = NULL;
  PT_VEC_INST (ptree) = vNULL;
  PT_TARGET_COST (ptree) = 0;
  PT_NUM_INSTANCES (ptree) = 0;
  PT_LOOP_DEPENDENCES (ptree) = vNULL;
#ifndef GENERATOR_FILE
  PT_DEP (ptree) =vNULL;
#endif
  PT_DEPTH (ptree) = 0;
  PT_ATTR_NO (ptree) = 0;
  PT_AUX (ptree) = -1;
  memset (&ptree->u, 0, sizeof (ptree->u));
  return ptree;
}

/* Function populate_prim_node.

   This function returns PRIMOP_TREE node initialized with given information.
*/

struct primop_tree *
populate_prim_node (enum primop_code pcode, tree iter_count,
		    struct primop_tree *parent, gimple *stmt, tree vec_type)
{
  struct primop_tree *ptree;
  ptree = init_primop_node ();

  PT_NODE_OP (ptree) = (int) pcode;
  PT_PARENT (ptree) = parent;
  PT_ITER_COUNT (ptree) = iter_count;
  PT_VEC_TYPE (ptree) = vec_type;
#ifndef GENERATOR_FILE
  if (stmt)
    {
      PT_VEC_TYPE (ptree) = STMT_ATTR_VECTYPE (stmt);
      PT_ATTR_NO (ptree) = gimple_uid (stmt);
      STMT_ATTR_TREE (stmt) = ptree;
    }
#endif
  return ptree;
}

/* Function create_primTree_combine.

   Create primtree with PCODE as interleave or concat.  STMT is statement for
   which primtree is being created.  */
struct primop_tree *
create_primTree_combine (enum primop_code pcode, gimple *stmt, int parts,
			 tree iter_count, struct primop_tree *parent,
			 tree vec_type)
{
  struct primop_tree * ptree;

  ptree = populate_prim_node (pcode, iter_count, parent, stmt, vec_type);
  PT_OPERAND_SELECTOR (ptree) = -1;
  PT_DIVISION (ptree) = parts;
  PT_VAR_STRIDE (ptree) = NULL;

#ifndef GENERATOR_FILE
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       " create_primTree_combine %d : parts - %d\n",
			PT_PID (ptree), parts);
    }
#endif

  return ptree;
}

/* Function create_primTree_partition.

   Create primtree with PCODE as split or extract.  STMT is statement for which
   primtree is being created.  PARTS is number of partitions to be created.
   SELECTOR is the part being selected.  */
struct primop_tree *
create_primTree_partition (enum primop_code pcode, gimple *stmt, int parts,
			   int selector, tree iter_count,
			   struct primop_tree *parent, tree vec_type)
{
  struct primop_tree * ptree;

  ptree = populate_prim_node (pcode, iter_count, parent, stmt, vec_type);
  PT_OPERAND_SELECTOR (ptree) = selector;
  PT_DIVISION (ptree) = parts;
  PT_VAR_STRIDE (ptree) = NULL;

#ifndef GENERATOR_FILE
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		   " create_primTree_partition %d : parts - %d selector - %d\n",
		   PT_PID (ptree), parts, selector);
    }
#endif

  return ptree;
}

/* Function add_child_at_index.

   Attach PCHILD node as idx^th child of PNODE.  */
void
add_child_at_index (struct primop_tree *ptree,
		    struct primop_tree *pchild, int idx)
{
  (PT_ARITY (ptree))++;
  while (idx >= ptree->children.length ())
    {
      ptree->children.safe_push (NULL);
    }
  PT_CHILD (ptree, idx) = pchild;
}

/* Function get_child_at_index.

   Get idx^th child of PNODE.  */
struct primop_tree *
get_child_at_index (struct primop_tree *ptree, int idx)
{
#ifndef GENERATOR_FILE
  gcc_assert (idx < PT_ARITY (ptree));
#endif
  return PT_CHILD (ptree, idx);
}

/* Function duplicate_prim_node.

   This function copies contents of SRC node into new node, and returns pointer
   to it.
*/

struct primop_tree *
duplicate_prim_node (struct primop_tree *src)
{
  struct primop_tree *ptree;
  int i;

  ptree = init_primop_node ();

  PT_NODE_OP (ptree) = PT_NODE_OP (src);
  PT_PARENT (ptree) = PT_PARENT (src);
  PT_ITER_COUNT (ptree) = PT_ITER_COUNT (src);
  PT_VEC_SIZE (ptree) = PT_VEC_SIZE (src);
  PT_VEC_TYPE (ptree) = PT_VEC_TYPE (src);

  PT_ATTR_NO (ptree) = PT_ATTR_NO (src);

  for (i = 0; i < src->children.length (); i++)
    {
      add_child_at_index (ptree, PT_CHILD (src, i), i);
    }

  memcpy (&ptree->u, &src->u, sizeof (ptree->u));

#ifndef GENERATOR_FILE
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		   " duplicate_prim_node : pid - %d node_op - %s\n",
		   PT_PID (src), tree_code_name[PT_NODE_OP (ptree)]);
    }
#endif

  return ptree;
}



#endif
