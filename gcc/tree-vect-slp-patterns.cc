/* SLP - Pattern matcher on SLP trees
   Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "insn-config.h"
#include "recog.h"		/* FIXME: for insn_data */
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "langhooks.h"
#include "gimple-walk.h"
#include "dbgcnt.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"
#include "gimple-fold.h"
#include "internal-fn.h"

/* SLP Pattern matching mechanism.

  This extension to the SLP vectorizer allows one to transform the generated SLP
  tree based on any pattern.  The difference between this and the normal vect
  pattern matcher is that unlike the former, this matcher allows you to match
  with instructions that do not belong to the same SSA dominator graph.

  The only requirement that this pattern matcher has is that you are only
  only allowed to either match an entire group or none.

  The pattern matcher currently only allows you to perform replacements to
  internal functions.

  Once the patterns are matched it is one way, these cannot be undone.  It is
  currently not supported to match patterns recursively.

  To add a new pattern, implement the vect_pattern class and add the type to
  slp_patterns.

*/

/*******************************************************************************
 * vect_pattern class
 ******************************************************************************/

/* Default implementation of recognize that performs matching, validation and
   replacement of nodes but that can be overriden if required.  */

static bool
vect_pattern_validate_optab (internal_fn ifn, slp_tree node)
{
  tree vectype = SLP_TREE_VECTYPE (node);
  if (ifn == IFN_LAST || !vectype)
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Found %s pattern in SLP tree\n",
		     internal_fn_name (ifn));

  if (direct_internal_fn_supported_p (ifn, vectype, OPTIMIZE_FOR_SPEED))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Target supports %s vectorization with mode %T\n",
			 internal_fn_name (ifn), vectype);
    }
  else
    {
      if (dump_enabled_p ())
        {
	  if (!vectype)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Target does not support vector type for %G\n",
			     STMT_VINFO_STMT (SLP_TREE_REPRESENTATIVE (node)));
	  else
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Target does not support %s for vector type "
			     "%T\n", internal_fn_name (ifn), vectype);
	}
      return false;
    }
  return true;
}

/*******************************************************************************
 * General helper types
 ******************************************************************************/

/* The COMPLEX_OPERATION enum denotes the possible pair of operations that can
   be matched when looking for expressions that we are interested matching for
   complex numbers addition and mla.  */

typedef enum _complex_operation : unsigned {
  PLUS_PLUS,
  MINUS_PLUS,
  PLUS_MINUS,
  MULT_MULT,
  CMPLX_NONE
} complex_operation_t;

/*******************************************************************************
 * General helper functions
 ******************************************************************************/

/* Helper function of linear_loads_p that checks to see if the load permutation
   is sequential and in monotonically increasing order of loads with no gaps.
*/

static inline complex_perm_kinds_t
is_linear_load_p (load_permutation_t loads)
{
  if (loads.length() == 0)
    return PERM_UNKNOWN;

  unsigned load, i;
  complex_perm_kinds_t candidates[4]
    = { PERM_ODDODD
      , PERM_EVENEVEN
      , PERM_EVENODD
      , PERM_ODDEVEN
      };

  int valid_patterns = 4;
  FOR_EACH_VEC_ELT (loads, i, load)
    {
      unsigned adj_load = load % 2;
      if (candidates[0] != PERM_UNKNOWN && adj_load != 1)
	{
	  candidates[0] = PERM_UNKNOWN;
	  valid_patterns--;
	}
      if (candidates[1] != PERM_UNKNOWN && adj_load != 0)
	{
	  candidates[1] = PERM_UNKNOWN;
	  valid_patterns--;
	}
      if (candidates[2] != PERM_UNKNOWN && load != i)
	{
	  candidates[2] = PERM_UNKNOWN;
	  valid_patterns--;
	}
      if (candidates[3] != PERM_UNKNOWN
	  && load != (i % 2 == 0 ? i + 1 : i - 1))
	{
	  candidates[3] = PERM_UNKNOWN;
	  valid_patterns--;
	}

      if (valid_patterns == 0)
	return PERM_UNKNOWN;
    }

  for (i = 0; i < sizeof(candidates); i++)
    if (candidates[i] != PERM_UNKNOWN)
      return candidates[i];

  return PERM_UNKNOWN;
}

/* Combine complex_perm_kinds A and B into a new permute kind that describes the
   resulting operation.  */

static inline complex_perm_kinds_t
vect_merge_perms (complex_perm_kinds_t a, complex_perm_kinds_t b)
{
  if (a == b)
    return a;

  if (a == PERM_TOP)
    return b;

  if (b == PERM_TOP)
    return a;

  return PERM_UNKNOWN;
}

/* Check to see if all loads rooted in ROOT are linear.  Linearity is
   defined as having no gaps between values loaded.  */

static complex_perm_kinds_t
linear_loads_p (slp_tree_to_load_perm_map_t *perm_cache, slp_tree root)
{
  if (!root)
    return PERM_UNKNOWN;

  unsigned i;
  complex_perm_kinds_t *tmp;

  if ((tmp = perm_cache->get (root)) != NULL)
    return *tmp;

  complex_perm_kinds_t retval = PERM_UNKNOWN;
  perm_cache->put (root, retval);

  /* If it's a load node, then just read the load permute.  */
  if (SLP_TREE_DEF_TYPE (root) == vect_internal_def
      && SLP_TREE_CODE (root) != VEC_PERM_EXPR
      && STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (root))
      && DR_IS_READ (STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (root))))
    {
      if (SLP_TREE_LOAD_PERMUTATION (root).exists ())
	retval = is_linear_load_p (SLP_TREE_LOAD_PERMUTATION (root));
      else
	retval = PERM_EVENODD;
      perm_cache->put (root, retval);
      return retval;
    }
  else if (SLP_TREE_DEF_TYPE (root) != vect_internal_def)
    {
      retval = PERM_TOP;
      perm_cache->put (root, retval);
      return retval;
    }

  complex_perm_kinds_t kind = PERM_TOP;

  slp_tree child;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (root), i, child)
    {
      complex_perm_kinds_t res = linear_loads_p (perm_cache, child);
      kind = vect_merge_perms (kind, res);
      /* Unknown and Top are not valid on blends as they produce no permute.  */
      retval = kind;
      if (kind == PERM_UNKNOWN || kind == PERM_TOP)
	return retval;
    }

  retval = kind;

  perm_cache->put (root, retval);
  return retval;
}


/* This function attempts to make a node rooted in NODE is linear.  If the node
   if already linear than the node itself is returned in RESULT.

   If the node is not linear then a new VEC_PERM_EXPR node is created with a
   lane permute that when applied will make the node linear.   If such a
   permute cannot be created then FALSE is returned from the function.

   Here linearity is defined as having a sequential, monotically increasing
   load position inside the load permute generated by the loads reachable from
   NODE.  */

static slp_tree
vect_build_swap_evenodd_node (slp_tree node)
{
  /* Attempt to linearise the permute.  */
  vec<std::pair<unsigned, unsigned> > zipped;
  zipped.create (SLP_TREE_LANES (node));

  for (unsigned x = 0; x < SLP_TREE_LANES (node); x+=2)
    {
      zipped.quick_push (std::make_pair (0, x+1));
      zipped.quick_push (std::make_pair (0, x));
    }

  /* Create the new permute node and store it instead.  */
  slp_tree vnode = vect_create_new_slp_node (1, VEC_PERM_EXPR);
  SLP_TREE_LANE_PERMUTATION (vnode) = zipped;
  SLP_TREE_VECTYPE (vnode) = SLP_TREE_VECTYPE (node);
  SLP_TREE_CHILDREN (vnode).quick_push (node);
  SLP_TREE_REF_COUNT (vnode) = 1;
  SLP_TREE_LANES (vnode) = SLP_TREE_LANES (node);
  SLP_TREE_REPRESENTATIVE (vnode) = SLP_TREE_REPRESENTATIVE (node);
  SLP_TREE_REF_COUNT (node)++;
  return vnode;
}

/* Checks to see of the expression represented by NODE is a gimple assign with
   code CODE.  */

static inline bool
vect_match_expression_p (slp_tree node, tree_code code)
{
  if (!node
      || !SLP_TREE_REPRESENTATIVE (node))
    return false;

  gimple* expr = STMT_VINFO_STMT (SLP_TREE_REPRESENTATIVE (node));
  if (!is_gimple_assign (expr)
      || gimple_assign_rhs_code (expr) != code)
    return false;

  return true;
}

/* Check if the given lane permute in PERMUTES matches an alternating sequence
   of {even odd even odd ...}.  This to account for unrolled loops.  Further
   mode there resulting permute must be linear.   */

static inline bool
vect_check_evenodd_blend (lane_permutation_t &permutes,
			 unsigned even, unsigned odd)
{
  if (permutes.length () == 0
      || permutes.length () % 2 != 0)
    return false;

  unsigned val[2] = {even, odd};
  unsigned seed = 0;
  for (unsigned i = 0; i < permutes.length (); i++)
    if (permutes[i].first != val[i % 2]
	|| permutes[i].second != seed++)
      return false;

  return true;
}

/* This function will match the two gimple expressions representing NODE1 and
   NODE2 in parallel and returns the pair operation that represents the two
   expressions in the two statements.

   If match is successful then the corresponding complex_operation is
   returned and the arguments to the two matched operations are returned in OPS.

   If TWO_OPERANDS it is expected that the LANES of the parent VEC_PERM select
   from the two nodes alternatingly.

   If unsuccessful then CMPLX_NONE is returned and OPS is untouched.

   e.g. the following gimple statements

   stmt 0 _39 = _37 + _12;
   stmt 1 _6 = _38 - _36;

   will return PLUS_MINUS along with OPS containing {_37, _12, _38, _36}.
*/

static complex_operation_t
vect_detect_pair_op (slp_tree node1, slp_tree node2, lane_permutation_t &lanes,
		     bool two_operands = true, vec<slp_tree> *ops = NULL)
{
  complex_operation_t result = CMPLX_NONE;

  if (vect_match_expression_p (node1, MINUS_EXPR)
      && vect_match_expression_p (node2, PLUS_EXPR)
      && (!two_operands || vect_check_evenodd_blend (lanes, 0, 1)))
    result = MINUS_PLUS;
  else if (vect_match_expression_p (node1, PLUS_EXPR)
	   && vect_match_expression_p (node2, MINUS_EXPR)
	   && (!two_operands || vect_check_evenodd_blend (lanes, 0, 1)))
    result = PLUS_MINUS;
  else if (vect_match_expression_p (node1, PLUS_EXPR)
	   && vect_match_expression_p (node2, PLUS_EXPR))
    result = PLUS_PLUS;
  else if (vect_match_expression_p (node1, MULT_EXPR)
	   && vect_match_expression_p (node2, MULT_EXPR))
    result = MULT_MULT;

  if (result != CMPLX_NONE && ops != NULL)
    {
      if (two_operands)
	{
	  auto l0node = SLP_TREE_CHILDREN (node1);
	  auto l1node = SLP_TREE_CHILDREN (node2);

	  /* Check if the tree is connected as we expect it.  */
	  if (!((l0node[0] == l1node[0] && l0node[1] == l1node[1])
	      || (l0node[0] == l1node[1] && l0node[1] == l1node[0])))
	    return CMPLX_NONE;
	}
      ops->safe_push (node1);
      ops->safe_push (node2);
    }
  return result;
}

/* Overload of vect_detect_pair_op that matches against the representative
   statements in the children of NODE.  It is expected that NODE has exactly
   two children and when TWO_OPERANDS then NODE must be a VEC_PERM.  */

static complex_operation_t
vect_detect_pair_op (slp_tree node, bool two_operands = true,
		     vec<slp_tree> *ops = NULL)
{
  if (!two_operands && SLP_TREE_CODE (node) == VEC_PERM_EXPR)
    return CMPLX_NONE;

  if (SLP_TREE_CHILDREN (node).length () != 2)
    return CMPLX_NONE;

  vec<slp_tree> children = SLP_TREE_CHILDREN (node);
  lane_permutation_t &lanes = SLP_TREE_LANE_PERMUTATION (node);

  return vect_detect_pair_op (children[0], children[1], lanes, two_operands,
			      ops);
}

/*******************************************************************************
 * complex_pattern class
 ******************************************************************************/

/* SLP Complex Numbers pattern matching.

  As an example, the following simple loop:

    double a[restrict N]; double b[restrict N]; double c[restrict N];

    for (int i=0; i < N; i+=2)
    {
      c[i] = a[i] - b[i+1];
      c[i+1] = a[i+1] + b[i];
    }

  which represents a complex addition on with a rotation of 90* around the
  argand plane. i.e. if `a` and `b` were complex numbers then this would be the
  same as `a + (b * I)`.

  Here the expressions for `c[i]` and `c[i+1]` are independent but have to be
  both recognized in order for the pattern to work.  As an SLP tree this is
  represented as

                +--------------------------------+
                |       stmt 0 *_9 = _10;        |
                |       stmt 1 *_15 = _16;       |
                +--------------------------------+
                                |
                                |
                                v
                +--------------------------------+
                |     stmt 0 _10 = _4 - _8;      |
                |    stmt 1 _16 = _12 + _14;     |
                | lane permutation { 0[0] 1[1] } |
                +--------------------------------+
                            |        |
                            |        |
                            |        |
               +-----+      |        |      +-----+
               |     |      |        |      |     |
         +-----| { } |<-----+        +----->| { } --------+
         |     |     |   +------------------|     |       |
         |     +-----+   |                  +-----+       |
         |        |      |                                |
         |        |      |                                |
         |        +------|------------------+             |
         |               |                  |             |
         v               v                  v             v
     +--------------------------+     +--------------------------------+
     |     stmt 0 _8 = *_7;     |     |        stmt 0 _4 = *_3;        |
     |    stmt 1 _14 = *_13;    |     |       stmt 1 _12 = *_11;       |
     | load permutation { 1 0 } |     |    load permutation { 0 1 }    |
     +--------------------------+     +--------------------------------+

  The pattern matcher allows you to replace both statements 0 and 1 or none at
  all.  Because this operation is a two operands operation the actual nodes
  being replaced are those in the { } nodes.  The actual scalar statements
  themselves are not replaced or used during the matching but instead the
  SLP_TREE_REPRESENTATIVE statements are inspected.  You are also allowed to
  replace and match on any number of nodes.

  Because the pattern matcher matches on the representative statement for the
  SLP node the case of two_operators it allows you to match the children of the
  node.  This is done using the method `recognize ()`.

*/

/* The complex_pattern class contains common code for pattern matchers that work
   on complex numbers.  These provide functionality to allow de-construction and
   validation of sequences depicting/transforming REAL and IMAG pairs.  */

class complex_pattern : public vect_pattern
{
  protected:
    auto_vec<slp_tree> m_workset;
    complex_pattern (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
      : vect_pattern (node, m_ops, ifn)
    {
      this->m_workset.safe_push (*node);
    }

  public:
    void build (vec_info *) override;

    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *, slp_tree *,
	     vec<slp_tree> *);
};

/* Create a replacement pattern statement for each node in m_node and inserts
   the new statement into m_node as the new representative statement.  The old
   statement is marked as being in a pattern defined by the new statement.  The
   statement is created as call to internal function IFN with m_num_args
   arguments.

   Futhermore the new pattern is also added to the vectorization information
   structure VINFO and the old statement STMT_INFO is marked as unused while
   the new statement is marked as used and the number of SLP uses of the new
   statement is incremented.

   The newly created SLP nodes are marked as SLP only and will be dissolved
   if SLP is aborted.

   The newly created gimple call is returned and the BB remains unchanged.

   This default method is designed to only match against simple operands where
   all the input and output types are the same.
*/

void
complex_pattern::build (vec_info *vinfo)
{
  stmt_vec_info stmt_info;

  auto_vec<tree> args;
  args.create (this->m_num_args);
  args.quick_grow_cleared (this->m_num_args);
  slp_tree node;
  unsigned ix;
  stmt_vec_info call_stmt_info;
  gcall *call_stmt = NULL;

  /* Now modify the nodes themselves.  */
  FOR_EACH_VEC_ELT (this->m_workset, ix, node)
    {
      /* Calculate the location of the statement in NODE to replace.  */
      stmt_info = SLP_TREE_REPRESENTATIVE (node);
      stmt_vec_info reduc_def
	= STMT_VINFO_REDUC_DEF (vect_orig_stmt (stmt_info));
      gimple* old_stmt = STMT_VINFO_STMT (stmt_info);
      tree lhs_old_stmt = gimple_get_lhs (old_stmt);
      tree type = TREE_TYPE (lhs_old_stmt);

      /* Create the argument set for use by gimple_build_call_internal_vec.  */
      for (unsigned i = 0; i < this->m_num_args; i++)
	args[i] = lhs_old_stmt;

      /* Create the new pattern statements.  */
      call_stmt = gimple_build_call_internal_vec (this->m_ifn, args);
      tree var = make_temp_ssa_name (type, call_stmt, "slp_patt");
      gimple_call_set_lhs (call_stmt, var);
      gimple_set_location (call_stmt, gimple_location (old_stmt));
      gimple_call_set_nothrow (call_stmt, true);

      /* Adjust the book-keeping for the new and old statements for use during
	 SLP.  This is required to get the right VF and statement during SLP
	 analysis.  These changes are created after relevancy has been set for
	 the nodes as such we need to manually update them.  Any changes will be
	 undone if SLP is cancelled.  */
      call_stmt_info
	= vinfo->add_pattern_stmt (call_stmt, stmt_info);

      /* Make sure to mark the representative statement pure_slp and
	 relevant and transfer reduction info. */
      STMT_VINFO_RELEVANT (call_stmt_info) = vect_used_in_scope;
      STMT_SLP_TYPE (call_stmt_info) = pure_slp;
      STMT_VINFO_REDUC_DEF (call_stmt_info) = reduc_def;

      gimple_set_bb (call_stmt, gimple_bb (stmt_info->stmt));
      STMT_VINFO_VECTYPE (call_stmt_info) = SLP_TREE_VECTYPE (node);
      STMT_VINFO_SLP_VECT_ONLY_PATTERN (call_stmt_info) = true;

      /* Since we are replacing all the statements in the group with the same
	 thing it doesn't really matter.  So just set it every time a new stmt
	 is created.  */
      SLP_TREE_REPRESENTATIVE (node) = call_stmt_info;
      SLP_TREE_LANE_PERMUTATION (node).release ();
      SLP_TREE_CODE (node) = CALL_EXPR;
    }
}

/*******************************************************************************
 * complex_add_pattern class
 ******************************************************************************/

class complex_add_pattern : public complex_pattern
{
  protected:
    complex_add_pattern (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
      : complex_pattern (node, m_ops, ifn)
    {
      this->m_num_args = 2;
    }

  public:
    void build (vec_info *) final override;
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *,
	     slp_compat_nodes_map_t *, slp_tree *, vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_compat_nodes_map_t *,
	       slp_tree *);

    static vect_pattern*
    mkInstance (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
    {
      return new complex_add_pattern (node, m_ops, ifn);
    }
};

/* Perform a replacement of the detected complex add pattern with the new
   instruction sequences.  */

void
complex_add_pattern::build (vec_info *vinfo)
{
  SLP_TREE_CHILDREN (*this->m_node).reserve_exact (2);

  slp_tree node = this->m_ops[0];
  vec<slp_tree> children = SLP_TREE_CHILDREN (node);

  /* First re-arrange the children.  */
  SLP_TREE_CHILDREN (*this->m_node)[0] = children[0];
  SLP_TREE_CHILDREN (*this->m_node)[1] =
    vect_build_swap_evenodd_node (children[1]);

  SLP_TREE_REF_COUNT (SLP_TREE_CHILDREN (*this->m_node)[0])++;
  SLP_TREE_REF_COUNT (SLP_TREE_CHILDREN (*this->m_node)[1])++;
  vect_free_slp_tree (this->m_ops[0]);
  vect_free_slp_tree (this->m_ops[1]);

  complex_pattern::build (vinfo);
}

/* Pattern matcher for trying to match complex addition pattern in SLP tree.

   If no match is found then IFN is set to IFN_LAST.
   This function matches the patterns shaped as:

   c[i] = a[i] - b[i+1];
   c[i+1] = a[i+1] + b[i];

   If a match occurred then TRUE is returned, else FALSE.  The initial match is
   expected to be in OP1 and the initial match operands in args0.  */

internal_fn
complex_add_pattern::matches (complex_operation_t op,
			      slp_tree_to_load_perm_map_t *perm_cache,
			      slp_compat_nodes_map_t * /* compat_cache */,
			      slp_tree *node, vec<slp_tree> *ops)
{
  internal_fn ifn = IFN_LAST;

  /* Find the two components.  Rotation in the complex plane will modify
     the operations:

      * Rotation  0: + +
      * Rotation 90: - +
      * Rotation 180: - -
      * Rotation 270: + -

      Rotation 0 and 180 can be handled by normal SIMD code, so we don't need
      to care about them here.  */
  if (op == MINUS_PLUS)
    ifn = IFN_COMPLEX_ADD_ROT90;
  else if (op == PLUS_MINUS)
    ifn = IFN_COMPLEX_ADD_ROT270;
  else
    return ifn;

  /* verify that there is a permute, otherwise this isn't a pattern we
     we support.  */
  gcc_assert (ops->length () == 2);

  vec<slp_tree> children = SLP_TREE_CHILDREN ((*ops)[0]);

  /* First node must be unpermuted.  */
  if (linear_loads_p (perm_cache, children[0]) != PERM_EVENODD)
    return IFN_LAST;

  /* Second node must be permuted.  */
  if (linear_loads_p (perm_cache, children[1]) != PERM_ODDEVEN)
    return IFN_LAST;

  if (!vect_pattern_validate_optab (ifn, *node))
    return IFN_LAST;

  return ifn;
}

/* Attempt to recognize a complex add pattern.  */

vect_pattern*
complex_add_pattern::recognize (slp_tree_to_load_perm_map_t *perm_cache,
				slp_compat_nodes_map_t *compat_cache,
				slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn
    = complex_add_pattern::matches (op, perm_cache, compat_cache, node, &ops);
  if (ifn == IFN_LAST)
    return NULL;

  return new complex_add_pattern (node, &ops, ifn);
}

/*******************************************************************************
 * complex_mul_pattern
 ******************************************************************************/

/* Helper function to check if PERM is KIND or PERM_TOP.  */

static inline bool
is_eq_or_top (slp_tree_to_load_perm_map_t *perm_cache,
	      slp_tree op1, complex_perm_kinds_t kind1,
	      slp_tree op2, complex_perm_kinds_t kind2)
{
  complex_perm_kinds_t perm1 = linear_loads_p (perm_cache, op1);
  if (perm1 != kind1 && perm1 != PERM_TOP)
    return false;

  complex_perm_kinds_t perm2 = linear_loads_p (perm_cache, op2);
  if (perm2 != kind2 && perm2 != PERM_TOP)
    return false;

  return true;
}

enum _conj_status { CONJ_NONE, CONJ_FST, CONJ_SND };

static inline bool
compatible_complex_nodes_p (slp_compat_nodes_map_t *compat_cache,
			    slp_tree a, int *pa, slp_tree b, int *pb)
{
  bool *tmp;
  std::pair<slp_tree, slp_tree> key = std::make_pair(a, b);
  if ((tmp = compat_cache->get (key)) != NULL)
    return *tmp;

   compat_cache->put (key, false);

  if (SLP_TREE_CHILDREN (a).length () != SLP_TREE_CHILDREN (b).length ())
    return false;

  if (SLP_TREE_DEF_TYPE (a) != SLP_TREE_DEF_TYPE (b))
    return false;

  /* Only internal nodes can be loads, as such we can't check further if they
     are externals.  */
  if (SLP_TREE_DEF_TYPE (a) != vect_internal_def)
    {
      for (unsigned i = 0; i < SLP_TREE_SCALAR_OPS (a).length (); i++)
	{
	  tree op1 = SLP_TREE_SCALAR_OPS (a)[pa[i % 2]];
	  tree op2 = SLP_TREE_SCALAR_OPS (b)[pb[i % 2]];
	  if (!operand_equal_p (op1, op2, 0))
	    return false;
	}

      compat_cache->put (key, true);
      return true;
    }

  auto a_stmt = STMT_VINFO_STMT (SLP_TREE_REPRESENTATIVE (a));
  auto b_stmt = STMT_VINFO_STMT (SLP_TREE_REPRESENTATIVE (b));

  if (gimple_code (a_stmt) != gimple_code (b_stmt))
    return false;

  /* code, children, type, externals, loads, constants  */
  if (gimple_num_args (a_stmt) != gimple_num_args (b_stmt))
    return false;

  /* At this point, a and b are known to be the same gimple operations.  */
  if (is_gimple_call (a_stmt))
    {
	if (!compatible_calls_p (dyn_cast <gcall *> (a_stmt),
				 dyn_cast <gcall *> (b_stmt)))
	  return false;
    }
  else if (!is_gimple_assign (a_stmt))
    return false;
  else
    {
      tree_code acode = gimple_assign_rhs_code (a_stmt);
      tree_code bcode = gimple_assign_rhs_code (b_stmt);
      if ((acode == REALPART_EXPR || acode == IMAGPART_EXPR)
	  && (bcode == REALPART_EXPR || bcode == IMAGPART_EXPR))
	return true;

      if (acode != bcode)
	return false;
    }

  if (!STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (a))
      || !STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (b)))
    {
      for (unsigned i = 0; i < gimple_num_args (a_stmt); i++)
	{
	  tree t1 = gimple_arg (a_stmt, i);
	  tree t2 = gimple_arg (b_stmt, i);
	  if (TREE_CODE (t1) != TREE_CODE (t2))
	    return false;

	  /* If SSA name then we will need to inspect the children
	     so we can punt here.  */
	  if (TREE_CODE (t1) == SSA_NAME)
	    continue;

	  if (!operand_equal_p (t1, t2, 0))
	    return false;
	}
    }
  else
    {
      auto dr1 = STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (a));
      auto dr2 = STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (b));
      /* Don't check the last dimension as that's checked by the lineary
	 checks.  This check is also much stricter than what we need
	 because it doesn't consider loading from adjacent elements
	 in the same struct as loading from the same base object.
	 But for now, I'll play it safe.  */
      if (!same_data_refs (dr1, dr2, 1))
	return false;
    }

  for (unsigned i = 0; i < SLP_TREE_CHILDREN (a).length (); i++)
    {
      if (!compatible_complex_nodes_p (compat_cache,
				       SLP_TREE_CHILDREN (a)[i], pa,
				       SLP_TREE_CHILDREN (b)[i], pb))
	return false;
    }

  compat_cache->put (key, true);
  return true;
}

static inline bool
vect_validate_multiplication (slp_tree_to_load_perm_map_t *perm_cache,
			      slp_compat_nodes_map_t *compat_cache,
			      vec<slp_tree> &left_op,
			      vec<slp_tree> &right_op,
			      bool subtract,
			      enum _conj_status *_status)
{
  auto_vec<slp_tree> ops;
  enum _conj_status stats = CONJ_NONE;

  /* The complex operations can occur in two layouts and two permute sequences
     so declare them and re-use them.  */
  int styles[][4] = { { 0, 2, 1, 3} /* {L1, R1} + {L2, R2}.  */
		    , { 0, 3, 1, 2} /* {L1, R2} + {L2, R1}.  */
		    };

  /* Now for the corresponding permutes that go with these values.  */
  complex_perm_kinds_t perms[][4]
    = { { PERM_EVENEVEN, PERM_ODDODD, PERM_EVENODD, PERM_ODDEVEN }
      , { PERM_EVENODD, PERM_ODDEVEN, PERM_EVENEVEN, PERM_ODDODD }
      };

  /* These permutes are used during comparisons of externals on which
     we require strict equality.  */
  int cq[][4][2]
    = { { { 0, 0 }, { 1, 1 }, { 0, 1 }, { 1, 0 } }
      , { { 0, 1 }, { 1, 0 }, { 0, 0 }, { 1, 1 } }
      };

  /* Default to style and perm 0, most operations use this one.  */
  int style = 0;
  int perm = subtract ? 1 : 0;

  /* Check if we have a negate operation, if so absorb the node and continue
     looking.  */
  bool neg0 = vect_match_expression_p (right_op[0], NEGATE_EXPR);
  bool neg1 = vect_match_expression_p (right_op[1], NEGATE_EXPR);

  /* Determine which style we're looking at.  We only have different ones
     whenever a conjugate is involved.  */
  if (neg0 && neg1)
    ;
  else if (neg0)
    {
      right_op[0] = SLP_TREE_CHILDREN (right_op[0])[0];
      stats = CONJ_FST;
      if (subtract)
	perm = 0;
    }
  else if (neg1)
    {
      right_op[1] = SLP_TREE_CHILDREN (right_op[1])[0];
      stats = CONJ_SND;
      perm = 1;
    }

  *_status = stats;

  /* Flatten the inputs after we've remapped them.  */
  ops.create (4);
  ops.safe_splice (left_op);
  ops.safe_splice (right_op);

  /* Extract out the elements to check.  */
  slp_tree op0 = ops[styles[style][0]];
  slp_tree op1 = ops[styles[style][1]];
  slp_tree op2 = ops[styles[style][2]];
  slp_tree op3 = ops[styles[style][3]];

  /* Do cheapest test first.  If failed no need to analyze further.  */
  if (linear_loads_p (perm_cache, op0) != perms[perm][0]
      || linear_loads_p (perm_cache, op1) != perms[perm][1]
      || !is_eq_or_top (perm_cache, op2, perms[perm][2], op3, perms[perm][3]))
    return false;

  return compatible_complex_nodes_p (compat_cache, op0, cq[perm][0], op1,
				     cq[perm][1])
	 && compatible_complex_nodes_p (compat_cache, op2, cq[perm][2], op3,
					cq[perm][3]);
}

/* This function combines two nodes containing only even and only odd lanes
   together into a single node which contains the nodes in even/odd order
   by using a lane permute.

   The lanes in EVEN and ODD are duplicated 2 times inside the vectors.
   So for a lanes = 4 EVEN contains {EVEN1, EVEN1, EVEN2, EVEN2}.

   The tree REPRESENTATION is taken from the supplied REP along with the
   vectype which must be the same between all three nodes.
*/

static slp_tree
vect_build_combine_node (slp_tree even, slp_tree odd, slp_tree rep)
{
  vec<std::pair<unsigned, unsigned> > perm;
  perm.create (SLP_TREE_LANES (rep));

  for (unsigned x = 0; x < SLP_TREE_LANES (rep); x+=2)
    {
      perm.quick_push (std::make_pair (0, x));
      perm.quick_push (std::make_pair (1, x+1));
    }

  slp_tree vnode = vect_create_new_slp_node (2, SLP_TREE_CODE (even));
  SLP_TREE_CODE (vnode) = VEC_PERM_EXPR;
  SLP_TREE_LANE_PERMUTATION (vnode) = perm;

  SLP_TREE_CHILDREN (vnode).create (2);
  SLP_TREE_CHILDREN (vnode).quick_push (even);
  SLP_TREE_CHILDREN (vnode).quick_push (odd);
  SLP_TREE_REF_COUNT (even)++;
  SLP_TREE_REF_COUNT (odd)++;
  SLP_TREE_REF_COUNT (vnode) = 1;

  SLP_TREE_LANES (vnode) = SLP_TREE_LANES (rep);
  gcc_assert (perm.length () == SLP_TREE_LANES (vnode));
  /* Representation is set to that of the current node as the vectorizer
     can't deal with VEC_PERMs with no representation, as would be the
     case with invariants.  */
  SLP_TREE_REPRESENTATIVE (vnode) = SLP_TREE_REPRESENTATIVE (rep);
  SLP_TREE_VECTYPE (vnode) = SLP_TREE_VECTYPE (rep);
  return vnode;
}

class complex_mul_pattern : public complex_pattern
{
  protected:
    complex_mul_pattern (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
      : complex_pattern (node, m_ops, ifn)
    {
      this->m_num_args = 2;
    }

  public:
    void build (vec_info *) final override;
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *,
	     slp_compat_nodes_map_t *, slp_tree *, vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_compat_nodes_map_t *,
	       slp_tree *);

    static vect_pattern*
    mkInstance (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
    {
      return new complex_mul_pattern (node, m_ops, ifn);
    }

};

/* Pattern matcher for trying to match complex multiply and complex multiply
   and accumulate pattern in SLP tree.  If the operation matches then IFN
   is set to the operation it matched and the arguments to the two
   replacement statements are put in m_ops.

   If no match is found then IFN is set to IFN_LAST and m_ops is unchanged.

   This function matches the patterns shaped as:

   double ax = (b[i+1] * a[i]);
   double bx = (a[i+1] * b[i]);

   c[i] = c[i] - ax;
   c[i+1] = c[i+1] + bx;

   If a match occurred then TRUE is returned, else FALSE.  The initial match is
   expected to be in OP1 and the initial match operands in args0.  */

internal_fn
complex_mul_pattern::matches (complex_operation_t op,
			      slp_tree_to_load_perm_map_t *perm_cache,
			      slp_compat_nodes_map_t *compat_cache,
			      slp_tree *node, vec<slp_tree> *ops)
{
  internal_fn ifn = IFN_LAST;

  if (op != MINUS_PLUS)
    return IFN_LAST;

  auto childs = *ops;
  auto l0node = SLP_TREE_CHILDREN (childs[0]);

  bool mul0 = vect_match_expression_p (l0node[0], MULT_EXPR);
  bool mul1 = vect_match_expression_p (l0node[1], MULT_EXPR);
  if (!mul0 && !mul1)
    return IFN_LAST;

  /* Now operand2+4 may lead to another expression.  */
  auto_vec<slp_tree> left_op, right_op;
  slp_tree add0 = NULL;

  /* Check if we may be a multiply add.  It's only valid to form FMAs
     with -ffp-contract=fast.  */
  if (!mul0
      && (flag_fp_contract_mode == FP_CONTRACT_FAST
	  || !FLOAT_TYPE_P (SLP_TREE_VECTYPE (*node)))
      && vect_match_expression_p (l0node[0], PLUS_EXPR))
    {
      auto vals = SLP_TREE_CHILDREN (l0node[0]);
      /* Check if it's a multiply, otherwise no idea what this is.  */
      if (!(mul0 = vect_match_expression_p (vals[1], MULT_EXPR)))
	return IFN_LAST;

      /* Check if the ADD is linear, otherwise it's not valid complex FMA.  */
      if (linear_loads_p (perm_cache, vals[0]) != PERM_EVENODD)
	return IFN_LAST;

      left_op.safe_splice (SLP_TREE_CHILDREN (vals[1]));
      add0 = vals[0];
    }
  else
    left_op.safe_splice (SLP_TREE_CHILDREN (l0node[0]));

  right_op.safe_splice (SLP_TREE_CHILDREN (l0node[1]));

  if (left_op.length () != 2
      || right_op.length () != 2
      || !mul0
      || !mul1
      || linear_loads_p (perm_cache, left_op[1]) == PERM_ODDEVEN)
    return IFN_LAST;

  enum _conj_status status;
  if (!vect_validate_multiplication (perm_cache, compat_cache, left_op,
				     right_op, false, &status))
    return IFN_LAST;

  if (status == CONJ_NONE)
    {
      if (add0)
	ifn = IFN_COMPLEX_FMA;
      else
	ifn = IFN_COMPLEX_MUL;
    }
  else
    {
      if(add0)
	ifn = IFN_COMPLEX_FMA_CONJ;
      else
	ifn = IFN_COMPLEX_MUL_CONJ;
    }

  if (!vect_pattern_validate_optab (ifn, *node))
    return IFN_LAST;

  ops->truncate (0);
  ops->create (add0 ? 4 : 3);

  if (add0)
    ops->quick_push (add0);

  complex_perm_kinds_t kind = linear_loads_p (perm_cache, left_op[0]);
  if (kind == PERM_EVENODD || kind == PERM_TOP)
    {
      ops->quick_push (left_op[1]);
      ops->quick_push (right_op[1]);
      ops->quick_push (left_op[0]);
    }
  else if (kind == PERM_EVENEVEN && status != CONJ_SND)
    {
      ops->quick_push (left_op[0]);
      ops->quick_push (right_op[0]);
      ops->quick_push (left_op[1]);
    }
  else
    {
      ops->quick_push (left_op[0]);
      ops->quick_push (right_op[1]);
      ops->quick_push (left_op[1]);
    }

  return ifn;
}

/* Attempt to recognize a complex mul pattern.  */

vect_pattern*
complex_mul_pattern::recognize (slp_tree_to_load_perm_map_t *perm_cache,
				slp_compat_nodes_map_t *compat_cache,
				slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn
    = complex_mul_pattern::matches (op, perm_cache, compat_cache, node, &ops);
  if (ifn == IFN_LAST)
    return NULL;

  return new complex_mul_pattern (node, &ops, ifn);
}

/* Perform a replacement of the detected complex mul pattern with the new
   instruction sequences.  */

void
complex_mul_pattern::build (vec_info *vinfo)
{
  slp_tree node;
  unsigned i;
  switch (this->m_ifn)
  {
    case IFN_COMPLEX_MUL:
    case IFN_COMPLEX_MUL_CONJ:
      {
	slp_tree newnode
	  = vect_build_combine_node (this->m_ops[0], this->m_ops[1],
				     *this->m_node);
	SLP_TREE_REF_COUNT (this->m_ops[2])++;

	FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (*this->m_node), i, node)
	  vect_free_slp_tree (node);

	/* First re-arrange the children.  */
	SLP_TREE_CHILDREN (*this->m_node).reserve_exact (2);
	SLP_TREE_CHILDREN (*this->m_node)[0] = this->m_ops[2];
	SLP_TREE_CHILDREN (*this->m_node)[1] = newnode;
	break;
      }
    case IFN_COMPLEX_FMA:
    case IFN_COMPLEX_FMA_CONJ:
      {
	SLP_TREE_REF_COUNT (this->m_ops[0])++;
	slp_tree newnode
	  = vect_build_combine_node (this->m_ops[1], this->m_ops[2],
				     *this->m_node);
	SLP_TREE_REF_COUNT (this->m_ops[3])++;

	FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (*this->m_node), i, node)
	  vect_free_slp_tree (node);

	/* First re-arrange the children.  */
	SLP_TREE_CHILDREN (*this->m_node).safe_grow (3);
	SLP_TREE_CHILDREN (*this->m_node)[0] = this->m_ops[3];
	SLP_TREE_CHILDREN (*this->m_node)[1] = newnode;
	SLP_TREE_CHILDREN (*this->m_node)[2] = this->m_ops[0];

	/* Tell the builder to expect an extra argument.  */
	this->m_num_args++;
	break;
      }
    default:
      gcc_unreachable ();
  }

  /* And then rewrite the node itself.  */
  complex_pattern::build (vinfo);
}

/*******************************************************************************
 * complex_fms_pattern class
 ******************************************************************************/

class complex_fms_pattern : public complex_pattern
{
  protected:
    complex_fms_pattern (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
      : complex_pattern (node, m_ops, ifn)
    {
      this->m_num_args = 3;
    }

  public:
    void build (vec_info *) final override;
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *,
	     slp_compat_nodes_map_t *, slp_tree *, vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_compat_nodes_map_t *,
	       slp_tree *);

    static vect_pattern*
    mkInstance (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
    {
      return new complex_fms_pattern (node, m_ops, ifn);
    }
};


/* Pattern matcher for trying to match complex multiply and subtract pattern
   in SLP tree.  If the operation matches then IFN is set to the operation
   it matched and the arguments to the two replacement statements are put in
   m_ops.

   If no match is found then IFN is set to IFN_LAST and m_ops is unchanged.

   This function matches the patterns shaped as:

   double ax = (b[i+1] * a[i]) + (b[i] * a[i]);
   double bx = (a[i+1] * b[i]) - (a[i+1] * b[i+1]);

   c[i] = c[i] - ax;
   c[i+1] = c[i+1] + bx;

   If a match occurred then TRUE is returned, else FALSE.  The initial match is
   expected to be in OP1 and the initial match operands in args0.  */

internal_fn
complex_fms_pattern::matches (complex_operation_t op,
			      slp_tree_to_load_perm_map_t *perm_cache,
			      slp_compat_nodes_map_t *compat_cache,
			      slp_tree * ref_node, vec<slp_tree> *ops)
{
  internal_fn ifn = IFN_LAST;

  /* We need to ignore the two_operands nodes that may also match,
     for that we can check if they have any scalar statements and also
     check that it's not a permute node as we're looking for a normal
     MINUS_EXPR operation.  */
  if (op != CMPLX_NONE)
    return IFN_LAST;

  slp_tree root = *ref_node;
  if (!vect_match_expression_p (root, MINUS_EXPR))
    return IFN_LAST;

  /* TODO: Support invariants here, with the new layout CADD now
	   can match before we get a chance to try CFMS.  */
  auto nodes = SLP_TREE_CHILDREN (root);
  if (!vect_match_expression_p (nodes[1], MULT_EXPR)
      || vect_detect_pair_op (nodes[0]) != PLUS_MINUS)
    return IFN_LAST;

  auto childs = SLP_TREE_CHILDREN (nodes[0]);
  auto l0node = SLP_TREE_CHILDREN (childs[0]);

  /* Now operand2+4 may lead to another expression.  */
  auto_vec<slp_tree> left_op, right_op;
  left_op.safe_splice (SLP_TREE_CHILDREN (l0node[1]));
  right_op.safe_splice (SLP_TREE_CHILDREN (nodes[1]));

  /* If these nodes don't have any children then they're
     not ones we're interested in.  */
  if (left_op.length () != 2
      || right_op.length () != 2
      || !vect_match_expression_p (l0node[1], MULT_EXPR))
    return IFN_LAST;

  enum _conj_status status;
  if (!vect_validate_multiplication (perm_cache, compat_cache, right_op,
				     left_op, true, &status))
    return IFN_LAST;

  if (status == CONJ_NONE)
    ifn = IFN_COMPLEX_FMS;
  else
    ifn = IFN_COMPLEX_FMS_CONJ;

  if (!vect_pattern_validate_optab (ifn, *ref_node))
    return IFN_LAST;

  ops->truncate (0);
  ops->create (4);

  complex_perm_kinds_t kind = linear_loads_p (perm_cache, right_op[0]);
  if (kind == PERM_EVENODD)
    {
      ops->quick_push (l0node[0]);
      ops->quick_push (right_op[0]);
      ops->quick_push (right_op[1]);
      ops->quick_push (left_op[1]);
    }
  else
    {
      ops->quick_push (l0node[0]);
      ops->quick_push (right_op[1]);
      ops->quick_push (right_op[0]);
      ops->quick_push (left_op[0]);
    }

  return ifn;
}

/* Attempt to recognize a complex mul pattern.  */

vect_pattern*
complex_fms_pattern::recognize (slp_tree_to_load_perm_map_t *perm_cache,
				slp_compat_nodes_map_t *compat_cache,
				slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn
    = complex_fms_pattern::matches (op, perm_cache, compat_cache, node, &ops);
  if (ifn == IFN_LAST)
    return NULL;

  return new complex_fms_pattern (node, &ops, ifn);
}

/* Perform a replacement of the detected complex mul pattern with the new
   instruction sequences.  */

void
complex_fms_pattern::build (vec_info *vinfo)
{
  slp_tree node;
  unsigned i;
  slp_tree newnode =
    vect_build_combine_node (this->m_ops[2], this->m_ops[3], *this->m_node);
  SLP_TREE_REF_COUNT (this->m_ops[0])++;
  SLP_TREE_REF_COUNT (this->m_ops[1])++;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (*this->m_node), i, node)
    vect_free_slp_tree (node);

  SLP_TREE_CHILDREN (*this->m_node).release ();
  SLP_TREE_CHILDREN (*this->m_node).create (3);

  /* First re-arrange the children.  */
  SLP_TREE_CHILDREN (*this->m_node).quick_push (this->m_ops[1]);
  SLP_TREE_CHILDREN (*this->m_node).quick_push (newnode);
  SLP_TREE_CHILDREN (*this->m_node).quick_push (this->m_ops[0]);

  /* And then rewrite the node itself.  */
  complex_pattern::build (vinfo);
}

/*******************************************************************************
 * complex_operations_pattern class
 ******************************************************************************/

/* This function combines all the existing pattern matchers above into one class
   that shares the functionality between them.  The initial match is shared
   between all complex operations.  */

class complex_operations_pattern : public complex_pattern
{
  protected:
    complex_operations_pattern (slp_tree *node, vec<slp_tree> *m_ops,
				internal_fn ifn)
      : complex_pattern (node, m_ops, ifn)
    {
      this->m_num_args = 0;
    }

  public:
    void build (vec_info *) final override;
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *,
	     slp_compat_nodes_map_t *, slp_tree *, vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_compat_nodes_map_t *,
	       slp_tree *);
};

/* Dummy matches implementation for proxy object.  */

internal_fn
complex_operations_pattern::
matches (complex_operation_t /* op */,
	 slp_tree_to_load_perm_map_t * /* perm_cache */,
	 slp_compat_nodes_map_t * /* compat_cache */,
	 slp_tree * /* ref_node */, vec<slp_tree> * /* ops */)
{
  return IFN_LAST;
}

/* Attempt to recognize a complex mul pattern.  */

vect_pattern*
complex_operations_pattern::recognize (slp_tree_to_load_perm_map_t *perm_cache,
				       slp_compat_nodes_map_t *ccache,
				       slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn = IFN_LAST;

  ifn  = complex_fms_pattern::matches (op, perm_cache, ccache, node, &ops);
  if (ifn != IFN_LAST)
    return complex_fms_pattern::mkInstance (node, &ops, ifn);

  ifn  = complex_mul_pattern::matches (op, perm_cache, ccache, node, &ops);
  if (ifn != IFN_LAST)
    return complex_mul_pattern::mkInstance (node, &ops, ifn);

  ifn  = complex_add_pattern::matches (op, perm_cache, ccache, node, &ops);
  if (ifn != IFN_LAST)
    return complex_add_pattern::mkInstance (node, &ops, ifn);

  return NULL;
}

/* Dummy implementation of build.  */

void
complex_operations_pattern::build (vec_info * /* vinfo */)
{
  gcc_unreachable ();
}


/* The addsub_pattern.  */

class addsub_pattern : public vect_pattern
{
  public:
    addsub_pattern (slp_tree *node, internal_fn ifn)
	: vect_pattern (node, NULL, ifn) {};

    void build (vec_info *) final override;

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_compat_nodes_map_t *,
	       slp_tree *);
};

vect_pattern *
addsub_pattern::recognize (slp_tree_to_load_perm_map_t *,
			   slp_compat_nodes_map_t *, slp_tree *node_)
{
  slp_tree node = *node_;
  if (SLP_TREE_CODE (node) != VEC_PERM_EXPR
      || SLP_TREE_CHILDREN (node).length () != 2
      || SLP_TREE_LANE_PERMUTATION (node).length () % 2)
    return NULL;

  /* Match a blend of a plus and a minus op with the same number of plus and
     minus lanes on the same operands.  */
  unsigned l0 = SLP_TREE_LANE_PERMUTATION (node)[0].first;
  unsigned l1 = SLP_TREE_LANE_PERMUTATION (node)[1].first;
  if (l0 == l1)
    return NULL;
  bool l0add_p = vect_match_expression_p (SLP_TREE_CHILDREN (node)[l0],
					  PLUS_EXPR);
  if (!l0add_p
      && !vect_match_expression_p (SLP_TREE_CHILDREN (node)[l0], MINUS_EXPR))
    return NULL;
  bool l1add_p = vect_match_expression_p (SLP_TREE_CHILDREN (node)[l1],
					  PLUS_EXPR);
  if (!l1add_p
      && !vect_match_expression_p (SLP_TREE_CHILDREN (node)[l1], MINUS_EXPR))
    return NULL;

  slp_tree l0node = SLP_TREE_CHILDREN (node)[l0];
  slp_tree l1node = SLP_TREE_CHILDREN (node)[l1];
  if (!((SLP_TREE_CHILDREN (l0node)[0] == SLP_TREE_CHILDREN (l1node)[0]
	 && SLP_TREE_CHILDREN (l0node)[1] == SLP_TREE_CHILDREN (l1node)[1])
	|| (SLP_TREE_CHILDREN (l0node)[0] == SLP_TREE_CHILDREN (l1node)[1]
	    && SLP_TREE_CHILDREN (l0node)[1] == SLP_TREE_CHILDREN (l1node)[0])))
    return NULL;

  for (unsigned i = 0; i < SLP_TREE_LANE_PERMUTATION (node).length (); ++i)
    {
      std::pair<unsigned, unsigned> perm = SLP_TREE_LANE_PERMUTATION (node)[i];
      /* It has to be alternating -, +, -,
	 While we could permute the .ADDSUB inputs and the .ADDSUB output
	 that's only profitable over the add + sub + blend if at least
	 one of the permute is optimized which we can't determine here.  */
      if (perm.first != ((i & 1) ? l1 : l0)
	  || perm.second != i)
	return NULL;
    }

  /* Now we have either { -, +, -, + ... } (!l0add_p) or { +, -, +, - ... }
     (l0add_p), see whether we have FMA variants.  We can only form FMAs
     if allowed via -ffp-contract=fast.  */
  if (flag_fp_contract_mode != FP_CONTRACT_FAST
      && FLOAT_TYPE_P (SLP_TREE_VECTYPE (l0node)))
    ;
  else if (!l0add_p
	   && vect_match_expression_p (SLP_TREE_CHILDREN (l0node)[0], MULT_EXPR))
    {
      /* (c * d) -+ a */
      if (vect_pattern_validate_optab (IFN_VEC_FMADDSUB, node))
	return new addsub_pattern (node_, IFN_VEC_FMADDSUB);
    }
  else if (l0add_p
	   && vect_match_expression_p (SLP_TREE_CHILDREN (l1node)[0], MULT_EXPR))
    {
      /* (c * d) +- a */
      if (vect_pattern_validate_optab (IFN_VEC_FMSUBADD, node))
	return new addsub_pattern (node_, IFN_VEC_FMSUBADD);
    }

  if (!l0add_p && vect_pattern_validate_optab (IFN_VEC_ADDSUB, node))
    return new addsub_pattern (node_, IFN_VEC_ADDSUB);

  return NULL;
}

void
addsub_pattern::build (vec_info *vinfo)
{
  slp_tree node = *m_node;

  unsigned l0 = SLP_TREE_LANE_PERMUTATION (node)[0].first;
  unsigned l1 = SLP_TREE_LANE_PERMUTATION (node)[1].first;

  switch (m_ifn)
    {
    case IFN_VEC_ADDSUB:
      {
	slp_tree sub = SLP_TREE_CHILDREN (node)[l0];
	slp_tree add = SLP_TREE_CHILDREN (node)[l1];

	/* Modify the blend node in-place.  */
	SLP_TREE_CHILDREN (node)[0] = SLP_TREE_CHILDREN (sub)[0];
	SLP_TREE_CHILDREN (node)[1] = SLP_TREE_CHILDREN (sub)[1];
	SLP_TREE_REF_COUNT (SLP_TREE_CHILDREN (node)[0])++;
	SLP_TREE_REF_COUNT (SLP_TREE_CHILDREN (node)[1])++;

	/* Build IFN_VEC_ADDSUB from the sub representative operands.  */
	stmt_vec_info rep = SLP_TREE_REPRESENTATIVE (sub);
	gcall *call = gimple_build_call_internal (IFN_VEC_ADDSUB, 2,
						  gimple_assign_rhs1 (rep->stmt),
						  gimple_assign_rhs2 (rep->stmt));
	gimple_call_set_lhs (call, make_ssa_name
			     (TREE_TYPE (gimple_assign_lhs (rep->stmt))));
	gimple_call_set_nothrow (call, true);
	gimple_set_bb (call, gimple_bb (rep->stmt));
	stmt_vec_info new_rep = vinfo->add_pattern_stmt (call, rep);
	SLP_TREE_REPRESENTATIVE (node) = new_rep;
	STMT_VINFO_RELEVANT (new_rep) = vect_used_in_scope;
	STMT_SLP_TYPE (new_rep) = pure_slp;
	STMT_VINFO_VECTYPE (new_rep) = SLP_TREE_VECTYPE (node);
	STMT_VINFO_SLP_VECT_ONLY_PATTERN (new_rep) = true;
	STMT_VINFO_REDUC_DEF (new_rep) = STMT_VINFO_REDUC_DEF (vect_orig_stmt (rep));
	SLP_TREE_CODE (node) = ERROR_MARK;
	SLP_TREE_LANE_PERMUTATION (node).release ();

	vect_free_slp_tree (sub);
	vect_free_slp_tree (add);
	break;
      }
    case IFN_VEC_FMADDSUB:
    case IFN_VEC_FMSUBADD:
      {
	slp_tree sub, add;
	if (m_ifn == IFN_VEC_FMADDSUB)
	  {
	    sub = SLP_TREE_CHILDREN (node)[l0];
	    add = SLP_TREE_CHILDREN (node)[l1];
	  }
	else /* m_ifn == IFN_VEC_FMSUBADD */
	  {
	    sub = SLP_TREE_CHILDREN (node)[l1];
	    add = SLP_TREE_CHILDREN (node)[l0];
	  }
	slp_tree mul = SLP_TREE_CHILDREN (sub)[0];
	/* Modify the blend node in-place.  */
	SLP_TREE_CHILDREN (node).safe_grow (3, true);
	SLP_TREE_CHILDREN (node)[0] = SLP_TREE_CHILDREN (mul)[0];
	SLP_TREE_CHILDREN (node)[1] = SLP_TREE_CHILDREN (mul)[1];
	SLP_TREE_CHILDREN (node)[2] = SLP_TREE_CHILDREN (sub)[1];
	SLP_TREE_REF_COUNT (SLP_TREE_CHILDREN (node)[0])++;
	SLP_TREE_REF_COUNT (SLP_TREE_CHILDREN (node)[1])++;
	SLP_TREE_REF_COUNT (SLP_TREE_CHILDREN (node)[2])++;

	/* Build IFN_VEC_FMADDSUB from the mul/sub representative operands.  */
	stmt_vec_info srep = SLP_TREE_REPRESENTATIVE (sub);
	stmt_vec_info mrep = SLP_TREE_REPRESENTATIVE (mul);
	gcall *call = gimple_build_call_internal (m_ifn, 3,
						  gimple_assign_rhs1 (mrep->stmt),
						  gimple_assign_rhs2 (mrep->stmt),
						  gimple_assign_rhs2 (srep->stmt));
	gimple_call_set_lhs (call, make_ssa_name
			     (TREE_TYPE (gimple_assign_lhs (srep->stmt))));
	gimple_call_set_nothrow (call, true);
	gimple_set_bb (call, gimple_bb (srep->stmt));
	stmt_vec_info new_rep = vinfo->add_pattern_stmt (call, srep);
	SLP_TREE_REPRESENTATIVE (node) = new_rep;
	STMT_VINFO_RELEVANT (new_rep) = vect_used_in_scope;
	STMT_SLP_TYPE (new_rep) = pure_slp;
	STMT_VINFO_VECTYPE (new_rep) = SLP_TREE_VECTYPE (node);
	STMT_VINFO_SLP_VECT_ONLY_PATTERN (new_rep) = true;
	STMT_VINFO_REDUC_DEF (new_rep) = STMT_VINFO_REDUC_DEF (vect_orig_stmt (srep));
	SLP_TREE_CODE (node) = ERROR_MARK;
	SLP_TREE_LANE_PERMUTATION (node).release ();

	vect_free_slp_tree (sub);
	vect_free_slp_tree (add);
	break;
      }
    default:;
    }
}

/*******************************************************************************
 * Pattern matching definitions
 ******************************************************************************/

#define SLP_PATTERN(x) &x::recognize
vect_pattern_decl_t slp_patterns[]
{
  /* For least amount of back-tracking and more efficient matching
     order patterns from the largest to the smallest.  Especially if they
     overlap in what they can detect.  */

  SLP_PATTERN (complex_operations_pattern),
  SLP_PATTERN (addsub_pattern)
};
#undef SLP_PATTERN

/* Set the number of SLP pattern matchers available.  */
size_t num__slp_patterns = ARRAY_SIZE (slp_patterns);
