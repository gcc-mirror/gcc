/* SLP - Pattern matcher on SLP trees
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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
			     "Target does not support vector type for %T\n",
			     SLP_TREE_DEF_TYPE (node));
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
      if (candidates[0] != PERM_UNKNOWN && load != 1)
	{
	  candidates[0] = PERM_UNKNOWN;
	  valid_patterns--;
	}
      if (candidates[1] != PERM_UNKNOWN && load != 0)
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
  if (SLP_TREE_LOAD_PERMUTATION (root).exists ())
    {
      retval = is_linear_load_p (SLP_TREE_LOAD_PERMUTATION (root));
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

/* Checks to see if the expression represented by NODE is a call to the internal
   function FN.  */

static inline bool
vect_match_call_p (slp_tree node, internal_fn fn)
{
  if (!node
      || !SLP_TREE_REPRESENTATIVE (node))
    return false;

  gimple* expr = STMT_VINFO_STMT (SLP_TREE_REPRESENTATIVE (node));
  if (!expr
      || !gimple_call_internal_p (expr, fn))
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
    void build (vec_info *);

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
    void build (vec_info *);
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *, slp_tree *,
	     vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_tree *);

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
				slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn
    = complex_add_pattern::matches (op, perm_cache, node, &ops);
  if (ifn == IFN_LAST)
    return NULL;

  return new complex_add_pattern (node, &ops, ifn);
}

/*******************************************************************************
 * complex_mul_pattern
 ******************************************************************************/

/* Helper function of that looks for a match in the CHILDth child of NODE.  The
   child used is stored in RES.

   If the match is successful then ARGS will contain the operands matched
   and the complex_operation_t type is returned.  If match is not successful
   then CMPLX_NONE is returned and ARGS is left unmodified.  */

static inline complex_operation_t
vect_match_call_complex_mla (slp_tree node, unsigned child,
			     vec<slp_tree> *args = NULL, slp_tree *res = NULL)
{
  gcc_assert (child < SLP_TREE_CHILDREN (node).length ());

  slp_tree data = SLP_TREE_CHILDREN (node)[child];

  if (res)
    *res = data;

  return vect_detect_pair_op (data, false, args);
}

/* Check to see if either of the trees in ARGS are a NEGATE_EXPR.  If the first
   child (args[0]) is a NEGATE_EXPR then NEG_FIRST_P is set to TRUE.

   If a negate is found then the values in ARGS are reordered such that the
   negate node is always the second one and the entry is replaced by the child
   of the negate node.  */

static inline bool
vect_normalize_conj_loc (vec<slp_tree> &args, bool *neg_first_p = NULL)
{
  gcc_assert (args.length () == 2);
  bool neg_found = false;

  if (vect_match_expression_p (args[0], NEGATE_EXPR))
    {
      std::swap (args[0], args[1]);
      neg_found = true;
      if (neg_first_p)
	*neg_first_p = true;
    }
  else if (vect_match_expression_p (args[1], NEGATE_EXPR))
    {
      neg_found = true;
      if (neg_first_p)
	*neg_first_p = false;
    }

  if (neg_found)
    args[1] = SLP_TREE_CHILDREN (args[1])[0];

  return neg_found;
}

/* Helper function to check if PERM is KIND or PERM_TOP.  */

static inline bool
is_eq_or_top (complex_perm_kinds_t perm, complex_perm_kinds_t kind)
{
  return perm == kind || perm == PERM_TOP;
}

/* Helper function that checks to see if LEFT_OP and RIGHT_OP are both MULT_EXPR
   nodes but also that they represent an operation that is either a complex
   multiplication or a complex multiplication by conjugated value.

   Of the negation is expected to be in the first half of the tree (As required
   by an FMS pattern) then NEG_FIRST is true.  If the operation is a conjugate
   operation then CONJ_FIRST_OPERAND is set to indicate whether the first or
   second operand contains the conjugate operation.  */

static inline bool
vect_validate_multiplication (slp_tree_to_load_perm_map_t *perm_cache,
			      const vec<slp_tree> &left_op,
			      const vec<slp_tree> &right_op,
			     bool neg_first, bool *conj_first_operand,
			     bool fms)
{
  /* The presence of a negation indicates that we have either a conjugate or a
     rotation.  We need to distinguish which one.  */
  *conj_first_operand = false;
  complex_perm_kinds_t kind;

  /* Complex conjugates have the negation on the imaginary part of the
     number where rotations affect the real component.  So check if the
     negation is on a dup of lane 1.  */
  if (fms)
    {
      /* Canonicalization for fms is not consistent. So have to test both
	 variants to be sure.  This needs to be fixed in the mid-end so
	 this part can be simpler.  */
      kind = linear_loads_p (perm_cache, right_op[0]);
      if (!((is_eq_or_top (linear_loads_p (perm_cache, right_op[0]), PERM_ODDODD)
	   && is_eq_or_top (linear_loads_p (perm_cache, right_op[1]),
			     PERM_ODDEVEN))
	  || (kind == PERM_ODDEVEN
	      && is_eq_or_top (linear_loads_p (perm_cache, right_op[1]),
			     PERM_ODDODD))))
	return false;
    }
  else
    {
      if (linear_loads_p (perm_cache, right_op[1]) != PERM_ODDODD
	  && !is_eq_or_top (linear_loads_p (perm_cache, right_op[0]),
			    PERM_ODDEVEN))
	return false;
    }

  /* Deal with differences in indexes.  */
  int index1 = fms ? 1 : 0;
  int index2 = fms ? 0 : 1;

  /* Check if the conjugate is on the second first or second operand.  The
     order of the node with the conjugate value determines this, and the dup
     node must be one of lane 0 of the same DR as the neg node.  */
  kind = linear_loads_p (perm_cache, left_op[index1]);
  if (kind == PERM_TOP)
    {
      if (linear_loads_p (perm_cache, left_op[index2]) == PERM_EVENODD)
	return true;
    }
  else if (kind == PERM_EVENODD)
    {
      if ((kind = linear_loads_p (perm_cache, left_op[index2])) == PERM_EVENODD)
	return false;
      return true;
    }
  else if (!neg_first)
    *conj_first_operand = true;
  else
    return false;

  if (kind != PERM_EVENEVEN)
    return false;

  return true;
}

/* Helper function to help distinguish between a conjugate and a rotation in a
   complex multiplication.  The operations have similar shapes but the order of
   the load permutes are different.  This function returns TRUE when the order
   is consistent with a multiplication or multiplication by conjugated
   operand but returns FALSE if it's a multiplication by rotated operand.  */

static inline bool
vect_validate_multiplication (slp_tree_to_load_perm_map_t *perm_cache,
			      const vec<slp_tree> &op,
			      complex_perm_kinds_t permKind)
{
  /* The left node is the more common case, test it first.  */
  if (!is_eq_or_top (linear_loads_p (perm_cache, op[0]), permKind))
    {
      if (!is_eq_or_top (linear_loads_p (perm_cache, op[1]), permKind))
	return false;
    }
  return true;
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
    void build (vec_info *);
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *, slp_tree *,
	     vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_tree *);

    static vect_pattern*
    mkInstance (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
    {
      return new complex_mul_pattern (node, m_ops, ifn);
    }

};

/* Pattern matcher for trying to match complex multiply pattern in SLP tree
   If the operation matches then IFN is set to the operation it matched
   and the arguments to the two replacement statements are put in m_ops.

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
			      slp_tree *node, vec<slp_tree> *ops)
{
  internal_fn ifn = IFN_LAST;

  if (op != MINUS_PLUS)
    return IFN_LAST;

  slp_tree root = *node;
  /* First two nodes must be a multiply.  */
  auto_vec<slp_tree> muls;
  if (vect_match_call_complex_mla (root, 0) != MULT_MULT
      || vect_match_call_complex_mla (root, 1, &muls) != MULT_MULT)
    return IFN_LAST;

  /* Now operand2+4 may lead to another expression.  */
  auto_vec<slp_tree> left_op, right_op;
  left_op.safe_splice (SLP_TREE_CHILDREN (muls[0]));
  right_op.safe_splice (SLP_TREE_CHILDREN (muls[1]));

  if (linear_loads_p (perm_cache, left_op[1]) == PERM_ODDEVEN)
    return IFN_LAST;

  bool neg_first = false;
  bool conj_first_operand = false;
  bool is_neg = vect_normalize_conj_loc (right_op, &neg_first);

  if (!is_neg)
    {
      /* A multiplication needs to multiply agains the real pair, otherwise
	 the pattern matches that of FMS.   */
      if (!vect_validate_multiplication (perm_cache, left_op, PERM_EVENEVEN)
	  || vect_normalize_conj_loc (left_op))
	return IFN_LAST;
      ifn = IFN_COMPLEX_MUL;
    }
  else if (is_neg)
    {
      if (!vect_validate_multiplication (perm_cache, left_op, right_op,
					 neg_first, &conj_first_operand,
					 false))
	return IFN_LAST;

      ifn = IFN_COMPLEX_MUL_CONJ;
    }

  if (!vect_pattern_validate_optab (ifn, *node))
    return IFN_LAST;

  ops->truncate (0);
  ops->create (3);

  complex_perm_kinds_t kind = linear_loads_p (perm_cache, left_op[0]);
  if (kind == PERM_EVENODD)
    {
      ops->quick_push (left_op[1]);
      ops->quick_push (right_op[1]);
      ops->quick_push (left_op[0]);
    }
  else if (kind == PERM_TOP)
    {
      ops->quick_push (left_op[1]);
      ops->quick_push (right_op[1]);
      ops->quick_push (left_op[0]);
    }
  else if (kind == PERM_EVENEVEN && !conj_first_operand)
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
				slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn
    = complex_mul_pattern::matches (op, perm_cache, node, &ops);
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
  slp_tree newnode
    = vect_build_combine_node (this->m_ops[0], this->m_ops[1], *this->m_node);
  SLP_TREE_REF_COUNT (this->m_ops[2])++;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (*this->m_node), i, node)
    vect_free_slp_tree (node);

  /* First re-arrange the children.  */
  SLP_TREE_CHILDREN (*this->m_node).reserve_exact (2);
  SLP_TREE_CHILDREN (*this->m_node)[0] = this->m_ops[2];
  SLP_TREE_CHILDREN (*this->m_node)[1] = newnode;

  /* And then rewrite the node itself.  */
  complex_pattern::build (vinfo);
}

/*******************************************************************************
 * complex_fma_pattern class
 ******************************************************************************/

class complex_fma_pattern : public complex_pattern
{
  protected:
    complex_fma_pattern (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
      : complex_pattern (node, m_ops, ifn)
    {
      this->m_num_args = 3;
    }

  public:
    void build (vec_info *);
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *, slp_tree *,
	     vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_tree *);

    static vect_pattern*
    mkInstance (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
    {
      return new complex_fma_pattern (node, m_ops, ifn);
    }
};

/* Pattern matcher for trying to match complex multiply and accumulate
   and multiply and subtract patterns in SLP tree.
   If the operation matches then IFN is set to the operation it matched and
   the arguments to the two replacement statements are put in m_ops.

   If no match is found then IFN is set to IFN_LAST and m_ops is unchanged.

   This function matches the patterns shaped as:

   double ax = (b[i+1] * a[i]) + (b[i] * a[i]);
   double bx = (a[i+1] * b[i]) - (a[i+1] * b[i+1]);

   c[i] = c[i] - ax;
   c[i+1] = c[i+1] + bx;

   If a match occurred then TRUE is returned, else FALSE.  The match is
   performed after COMPLEX_MUL which would have done the majority of the work.
   This function merely matches an ADD with a COMPLEX_MUL IFN.  The initial
   match is expected to be in OP1 and the initial match operands in args0.  */

internal_fn
complex_fma_pattern::matches (complex_operation_t op,
			      slp_tree_to_load_perm_map_t * /* perm_cache */,
			      slp_tree *ref_node, vec<slp_tree> *ops)
{
  internal_fn ifn = IFN_LAST;

  /* Find the two components.  We match Complex MUL first which reduces the
     amount of work this pattern has to do.  After that we just match the
     head node and we're done.:

     * FMA: + +.

     We need to ignore the two_operands nodes that may also match.
     For that we can check if they have any scalar statements and also
     check that it's not a permute node as we're looking for a normal
     PLUS_EXPR operation.  */
  if (op != CMPLX_NONE)
    return IFN_LAST;

  /* Find the two components.  We match Complex MUL first which reduces the
     amount of work this pattern has to do.  After that we just match the
     head node and we're done.:

   * FMA: + + on a non-two_operands node.  */
  slp_tree vnode = *ref_node;
  if (SLP_TREE_LANE_PERMUTATION (vnode).exists ()
      || !SLP_TREE_CHILDREN (vnode).exists ()
      || !vect_match_expression_p (vnode, PLUS_EXPR))
    return IFN_LAST;

  slp_tree node = SLP_TREE_CHILDREN (vnode)[1];

  if (vect_match_call_p (node, IFN_COMPLEX_MUL))
    ifn = IFN_COMPLEX_FMA;
  else if (vect_match_call_p (node, IFN_COMPLEX_MUL_CONJ))
    ifn = IFN_COMPLEX_FMA_CONJ;
  else
    return IFN_LAST;

  if (!vect_pattern_validate_optab (ifn, vnode))
    return IFN_LAST;

  ops->truncate (0);
  ops->create (3);

  if (ifn == IFN_COMPLEX_FMA)
    {
      ops->quick_push (SLP_TREE_CHILDREN (vnode)[0]);
      ops->quick_push (SLP_TREE_CHILDREN (node)[1]);
      ops->quick_push (SLP_TREE_CHILDREN (node)[0]);
    }
  else
    {
      ops->quick_push (SLP_TREE_CHILDREN (vnode)[0]);
      ops->quick_push (SLP_TREE_CHILDREN (node)[0]);
      ops->quick_push (SLP_TREE_CHILDREN (node)[1]);
    }

  return ifn;
}

/* Attempt to recognize a complex mul pattern.  */

vect_pattern*
complex_fma_pattern::recognize (slp_tree_to_load_perm_map_t *perm_cache,
				slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn
    = complex_fma_pattern::matches (op, perm_cache, node, &ops);
  if (ifn == IFN_LAST)
    return NULL;

  return new complex_fma_pattern (node, &ops, ifn);
}

/* Perform a replacement of the detected complex mul pattern with the new
   instruction sequences.  */

void
complex_fma_pattern::build (vec_info *vinfo)
{
  slp_tree node = SLP_TREE_CHILDREN (*this->m_node)[1];

  SLP_TREE_CHILDREN (*this->m_node).release ();
  SLP_TREE_CHILDREN (*this->m_node).create (3);
  SLP_TREE_CHILDREN (*this->m_node).safe_splice (this->m_ops);

  SLP_TREE_REF_COUNT (this->m_ops[1])++;
  SLP_TREE_REF_COUNT (this->m_ops[2])++;

  vect_free_slp_tree (node);

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
    void build (vec_info *);
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *, slp_tree *,
	     vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_tree *);

    static vect_pattern*
    mkInstance (slp_tree *node, vec<slp_tree> *m_ops, internal_fn ifn)
    {
      return new complex_fms_pattern (node, m_ops, ifn);
    }
};


/* Pattern matcher for trying to match complex multiply and accumulate
   and multiply and subtract patterns in SLP tree.
   If the operation matches then IFN is set to the operation it matched and
   the arguments to the two replacement statements are put in m_ops.

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
			      slp_tree * ref_node, vec<slp_tree> *ops)
{
  internal_fn ifn = IFN_LAST;

  /* Find the two components.  We match Complex MUL first which reduces the
     amount of work this pattern has to do.  After that we just match the
     head node and we're done.:

     * FMS: - +.  */
  slp_tree child = NULL;

  /* We need to ignore the two_operands nodes that may also match,
     for that we can check if they have any scalar statements and also
     check that it's not a permute node as we're looking for a normal
     PLUS_EXPR operation.  */
  if (op != PLUS_MINUS)
    return IFN_LAST;

  child = SLP_TREE_CHILDREN ((*ops)[1])[1];
  if (vect_detect_pair_op (child) != MINUS_PLUS)
    return IFN_LAST;

  /* First two nodes must be a multiply.  */
  auto_vec<slp_tree> muls;
  if (vect_match_call_complex_mla (child, 0) != MULT_MULT
      || vect_match_call_complex_mla (child, 1, &muls) != MULT_MULT)
    return IFN_LAST;

  /* Now operand2+4 may lead to another expression.  */
  auto_vec<slp_tree> left_op, right_op;
  left_op.safe_splice (SLP_TREE_CHILDREN (muls[0]));
  right_op.safe_splice (SLP_TREE_CHILDREN (muls[1]));

  bool is_neg = vect_normalize_conj_loc (left_op);

  child = SLP_TREE_CHILDREN ((*ops)[1])[0];
  bool conj_first_operand = false;
  if (!vect_validate_multiplication (perm_cache, right_op, left_op, false,
				     &conj_first_operand, true))
    return IFN_LAST;

  if (!is_neg)
    ifn = IFN_COMPLEX_FMS;
  else if (is_neg)
    ifn = IFN_COMPLEX_FMS_CONJ;

  if (!vect_pattern_validate_optab (ifn, *ref_node))
    return IFN_LAST;

  ops->truncate (0);
  ops->create (4);

  complex_perm_kinds_t kind = linear_loads_p (perm_cache, right_op[0]);
  if (kind == PERM_EVENODD)
    {
      ops->quick_push (child);
      ops->quick_push (right_op[0]);
      ops->quick_push (right_op[1]);
      ops->quick_push (left_op[1]);
    }
  else if (kind == PERM_TOP)
    {
      ops->quick_push (child);
      ops->quick_push (right_op[1]);
      ops->quick_push (right_op[0]);
      ops->quick_push (left_op[0]);
    }
  else if (kind == PERM_EVENEVEN && !is_neg)
    {
      ops->quick_push (child);
      ops->quick_push (right_op[1]);
      ops->quick_push (right_op[0]);
      ops->quick_push (left_op[0]);
    }
  else
    {
      ops->quick_push (child);
      ops->quick_push (right_op[1]);
      ops->quick_push (right_op[0]);
      ops->quick_push (left_op[1]);
    }

  return ifn;
}

/* Attempt to recognize a complex mul pattern.  */

vect_pattern*
complex_fms_pattern::recognize (slp_tree_to_load_perm_map_t *perm_cache,
				slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn
    = complex_fms_pattern::matches (op, perm_cache, node, &ops);
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
  SLP_TREE_CHILDREN (*this->m_node).quick_push (this->m_ops[0]);
  SLP_TREE_CHILDREN (*this->m_node).quick_push (this->m_ops[1]);
  SLP_TREE_CHILDREN (*this->m_node).quick_push (newnode);

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
    void build (vec_info *);
    static internal_fn
    matches (complex_operation_t op, slp_tree_to_load_perm_map_t *, slp_tree *,
	     vec<slp_tree> *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_tree *);
};

/* Dummy matches implementation for proxy object.  */

internal_fn
complex_operations_pattern::
matches (complex_operation_t /* op */,
	 slp_tree_to_load_perm_map_t * /* perm_cache */,
	 slp_tree * /* ref_node */, vec<slp_tree> * /* ops */)
{
  return IFN_LAST;
}

/* Attempt to recognize a complex mul pattern.  */

vect_pattern*
complex_operations_pattern::recognize (slp_tree_to_load_perm_map_t *perm_cache,
				       slp_tree *node)
{
  auto_vec<slp_tree> ops;
  complex_operation_t op
    = vect_detect_pair_op (*node, true, &ops);
  internal_fn ifn = IFN_LAST;

  ifn  = complex_fms_pattern::matches (op, perm_cache, node, &ops);
  if (ifn != IFN_LAST)
    return complex_fms_pattern::mkInstance (node, &ops, ifn);

  ifn  = complex_mul_pattern::matches (op, perm_cache, node, &ops);
  if (ifn != IFN_LAST)
    return complex_mul_pattern::mkInstance (node, &ops, ifn);

  ifn  = complex_fma_pattern::matches (op, perm_cache, node, &ops);
  if (ifn != IFN_LAST)
    return complex_fma_pattern::mkInstance (node, &ops, ifn);

  ifn  = complex_add_pattern::matches (op, perm_cache, node, &ops);
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

    void build (vec_info *);

    static vect_pattern*
    recognize (slp_tree_to_load_perm_map_t *, slp_tree *);
};

vect_pattern *
addsub_pattern::recognize (slp_tree_to_load_perm_map_t *, slp_tree *node_)
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
     (l0add_p), see whether we have FMA variants.  */
  if (!l0add_p
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
size_t num__slp_patterns = sizeof(slp_patterns)/sizeof(vect_pattern_decl_t);
