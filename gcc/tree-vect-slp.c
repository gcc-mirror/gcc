/* SLP - Basic Block Vectorization
   Copyright (C) 2007-2019 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>
   and Ira Rosen <irar@il.ibm.com>

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


/* Recursively free the memory allocated for the SLP tree rooted at NODE.
   FINAL_P is true if we have vectorized the instance or if we have
   made a final decision not to vectorize the statements in any way.  */

static void
vect_free_slp_tree (slp_tree node, bool final_p)
{
  int i;
  slp_tree child;

  if (--node->refcnt != 0)
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_free_slp_tree (child, final_p);

  /* Don't update STMT_VINFO_NUM_SLP_USES if it isn't relevant.
     Some statements might no longer exist, after having been
     removed by vect_transform_stmt.  Updating the remaining
     statements would be redundant.  */
  if (!final_p)
    {
      stmt_vec_info stmt_info;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
	{
	  gcc_assert (STMT_VINFO_NUM_SLP_USES (stmt_info) > 0);
	  STMT_VINFO_NUM_SLP_USES (stmt_info)--;
	}
    }

  SLP_TREE_CHILDREN (node).release ();
  SLP_TREE_SCALAR_STMTS (node).release ();
  SLP_TREE_SCALAR_OPS (node).release ();
  SLP_TREE_VEC_STMTS (node).release ();
  SLP_TREE_LOAD_PERMUTATION (node).release ();

  free (node);
}

/* Free the memory allocated for the SLP instance.  FINAL_P is true if we
   have vectorized the instance or if we have made a final decision not
   to vectorize the statements in any way.  */

void
vect_free_slp_instance (slp_instance instance, bool final_p)
{
  vect_free_slp_tree (SLP_INSTANCE_TREE (instance), final_p);
  SLP_INSTANCE_LOADS (instance).release ();
  free (instance);
}


/* Create an SLP node for SCALAR_STMTS.  */

static slp_tree
vect_create_new_slp_node (vec<stmt_vec_info> scalar_stmts)
{
  slp_tree node;
  stmt_vec_info stmt_info = scalar_stmts[0];
  unsigned int nops;

  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
    nops = gimple_call_num_args (stmt);
  else if (gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt))
    {
      nops = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	nops++;
    }
  else if (is_a <gphi *> (stmt_info->stmt))
    nops = 0;
  else
    return NULL;

  node = XNEW (struct _slp_tree);
  SLP_TREE_SCALAR_STMTS (node) = scalar_stmts;
  SLP_TREE_SCALAR_OPS (node) = vNULL;
  SLP_TREE_VEC_STMTS (node).create (0);
  SLP_TREE_NUMBER_OF_VEC_STMTS (node) = 0;
  SLP_TREE_CHILDREN (node).create (nops);
  SLP_TREE_LOAD_PERMUTATION (node) = vNULL;
  SLP_TREE_TWO_OPERATORS (node) = false;
  SLP_TREE_DEF_TYPE (node) = vect_internal_def;
  node->refcnt = 1;
  node->max_nunits = 1;

  unsigned i;
  FOR_EACH_VEC_ELT (scalar_stmts, i, stmt_info)
    STMT_VINFO_NUM_SLP_USES (stmt_info)++;

  return node;
}

/* Create an SLP node for OPS.  */

static slp_tree
vect_create_new_slp_node (vec<tree> ops)
{
  slp_tree node;

  node = XNEW (struct _slp_tree);
  SLP_TREE_SCALAR_STMTS (node) = vNULL;
  SLP_TREE_SCALAR_OPS (node) = ops;
  SLP_TREE_VEC_STMTS (node).create (0);
  SLP_TREE_NUMBER_OF_VEC_STMTS (node) = 0;
  SLP_TREE_CHILDREN (node) = vNULL;
  SLP_TREE_LOAD_PERMUTATION (node) = vNULL;
  SLP_TREE_TWO_OPERATORS (node) = false;
  SLP_TREE_DEF_TYPE (node) = vect_external_def;
  node->refcnt = 1;
  node->max_nunits = 1;

  return node;
}


/* This structure is used in creation of an SLP tree.  Each instance
   corresponds to the same operand in a group of scalar stmts in an SLP
   node.  */
typedef struct _slp_oprnd_info
{
  /* Def-stmts for the operands.  */
  vec<stmt_vec_info> def_stmts;
  /* Operands.  */
  vec<tree> ops;
  /* Information about the first statement, its vector def-type, type, the
     operand itself in case it's constant, and an indication if it's a pattern
     stmt.  */
  tree first_op_type;
  enum vect_def_type first_dt;
  bool any_pattern;
} *slp_oprnd_info;


/* Allocate operands info for NOPS operands, and GROUP_SIZE def-stmts for each
   operand.  */
static vec<slp_oprnd_info> 
vect_create_oprnd_info (int nops, int group_size)
{
  int i;
  slp_oprnd_info oprnd_info;
  vec<slp_oprnd_info> oprnds_info;

  oprnds_info.create (nops);
  for (i = 0; i < nops; i++)
    {
      oprnd_info = XNEW (struct _slp_oprnd_info);
      oprnd_info->def_stmts.create (group_size);
      oprnd_info->ops.create (group_size);
      oprnd_info->first_dt = vect_uninitialized_def;
      oprnd_info->first_op_type = NULL_TREE;
      oprnd_info->any_pattern = false;
      oprnds_info.quick_push (oprnd_info);
    }

  return oprnds_info;
}


/* Free operands info.  */

static void
vect_free_oprnd_info (vec<slp_oprnd_info> &oprnds_info)
{
  int i;
  slp_oprnd_info oprnd_info;

  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      oprnd_info->def_stmts.release ();
      oprnd_info->ops.release ();
      XDELETE (oprnd_info);
    }

  oprnds_info.release ();
}


/* Return true if STMTS contains a pattern statement.  */

static bool
vect_contains_pattern_stmt_p (vec<stmt_vec_info> stmts)
{
  stmt_vec_info stmt_info;
  unsigned int i;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    if (is_pattern_stmt_p (stmt_info))
      return true;
  return false;
}

/* Find the place of the data-ref in STMT_INFO in the interleaving chain
   that starts from FIRST_STMT_INFO.  Return -1 if the data-ref is not a part
   of the chain.  */

int
vect_get_place_in_interleaving_chain (stmt_vec_info stmt_info,
				      stmt_vec_info first_stmt_info)
{
  stmt_vec_info next_stmt_info = first_stmt_info;
  int result = 0;

  if (first_stmt_info != DR_GROUP_FIRST_ELEMENT (stmt_info))
    return -1;

  do
    {
      if (next_stmt_info == stmt_info)
	return result;
      next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
      if (next_stmt_info)
	result += DR_GROUP_GAP (next_stmt_info);
    }
  while (next_stmt_info);

  return -1;
}

/* Check whether it is possible to load COUNT elements of type ELT_TYPE
   using the method implemented by duplicate_and_interleave.  Return true
   if so, returning the number of intermediate vectors in *NVECTORS_OUT
   (if nonnull) and the type of each intermediate vector in *VECTOR_TYPE_OUT
   (if nonnull).  */

bool
can_duplicate_and_interleave_p (vec_info *vinfo, unsigned int count,
				tree elt_type, unsigned int *nvectors_out,
				tree *vector_type_out,
				tree *permutes)
{
  tree base_vector_type = get_vectype_for_scalar_type (vinfo, elt_type, count);
  if (!base_vector_type || !VECTOR_MODE_P (TYPE_MODE (base_vector_type)))
    return false;

  machine_mode base_vector_mode = TYPE_MODE (base_vector_type);
  poly_int64 elt_bytes = count * GET_MODE_UNIT_SIZE (base_vector_mode);
  unsigned int nvectors = 1;
  for (;;)
    {
      scalar_int_mode int_mode;
      poly_int64 elt_bits = elt_bytes * BITS_PER_UNIT;
      if (int_mode_for_size (elt_bits, 1).exists (&int_mode))
	{
	  /* Get the natural vector type for this SLP group size.  */
	  tree int_type = build_nonstandard_integer_type
	    (GET_MODE_BITSIZE (int_mode), 1);
	  tree vector_type
	    = get_vectype_for_scalar_type (vinfo, int_type, count);
	  if (vector_type
	      && VECTOR_MODE_P (TYPE_MODE (vector_type))
	      && known_eq (GET_MODE_SIZE (TYPE_MODE (vector_type)),
			   GET_MODE_SIZE (base_vector_mode)))
	    {
	      /* Try fusing consecutive sequences of COUNT / NVECTORS elements
		 together into elements of type INT_TYPE and using the result
		 to build NVECTORS vectors.  */
	      poly_uint64 nelts = GET_MODE_NUNITS (TYPE_MODE (vector_type));
	      vec_perm_builder sel1 (nelts, 2, 3);
	      vec_perm_builder sel2 (nelts, 2, 3);
	      poly_int64 half_nelts = exact_div (nelts, 2);
	      for (unsigned int i = 0; i < 3; ++i)
		{
		  sel1.quick_push (i);
		  sel1.quick_push (i + nelts);
		  sel2.quick_push (half_nelts + i);
		  sel2.quick_push (half_nelts + i + nelts);
		}
	      vec_perm_indices indices1 (sel1, 2, nelts);
	      vec_perm_indices indices2 (sel2, 2, nelts);
	      if (can_vec_perm_const_p (TYPE_MODE (vector_type), indices1)
		  && can_vec_perm_const_p (TYPE_MODE (vector_type), indices2))
		{
		  if (nvectors_out)
		    *nvectors_out = nvectors;
		  if (vector_type_out)
		    *vector_type_out = vector_type;
		  if (permutes)
		    {
		      permutes[0] = vect_gen_perm_mask_checked (vector_type,
								indices1);
		      permutes[1] = vect_gen_perm_mask_checked (vector_type,
								indices2);
		    }
		  return true;
		}
	    }
	}
      if (!multiple_p (elt_bytes, 2, &elt_bytes))
	return false;
      nvectors *= 2;
    }
}

/* Get the defs for the rhs of STMT (collect them in OPRNDS_INFO), check that
   they are of a valid type and that they match the defs of the first stmt of
   the SLP group (stored in OPRNDS_INFO).  This function tries to match stmts
   by swapping operands of STMTS[STMT_NUM] when possible.  Non-zero *SWAP
   indicates swap is required for cond_expr stmts.  Specifically, *SWAP
   is 1 if STMT is cond and operands of comparison need to be swapped;
   *SWAP is 2 if STMT is cond and code of comparison needs to be inverted.
   If there is any operand swap in this function, *SWAP is set to non-zero
   value.
   If there was a fatal error return -1; if the error could be corrected by
   swapping operands of father node of this one, return 1; if everything is
   ok return 0.  */
static int
vect_get_and_check_slp_defs (vec_info *vinfo, unsigned char *swap,
			     vec<stmt_vec_info> stmts, unsigned stmt_num,
			     vec<slp_oprnd_info> *oprnds_info)
{
  stmt_vec_info stmt_info = stmts[stmt_num];
  tree oprnd;
  unsigned int i, number_of_oprnds;
  enum vect_def_type dt = vect_uninitialized_def;
  slp_oprnd_info oprnd_info;
  int first_op_idx = 1;
  unsigned int commutative_op = -1U;
  bool first_op_cond = false;
  bool first = stmt_num == 0;

  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
    {
      number_of_oprnds = gimple_call_num_args (stmt);
      first_op_idx = 3;
      if (gimple_call_internal_p (stmt))
	{
	  internal_fn ifn = gimple_call_internal_fn (stmt);
	  commutative_op = first_commutative_argument (ifn);

	  /* Masked load, only look at mask.  */
	  if (ifn == IFN_MASK_LOAD)
	    {
	      number_of_oprnds = 1;
	      /* Mask operand index.  */
	      first_op_idx = 5;
	    }
	}
    }
  else if (gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt))
    {
      enum tree_code code = gimple_assign_rhs_code (stmt);
      number_of_oprnds = gimple_num_ops (stmt) - 1;
      /* Swap can only be done for cond_expr if asked to, otherwise we
	 could result in different comparison code to the first stmt.  */
      if (code == COND_EXPR
	  && COMPARISON_CLASS_P (gimple_assign_rhs1 (stmt)))
	{
	  first_op_cond = true;
	  number_of_oprnds++;
	}
      else
	commutative_op = commutative_tree_code (code) ? 0U : -1U;
    }
  else
    return -1;

  bool swapped = (*swap != 0);
  gcc_assert (!swapped || first_op_cond);
  for (i = 0; i < number_of_oprnds; i++)
    {
again:
      if (first_op_cond)
	{
	  /* Map indicating how operands of cond_expr should be swapped.  */
	  int maps[3][4] = {{0, 1, 2, 3}, {1, 0, 2, 3}, {0, 1, 3, 2}};
	  int *map = maps[*swap];

	  if (i < 2)
	    oprnd = TREE_OPERAND (gimple_op (stmt_info->stmt,
					     first_op_idx), map[i]);
	  else
	    oprnd = gimple_op (stmt_info->stmt, map[i]);
	}
      else
	oprnd = gimple_op (stmt_info->stmt, first_op_idx + (swapped ? !i : i));
      if (TREE_CODE (oprnd) == VIEW_CONVERT_EXPR)
	oprnd = TREE_OPERAND (oprnd, 0);

      oprnd_info = (*oprnds_info)[i];

      stmt_vec_info def_stmt_info;
      if (!vect_is_simple_use (oprnd, vinfo, &dt, &def_stmt_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: can't analyze def for %T\n",
			     oprnd);

	  return -1;
	}

      if (def_stmt_info && is_pattern_stmt_p (def_stmt_info))
	oprnd_info->any_pattern = true;

      if (first)
	{
	  /* For the swapping logic below force vect_reduction_def
	     for the reduction op in a SLP reduction group.  */
	  if (!STMT_VINFO_DATA_REF (stmt_info)
	      && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
	      && (int)i == STMT_VINFO_REDUC_IDX (stmt_info)
	      && def_stmt_info)
	    dt = vect_reduction_def;
	  oprnd_info->first_dt = dt;
	  oprnd_info->first_op_type = TREE_TYPE (oprnd);
	}
      else
	{
	  /* Not first stmt of the group, check that the def-stmt/s match
	     the def-stmt/s of the first stmt.  Allow different definition
	     types for reduction chains: the first stmt must be a
	     vect_reduction_def (a phi node), and the rest
	     end in the reduction chain.  */
	  tree type = TREE_TYPE (oprnd);
	  if ((oprnd_info->first_dt != dt
	       && !(oprnd_info->first_dt == vect_reduction_def
		    && !STMT_VINFO_DATA_REF (stmt_info)
		    && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
		    && def_stmt_info
		    && !STMT_VINFO_DATA_REF (def_stmt_info)
		    && (REDUC_GROUP_FIRST_ELEMENT (def_stmt_info)
			== REDUC_GROUP_FIRST_ELEMENT (stmt_info)))
	       && !((oprnd_info->first_dt == vect_external_def
		     || oprnd_info->first_dt == vect_constant_def)
		    && (dt == vect_external_def
			|| dt == vect_constant_def)))
	      || !types_compatible_p (oprnd_info->first_op_type, type)
	      || (!STMT_VINFO_DATA_REF (stmt_info)
		  && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
		  && ((!def_stmt_info
		       || STMT_VINFO_DATA_REF (def_stmt_info)
		       || (REDUC_GROUP_FIRST_ELEMENT (def_stmt_info)
			   != REDUC_GROUP_FIRST_ELEMENT (stmt_info)))
		      != (oprnd_info->first_dt != vect_reduction_def))))
	    {
	      /* Try swapping operands if we got a mismatch.  */
	      if (i == commutative_op && !swapped)
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "trying swapped operands\n");
		  swapped = true;
		  goto again;
		}

	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different types\n");

	      return 1;
	    }
	  if ((dt == vect_constant_def
	       || dt == vect_external_def)
	      && !GET_MODE_SIZE (vinfo->vector_mode).is_constant ()
	      && (TREE_CODE (type) == BOOLEAN_TYPE
		  || !can_duplicate_and_interleave_p (vinfo, stmts.length (),
						      type)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: invalid type of def "
				 "for variable-length SLP %T\n", oprnd);
	      return -1;
	    }
	}

      /* Check the types of the definitions.  */
      switch (dt)
	{
	case vect_external_def:
	  /* Make sure to demote the overall operand to external.  */
	  oprnd_info->first_dt = vect_external_def;
	  /* Fallthru.  */
	case vect_constant_def:
	  oprnd_info->def_stmts.quick_push (NULL);
	  oprnd_info->ops.quick_push (oprnd);
	  break;

	case vect_internal_def:
	case vect_reduction_def:
	  if (oprnd_info->first_dt == vect_reduction_def
	      && !STMT_VINFO_DATA_REF (stmt_info)
	      && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
	      && !STMT_VINFO_DATA_REF (def_stmt_info)
	      && (REDUC_GROUP_FIRST_ELEMENT (def_stmt_info)
		  == REDUC_GROUP_FIRST_ELEMENT (stmt_info)))
	    {
	      /* For a SLP reduction chain we want to duplicate the
	         reduction to each of the chain members.  That gets
		 us a sane SLP graph (still the stmts are not 100%
		 correct wrt the initial values).  */
	      gcc_assert (!first);
	      oprnd_info->def_stmts.quick_push (oprnd_info->def_stmts[0]);
	      oprnd_info->ops.quick_push (oprnd_info->ops[0]);
	      break;
	    }
	  /* Fallthru.  */
	case vect_induction_def:
	  oprnd_info->def_stmts.quick_push (def_stmt_info);
	  oprnd_info->ops.quick_push (oprnd);
	  break;

	default:
	  /* FORNOW: Not supported.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: illegal type of def %T\n",
			     oprnd);

	  return -1;
	}
    }

  /* Swap operands.  */
  if (swapped)
    {
      if (first_op_cond)
	{
	  /* If there are already uses of this stmt in a SLP instance then
	     we've committed to the operand order and can't swap it.  */
	  if (STMT_VINFO_NUM_SLP_USES (stmt_info) != 0)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: cannot swap operands of "
				 "shared stmt %G", stmt_info->stmt);
	      return -1;
	    }

	  /* To get rid of this swapping we have to move the stmt code
	     to the SLP tree as well (and gather it here per stmt).  */
	  gassign *stmt = as_a <gassign *> (stmt_info->stmt);
	  tree cond = gimple_assign_rhs1 (stmt);
	  enum tree_code code = TREE_CODE (cond);

	  /* Swap.  */
	  if (*swap == 1)
	    {
	      swap_ssa_operands (stmt, &TREE_OPERAND (cond, 0),
				 &TREE_OPERAND (cond, 1));
	      TREE_SET_CODE (cond, swap_tree_comparison (code));
	    }
	  /* Invert.  */
	  else
	    {
	      swap_ssa_operands (stmt, gimple_assign_rhs2_ptr (stmt),
				 gimple_assign_rhs3_ptr (stmt));
	      if (STMT_VINFO_REDUC_IDX (stmt_info) == 1)
		STMT_VINFO_REDUC_IDX (stmt_info) = 2;
	      else if (STMT_VINFO_REDUC_IDX (stmt_info) == 2)
		STMT_VINFO_REDUC_IDX (stmt_info) = 1;
	      bool honor_nans = HONOR_NANS (TREE_OPERAND (cond, 0));
	      code = invert_tree_comparison (TREE_CODE (cond), honor_nans);
	      gcc_assert (code != ERROR_MARK);
	      TREE_SET_CODE (cond, code);
	    }
	}
      else
	{
	  /* Commutative ops need not reflect swapping, ops are in
	     the SLP tree.  */
	}
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "swapped operands to match def types in %G",
			 stmt_info->stmt);
    }

  *swap = swapped;
  return 0;
}

/* Try to assign vector type VECTYPE to STMT_INFO for BB vectorization.
   Return true if we can, meaning that this choice doesn't conflict with
   existing SLP nodes that use STMT_INFO.  */

static bool
vect_update_shared_vectype (stmt_vec_info stmt_info, tree vectype)
{
  tree old_vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (old_vectype && useless_type_conversion_p (vectype, old_vectype))
    return true;

  if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
    {
      /* We maintain the invariant that if any statement in the group is
	 used, all other members of the group have the same vector type.  */
      stmt_vec_info first_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      stmt_vec_info member_info = first_info;
      for (; member_info; member_info = DR_GROUP_NEXT_ELEMENT (member_info))
	if (STMT_VINFO_NUM_SLP_USES (member_info) > 0
	    || is_pattern_stmt_p (member_info))
	  break;

      if (!member_info)
	{
	  for (member_info = first_info; member_info;
	       member_info = DR_GROUP_NEXT_ELEMENT (member_info))
	    STMT_VINFO_VECTYPE (member_info) = vectype;
	  return true;
	}
    }
  else if (STMT_VINFO_NUM_SLP_USES (stmt_info) == 0
	   && !is_pattern_stmt_p (stmt_info))
    {
      STMT_VINFO_VECTYPE (stmt_info) = vectype;
      return true;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		       "Build SLP failed: incompatible vector"
		       " types for: %G", stmt_info->stmt);
      dump_printf_loc (MSG_NOTE, vect_location,
		       "    old vector type: %T\n", old_vectype);
      dump_printf_loc (MSG_NOTE, vect_location,
		       "    new vector type: %T\n", vectype);
    }
  return false;
}

/* Try to infer and assign a vector type to all the statements in STMTS.
   Used only for BB vectorization.  */

static bool
vect_update_all_shared_vectypes (vec<stmt_vec_info> stmts)
{
  tree vectype, nunits_vectype;
  if (!vect_get_vector_types_for_stmt (stmts[0], &vectype,
				       &nunits_vectype, stmts.length ()))
    return false;

  stmt_vec_info stmt_info;
  unsigned int i;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    if (!vect_update_shared_vectype (stmt_info, vectype))
      return false;

  return true;
}

/* Return true if call statements CALL1 and CALL2 are similar enough
   to be combined into the same SLP group.  */

static bool
compatible_calls_p (gcall *call1, gcall *call2)
{
  unsigned int nargs = gimple_call_num_args (call1);
  if (nargs != gimple_call_num_args (call2))
    return false;

  if (gimple_call_combined_fn (call1) != gimple_call_combined_fn (call2))
    return false;

  if (gimple_call_internal_p (call1))
    {
      if (!types_compatible_p (TREE_TYPE (gimple_call_lhs (call1)),
			       TREE_TYPE (gimple_call_lhs (call2))))
	return false;
      for (unsigned int i = 0; i < nargs; ++i)
	if (!types_compatible_p (TREE_TYPE (gimple_call_arg (call1, i)),
				 TREE_TYPE (gimple_call_arg (call2, i))))
	  return false;
    }
  else
    {
      if (!operand_equal_p (gimple_call_fn (call1),
			    gimple_call_fn (call2), 0))
	return false;

      if (gimple_call_fntype (call1) != gimple_call_fntype (call2))
	return false;
    }
  return true;
}

/* A subroutine of vect_build_slp_tree for checking VECTYPE, which is the
   caller's attempt to find the vector type in STMT_INFO with the narrowest
   element type.  Return true if VECTYPE is nonnull and if it is valid
   for STMT_INFO.  When returning true, update MAX_NUNITS to reflect the
   number of units in VECTYPE.  GROUP_SIZE and MAX_NUNITS are as for
   vect_build_slp_tree.  */

static bool
vect_record_max_nunits (stmt_vec_info stmt_info, unsigned int group_size,
			tree vectype, poly_uint64 *max_nunits)
{
  if (!vectype)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Build SLP failed: unsupported data-type in %G\n",
			 stmt_info->stmt);
      /* Fatal mismatch.  */
      return false;
    }

  /* If populating the vector type requires unrolling then fail
     before adjusting *max_nunits for basic-block vectorization.  */
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  unsigned HOST_WIDE_INT const_nunits;
  if (STMT_VINFO_BB_VINFO (stmt_info)
      && (!nunits.is_constant (&const_nunits)
	  || const_nunits > group_size))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Build SLP failed: unrolling required "
			 "in basic block SLP\n");
      /* Fatal mismatch.  */
      return false;
    }

  /* In case of multiple types we need to detect the smallest type.  */
  vect_update_max_nunits (max_nunits, vectype);
  return true;
}

/* STMTS is a group of GROUP_SIZE SLP statements in which some
   statements do the same operation as the first statement and in which
   the others do ALT_STMT_CODE.  Return true if we can take one vector
   of the first operation and one vector of the second and permute them
   to get the required result.  VECTYPE is the type of the vector that
   would be permuted.  */

static bool
vect_two_operations_perm_ok_p (vec<stmt_vec_info> stmts,
			       unsigned int group_size, tree vectype,
			       tree_code alt_stmt_code)
{
  unsigned HOST_WIDE_INT count;
  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&count))
    return false;

  vec_perm_builder sel (count, count, 1);
  for (unsigned int i = 0; i < count; ++i)
    {
      unsigned int elt = i;
      gassign *stmt = as_a <gassign *> (stmts[i % group_size]->stmt);
      if (gimple_assign_rhs_code (stmt) == alt_stmt_code)
	elt += count;
      sel.quick_push (elt);
    }
  vec_perm_indices indices (sel, 2, count);
  return can_vec_perm_const_p (TYPE_MODE (vectype), indices);
}

/* Verify if the scalar stmts STMTS are isomorphic, require data
   permutation or are of unsupported types of operation.  Return
   true if they are, otherwise return false and indicate in *MATCHES
   which stmts are not isomorphic to the first one.  If MATCHES[0]
   is false then this indicates the comparison could not be
   carried out or the stmts will never be vectorized by SLP.

   Note COND_EXPR is possibly isomorphic to another one after swapping its
   operands.  Set SWAP[i] to 1 if stmt I is COND_EXPR and isomorphic to
   the first stmt by swapping the two operands of comparison; set SWAP[i]
   to 2 if stmt I is isormorphic to the first stmt by inverting the code
   of comparison.  Take A1 >= B1 ? X1 : Y1 as an exmple, it can be swapped
   to (B1 <= A1 ? X1 : Y1); or be inverted to (A1 < B1) ? Y1 : X1.  */

static bool
vect_build_slp_tree_1 (unsigned char *swap,
		       vec<stmt_vec_info> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits, bool *matches,
		       bool *two_operators)
{
  unsigned int i;
  stmt_vec_info first_stmt_info = stmts[0];
  enum tree_code first_stmt_code = ERROR_MARK;
  enum tree_code alt_stmt_code = ERROR_MARK;
  enum tree_code rhs_code = ERROR_MARK;
  enum tree_code first_cond_code = ERROR_MARK;
  tree lhs;
  bool need_same_oprnds = false;
  tree vectype = NULL_TREE, first_op1 = NULL_TREE;
  optab optab;
  int icode;
  machine_mode optab_op2_mode;
  machine_mode vec_mode;
  stmt_vec_info first_load = NULL, prev_first_load = NULL;
  bool load_p = false;

  /* For every stmt in NODE find its def stmt/s.  */
  stmt_vec_info stmt_info;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    {
      vec_info *vinfo = stmt_info->vinfo;
      gimple *stmt = stmt_info->stmt;
      swap[i] = 0;
      matches[i] = false;

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "Build SLP for %G", stmt);

      /* Fail to vectorize statements marked as unvectorizable.  */
      if (!STMT_VINFO_VECTORIZABLE (stmt_info))
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: unvectorizable statement %G",
			     stmt);
	  /* Fatal mismatch.  */
	  matches[0] = false;
          return false;
        }

      lhs = gimple_get_lhs (stmt);
      if (lhs == NULL_TREE)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: not GIMPLE_ASSIGN nor "
			     "GIMPLE_CALL %G", stmt);
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}

      tree nunits_vectype;
      if (!vect_get_vector_types_for_stmt (stmt_info, &vectype,
					   &nunits_vectype, group_size)
	  || (nunits_vectype
	      && !vect_record_max_nunits (stmt_info, group_size,
					  nunits_vectype, max_nunits)))
	{
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}

      gcc_assert (vectype);

      if (is_a <bb_vec_info> (vinfo)
	  && !vect_update_shared_vectype (stmt_info, vectype))
	continue;

      if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
	{
	  rhs_code = CALL_EXPR;

	  if (gimple_call_internal_p (stmt, IFN_MASK_LOAD))
	    load_p = true;
	  else if ((gimple_call_internal_p (call_stmt)
		    && (!vectorizable_internal_fn_p
			(gimple_call_internal_fn (call_stmt))))
		   || gimple_call_tail_p (call_stmt)
		   || gimple_call_noreturn_p (call_stmt)
		   || !gimple_call_nothrow_p (call_stmt)
		   || gimple_call_chain (call_stmt))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: unsupported call type %G",
				 call_stmt);
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }
	}
      else
	{
	  rhs_code = gimple_assign_rhs_code (stmt);
	  load_p = TREE_CODE_CLASS (rhs_code) == tcc_reference;
	}

      /* Check the operation.  */
      if (i == 0)
	{
	  first_stmt_code = rhs_code;

	  /* Shift arguments should be equal in all the packed stmts for a
	     vector shift with scalar shift operand.  */
	  if (rhs_code == LSHIFT_EXPR || rhs_code == RSHIFT_EXPR
	      || rhs_code == LROTATE_EXPR
	      || rhs_code == RROTATE_EXPR)
	    {
	      vec_mode = TYPE_MODE (vectype);

	      /* First see if we have a vector/vector shift.  */
	      optab = optab_for_tree_code (rhs_code, vectype,
					   optab_vector);

	      if (!optab
		  || optab_handler (optab, vec_mode) == CODE_FOR_nothing)
		{
		  /* No vector/vector shift, try for a vector/scalar shift.  */
		  optab = optab_for_tree_code (rhs_code, vectype,
					       optab_scalar);

		  if (!optab)
		    {
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
					 "Build SLP failed: no optab.\n");
		      /* Fatal mismatch.  */
		      matches[0] = false;
		      return false;
		    }
		  icode = (int) optab_handler (optab, vec_mode);
		  if (icode == CODE_FOR_nothing)
		    {
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
					 "Build SLP failed: "
					 "op not supported by target.\n");
		      /* Fatal mismatch.  */
		      matches[0] = false;
		      return false;
		    }
		  optab_op2_mode = insn_data[icode].operand[2].mode;
		  if (!VECTOR_MODE_P (optab_op2_mode))
		    {
		      need_same_oprnds = true;
		      first_op1 = gimple_assign_rhs2 (stmt);
		    }
		}
	    }
	  else if (rhs_code == WIDEN_LSHIFT_EXPR)
            {
              need_same_oprnds = true;
              first_op1 = gimple_assign_rhs2 (stmt);
            }
	}
      else
	{
	  if (first_stmt_code != rhs_code
	      && alt_stmt_code == ERROR_MARK)
	    alt_stmt_code = rhs_code;
	  if (first_stmt_code != rhs_code
	      && (first_stmt_code != IMAGPART_EXPR
		  || rhs_code != REALPART_EXPR)
	      && (first_stmt_code != REALPART_EXPR
		  || rhs_code != IMAGPART_EXPR)
	      /* Handle mismatches in plus/minus by computing both
		 and merging the results.  */
	      && !((first_stmt_code == PLUS_EXPR
		    || first_stmt_code == MINUS_EXPR)
		   && (alt_stmt_code == PLUS_EXPR
		       || alt_stmt_code == MINUS_EXPR)
		   && rhs_code == alt_stmt_code)
	      && !(STMT_VINFO_GROUPED_ACCESS (stmt_info)
                   && (first_stmt_code == ARRAY_REF
                       || first_stmt_code == BIT_FIELD_REF
                       || first_stmt_code == INDIRECT_REF
                       || first_stmt_code == COMPONENT_REF
                       || first_stmt_code == MEM_REF)))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
				   "Build SLP failed: different operation "
				   "in stmt %G", stmt);
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "original stmt %G", first_stmt_info->stmt);
		}
	      /* Mismatch.  */
	      continue;
	    }

	  if (need_same_oprnds
	      && !operand_equal_p (first_op1, gimple_assign_rhs2 (stmt), 0))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different shift "
				 "arguments in %G", stmt);
	      /* Mismatch.  */
	      continue;
	    }

	  if (!load_p && rhs_code == CALL_EXPR)
	    {
	      if (!compatible_calls_p (as_a <gcall *> (stmts[0]->stmt),
				       as_a <gcall *> (stmt)))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different calls in %G",
				     stmt);
		  /* Mismatch.  */
		  continue;
		}
	    }
	}

      /* Grouped store or load.  */
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
	{
	  if (REFERENCE_CLASS_P (lhs))
	    {
	      /* Store.  */
	      ;
	    }
	  else
	    {
	      /* Load.  */
	      first_load = DR_GROUP_FIRST_ELEMENT (stmt_info);
              if (prev_first_load)
                {
                  /* Check that there are no loads from different interleaving
                     chains in the same node.  */
                  if (prev_first_load != first_load)
                    {
                      if (dump_enabled_p ())
			dump_printf_loc (MSG_MISSED_OPTIMIZATION,
					 vect_location,
					 "Build SLP failed: different "
					 "interleaving chains in one node %G",
					 stmt);
		      /* Mismatch.  */
		      continue;
                    }
                }
              else
                prev_first_load = first_load;
           }
        } /* Grouped access.  */
      else
	{
	  if (load_p)
	    {
	      /* Not grouped load.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: not grouped load %G", stmt);

	      /* FORNOW: Not grouped loads are not supported.  */
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }

	  /* Not memory operation.  */
	  if (TREE_CODE_CLASS (rhs_code) != tcc_binary
	      && TREE_CODE_CLASS (rhs_code) != tcc_unary
	      && TREE_CODE_CLASS (rhs_code) != tcc_expression
	      && TREE_CODE_CLASS (rhs_code) != tcc_comparison
	      && rhs_code != VIEW_CONVERT_EXPR
	      && rhs_code != CALL_EXPR)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: operation unsupported %G",
				 stmt);
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }

	  if (rhs_code == COND_EXPR)
	    {
	      tree cond_expr = gimple_assign_rhs1 (stmt);
	      enum tree_code cond_code = TREE_CODE (cond_expr);
	      enum tree_code swap_code = ERROR_MARK;
	      enum tree_code invert_code = ERROR_MARK;

	      if (i == 0)
		first_cond_code = TREE_CODE (cond_expr);
	      else if (TREE_CODE_CLASS (cond_code) == tcc_comparison)
		{
		  bool honor_nans = HONOR_NANS (TREE_OPERAND (cond_expr, 0));
		  swap_code = swap_tree_comparison (cond_code);
		  invert_code = invert_tree_comparison (cond_code, honor_nans);
		}

	      if (first_cond_code == cond_code)
		;
	      /* Isomorphic can be achieved by swapping.  */
	      else if (first_cond_code == swap_code)
		swap[i] = 1;
	      /* Isomorphic can be achieved by inverting.  */
	      else if (first_cond_code == invert_code)
		swap[i] = 2;
	      else
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different"
				     " operation %G", stmt);
		  /* Mismatch.  */
		  continue;
		}
	    }
	}

      matches[i] = true;
    }

  for (i = 0; i < group_size; ++i)
    if (!matches[i])
      return false;

  /* If we allowed a two-operation SLP node verify the target can cope
     with the permute we are going to use.  */
  if (alt_stmt_code != ERROR_MARK
      && TREE_CODE_CLASS (alt_stmt_code) != tcc_reference)
    {
      if (!vect_two_operations_perm_ok_p (stmts, group_size,
					  vectype, alt_stmt_code))
	{
	  for (i = 0; i < group_size; ++i)
	    if (gimple_assign_rhs_code (stmts[i]->stmt) == alt_stmt_code)
	      {
		matches[i] = false;
		if (dump_enabled_p ())
		  {
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different operation "
				     "in stmt %G", stmts[i]->stmt);
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "original stmt %G", first_stmt_info->stmt);
		  }
	      }
	  return false;
	}
      *two_operators = true;
    }

  return true;
}

/* Traits for the hash_set to record failed SLP builds for a stmt set.
   Note we never remove apart from at destruction time so we do not
   need a special value for deleted that differs from empty.  */
struct bst_traits
{
  typedef vec <stmt_vec_info> value_type;
  typedef vec <stmt_vec_info> compare_type;
  static inline hashval_t hash (value_type);
  static inline bool equal (value_type existing, value_type candidate);
  static inline bool is_empty (value_type x) { return !x.exists (); }
  static inline bool is_deleted (value_type x) { return !x.exists (); }
  static inline void mark_empty (value_type &x) { x.release (); }
  static inline void mark_deleted (value_type &x) { x.release (); }
  static inline void remove (value_type &x) { x.release (); }
};
inline hashval_t
bst_traits::hash (value_type x)
{
  inchash::hash h;
  for (unsigned i = 0; i < x.length (); ++i)
    h.add_int (gimple_uid (x[i]->stmt));
  return h.end ();
}
inline bool
bst_traits::equal (value_type existing, value_type candidate)
{
  if (existing.length () != candidate.length ())
    return false;
  for (unsigned i = 0; i < existing.length (); ++i)
    if (existing[i] != candidate[i])
      return false;
  return true;
}

typedef hash_map <vec <gimple *>, slp_tree,
		  simple_hashmap_traits <bst_traits, slp_tree> >
  scalar_stmts_to_slp_tree_map_t;

static slp_tree
vect_build_slp_tree_2 (vec_info *vinfo,
		       vec<stmt_vec_info> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits,
		       bool *matches, unsigned *npermutes, unsigned *tree_size,
		       scalar_stmts_to_slp_tree_map_t *bst_map);

static slp_tree
vect_build_slp_tree (vec_info *vinfo,
		     vec<stmt_vec_info> stmts, unsigned int group_size,
		     poly_uint64 *max_nunits,
		     bool *matches, unsigned *npermutes, unsigned *tree_size,
		     scalar_stmts_to_slp_tree_map_t *bst_map)
{
  if (slp_tree *leader = bst_map->get (stmts))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "re-using %sSLP tree %p\n",
			 *leader ? "" : "failed ", *leader);
      if (*leader)
	{
	  (*leader)->refcnt++;
	  vect_update_max_nunits (max_nunits, (*leader)->max_nunits);
	}
      return *leader;
    }
  poly_uint64 this_max_nunits = 1;
  slp_tree res = vect_build_slp_tree_2 (vinfo, stmts, group_size,
					&this_max_nunits,
					matches, npermutes, tree_size, bst_map);
  if (res)
    {
      res->max_nunits = this_max_nunits;
      vect_update_max_nunits (max_nunits, this_max_nunits);
      /* Keep a reference for the bst_map use.  */
      res->refcnt++;
    }
  bst_map->put (stmts.copy (), res);
  return res;
}

/* Recursively build an SLP tree starting from NODE.
   Fail (and return a value not equal to zero) if def-stmts are not
   isomorphic, require data permutation or are of unsupported types of
   operation.  Otherwise, return 0.
   The value returned is the depth in the SLP tree where a mismatch
   was found.  */

static slp_tree
vect_build_slp_tree_2 (vec_info *vinfo,
		       vec<stmt_vec_info> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits,
		       bool *matches, unsigned *npermutes, unsigned *tree_size,
		       scalar_stmts_to_slp_tree_map_t *bst_map)
{
  unsigned nops, i, this_tree_size = 0;
  poly_uint64 this_max_nunits = *max_nunits;
  slp_tree node;

  matches[0] = false;

  stmt_vec_info stmt_info = stmts[0];
  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
    nops = gimple_call_num_args (stmt);
  else if (gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt))
    {
      nops = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	nops++;
    }
  else if (is_a <gphi *> (stmt_info->stmt))
    nops = 0;
  else
    return NULL;

  /* If the SLP node is a PHI (induction or reduction), terminate
     the recursion.  */
  if (gphi *stmt = dyn_cast <gphi *> (stmt_info->stmt))
    {
      tree scalar_type = TREE_TYPE (PHI_RESULT (stmt));
      tree vectype = get_vectype_for_scalar_type (vinfo, scalar_type);
      if (!vect_record_max_nunits (stmt_info, group_size, vectype, max_nunits))
	return NULL;

      vect_def_type def_type = STMT_VINFO_DEF_TYPE (stmt_info);
      /* Induction from different IVs is not supported.  */
      if (def_type == vect_induction_def)
	{
	  stmt_vec_info other_info;
	  FOR_EACH_VEC_ELT (stmts, i, other_info)
	    if (stmt_info != other_info)
	      return NULL;
	}
      else if (def_type == vect_reduction_def
	       || def_type == vect_double_reduction_def
	       || def_type == vect_nested_cycle)
	{
	  /* Else def types have to match.  */
	  stmt_vec_info other_info;
	  FOR_EACH_VEC_ELT (stmts, i, other_info)
	    if (STMT_VINFO_DEF_TYPE (other_info) != def_type)
	      return NULL;
	}
      else
	return NULL;
      (*tree_size)++;
      node = vect_create_new_slp_node (stmts);
      return node;
    }


  bool two_operators = false;
  unsigned char *swap = XALLOCAVEC (unsigned char, group_size);
  if (!vect_build_slp_tree_1 (swap, stmts, group_size,
			      &this_max_nunits, matches, &two_operators))
    return NULL;

  /* If the SLP node is a load, terminate the recursion unless masked.  */
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
    {
      if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
	{
	  /* Masked load.  */
	  gcc_assert (gimple_call_internal_p (stmt, IFN_MASK_LOAD));
	  nops = 1;
	}
      else
	{
	  *max_nunits = this_max_nunits;
	  (*tree_size)++;
	  node = vect_create_new_slp_node (stmts);
	  return node;
	}
    }

  /* Get at the operands, verifying they are compatible.  */
  vec<slp_oprnd_info> oprnds_info = vect_create_oprnd_info (nops, group_size);
  slp_oprnd_info oprnd_info;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    {
      int res = vect_get_and_check_slp_defs (vinfo, &swap[i],
					     stmts, i, &oprnds_info);
      if (res != 0)
	matches[(res == -1) ? 0 : i] = false;
      if (!matches[0])
	break;
    }
  for (i = 0; i < group_size; ++i)
    if (!matches[i])
      {
	vect_free_oprnd_info (oprnds_info);
	return NULL;
      }

  auto_vec<slp_tree, 4> children;

  stmt_info = stmts[0];

  /* Create SLP_TREE nodes for the definition node/s.  */
  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      slp_tree child;
      unsigned old_tree_size = this_tree_size;
      unsigned int j;

      if (oprnd_info->first_dt == vect_uninitialized_def)
	{
	  /* COND_EXPR have one too many eventually if the condition
	     is a SSA name.  */
	  gcc_assert (i == 3 && nops == 4);
	  continue;
	}

      if (oprnd_info->first_dt != vect_internal_def
	  && oprnd_info->first_dt != vect_reduction_def
	  && oprnd_info->first_dt != vect_induction_def)
	{
	  slp_tree invnode = vect_create_new_slp_node (oprnd_info->ops);
	  SLP_TREE_DEF_TYPE (invnode) = oprnd_info->first_dt;
	  oprnd_info->ops = vNULL;
	  children.safe_push (invnode);
	  continue;
	}

      if ((child = vect_build_slp_tree (vinfo, oprnd_info->def_stmts,
					group_size, &this_max_nunits,
					matches, npermutes,
					&this_tree_size, bst_map)) != NULL)
	{
	  /* If we have all children of a non-unary child built up from
	     scalars then just throw that away and build it up this node
	     from scalars.  */
	  if (is_a <bb_vec_info> (vinfo)
	      && SLP_TREE_CHILDREN (child).length () > 1
	      /* ???  Rejecting patterns this way doesn't work.  We'd have to
		 do extra work to cancel the pattern so the uses see the
		 scalar version.  */
	      && !oprnd_info->any_pattern)
	    {
	      slp_tree grandchild;

	      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		if (SLP_TREE_DEF_TYPE (grandchild) != vect_external_def)
		  break;
	      if (!grandchild
		  && vect_update_all_shared_vectypes (oprnd_info->def_stmts))
		{
		  /* Roll back.  */
		  this_tree_size = old_tree_size;
		  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		    vect_free_slp_tree (grandchild, false);
		  SLP_TREE_CHILDREN (child).truncate (0);

		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "Building parent vector operands from "
				     "scalars instead\n");
		  oprnd_info->def_stmts = vNULL;
		  SLP_TREE_DEF_TYPE (child) = vect_external_def;
		  SLP_TREE_SCALAR_OPS (child) = oprnd_info->ops;
		  oprnd_info->ops = vNULL;
		  ++this_tree_size;
		  children.safe_push (child);
		  continue;
		}
	    }

	  oprnd_info->def_stmts = vNULL;
	  children.safe_push (child);
	  continue;
	}

      /* If the SLP build failed fatally and we analyze a basic-block
         simply treat nodes we fail to build as externally defined
	 (and thus build vectors from the scalar defs).
	 The cost model will reject outright expensive cases.
	 ???  This doesn't treat cases where permutation ultimatively
	 fails (or we don't try permutation below).  Ideally we'd
	 even compute a permutation that will end up with the maximum
	 SLP tree size...  */
      if (is_a <bb_vec_info> (vinfo)
	  && !matches[0]
	  /* ???  Rejecting patterns this way doesn't work.  We'd have to
	     do extra work to cancel the pattern so the uses see the
	     scalar version.  */
	  && !is_pattern_stmt_p (stmt_info)
	  && !oprnd_info->any_pattern
	  && vect_update_all_shared_vectypes (oprnd_info->def_stmts))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Building vector operands from scalars\n");
	  this_tree_size++;
	  child = vect_create_new_slp_node (oprnd_info->def_stmts);
	  SLP_TREE_DEF_TYPE (child) = vect_external_def;
	  SLP_TREE_SCALAR_OPS (child) = oprnd_info->ops;
	  children.safe_push (child);
	  oprnd_info->ops = vNULL;
	  oprnd_info->def_stmts = vNULL;
	  continue;
	}

      /* If the SLP build for operand zero failed and operand zero
	 and one can be commutated try that for the scalar stmts
	 that failed the match.  */
      if (i == 0
	  /* A first scalar stmt mismatch signals a fatal mismatch.  */
	  && matches[0]
	  /* ???  For COND_EXPRs we can swap the comparison operands
	     as well as the arms under some constraints.  */
	  && nops == 2
	  && oprnds_info[1]->first_dt == vect_internal_def
	  && is_gimple_assign (stmt_info->stmt)
	  /* Swapping operands for reductions breaks assumptions later on.  */
	  && STMT_VINFO_DEF_TYPE (stmt_info) != vect_reduction_def
	  && STMT_VINFO_DEF_TYPE (stmt_info) != vect_double_reduction_def
	  /* Do so only if the number of not successful permutes was nor more
	     than a cut-ff as re-trying the recursive match on
	     possibly each level of the tree would expose exponential
	     behavior.  */
	  && *npermutes < 4)
	{
	  /* See whether we can swap the matching or the non-matching
	     stmt operands.  */
	  bool swap_not_matching = true;
	  do
	    {
	      for (j = 0; j < group_size; ++j)
		{
		  if (matches[j] != !swap_not_matching)
		    continue;
		  stmt_vec_info stmt_info = stmts[j];
		  /* Verify if we can swap operands of this stmt.  */
		  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
		  if (!stmt
		      || !commutative_tree_code (gimple_assign_rhs_code (stmt)))
		    {
		      if (!swap_not_matching)
			goto fail;
		      swap_not_matching = false;
		      break;
		    }
		}
	    }
	  while (j != group_size);

	  /* Swap mismatched definition stmts.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Re-trying with swapped operands of stmts ");
	  for (j = 0; j < group_size; ++j)
	    if (matches[j] == !swap_not_matching)
	      {
		std::swap (oprnds_info[0]->def_stmts[j],
			   oprnds_info[1]->def_stmts[j]);
		std::swap (oprnds_info[0]->ops[j],
			   oprnds_info[1]->ops[j]);
		if (dump_enabled_p ())
		  dump_printf (MSG_NOTE, "%d ", j);
	      }
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "\n");
	  /* And try again with scratch 'matches' ... */
	  bool *tem = XALLOCAVEC (bool, group_size);
	  if ((child = vect_build_slp_tree (vinfo, oprnd_info->def_stmts,
					    group_size, &this_max_nunits,
					    tem, npermutes,
					    &this_tree_size, bst_map)) != NULL)
	    {
	      /* If we have all children of a non-unary child built up from
		 scalars then just throw that away and build it up this node
		 from scalars.  */
	      if (is_a <bb_vec_info> (vinfo)
		  && SLP_TREE_CHILDREN (child).length () > 1
		  /* ???  Rejecting patterns this way doesn't work.  We'd have
		     to do extra work to cancel the pattern so the uses see the
		     scalar version.  */
		  && !oprnd_info->any_pattern)
		{
		  unsigned int j;
		  slp_tree grandchild;

		  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		    if (SLP_TREE_DEF_TYPE (grandchild) != vect_external_def)
		      break;
		  if (!grandchild
		      && (vect_update_all_shared_vectypes
			  (oprnd_info->def_stmts)))
		    {
		      /* Roll back.  */
		      this_tree_size = old_tree_size;
		      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
			vect_free_slp_tree (grandchild, false);
		      SLP_TREE_CHILDREN (child).truncate (0);

		      if (dump_enabled_p ())
			dump_printf_loc (MSG_NOTE, vect_location,
					 "Building parent vector operands from "
					 "scalars instead\n");
		      oprnd_info->def_stmts = vNULL;
		      SLP_TREE_DEF_TYPE (child) = vect_external_def;
		      SLP_TREE_SCALAR_OPS (child) = oprnd_info->ops;
		      oprnd_info->ops = vNULL;
		      ++this_tree_size;
		      children.safe_push (child);
		      continue;
		    }
		}

	      oprnd_info->def_stmts = vNULL;
	      children.safe_push (child);
	      continue;
	    }

	  ++*npermutes;
	}

fail:
      gcc_assert (child == NULL);
      FOR_EACH_VEC_ELT (children, j, child)
	vect_free_slp_tree (child, false);
      vect_free_oprnd_info (oprnds_info);
      return NULL;
    }

  vect_free_oprnd_info (oprnds_info);

  *tree_size += this_tree_size + 1;
  *max_nunits = this_max_nunits;

  node = vect_create_new_slp_node (stmts);
  SLP_TREE_TWO_OPERATORS (node) = two_operators;
  SLP_TREE_CHILDREN (node).splice (children);
  return node;
}

/* Dump a slp tree NODE using flags specified in DUMP_KIND.  */

static void
vect_print_slp_tree (dump_flags_t dump_kind, dump_location_t loc,
		     slp_tree node, hash_set<slp_tree> &visited)
{
  unsigned i;
  stmt_vec_info stmt_info;
  slp_tree child;
  tree op;

  if (visited.add (node))
    return;

  dump_metadata_t metadata (dump_kind, loc.get_impl_location ());
  dump_user_location_t user_loc = loc.get_user_location ();
  dump_printf_loc (metadata, user_loc, "node%s %p (max_nunits=%u)\n",
		   SLP_TREE_DEF_TYPE (node) == vect_external_def
		   ? " (external)"
		   : (SLP_TREE_DEF_TYPE (node) == vect_constant_def
		      ? " (constant)"
		      : ""), node,
		   estimated_poly_value (node->max_nunits));
  if (SLP_TREE_SCALAR_STMTS (node).exists ())
    FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
      dump_printf_loc (metadata, user_loc, "\tstmt %u %G", i, stmt_info->stmt);
  else
    {
      dump_printf_loc (metadata, user_loc, "\t{ ");
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (node), i, op)
	dump_printf (metadata, "%T%s ", op,
		     i < SLP_TREE_SCALAR_OPS (node).length () - 1 ? "," : "");
      dump_printf (metadata, "}\n");
    }
  if (SLP_TREE_CHILDREN (node).is_empty ())
    return;
  dump_printf_loc (metadata, user_loc, "\tchildren");
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    dump_printf (dump_kind, " %p", (void *)child);
  dump_printf (dump_kind, "\n");
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_print_slp_tree (dump_kind, loc, child, visited);
}

static void
vect_print_slp_tree (dump_flags_t dump_kind, dump_location_t loc,
		     slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_print_slp_tree (dump_kind, loc, node, visited);
}

/* Mark the tree rooted at NODE with PURE_SLP.  */

static void
vect_mark_slp_stmts (slp_tree node, hash_set<slp_tree> &visited)
{
  int i;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    STMT_SLP_TYPE (stmt_info) = pure_slp;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_mark_slp_stmts (child, visited);
}

static void
vect_mark_slp_stmts (slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_mark_slp_stmts (node, visited);
}

/* Mark the statements of the tree rooted at NODE as relevant (vect_used).  */

static void
vect_mark_slp_stmts_relevant (slp_tree node, hash_set<slp_tree> &visited)
{
  int i;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      gcc_assert (!STMT_VINFO_RELEVANT (stmt_info)
                  || STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope);
      STMT_VINFO_RELEVANT (stmt_info) = vect_used_in_scope;
    }

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_mark_slp_stmts_relevant (child, visited);
}

static void
vect_mark_slp_stmts_relevant (slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_mark_slp_stmts_relevant (node, visited);
}


/* Rearrange the statements of NODE according to PERMUTATION.  */

static void
vect_slp_rearrange_stmts (slp_tree node, unsigned int group_size,
                          vec<unsigned> permutation,
			  hash_set<slp_tree> &visited)
{
  unsigned int i;
  slp_tree child;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_slp_rearrange_stmts (child, group_size, permutation, visited);

  if (SLP_TREE_SCALAR_STMTS (node).exists ())
    {
      gcc_assert (group_size == SLP_TREE_SCALAR_STMTS (node).length ());
      vec<stmt_vec_info> tmp_stmts;
      tmp_stmts.create (group_size);
      tmp_stmts.quick_grow (group_size);
      stmt_vec_info stmt_info;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
	tmp_stmts[permutation[i]] = stmt_info;
      SLP_TREE_SCALAR_STMTS (node).release ();
      SLP_TREE_SCALAR_STMTS (node) = tmp_stmts;
    }
  if (SLP_TREE_SCALAR_OPS (node).exists ())
    {
      gcc_assert (group_size == SLP_TREE_SCALAR_OPS (node).length ());
      vec<tree> tmp_ops;
      tmp_ops.create (group_size);
      tmp_ops.quick_grow (group_size);
      tree op;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (node), i, op)
	tmp_ops[permutation[i]] = op;
      SLP_TREE_SCALAR_OPS (node).release ();
      SLP_TREE_SCALAR_OPS (node) = tmp_ops;
    }
}


/* Attempt to reorder stmts in a reduction chain so that we don't
   require any load permutation.  Return true if that was possible,
   otherwise return false.  */

static bool
vect_attempt_slp_rearrange_stmts (slp_instance slp_instn)
{
  unsigned int group_size = SLP_INSTANCE_GROUP_SIZE (slp_instn);
  unsigned int i, j;
  unsigned int lidx;
  slp_tree node, load;

  /* Compare all the permutation sequences to the first one.  We know
     that at least one load is permuted.  */
  node = SLP_INSTANCE_LOADS (slp_instn)[0];
  if (!node->load_permutation.exists ())
    return false;
  for (i = 1; SLP_INSTANCE_LOADS (slp_instn).iterate (i, &load); ++i)
    {
      if (!load->load_permutation.exists ())
	return false;
      FOR_EACH_VEC_ELT (load->load_permutation, j, lidx)
	if (lidx != node->load_permutation[j])
	  return false;
    }

  /* Check that the loads in the first sequence are different and there
     are no gaps between them.  */
  auto_sbitmap load_index (group_size);
  bitmap_clear (load_index);
  FOR_EACH_VEC_ELT (node->load_permutation, i, lidx)
    {
      if (lidx >= group_size)
	return false;
      if (bitmap_bit_p (load_index, lidx))
	return false;

      bitmap_set_bit (load_index, lidx);
    }
  for (i = 0; i < group_size; i++)
    if (!bitmap_bit_p (load_index, i))
      return false;

  /* This permutation is valid for reduction.  Since the order of the
     statements in the nodes is not important unless they are memory
     accesses, we can rearrange the statements in all the nodes
     according to the order of the loads.  */
  hash_set<slp_tree> visited;
  vect_slp_rearrange_stmts (SLP_INSTANCE_TREE (slp_instn), group_size,
			    node->load_permutation, visited);

  /* We are done, no actual permutations need to be generated.  */
  poly_uint64 unrolling_factor = SLP_INSTANCE_UNROLLING_FACTOR (slp_instn);
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    {
      stmt_vec_info first_stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
      first_stmt_info = DR_GROUP_FIRST_ELEMENT (first_stmt_info);
      /* But we have to keep those permutations that are required because
         of handling of gaps.  */
      if (known_eq (unrolling_factor, 1U)
	  || (group_size == DR_GROUP_SIZE (first_stmt_info)
	      && DR_GROUP_GAP (first_stmt_info) == 0))
	SLP_TREE_LOAD_PERMUTATION (node).release ();
      else
	for (j = 0; j < SLP_TREE_LOAD_PERMUTATION (node).length (); ++j)
	  SLP_TREE_LOAD_PERMUTATION (node)[j] = j;
    }

  return true;
}

/* Gather loads in the SLP graph NODE and populate the INST loads array.  */

static void
vect_gather_slp_loads (slp_instance inst, slp_tree node,
		       hash_set<slp_tree> &visited)
{
  if (visited.add (node))
    return;

  if (SLP_TREE_CHILDREN (node).length () == 0)
    {
      if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
	return;
      stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
	  && DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
	SLP_INSTANCE_LOADS (inst).safe_push (node);
    }
  else
    {
      unsigned i;
      slp_tree child;
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
	vect_gather_slp_loads (inst, child, visited);
    }
}

static void
vect_gather_slp_loads (slp_instance inst, slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_gather_slp_loads (inst, node, visited);
}

/* Check if the required load permutations in the SLP instance
   SLP_INSTN are supported.  */

static bool
vect_supported_load_permutation_p (slp_instance slp_instn)
{
  unsigned int group_size = SLP_INSTANCE_GROUP_SIZE (slp_instn);
  unsigned int i, j, k, next;
  slp_tree node;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "Load permutation ");
      FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
	if (node->load_permutation.exists ())
	  FOR_EACH_VEC_ELT (node->load_permutation, j, next)
	    dump_printf (MSG_NOTE, "%d ", next);
	else
	  for (k = 0; k < group_size; ++k)
	    dump_printf (MSG_NOTE, "%d ", k);
      dump_printf (MSG_NOTE, "\n");
    }

  /* In case of reduction every load permutation is allowed, since the order
     of the reduction statements is not important (as opposed to the case of
     grouped stores).  The only condition we need to check is that all the
     load nodes are of the same size and have the same permutation (and then
     rearrange all the nodes of the SLP instance according to this 
     permutation).  */

  /* Check that all the load nodes are of the same size.  */
  /* ???  Can't we assert this? */
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    if (SLP_TREE_SCALAR_STMTS (node).length () != (unsigned) group_size)
      return false;

  node = SLP_INSTANCE_TREE (slp_instn);
  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];

  /* Reduction (there are no data-refs in the root).
     In reduction chain the order of the loads is not important.  */
  if (!STMT_VINFO_DATA_REF (stmt_info)
      && !REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    vect_attempt_slp_rearrange_stmts (slp_instn);

  /* In basic block vectorization we allow any subchain of an interleaving
     chain.
     FORNOW: not supported in loop SLP because of realignment compications.  */
  if (STMT_VINFO_BB_VINFO (stmt_info))
    {
      /* Check whether the loads in an instance form a subchain and thus
         no permutation is necessary.  */
      FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
        {
	  if (!SLP_TREE_LOAD_PERMUTATION (node).exists ())
	    continue;
	  bool subchain_p = true;
	  stmt_vec_info next_load_info = NULL;
	  stmt_vec_info load_info;
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load_info)
	    {
	      if (j != 0
		  && (next_load_info != load_info
		      || DR_GROUP_GAP (load_info) != 1))
		{
		  subchain_p = false;
		  break;
		}
	      next_load_info = DR_GROUP_NEXT_ELEMENT (load_info);
	    }
	  if (subchain_p)
	    SLP_TREE_LOAD_PERMUTATION (node).release ();
	  else
	    {
	      stmt_vec_info group_info = SLP_TREE_SCALAR_STMTS (node)[0];
	      group_info = DR_GROUP_FIRST_ELEMENT (group_info);
	      unsigned HOST_WIDE_INT nunits;
	      unsigned k, maxk = 0;
	      FOR_EACH_VEC_ELT (SLP_TREE_LOAD_PERMUTATION (node), j, k)
		if (k > maxk)
		  maxk = k;
	      /* In BB vectorization we may not actually use a loaded vector
		 accessing elements in excess of DR_GROUP_SIZE.  */
	      tree vectype = STMT_VINFO_VECTYPE (group_info);
	      if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&nunits)
		  || maxk >= (DR_GROUP_SIZE (group_info) & ~(nunits - 1)))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "BB vectorization with gaps at the end of "
				     "a load is not supported\n");
		  return false;
		}

	      /* Verify the permutation can be generated.  */
	      vec<tree> tem;
	      unsigned n_perms;
	      if (!vect_transform_slp_perm_load (node, tem, NULL,
						 1, slp_instn, true, &n_perms))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				     vect_location,
				     "unsupported load permutation\n");
		  return false;
		}
	    }
        }
      return true;
    }

  /* For loop vectorization verify we can generate the permutation.  Be
     conservative about the vectorization factor, there are permutations
     that will use three vector inputs only starting from a specific factor
     and the vectorization factor is not yet final.
     ???  The SLP instance unrolling factor might not be the maximum one.  */
  unsigned n_perms;
  poly_uint64 test_vf
    = force_common_multiple (SLP_INSTANCE_UNROLLING_FACTOR (slp_instn),
			     LOOP_VINFO_VECT_FACTOR
			     (STMT_VINFO_LOOP_VINFO (stmt_info)));
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    if (node->load_permutation.exists ()
	&& !vect_transform_slp_perm_load (node, vNULL, NULL, test_vf,
					  slp_instn, true, &n_perms))
      return false;

  return true;
}


/* Find the last store in SLP INSTANCE.  */

stmt_vec_info
vect_find_last_scalar_stmt_in_slp (slp_tree node)
{
  stmt_vec_info last = NULL;
  stmt_vec_info stmt_vinfo;

  for (int i = 0; SLP_TREE_SCALAR_STMTS (node).iterate (i, &stmt_vinfo); i++)
    {
      stmt_vinfo = vect_orig_stmt (stmt_vinfo);
      last = last ? get_later_stmt (stmt_vinfo, last) : stmt_vinfo;
    }

  return last;
}

/* Splits a group of stores, currently beginning at FIRST_VINFO, into
   two groups: one (still beginning at FIRST_VINFO) of size GROUP1_SIZE
   (also containing the first GROUP1_SIZE stmts, since stores are
   consecutive), the second containing the remainder.
   Return the first stmt in the second group.  */

static stmt_vec_info
vect_split_slp_store_group (stmt_vec_info first_vinfo, unsigned group1_size)
{
  gcc_assert (DR_GROUP_FIRST_ELEMENT (first_vinfo) == first_vinfo);
  gcc_assert (group1_size > 0);
  int group2_size = DR_GROUP_SIZE (first_vinfo) - group1_size;
  gcc_assert (group2_size > 0);
  DR_GROUP_SIZE (first_vinfo) = group1_size;

  stmt_vec_info stmt_info = first_vinfo;
  for (unsigned i = group1_size; i > 1; i--)
    {
      stmt_info = DR_GROUP_NEXT_ELEMENT (stmt_info);
      gcc_assert (DR_GROUP_GAP (stmt_info) == 1);
    }
  /* STMT is now the last element of the first group.  */
  stmt_vec_info group2 = DR_GROUP_NEXT_ELEMENT (stmt_info);
  DR_GROUP_NEXT_ELEMENT (stmt_info) = 0;

  DR_GROUP_SIZE (group2) = group2_size;
  for (stmt_info = group2; stmt_info;
       stmt_info = DR_GROUP_NEXT_ELEMENT (stmt_info))
    {
      DR_GROUP_FIRST_ELEMENT (stmt_info) = group2;
      gcc_assert (DR_GROUP_GAP (stmt_info) == 1);
    }

  /* For the second group, the DR_GROUP_GAP is that before the original group,
     plus skipping over the first vector.  */
  DR_GROUP_GAP (group2) = DR_GROUP_GAP (first_vinfo) + group1_size;

  /* DR_GROUP_GAP of the first group now has to skip over the second group too.  */
  DR_GROUP_GAP (first_vinfo) += group2_size;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "Split group into %d and %d\n",
		     group1_size, group2_size);

  return group2;
}

/* Calculate the unrolling factor for an SLP instance with GROUP_SIZE
   statements and a vector of NUNITS elements.  */

static poly_uint64
calculate_unrolling_factor (poly_uint64 nunits, unsigned int group_size)
{
  return exact_div (common_multiple (nunits, group_size), group_size);
}

/* Analyze an SLP instance starting from a group of grouped stores.  Call
   vect_build_slp_tree to build a tree of packed stmts if possible.
   Return FALSE if it's impossible to SLP any stmt in the loop.  */

static bool
vect_analyze_slp_instance (vec_info *vinfo,
			   scalar_stmts_to_slp_tree_map_t *bst_map,
			   stmt_vec_info stmt_info, unsigned max_tree_size)
{
  slp_instance new_instance;
  slp_tree node;
  unsigned int group_size;
  tree vectype, scalar_type = NULL_TREE;
  unsigned int i;
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  vec<stmt_vec_info> scalar_stmts;
  bool constructor = false;

  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      scalar_type = TREE_TYPE (DR_REF (dr));
      group_size = DR_GROUP_SIZE (stmt_info);
      vectype = get_vectype_for_scalar_type (vinfo, scalar_type, group_size);
    }
  else if (!dr && REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    {
      gcc_assert (is_a <loop_vec_info> (vinfo));
      vectype = STMT_VINFO_VECTYPE (stmt_info);
      group_size = REDUC_GROUP_SIZE (stmt_info);
    }
  else if (is_gimple_assign (stmt_info->stmt)
	    && gimple_assign_rhs_code (stmt_info->stmt) == CONSTRUCTOR)
    {
      vectype = TREE_TYPE (gimple_assign_rhs1 (stmt_info->stmt));
      group_size = CONSTRUCTOR_NELTS (gimple_assign_rhs1 (stmt_info->stmt));
      constructor = true;
    }
  else
    {
      gcc_assert (is_a <loop_vec_info> (vinfo));
      vectype = STMT_VINFO_VECTYPE (stmt_info);
      group_size = as_a <loop_vec_info> (vinfo)->reductions.length ();
    }

  if (!vectype)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Build SLP failed: unsupported data-type %T\n",
			 scalar_type);

      return false;
    }
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* Create a node (a root of the SLP tree) for the packed grouped stores.  */
  scalar_stmts.create (group_size);
  stmt_vec_info next_info = stmt_info;
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      /* Collect the stores and store them in SLP_TREE_SCALAR_STMTS.  */
      while (next_info)
        {
	  scalar_stmts.safe_push (vect_stmt_to_vectorize (next_info));
	  next_info = DR_GROUP_NEXT_ELEMENT (next_info);
        }
    }
  else if (!dr && REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    {
      /* Collect the reduction stmts and store them in
	 SLP_TREE_SCALAR_STMTS.  */
      while (next_info)
        {
	  scalar_stmts.safe_push (vect_stmt_to_vectorize (next_info));
	  next_info = REDUC_GROUP_NEXT_ELEMENT (next_info);
        }
      /* Mark the first element of the reduction chain as reduction to properly
	 transform the node.  In the reduction analysis phase only the last
	 element of the chain is marked as reduction.  */
      STMT_VINFO_DEF_TYPE (stmt_info)
	= STMT_VINFO_DEF_TYPE (scalar_stmts.last ());
      STMT_VINFO_REDUC_DEF (vect_orig_stmt (stmt_info))
	= STMT_VINFO_REDUC_DEF (vect_orig_stmt (scalar_stmts.last ()));
    }
  else if (constructor)
    {
      tree rhs = gimple_assign_rhs1 (stmt_info->stmt);
      tree val;
      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), i, val)
	{
	  if (TREE_CODE (val) == SSA_NAME)
	    {
	      gimple* def = SSA_NAME_DEF_STMT (val);
	      stmt_vec_info def_info = vinfo->lookup_stmt (def);
	      /* Value is defined in another basic block.  */
	      if (!def_info)
		return false;
	      scalar_stmts.safe_push (def_info);
	    }
	  else
	    return false;
	}
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Analyzing vectorizable constructor: %G\n",
			 stmt_info->stmt);
    }
  else
    {
      /* Collect reduction statements.  */
      vec<stmt_vec_info> reductions = as_a <loop_vec_info> (vinfo)->reductions;
      for (i = 0; reductions.iterate (i, &next_info); i++)
	scalar_stmts.safe_push (next_info);
    }

  /* Build the tree for the SLP instance.  */
  bool *matches = XALLOCAVEC (bool, group_size);
  unsigned npermutes = 0;
  poly_uint64 max_nunits = nunits;
  unsigned tree_size = 0;
  node = vect_build_slp_tree (vinfo, scalar_stmts, group_size,
			      &max_nunits, matches, &npermutes,
			      &tree_size, bst_map);
  if (node != NULL)
    {
      /* If this is a reduction chain with a conversion in front
         amend the SLP tree with a node for that.  */
      if (!dr
	  && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
	  && STMT_VINFO_DEF_TYPE (stmt_info) != vect_reduction_def)
	{
	  /* Get at the conversion stmt - we know it's the single use
	     of the last stmt of the reduction chain.  */
	  gimple *tem = vect_orig_stmt (scalar_stmts[group_size - 1])->stmt;
	  use_operand_p use_p;
	  gimple *use_stmt;
	  bool r = single_imm_use (gimple_assign_lhs (tem), &use_p, &use_stmt);
	  gcc_assert (r);
	  next_info = vinfo->lookup_stmt (use_stmt);
	  next_info = vect_stmt_to_vectorize (next_info);
	  scalar_stmts = vNULL;
	  scalar_stmts.create (group_size);
	  for (unsigned i = 0; i < group_size; ++i)
	    scalar_stmts.quick_push (next_info);
	  slp_tree conv = vect_create_new_slp_node (scalar_stmts);
	  SLP_TREE_CHILDREN (conv).quick_push (node);
	  node = conv;
	  /* We also have to fake this conversion stmt as SLP reduction group
	     so we don't have to mess with too much code elsewhere.  */
	  REDUC_GROUP_FIRST_ELEMENT (next_info) = next_info;
	  REDUC_GROUP_NEXT_ELEMENT (next_info) = NULL;
	}

      /* Calculate the unrolling factor based on the smallest type.  */
      poly_uint64 unrolling_factor
	= calculate_unrolling_factor (max_nunits, group_size);

      if (maybe_ne (unrolling_factor, 1U)
	  && is_a <bb_vec_info> (vinfo))
	{
	  unsigned HOST_WIDE_INT const_max_nunits;
	  if (!max_nunits.is_constant (&const_max_nunits)
	      || const_max_nunits > group_size)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: store group "
				 "size not a multiple of the vector size "
				 "in basic block SLP\n");
	      vect_free_slp_tree (node, false);
	      return false;
	    }
	  /* Fatal mismatch.  */
	  matches[group_size / const_max_nunits * const_max_nunits] = false;
	  vect_free_slp_tree (node, false);
	}
      else
	{
	  /* Create a new SLP instance.  */
	  new_instance = XNEW (class _slp_instance);
	  SLP_INSTANCE_TREE (new_instance) = node;
	  SLP_INSTANCE_GROUP_SIZE (new_instance) = group_size;
	  SLP_INSTANCE_UNROLLING_FACTOR (new_instance) = unrolling_factor;
	  SLP_INSTANCE_LOADS (new_instance) = vNULL;
	  SLP_INSTANCE_ROOT_STMT (new_instance) = constructor ? stmt_info : NULL;

	  vect_gather_slp_loads (new_instance, node);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "SLP size %u vs. limit %u.\n",
			     tree_size, max_tree_size);

	  /* Compute the load permutation.  */
	  slp_tree load_node;
	  bool loads_permuted = false;
	  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (new_instance), i, load_node)
	    {
	      vec<unsigned> load_permutation;
	      int j;
	      stmt_vec_info load_info;
	      bool this_load_permuted = false;
	      load_permutation.create (group_size);
	      stmt_vec_info first_stmt_info = DR_GROUP_FIRST_ELEMENT
		  (SLP_TREE_SCALAR_STMTS (load_node)[0]);
	      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (load_node), j, load_info)
		{
		  int load_place = vect_get_place_in_interleaving_chain
		      (load_info, first_stmt_info);
		  gcc_assert (load_place != -1);
		  if (load_place != j)
		    this_load_permuted = true;
		  load_permutation.safe_push (load_place);
		}
	      if (!this_load_permuted
		  /* The load requires permutation when unrolling exposes
		     a gap either because the group is larger than the SLP
		     group-size or because there is a gap between the groups.  */
		  && (known_eq (unrolling_factor, 1U)
		      || (group_size == DR_GROUP_SIZE (first_stmt_info)
			  && DR_GROUP_GAP (first_stmt_info) == 0)))
		{
		  load_permutation.release ();
		  continue;
		}
	      SLP_TREE_LOAD_PERMUTATION (load_node) = load_permutation;
	      loads_permuted = true;
	    }

	  if (loads_permuted)
	    {
	      if (!vect_supported_load_permutation_p (new_instance))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: unsupported load "
				     "permutation %G", stmt_info->stmt);
		  vect_free_slp_instance (new_instance, false);
		  return false;
		}
	    }

	  /* If the loads and stores can be handled with load/store-lan
	     instructions do not generate this SLP instance.  */
	  if (is_a <loop_vec_info> (vinfo)
	      && loads_permuted
	      && dr && vect_store_lanes_supported (vectype, group_size, false))
	    {
	      slp_tree load_node;
	      FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (new_instance), i, load_node)
		{
		  stmt_vec_info stmt_vinfo = DR_GROUP_FIRST_ELEMENT
		      (SLP_TREE_SCALAR_STMTS (load_node)[0]);
		  /* Use SLP for strided accesses (or if we can't load-lanes).  */
		  if (STMT_VINFO_STRIDED_P (stmt_vinfo)
		      || ! vect_load_lanes_supported
		      (STMT_VINFO_VECTYPE (stmt_vinfo),
		       DR_GROUP_SIZE (stmt_vinfo), false))
		    break;
		}
	      if (i == SLP_INSTANCE_LOADS (new_instance).length ())
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Built SLP cancelled: can use "
				     "load/store-lanes\n");
		  vect_free_slp_instance (new_instance, false);
		  return false;
		}
	    }

	  vinfo->slp_instances.safe_push (new_instance);

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "Final SLP tree for instance:\n");
	      vect_print_slp_tree (MSG_NOTE, vect_location, node);
	    }

	  return true;
	}
    }
  else
    {
      /* Failed to SLP.  */
      /* Free the allocated memory.  */
      scalar_stmts.release ();
    }

  /* For basic block SLP, try to break the group up into multiples of the
     vector size.  */
  unsigned HOST_WIDE_INT const_nunits;
  if (is_a <bb_vec_info> (vinfo)
      && STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && DR_GROUP_FIRST_ELEMENT (stmt_info)
      && nunits.is_constant (&const_nunits))
    {
      /* We consider breaking the group only on VF boundaries from the existing
	 start.  */
      for (i = 0; i < group_size; i++)
	if (!matches[i]) break;

      if (i >= const_nunits && i < group_size)
	{
	  /* Split into two groups at the first vector boundary before i.  */
	  gcc_assert ((const_nunits & (const_nunits - 1)) == 0);
	  unsigned group1_size = i & ~(const_nunits - 1);

	  stmt_vec_info rest = vect_split_slp_store_group (stmt_info,
							   group1_size);
	  bool res = vect_analyze_slp_instance (vinfo, bst_map, stmt_info,
						max_tree_size);
	  /* If the first non-match was in the middle of a vector,
	     skip the rest of that vector.  */
	  if (group1_size < i)
	    {
	      i = group1_size + const_nunits;
	      if (i < group_size)
		rest = vect_split_slp_store_group (rest, const_nunits);
	    }
	  if (i < group_size)
	    res |= vect_analyze_slp_instance (vinfo, bst_map,
					      rest, max_tree_size);
	  return res;
	}
      /* Even though the first vector did not all match, we might be able to SLP
	 (some) of the remainder.  FORNOW ignore this possibility.  */
    }

  return false;
}


/* Check if there are stmts in the loop can be vectorized using SLP.  Build SLP
   trees of packed scalar stmts if SLP is possible.  */

opt_result
vect_analyze_slp (vec_info *vinfo, unsigned max_tree_size)
{
  unsigned int i;
  stmt_vec_info first_element;

  DUMP_VECT_SCOPE ("vect_analyze_slp");

  scalar_stmts_to_slp_tree_map_t *bst_map
    = new scalar_stmts_to_slp_tree_map_t ();

  /* Find SLP sequences starting from groups of grouped stores.  */
  FOR_EACH_VEC_ELT (vinfo->grouped_stores, i, first_element)
    vect_analyze_slp_instance (vinfo, bst_map, first_element, max_tree_size);

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      if (loop_vinfo->reduction_chains.length () > 0)
	{
	  /* Find SLP sequences starting from reduction chains.  */
	  FOR_EACH_VEC_ELT (loop_vinfo->reduction_chains, i, first_element)
	    if (! vect_analyze_slp_instance (vinfo, bst_map, first_element,
					     max_tree_size))
	      {
		/* Dissolve reduction chain group.  */
		stmt_vec_info vinfo = first_element;
		stmt_vec_info last = NULL;
		while (vinfo)
		  {
		    stmt_vec_info next = REDUC_GROUP_NEXT_ELEMENT (vinfo);
		    REDUC_GROUP_FIRST_ELEMENT (vinfo) = NULL;
		    REDUC_GROUP_NEXT_ELEMENT (vinfo) = NULL;
		    last = vinfo;
		    vinfo = next;
		  }
		STMT_VINFO_DEF_TYPE (first_element) = vect_internal_def;
		/* It can be still vectorized as part of an SLP reduction.  */
		loop_vinfo->reductions.safe_push (last);
	      }
	}

      /* Find SLP sequences starting from groups of reductions.  */
      if (loop_vinfo->reductions.length () > 1)
	vect_analyze_slp_instance (vinfo, bst_map, loop_vinfo->reductions[0],
				   max_tree_size);
    }

  /* The map keeps a reference on SLP nodes built, release that.  */
  for (scalar_stmts_to_slp_tree_map_t::iterator it = bst_map->begin ();
       it != bst_map->end (); ++it)
    if ((*it).second)
      vect_free_slp_tree ((*it).second, false);
  delete bst_map;

  return opt_result::success ();
}


/* For each possible SLP instance decide whether to SLP it and calculate overall
   unrolling factor needed to SLP the loop.  Return TRUE if decided to SLP at
   least one instance.  */

bool
vect_make_slp_decision (loop_vec_info loop_vinfo)
{
  unsigned int i;
  poly_uint64 unrolling_factor = 1;
  vec<slp_instance> slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;
  int decided_to_slp = 0;

  DUMP_VECT_SCOPE ("vect_make_slp_decision");

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      /* FORNOW: SLP if you can.  */
      /* All unroll factors have the form:

	   GET_MODE_SIZE (vinfo->vector_mode) * X

	 for some rational X, so they must have a common multiple.  */
      unrolling_factor
	= force_common_multiple (unrolling_factor,
				 SLP_INSTANCE_UNROLLING_FACTOR (instance));

      /* Mark all the stmts that belong to INSTANCE as PURE_SLP stmts.  Later we
	 call vect_detect_hybrid_slp () to find stmts that need hybrid SLP and
	 loop-based vectorization.  Such stmts will be marked as HYBRID.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance));
      decided_to_slp++;
    }

  LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo) = unrolling_factor;

  if (decided_to_slp && dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "Decided to SLP %d instances. Unrolling factor ",
		       decided_to_slp);
      dump_dec (MSG_NOTE, unrolling_factor);
      dump_printf (MSG_NOTE, "\n");
    }

  return (decided_to_slp > 0);
}


/* Find stmts that must be both vectorized and SLPed (since they feed stmts that
   can't be SLPed) in the tree rooted at NODE.  Mark such stmts as HYBRID.  */

static void
vect_detect_hybrid_slp_stmts (slp_tree node, unsigned i, slp_vect_type stype,
			      hash_map<slp_tree, unsigned> &visited)
{
  stmt_vec_info stmt_vinfo = SLP_TREE_SCALAR_STMTS (node)[i];
  imm_use_iterator imm_iter;
  gimple *use_stmt;
  stmt_vec_info use_vinfo;
  slp_tree child;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  int j;

  /* We need to union stype over the incoming graph edges but we still
     want to limit recursion to stay O(N+E).  */
  unsigned visited_cnt = ++visited.get_or_insert (node);
  gcc_assert (visited_cnt <= node->refcnt);
  bool only_edge = (visited_cnt != node->refcnt);

  /* Propagate hybrid down the SLP tree.  */
  if (stype == hybrid)
    ;
  else if (HYBRID_SLP_STMT (stmt_vinfo))
    stype = hybrid;
  else if (!only_edge)
    {
      /* Check if a pure SLP stmt has uses in non-SLP stmts.  */
      gcc_checking_assert (PURE_SLP_STMT (stmt_vinfo));
      /* If we get a pattern stmt here we have to use the LHS of the
         original stmt for immediate uses.  */
      gimple *stmt = vect_orig_stmt (stmt_vinfo)->stmt;
      tree def;
      if (gimple_code (stmt) == GIMPLE_PHI)
	def = gimple_phi_result (stmt);
      else
	def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF);
      if (def)
	FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
	  {
	    use_vinfo = loop_vinfo->lookup_stmt (use_stmt);
	    if (!use_vinfo)
	      continue;
	    use_vinfo = vect_stmt_to_vectorize (use_vinfo);
	    if (!STMT_SLP_TYPE (use_vinfo)
		&& (STMT_VINFO_RELEVANT (use_vinfo)
		    || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (use_vinfo)))
		&& !(gimple_code (use_stmt) == GIMPLE_PHI
		     && STMT_VINFO_DEF_TYPE (use_vinfo) == vect_reduction_def))
	      {
		if (dump_enabled_p ())
		  dump_printf_loc (MSG_NOTE, vect_location, "use of SLP "
				   "def in non-SLP stmt: %G", use_stmt);
		stype = hybrid;
	      }
	  }
    }

  if (stype == hybrid
      && !HYBRID_SLP_STMT (stmt_vinfo))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "marking hybrid: %G",
			 stmt_vinfo->stmt);
      STMT_SLP_TYPE (stmt_vinfo) = hybrid;
    }

  if (!only_edge)
    FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
      if (SLP_TREE_DEF_TYPE (child) != vect_external_def
	  && SLP_TREE_DEF_TYPE (child) != vect_constant_def)
	vect_detect_hybrid_slp_stmts (child, i, stype, visited);
}

/* Helpers for vect_detect_hybrid_slp walking pattern stmt uses.  */

static tree
vect_detect_hybrid_slp_1 (tree *tp, int *, void *data)
{
  walk_stmt_info *wi = (walk_stmt_info *)data;
  loop_vec_info loop_vinfo = (loop_vec_info) wi->info;

  if (wi->is_lhs)
    return NULL_TREE;

  stmt_vec_info def_stmt_info = loop_vinfo->lookup_def (*tp);
  if (def_stmt_info && PURE_SLP_STMT (def_stmt_info))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "marking hybrid: %G",
			 def_stmt_info->stmt);
      STMT_SLP_TYPE (def_stmt_info) = hybrid;
    }

  return NULL_TREE;
}

static tree
vect_detect_hybrid_slp_2 (gimple_stmt_iterator *gsi, bool *handled,
			  walk_stmt_info *wi)
{
  loop_vec_info loop_vinfo = (loop_vec_info) wi->info;
  stmt_vec_info use_vinfo = loop_vinfo->lookup_stmt (gsi_stmt (*gsi));
  /* If the stmt is in a SLP instance then this isn't a reason
     to mark use definitions in other SLP instances as hybrid.  */
  if (! STMT_SLP_TYPE (use_vinfo)
      && (STMT_VINFO_RELEVANT (use_vinfo)
	  || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (use_vinfo)))
      && ! (gimple_code (gsi_stmt (*gsi)) == GIMPLE_PHI
	    && STMT_VINFO_DEF_TYPE (use_vinfo) == vect_reduction_def))
    ;
  else
    *handled = true;
  return NULL_TREE;
}

/* Find stmts that must be both vectorized and SLPed.  */

void
vect_detect_hybrid_slp (loop_vec_info loop_vinfo)
{
  unsigned int i;
  vec<slp_instance> slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;

  DUMP_VECT_SCOPE ("vect_detect_hybrid_slp");

  /* First walk all pattern stmt in the loop and mark defs of uses as
     hybrid because immediate uses in them are not recorded.  */
  for (i = 0; i < LOOP_VINFO_LOOP (loop_vinfo)->num_nodes; ++i)
    {
      basic_block bb = LOOP_VINFO_BBS (loop_vinfo)[i];
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (stmt);
	  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
	    {
	      walk_stmt_info wi;
	      memset (&wi, 0, sizeof (wi));
	      wi.info = loop_vinfo;
	      gimple_stmt_iterator gsi2
		= gsi_for_stmt (STMT_VINFO_RELATED_STMT (stmt_info)->stmt);
	      walk_gimple_stmt (&gsi2, vect_detect_hybrid_slp_2,
				vect_detect_hybrid_slp_1, &wi);
	      walk_gimple_seq (STMT_VINFO_PATTERN_DEF_SEQ (stmt_info),
			       vect_detect_hybrid_slp_2,
			       vect_detect_hybrid_slp_1, &wi);
	    }
	}
    }

  /* Then walk the SLP instance trees marking stmts with uses in
     non-SLP stmts as hybrid, also propagating hybrid down the
     SLP tree, collecting the above info on-the-fly.  */
  for (unsigned j = 0;; ++j)
    {
      hash_map<slp_tree, unsigned> visited;
      bool any = false;
      FOR_EACH_VEC_ELT (slp_instances, i, instance)
	if (j < SLP_INSTANCE_GROUP_SIZE (instance))
	  {
	    any = true;
	    vect_detect_hybrid_slp_stmts (SLP_INSTANCE_TREE (instance),
					  j, pure_slp, visited);
	  }
      if (!any)
	break;
    }
}


/* Initialize a bb_vec_info struct for the statements between
   REGION_BEGIN_IN (inclusive) and REGION_END_IN (exclusive).  */

_bb_vec_info::_bb_vec_info (gimple_stmt_iterator region_begin_in,
			    gimple_stmt_iterator region_end_in,
			    vec_info_shared *shared)
  : vec_info (vec_info::bb, init_cost (NULL), shared),
    bb (gsi_bb (region_begin_in)),
    region_begin (region_begin_in),
    region_end (region_end_in)
{
  gimple_stmt_iterator gsi;

  for (gsi = region_begin; gsi_stmt (gsi) != gsi_stmt (region_end);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      gimple_set_uid (stmt, 0);
      add_stmt (stmt);
    }

  bb->aux = this;
}


/* Free BB_VINFO struct, as well as all the stmt_vec_info structs of all the
   stmts in the basic block.  */

_bb_vec_info::~_bb_vec_info ()
{
  for (gimple_stmt_iterator si = region_begin;
       gsi_stmt (si) != gsi_stmt (region_end); gsi_next (&si))
    /* Reset region marker.  */
    gimple_set_uid (gsi_stmt (si), -1);

  bb->aux = NULL;
}

/* Subroutine of vect_slp_analyze_node_operations.  Handle the root of NODE,
   given then that child nodes have already been processed, and that
   their def types currently match their SLP node's def type.  */

static bool
vect_slp_analyze_node_operations_1 (vec_info *vinfo, slp_tree node,
				    slp_instance node_instance,
				    stmt_vector_for_cost *cost_vec)
{
  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
  gcc_assert (STMT_SLP_TYPE (stmt_info) != loop_vect);

  /* Calculate the number of vector statements to be created for the
     scalar stmts in this node.  For SLP reductions it is equal to the
     number of vector statements in the children (which has already been
     calculated by the recursive call).  Otherwise it is the number of
     scalar elements in one scalar iteration (DR_GROUP_SIZE) multiplied by
     VF divided by the number of elements in a vector.  */
  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    {
      for (unsigned i = 0; i < SLP_TREE_CHILDREN (node).length (); ++i)
	if (SLP_TREE_DEF_TYPE (SLP_TREE_CHILDREN (node)[i]) == vect_internal_def)
	  {
	    SLP_TREE_NUMBER_OF_VEC_STMTS (node)
	      = SLP_TREE_NUMBER_OF_VEC_STMTS (SLP_TREE_CHILDREN (node)[i]);
	    break;
	  }
    }
  else
    {
      poly_uint64 vf;
      if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
	vf = loop_vinfo->vectorization_factor;
      else
	vf = 1;
      unsigned int group_size = SLP_INSTANCE_GROUP_SIZE (node_instance);
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      SLP_TREE_NUMBER_OF_VEC_STMTS (node)
	= vect_get_num_vectors (vf * group_size, vectype);
    }

  bool dummy;
  return vect_analyze_stmt (stmt_info, &dummy, node, node_instance, cost_vec);
}

/* Try to build NODE from scalars, returning true on success.
   NODE_INSTANCE is the SLP instance that contains NODE.  */

static bool
vect_slp_convert_to_external (vec_info *vinfo, slp_tree node,
			      slp_instance node_instance)
{
  stmt_vec_info stmt_info;
  unsigned int i;

  if (!is_a <bb_vec_info> (vinfo)
      || node == SLP_INSTANCE_TREE (node_instance)
      || vect_contains_pattern_stmt_p (SLP_TREE_SCALAR_STMTS (node)))
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Building vector operands from scalars instead\n");

  /* Don't remove and free the child nodes here, since they could be
     referenced by other structures.  The analysis and scheduling phases
     (need to) ignore child nodes of anything that isn't vect_internal_def.  */
  unsigned int group_size = SLP_TREE_SCALAR_STMTS (node).length ();
  SLP_TREE_DEF_TYPE (node) = vect_external_def;
  SLP_TREE_SCALAR_OPS (node).safe_grow (group_size);
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      tree lhs = gimple_get_lhs (vect_orig_stmt (stmt_info)->stmt);
      SLP_TREE_SCALAR_OPS (node)[i] = lhs;
    }
  return true;
}

/* Analyze statements contained in SLP tree NODE after recursively analyzing
   the subtree.  NODE_INSTANCE contains NODE and VINFO contains INSTANCE.

   Return true if the operations are supported.  */

static bool
vect_slp_analyze_node_operations (vec_info *vinfo, slp_tree node,
				  slp_instance node_instance,
				  hash_set<slp_tree> &visited,
				  hash_set<slp_tree> &lvisited,
				  stmt_vector_for_cost *cost_vec)
{
  int i, j;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return true;

  /* If we already analyzed the exact same set of scalar stmts we're done.
     We share the generated vector stmts for those.
     The SLP graph is acyclic so not caching whether we failed or succeeded
     doesn't result in any issue since we throw away the lvisited set
     when we fail.  */
  if (visited.contains (node)
      || lvisited.add (node))
    return true;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (!vect_slp_analyze_node_operations (vinfo, child, node_instance,
					   visited, lvisited, cost_vec))
      return false;

  /* ???  We have to catch the case late where two first scalar stmts appear
     in multiple SLP children with different def type and fail.  Remember
     original def types first since SLP_TREE_DEF_TYPE doesn't necessarily
     match it when that is vect_internal_def.  */
  auto_vec<vect_def_type, 4> dt;
  dt.safe_grow (SLP_TREE_CHILDREN (node).length ());
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
    if (SLP_TREE_SCALAR_STMTS (child).length () != 0)
      dt[j] = STMT_VINFO_DEF_TYPE (SLP_TREE_SCALAR_STMTS (child)[0]);

  /* Push SLP node def-type to stmt operands.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def
	&& SLP_TREE_SCALAR_STMTS (child).length () != 0)
      STMT_VINFO_DEF_TYPE (SLP_TREE_SCALAR_STMTS (child)[0])
	= SLP_TREE_DEF_TYPE (child);

  /* Check everything worked out.  */
  bool res = true;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
      if (SLP_TREE_SCALAR_STMTS (child).length () != 0)
	{
	  if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
	    {
	      if (STMT_VINFO_DEF_TYPE (SLP_TREE_SCALAR_STMTS (child)[0])
		  != SLP_TREE_DEF_TYPE (child))
		res = false;
	    }
	  else if (STMT_VINFO_DEF_TYPE (SLP_TREE_SCALAR_STMTS (child)[0])
		   != dt[j])
	    res = false;
	}
  if (!res && dump_enabled_p ())
    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		     "not vectorized: same operand with different "
		     "def type in stmt.\n");

  if (res)
    res = vect_slp_analyze_node_operations_1 (vinfo, node, node_instance,
					      cost_vec);

  /* Restore def-types.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
    if (SLP_TREE_SCALAR_STMTS (child).length () != 0)
      STMT_VINFO_DEF_TYPE (SLP_TREE_SCALAR_STMTS (child)[0]) = dt[j];

  /* If this node can't be vectorized, try pruning the tree here rather
     than felling the whole thing.  */
  if (!res && vect_slp_convert_to_external (vinfo, node, node_instance))
    res = true;

  return res;
}


/* Analyze statements in SLP instances of VINFO.  Return true if the
   operations are supported. */

bool
vect_slp_analyze_operations (vec_info *vinfo)
{
  slp_instance instance;
  int i;

  DUMP_VECT_SCOPE ("vect_slp_analyze_operations");

  hash_set<slp_tree> visited;
  for (i = 0; vinfo->slp_instances.iterate (i, &instance); )
    {
      hash_set<slp_tree> lvisited;
      stmt_vector_for_cost cost_vec;
      cost_vec.create (2);
      if (!vect_slp_analyze_node_operations (vinfo,
					     SLP_INSTANCE_TREE (instance),
					     instance, visited, lvisited,
					     &cost_vec)
	  /* Instances with a root stmt require vectorized defs for the
	     SLP tree root.  */
	  || (SLP_INSTANCE_ROOT_STMT (instance)
	      && (SLP_TREE_DEF_TYPE (SLP_INSTANCE_TREE (instance))
		  != vect_internal_def)))
        {
	  slp_tree node = SLP_INSTANCE_TREE (instance);
	  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "removing SLP instance operations starting from: %G",
			     stmt_info->stmt);
	  vect_free_slp_instance (instance, false);
          vinfo->slp_instances.ordered_remove (i);
	  cost_vec.release ();
	}
      else
	{
	  for (hash_set<slp_tree>::iterator x = lvisited.begin();
	       x != lvisited.end(); ++x)
	    visited.add (*x);
	  i++;

	  add_stmt_costs (vinfo->target_cost_data, &cost_vec);
	  cost_vec.release ();
	}
    }

  return !vinfo->slp_instances.is_empty ();
}


/* Compute the scalar cost of the SLP node NODE and its children
   and return it.  Do not account defs that are marked in LIFE and
   update LIFE according to uses of NODE.  */

static void 
vect_bb_slp_scalar_cost (basic_block bb,
			 slp_tree node, vec<bool, va_heap> *life,
			 stmt_vector_for_cost *cost_vec,
			 hash_set<slp_tree> &visited)
{
  unsigned i;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (visited.add (node))
    return; 

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      gimple *stmt = stmt_info->stmt;
      vec_info *vinfo = stmt_info->vinfo;
      ssa_op_iter op_iter;
      def_operand_p def_p;

      if ((*life)[i])
	continue;

      /* If there is a non-vectorized use of the defs then the scalar
         stmt is kept live in which case we do not account it or any
	 required defs in the SLP children in the scalar cost.  This
	 way we make the vectorization more costly when compared to
	 the scalar cost.  */
      FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, op_iter, SSA_OP_DEF)
	{
	  imm_use_iterator use_iter;
	  gimple *use_stmt;
	  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, DEF_FROM_PTR (def_p))
	    if (!is_gimple_debug (use_stmt))
	      {
		stmt_vec_info use_stmt_info = vinfo->lookup_stmt (use_stmt);
		if (!use_stmt_info || !PURE_SLP_STMT (use_stmt_info))
		  {
		    (*life)[i] = true;
		    BREAK_FROM_IMM_USE_STMT (use_iter);
		  }
	      }
	}
      if ((*life)[i])
	continue;

      /* Count scalar stmts only once.  */
      if (gimple_visited_p (stmt))
	continue;
      gimple_set_visited (stmt, true);

      vect_cost_for_stmt kind;
      if (STMT_VINFO_DATA_REF (stmt_info))
        {
          if (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
	    kind = scalar_load;
          else
	    kind = scalar_store;
        }
      else if (vect_nop_conversion_p (stmt_info))
	continue;
      else
	kind = scalar_stmt;
      record_stmt_cost (cost_vec, 1, kind, stmt_info, 0, vect_body);
    }

  auto_vec<bool, 20> subtree_life;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    {
      if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	{
	  /* Do not directly pass LIFE to the recursive call, copy it to
	     confine changes in the callee to the current child/subtree.  */
	  subtree_life.safe_splice (*life);
	  vect_bb_slp_scalar_cost (bb, child, &subtree_life, cost_vec,
				   visited);
	  subtree_life.truncate (0);
	}
    }
}

/* Check if vectorization of the basic block is profitable.  */

static bool
vect_bb_vectorization_profitable_p (bb_vec_info bb_vinfo)
{
  vec<slp_instance> slp_instances = BB_VINFO_SLP_INSTANCES (bb_vinfo);
  slp_instance instance;
  int i;
  unsigned int vec_inside_cost = 0, vec_outside_cost = 0, scalar_cost = 0;
  unsigned int vec_prologue_cost = 0, vec_epilogue_cost = 0;

  /* Calculate scalar cost.  */
  stmt_vector_for_cost scalar_costs;
  scalar_costs.create (0);
  hash_set<slp_tree> visited;
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      auto_vec<bool, 20> life;
      life.safe_grow_cleared (SLP_INSTANCE_GROUP_SIZE (instance));
      vect_bb_slp_scalar_cost (BB_VINFO_BB (bb_vinfo),
			       SLP_INSTANCE_TREE (instance),
			       &life, &scalar_costs, visited);
    }
  void *target_cost_data = init_cost (NULL);
  add_stmt_costs (target_cost_data, &scalar_costs);
  scalar_costs.release ();
  unsigned dummy;
  finish_cost (target_cost_data, &dummy, &scalar_cost, &dummy);
  destroy_cost_data (target_cost_data);

  /* Unset visited flag.  */
  for (gimple_stmt_iterator gsi = bb_vinfo->region_begin;
       gsi_stmt (gsi) != gsi_stmt (bb_vinfo->region_end); gsi_next (&gsi))
    gimple_set_visited  (gsi_stmt (gsi), false);

  /* Complete the target-specific cost calculation.  */
  finish_cost (BB_VINFO_TARGET_COST_DATA (bb_vinfo), &vec_prologue_cost,
	       &vec_inside_cost, &vec_epilogue_cost);

  vec_outside_cost = vec_prologue_cost + vec_epilogue_cost;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "Cost model analysis: \n");
      dump_printf (MSG_NOTE, "  Vector inside of basic block cost: %d\n",
		   vec_inside_cost);
      dump_printf (MSG_NOTE, "  Vector prologue cost: %d\n", vec_prologue_cost);
      dump_printf (MSG_NOTE, "  Vector epilogue cost: %d\n", vec_epilogue_cost);
      dump_printf (MSG_NOTE, "  Scalar cost of basic block: %d\n", scalar_cost);
    }

  /* Vectorization is profitable if its cost is more than the cost of scalar
     version.  Note that we err on the vector side for equal cost because
     the cost estimate is otherwise quite pessimistic (constant uses are
     free on the scalar side but cost a load on the vector side for
     example).  */
  if (vec_outside_cost + vec_inside_cost > scalar_cost)
    return false;

  return true;
}

/* Find any vectorizable constructors and add them to the grouped_store
   array.  */

static void
vect_slp_check_for_constructors (bb_vec_info bb_vinfo)
{
  gimple_stmt_iterator gsi;

  for (gsi = bb_vinfo->region_begin;
       gsi_stmt (gsi) != gsi_stmt (bb_vinfo->region_end); gsi_next (&gsi))
    {
      gassign *stmt = dyn_cast <gassign *> (gsi_stmt (gsi));
      if (!stmt || gimple_assign_rhs_code (stmt) != CONSTRUCTOR)
	continue;

      tree rhs = gimple_assign_rhs1 (stmt);
      if (!VECTOR_TYPE_P (TREE_TYPE (rhs))
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs)),
		       CONSTRUCTOR_NELTS (rhs))
	  || VECTOR_TYPE_P (TREE_TYPE (CONSTRUCTOR_ELT (rhs, 0)->value))
	  || uniform_vector_p (rhs))
	continue;

      stmt_vec_info stmt_info = bb_vinfo->lookup_stmt (stmt);
      BB_VINFO_GROUPED_STORES (bb_vinfo).safe_push (stmt_info);
    }
}

/* Check if the region described by BB_VINFO can be vectorized, returning
   true if so.  When returning false, set FATAL to true if the same failure
   would prevent vectorization at other vector sizes, false if it is still
   worth trying other sizes.  N_STMTS is the number of statements in the
   region.  */

static bool
vect_slp_analyze_bb_1 (bb_vec_info bb_vinfo, int n_stmts, bool &fatal)
{
  DUMP_VECT_SCOPE ("vect_slp_analyze_bb");

  slp_instance instance;
  int i;
  poly_uint64 min_vf = 2;

  /* The first group of checks is independent of the vector size.  */
  fatal = true;

  /* Analyze the data references.  */

  if (!vect_analyze_data_refs (bb_vinfo, &min_vf, NULL))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: unhandled data-ref in basic "
			 "block.\n");
      return false;
    }

  if (BB_VINFO_DATAREFS (bb_vinfo).length () < 2)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: not enough data-refs in "
			 "basic block.\n");
      return false;
    }

  if (!vect_analyze_data_ref_accesses (bb_vinfo))
    {
     if (dump_enabled_p ())
       dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"not vectorized: unhandled data access in "
			"basic block.\n");
      return false;
    }

  vect_slp_check_for_constructors (bb_vinfo);

  /* If there are no grouped stores in the region there is no need
     to continue with pattern recog as vect_analyze_slp will fail
     anyway.  */
  if (bb_vinfo->grouped_stores.is_empty ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: no grouped stores in "
			 "basic block.\n");
      return false;
    }

  /* While the rest of the analysis below depends on it in some way.  */
  fatal = false;

  vect_pattern_recog (bb_vinfo);

  /* Check the SLP opportunities in the basic block, analyze and build SLP
     trees.  */
  if (!vect_analyze_slp (bb_vinfo, n_stmts))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Failed to SLP the basic block.\n");
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
			   "not vectorized: failed to find SLP opportunities "
			   "in basic block.\n");
	}
      return false;
    }

  vect_record_base_alignments (bb_vinfo);

  /* Analyze and verify the alignment of data references and the
     dependence in the SLP instances.  */
  for (i = 0; BB_VINFO_SLP_INSTANCES (bb_vinfo).iterate (i, &instance); )
    {
      if (! vect_slp_analyze_and_verify_instance_alignment (instance)
	  || ! vect_slp_analyze_instance_dependence (instance))
	{
	  slp_tree node = SLP_INSTANCE_TREE (instance);
	  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "removing SLP instance operations starting from: %G",
			     stmt_info->stmt);
	  vect_free_slp_instance (instance, false);
	  BB_VINFO_SLP_INSTANCES (bb_vinfo).ordered_remove (i);
	  continue;
	}

      /* Mark all the statements that we want to vectorize as pure SLP and
	 relevant.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance));
      vect_mark_slp_stmts_relevant (SLP_INSTANCE_TREE (instance));
      if (SLP_INSTANCE_ROOT_STMT (instance))
	STMT_SLP_TYPE (SLP_INSTANCE_ROOT_STMT (instance)) = pure_slp;

      i++;
    }
  if (! BB_VINFO_SLP_INSTANCES (bb_vinfo).length ())
    return false;

  if (!vect_slp_analyze_operations (bb_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: bad operation in basic block.\n");
      return false;
    }

  /* Cost model: check if the vectorization is worthwhile.  */
  if (!unlimited_cost_model (NULL)
      && !vect_bb_vectorization_profitable_p (bb_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: vectorization is not "
			 "profitable.\n");
      return false;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Basic block will be vectorized using SLP\n");
  return true;
}

/* Subroutine of vect_slp_bb.  Try to vectorize the statements between
   REGION_BEGIN (inclusive) and REGION_END (exclusive), returning true
   on success.  The region has N_STMTS statements and has the datarefs
   given by DATAREFS.  */

static bool
vect_slp_bb_region (gimple_stmt_iterator region_begin,
		    gimple_stmt_iterator region_end,
		    vec<data_reference_p> datarefs,
		    unsigned int n_stmts)
{
  bb_vec_info bb_vinfo;
  auto_vector_modes vector_modes;

  /* Autodetect first vector size we try.  */
  machine_mode next_vector_mode = VOIDmode;
  targetm.vectorize.autovectorize_vector_modes (&vector_modes, false);
  unsigned int mode_i = 0;

  vec_info_shared shared;

  machine_mode autodetected_vector_mode = VOIDmode;
  while (1)
    {
      bool vectorized = false;
      bool fatal = false;
      bb_vinfo = new _bb_vec_info (region_begin, region_end, &shared);

      bool first_time_p = shared.datarefs.is_empty ();
      BB_VINFO_DATAREFS (bb_vinfo) = datarefs;
      if (first_time_p)
	bb_vinfo->shared->save_datarefs ();
      else
	bb_vinfo->shared->check_datarefs ();
      bb_vinfo->vector_mode = next_vector_mode;

      if (vect_slp_analyze_bb_1 (bb_vinfo, n_stmts, fatal)
	  && dbg_cnt (vect_slp))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "***** Analysis succeeded with vector mode"
			       " %s\n", GET_MODE_NAME (bb_vinfo->vector_mode));
	      dump_printf_loc (MSG_NOTE, vect_location, "SLPing BB part\n");
	    }

	  bb_vinfo->shared->check_datarefs ();
	  vect_schedule_slp (bb_vinfo);

	  unsigned HOST_WIDE_INT bytes;
	  if (dump_enabled_p ())
	    {
	      if (GET_MODE_SIZE (bb_vinfo->vector_mode).is_constant (&bytes))
		dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
				 "basic block part vectorized using %wu byte "
				 "vectors\n", bytes);
	      else
		dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
				 "basic block part vectorized using variable "
				 "length vectors\n");
	    }

	  vectorized = true;
	}
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "***** Analysis failed with vector mode %s\n",
			     GET_MODE_NAME (bb_vinfo->vector_mode));
	}

      if (mode_i == 0)
	autodetected_vector_mode = bb_vinfo->vector_mode;

      if (!fatal)
	while (mode_i < vector_modes.length ()
	       && vect_chooses_same_modes_p (bb_vinfo, vector_modes[mode_i]))
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "***** The result for vector mode %s would"
			       " be the same\n",
			       GET_MODE_NAME (vector_modes[mode_i]));
	    mode_i += 1;
	  }

      delete bb_vinfo;

      if (mode_i < vector_modes.length ()
	  && VECTOR_MODE_P (autodetected_vector_mode)
	  && (related_vector_mode (vector_modes[mode_i],
				   GET_MODE_INNER (autodetected_vector_mode))
	      == autodetected_vector_mode)
	  && (related_vector_mode (autodetected_vector_mode,
				   GET_MODE_INNER (vector_modes[mode_i]))
	      == vector_modes[mode_i]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "***** Skipping vector mode %s, which would"
			     " repeat the analysis for %s\n",
			     GET_MODE_NAME (vector_modes[mode_i]),
			     GET_MODE_NAME (autodetected_vector_mode));
	  mode_i += 1;
	}

      if (vectorized
	  || mode_i == vector_modes.length ()
	  || autodetected_vector_mode == VOIDmode
	  /* If vect_slp_analyze_bb_1 signaled that analysis for all
	     vector sizes will fail do not bother iterating.  */
	  || fatal)
	return vectorized;

      /* Try the next biggest vector size.  */
      next_vector_mode = vector_modes[mode_i++];
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "***** Re-trying analysis with vector mode %s\n",
			 GET_MODE_NAME (next_vector_mode));
    }
}

/* Main entry for the BB vectorizer.  Analyze and transform BB, returns
   true if anything in the basic-block was vectorized.  */

bool
vect_slp_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  bool any_vectorized = false;

  gsi = gsi_start_bb (bb);
  while (!gsi_end_p (gsi))
    {
      gimple_stmt_iterator region_begin = gsi;
      vec<data_reference_p> datarefs = vNULL;
      int insns = 0;

      for (; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt))
	    continue;
	  insns++;

	  if (gimple_location (stmt) != UNKNOWN_LOCATION)
	    vect_location = stmt;

	  if (!vect_find_stmt_data_reference (NULL, stmt, &datarefs))
	    break;
	}

      /* Skip leading unhandled stmts.  */
      if (gsi_stmt (region_begin) == gsi_stmt (gsi))
	{
	  gsi_next (&gsi);
	  continue;
	}

      gimple_stmt_iterator region_end = gsi;

      if (insns > param_slp_max_insns_in_bb)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: too many instructions in "
			     "basic block.\n");
	}
      else if (vect_slp_bb_region (region_begin, region_end, datarefs, insns))
	any_vectorized = true;

      if (gsi_end_p (region_end))
	break;

      /* Skip the unhandled stmt.  */
      gsi_next (&gsi);
    }

  return any_vectorized;
}


/* Return 1 if vector type STMT_VINFO is a boolean vector.  */

static bool
vect_mask_constant_operand_p (stmt_vec_info stmt_vinfo, unsigned op_num)
{
  enum tree_code code = gimple_expr_code (stmt_vinfo->stmt);
  tree op, vectype;
  enum vect_def_type dt;

  /* For comparison and COND_EXPR type is chosen depending
     on the non-constant other comparison operand.  */
  if (TREE_CODE_CLASS (code) == tcc_comparison)
    {
      gassign *stmt = as_a <gassign *> (stmt_vinfo->stmt);
      op = gimple_assign_rhs1 (stmt);

      if (!vect_is_simple_use (op, stmt_vinfo->vinfo, &dt, &vectype))
	gcc_unreachable ();

      return !vectype || VECTOR_BOOLEAN_TYPE_P (vectype);
    }

  if (code == COND_EXPR)
    {
      gassign *stmt = as_a <gassign *> (stmt_vinfo->stmt);
      tree cond = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (cond) == SSA_NAME)
	{
	  if (op_num > 0)
	    return VECTOR_BOOLEAN_TYPE_P (STMT_VINFO_VECTYPE (stmt_vinfo));
	  op = cond;
	}
      else
	{
	  if (op_num > 1)
	    return VECTOR_BOOLEAN_TYPE_P (STMT_VINFO_VECTYPE (stmt_vinfo));
	  op = TREE_OPERAND (cond, 0);
	}

      if (!vect_is_simple_use (op, stmt_vinfo->vinfo, &dt, &vectype))
	gcc_unreachable ();

      return !vectype || VECTOR_BOOLEAN_TYPE_P (vectype);
    }

  return VECTOR_BOOLEAN_TYPE_P (STMT_VINFO_VECTYPE (stmt_vinfo));
}

/* Build a variable-length vector in which the elements in ELTS are repeated
   to a fill NRESULTS vectors of type VECTOR_TYPE.  Store the vectors in
   RESULTS and add any new instructions to SEQ.

   The approach we use is:

   (1) Find a vector mode VM with integer elements of mode IM.

   (2) Replace ELTS[0:NELTS] with ELTS'[0:NELTS'], where each element of
       ELTS' has mode IM.  This involves creating NELTS' VIEW_CONVERT_EXPRs
       from small vectors to IM.

   (3) Duplicate each ELTS'[I] into a vector of mode VM.

   (4) Use a tree of interleaving VEC_PERM_EXPRs to create VMs with the
       correct byte contents.

   (5) Use VIEW_CONVERT_EXPR to cast the final VMs to the required type.

   We try to find the largest IM for which this sequence works, in order
   to cut down on the number of interleaves.  */

void
duplicate_and_interleave (vec_info *vinfo, gimple_seq *seq, tree vector_type,
			  vec<tree> elts, unsigned int nresults,
			  vec<tree> &results)
{
  unsigned int nelts = elts.length ();
  tree element_type = TREE_TYPE (vector_type);

  /* (1) Find a vector mode VM with integer elements of mode IM.  */
  unsigned int nvectors = 1;
  tree new_vector_type;
  tree permutes[2];
  if (!can_duplicate_and_interleave_p (vinfo, nelts, element_type,
				       &nvectors, &new_vector_type,
				       permutes))
    gcc_unreachable ();

  /* Get a vector type that holds ELTS[0:NELTS/NELTS'].  */
  unsigned int partial_nelts = nelts / nvectors;
  tree partial_vector_type = build_vector_type (element_type, partial_nelts);

  tree_vector_builder partial_elts;
  auto_vec<tree, 32> pieces (nvectors * 2);
  pieces.quick_grow (nvectors * 2);
  for (unsigned int i = 0; i < nvectors; ++i)
    {
      /* (2) Replace ELTS[0:NELTS] with ELTS'[0:NELTS'], where each element of
	     ELTS' has mode IM.  */
      partial_elts.new_vector (partial_vector_type, partial_nelts, 1);
      for (unsigned int j = 0; j < partial_nelts; ++j)
	partial_elts.quick_push (elts[i * partial_nelts + j]);
      tree t = gimple_build_vector (seq, &partial_elts);
      t = gimple_build (seq, VIEW_CONVERT_EXPR,
			TREE_TYPE (new_vector_type), t);

      /* (3) Duplicate each ELTS'[I] into a vector of mode VM.  */
      pieces[i] = gimple_build_vector_from_val (seq, new_vector_type, t);
    }

  /* (4) Use a tree of VEC_PERM_EXPRs to create a single VM with the
	 correct byte contents.

     We need to repeat the following operation log2(nvectors) times:

	out[i * 2] = VEC_PERM_EXPR (in[i], in[i + hi_start], lo_permute);
	out[i * 2 + 1] = VEC_PERM_EXPR (in[i], in[i + hi_start], hi_permute);

     However, if each input repeats every N elements and the VF is
     a multiple of N * 2, the HI result is the same as the LO.  */
  unsigned int in_start = 0;
  unsigned int out_start = nvectors;
  unsigned int hi_start = nvectors / 2;
  /* A bound on the number of outputs needed to produce NRESULTS results
     in the final iteration.  */
  unsigned int noutputs_bound = nvectors * nresults;
  for (unsigned int in_repeat = 1; in_repeat < nvectors; in_repeat *= 2)
    {
      noutputs_bound /= 2;
      unsigned int limit = MIN (noutputs_bound, nvectors);
      for (unsigned int i = 0; i < limit; ++i)
	{
	  if ((i & 1) != 0
	      && multiple_p (TYPE_VECTOR_SUBPARTS (new_vector_type),
			     2 * in_repeat))
	    {
	      pieces[out_start + i] = pieces[out_start + i - 1];
	      continue;
	    }

	  tree output = make_ssa_name (new_vector_type);
	  tree input1 = pieces[in_start + (i / 2)];
	  tree input2 = pieces[in_start + (i / 2) + hi_start];
	  gassign *stmt = gimple_build_assign (output, VEC_PERM_EXPR,
					       input1, input2,
					       permutes[i & 1]);
	  gimple_seq_add_stmt (seq, stmt);
	  pieces[out_start + i] = output;
	}
      std::swap (in_start, out_start);
    }

  /* (5) Use VIEW_CONVERT_EXPR to cast the final VM to the required type.  */
  results.reserve (nresults);
  for (unsigned int i = 0; i < nresults; ++i)
    if (i < nvectors)
      results.quick_push (gimple_build (seq, VIEW_CONVERT_EXPR, vector_type,
					pieces[in_start + i]));
    else
      results.quick_push (results[i - nvectors]);
}


/* For constant and loop invariant defs of SLP_NODE this function returns
   (vector) defs (VEC_OPRNDS) that will be used in the vectorized stmts.
   OP_NODE determines the node for the operand containing the scalar
   operands.  */

static void
vect_get_constant_vectors (slp_tree slp_node, unsigned op_num,
                           vec<tree> *vec_oprnds)
{
  slp_tree op_node = SLP_TREE_CHILDREN (slp_node)[op_num];
  stmt_vec_info stmt_vinfo = SLP_TREE_SCALAR_STMTS (slp_node)[0];
  vec_info *vinfo = stmt_vinfo->vinfo;
  unsigned HOST_WIDE_INT nunits;
  tree vec_cst;
  unsigned j, number_of_places_left_in_vector;
  tree vector_type;
  tree vop;
  int group_size = op_node->ops.length ();
  unsigned int vec_num, i;
  unsigned number_of_copies = 1;
  bool constant_p;
  tree neutral_op = NULL;
  gimple_seq ctor_seq = NULL;
  auto_vec<tree, 16> permute_results;

  /* ???  SLP analysis should compute the vector type for the
     constant / invariant and store it in the SLP node.  */
  tree op = op_node->ops[0];
  /* Check if vector type is a boolean vector.  */
  tree stmt_vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
  if (VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (op))
      && vect_mask_constant_operand_p (stmt_vinfo, op_num))
    vector_type = truth_type_for (stmt_vectype);
  else
    vector_type = get_vectype_for_scalar_type (vinfo, TREE_TYPE (op), op_node);

  /* ???  For lane-reducing ops we should also have the required number
     of vector stmts initialized rather than second-guessing here.  */
  unsigned int number_of_vectors;
  if (is_gimple_assign (stmt_vinfo->stmt)
      && (gimple_assign_rhs_code (stmt_vinfo->stmt) == SAD_EXPR
	  || gimple_assign_rhs_code (stmt_vinfo->stmt) == DOT_PROD_EXPR
	  || gimple_assign_rhs_code (stmt_vinfo->stmt) == WIDEN_SUM_EXPR))
    number_of_vectors = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
  else
    number_of_vectors
      = vect_get_num_vectors (SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node)
			      * TYPE_VECTOR_SUBPARTS (stmt_vectype),
			      vector_type);
  vec_oprnds->create (number_of_vectors);
  auto_vec<tree> voprnds (number_of_vectors);

  /* NUMBER_OF_COPIES is the number of times we need to use the same values in
     created vectors. It is greater than 1 if unrolling is performed.

     For example, we have two scalar operands, s1 and s2 (e.g., group of
     strided accesses of size two), while NUNITS is four (i.e., four scalars
     of this type can be packed in a vector).  The output vector will contain
     two copies of each scalar operand: {s1, s2, s1, s2}.  (NUMBER_OF_COPIES
     will be 2).

     If GROUP_SIZE > NUNITS, the scalars will be split into several vectors
     containing the operands.

     For example, NUNITS is four as before, and the group size is 8
     (s1, s2, ..., s8).  We will create two vectors {s1, s2, s3, s4} and
     {s5, s6, s7, s8}.  */

  /* When using duplicate_and_interleave, we just need one element for
     each scalar statement.  */
  if (!TYPE_VECTOR_SUBPARTS (vector_type).is_constant (&nunits))
    nunits = group_size;

  number_of_copies = nunits * number_of_vectors / group_size;

  number_of_places_left_in_vector = nunits;
  constant_p = true;
  tree_vector_builder elts (vector_type, nunits, 1);
  elts.quick_grow (nunits);
  bool place_after_defs = false;
  for (j = 0; j < number_of_copies; j++)
    {
      for (i = group_size - 1; op_node->ops.iterate (i, &op); i--)
        {
          /* Create 'vect_ = {op0,op1,...,opn}'.  */
          number_of_places_left_in_vector--;
	  tree orig_op = op;
	  if (!types_compatible_p (TREE_TYPE (vector_type), TREE_TYPE (op)))
	    {
	      if (CONSTANT_CLASS_P (op))
		{
		  if (VECTOR_BOOLEAN_TYPE_P (vector_type))
		    {
		      /* Can't use VIEW_CONVERT_EXPR for booleans because
			 of possibly different sizes of scalar value and
			 vector element.  */
		      if (integer_zerop (op))
			op = build_int_cst (TREE_TYPE (vector_type), 0);
		      else if (integer_onep (op))
			op = build_all_ones_cst (TREE_TYPE (vector_type));
		      else
			gcc_unreachable ();
		    }
		  else
		    op = fold_unary (VIEW_CONVERT_EXPR,
				     TREE_TYPE (vector_type), op);
		  gcc_assert (op && CONSTANT_CLASS_P (op));
		}
	      else
		{
		  tree new_temp = make_ssa_name (TREE_TYPE (vector_type));
		  gimple *init_stmt;
		  if (VECTOR_BOOLEAN_TYPE_P (vector_type))
		    {
		      tree true_val
			= build_all_ones_cst (TREE_TYPE (vector_type));
		      tree false_val
			= build_zero_cst (TREE_TYPE (vector_type));
		      gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (op)));
		      init_stmt = gimple_build_assign (new_temp, COND_EXPR,
						       op, true_val,
						       false_val);
		    }
		  else
		    {
		      op = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (vector_type),
				   op);
		      init_stmt
			= gimple_build_assign (new_temp, VIEW_CONVERT_EXPR,
					       op);
		    }
		  gimple_seq_add_stmt (&ctor_seq, init_stmt);
		  op = new_temp;
		}
	    }
	  elts[number_of_places_left_in_vector] = op;
	  if (!CONSTANT_CLASS_P (op))
	    constant_p = false;
	  if (TREE_CODE (orig_op) == SSA_NAME
	      && !SSA_NAME_IS_DEFAULT_DEF (orig_op)
	      && STMT_VINFO_BB_VINFO (stmt_vinfo)
	      && (STMT_VINFO_BB_VINFO (stmt_vinfo)->bb
		  == gimple_bb (SSA_NAME_DEF_STMT (orig_op))))
	    place_after_defs = true;

          if (number_of_places_left_in_vector == 0)
            {
	      if (constant_p
		  ? multiple_p (TYPE_VECTOR_SUBPARTS (vector_type), nunits)
		  : known_eq (TYPE_VECTOR_SUBPARTS (vector_type), nunits))
		vec_cst = gimple_build_vector (&ctor_seq, &elts);
	      else
		{
		  if (permute_results.is_empty ())
		    duplicate_and_interleave (vinfo, &ctor_seq, vector_type,
					      elts, number_of_vectors,
					      permute_results);
		  vec_cst = permute_results[number_of_vectors - j - 1];
		}
	      tree init;
	      gimple_stmt_iterator gsi;
	      if (place_after_defs)
		{
		  stmt_vec_info last_stmt_info
		    = vect_find_last_scalar_stmt_in_slp (slp_node);
		  gsi = gsi_for_stmt (last_stmt_info->stmt);
		  init = vect_init_vector (stmt_vinfo, vec_cst, vector_type,
					   &gsi);
		}
	      else
		init = vect_init_vector (stmt_vinfo, vec_cst, vector_type,
					 NULL);
	      if (ctor_seq != NULL)
		{
		  gsi = gsi_for_stmt (SSA_NAME_DEF_STMT (init));
		  gsi_insert_seq_before (&gsi, ctor_seq, GSI_SAME_STMT);
		  ctor_seq = NULL;
		}
	      voprnds.quick_push (init);
	      place_after_defs = false;
              number_of_places_left_in_vector = nunits;
	      constant_p = true;
	      elts.new_vector (vector_type, nunits, 1);
	      elts.quick_grow (nunits);
            }
        }
    }

  /* Since the vectors are created in the reverse order, we should invert
     them.  */
  vec_num = voprnds.length ();
  for (j = vec_num; j != 0; j--)
    {
      vop = voprnds[j - 1];
      vec_oprnds->quick_push (vop);
    }

  /* In case that VF is greater than the unrolling factor needed for the SLP
     group of stmts, NUMBER_OF_VECTORS to be created is greater than
     NUMBER_OF_SCALARS/NUNITS or NUNITS/NUMBER_OF_SCALARS, and hence we have
     to replicate the vectors.  */
  while (number_of_vectors > vec_oprnds->length ())
    {
      tree neutral_vec = NULL;

      if (neutral_op)
        {
          if (!neutral_vec)
	    neutral_vec = build_vector_from_val (vector_type, neutral_op);

          vec_oprnds->quick_push (neutral_vec);
        }
      else
        {
          for (i = 0; vec_oprnds->iterate (i, &vop) && i < vec_num; i++)
            vec_oprnds->quick_push (vop);
        }
    }
}


/* Get vectorized definitions from SLP_NODE that contains corresponding
   vectorized def-stmts.  */

static void
vect_get_slp_vect_defs (slp_tree slp_node, vec<tree> *vec_oprnds)
{
  stmt_vec_info vec_def_stmt_info;
  unsigned int i;

  gcc_assert (SLP_TREE_VEC_STMTS (slp_node).exists ());

  FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (slp_node), i, vec_def_stmt_info)
    vec_oprnds->quick_push (gimple_get_lhs (vec_def_stmt_info->stmt));
}


/* Get N vectorized definitions for SLP_NODE.
   If the scalar definitions are loop invariants or constants, collect them and
   call vect_get_constant_vectors() to create vector stmts.
   Otherwise, the def-stmts must be already vectorized and the vectorized stmts
   must be stored in the corresponding child of SLP_NODE, and we call
   vect_get_slp_vect_defs () to retrieve them.  */

void
vect_get_slp_defs (slp_tree slp_node, vec<vec<tree> > *vec_oprnds, unsigned n)
{
  if (n == -1U)
    n = SLP_TREE_CHILDREN (slp_node).length ();

  for (unsigned i = 0; i < n; ++i)
    {
      slp_tree child = SLP_TREE_CHILDREN (slp_node)[i];

      vec<tree> vec_defs = vNULL;

      /* For each operand we check if it has vectorized definitions in a child
	 node or we need to create them (for invariants and constants).  */
      if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	{
	  vec_defs.create (SLP_TREE_NUMBER_OF_VEC_STMTS (child));
	  vect_get_slp_vect_defs (child, &vec_defs);
	}
      else
	vect_get_constant_vectors (slp_node, i, &vec_defs);

      vec_oprnds->quick_push (vec_defs);
    }
}

/* Generate vector permute statements from a list of loads in DR_CHAIN.
   If ANALYZE_ONLY is TRUE, only check that it is possible to create valid
   permute statements for the SLP node NODE of the SLP instance
   SLP_NODE_INSTANCE.  */

bool
vect_transform_slp_perm_load (slp_tree node, vec<tree> dr_chain,
			      gimple_stmt_iterator *gsi, poly_uint64 vf,
			      slp_instance slp_node_instance, bool analyze_only,
			      unsigned *n_perms)
{
  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
  vec_info *vinfo = stmt_info->vinfo;
  int vec_index = 0;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  unsigned int group_size = SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
  unsigned int mask_element;
  machine_mode mode;

  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
    return false;

  stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);

  mode = TYPE_MODE (vectype);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* Initialize the vect stmts of NODE to properly insert the generated
     stmts later.  */
  if (! analyze_only)
    for (unsigned i = SLP_TREE_VEC_STMTS (node).length ();
	 i < SLP_TREE_NUMBER_OF_VEC_STMTS (node); i++)
      SLP_TREE_VEC_STMTS (node).quick_push (NULL);

  /* Generate permutation masks for every NODE. Number of masks for each NODE
     is equal to GROUP_SIZE.
     E.g., we have a group of three nodes with three loads from the same
     location in each node, and the vector size is 4. I.e., we have a
     a0b0c0a1b1c1... sequence and we need to create the following vectors:
     for a's: a0a0a0a1 a1a1a2a2 a2a3a3a3
     for b's: b0b0b0b1 b1b1b2b2 b2b3b3b3
     ...

     The masks for a's should be: {0,0,0,3} {3,3,6,6} {6,9,9,9}.
     The last mask is illegal since we assume two operands for permute
     operation, and the mask element values can't be outside that range.
     Hence, the last mask must be converted into {2,5,5,5}.
     For the first two permutations we need the first and the second input
     vectors: {a0,b0,c0,a1} and {b1,c1,a2,b2}, and for the last permutation
     we need the second and the third vectors: {b1,c1,a2,b2} and
     {c2,a3,b3,c3}.  */

  int vect_stmts_counter = 0;
  unsigned int index = 0;
  int first_vec_index = -1;
  int second_vec_index = -1;
  bool noop_p = true;
  *n_perms = 0;

  vec_perm_builder mask;
  unsigned int nelts_to_build;
  unsigned int nvectors_per_build;
  bool repeating_p = (group_size == DR_GROUP_SIZE (stmt_info)
		      && multiple_p (nunits, group_size));
  if (repeating_p)
    {
      /* A single vector contains a whole number of copies of the node, so:
	 (a) all permutes can use the same mask; and
	 (b) the permutes only need a single vector input.  */
      mask.new_vector (nunits, group_size, 3);
      nelts_to_build = mask.encoded_nelts ();
      nvectors_per_build = SLP_TREE_VEC_STMTS (node).length ();
    }
  else
    {
      /* We need to construct a separate mask for each vector statement.  */
      unsigned HOST_WIDE_INT const_nunits, const_vf;
      if (!nunits.is_constant (&const_nunits)
	  || !vf.is_constant (&const_vf))
	return false;
      mask.new_vector (const_nunits, const_nunits, 1);
      nelts_to_build = const_vf * group_size;
      nvectors_per_build = 1;
    }

  unsigned int count = mask.encoded_nelts ();
  mask.quick_grow (count);
  vec_perm_indices indices;

  for (unsigned int j = 0; j < nelts_to_build; j++)
    {
      unsigned int iter_num = j / group_size;
      unsigned int stmt_num = j % group_size;
      unsigned int i = (iter_num * DR_GROUP_SIZE (stmt_info)
			+ SLP_TREE_LOAD_PERMUTATION (node)[stmt_num]);
      if (repeating_p)
	{
	  first_vec_index = 0;
	  mask_element = i;
	}
      else
	{
	  /* Enforced before the loop when !repeating_p.  */
	  unsigned int const_nunits = nunits.to_constant ();
	  vec_index = i / const_nunits;
	  mask_element = i % const_nunits;
	  if (vec_index == first_vec_index
	      || first_vec_index == -1)
	    {
	      first_vec_index = vec_index;
	    }
	  else if (vec_index == second_vec_index
		   || second_vec_index == -1)
	    {
	      second_vec_index = vec_index;
	      mask_element += const_nunits;
	    }
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "permutation requires at "
				 "least three vectors %G",
				 stmt_info->stmt);
	      gcc_assert (analyze_only);
	      return false;
	    }

	  gcc_assert (mask_element < 2 * const_nunits);
	}

      if (mask_element != index)
	noop_p = false;
      mask[index++] = mask_element;

      if (index == count && !noop_p)
	{
	  indices.new_vector (mask, second_vec_index == -1 ? 1 : 2, nunits);
	  if (!can_vec_perm_const_p (mode, indices))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				   vect_location,
				   "unsupported vect permute { ");
		  for (i = 0; i < count; ++i)
		    {
		      dump_dec (MSG_MISSED_OPTIMIZATION, mask[i]);
		      dump_printf (MSG_MISSED_OPTIMIZATION, " ");
		    }
		  dump_printf (MSG_MISSED_OPTIMIZATION, "}\n");
		}
	      gcc_assert (analyze_only);
	      return false;
	    }

	  ++*n_perms;
	}

      if (index == count)
	{
	  if (!analyze_only)
	    {
	      tree mask_vec = NULL_TREE;
		  
	      if (! noop_p)
		mask_vec = vect_gen_perm_mask_checked (vectype, indices);

	      if (second_vec_index == -1)
		second_vec_index = first_vec_index;

	      for (unsigned int ri = 0; ri < nvectors_per_build; ++ri)
		{
		  /* Generate the permute statement if necessary.  */
		  tree first_vec = dr_chain[first_vec_index + ri];
		  tree second_vec = dr_chain[second_vec_index + ri];
		  stmt_vec_info perm_stmt_info;
		  if (! noop_p)
		    {
		      gassign *stmt = as_a <gassign *> (stmt_info->stmt);
		      tree perm_dest
			= vect_create_destination_var (gimple_assign_lhs (stmt),
						       vectype);
		      perm_dest = make_ssa_name (perm_dest);
		      gassign *perm_stmt
			= gimple_build_assign (perm_dest, VEC_PERM_EXPR,
					       first_vec, second_vec,
					       mask_vec);
		      perm_stmt_info
			= vect_finish_stmt_generation (stmt_info, perm_stmt,
						       gsi);
		    }
		  else
		    /* If mask was NULL_TREE generate the requested
		       identity transform.  */
		    perm_stmt_info = vinfo->lookup_def (first_vec);

		  /* Store the vector statement in NODE.  */
		  SLP_TREE_VEC_STMTS (node)[vect_stmts_counter++]
		    = perm_stmt_info;
		}
	    }

	  index = 0;
	  first_vec_index = -1;
	  second_vec_index = -1;
	  noop_p = true;
	}
    }

  return true;
}

/* Vectorize SLP instance tree in postorder.  */

static void
vect_schedule_slp_instance (slp_tree node, slp_instance instance)
{
  gimple_stmt_iterator si;
  stmt_vec_info stmt_info;
  unsigned int group_size;
  tree vectype;
  int i, j;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  /* See if we have already vectorized the node in the graph of the
     SLP instance.  */
  if (SLP_TREE_VEC_STMTS (node).exists ())
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_schedule_slp_instance (child, instance);

  /* Push SLP node def-type to stmts.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      {
	stmt_vec_info child_stmt_info;
	FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (child), j, child_stmt_info)
	  STMT_VINFO_DEF_TYPE (child_stmt_info) = SLP_TREE_DEF_TYPE (child);
      }

  stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];

  /* VECTYPE is the type of the destination.  */
  vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  group_size = SLP_INSTANCE_GROUP_SIZE (instance);

  gcc_assert (SLP_TREE_NUMBER_OF_VEC_STMTS (node) != 0);
  SLP_TREE_VEC_STMTS (node).create (SLP_TREE_NUMBER_OF_VEC_STMTS (node));

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "------>vectorizing SLP node starting from: %G",
		     stmt_info->stmt);

  /* Vectorized stmts go before the last scalar stmt which is where
     all uses are ready.  */
  stmt_vec_info last_stmt_info = vect_find_last_scalar_stmt_in_slp (node);
  si = gsi_for_stmt (last_stmt_info->stmt);

  /* Handle two-operation SLP nodes by vectorizing the group with
     both operations and then performing a merge.  */
  bool done_p = false;
  if (SLP_TREE_TWO_OPERATORS (node))
    {
      gassign *stmt = as_a <gassign *> (stmt_info->stmt);
      enum tree_code code0 = gimple_assign_rhs_code (stmt);
      enum tree_code ocode = ERROR_MARK;
      stmt_vec_info ostmt_info;
      vec_perm_builder mask (group_size, group_size, 1);
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, ostmt_info)
	{
	  gassign *ostmt = as_a <gassign *> (ostmt_info->stmt);
	  if (gimple_assign_rhs_code (ostmt) != code0)
	    {
	      mask.quick_push (1);
	      ocode = gimple_assign_rhs_code (ostmt);
	    }
	  else
	    mask.quick_push (0);
	}
      if (ocode != ERROR_MARK)
	{
	  vec<stmt_vec_info> v0;
	  vec<stmt_vec_info> v1;
	  unsigned j;
	  tree tmask = NULL_TREE;
	  vect_transform_stmt (stmt_info, &si, node, instance);
	  v0 = SLP_TREE_VEC_STMTS (node).copy ();
	  SLP_TREE_VEC_STMTS (node).truncate (0);
	  gimple_assign_set_rhs_code (stmt, ocode);
	  vect_transform_stmt (stmt_info, &si, node, instance);
	  gimple_assign_set_rhs_code (stmt, code0);
	  v1 = SLP_TREE_VEC_STMTS (node).copy ();
	  SLP_TREE_VEC_STMTS (node).truncate (0);
	  tree meltype = build_nonstandard_integer_type
	      (GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (vectype))), 1);
	  tree mvectype = get_same_sized_vectype (meltype, vectype);
	  unsigned k = 0, l;
	  for (j = 0; j < v0.length (); ++j)
	    {
	      /* Enforced by vect_build_slp_tree, which rejects variable-length
		 vectors for SLP_TREE_TWO_OPERATORS.  */
	      unsigned int const_nunits = nunits.to_constant ();
	      tree_vector_builder melts (mvectype, const_nunits, 1);
	      for (l = 0; l < const_nunits; ++l)
		{
		  if (k >= group_size)
		    k = 0;
		  tree t = build_int_cst (meltype,
					  mask[k++] * const_nunits + l);
		  melts.quick_push (t);
		}
	      tmask = melts.build ();

	      /* ???  Not all targets support a VEC_PERM_EXPR with a
	         constant mask that would translate to a vec_merge RTX
		 (with their vec_perm_const_ok).  We can either not
		 vectorize in that case or let veclower do its job.
		 Unfortunately that isn't too great and at least for
		 plus/minus we'd eventually like to match targets
		 vector addsub instructions.  */
	      gimple *vstmt;
	      vstmt = gimple_build_assign (make_ssa_name (vectype),
					   VEC_PERM_EXPR,
					   gimple_assign_lhs (v0[j]->stmt),
					   gimple_assign_lhs (v1[j]->stmt),
					   tmask);
	      SLP_TREE_VEC_STMTS (node).quick_push
		(vect_finish_stmt_generation (stmt_info, vstmt, &si));
	    }
	  v0.release ();
	  v1.release ();
	  done_p = true;
	}
    }
  if (!done_p)
    vect_transform_stmt (stmt_info, &si, node, instance);

  /* Restore stmt def-types.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      {
	stmt_vec_info child_stmt_info;
	FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (child), j, child_stmt_info)
	  STMT_VINFO_DEF_TYPE (child_stmt_info) = vect_internal_def;
      }
}

/* Replace scalar calls from SLP node NODE with setting of their lhs to zero.
   For loop vectorization this is done in vectorizable_call, but for SLP
   it needs to be deferred until end of vect_schedule_slp, because multiple
   SLP instances may refer to the same scalar stmt.  */

static void
vect_remove_slp_scalar_calls (slp_tree node, hash_set<slp_tree> &visited)
{
  gimple *new_stmt;
  gimple_stmt_iterator gsi;
  int i;
  slp_tree child;
  tree lhs;
  stmt_vec_info stmt_info;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_remove_slp_scalar_calls (child, visited);

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt);
      if (!stmt || gimple_bb (stmt) == NULL)
	continue;
      if (is_pattern_stmt_p (stmt_info)
	  || !PURE_SLP_STMT (stmt_info))
	continue;
      lhs = gimple_call_lhs (stmt);
      new_stmt = gimple_build_assign (lhs, build_zero_cst (TREE_TYPE (lhs)));
      gsi = gsi_for_stmt (stmt);
      stmt_info->vinfo->replace_stmt (&gsi, stmt_info, new_stmt);
      SSA_NAME_DEF_STMT (gimple_assign_lhs (new_stmt)) = new_stmt;
    }
}

static void
vect_remove_slp_scalar_calls (slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_remove_slp_scalar_calls (node, visited);
}

/* Vectorize the instance root.  */

void
vectorize_slp_instance_root_stmt (slp_tree node, slp_instance instance)
{
  gassign *rstmt = NULL;

  if (SLP_TREE_NUMBER_OF_VEC_STMTS (node) == 1)
    {
      stmt_vec_info child_stmt_info;
      int j;

      FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (node), j, child_stmt_info)
	{
	  tree vect_lhs = gimple_get_lhs (child_stmt_info->stmt);
	  tree root_lhs = gimple_get_lhs (instance->root_stmt->stmt);
	  rstmt = gimple_build_assign (root_lhs, vect_lhs);
	  break;
	}
    }
  else if (SLP_TREE_NUMBER_OF_VEC_STMTS (node) > 1)
    {
      int nelts = SLP_TREE_NUMBER_OF_VEC_STMTS (node);
      stmt_vec_info child_stmt_info;
      int j;
      vec<constructor_elt, va_gc> *v;
      vec_alloc (v, nelts);

      FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (node), j, child_stmt_info)
	{
	  CONSTRUCTOR_APPEND_ELT (v,
				  NULL_TREE,
				  gimple_get_lhs (child_stmt_info->stmt));
	}
      tree lhs = gimple_get_lhs (instance->root_stmt->stmt);
      tree rtype = TREE_TYPE (gimple_assign_rhs1 (instance->root_stmt->stmt));
      tree r_constructor = build_constructor (rtype, v);
      rstmt = gimple_build_assign (lhs, r_constructor);
    }

    gcc_assert (rstmt);

    gimple_stmt_iterator rgsi = gsi_for_stmt (instance->root_stmt->stmt);
    gsi_replace (&rgsi, rstmt, true);
}

/* Generate vector code for all SLP instances in the loop/basic block.  */

void
vect_schedule_slp (vec_info *vinfo)
{
  vec<slp_instance> slp_instances;
  slp_instance instance;
  unsigned int i;

  slp_instances = vinfo->slp_instances;
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      slp_tree node = SLP_INSTANCE_TREE (instance);
      /* Schedule the tree of INSTANCE.  */
      vect_schedule_slp_instance (node, instance);

      if (SLP_INSTANCE_ROOT_STMT (instance))
	vectorize_slp_instance_root_stmt (node, instance);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "vectorizing stmts using SLP.\n");
    }

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      slp_tree root = SLP_INSTANCE_TREE (instance);
      stmt_vec_info store_info;
      unsigned int j;

      /* Remove scalar call stmts.  Do not do this for basic-block
	 vectorization as not all uses may be vectorized.
	 ???  Why should this be necessary?  DCE should be able to
	 remove the stmts itself.
	 ???  For BB vectorization we can as well remove scalar
	 stmts starting from the SLP tree root if they have no
	 uses.  */
      if (is_a <loop_vec_info> (vinfo))
	vect_remove_slp_scalar_calls (root);

      for (j = 0; SLP_TREE_SCALAR_STMTS (root).iterate (j, &store_info)
                  && j < SLP_INSTANCE_GROUP_SIZE (instance); j++)
        {
	  if (!STMT_VINFO_DATA_REF (store_info))
	    break;

	  if (SLP_INSTANCE_ROOT_STMT (instance))
	    continue;

	  store_info = vect_orig_stmt (store_info);
	  /* Free the attached stmt_vec_info and remove the stmt.  */
	  vinfo->remove_stmt (store_info);
        }
    }
}
