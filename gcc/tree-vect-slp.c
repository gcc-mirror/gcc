/* SLP - Basic Block Vectorization
   Copyright (C) 2007-2018 Free Software Foundation, Inc.
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
#include "params.h"
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


/* Recursively free the memory allocated for the SLP tree rooted at NODE.  */

static void
vect_free_slp_tree (slp_tree node)
{
  int i;
  slp_tree child;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_free_slp_tree (child);

  gimple *stmt;
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    /* After transform some stmts are removed and thus their vinfo is gone.  */
    if (vinfo_for_stmt (stmt))
      {
	gcc_assert (STMT_VINFO_NUM_SLP_USES (vinfo_for_stmt (stmt)) > 0);
	STMT_VINFO_NUM_SLP_USES (vinfo_for_stmt (stmt))--;
      }

  SLP_TREE_CHILDREN (node).release ();
  SLP_TREE_SCALAR_STMTS (node).release ();
  SLP_TREE_VEC_STMTS (node).release ();
  SLP_TREE_LOAD_PERMUTATION (node).release ();

  free (node);
}


/* Free the memory allocated for the SLP instance.  */

void
vect_free_slp_instance (slp_instance instance)
{
  vect_free_slp_tree (SLP_INSTANCE_TREE (instance));
  SLP_INSTANCE_LOADS (instance).release ();
  free (instance);
}


/* Create an SLP node for SCALAR_STMTS.  */

static slp_tree
vect_create_new_slp_node (vec<gimple *> scalar_stmts)
{
  slp_tree node;
  gimple *stmt = scalar_stmts[0];
  unsigned int nops;

  if (is_gimple_call (stmt))
    nops = gimple_call_num_args (stmt);
  else if (is_gimple_assign (stmt))
    {
      nops = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	nops++;
    }
  else if (gimple_code (stmt) == GIMPLE_PHI)
    nops = 0;
  else
    return NULL;

  node = XNEW (struct _slp_tree);
  SLP_TREE_SCALAR_STMTS (node) = scalar_stmts;
  SLP_TREE_VEC_STMTS (node).create (0);
  SLP_TREE_NUMBER_OF_VEC_STMTS (node) = 0;
  SLP_TREE_CHILDREN (node).create (nops);
  SLP_TREE_LOAD_PERMUTATION (node) = vNULL;
  SLP_TREE_TWO_OPERATORS (node) = false;
  SLP_TREE_DEF_TYPE (node) = vect_internal_def;

  unsigned i;
  FOR_EACH_VEC_ELT (scalar_stmts, i, stmt)
    STMT_VINFO_NUM_SLP_USES (vinfo_for_stmt (stmt))++;

  return node;
}


/* This structure is used in creation of an SLP tree.  Each instance
   corresponds to the same operand in a group of scalar stmts in an SLP
   node.  */
typedef struct _slp_oprnd_info
{
  /* Def-stmts for the operands.  */
  vec<gimple *> def_stmts;
  /* Information about the first statement, its vector def-type, type, the
     operand itself in case it's constant, and an indication if it's a pattern
     stmt.  */
  tree first_op_type;
  enum vect_def_type first_dt;
  bool first_pattern;
  bool second_pattern;
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
      oprnd_info->first_dt = vect_uninitialized_def;
      oprnd_info->first_op_type = NULL_TREE;
      oprnd_info->first_pattern = false;
      oprnd_info->second_pattern = false;
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
      XDELETE (oprnd_info);
    }

  oprnds_info.release ();
}


/* Find the place of the data-ref in STMT in the interleaving chain that starts
   from FIRST_STMT.  Return -1 if the data-ref is not a part of the chain.  */

int
vect_get_place_in_interleaving_chain (gimple *stmt, gimple *first_stmt)
{
  gimple *next_stmt = first_stmt;
  int result = 0;

  if (first_stmt != DR_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    return -1;

  do
    {
      if (next_stmt == stmt)
	return result;
      next_stmt = DR_GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
      if (next_stmt)
	result += DR_GROUP_GAP (vinfo_for_stmt (next_stmt));
    }
  while (next_stmt);

  return -1;
}

/* Check whether it is possible to load COUNT elements of type ELT_MODE
   using the method implemented by duplicate_and_interleave.  Return true
   if so, returning the number of intermediate vectors in *NVECTORS_OUT
   (if nonnull) and the type of each intermediate vector in *VECTOR_TYPE_OUT
   (if nonnull).  */

bool
can_duplicate_and_interleave_p (unsigned int count, machine_mode elt_mode,
				unsigned int *nvectors_out,
				tree *vector_type_out,
				tree *permutes)
{
  poly_int64 elt_bytes = count * GET_MODE_SIZE (elt_mode);
  poly_int64 nelts;
  unsigned int nvectors = 1;
  for (;;)
    {
      scalar_int_mode int_mode;
      poly_int64 elt_bits = elt_bytes * BITS_PER_UNIT;
      if (multiple_p (current_vector_size, elt_bytes, &nelts)
	  && int_mode_for_size (elt_bits, 0).exists (&int_mode))
	{
	  tree int_type = build_nonstandard_integer_type
	    (GET_MODE_BITSIZE (int_mode), 1);
	  tree vector_type = build_vector_type (int_type, nelts);
	  if (VECTOR_MODE_P (TYPE_MODE (vector_type)))
	    {
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
			     vec<gimple *> stmts, unsigned stmt_num,
			     vec<slp_oprnd_info> *oprnds_info)
{
  gimple *stmt = stmts[stmt_num];
  tree oprnd;
  unsigned int i, number_of_oprnds;
  gimple *def_stmt;
  enum vect_def_type dt = vect_uninitialized_def;
  bool pattern = false;
  slp_oprnd_info oprnd_info;
  int first_op_idx = 1;
  bool commutative = false;
  bool first_op_cond = false;
  bool first = stmt_num == 0;
  bool second = stmt_num == 1;

  if (is_gimple_call (stmt))
    {
      number_of_oprnds = gimple_call_num_args (stmt);
      first_op_idx = 3;
    }
  else if (is_gimple_assign (stmt))
    {
      enum tree_code code = gimple_assign_rhs_code (stmt);
      number_of_oprnds = gimple_num_ops (stmt) - 1;
      /* Swap can only be done for cond_expr if asked to, otherwise we
	 could result in different comparison code to the first stmt.  */
      if (gimple_assign_rhs_code (stmt) == COND_EXPR
	  && COMPARISON_CLASS_P (gimple_assign_rhs1 (stmt)))
	{
	  first_op_cond = true;
	  number_of_oprnds++;
	}
      else
	commutative = commutative_tree_code (code);
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
	    oprnd = TREE_OPERAND (gimple_op (stmt, first_op_idx), map[i]);
	  else
	    oprnd = gimple_op (stmt, map[i]);
	}
      else
	oprnd = gimple_op (stmt, first_op_idx + (swapped ? !i : i));

      oprnd_info = (*oprnds_info)[i];

      if (!vect_is_simple_use (oprnd, vinfo, &dt, &def_stmt))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: can't analyze def for ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, oprnd);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return -1;
	}

      /* Check if DEF_STMT is a part of a pattern in LOOP and get the def stmt
         from the pattern.  Check that all the stmts of the node are in the
         pattern.  */
      if (def_stmt && gimple_bb (def_stmt)
	  && vect_stmt_in_region_p (vinfo, def_stmt)
	  && vinfo_for_stmt (def_stmt)
	  && is_pattern_stmt_p (vinfo_for_stmt (def_stmt)))
        {
          pattern = true;
          if (!first && !oprnd_info->first_pattern
	      /* Allow different pattern state for the defs of the
		 first stmt in reduction chains.  */
	      && (oprnd_info->first_dt != vect_reduction_def
		  || (!second && !oprnd_info->second_pattern)))
	    {
	      if (i == 0
		  && !swapped
		  && commutative)
		{
		  swapped = true;
		  goto again;
		}

	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "Build SLP failed: some of the stmts"
				   " are in a pattern, and others are not ");
		  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, oprnd);
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}

	      return 1;
            }

          dt = STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt));

          if (dt == vect_unknown_def_type)
            {
              if (dump_enabled_p ())
                dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Unsupported pattern.\n");
              return -1;
            }

          switch (gimple_code (def_stmt))
            {
            case GIMPLE_PHI:
            case GIMPLE_ASSIGN:
	      break;

	    default:
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "unsupported defining stmt:\n");
	      return -1;
            }
        }

      if (second)
	oprnd_info->second_pattern = pattern;

      if (first)
	{
	  oprnd_info->first_dt = dt;
	  oprnd_info->first_pattern = pattern;
	  oprnd_info->first_op_type = TREE_TYPE (oprnd);
	}
      else
	{
	  /* Not first stmt of the group, check that the def-stmt/s match
	     the def-stmt/s of the first stmt.  Allow different definition
	     types for reduction chains: the first stmt must be a
	     vect_reduction_def (a phi node), and the rest
	     vect_internal_def.  */
	  tree type = TREE_TYPE (oprnd);
	  if ((oprnd_info->first_dt != dt
	       && !(oprnd_info->first_dt == vect_reduction_def
		    && dt == vect_internal_def)
	       && !((oprnd_info->first_dt == vect_external_def
		     || oprnd_info->first_dt == vect_constant_def)
		    && (dt == vect_external_def
			|| dt == vect_constant_def)))
	      || !types_compatible_p (oprnd_info->first_op_type, type))
	    {
	      /* Try swapping operands if we got a mismatch.  */
	      if (i == 0
		  && !swapped
		  && commutative)
		{
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
	      && !current_vector_size.is_constant ()
	      && (TREE_CODE (type) == BOOLEAN_TYPE
		  || !can_duplicate_and_interleave_p (stmts.length (),
						      TYPE_MODE (type))))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "Build SLP failed: invalid type of def "
				   "for variable-length SLP ");
		  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, oprnd);
		  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}
	      return -1;
	    }
	}

      /* Check the types of the definitions.  */
      switch (dt)
	{
	case vect_constant_def:
	case vect_external_def:
	  break;

	case vect_reduction_def:
	case vect_induction_def:
	case vect_internal_def:
	  oprnd_info->def_stmts.quick_push (def_stmt);
	  break;

	default:
	  /* FORNOW: Not supported.  */
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: illegal type of def ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, oprnd);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return -1;
	}
    }

  /* Swap operands.  */
  if (swapped)
    {
      /* If there are already uses of this stmt in a SLP instance then
         we've committed to the operand order and can't swap it.  */
      if (STMT_VINFO_NUM_SLP_USES (vinfo_for_stmt (stmt)) != 0)
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: cannot swap operands of "
			       "shared stmt ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	    }
	  return -1;
	}

      if (first_op_cond)
	{
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
	      bool honor_nans = HONOR_NANS (TREE_OPERAND (cond, 0));
	      code = invert_tree_comparison (TREE_CODE (cond), honor_nans);
	      gcc_assert (code != ERROR_MARK);
	      TREE_SET_CODE (cond, code);
	    }
	}
      else
	swap_ssa_operands (stmt, gimple_assign_rhs1_ptr (stmt),
			   gimple_assign_rhs2_ptr (stmt));
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "swapped operands to match def types in ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	}
    }

  *swap = swapped;
  return 0;
}

/* A subroutine of vect_build_slp_tree for checking VECTYPE, which is the
   caller's attempt to find the vector type in STMT with the narrowest
   element type.  Return true if VECTYPE is nonnull and if it is valid
   for VINFO.  When returning true, update MAX_NUNITS to reflect the
   number of units in VECTYPE.  VINFO, GORUP_SIZE and MAX_NUNITS are
   as for vect_build_slp_tree.  */

static bool
vect_record_max_nunits (vec_info *vinfo, gimple *stmt, unsigned int group_size,
			tree vectype, poly_uint64 *max_nunits)
{
  if (!vectype)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Build SLP failed: unsupported data-type in ");
	  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}
      /* Fatal mismatch.  */
      return false;
    }

  /* If populating the vector type requires unrolling then fail
     before adjusting *max_nunits for basic-block vectorization.  */
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  unsigned HOST_WIDE_INT const_nunits;
  if (is_a <bb_vec_info> (vinfo)
      && (!nunits.is_constant (&const_nunits)
	  || const_nunits > group_size))
    {
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
vect_two_operations_perm_ok_p (vec<gimple *> stmts, unsigned int group_size,
			       tree vectype, tree_code alt_stmt_code)
{
  unsigned HOST_WIDE_INT count;
  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&count))
    return false;

  vec_perm_builder sel (count, count, 1);
  for (unsigned int i = 0; i < count; ++i)
    {
      unsigned int elt = i;
      if (gimple_assign_rhs_code (stmts[i % group_size]) == alt_stmt_code)
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

   Note COND_EXPR is possibly ismorphic to another one after swapping its
   operands.  Set SWAP[i] to 1 if stmt I is COND_EXPR and isomorphic to
   the first stmt by swapping the two operands of comparison; set SWAP[i]
   to 2 if stmt I is isormorphic to the first stmt by inverting the code
   of comparison.  Take A1 >= B1 ? X1 : Y1 as an exmple, it can be swapped
   to (B1 <= A1 ? X1 : Y1); or be inverted to (A1 < B1) ? Y1 : X1.  */

static bool
vect_build_slp_tree_1 (vec_info *vinfo, unsigned char *swap,
		       vec<gimple *> stmts, unsigned int group_size,
		       unsigned nops, poly_uint64 *max_nunits,
		       bool *matches, bool *two_operators)
{
  unsigned int i;
  gimple *first_stmt = stmts[0], *stmt = stmts[0];
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
  gimple *first_load = NULL, *prev_first_load = NULL;

  /* For every stmt in NODE find its def stmt/s.  */
  FOR_EACH_VEC_ELT (stmts, i, stmt)
    {
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
      swap[i] = 0;
      matches[i] = false;

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "Build SLP for ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	}

      /* Fail to vectorize statements marked as unvectorizable.  */
      if (!STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (stmt)))
        {
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: unvectorizable statement ");
              dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
            }
	  /* Fatal mismatch.  */
	  matches[0] = false;
          return false;
        }

      lhs = gimple_get_lhs (stmt);
      if (lhs == NULL_TREE)
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: not GIMPLE_ASSIGN nor "
			       "GIMPLE_CALL ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	    }
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}

      tree nunits_vectype;
      if (!vect_get_vector_types_for_stmt (stmt_info, &vectype,
					   &nunits_vectype)
	  || (nunits_vectype
	      && !vect_record_max_nunits (vinfo, stmt, group_size,
					  nunits_vectype, max_nunits)))
	{
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}

      gcc_assert (vectype);

      if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
	{
	  rhs_code = CALL_EXPR;
	  if (gimple_call_internal_p (call_stmt)
	      || gimple_call_tail_p (call_stmt)
	      || gimple_call_noreturn_p (call_stmt)
	      || !gimple_call_nothrow_p (call_stmt)
	      || gimple_call_chain (call_stmt))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
				   "Build SLP failed: unsupported call type ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				    call_stmt, 0);
		}
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }
	}
      else
	rhs_code = gimple_assign_rhs_code (stmt);

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
	      if (vectype == boolean_type_node)
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: shift of a"
				     " boolean.\n");
		  /* Fatal mismatch.  */
		  matches[0] = false;
		  return false;
		}

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
              && !(STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt))
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
				   "in stmt ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "original stmt ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				    first_stmt, 0);
		}
	      /* Mismatch.  */
	      continue;
	    }

	  if (need_same_oprnds
	      && !operand_equal_p (first_op1, gimple_assign_rhs2 (stmt), 0))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
				   "Build SLP failed: different shift "
				   "arguments in ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
		}
	      /* Mismatch.  */
	      continue;
	    }

	  if (rhs_code == CALL_EXPR)
	    {
	      gimple *first_stmt = stmts[0];
	      if (gimple_call_num_args (stmt) != nops
		  || !operand_equal_p (gimple_call_fn (first_stmt),
				       gimple_call_fn (stmt), 0)
		  || gimple_call_fntype (first_stmt)
		     != gimple_call_fntype (stmt))
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
				       "Build SLP failed: different calls in ");
		      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					stmt, 0);
		    }
		  /* Mismatch.  */
		  continue;
		}
	    }
	}

      /* Grouped store or load.  */
      if (STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt)))
	{
	  if (REFERENCE_CLASS_P (lhs))
	    {
	      /* Store.  */
	      ;
	    }
	  else
	    {
	      /* Load.  */
              first_load = DR_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt));
              if (prev_first_load)
                {
                  /* Check that there are no loads from different interleaving
                     chains in the same node.  */
                  if (prev_first_load != first_load)
                    {
                      if (dump_enabled_p ())
                        {
                          dump_printf_loc (MSG_MISSED_OPTIMIZATION,
					   vect_location, 
					   "Build SLP failed: different "
					   "interleaving chains in one node ");
                          dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					    stmt, 0);
                        }
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
	  if (TREE_CODE_CLASS (rhs_code) == tcc_reference)
	    {
	      /* Not grouped load.  */
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
				   "Build SLP failed: not grouped load ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
		}

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
	      && rhs_code != CALL_EXPR)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "Build SLP failed: operation");
		  dump_printf (MSG_MISSED_OPTIMIZATION, " unsupported ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
		}
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
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				       "Build SLP failed: different"
				       " operation");
		      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					stmt, 0);
		    }
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
      if (vectype == boolean_type_node
	  || !vect_two_operations_perm_ok_p (stmts, group_size,
					     vectype, alt_stmt_code))
	{
	  for (i = 0; i < group_size; ++i)
	    if (gimple_assign_rhs_code (stmts[i]) == alt_stmt_code)
	      {
		matches[i] = false;
		if (dump_enabled_p ())
		  {
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different operation "
				     "in stmt ");
		    dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				      stmts[i], 0);
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "original stmt ");
		    dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				      first_stmt, 0);
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
  typedef vec <gimple *> value_type;
  typedef vec <gimple *> compare_type;
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
    h.add_int (gimple_uid (x[i]));
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

typedef hash_set <vec <gimple *>, bst_traits> scalar_stmts_set_t;
static scalar_stmts_set_t *bst_fail;

typedef hash_map <vec <gimple *>, slp_tree,
		  simple_hashmap_traits <bst_traits, slp_tree> >
  scalar_stmts_to_slp_tree_map_t;

static slp_tree
vect_build_slp_tree_2 (vec_info *vinfo,
		       vec<gimple *> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits,
		       vec<slp_tree> *loads,
		       bool *matches, unsigned *npermutes, unsigned *tree_size,
		       unsigned max_tree_size);

static slp_tree
vect_build_slp_tree (vec_info *vinfo,
		     vec<gimple *> stmts, unsigned int group_size,
		     poly_uint64 *max_nunits, vec<slp_tree> *loads,
		     bool *matches, unsigned *npermutes, unsigned *tree_size,
		     unsigned max_tree_size)
{
  if (bst_fail->contains (stmts))
    return NULL;
  slp_tree res = vect_build_slp_tree_2 (vinfo, stmts, group_size, max_nunits,
					loads, matches, npermutes, tree_size,
					max_tree_size);
  /* When SLP build fails for stmts record this, otherwise SLP build
     can be exponential in time when we allow to construct parts from
     scalars, see PR81723.  */
  if (! res)
    {
      vec <gimple *> x;
      x.create (stmts.length ());
      x.splice (stmts);
      bst_fail->add (x);
    }
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
		       vec<gimple *> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits,
		       vec<slp_tree> *loads,
		       bool *matches, unsigned *npermutes, unsigned *tree_size,
		       unsigned max_tree_size)
{
  unsigned nops, i, this_tree_size = 0;
  poly_uint64 this_max_nunits = *max_nunits;
  gimple *stmt;
  slp_tree node;

  matches[0] = false;

  stmt = stmts[0];
  if (is_gimple_call (stmt))
    nops = gimple_call_num_args (stmt);
  else if (is_gimple_assign (stmt))
    {
      nops = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	nops++;
    }
  else if (gimple_code (stmt) == GIMPLE_PHI)
    nops = 0;
  else
    return NULL;

  /* If the SLP node is a PHI (induction or reduction), terminate
     the recursion.  */
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      tree scalar_type = TREE_TYPE (PHI_RESULT (stmt));
      tree vectype = get_vectype_for_scalar_type (scalar_type);
      if (!vect_record_max_nunits (vinfo, stmt, group_size, vectype,
				   max_nunits))
	return NULL;

      vect_def_type def_type = STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt));
      /* Induction from different IVs is not supported.  */
      if (def_type == vect_induction_def)
	{
	  FOR_EACH_VEC_ELT (stmts, i, stmt)
	    if (stmt != stmts[0])
	      return NULL;
	}
      else
	{
	  /* Else def types have to match.  */
	  FOR_EACH_VEC_ELT (stmts, i, stmt)
	    {
	      /* But for reduction chains only check on the first stmt.  */
	      if (REDUC_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt))
		  && REDUC_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) != stmt)
		continue;
	      if (STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) != def_type)
		return NULL;
	    }
	}
      node = vect_create_new_slp_node (stmts);
      return node;
    }


  bool two_operators = false;
  unsigned char *swap = XALLOCAVEC (unsigned char, group_size);
  if (!vect_build_slp_tree_1 (vinfo, swap,
			      stmts, group_size, nops,
			      &this_max_nunits, matches, &two_operators))
    return NULL;

  /* If the SLP node is a load, terminate the recursion.  */
  if (STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt))
      && DR_IS_READ (STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt))))
    {
      *max_nunits = this_max_nunits;
      node = vect_create_new_slp_node (stmts);
      loads->safe_push (node);
      return node;
    }

  /* Get at the operands, verifying they are compatible.  */
  vec<slp_oprnd_info> oprnds_info = vect_create_oprnd_info (nops, group_size);
  slp_oprnd_info oprnd_info;
  FOR_EACH_VEC_ELT (stmts, i, stmt)
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
  auto_vec<slp_tree> this_loads;

  stmt = stmts[0];

  if (tree_size)
    max_tree_size -= *tree_size;

  /* Create SLP_TREE nodes for the definition node/s.  */
  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      slp_tree child;
      unsigned old_nloads = this_loads.length ();
      unsigned old_tree_size = this_tree_size;
      unsigned int j;

      if (oprnd_info->first_dt != vect_internal_def
	  && oprnd_info->first_dt != vect_reduction_def
	  && oprnd_info->first_dt != vect_induction_def)
        continue;

      if (++this_tree_size > max_tree_size)
	{
	  FOR_EACH_VEC_ELT (children, j, child)
	    vect_free_slp_tree (child);
	  vect_free_oprnd_info (oprnds_info);
	  return NULL;
	}

      if ((child = vect_build_slp_tree (vinfo, oprnd_info->def_stmts,
					group_size, &this_max_nunits,
					&this_loads, matches, npermutes,
					&this_tree_size,
					max_tree_size)) != NULL)
	{
	  /* If we have all children of child built up from scalars then just
	     throw that away and build it up this node from scalars.  */
	  if (!SLP_TREE_CHILDREN (child).is_empty ()
	      /* ???  Rejecting patterns this way doesn't work.  We'd have to
		 do extra work to cancel the pattern so the uses see the
		 scalar version.  */
	      && !is_pattern_stmt_p
	            (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (child)[0])))
	    {
	      slp_tree grandchild;

	      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		if (SLP_TREE_DEF_TYPE (grandchild) == vect_internal_def)
		  break;
	      if (!grandchild)
		{
		  /* Roll back.  */
		  this_loads.truncate (old_nloads);
		  this_tree_size = old_tree_size;
		  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		    vect_free_slp_tree (grandchild);
		  SLP_TREE_CHILDREN (child).truncate (0);

		  dump_printf_loc (MSG_NOTE, vect_location,
				   "Building parent vector operands from "
				   "scalars instead\n");
		  oprnd_info->def_stmts = vNULL;
		  SLP_TREE_DEF_TYPE (child) = vect_external_def;
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
	  && !is_pattern_stmt_p (vinfo_for_stmt (stmt)))
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Building vector operands from scalars\n");
	  child = vect_create_new_slp_node (oprnd_info->def_stmts);
	  SLP_TREE_DEF_TYPE (child) = vect_external_def;
	  children.safe_push (child);
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
	  && is_gimple_assign (stmt)
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
		  gimple *stmt = stmts[j];
		  /* Verify if we can swap operands of this stmt.  */
		  if (!is_gimple_assign (stmt)
		      || !commutative_tree_code (gimple_assign_rhs_code (stmt)))
		    {
		      if (!swap_not_matching)
			goto fail;
		      swap_not_matching = false;
		      break;
		    }
		  /* Verify if we can safely swap or if we committed to a
		     specific operand order already.
		     ???  Instead of modifying GIMPLE stmts here we could
		     record whether we want to swap operands in the SLP
		     node and temporarily do that when processing it
		     (or wrap operand accessors in a helper).  */
		  else if (swap[j] != 0
			   || STMT_VINFO_NUM_SLP_USES (vinfo_for_stmt (stmt)))
		    {
		      if (!swap_not_matching)
			{
			  if (dump_enabled_p ())
			    {
			      dump_printf_loc (MSG_MISSED_OPTIMIZATION,
					       vect_location,
					       "Build SLP failed: cannot swap "
					       "operands of shared stmt ");
			      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION,
						TDF_SLIM, stmts[j], 0);
			    }
			  goto fail;
			}
		      swap_not_matching = false;
		      break;
		    }
		}
	    }
	  while (j != group_size);

	  /* Swap mismatched definition stmts.  */
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Re-trying with swapped operands of stmts ");
	  for (j = 0; j < group_size; ++j)
	    if (matches[j] == !swap_not_matching)
	      {
		std::swap (oprnds_info[0]->def_stmts[j],
			   oprnds_info[1]->def_stmts[j]);
		dump_printf (MSG_NOTE, "%d ", j);
	      }
	  dump_printf (MSG_NOTE, "\n");
	  /* And try again with scratch 'matches' ... */
	  bool *tem = XALLOCAVEC (bool, group_size);
	  if ((child = vect_build_slp_tree (vinfo, oprnd_info->def_stmts,
					    group_size, &this_max_nunits,
					    &this_loads, tem, npermutes,
					    &this_tree_size,
					    max_tree_size)) != NULL)
	    {
	      /* ... so if successful we can apply the operand swapping
		 to the GIMPLE IL.  This is necessary because for example
		 vect_get_slp_defs uses operand indexes and thus expects
		 canonical operand order.  This is also necessary even
		 if we end up building the operand from scalars as
		 we'll continue to process swapped operand two.  */
	      for (j = 0; j < group_size; ++j)
		{
		  gimple *stmt = stmts[j];
		  gimple_set_plf (stmt, GF_PLF_1, false);
		}
	      for (j = 0; j < group_size; ++j)
		{
		  gimple *stmt = stmts[j];
		  if (matches[j] == !swap_not_matching)
		    {
		      /* Avoid swapping operands twice.  */
		      if (gimple_plf (stmt, GF_PLF_1))
			continue;
		      swap_ssa_operands (stmt, gimple_assign_rhs1_ptr (stmt),
					 gimple_assign_rhs2_ptr (stmt));
		      gimple_set_plf (stmt, GF_PLF_1, true);
		    }
		}
	      /* Verify we swap all duplicates or none.  */
	      if (flag_checking)
		for (j = 0; j < group_size; ++j)
		  {
		    gimple *stmt = stmts[j];
		    gcc_assert (gimple_plf (stmt, GF_PLF_1)
				== (matches[j] == !swap_not_matching));
		  }

	      /* If we have all children of child built up from scalars then
		 just throw that away and build it up this node from scalars.  */
	      if (!SLP_TREE_CHILDREN (child).is_empty ()
		  /* ???  Rejecting patterns this way doesn't work.  We'd have
		     to do extra work to cancel the pattern so the uses see the
		     scalar version.  */
		  && !is_pattern_stmt_p
			(vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (child)[0])))
		{
		  unsigned int j;
		  slp_tree grandchild;

		  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		    if (SLP_TREE_DEF_TYPE (grandchild) == vect_internal_def)
		      break;
		  if (!grandchild)
		    {
		      /* Roll back.  */
		      this_loads.truncate (old_nloads);
		      this_tree_size = old_tree_size;
		      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
			vect_free_slp_tree (grandchild);
		      SLP_TREE_CHILDREN (child).truncate (0);

		      dump_printf_loc (MSG_NOTE, vect_location,
				       "Building parent vector operands from "
				       "scalars instead\n");
		      oprnd_info->def_stmts = vNULL;
		      SLP_TREE_DEF_TYPE (child) = vect_external_def;
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
	vect_free_slp_tree (child);
      vect_free_oprnd_info (oprnds_info);
      return NULL;
    }

  vect_free_oprnd_info (oprnds_info);

  if (tree_size)
    *tree_size += this_tree_size;
  *max_nunits = this_max_nunits;
  loads->safe_splice (this_loads);

  node = vect_create_new_slp_node (stmts);
  SLP_TREE_TWO_OPERATORS (node) = two_operators;
  SLP_TREE_CHILDREN (node).splice (children);
  return node;
}

/* Dump a slp tree NODE using flags specified in DUMP_KIND.  */

static void
vect_print_slp_tree (dump_flags_t dump_kind, dump_location_t loc,
		     slp_tree node)
{
  int i;
  gimple *stmt;
  slp_tree child;

  dump_printf_loc (dump_kind, loc, "node%s\n",
		   SLP_TREE_DEF_TYPE (node) != vect_internal_def
		   ? " (external)" : "");
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      dump_printf_loc (dump_kind, loc, "\tstmt %d ", i);
      dump_gimple_stmt (dump_kind, TDF_SLIM, stmt, 0);
    }
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_print_slp_tree (dump_kind, loc, child);
}


/* Mark the tree rooted at NODE with MARK (PURE_SLP or HYBRID).
   If MARK is HYBRID, it refers to a specific stmt in NODE (the stmt at index
   J).  Otherwise, MARK is PURE_SLP and J is -1, which indicates that all the
   stmts in NODE are to be marked.  */

static void
vect_mark_slp_stmts (slp_tree node, enum slp_vect_type mark, int j)
{
  int i;
  gimple *stmt;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    if (j < 0 || i == j)
      STMT_SLP_TYPE (vinfo_for_stmt (stmt)) = mark;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_mark_slp_stmts (child, mark, j);
}


/* Mark the statements of the tree rooted at NODE as relevant (vect_used).  */

static void
vect_mark_slp_stmts_relevant (slp_tree node)
{
  int i;
  gimple *stmt;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (!STMT_VINFO_RELEVANT (stmt_info)
                  || STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope);
      STMT_VINFO_RELEVANT (stmt_info) = vect_used_in_scope;
    }

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_mark_slp_stmts_relevant (child);
}


/* Rearrange the statements of NODE according to PERMUTATION.  */

static void
vect_slp_rearrange_stmts (slp_tree node, unsigned int group_size,
                          vec<unsigned> permutation)
{
  gimple *stmt;
  vec<gimple *> tmp_stmts;
  unsigned int i;
  slp_tree child;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_slp_rearrange_stmts (child, group_size, permutation);

  gcc_assert (group_size == SLP_TREE_SCALAR_STMTS (node).length ());
  tmp_stmts.create (group_size);
  tmp_stmts.quick_grow_cleared (group_size);

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    tmp_stmts[permutation[i]] = stmt;

  SLP_TREE_SCALAR_STMTS (node).release ();
  SLP_TREE_SCALAR_STMTS (node) = tmp_stmts;
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
  vect_slp_rearrange_stmts (SLP_INSTANCE_TREE (slp_instn), group_size,
			    node->load_permutation);

  /* We are done, no actual permutations need to be generated.  */
  poly_uint64 unrolling_factor = SLP_INSTANCE_UNROLLING_FACTOR (slp_instn);
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    {
      gimple *first_stmt = SLP_TREE_SCALAR_STMTS (node)[0];
      first_stmt = DR_GROUP_FIRST_ELEMENT (vinfo_for_stmt (first_stmt));
      /* But we have to keep those permutations that are required because
         of handling of gaps.  */
      if (known_eq (unrolling_factor, 1U)
	  || (group_size == DR_GROUP_SIZE (vinfo_for_stmt (first_stmt))
	      && DR_GROUP_GAP (vinfo_for_stmt (first_stmt)) == 0))
	SLP_TREE_LOAD_PERMUTATION (node).release ();
      else
	for (j = 0; j < SLP_TREE_LOAD_PERMUTATION (node).length (); ++j)
	  SLP_TREE_LOAD_PERMUTATION (node)[j] = j;
    }

  return true;
}

/* Check if the required load permutations in the SLP instance
   SLP_INSTN are supported.  */

static bool
vect_supported_load_permutation_p (slp_instance slp_instn)
{
  unsigned int group_size = SLP_INSTANCE_GROUP_SIZE (slp_instn);
  unsigned int i, j, k, next;
  slp_tree node;
  gimple *stmt, *load, *next_load;

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
  stmt = SLP_TREE_SCALAR_STMTS (node)[0];

  /* Reduction (there are no data-refs in the root).
     In reduction chain the order of the loads is not important.  */
  if (!STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt))
      && !REDUC_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    vect_attempt_slp_rearrange_stmts (slp_instn);

  /* In basic block vectorization we allow any subchain of an interleaving
     chain.
     FORNOW: not supported in loop SLP because of realignment compications.  */
  if (STMT_VINFO_BB_VINFO (vinfo_for_stmt (stmt)))
    {
      /* Check whether the loads in an instance form a subchain and thus
         no permutation is necessary.  */
      FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
        {
	  if (!SLP_TREE_LOAD_PERMUTATION (node).exists ())
	    continue;
	  bool subchain_p = true;
          next_load = NULL;
          FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load)
            {
              if (j != 0
		  && (next_load != load
		      || DR_GROUP_GAP (vinfo_for_stmt (load)) != 1))
		{
		  subchain_p = false;
		  break;
		}
              next_load = DR_GROUP_NEXT_ELEMENT (vinfo_for_stmt (load));
            }
	  if (subchain_p)
	    SLP_TREE_LOAD_PERMUTATION (node).release ();
	  else
	    {
	      stmt_vec_info group_info
		= vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (node)[0]);
	      group_info = vinfo_for_stmt (DR_GROUP_FIRST_ELEMENT (group_info));
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
			     (STMT_VINFO_LOOP_VINFO (vinfo_for_stmt (stmt))));
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    if (node->load_permutation.exists ()
	&& !vect_transform_slp_perm_load (node, vNULL, NULL, test_vf,
					  slp_instn, true, &n_perms))
      return false;

  return true;
}


/* Find the last store in SLP INSTANCE.  */

gimple *
vect_find_last_scalar_stmt_in_slp (slp_tree node)
{
  gimple *last = NULL, *stmt;

  for (int i = 0; SLP_TREE_SCALAR_STMTS (node).iterate (i, &stmt); i++)
    {
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
      if (is_pattern_stmt_p (stmt_vinfo))
	last = get_later_stmt (STMT_VINFO_RELATED_STMT (stmt_vinfo), last);
      else
	last = get_later_stmt (stmt, last);
    }

  return last;
}

/* Splits a group of stores, currently beginning at FIRST_STMT, into two groups:
   one (still beginning at FIRST_STMT) of size GROUP1_SIZE (also containing
   the first GROUP1_SIZE stmts, since stores are consecutive), the second
   containing the remainder.
   Return the first stmt in the second group.  */

static gimple *
vect_split_slp_store_group (gimple *first_stmt, unsigned group1_size)
{
  stmt_vec_info first_vinfo = vinfo_for_stmt (first_stmt);
  gcc_assert (DR_GROUP_FIRST_ELEMENT (first_vinfo) == first_stmt);
  gcc_assert (group1_size > 0);
  int group2_size = DR_GROUP_SIZE (first_vinfo) - group1_size;
  gcc_assert (group2_size > 0);
  DR_GROUP_SIZE (first_vinfo) = group1_size;

  gimple *stmt = first_stmt;
  for (unsigned i = group1_size; i > 1; i--)
    {
      stmt = DR_GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt));
      gcc_assert (DR_GROUP_GAP (vinfo_for_stmt (stmt)) == 1);
    }
  /* STMT is now the last element of the first group.  */
  gimple *group2 = DR_GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt));
  DR_GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt)) = 0;

  DR_GROUP_SIZE (vinfo_for_stmt (group2)) = group2_size;
  for (stmt = group2; stmt; stmt = DR_GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt)))
    {
      DR_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) = group2;
      gcc_assert (DR_GROUP_GAP (vinfo_for_stmt (stmt)) == 1);
    }

  /* For the second group, the DR_GROUP_GAP is that before the original group,
     plus skipping over the first vector.  */
  DR_GROUP_GAP (vinfo_for_stmt (group2))
    = DR_GROUP_GAP (first_vinfo) + group1_size;

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
			   gimple *stmt, unsigned max_tree_size)
{
  slp_instance new_instance;
  slp_tree node;
  unsigned int group_size;
  tree vectype, scalar_type = NULL_TREE;
  gimple *next;
  unsigned int i;
  vec<slp_tree> loads;
  struct data_reference *dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt));
  vec<gimple *> scalar_stmts;

  if (STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt)))
    {
      scalar_type = TREE_TYPE (DR_REF (dr));
      vectype = get_vectype_for_scalar_type (scalar_type);
      group_size = DR_GROUP_SIZE (vinfo_for_stmt (stmt));
    }
  else if (!dr && REDUC_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      gcc_assert (is_a <loop_vec_info> (vinfo));
      vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
      group_size = REDUC_GROUP_SIZE (vinfo_for_stmt (stmt));
    }
  else
    {
      gcc_assert (is_a <loop_vec_info> (vinfo));
      vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
      group_size = as_a <loop_vec_info> (vinfo)->reductions.length ();
    }

  if (!vectype)
    {
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Build SLP failed: unsupported data-type ");
          dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, scalar_type);
          dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
        }

      return false;
    }
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* Create a node (a root of the SLP tree) for the packed grouped stores.  */
  scalar_stmts.create (group_size);
  next = stmt;
  if (STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt)))
    {
      /* Collect the stores and store them in SLP_TREE_SCALAR_STMTS.  */
      while (next)
        {
	  if (STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (next))
	      && STMT_VINFO_RELATED_STMT (vinfo_for_stmt (next)))
	    scalar_stmts.safe_push (
		  STMT_VINFO_RELATED_STMT (vinfo_for_stmt (next)));
	  else
            scalar_stmts.safe_push (next);
          next = DR_GROUP_NEXT_ELEMENT (vinfo_for_stmt (next));
        }
    }
  else if (!dr && REDUC_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      /* Collect the reduction stmts and store them in
	 SLP_TREE_SCALAR_STMTS.  */
      while (next)
        {
	  if (STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (next))
	      && STMT_VINFO_RELATED_STMT (vinfo_for_stmt (next)))
	    scalar_stmts.safe_push (
		  STMT_VINFO_RELATED_STMT (vinfo_for_stmt (next)));
	  else
            scalar_stmts.safe_push (next);
          next = REDUC_GROUP_NEXT_ELEMENT (vinfo_for_stmt (next));
        }
      /* Mark the first element of the reduction chain as reduction to properly
	 transform the node.  In the reduction analysis phase only the last
	 element of the chain is marked as reduction.  */
      STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) = vect_reduction_def;
    }
  else
    {
      /* Collect reduction statements.  */
      vec<gimple *> reductions = as_a <loop_vec_info> (vinfo)->reductions;
      for (i = 0; reductions.iterate (i, &next); i++)
	scalar_stmts.safe_push (next);
    }

  loads.create (group_size);

  /* Build the tree for the SLP instance.  */
  bool *matches = XALLOCAVEC (bool, group_size);
  unsigned npermutes = 0;
  bst_fail = new scalar_stmts_set_t ();
  poly_uint64 max_nunits = nunits;
  node = vect_build_slp_tree (vinfo, scalar_stmts, group_size,
			      &max_nunits, &loads, matches, &npermutes,
			      NULL, max_tree_size);
  delete bst_fail;
  if (node != NULL)
    {
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
	      vect_free_slp_tree (node);
	      loads.release ();
	      return false;
	    }
	  /* Fatal mismatch.  */
	  matches[group_size / const_max_nunits * const_max_nunits] = false;
	  vect_free_slp_tree (node);
	  loads.release ();
	}
      else
	{
      /* Create a new SLP instance.  */
      new_instance = XNEW (struct _slp_instance);
      SLP_INSTANCE_TREE (new_instance) = node;
      SLP_INSTANCE_GROUP_SIZE (new_instance) = group_size;
      SLP_INSTANCE_UNROLLING_FACTOR (new_instance) = unrolling_factor;
      SLP_INSTANCE_LOADS (new_instance) = loads;

      /* Compute the load permutation.  */
      slp_tree load_node;
      bool loads_permuted = false;
      FOR_EACH_VEC_ELT (loads, i, load_node)
	{
	  vec<unsigned> load_permutation;
	  int j;
	  gimple *load, *first_stmt;
	  bool this_load_permuted = false;
	  load_permutation.create (group_size);
	  first_stmt = DR_GROUP_FIRST_ELEMENT
	      (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (load_node)[0]));
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (load_node), j, load)
	    {
		  int load_place = vect_get_place_in_interleaving_chain
				     (load, first_stmt);
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
		  || (group_size == DR_GROUP_SIZE (vinfo_for_stmt (first_stmt))
		      && DR_GROUP_GAP (vinfo_for_stmt (first_stmt)) == 0)))
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
                {
                  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "Build SLP failed: unsupported load "
				   "permutation ");
		      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION,
					TDF_SLIM, stmt, 0);
                }
              vect_free_slp_instance (new_instance);
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
	  FOR_EACH_VEC_ELT (loads, i, load_node)
	    {
	      gimple *first_stmt = DR_GROUP_FIRST_ELEMENT
		  (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (load_node)[0]));
	      stmt_vec_info stmt_vinfo = vinfo_for_stmt (first_stmt);
		  /* Use SLP for strided accesses (or if we
		     can't load-lanes).  */
	      if (STMT_VINFO_STRIDED_P (stmt_vinfo)
		  || ! vect_load_lanes_supported
			(STMT_VINFO_VECTYPE (stmt_vinfo),
			 DR_GROUP_SIZE (stmt_vinfo), false))
		break;
	    }
	  if (i == loads.length ())
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Built SLP cancelled: can use "
				 "load/store-lanes\n");
	      vect_free_slp_instance (new_instance);
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
  loads.release ();
    }

  /* For basic block SLP, try to break the group up into multiples of the
     vector size.  */
  unsigned HOST_WIDE_INT const_nunits;
  if (is_a <bb_vec_info> (vinfo)
      && STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt))
      && DR_GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt))
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

	  gimple *rest = vect_split_slp_store_group (stmt, group1_size);
	  bool res = vect_analyze_slp_instance (vinfo, stmt, max_tree_size);
	  /* If the first non-match was in the middle of a vector,
	     skip the rest of that vector.  */
	  if (group1_size < i)
	    {
	      i = group1_size + const_nunits;
	      if (i < group_size)
		rest = vect_split_slp_store_group (rest, const_nunits);
	    }
	  if (i < group_size)
	    res |= vect_analyze_slp_instance (vinfo, rest, max_tree_size);
	  return res;
	}
      /* Even though the first vector did not all match, we might be able to SLP
	 (some) of the remainder.  FORNOW ignore this possibility.  */
    }

  return false;
}


/* Check if there are stmts in the loop can be vectorized using SLP.  Build SLP
   trees of packed scalar stmts if SLP is possible.  */

bool
vect_analyze_slp (vec_info *vinfo, unsigned max_tree_size)
{
  unsigned int i;
  gimple *first_element;

  DUMP_VECT_SCOPE ("vect_analyze_slp");

  /* Find SLP sequences starting from groups of grouped stores.  */
  FOR_EACH_VEC_ELT (vinfo->grouped_stores, i, first_element)
    vect_analyze_slp_instance (vinfo, first_element, max_tree_size);

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      if (loop_vinfo->reduction_chains.length () > 0)
	{
	  /* Find SLP sequences starting from reduction chains.  */
	  FOR_EACH_VEC_ELT (loop_vinfo->reduction_chains, i, first_element)
	    if (! vect_analyze_slp_instance (vinfo, first_element,
					     max_tree_size))
	      {
		/* Dissolve reduction chain group.  */
		gimple *next, *stmt = first_element;
		while (stmt)
		  {
		    stmt_vec_info vinfo = vinfo_for_stmt (stmt);
		    next = REDUC_GROUP_NEXT_ELEMENT (vinfo);
		    REDUC_GROUP_FIRST_ELEMENT (vinfo) = NULL;
		    REDUC_GROUP_NEXT_ELEMENT (vinfo) = NULL;
		    stmt = next;
		  }
		STMT_VINFO_DEF_TYPE (vinfo_for_stmt (first_element))
		  = vect_internal_def;
	      }
	}

      /* Find SLP sequences starting from groups of reductions.  */
      if (loop_vinfo->reductions.length () > 1)
	vect_analyze_slp_instance (vinfo, loop_vinfo->reductions[0],
				   max_tree_size);
    }

  return true;
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
      /* All unroll factors have the form current_vector_size * X for some
	 rational X, so they must have a common multiple.  */
      unrolling_factor
	= force_common_multiple (unrolling_factor,
				 SLP_INSTANCE_UNROLLING_FACTOR (instance));

      /* Mark all the stmts that belong to INSTANCE as PURE_SLP stmts.  Later we
	 call vect_detect_hybrid_slp () to find stmts that need hybrid SLP and
	 loop-based vectorization.  Such stmts will be marked as HYBRID.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance), pure_slp, -1);
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
vect_detect_hybrid_slp_stmts (slp_tree node, unsigned i, slp_vect_type stype)
{
  gimple *stmt = SLP_TREE_SCALAR_STMTS (node)[i];
  imm_use_iterator imm_iter;
  gimple *use_stmt;
  stmt_vec_info use_vinfo, stmt_vinfo = vinfo_for_stmt (stmt);
  slp_tree child;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int j;

  /* Propagate hybrid down the SLP tree.  */
  if (stype == hybrid)
    ;
  else if (HYBRID_SLP_STMT (stmt_vinfo))
    stype = hybrid;
  else
    {
      /* Check if a pure SLP stmt has uses in non-SLP stmts.  */
      gcc_checking_assert (PURE_SLP_STMT (stmt_vinfo));
      /* If we get a pattern stmt here we have to use the LHS of the
         original stmt for immediate uses.  */
      if (! STMT_VINFO_IN_PATTERN_P (stmt_vinfo)
	  && STMT_VINFO_RELATED_STMT (stmt_vinfo))
	stmt = STMT_VINFO_RELATED_STMT (stmt_vinfo);
      tree def;
      if (gimple_code (stmt) == GIMPLE_PHI)
	def = gimple_phi_result (stmt);
      else
	def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF);
      if (def)
	FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
	  {
	    if (!flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
	      continue;
	    use_vinfo = vinfo_for_stmt (use_stmt);
	    if (STMT_VINFO_IN_PATTERN_P (use_vinfo)
		&& STMT_VINFO_RELATED_STMT (use_vinfo))
	      use_vinfo = vinfo_for_stmt (STMT_VINFO_RELATED_STMT (use_vinfo));
	    if (!STMT_SLP_TYPE (use_vinfo)
		&& (STMT_VINFO_RELEVANT (use_vinfo)
		    || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (use_vinfo)))
		&& !(gimple_code (use_stmt) == GIMPLE_PHI
		     && STMT_VINFO_DEF_TYPE (use_vinfo) == vect_reduction_def))
	      {
		if (dump_enabled_p ())
		  {
		    dump_printf_loc (MSG_NOTE, vect_location, "use of SLP "
				     "def in non-SLP stmt: ");
		    dump_gimple_stmt (MSG_NOTE, TDF_SLIM, use_stmt, 0);
		  }
		stype = hybrid;
	      }
	  }
    }

  if (stype == hybrid
      && !HYBRID_SLP_STMT (stmt_vinfo))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "marking hybrid: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	}
      STMT_SLP_TYPE (stmt_vinfo) = hybrid;
    }

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_external_def)
      vect_detect_hybrid_slp_stmts (child, i, stype);
}

/* Helpers for vect_detect_hybrid_slp walking pattern stmt uses.  */

static tree
vect_detect_hybrid_slp_1 (tree *tp, int *, void *data)
{
  walk_stmt_info *wi = (walk_stmt_info *)data;
  struct loop *loopp = (struct loop *)wi->info;

  if (wi->is_lhs)
    return NULL_TREE;

  if (TREE_CODE (*tp) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (*tp))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (*tp);
      if (flow_bb_inside_loop_p (loopp, gimple_bb (def_stmt))
	  && PURE_SLP_STMT (vinfo_for_stmt (def_stmt)))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location, "marking hybrid: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, def_stmt, 0);
	    }
	  STMT_SLP_TYPE (vinfo_for_stmt (def_stmt)) = hybrid;
	}
    }

  return NULL_TREE;
}

static tree
vect_detect_hybrid_slp_2 (gimple_stmt_iterator *gsi, bool *handled,
			  walk_stmt_info *)
{
  stmt_vec_info use_vinfo = vinfo_for_stmt (gsi_stmt (*gsi));
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
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
	    {
	      walk_stmt_info wi;
	      memset (&wi, 0, sizeof (wi));
	      wi.info = LOOP_VINFO_LOOP (loop_vinfo);
	      gimple_stmt_iterator gsi2
		= gsi_for_stmt (STMT_VINFO_RELATED_STMT (stmt_info));
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
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      for (unsigned i = 0; i < SLP_INSTANCE_GROUP_SIZE (instance); ++i)
	vect_detect_hybrid_slp_stmts (SLP_INSTANCE_TREE (instance),
				      i, pure_slp);
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
      set_vinfo_for_stmt (stmt, new_stmt_vec_info (stmt, this));
    }

  bb->aux = this;
}


/* Free BB_VINFO struct, as well as all the stmt_vec_info structs of all the
   stmts in the basic block.  */

_bb_vec_info::~_bb_vec_info ()
{
  for (gimple_stmt_iterator si = region_begin;
       gsi_stmt (si) != gsi_stmt (region_end); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

      if (stmt_info)
        /* Free stmt_vec_info.  */
        free_stmt_vec_info (stmt);

      /* Reset region marker.  */
      gimple_set_uid (stmt, -1);
    }

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
  gimple *stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  gcc_assert (stmt_info);
  gcc_assert (STMT_SLP_TYPE (stmt_info) != loop_vect);

  /* For BB vectorization vector types are assigned here.
     Memory accesses already got their vector type assigned
     in vect_analyze_data_refs.  */
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  if (bb_vinfo
      && ! STMT_VINFO_DATA_REF (stmt_info))
    {
      tree vectype, nunits_vectype;
      if (!vect_get_vector_types_for_stmt (stmt_info, &vectype,
					   &nunits_vectype))
	/* We checked this when building the node.  */
	gcc_unreachable ();
      if (vectype == boolean_type_node)
	{
	  vectype = vect_get_mask_type_for_stmt (stmt_info);
	  if (!vectype)
	    /* vect_get_mask_type_for_stmt has already explained the
	       failure.  */
	    return false;
	}

      gimple *sstmt;
      unsigned int i;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, sstmt)
	STMT_VINFO_VECTYPE (vinfo_for_stmt (sstmt)) = vectype;
    }

  /* Calculate the number of vector statements to be created for the
     scalar stmts in this node.  For SLP reductions it is equal to the
     number of vector statements in the children (which has already been
     calculated by the recursive call).  Otherwise it is the number of
     scalar elements in one scalar iteration (DR_GROUP_SIZE) multiplied by
     VF divided by the number of elements in a vector.  */
  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    SLP_TREE_NUMBER_OF_VEC_STMTS (node)
      = SLP_TREE_NUMBER_OF_VEC_STMTS (SLP_TREE_CHILDREN (node)[0]);
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
  return vect_analyze_stmt (stmt, &dummy, node, node_instance, cost_vec);
}

/* Analyze statements contained in SLP tree NODE after recursively analyzing
   the subtree.  NODE_INSTANCE contains NODE and VINFO contains INSTANCE.

   Return true if the operations are supported.  */

static bool
vect_slp_analyze_node_operations (vec_info *vinfo, slp_tree node,
				  slp_instance node_instance,
				  scalar_stmts_to_slp_tree_map_t *visited,
				  scalar_stmts_to_slp_tree_map_t *lvisited,
				  stmt_vector_for_cost *cost_vec)
{
  int i, j;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return true;

  /* If we already analyzed the exact same set of scalar stmts we're done.
     We share the generated vector stmts for those.  */
  slp_tree *leader;
  if ((leader = visited->get (SLP_TREE_SCALAR_STMTS (node)))
      || (leader = lvisited->get (SLP_TREE_SCALAR_STMTS (node))))
    {
      SLP_TREE_NUMBER_OF_VEC_STMTS (node)
	= SLP_TREE_NUMBER_OF_VEC_STMTS (*leader);
      return true;
    }

  /* The SLP graph is acyclic so not caching whether we failed or succeeded
     doesn't result in any issue since we throw away the lvisited set
     when we fail.  */
  lvisited->put (SLP_TREE_SCALAR_STMTS (node).copy (), node);

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (!vect_slp_analyze_node_operations (vinfo, child, node_instance,
					   visited, lvisited, cost_vec))
      return false;

  /* Push SLP node def-type to stmt operands.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      STMT_VINFO_DEF_TYPE (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (child)[0]))
	= SLP_TREE_DEF_TYPE (child);
  bool res = vect_slp_analyze_node_operations_1 (vinfo, node, node_instance,
						 cost_vec);
  /* Restore def-types.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      STMT_VINFO_DEF_TYPE (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (child)[0]))
	= vect_internal_def;
  if (! res)
    return false;

  return true;
}


/* Analyze statements in SLP instances of VINFO.  Return true if the
   operations are supported. */

bool
vect_slp_analyze_operations (vec_info *vinfo)
{
  slp_instance instance;
  int i;

  DUMP_VECT_SCOPE ("vect_slp_analyze_operations");

  scalar_stmts_to_slp_tree_map_t *visited
    = new scalar_stmts_to_slp_tree_map_t ();
  for (i = 0; vinfo->slp_instances.iterate (i, &instance); )
    {
      scalar_stmts_to_slp_tree_map_t lvisited;
      stmt_vector_for_cost cost_vec;
      cost_vec.create (2);
      if (!vect_slp_analyze_node_operations (vinfo,
					     SLP_INSTANCE_TREE (instance),
					     instance, visited, &lvisited,
					     &cost_vec))
        {
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "removing SLP instance operations starting from: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM,
			    SLP_TREE_SCALAR_STMTS
			      (SLP_INSTANCE_TREE (instance))[0], 0);
	  vect_free_slp_instance (instance);
          vinfo->slp_instances.ordered_remove (i);
	  cost_vec.release ();
	}
      else
	{
	  for (scalar_stmts_to_slp_tree_map_t::iterator x = lvisited.begin();
	       x != lvisited.end(); ++x)
	    visited->put ((*x).first.copy (), (*x).second);
	  i++;

	  add_stmt_costs (vinfo->target_cost_data, &cost_vec);
	  cost_vec.release ();
	}
    }
  delete visited;

  return !vinfo->slp_instances.is_empty ();
}


/* Compute the scalar cost of the SLP node NODE and its children
   and return it.  Do not account defs that are marked in LIFE and
   update LIFE according to uses of NODE.  */

static void 
vect_bb_slp_scalar_cost (basic_block bb,
			 slp_tree node, vec<bool, va_heap> *life,
			 stmt_vector_for_cost *cost_vec)
{
  unsigned i;
  gimple *stmt;
  slp_tree child;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      ssa_op_iter op_iter;
      def_operand_p def_p;
      stmt_vec_info stmt_info;

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
	    if (!is_gimple_debug (use_stmt)
		&& (! vect_stmt_in_region_p (vinfo_for_stmt (stmt)->vinfo,
					     use_stmt)
		    || ! PURE_SLP_STMT (vinfo_for_stmt (use_stmt))))
	      {
		(*life)[i] = true;
		BREAK_FROM_IMM_USE_STMT (use_iter);
	      }
	}
      if ((*life)[i])
	continue;

      /* Count scalar stmts only once.  */
      if (gimple_visited_p (stmt))
	continue;
      gimple_set_visited (stmt, true);

      stmt_info = vinfo_for_stmt (stmt);
      vect_cost_for_stmt kind;
      if (STMT_VINFO_DATA_REF (stmt_info))
        {
          if (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
	    kind = scalar_load;
          else
	    kind = scalar_store;
        }
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
	  vect_bb_slp_scalar_cost (bb, child, &subtree_life, cost_vec);
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
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      auto_vec<bool, 20> life;
      life.safe_grow_cleared (SLP_INSTANCE_GROUP_SIZE (instance));
      vect_bb_slp_scalar_cost (BB_VINFO_BB (bb_vinfo),
			       SLP_INSTANCE_TREE (instance),
			       &life, &scalar_costs);
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

/* Check if the basic block can be vectorized.  Returns a bb_vec_info
   if so and sets fatal to true if failure is independent of
   current_vector_size.  */

static bb_vec_info
vect_slp_analyze_bb_1 (gimple_stmt_iterator region_begin,
		       gimple_stmt_iterator region_end,
		       vec<data_reference_p> datarefs, int n_stmts,
		       bool &fatal, vec_info_shared *shared)
{
  bb_vec_info bb_vinfo;
  slp_instance instance;
  int i;
  poly_uint64 min_vf = 2;

  /* The first group of checks is independent of the vector size.  */
  fatal = true;

  if (n_stmts > PARAM_VALUE (PARAM_SLP_MAX_INSNS_IN_BB))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: too many instructions in "
			 "basic block.\n");
      free_data_refs (datarefs);
      return NULL;
    }

  bb_vinfo = new _bb_vec_info (region_begin, region_end, shared);
  if (!bb_vinfo)
    return NULL;

  BB_VINFO_DATAREFS (bb_vinfo) = datarefs;
  bb_vinfo->shared->save_datarefs ();

  /* Analyze the data references.  */

  if (!vect_analyze_data_refs (bb_vinfo, &min_vf))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: unhandled data-ref in basic "
			 "block.\n");

      delete bb_vinfo;
      return NULL;
    }

  if (BB_VINFO_DATAREFS (bb_vinfo).length () < 2)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: not enough data-refs in "
			 "basic block.\n");

      delete bb_vinfo;
      return NULL;
    }

  if (!vect_analyze_data_ref_accesses (bb_vinfo))
    {
     if (dump_enabled_p ())
       dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"not vectorized: unhandled data access in "
			"basic block.\n");

      delete bb_vinfo;
      return NULL;
    }

  /* If there are no grouped stores in the region there is no need
     to continue with pattern recog as vect_analyze_slp will fail
     anyway.  */
  if (bb_vinfo->grouped_stores.is_empty ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: no grouped stores in "
			 "basic block.\n");

      delete bb_vinfo;
      return NULL;
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

      delete bb_vinfo;
      return NULL;
    }

  vect_record_base_alignments (bb_vinfo);

  /* Analyze and verify the alignment of data references and the
     dependence in the SLP instances.  */
  for (i = 0; BB_VINFO_SLP_INSTANCES (bb_vinfo).iterate (i, &instance); )
    {
      if (! vect_slp_analyze_and_verify_instance_alignment (instance)
	  || ! vect_slp_analyze_instance_dependence (instance))
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "removing SLP instance operations starting from: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM,
			    SLP_TREE_SCALAR_STMTS
			      (SLP_INSTANCE_TREE (instance))[0], 0);
	  vect_free_slp_instance (instance);
	  BB_VINFO_SLP_INSTANCES (bb_vinfo).ordered_remove (i);
	  continue;
	}

      /* Mark all the statements that we want to vectorize as pure SLP and
	 relevant.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance), pure_slp, -1);
      vect_mark_slp_stmts_relevant (SLP_INSTANCE_TREE (instance));

      i++;
    }
  if (! BB_VINFO_SLP_INSTANCES (bb_vinfo).length ())
    {
      delete bb_vinfo;
      return NULL;
    }

  if (!vect_slp_analyze_operations (bb_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: bad operation in basic block.\n");

      delete bb_vinfo;
      return NULL;
    }

  /* Cost model: check if the vectorization is worthwhile.  */
  if (!unlimited_cost_model (NULL)
      && !vect_bb_vectorization_profitable_p (bb_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: vectorization is not "
			 "profitable.\n");

      delete bb_vinfo;
      return NULL;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Basic block will be vectorized using SLP\n");

  return bb_vinfo;
}


/* Main entry for the BB vectorizer.  Analyze and transform BB, returns
   true if anything in the basic-block was vectorized.  */

bool
vect_slp_bb (basic_block bb)
{
  bb_vec_info bb_vinfo;
  gimple_stmt_iterator gsi;
  bool any_vectorized = false;
  auto_vector_sizes vector_sizes;

  DUMP_VECT_SCOPE ("vect_slp_analyze_bb");

  /* Autodetect first vector size we try.  */
  current_vector_size = 0;
  targetm.vectorize.autovectorize_vector_sizes (&vector_sizes);
  unsigned int next_size = 0;

  gsi = gsi_start_bb (bb);

  poly_uint64 autodetected_vector_size = 0;
  while (1)
    {
      if (gsi_end_p (gsi))
	break;

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

      bool vectorized = false;
      bool fatal = false;
      vec_info_shared shared;
      bb_vinfo = vect_slp_analyze_bb_1 (region_begin, region_end,
					datarefs, insns, fatal, &shared);
      if (bb_vinfo
	  && dbg_cnt (vect_slp))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "SLPing BB part\n");

	  bb_vinfo->shared->check_datarefs ();
	  vect_schedule_slp (bb_vinfo);

	  unsigned HOST_WIDE_INT bytes;
	  if (current_vector_size.is_constant (&bytes))
	    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
			     "basic block part vectorized using "
			     HOST_WIDE_INT_PRINT_UNSIGNED " byte "
			     "vectors\n", bytes);
	  else
	    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
			     "basic block part vectorized using variable "
			     "length vectors\n");

	  vectorized = true;
	}
      delete bb_vinfo;

      any_vectorized |= vectorized;

      if (next_size == 0)
	autodetected_vector_size = current_vector_size;

      if (next_size < vector_sizes.length ()
	  && known_eq (vector_sizes[next_size], autodetected_vector_size))
	next_size += 1;

      if (vectorized
	  || next_size == vector_sizes.length ()
	  || known_eq (current_vector_size, 0U)
	  /* If vect_slp_analyze_bb_1 signaled that analysis for all
	     vector sizes will fail do not bother iterating.  */
	  || fatal)
	{
	  if (gsi_end_p (region_end))
	    break;

	  /* Skip the unhandled stmt.  */
	  gsi_next (&gsi);

	  /* And reset vector sizes.  */
	  current_vector_size = 0;
	  next_size = 0;
	}
      else
	{
	  /* Try the next biggest vector size.  */
	  current_vector_size = vector_sizes[next_size++];
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "***** Re-trying analysis with "
			       "vector size ");
	      dump_dec (MSG_NOTE, current_vector_size);
	      dump_printf (MSG_NOTE, "\n");
	    }

	  /* Start over.  */
	  gsi = region_begin;
	}
    }

  return any_vectorized;
}


/* Return 1 if vector type of boolean constant which is OPNUM
   operand in statement STMT is a boolean vector.  */

static bool
vect_mask_constant_operand_p (gimple *stmt, int opnum)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  enum tree_code code = gimple_expr_code (stmt);
  tree op, vectype;
  enum vect_def_type dt;

  /* For comparison and COND_EXPR type is chosen depending
     on the other comparison operand.  */
  if (TREE_CODE_CLASS (code) == tcc_comparison)
    {
      if (opnum)
	op = gimple_assign_rhs1 (stmt);
      else
	op = gimple_assign_rhs2 (stmt);

      if (!vect_is_simple_use (op, stmt_vinfo->vinfo, &dt, &vectype))
	gcc_unreachable ();

      return !vectype || VECTOR_BOOLEAN_TYPE_P (vectype);
    }

  if (code == COND_EXPR)
    {
      tree cond = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (cond) == SSA_NAME)
	op = cond;
      else if (opnum)
	op = TREE_OPERAND (cond, 1);
      else
	op = TREE_OPERAND (cond, 0);

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
duplicate_and_interleave (gimple_seq *seq, tree vector_type, vec<tree> elts,
			  unsigned int nresults, vec<tree> &results)
{
  unsigned int nelts = elts.length ();
  tree element_type = TREE_TYPE (vector_type);

  /* (1) Find a vector mode VM with integer elements of mode IM.  */
  unsigned int nvectors = 1;
  tree new_vector_type;
  tree permutes[2];
  if (!can_duplicate_and_interleave_p (nelts, TYPE_MODE (element_type),
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
   OP_NUM determines if we gather defs for operand 0 or operand 1 of the RHS of
   scalar stmts.  NUMBER_OF_VECTORS is the number of vector defs to create.
   REDUC_INDEX is the index of the reduction operand in the statements, unless
   it is -1.  */

static void
vect_get_constant_vectors (tree op, slp_tree slp_node,
                           vec<tree> *vec_oprnds,
			   unsigned int op_num, unsigned int number_of_vectors)
{
  vec<gimple *> stmts = SLP_TREE_SCALAR_STMTS (slp_node);
  gimple *stmt = stmts[0];
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  unsigned HOST_WIDE_INT nunits;
  tree vec_cst;
  unsigned j, number_of_places_left_in_vector;
  tree vector_type;
  tree vop;
  int group_size = stmts.length ();
  unsigned int vec_num, i;
  unsigned number_of_copies = 1;
  vec<tree> voprnds;
  voprnds.create (number_of_vectors);
  bool constant_p, is_store;
  tree neutral_op = NULL;
  enum tree_code code = gimple_expr_code (stmt);
  gimple_seq ctor_seq = NULL;
  auto_vec<tree, 16> permute_results;

  /* Check if vector type is a boolean vector.  */
  if (VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (op))
      && vect_mask_constant_operand_p (stmt, op_num))
    vector_type
      = build_same_sized_truth_vector_type (STMT_VINFO_VECTYPE (stmt_vinfo));
  else
    vector_type = get_vectype_for_scalar_type (TREE_TYPE (op));

  if (STMT_VINFO_DATA_REF (stmt_vinfo))
    {
      is_store = true;
      op = gimple_assign_rhs1 (stmt);
    }
  else
    is_store = false;

  gcc_assert (op);

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
      for (i = group_size - 1; stmts.iterate (i, &stmt); i--)
        {
          if (is_store)
            op = gimple_assign_rhs1 (stmt);
          else
	    {
	      switch (code)
		{
		  case COND_EXPR:
		    {
		      tree cond = gimple_assign_rhs1 (stmt);
		      if (TREE_CODE (cond) == SSA_NAME)
			op = gimple_op (stmt, op_num + 1);
		      else if (op_num == 0 || op_num == 1)
			op = TREE_OPERAND (cond, op_num);
		      else
			{
			  if (op_num == 2)
			    op = gimple_assign_rhs2 (stmt);
			  else
			    op = gimple_assign_rhs3 (stmt);
			}
		    }
		    break;

		  case CALL_EXPR:
		    op = gimple_call_arg (stmt, op_num);
		    break;

		  case LSHIFT_EXPR:
		  case RSHIFT_EXPR:
		  case LROTATE_EXPR:
		  case RROTATE_EXPR:
		    op = gimple_op (stmt, op_num + 1);
		    /* Unlike the other binary operators, shifts/rotates have
		       the shift count being int, instead of the same type as
		       the lhs, so make sure the scalar is the right type if
		       we are dealing with vectors of
		       long long/long/short/char.  */
		    if (op_num == 1 && TREE_CODE (op) == INTEGER_CST)
		      op = fold_convert (TREE_TYPE (vector_type), op);
		    break;

		  default:
		    op = gimple_op (stmt, op_num + 1);
		    break;
		}
	    }

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
		  if (vec_oprnds->is_empty ())
		    duplicate_and_interleave (&ctor_seq, vector_type, elts,
					      number_of_vectors,
					      permute_results);
		  vec_cst = permute_results[number_of_vectors - j - 1];
		}
	      tree init;
	      gimple_stmt_iterator gsi;
	      if (place_after_defs)
		{
		  gsi = gsi_for_stmt
		          (vect_find_last_scalar_stmt_in_slp (slp_node));
		  init = vect_init_vector (stmt, vec_cst, vector_type, &gsi);
		}
	      else
		init = vect_init_vector (stmt, vec_cst, vector_type, NULL);
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

  voprnds.release ();

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
  tree vec_oprnd;
  gimple *vec_def_stmt;
  unsigned int i;

  gcc_assert (SLP_TREE_VEC_STMTS (slp_node).exists ());

  FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (slp_node), i, vec_def_stmt)
    {
      gcc_assert (vec_def_stmt);
      if (gimple_code (vec_def_stmt) == GIMPLE_PHI)
	vec_oprnd = gimple_phi_result (vec_def_stmt);
      else
	vec_oprnd = gimple_get_lhs (vec_def_stmt);
      vec_oprnds->quick_push (vec_oprnd);
    }
}


/* Get vectorized definitions for SLP_NODE.
   If the scalar definitions are loop invariants or constants, collect them and
   call vect_get_constant_vectors() to create vector stmts.
   Otherwise, the def-stmts must be already vectorized and the vectorized stmts
   must be stored in the corresponding child of SLP_NODE, and we call
   vect_get_slp_vect_defs () to retrieve them.  */

void
vect_get_slp_defs (vec<tree> ops, slp_tree slp_node,
		   vec<vec<tree> > *vec_oprnds)
{
  gimple *first_stmt;
  int number_of_vects = 0, i;
  unsigned int child_index = 0;
  HOST_WIDE_INT lhs_size_unit, rhs_size_unit;
  slp_tree child = NULL;
  vec<tree> vec_defs;
  tree oprnd;
  bool vectorized_defs;

  first_stmt = SLP_TREE_SCALAR_STMTS (slp_node)[0];
  FOR_EACH_VEC_ELT (ops, i, oprnd)
    {
      /* For each operand we check if it has vectorized definitions in a child
	 node or we need to create them (for invariants and constants).  We
	 check if the LHS of the first stmt of the next child matches OPRND.
	 If it does, we found the correct child.  Otherwise, we call
	 vect_get_constant_vectors (), and not advance CHILD_INDEX in order
	 to check this child node for the next operand.  */
      vectorized_defs = false;
      if (SLP_TREE_CHILDREN (slp_node).length () > child_index)
        {
          child = SLP_TREE_CHILDREN (slp_node)[child_index];

	  /* We have to check both pattern and original def, if available.  */
	  if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	    {
	      gimple *first_def = SLP_TREE_SCALAR_STMTS (child)[0];
	      gimple *related
		= STMT_VINFO_RELATED_STMT (vinfo_for_stmt (first_def));
	      tree first_def_op;

	      if (gimple_code (first_def) == GIMPLE_PHI)
		first_def_op = gimple_phi_result (first_def);
	      else
		first_def_op = gimple_get_lhs (first_def);
	      if (operand_equal_p (oprnd, first_def_op, 0)
		  || (related
		      && operand_equal_p (oprnd, gimple_get_lhs (related), 0)))
		{
		  /* The number of vector defs is determined by the number of
		     vector statements in the node from which we get those
		     statements.  */
		  number_of_vects = SLP_TREE_NUMBER_OF_VEC_STMTS (child);
		  vectorized_defs = true;
		  child_index++;
		}
	    }
	  else
	    child_index++;
        }

      if (!vectorized_defs)
        {
          if (i == 0)
            {
              number_of_vects = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
              /* Number of vector stmts was calculated according to LHS in
                 vect_schedule_slp_instance (), fix it by replacing LHS with
                 RHS, if necessary.  See vect_get_smallest_scalar_type () for
                 details.  */
              vect_get_smallest_scalar_type (first_stmt, &lhs_size_unit,
                                             &rhs_size_unit);
              if (rhs_size_unit != lhs_size_unit)
                {
                  number_of_vects *= rhs_size_unit;
                  number_of_vects /= lhs_size_unit;
                }
            }
        }

      /* Allocate memory for vectorized defs.  */
      vec_defs = vNULL;
      vec_defs.create (number_of_vects);

      /* For reduction defs we call vect_get_constant_vectors (), since we are
         looking for initial loop invariant values.  */
      if (vectorized_defs)
        /* The defs are already vectorized.  */
	vect_get_slp_vect_defs (child, &vec_defs);
      else
	/* Build vectors from scalar defs.  */
	vect_get_constant_vectors (oprnd, slp_node, &vec_defs, i,
				   number_of_vects);

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
  gimple *stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree mask_element_type = NULL_TREE, mask_type;
  int vec_index = 0;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  int group_size = SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
  unsigned int mask_element;
  machine_mode mode;
  unsigned HOST_WIDE_INT nunits, const_vf;

  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
    return false;

  stmt_info = vinfo_for_stmt (DR_GROUP_FIRST_ELEMENT (stmt_info));

  mode = TYPE_MODE (vectype);

  /* At the moment, all permutations are represented using per-element
     indices, so we can't cope with variable vector lengths or
     vectorization factors.  */
  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&nunits)
      || !vf.is_constant (&const_vf))
    return false;

  /* The generic VEC_PERM_EXPR code always uses an integral type of the
     same size as the vector element being permuted.  */
  mask_element_type = lang_hooks.types.type_for_mode
    (int_mode_for_mode (TYPE_MODE (TREE_TYPE (vectype))).require (), 1);
  mask_type = get_vectype_for_scalar_type (mask_element_type);
  vec_perm_builder mask (nunits, nunits, 1);
  mask.quick_grow (nunits);
  vec_perm_indices indices;

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

  for (unsigned int j = 0; j < const_vf; j++)
    {
      for (int k = 0; k < group_size; k++)
	{
	  unsigned int i = (SLP_TREE_LOAD_PERMUTATION (node)[k]
			    + j * DR_GROUP_SIZE (stmt_info));
	  vec_index = i / nunits;
	  mask_element = i % nunits;
	  if (vec_index == first_vec_index
	      || first_vec_index == -1)
	    {
	      first_vec_index = vec_index;
	    }
	  else if (vec_index == second_vec_index
		   || second_vec_index == -1)
	    {
	      second_vec_index = vec_index;
	      mask_element += nunits;
	    }
	  else
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "permutation requires at "
				   "least three vectors ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				    stmt, 0);
		}
	      gcc_assert (analyze_only);
	      return false;
	    }

	  gcc_assert (mask_element < 2 * nunits);
	  if (mask_element != index)
	    noop_p = false;
	  mask[index++] = mask_element;

	  if (index == nunits && !noop_p)
	    {
	      indices.new_vector (mask, 2, nunits);
	      if (!can_vec_perm_const_p (mode, indices))
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				       vect_location, 
				       "unsupported vect permute { ");
		      for (i = 0; i < nunits; ++i)
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

	  if (index == nunits)
	    {
	      if (!analyze_only)
		{
		  tree mask_vec = NULL_TREE;
		  
		  if (! noop_p)
		    mask_vec = vec_perm_indices_to_tree (mask_type, indices);

		  if (second_vec_index == -1)
		    second_vec_index = first_vec_index;

		  /* Generate the permute statement if necessary.  */
		  tree first_vec = dr_chain[first_vec_index];
		  tree second_vec = dr_chain[second_vec_index];
		  gimple *perm_stmt;
		  if (! noop_p)
		    {
		      tree perm_dest
			= vect_create_destination_var (gimple_assign_lhs (stmt),
						       vectype);
		      perm_dest = make_ssa_name (perm_dest);
		      perm_stmt = gimple_build_assign (perm_dest,
						       VEC_PERM_EXPR,
						       first_vec, second_vec,
						       mask_vec);
		      vect_finish_stmt_generation (stmt, perm_stmt, gsi);
		    }
		  else
		    /* If mask was NULL_TREE generate the requested
		       identity transform.  */
		    perm_stmt = SSA_NAME_DEF_STMT (first_vec);

		  /* Store the vector statement in NODE.  */
		  SLP_TREE_VEC_STMTS (node)[vect_stmts_counter++] = perm_stmt;
		}

	      index = 0;
	      first_vec_index = -1;
	      second_vec_index = -1;
	      noop_p = true;
	    }
	}
    }

  return true;
}

/* Vectorize SLP instance tree in postorder.  */

static bool
vect_schedule_slp_instance (slp_tree node, slp_instance instance,
			    scalar_stmts_to_slp_tree_map_t *bst_map)
{
  gimple *stmt;
  bool grouped_store, is_store;
  gimple_stmt_iterator si;
  stmt_vec_info stmt_info;
  unsigned int group_size;
  tree vectype;
  int i, j;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return false;

  /* See if we have already vectorized the same set of stmts and reuse their
     vectorized stmts.  */
  if (slp_tree *leader = bst_map->get (SLP_TREE_SCALAR_STMTS (node)))
    {
      SLP_TREE_VEC_STMTS (node).safe_splice (SLP_TREE_VEC_STMTS (*leader));
      return false;
    }

  bst_map->put (SLP_TREE_SCALAR_STMTS (node).copy (), node);
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_schedule_slp_instance (child, instance, bst_map);

  /* Push SLP node def-type to stmts.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (child), j, stmt)
	STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) = SLP_TREE_DEF_TYPE (child);

  stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  stmt_info = vinfo_for_stmt (stmt);

  /* VECTYPE is the type of the destination.  */
  vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  group_size = SLP_INSTANCE_GROUP_SIZE (instance);

  gcc_assert (SLP_TREE_NUMBER_OF_VEC_STMTS (node) != 0);
  if (!SLP_TREE_VEC_STMTS (node).exists ())
    SLP_TREE_VEC_STMTS (node).create (SLP_TREE_NUMBER_OF_VEC_STMTS (node));

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE,vect_location,
		       "------>vectorizing SLP node starting from: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
    }

  /* Vectorized stmts go before the last scalar stmt which is where
     all uses are ready.  */
  si = gsi_for_stmt (vect_find_last_scalar_stmt_in_slp (node));

  /* Mark the first element of the reduction chain as reduction to properly
     transform the node.  In the analysis phase only the last element of the
     chain is marked as reduction.  */
  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
      && REDUC_GROUP_FIRST_ELEMENT (stmt_info) == stmt)
    {
      STMT_VINFO_DEF_TYPE (stmt_info) = vect_reduction_def;
      STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
    }

  /* Handle two-operation SLP nodes by vectorizing the group with
     both operations and then performing a merge.  */
  if (SLP_TREE_TWO_OPERATORS (node))
    {
      enum tree_code code0 = gimple_assign_rhs_code (stmt);
      enum tree_code ocode = ERROR_MARK;
      gimple *ostmt;
      vec_perm_builder mask (group_size, group_size, 1);
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, ostmt)
	if (gimple_assign_rhs_code (ostmt) != code0)
	  {
	    mask.quick_push (1);
	    ocode = gimple_assign_rhs_code (ostmt);
	  }
	else
	  mask.quick_push (0);
      if (ocode != ERROR_MARK)
	{
	  vec<gimple *> v0;
	  vec<gimple *> v1;
	  unsigned j;
	  tree tmask = NULL_TREE;
	  vect_transform_stmt (stmt, &si, &grouped_store, node, instance);
	  v0 = SLP_TREE_VEC_STMTS (node).copy ();
	  SLP_TREE_VEC_STMTS (node).truncate (0);
	  gimple_assign_set_rhs_code (stmt, ocode);
	  vect_transform_stmt (stmt, &si, &grouped_store, node, instance);
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
					   gimple_assign_lhs (v0[j]),
					   gimple_assign_lhs (v1[j]), tmask);
	      vect_finish_stmt_generation (stmt, vstmt, &si);
	      SLP_TREE_VEC_STMTS (node).quick_push (vstmt);
	    }
	  v0.release ();
	  v1.release ();
	  return false;
	}
    }
  is_store = vect_transform_stmt (stmt, &si, &grouped_store, node, instance);

  /* Restore stmt def-types.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (child), j, stmt)
	STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) = vect_internal_def;

  return is_store;
}

/* Replace scalar calls from SLP node NODE with setting of their lhs to zero.
   For loop vectorization this is done in vectorizable_call, but for SLP
   it needs to be deferred until end of vect_schedule_slp, because multiple
   SLP instances may refer to the same scalar stmt.  */

static void
vect_remove_slp_scalar_calls (slp_tree node)
{
  gimple *stmt, *new_stmt;
  gimple_stmt_iterator gsi;
  int i;
  slp_tree child;
  tree lhs;
  stmt_vec_info stmt_info;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_remove_slp_scalar_calls (child);

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      if (!is_gimple_call (stmt) || gimple_bb (stmt) == NULL)
	continue;
      stmt_info = vinfo_for_stmt (stmt);
      if (stmt_info == NULL
	  || is_pattern_stmt_p (stmt_info)
	  || !PURE_SLP_STMT (stmt_info))
	continue;
      lhs = gimple_call_lhs (stmt);
      new_stmt = gimple_build_assign (lhs, build_zero_cst (TREE_TYPE (lhs)));
      set_vinfo_for_stmt (new_stmt, stmt_info);
      set_vinfo_for_stmt (stmt, NULL);
      STMT_VINFO_STMT (stmt_info) = new_stmt;
      gsi = gsi_for_stmt (stmt);
      gsi_replace (&gsi, new_stmt, false);
      SSA_NAME_DEF_STMT (gimple_assign_lhs (new_stmt)) = new_stmt;
    }
}

/* Generate vector code for all SLP instances in the loop/basic block.  */

bool
vect_schedule_slp (vec_info *vinfo)
{
  vec<slp_instance> slp_instances;
  slp_instance instance;
  unsigned int i;
  bool is_store = false;


  scalar_stmts_to_slp_tree_map_t *bst_map
    = new scalar_stmts_to_slp_tree_map_t ();
  slp_instances = vinfo->slp_instances;
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      /* Schedule the tree of INSTANCE.  */
      is_store = vect_schedule_slp_instance (SLP_INSTANCE_TREE (instance),
                                             instance, bst_map);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "vectorizing stmts using SLP.\n");
    }
  delete bst_map;

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      slp_tree root = SLP_INSTANCE_TREE (instance);
      gimple *store;
      unsigned int j;
      gimple_stmt_iterator gsi;

      /* Remove scalar call stmts.  Do not do this for basic-block
	 vectorization as not all uses may be vectorized.
	 ???  Why should this be necessary?  DCE should be able to
	 remove the stmts itself.
	 ???  For BB vectorization we can as well remove scalar
	 stmts starting from the SLP tree root if they have no
	 uses.  */
      if (is_a <loop_vec_info> (vinfo))
	vect_remove_slp_scalar_calls (root);

      for (j = 0; SLP_TREE_SCALAR_STMTS (root).iterate (j, &store)
                  && j < SLP_INSTANCE_GROUP_SIZE (instance); j++)
        {
          if (!STMT_VINFO_DATA_REF (vinfo_for_stmt (store)))
            break;

         if (is_pattern_stmt_p (vinfo_for_stmt (store)))
           store = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (store));
          /* Free the attached stmt_vec_info and remove the stmt.  */
          gsi = gsi_for_stmt (store);
	  unlink_stmt_vdef (store);
          gsi_remove (&gsi, true);
	  release_defs (store);
          free_stmt_vec_info (store);
        }
    }

  return is_store;
}
