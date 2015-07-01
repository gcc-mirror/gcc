/* SLP - Basic Block Vectorization
   Copyright (C) 2007-2015 Free Software Foundation, Inc.
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
#include "dumpfile.h"
#include "tm.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "target.h"
#include "predict.h"
#include "hard-reg-set.h"
#include "function.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "rtl.h"
#include "flags.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "recog.h"		/* FIXME: for insn_data */
#include "insn-codes.h"
#include "optabs.h"
#include "tree-vectorizer.h"
#include "langhooks.h"
#include "gimple-walk.h"

/* Extract the location of the basic block in the source code.
   Return the basic block location if succeed and NULL if not.  */

source_location
find_bb_location (basic_block bb)
{
  gimple stmt = NULL;
  gimple_stmt_iterator si;

  if (!bb)
    return UNKNOWN_LOCATION;

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      stmt = gsi_stmt (si);
      if (gimple_location (stmt) != UNKNOWN_LOCATION)
        return gimple_location (stmt);
    }

  return UNKNOWN_LOCATION;
}


/* Recursively free the memory allocated for the SLP tree rooted at NODE.  */

static void
vect_free_slp_tree (slp_tree node)
{
  int i;
  slp_tree child;

  if (!node)
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_free_slp_tree (child);

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
vect_create_new_slp_node (vec<gimple> scalar_stmts)
{
  slp_tree node;
  gimple stmt = scalar_stmts[0];
  unsigned int nops;

  if (is_gimple_call (stmt))
    nops = gimple_call_num_args (stmt);
  else if (is_gimple_assign (stmt))
    {
      nops = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	nops++;
    }
  else
    return NULL;

  node = XNEW (struct _slp_tree);
  SLP_TREE_SCALAR_STMTS (node) = scalar_stmts;
  SLP_TREE_VEC_STMTS (node).create (0);
  SLP_TREE_CHILDREN (node).create (nops);
  SLP_TREE_LOAD_PERMUTATION (node) = vNULL;
  SLP_TREE_TWO_OPERATORS (node) = false;

  return node;
}


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

static int
vect_get_place_in_interleaving_chain (gimple stmt, gimple first_stmt)
{
  gimple next_stmt = first_stmt;
  int result = 0;

  if (first_stmt != GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    return -1;

  do
    {
      if (next_stmt == stmt)
	return result;
      next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
      if (next_stmt)
	result += GROUP_GAP (vinfo_for_stmt (next_stmt));
    }
  while (next_stmt);

  return -1;
}


/* Get the defs for the rhs of STMT (collect them in OPRNDS_INFO), check that
   they are of a valid type and that they match the defs of the first stmt of
   the SLP group (stored in OPRNDS_INFO).  If there was a fatal error
   return -1, if the error could be corrected by swapping operands of the
   operation return 1, if everything is ok return 0.  */

static int 
vect_get_and_check_slp_defs (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                             gimple stmt, unsigned stmt_num,
                             vec<slp_oprnd_info> *oprnds_info)
{
  tree oprnd;
  unsigned int i, number_of_oprnds;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt = vect_uninitialized_def;
  struct loop *loop = NULL;
  bool pattern = false;
  slp_oprnd_info oprnd_info;
  int first_op_idx = 1;
  bool commutative = false;
  bool first_op_cond = false;
  bool first = stmt_num == 0;
  bool second = stmt_num == 1;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  if (is_gimple_call (stmt))
    {
      number_of_oprnds = gimple_call_num_args (stmt);
      first_op_idx = 3;
    }
  else if (is_gimple_assign (stmt))
    {
      enum tree_code code = gimple_assign_rhs_code (stmt);
      number_of_oprnds = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	{
	  first_op_cond = true;
	  commutative = true;
	  number_of_oprnds++;
	}
      else
	commutative = commutative_tree_code (code);
    }
  else
    return -1;

  bool swapped = false;
  for (i = 0; i < number_of_oprnds; i++)
    {
again:
      if (first_op_cond)
	{
	  if (i == 0 || i == 1)
	    oprnd = TREE_OPERAND (gimple_op (stmt, first_op_idx),
				  swapped ? !i : i);
	  else
	    oprnd = gimple_op (stmt, first_op_idx + i - 1);
	}
      else
        oprnd = gimple_op (stmt, first_op_idx + (swapped ? !i : i));

      oprnd_info = (*oprnds_info)[i];

      if (!vect_is_simple_use (oprnd, NULL, loop_vinfo, bb_vinfo, &def_stmt,
			       &def, &dt))
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
          && ((loop && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt)))
	      || (!loop && gimple_bb (def_stmt) == BB_VINFO_BB (bb_vinfo)
		  && gimple_code (def_stmt) != GIMPLE_PHI))
          && vinfo_for_stmt (def_stmt)
          && STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (def_stmt))
	  && !STMT_VINFO_RELEVANT (vinfo_for_stmt (def_stmt))
	  && !STMT_VINFO_LIVE_P (vinfo_for_stmt (def_stmt)))
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

          def_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt));
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
                def = gimple_phi_result (def_stmt);
                break;

              case GIMPLE_ASSIGN:
                def = gimple_assign_lhs (def_stmt);
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
	  if (((oprnd_info->first_dt != dt
                && !(oprnd_info->first_dt == vect_reduction_def
                     && dt == vect_internal_def)
		&& !((oprnd_info->first_dt == vect_external_def
		      || oprnd_info->first_dt == vect_constant_def)
		     && (dt == vect_external_def
			 || dt == vect_constant_def)))
               || !types_compatible_p (oprnd_info->first_op_type,
				       TREE_TYPE (oprnd))))
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
	}

      /* Check the types of the definitions.  */
      switch (dt)
	{
	case vect_constant_def:
	case vect_external_def:
        case vect_reduction_def:
	  break;

	case vect_internal_def:
	  oprnd_info->def_stmts.quick_push (def_stmt);
	  break;

	default:
	  /* FORNOW: Not supported.  */
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: illegal type of def ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, def);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return -1;
	}
    }

  /* Swap operands.  */
  if (swapped)
    {
      if (first_op_cond)
	{
	  tree cond = gimple_assign_rhs1 (stmt);
	  swap_ssa_operands (stmt, &TREE_OPERAND (cond, 0),
			     &TREE_OPERAND (cond, 1));
	  TREE_SET_CODE (cond, swap_tree_comparison (TREE_CODE (cond)));
	}
      else
	swap_ssa_operands (stmt, gimple_assign_rhs1_ptr (stmt),
			   gimple_assign_rhs2_ptr (stmt));
    }

  return 0;
}


/* Verify if the scalar stmts STMTS are isomorphic, require data
   permutation or are of unsupported types of operation.  Return
   true if they are, otherwise return false and indicate in *MATCHES
   which stmts are not isomorphic to the first one.  If MATCHES[0]
   is false then this indicates the comparison could not be
   carried out or the stmts will never be vectorized by SLP.  */

static bool
vect_build_slp_tree_1 (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
		       vec<gimple> stmts, unsigned int group_size,
		       unsigned nops, unsigned int *max_nunits,
		       unsigned int vectorization_factor, bool *matches,
		       bool *two_operators)
{
  unsigned int i;
  gimple first_stmt = stmts[0], stmt = stmts[0];
  enum tree_code first_stmt_code = ERROR_MARK;
  enum tree_code alt_stmt_code = ERROR_MARK;
  enum tree_code rhs_code = ERROR_MARK;
  enum tree_code first_cond_code = ERROR_MARK;
  tree lhs;
  bool need_same_oprnds = false;
  tree vectype = NULL_TREE, scalar_type, first_op1 = NULL_TREE;
  optab optab;
  int icode;
  machine_mode optab_op2_mode;
  machine_mode vec_mode;
  HOST_WIDE_INT dummy;
  gimple first_load = NULL, prev_first_load = NULL;
  tree cond;

  /* For every stmt in NODE find its def stmt/s.  */
  FOR_EACH_VEC_ELT (stmts, i, stmt)
    {
      matches[i] = false;

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "Build SLP for ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
          dump_printf (MSG_NOTE, "\n");
	}

      /* Fail to vectorize statements marked as unvectorizable.  */
      if (!STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (stmt)))
        {
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: unvectorizable statement ");
              dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
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
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}

       if (is_gimple_assign (stmt)
	   && gimple_assign_rhs_code (stmt) == COND_EXPR
           && (cond = gimple_assign_rhs1 (stmt))
           && !COMPARISON_CLASS_P (cond))
        {
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
			       "Build SLP failed: condition is not "
			       "comparison ");
              dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
            }
	  /* Fatal mismatch.  */
	  matches[0] = false;
          return false;
        }

      scalar_type = vect_get_smallest_scalar_type (stmt, &dummy, &dummy);
      vectype = get_vectype_for_scalar_type (scalar_type);
      if (!vectype)
        {
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
			       "Build SLP failed: unsupported data-type ");
              dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				 scalar_type);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
            }
	  /* Fatal mismatch.  */
	  matches[0] = false;
          return false;
        }

      /* If populating the vector type requires unrolling then fail
         before adjusting *max_nunits for basic-block vectorization.  */
      if (bb_vinfo
	  && TYPE_VECTOR_SUBPARTS (vectype) > group_size)
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
			   "Build SLP failed: unrolling required "
			   "in basic block SLP\n");
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}

      /* In case of multiple types we need to detect the smallest type.  */
      if (*max_nunits < TYPE_VECTOR_SUBPARTS (vectype))
        {
          *max_nunits = TYPE_VECTOR_SUBPARTS (vectype);
          if (bb_vinfo)
            vectorization_factor = *max_nunits;
        }

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
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
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
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}
	      /* Mismatch.  */
	      continue;
	    }

	  if (rhs_code == CALL_EXPR)
	    {
	      gimple first_stmt = stmts[0];
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
                      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
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
              /* Check that the size of interleaved loads group is not
                 greater than the SLP group size.  */
	      unsigned ncopies
		= vectorization_factor / TYPE_VECTOR_SUBPARTS (vectype);
              if (loop_vinfo
		  && GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) == stmt
                  && ((GROUP_SIZE (vinfo_for_stmt (stmt))
		       - GROUP_GAP (vinfo_for_stmt (stmt)))
		      > ncopies * group_size))
                {
                  if (dump_enabled_p ())
                    {
                      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				       "Build SLP failed: the number "
				       "of interleaved loads is greater than "
				       "the SLP group size ");
                      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					stmt, 0);
                      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
                    }
		  /* Fatal mismatch.  */
		  matches[0] = false;
                  return false;
                }

              first_load = GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt));
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
                          dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
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
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
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
	      && rhs_code != CALL_EXPR)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "Build SLP failed: operation");
		  dump_printf (MSG_MISSED_OPTIMIZATION, " unsupported ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }

          if (rhs_code == COND_EXPR)
            {
              tree cond_expr = gimple_assign_rhs1 (stmt);

	      if (i == 0)
		first_cond_code = TREE_CODE (cond_expr);
              else if (first_cond_code != TREE_CODE (cond_expr))
                {
                  if (dump_enabled_p ())
                    {
                      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				       "Build SLP failed: different"
				       " operation");
                      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					stmt, 0);
                      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
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
      unsigned char *sel
	= XALLOCAVEC (unsigned char, TYPE_VECTOR_SUBPARTS (vectype));
      for (i = 0; i < TYPE_VECTOR_SUBPARTS (vectype); ++i)
	{
	  sel[i] = i;
	  if (gimple_assign_rhs_code (stmts[i % group_size]) == alt_stmt_code)
	    sel[i] += TYPE_VECTOR_SUBPARTS (vectype);
	}
      if (!can_vec_perm_p (TYPE_MODE (vectype), false, sel))
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

/* Recursively build an SLP tree starting from NODE.
   Fail (and return a value not equal to zero) if def-stmts are not
   isomorphic, require data permutation or are of unsupported types of
   operation.  Otherwise, return 0.
   The value returned is the depth in the SLP tree where a mismatch
   was found.  */

static bool
vect_build_slp_tree (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                     slp_tree *node, unsigned int group_size,
                     unsigned int *max_nunits,
                     vec<slp_tree> *loads,
                     unsigned int vectorization_factor,
		     bool *matches, unsigned *npermutes, unsigned *tree_size,
		     unsigned max_tree_size)
{
  unsigned nops, i, this_tree_size = 0;
  gimple stmt;

  matches[0] = false;

  stmt = SLP_TREE_SCALAR_STMTS (*node)[0];
  if (is_gimple_call (stmt))
    nops = gimple_call_num_args (stmt);
  else if (is_gimple_assign (stmt))
    {
      nops = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	nops++;
    }
  else
    return false;

  bool two_operators = false;
  if (!vect_build_slp_tree_1 (loop_vinfo, bb_vinfo,
			      SLP_TREE_SCALAR_STMTS (*node), group_size, nops,
			      max_nunits, vectorization_factor, matches,
			      &two_operators))
    return false;
  SLP_TREE_TWO_OPERATORS (*node) = two_operators;

  /* If the SLP node is a load, terminate the recursion.  */
  if (STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt))
      && DR_IS_READ (STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt))))
    {
      loads->safe_push (*node);
      return true;
    }

  /* Get at the operands, verifying they are compatible.  */
  vec<slp_oprnd_info> oprnds_info = vect_create_oprnd_info (nops, group_size);
  slp_oprnd_info oprnd_info;
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (*node), i, stmt)
    {
      switch (vect_get_and_check_slp_defs (loop_vinfo, bb_vinfo,
					   stmt, i, &oprnds_info))
	{
	case 0:
	  break;
	case -1:
	  matches[0] = false;
	  vect_free_oprnd_info (oprnds_info);
	  return false;
	case 1:
	  matches[i] = false;
	  break;
	}
    }
  for (i = 0; i < group_size; ++i)
    if (!matches[i])
      {
	vect_free_oprnd_info (oprnds_info);
	return false;
      }

  stmt = SLP_TREE_SCALAR_STMTS (*node)[0];

  /* Create SLP_TREE nodes for the definition node/s.  */
  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      slp_tree child;
      unsigned old_nloads = loads->length ();
      unsigned old_max_nunits = *max_nunits;

      if (oprnd_info->first_dt != vect_internal_def)
        continue;

      if (++this_tree_size > max_tree_size)
	{
	  vect_free_oprnd_info (oprnds_info);
	  return false;
	}

      child = vect_create_new_slp_node (oprnd_info->def_stmts);
      if (!child)
	{
	  vect_free_oprnd_info (oprnds_info);
	  return false;
	}

      if (vect_build_slp_tree (loop_vinfo, bb_vinfo, &child,
			       group_size, max_nunits, loads,
			       vectorization_factor, matches,
			       npermutes, &this_tree_size, max_tree_size))
	{
	  /* If we have all children of child built up from scalars then just
	     throw that away and build it up this node from scalars.  */
	  if (!SLP_TREE_CHILDREN (child).is_empty ())
	    {
	      unsigned int j;
	      slp_tree grandchild;

	      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		if (grandchild != NULL)
		  break;
	      if (!grandchild)
		{
		  /* Roll back.  */
		  *max_nunits = old_max_nunits;
		  loads->truncate (old_nloads);
		  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
		      vect_free_slp_tree (grandchild);
		  SLP_TREE_CHILDREN (child).truncate (0);

		  dump_printf_loc (MSG_NOTE, vect_location,
				   "Building parent vector operands from "
				   "scalars instead\n");
		  oprnd_info->def_stmts = vNULL;
		  vect_free_slp_tree (child);
		  SLP_TREE_CHILDREN (*node).quick_push (NULL);
		  continue;
		}
	    }

	  oprnd_info->def_stmts = vNULL;
	  SLP_TREE_CHILDREN (*node).quick_push (child);
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
      if (bb_vinfo
	  && !matches[0]
	  /* ???  Rejecting patterns this way doesn't work.  We'd have to
	     do extra work to cancel the pattern so the uses see the
	     scalar version.  */
	  && !is_pattern_stmt_p (vinfo_for_stmt (stmt)))
	{
	  unsigned int j;
	  slp_tree grandchild;

	  /* Roll back.  */
	  *max_nunits = old_max_nunits;
	  loads->truncate (old_nloads);
	  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
	    vect_free_slp_tree (grandchild);
	  SLP_TREE_CHILDREN (child).truncate (0);

	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Building vector operands from scalars\n");
	  oprnd_info->def_stmts = vNULL;
	  vect_free_slp_tree (child);
	  SLP_TREE_CHILDREN (*node).quick_push (NULL);
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
	  && commutative_tree_code (gimple_assign_rhs_code (stmt))
	  && !SLP_TREE_TWO_OPERATORS (*node)
	  /* Do so only if the number of not successful permutes was nor more
	     than a cut-ff as re-trying the recursive match on
	     possibly each level of the tree would expose exponential
	     behavior.  */
	  && *npermutes < 4)
	{
	  unsigned int j;
	  slp_tree grandchild;

	  /* Roll back.  */
	  *max_nunits = old_max_nunits;
	  loads->truncate (old_nloads);
	  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (child), j, grandchild)
	    vect_free_slp_tree (grandchild);
	  SLP_TREE_CHILDREN (child).truncate (0);

	  /* Swap mismatched definition stmts.  */
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Re-trying with swapped operands of stmts ");
	  for (j = 0; j < group_size; ++j)
	    if (!matches[j])
	      {
		std::swap (oprnds_info[0]->def_stmts[j],
			   oprnds_info[1]->def_stmts[j]);
		dump_printf (MSG_NOTE, "%d ", j);
	      }
	  dump_printf (MSG_NOTE, "\n");
	  /* And try again with scratch 'matches' ... */
	  bool *tem = XALLOCAVEC (bool, group_size);
	  if (vect_build_slp_tree (loop_vinfo, bb_vinfo, &child,
				   group_size, max_nunits, loads,
				   vectorization_factor,
				   tem, npermutes, &this_tree_size,
				   max_tree_size))
	    {
	      /* ... so if successful we can apply the operand swapping
		 to the GIMPLE IL.  This is necessary because for example
		 vect_get_slp_defs uses operand indexes and thus expects
		 canonical operand order.  */
	      for (j = 0; j < group_size; ++j)
		if (!matches[j])
		  {
		    gimple stmt = SLP_TREE_SCALAR_STMTS (*node)[j];
		    swap_ssa_operands (stmt, gimple_assign_rhs1_ptr (stmt),
				       gimple_assign_rhs2_ptr (stmt));
		  }
	      oprnd_info->def_stmts = vNULL;
	      SLP_TREE_CHILDREN (*node).quick_push (child);
	      continue;
	    }

	  ++*npermutes;
	}

      oprnd_info->def_stmts = vNULL;
      vect_free_slp_tree (child);
      vect_free_oprnd_info (oprnds_info);
      return false;
    }

  if (tree_size)
    *tree_size += this_tree_size;

  vect_free_oprnd_info (oprnds_info);
  return true;
}

/* Dump a slp tree NODE using flags specified in DUMP_KIND.  */

static void
vect_print_slp_tree (int dump_kind, slp_tree node)
{
  int i;
  gimple stmt;
  slp_tree child;

  if (!node)
    return;

  dump_printf (dump_kind, "node ");
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      dump_printf (dump_kind, "\n\tstmt %d ", i);
      dump_gimple_stmt (dump_kind, TDF_SLIM, stmt, 0);
    }
  dump_printf (dump_kind, "\n");

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_print_slp_tree (dump_kind, child);
}


/* Mark the tree rooted at NODE with MARK (PURE_SLP or HYBRID).
   If MARK is HYBRID, it refers to a specific stmt in NODE (the stmt at index
   J).  Otherwise, MARK is PURE_SLP and J is -1, which indicates that all the
   stmts in NODE are to be marked.  */

static void
vect_mark_slp_stmts (slp_tree node, enum slp_vect_type mark, int j)
{
  int i;
  gimple stmt;
  slp_tree child;

  if (!node)
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
  gimple stmt;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (!node)
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
  gimple stmt;
  vec<gimple> tmp_stmts;
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
  sbitmap load_index;
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
  load_index = sbitmap_alloc (group_size);
  bitmap_clear (load_index);
  FOR_EACH_VEC_ELT (node->load_permutation, i, lidx)
    {
      if (bitmap_bit_p (load_index, lidx))
	{
	  sbitmap_free (load_index);
	  return false;
	}
      bitmap_set_bit (load_index, lidx);
    }
  for (i = 0; i < group_size; i++)
    if (!bitmap_bit_p (load_index, i))
      {
	sbitmap_free (load_index);
	return false;
      }
  sbitmap_free (load_index);

  /* This permutation is valid for reduction.  Since the order of the
     statements in the nodes is not important unless they are memory
     accesses, we can rearrange the statements in all the nodes
     according to the order of the loads.  */
  vect_slp_rearrange_stmts (SLP_INSTANCE_TREE (slp_instn), group_size,
			    node->load_permutation);

  /* We are done, no actual permutations need to be generated.  */
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    SLP_TREE_LOAD_PERMUTATION (node).release ();
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
  gimple stmt, load, next_load, first_load;
  struct data_reference *dr;

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
      && !GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      if (vect_attempt_slp_rearrange_stmts (slp_instn))
	return true;

      /* Fallthru to general load permutation handling.  */
    }

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
		      || GROUP_GAP (vinfo_for_stmt (load)) != 1))
		{
		  subchain_p = false;
		  break;
		}
              next_load = GROUP_NEXT_ELEMENT (vinfo_for_stmt (load));
            }
	  if (subchain_p)
	    SLP_TREE_LOAD_PERMUTATION (node).release ();
	  else
	    {
	      /* Verify the permutation can be generated.  */
	      vec<tree> tem;
	      if (!vect_transform_slp_perm_load (node, tem, NULL,
						 1, slp_instn, true))
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				   vect_location,
				   "unsupported load permutation\n");
		  return false;
		}
	    }
        }

      /* Check that the alignment of the first load in every subchain, i.e.,
         the first statement in every load node, is supported.
	 ???  This belongs in alignment checking.  */
      FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
	{
	  first_load = SLP_TREE_SCALAR_STMTS (node)[0];
	  if (first_load != GROUP_FIRST_ELEMENT (vinfo_for_stmt (first_load)))
	    {
	      dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_load));
	      if (vect_supportable_dr_alignment (dr, false)
		  == dr_unaligned_unsupported)
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				       vect_location,
				       "unsupported unaligned load ");
		      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					first_load, 0);
                      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		    }
		  return false;
		}
	    }
	}

      return true;
    }

  /* For loop vectorization verify we can generate the permutation.  */
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    if (node->load_permutation.exists ()
	&& !vect_transform_slp_perm_load
	      (node, vNULL, NULL,
	       SLP_INSTANCE_UNROLLING_FACTOR (slp_instn), slp_instn, true))
      return false;

  return true;
}


/* Find the last store in SLP INSTANCE.  */

static gimple
vect_find_last_scalar_stmt_in_slp (slp_tree node)
{
  gimple last = NULL, stmt;

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

/* Compute the cost for the SLP node NODE in the SLP instance INSTANCE.  */

static void
vect_analyze_slp_cost_1 (slp_instance instance, slp_tree node,
			 stmt_vector_for_cost *prologue_cost_vec,
			 stmt_vector_for_cost *body_cost_vec,
			 unsigned ncopies_for_cost)
{
  unsigned i;
  slp_tree child;
  gimple stmt, s;
  stmt_vec_info stmt_info;
  tree lhs;
  unsigned group_size = SLP_INSTANCE_GROUP_SIZE (instance);

  /* Recurse down the SLP tree.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      vect_analyze_slp_cost_1 (instance, child, prologue_cost_vec,
			       body_cost_vec, ncopies_for_cost);

  /* Look at the first scalar stmt to determine the cost.  */
  stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  stmt_info = vinfo_for_stmt (stmt);
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      if (DR_IS_WRITE (STMT_VINFO_DATA_REF (stmt_info)))
	vect_model_store_cost (stmt_info, ncopies_for_cost, false,
			       vect_uninitialized_def,
			       node, prologue_cost_vec, body_cost_vec);
      else
	{
	  int i;
	  gcc_checking_assert (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)));
	  vect_model_load_cost (stmt_info, ncopies_for_cost, false,
				node, prologue_cost_vec, body_cost_vec);
	  /* If the load is permuted record the cost for the permutation.
	     ???  Loads from multiple chains are let through here only
	     for a single special case involving complex numbers where
	     in the end no permutation is necessary.  */
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, s)
	    if ((STMT_VINFO_GROUP_FIRST_ELEMENT (vinfo_for_stmt (s))
		 == STMT_VINFO_GROUP_FIRST_ELEMENT (stmt_info))
		&& vect_get_place_in_interleaving_chain
		     (s, STMT_VINFO_GROUP_FIRST_ELEMENT (stmt_info)) != i)
	      {
		record_stmt_cost (body_cost_vec, group_size, vec_perm,
				  stmt_info, 0, vect_body);
		break;
	      }
	}
    }
  else
    {
      record_stmt_cost (body_cost_vec, ncopies_for_cost, vector_stmt,
			stmt_info, 0, vect_body);
      if (SLP_TREE_TWO_OPERATORS (node))
	{
	  record_stmt_cost (body_cost_vec, ncopies_for_cost, vector_stmt,
			    stmt_info, 0, vect_body);
	  record_stmt_cost (body_cost_vec, ncopies_for_cost, vec_perm,
			    stmt_info, 0, vect_body);
	}
    }

  /* Scan operands and account for prologue cost of constants/externals.
     ???  This over-estimates cost for multiple uses and should be
     re-engineered.  */
  lhs = gimple_get_lhs (stmt);
  for (i = 0; i < gimple_num_ops (stmt); ++i)
    {
      tree def, op = gimple_op (stmt, i);
      gimple def_stmt;
      enum vect_def_type dt;
      if (!op || op == lhs)
	continue;
      if (vect_is_simple_use (op, NULL, STMT_VINFO_LOOP_VINFO (stmt_info),
			      STMT_VINFO_BB_VINFO (stmt_info),
			      &def_stmt, &def, &dt))
	{
	  /* Without looking at the actual initializer a vector of
	     constants can be implemented as load from the constant pool.
	     ???  We need to pass down stmt_info for a vector type
	     even if it points to the wrong stmt.  */
	  if (dt == vect_constant_def)
	    record_stmt_cost (prologue_cost_vec, 1, vector_load,
			      stmt_info, 0, vect_prologue);
	  else if (dt == vect_external_def)
	    record_stmt_cost (prologue_cost_vec, 1, vec_construct,
			      stmt_info, 0, vect_prologue);
	}
    }
}

/* Compute the cost for the SLP instance INSTANCE.  */

static void
vect_analyze_slp_cost (slp_instance instance, void *data)
{
  stmt_vector_for_cost body_cost_vec, prologue_cost_vec;
  unsigned ncopies_for_cost;
  stmt_info_for_cost *si;
  unsigned i;

  /* Calculate the number of vector stmts to create based on the unrolling
     factor (number of vectors is 1 if NUNITS >= GROUP_SIZE, and is
     GROUP_SIZE / NUNITS otherwise.  */
  unsigned group_size = SLP_INSTANCE_GROUP_SIZE (instance);
  slp_tree node = SLP_INSTANCE_TREE (instance);
  stmt_vec_info stmt_info = vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (node)[0]);
  /* Adjust the group_size by the vectorization factor which is always one
     for basic-block vectorization.  */
  if (STMT_VINFO_LOOP_VINFO (stmt_info))
    group_size *= LOOP_VINFO_VECT_FACTOR (STMT_VINFO_LOOP_VINFO (stmt_info));
  unsigned nunits = TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
  /* For reductions look at a reduction operand in case the reduction
     operation is widening like DOT_PROD or SAD.  */
  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      gimple stmt = SLP_TREE_SCALAR_STMTS (node)[0];
      switch (gimple_assign_rhs_code (stmt))
	{
	case DOT_PROD_EXPR:
	case SAD_EXPR:
	  nunits = TYPE_VECTOR_SUBPARTS (get_vectype_for_scalar_type
				(TREE_TYPE (gimple_assign_rhs1 (stmt))));
	  break;
	default:;
	}
    }
  ncopies_for_cost = least_common_multiple (nunits, group_size) / nunits;

  prologue_cost_vec.create (10);
  body_cost_vec.create (10);
  vect_analyze_slp_cost_1 (instance, SLP_INSTANCE_TREE (instance),
			   &prologue_cost_vec, &body_cost_vec,
			   ncopies_for_cost);

  /* Record the prologue costs, which were delayed until we were
     sure that SLP was successful.  */
  FOR_EACH_VEC_ELT (prologue_cost_vec, i, si)
    {
      struct _stmt_vec_info *stmt_info
	= si->stmt ? vinfo_for_stmt (si->stmt) : NULL;
      (void) add_stmt_cost (data, si->count, si->kind, stmt_info,
			    si->misalign, vect_prologue);
    }

  /* Record the instance's instructions in the target cost model.  */
  FOR_EACH_VEC_ELT (body_cost_vec, i, si)
    {
      struct _stmt_vec_info *stmt_info
	= si->stmt ? vinfo_for_stmt (si->stmt) : NULL;
      (void) add_stmt_cost (data, si->count, si->kind, stmt_info,
			    si->misalign, vect_body);
    }

  prologue_cost_vec.release ();
  body_cost_vec.release ();
}

/* Analyze an SLP instance starting from a group of grouped stores.  Call
   vect_build_slp_tree to build a tree of packed stmts if possible.
   Return FALSE if it's impossible to SLP any stmt in the loop.  */

static bool
vect_analyze_slp_instance (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                           gimple stmt, unsigned max_tree_size)
{
  slp_instance new_instance;
  slp_tree node;
  unsigned int group_size = GROUP_SIZE (vinfo_for_stmt (stmt));
  unsigned int unrolling_factor = 1, nunits;
  tree vectype, scalar_type = NULL_TREE;
  gimple next;
  unsigned int vectorization_factor = 0;
  int i;
  unsigned int max_nunits = 0;
  vec<slp_tree> loads;
  struct data_reference *dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt));
  vec<gimple> scalar_stmts;

  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      if (dr)
        {
          scalar_type = TREE_TYPE (DR_REF (dr));
          vectype = get_vectype_for_scalar_type (scalar_type);
        }
      else
        {
          gcc_assert (loop_vinfo);
          vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
        }

      group_size = GROUP_SIZE (vinfo_for_stmt (stmt));
    }
  else
    {
      gcc_assert (loop_vinfo);
      vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
      group_size = LOOP_VINFO_REDUCTIONS (loop_vinfo).length ();
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

  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  if (loop_vinfo)
    vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  else
    vectorization_factor = nunits;

  /* Calculate the unrolling factor.  */
  unrolling_factor = least_common_multiple (nunits, group_size) / group_size;
  if (unrolling_factor != 1 && !loop_vinfo)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Build SLP failed: unrolling required in basic"
			 " block SLP\n");

      return false;
    }

  /* Create a node (a root of the SLP tree) for the packed grouped stores.  */
  scalar_stmts.create (group_size);
  next = stmt;
  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
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
          next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next));
        }
      /* Mark the first element of the reduction chain as reduction to properly
	 transform the node.  In the reduction analysis phase only the last
	 element of the chain is marked as reduction.  */
      if (!STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt)))
	STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) = vect_reduction_def;
    }
  else
    {
      /* Collect reduction statements.  */
      vec<gimple> reductions = LOOP_VINFO_REDUCTIONS (loop_vinfo);
      for (i = 0; reductions.iterate (i, &next); i++)
	scalar_stmts.safe_push (next);
    }

  node = vect_create_new_slp_node (scalar_stmts);

  loads.create (group_size);

  /* Build the tree for the SLP instance.  */
  bool *matches = XALLOCAVEC (bool, group_size);
  unsigned npermutes = 0;
  if (vect_build_slp_tree (loop_vinfo, bb_vinfo, &node, group_size,
			   &max_nunits, &loads,
			   vectorization_factor, matches, &npermutes, NULL,
			   max_tree_size))
    {
      /* Calculate the unrolling factor based on the smallest type.  */
      if (max_nunits > nunits)
        unrolling_factor = least_common_multiple (max_nunits, group_size)
                           / group_size;

      if (unrolling_factor != 1 && !loop_vinfo)
        {
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: unrolling required in basic"
			     " block SLP\n");
	  vect_free_slp_tree (node);
	  loads.release ();
          return false;
        }

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
	  gimple load, first_stmt;
	  bool this_load_permuted = false;
	  load_permutation.create (group_size);
	  first_stmt = GROUP_FIRST_ELEMENT
	      (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (load_node)[0]));
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (load_node), j, load)
	    {
	      int load_place
		= vect_get_place_in_interleaving_chain (load, first_stmt);
	      gcc_assert (load_place != -1);
	      if (load_place != j)
		this_load_permuted = true;
	      load_permutation.safe_push (load_place);
	    }
	  if (!this_load_permuted
	      /* The load requires permutation when unrolling exposes
	         a gap either because the group is larger than the SLP
		 group-size or because there is a gap between the groups.  */
	      && (unrolling_factor == 1
		  || (group_size == GROUP_SIZE (vinfo_for_stmt (first_stmt))
		      && GROUP_GAP (vinfo_for_stmt (first_stmt)) == 0)))
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
                  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
                }
              vect_free_slp_instance (new_instance);
              return false;
            }
        }


      if (loop_vinfo)
	LOOP_VINFO_SLP_INSTANCES (loop_vinfo).safe_push (new_instance);
      else
        BB_VINFO_SLP_INSTANCES (bb_vinfo).safe_push (new_instance);

      if (dump_enabled_p ())
	vect_print_slp_tree (MSG_NOTE, node);

      return true;
    }

  /* Failed to SLP.  */
  /* Free the allocated memory.  */
  vect_free_slp_tree (node);
  loads.release ();

  return false;
}


/* Check if there are stmts in the loop can be vectorized using SLP.  Build SLP
   trees of packed scalar stmts if SLP is possible.  */

bool
vect_analyze_slp (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
		  unsigned max_tree_size)
{
  unsigned int i;
  vec<gimple> grouped_stores;
  vec<gimple> reductions = vNULL;
  vec<gimple> reduc_chains = vNULL;
  gimple first_element;
  bool ok = false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "=== vect_analyze_slp ===\n");

  if (loop_vinfo)
    {
      grouped_stores = LOOP_VINFO_GROUPED_STORES (loop_vinfo);
      reduc_chains = LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo);
      reductions = LOOP_VINFO_REDUCTIONS (loop_vinfo);
    }
  else
    grouped_stores = BB_VINFO_GROUPED_STORES (bb_vinfo);

  /* Find SLP sequences starting from groups of grouped stores.  */
  FOR_EACH_VEC_ELT (grouped_stores, i, first_element)
    if (vect_analyze_slp_instance (loop_vinfo, bb_vinfo, first_element,
				   max_tree_size))
      ok = true;

  if (reduc_chains.length () > 0)
    {
      /* Find SLP sequences starting from reduction chains.  */
      FOR_EACH_VEC_ELT (reduc_chains, i, first_element)
        if (vect_analyze_slp_instance (loop_vinfo, bb_vinfo, first_element,
				       max_tree_size))
          ok = true;
        else
          return false;

      /* Don't try to vectorize SLP reductions if reduction chain was
         detected.  */
      return ok;
    }

  /* Find SLP sequences starting from groups of reductions.  */
  if (reductions.length () > 1
      && vect_analyze_slp_instance (loop_vinfo, bb_vinfo, reductions[0],
				    max_tree_size))
    ok = true;

  return true;
}


/* For each possible SLP instance decide whether to SLP it and calculate overall
   unrolling factor needed to SLP the loop.  Return TRUE if decided to SLP at
   least one instance.  */

bool
vect_make_slp_decision (loop_vec_info loop_vinfo)
{
  unsigned int i, unrolling_factor = 1;
  vec<slp_instance> slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;
  int decided_to_slp = 0;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "=== vect_make_slp_decision ==="
                     "\n");

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      /* FORNOW: SLP if you can.  */
      if (unrolling_factor < SLP_INSTANCE_UNROLLING_FACTOR (instance))
	unrolling_factor = SLP_INSTANCE_UNROLLING_FACTOR (instance);

      /* Mark all the stmts that belong to INSTANCE as PURE_SLP stmts.  Later we
	 call vect_detect_hybrid_slp () to find stmts that need hybrid SLP and
	 loop-based vectorization.  Such stmts will be marked as HYBRID.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance), pure_slp, -1);
      decided_to_slp++;
    }

  LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo) = unrolling_factor;

  if (decided_to_slp && dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Decided to SLP %d instances. Unrolling factor %d\n",
		     decided_to_slp, unrolling_factor);

  return (decided_to_slp > 0);
}


/* Find stmts that must be both vectorized and SLPed (since they feed stmts that
   can't be SLPed) in the tree rooted at NODE.  Mark such stmts as HYBRID.  */

static void
vect_detect_hybrid_slp_stmts (slp_tree node, unsigned i, slp_vect_type stype)
{
  gimple stmt = SLP_TREE_SCALAR_STMTS (node)[i];
  imm_use_iterator imm_iter;
  gimple use_stmt;
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
      /* We always get the pattern stmt here, but for immediate
	 uses we have to use the LHS of the original stmt.  */
      gcc_checking_assert (!STMT_VINFO_IN_PATTERN_P (stmt_vinfo));
      if (STMT_VINFO_RELATED_STMT (stmt_vinfo))
	stmt = STMT_VINFO_RELATED_STMT (stmt_vinfo);
      if (TREE_CODE (gimple_op (stmt, 0)) == SSA_NAME)
	FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, gimple_op (stmt, 0))
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
    if (child)
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
      gimple def_stmt = SSA_NAME_DEF_STMT (*tp);
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
  /* If the stmt is in a SLP instance then this isn't a reason
     to mark use definitions in other SLP instances as hybrid.  */
  if (STMT_SLP_TYPE (vinfo_for_stmt (gsi_stmt (*gsi))) != loop_vect)
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

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "=== vect_detect_hybrid_slp ==="
                     "\n");

  /* First walk all pattern stmt in the loop and mark defs of uses as
     hybrid because immediate uses in them are not recorded.  */
  for (i = 0; i < LOOP_VINFO_LOOP (loop_vinfo)->num_nodes; ++i)
    {
      basic_block bb = LOOP_VINFO_BBS (loop_vinfo)[i];
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
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


/* Create and initialize a new bb_vec_info struct for BB, as well as
   stmt_vec_info structs for all the stmts in it.  */

static bb_vec_info
new_bb_vec_info (basic_block bb)
{
  bb_vec_info res = NULL;
  gimple_stmt_iterator gsi;

  res = (bb_vec_info) xcalloc (1, sizeof (struct _bb_vec_info));
  BB_VINFO_BB (res) = bb;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      gimple_set_uid (stmt, 0);
      set_vinfo_for_stmt (stmt, new_stmt_vec_info (stmt, NULL, res));
    }

  BB_VINFO_GROUPED_STORES (res).create (10);
  BB_VINFO_SLP_INSTANCES (res).create (2);
  BB_VINFO_TARGET_COST_DATA (res) = init_cost (NULL);

  bb->aux = res;
  return res;
}


/* Free BB_VINFO struct, as well as all the stmt_vec_info structs of all the
   stmts in the basic block.  */

static void
destroy_bb_vec_info (bb_vec_info bb_vinfo)
{
  vec<slp_instance> slp_instances;
  slp_instance instance;
  basic_block bb;
  gimple_stmt_iterator si;
  unsigned i;

  if (!bb_vinfo)
    return;

  bb = BB_VINFO_BB (bb_vinfo);

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple stmt = gsi_stmt (si);
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

      if (stmt_info)
        /* Free stmt_vec_info.  */
        free_stmt_vec_info (stmt);
    }

  vect_destroy_datarefs (NULL, bb_vinfo);
  free_dependence_relations (BB_VINFO_DDRS (bb_vinfo));
  BB_VINFO_GROUPED_STORES (bb_vinfo).release ();
  slp_instances = BB_VINFO_SLP_INSTANCES (bb_vinfo);
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    vect_free_slp_instance (instance);
  BB_VINFO_SLP_INSTANCES (bb_vinfo).release ();
  destroy_cost_data (BB_VINFO_TARGET_COST_DATA (bb_vinfo));
  free (bb_vinfo);
  bb->aux = NULL;
}


/* Analyze statements contained in SLP tree node after recursively analyzing
   the subtree. Return TRUE if the operations are supported.  */

static bool
vect_slp_analyze_node_operations (slp_tree node)
{
  bool dummy;
  int i;
  gimple stmt;
  slp_tree child;

  if (!node)
    return true;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (!vect_slp_analyze_node_operations (child))
      return false;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (stmt_info);
      gcc_assert (STMT_SLP_TYPE (stmt_info) != loop_vect);

      if (!vect_analyze_stmt (stmt, &dummy, node))
	return false;
    }

  return true;
}


/* Analyze statements in SLP instances of the basic block.  Return TRUE if the
   operations are supported. */

bool
vect_slp_analyze_operations (vec<slp_instance> slp_instances, void *data)
{
  slp_instance instance;
  int i;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_slp_analyze_operations ===\n");

  for (i = 0; slp_instances.iterate (i, &instance); )
    {
      if (!vect_slp_analyze_node_operations (SLP_INSTANCE_TREE (instance)))
        {
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "removing SLP instance operations starting from: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM,
			    SLP_TREE_SCALAR_STMTS
			      (SLP_INSTANCE_TREE (instance))[0], 0);
	  vect_free_slp_instance (instance);
          slp_instances.ordered_remove (i);
	}
      else
	{
	  /* Compute the costs of the SLP instance.  */
	  vect_analyze_slp_cost (instance, data);
	  i++;
	}
    }

  if (!slp_instances.length ())
    return false;

  return true;
}


/* Compute the scalar cost of the SLP node NODE and its children
   and return it.  Do not account defs that are marked in LIFE and
   update LIFE according to uses of NODE.  */

static unsigned
vect_bb_slp_scalar_cost (basic_block bb,
			 slp_tree node, vec<bool, va_heap> *life)
{
  unsigned scalar_cost = 0;
  unsigned i;
  gimple stmt;
  slp_tree child;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      unsigned stmt_cost;
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
	  gimple use_stmt;
	  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, DEF_FROM_PTR (def_p))
	    if (!is_gimple_debug (use_stmt)
		&& (gimple_code (use_stmt) == GIMPLE_PHI
		    || gimple_bb (use_stmt) != bb
		    || !STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (use_stmt))))
	      {
		(*life)[i] = true;
		BREAK_FROM_IMM_USE_STMT (use_iter);
	      }
	}
      if ((*life)[i])
	continue;

      stmt_info = vinfo_for_stmt (stmt);
      if (STMT_VINFO_DATA_REF (stmt_info))
        {
          if (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
            stmt_cost = vect_get_stmt_cost (scalar_load);
          else
            stmt_cost = vect_get_stmt_cost (scalar_store);
        }
      else
        stmt_cost = vect_get_stmt_cost (scalar_stmt);

      scalar_cost += stmt_cost;
    }

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      scalar_cost += vect_bb_slp_scalar_cost (bb, child, life);

  return scalar_cost;
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
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      auto_vec<bool, 20> life;
      life.safe_grow_cleared (SLP_INSTANCE_GROUP_SIZE (instance));
      scalar_cost += vect_bb_slp_scalar_cost (BB_VINFO_BB (bb_vinfo),
					      SLP_INSTANCE_TREE (instance),
					      &life);
    }

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

  /* Vectorization is profitable if its cost is less than the cost of scalar
     version.  */
  if (vec_outside_cost + vec_inside_cost >= scalar_cost)
    return false;

  return true;
}

/* Check if the basic block can be vectorized.  */

static bb_vec_info
vect_slp_analyze_bb_1 (basic_block bb)
{
  bb_vec_info bb_vinfo;
  vec<slp_instance> slp_instances;
  slp_instance instance;
  int i;
  int min_vf = 2;
  unsigned n_stmts = 0;

  bb_vinfo = new_bb_vec_info (bb);
  if (!bb_vinfo)
    return NULL;

  if (!vect_analyze_data_refs (NULL, bb_vinfo, &min_vf, &n_stmts))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: unhandled data-ref in basic "
			 "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  if (BB_VINFO_DATAREFS (bb_vinfo).length () < 2)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: not enough data-refs in "
			 "basic block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  if (!vect_analyze_data_ref_accesses (NULL, bb_vinfo))
    {
     if (dump_enabled_p ())
       dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"not vectorized: unhandled data access in "
			"basic block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  vect_pattern_recog (NULL, bb_vinfo);

  if (!vect_analyze_data_refs_alignment (NULL, bb_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: bad data alignment in basic "
			 "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  /* Check the SLP opportunities in the basic block, analyze and build SLP
     trees.  */
  if (!vect_analyze_slp (NULL, bb_vinfo, n_stmts))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Failed to SLP the basic block.\n");
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
			   "not vectorized: failed to find SLP opportunities "
			   "in basic block.\n");
	}

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  slp_instances = BB_VINFO_SLP_INSTANCES (bb_vinfo);

  /* Mark all the statements that we want to vectorize as pure SLP and
     relevant.  */
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance), pure_slp, -1);
      vect_mark_slp_stmts_relevant (SLP_INSTANCE_TREE (instance));
    }

  /* Mark all the statements that we do not want to vectorize.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (BB_VINFO_BB (bb_vinfo));
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt_vec_info vinfo = vinfo_for_stmt (gsi_stmt (gsi));
      if (STMT_SLP_TYPE (vinfo) != pure_slp)
	STMT_VINFO_VECTORIZABLE (vinfo) = false;
    }

  /* Analyze dependences.  At this point all stmts not participating in
     vectorization have to be marked.  Dependence analysis assumes
     that we either vectorize all SLP instances or none at all.  */
  if (!vect_slp_analyze_data_ref_dependences (bb_vinfo))
     {
       if (dump_enabled_p ())
	 dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			  "not vectorized: unhandled data dependence "
			  "in basic block.\n");

       destroy_bb_vec_info (bb_vinfo);
       return NULL;
     }

  if (!vect_verify_datarefs_alignment (NULL, bb_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not vectorized: unsupported alignment in basic "
                         "block.\n");
      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  if (!vect_slp_analyze_operations (BB_VINFO_SLP_INSTANCES (bb_vinfo),
				    BB_VINFO_TARGET_COST_DATA (bb_vinfo)))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: bad operation in basic block.\n");

      destroy_bb_vec_info (bb_vinfo);
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

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Basic block will be vectorized using SLP\n");

  return bb_vinfo;
}


bb_vec_info
vect_slp_analyze_bb (basic_block bb)
{
  bb_vec_info bb_vinfo;
  int insns = 0;
  gimple_stmt_iterator gsi;
  unsigned int vector_sizes;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "===vect_slp_analyze_bb===\n");

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      if (!is_gimple_debug (stmt)
          && !gimple_nop_p (stmt)
          && gimple_code (stmt) != GIMPLE_LABEL)
        insns++;
    }

  if (insns > PARAM_VALUE (PARAM_SLP_MAX_INSNS_IN_BB))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: too many instructions in "
			 "basic block.\n");

      return NULL;
    }

  /* Autodetect first vector size we try.  */
  current_vector_size = 0;
  vector_sizes = targetm.vectorize.autovectorize_vector_sizes ();

  while (1)
    {
      bb_vinfo = vect_slp_analyze_bb_1 (bb);
      if (bb_vinfo)
        return bb_vinfo;

      destroy_bb_vec_info (bb_vinfo);

      vector_sizes &= ~current_vector_size;
      if (vector_sizes == 0
          || current_vector_size == 0)
        return NULL;

      /* Try the next biggest vector size.  */
      current_vector_size = 1 << floor_log2 (vector_sizes);
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "***** Re-trying analysis with "
			 "vector size %d\n", current_vector_size);
    }
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
			   unsigned int op_num, unsigned int number_of_vectors,
                           int reduc_index)
{
  vec<gimple> stmts = SLP_TREE_SCALAR_STMTS (slp_node);
  gimple stmt = stmts[0];
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  unsigned nunits;
  tree vec_cst;
  tree *elts;
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
  gimple def_stmt;
  struct loop *loop;
  gimple_seq ctor_seq = NULL;

  vector_type = get_vectype_for_scalar_type (TREE_TYPE (op));
  nunits = TYPE_VECTOR_SUBPARTS (vector_type);

  if (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
      && reduc_index != -1)
    {
      op_num = reduc_index;
      op = gimple_op (stmt, op_num + 1);
      /* For additional copies (see the explanation of NUMBER_OF_COPIES below)
         we need either neutral operands or the original operands.  See
         get_initial_def_for_reduction() for details.  */
      switch (code)
        {
          case WIDEN_SUM_EXPR:
          case DOT_PROD_EXPR:
	  case SAD_EXPR:
          case PLUS_EXPR:
          case MINUS_EXPR:
          case BIT_IOR_EXPR:
          case BIT_XOR_EXPR:
             if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (op)))
               neutral_op = build_real (TREE_TYPE (op), dconst0);
             else
               neutral_op = build_int_cst (TREE_TYPE (op), 0);

             break;

          case MULT_EXPR:
             if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (op)))
               neutral_op = build_real (TREE_TYPE (op), dconst1);
             else
               neutral_op = build_int_cst (TREE_TYPE (op), 1);

             break;

          case BIT_AND_EXPR:
            neutral_op = build_int_cst (TREE_TYPE (op), -1);
            break;

	  /* For MIN/MAX we don't have an easy neutral operand but
	     the initial values can be used fine here.  Only for
	     a reduction chain we have to force a neutral element.  */
	  case MAX_EXPR:
	  case MIN_EXPR:
	    if (!GROUP_FIRST_ELEMENT (stmt_vinfo))
	      neutral_op = NULL;
	    else
	      {
		def_stmt = SSA_NAME_DEF_STMT (op);
		loop = (gimple_bb (stmt))->loop_father;
		neutral_op = PHI_ARG_DEF_FROM_EDGE (def_stmt,
						    loop_preheader_edge (loop));
	      }
	    break;

          default:
	    gcc_assert (!GROUP_FIRST_ELEMENT (stmt_vinfo));
            neutral_op = NULL;
        }
    }

  if (STMT_VINFO_DATA_REF (stmt_vinfo))
    {
      is_store = true;
      op = gimple_assign_rhs1 (stmt);
    }
  else
    is_store = false;

  gcc_assert (op);

  if (CONSTANT_CLASS_P (op))
    constant_p = true;
  else
    constant_p = false;

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

  number_of_copies = nunits * number_of_vectors / group_size;

  number_of_places_left_in_vector = nunits;
  elts = XALLOCAVEC (tree, nunits);
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
		    if (op_num == 0 || op_num == 1)
		      {
			tree cond = gimple_assign_rhs1 (stmt);
			op = TREE_OPERAND (cond, op_num);
		      }
		    else
		      {
			if (op_num == 2)
			  op = gimple_assign_rhs2 (stmt);
			else
			  op = gimple_assign_rhs3 (stmt);
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

          if (reduc_index != -1)
            {
              loop = (gimple_bb (stmt))->loop_father;
              def_stmt = SSA_NAME_DEF_STMT (op);

              gcc_assert (loop);

              /* Get the def before the loop.  In reduction chain we have only
                 one initial value.  */
              if ((j != (number_of_copies - 1)
                   || (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt))
                       && i != 0))
                  && neutral_op)
                op = neutral_op;
              else
                op = PHI_ARG_DEF_FROM_EDGE (def_stmt,
                                            loop_preheader_edge (loop));
            }

          /* Create 'vect_ = {op0,op1,...,opn}'.  */
          number_of_places_left_in_vector--;
	  tree orig_op = op;
	  if (!types_compatible_p (TREE_TYPE (vector_type), TREE_TYPE (op)))
	    {
	      if (CONSTANT_CLASS_P (op))
		{
		  op = fold_unary (VIEW_CONVERT_EXPR,
				   TREE_TYPE (vector_type), op);
		  gcc_assert (op && CONSTANT_CLASS_P (op));
		}
	      else
		{
		  tree new_temp = make_ssa_name (TREE_TYPE (vector_type));
		  gimple init_stmt;
		  op = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (vector_type), op);
		  init_stmt
		    = gimple_build_assign (new_temp, VIEW_CONVERT_EXPR, op);
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
              number_of_places_left_in_vector = nunits;

	      if (constant_p)
		vec_cst = build_vector (vector_type, elts);
	      else
		{
		  vec<constructor_elt, va_gc> *v;
		  unsigned k;
		  vec_alloc (v, nunits);
		  for (k = 0; k < nunits; ++k)
		    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, elts[k]);
		  vec_cst = build_constructor (vector_type, v);
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
		  gsi_insert_seq_before_without_update (&gsi, ctor_seq,
							GSI_SAME_STMT);
		  ctor_seq = NULL;
		}
	      voprnds.quick_push (init);
	      place_after_defs = false;
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
  gimple vec_def_stmt;
  unsigned int i;

  gcc_assert (SLP_TREE_VEC_STMTS (slp_node).exists ());

  FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (slp_node), i, vec_def_stmt)
    {
      gcc_assert (vec_def_stmt);
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
		   vec<vec<tree> > *vec_oprnds, int reduc_index)
{
  gimple first_stmt;
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
	  if (child)
	    {
	      gimple first_def = SLP_TREE_SCALAR_STMTS (child)[0];
	      gimple related
		= STMT_VINFO_RELATED_STMT (vinfo_for_stmt (first_def));

	      if (operand_equal_p (oprnd, gimple_get_lhs (first_def), 0)
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
      if (vectorized_defs && reduc_index == -1)
        /* The defs are already vectorized.  */
	vect_get_slp_vect_defs (child, &vec_defs);
      else
        /* Build vectors from scalar defs.  */
	vect_get_constant_vectors (oprnd, slp_node, &vec_defs, i,
                                   number_of_vects, reduc_index);

      vec_oprnds->quick_push (vec_defs);

      /* For reductions, we only need initial values.  */
      if (reduc_index != -1)
        return;
    }
}


/* Create NCOPIES permutation statements using the mask MASK_BYTES (by
   building a vector of type MASK_TYPE from it) and two input vectors placed in
   DR_CHAIN at FIRST_VEC_INDX and SECOND_VEC_INDX for the first copy and
   shifting by STRIDE elements of DR_CHAIN for every copy.
   (STRIDE is the number of vectorized stmts for NODE divided by the number of
   copies).
   VECT_STMTS_COUNTER specifies the index in the vectorized stmts of NODE, where
   the created stmts must be inserted.  */

static inline void
vect_create_mask_and_perm (gimple stmt,
                           tree mask, int first_vec_indx, int second_vec_indx,
                           gimple_stmt_iterator *gsi, slp_tree node,
                           tree vectype, vec<tree> dr_chain,
                           int ncopies, int vect_stmts_counter)
{
  tree perm_dest;
  gimple perm_stmt = NULL;
  int i, stride;
  tree first_vec, second_vec, data_ref;

  stride = SLP_TREE_NUMBER_OF_VEC_STMTS (node) / ncopies;

  /* Initialize the vect stmts of NODE to properly insert the generated
     stmts later.  */
  for (i = SLP_TREE_VEC_STMTS (node).length ();
       i < (int) SLP_TREE_NUMBER_OF_VEC_STMTS (node); i++)
    SLP_TREE_VEC_STMTS (node).quick_push (NULL);

  perm_dest = vect_create_destination_var (gimple_assign_lhs (stmt), vectype);
  for (i = 0; i < ncopies; i++)
    {
      first_vec = dr_chain[first_vec_indx];
      second_vec = dr_chain[second_vec_indx];

      /* Generate the permute statement.  */
      perm_stmt = gimple_build_assign (perm_dest, VEC_PERM_EXPR,
				       first_vec, second_vec, mask);
      data_ref = make_ssa_name (perm_dest, perm_stmt);
      gimple_set_lhs (perm_stmt, data_ref);
      vect_finish_stmt_generation (stmt, perm_stmt, gsi);

      /* Store the vector statement in NODE.  */
      SLP_TREE_VEC_STMTS (node)[stride * i + vect_stmts_counter] = perm_stmt;

      first_vec_indx += stride;
      second_vec_indx += stride;
    }
}


/* Given FIRST_MASK_ELEMENT - the mask element in element representation,
   return in CURRENT_MASK_ELEMENT its equivalent in target specific
   representation.  Check that the mask is valid and return FALSE if not.
   Return TRUE in NEED_NEXT_VECTOR if the permutation requires to move to
   the next vector, i.e., the current first vector is not needed.  */

static bool
vect_get_mask_element (gimple stmt, int first_mask_element, int m,
                       int mask_nunits, bool only_one_vec, int index,
		       unsigned char *mask, int *current_mask_element,
                       bool *need_next_vector, int *number_of_mask_fixes,
                       bool *mask_fixed, bool *needs_first_vector)
{
  int i;

  /* Convert to target specific representation.  */
  *current_mask_element = first_mask_element + m;
  /* Adjust the value in case it's a mask for second and third vectors.  */
  *current_mask_element -= mask_nunits * (*number_of_mask_fixes - 1);

  if (*current_mask_element < 0)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "permutation requires past vector ");
	  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}
      return false;
    }

  if (*current_mask_element < mask_nunits)
    *needs_first_vector = true;

  /* We have only one input vector to permute but the mask accesses values in
     the next vector as well.  */
  if (only_one_vec && *current_mask_element >= mask_nunits)
    {
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "permutation requires at least two vectors ");
          dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
          dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
        }

      return false;
    }

  /* The mask requires the next vector.  */
  while (*current_mask_element >= mask_nunits * 2)
    {
      if (*needs_first_vector || *mask_fixed)
        {
          /* We either need the first vector too or have already moved to the
             next vector. In both cases, this permutation needs three
             vectors.  */
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "permutation requires at "
			       "least three vectors ");
              dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
            }

          return false;
        }

      /* We move to the next vector, dropping the first one and working with
         the second and the third - we need to adjust the values of the mask
         accordingly.  */
      *current_mask_element -= mask_nunits * *number_of_mask_fixes;

      for (i = 0; i < index; i++)
        mask[i] -= mask_nunits * *number_of_mask_fixes;

      (*number_of_mask_fixes)++;
      *mask_fixed = true;
    }

  *need_next_vector = *mask_fixed;

  /* This was the last element of this mask. Start a new one.  */
  if (index == mask_nunits - 1)
    {
      *number_of_mask_fixes = 1;
      *mask_fixed = false;
      *needs_first_vector = false;
    }

  return true;
}


/* Generate vector permute statements from a list of loads in DR_CHAIN.
   If ANALYZE_ONLY is TRUE, only check that it is possible to create valid
   permute statements for the SLP node NODE of the SLP instance
   SLP_NODE_INSTANCE.  */

bool
vect_transform_slp_perm_load (slp_tree node, vec<tree> dr_chain,
                              gimple_stmt_iterator *gsi, int vf,
                              slp_instance slp_node_instance, bool analyze_only)
{
  gimple stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree mask_element_type = NULL_TREE, mask_type;
  int i, j, k, nunits, vec_index = 0;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  int group_size = SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
  int first_mask_element;
  int index, unroll_factor, current_mask_element, ncopies;
  unsigned char *mask;
  bool only_one_vec = false, need_next_vector = false;
  int first_vec_index, second_vec_index, orig_vec_stmts_num, vect_stmts_counter;
  int number_of_mask_fixes = 1;
  bool mask_fixed = false;
  bool needs_first_vector = false;
  machine_mode mode;

  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
    return false;

  stmt_info = vinfo_for_stmt (GROUP_FIRST_ELEMENT (stmt_info));

  mode = TYPE_MODE (vectype);

  if (!can_vec_perm_p (mode, false, NULL))
    {
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "no vect permute for ");
          dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
          dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
        }
      return false;
    }

  /* The generic VEC_PERM_EXPR code always uses an integral type of the
     same size as the vector element being permuted.  */
  mask_element_type = lang_hooks.types.type_for_mode
		(int_mode_for_mode (TYPE_MODE (TREE_TYPE (vectype))), 1);
  mask_type = get_vectype_for_scalar_type (mask_element_type);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  mask = XALLOCAVEC (unsigned char, nunits);
  unroll_factor = SLP_INSTANCE_UNROLLING_FACTOR (slp_node_instance);

  /* The number of vector stmts to generate based only on SLP_NODE_INSTANCE
     unrolling factor.  */
  orig_vec_stmts_num
    = (STMT_VINFO_GROUP_SIZE (stmt_info)
       * SLP_INSTANCE_UNROLLING_FACTOR (slp_node_instance)
       + nunits - 1) / nunits;
  if (orig_vec_stmts_num == 1)
    only_one_vec = true;

  /* Number of copies is determined by the final vectorization factor
     relatively to SLP_NODE_INSTANCE unrolling factor.  */
  ncopies = vf / SLP_INSTANCE_UNROLLING_FACTOR (slp_node_instance);

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

  {
      index = 0;
      vect_stmts_counter = 0;
      vec_index = 0;
      first_vec_index = vec_index++;
      if (only_one_vec)
        second_vec_index = first_vec_index;
      else
        second_vec_index =  vec_index++;

      for (j = 0; j < unroll_factor; j++)
        {
          for (k = 0; k < group_size; k++)
            {
	      i = SLP_TREE_LOAD_PERMUTATION (node)[k];
              first_mask_element = i + j * STMT_VINFO_GROUP_SIZE (stmt_info);
              if (!vect_get_mask_element (stmt, first_mask_element, 0,
					  nunits, only_one_vec, index,
					  mask, &current_mask_element,
					  &need_next_vector,
					  &number_of_mask_fixes, &mask_fixed,
					  &needs_first_vector))
		return false;
	      gcc_assert (current_mask_element >= 0
			  && current_mask_element < 2 * nunits);
	      mask[index++] = current_mask_element;

              if (index == nunits)
                {
		  index = 0;
		  if (!can_vec_perm_p (mode, false, mask))
		    {
		      if (dump_enabled_p ())
			{
			  dump_printf_loc (MSG_MISSED_OPTIMIZATION,
					   vect_location, 
					   "unsupported vect permute { ");
			  for (i = 0; i < nunits; ++i)
			    dump_printf (MSG_MISSED_OPTIMIZATION, "%d ",
					 mask[i]);
			  dump_printf (MSG_MISSED_OPTIMIZATION, "}\n");
			}
		      return false;
		    }

                  if (!analyze_only)
                    {
		      int l;
		      tree mask_vec, *mask_elts;
		      mask_elts = XALLOCAVEC (tree, nunits);
		      for (l = 0; l < nunits; ++l)
			mask_elts[l] = build_int_cst (mask_element_type,
						      mask[l]);
		      mask_vec = build_vector (mask_type, mask_elts);

		      if (need_next_vector)
                        {
                          first_vec_index = second_vec_index;
                          second_vec_index = vec_index;
                        }

                      vect_create_mask_and_perm (stmt,
                               mask_vec, first_vec_index, second_vec_index,
			       gsi, node, vectype, dr_chain,
			       ncopies, vect_stmts_counter++);
                    }
                }
            }
        }
    }

  return true;
}



/* Vectorize SLP instance tree in postorder.  */

static bool
vect_schedule_slp_instance (slp_tree node, slp_instance instance,
                            unsigned int vectorization_factor)
{
  gimple stmt;
  bool grouped_store, is_store;
  gimple_stmt_iterator si;
  stmt_vec_info stmt_info;
  unsigned int vec_stmts_size, nunits, group_size;
  tree vectype;
  int i;
  slp_tree child;

  if (!node)
    return false;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_schedule_slp_instance (child, instance, vectorization_factor);

  stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  stmt_info = vinfo_for_stmt (stmt);

  /* VECTYPE is the type of the destination.  */
  vectype = STMT_VINFO_VECTYPE (stmt_info);
  nunits = (unsigned int) TYPE_VECTOR_SUBPARTS (vectype);
  group_size = SLP_INSTANCE_GROUP_SIZE (instance);

  /* For each SLP instance calculate number of vector stmts to be created
     for the scalar stmts in each node of the SLP tree.  Number of vector
     elements in one vector iteration is the number of scalar elements in
     one scalar iteration (GROUP_SIZE) multiplied by VF divided by vector
     size.
     Unless this is a SLP reduction in which case the number of vector
     stmts is equal to the number of vector stmts of the children.  */
  if (GROUP_FIRST_ELEMENT (stmt_info)
      && !STMT_VINFO_GROUPED_ACCESS (stmt_info))
    vec_stmts_size = SLP_TREE_NUMBER_OF_VEC_STMTS (SLP_TREE_CHILDREN (node)[0]);
  else
    vec_stmts_size = (vectorization_factor * group_size) / nunits;

  if (!SLP_TREE_VEC_STMTS (node).exists ())
    {
      SLP_TREE_VEC_STMTS (node).create (vec_stmts_size);
      SLP_TREE_NUMBER_OF_VEC_STMTS (node) = vec_stmts_size;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE,vect_location,
		       "------>vectorizing SLP node starting from: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
      dump_printf (MSG_NOTE, "\n");
    }

  /* Vectorized stmts go before the last scalar stmt which is where
     all uses are ready.  */
  si = gsi_for_stmt (vect_find_last_scalar_stmt_in_slp (node));

  /* Mark the first element of the reduction chain as reduction to properly
     transform the node.  In the analysis phase only the last element of the
     chain is marked as reduction.  */
  if (GROUP_FIRST_ELEMENT (stmt_info) && !STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && GROUP_FIRST_ELEMENT (stmt_info) == stmt)
    {
      STMT_VINFO_DEF_TYPE (stmt_info) = vect_reduction_def;
      STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
    }

  /* Handle two-operation SLP nodes by vectorizing the group with
     both operations and then performing a merge.  */
  if (SLP_TREE_TWO_OPERATORS (node))
    {
      enum tree_code code0 = gimple_assign_rhs_code (stmt);
      enum tree_code ocode;
      gimple ostmt;
      unsigned char *mask = XALLOCAVEC (unsigned char, group_size);
      bool allsame = true;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, ostmt)
	if (gimple_assign_rhs_code (ostmt) != code0)
	  {
	    mask[i] = 1;
	    allsame = false;
	    ocode = gimple_assign_rhs_code (ostmt);
	  }
	else
	  mask[i] = 0;
      if (!allsame)
	{
	  vec<gimple> v0;
	  vec<gimple> v1;
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
	      (GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (vectype))), 1);
	  tree mvectype = get_same_sized_vectype (meltype, vectype);
	  unsigned k = 0, l;
	  for (j = 0; j < v0.length (); ++j)
	    {
	      tree *melts = XALLOCAVEC (tree, TYPE_VECTOR_SUBPARTS (vectype));
	      for (l = 0; l < TYPE_VECTOR_SUBPARTS (vectype); ++l)
		{
		  if (k >= group_size)
		    k = 0;
		  melts[l] = build_int_cst
		      (meltype, mask[k++] * TYPE_VECTOR_SUBPARTS (vectype) + l);
		}
	      tmask = build_vector (mvectype, melts);

	      /* ???  Not all targets support a VEC_PERM_EXPR with a
	         constant mask that would translate to a vec_merge RTX
		 (with their vec_perm_const_ok).  We can either not
		 vectorize in that case or let veclower do its job.
		 Unfortunately that isn't too great and at least for
		 plus/minus we'd eventually like to match targets
		 vector addsub instructions.  */
	      gimple vstmt;
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
  return is_store;
}

/* Replace scalar calls from SLP node NODE with setting of their lhs to zero.
   For loop vectorization this is done in vectorizable_call, but for SLP
   it needs to be deferred until end of vect_schedule_slp, because multiple
   SLP instances may refer to the same scalar stmt.  */

static void
vect_remove_slp_scalar_calls (slp_tree node)
{
  gimple stmt, new_stmt;
  gimple_stmt_iterator gsi;
  int i;
  slp_tree child;
  tree lhs;
  stmt_vec_info stmt_info;

  if (!node)
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
vect_schedule_slp (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo)
{
  vec<slp_instance> slp_instances;
  slp_instance instance;
  unsigned int i, vf;
  bool is_store = false;

  if (loop_vinfo)
    {
      slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
      vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
    }
  else
    {
      slp_instances = BB_VINFO_SLP_INSTANCES (bb_vinfo);
      vf = 1;
    }

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      /* Schedule the tree of INSTANCE.  */
      is_store = vect_schedule_slp_instance (SLP_INSTANCE_TREE (instance),
                                             instance, vf);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "vectorizing stmts using SLP.\n");
    }

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      slp_tree root = SLP_INSTANCE_TREE (instance);
      gimple store;
      unsigned int j;
      gimple_stmt_iterator gsi;

      /* Remove scalar call stmts.  Do not do this for basic-block
	 vectorization as not all uses may be vectorized.
	 ???  Why should this be necessary?  DCE should be able to
	 remove the stmts itself.
	 ???  For BB vectorization we can as well remove scalar
	 stmts starting from the SLP tree root if they have no
	 uses.  */
      if (loop_vinfo)
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


/* Vectorize the basic block.  */

void
vect_slp_transform_bb (basic_block bb)
{
  bb_vec_info bb_vinfo = vec_info_for_bb (bb);
  gimple_stmt_iterator si;

  gcc_assert (bb_vinfo);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "SLPing BB\n");

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple stmt = gsi_stmt (si);
      stmt_vec_info stmt_info;

      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
                           "------>SLPing statement: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
          dump_printf (MSG_NOTE, "\n");
        }

      stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (stmt_info);

      /* Schedule all the SLP instances when the first SLP stmt is reached.  */
      if (STMT_SLP_TYPE (stmt_info))
        {
          vect_schedule_slp (NULL, bb_vinfo);
          break;
        }
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "BASIC BLOCK VECTORIZED\n");

  destroy_bb_vec_info (bb_vinfo);
}
