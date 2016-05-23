/* SLP - Basic Block Vectorization
   Copyright (C) 2007-2016 Free Software Foundation, Inc.
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
  else
    return NULL;

  node = XNEW (struct _slp_tree);
  SLP_TREE_SCALAR_STMTS (node) = scalar_stmts;
  SLP_TREE_VEC_STMTS (node).create (0);
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
  enum vect_def_type first_dt;
  tree first_op_type;
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

static int
vect_get_place_in_interleaving_chain (gimple *stmt, gimple *first_stmt)
{
  gimple *next_stmt = first_stmt;
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
vect_get_and_check_slp_defs (vec_info *vinfo,
			     gimple *stmt, unsigned stmt_num,
                             vec<slp_oprnd_info> *oprnds_info)
{
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
      if (gimple_assign_rhs_code (stmt) == COND_EXPR
	  && COMPARISON_CLASS_P (gimple_assign_rhs1 (stmt)))
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

      if (!vect_is_simple_use (oprnd, vinfo, &def_stmt, &dt))
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
	  swap_ssa_operands (stmt, &TREE_OPERAND (cond, 0),
			     &TREE_OPERAND (cond, 1));
	  TREE_SET_CODE (cond, swap_tree_comparison (TREE_CODE (cond)));
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

  return 0;
}


/* Verify if the scalar stmts STMTS are isomorphic, require data
   permutation or are of unsupported types of operation.  Return
   true if they are, otherwise return false and indicate in *MATCHES
   which stmts are not isomorphic to the first one.  If MATCHES[0]
   is false then this indicates the comparison could not be
   carried out or the stmts will never be vectorized by SLP.  */

static bool
vect_build_slp_tree_1 (vec_info *vinfo,
		       vec<gimple *> stmts, unsigned int group_size,
		       unsigned nops, unsigned int *max_nunits,
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
  tree vectype = NULL_TREE, scalar_type, first_op1 = NULL_TREE;
  optab optab;
  int icode;
  machine_mode optab_op2_mode;
  machine_mode vec_mode;
  HOST_WIDE_INT dummy;
  gimple *first_load = NULL, *prev_first_load = NULL;

  /* For every stmt in NODE find its def stmt/s.  */
  FOR_EACH_VEC_ELT (stmts, i, stmt)
    {
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
      if (is_a <bb_vec_info> (vinfo)
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
	*max_nunits = TYPE_VECTOR_SUBPARTS (vectype);

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
	      && TREE_CODE_CLASS (rhs_code) != tcc_comparison
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

static slp_tree
vect_build_slp_tree (vec_info *vinfo,
                     vec<gimple *> stmts, unsigned int group_size,
                     unsigned int *max_nunits,
                     vec<slp_tree> *loads,
		     bool *matches, unsigned *npermutes, unsigned *tree_size,
		     unsigned max_tree_size)
{
  unsigned nops, i, this_tree_size = 0, this_max_nunits = *max_nunits;
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
  else
    return NULL;

  bool two_operators = false;
  if (!vect_build_slp_tree_1 (vinfo,
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
      switch (vect_get_and_check_slp_defs (vinfo, stmt, i, &oprnds_info))
	{
	case 0:
	  break;
	case -1:
	  matches[0] = false;
	  vect_free_oprnd_info (oprnds_info);
	  return NULL;
	case 1:
	  matches[i] = false;
	  break;
	}
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

  /* Create SLP_TREE nodes for the definition node/s.  */
  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      slp_tree child;
      unsigned old_nloads = this_loads.length ();
      unsigned old_tree_size = this_tree_size;
      unsigned int j;

      if (oprnd_info->first_dt != vect_internal_def)
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
	  && commutative_tree_code (gimple_assign_rhs_code (stmt))
	  && ! two_operators
	  /* Do so only if the number of not successful permutes was nor more
	     than a cut-ff as re-trying the recursive match on
	     possibly each level of the tree would expose exponential
	     behavior.  */
	  && *npermutes < 4)
	{
	  /* Verify if we can safely swap or if we committed to a specific
	     operand order already.  */
	  for (j = 0; j < group_size; ++j)
	    if (!matches[j]
		&& STMT_VINFO_NUM_SLP_USES (vinfo_for_stmt (stmts[j])) != 0)
	      {
		if (dump_enabled_p ())
		  {
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: cannot swap operands "
				     "of shared stmt ");
		    dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				      stmts[j], 0);
		  }
		goto fail;
	      }

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
		  if (!matches[j])
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
		    gcc_assert (gimple_plf (stmt, GF_PLF_1) == ! matches[j]);
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
vect_print_slp_tree (int dump_kind, location_t loc, slp_tree node)
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
      if (lidx >= group_size)
	{
	  sbitmap_free (load_index);
	  return false;
	}
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
  unsigned int unrolling_factor = SLP_INSTANCE_UNROLLING_FACTOR (slp_instn);
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (slp_instn), i, node)
    {
      gimple *first_stmt = SLP_TREE_SCALAR_STMTS (node)[0];
      first_stmt = GROUP_FIRST_ELEMENT (vinfo_for_stmt (first_stmt));
      /* But we have to keep those permutations that are required because
         of handling of gaps.  */
      if (unrolling_factor == 1
	  || (group_size == GROUP_SIZE (vinfo_for_stmt (first_stmt))
	      && GROUP_GAP (vinfo_for_stmt (first_stmt)) == 0))
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
      && !GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
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

/* Compute the cost for the SLP node NODE in the SLP instance INSTANCE.  */

static void
vect_analyze_slp_cost_1 (slp_instance instance, slp_tree node,
			 stmt_vector_for_cost *prologue_cost_vec,
			 stmt_vector_for_cost *body_cost_vec,
			 unsigned ncopies_for_cost)
{
  unsigned i, j;
  slp_tree child;
  gimple *stmt;
  stmt_vec_info stmt_info;
  tree lhs;

  /* Recurse down the SLP tree.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
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
	  gcc_checking_assert (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)));
	  if (SLP_TREE_LOAD_PERMUTATION (node).exists ())
	    {
	      /* If the load is permuted then the alignment is determined by
		 the first group element not by the first scalar stmt DR.  */
	      stmt = GROUP_FIRST_ELEMENT (stmt_info);
	      stmt_info = vinfo_for_stmt (stmt);
	      /* Record the cost for the permutation.  */
	      record_stmt_cost (body_cost_vec, ncopies_for_cost, vec_perm,
				stmt_info, 0, vect_body);
	      /* And adjust the number of loads performed.  */
	      unsigned nunits
		= TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
	      ncopies_for_cost
	        = (GROUP_SIZE (stmt_info) - GROUP_GAP (stmt_info)
		   + nunits - 1) / nunits;
	      ncopies_for_cost *= SLP_INSTANCE_UNROLLING_FACTOR (instance);
	    }
	  /* Record the cost for the vector loads.  */
	  vect_model_load_cost (stmt_info, ncopies_for_cost, false,
				node, prologue_cost_vec, body_cost_vec);
	  return;
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

  /* Push SLP node def-type to stmts.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (child), j, stmt)
	STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) = SLP_TREE_DEF_TYPE (child);

  /* Scan operands and account for prologue cost of constants/externals.
     ???  This over-estimates cost for multiple uses and should be
     re-engineered.  */
  stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  lhs = gimple_get_lhs (stmt);
  for (i = 0; i < gimple_num_ops (stmt); ++i)
    {
      tree op = gimple_op (stmt, i);
      gimple *def_stmt;
      enum vect_def_type dt;
      if (!op || op == lhs)
	continue;
      if (vect_is_simple_use (op, stmt_info->vinfo, &def_stmt, &dt))
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

  /* Restore stmt def-types.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (child), j, stmt)
	STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) = vect_internal_def;
}

/* Compute the cost for the SLP instance INSTANCE.  */

static void
vect_analyze_slp_cost (slp_instance instance, void *data)
{
  stmt_vector_for_cost body_cost_vec, prologue_cost_vec;
  unsigned ncopies_for_cost;
  stmt_info_for_cost *si;
  unsigned i;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_analyze_slp_cost ===\n");

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
      gimple *stmt = SLP_TREE_SCALAR_STMTS (node)[0];
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

/* Splits a group of stores, currently beginning at FIRST_STMT, into two groups:
   one (still beginning at FIRST_STMT) of size GROUP1_SIZE (also containing
   the first GROUP1_SIZE stmts, since stores are consecutive), the second
   containing the remainder.
   Return the first stmt in the second group.  */

static gimple *
vect_split_slp_store_group (gimple *first_stmt, unsigned group1_size)
{
  stmt_vec_info first_vinfo = vinfo_for_stmt (first_stmt);
  gcc_assert (GROUP_FIRST_ELEMENT (first_vinfo) == first_stmt);
  gcc_assert (group1_size > 0);
  int group2_size = GROUP_SIZE (first_vinfo) - group1_size;
  gcc_assert (group2_size > 0);
  GROUP_SIZE (first_vinfo) = group1_size;

  gimple *stmt = first_stmt;
  for (unsigned i = group1_size; i > 1; i--)
    {
      stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt));
      gcc_assert (GROUP_GAP (vinfo_for_stmt (stmt)) == 1);
    }
  /* STMT is now the last element of the first group.  */
  gimple *group2 = GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt));
  GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt)) = 0;

  GROUP_SIZE (vinfo_for_stmt (group2)) = group2_size;
  for (stmt = group2; stmt; stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt)))
    {
      GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) = group2;
      gcc_assert (GROUP_GAP (vinfo_for_stmt (stmt)) == 1);
    }

  /* For the second group, the GROUP_GAP is that before the original group,
     plus skipping over the first vector.  */
  GROUP_GAP (vinfo_for_stmt (group2)) =
    GROUP_GAP (first_vinfo) + group1_size;

  /* GROUP_GAP of the first group now has to skip over the second group too.  */
  GROUP_GAP (first_vinfo) += group2_size;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "Split group into %d and %d\n",
		     group1_size, group2_size);

  return group2;
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
  unsigned int group_size = GROUP_SIZE (vinfo_for_stmt (stmt));
  unsigned int unrolling_factor = 1, nunits;
  tree vectype, scalar_type = NULL_TREE;
  gimple *next;
  unsigned int i;
  unsigned int max_nunits = 0;
  vec<slp_tree> loads;
  struct data_reference *dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt));
  vec<gimple *> scalar_stmts;

  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      if (dr)
        {
          scalar_type = TREE_TYPE (DR_REF (dr));
          vectype = get_vectype_for_scalar_type (scalar_type);
        }
      else
        {
          gcc_assert (is_a <loop_vec_info> (vinfo));
          vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
        }

      group_size = GROUP_SIZE (vinfo_for_stmt (stmt));
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
  nunits = TYPE_VECTOR_SUBPARTS (vectype);

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
      vec<gimple *> reductions = as_a <loop_vec_info> (vinfo)->reductions;
      for (i = 0; reductions.iterate (i, &next); i++)
	scalar_stmts.safe_push (next);
    }

  loads.create (group_size);

  /* Build the tree for the SLP instance.  */
  bool *matches = XALLOCAVEC (bool, group_size);
  unsigned npermutes = 0;
  node = vect_build_slp_tree (vinfo, scalar_stmts, group_size,
				   &max_nunits, &loads, matches, &npermutes,
			      NULL, max_tree_size);
  if (node != NULL)
    {
      /* Calculate the unrolling factor based on the smallest type.  */
      unrolling_factor
	= least_common_multiple (max_nunits, group_size) / group_size;

      if (unrolling_factor != 1
	  && is_a <bb_vec_info> (vinfo))
	{

	  if (max_nunits > group_size)
        {
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Build SLP failed: store group "
			       "size not a multiple of the vector size "
			       "in basic block SLP\n");
	  vect_free_slp_tree (node);
	  loads.release ();
          return false;
        }
	  /* Fatal mismatch.  */
	  matches[group_size/max_nunits * max_nunits] = false;
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
	  first_stmt = GROUP_FIRST_ELEMENT
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
		      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION,
					TDF_SLIM, stmt, 0);
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
                }
              vect_free_slp_instance (new_instance);
              return false;
            }
        }

	  /* If the loads and stores can be handled with load/store-lan
	 instructions do not generate this SLP instance.  */
      if (is_a <loop_vec_info> (vinfo)
	  && loads_permuted
	  && dr && vect_store_lanes_supported (vectype, group_size))
	{
	  slp_tree load_node;
	  FOR_EACH_VEC_ELT (loads, i, load_node)
	    {
	      gimple *first_stmt = GROUP_FIRST_ELEMENT
		  (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (load_node)[0]));
	      stmt_vec_info stmt_vinfo = vinfo_for_stmt (first_stmt);
		  /* Use SLP for strided accesses (or if we
		     can't load-lanes).  */
	      if (STMT_VINFO_STRIDED_P (stmt_vinfo)
		  || ! vect_load_lanes_supported
			(STMT_VINFO_VECTYPE (stmt_vinfo),
			 GROUP_SIZE (stmt_vinfo)))
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
  if (is_a <bb_vec_info> (vinfo)
      && GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt))
      && STMT_VINFO_GROUPED_ACCESS (vinfo_for_stmt (stmt)))
    {
      /* We consider breaking the group only on VF boundaries from the existing
	 start.  */
      for (i = 0; i < group_size; i++)
	if (!matches[i]) break;

      if (i >= nunits && i < group_size)
	{
	  /* Split into two groups at the first vector boundary before i.  */
	  gcc_assert ((nunits & (nunits - 1)) == 0);
	  unsigned group1_size = i & ~(nunits - 1);

	  gimple *rest = vect_split_slp_store_group (stmt, group1_size);
	  bool res = vect_analyze_slp_instance (vinfo, stmt, max_tree_size);
	  /* If the first non-match was in the middle of a vector,
	     skip the rest of that vector.  */
	  if (group1_size < i)
	    {
	      i = group1_size + nunits;
	      if (i < group_size)
		rest = vect_split_slp_store_group (rest, nunits);
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
  bool ok = false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "=== vect_analyze_slp ===\n");

  /* Find SLP sequences starting from groups of grouped stores.  */
  FOR_EACH_VEC_ELT (vinfo->grouped_stores, i, first_element)
    if (vect_analyze_slp_instance (vinfo, first_element, max_tree_size))
      ok = true;

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      if (loop_vinfo->reduction_chains.length () > 0)
	{
	  /* Find SLP sequences starting from reduction chains.  */
	  FOR_EACH_VEC_ELT (loop_vinfo->reduction_chains, i, first_element)
	      if (vect_analyze_slp_instance (vinfo, first_element,
					     max_tree_size))
		ok = true;
	      else
		return false;

	  /* Don't try to vectorize SLP reductions if reduction chain was
	     detected.  */
	  return ok;
	}

      /* Find SLP sequences starting from groups of reductions.  */
      if (loop_vinfo->reductions.length () > 1
	  && vect_analyze_slp_instance (vinfo, loop_vinfo->reductions[0],
					max_tree_size))
	ok = true;
    }

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


/* Create and initialize a new bb_vec_info struct for BB, as well as
   stmt_vec_info structs for all the stmts in it.  */

static bb_vec_info
new_bb_vec_info (gimple_stmt_iterator region_begin,
		 gimple_stmt_iterator region_end)
{
  basic_block bb = gsi_bb (region_begin);
  bb_vec_info res = NULL;
  gimple_stmt_iterator gsi;

  res = (bb_vec_info) xcalloc (1, sizeof (struct _bb_vec_info));
  res->kind = vec_info::bb;
  BB_VINFO_BB (res) = bb;
  res->region_begin = region_begin;
  res->region_end = region_end;

  for (gsi = region_begin; gsi_stmt (gsi) != gsi_stmt (region_end);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      gimple_set_uid (stmt, 0);
      set_vinfo_for_stmt (stmt, new_stmt_vec_info (stmt, res));
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
  slp_instance instance;
  unsigned i;

  if (!bb_vinfo)
    return;

  vect_destroy_datarefs (bb_vinfo);
  free_dependence_relations (BB_VINFO_DDRS (bb_vinfo));
  BB_VINFO_GROUPED_STORES (bb_vinfo).release ();
  FOR_EACH_VEC_ELT (BB_VINFO_SLP_INSTANCES (bb_vinfo), i, instance)
    vect_free_slp_instance (instance);
  BB_VINFO_SLP_INSTANCES (bb_vinfo).release ();
  destroy_cost_data (BB_VINFO_TARGET_COST_DATA (bb_vinfo));

  for (gimple_stmt_iterator si = bb_vinfo->region_begin;
       gsi_stmt (si) != gsi_stmt (bb_vinfo->region_end); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

      if (stmt_info)
        /* Free stmt_vec_info.  */
        free_stmt_vec_info (stmt);

      /* Reset region marker.  */
      gimple_set_uid (stmt, -1);
    }

  BB_VINFO_BB (bb_vinfo)->aux = NULL;
  free (bb_vinfo);
}


/* Analyze statements contained in SLP tree node after recursively analyzing
   the subtree. Return TRUE if the operations are supported.  */

static bool
vect_slp_analyze_node_operations (slp_tree node)
{
  bool dummy;
  int i, j;
  gimple *stmt;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return true;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (!vect_slp_analyze_node_operations (child))
      return false;

  bool res = true;
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (stmt_info);
      gcc_assert (STMT_SLP_TYPE (stmt_info) != loop_vect);

      /* Push SLP node def-type to stmt operands.  */
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
	if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
	  STMT_VINFO_DEF_TYPE (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (child)[i]))
	    = SLP_TREE_DEF_TYPE (child);
      res = vect_analyze_stmt (stmt, &dummy, node);
      /* Restore def-types.  */
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
	if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
	  STMT_VINFO_DEF_TYPE (vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (child)[i]))
	    = vect_internal_def;
      if (! res)
	break;
    }

  return res;
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
  gimple *stmt;
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
    if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
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
		       bool &fatal)
{
  bb_vec_info bb_vinfo;
  slp_instance instance;
  int i;
  int min_vf = 2;

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

  bb_vinfo = new_bb_vec_info (region_begin, region_end);
  if (!bb_vinfo)
    return NULL;

  BB_VINFO_DATAREFS (bb_vinfo) = datarefs;

  /* Analyze the data references.  */

  if (!vect_analyze_data_refs (bb_vinfo, &min_vf))
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

  if (!vect_analyze_data_ref_accesses (bb_vinfo))
    {
     if (dump_enabled_p ())
       dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"not vectorized: unhandled data access in "
			"basic block.\n");

      destroy_bb_vec_info (bb_vinfo);
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

      destroy_bb_vec_info (bb_vinfo);
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

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

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


/* Main entry for the BB vectorizer.  Analyze and transform BB, returns
   true if anything in the basic-block was vectorized.  */

bool
vect_slp_bb (basic_block bb)
{
  bb_vec_info bb_vinfo;
  gimple_stmt_iterator gsi;
  unsigned int vector_sizes;
  bool any_vectorized = false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "===vect_slp_analyze_bb===\n");

  /* Autodetect first vector size we try.  */
  current_vector_size = 0;
  vector_sizes = targetm.vectorize.autovectorize_vector_sizes ();

  gsi = gsi_start_bb (bb);

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
	    vect_location = gimple_location (stmt);

	  if (!find_data_references_in_stmt (NULL, stmt, &datarefs))
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
      bb_vinfo = vect_slp_analyze_bb_1 (region_begin, region_end,
					datarefs, insns, fatal);
      if (bb_vinfo
	  && dbg_cnt (vect_slp))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "SLPing BB part\n");

	  vect_schedule_slp (bb_vinfo);

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "basic block part vectorized\n");

	  destroy_bb_vec_info (bb_vinfo);

	  vectorized = true;
	}
      else
	destroy_bb_vec_info (bb_vinfo);

      any_vectorized |= vectorized;

      vector_sizes &= ~current_vector_size;
      if (vectorized
	  || vector_sizes == 0
	  || current_vector_size == 0
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
	  vector_sizes = targetm.vectorize.autovectorize_vector_sizes ();
	}
      else
	{
	  /* Try the next biggest vector size.  */
	  current_vector_size = 1 << floor_log2 (vector_sizes);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "***** Re-trying analysis with "
			     "vector size %d\n", current_vector_size);

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
  gimple *def_stmt;
  enum vect_def_type dt;

  /* For comparison and COND_EXPR type is chosen depending
     on the other comparison operand.  */
  if (TREE_CODE_CLASS (code) == tcc_comparison)
    {
      if (opnum)
	op = gimple_assign_rhs1 (stmt);
      else
	op = gimple_assign_rhs2 (stmt);

      if (!vect_is_simple_use (op, stmt_vinfo->vinfo, &def_stmt,
			       &dt, &vectype))
	gcc_unreachable ();

      return !vectype || VECTOR_BOOLEAN_TYPE_P (vectype);
    }

  if (code == COND_EXPR)
    {
      tree cond = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (cond) == SSA_NAME)
	return false;

      if (opnum)
	op = TREE_OPERAND (cond, 1);
      else
	op = TREE_OPERAND (cond, 0);

      if (!vect_is_simple_use (op, stmt_vinfo->vinfo, &def_stmt,
			       &dt, &vectype))
	gcc_unreachable ();

      return !vectype || VECTOR_BOOLEAN_TYPE_P (vectype);
    }

  return VECTOR_BOOLEAN_TYPE_P (STMT_VINFO_VECTYPE (stmt_vinfo));
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
  vec<gimple *> stmts = SLP_TREE_SCALAR_STMTS (slp_node);
  gimple *stmt = stmts[0];
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
  gimple *def_stmt;
  struct loop *loop;
  gimple_seq ctor_seq = NULL;

  /* Check if vector type is a boolean vector.  */
  if (TREE_CODE (TREE_TYPE (op)) == BOOLEAN_TYPE
      && vect_mask_constant_operand_p (stmt, op_num))
    vector_type
      = build_same_sized_truth_vector_type (STMT_VINFO_VECTYPE (stmt_vinfo));
  else
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
		  if (VECTOR_BOOLEAN_TYPE_P (vector_type))
		    {
		      /* Can't use VIEW_CONVERT_EXPR for booleans because
			 of possibly different sizes of scalar value and
			 vector element.  */
		      if (integer_zerop (op))
			op = build_int_cst (TREE_TYPE (vector_type), 0);
		      else if (integer_onep (op))
			op = build_int_cst (TREE_TYPE (vector_type), 1);
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
		      gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (op)));
		      init_stmt = gimple_build_assign (new_temp, NOP_EXPR, op);
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
  gimple *vec_def_stmt;
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
vect_create_mask_and_perm (gimple *stmt,
                           tree mask, int first_vec_indx, int second_vec_indx,
                           gimple_stmt_iterator *gsi, slp_tree node,
                           tree vectype, vec<tree> dr_chain,
                           int ncopies, int vect_stmts_counter)
{
  tree perm_dest;
  gimple *perm_stmt = NULL;
  int i, stride_in, stride_out;
  tree first_vec, second_vec, data_ref;

  stride_out = SLP_TREE_NUMBER_OF_VEC_STMTS (node) / ncopies;
  stride_in = dr_chain.length () / ncopies;

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

      /* Generate the permute statement if necessary.  */
      if (mask)
	{
	  perm_stmt = gimple_build_assign (perm_dest, VEC_PERM_EXPR,
					   first_vec, second_vec, mask);
	  data_ref = make_ssa_name (perm_dest, perm_stmt);
	  gimple_set_lhs (perm_stmt, data_ref);
	  vect_finish_stmt_generation (stmt, perm_stmt, gsi);
	}
      else
	/* If mask was NULL_TREE generate the requested identity transform.  */
	perm_stmt = SSA_NAME_DEF_STMT (first_vec);

      /* Store the vector statement in NODE.  */
      SLP_TREE_VEC_STMTS (node)[stride_out * i + vect_stmts_counter]
	= perm_stmt;

      first_vec_indx += stride_in;
      second_vec_indx += stride_in;
    }
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
  gimple *stmt = SLP_TREE_SCALAR_STMTS (node)[0];
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree mask_element_type = NULL_TREE, mask_type;
  int nunits, vec_index = 0;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  int group_size = SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
  int unroll_factor, mask_element, ncopies;
  unsigned char *mask;
  machine_mode mode;

  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
    return false;

  stmt_info = vinfo_for_stmt (GROUP_FIRST_ELEMENT (stmt_info));

  mode = TYPE_MODE (vectype);

  /* The generic VEC_PERM_EXPR code always uses an integral type of the
     same size as the vector element being permuted.  */
  mask_element_type = lang_hooks.types.type_for_mode
		(int_mode_for_mode (TYPE_MODE (TREE_TYPE (vectype))), 1);
  mask_type = get_vectype_for_scalar_type (mask_element_type);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  mask = XALLOCAVEC (unsigned char, nunits);
  unroll_factor = SLP_INSTANCE_UNROLLING_FACTOR (slp_node_instance);

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

  int vect_stmts_counter = 0;
  int index = 0;
  int first_vec_index = -1;
  int second_vec_index = -1;
  bool noop_p = true;

  for (int j = 0; j < unroll_factor; j++)
    {
      for (int k = 0; k < group_size; k++)
	{
	  int i = (SLP_TREE_LOAD_PERMUTATION (node)[k]
		   + j * STMT_VINFO_GROUP_SIZE (stmt_info));
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
		  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}
	      return false;
	    }

	  gcc_assert (mask_element >= 0
		      && mask_element < 2 * nunits);
	  if (mask_element != index)
	    noop_p = false;
	  mask[index++] = mask_element;

	  if (index == nunits)
	    {
	      if (! noop_p
		  && ! can_vec_perm_p (mode, false, mask))
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				       vect_location, 
				       "unsupported vect permute { ");
		      for (i = 0; i < nunits; ++i)
			dump_printf (MSG_MISSED_OPTIMIZATION, "%d ", mask[i]);
		      dump_printf (MSG_MISSED_OPTIMIZATION, "}\n");
		    }
		  return false;
		}

	      if (!analyze_only)
		{
		  tree mask_vec = NULL_TREE;
		  
		  if (! noop_p)
		    {
		      tree *mask_elts = XALLOCAVEC (tree, nunits);
		      for (int l = 0; l < nunits; ++l)
			mask_elts[l] = build_int_cst (mask_element_type,
						      mask[l]);
		      mask_vec = build_vector (mask_type, mask_elts);
		    }

		  if (second_vec_index == -1)
		    second_vec_index = first_vec_index;
		  vect_create_mask_and_perm (stmt, mask_vec, first_vec_index,
					     second_vec_index,
					     gsi, node, vectype, dr_chain,
					     ncopies, vect_stmts_counter++);
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
                            unsigned int vectorization_factor)
{
  gimple *stmt;
  bool grouped_store, is_store;
  gimple_stmt_iterator si;
  stmt_vec_info stmt_info;
  unsigned int vec_stmts_size, nunits, group_size;
  tree vectype;
  int i, j;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return false;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_schedule_slp_instance (child, instance, vectorization_factor);

  /* Push SLP node def-type to stmts.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (SLP_TREE_DEF_TYPE (child) != vect_internal_def)
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (child), j, stmt)
	STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) = SLP_TREE_DEF_TYPE (child);

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
      enum tree_code ocode = ERROR_MARK;
      gimple *ostmt;
      unsigned char *mask = XALLOCAVEC (unsigned char, group_size);
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, ostmt)
	if (gimple_assign_rhs_code (ostmt) != code0)
	  {
	    mask[i] = 1;
	    ocode = gimple_assign_rhs_code (ostmt);
	  }
	else
	  mask[i] = 0;
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
  unsigned int i, vf;
  bool is_store = false;

  slp_instances = vinfo->slp_instances;
  if (is_a <loop_vec_info> (vinfo))
    vf = as_a <loop_vec_info> (vinfo)->vectorization_factor;
  else
    vf = 1;

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
