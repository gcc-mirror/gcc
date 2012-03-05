/* SLP - Basic Block Vectorization
   Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.
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
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "target.h"
#include "basic-block.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "expr.h"
#include "recog.h"
#include "optabs.h"
#include "tree-vectorizer.h"
#include "langhooks.h"

/* Extract the location of the basic block in the source code.
   Return the basic block location if succeed and NULL if not.  */

LOC
find_bb_location (basic_block bb)
{
  gimple stmt = NULL;
  gimple_stmt_iterator si;

  if (!bb)
    return UNKNOWN_LOC;

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      stmt = gsi_stmt (si);
      if (gimple_location (stmt) != UNKNOWN_LOC)
        return gimple_location (stmt);
    }

  return UNKNOWN_LOC;
}


/* Recursively free the memory allocated for the SLP tree rooted at NODE.  */

static void
vect_free_slp_tree (slp_tree node)
{
  int i;
  slp_void_p child;

  if (!node)
    return;

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_free_slp_tree ((slp_tree) child);

  VEC_free (slp_void_p, heap, SLP_TREE_CHILDREN (node));
  VEC_free (gimple, heap, SLP_TREE_SCALAR_STMTS (node));

  if (SLP_TREE_VEC_STMTS (node))
    VEC_free (gimple, heap, SLP_TREE_VEC_STMTS (node));

  free (node);
}


/* Free the memory allocated for the SLP instance.  */

void
vect_free_slp_instance (slp_instance instance)
{
  vect_free_slp_tree (SLP_INSTANCE_TREE (instance));
  VEC_free (int, heap, SLP_INSTANCE_LOAD_PERMUTATION (instance));
  VEC_free (slp_tree, heap, SLP_INSTANCE_LOADS (instance));
}


/* Create an SLP node for SCALAR_STMTS.  */

static slp_tree
vect_create_new_slp_node (VEC (gimple, heap) *scalar_stmts)
{
  slp_tree node;
  gimple stmt = VEC_index (gimple, scalar_stmts, 0);
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
  SLP_TREE_VEC_STMTS (node) = NULL;
  SLP_TREE_CHILDREN (node) = VEC_alloc (slp_void_p, heap, nops);
  SLP_TREE_OUTSIDE_OF_LOOP_COST (node) = 0;
  SLP_TREE_INSIDE_OF_LOOP_COST (node) = 0;

  return node;
}


/* Allocate operands info for NOPS operands, and GROUP_SIZE def-stmts for each
   operand.  */
static VEC (slp_oprnd_info, heap) *
vect_create_oprnd_info (int nops, int group_size)
{
  int i;
  slp_oprnd_info oprnd_info;
  VEC (slp_oprnd_info, heap) *oprnds_info;

  oprnds_info = VEC_alloc (slp_oprnd_info, heap, nops);
  for (i = 0; i < nops; i++)
    {
      oprnd_info = XNEW (struct _slp_oprnd_info);
      oprnd_info->def_stmts = VEC_alloc (gimple, heap, group_size);
      oprnd_info->first_dt = vect_uninitialized_def;
      oprnd_info->first_def_type = NULL_TREE;
      oprnd_info->first_const_oprnd = NULL_TREE;
      oprnd_info->first_pattern = false;
      VEC_quick_push (slp_oprnd_info, oprnds_info, oprnd_info);
    }

  return oprnds_info;
}


/* Free operands info.  */

static void
vect_free_oprnd_info (VEC (slp_oprnd_info, heap) **oprnds_info)
{
  int i;
  slp_oprnd_info oprnd_info;

  FOR_EACH_VEC_ELT (slp_oprnd_info, *oprnds_info, i, oprnd_info)
    {
      VEC_free (gimple, heap, oprnd_info->def_stmts);
      XDELETE (oprnd_info);
    }

  VEC_free (slp_oprnd_info, heap, *oprnds_info);
}


/* Get the defs for the rhs of STMT (collect them in OPRNDS_INFO), check that
   they are of a valid type and that they match the defs of the first stmt of
   the SLP group (stored in OPRNDS_INFO).  */

static bool
vect_get_and_check_slp_defs (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                             slp_tree slp_node, gimple stmt,
			     int ncopies_for_cost, bool first,
                             VEC (slp_oprnd_info, heap) **oprnds_info)
{
  tree oprnd;
  unsigned int i, number_of_oprnds;
  tree def, def_op0 = NULL_TREE;
  gimple def_stmt;
  enum vect_def_type dt = vect_uninitialized_def;
  enum vect_def_type dt_op0 = vect_uninitialized_def;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree lhs = gimple_get_lhs (stmt);
  struct loop *loop = NULL;
  enum tree_code rhs_code;
  bool different_types = false;
  bool pattern = false;
  slp_oprnd_info oprnd_info, oprnd0_info, oprnd1_info;
  int op_idx = 1;
  tree compare_rhs = NULL_TREE;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  if (is_gimple_call (stmt))
    {
      number_of_oprnds = gimple_call_num_args (stmt);
      op_idx = 3;
    }
  else if (is_gimple_assign (stmt))
    {
      number_of_oprnds = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
        number_of_oprnds++;
    }
  else
    return false;

  for (i = 0; i < number_of_oprnds; i++)
    {
      if (compare_rhs)
	{
	  oprnd = compare_rhs;
	  compare_rhs = NULL_TREE;
	}
      else
        oprnd = gimple_op (stmt, op_idx++);

      oprnd_info = VEC_index (slp_oprnd_info, *oprnds_info, i);

      if (COMPARISON_CLASS_P (oprnd))
        {
          compare_rhs = TREE_OPERAND (oprnd, 1);
          oprnd = TREE_OPERAND (oprnd, 0);
	}

      if (!vect_is_simple_use (oprnd, NULL, loop_vinfo, bb_vinfo, &def_stmt,
			       &def, &dt)
	  || (!def_stmt && dt != vect_constant_def))
	{
	  if (vect_print_dump_info (REPORT_SLP))
	    {
	      fprintf (vect_dump, "Build SLP failed: can't find def for ");
	      print_generic_expr (vect_dump, oprnd, TDF_SLIM);
	    }

	  return false;
	}

      /* Check if DEF_STMT is a part of a pattern in LOOP and get the def stmt
         from the pattern.  Check that all the stmts of the node are in the
         pattern.  */
      if (loop && def_stmt && gimple_bb (def_stmt)
          && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
          && vinfo_for_stmt (def_stmt)
          && STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (def_stmt))
          && !STMT_VINFO_RELEVANT (vinfo_for_stmt (def_stmt))
          && !STMT_VINFO_LIVE_P (vinfo_for_stmt (def_stmt)))
        {
          pattern = true;
          if (!first && !oprnd_info->first_pattern)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		{
		  fprintf (vect_dump, "Build SLP failed: some of the stmts"
				" are in a pattern, and others are not ");
		  print_generic_expr (vect_dump, oprnd, TDF_SLIM);
		}

	      return false;
            }

          def_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt));
          dt = STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt));

          if (dt == vect_unknown_def_type)
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "Unsupported pattern.");
              return false;
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
                if (vect_print_dump_info (REPORT_DETAILS))
                  fprintf (vect_dump, "unsupported defining stmt: ");
                return false;
            }
        }

      if (first)
	{
	  oprnd_info->first_dt = dt;
	  oprnd_info->first_pattern = pattern;
	  if (def)
	    {
	      oprnd_info->first_def_type = TREE_TYPE (def);
	      oprnd_info->first_const_oprnd = NULL_TREE;
	    }
	  else
            {
              oprnd_info->first_def_type = NULL_TREE;
              oprnd_info->first_const_oprnd = oprnd;
            }

	  if (i == 0)
	    {
	      def_op0 = def;
	      dt_op0 = dt;
	      /* Analyze costs (for the first stmt of the group only).  */
	      if (REFERENCE_CLASS_P (lhs))
		/* Store.  */
                vect_model_store_cost (stmt_info, ncopies_for_cost, false,
                                        dt, slp_node);
	      else
		{
		  enum vect_def_type dts[2];
		  dts[0] = dt;
		  dts[1] = vect_uninitialized_def;
		  /* Not memory operation (we don't call this function for
		     loads).  */
		  vect_model_simple_cost (stmt_info, ncopies_for_cost, dts,
					  slp_node);
		}
	    }
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
                     && dt == vect_internal_def))
               || (oprnd_info->first_def_type != NULL_TREE
		   && def
		   && !types_compatible_p (oprnd_info->first_def_type,
					   TREE_TYPE (def))))
	       || (!def
		   && !types_compatible_p (TREE_TYPE (oprnd_info->first_const_oprnd),
					   TREE_TYPE (oprnd)))
	       || different_types)
	    {
	      if (number_of_oprnds != 2)
		{
		  if (vect_print_dump_info (REPORT_SLP))
		    fprintf (vect_dump, "Build SLP failed: different types ");

		  return false;
                }

	      /* Try to swap operands in case of binary operation.  */
              if (i == 0)
                different_types = true;
              else
		{
		  oprnd0_info = VEC_index (slp_oprnd_info, *oprnds_info, 0);
		  if (is_gimple_assign (stmt)
		      && (rhs_code = gimple_assign_rhs_code (stmt))
 		      && TREE_CODE_CLASS (rhs_code) == tcc_binary
		      && commutative_tree_code (rhs_code)
		      && oprnd0_info->first_dt == dt
		      && oprnd_info->first_dt == dt_op0
		      && def_op0 && def
		      && !(oprnd0_info->first_def_type
			   && !types_compatible_p (oprnd0_info->first_def_type,
			                           TREE_TYPE (def)))
                      && !(oprnd_info->first_def_type
                           && !types_compatible_p (oprnd_info->first_def_type,
                                                   TREE_TYPE (def_op0))))
                    {
                      if (vect_print_dump_info (REPORT_SLP))
	                {
			  fprintf (vect_dump, "Swapping operands of ");
 		          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
			}

 		      swap_tree_operands (stmt, gimple_assign_rhs1_ptr (stmt),
 	                                  gimple_assign_rhs2_ptr (stmt));
		    }
                  else
                    {
         	      if (vect_print_dump_info (REPORT_SLP))
			fprintf (vect_dump, "Build SLP failed: different types ");

		      return false;
		    }
		}
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
          if (different_types)
            {
	      oprnd0_info = VEC_index (slp_oprnd_info, *oprnds_info, 0);
	      oprnd1_info = VEC_index (slp_oprnd_info, *oprnds_info, 0);
              if (i == 0)
                VEC_quick_push (gimple, oprnd1_info->def_stmts, def_stmt);
              else
                VEC_quick_push (gimple, oprnd0_info->def_stmts, def_stmt);
            }
	  else
 	    VEC_quick_push (gimple, oprnd_info->def_stmts, def_stmt);

	  break;

	default:
	  /* FORNOW: Not supported.  */
	  if (vect_print_dump_info (REPORT_SLP))
	    {
	      fprintf (vect_dump, "Build SLP failed: illegal type of def ");
	      print_generic_expr (vect_dump, def, TDF_SLIM);
	    }

	  return false;
	}
    }

  return true;
}


/* Recursively build an SLP tree starting from NODE.
   Fail (and return FALSE) if def-stmts are not isomorphic, require data
   permutation or are of unsupported types of operation.  Otherwise, return
   TRUE.  */

static bool
vect_build_slp_tree (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                     slp_tree *node, unsigned int group_size,
                     int *inside_cost, int *outside_cost,
                     int ncopies_for_cost, unsigned int *max_nunits,
                     VEC (int, heap) **load_permutation,
                     VEC (slp_tree, heap) **loads,
                     unsigned int vectorization_factor, bool *loads_permuted)
{
  unsigned int i;
  VEC (gimple, heap) *stmts = SLP_TREE_SCALAR_STMTS (*node);
  gimple stmt = VEC_index (gimple, stmts, 0);
  enum tree_code first_stmt_code = ERROR_MARK, rhs_code = ERROR_MARK;
  enum tree_code first_cond_code = ERROR_MARK;
  tree lhs;
  bool stop_recursion = false, need_same_oprnds = false;
  tree vectype, scalar_type, first_op1 = NULL_TREE;
  unsigned int ncopies;
  optab optab;
  int icode;
  enum machine_mode optab_op2_mode;
  enum machine_mode vec_mode;
  struct data_reference *first_dr;
  HOST_WIDE_INT dummy;
  bool permutation = false;
  unsigned int load_place;
  gimple first_load, prev_first_load = NULL;
  VEC (slp_oprnd_info, heap) *oprnds_info;
  unsigned int nops;
  slp_oprnd_info oprnd_info;
  tree cond;

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

  oprnds_info = vect_create_oprnd_info (nops, group_size);

  /* For every stmt in NODE find its def stmt/s.  */
  FOR_EACH_VEC_ELT (gimple, stmts, i, stmt)
    {
      if (vect_print_dump_info (REPORT_SLP))
	{
	  fprintf (vect_dump, "Build SLP for ");
	  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	}

      /* Fail to vectorize statements marked as unvectorizable.  */
      if (!STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (stmt)))
        {
          if (vect_print_dump_info (REPORT_SLP))
            {
              fprintf (vect_dump,
                       "Build SLP failed: unvectorizable statement ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
            }

	  vect_free_oprnd_info (&oprnds_info);
          return false;
        }

      lhs = gimple_get_lhs (stmt);
      if (lhs == NULL_TREE)
	{
	  if (vect_print_dump_info (REPORT_SLP))
	    {
	      fprintf (vect_dump,
		       "Build SLP failed: not GIMPLE_ASSIGN nor GIMPLE_CALL ");
	      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	    }

	  vect_free_oprnd_info (&oprnds_info);
	  return false;
	}

       if (is_gimple_assign (stmt)
	   && gimple_assign_rhs_code (stmt) == COND_EXPR
           && (cond = gimple_assign_rhs1 (stmt))
           && !COMPARISON_CLASS_P (cond))
        {
          if (vect_print_dump_info (REPORT_SLP))
            {
              fprintf (vect_dump,
                       "Build SLP failed: condition is not comparison ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
            }

	  vect_free_oprnd_info (&oprnds_info);
          return false;
        }

      scalar_type = vect_get_smallest_scalar_type (stmt, &dummy, &dummy);
      vectype = get_vectype_for_scalar_type (scalar_type);
      if (!vectype)
        {
          if (vect_print_dump_info (REPORT_SLP))
            {
              fprintf (vect_dump, "Build SLP failed: unsupported data-type ");
              print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
            }

	  vect_free_oprnd_info (&oprnds_info);
          return false;
        }

      /* In case of multiple types we need to detect the smallest type.  */
      if (*max_nunits < TYPE_VECTOR_SUBPARTS (vectype))
        {
          *max_nunits = TYPE_VECTOR_SUBPARTS (vectype);
          if (bb_vinfo)
            vectorization_factor = *max_nunits;
        }

      ncopies = vectorization_factor / TYPE_VECTOR_SUBPARTS (vectype);

      if (is_gimple_call (stmt))
	{
	  rhs_code = CALL_EXPR;
	  if (gimple_call_internal_p (stmt)
	      || gimple_call_tail_p (stmt)
	      || gimple_call_noreturn_p (stmt)
	      || !gimple_call_nothrow_p (stmt)
	      || gimple_call_chain (stmt))
	    {
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump,
			   "Build SLP failed: unsupported call type ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      vect_free_oprnd_info (&oprnds_info);
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
		      if (vect_print_dump_info (REPORT_SLP))
			fprintf (vect_dump, "Build SLP failed: no optab.");
	  	      vect_free_oprnd_info (&oprnds_info);
		      return false;
		    }
		  icode = (int) optab_handler (optab, vec_mode);
		  if (icode == CODE_FOR_nothing)
		    {
		      if (vect_print_dump_info (REPORT_SLP))
			fprintf (vect_dump, "Build SLP failed: "
				            "op not supported by target.");
	  	      vect_free_oprnd_info (&oprnds_info);
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
	      && (first_stmt_code != IMAGPART_EXPR
		  || rhs_code != REALPART_EXPR)
	      && (first_stmt_code != REALPART_EXPR
		  || rhs_code != IMAGPART_EXPR)
              && !(STMT_VINFO_STRIDED_ACCESS (vinfo_for_stmt (stmt))
                   && (first_stmt_code == ARRAY_REF
                       || first_stmt_code == INDIRECT_REF
                       || first_stmt_code == COMPONENT_REF
                       || first_stmt_code == MEM_REF)))
	    {
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump,
			   "Build SLP failed: different operation in stmt ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      vect_free_oprnd_info (&oprnds_info);
	      return false;
	    }

	  if (need_same_oprnds
	      && !operand_equal_p (first_op1, gimple_assign_rhs2 (stmt), 0))
	    {
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump,
			   "Build SLP failed: different shift arguments in ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      vect_free_oprnd_info (&oprnds_info);
	      return false;
	    }

	  if (rhs_code == CALL_EXPR)
	    {
	      gimple first_stmt = VEC_index (gimple, stmts, 0);
	      if (gimple_call_num_args (stmt) != nops
		  || !operand_equal_p (gimple_call_fn (first_stmt),
				       gimple_call_fn (stmt), 0)
		  || gimple_call_fntype (first_stmt)
		     != gimple_call_fntype (stmt))
		{
		  if (vect_print_dump_info (REPORT_SLP))
		    {
		      fprintf (vect_dump,
			       "Build SLP failed: different calls in ");
		      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		    }

		  vect_free_oprnd_info (&oprnds_info);
		  return false;
		}
	    }
	}

      /* Strided store or load.  */
      if (STMT_VINFO_STRIDED_ACCESS (vinfo_for_stmt (stmt)))
	{
	  if (REFERENCE_CLASS_P (lhs))
	    {
	      /* Store.  */
	      if (!vect_get_and_check_slp_defs (loop_vinfo, bb_vinfo, *node,
						stmt, ncopies_for_cost,
						(i == 0), &oprnds_info))
		{
	  	  vect_free_oprnd_info (&oprnds_info);
 		  return false;
		}
	    }
	  else
	    {
	      /* Load.  */
              /* FORNOW: Check that there is no gap between the loads.  */
              if ((GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) == stmt
                   && GROUP_GAP (vinfo_for_stmt (stmt)) != 0)
                  || (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) != stmt
                      && GROUP_GAP (vinfo_for_stmt (stmt)) != 1))
                {
                  if (vect_print_dump_info (REPORT_SLP))
                    {
                      fprintf (vect_dump, "Build SLP failed: strided "
                                          "loads have gaps ");
                      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                    }

	  	  vect_free_oprnd_info (&oprnds_info);
                  return false;
                }

              /* Check that the size of interleaved loads group is not
                 greater than the SLP group size.  */
              if (loop_vinfo
                  && GROUP_SIZE (vinfo_for_stmt (stmt)) > ncopies * group_size)
                {
                  if (vect_print_dump_info (REPORT_SLP))
                    {
                      fprintf (vect_dump, "Build SLP failed: the number of "
                                          "interleaved loads is greater than"
                                          " the SLP group size ");
                      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                    }

	  	  vect_free_oprnd_info (&oprnds_info);
                  return false;
                }

              first_load = GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt));
              if (prev_first_load)
                {
                  /* Check that there are no loads from different interleaving
                     chains in the same node.  The only exception is complex
                     numbers.  */
                  if (prev_first_load != first_load
                      && rhs_code != REALPART_EXPR 
                      && rhs_code != IMAGPART_EXPR)
                    {    
                      if (vect_print_dump_info (REPORT_SLP))
                        {
                          fprintf (vect_dump, "Build SLP failed: different "
                                           "interleaving chains in one node ");
                          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                        }
 
	  	      vect_free_oprnd_info (&oprnds_info);
                      return false;
                    }
                }
              else
                prev_first_load = first_load;

              if (first_load == stmt)
                {
                  first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt));
                  if (vect_supportable_dr_alignment (first_dr, false)
                      == dr_unaligned_unsupported)
                    {
                      if (vect_print_dump_info (REPORT_SLP))
                        {
                          fprintf (vect_dump, "Build SLP failed: unsupported "
                                              "unaligned load ");
                          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                        }

	  	      vect_free_oprnd_info (&oprnds_info);
                      return false;
                    }

                  /* Analyze costs (for the first stmt in the group).  */
                  vect_model_load_cost (vinfo_for_stmt (stmt),
                                        ncopies_for_cost, false, *node);
                }

              /* Store the place of this load in the interleaving chain.  In
                 case that permutation is needed we later decide if a specific
                 permutation is supported.  */
              load_place = vect_get_place_in_interleaving_chain (stmt,
                                                                 first_load);
              if (load_place != i)
                permutation = true;

              VEC_safe_push (int, heap, *load_permutation, load_place);

              /* We stop the tree when we reach a group of loads.  */
              stop_recursion = true;
             continue;
           }
        } /* Strided access.  */
      else
	{
	  if (TREE_CODE_CLASS (rhs_code) == tcc_reference)
	    {
	      /* Not strided load.  */
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump, "Build SLP failed: not strided load ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      /* FORNOW: Not strided loads are not supported.  */
	      vect_free_oprnd_info (&oprnds_info);
	      return false;
	    }

	  /* Not memory operation.  */
	  if (TREE_CODE_CLASS (rhs_code) != tcc_binary
	      && TREE_CODE_CLASS (rhs_code) != tcc_unary
	      && rhs_code != COND_EXPR
	      && rhs_code != CALL_EXPR)
	    {
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump, "Build SLP failed: operation");
		  fprintf (vect_dump, " unsupported ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      vect_free_oprnd_info (&oprnds_info);
	      return false;
	    }

          if (rhs_code == COND_EXPR)
            {
              tree cond_expr = gimple_assign_rhs1 (stmt);

	      if (i == 0)
		first_cond_code = TREE_CODE (cond_expr);
              else if (first_cond_code != TREE_CODE (cond_expr))
                {
                  if (vect_print_dump_info (REPORT_SLP))
                    {
                      fprintf (vect_dump, "Build SLP failed: different"
					  " operation");
                      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                    }

		  vect_free_oprnd_info (&oprnds_info);
                  return false;
		}
            }

	  /* Find the def-stmts.  */
	  if (!vect_get_and_check_slp_defs (loop_vinfo, bb_vinfo, *node, stmt,
					    ncopies_for_cost, (i == 0),
					    &oprnds_info))
	    {
	      vect_free_oprnd_info (&oprnds_info);
	      return false;
	    }
	}
    }

  /* Add the costs of the node to the overall instance costs.  */
  *inside_cost += SLP_TREE_INSIDE_OF_LOOP_COST (*node);
  *outside_cost += SLP_TREE_OUTSIDE_OF_LOOP_COST (*node);

  /* Strided loads were reached - stop the recursion.  */
  if (stop_recursion)
    {
      VEC_safe_push (slp_tree, heap, *loads, *node);
      if (permutation)
        {

          *loads_permuted = true;
          *inside_cost 
            += targetm.vectorize.builtin_vectorization_cost (vec_perm, NULL, 0) 
               * group_size;
        }
      else
        {
          /* We don't check here complex numbers chains, so we set
             LOADS_PERMUTED for further check in
             vect_supported_load_permutation_p.  */
          if (rhs_code == REALPART_EXPR || rhs_code == IMAGPART_EXPR)
            *loads_permuted = true;
        }

      vect_free_oprnd_info (&oprnds_info);
      return true;
    }

  /* Create SLP_TREE nodes for the definition node/s.  */
  FOR_EACH_VEC_ELT (slp_oprnd_info, oprnds_info, i, oprnd_info)
    {
      slp_tree child;

      if (oprnd_info->first_dt != vect_internal_def)
        continue;

      child = vect_create_new_slp_node (oprnd_info->def_stmts);
      if (!child
          || !vect_build_slp_tree (loop_vinfo, bb_vinfo, &child, group_size,
				inside_cost, outside_cost, ncopies_for_cost,
				max_nunits, load_permutation, loads,
				vectorization_factor, loads_permuted))
        {
	  if (child)
	    oprnd_info->def_stmts = NULL;
	  vect_free_slp_tree (child);
	  vect_free_oprnd_info (&oprnds_info);
   	  return false;
	}

      oprnd_info->def_stmts = NULL;
      VEC_quick_push (slp_void_p, SLP_TREE_CHILDREN (*node), child);
    }

  vect_free_oprnd_info (&oprnds_info);
  return true;
}


static void
vect_print_slp_tree (slp_tree node)
{
  int i;
  gimple stmt;
  slp_void_p child;

  if (!node)
    return;

  fprintf (vect_dump, "node ");
  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      fprintf (vect_dump, "\n\tstmt %d ", i);
      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
    }
  fprintf (vect_dump, "\n");

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_print_slp_tree ((slp_tree) child);
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
  slp_void_p child;

  if (!node)
    return;

  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt)
    if (j < 0 || i == j)
      STMT_SLP_TYPE (vinfo_for_stmt (stmt)) = mark;

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_mark_slp_stmts ((slp_tree) child, mark, j);
}


/* Mark the statements of the tree rooted at NODE as relevant (vect_used).  */

static void
vect_mark_slp_stmts_relevant (slp_tree node)
{
  int i;
  gimple stmt;
  stmt_vec_info stmt_info;
  slp_void_p child;

  if (!node)
    return;

  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (!STMT_VINFO_RELEVANT (stmt_info)
                  || STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope);
      STMT_VINFO_RELEVANT (stmt_info) = vect_used_in_scope;
    }

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_mark_slp_stmts_relevant ((slp_tree) child);
}


/* Check if the permutation required by the SLP INSTANCE is supported.
   Reorganize the SLP nodes stored in SLP_INSTANCE_LOADS if needed.  */

static bool
vect_supported_slp_permutation_p (slp_instance instance)
{
  slp_tree node = VEC_index (slp_tree, SLP_INSTANCE_LOADS (instance), 0);
  gimple stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (node), 0);
  gimple first_load = GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt));
  VEC (slp_tree, heap) *sorted_loads = NULL;
  int index;
  slp_tree *tmp_loads = NULL;
  int group_size = SLP_INSTANCE_GROUP_SIZE (instance), i, j;
  slp_tree load;

  /* FORNOW: The only supported loads permutation is loads from the same
     location in all the loads in the node, when the data-refs in
     nodes of LOADS constitute an interleaving chain.
     Sort the nodes according to the order of accesses in the chain.  */
  tmp_loads = (slp_tree *) xmalloc (sizeof (slp_tree) * group_size);
  for (i = 0, j = 0;
       VEC_iterate (int, SLP_INSTANCE_LOAD_PERMUTATION (instance), i, index)
       && VEC_iterate (slp_tree, SLP_INSTANCE_LOADS (instance), j, load);
       i += group_size, j++)
    {
      gimple scalar_stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (load), 0);
      /* Check that the loads are all in the same interleaving chain.  */
      if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (scalar_stmt)) != first_load)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            {
              fprintf (vect_dump, "Build SLP failed: unsupported data "
                                   "permutation ");
              print_gimple_stmt (vect_dump, scalar_stmt, 0, TDF_SLIM);
            }

          free (tmp_loads);
          return false;
        }

      tmp_loads[index] = load;
    }

  sorted_loads = VEC_alloc (slp_tree, heap, group_size);
  for (i = 0; i < group_size; i++)
     VEC_safe_push (slp_tree, heap, sorted_loads, tmp_loads[i]);

  VEC_free (slp_tree, heap, SLP_INSTANCE_LOADS (instance));
  SLP_INSTANCE_LOADS (instance) = sorted_loads;
  free (tmp_loads);

  if (!vect_transform_slp_perm_load (stmt, NULL, NULL,
                                     SLP_INSTANCE_UNROLLING_FACTOR (instance),
                                     instance, true))
    return false;

  return true;
}


/* Rearrange the statements of NODE according to PERMUTATION.  */

static void
vect_slp_rearrange_stmts (slp_tree node, unsigned int group_size,
                          VEC (int, heap) *permutation)
{
  gimple stmt;
  VEC (gimple, heap) *tmp_stmts;
  unsigned int index, i;
  slp_void_p child;

  if (!node)
    return;

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_slp_rearrange_stmts ((slp_tree) child, group_size, permutation);

  gcc_assert (group_size == VEC_length (gimple, SLP_TREE_SCALAR_STMTS (node)));
  tmp_stmts = VEC_alloc (gimple, heap, group_size);

  for (i = 0; i < group_size; i++)
    VEC_safe_push (gimple, heap, tmp_stmts, NULL);

  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      index = VEC_index (int, permutation, i);
      VEC_replace (gimple, tmp_stmts, index, stmt);
    }

  VEC_free (gimple, heap, SLP_TREE_SCALAR_STMTS (node));
  SLP_TREE_SCALAR_STMTS (node) = tmp_stmts;
}


/* Check if the required load permutation is supported.
   LOAD_PERMUTATION contains a list of indices of the loads.
   In SLP this permutation is relative to the order of strided stores that are
   the base of the SLP instance.  */

static bool
vect_supported_load_permutation_p (slp_instance slp_instn, int group_size,
                                   VEC (int, heap) *load_permutation)
{
  int i = 0, j, prev = -1, next, k, number_of_groups;
  bool supported, bad_permutation = false;
  sbitmap load_index;
  slp_tree node, other_complex_node;
  gimple stmt, first = NULL, other_node_first, load, next_load, first_load;
  unsigned complex_numbers = 0;
  struct data_reference *dr;
  bb_vec_info bb_vinfo;

  /* FORNOW: permutations are only supported in SLP.  */
  if (!slp_instn)
    return false;

  if (vect_print_dump_info (REPORT_SLP))
    {
      fprintf (vect_dump, "Load permutation ");
      FOR_EACH_VEC_ELT (int, load_permutation, i, next)
        fprintf (vect_dump, "%d ", next);
    }

  /* In case of reduction every load permutation is allowed, since the order
     of the reduction statements is not important (as opposed to the case of
     strided stores).  The only condition we need to check is that all the
     load nodes are of the same size and have the same permutation (and then
     rearrange all the nodes of the SLP instance according to this 
     permutation).  */

  /* Check that all the load nodes are of the same size.  */
  FOR_EACH_VEC_ELT (slp_tree, SLP_INSTANCE_LOADS (slp_instn), i, node)
    {
      if (VEC_length (gimple, SLP_TREE_SCALAR_STMTS (node))
          != (unsigned) group_size)
        return false;

      stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (node), 0);
      if (is_gimple_assign (stmt) 
          && (gimple_assign_rhs_code (stmt) == REALPART_EXPR
              || gimple_assign_rhs_code (stmt) == IMAGPART_EXPR))
        complex_numbers++;
    }

  /* Complex operands can be swapped as following:
      real_c = real_b + real_a;
      imag_c = imag_a + imag_b;
     i.e., we have {real_b, imag_a} and {real_a, imag_b} instead of 
     {real_a, imag_a} and {real_b, imag_b}.  We check here that if interleaving
     chains are mixed, they match the above pattern.  */
  if (complex_numbers)
    {
      FOR_EACH_VEC_ELT (slp_tree, SLP_INSTANCE_LOADS (slp_instn), i, node)
        {
	  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), j, stmt)
            {
              if (j == 0)
                first = stmt;
              else
                {
                  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) != first)
                    {
                      if (complex_numbers != 2)
                        return false;

                      if (i == 0)
                        k = 1;
                      else
                        k = 0;
 
                      other_complex_node = VEC_index (slp_tree, 
                                            SLP_INSTANCE_LOADS (slp_instn), k);
                      other_node_first = VEC_index (gimple, 
                                SLP_TREE_SCALAR_STMTS (other_complex_node), 0);

                      if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt))
                          != other_node_first)
                       return false;
                    }
                }
            }
        }
    }

  /* We checked that this case ok, so there is no need to proceed with 
     permutation tests.  */
  if (complex_numbers == 2)
    {
      VEC_free (slp_tree, heap, SLP_INSTANCE_LOADS (slp_instn));
      VEC_free (int, heap, SLP_INSTANCE_LOAD_PERMUTATION (slp_instn));
      return true;
    }
                   
  node = SLP_INSTANCE_TREE (slp_instn);
  stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (node), 0);
  /* LOAD_PERMUTATION is a list of indices of all the loads of the SLP
     instance, not all the loads belong to the same node or interleaving
     group.  Hence, we need to divide them into groups according to
     GROUP_SIZE.  */
  number_of_groups = VEC_length (int, load_permutation) / group_size;

  /* Reduction (there are no data-refs in the root).
     In reduction chain the order of the loads is important.  */
  if (!STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt))
      && !GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      int first_group_load_index;

      /* Compare all the permutation sequences to the first one.  */
      for (i = 1; i < number_of_groups; i++)
        {
          k = 0;
          for (j = i * group_size; j < i * group_size + group_size; j++)
            {
              next = VEC_index (int, load_permutation, j);
              first_group_load_index = VEC_index (int, load_permutation, k);

              if (next != first_group_load_index)
                {
                  bad_permutation = true;
                  break;
                }

              k++;
            }

          if (bad_permutation)
            break;
        }

      if (!bad_permutation)
        {
          /* Check that the loads in the first sequence are different and there
             are no gaps between them.  */
          load_index = sbitmap_alloc (group_size);
          sbitmap_zero (load_index);
          for (k = 0; k < group_size; k++)
            {
              first_group_load_index = VEC_index (int, load_permutation, k);
              if (TEST_BIT (load_index, first_group_load_index))
                {
                  bad_permutation = true;
                  break;
                }

              SET_BIT (load_index, first_group_load_index);
            }

          if (!bad_permutation)
            for (k = 0; k < group_size; k++)
              if (!TEST_BIT (load_index, k))
                {
                  bad_permutation = true;
                  break;
                }

          sbitmap_free (load_index);
        }

      if (!bad_permutation)
        {
          /* This permutation is valid for reduction.  Since the order of the
             statements in the nodes is not important unless they are memory
             accesses, we can rearrange the statements in all the nodes 
             according to the order of the loads.  */
          vect_slp_rearrange_stmts (SLP_INSTANCE_TREE (slp_instn), group_size,
                                    load_permutation);
          VEC_free (int, heap, SLP_INSTANCE_LOAD_PERMUTATION (slp_instn));
          return true;
        }
    }

  /* In basic block vectorization we allow any subchain of an interleaving
     chain.
     FORNOW: not supported in loop SLP because of realignment compications.  */
  bb_vinfo = STMT_VINFO_BB_VINFO (vinfo_for_stmt (stmt));
  bad_permutation = false;
  /* Check that for every node in the instance teh loads form a subchain.  */
  if (bb_vinfo)
    {
      FOR_EACH_VEC_ELT (slp_tree, SLP_INSTANCE_LOADS (slp_instn), i, node)
        {
          next_load = NULL;
          first_load = NULL;
          FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), j, load)
            {
              if (!first_load)
                first_load = GROUP_FIRST_ELEMENT (vinfo_for_stmt (load));
              else if (first_load
                         != GROUP_FIRST_ELEMENT (vinfo_for_stmt (load)))
                {
                  bad_permutation = true;
	          break;
	        }

              if (j != 0 && next_load != load)
                {
                  bad_permutation = true;
                  break;
                }

              next_load = GROUP_NEXT_ELEMENT (vinfo_for_stmt (load));
            }

          if (bad_permutation)
            break;
        }

      /* Check that the alignment of the first load in every subchain, i.e.,
         the first statement in every load node, is supported.  */
      if (!bad_permutation)
        {
          FOR_EACH_VEC_ELT (slp_tree, SLP_INSTANCE_LOADS (slp_instn), i, node)
            {
              first_load = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (node), 0);
              if (first_load
                    != GROUP_FIRST_ELEMENT (vinfo_for_stmt (first_load)))
                {
                  dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_load));
                  if (vect_supportable_dr_alignment (dr, false)
 	               == dr_unaligned_unsupported)
                    {
   		      if (vect_print_dump_info (REPORT_SLP))
		        {
  	                  fprintf (vect_dump, "unsupported unaligned load ");
                          print_gimple_stmt (vect_dump, first_load, 0,
					     TDF_SLIM);
                        }
  		      bad_permutation = true;
                      break;
                    }
	        }
            }

          if (!bad_permutation)
            {
              VEC_free (int, heap, SLP_INSTANCE_LOAD_PERMUTATION (slp_instn));
              return true;
    	    }
        }
    }

  /* FORNOW: the only supported permutation is 0..01..1.. of length equal to
     GROUP_SIZE and where each sequence of same drs is of GROUP_SIZE length as
     well (unless it's reduction).  */
  if (VEC_length (int, load_permutation)
      != (unsigned int) (group_size * group_size))
    return false;

  supported = true;
  load_index = sbitmap_alloc (group_size);
  sbitmap_zero (load_index);
  for (j = 0; j < group_size; j++)
    {
      for (i = j * group_size, k = 0;
           VEC_iterate (int, load_permutation, i, next) && k < group_size;
           i++, k++)
       {
         if (i != j * group_size && next != prev)
          {
            supported = false;
            break;
          }

         prev = next;
       }

      if (TEST_BIT (load_index, prev))
        {
          supported = false;
          break;
        }

      SET_BIT (load_index, prev);
    }
 
  for (j = 0; j < group_size; j++)
    if (!TEST_BIT (load_index, j))
      return false;

  sbitmap_free (load_index);

  if (supported && i == group_size * group_size
      && vect_supported_slp_permutation_p (slp_instn))
    return true;

  return false;
}


/* Find the first load in the loop that belongs to INSTANCE.
   When loads are in several SLP nodes, there can be a case in which the first
   load does not appear in the first SLP node to be transformed, causing
   incorrect order of statements.  Since we generate all the loads together,
   they must be inserted before the first load of the SLP instance and not
   before the first load of the first node of the instance.  */

static gimple
vect_find_first_load_in_slp_instance (slp_instance instance)
{
  int i, j;
  slp_tree load_node;
  gimple first_load = NULL, load;

  FOR_EACH_VEC_ELT (slp_tree, SLP_INSTANCE_LOADS (instance), i, load_node)
    FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (load_node), j, load)
      first_load = get_earlier_stmt (load, first_load);

  return first_load;
}


/* Find the last store in SLP INSTANCE.  */

static gimple
vect_find_last_store_in_slp_instance (slp_instance instance)
{
  int i;
  slp_tree node;
  gimple last_store = NULL, store;

  node = SLP_INSTANCE_TREE (instance);
  for (i = 0;
       VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (node), i, store);
       i++)
    last_store = get_later_stmt (store, last_store);

  return last_store;
}


/* Analyze an SLP instance starting from a group of strided stores.  Call
   vect_build_slp_tree to build a tree of packed stmts if possible.
   Return FALSE if it's impossible to SLP any stmt in the loop.  */

static bool
vect_analyze_slp_instance (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                           gimple stmt)
{
  slp_instance new_instance;
  slp_tree node;
  unsigned int group_size = GROUP_SIZE (vinfo_for_stmt (stmt));
  unsigned int unrolling_factor = 1, nunits;
  tree vectype, scalar_type = NULL_TREE;
  gimple next;
  unsigned int vectorization_factor = 0;
  int inside_cost = 0, outside_cost = 0, ncopies_for_cost, i;
  unsigned int max_nunits = 0;
  VEC (int, heap) *load_permutation;
  VEC (slp_tree, heap) *loads;
  struct data_reference *dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt));
  bool loads_permuted = false;
  VEC (gimple, heap) *scalar_stmts;

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
      group_size = VEC_length (gimple, LOOP_VINFO_REDUCTIONS (loop_vinfo));
    }

  if (!vectype)
    {
      if (vect_print_dump_info (REPORT_SLP))
        {
          fprintf (vect_dump, "Build SLP failed: unsupported data-type ");
          print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
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
      if (vect_print_dump_info (REPORT_SLP))
        fprintf (vect_dump, "Build SLP failed: unrolling required in basic"
                            " block SLP");

      return false;
    }

  /* Create a node (a root of the SLP tree) for the packed strided stores.  */
  scalar_stmts = VEC_alloc (gimple, heap, group_size);
  next = stmt;
  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      /* Collect the stores and store them in SLP_TREE_SCALAR_STMTS.  */
      while (next)
        {
	  if (STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (next))
	      && STMT_VINFO_RELATED_STMT (vinfo_for_stmt (next)))
	    VEC_safe_push (gimple, heap, scalar_stmts,
			STMT_VINFO_RELATED_STMT (vinfo_for_stmt (next)));
	  else
            VEC_safe_push (gimple, heap, scalar_stmts, next);
          next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next));
        }
    }
  else
    {
      /* Collect reduction statements.  */
      VEC (gimple, heap) *reductions = LOOP_VINFO_REDUCTIONS (loop_vinfo);
      for (i = 0; VEC_iterate (gimple, reductions, i, next); i++)
	VEC_safe_push (gimple, heap, scalar_stmts, next);
    }

  node = vect_create_new_slp_node (scalar_stmts);

  /* Calculate the number of vector stmts to create based on the unrolling
     factor (number of vectors is 1 if NUNITS >= GROUP_SIZE, and is
     GROUP_SIZE / NUNITS otherwise.  */
  ncopies_for_cost = unrolling_factor * group_size / nunits;

  load_permutation = VEC_alloc (int, heap, group_size * group_size);
  loads = VEC_alloc (slp_tree, heap, group_size);

  /* Build the tree for the SLP instance.  */
  if (vect_build_slp_tree (loop_vinfo, bb_vinfo, &node, group_size,
                           &inside_cost, &outside_cost, ncopies_for_cost,
			   &max_nunits, &load_permutation, &loads,
			   vectorization_factor, &loads_permuted))
    {
      /* Calculate the unrolling factor based on the smallest type.  */
      if (max_nunits > nunits)
        unrolling_factor = least_common_multiple (max_nunits, group_size)
                           / group_size;

      if (unrolling_factor != 1 && !loop_vinfo)
        {
          if (vect_print_dump_info (REPORT_SLP))
            fprintf (vect_dump, "Build SLP failed: unrolling required in basic"
                               " block SLP");
          return false;
        }

      /* Create a new SLP instance.  */
      new_instance = XNEW (struct _slp_instance);
      SLP_INSTANCE_TREE (new_instance) = node;
      SLP_INSTANCE_GROUP_SIZE (new_instance) = group_size;
      SLP_INSTANCE_UNROLLING_FACTOR (new_instance) = unrolling_factor;
      SLP_INSTANCE_OUTSIDE_OF_LOOP_COST (new_instance) = outside_cost;
      SLP_INSTANCE_INSIDE_OF_LOOP_COST (new_instance) = inside_cost;
      SLP_INSTANCE_LOADS (new_instance) = loads;
      SLP_INSTANCE_FIRST_LOAD_STMT (new_instance) = NULL;
      SLP_INSTANCE_LOAD_PERMUTATION (new_instance) = load_permutation;

      if (loads_permuted)
        {
          if (!vect_supported_load_permutation_p (new_instance, group_size,
                                                  load_permutation))
            {
              if (vect_print_dump_info (REPORT_SLP))
                {
                  fprintf (vect_dump, "Build SLP failed: unsupported load "
                                      "permutation ");
                  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                }

              vect_free_slp_instance (new_instance);
              return false;
            }

          SLP_INSTANCE_FIRST_LOAD_STMT (new_instance)
             = vect_find_first_load_in_slp_instance (new_instance);
        }
      else
        VEC_free (int, heap, SLP_INSTANCE_LOAD_PERMUTATION (new_instance));

      if (loop_vinfo)
        VEC_safe_push (slp_instance, heap,
                       LOOP_VINFO_SLP_INSTANCES (loop_vinfo),
  		       new_instance);
      else
        VEC_safe_push (slp_instance, heap, BB_VINFO_SLP_INSTANCES (bb_vinfo),
                       new_instance);

      if (vect_print_dump_info (REPORT_SLP))
	vect_print_slp_tree (node);

      return true;
    }

  /* Failed to SLP.  */
  /* Free the allocated memory.  */
  vect_free_slp_tree (node);
  VEC_free (int, heap, load_permutation);
  VEC_free (slp_tree, heap, loads);

  return false;
}


/* Check if there are stmts in the loop can be vectorized using SLP.  Build SLP
   trees of packed scalar stmts if SLP is possible.  */

bool
vect_analyze_slp (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo)
{
  unsigned int i;
  VEC (gimple, heap) *strided_stores, *reductions = NULL, *reduc_chains = NULL;
  gimple first_element;
  bool ok = false;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_analyze_slp ===");

  if (loop_vinfo)
    {
      strided_stores = LOOP_VINFO_STRIDED_STORES (loop_vinfo);
      reduc_chains = LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo);
      reductions = LOOP_VINFO_REDUCTIONS (loop_vinfo);
    }
  else
    strided_stores = BB_VINFO_STRIDED_STORES (bb_vinfo);

  /* Find SLP sequences starting from groups of strided stores.  */
  FOR_EACH_VEC_ELT (gimple, strided_stores, i, first_element)
    if (vect_analyze_slp_instance (loop_vinfo, bb_vinfo, first_element))
      ok = true;

  if (bb_vinfo && !ok)
    {
      if (vect_print_dump_info (REPORT_SLP))
        fprintf (vect_dump, "Failed to SLP the basic block.");

      return false;
    }

  if (loop_vinfo
      && VEC_length (gimple, LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo)) > 0)
    {
      /* Find SLP sequences starting from reduction chains.  */
      FOR_EACH_VEC_ELT (gimple, reduc_chains, i, first_element)
        if (vect_analyze_slp_instance (loop_vinfo, bb_vinfo, first_element))
          ok = true;
        else
          return false;

      /* Don't try to vectorize SLP reductions if reduction chain was
         detected.  */
      return ok;
    }

  /* Find SLP sequences starting from groups of reductions.  */
  if (loop_vinfo && VEC_length (gimple, LOOP_VINFO_REDUCTIONS (loop_vinfo)) > 1
      && vect_analyze_slp_instance (loop_vinfo, bb_vinfo, 
                                    VEC_index (gimple, reductions, 0)))
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
  VEC (slp_instance, heap) *slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;
  int decided_to_slp = 0;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_make_slp_decision ===");

  FOR_EACH_VEC_ELT (slp_instance, slp_instances, i, instance)
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

  if (decided_to_slp && vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "Decided to SLP %d instances. Unrolling factor %d",
	     decided_to_slp, unrolling_factor);

  return (decided_to_slp > 0);
}


/* Find stmts that must be both vectorized and SLPed (since they feed stmts that
   can't be SLPed) in the tree rooted at NODE.  Mark such stmts as HYBRID.  */

static void
vect_detect_hybrid_slp_stmts (slp_tree node)
{
  int i;
  VEC (gimple, heap) *stmts = SLP_TREE_SCALAR_STMTS (node);
  gimple stmt = VEC_index (gimple, stmts, 0);
  imm_use_iterator imm_iter;
  gimple use_stmt;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  slp_void_p child;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = NULL;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_vinfo);
  basic_block bb = NULL;

  if (!node)
    return;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);
  else
    bb = BB_VINFO_BB (bb_vinfo);

  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt)
    if (PURE_SLP_STMT (vinfo_for_stmt (stmt))
	&& TREE_CODE (gimple_op (stmt, 0)) == SSA_NAME)
      FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, gimple_op (stmt, 0))
	if (gimple_bb (use_stmt)
            && ((loop && flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
		 || bb == gimple_bb (use_stmt))
	    && (stmt_vinfo = vinfo_for_stmt (use_stmt))
	    && !STMT_SLP_TYPE (stmt_vinfo)
            && (STMT_VINFO_RELEVANT (stmt_vinfo)
                || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (stmt_vinfo)))
	    && !(gimple_code (use_stmt) == GIMPLE_PHI
                 && STMT_VINFO_DEF_TYPE (stmt_vinfo)
                  == vect_reduction_def))
	  vect_mark_slp_stmts (node, hybrid, i);

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_detect_hybrid_slp_stmts ((slp_tree) child);
}


/* Find stmts that must be both vectorized and SLPed.  */

void
vect_detect_hybrid_slp (loop_vec_info loop_vinfo)
{
  unsigned int i;
  VEC (slp_instance, heap) *slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_detect_hybrid_slp ===");

  FOR_EACH_VEC_ELT (slp_instance, slp_instances, i, instance)
    vect_detect_hybrid_slp_stmts (SLP_INSTANCE_TREE (instance));
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

  BB_VINFO_STRIDED_STORES (res) = VEC_alloc (gimple, heap, 10);
  BB_VINFO_SLP_INSTANCES (res) = VEC_alloc (slp_instance, heap, 2);

  bb->aux = res;
  return res;
}


/* Free BB_VINFO struct, as well as all the stmt_vec_info structs of all the
   stmts in the basic block.  */

static void
destroy_bb_vec_info (bb_vec_info bb_vinfo)
{
  basic_block bb;
  gimple_stmt_iterator si;

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

  free_data_refs (BB_VINFO_DATAREFS (bb_vinfo));
  free_dependence_relations (BB_VINFO_DDRS (bb_vinfo));
  VEC_free (gimple, heap, BB_VINFO_STRIDED_STORES (bb_vinfo));
  VEC_free (slp_instance, heap, BB_VINFO_SLP_INSTANCES (bb_vinfo));
  free (bb_vinfo);
  bb->aux = NULL;
}


/* Analyze statements contained in SLP tree node after recursively analyzing
   the subtree. Return TRUE if the operations are supported.  */

static bool
vect_slp_analyze_node_operations (bb_vec_info bb_vinfo, slp_tree node)
{
  bool dummy;
  int i;
  gimple stmt;
  slp_void_p child;

  if (!node)
    return true;

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    if (!vect_slp_analyze_node_operations (bb_vinfo, (slp_tree) child))
      return false;

  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt)
    {
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (stmt_info);
      gcc_assert (PURE_SLP_STMT (stmt_info));

      if (!vect_analyze_stmt (stmt, &dummy, node))
        return false;
    }

  return true;
}


/* Analyze statements in SLP instances of the basic block.  Return TRUE if the
   operations are supported. */

static bool
vect_slp_analyze_operations (bb_vec_info bb_vinfo)
{
  VEC (slp_instance, heap) *slp_instances = BB_VINFO_SLP_INSTANCES (bb_vinfo);
  slp_instance instance;
  int i;

  for (i = 0; VEC_iterate (slp_instance, slp_instances, i, instance); )
    {
      if (!vect_slp_analyze_node_operations (bb_vinfo,
                                             SLP_INSTANCE_TREE (instance)))
        {
 	  vect_free_slp_instance (instance);
          VEC_ordered_remove (slp_instance, slp_instances, i);
	}
      else
        i++;
    }

  if (!VEC_length (slp_instance, slp_instances))
    return false;

  return true;
}

/* Check if vectorization of the basic block is profitable.  */

static bool
vect_bb_vectorization_profitable_p (bb_vec_info bb_vinfo)
{
  VEC (slp_instance, heap) *slp_instances = BB_VINFO_SLP_INSTANCES (bb_vinfo);
  slp_instance instance;
  int i;
  unsigned int vec_outside_cost = 0, vec_inside_cost = 0, scalar_cost = 0;
  unsigned int stmt_cost;
  gimple stmt;
  gimple_stmt_iterator si;
  basic_block bb = BB_VINFO_BB (bb_vinfo);
  stmt_vec_info stmt_info = NULL;
  tree dummy_type = NULL;
  int dummy = 0;

  /* Calculate vector costs.  */
  FOR_EACH_VEC_ELT (slp_instance, slp_instances, i, instance)
    {
      vec_outside_cost += SLP_INSTANCE_OUTSIDE_OF_LOOP_COST (instance);
      vec_inside_cost += SLP_INSTANCE_INSIDE_OF_LOOP_COST (instance);
    }

  /* Calculate scalar cost.  */
  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      stmt = gsi_stmt (si);
      stmt_info = vinfo_for_stmt (stmt);

      if (!stmt_info || !STMT_VINFO_VECTORIZABLE (stmt_info)
          || !PURE_SLP_STMT (stmt_info))
        continue;

      if (STMT_VINFO_DATA_REF (stmt_info))
        {
          if (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
            stmt_cost = targetm.vectorize.builtin_vectorization_cost 
                          (scalar_load, dummy_type, dummy);
          else
            stmt_cost = targetm.vectorize.builtin_vectorization_cost
                          (scalar_store, dummy_type, dummy);
        }
      else
        stmt_cost = targetm.vectorize.builtin_vectorization_cost
                      (scalar_stmt, dummy_type, dummy);

      scalar_cost += stmt_cost;
    }

  if (vect_print_dump_info (REPORT_COST))
    {
      fprintf (vect_dump, "Cost model analysis: \n");
      fprintf (vect_dump, "  Vector inside of basic block cost: %d\n",
               vec_inside_cost);
      fprintf (vect_dump, "  Vector outside of basic block cost: %d\n",
               vec_outside_cost);
      fprintf (vect_dump, "  Scalar cost of basic block: %d", scalar_cost);
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
  VEC (ddr_p, heap) *ddrs;
  VEC (slp_instance, heap) *slp_instances;
  slp_instance instance;
  int i;
  int min_vf = 2;
  int max_vf = MAX_VECTORIZATION_FACTOR;

  bb_vinfo = new_bb_vec_info (bb);
  if (!bb_vinfo)
    return NULL;

  if (!vect_analyze_data_refs (NULL, bb_vinfo, &min_vf))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: unhandled data-ref in basic "
                            "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  ddrs = BB_VINFO_DDRS (bb_vinfo);
  if (!VEC_length (ddr_p, ddrs))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: not enough data-refs in basic "
                            "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

   if (!vect_analyze_data_ref_dependences (NULL, bb_vinfo, &max_vf)
       || min_vf > max_vf)
     {
       if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	 fprintf (vect_dump, "not vectorized: unhandled data dependence "
		  "in basic block.\n");

       destroy_bb_vec_info (bb_vinfo);
       return NULL;
     }

  if (!vect_analyze_data_refs_alignment (NULL, bb_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: bad data alignment in basic "
                            "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  if (!vect_analyze_data_ref_accesses (NULL, bb_vinfo))
    {
     if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
       fprintf (vect_dump, "not vectorized: unhandled data access in basic "
                           "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

   if (!vect_verify_datarefs_alignment (NULL, bb_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: unsupported alignment in basic "
                            "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  /* Check the SLP opportunities in the basic block, analyze and build SLP
     trees.  */
  if (!vect_analyze_slp (NULL, bb_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: failed to find SLP opportunities "
                            "in basic block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  slp_instances = BB_VINFO_SLP_INSTANCES (bb_vinfo);

  /* Mark all the statements that we want to vectorize as pure SLP and
     relevant.  */
  FOR_EACH_VEC_ELT (slp_instance, slp_instances, i, instance)
    {
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance), pure_slp, -1);
      vect_mark_slp_stmts_relevant (SLP_INSTANCE_TREE (instance));
    }

  if (!vect_slp_analyze_operations (bb_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: bad operation in basic block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  /* Cost model: check if the vectorization is worthwhile.  */
  if (flag_vect_cost_model
      && !vect_bb_vectorization_profitable_p (bb_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: vectorization is not "
                            "profitable.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "Basic block will be vectorized using SLP\n");

  return bb_vinfo;
}


bb_vec_info
vect_slp_analyze_bb (basic_block bb)
{
  bb_vec_info bb_vinfo;
  int insns = 0;
  gimple_stmt_iterator gsi;
  unsigned int vector_sizes;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "===vect_slp_analyze_bb===\n");

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
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: too many instructions in basic "
                            "block.\n");

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
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "***** Re-trying analysis with "
                 "vector size %d\n", current_vector_size);
    }
}


/* SLP costs are calculated according to SLP instance unrolling factor (i.e.,
   the number of created vector stmts depends on the unrolling factor).
   However, the actual number of vector stmts for every SLP node depends on
   VF which is set later in vect_analyze_operations ().  Hence, SLP costs
   should be updated.  In this function we assume that the inside costs
   calculated in vect_model_xxx_cost are linear in ncopies.  */

void
vect_update_slp_costs_according_to_vf (loop_vec_info loop_vinfo)
{
  unsigned int i, vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  VEC (slp_instance, heap) *slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_update_slp_costs_according_to_vf ===");

  FOR_EACH_VEC_ELT (slp_instance, slp_instances, i, instance)
    /* We assume that costs are linear in ncopies.  */
    SLP_INSTANCE_INSIDE_OF_LOOP_COST (instance) *= vf
      / SLP_INSTANCE_UNROLLING_FACTOR (instance);
}


/* For constant and loop invariant defs of SLP_NODE this function returns
   (vector) defs (VEC_OPRNDS) that will be used in the vectorized stmts.
   OP_NUM determines if we gather defs for operand 0 or operand 1 of the RHS of
   scalar stmts.  NUMBER_OF_VECTORS is the number of vector defs to create.
   REDUC_INDEX is the index of the reduction operand in the statements, unless
   it is -1.  */

static void
vect_get_constant_vectors (tree op, slp_tree slp_node,
                           VEC (tree, heap) **vec_oprnds,
			   unsigned int op_num, unsigned int number_of_vectors,
                           int reduc_index)
{
  VEC (gimple, heap) *stmts = SLP_TREE_SCALAR_STMTS (slp_node);
  gimple stmt = VEC_index (gimple, stmts, 0);
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  int nunits;
  tree vec_cst;
  tree t = NULL_TREE;
  int j, number_of_places_left_in_vector;
  tree vector_type;
  tree vop;
  int group_size = VEC_length (gimple, stmts);
  unsigned int vec_num, i;
  int number_of_copies = 1;
  VEC (tree, heap) *voprnds = VEC_alloc (tree, heap, number_of_vectors);
  bool constant_p, is_store;
  tree neutral_op = NULL;
  enum tree_code code = gimple_expr_code (stmt);
  gimple def_stmt;
  struct loop *loop;

  if (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
      && reduc_index != -1)
    {
      op_num = reduc_index - 1;
      op = gimple_op (stmt, reduc_index);
      /* For additional copies (see the explanation of NUMBER_OF_COPIES below)
         we need either neutral operands or the original operands.  See
         get_initial_def_for_reduction() for details.  */
      switch (code)
        {
          case WIDEN_SUM_EXPR:
          case DOT_PROD_EXPR:
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

          case MAX_EXPR:
          case MIN_EXPR:
            def_stmt = SSA_NAME_DEF_STMT (op);
            loop = (gimple_bb (stmt))->loop_father;
            neutral_op = PHI_ARG_DEF_FROM_EDGE (def_stmt,
                                                loop_preheader_edge (loop));
            break;

          default:
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

  vector_type = get_vectype_for_scalar_type (TREE_TYPE (op));
  gcc_assert (vector_type);
  nunits = TYPE_VECTOR_SUBPARTS (vector_type);

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

  number_of_copies = least_common_multiple (nunits, group_size) / group_size;

  number_of_places_left_in_vector = nunits;
  for (j = 0; j < number_of_copies; j++)
    {
      for (i = group_size - 1; VEC_iterate (gimple, stmts, i, stmt); i--)
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

		  default:
		    op = gimple_op (stmt, op_num + 1);
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
          t = tree_cons (NULL_TREE, op, t);

          number_of_places_left_in_vector--;

          if (number_of_places_left_in_vector == 0)
            {
              number_of_places_left_in_vector = nunits;

	      if (constant_p)
		vec_cst = build_vector (vector_type, t);
	      else
		vec_cst = build_constructor_from_list (vector_type, t);
              VEC_quick_push (tree, voprnds,
                              vect_init_vector (stmt, vec_cst, vector_type, NULL));
              t = NULL_TREE;
            }
        }
    }

  /* Since the vectors are created in the reverse order, we should invert
     them.  */
  vec_num = VEC_length (tree, voprnds);
  for (j = vec_num - 1; j >= 0; j--)
    {
      vop = VEC_index (tree, voprnds, j);
      VEC_quick_push (tree, *vec_oprnds, vop);
    }

  VEC_free (tree, heap, voprnds);

  /* In case that VF is greater than the unrolling factor needed for the SLP
     group of stmts, NUMBER_OF_VECTORS to be created is greater than
     NUMBER_OF_SCALARS/NUNITS or NUNITS/NUMBER_OF_SCALARS, and hence we have
     to replicate the vectors.  */
  while (number_of_vectors > VEC_length (tree, *vec_oprnds))
    {
      tree neutral_vec = NULL;

      if (neutral_op)
        {
          if (!neutral_vec)
	    neutral_vec = build_vector_from_val (vector_type, neutral_op);

          VEC_quick_push (tree, *vec_oprnds, neutral_vec);
        }
      else
        {
          for (i = 0; VEC_iterate (tree, *vec_oprnds, i, vop) && i < vec_num; i++)
            VEC_quick_push (tree, *vec_oprnds, vop);
        }
    }
}


/* Get vectorized definitions from SLP_NODE that contains corresponding
   vectorized def-stmts.  */

static void
vect_get_slp_vect_defs (slp_tree slp_node, VEC (tree,heap) **vec_oprnds)
{
  tree vec_oprnd;
  gimple vec_def_stmt;
  unsigned int i;

  gcc_assert (SLP_TREE_VEC_STMTS (slp_node));

  FOR_EACH_VEC_ELT (gimple, SLP_TREE_VEC_STMTS (slp_node), i, vec_def_stmt)
    {
      gcc_assert (vec_def_stmt);
      vec_oprnd = gimple_get_lhs (vec_def_stmt);
      VEC_quick_push (tree, *vec_oprnds, vec_oprnd);
    }
}


/* Get vectorized definitions for SLP_NODE.
   If the scalar definitions are loop invariants or constants, collect them and
   call vect_get_constant_vectors() to create vector stmts.
   Otherwise, the def-stmts must be already vectorized and the vectorized stmts
   must be stored in the corresponding child of SLP_NODE, and we call
   vect_get_slp_vect_defs () to retrieve them.  */

void
vect_get_slp_defs (VEC (tree, heap) *ops, slp_tree slp_node,
                   VEC (slp_void_p, heap) **vec_oprnds, int reduc_index)
{
  gimple first_stmt, first_def;
  int number_of_vects = 0, i;
  unsigned int child_index = 0;
  HOST_WIDE_INT lhs_size_unit, rhs_size_unit;
  slp_tree child = NULL;
  VEC (tree, heap) *vec_defs;
  tree oprnd, def_lhs;
  bool vectorized_defs;

  first_stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (slp_node), 0);
  FOR_EACH_VEC_ELT (tree, ops, i, oprnd)
    {
      /* For each operand we check if it has vectorized definitions in a child
	 node or we need to create them (for invariants and constants).  We
	 check if the LHS of the first stmt of the next child matches OPRND.
	 If it does, we found the correct child.  Otherwise, we call
	 vect_get_constant_vectors (), and not advance CHILD_INDEX in order
	 to check this child node for the next operand.  */
      vectorized_defs = false;
      if (VEC_length (slp_void_p, SLP_TREE_CHILDREN (slp_node)) > child_index)
        {
          child = (slp_tree) VEC_index (slp_void_p,
					SLP_TREE_CHILDREN (slp_node),
					child_index);
          first_def = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (child), 0);

	  /* In the end of a pattern sequence we have a use of the original stmt,
	     so we need to compare OPRND with the original def.  */
          if (is_pattern_stmt_p (vinfo_for_stmt (first_def))
	      && !STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (first_stmt))
              && !is_pattern_stmt_p (vinfo_for_stmt (first_stmt)))
            first_def = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (first_def));

          if (is_gimple_call (first_def))
            def_lhs = gimple_call_lhs (first_def);
          else
            def_lhs = gimple_assign_lhs (first_def);

          if (operand_equal_p (oprnd, def_lhs, 0))
            {
              /* The number of vector defs is determined by the number of
                 vector statements in the node from which we get those
		 statements.  */
                 number_of_vects = SLP_TREE_NUMBER_OF_VEC_STMTS (child);
                 vectorized_defs = true;
	      child_index++;
            }
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
      vec_defs = VEC_alloc (tree, heap, number_of_vects);

      /* For reduction defs we call vect_get_constant_vectors (), since we are
         looking for initial loop invariant values.  */
      if (vectorized_defs && reduc_index == -1)
        /* The defs are already vectorized.  */
        vect_get_slp_vect_defs (child, &vec_defs);
      else
        /* Build vectors from scalar defs.  */
        vect_get_constant_vectors (oprnd, slp_node, &vec_defs, i,
                                   number_of_vects, reduc_index);

      VEC_quick_push (slp_void_p, *vec_oprnds, (slp_void_p) vec_defs);

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
vect_create_mask_and_perm (gimple stmt, gimple next_scalar_stmt,
                           tree mask, int first_vec_indx, int second_vec_indx,
                           gimple_stmt_iterator *gsi, slp_tree node,
                           tree vectype, VEC(tree,heap) *dr_chain,
                           int ncopies, int vect_stmts_counter)
{
  tree perm_dest;
  gimple perm_stmt = NULL;
  stmt_vec_info next_stmt_info;
  int i, stride;
  tree first_vec, second_vec, data_ref;

  stride = SLP_TREE_NUMBER_OF_VEC_STMTS (node) / ncopies;

  /* Initialize the vect stmts of NODE to properly insert the generated
     stmts later.  */
  for (i = VEC_length (gimple, SLP_TREE_VEC_STMTS (node));
       i < (int) SLP_TREE_NUMBER_OF_VEC_STMTS (node); i++)
    VEC_quick_push (gimple, SLP_TREE_VEC_STMTS (node), NULL);

  perm_dest = vect_create_destination_var (gimple_assign_lhs (stmt), vectype);
  for (i = 0; i < ncopies; i++)
    {
      first_vec = VEC_index (tree, dr_chain, first_vec_indx);
      second_vec = VEC_index (tree, dr_chain, second_vec_indx);

      /* Generate the permute statement.  */
      perm_stmt = gimple_build_assign_with_ops3 (VEC_PERM_EXPR, perm_dest,
						 first_vec, second_vec, mask);
      data_ref = make_ssa_name (perm_dest, perm_stmt);
      gimple_set_lhs (perm_stmt, data_ref);
      vect_finish_stmt_generation (stmt, perm_stmt, gsi);

      /* Store the vector statement in NODE.  */
      VEC_replace (gimple, SLP_TREE_VEC_STMTS (node),
                   stride * i + vect_stmts_counter, perm_stmt);

      first_vec_indx += stride;
      second_vec_indx += stride;
    }

  /* Mark the scalar stmt as vectorized.  */
  next_stmt_info = vinfo_for_stmt (next_scalar_stmt);
  STMT_VINFO_VEC_STMT (next_stmt_info) = perm_stmt;
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

  if (*current_mask_element < mask_nunits)
    *needs_first_vector = true;

  /* We have only one input vector to permute but the mask accesses values in
     the next vector as well.  */
  if (only_one_vec && *current_mask_element >= mask_nunits)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "permutation requires at least two vectors ");
          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
        }

      return false;
    }

  /* The mask requires the next vector.  */
  if (*current_mask_element >= mask_nunits * 2)
    {
      if (*needs_first_vector || *mask_fixed)
        {
          /* We either need the first vector too or have already moved to the
             next vector. In both cases, this permutation needs three
             vectors.  */
          if (vect_print_dump_info (REPORT_DETAILS))
            {
              fprintf (vect_dump, "permutation requires at "
                                  "least three vectors ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
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
   permute statements for SLP_NODE_INSTANCE.  */
bool
vect_transform_slp_perm_load (gimple stmt, VEC (tree, heap) *dr_chain,
                              gimple_stmt_iterator *gsi, int vf,
                              slp_instance slp_node_instance, bool analyze_only)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree mask_element_type = NULL_TREE, mask_type;
  int i, j, k, nunits, vec_index = 0, scalar_index;
  slp_tree node;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  gimple next_scalar_stmt;
  int group_size = SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
  int first_mask_element;
  int index, unroll_factor, current_mask_element, ncopies;
  unsigned char *mask;
  bool only_one_vec = false, need_next_vector = false;
  int first_vec_index, second_vec_index, orig_vec_stmts_num, vect_stmts_counter;
  int number_of_mask_fixes = 1;
  bool mask_fixed = false;
  bool needs_first_vector = false;
  enum machine_mode mode;

  mode = TYPE_MODE (vectype);

  if (!can_vec_perm_p (mode, false, NULL))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "no vect permute for ");
          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
        }
      return false;
    }

  /* The generic VEC_PERM_EXPR code always uses an integral type of the
     same size as the vector element being permuted.  */
  mask_element_type
    = lang_hooks.types.type_for_size
    (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (vectype))), 1);
  mask_type = get_vectype_for_scalar_type (mask_element_type);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  mask = XALLOCAVEC (unsigned char, nunits);
  unroll_factor = SLP_INSTANCE_UNROLLING_FACTOR (slp_node_instance);

  /* The number of vector stmts to generate based only on SLP_NODE_INSTANCE
     unrolling factor.  */
  orig_vec_stmts_num = group_size *
                SLP_INSTANCE_UNROLLING_FACTOR (slp_node_instance) / nunits;
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

  FOR_EACH_VEC_ELT  (slp_tree, SLP_INSTANCE_LOADS (slp_node_instance), i, node)
    {
      scalar_index = 0;
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
              first_mask_element = i + j * group_size;
              if (!vect_get_mask_element (stmt, first_mask_element, 0,
					  nunits, only_one_vec, index,
					  mask, &current_mask_element,
					  &need_next_vector,
					  &number_of_mask_fixes, &mask_fixed,
					  &needs_first_vector))
		return false;
	      mask[index++] = current_mask_element;

              if (index == nunits)
                {
		  tree mask_vec = NULL;

		  if (!can_vec_perm_p (mode, false, mask))
		    {
		      if (vect_print_dump_info (REPORT_DETAILS))
			{
			  fprintf (vect_dump, "unsupported vect permute { ");
			  for (i = 0; i < nunits; ++i)
			    fprintf (vect_dump, "%d ", mask[i]);
			  fprintf (vect_dump, "}\n");
			}
		      return false;
		    }

		  while (--index >= 0)
		    {
		      tree t = build_int_cst (mask_element_type, mask[index]);
		      mask_vec = tree_cons (NULL, t, mask_vec);
		    }
		  mask_vec = build_vector (mask_type, mask_vec);
		  index = 0;

                  if (!analyze_only)
                    {
                      if (need_next_vector)
                        {
                          first_vec_index = second_vec_index;
                          second_vec_index = vec_index;
                        }

                      next_scalar_stmt = VEC_index (gimple,
                                SLP_TREE_SCALAR_STMTS (node), scalar_index++);

                      vect_create_mask_and_perm (stmt, next_scalar_stmt,
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
  bool strided_store, is_store;
  gimple_stmt_iterator si;
  stmt_vec_info stmt_info;
  unsigned int vec_stmts_size, nunits, group_size;
  tree vectype;
  int i;
  slp_tree loads_node;
  slp_void_p child;

  if (!node)
    return false;

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_schedule_slp_instance ((slp_tree) child, instance,
                                vectorization_factor);

  stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (node), 0);
  stmt_info = vinfo_for_stmt (stmt);

  /* VECTYPE is the type of the destination.  */
  vectype = STMT_VINFO_VECTYPE (stmt_info);
  nunits = (unsigned int) TYPE_VECTOR_SUBPARTS (vectype);
  group_size = SLP_INSTANCE_GROUP_SIZE (instance);

  /* For each SLP instance calculate number of vector stmts to be created
     for the scalar stmts in each node of the SLP tree.  Number of vector
     elements in one vector iteration is the number of scalar elements in
     one scalar iteration (GROUP_SIZE) multiplied by VF divided by vector
     size.  */
  vec_stmts_size = (vectorization_factor * group_size) / nunits;

  /* In case of load permutation we have to allocate vectorized statements for
     all the nodes that participate in that permutation.  */
  if (SLP_INSTANCE_LOAD_PERMUTATION (instance))
    {
      FOR_EACH_VEC_ELT (slp_tree, SLP_INSTANCE_LOADS (instance), i, loads_node)
        {
          if (!SLP_TREE_VEC_STMTS (loads_node))
            {
              SLP_TREE_VEC_STMTS (loads_node) = VEC_alloc (gimple, heap,
                                                           vec_stmts_size);
              SLP_TREE_NUMBER_OF_VEC_STMTS (loads_node) = vec_stmts_size;
            }
        }
    }

  if (!SLP_TREE_VEC_STMTS (node))
    {
      SLP_TREE_VEC_STMTS (node) = VEC_alloc (gimple, heap, vec_stmts_size);
      SLP_TREE_NUMBER_OF_VEC_STMTS (node) = vec_stmts_size;
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "------>vectorizing SLP node starting from: ");
      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
    }

  /* Loads should be inserted before the first load.  */
  if (SLP_INSTANCE_FIRST_LOAD_STMT (instance)
      && STMT_VINFO_STRIDED_ACCESS (stmt_info)
      && !REFERENCE_CLASS_P (gimple_get_lhs (stmt))
      && SLP_INSTANCE_LOAD_PERMUTATION (instance))
    si = gsi_for_stmt (SLP_INSTANCE_FIRST_LOAD_STMT (instance));
  else if (is_pattern_stmt_p (stmt_info))
    si = gsi_for_stmt (STMT_VINFO_RELATED_STMT (stmt_info));
  else
    si = gsi_for_stmt (stmt);

  /* Stores should be inserted just before the last store.  */
  if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
      && REFERENCE_CLASS_P (gimple_get_lhs (stmt)))
    { 
      gimple last_store = vect_find_last_store_in_slp_instance (instance);
      if (is_pattern_stmt_p (vinfo_for_stmt (last_store)))
       last_store = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (last_store));
      si = gsi_for_stmt (last_store);
    }

  /* Mark the first element of the reduction chain as reduction to properly
     transform the node.  In the analysis phase only the last element of the
     chain is marked as reduction.  */
  if (GROUP_FIRST_ELEMENT (stmt_info) && !STMT_VINFO_STRIDED_ACCESS (stmt_info)
      && GROUP_FIRST_ELEMENT (stmt_info) == stmt)
    {
      STMT_VINFO_DEF_TYPE (stmt_info) = vect_reduction_def;
      STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
    }

  is_store = vect_transform_stmt (stmt, &si, &strided_store, node, instance);
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
  slp_void_p child;
  tree lhs;
  stmt_vec_info stmt_info;

  if (!node)
    return;

  FOR_EACH_VEC_ELT (slp_void_p, SLP_TREE_CHILDREN (node), i, child)
    vect_remove_slp_scalar_calls ((slp_tree) child);

  FOR_EACH_VEC_ELT (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt)
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
  VEC (slp_instance, heap) *slp_instances;
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

  FOR_EACH_VEC_ELT (slp_instance, slp_instances, i, instance)
    {
      /* Schedule the tree of INSTANCE.  */
      is_store = vect_schedule_slp_instance (SLP_INSTANCE_TREE (instance),
                                             instance, vf);
      if (vect_print_dump_info (REPORT_VECTORIZED_LOCATIONS)
	  || vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	fprintf (vect_dump, "vectorizing stmts using SLP.");
    }

  FOR_EACH_VEC_ELT (slp_instance, slp_instances, i, instance)
    {
      slp_tree root = SLP_INSTANCE_TREE (instance);
      gimple store;
      unsigned int j;
      gimple_stmt_iterator gsi;

      vect_remove_slp_scalar_calls (root);

      for (j = 0; VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (root), j, store)
                  && j < SLP_INSTANCE_GROUP_SIZE (instance); j++)
        {
          if (!STMT_VINFO_DATA_REF (vinfo_for_stmt (store)))
            break;

         if (is_pattern_stmt_p (vinfo_for_stmt (store)))
           store = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (store));
          /* Free the attached stmt_vec_info and remove the stmt.  */
          gsi = gsi_for_stmt (store);
          gsi_remove (&gsi, true);
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

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "SLPing BB\n");

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple stmt = gsi_stmt (si);
      stmt_vec_info stmt_info;

      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "------>SLPing statement: ");
          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
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

  mark_sym_for_renaming (gimple_vop (cfun));
  /* The memory tags and pointers in vectorized statements need to
     have their SSA forms updated.  FIXME, why can't this be delayed
     until all the loops have been transformed?  */
  update_ssa (TODO_update_ssa);

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "BASIC BLOCK VECTORIZED\n");

  destroy_bb_vec_info (bb_vinfo);
}

