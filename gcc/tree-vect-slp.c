/* SLP - Basic Block Vectorization
   Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
   Foundation, Inc.
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
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "expr.h"
#include "recog.h"
#include "optabs.h"
#include "tree-vectorizer.h"

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
  if (!node)
    return;

  if (SLP_TREE_LEFT (node))
    vect_free_slp_tree (SLP_TREE_LEFT (node));

  if (SLP_TREE_RIGHT (node))
    vect_free_slp_tree (SLP_TREE_RIGHT (node));

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


/* Get the defs for the rhs of STMT (collect them in DEF_STMTS0/1), check that
   they are of a legal type and that they match the defs of the first stmt of
   the SLP group (stored in FIRST_STMT_...).  */

static bool
vect_get_and_check_slp_defs (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                             slp_tree slp_node, gimple stmt,
			     VEC (gimple, heap) **def_stmts0,
			     VEC (gimple, heap) **def_stmts1,
			     enum vect_def_type *first_stmt_dt0,
			     enum vect_def_type *first_stmt_dt1,
			     tree *first_stmt_def0_type,
			     tree *first_stmt_def1_type,
			     tree *first_stmt_const_oprnd,
			     int ncopies_for_cost,
                             bool *pattern0, bool *pattern1)
{
  tree oprnd;
  unsigned int i, number_of_oprnds;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  stmt_vec_info stmt_info =
    vinfo_for_stmt (VEC_index (gimple, SLP_TREE_SCALAR_STMTS (slp_node), 0));
  enum gimple_rhs_class rhs_class;
  struct loop *loop = NULL;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  rhs_class = get_gimple_rhs_class (gimple_assign_rhs_code (stmt));
  number_of_oprnds = gimple_num_ops (stmt) - 1;	/* RHS only */

  for (i = 0; i < number_of_oprnds; i++)
    {
      oprnd = gimple_op (stmt, i + 1);

      if (!vect_is_simple_use (oprnd, loop_vinfo, bb_vinfo, &def_stmt, &def,
                               &dt[i])
	  || (!def_stmt && dt[i] != vect_constant_def))
	{
	  if (vect_print_dump_info (REPORT_SLP))
	    {
	      fprintf (vect_dump, "Build SLP failed: can't find def for ");
	      print_generic_expr (vect_dump, oprnd, TDF_SLIM);
	    }

	  return false;
	}

      /* Check if DEF_STMT is a part of a pattern in LOOP and get the def stmt
         from the pattern. Check that all the stmts of the node are in the
         pattern.  */
      if (loop && def_stmt && gimple_bb (def_stmt)
          && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
          && vinfo_for_stmt (def_stmt)
          && STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (def_stmt)))
        {
          if (!*first_stmt_dt0)
            *pattern0 = true;
          else
            {
              if (i == 1 && !*first_stmt_dt1)
                *pattern1 = true;
              else if ((i == 0 && !*pattern0) || (i == 1 && !*pattern1))
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    {
                      fprintf (vect_dump, "Build SLP failed: some of the stmts"
                                     " are in a pattern, and others are not ");
                      print_generic_expr (vect_dump, oprnd, TDF_SLIM);
                    }

                  return false;
                }
            }

          def_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt));
          dt[i] = STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt));

          if (*dt == vect_unknown_def_type)
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

      if (!*first_stmt_dt0)
	{
	  /* op0 of the first stmt of the group - store its info.  */
	  *first_stmt_dt0 = dt[i];
	  if (def)
	    *first_stmt_def0_type = TREE_TYPE (def);
	  else
	    *first_stmt_const_oprnd = oprnd;

	  /* Analyze costs (for the first stmt of the group only).  */
	  if (rhs_class != GIMPLE_SINGLE_RHS)
	    /* Not memory operation (we don't call this functions for loads).  */
	    vect_model_simple_cost (stmt_info, ncopies_for_cost, dt, slp_node);
	  else
	    /* Store.  */
	    vect_model_store_cost (stmt_info, ncopies_for_cost, dt[0], slp_node);
	}

      else
	{
	  if (!*first_stmt_dt1 && i == 1)
	    {
	      /* op1 of the first stmt of the group - store its info.  */
	      *first_stmt_dt1 = dt[i];
	      if (def)
		*first_stmt_def1_type = TREE_TYPE (def);
	      else
		{
		  /* We assume that the stmt contains only one constant
		     operand. We fail otherwise, to be on the safe side.  */
		  if (*first_stmt_const_oprnd)
		    {
		      if (vect_print_dump_info (REPORT_SLP))
			fprintf (vect_dump, "Build SLP failed: two constant "
				 "oprnds in stmt");
		      return false;
		    }
		  *first_stmt_const_oprnd = oprnd;
		}
	    }
	  else
	    {
	      /* Not first stmt of the group, check that the def-stmt/s match
		 the def-stmt/s of the first stmt.  */
	      if ((i == 0
		   && (*first_stmt_dt0 != dt[i]
		       || (*first_stmt_def0_type && def
			   && *first_stmt_def0_type != TREE_TYPE (def))))
		  || (i == 1
		      && (*first_stmt_dt1 != dt[i]
			  || (*first_stmt_def1_type && def
			      && *first_stmt_def1_type != TREE_TYPE (def))))
		  || (!def
		      && TREE_TYPE (*first_stmt_const_oprnd)
		      != TREE_TYPE (oprnd)))
		{
		  if (vect_print_dump_info (REPORT_SLP))
		    fprintf (vect_dump, "Build SLP failed: different types ");

		  return false;
		}
	    }
	}

      /* Check the types of the definitions.  */
      switch (dt[i])
	{
	case vect_constant_def:
	case vect_external_def:
	  break;

	case vect_internal_def:
	  if (i == 0)
	    VEC_safe_push (gimple, heap, *def_stmts0, def_stmt);
	  else
	    VEC_safe_push (gimple, heap, *def_stmts1, def_stmt);
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
   permutation or are of unsupported types of operation. Otherwise, return
   TRUE.  */

static bool
vect_build_slp_tree (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                     slp_tree *node, unsigned int group_size,
                     int *inside_cost, int *outside_cost,
                     int ncopies_for_cost, unsigned int *max_nunits,
                     VEC (int, heap) **load_permutation,
                     VEC (slp_tree, heap) **loads,
                     unsigned int vectorization_factor)
{
  VEC (gimple, heap) *def_stmts0 = VEC_alloc (gimple, heap, group_size);
  VEC (gimple, heap) *def_stmts1 =  VEC_alloc (gimple, heap, group_size);
  unsigned int i;
  VEC (gimple, heap) *stmts = SLP_TREE_SCALAR_STMTS (*node);
  gimple stmt = VEC_index (gimple, stmts, 0);
  enum vect_def_type first_stmt_dt0 = vect_uninitialized_def;
  enum vect_def_type first_stmt_dt1 = vect_uninitialized_def;
  enum tree_code first_stmt_code = ERROR_MARK, rhs_code;
  tree first_stmt_def1_type = NULL_TREE, first_stmt_def0_type = NULL_TREE;
  tree lhs;
  bool stop_recursion = false, need_same_oprnds = false;
  tree vectype, scalar_type, first_op1 = NULL_TREE;
  unsigned int ncopies;
  optab optab;
  int icode;
  enum machine_mode optab_op2_mode;
  enum machine_mode vec_mode;
  tree first_stmt_const_oprnd = NULL_TREE;
  struct data_reference *first_dr;
  bool pattern0 = false, pattern1 = false;
  HOST_WIDE_INT dummy;
  bool permutation = false;
  unsigned int load_place;
  gimple first_load;

  /* For every stmt in NODE find its def stmt/s.  */
  for (i = 0; VEC_iterate (gimple, stmts, i, stmt); i++)
    {
      if (vect_print_dump_info (REPORT_SLP))
	{
	  fprintf (vect_dump, "Build SLP for ");
	  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	}

      lhs = gimple_get_lhs (stmt);
      if (lhs == NULL_TREE)
	{
	  if (vect_print_dump_info (REPORT_SLP))
	    {
	      fprintf (vect_dump,
		       "Build SLP failed: not GIMPLE_ASSIGN nor GIMPLE_CALL");
	      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	    }

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
          return false;
        }

      ncopies = vectorization_factor / TYPE_VECTOR_SUBPARTS (vectype);
      if (ncopies != 1)
        {
	  if (vect_print_dump_info (REPORT_SLP))
            fprintf (vect_dump, "SLP with multiple types ");

          /* FORNOW: multiple types are unsupported in BB SLP.  */
	  if (bb_vinfo)
	    return false;
        }

      /* In case of multiple types we need to detect the smallest type.  */
      if (*max_nunits < TYPE_VECTOR_SUBPARTS (vectype))
        *max_nunits = TYPE_VECTOR_SUBPARTS (vectype);

      if (is_gimple_call (stmt))
	rhs_code = CALL_EXPR;
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
		  || (optab->handlers[(int) vec_mode].insn_code
		      == CODE_FOR_nothing))
		{
		  /* No vector/vector shift, try for a vector/scalar shift.  */
		  optab = optab_for_tree_code (rhs_code, vectype,
					       optab_scalar);

		  if (!optab)
		    {
		      if (vect_print_dump_info (REPORT_SLP))
			fprintf (vect_dump, "Build SLP failed: no optab.");
		      return false;
		    }
		  icode = (int) optab->handlers[(int) vec_mode].insn_code;
		  if (icode == CODE_FOR_nothing)
		    {
		      if (vect_print_dump_info (REPORT_SLP))
			fprintf (vect_dump, "Build SLP failed: "
				            "op not supported by target.");
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
	}
      else
	{
	  if (first_stmt_code != rhs_code
	      && (first_stmt_code != IMAGPART_EXPR
		  || rhs_code != REALPART_EXPR)
	      && (first_stmt_code != REALPART_EXPR
		  || rhs_code != IMAGPART_EXPR))
	    {
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump,
			   "Build SLP failed: different operation in stmt ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

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

	      return false;
	    }
	}

      /* Strided store or load.  */
      if (STMT_VINFO_STRIDED_ACCESS (vinfo_for_stmt (stmt)))
	{
	  if (REFERENCE_CLASS_P (lhs))
	    {
	      /* Store.  */
	      if (!vect_get_and_check_slp_defs (loop_vinfo, bb_vinfo, *node,
						stmt, &def_stmts0, &def_stmts1,
						&first_stmt_dt0,
						&first_stmt_dt1,
						&first_stmt_def0_type,
						&first_stmt_def1_type,
						&first_stmt_const_oprnd,
						ncopies_for_cost,
                                                &pattern0, &pattern1))
		return false;
	    }
	    else
	      {
		/* Load.  */
                /* FORNOW: Check that there is no gap between the loads.  */
                if ((DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)) == stmt
                     && DR_GROUP_GAP (vinfo_for_stmt (stmt)) != 0)
                    || (DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)) != stmt
                        && DR_GROUP_GAP (vinfo_for_stmt (stmt)) != 1))
                  {
                    if (vect_print_dump_info (REPORT_SLP))
                      {
                        fprintf (vect_dump, "Build SLP failed: strided "
                                            "loads have gaps ");
                        print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                      }

                    return false;
                  }

                /* Check that the size of interleaved loads group is not
                   greater than the SLP group size.  */
                if (DR_GROUP_SIZE (vinfo_for_stmt (stmt))
                    > ncopies * group_size)
                  {
                    if (vect_print_dump_info (REPORT_SLP))
                      {
                        fprintf (vect_dump, "Build SLP failed: the number of "
                                            "interleaved loads is greater than"
                                            " the SLP group size ");
                        print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                      }

                    return false;
                  }

                first_load = DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt));

              if (first_load == stmt)
                {
                  first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt));
                  if (vect_supportable_dr_alignment (first_dr)
                      == dr_unaligned_unsupported)
                    {
                      if (vect_print_dump_info (REPORT_SLP))
                        {
                          fprintf (vect_dump, "Build SLP failed: unsupported "
                                              "unaligned load ");
                          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
                        }

                      return false;
                    }

                  /* Analyze costs (for the first stmt in the group).  */
                  vect_model_load_cost (vinfo_for_stmt (stmt),
                                        ncopies_for_cost, *node);
                }

              /* Store the place of this load in the interleaving chain. In
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
	      /* Not strided load. */
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump, "Build SLP failed: not strided load ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      /* FORNOW: Not strided loads are not supported.  */
	      return false;
	    }

	  /* Not memory operation.  */
	  if (TREE_CODE_CLASS (rhs_code) != tcc_binary
	      && TREE_CODE_CLASS (rhs_code) != tcc_unary)
	    {
	      if (vect_print_dump_info (REPORT_SLP))
		{
		  fprintf (vect_dump, "Build SLP failed: operation");
		  fprintf (vect_dump, " unsupported ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      return false;
	    }

	  /* Find the def-stmts.  */
	  if (!vect_get_and_check_slp_defs (loop_vinfo, bb_vinfo, *node, stmt,
					    &def_stmts0, &def_stmts1,
					    &first_stmt_dt0, &first_stmt_dt1,
					    &first_stmt_def0_type,
					    &first_stmt_def1_type,
					    &first_stmt_const_oprnd,
					    ncopies_for_cost,
                                            &pattern0, &pattern1))
	    return false;
	}
    }

  /* Add the costs of the node to the overall instance costs.  */
  *inside_cost += SLP_TREE_INSIDE_OF_LOOP_COST (*node);
  *outside_cost += SLP_TREE_OUTSIDE_OF_LOOP_COST (*node);

  /* Strided loads were reached - stop the recursion.  */
  if (stop_recursion)
    {
      if (permutation)
        {
          VEC_safe_push (slp_tree, heap, *loads, *node);
          *inside_cost += TARG_VEC_PERMUTE_COST * group_size;
        }

      return true;
    }

  /* Create SLP_TREE nodes for the definition node/s.  */
  if (first_stmt_dt0 == vect_internal_def)
    {
      slp_tree left_node = XNEW (struct _slp_tree);
      SLP_TREE_SCALAR_STMTS (left_node) = def_stmts0;
      SLP_TREE_VEC_STMTS (left_node) = NULL;
      SLP_TREE_LEFT (left_node) = NULL;
      SLP_TREE_RIGHT (left_node) = NULL;
      SLP_TREE_OUTSIDE_OF_LOOP_COST (left_node) = 0;
      SLP_TREE_INSIDE_OF_LOOP_COST (left_node) = 0;
      if (!vect_build_slp_tree (loop_vinfo, bb_vinfo, &left_node, group_size,
				inside_cost, outside_cost, ncopies_for_cost,
				max_nunits, load_permutation, loads,
				vectorization_factor))
	return false;

      SLP_TREE_LEFT (*node) = left_node;
    }

  if (first_stmt_dt1 == vect_internal_def)
    {
      slp_tree right_node = XNEW (struct _slp_tree);
      SLP_TREE_SCALAR_STMTS (right_node) = def_stmts1;
      SLP_TREE_VEC_STMTS (right_node) = NULL;
      SLP_TREE_LEFT (right_node) = NULL;
      SLP_TREE_RIGHT (right_node) = NULL;
      SLP_TREE_OUTSIDE_OF_LOOP_COST (right_node) = 0;
      SLP_TREE_INSIDE_OF_LOOP_COST (right_node) = 0;
      if (!vect_build_slp_tree (loop_vinfo, bb_vinfo, &right_node, group_size,
				inside_cost, outside_cost, ncopies_for_cost,
				max_nunits, load_permutation, loads,
				vectorization_factor))
	return false;

      SLP_TREE_RIGHT (*node) = right_node;
    }

  return true;
}


static void
vect_print_slp_tree (slp_tree node)
{
  int i;
  gimple stmt;

  if (!node)
    return;

  fprintf (vect_dump, "node ");
  for (i = 0; VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt); i++)
    {
      fprintf (vect_dump, "\n\tstmt %d ", i);
      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
    }
  fprintf (vect_dump, "\n");

  vect_print_slp_tree (SLP_TREE_LEFT (node));
  vect_print_slp_tree (SLP_TREE_RIGHT (node));
}


/* Mark the tree rooted at NODE with MARK (PURE_SLP or HYBRID).
   If MARK is HYBRID, it refers to a specific stmt in NODE (the stmt at index
   J). Otherwise, MARK is PURE_SLP and J is -1, which indicates that all the
   stmts in NODE are to be marked.  */

static void
vect_mark_slp_stmts (slp_tree node, enum slp_vect_type mark, int j)
{
  int i;
  gimple stmt;

  if (!node)
    return;

  for (i = 0; VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt); i++)
    if (j < 0 || i == j)
      STMT_SLP_TYPE (vinfo_for_stmt (stmt)) = mark;

  vect_mark_slp_stmts (SLP_TREE_LEFT (node), mark, j);
  vect_mark_slp_stmts (SLP_TREE_RIGHT (node), mark, j);
}


/* Mark the statements of the tree rooted at NODE as relevant (vect_used).  */

static void
vect_mark_slp_stmts_relevant (slp_tree node)
{
  int i;
  gimple stmt;
  stmt_vec_info stmt_info;

  if (!node)
    return;

  for (i = 0; VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt); i++)
    {
      stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (!STMT_VINFO_RELEVANT (stmt_info)
                  || STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope);
      STMT_VINFO_RELEVANT (stmt_info) = vect_used_in_scope;
    }

  vect_mark_slp_stmts_relevant (SLP_TREE_LEFT (node));
  vect_mark_slp_stmts_relevant (SLP_TREE_RIGHT (node));
}


/* Check if the permutation required by the SLP INSTANCE is supported.
   Reorganize the SLP nodes stored in SLP_INSTANCE_LOADS if needed.  */

static bool
vect_supported_slp_permutation_p (slp_instance instance)
{
  slp_tree node = VEC_index (slp_tree, SLP_INSTANCE_LOADS (instance), 0);
  gimple stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (node), 0);
  gimple first_load = DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt));
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
      if (DR_GROUP_FIRST_DR (vinfo_for_stmt (scalar_stmt)) != first_load)
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


/* Check if the required load permutation is supported.
   LOAD_PERMUTATION contains a list of indices of the loads.
   In SLP this permutation is relative to the order of strided stores that are
   the base of the SLP instance.  */

static bool
vect_supported_load_permutation_p (slp_instance slp_instn, int group_size,
                                   VEC (int, heap) *load_permutation)
{
  int i = 0, j, prev = -1, next, k;
  bool supported;
  sbitmap load_index;

  /* FORNOW: permutations are only supported in SLP.  */
  if (!slp_instn)
    return false;

  if (vect_print_dump_info (REPORT_SLP))
    {
      fprintf (vect_dump, "Load permutation ");
      for (i = 0; VEC_iterate (int, load_permutation, i, next); i++)
        fprintf (vect_dump, "%d ", next);
    }

  /* FORNOW: the only supported permutation is 0..01..1.. of length equal to
     GROUP_SIZE and where each sequence of same drs is of GROUP_SIZE length as
     well.  */
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
  
  sbitmap_free (load_index);

  if (supported && i == group_size * group_size
      && vect_supported_slp_permutation_p (slp_instn))
    return true;

  return false;
}


/* Find the first load in the loop that belongs to INSTANCE.
   When loads are in several SLP nodes, there can be a case in which the first
   load does not appear in the first SLP node to be transformed, causing
   incorrect order of statements. Since we generate all the loads together,
   they must be inserted before the first load of the SLP instance and not
   before the first load of the first node of the instance.  */
static gimple
vect_find_first_load_in_slp_instance (slp_instance instance)
{
  int i, j;
  slp_tree load_node;
  gimple first_load = NULL, load;

  for (i = 0;
       VEC_iterate (slp_tree, SLP_INSTANCE_LOADS (instance), i, load_node);
       i++)
    for (j = 0;
         VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (load_node), j, load);
         j++)
      first_load = get_earlier_stmt (load, first_load);

  return first_load;
}


/* Analyze an SLP instance starting from a group of strided stores. Call
   vect_build_slp_tree to build a tree of packed stmts if possible.
   Return FALSE if it's impossible to SLP any stmt in the loop.  */

static bool
vect_analyze_slp_instance (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo,
                           gimple stmt)
{
  slp_instance new_instance;
  slp_tree node = XNEW (struct _slp_tree);
  unsigned int group_size = DR_GROUP_SIZE (vinfo_for_stmt (stmt));
  unsigned int unrolling_factor = 1, nunits;
  tree vectype, scalar_type;
  gimple next;
  unsigned int vectorization_factor = 0;
  int inside_cost = 0, outside_cost = 0, ncopies_for_cost;
  unsigned int max_nunits = 0;
  VEC (int, heap) *load_permutation;
  VEC (slp_tree, heap) *loads;

  scalar_type = TREE_TYPE (DR_REF (STMT_VINFO_DATA_REF (
                                             vinfo_for_stmt (stmt))));
  vectype = get_vectype_for_scalar_type (scalar_type);
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
    /* No multitypes in BB SLP.  */
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
  SLP_TREE_SCALAR_STMTS (node) = VEC_alloc (gimple, heap, group_size);
  next = stmt;
  /* Collect the stores and store them in SLP_TREE_SCALAR_STMTS.  */
  while (next)
    {
      VEC_safe_push (gimple, heap, SLP_TREE_SCALAR_STMTS (node), next);
      next = DR_GROUP_NEXT_DR (vinfo_for_stmt (next));
    }

  SLP_TREE_VEC_STMTS (node) = NULL;
  SLP_TREE_NUMBER_OF_VEC_STMTS (node) = 0;
  SLP_TREE_LEFT (node) = NULL;
  SLP_TREE_RIGHT (node) = NULL;
  SLP_TREE_OUTSIDE_OF_LOOP_COST (node) = 0;
  SLP_TREE_INSIDE_OF_LOOP_COST (node) = 0;

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
			   vectorization_factor))
    {
      /* Create a new SLP instance.  */
      new_instance = XNEW (struct _slp_instance);
      SLP_INSTANCE_TREE (new_instance) = node;
      SLP_INSTANCE_GROUP_SIZE (new_instance) = group_size;
      /* Calculate the unrolling factor based on the smallest type in the
         loop.  */
      if (max_nunits > nunits)
        unrolling_factor = least_common_multiple (max_nunits, group_size)
                           / group_size;

      SLP_INSTANCE_UNROLLING_FACTOR (new_instance) = unrolling_factor;
      SLP_INSTANCE_OUTSIDE_OF_LOOP_COST (new_instance) = outside_cost;
      SLP_INSTANCE_INSIDE_OF_LOOP_COST (new_instance) = inside_cost;
      SLP_INSTANCE_LOADS (new_instance) = loads;
      SLP_INSTANCE_FIRST_LOAD_STMT (new_instance) = NULL;
      SLP_INSTANCE_LOAD_PERMUTATION (new_instance) = load_permutation;
      if (VEC_length (slp_tree, loads))
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


/* Check if there are stmts in the loop can be vectorized using SLP. Build SLP
   trees of packed scalar stmts if SLP is possible.  */

bool
vect_analyze_slp (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo)
{
  unsigned int i;
  VEC (gimple, heap) *strided_stores;
  gimple store;
  bool ok = false;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_analyze_slp ===");

  if (loop_vinfo)
    strided_stores = LOOP_VINFO_STRIDED_STORES (loop_vinfo);
  else
    strided_stores = BB_VINFO_STRIDED_STORES (bb_vinfo);

  for (i = 0; VEC_iterate (gimple, strided_stores, i, store); i++)
    if (vect_analyze_slp_instance (loop_vinfo, bb_vinfo, store))
      ok = true;

  if (bb_vinfo && !ok)
    {
      if (vect_print_dump_info (REPORT_SLP))
        fprintf (vect_dump, "Failed to SLP the basic block.");

      return false;
    }

  return true;
}


/* For each possible SLP instance decide whether to SLP it and calculate overall
   unrolling factor needed to SLP the loop.  */

void
vect_make_slp_decision (loop_vec_info loop_vinfo)
{
  unsigned int i, unrolling_factor = 1;
  VEC (slp_instance, heap) *slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;
  int decided_to_slp = 0;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_make_slp_decision ===");

  for (i = 0; VEC_iterate (slp_instance, slp_instances, i, instance); i++)
    {
      /* FORNOW: SLP if you can.  */
      if (unrolling_factor < SLP_INSTANCE_UNROLLING_FACTOR (instance))
	unrolling_factor = SLP_INSTANCE_UNROLLING_FACTOR (instance);

      /* Mark all the stmts that belong to INSTANCE as PURE_SLP stmts. Later we
	 call vect_detect_hybrid_slp () to find stmts that need hybrid SLP and
	 loop-based vectorization. Such stmts will be marked as HYBRID.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance), pure_slp, -1);
      decided_to_slp++;
    }

  LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo) = unrolling_factor;

  if (decided_to_slp && vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "Decided to SLP %d instances. Unrolling factor %d",
	     decided_to_slp, unrolling_factor);
}


/* Find stmts that must be both vectorized and SLPed (since they feed stmts that
   can't be SLPed) in the tree rooted at NODE. Mark such stmts as HYBRID.  */

static void
vect_detect_hybrid_slp_stmts (slp_tree node)
{
  int i;
  gimple stmt;
  imm_use_iterator imm_iter;
  gimple use_stmt;

  if (!node)
    return;

  for (i = 0; VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt); i++)
    if (PURE_SLP_STMT (vinfo_for_stmt (stmt))
	&& TREE_CODE (gimple_op (stmt, 0)) == SSA_NAME)
      FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, gimple_op (stmt, 0))
	if (vinfo_for_stmt (use_stmt)
	    && !STMT_SLP_TYPE (vinfo_for_stmt (use_stmt))
            && STMT_VINFO_RELEVANT (vinfo_for_stmt (use_stmt)))
	  vect_mark_slp_stmts (node, hybrid, i);

  vect_detect_hybrid_slp_stmts (SLP_TREE_LEFT (node));
  vect_detect_hybrid_slp_stmts (SLP_TREE_RIGHT (node));
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

  for (i = 0; VEC_iterate (slp_instance, slp_instances, i, instance); i++)
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

  if (!node)
    return true;

  if (!vect_slp_analyze_node_operations (bb_vinfo, SLP_TREE_LEFT (node))
      || !vect_slp_analyze_node_operations (bb_vinfo, SLP_TREE_RIGHT (node)))
    return false;

  for (i = 0; VEC_iterate (gimple, SLP_TREE_SCALAR_STMTS (node), i, stmt); i++)
    {
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
      gcc_assert (stmt_info);
      gcc_assert (PURE_SLP_STMT (stmt_info));

      if (!vect_analyze_stmt (stmt, &dummy, node))
        return false;
    }

  return true;
}


/* Analyze statements in SLP instances of the basic block. Return TRUE if the
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


/* Cheick if the basic block can be vectorized.  */

bb_vec_info
vect_slp_analyze_bb (basic_block bb)
{
  bb_vec_info bb_vinfo;
  VEC (ddr_p, heap) *ddrs;
  VEC (slp_instance, heap) *slp_instances;
  slp_instance instance;
  int i, insns = 0;
  gimple_stmt_iterator gsi;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "===vect_slp_analyze_bb===\n");

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    insns++;

  if (insns > PARAM_VALUE (PARAM_SLP_MAX_INSNS_IN_BB))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: too many instructions in basic "
                            "block.\n");

      return NULL;
    }

  bb_vinfo = new_bb_vec_info (bb);
  if (!bb_vinfo)
    return NULL;

  if (!vect_analyze_data_refs (NULL, bb_vinfo))
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

  if (!vect_analyze_data_refs_alignment (NULL, bb_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: bad data alignment in basic "
                            "block.\n");

      destroy_bb_vec_info (bb_vinfo);
      return NULL;
    }

   if (!vect_analyze_data_ref_dependences (NULL, bb_vinfo))
    {
     if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
       fprintf (vect_dump, "not vectorized: unhandled data dependence in basic"
                           " block.\n");

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
  for (i = 0; VEC_iterate (slp_instance, slp_instances, i, instance); i++)
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

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "Basic block will be vectorized using SLP\n");

  return bb_vinfo;
}


/* SLP costs are calculated according to SLP instance unrolling factor (i.e.,
   the number of created vector stmts depends on the unrolling factor). However,
   the actual number of vector stmts for every SLP node depends on VF which is
   set later in vect_analyze_operations(). Hence, SLP costs should be updated.
   In this function we assume that the inside costs calculated in
   vect_model_xxx_cost are linear in ncopies.  */

void
vect_update_slp_costs_according_to_vf (loop_vec_info loop_vinfo)
{
  unsigned int i, vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  VEC (slp_instance, heap) *slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_update_slp_costs_according_to_vf ===");

  for (i = 0; VEC_iterate (slp_instance, slp_instances, i, instance); i++)
    /* We assume that costs are linear in ncopies.  */
    SLP_INSTANCE_INSIDE_OF_LOOP_COST (instance) *= vf
      / SLP_INSTANCE_UNROLLING_FACTOR (instance);
}


/* For constant and loop invariant defs of SLP_NODE this function returns
   (vector) defs (VEC_OPRNDS) that will be used in the vectorized stmts.
   OP_NUM determines if we gather defs for operand 0 or operand 1 of the scalar
   stmts. NUMBER_OF_VECTORS is the number of vector defs to create.  */

static void
vect_get_constant_vectors (slp_tree slp_node, VEC(tree,heap) **vec_oprnds,
			   unsigned int op_num, unsigned int number_of_vectors)
{
  VEC (gimple, heap) *stmts = SLP_TREE_SCALAR_STMTS (slp_node);
  gimple stmt = VEC_index (gimple, stmts, 0);
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  int nunits;
  tree vec_cst;
  tree t = NULL_TREE;
  int j, number_of_places_left_in_vector;
  tree vector_type;
  tree op, vop;
  int group_size = VEC_length (gimple, stmts);
  unsigned int vec_num, i;
  int number_of_copies = 1;
  VEC (tree, heap) *voprnds = VEC_alloc (tree, heap, number_of_vectors);
  bool constant_p, is_store;

  if (STMT_VINFO_DATA_REF (stmt_vinfo))
    {
      is_store = true;
      op = gimple_assign_rhs1 (stmt);
    }
  else
    {
      is_store = false;
      op = gimple_op (stmt, op_num + 1);
    }

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
     of this type can be packed in a vector). The output vector will contain
     two copies of each scalar operand: {s1, s2, s1, s2}. (NUMBER_OF_COPIES
     will be 2).

     If GROUP_SIZE > NUNITS, the scalars will be split into several vectors
     containing the operands.

     For example, NUNITS is four as before, and the group size is 8
     (s1, s2, ..., s8). We will create two vectors {s1, s2, s3, s4} and
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
            op = gimple_op (stmt, op_num + 1);

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
      for (i = 0; VEC_iterate (tree, *vec_oprnds, i, vop) && i < vec_num; i++)
        VEC_quick_push (tree, *vec_oprnds, vop);
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

  for (i = 0;
       VEC_iterate (gimple, SLP_TREE_VEC_STMTS (slp_node), i, vec_def_stmt);
       i++)
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
   must be stored in the LEFT/RIGHT node of SLP_NODE, and we call
   vect_get_slp_vect_defs() to retrieve them.
   If VEC_OPRNDS1 is NULL, don't get vector defs for the second operand (from
   the right node. This is used when the second operand must remain scalar.  */

void
vect_get_slp_defs (slp_tree slp_node, VEC (tree,heap) **vec_oprnds0,
                   VEC (tree,heap) **vec_oprnds1)
{
  gimple first_stmt;
  enum tree_code code;
  int number_of_vects;
  HOST_WIDE_INT lhs_size_unit, rhs_size_unit;

  first_stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (slp_node), 0);
  /* The number of vector defs is determined by the number of vector statements
     in the node from which we get those statements.  */
  if (SLP_TREE_LEFT (slp_node))
    number_of_vects = SLP_TREE_NUMBER_OF_VEC_STMTS (SLP_TREE_LEFT (slp_node));
  else
    {
      number_of_vects = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
      /* Number of vector stmts was calculated according to LHS in
         vect_schedule_slp_instance(), fix it by replacing LHS with RHS, if
         necessary. See vect_get_smallest_scalar_type() for details.  */
      vect_get_smallest_scalar_type (first_stmt, &lhs_size_unit,
                                     &rhs_size_unit);
      if (rhs_size_unit != lhs_size_unit)
        {
          number_of_vects *= rhs_size_unit;
          number_of_vects /= lhs_size_unit;
        }
    }

  /* Allocate memory for vectorized defs.  */
  *vec_oprnds0 = VEC_alloc (tree, heap, number_of_vects);

  /* SLP_NODE corresponds either to a group of stores or to a group of
     unary/binary operations. We don't call this function for loads.  */
  if (SLP_TREE_LEFT (slp_node))
    /* The defs are already vectorized.  */
    vect_get_slp_vect_defs (SLP_TREE_LEFT (slp_node), vec_oprnds0);
  else
    /* Build vectors from scalar defs.  */
    vect_get_constant_vectors (slp_node, vec_oprnds0, 0, number_of_vects);

  if (STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt)))
    /* Since we don't call this function with loads, this is a group of
       stores.  */
    return;

  code = gimple_assign_rhs_code (first_stmt);
  if (get_gimple_rhs_class (code) != GIMPLE_BINARY_RHS || !vec_oprnds1)
    return;

  /* The number of vector defs is determined by the number of vector statements
     in the node from which we get those statements.  */
  if (SLP_TREE_RIGHT (slp_node))
    number_of_vects = SLP_TREE_NUMBER_OF_VEC_STMTS (SLP_TREE_RIGHT (slp_node));
  else
    number_of_vects = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);

  *vec_oprnds1 = VEC_alloc (tree, heap, number_of_vects);

  if (SLP_TREE_RIGHT (slp_node))
    /* The defs are already vectorized.  */
    vect_get_slp_vect_defs (SLP_TREE_RIGHT (slp_node), vec_oprnds1);
  else
    /* Build vectors from scalar defs.  */
    vect_get_constant_vectors (slp_node, vec_oprnds1, 1, number_of_vects);
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
                           tree builtin_decl, tree vectype,
                           VEC(tree,heap) *dr_chain,
                           int ncopies, int vect_stmts_counter)
{
  tree perm_dest;
  gimple perm_stmt = NULL;
  stmt_vec_info next_stmt_info;
  int i, stride;
  tree first_vec, second_vec, data_ref;
  VEC (tree, heap) *params = NULL;

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

      /* Build argument list for the vectorized call.  */
      VEC_free (tree, heap, params);
      params = VEC_alloc (tree, heap, 3);
      VEC_quick_push (tree, params, first_vec);
      VEC_quick_push (tree, params, second_vec);
      VEC_quick_push (tree, params, mask);

      /* Generate the permute statement.  */
      perm_stmt = gimple_build_call_vec (builtin_decl, params);
      data_ref = make_ssa_name (perm_dest, perm_stmt);
      gimple_call_set_lhs (perm_stmt, data_ref);
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
   representation. Check that the mask is valid and return FALSE if not.
   Return TRUE in NEED_NEXT_VECTOR if the permutation requires to move to
   the next vector, i.e., the current first vector is not needed.  */

static bool
vect_get_mask_element (gimple stmt, int first_mask_element, int m,
                       int mask_nunits, bool only_one_vec, int index,
                       int *mask, int *current_mask_element,
                       bool *need_next_vector)
{
  int i;
  static int number_of_mask_fixes = 1;
  static bool mask_fixed = false;
  static bool needs_first_vector = false;

  /* Convert to target specific representation.  */
  *current_mask_element = first_mask_element + m;
  /* Adjust the value in case it's a mask for second and third vectors.  */
  *current_mask_element -= mask_nunits * (number_of_mask_fixes - 1);

  if (*current_mask_element < mask_nunits)
    needs_first_vector = true;

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
      if (needs_first_vector || mask_fixed)
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
      *current_mask_element -= mask_nunits * number_of_mask_fixes;

      for (i = 0; i < index; i++)
        mask[i] -= mask_nunits * number_of_mask_fixes;

      (number_of_mask_fixes)++;
      mask_fixed = true;
    }

  *need_next_vector = mask_fixed;

  /* This was the last element of this mask. Start a new one.  */
  if (index == mask_nunits - 1)
    {
      number_of_mask_fixes = 1;
      mask_fixed = false;
      needs_first_vector = false;
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
  int i, j, k, m, scale, mask_nunits, nunits, vec_index = 0, scalar_index;
  slp_tree node;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info), builtin_decl;
  gimple next_scalar_stmt;
  int group_size = SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
  int first_mask_element;
  int index, unroll_factor, *mask, current_mask_element, ncopies;
  bool only_one_vec = false, need_next_vector = false;
  int first_vec_index, second_vec_index, orig_vec_stmts_num, vect_stmts_counter;

  if (!targetm.vectorize.builtin_vec_perm)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "no builtin for vect permute for ");
          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
        }

       return false;
    }

  builtin_decl = targetm.vectorize.builtin_vec_perm (vectype,
                                                     &mask_element_type);
  if (!builtin_decl || !mask_element_type)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "no builtin for vect permute for ");
          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
        }

       return false;
    }

  mask_type = get_vectype_for_scalar_type (mask_element_type);
  mask_nunits = TYPE_VECTOR_SUBPARTS (mask_type);
  mask = (int *) xmalloc (sizeof (int) * mask_nunits);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  scale = mask_nunits / nunits;
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

     The masks for a's should be: {0,0,0,3} {3,3,6,6} {6,9,9,9} (in target
     scpecific type, e.g., in bytes for Altivec.
     The last mask is illegal since we assume two operands for permute
     operation, and the mask element values can't be outside that range. Hence,
     the last mask must be converted into {2,5,5,5}.
     For the first two permutations we need the first and the second input
     vectors: {a0,b0,c0,a1} and {b1,c1,a2,b2}, and for the last permutation
     we need the second and the third vectors: {b1,c1,a2,b2} and
     {c2,a3,b3,c3}.  */

  for (i = 0;
       VEC_iterate (slp_tree, SLP_INSTANCE_LOADS (slp_node_instance),
                    i, node);
       i++)
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
              first_mask_element = (i + j * group_size) * scale;
              for (m = 0; m < scale; m++)
                {
                  if (!vect_get_mask_element (stmt, first_mask_element, m,
                                   mask_nunits, only_one_vec, index, mask,
                                   &current_mask_element, &need_next_vector))
                    return false;

                  mask[index++] = current_mask_element;
                }

              if (index == mask_nunits)
                {
		  tree mask_vec = NULL;

		  while (--index >= 0)
		    {
		      tree t = build_int_cst (mask_element_type, mask[index]);
		      mask_vec = tree_cons (NULL, t, mask_vec);
		    }
		  mask_vec = build_vector (mask_type, mask_vec);
		  index = 0;

		  if (!targetm.vectorize.builtin_vec_perm_ok (vectype,
							      mask_vec))
		    {
		      if (vect_print_dump_info (REPORT_DETAILS))
			{
			  fprintf (vect_dump, "unsupported vect permute ");
			  print_generic_expr (vect_dump, mask_vec, 0);
			}
		      free (mask);
		      return false;
		    }

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
			       gsi, node, builtin_decl, vectype, dr_chain,
			       ncopies, vect_stmts_counter++);
                    }
                }
            }
        }
    }

  free (mask);
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

  if (!node)
    return false;

  vect_schedule_slp_instance (SLP_TREE_LEFT (node), instance,
                              vectorization_factor);
  vect_schedule_slp_instance (SLP_TREE_RIGHT (node), instance,
                              vectorization_factor);

  stmt = VEC_index (gimple, SLP_TREE_SCALAR_STMTS (node), 0);
  stmt_info = vinfo_for_stmt (stmt);

  /* VECTYPE is the type of the destination.  */
  vectype = get_vectype_for_scalar_type (TREE_TYPE (gimple_assign_lhs (stmt)));
  nunits = (unsigned int) TYPE_VECTOR_SUBPARTS (vectype);
  group_size = SLP_INSTANCE_GROUP_SIZE (instance);

  /* For each SLP instance calculate number of vector stmts to be created
     for the scalar stmts in each node of the SLP tree. Number of vector
     elements in one vector iteration is the number of scalar elements in
     one scalar iteration (GROUP_SIZE) multiplied by VF divided by vector
     size.  */
  vec_stmts_size = (vectorization_factor * group_size) / nunits;

  /* In case of load permutation we have to allocate vectorized statements for
     all the nodes that participate in that permutation.  */
  if (SLP_INSTANCE_LOAD_PERMUTATION (instance))
    {
      for (i = 0;
           VEC_iterate (slp_tree, SLP_INSTANCE_LOADS (instance), i, loads_node);
           i++)
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
      && !REFERENCE_CLASS_P (gimple_get_lhs (stmt)))
    si = gsi_for_stmt (SLP_INSTANCE_FIRST_LOAD_STMT (instance));
  else
    si = gsi_for_stmt (stmt);

  is_store = vect_transform_stmt (stmt, &si, &strided_store, node, instance);
  if (is_store)
    {
      if (DR_GROUP_FIRST_DR (stmt_info))
	/* If IS_STORE is TRUE, the vectorization of the
	   interleaving chain was completed - free all the stores in
	   the chain.  */
	vect_remove_stores (DR_GROUP_FIRST_DR (stmt_info));
      else
	/* FORNOW: SLP originates only from strided stores.  */
	gcc_unreachable ();

      return true;
    }

  /* FORNOW: SLP originates only from strided stores.  */
  return false;
}


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

  for (i = 0; VEC_iterate (slp_instance, slp_instances, i, instance); i++)
    {
      /* Schedule the tree of INSTANCE.  */
      is_store = vect_schedule_slp_instance (SLP_INSTANCE_TREE (instance),
                                             instance, vf);
      if (vect_print_dump_info (REPORT_VECTORIZED_LOCATIONS)
	  || vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	fprintf (vect_dump, "vectorizing stmts using SLP.");
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

