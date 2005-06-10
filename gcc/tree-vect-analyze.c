/* Analysis Utilities for Loop Vectorization.
   Copyright (C) 2003,2004,2005 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "expr.h"
#include "optabs.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"

/* Main analysis functions.  */
static loop_vec_info vect_analyze_loop_form (struct loop *);
static bool vect_analyze_data_refs (loop_vec_info);
static bool vect_mark_stmts_to_be_vectorized (loop_vec_info);
static void vect_analyze_scalar_cycles (loop_vec_info);
static bool vect_analyze_data_ref_accesses (loop_vec_info);
static bool vect_analyze_data_ref_dependences (loop_vec_info);
static bool vect_analyze_data_refs_alignment (loop_vec_info);
static bool vect_compute_data_refs_alignment (loop_vec_info);
static void vect_enhance_data_refs_alignment (loop_vec_info);
static bool vect_analyze_operations (loop_vec_info);
static bool vect_determine_vectorization_factor (loop_vec_info);

/* Utility functions for the analyses.  */
static bool exist_non_indexing_operands_for_use_p (tree, tree);
static void vect_mark_relevant (VEC(tree,heap) **, tree, bool, bool);
static bool vect_stmt_relevant_p (tree, loop_vec_info, bool *, bool *);
static tree vect_get_loop_niters (struct loop *, tree *);
static bool vect_analyze_data_ref_dependence
  (struct data_reference *, struct data_reference *, loop_vec_info);
static bool vect_compute_data_ref_alignment (struct data_reference *);
static bool vect_analyze_data_ref_access (struct data_reference *);
static struct data_reference * vect_analyze_pointer_ref_access 
  (tree, tree, bool, tree, tree *, tree *);
static bool vect_can_advance_ivs_p (loop_vec_info);
static tree vect_get_ptr_offset (tree, tree, tree *);
static bool vect_analyze_offset_expr (tree, struct loop *, tree, tree *, 
				      tree *, tree *);
static bool vect_base_addr_differ_p (struct data_reference *,
				     struct data_reference *drb, bool *);
static tree vect_object_analysis (tree, tree, bool, tree, 
				  struct data_reference **, tree *, tree *, 
				  tree *, bool *, tree *, struct ptr_info_def **,
				  subvar_t *);
static tree vect_address_analysis (tree, tree, bool, tree, 
				   struct data_reference *, tree *, tree *, 
				   tree *, bool *);


/* Function vect_get_ptr_offset

   Compute the OFFSET modulo vector-type alignment of pointer REF in bits.  */

static tree 
vect_get_ptr_offset (tree ref ATTRIBUTE_UNUSED, 
		     tree vectype ATTRIBUTE_UNUSED, 
		     tree *offset ATTRIBUTE_UNUSED)
{
  /* TODO: Use alignment information.  */
  return NULL_TREE; 
}


/* Function vect_analyze_offset_expr

   Given an offset expression EXPR received from get_inner_reference, analyze
   it and create an expression for INITIAL_OFFSET by substituting the variables 
   of EXPR with initial_condition of the corresponding access_fn in the loop. 
   E.g., 
      for i
         for (j = 3; j < N; j++)
            a[j].b[i][j] = 0;
	 
   For a[j].b[i][j], EXPR will be 'i * C_i + j * C_j + C'. 'i' cannot be 
   substituted, since its access_fn in the inner loop is i. 'j' will be 
   substituted with 3. An INITIAL_OFFSET will be 'i * C_i + C`', where
   C` =  3 * C_j + C.

   Compute MISALIGN (the misalignment of the data reference initial access from
   its base) if possible. Misalignment can be calculated only if all the
   variables can be substituted with constants, or if a variable is multiplied
   by a multiple of VECTYPE_ALIGNMENT. In the above example, since 'i' cannot
   be substituted, MISALIGN will be NULL_TREE in case that C_i is not a multiple
   of VECTYPE_ALIGNMENT, and C` otherwise. (We perform MISALIGN modulo 
   VECTYPE_ALIGNMENT computation in the caller of this function).

   STEP is an evolution of the data reference in this loop in bytes.
   In the above example, STEP is C_j.

   Return FALSE, if the analysis fails, e.g., there is no access_fn for a 
   variable. In this case, all the outputs (INITIAL_OFFSET, MISALIGN and STEP) 
   are NULL_TREEs. Otherwise, return TRUE.

*/

static bool
vect_analyze_offset_expr (tree expr, 
			  struct loop *loop, 
			  tree vectype_alignment,
			  tree *initial_offset,
			  tree *misalign,
			  tree *step)
{
  tree oprnd0;
  tree oprnd1;
  tree left_offset = ssize_int (0);
  tree right_offset = ssize_int (0);
  tree left_misalign = ssize_int (0);
  tree right_misalign = ssize_int (0);
  tree left_step = ssize_int (0);
  tree right_step = ssize_int (0);
  enum tree_code code;
  tree init, evolution;

  *step = NULL_TREE;
  *misalign = NULL_TREE;
  *initial_offset = NULL_TREE;

  /* Strip conversions that don't narrow the mode.  */
  expr = vect_strip_conversion (expr);
  if (!expr)
    return false;

  /* Stop conditions:
     1. Constant.  */
  if (TREE_CODE (expr) == INTEGER_CST)
    {
      *initial_offset = fold_convert (ssizetype, expr);
      *misalign = fold_convert (ssizetype, expr);      
      *step = ssize_int (0);
      return true;
    }

  /* 2. Variable. Try to substitute with initial_condition of the corresponding
     access_fn in the current loop.  */
  if (SSA_VAR_P (expr))
    {
      tree access_fn = analyze_scalar_evolution (loop, expr);

      if (access_fn == chrec_dont_know)
	/* No access_fn.  */
	return false;

      init = initial_condition_in_loop_num (access_fn, loop->num);
      if (init == expr && !expr_invariant_in_loop_p (loop, init))
	/* Not enough information: may be not loop invariant.  
	   E.g., for a[b[i]], we get a[D], where D=b[i]. EXPR is D, its 
	   initial_condition is D, but it depends on i - loop's induction
	   variable.  */	  
	return false;

      evolution = evolution_part_in_loop_num (access_fn, loop->num);
      if (evolution && TREE_CODE (evolution) != INTEGER_CST)
	/* Evolution is not constant.  */
	return false;

      if (TREE_CODE (init) == INTEGER_CST)
	*misalign = fold_convert (ssizetype, init);
      else
	/* Not constant, misalignment cannot be calculated.  */
	*misalign = NULL_TREE;

      *initial_offset = fold_convert (ssizetype, init); 

      *step = evolution ? fold_convert (ssizetype, evolution) : ssize_int (0);
      return true;      
    }

  /* Recursive computation.  */
  if (!BINARY_CLASS_P (expr))
    {
      /* We expect to get binary expressions (PLUS/MINUS and MULT).  */
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
        {
	  fprintf (vect_dump, "Not binary expression ");
          print_generic_expr (vect_dump, expr, TDF_SLIM);
	}
      return false;
    }
  oprnd0 = TREE_OPERAND (expr, 0);
  oprnd1 = TREE_OPERAND (expr, 1);

  if (!vect_analyze_offset_expr (oprnd0, loop, vectype_alignment, &left_offset, 
				&left_misalign, &left_step)
      || !vect_analyze_offset_expr (oprnd1, loop, vectype_alignment, 
				   &right_offset, &right_misalign, &right_step))
    return false;

  /* The type of the operation: plus, minus or mult.  */
  code = TREE_CODE (expr);
  switch (code)
    {
    case MULT_EXPR:
      if (TREE_CODE (right_offset) != INTEGER_CST)
	/* RIGHT_OFFSET can be not constant. For example, for arrays of variable 
	   sized types. 
	   FORNOW: We don't support such cases.  */
	return false;

      /* Strip conversions that don't narrow the mode.  */
      left_offset = vect_strip_conversion (left_offset);      
      if (!left_offset)
	return false;      
      /* Misalignment computation.  */
      if (SSA_VAR_P (left_offset))
	{
	  /* If the left side contains variables that can't be substituted with 
	     constants, we check if the right side is a multiple of ALIGNMENT.
	   */
	  if (integer_zerop (size_binop (TRUNC_MOD_EXPR, right_offset, 
			          fold_convert (ssizetype, vectype_alignment))))
	    *misalign = ssize_int (0);
	  else
	    /* If the remainder is not zero or the right side isn't constant,
	       we can't compute  misalignment.  */
	    *misalign = NULL_TREE;
	}
      else 
	{
	  /* The left operand was successfully substituted with constant.  */	  
	  if (left_misalign)
	    /* In case of EXPR '(i * C1 + j) * C2', LEFT_MISALIGN is 
	       NULL_TREE.  */
	    *misalign  = size_binop (code, left_misalign, right_misalign);
	  else
	    *misalign = NULL_TREE; 
	}

      /* Step calculation.  */
      /* Multiply the step by the right operand.  */
      *step  = size_binop (MULT_EXPR, left_step, right_offset);
      break;
   
    case PLUS_EXPR:
    case MINUS_EXPR:
      /* Combine the recursive calculations for step and misalignment.  */
      *step = size_binop (code, left_step, right_step);
   
      if (left_misalign && right_misalign)
	*misalign  = size_binop (code, left_misalign, right_misalign);
      else
	*misalign = NULL_TREE;
    
      break;

    default:
      gcc_unreachable ();
    }

  /* Compute offset.  */
  *initial_offset = fold_convert (ssizetype, 
				  fold (build2 (code, TREE_TYPE (left_offset), 
						left_offset, 
						right_offset)));
  return true;
}


/* Function vect_determine_vectorization_factor

   Determine the vectorization factor (VF). VF is the number of data elements
   that are operated upon in parallel in a single iteration of the vectorized
   loop. For example, when vectorizing a loop that operates on 4byte elements,
   on a target with vector size (VS) 16byte, the VF is set to 4, since 4
   elements can fit in a single vector register.

   We currently support vectorization of loops in which all types operated upon
   are of the same size. Therefore this function currently sets VF according to
   the size of the types operated upon, and fails if there are multiple sizes
   in the loop.

   VF is also the factor by which the loop iterations are strip-mined, e.g.:
   original loop:
        for (i=0; i<N; i++){
          a[i] = b[i] + c[i];
        }

   vectorized loop:
        for (i=0; i<N; i+=VF){
          a[i:VF] = b[i:VF] + c[i:VF];
        }
*/

static bool
vect_determine_vectorization_factor (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  unsigned int vectorization_factor = 0;
  int i;
  tree scalar_type;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_determine_vectorization_factor ===");

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
        {
          tree stmt = bsi_stmt (si);
          unsigned int nunits;
          stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
          tree vectype;

          if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
            {
              fprintf (vect_dump, "==> examining statement: ");
              print_generic_expr (vect_dump, stmt, TDF_SLIM);
            }

          gcc_assert (stmt_info);
          /* skip stmts which do not need to be vectorized.  */
          if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
            {
              if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
                fprintf (vect_dump, "skip.");
              continue;
            }

          if (VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (stmt))))
            {
              if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                        LOOP_LOC (loop_vinfo)))
                {
                  fprintf (vect_dump, "not vectorized: vector stmt in loop:");
                  print_generic_expr (vect_dump, stmt, TDF_SLIM);
                }
              return false;
            }

          if (STMT_VINFO_DATA_REF (stmt_info))
            scalar_type = TREE_TYPE (DR_REF (STMT_VINFO_DATA_REF (stmt_info)));
          else if (TREE_CODE (stmt) == MODIFY_EXPR)
            scalar_type = TREE_TYPE (TREE_OPERAND (stmt, 0));
          else
            scalar_type = TREE_TYPE (stmt);

          if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
            {
              fprintf (vect_dump, "get vectype for scalar type:  ");
              print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
            }

          vectype = get_vectype_for_scalar_type (scalar_type);
          if (!vectype)
            {
              if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                        LOOP_LOC (loop_vinfo)))
                {
                  fprintf (vect_dump, "not vectorized: unsupported data-type ");
                  print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
                }
              return false;
            }
          if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
            {
              fprintf (vect_dump, "vectype: ");
              print_generic_expr (vect_dump, vectype, TDF_SLIM);
            }
          STMT_VINFO_VECTYPE (stmt_info) = vectype;

          nunits = TYPE_VECTOR_SUBPARTS (vectype);
          if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
            fprintf (vect_dump, "nunits = %d", nunits);

          if (vectorization_factor)
            {
              /* FORNOW: don't allow mixed units. 
                 This restriction will be relaxed in the future.  */
              if (nunits != vectorization_factor) 
                {
                  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                            LOOP_LOC (loop_vinfo)))
                    fprintf (vect_dump, "not vectorized: mixed data-types");
                  return false;
                }
            }
          else
            vectorization_factor = nunits;

#ifdef ENABLE_CHECKING
          gcc_assert (GET_MODE_SIZE (TYPE_MODE (scalar_type))
                        * vectorization_factor == UNITS_PER_SIMD_WORD);
#endif
        }
    }

  /* TODO: Analyze cost. Decide if worth while to vectorize.  */

  if (vectorization_factor <= 1)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                LOOP_LOC (loop_vinfo)))
        fprintf (vect_dump, "not vectorized: unsupported data-type");
      return false;
    }
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;

  return true;
}


/* Function vect_analyze_operations.

   Scan the loop stmts and make sure they are all vectorizable.  */

static bool
vect_analyze_operations (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  unsigned int vectorization_factor = 0;
  int i;
  bool ok;
  tree phi;
  stmt_vec_info stmt_info;
  bool need_to_vectorize = false;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_analyze_operations ===");

  gcc_assert (LOOP_VINFO_VECT_FACTOR (loop_vinfo));
  vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
        {
	  stmt_info = vinfo_for_stmt (phi);
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    {
	      fprintf (vect_dump, "examining phi: ");
	      print_generic_expr (vect_dump, phi, TDF_SLIM);
	    }

	  gcc_assert (stmt_info);

	  if (STMT_VINFO_LIVE_P (stmt_info))
	    {
	      /* FORNOW: not yet supported.  */
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
					LOOP_LOC (loop_vinfo)))
		fprintf (vect_dump, "not vectorized: value used after loop.");
	    return false;
	  }

          gcc_assert (!STMT_VINFO_RELEVANT_P (stmt_info));
        }

      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  tree stmt = bsi_stmt (si);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    {
	      fprintf (vect_dump, "==> examining statement: ");
	      print_generic_expr (vect_dump, stmt, TDF_SLIM);
	    }

	  gcc_assert (stmt_info);

	  /* skip stmts which do not need to be vectorized.
	     this is expected to include:
	     - the COND_EXPR which is the loop exit condition
	     - any LABEL_EXPRs in the loop
	     - computations that are used only for array indexing or loop
	     control  */

	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
	    {
	      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	        fprintf (vect_dump, "irrelevant.");
	      continue;
	    }

          if (STMT_VINFO_RELEVANT_P (stmt_info))
            {
              gcc_assert (!VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (stmt))));
              gcc_assert (STMT_VINFO_VECTYPE (stmt_info));

	      ok = (vectorizable_operation (stmt, NULL, NULL)
		    || vectorizable_assignment (stmt, NULL, NULL)
		    || vectorizable_load (stmt, NULL, NULL)
		    || vectorizable_store (stmt, NULL, NULL)
		    || vectorizable_condition (stmt, NULL, NULL));

	      if (!ok)
		{
		  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
					    LOOP_LOC (loop_vinfo)))
		    {
		      fprintf (vect_dump, 
			       "not vectorized: relevant stmt not supported: ");
		      print_generic_expr (vect_dump, stmt, TDF_SLIM);
		    }
		  return false;
		}	
	      need_to_vectorize = true;
            }

	  if (STMT_VINFO_LIVE_P (stmt_info))
	    {
	      ok = vectorizable_live_operation (stmt, NULL, NULL);

	      if (!ok)
		{
		  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
					    LOOP_LOC (loop_vinfo)))
		    {
		      fprintf (vect_dump, 
			       "not vectorized: live stmt not supported: ");
		      print_generic_expr (vect_dump, stmt, TDF_SLIM);
		    }
		  return false;
		}
	    }
	} /* stmts in bb */
    } /* bbs */

  /* TODO: Analyze cost. Decide if worth while to vectorize.  */

  /* All operations in the loop are either irrelevant (deal with loop
     control, or dead), or only used outside the loop and can be moved
     out of the loop (e.g. invariants, inductions).  The loop can be 
     optimized away by scalar optimizations.  We're better off not 
     touching this loop.  */
  if (!need_to_vectorize)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, 
		 "All the computation can be taken out of the loop.");
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                LOOP_LOC (loop_vinfo)))
        fprintf (vect_dump, 
		 "not vectorized: redundant loop. no profit to vectorize.");
      return false;
    }

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump,
        "vectorization_factor = %d, niters = " HOST_WIDE_INT_PRINT_DEC,
        vectorization_factor, LOOP_VINFO_INT_NITERS (loop_vinfo));

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && LOOP_VINFO_INT_NITERS (loop_vinfo) < vectorization_factor)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                 LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "not vectorized: iteration count too small.");
      return false;
    }

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      || LOOP_VINFO_INT_NITERS (loop_vinfo) % vectorization_factor != 0)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
        fprintf (vect_dump, "epilog loop required.");
      if (!vect_can_advance_ivs_p (loop_vinfo))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                     LOOP_LOC (loop_vinfo)))
            fprintf (vect_dump,
                     "not vectorized: can't create epilog loop 1.");
          return false;
        }
      if (!slpeel_can_duplicate_loop_p (loop, loop->single_exit))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                     LOOP_LOC (loop_vinfo)))
            fprintf (vect_dump,
                     "not vectorized: can't create epilog loop 2.");
          return false;
        }
    }

  return true;
}


/* Function exist_non_indexing_operands_for_use_p 

   USE is one of the uses attached to STMT. Check if USE is 
   used in STMT for anything other than indexing an array.  */

static bool
exist_non_indexing_operands_for_use_p (tree use, tree stmt)
{
  tree operand;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
 
  /* USE corresponds to some operand in STMT. If there is no data
     reference in STMT, then any operand that corresponds to USE
     is not indexing an array.  */
  if (!STMT_VINFO_DATA_REF (stmt_info))
    return true;
 
  /* STMT has a data_ref. FORNOW this means that its of one of
     the following forms:
     -1- ARRAY_REF = var
     -2- var = ARRAY_REF
     (This should have been verified in analyze_data_refs).

     'var' in the second case corresponds to a def, not a use,
     so USE cannot correspond to any operands that are not used 
     for array indexing.

     Therefore, all we need to check is if STMT falls into the
     first case, and whether var corresponds to USE.  */
 
  if (TREE_CODE (TREE_OPERAND (stmt, 0)) == SSA_NAME)
    return false;

  operand = TREE_OPERAND (stmt, 1);

  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  if (operand == use)
    return true;

  return false;
}


/* Function vect_analyze_scalar_cycles.

   Examine the cross iteration def-use cycles of scalar variables, by
   analyzing the loop (scalar) PHIs; Classify each cycle as one of the
   following: invariant, induction, reduction, unknown.
   
   Some forms of scalar cycles are not yet supported.

   Example1: reduction: (unsupported yet)

              loop1:
              for (i=0; i<N; i++)
                 sum += a[i];

   Example2: induction: (unsupported yet)

              loop2:
              for (i=0; i<N; i++)
                 a[i] = i;

   Note: the following loop *is* vectorizable:

              loop3:
              for (i=0; i<N; i++)
                 a[i] = b[i];

         even though it has a def-use cycle caused by the induction variable i:

              loop: i_2 = PHI (i_0, i_1)
                    a[i_2] = ...;
                    i_1 = i_2 + 1;
                    GOTO loop;

         because the def-use cycle in loop3 is considered "not relevant" - i.e.,
         it does not need to be vectorized because it is only used for array
         indexing (see 'mark_stmts_to_be_vectorized'). The def-use cycle in
         loop2 on the other hand is relevant (it is being written to memory).
*/

static void
vect_analyze_scalar_cycles (loop_vec_info loop_vinfo)
{
  tree phi;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block bb = loop->header;
  tree dummy;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_analyze_scalar_cycles ===");

  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree access_fn = NULL;
      tree def = PHI_RESULT (phi);
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (phi);
      tree reduc_stmt;

      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	{
          fprintf (vect_dump, "Analyze phi: ");
          print_generic_expr (vect_dump, phi, TDF_SLIM);
	}

      /* Skip virtual phi's. The data dependences that are associated with
         virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.  */

      if (!is_gimple_reg (SSA_NAME_VAR (def)))
	{
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    fprintf (vect_dump, "virtual phi. skip.");
	  continue;
	}

      STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_unknown_def_type;

      /* Analyze the evolution function.  */

      access_fn = analyze_scalar_evolution (loop, def);

      if (!access_fn)
	continue;

      if (vect_print_dump_info (REPORT_DETAILS,
				LOOP_LOC (loop_vinfo)))
        {
           fprintf (vect_dump, "Access function of PHI: ");
           print_generic_expr (vect_dump, access_fn, TDF_SLIM);
        }

      if (vect_is_simple_iv_evolution (loop->num, access_fn, &dummy, &dummy))
	{
	  if (vect_print_dump_info (REPORT_DETAILS,LOOP_LOC (loop_vinfo)))
	    fprintf (vect_dump, "Detected induction.");
	  STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_induction_def;
          continue;
	}

      /* TODO: handle invariant phis  */

      reduc_stmt = vect_is_simple_reduction (loop, phi);
      if (reduc_stmt)
        {
          if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
            fprintf (vect_dump, "Detected reduction.");
          STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_reduction_def;
          STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                        vect_reduction_def;
        }
      else
        if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
          fprintf (vect_dump, "Unknown def-use cycle pattern.");

    }

  return;
}


/* Function vect_base_addr_differ_p.

   This is the simplest data dependence test: determines whether the
   data references A and B access the same array/region.  Returns
   false when the property is not computable at compile time.
   Otherwise return true, and DIFFER_P will record the result. This
   utility will not be necessary when alias_sets_conflict_p will be
   less conservative.  */

static bool
vect_base_addr_differ_p (struct data_reference *dra,
			 struct data_reference *drb,
			 bool *differ_p)
{
  tree stmt_a = DR_STMT (dra);
  stmt_vec_info stmt_info_a = vinfo_for_stmt (stmt_a);   
  tree stmt_b = DR_STMT (drb);
  stmt_vec_info stmt_info_b = vinfo_for_stmt (stmt_b);   
  tree addr_a = STMT_VINFO_VECT_DR_BASE_ADDRESS (stmt_info_a);
  tree addr_b = STMT_VINFO_VECT_DR_BASE_ADDRESS (stmt_info_b);
  tree type_a = TREE_TYPE (addr_a);
  tree type_b = TREE_TYPE (addr_b);
  HOST_WIDE_INT alias_set_a, alias_set_b;

  gcc_assert (POINTER_TYPE_P (type_a) &&  POINTER_TYPE_P (type_b));
  
  /* Both references are ADDR_EXPR, i.e., we have the objects.  */
  if (TREE_CODE (addr_a) == ADDR_EXPR && TREE_CODE (addr_b) == ADDR_EXPR)
    return array_base_name_differ_p (dra, drb, differ_p);  

  alias_set_a = (TREE_CODE (addr_a) == ADDR_EXPR) ? 
    get_alias_set (TREE_OPERAND (addr_a, 0)) : get_alias_set (addr_a);
  alias_set_b = (TREE_CODE (addr_b) == ADDR_EXPR) ? 
    get_alias_set (TREE_OPERAND (addr_b, 0)) : get_alias_set (addr_b);

  if (!alias_sets_conflict_p (alias_set_a, alias_set_b))
    {
      *differ_p = true;
      return true;
    }
  
  /* An instruction writing through a restricted pointer is "independent" of any 
     instruction reading or writing through a different pointer, in the same 
     block/scope.  */
  else if ((TYPE_RESTRICT (type_a) && !DR_IS_READ (dra))
      || (TYPE_RESTRICT (type_b) && !DR_IS_READ (drb)))
    {
      *differ_p = true;
      return true;
    }
  return false;
}


/* Function vect_analyze_data_ref_dependence.

   Return TRUE if there (might) exist a dependence between a memory-reference
   DRA and a memory-reference DRB.  */

static bool
vect_analyze_data_ref_dependence (struct data_reference *dra,
				  struct data_reference *drb, 
				  loop_vec_info loop_vinfo)
{
  bool differ_p; 
  struct data_dependence_relation *ddr;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  int dist = 0;
  unsigned int loop_depth = 0;
  struct loop *loop_nest = loop;  
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra));
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));
  
  if (!vect_base_addr_differ_p (dra, drb, &differ_p))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo)))
        {
          fprintf (vect_dump,
                "not vectorized: can't determine dependence between: ");
          print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
          fprintf (vect_dump, " and ");
          print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
        }
      return true;
    }

  if (differ_p)
    return false;

  ddr = initialize_data_dependence_relation (dra, drb);
  compute_affine_dependence (ddr);

  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return false;

  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                LOOP_LOC (loop_vinfo)))
        {
          fprintf (vect_dump, 
                   "not vectorized: can't determine dependence between "); 
          print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
          fprintf (vect_dump, " and ");
          print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
        }
      return true;
    }

  /* Find loop depth.  */
  while (loop_nest)
    {
      if (loop_nest->outer && loop_nest->outer->outer)
	{
	  loop_nest = loop_nest->outer;
	  loop_depth++;
	}
      else
	break;
    }

  /* Compute distance vector.  */
  compute_subscript_distance (ddr);
  build_classic_dist_vector (ddr, vect_loops_num, loop_nest->depth);

  if (!DDR_DIST_VECT (ddr))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo)))
	{
	  fprintf (vect_dump, "not vectorized: bad dist vector for ");
	  print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	  fprintf (vect_dump, " and ");
	  print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	}      
      return true;
    }

  dist = DDR_DIST_VECT (ddr)[loop_depth];

  /* Same loop iteration.  */
  if (dist % vectorization_factor == 0)
    {
      /* Two references with distance zero have the same alignment.  */
      VEC_safe_push (dr_p, heap, STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_a), drb);
      VEC_safe_push (dr_p, heap, STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_b), dra);
      if (vect_print_dump_info (REPORT_ALIGNMENT, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "accesses have the same alignment.");
      return false;
    }

  if (dist >= vectorization_factor)
    /* Dependence distance does not create dependence, as far as vectorization
       is concerned, in this case.  */
    return false;
    
  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
			    LOOP_LOC (loop_vinfo)))
    {
      fprintf (vect_dump,
	"not vectorized: possible dependence between data-refs ");
      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
      fprintf (vect_dump, " and ");
      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
    }

  return true;
}


/* Function vect_analyze_data_ref_dependences.

   Examine all the data references in the loop, and make sure there do not
   exist any data dependences between them.  */

static bool
vect_analyze_data_ref_dependences (loop_vec_info loop_vinfo)
{
  unsigned int i, j;
  varray_type loop_write_refs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  varray_type loop_read_refs = LOOP_VINFO_DATAREF_READS (loop_vinfo);

  /* Examine store-store (output) dependences.  */

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_analyze_dependences ===");

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "compare all store-store pairs.");

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_refs); i++)
    {
      for (j = i + 1; j < VARRAY_ACTIVE_SIZE (loop_write_refs); j++)
	{
	  struct data_reference *dra =
	    VARRAY_GENERIC_PTR (loop_write_refs, i);
	  struct data_reference *drb =
	    VARRAY_GENERIC_PTR (loop_write_refs, j);
	  if (vect_analyze_data_ref_dependence (dra, drb, loop_vinfo))
	    return false;
	}
    }

  /* Examine load-store (true/anti) dependences.  */

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "compare all load-store pairs.");

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_refs); i++)
    {
      for (j = 0; j < VARRAY_ACTIVE_SIZE (loop_write_refs); j++)
	{
	  struct data_reference *dra = VARRAY_GENERIC_PTR (loop_read_refs, i);
	  struct data_reference *drb =
	    VARRAY_GENERIC_PTR (loop_write_refs, j);
	  if (vect_analyze_data_ref_dependence (dra, drb, loop_vinfo))
	    return false;
	}
    }

  return true;
}


/* Function vect_compute_data_ref_alignment

   Compute the misalignment of the data reference DR.

   Output:
   1. If during the misalignment computation it is found that the data reference
      cannot be vectorized then false is returned.
   2. DR_MISALIGNMENT (DR) is defined.

   FOR NOW: No analysis is actually performed. Misalignment is calculated
   only for trivial cases. TODO.  */

static bool
vect_compute_data_ref_alignment (struct data_reference *dr)
{
  tree stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);  
  tree ref = DR_REF (dr);
  tree vectype;
  tree base, alignment;
  bool base_aligned_p;
  tree misalign;
   
  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "vect_compute_data_ref_alignment:");

  /* Initialize misalignment to unknown.  */
  DR_MISALIGNMENT (dr) = -1;

  misalign = STMT_VINFO_VECT_MISALIGNMENT (stmt_info);
  base_aligned_p = STMT_VINFO_VECT_BASE_ALIGNED_P (stmt_info);
  base = build_fold_indirect_ref (STMT_VINFO_VECT_DR_BASE_ADDRESS (stmt_info));
  vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (!misalign)
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC)) 
	{
	  fprintf (vect_dump, "Unknown alignment for access: ");
	  print_generic_expr (vect_dump, base, TDF_SLIM);
	}
      return true;
    }

  if (!base_aligned_p) 
    {
      if (!vect_can_force_dr_alignment_p (base, TYPE_ALIGN (vectype)))
	{
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    {
	      fprintf (vect_dump, "can't force alignment of ref: ");
	      print_generic_expr (vect_dump, ref, TDF_SLIM);
	    }
	  return true;
	}
      
      /* Force the alignment of the decl.
	 NOTE: This is the only change to the code we make during
	 the analysis phase, before deciding to vectorize the loop.  */
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	fprintf (vect_dump, "force alignment");
      DECL_ALIGN (base) = TYPE_ALIGN (vectype);
      DECL_USER_ALIGN (base) = 1;
    }

  /* At this point we assume that the base is aligned.  */
  gcc_assert (base_aligned_p 
	      || (TREE_CODE (base) == VAR_DECL 
		  && DECL_ALIGN (base) >= TYPE_ALIGN (vectype)));

  /* Alignment required, in bytes:  */
  alignment = ssize_int (TYPE_ALIGN (vectype)/BITS_PER_UNIT);

  /* Modulo alignment.  */
  misalign = size_binop (TRUNC_MOD_EXPR, misalign, alignment);
  if (tree_int_cst_sgn (misalign) < 0)
    {
      /* Negative misalignment value.  */
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	fprintf (vect_dump, "unexpected misalign value");
      return false;
    }

  DR_MISALIGNMENT (dr) = tree_low_cst (misalign, 1);

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "misalign = %d bytes", DR_MISALIGNMENT (dr));

  return true;
}


/* Function vect_compute_data_refs_alignment

   Compute the misalignment of data references in the loop.
   This pass may take place at function granularity instead of at loop
   granularity.

   FOR NOW: No analysis is actually performed. Misalignment is calculated
   only for trivial cases. TODO.  */

static bool
vect_compute_data_refs_alignment (loop_vec_info loop_vinfo)
{
  varray_type loop_write_datarefs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  varray_type loop_read_datarefs = LOOP_VINFO_DATAREF_READS (loop_vinfo);
  unsigned int i;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_write_datarefs, i);
      if (!vect_compute_data_ref_alignment (dr))
	return false;
    }

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_read_datarefs, i);
      if (!vect_compute_data_ref_alignment (dr))
	return false;
    }

  return true;
}


/* Function vect_enhance_data_refs_alignment

   This pass will use loop versioning and loop peeling in order to enhance
   the alignment of data references in the loop.

   FOR NOW: we assume that whatever versioning/peeling takes place, only the
   original loop is to be vectorized; Any other loops that are created by
   the transformations performed in this pass - are not supposed to be
   vectorized. This restriction will be relaxed.  */

static void
vect_enhance_data_refs_alignment (loop_vec_info loop_vinfo)
{
  varray_type loop_read_datarefs = LOOP_VINFO_DATAREF_READS (loop_vinfo);
  varray_type loop_write_datarefs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  varray_type datarefs;
  VEC(dr_p,heap) *same_align_drs;
  struct data_reference *dr0 = NULL;
  struct data_reference *dr;
  unsigned int i, j;

  /*
     This pass will require a cost model to guide it whether to apply peeling 
     or versioning or a combination of the two. For example, the scheme that
     intel uses when given a loop with several memory accesses, is as follows:
     choose one memory access ('p') which alignment you want to force by doing 
     peeling. Then, either (1) generate a loop in which 'p' is aligned and all 
     other accesses are not necessarily aligned, or (2) use loop versioning to 
     generate one loop in which all accesses are aligned, and another loop in 
     which only 'p' is necessarily aligned. 

     ("Automatic Intra-Register Vectorization for the Intel Architecture",
      Aart J.C. Bik, Milind Girkar, Paul M. Grey and Ximmin Tian, International
      Journal of Parallel Programming, Vol. 30, No. 2, April 2002.)	

     Devising a cost model is the most critical aspect of this work. It will 
     guide us on which access to peel for, whether to use loop versioning, how 
     many versions to create, etc. The cost model will probably consist of 
     generic considerations as well as target specific considerations (on 
     powerpc for example, misaligned stores are more painful than misaligned 
     loads). 

     Here is the general steps involved in alignment enhancements:
    
     -- original loop, before alignment analysis:
	for (i=0; i<N; i++){
	  x = q[i];			# DR_MISALIGNMENT(q) = unknown
	  p[i] = y;			# DR_MISALIGNMENT(p) = unknown
	}

     -- After vect_compute_data_refs_alignment:
	for (i=0; i<N; i++){
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = unknown
	}

     -- Possibility 1: we do loop versioning:
     if (p is aligned) {
	for (i=0; i<N; i++){	# loop 1A
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = 0
	}
     } 
     else {
	for (i=0; i<N; i++){	# loop 1B
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = unaligned
	}
     }
   
     -- Possibility 2: we do loop peeling:
     for (i = 0; i < 3; i++){	# (scalar loop, not to be vectorized).
	x = q[i];
	p[i] = y;
     }
     for (i = 3; i < N; i++){	# loop 2A
	x = q[i];			# DR_MISALIGNMENT(q) = 0
	p[i] = y;			# DR_MISALIGNMENT(p) = unknown
     }

     -- Possibility 3: combination of loop peeling and versioning:
     for (i = 0; i < 3; i++){	# (scalar loop, not to be vectorized).
	x = q[i];
	p[i] = y;
     }
     if (p is aligned) {
	for (i = 3; i<N; i++){  # loop 3A
	  x = q[i];			# DR_MISALIGNMENT(q) = 0
	  p[i] = y;			# DR_MISALIGNMENT(p) = 0
	}
     } 
     else {
	for (i = 3; i<N; i++){	# loop 3B
	  x = q[i];			# DR_MISALIGNMENT(q) = 0
	  p[i] = y;			# DR_MISALIGNMENT(p) = unaligned
	}
     }

     These loops are later passed to loop_transform to be vectorized. The 
     vectorizer will use the alignment information to guide the transformation 
     (whether to generate regular loads/stores, or with special handling for 
     misalignment). 
   */

  /* (1) Peeling to force alignment.  */

  /* (1.1) Decide whether to perform peeling, and how many iterations to peel:
     Considerations:
     + How many accesses will become aligned due to the peeling
     - How many accesses will become unaligned due to the peeling,
       and the cost of misaligned accesses.
     - The cost of peeling (the extra runtime checks, the increase 
       in code size).

     The scheme we use FORNOW: peel to force the alignment of the first
     misaligned store in the loop.
     Rationale: misaligned stores are not yet supported.

     TODO: Use a better cost model.  */

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_datarefs); i++)
    {
      dr0 = VARRAY_GENERIC_PTR (loop_write_datarefs, i);
      if (!aligned_access_p (dr0))
	{
	  LOOP_VINFO_UNALIGNED_DR (loop_vinfo) = dr0;
	  LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo) = DR_MISALIGNMENT (dr0);
	  break;
	}
    }

  /* (1.2) Update the alignment info according to the peeling factor.
	   If the misalignment of the DR we peel for is M, then the
	   peeling factor is VF - M, and the misalignment of each access DR_i
	   in the loop is DR_MISALIGNMENT (DR_i) + VF - M.
	   If the misalignment of the DR we peel for is unknown, then the 
	   misalignment of each access DR_i in the loop is also unknown.

           TODO: - consider accesses that are known to have the same
                   alignment, even if that alignment is unknown.  */

  if (LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo))
    {
      int mis;
      int npeel = 0;

      if (known_alignment_for_access_p (dr0))
	{
	  /* Since it's known at compile time, compute the number of iterations
	     in the peeled loop (the peeling factor) for use in updating
	     DR_MISALIGNMENT values.  The peeling factor is the vectorization
	     factor minus the misalignment as an element count.  */
	  mis = DR_MISALIGNMENT (dr0);
	  mis /= GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr0))));
	  npeel = LOOP_VINFO_VECT_FACTOR (loop_vinfo) - mis;
	}

      datarefs = loop_write_datarefs;
      for (j = 0; j < 2; j++)
	{
	  for (i = 0; i < VARRAY_ACTIVE_SIZE (datarefs); i++)
	    {
	      struct data_reference *dr = VARRAY_GENERIC_PTR (datarefs, i);

	      if (dr == dr0)
		continue;
	      if (known_alignment_for_access_p (dr)
		  && DR_MISALIGNMENT (dr) == DR_MISALIGNMENT (dr0))
		DR_MISALIGNMENT (dr) = 0;
	      else if (known_alignment_for_access_p (dr)
		       && known_alignment_for_access_p (dr0))
		{
		  int drsize = 
			GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr))));

		  DR_MISALIGNMENT (dr) += npeel * drsize;
		  DR_MISALIGNMENT (dr) %= UNITS_PER_SIMD_WORD;
		}
	      else
		DR_MISALIGNMENT (dr) = -1;
	    }
	  datarefs = loop_read_datarefs;
	}

      same_align_drs = 
	STMT_VINFO_SAME_ALIGN_REFS (vinfo_for_stmt (DR_STMT (dr0)));
      for (i = 0; VEC_iterate (dr_p, same_align_drs, i, dr); i++)
        {
          DR_MISALIGNMENT (dr) = 0;
        }

      DR_MISALIGNMENT (dr0) = 0;
    }
}


/* Function vect_analyze_data_refs_alignment

   Analyze the alignment of the data-references in the loop.
   FOR NOW: Until support for misaligned accesses is in place, only if all
   accesses are aligned can the loop be vectorized. This restriction will be 
   relaxed.  */ 

static bool
vect_analyze_data_refs_alignment (loop_vec_info loop_vinfo)
{
  varray_type loop_read_datarefs = LOOP_VINFO_DATAREF_READS (loop_vinfo);
  varray_type loop_write_datarefs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  enum dr_alignment_support supportable_dr_alignment;
  unsigned int i;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_analyze_data_refs_alignment ===");


  /* This pass may take place at function granularity instead of at loop
     granularity.  */

  if (!vect_compute_data_refs_alignment (loop_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, 
		 "not vectorized: can't calculate alignment for data ref.");
      return false;
    }


  /* This pass will decide on using loop versioning and/or loop peeling in 
     order to enhance the alignment of data references in the loop.  */

  vect_enhance_data_refs_alignment (loop_vinfo);


  /* Finally, check that all the data references in the loop can be
     handled with respect to their alignment.  */

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_read_datarefs, i);
      supportable_dr_alignment = vect_supportable_dr_alignment (dr);
      if (!supportable_dr_alignment)
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				    LOOP_LOC (loop_vinfo)))
	    fprintf (vect_dump, "not vectorized: unsupported unaligned load.");
	  return false;
	}
      if (supportable_dr_alignment != dr_aligned 
	  && (vect_print_dump_info (REPORT_ALIGNMENT, LOOP_LOC (loop_vinfo))))
	fprintf (vect_dump, "Vectorizing an unaligned access.");
    }
  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_write_datarefs, i);
      supportable_dr_alignment = vect_supportable_dr_alignment (dr);
      if (!supportable_dr_alignment)
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				    LOOP_LOC (loop_vinfo)))
	    fprintf (vect_dump, "not vectorized: unsupported unaligned store.");
	  return false;
	}
      if (supportable_dr_alignment != dr_aligned 
	  && (vect_print_dump_info (REPORT_ALIGNMENT, LOOP_LOC (loop_vinfo))))
	fprintf (vect_dump, "Vectorizing an unaligned access.");
    }
  if (LOOP_VINFO_UNALIGNED_DR (loop_vinfo)
      && vect_print_dump_info (REPORT_ALIGNMENT, LOOP_LOC (loop_vinfo)))
    fprintf (vect_dump, "Alignment of access forced using peeling.");

  return true;
}


/* Function vect_analyze_data_ref_access.

   Analyze the access pattern of the data-reference DR. For now, a data access
   has to consecutive to be considered vectorizable.  */

static bool
vect_analyze_data_ref_access (struct data_reference *dr)
{
  tree stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt); 
  tree step = STMT_VINFO_VECT_STEP (stmt_info);
  tree scalar_type = TREE_TYPE (DR_REF (dr));

  if (!step || tree_int_cst_compare (step, TYPE_SIZE_UNIT (scalar_type)))
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	fprintf (vect_dump, "not consecutive access");
      return false;
    }
  return true;
}


/* Function vect_analyze_data_ref_accesses.

   Analyze the access pattern of all the data references in the loop.

   FORNOW: the only access pattern that is considered vectorizable is a
	   simple step 1 (consecutive) access.

   FORNOW: handle only arrays and pointer accesses.  */

static bool
vect_analyze_data_ref_accesses (loop_vec_info loop_vinfo)
{
  unsigned int i;
  varray_type loop_write_datarefs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  varray_type loop_read_datarefs = LOOP_VINFO_DATAREF_READS (loop_vinfo);

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_analyze_data_ref_accesses ===");

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_write_datarefs, i);
      bool ok = vect_analyze_data_ref_access (dr);
      if (!ok)
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                                      LOOP_LOC (loop_vinfo)))
	    fprintf (vect_dump, "not vectorized: complicated access pattern.");
	  return false;
	}
    }

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_read_datarefs, i);
      bool ok = vect_analyze_data_ref_access (dr);
      if (!ok)
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				    LOOP_LOC (loop_vinfo)))
	    fprintf (vect_dump, "not vectorized: complicated access pattern.");
	  return false;
	}
    }

  return true;
}


/* Function vect_analyze_pointer_ref_access.

   Input:
   STMT - a stmt that contains a data-ref.
   MEMREF - a data-ref in STMT, which is an INDIRECT_REF.
   ACCESS_FN - the access function of MEMREF.

   Output:
   If the data-ref access is vectorizable, return a data_reference structure
   that represents it (DR). Otherwise - return NULL.  
   STEP - the stride of MEMREF in the loop.
   INIT - the initial condition of MEMREF in the loop.
*/

static struct data_reference *
vect_analyze_pointer_ref_access (tree memref, tree stmt, bool is_read, 
				 tree access_fn, tree *ptr_init, tree *ptr_step)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree step, init;	
  tree reftype, innertype;
  tree indx_access_fn; 
  int loopnum = loop->num;
  struct data_reference *dr;

  if (!vect_is_simple_iv_evolution (loopnum, access_fn, &init, &step))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS, 
				LOOP_LOC (loop_vinfo))) 
	fprintf (vect_dump, "not vectorized: pointer access is not simple.");	
      return NULL;
    }

  STRIP_NOPS (init);

  if (!expr_invariant_in_loop_p (loop, init))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo))) 
	fprintf (vect_dump, 
		 "not vectorized: initial condition is not loop invariant.");	
      return NULL;
    }

  if (TREE_CODE (step) != INTEGER_CST)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo))) 
	fprintf (vect_dump, 
		"not vectorized: non constant step for pointer access.");	
      return NULL;
    }

  reftype = TREE_TYPE (TREE_OPERAND (memref, 0));
  if (!POINTER_TYPE_P (reftype)) 
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "not vectorized: unexpected pointer access form.");	
      return NULL;
    }

  if (!POINTER_TYPE_P (TREE_TYPE (init))) 
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo))) 
	fprintf (vect_dump, "not vectorized: unexpected pointer access form.");
      return NULL;
    }

  *ptr_step = fold_convert (ssizetype, step);
  innertype = TREE_TYPE (reftype);
  if (!COMPLETE_TYPE_P (innertype))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
                              LOOP_LOC (loop_vinfo)))
      fprintf (vect_dump, "not vectorized: pointer to incomplete type.");
      return NULL;
    }
   
  /* Check that STEP is a multiple of type size.  */
  if (!integer_zerop (size_binop (TRUNC_MOD_EXPR, *ptr_step, 
 		        fold_convert (ssizetype, TYPE_SIZE_UNIT (innertype)))))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				LOOP_LOC (loop_vinfo))) 
	fprintf (vect_dump, "not vectorized: non consecutive access.");	
      return NULL;
    }
   
  indx_access_fn = 
	build_polynomial_chrec (loopnum, integer_zero_node, integer_one_node);
  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    {
      fprintf (vect_dump, "Access function of ptr indx: ");
      print_generic_expr (vect_dump, indx_access_fn, TDF_SLIM);
    }
  dr = init_data_ref (stmt, memref, NULL_TREE, indx_access_fn, is_read);
  *ptr_init = init;
  return dr;
}


/* Function vect_address_analysis

   Return the BASE of the address expression EXPR.
   Also compute the INITIAL_OFFSET from BASE, MISALIGN and STEP.

   Input:
   EXPR - the address expression that is being analyzed
   STMT - the statement that contains EXPR or its original memory reference
   IS_READ - TRUE if STMT reads from EXPR, FALSE if writes to EXPR
   VECTYPE - the type that defines the alignment (i.e, we compute
             alignment relative to TYPE_ALIGN(VECTYPE))
   DR - data_reference struct for the original memory reference

   Output:
   BASE (returned value) - the base of the data reference EXPR.
   INITIAL_OFFSET - initial offset of EXPR from BASE (an expression)
   MISALIGN - offset of EXPR from BASE in bytes (a constant) or NULL_TREE if the
              computation is impossible
   STEP - evolution of EXPR in the loop
   BASE_ALIGNED - indicates if BASE is aligned
 
   If something unexpected is encountered (an unsupported form of data-ref),
   then NULL_TREE is returned.  
 */

static tree
vect_address_analysis (tree expr, tree stmt, bool is_read, tree vectype, 
		       struct data_reference *dr, tree *offset, tree *misalign,
		       tree *step, bool *base_aligned)
{
  tree oprnd0, oprnd1, base_address, offset_expr, base_addr0, base_addr1;
  tree address_offset = ssize_int (0), address_misalign = ssize_int (0);
  tree dummy;
  struct ptr_info_def *dummy1;
  subvar_t dummy2;

  switch (TREE_CODE (expr))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      /* EXPR is of form {base +/- offset} (or {offset +/- base}).  */
      oprnd0 = TREE_OPERAND (expr, 0);
      oprnd1 = TREE_OPERAND (expr, 1);

      STRIP_NOPS (oprnd0);
      STRIP_NOPS (oprnd1);
      
      /* Recursively try to find the base of the address contained in EXPR.
	 For offset, the returned base will be NULL.  */
      base_addr0 = vect_address_analysis (oprnd0, stmt, is_read, vectype, dr, 
				     &address_offset, &address_misalign, step, 
				     base_aligned);

      base_addr1 = vect_address_analysis (oprnd1, stmt, is_read, vectype, dr, 
				     &address_offset, &address_misalign, step, 
				     base_aligned);

      /* We support cases where only one of the operands contains an 
	 address.  */
      if ((base_addr0 && base_addr1) || (!base_addr0 && !base_addr1))
	return NULL_TREE;

      /* To revert STRIP_NOPS.  */
      oprnd0 = TREE_OPERAND (expr, 0);
      oprnd1 = TREE_OPERAND (expr, 1);
      
      offset_expr = base_addr0 ? 
	fold_convert (ssizetype, oprnd1) : fold_convert (ssizetype, oprnd0);

      /* EXPR is of form {base +/- offset} (or {offset +/- base}). If offset is 
	 a number, we can add it to the misalignment value calculated for base,
	 otherwise, misalignment is NULL.  */
      if (TREE_CODE (offset_expr) == INTEGER_CST && address_misalign)
	*misalign = size_binop (TREE_CODE (expr), address_misalign, 
				offset_expr);
      else
	*misalign = NULL_TREE;

      /* Combine offset (from EXPR {base + offset}) with the offset calculated
	 for base.  */
      *offset = size_binop (TREE_CODE (expr), address_offset, offset_expr);
      return base_addr0 ? base_addr0 : base_addr1;

    case ADDR_EXPR:
      base_address = vect_object_analysis (TREE_OPERAND (expr, 0), stmt,
					   is_read, vectype, &dr, offset, 
					   misalign, step, base_aligned, 
					   &dummy, &dummy1, &dummy2);
      return base_address;

    case SSA_NAME:
      if (!POINTER_TYPE_P (TREE_TYPE (expr)))
	return NULL_TREE;

      if (TYPE_ALIGN (TREE_TYPE (TREE_TYPE (expr))) < TYPE_ALIGN (vectype)) 
	{
	  if (vect_get_ptr_offset (expr, vectype, misalign))
	    *base_aligned = true;	  
	  else
	    *base_aligned = false;
	}
      else
	{	  
	  *base_aligned = true;
	  *misalign = ssize_int (0);
	}
      *offset = ssize_int (0);
      *step = ssize_int (0);
      return expr;
      
    default:
      return NULL_TREE;
    }
}


/* Function vect_object_analysis

   Return the BASE of the data reference MEMREF.
   Also compute the INITIAL_OFFSET from BASE, MISALIGN and STEP.
   E.g., for EXPR a.b[i] + 4B, BASE is a, and OFFSET is the overall offset  
   'a.b[i] + 4B' from a (can be an expression), MISALIGN is an OFFSET 
   instantiated with initial_conditions of access_functions of variables, 
   modulo alignment, and STEP is the evolution of the DR_REF in this loop.

   Function get_inner_reference is used for the above in case of ARRAY_REF and
   COMPONENT_REF.

   The structure of the function is as follows:
   Part 1:
   Case 1. For handled_component_p refs 
          1.1 call get_inner_reference
            1.1.1 analyze offset expr received from get_inner_reference
	  1.2. build data-reference structure for MEMREF
        (fall through with BASE)
   Case 2. For declarations 
          2.1 check alignment
          2.2 update DR_BASE_NAME if necessary for alias
   Case 3. For INDIRECT_REFs 
          3.1 get the access function
	  3.2 analyze evolution of MEMREF
	  3.3 set data-reference structure for MEMREF
          3.4 call vect_address_analysis to analyze INIT of the access function

   Part 2:
   Combine the results of object and address analysis to calculate 
   INITIAL_OFFSET, STEP and misalignment info.   

   Input:
   MEMREF - the memory reference that is being analyzed
   STMT - the statement that contains MEMREF
   IS_READ - TRUE if STMT reads from MEMREF, FALSE if writes to MEMREF
   VECTYPE - the type that defines the alignment (i.e, we compute
             alignment relative to TYPE_ALIGN(VECTYPE))
   
   Output:
   BASE_ADDRESS (returned value) - the base address of the data reference MEMREF
                                   E.g, if MEMREF is a.b[k].c[i][j] the returned
			           base is &a.
   DR - data_reference struct for MEMREF
   INITIAL_OFFSET - initial offset of MEMREF from BASE (an expression)
   MISALIGN - offset of MEMREF from BASE in bytes (a constant) or NULL_TREE if 
              the computation is impossible
   STEP - evolution of the DR_REF in the loop
   BASE_ALIGNED - indicates if BASE is aligned
   MEMTAG - memory tag for aliasing purposes
   PTR_INFO - NULL or points-to aliasing info from a pointer SSA_NAME
   SUBVAR - Sub-variables of the variable
 
   If something unexpected is encountered (an unsupported form of data-ref),
   then NULL_TREE is returned.  */

static tree
vect_object_analysis (tree memref, tree stmt, bool is_read,
		      tree vectype, struct data_reference **dr,
		      tree *offset, tree *misalign, tree *step,
		      bool *base_aligned, tree *memtag,
		      struct ptr_info_def **ptr_info, subvar_t *subvars)
{
  tree base = NULL_TREE, base_address = NULL_TREE;
  tree object_offset = ssize_int (0), object_misalign = ssize_int (0);
  tree object_step = ssize_int (0), address_step = ssize_int (0);
  bool object_base_aligned = true, address_base_aligned = true;
  tree address_offset = ssize_int (0), address_misalign = ssize_int (0);
  HOST_WIDE_INT pbitsize, pbitpos;
  tree poffset, bit_pos_in_bytes;
  enum machine_mode pmode;
  int punsignedp, pvolatilep;
  tree ptr_step = ssize_int (0), ptr_init = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct data_reference *ptr_dr = NULL;
  tree access_fn, evolution_part, address_to_analyze;

  *ptr_info = NULL;
   
  /* Part 1: */
  /* Case 1. handled_component_p refs.  */
  if (handled_component_p (memref))
    {
      /* 1.1 call get_inner_reference.  */
      /* Find the base and the offset from it.  */
      base = get_inner_reference (memref, &pbitsize, &pbitpos, &poffset,
				  &pmode, &punsignedp, &pvolatilep, false);
      if (!base)
	return NULL_TREE;

      /* 1.1.1 analyze offset expr received from get_inner_reference.  */
      if (poffset 
	  && !vect_analyze_offset_expr (poffset, loop, TYPE_SIZE_UNIT (vectype), 
				&object_offset, &object_misalign, &object_step))
	{
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    {
	      fprintf (vect_dump, "failed to compute offset or step for ");
	      print_generic_expr (vect_dump, memref, TDF_SLIM);
	    }
	  return NULL_TREE;
	}

      /* Add bit position to OFFSET and MISALIGN.  */

      bit_pos_in_bytes = ssize_int (pbitpos/BITS_PER_UNIT);
      /* Check that there is no remainder in bits.  */
      if (pbitpos%BITS_PER_UNIT)
	{
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    fprintf (vect_dump, "bit offset alignment.");
	  return NULL_TREE;
	}
      object_offset = size_binop (PLUS_EXPR, bit_pos_in_bytes, object_offset);     
      if (object_misalign) 
	object_misalign = size_binop (PLUS_EXPR, object_misalign, 
				      bit_pos_in_bytes); 

      /* Create data-reference for MEMREF. TODO: handle COMPONENT_REFs.  */
      if (!(*dr))
	{ 
	  if (TREE_CODE (memref) == ARRAY_REF)
	    *dr = analyze_array (stmt, memref, is_read);
	  else
	    /* FORNOW.  */
	    return NULL_TREE;
	}
      memref = base; /* To continue analysis of BASE.  */
      /* fall through  */
    }
  
  /*  Part 1: Case 2. Declarations.  */ 
  if (DECL_P (memref))
    {
      /* We expect to get a decl only if we already have a DR.  */
      if (!(*dr))
	{
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    {
	      fprintf (vect_dump, "unhandled decl ");
	      print_generic_expr (vect_dump, memref, TDF_SLIM);
	    }
	  return NULL_TREE;
	}

      /* 2.1 check the alignment.  */
      if (DECL_ALIGN (memref) >= TYPE_ALIGN (vectype))
	object_base_aligned = true;
      else
	object_base_aligned = false;

      /* 2.2 update DR_BASE_NAME if necessary.  */
      if (!DR_BASE_NAME ((*dr)))
	/* For alias analysis.  In case the analysis of INDIRECT_REF brought 
	   us to object.  */
	DR_BASE_NAME ((*dr)) = memref;

      if (SSA_VAR_P (memref) && var_can_have_subvars (memref))	
	*subvars = get_subvars_for_var (memref);
      base_address = build_fold_addr_expr (memref);
      *memtag = memref;
    }

  /* Part 1:  Case 3. INDIRECT_REFs.  */
  else if (TREE_CODE (memref) == INDIRECT_REF)
    {
      tree ptr_ref = TREE_OPERAND (memref, 0);
      if (TREE_CODE (ptr_ref) == SSA_NAME)
        *ptr_info = SSA_NAME_PTR_INFO (ptr_ref);

      /* 3.1 get the access function.  */
      access_fn = analyze_scalar_evolution (loop, ptr_ref);
      if (!access_fn)
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				    LOOP_LOC (loop_vinfo)))
	    fprintf (vect_dump, "not vectorized: complicated pointer access.");	
	  return NULL_TREE;
	}
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	{
	  fprintf (vect_dump, "Access function of ptr: ");
	  print_generic_expr (vect_dump, access_fn, TDF_SLIM);
	}

      /* 3.2 analyze evolution of MEMREF.  */
      evolution_part = evolution_part_in_loop_num (access_fn, loop->num);
      if (evolution_part)
	{
	  ptr_dr = vect_analyze_pointer_ref_access (memref, stmt, is_read, 
				         access_fn, &ptr_init, &ptr_step);
	  if (!(ptr_dr))
	    return NULL_TREE; 
	  
	  object_step = size_binop (PLUS_EXPR, object_step, ptr_step);
	  address_to_analyze = ptr_init;
	}
      else
	{
	  if (!(*dr))
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
					LOOP_LOC (loop_vinfo))) 
		fprintf (vect_dump, "not vectorized: ptr is loop invariant.");	
	      return NULL_TREE;
	    }
	  /* Since there exists DR for MEMREF, we are analyzing the init of 
	     the access function, which not necessary has evolution in the 
	     loop.  */
	  address_to_analyze = initial_condition_in_loop_num (access_fn,
                                                              loop->num);
	}
      
      /* 3.3 set data-reference structure for MEMREF.  */
      *dr = (*dr) ? *dr : ptr_dr;

      /* 3.4 call vect_address_analysis to analyze INIT of the access 
	 function.  */
      base_address = vect_address_analysis (address_to_analyze, stmt, is_read, 
			       vectype, *dr, &address_offset, &address_misalign, 
			       &address_step, &address_base_aligned);
      if (!base_address)
	return NULL_TREE;

      switch (TREE_CODE (base_address))
	{
	case SSA_NAME:
	  *memtag = get_var_ann (SSA_NAME_VAR (base_address))->type_mem_tag;
	  if (!(*memtag) && TREE_CODE (TREE_OPERAND (memref, 0)) == SSA_NAME)
	    *memtag = get_var_ann (
		      SSA_NAME_VAR (TREE_OPERAND (memref, 0)))->type_mem_tag;
	  break;
	case ADDR_EXPR:
	  *memtag = TREE_OPERAND (base_address, 0);
	  break;
	default:
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
				    LOOP_LOC (loop_vinfo)))
	    {
	      fprintf (vect_dump, "not vectorized: no memtag ref: "); 
	      print_generic_expr (vect_dump, memref, TDF_SLIM);
	    }
	  return NULL_TREE;
	}
    }
    	    
  if (!base_address)
    /* MEMREF cannot be analyzed.  */
    return NULL_TREE;

  if (SSA_VAR_P (*memtag) && var_can_have_subvars (*memtag))
    *subvars = get_subvars_for_var (*memtag);

  /* Part 2: Combine the results of object and address analysis to calculate 
     INITIAL_OFFSET, STEP and misalignment info.  */
  *offset = size_binop (PLUS_EXPR, object_offset, address_offset);
  if (object_misalign && address_misalign)
    *misalign = size_binop (PLUS_EXPR, object_misalign, address_misalign);
  else
    *misalign = NULL_TREE;
  *step = size_binop (PLUS_EXPR, object_step, address_step); 
  *base_aligned = object_base_aligned && address_base_aligned;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    {
      fprintf (vect_dump, "Results of object analysis for: ");
      print_generic_expr (vect_dump, memref, TDF_SLIM);
      fprintf (vect_dump, "\n\tbase_address: ");
      print_generic_expr (vect_dump, base_address, TDF_SLIM);
      fprintf (vect_dump, "\n\toffset: ");
      print_generic_expr (vect_dump, *offset, TDF_SLIM);
      fprintf (vect_dump, "\n\tstep: ");
      print_generic_expr (vect_dump, *step, TDF_SLIM);
      fprintf (vect_dump, "\n\tbase aligned %d\n\tmisalign: ", *base_aligned);
      print_generic_expr (vect_dump, *misalign, TDF_SLIM);
    }
  return base_address;
}


/* Function vect_analyze_data_refs.

   Find all the data references in the loop.

   The general structure of the analysis of data refs in the vectorizer is as 
   follows:
   1- vect_analyze_data_refs(loop): 
      Find and analyze all data-refs in the loop:
          foreach ref
	     base_address = vect_object_analysis(ref)
      1.1- vect_object_analysis(ref): 
           Analyze ref, and build a DR (data_reference struct) for it;
           compute base, initial_offset, step and alignment. 
           Call get_inner_reference for refs handled in this function.
           Call vect_addr_analysis(addr) to analyze pointer type expressions.
      Set ref_stmt.base, ref_stmt.initial_offset, ref_stmt.alignment,  
      ref_stmt.memtag, ref_stmt.ptr_info and ref_stmt.step accordingly. 
   2- vect_analyze_dependences(): apply dependence testing using ref_stmt.DR
   3- vect_analyze_drs_alignment(): check that ref_stmt.alignment is ok.
   4- vect_analyze_drs_access(): check that ref_stmt.step is ok.

   FORNOW: Handle aligned INDIRECT_REFs and ARRAY_REFs 
	   which base is really an array (not a pointer) and which alignment 
	   can be forced. This restriction will be relaxed.  */

static bool
vect_analyze_data_refs (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  int j;
  struct data_reference *dr;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_analyze_data_refs ===");

  for (j = 0; j < nbbs; j++)
    {
      basic_block bb = bbs[j];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  bool is_read = false;
	  tree stmt = bsi_stmt (si);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  varray_type *datarefs = NULL;
	  tree memref = NULL;
	  tree scalar_type, vectype;	  
	  tree base, offset, misalign, step, tag;
	  struct ptr_info_def *ptr_info;
	  bool base_aligned;
	  subvar_t subvars = NULL;
	  bool no_vuse, no_vmaymust;

	  /* Assumption: there exists a data-ref in stmt, if and only if 
             it has vuses/vdefs.  */

	  no_vuse = ZERO_SSA_OPERANDS (stmt, SSA_OP_VUSE);
	  no_vmaymust = ZERO_SSA_OPERANDS (stmt,
					   SSA_OP_VMAYDEF | SSA_OP_VMUSTDEF);
	  if (no_vuse && no_vmaymust)
	    continue;

	  if (!no_vuse && !no_vmaymust)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
		{
		  fprintf (vect_dump, "unexpected vdefs and vuses in stmt: ");
		  print_generic_expr (vect_dump, stmt, TDF_SLIM);
		}
	      return false;
	    }

	  if (TREE_CODE (stmt) != MODIFY_EXPR)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
		{
		  fprintf (vect_dump, "unexpected vops in stmt: ");
		  print_generic_expr (vect_dump, stmt, TDF_SLIM);
		}
	      return false;
	    }

	  if (!no_vuse)
	    {
	      memref = TREE_OPERAND (stmt, 1);
	      datarefs = &(LOOP_VINFO_DATAREF_READS (loop_vinfo));
	      is_read = true;
	    } 
	  else /* vdefs */
	    {
	      memref = TREE_OPERAND (stmt, 0);
	      datarefs = &(LOOP_VINFO_DATAREF_WRITES (loop_vinfo));
	      is_read = false;
	    }
	  
	  scalar_type = TREE_TYPE (memref);
	  vectype = get_vectype_for_scalar_type (scalar_type);
	  if (!vectype)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
		{
		  fprintf (vect_dump, "no vectype for stmt: ");
		  print_generic_expr (vect_dump, stmt, TDF_SLIM);
		  fprintf (vect_dump, " scalar_type: ");
		  print_generic_expr (vect_dump, scalar_type, TDF_DETAILS);
		}
	      /* It is not possible to vectorize this data reference.  */
	      return false;
	    }
	 /* Analyze MEMREF. If it is of a supported form, build data_reference
	     struct for it (DR).  */
	  dr = NULL; 
	  base = vect_object_analysis (memref, stmt, is_read, vectype, &dr, 
				       &offset, &misalign, &step, 
				       &base_aligned, &tag, &ptr_info,
				       &subvars);
	  if (!base)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
					LOOP_LOC (loop_vinfo)))
		{
		  fprintf (vect_dump, "not vectorized: unhandled data ref: "); 
		  print_generic_expr (vect_dump, stmt, TDF_SLIM);
		}
	      return false;
	    }
	  STMT_VINFO_VECT_DR_BASE_ADDRESS (stmt_info) = base;
	  STMT_VINFO_VECT_INIT_OFFSET (stmt_info) = offset;
	  STMT_VINFO_VECT_STEP (stmt_info) = step;
	  STMT_VINFO_VECT_MISALIGNMENT (stmt_info) = misalign;
	  STMT_VINFO_VECT_BASE_ALIGNED_P (stmt_info) = base_aligned;
	  STMT_VINFO_MEMTAG (stmt_info) = tag;
	  STMT_VINFO_PTR_INFO (stmt_info) = ptr_info;
	  STMT_VINFO_SUBVARS (stmt_info) = subvars;
	  STMT_VINFO_VECTYPE (stmt_info) = vectype;
	  VARRAY_PUSH_GENERIC_PTR (*datarefs, dr);
	  STMT_VINFO_DATA_REF (stmt_info) = dr;
	}
    }

  return true;
}


/* Utility functions used by vect_mark_stmts_to_be_vectorized.  */

/* Function vect_mark_relevant.

   Mark STMT as "relevant for vectorization" and add it to WORKLIST.  */

static void
vect_mark_relevant (VEC(tree,heap) **worklist, tree stmt,
		    bool relevant_p, bool live_p)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  bool save_relevant_p = STMT_VINFO_RELEVANT_P (stmt_info);
  bool save_live_p = STMT_VINFO_LIVE_P (stmt_info);

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "mark relevant %d, live %d.",relevant_p, live_p);

  STMT_VINFO_LIVE_P (stmt_info) |= live_p;

  if (TREE_CODE (stmt) == PHI_NODE)
    /* Don't mark as relevant because it's not going to vectorized.  */
    return;

  STMT_VINFO_RELEVANT_P (stmt_info) |= relevant_p;

  if (STMT_VINFO_RELEVANT_P (stmt_info) == save_relevant_p
      && STMT_VINFO_LIVE_P (stmt_info) == save_live_p)
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
        fprintf (vect_dump, "already marked relevant/live.");
      return;
    }

  VEC_safe_push (tree, heap, *worklist, stmt);
}


/* Function vect_stmt_relevant_p.

   Return true if STMT in loop that is represented by LOOP_VINFO is
   "relevant for vectorization".

   A stmt is considered "relevant for vectorization" if:
   - it has uses outside the loop.
   - it has vdefs (it alters memory).
   - control stmts in the loop (except for the exit condition).

   CHECKME: what other side effects would the vectorizer allow?  */

static bool
vect_stmt_relevant_p (tree stmt, loop_vec_info loop_vinfo,
		      bool *relevant_p, bool *live_p)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  def_operand_p def_p;

  *relevant_p = false;
  *live_p = false;

  /* cond stmt other than loop exit cond.  */
  if (is_ctrl_stmt (stmt) && (stmt != LOOP_VINFO_EXIT_COND (loop_vinfo)))
    *relevant_p = true;

  /* changing memory.  */
  if (TREE_CODE (stmt) != PHI_NODE)
    if (!ZERO_SSA_OPERANDS (stmt, SSA_OP_VIRTUAL_DEFS))
      {
	if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	  fprintf (vect_dump, "vec_stmt_relevant_p: stmt has vdefs.");
	*relevant_p = true;
      }

  /* uses outside the loop.  */
  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, op_iter, SSA_OP_DEF)
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, DEF_FROM_PTR (def_p))
	{
	  basic_block bb = bb_for_stmt (USE_STMT (use_p));
	  if (!flow_bb_inside_loop_p (loop, bb))
	    {
	      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
		fprintf (vect_dump, "vec_stmt_relevant_p: used out of loop.");

	      /* We expect all such uses to be in the loop exit phis
		 (because of loop closed form)   */
	      gcc_assert (TREE_CODE (USE_STMT (use_p)) == PHI_NODE);
	      gcc_assert (bb == loop->single_exit->dest);

              *live_p = true;
	    }
	}
    }

  return (*live_p || *relevant_p);
}


/* Function vect_mark_stmts_to_be_vectorized.

   Not all stmts in the loop need to be vectorized. For example:

     for i...
       for j...
   1.    T0 = i + j
   2.	 T1 = a[T0]

   3.    j = j + 1

   Stmt 1 and 3 do not need to be vectorized, because loop control and
   addressing of vectorized data-refs are handled differently.

   This pass detects such stmts.  */

static bool
vect_mark_stmts_to_be_vectorized (loop_vec_info loop_vinfo)
{
  VEC(tree,heap) *worklist;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  tree stmt, use;
  stmt_ann_t ann;
  ssa_op_iter iter;
  unsigned int i;
  stmt_vec_info stmt_vinfo;
  basic_block bb;
  tree phi;
  bool relevant_p, live_p;
  tree def, def_stmt;
  enum vect_def_type dt;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_mark_stmts_to_be_vectorized ===");

  worklist = VEC_alloc (tree, heap, 64);

  /* 1. Init worklist.  */

  bb = loop->header;
  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
        {
          fprintf (vect_dump, "init: phi relevant? ");
          print_generic_expr (vect_dump, phi, TDF_SLIM);
        }

      if (vect_stmt_relevant_p (phi, loop_vinfo, &relevant_p, &live_p))
	vect_mark_relevant (&worklist, phi, relevant_p, live_p);
    }

  for (i = 0; i < nbbs; i++)
    {
      bb = bbs[i];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  stmt = bsi_stmt (si);

	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    {
	      fprintf (vect_dump, "init: stmt relevant? ");
	      print_generic_expr (vect_dump, stmt, TDF_SLIM);
	    } 

	  if (vect_stmt_relevant_p (stmt, loop_vinfo, &relevant_p, &live_p))
            vect_mark_relevant (&worklist, stmt, relevant_p, live_p);
	}
    }


  /* 2. Process_worklist */

  while (VEC_length (tree, worklist) > 0)
    {
      stmt = VEC_pop (tree, worklist);

      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	{
          fprintf (vect_dump, "worklist: examine stmt: ");
          print_generic_expr (vect_dump, stmt, TDF_SLIM);
	}

      /* Examine the USEs of STMT. For each ssa-name USE thta is defined
         in the loop, mark the stmt that defines it (DEF_STMT) as
         relevant/irrelevant and live/dead according to the liveness and
         relevance properties of STMT.
       */

      gcc_assert (TREE_CODE (stmt) != PHI_NODE);

      ann = stmt_ann (stmt);
      stmt_vinfo = vinfo_for_stmt (stmt);

      relevant_p = STMT_VINFO_RELEVANT_P (stmt_vinfo);
      live_p = STMT_VINFO_LIVE_P (stmt_vinfo);

      /* Generally, the liveness and relevance properties of STMT are
         propagated to the DEF_STMTs of its USEs:
             STMT_VINFO_LIVE_P (DEF_STMT_info) <-- live_p
             STMT_VINFO_RELEVANT_P (DEF_STMT_info) <-- relevant_p

         Exceptions:

         - if USE is used only for address computations (e.g. array indexing),
           which does not need to be directly vectorized, then the
           liveness/relevance of the respective DEF_STMT is left unchanged.

         - if STMT has been identified as defining a reduction variable, then:
             STMT_VINFO_LIVE_P (DEF_STMT_info) <-- false
             STMT_VINFO_RELEVANT_P (DEF_STMT_info) <-- true
           because even though STMT is classified as live (since it defines a
           value that is used across loop iterations) and irrelevant (since it
           is not used inside the loop), it will be vectorized, and therefore
           the corresponding DEF_STMTs need to marked as relevant.
       */

      if (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def)
        {
          gcc_assert (!relevant_p && live_p);
          relevant_p = true;
          live_p = false;
        }

      FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	{
	  /* We are only interested in uses that need to be vectorized. Uses 
	     that are used for address computation are not considered relevant.
	   */
	  if (exist_non_indexing_operands_for_use_p (use, stmt))
	    {
	      if (!vect_is_simple_use (use, loop_vinfo, &def_stmt, &def, &dt))
                {
                  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS,
					    LOOP_LOC (loop_vinfo)))
                    fprintf (vect_dump, 
			     "not vectorized: unsupported use in stmt.");
		  VEC_free (tree, heap, worklist);
                  return false;
                }

	      if (!def_stmt || IS_EMPTY_STMT (def_stmt))
		continue;

              if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
                {
                  fprintf (vect_dump, "worklist: examine use %d: ", i);
                  print_generic_expr (vect_dump, use, TDF_SLIM);
                }

	      bb = bb_for_stmt (def_stmt);
              if (!flow_bb_inside_loop_p (loop, bb))
                continue;

              if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
                {
                  fprintf (vect_dump, "def_stmt: ");
                  print_generic_expr (vect_dump, def_stmt, TDF_SLIM);
                }

              vect_mark_relevant (&worklist, def_stmt, relevant_p, live_p);
	    }
	}
    }				/* while worklist */

  VEC_free (tree, heap, worklist);
  return true;
}


/* Function vect_can_advance_ivs_p

   In case the number of iterations that LOOP iterates in unknown at compile 
   time, an epilog loop will be generated, and the loop induction variables 
   (IVs) will be "advanced" to the value they are supposed to take just before 
   the epilog loop.  Here we check that the access function of the loop IVs
   and the expression that represents the loop bound are simple enough.
   These restrictions will be relaxed in the future.  */

static bool 
vect_can_advance_ivs_p (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block bb = loop->header;
  tree phi;

  /* Analyze phi functions of the loop header.  */

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== vect_can_advance_ivs_p ===");

  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree access_fn = NULL;
      tree evolution_part;

      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	{
          fprintf (vect_dump, "Analyze phi: ");
          print_generic_expr (vect_dump, phi, TDF_SLIM);
	}

      /* Skip virtual phi's. The data dependences that are associated with
         virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.  */

      if (!is_gimple_reg (SSA_NAME_VAR (PHI_RESULT (phi))))
	{
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    fprintf (vect_dump, "virtual phi. skip.");
	  continue;
	}

      /* Analyze the evolution function.  */

      access_fn = instantiate_parameters
	(loop, analyze_scalar_evolution (loop, PHI_RESULT (phi)));

      if (!access_fn)
	{
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    fprintf (vect_dump, "No Access function.");
	  return false;
	}

      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
        {
	  fprintf (vect_dump, "Access function of PHI: ");
	  print_generic_expr (vect_dump, access_fn, TDF_SLIM);
        }

      evolution_part = evolution_part_in_loop_num (access_fn, loop->num);
      
      if (evolution_part == NULL_TREE)
        {
	  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	    fprintf (vect_dump, "No evolution.");
	  return false;
        }
  
      /* FORNOW: We do not transform initial conditions of IVs 
	 which evolution functions are a polynomial of degree >= 2.  */

      if (tree_is_chrec (evolution_part))
	return false;  
    }

  return true;
}


/* Function vect_get_loop_niters.

   Determine how many iterations the loop is executed.
   If an expression that represents the number of iterations
   can be constructed, place it in NUMBER_OF_ITERATIONS.
   Return the loop exit condition.  */

static tree
vect_get_loop_niters (struct loop *loop, tree *number_of_iterations)
{
  tree niters;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "=== get_loop_niters ===");

  niters = number_of_iterations_in_loop (loop);

  if (niters != NULL_TREE
      && niters != chrec_dont_know)
    {
      *number_of_iterations = niters;

      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	{
	  fprintf (vect_dump, "==> get_loop_niters:" );
	  print_generic_expr (vect_dump, *number_of_iterations, TDF_SLIM);
	}
    }

  return get_loop_exit_condition (loop);
}


/* Function vect_analyze_loop_form.

   Verify the following restrictions (some may be relaxed in the future):
   - it's an inner-most loop
   - number of BBs = 2 (which are the loop header and the latch)
   - the loop has a pre-header
   - the loop has a single entry and exit
   - the loop exit condition is simple enough, and the number of iterations
     can be analyzed (a countable loop).  */

static loop_vec_info
vect_analyze_loop_form (struct loop *loop)
{
  loop_vec_info loop_vinfo;
  tree loop_cond;
  tree number_of_iterations = NULL;
  LOC loop_loc;

  loop_loc = find_loop_location (loop);

  if (vect_print_dump_info (REPORT_DETAILS, loop_loc))
    fprintf (vect_dump, "=== vect_analyze_loop_form ===");

  if (loop->inner)
    {
      if (vect_print_dump_info (REPORT_OUTER_LOOPS, loop_loc))
        fprintf (vect_dump, "not vectorized: nested loop.");
      return NULL;
    }
  
  if (!loop->single_exit 
      || loop->num_nodes != 2
      || EDGE_COUNT (loop->header->preds) != 2)
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS, loop_loc))
        {
          if (!loop->single_exit)
            fprintf (vect_dump, "not vectorized: multiple exits.");
          else if (loop->num_nodes != 2)
            fprintf (vect_dump, "not vectorized: too many BBs in loop.");
          else if (EDGE_COUNT (loop->header->preds) != 2)
            fprintf (vect_dump, "not vectorized: too many incoming edges.");
        }

      return NULL;
    }

  /* We assume that the loop exit condition is at the end of the loop. i.e,
     that the loop is represented as a do-while (with a proper if-guard
     before the loop if needed), where the loop header contains all the
     executable statements, and the latch is empty.  */
  if (!empty_block_p (loop->latch))
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS, loop_loc))
        fprintf (vect_dump, "not vectorized: unexpected loop form.");
      return NULL;
    }

  /* Make sure there exists a single-predecessor exit bb:  */
  if (!single_pred_p (loop->single_exit->dest))
    {
      edge e = loop->single_exit;
      if (!(e->flags & EDGE_ABNORMAL))
	{
	  split_loop_exit_edge (e);
	  if (vect_print_dump_info (REPORT_DETAILS, loop_loc))
	    fprintf (vect_dump, "split exit edge.");
	}
      else
	{
	  if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS, loop_loc))
	    fprintf (vect_dump, "not vectorized: abnormal loop exit edge.");
	  return NULL;
	}
    }

  if (empty_block_p (loop->header))
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS, loop_loc))
        fprintf (vect_dump, "not vectorized: empty loop.");
      return NULL;
    }

  loop_cond = vect_get_loop_niters (loop, &number_of_iterations);
  if (!loop_cond)
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS, loop_loc))
	fprintf (vect_dump, "not vectorized: complicated exit condition.");
      return NULL;
    }
  
  if (!number_of_iterations) 
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS, loop_loc))
	fprintf (vect_dump, 
		 "not vectorized: number of iterations cannot be computed.");
      return NULL;
    }

  if (chrec_contains_undetermined (number_of_iterations))
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS, loop_loc))
        fprintf (vect_dump, "Infinite number of iterations.");
      return false;
    }

  loop_vinfo = new_loop_vec_info (loop);
  LOOP_VINFO_NITERS (loop_vinfo) = number_of_iterations;

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    {
      if (vect_print_dump_info (REPORT_DETAILS, loop_loc))
        {
          fprintf (vect_dump, "Symbolic number of iterations is ");
          print_generic_expr (vect_dump, number_of_iterations, TDF_DETAILS);
        }
    }
  else
  if (LOOP_VINFO_INT_NITERS (loop_vinfo) == 0)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS, loop_loc))
        fprintf (vect_dump, "not vectorized: number of iterations = 0.");
      return NULL;
    }

  LOOP_VINFO_EXIT_COND (loop_vinfo) = loop_cond;
  LOOP_VINFO_LOC (loop_vinfo) = loop_loc;

  return loop_vinfo;
}


/* Function vect_analyze_loop.

   Apply a set of analyses on LOOP, and create a loop_vec_info struct
   for it. The different analyses will record information in the
   loop_vec_info struct.  */
loop_vec_info
vect_analyze_loop (struct loop *loop)
{
  bool ok;
  loop_vec_info loop_vinfo;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    fprintf (vect_dump, "===== analyze_loop_nest =====");

  /* Check the CFG characteristics of the loop (nesting, entry/exit, etc.  */

  loop_vinfo = vect_analyze_loop_form (loop);
  if (!loop_vinfo)
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	fprintf (vect_dump, "bad loop form.");
      return NULL;
    }

  /* Find all data references in the loop (which correspond to vdefs/vuses)
     and analyze their evolution in the loop.

     FORNOW: Handle only simple, array references, which
     alignment can be forced, and aligned pointer-references.  */

  ok = vect_analyze_data_refs (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "bad data references.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Classify all cross-iteration scalar data-flow cycles.
     Cross-iteration cycles caused by virtual phis are analyzed separately.  */

  vect_analyze_scalar_cycles (loop_vinfo);

  /* Data-flow analysis to detect stmts that do not need to be vectorized.  */

  ok = vect_mark_stmts_to_be_vectorized (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "unexpected pattern.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  ok = vect_determine_vectorization_factor (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
        fprintf (vect_dump, "can't determine vectorization factor.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Analyze data dependences between the data-refs in the loop. 
     FORNOW: fail at the first data dependence that we encounter.  */

  ok = vect_analyze_data_ref_dependences (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "bad data dependence.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Analyze the access patterns of the data-refs in the loop (consecutive,
     complex, etc.). FORNOW: Only handle consecutive access pattern.  */

  ok = vect_analyze_data_ref_accesses (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "bad data access.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Analyze the alignment of the data-refs in the loop.
     FORNOW: Only aligned accesses are handled.  */

  ok = vect_analyze_data_refs_alignment (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "bad data alignment.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Scan all the operations in the loop and make sure they are
     vectorizable.  */

  ok = vect_analyze_operations (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS, LOOP_LOC (loop_vinfo)))
	fprintf (vect_dump, "bad operation or unsupported loop bound.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  LOOP_VINFO_VECTORIZABLE_P (loop_vinfo) = 1;

  return loop_vinfo;
}
