/* Analysis Utilities for Loop Vectorization.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 Free Software
   Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>

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
#include "timevar.h"
#include "cfgloop.h"
#include "expr.h"
#include "optabs.h"
#include "params.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "toplev.h"
#include "recog.h"

static bool vect_can_advance_ivs_p (loop_vec_info);

/* Return the smallest scalar part of STMT.
   This is used to determine the vectype of the stmt. We generally set the 
   vectype according to the type of the result (lhs). For stmts whose 
   result-type is different than the type of the arguments (e.g., demotion,
   promotion), vectype will be reset appropriately (later).  Note that we have 
   to visit the smallest datatype in this function, because that determines the
   VF. If the smallest datatype in the loop is present only as the rhs of a 
   promotion operation - we'd miss it.
   Such a case, where a variable of this datatype does not appear in the lhs
   anywhere in the loop, can only occur if it's an invariant: e.g.:
   'int_x = (int) short_inv', which we'd expect to have been optimized away by 
   invariant motion. However, we cannot rely on invariant motion to always take
   invariants out of the loop, and so in the case of promotion we also have to
   check the rhs. 
   LHS_SIZE_UNIT and RHS_SIZE_UNIT contain the sizes of the corresponding
   types.  */

tree
vect_get_smallest_scalar_type (gimple stmt, HOST_WIDE_INT *lhs_size_unit,
                               HOST_WIDE_INT *rhs_size_unit)
{
  tree scalar_type = gimple_expr_type (stmt);
  HOST_WIDE_INT lhs, rhs;

  lhs = rhs = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (scalar_type));

  if (is_gimple_assign (stmt)
      && (gimple_assign_cast_p (stmt)
          || gimple_assign_rhs_code (stmt) == WIDEN_MULT_EXPR
          || gimple_assign_rhs_code (stmt) == FLOAT_EXPR))
    {
      tree rhs_type = TREE_TYPE (gimple_assign_rhs1 (stmt));

      rhs = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (rhs_type));
      if (rhs < lhs)
        scalar_type = rhs_type;
    }
     
  *lhs_size_unit = lhs; 
  *rhs_size_unit = rhs;
  return scalar_type;
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
  gimple_stmt_iterator si;
  unsigned int vectorization_factor = 0;
  tree scalar_type;
  gimple phi;
  tree vectype;
  unsigned int nunits;
  stmt_vec_info stmt_info;
  int i;
  HOST_WIDE_INT dummy;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_determine_vectorization_factor ===");

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  phi = gsi_stmt (si);
	  stmt_info = vinfo_for_stmt (phi);
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "==> examining phi: ");
	      print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
	    }

	  gcc_assert (stmt_info);

	  if (STMT_VINFO_RELEVANT_P (stmt_info))
            {
	      gcc_assert (!STMT_VINFO_VECTYPE (stmt_info));
              scalar_type = TREE_TYPE (PHI_RESULT (phi));

	      if (vect_print_dump_info (REPORT_DETAILS))
		{
		  fprintf (vect_dump, "get vectype for scalar type:  ");
		  print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
		}

	      vectype = get_vectype_for_scalar_type (scalar_type);
	      if (!vectype)
		{
		  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		    {
		      fprintf (vect_dump,
		               "not vectorized: unsupported data-type ");
		      print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
		    }
		  return false;
		}
	      STMT_VINFO_VECTYPE (stmt_info) = vectype;

	      if (vect_print_dump_info (REPORT_DETAILS))
		{
		  fprintf (vect_dump, "vectype: ");
		  print_generic_expr (vect_dump, vectype, TDF_SLIM);
		}

	      nunits = TYPE_VECTOR_SUBPARTS (vectype);
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "nunits = %d", nunits);

	      if (!vectorization_factor
		  || (nunits > vectorization_factor))
		vectorization_factor = nunits;
	    }
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
        {
	  gimple stmt = gsi_stmt (si);
	  stmt_info = vinfo_for_stmt (stmt);

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "==> examining statement: ");
	      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	    }

          if (gimple_has_volatile_ops (stmt))
            {
              if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
                fprintf (vect_dump, "not vectorized: stmt has volatile"
                                    " operands");

              return false;
            }

	  gcc_assert (stmt_info);

	  /* skip stmts which do not need to be vectorized.  */
	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
	        fprintf (vect_dump, "skip.");
	      continue;
	    }

	  if (gimple_get_lhs (stmt) == NULL_TREE)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		{
	          fprintf (vect_dump, "not vectorized: irregular stmt.");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}
	      return false;
	    }

	  if (VECTOR_MODE_P (TYPE_MODE (gimple_expr_type (stmt))))
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
	        {
	          fprintf (vect_dump, "not vectorized: vector stmt in loop:");
	          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	        }
	      return false;
	    }

	  if (STMT_VINFO_VECTYPE (stmt_info))
	    {
	      /* The only case when a vectype had been already set is for stmts 
	         that contain a dataref, or for "pattern-stmts" (stmts generated
		 by the vectorizer to represent/replace a certain idiom).  */
	      gcc_assert (STMT_VINFO_DATA_REF (stmt_info) 
			  || is_pattern_stmt_p (stmt_info));
	      vectype = STMT_VINFO_VECTYPE (stmt_info);
	    }
	  else
	    {

	      gcc_assert (! STMT_VINFO_DATA_REF (stmt_info)
			  && !is_pattern_stmt_p (stmt_info));

	      scalar_type = vect_get_smallest_scalar_type (stmt, &dummy, 
                                                           &dummy);
	      if (vect_print_dump_info (REPORT_DETAILS))
		{
		  fprintf (vect_dump, "get vectype for scalar type:  ");
		  print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
		}

	      vectype = get_vectype_for_scalar_type (scalar_type);
	      if (!vectype)
		{
		  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		    {
		      fprintf (vect_dump, 
			       "not vectorized: unsupported data-type ");
		      print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
		    }
		  return false;
		}
	      STMT_VINFO_VECTYPE (stmt_info) = vectype;
            }

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "vectype: ");
	      print_generic_expr (vect_dump, vectype, TDF_SLIM);
	    }

	  nunits = TYPE_VECTOR_SUBPARTS (vectype);
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "nunits = %d", nunits);

	  if (!vectorization_factor
	      || (nunits > vectorization_factor))
	    vectorization_factor = nunits;

        }
    }

  /* TODO: Analyze cost. Decide if worth while to vectorize.  */
  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "vectorization factor = %d", vectorization_factor);
  if (vectorization_factor <= 1)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
        fprintf (vect_dump, "not vectorized: unsupported data-type");
      return false;
    }
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;

  return true;
}


/* SLP costs are calculated according to SLP instance unrolling factor (i.e., 
   the number of created vector stmts depends on the unrolling factor). However,
   the actual number of vector stmts for every SLP node depends on VF which is
   set later in vect_analyze_operations(). Hence, SLP costs should be updated.
   In this function we assume that the inside costs calculated in 
   vect_model_xxx_cost are linear in ncopies.  */

static void
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


/* Function vect_analyze_operations.

   Scan the loop stmts and make sure they are all vectorizable.  */

static bool
vect_analyze_operations (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  gimple_stmt_iterator si;
  unsigned int vectorization_factor = 0;
  int i;
  bool ok;
  gimple phi;
  stmt_vec_info stmt_info;
  bool need_to_vectorize = false;
  int min_profitable_iters;
  int min_scalar_loop_bound;
  unsigned int th;
  bool only_slp_in_loop = true;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_operations ===");

  gcc_assert (LOOP_VINFO_VECT_FACTOR (loop_vinfo));
  vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
        {
	  phi = gsi_stmt (si);
	  ok = true;

	  stmt_info = vinfo_for_stmt (phi);
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "examining phi: ");
	      print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
	    }

	  if (! is_loop_header_bb_p (bb))
	    {
	      /* inner-loop loop-closed exit phi in outer-loop vectorization
		 (i.e. a phi in the tail of the outer-loop). 
		 FORNOW: we currently don't support the case that these phis
		 are not used in the outerloop, cause this case requires
		 to actually do something here.  */
	      if (!STMT_VINFO_RELEVANT_P (stmt_info) 
		  || STMT_VINFO_LIVE_P (stmt_info))
		{
		  if (vect_print_dump_info (REPORT_DETAILS))
		    fprintf (vect_dump, 
			     "Unsupported loop-closed phi in outer-loop.");
		  return false;
		}
	      continue;
	    }

	  gcc_assert (stmt_info);

	  if (STMT_VINFO_LIVE_P (stmt_info))
	    {
	      /* FORNOW: not yet supported.  */
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		fprintf (vect_dump, "not vectorized: value used after loop.");
	      return false;
	    }

	  if (STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_loop
	      && STMT_VINFO_DEF_TYPE (stmt_info) != vect_induction_def)
	    {
	      /* A scalar-dependence cycle that we don't support.  */
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		fprintf (vect_dump, "not vectorized: scalar dependence cycle.");
	      return false;
	    }

	  if (STMT_VINFO_RELEVANT_P (stmt_info))
	    {
	      need_to_vectorize = true;
	      if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def)
		ok = vectorizable_induction (phi, NULL, NULL);
	    }

	  if (!ok)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		{
		  fprintf (vect_dump,
			   "not vectorized: relevant phi not supported: ");
		  print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
		}
	      return false;
	    }
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple stmt = gsi_stmt (si);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  enum vect_relevant relevance = STMT_VINFO_RELEVANT (stmt_info);

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "==> examining statement: ");
	      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
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
	      if (vect_print_dump_info (REPORT_DETAILS))
	        fprintf (vect_dump, "irrelevant.");
	      continue;
	    }

	  switch (STMT_VINFO_DEF_TYPE (stmt_info))
	    {
 	    case vect_loop_def:
	      break;
	
	    case vect_reduction_def:
	      gcc_assert (relevance == vect_used_in_outer
			  || relevance == vect_used_in_outer_by_reduction
			  || relevance == vect_unused_in_loop);
	      break;	

	    case vect_induction_def:
	    case vect_constant_def:
	    case vect_invariant_def:
	    case vect_unknown_def_type:
	    default:
	      gcc_unreachable ();	
	    }

	  if (STMT_VINFO_RELEVANT_P (stmt_info))
	    {
	      gcc_assert (!VECTOR_MODE_P (TYPE_MODE (gimple_expr_type (stmt))));
	      gcc_assert (STMT_VINFO_VECTYPE (stmt_info));
	      need_to_vectorize = true;
	    }

	  ok = true;
	  if (STMT_VINFO_RELEVANT_P (stmt_info)
	      || STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def)
	    ok = (vectorizable_type_promotion (stmt, NULL, NULL, NULL)
		|| vectorizable_type_demotion (stmt, NULL, NULL, NULL)
		|| vectorizable_conversion (stmt, NULL, NULL, NULL)
		|| vectorizable_operation (stmt, NULL, NULL, NULL)
		|| vectorizable_assignment (stmt, NULL, NULL, NULL)
		|| vectorizable_load (stmt, NULL, NULL, NULL, NULL)
		|| vectorizable_call (stmt, NULL, NULL)
		|| vectorizable_store (stmt, NULL, NULL, NULL)
		|| vectorizable_condition (stmt, NULL, NULL)
		|| vectorizable_reduction (stmt, NULL, NULL));

	  if (!ok)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		{
		  fprintf (vect_dump, "not vectorized: relevant stmt not ");
		  fprintf (vect_dump, "supported: ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}
	      return false;
	    }

	  /* Stmts that are (also) "live" (i.e. - that are used out of the loop)
	     need extra handling, except for vectorizable reductions.  */
	  if (STMT_VINFO_LIVE_P (stmt_info)
	      && STMT_VINFO_TYPE (stmt_info) != reduc_vec_info_type) 
	    ok = vectorizable_live_operation (stmt, NULL, NULL);

	  if (!ok)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
		{
		  fprintf (vect_dump, "not vectorized: live stmt not ");
		  fprintf (vect_dump, "supported: ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}
	      return false;
	    }	

	  if (!PURE_SLP_STMT (stmt_info))
	    {
	      /* STMT needs loop-based vectorization.  */
	      only_slp_in_loop = false;

	      /* Groups of strided accesses whose size is not a power of 2 are 
		 not vectorizable yet using loop-vectorization. Therefore, if 
		 this stmt feeds non-SLP-able stmts (i.e., this stmt has to be 
		 both SLPed and loop-based vectorized), the loop cannot be
		 vectorized.  */
	      if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
		  && exact_log2 (DR_GROUP_SIZE (vinfo_for_stmt (
			          DR_GROUP_FIRST_DR (stmt_info)))) == -1)
		{
		  if (vect_print_dump_info (REPORT_DETAILS))
		    {
		      fprintf (vect_dump, "not vectorized: the size of group "
			       "of strided accesses is not a power of 2");
		      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		    }
		  return false;
		}
	    }
	} /* stmts in bb */
    } /* bbs */

  /* All operations in the loop are either irrelevant (deal with loop
     control, or dead), or only used outside the loop and can be moved
     out of the loop (e.g. invariants, inductions).  The loop can be 
     optimized away by scalar optimizations.  We're better off not 
     touching this loop.  */
  if (!need_to_vectorize)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, 
		 "All the computation can be taken out of the loop.");
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
        fprintf (vect_dump, 
		 "not vectorized: redundant loop. no profit to vectorize.");
      return false;
    }

  /* If all the stmts in the loop can be SLPed, we perform only SLP, and
     vectorization factor of the loop is the unrolling factor required by the
     SLP instances.  If that unrolling factor is 1, we say, that we perform
     pure SLP on loop - cross iteration parallelism is not exploited.  */
  if (only_slp_in_loop)
    vectorization_factor = LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo);
  else
    vectorization_factor = least_common_multiple (vectorization_factor,
				LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo));
  
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump,
        "vectorization_factor = %d, niters = " HOST_WIDE_INT_PRINT_DEC,
        vectorization_factor, LOOP_VINFO_INT_NITERS (loop_vinfo));

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && (LOOP_VINFO_INT_NITERS (loop_vinfo) < vectorization_factor))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
        fprintf (vect_dump, "not vectorized: iteration count too small.");
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump,"not vectorized: iteration count smaller than "
                 "vectorization factor.");
      return false;
    }

  /* Analyze cost. Decide if worth while to vectorize.  */

  /* Once VF is set, SLP costs should be updated since the number of created
     vector stmts depends on VF.  */
  vect_update_slp_costs_according_to_vf (loop_vinfo);

  min_profitable_iters = vect_estimate_min_profitable_iters (loop_vinfo);
  LOOP_VINFO_COST_MODEL_MIN_ITERS (loop_vinfo) = min_profitable_iters;

  if (min_profitable_iters < 0)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
        fprintf (vect_dump, "not vectorized: vectorization not profitable.");
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "not vectorized: vector version will never be "
                 "profitable.");
      return false;
    }

  min_scalar_loop_bound = ((PARAM_VALUE (PARAM_MIN_VECT_LOOP_BOUND)
			    * vectorization_factor) - 1);

  /* Use the cost model only if it is more conservative than user specified
     threshold.  */

  th = (unsigned) min_scalar_loop_bound;
  if (min_profitable_iters 
      && (!min_scalar_loop_bound
          || min_profitable_iters > min_scalar_loop_bound))
    th = (unsigned) min_profitable_iters;

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && LOOP_VINFO_INT_NITERS (loop_vinfo) <= th)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))	      
        fprintf (vect_dump, "not vectorized: vectorization not "
                 "profitable.");
      if (vect_print_dump_info (REPORT_DETAILS))	      
        fprintf (vect_dump, "not vectorized: iteration count smaller than "
                 "user specified loop bound parameter or minimum "
                 "profitable iterations (whichever is more conservative).");
      return false;
    }  

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      || LOOP_VINFO_INT_NITERS (loop_vinfo) % vectorization_factor != 0
      || LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "epilog loop required.");
      if (!vect_can_advance_ivs_p (loop_vinfo))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
            fprintf (vect_dump,
                     "not vectorized: can't create epilog loop 1.");
          return false;
        }
      if (!slpeel_can_duplicate_loop_p (loop, single_exit (loop)))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
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
exist_non_indexing_operands_for_use_p (tree use, gimple stmt)
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
 
  if (TREE_CODE (gimple_assign_lhs (stmt)) == SSA_NAME)
    return false;

  if (!gimple_assign_copy_p (stmt))
    return false;
  operand = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  if (operand == use)
    return true;

  return false;
}


/* Function vect_analyze_scalar_cycles_1.

   Examine the cross iteration def-use cycles of scalar variables
   in LOOP. LOOP_VINFO represents the loop that is now being
   considered for vectorization (can be LOOP, or an outer-loop
   enclosing LOOP).  */

static void
vect_analyze_scalar_cycles_1 (loop_vec_info loop_vinfo, struct loop *loop)
{
  basic_block bb = loop->header;
  tree dumy;
  VEC(gimple,heap) *worklist = VEC_alloc (gimple, heap, 64);
  gimple_stmt_iterator gsi;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_scalar_cycles ===");

  /* First - identify all inductions.  */
  for (gsi = gsi_start_phis  (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree access_fn = NULL;
      tree def = PHI_RESULT (phi);
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (phi);

      if (vect_print_dump_info (REPORT_DETAILS))
	{
	  fprintf (vect_dump, "Analyze phi: ");
	  print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
	}

      /* Skip virtual phi's. The data dependences that are associated with
         virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.  */
      if (!is_gimple_reg (SSA_NAME_VAR (def)))
	continue;

      STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_unknown_def_type;

      /* Analyze the evolution function.  */
      access_fn = analyze_scalar_evolution (loop, def);
      if (access_fn && vect_print_dump_info (REPORT_DETAILS))
	{
	  fprintf (vect_dump, "Access function of PHI: ");
	  print_generic_expr (vect_dump, access_fn, TDF_SLIM);
	}

      if (!access_fn
	  || !vect_is_simple_iv_evolution (loop->num, access_fn, &dumy, &dumy)) 
	{
	  VEC_safe_push (gimple, heap, worklist, phi);	  
	  continue;
	}

      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "Detected induction.");
      STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_induction_def;
    }


  /* Second - identify all reductions.  */
  while (VEC_length (gimple, worklist) > 0)
    {
      gimple phi = VEC_pop (gimple, worklist);
      tree def = PHI_RESULT (phi);
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (phi);
      gimple reduc_stmt;

      if (vect_print_dump_info (REPORT_DETAILS))
        { 
          fprintf (vect_dump, "Analyze phi: ");
          print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
        }

      gcc_assert (is_gimple_reg (SSA_NAME_VAR (def)));
      gcc_assert (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_unknown_def_type);

      reduc_stmt = vect_is_simple_reduction (loop_vinfo, phi);
      if (reduc_stmt)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "Detected reduction.");
          STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_reduction_def;
          STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                        vect_reduction_def;
        }
      else
        if (vect_print_dump_info (REPORT_DETAILS))
          fprintf (vect_dump, "Unknown def-use cycle pattern.");
    }

  VEC_free (gimple, heap, worklist);
  return;
}


/* Function vect_analyze_scalar_cycles.

   Examine the cross iteration def-use cycles of scalar variables, by
   analyzing the loop-header PHIs of scalar variables; Classify each 
   cycle as one of the following: invariant, induction, reduction, unknown.
   We do that for the loop represented by LOOP_VINFO, and also to its
   inner-loop, if exists.
   Examples for scalar cycles:

   Example1: reduction:

              loop1:
              for (i=0; i<N; i++)
                 sum += a[i];

   Example2: induction:

              loop2:
              for (i=0; i<N; i++)
                 a[i] = i;  */

static void
vect_analyze_scalar_cycles (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  vect_analyze_scalar_cycles_1 (loop_vinfo, loop);

  /* When vectorizing an outer-loop, the inner-loop is executed sequentially.
     Reductions in such inner-loop therefore have different properties than
     the reductions in the nest that gets vectorized:
     1. When vectorized, they are executed in the same order as in the original
        scalar loop, so we can't change the order of computation when
        vectorizing them.
     2. FIXME: Inner-loop reductions can be used in the inner-loop, so the 
        current checks are too strict.  */

  if (loop->inner)
    vect_analyze_scalar_cycles_1 (loop_vinfo, loop->inner);
}


/* Find the place of the data-ref in STMT in the interleaving chain that starts
   from FIRST_STMT. Return -1 if the data-ref is not a part of the chain.  */

static int 
vect_get_place_in_interleaving_chain (gimple stmt, gimple first_stmt)
{
  gimple next_stmt = first_stmt;
  int result = 0;

  if (first_stmt != DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)))
    return -1;

  while (next_stmt && next_stmt != stmt)
    {
      result++;
      next_stmt = DR_GROUP_NEXT_DR (vinfo_for_stmt (next_stmt));
    }

  if (next_stmt)
    return result;
  else
    return -1;
}


/* Function vect_insert_into_interleaving_chain.

   Insert DRA into the interleaving chain of DRB according to DRA's INIT.  */

static void
vect_insert_into_interleaving_chain (struct data_reference *dra,
				     struct data_reference *drb)
{
  gimple prev, next;
  tree next_init;
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra)); 
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));

  prev = DR_GROUP_FIRST_DR (stmtinfo_b);
  next = DR_GROUP_NEXT_DR (vinfo_for_stmt (prev));		  
  while (next)
    {
      next_init = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (next)));
      if (tree_int_cst_compare (next_init, DR_INIT (dra)) > 0)
	{
	  /* Insert here.  */
	  DR_GROUP_NEXT_DR (vinfo_for_stmt (prev)) = DR_STMT (dra);
	  DR_GROUP_NEXT_DR (stmtinfo_a) = next;
	  return;
	}
      prev = next;
      next = DR_GROUP_NEXT_DR (vinfo_for_stmt (prev));
    }

  /* We got to the end of the list. Insert here.  */
  DR_GROUP_NEXT_DR (vinfo_for_stmt (prev)) = DR_STMT (dra);
  DR_GROUP_NEXT_DR (stmtinfo_a) = NULL;
}


/* Function vect_update_interleaving_chain.
   
   For two data-refs DRA and DRB that are a part of a chain interleaved data 
   accesses, update the interleaving chain. DRB's INIT is smaller than DRA's.

   There are four possible cases:
   1. New stmts - both DRA and DRB are not a part of any chain:
      FIRST_DR = DRB
      NEXT_DR (DRB) = DRA
   2. DRB is a part of a chain and DRA is not:
      no need to update FIRST_DR
      no need to insert DRB
      insert DRA according to init
   3. DRA is a part of a chain and DRB is not:
      if (init of FIRST_DR > init of DRB)
          FIRST_DR = DRB
	  NEXT(FIRST_DR) = previous FIRST_DR
      else
          insert DRB according to its init
   4. both DRA and DRB are in some interleaving chains:
      choose the chain with the smallest init of FIRST_DR
      insert the nodes of the second chain into the first one.  */

static void
vect_update_interleaving_chain (struct data_reference *drb,
				struct data_reference *dra)
{
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra)); 
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));
  tree next_init, init_dra_chain, init_drb_chain;
  gimple first_a, first_b;
  tree node_init;
  gimple node, prev, next, first_stmt;

  /* 1. New stmts - both DRA and DRB are not a part of any chain.   */
  if (!DR_GROUP_FIRST_DR (stmtinfo_a) && !DR_GROUP_FIRST_DR (stmtinfo_b))
    {
      DR_GROUP_FIRST_DR (stmtinfo_a) = DR_STMT (drb);
      DR_GROUP_FIRST_DR (stmtinfo_b) = DR_STMT (drb);
      DR_GROUP_NEXT_DR (stmtinfo_b) = DR_STMT (dra);
      return;
    }

  /* 2. DRB is a part of a chain and DRA is not.  */
  if (!DR_GROUP_FIRST_DR (stmtinfo_a) && DR_GROUP_FIRST_DR (stmtinfo_b))
    {
      DR_GROUP_FIRST_DR (stmtinfo_a) = DR_GROUP_FIRST_DR (stmtinfo_b);
      /* Insert DRA into the chain of DRB.  */
      vect_insert_into_interleaving_chain (dra, drb);
      return;
    }

  /* 3. DRA is a part of a chain and DRB is not.  */  
  if (DR_GROUP_FIRST_DR (stmtinfo_a) && !DR_GROUP_FIRST_DR (stmtinfo_b))
    {
      gimple old_first_stmt = DR_GROUP_FIRST_DR (stmtinfo_a);
      tree init_old = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (
							      old_first_stmt)));
      gimple tmp;

      if (tree_int_cst_compare (init_old, DR_INIT (drb)) > 0)
	{
	  /* DRB's init is smaller than the init of the stmt previously marked 
	     as the first stmt of the interleaving chain of DRA. Therefore, we 
	     update FIRST_STMT and put DRB in the head of the list.  */
	  DR_GROUP_FIRST_DR (stmtinfo_b) = DR_STMT (drb);
	  DR_GROUP_NEXT_DR (stmtinfo_b) = old_first_stmt;
		
	  /* Update all the stmts in the list to point to the new FIRST_STMT.  */
	  tmp = old_first_stmt;
	  while (tmp)
	    {
	      DR_GROUP_FIRST_DR (vinfo_for_stmt (tmp)) = DR_STMT (drb);
	      tmp = DR_GROUP_NEXT_DR (vinfo_for_stmt (tmp));
	    }
	}
      else
	{
	  /* Insert DRB in the list of DRA.  */
	  vect_insert_into_interleaving_chain (drb, dra);
	  DR_GROUP_FIRST_DR (stmtinfo_b) = DR_GROUP_FIRST_DR (stmtinfo_a);	      
	}
      return;
    }
  
  /* 4. both DRA and DRB are in some interleaving chains.  */
  first_a = DR_GROUP_FIRST_DR (stmtinfo_a);
  first_b = DR_GROUP_FIRST_DR (stmtinfo_b);
  if (first_a == first_b)
    return;
  init_dra_chain = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (first_a)));
  init_drb_chain = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (first_b)));

  if (tree_int_cst_compare (init_dra_chain, init_drb_chain) > 0)
    {
      /* Insert the nodes of DRA chain into the DRB chain.  
	 After inserting a node, continue from this node of the DRB chain (don't
         start from the beginning.  */
      node = DR_GROUP_FIRST_DR (stmtinfo_a);
      prev = DR_GROUP_FIRST_DR (stmtinfo_b);      
      first_stmt = first_b;
    }
  else
    {
      /* Insert the nodes of DRB chain into the DRA chain.  
	 After inserting a node, continue from this node of the DRA chain (don't
         start from the beginning.  */
      node = DR_GROUP_FIRST_DR (stmtinfo_b);
      prev = DR_GROUP_FIRST_DR (stmtinfo_a);      
      first_stmt = first_a;
    }
  
  while (node)
    {
      node_init = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (node)));
      next = DR_GROUP_NEXT_DR (vinfo_for_stmt (prev));		  
      while (next)
	{	  
	  next_init = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (next)));
	  if (tree_int_cst_compare (next_init, node_init) > 0)
	    {
	      /* Insert here.  */
	      DR_GROUP_NEXT_DR (vinfo_for_stmt (prev)) = node;
	      DR_GROUP_NEXT_DR (vinfo_for_stmt (node)) = next;
	      prev = node;
	      break;
	    }
	  prev = next;
	  next = DR_GROUP_NEXT_DR (vinfo_for_stmt (prev));
	}
      if (!next)
	{
	  /* We got to the end of the list. Insert here.  */
	  DR_GROUP_NEXT_DR (vinfo_for_stmt (prev)) = node;
	  DR_GROUP_NEXT_DR (vinfo_for_stmt (node)) = NULL;
	  prev = node;
	}			
      DR_GROUP_FIRST_DR (vinfo_for_stmt (node)) = first_stmt;
      node = DR_GROUP_NEXT_DR (vinfo_for_stmt (node));	       
    }
}


/* Function vect_equal_offsets.

   Check if OFFSET1 and OFFSET2 are identical expressions.  */

static bool
vect_equal_offsets (tree offset1, tree offset2)
{
  bool res0, res1;

  STRIP_NOPS (offset1);
  STRIP_NOPS (offset2);

  if (offset1 == offset2)
    return true;

  if (TREE_CODE (offset1) != TREE_CODE (offset2)
      || !BINARY_CLASS_P (offset1)
      || !BINARY_CLASS_P (offset2))    
    return false;
  
  res0 = vect_equal_offsets (TREE_OPERAND (offset1, 0), 
			     TREE_OPERAND (offset2, 0));
  res1 = vect_equal_offsets (TREE_OPERAND (offset1, 1), 
			     TREE_OPERAND (offset2, 1));

  return (res0 && res1);
}


/* Function vect_check_interleaving.

   Check if DRA and DRB are a part of interleaving. In case they are, insert
   DRA and DRB in an interleaving chain.  */

static void
vect_check_interleaving (struct data_reference *dra,
			 struct data_reference *drb)
{
  HOST_WIDE_INT type_size_a, type_size_b, diff_mod_size, step, init_a, init_b;

  /* Check that the data-refs have same first location (except init) and they
     are both either store or load (not load and store).  */
  if ((DR_BASE_ADDRESS (dra) != DR_BASE_ADDRESS (drb)
       && (TREE_CODE (DR_BASE_ADDRESS (dra)) != ADDR_EXPR 
	   || TREE_CODE (DR_BASE_ADDRESS (drb)) != ADDR_EXPR
	   || TREE_OPERAND (DR_BASE_ADDRESS (dra), 0) 
	   != TREE_OPERAND (DR_BASE_ADDRESS (drb),0)))
      || !vect_equal_offsets (DR_OFFSET (dra), DR_OFFSET (drb))
      || !tree_int_cst_compare (DR_INIT (dra), DR_INIT (drb)) 
      || DR_IS_READ (dra) != DR_IS_READ (drb))
    return;

  /* Check:
     1. data-refs are of the same type
     2. their steps are equal
     3. the step is greater than the difference between data-refs' inits  */
  type_size_a = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dra))));
  type_size_b = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (drb))));

  if (type_size_a != type_size_b
      || tree_int_cst_compare (DR_STEP (dra), DR_STEP (drb))
      || !types_compatible_p (TREE_TYPE (DR_REF (dra)), 
                              TREE_TYPE (DR_REF (drb))))
    return;

  init_a = TREE_INT_CST_LOW (DR_INIT (dra));
  init_b = TREE_INT_CST_LOW (DR_INIT (drb));
  step = TREE_INT_CST_LOW (DR_STEP (dra));

  if (init_a > init_b)
    {
      /* If init_a == init_b + the size of the type * k, we have an interleaving, 
	 and DRB is accessed before DRA.  */
      diff_mod_size = (init_a - init_b) % type_size_a;

      if ((init_a - init_b) > step)
         return; 

      if (diff_mod_size == 0)
	{
	  vect_update_interleaving_chain (drb, dra);	  
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "Detected interleaving ");
	      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	      fprintf (vect_dump, " and ");
	      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	    }
	  return;
	} 
    }
  else 
    {
      /* If init_b == init_a + the size of the type * k, we have an 
	 interleaving, and DRA is accessed before DRB.  */
      diff_mod_size = (init_b - init_a) % type_size_a;

      if ((init_b - init_a) > step)
         return;

      if (diff_mod_size == 0)
	{
	  vect_update_interleaving_chain (dra, drb);	  
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "Detected interleaving ");
	      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	      fprintf (vect_dump, " and ");
	      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	    }
	  return;
	} 
    }
}

/* Check if data references pointed by DR_I and DR_J are same or
   belong to same interleaving group.  Return FALSE if drs are
   different, otherwise return TRUE.  */

static bool
vect_same_range_drs (data_reference_p dr_i, data_reference_p dr_j)
{
  gimple stmt_i = DR_STMT (dr_i);
  gimple stmt_j = DR_STMT (dr_j);

  if (operand_equal_p (DR_REF (dr_i), DR_REF (dr_j), 0)
      || (DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt_i))
	    && DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt_j))
	    && (DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt_i))
		== DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt_j)))))
    return true;
  else
    return false;
}

/* If address ranges represented by DDR_I and DDR_J are equal,
   return TRUE, otherwise return FALSE.  */

static bool
vect_vfa_range_equal (ddr_p ddr_i, ddr_p ddr_j)
{
  if ((vect_same_range_drs (DDR_A (ddr_i), DDR_A (ddr_j))
       && vect_same_range_drs (DDR_B (ddr_i), DDR_B (ddr_j)))
      || (vect_same_range_drs (DDR_A (ddr_i), DDR_B (ddr_j))
	  && vect_same_range_drs (DDR_B (ddr_i), DDR_A (ddr_j))))
    return true;
  else
    return false;
}

/* Insert DDR into LOOP_VINFO list of ddrs that may alias and need to be
   tested at run-time.  Return TRUE if DDR was successfully inserted.
   Return false if versioning is not supported.  */

static bool
vect_mark_for_runtime_alias_test (ddr_p ddr, loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  if ((unsigned) PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS) == 0)
    return false;

  if (vect_print_dump_info (REPORT_DR_DETAILS))
    {
      fprintf (vect_dump, "mark for run-time aliasing test between ");
      print_generic_expr (vect_dump, DR_REF (DDR_A (ddr)), TDF_SLIM);
      fprintf (vect_dump, " and ");
      print_generic_expr (vect_dump, DR_REF (DDR_B (ddr)), TDF_SLIM);
    }

  if (optimize_loop_nest_for_size_p (loop))
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
	fprintf (vect_dump, "versioning not supported when optimizing for size.");
      return false;
    }

  /* FORNOW: We don't support versioning with outer-loop vectorization.  */
  if (loop->inner)
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
	fprintf (vect_dump, "versioning not yet supported for outer-loops.");
      return false;
    }

  VEC_safe_push (ddr_p, heap, LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo), ddr);
  return true;
}

/* Function vect_analyze_data_ref_dependence.

   Return TRUE if there (might) exist a dependence between a memory-reference
   DRA and a memory-reference DRB.  When versioning for alias may check a
   dependence at run-time, return FALSE.  */
      
static bool
vect_analyze_data_ref_dependence (struct data_dependence_relation *ddr,
                                  loop_vec_info loop_vinfo)
{
  unsigned int i;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra)); 
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));
  int dra_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dra))));
  int drb_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (drb))));
  lambda_vector dist_v;
  unsigned int loop_depth;
         
  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    {
      /* Independent data accesses.  */
      vect_check_interleaving (dra, drb);
      return false;
    }

  if ((DR_IS_READ (dra) && DR_IS_READ (drb)) || dra == drb)
    return false;
  
  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
        {
          fprintf (vect_dump,
                   "versioning for alias required: can't determine dependence between ");
          print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
          fprintf (vect_dump, " and ");
          print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
        }
      /* Add to list of ddrs that need to be tested at run-time.  */
      return !vect_mark_for_runtime_alias_test (ddr, loop_vinfo);
    }

  if (DDR_NUM_DIST_VECTS (ddr) == 0)
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
        {
          fprintf (vect_dump, "versioning for alias required: bad dist vector for ");
          print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
          fprintf (vect_dump, " and ");
          print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
        }
      /* Add to list of ddrs that need to be tested at run-time.  */
      return !vect_mark_for_runtime_alias_test (ddr, loop_vinfo);
    }    

  loop_depth = index_in_loop_nest (loop->num, DDR_LOOP_NEST (ddr));
  for (i = 0; VEC_iterate (lambda_vector, DDR_DIST_VECTS (ddr), i, dist_v); i++)
    {
      int dist = dist_v[loop_depth];

      if (vect_print_dump_info (REPORT_DR_DETAILS))
	fprintf (vect_dump, "dependence distance  = %d.", dist);

      /* Same loop iteration.  */
      if (dist % vectorization_factor == 0 && dra_size == drb_size)
	{
	  /* Two references with distance zero have the same alignment.  */
	  VEC_safe_push (dr_p, heap, STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_a), drb);
	  VEC_safe_push (dr_p, heap, STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_b), dra);
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
	    fprintf (vect_dump, "accesses have the same alignment.");
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "dependence distance modulo vf == 0 between ");
	      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	      fprintf (vect_dump, " and ");
	      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	    }

          /* For interleaving, mark that there is a read-write dependency if
             necessary. We check before that one of the data-refs is store.  */ 
          if (DR_IS_READ (dra))
            DR_GROUP_READ_WRITE_DEPENDENCE (stmtinfo_a) = true;
	  else
            {
              if (DR_IS_READ (drb))
                DR_GROUP_READ_WRITE_DEPENDENCE (stmtinfo_b) = true;
	    }
	  
          continue;
	}

      if (abs (dist) >= vectorization_factor 
          || (dist > 0 && DDR_REVERSED_P (ddr)))
	{
	  /* Dependence distance does not create dependence, as far as 
	     vectorization is concerned, in this case. If DDR_REVERSED_P the 
	     order of the data-refs in DDR was reversed (to make distance
	     vector positive), and the actual distance is negative.  */
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    fprintf (vect_dump, "dependence distance >= VF or negative.");
	  continue;
	}

      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
	{
	  fprintf (vect_dump,
		   "not vectorized, possible dependence "
		   "between data-refs ");
	  print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	  fprintf (vect_dump, " and ");
	  print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	}

      return true;
    }

  return false;
}

/* Function vect_analyze_data_ref_dependences.
          
   Examine all the data references in the loop, and make sure there do not
   exist any data dependences between them.  */
         
static bool
vect_analyze_data_ref_dependences (loop_vec_info loop_vinfo)
{
  unsigned int i;
  VEC (ddr_p, heap) * ddrs = LOOP_VINFO_DDRS (loop_vinfo);
  struct data_dependence_relation *ddr;

  if (vect_print_dump_info (REPORT_DETAILS)) 
    fprintf (vect_dump, "=== vect_analyze_dependences ===");
     
  for (i = 0; VEC_iterate (ddr_p, ddrs, i, ddr); i++)
    if (vect_analyze_data_ref_dependence (ddr, loop_vinfo))
      return false;

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
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);  
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree ref = DR_REF (dr);
  tree vectype;
  tree base, base_addr;
  bool base_aligned;
  tree misalign;
  tree aligned_to, alignment;
   
  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "vect_compute_data_ref_alignment:");

  /* Initialize misalignment to unknown.  */
  SET_DR_MISALIGNMENT (dr, -1);

  misalign = DR_INIT (dr);
  aligned_to = DR_ALIGNED_TO (dr);
  base_addr = DR_BASE_ADDRESS (dr);
  vectype = STMT_VINFO_VECTYPE (stmt_info);

  /* In case the dataref is in an inner-loop of the loop that is being
     vectorized (LOOP), we use the base and misalignment information
     relative to the outer-loop (LOOP). This is ok only if the misalignment
     stays the same throughout the execution of the inner-loop, which is why
     we have to check that the stride of the dataref in the inner-loop evenly
     divides by the vector size.  */
  if (nested_in_vect_loop_p (loop, stmt))
    {
      tree step = DR_STEP (dr);
      HOST_WIDE_INT dr_step = TREE_INT_CST_LOW (step);
    
      if (dr_step % GET_MODE_SIZE (TYPE_MODE (vectype)) == 0)
        {
          if (vect_print_dump_info (REPORT_ALIGNMENT))
            fprintf (vect_dump, "inner step divides the vector-size.");
	  misalign = STMT_VINFO_DR_INIT (stmt_info);
	  aligned_to = STMT_VINFO_DR_ALIGNED_TO (stmt_info);
	  base_addr = STMT_VINFO_DR_BASE_ADDRESS (stmt_info);
        }
      else
	{
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
	    fprintf (vect_dump, "inner step doesn't divide the vector-size.");
	  misalign = NULL_TREE;
	}
    }

  base = build_fold_indirect_ref (base_addr);
  alignment = ssize_int (TYPE_ALIGN (vectype)/BITS_PER_UNIT);

  if ((aligned_to && tree_int_cst_compare (aligned_to, alignment) < 0)
      || !misalign)
    {
      if (vect_print_dump_info (REPORT_ALIGNMENT))
	{
	  fprintf (vect_dump, "Unknown alignment for access: ");
	  print_generic_expr (vect_dump, base, TDF_SLIM);
	}
      return true;
    }

  if ((DECL_P (base) 
       && tree_int_cst_compare (ssize_int (DECL_ALIGN_UNIT (base)),
				alignment) >= 0)
      || (TREE_CODE (base_addr) == SSA_NAME
	  && tree_int_cst_compare (ssize_int (TYPE_ALIGN_UNIT (TREE_TYPE (
						      TREE_TYPE (base_addr)))),
				   alignment) >= 0))
    base_aligned = true;
  else
    base_aligned = false;   

  if (!base_aligned) 
    {
      /* Do not change the alignment of global variables if 
	 flag_section_anchors is enabled.  */
      if (!vect_can_force_dr_alignment_p (base, TYPE_ALIGN (vectype))
	  || (TREE_STATIC (base) && flag_section_anchors))
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "can't force alignment of ref: ");
	      print_generic_expr (vect_dump, ref, TDF_SLIM);
	    }
	  return true;
	}
      
      /* Force the alignment of the decl.
	 NOTE: This is the only change to the code we make during
	 the analysis phase, before deciding to vectorize the loop.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "force alignment");
      DECL_ALIGN (base) = TYPE_ALIGN (vectype);
      DECL_USER_ALIGN (base) = 1;
    }

  /* At this point we assume that the base is aligned.  */
  gcc_assert (base_aligned
	      || (TREE_CODE (base) == VAR_DECL 
		  && DECL_ALIGN (base) >= TYPE_ALIGN (vectype)));

  /* Modulo alignment.  */
  misalign = size_binop (TRUNC_MOD_EXPR, misalign, alignment);

  if (!host_integerp (misalign, 1))
    {
      /* Negative or overflowed misalignment value.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "unexpected misalign value");
      return false;
    }

  SET_DR_MISALIGNMENT (dr, TREE_INT_CST_LOW (misalign));

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "misalign = %d bytes of ref ", DR_MISALIGNMENT (dr));
      print_generic_expr (vect_dump, ref, TDF_SLIM);
    }

  return true;
}


/* Function vect_compute_data_refs_alignment

   Compute the misalignment of data references in the loop.
   Return FALSE if a data reference is found that cannot be vectorized.  */

static bool
vect_compute_data_refs_alignment (loop_vec_info loop_vinfo)
{
  VEC (data_reference_p, heap) *datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct data_reference *dr;
  unsigned int i;

  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    if (!vect_compute_data_ref_alignment (dr))
      return false;

  return true;
}


/* Function vect_update_misalignment_for_peel

   DR - the data reference whose misalignment is to be adjusted.
   DR_PEEL - the data reference whose misalignment is being made
             zero in the vector loop by the peel.
   NPEEL - the number of iterations in the peel loop if the misalignment
           of DR_PEEL is known at compile time.  */

static void
vect_update_misalignment_for_peel (struct data_reference *dr,
                                   struct data_reference *dr_peel, int npeel)
{
  unsigned int i;
  VEC(dr_p,heap) *same_align_drs;
  struct data_reference *current_dr;
  int dr_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr))));
  int dr_peel_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr_peel))));
  stmt_vec_info stmt_info = vinfo_for_stmt (DR_STMT (dr));
  stmt_vec_info peel_stmt_info = vinfo_for_stmt (DR_STMT (dr_peel));

 /* For interleaved data accesses the step in the loop must be multiplied by
     the size of the interleaving group.  */
  if (STMT_VINFO_STRIDED_ACCESS (stmt_info))
    dr_size *= DR_GROUP_SIZE (vinfo_for_stmt (DR_GROUP_FIRST_DR (stmt_info)));
  if (STMT_VINFO_STRIDED_ACCESS (peel_stmt_info))
    dr_peel_size *= DR_GROUP_SIZE (peel_stmt_info);

  /* It can be assumed that the data refs with the same alignment as dr_peel
     are aligned in the vector loop.  */
  same_align_drs
    = STMT_VINFO_SAME_ALIGN_REFS (vinfo_for_stmt (DR_STMT (dr_peel)));
  for (i = 0; VEC_iterate (dr_p, same_align_drs, i, current_dr); i++)
    {
      if (current_dr != dr)
        continue;
      gcc_assert (DR_MISALIGNMENT (dr) / dr_size ==
                  DR_MISALIGNMENT (dr_peel) / dr_peel_size);
      SET_DR_MISALIGNMENT (dr, 0);
      return;
    }

  if (known_alignment_for_access_p (dr)
      && known_alignment_for_access_p (dr_peel))
    {
      int misal = DR_MISALIGNMENT (dr);
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      misal += npeel * dr_size;
      misal %= GET_MODE_SIZE (TYPE_MODE (vectype));
      SET_DR_MISALIGNMENT (dr, misal);
      return;
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "Setting misalignment to -1.");
  SET_DR_MISALIGNMENT (dr, -1);
}


/* Function vect_verify_datarefs_alignment

   Return TRUE if all data references in the loop can be
   handled with respect to alignment.  */

static bool
vect_verify_datarefs_alignment (loop_vec_info loop_vinfo)
{
  VEC (data_reference_p, heap) *datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct data_reference *dr;
  enum dr_alignment_support supportable_dr_alignment;
  unsigned int i;

  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    {
      gimple stmt = DR_STMT (dr);
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

      /* For interleaving, only the alignment of the first access matters.  */
      if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
          && DR_GROUP_FIRST_DR (stmt_info) != stmt)
        continue;

      supportable_dr_alignment = vect_supportable_dr_alignment (dr);
      if (!supportable_dr_alignment)
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
            {
              if (DR_IS_READ (dr))
                fprintf (vect_dump, 
                         "not vectorized: unsupported unaligned load.");
              else
                fprintf (vect_dump, 
                         "not vectorized: unsupported unaligned store.");
            }
          return false;
        }
      if (supportable_dr_alignment != dr_aligned
          && vect_print_dump_info (REPORT_ALIGNMENT))
        fprintf (vect_dump, "Vectorizing an unaligned access.");
    }
  return true;
}


/* Function vector_alignment_reachable_p

   Return true if vector alignment for DR is reachable by peeling
   a few loop iterations.  Return false otherwise.  */

static bool
vector_alignment_reachable_p (struct data_reference *dr)
{
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (STMT_VINFO_STRIDED_ACCESS (stmt_info))
    {
      /* For interleaved access we peel only if number of iterations in
	 the prolog loop ({VF - misalignment}), is a multiple of the
	 number of the interleaved accesses.  */
      int elem_size, mis_in_elements;
      int nelements = TYPE_VECTOR_SUBPARTS (vectype);

      /* FORNOW: handle only known alignment.  */
      if (!known_alignment_for_access_p (dr))
	return false;

      elem_size = GET_MODE_SIZE (TYPE_MODE (vectype)) / nelements;
      mis_in_elements = DR_MISALIGNMENT (dr) / elem_size;

      if ((nelements - mis_in_elements) % DR_GROUP_SIZE (stmt_info))
	return false;
    }

  /* If misalignment is known at the compile time then allow peeling
     only if natural alignment is reachable through peeling.  */
  if (known_alignment_for_access_p (dr) && !aligned_access_p (dr))
    {
      HOST_WIDE_INT elmsize = 
		int_cst_value (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
      if (vect_print_dump_info (REPORT_DETAILS))
	{
	  fprintf (vect_dump, "data size =" HOST_WIDE_INT_PRINT_DEC, elmsize);
	  fprintf (vect_dump, ". misalignment = %d. ", DR_MISALIGNMENT (dr));
	}
      if (DR_MISALIGNMENT (dr) % elmsize)
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "data size does not divide the misalignment.\n");
	  return false;
	}
    }

  if (!known_alignment_for_access_p (dr))
    {
      tree type = (TREE_TYPE (DR_REF (dr)));
      tree ba = DR_BASE_OBJECT (dr);
      bool is_packed = false;

      if (ba)
	is_packed = contains_packed_reference (ba);

      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "Unknown misalignment, is_packed = %d",is_packed);
      if (targetm.vectorize.vector_alignment_reachable (type, is_packed))
	return true;
      else
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
   vectorized. This restriction will be relaxed.

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

   Here are the general steps involved in alignment enhancements:

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
	for (i = 3; i<N; i++){	# loop 3A
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
     misalignment).  */

static bool
vect_enhance_data_refs_alignment (loop_vec_info loop_vinfo)
{
  VEC (data_reference_p, heap) *datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  enum dr_alignment_support supportable_dr_alignment;
  struct data_reference *dr0 = NULL;
  struct data_reference *dr;
  unsigned int i;
  bool do_peeling = false;
  bool do_versioning = false;
  bool stat;
  gimple stmt;
  stmt_vec_info stmt_info;
  int vect_versioning_for_alias_required;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_enhance_data_refs_alignment ===");

  /* While cost model enhancements are expected in the future, the high level
     view of the code at this time is as follows:

     A) If there is a misaligned write then see if peeling to align this write
        can make all data references satisfy vect_supportable_dr_alignment.
        If so, update data structures as needed and return true.  Note that
        at this time vect_supportable_dr_alignment is known to return false
        for a misaligned write.

     B) If peeling wasn't possible and there is a data reference with an
        unknown misalignment that does not satisfy vect_supportable_dr_alignment
        then see if loop versioning checks can be used to make all data
        references satisfy vect_supportable_dr_alignment.  If so, update
        data structures as needed and return true.

     C) If neither peeling nor versioning were successful then return false if
        any data reference does not satisfy vect_supportable_dr_alignment.

     D) Return true (all data references satisfy vect_supportable_dr_alignment).

     Note, Possibility 3 above (which is peeling and versioning together) is not
     being done at this time.  */

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

     TODO: Use a cost model.  */

  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    {
      stmt = DR_STMT (dr);
      stmt_info = vinfo_for_stmt (stmt);

      /* For interleaving, only the alignment of the first access
         matters.  */
      if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
          && DR_GROUP_FIRST_DR (stmt_info) != stmt)
        continue;

      if (!DR_IS_READ (dr) && !aligned_access_p (dr))
        {
	  do_peeling = vector_alignment_reachable_p (dr);
	  if (do_peeling)
	    dr0 = dr;
	  if (!do_peeling && vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "vector alignment may not be reachable");
	  break;
	}
    }

  vect_versioning_for_alias_required =
    (VEC_length (ddr_p, LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo)) > 0);

  /* Temporarily, if versioning for alias is required, we disable peeling
     until we support peeling and versioning.  Often peeling for alignment
     will require peeling for loop-bound, which in turn requires that we
     know how to adjust the loop ivs after the loop.  */
  if (vect_versioning_for_alias_required
       || !vect_can_advance_ivs_p (loop_vinfo)
      || !slpeel_can_duplicate_loop_p (loop, single_exit (loop)))
    do_peeling = false;

  if (do_peeling)
    {
      int mis;
      int npeel = 0;
      gimple stmt = DR_STMT (dr0);
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      int nelements = TYPE_VECTOR_SUBPARTS (vectype);

      if (known_alignment_for_access_p (dr0))
        {
          /* Since it's known at compile time, compute the number of iterations
             in the peeled loop (the peeling factor) for use in updating
             DR_MISALIGNMENT values.  The peeling factor is the vectorization
             factor minus the misalignment as an element count.  */
          mis = DR_MISALIGNMENT (dr0);
          mis /= GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr0))));
          npeel = nelements - mis;

	  /* For interleaved data access every iteration accesses all the 
	     members of the group, therefore we divide the number of iterations
	     by the group size.  */
	  stmt_info = vinfo_for_stmt (DR_STMT (dr0));	  
	  if (STMT_VINFO_STRIDED_ACCESS (stmt_info))
	    npeel /= DR_GROUP_SIZE (stmt_info);

          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "Try peeling by %d", npeel);
        }

      /* Ensure that all data refs can be vectorized after the peel.  */
      for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
        {
          int save_misalignment;

	  if (dr == dr0)
	    continue;

	  stmt = DR_STMT (dr);
	  stmt_info = vinfo_for_stmt (stmt);
	  /* For interleaving, only the alignment of the first access
            matters.  */
	  if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
	      && DR_GROUP_FIRST_DR (stmt_info) != stmt)
	    continue;

	  save_misalignment = DR_MISALIGNMENT (dr);
	  vect_update_misalignment_for_peel (dr, dr0, npeel);
	  supportable_dr_alignment = vect_supportable_dr_alignment (dr);
	  SET_DR_MISALIGNMENT (dr, save_misalignment);
	  
	  if (!supportable_dr_alignment)
	    {
	      do_peeling = false;
	      break;
	    }
	}

      if (do_peeling)
        {
          /* (1.2) Update the DR_MISALIGNMENT of each data reference DR_i.
             If the misalignment of DR_i is identical to that of dr0 then set
             DR_MISALIGNMENT (DR_i) to zero.  If the misalignment of DR_i and
             dr0 are known at compile time then increment DR_MISALIGNMENT (DR_i)
             by the peeling factor times the element size of DR_i (MOD the
             vectorization factor times the size).  Otherwise, the
             misalignment of DR_i must be set to unknown.  */
	  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
	    if (dr != dr0)
	      vect_update_misalignment_for_peel (dr, dr0, npeel);

          LOOP_VINFO_UNALIGNED_DR (loop_vinfo) = dr0;
          LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo) = DR_MISALIGNMENT (dr0);
	  SET_DR_MISALIGNMENT (dr0, 0);
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
            fprintf (vect_dump, "Alignment of access forced using peeling.");

          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "Peeling for alignment will be applied.");

	  stat = vect_verify_datarefs_alignment (loop_vinfo);
	  gcc_assert (stat);
          return stat;
        }
    }


  /* (2) Versioning to force alignment.  */

  /* Try versioning if:
     1) flag_tree_vect_loop_version is TRUE
     2) optimize loop for speed
     3) there is at least one unsupported misaligned data ref with an unknown
        misalignment, and
     4) all misaligned data refs with a known misalignment are supported, and
     5) the number of runtime alignment checks is within reason.  */

  do_versioning = 
	flag_tree_vect_loop_version 
	&& optimize_loop_nest_for_speed_p (loop)
	&& (!loop->inner); /* FORNOW */

  if (do_versioning)
    {
      for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
        {
	  stmt = DR_STMT (dr);
	  stmt_info = vinfo_for_stmt (stmt);

	  /* For interleaving, only the alignment of the first access
	     matters.  */
	  if (aligned_access_p (dr)
	      || (STMT_VINFO_STRIDED_ACCESS (stmt_info)
		  && DR_GROUP_FIRST_DR (stmt_info) != stmt))
	    continue;

	  supportable_dr_alignment = vect_supportable_dr_alignment (dr);

          if (!supportable_dr_alignment)
            {
              gimple stmt;
              int mask;
              tree vectype;

              if (known_alignment_for_access_p (dr)
                  || VEC_length (gimple,
                                 LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo))
                     >= (unsigned) PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIGNMENT_CHECKS))
                {
                  do_versioning = false;
                  break;
                }

              stmt = DR_STMT (dr);
              vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
              gcc_assert (vectype);
  
              /* The rightmost bits of an aligned address must be zeros.
                 Construct the mask needed for this test.  For example,
                 GET_MODE_SIZE for the vector mode V4SI is 16 bytes so the
                 mask must be 15 = 0xf. */
              mask = GET_MODE_SIZE (TYPE_MODE (vectype)) - 1;

              /* FORNOW: use the same mask to test all potentially unaligned
                 references in the loop.  The vectorizer currently supports
                 a single vector size, see the reference to
                 GET_MODE_NUNITS (TYPE_MODE (vectype)) where the
                 vectorization factor is computed.  */
              gcc_assert (!LOOP_VINFO_PTR_MASK (loop_vinfo)
                          || LOOP_VINFO_PTR_MASK (loop_vinfo) == mask);
              LOOP_VINFO_PTR_MASK (loop_vinfo) = mask;
              VEC_safe_push (gimple, heap,
                             LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo),
                             DR_STMT (dr));
            }
        }
      
      /* Versioning requires at least one misaligned data reference.  */
      if (VEC_length (gimple, LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo)) == 0)
        do_versioning = false;
      else if (!do_versioning)
        VEC_truncate (gimple, LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo), 0);
    }

  if (do_versioning)
    {
      VEC(gimple,heap) *may_misalign_stmts
        = LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo);
      gimple stmt;

      /* It can now be assumed that the data references in the statements
         in LOOP_VINFO_MAY_MISALIGN_STMTS will be aligned in the version
         of the loop being vectorized.  */
      for (i = 0; VEC_iterate (gimple, may_misalign_stmts, i, stmt); i++)
        {
          stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
          dr = STMT_VINFO_DATA_REF (stmt_info);
	  SET_DR_MISALIGNMENT (dr, 0);
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
            fprintf (vect_dump, "Alignment of access forced using versioning.");
        }

      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "Versioning for alignment will be applied.");

      /* Peeling and versioning can't be done together at this time.  */
      gcc_assert (! (do_peeling && do_versioning));

      stat = vect_verify_datarefs_alignment (loop_vinfo);
      gcc_assert (stat);
      return stat;
    }

  /* This point is reached if neither peeling nor versioning is being done.  */
  gcc_assert (! (do_peeling || do_versioning));

  stat = vect_verify_datarefs_alignment (loop_vinfo);
  return stat;
}


/* Function vect_analyze_data_refs_alignment

   Analyze the alignment of the data-references in the loop.
   Return FALSE if a data reference is found that cannot be vectorized.  */

static bool
vect_analyze_data_refs_alignment (loop_vec_info loop_vinfo)
{
  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_data_refs_alignment ===");

  if (!vect_compute_data_refs_alignment (loop_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
	fprintf (vect_dump, 
		 "not vectorized: can't calculate alignment for data ref.");
      return false;
    }

  return true;
}


/* Analyze groups of strided accesses: check that DR belongs to a group of
   strided accesses of legal size, step, etc. Detect gaps, single element
   interleaving, and other special cases. Set strided access info.
   Collect groups of strided stores for further use in SLP analysis.  */

static bool
vect_analyze_group_access (struct data_reference *dr)
{
  tree step = DR_STEP (dr);
  tree scalar_type = TREE_TYPE (DR_REF (dr));
  HOST_WIDE_INT type_size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (scalar_type));
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  HOST_WIDE_INT dr_step = TREE_INT_CST_LOW (step);
  HOST_WIDE_INT stride;
  bool slp_impossible = false;

  /* For interleaving, STRIDE is STEP counted in elements, i.e., the size of the 
     interleaving group (including gaps).  */
  stride = dr_step / type_size; 

  /* Not consecutive access is possible only if it is a part of interleaving.  */
  if (!DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)))
    {
      /* Check if it this DR is a part of interleaving, and is a single
	 element of the group that is accessed in the loop.  */
      
      /* Gaps are supported only for loads. STEP must be a multiple of the type
	 size.  The size of the group must be a power of 2.  */
      if (DR_IS_READ (dr)
	  && (dr_step % type_size) == 0
	  && stride > 0
	  && exact_log2 (stride) != -1)
	{
	  DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)) = stmt;
	  DR_GROUP_SIZE (vinfo_for_stmt (stmt)) = stride;
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "Detected single element interleaving %d ",
		       DR_GROUP_SIZE (vinfo_for_stmt (stmt)));
	      print_generic_expr (vect_dump, DR_REF (dr), TDF_SLIM);
	      fprintf (vect_dump, " step ");
	      print_generic_expr (vect_dump, step, TDF_SLIM);
	    }
	  return true;
	}
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "not consecutive access");
      return false;
    }

  if (DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)) == stmt)
    {
      /* First stmt in the interleaving chain. Check the chain.  */
      gimple next = DR_GROUP_NEXT_DR (vinfo_for_stmt (stmt));
      struct data_reference *data_ref = dr;
      unsigned int count = 1;
      tree next_step;
      tree prev_init = DR_INIT (data_ref);
      gimple prev = stmt;
      HOST_WIDE_INT diff, count_in_bytes, gaps = 0;

      while (next)
        {
          /* Skip same data-refs. In case that two or more stmts share data-ref
             (supported only for loads), we vectorize only the first stmt, and
             the rest get their vectorized loads from the first one.  */
          if (!tree_int_cst_compare (DR_INIT (data_ref),
                                     DR_INIT (STMT_VINFO_DATA_REF (
						   vinfo_for_stmt (next)))))
            {
              if (!DR_IS_READ (data_ref))
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    fprintf (vect_dump, "Two store stmts share the same dr.");
                  return false;
                }

              /* Check that there is no load-store dependencies for this loads
                 to prevent a case of load-store-load to the same location.  */
              if (DR_GROUP_READ_WRITE_DEPENDENCE (vinfo_for_stmt (next))
                  || DR_GROUP_READ_WRITE_DEPENDENCE (vinfo_for_stmt (prev)))
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    fprintf (vect_dump,
                             "READ_WRITE dependence in interleaving.");
                  return false;
                }

              /* For load use the same data-ref load.  */
              DR_GROUP_SAME_DR_STMT (vinfo_for_stmt (next)) = prev;

              prev = next;
              next = DR_GROUP_NEXT_DR (vinfo_for_stmt (next));
              continue;
            }
          prev = next;

          /* Check that all the accesses have the same STEP.  */
          next_step = DR_STEP (STMT_VINFO_DATA_REF (vinfo_for_stmt (next)));
          if (tree_int_cst_compare (step, next_step))
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "not consecutive access in interleaving");
              return false;
            }

          data_ref = STMT_VINFO_DATA_REF (vinfo_for_stmt (next));
          /* Check that the distance between two accesses is equal to the type
             size. Otherwise, we have gaps.  */
          diff = (TREE_INT_CST_LOW (DR_INIT (data_ref))
                  - TREE_INT_CST_LOW (prev_init)) / type_size;
	  if (diff != 1)
	    {
	      /* FORNOW: SLP of accesses with gaps is not supported.  */
	      slp_impossible = true;
	      if (!DR_IS_READ (data_ref))
		{
		  if (vect_print_dump_info (REPORT_DETAILS))
		    fprintf (vect_dump, "interleaved store with gaps");
		  return false;
		}

              gaps += diff - 1;
	    }

          /* Store the gap from the previous member of the group. If there is no
             gap in the access, DR_GROUP_GAP is always 1.  */
          DR_GROUP_GAP (vinfo_for_stmt (next)) = diff;

          prev_init = DR_INIT (data_ref);
          next = DR_GROUP_NEXT_DR (vinfo_for_stmt (next));
          /* Count the number of data-refs in the chain.  */
          count++;
        }

      /* COUNT is the number of accesses found, we multiply it by the size of
         the type to get COUNT_IN_BYTES.  */
      count_in_bytes = type_size * count;

      /* Check that the size of the interleaving (including gaps) is not greater
         than STEP.  */
      if (dr_step && dr_step < count_in_bytes + gaps * type_size)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            {
              fprintf (vect_dump, "interleaving size is greater than step for ");
              print_generic_expr (vect_dump, DR_REF (dr), TDF_SLIM);
            }
          return false;
        }

      /* Check that the size of the interleaving is equal to STEP for stores,
         i.e., that there are no gaps.  */
      if (dr_step != count_in_bytes)
        {
          if (DR_IS_READ (dr))
            {
              slp_impossible = true;
              /* There is a gap after the last load in the group. This gap is a
                 difference between the stride and the number of elements. When 
                 there is no gap, this difference should be 0.  */ 
              DR_GROUP_GAP (vinfo_for_stmt (stmt)) = stride - count; 
            }
          else
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "interleaved store with gaps");
              return false;
            }
        }

      /* Check that STEP is a multiple of type size.  */
      if ((dr_step % type_size) != 0)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            {
              fprintf (vect_dump, "step is not a multiple of type size: step ");
              print_generic_expr (vect_dump, step, TDF_SLIM);
              fprintf (vect_dump, " size ");
              print_generic_expr (vect_dump, TYPE_SIZE_UNIT (scalar_type),
                                  TDF_SLIM);
            }
          return false;
        }

      /* FORNOW: we handle only interleaving that is a power of 2.  
         We don't fail here if it may be still possible to vectorize the
         group using SLP. If not, the size of the group will be checked in
         vect_analyze_operations, and the vectorization will fail.  */
      if (exact_log2 (stride) == -1)
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "interleaving is not a power of 2");

	  if (slp_impossible)
	    return false;
	}
      DR_GROUP_SIZE (vinfo_for_stmt (stmt)) = stride;
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "Detected interleaving of size %d", (int)stride);

      /* SLP: create an SLP data structure for every interleaving group of 
	 stores for further analysis in vect_analyse_slp.  */
      if (!DR_IS_READ (dr) && !slp_impossible)
	VEC_safe_push (gimple, heap, LOOP_VINFO_STRIDED_STORES (loop_vinfo), stmt);
    }

  return true;
}


/* Analyze the access pattern of the data-reference DR.
   In case of non-consecutive accesses call vect_analyze_group_access() to
   analyze groups of strided accesses.  */

static bool
vect_analyze_data_ref_access (struct data_reference *dr)
{
  tree step = DR_STEP (dr);
  tree scalar_type = TREE_TYPE (DR_REF (dr));
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  HOST_WIDE_INT dr_step = TREE_INT_CST_LOW (step);

  if (!step)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data-ref access");
      return false;
    }

  /* Don't allow invariant accesses.  */
  if (dr_step == 0)
    return false; 

  if (nested_in_vect_loop_p (loop, stmt))
    {
      /* Interleaved accesses are not yet supported within outer-loop
        vectorization for references in the inner-loop.  */
      DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)) = NULL;

      /* For the rest of the analysis we use the outer-loop step.  */
      step = STMT_VINFO_DR_STEP (stmt_info);
      dr_step = TREE_INT_CST_LOW (step);
      
      if (dr_step == 0)
	{
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
	    fprintf (vect_dump, "zero step in outer loop.");
	  if (DR_IS_READ (dr))
  	    return true; 
	  else
	    return false;
	}
    }

  /* Consecutive?  */
  if (!tree_int_cst_compare (step, TYPE_SIZE_UNIT (scalar_type)))
    {
      /* Mark that it is not interleaving.  */
      DR_GROUP_FIRST_DR (vinfo_for_stmt (stmt)) = NULL;
      return true;
    }

  if (nested_in_vect_loop_p (loop, stmt))
    {
      if (vect_print_dump_info (REPORT_ALIGNMENT))
	fprintf (vect_dump, "strided access in outer loop.");
      return false;
    }

  /* Not consecutive access - check if it's a part of interleaving group.  */
  return vect_analyze_group_access (dr);
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
  VEC (data_reference_p, heap) *datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct data_reference *dr;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_data_ref_accesses ===");

  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    if (!vect_analyze_data_ref_access (dr))
      {
	if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
	  fprintf (vect_dump, "not vectorized: complicated access pattern.");
	return false;
      }

  return true;
}

/* Function vect_prune_runtime_alias_test_list.

   Prune a list of ddrs to be tested at run-time by versioning for alias.
   Return FALSE if resulting list of ddrs is longer then allowed by
   PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS, otherwise return TRUE.  */

static bool
vect_prune_runtime_alias_test_list (loop_vec_info loop_vinfo)
{
  VEC (ddr_p, heap) * ddrs =
    LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo);
  unsigned i, j;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_prune_runtime_alias_test_list ===");

  for (i = 0; i < VEC_length (ddr_p, ddrs); )
    {
      bool found;
      ddr_p ddr_i;

      ddr_i = VEC_index (ddr_p, ddrs, i);
      found = false;

      for (j = 0; j < i; j++)
        {
	  ddr_p ddr_j = VEC_index (ddr_p, ddrs, j);

	  if (vect_vfa_range_equal (ddr_i, ddr_j))
	    {
	      if (vect_print_dump_info (REPORT_DR_DETAILS))
		{
		  fprintf (vect_dump, "found equal ranges ");
		  print_generic_expr (vect_dump, DR_REF (DDR_A (ddr_i)), TDF_SLIM);
		  fprintf (vect_dump, ", ");
		  print_generic_expr (vect_dump, DR_REF (DDR_B (ddr_i)), TDF_SLIM);
		  fprintf (vect_dump, " and ");
		  print_generic_expr (vect_dump, DR_REF (DDR_A (ddr_j)), TDF_SLIM);
		  fprintf (vect_dump, ", ");
		  print_generic_expr (vect_dump, DR_REF (DDR_B (ddr_j)), TDF_SLIM);
		}
	      found = true;
	      break;
	    }
	}
      
      if (found)
      {
	VEC_ordered_remove (ddr_p, ddrs, i);
	continue;
      }
      i++;
    }

  if (VEC_length (ddr_p, ddrs) >
       (unsigned) PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS))
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
	{
	  fprintf (vect_dump,
		   "disable versioning for alias - max number of generated "
		   "checks exceeded.");
	}

      VEC_truncate (ddr_p, LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo), 0);

      return false;
    }

  return true;
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
vect_get_and_check_slp_defs (loop_vec_info loop_vinfo, slp_tree slp_node,
			     gimple stmt, VEC (gimple, heap) **def_stmts0,
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
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  rhs_class = get_gimple_rhs_class (gimple_assign_rhs_code (stmt));
  number_of_oprnds = gimple_num_ops (stmt) - 1;	/* RHS only */

  for (i = 0; i < number_of_oprnds; i++)
    {
      oprnd = gimple_op (stmt, i + 1);

      if (!vect_is_simple_use (oprnd, loop_vinfo, &def_stmt, &def, &dt[i])
	  || (!def_stmt && dt[i] != vect_constant_def))
	{
	  if (vect_print_dump_info (REPORT_SLP)) 
	    {
	      fprintf (vect_dump, "Build SLP failed: can't find def for ");
	      print_generic_expr (vect_dump, oprnd, TDF_SLIM);
	    }

	  return false;
	}

      /* Check if DEF_STMT is a part of a pattern and get the def stmt from
         the pattern. Check that all the stmts of the node are in the
         pattern.  */
      if (def_stmt && gimple_bb (def_stmt)
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
	case vect_invariant_def:
	  break;
	  
	case vect_loop_def:
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
vect_build_slp_tree (loop_vec_info loop_vinfo, slp_tree *node, 
		     unsigned int group_size, 
		     int *inside_cost, int *outside_cost,
		     int ncopies_for_cost, unsigned int *max_nunits,
                     VEC (int, heap) **load_permutation,
                     VEC (slp_tree, heap) **loads)
{
  VEC (gimple, heap) *def_stmts0 = VEC_alloc (gimple, heap, group_size);
  VEC (gimple, heap) *def_stmts1 =  VEC_alloc (gimple, heap, group_size);
  unsigned int i;
  VEC (gimple, heap) *stmts = SLP_TREE_SCALAR_STMTS (*node);
  gimple stmt = VEC_index (gimple, stmts, 0);
  enum vect_def_type first_stmt_dt0 = 0, first_stmt_dt1 = 0;
  enum tree_code first_stmt_code = 0, rhs_code;
  tree first_stmt_def1_type = NULL_TREE, first_stmt_def0_type = NULL_TREE;
  tree lhs;
  bool stop_recursion = false, need_same_oprnds = false;
  tree vectype, scalar_type, first_op1 = NULL_TREE;
  unsigned int vectorization_factor = 0, ncopies;
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

      gcc_assert (LOOP_VINFO_VECT_FACTOR (loop_vinfo));
      vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
      ncopies = vectorization_factor / TYPE_VECTOR_SUBPARTS (vectype);
      if (ncopies > 1 && vect_print_dump_info (REPORT_SLP))
        fprintf (vect_dump, "SLP with multiple types ");

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
	      if (!vect_get_and_check_slp_defs (loop_vinfo, *node, stmt,
						&def_stmts0, &def_stmts1, 
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
	  if (!vect_get_and_check_slp_defs (loop_vinfo, *node, stmt,
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
  if (first_stmt_dt0 == vect_loop_def)
    {
      slp_tree left_node = XNEW (struct _slp_tree);
      SLP_TREE_SCALAR_STMTS (left_node) = def_stmts0;
      SLP_TREE_VEC_STMTS (left_node) = NULL;
      SLP_TREE_LEFT (left_node) = NULL;
      SLP_TREE_RIGHT (left_node) = NULL;
      SLP_TREE_OUTSIDE_OF_LOOP_COST (left_node) = 0;
      SLP_TREE_INSIDE_OF_LOOP_COST (left_node) = 0;
      if (!vect_build_slp_tree (loop_vinfo, &left_node, group_size, 
				inside_cost, outside_cost, ncopies_for_cost, 
				max_nunits, load_permutation, loads))
	return false;
      
      SLP_TREE_LEFT (*node) = left_node;
    }

  if (first_stmt_dt1 == vect_loop_def)
    {
      slp_tree right_node = XNEW (struct _slp_tree);
      SLP_TREE_SCALAR_STMTS (right_node) = def_stmts1;
      SLP_TREE_VEC_STMTS (right_node) = NULL;
      SLP_TREE_LEFT (right_node) = NULL;
      SLP_TREE_RIGHT (right_node) = NULL;
      SLP_TREE_OUTSIDE_OF_LOOP_COST (right_node) = 0;
      SLP_TREE_INSIDE_OF_LOOP_COST (right_node) = 0;
      if (!vect_build_slp_tree (loop_vinfo, &right_node, group_size,
				inside_cost, outside_cost, ncopies_for_cost,
				max_nunits, load_permutation, loads))
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

  /* FORNOW: permutations are only supported for loop-aware SLP.  */
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
vect_analyze_slp_instance (loop_vec_info loop_vinfo, gimple stmt)
{
  slp_instance new_instance;
  slp_tree node = XNEW (struct _slp_tree);
  unsigned int group_size = DR_GROUP_SIZE (vinfo_for_stmt (stmt));
  unsigned int unrolling_factor = 1, nunits;
  tree vectype, scalar_type;
  gimple next;
  unsigned int vectorization_factor = 0, ncopies;
  bool slp_impossible = false; 
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
  vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  ncopies = vectorization_factor / nunits;

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

  /* Calculate the unrolling factor.  */
  unrolling_factor = least_common_multiple (nunits, group_size) / group_size;
	
  /* Calculate the number of vector stmts to create based on the unrolling
     factor (number of vectors is 1 if NUNITS >= GROUP_SIZE, and is
     GROUP_SIZE / NUNITS otherwise.  */
  ncopies_for_cost = unrolling_factor * group_size / nunits;
  
  load_permutation = VEC_alloc (int, heap, group_size * group_size); 
  loads = VEC_alloc (slp_tree, heap, group_size); 

  /* Build the tree for the SLP instance.  */
  if (vect_build_slp_tree (loop_vinfo, &node, group_size, &inside_cost,  
			   &outside_cost, ncopies_for_cost, &max_nunits,
                           &load_permutation, &loads))
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

      VEC_safe_push (slp_instance, heap, LOOP_VINFO_SLP_INSTANCES (loop_vinfo), 
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
   
  if (slp_impossible)
    return false;

  /* SLP failed for this instance, but it is still possible to SLP other stmts 
     in the loop.  */
  return true;
}


/* Check if there are stmts in the loop can be vectorized using SLP. Build SLP
   trees of packed scalar stmts if SLP is possible.  */

static bool
vect_analyze_slp (loop_vec_info loop_vinfo)
{
  unsigned int i;
  VEC (gimple, heap) *strided_stores = LOOP_VINFO_STRIDED_STORES (loop_vinfo);
  gimple store;

  if (vect_print_dump_info (REPORT_SLP))
    fprintf (vect_dump, "=== vect_analyze_slp ===");

  for (i = 0; VEC_iterate (gimple, strided_stores, i, store); i++)
    if (!vect_analyze_slp_instance (loop_vinfo, store))
      {
	/* SLP failed. No instance can be SLPed in the loop.  */
	if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))	
	  fprintf (vect_dump, "SLP failed.");

	return false;
      }

  return true;
}


/* For each possible SLP instance decide whether to SLP it and calculate overall
   unrolling factor needed to SLP the loop.  */

static void
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
            && (STMT_VINFO_RELEVANT (vinfo_for_stmt (use_stmt))
                || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (use_stmt)) 
                    == vect_reduction_def))
	  vect_mark_slp_stmts (node, hybrid, i);

  vect_detect_hybrid_slp_stmts (SLP_TREE_LEFT (node));
  vect_detect_hybrid_slp_stmts (SLP_TREE_RIGHT (node));
}


/* Find stmts that must be both vectorized and SLPed.  */

static void
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


/* Function vect_analyze_data_refs.

  Find all the data references in the loop.

   The general structure of the analysis of data refs in the vectorizer is as
   follows:
   1- vect_analyze_data_refs(loop): call compute_data_dependences_for_loop to
      find and analyze all data-refs in the loop and their dependences.
   2- vect_analyze_dependences(): apply dependence testing using ddrs.
   3- vect_analyze_drs_alignment(): check that ref_stmt.alignment is ok.
   4- vect_analyze_drs_access(): check that ref_stmt.step is ok.

*/

static bool
vect_analyze_data_refs (loop_vec_info loop_vinfo)  
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  unsigned int i;
  VEC (data_reference_p, heap) *datarefs;
  struct data_reference *dr;
  tree scalar_type;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_data_refs ===\n");

  compute_data_dependences_for_loop (loop, true,
                                     &LOOP_VINFO_DATAREFS (loop_vinfo),
                                     &LOOP_VINFO_DDRS (loop_vinfo));

  /* Go through the data-refs, check that the analysis succeeded. Update pointer
     from stmt_vec_info struct to DR and vectype.  */
  datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);

  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    {
      gimple stmt;
      stmt_vec_info stmt_info;
      basic_block bb;
      tree base, offset, init;	
   
      if (!dr || !DR_REF (dr))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
	    fprintf (vect_dump, "not vectorized: unhandled data-ref ");
          return false;
        }

      stmt = DR_STMT (dr);
      stmt_info = vinfo_for_stmt (stmt);

      /* Check that analysis of the data-ref succeeded.  */
      if (!DR_BASE_ADDRESS (dr) || !DR_OFFSET (dr) || !DR_INIT (dr)
          || !DR_STEP (dr))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
            {
              fprintf (vect_dump, "not vectorized: data ref analysis failed ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
            }
          return false;
        }

      if (TREE_CODE (DR_BASE_ADDRESS (dr)) == INTEGER_CST)
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
            fprintf (vect_dump, "not vectorized: base addr of dr is a "
                     "constant");
          return false;
        }

      if (!DR_SYMBOL_TAG (dr))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
            {
              fprintf (vect_dump, "not vectorized: no memory tag for ");
              print_generic_expr (vect_dump, DR_REF (dr), TDF_SLIM);
            }
          return false;
        }

      base = unshare_expr (DR_BASE_ADDRESS (dr));
      offset = unshare_expr (DR_OFFSET (dr));
      init = unshare_expr (DR_INIT (dr));
	
      /* Update DR field in stmt_vec_info struct.  */
      bb = gimple_bb (stmt);

      /* If the dataref is in an inner-loop of the loop that is considered for
	 for vectorization, we also want to analyze the access relative to
	 the outer-loop (DR contains information only relative to the 
	 inner-most enclosing loop).  We do that by building a reference to the
	 first location accessed by the inner-loop, and analyze it relative to
	 the outer-loop.  */ 	
      if (nested_in_vect_loop_p (loop, stmt)) 
	{
	  tree outer_step, outer_base, outer_init;
	  HOST_WIDE_INT pbitsize, pbitpos;
	  tree poffset;
	  enum machine_mode pmode;
	  int punsignedp, pvolatilep;
	  affine_iv base_iv, offset_iv;
	  tree dinit;

	  /* Build a reference to the first location accessed by the 
	     inner-loop: *(BASE+INIT). (The first location is actually
	     BASE+INIT+OFFSET, but we add OFFSET separately later).  */
          tree inner_base = build_fold_indirect_ref
                                (fold_build2 (POINTER_PLUS_EXPR,
                                              TREE_TYPE (base), base, 
                                              fold_convert (sizetype, init)));

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "analyze in outer-loop: ");
	      print_generic_expr (vect_dump, inner_base, TDF_SLIM);
	    }

	  outer_base = get_inner_reference (inner_base, &pbitsize, &pbitpos, 
		          &poffset, &pmode, &punsignedp, &pvolatilep, false);
	  gcc_assert (outer_base != NULL_TREE);

	  if (pbitpos % BITS_PER_UNIT != 0)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "failed: bit offset alignment.\n");
	      return false;
	    }

	  outer_base = build_fold_addr_expr (outer_base);
	  if (!simple_iv (loop, loop_containing_stmt (stmt), outer_base,
			  &base_iv, false))
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "failed: evolution of base is not affine.\n");
	      return false;
	    }

	  if (offset)
	    {
	      if (poffset)
		poffset = fold_build2 (PLUS_EXPR, TREE_TYPE (offset), offset, poffset);
	      else
		poffset = offset;
	    }

	  if (!poffset)
	    {
	      offset_iv.base = ssize_int (0);
	      offset_iv.step = ssize_int (0);
	    }
	  else if (!simple_iv (loop, loop_containing_stmt (stmt), poffset,
			       &offset_iv, false))
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
	        fprintf (vect_dump, "evolution of offset is not affine.\n");
	      return false;
	    }

	  outer_init = ssize_int (pbitpos / BITS_PER_UNIT);
	  split_constant_offset (base_iv.base, &base_iv.base, &dinit);
	  outer_init =  size_binop (PLUS_EXPR, outer_init, dinit);
	  split_constant_offset (offset_iv.base, &offset_iv.base, &dinit);
	  outer_init =  size_binop (PLUS_EXPR, outer_init, dinit);

	  outer_step = size_binop (PLUS_EXPR,
				fold_convert (ssizetype, base_iv.step),
				fold_convert (ssizetype, offset_iv.step));

	  STMT_VINFO_DR_STEP (stmt_info) = outer_step;
	  /* FIXME: Use canonicalize_base_object_address (base_iv.base); */
	  STMT_VINFO_DR_BASE_ADDRESS (stmt_info) = base_iv.base; 
	  STMT_VINFO_DR_INIT (stmt_info) = outer_init;
	  STMT_VINFO_DR_OFFSET (stmt_info) = 
				fold_convert (ssizetype, offset_iv.base);
	  STMT_VINFO_DR_ALIGNED_TO (stmt_info) = 
				size_int (highest_pow2_factor (offset_iv.base));

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "\touter base_address: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_BASE_ADDRESS (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter offset from base address: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_OFFSET (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter constant offset from base address: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_INIT (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter step: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_STEP (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter aligned to: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_ALIGNED_TO (stmt_info), TDF_SLIM);
	    }
	}

      if (STMT_VINFO_DATA_REF (stmt_info))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
            {
              fprintf (vect_dump,
                       "not vectorized: more than one data ref in stmt: ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
            }
          return false;
        }
      STMT_VINFO_DATA_REF (stmt_info) = dr;
     
      /* Set vectype for STMT.  */
      scalar_type = TREE_TYPE (DR_REF (dr));
      STMT_VINFO_VECTYPE (stmt_info) =
                get_vectype_for_scalar_type (scalar_type);
      if (!STMT_VINFO_VECTYPE (stmt_info)) 
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
            {
              fprintf (vect_dump,
                       "not vectorized: no vectype for stmt: ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
              fprintf (vect_dump, " scalar_type: ");
              print_generic_expr (vect_dump, scalar_type, TDF_DETAILS);
            }
          return false;
        }
    }
      
  return true;
}


/* Utility functions used by vect_mark_stmts_to_be_vectorized.  */

/* Function vect_mark_relevant.

   Mark STMT as "relevant for vectorization" and add it to WORKLIST.  */

static void
vect_mark_relevant (VEC(gimple,heap) **worklist, gimple stmt,
		    enum vect_relevant relevant, bool live_p)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  enum vect_relevant save_relevant = STMT_VINFO_RELEVANT (stmt_info);
  bool save_live_p = STMT_VINFO_LIVE_P (stmt_info);

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "mark relevant %d, live %d.", relevant, live_p);

  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
    {
      gimple pattern_stmt;

      /* This is the last stmt in a sequence that was detected as a 
         pattern that can potentially be vectorized.  Don't mark the stmt
         as relevant/live because it's not going to be vectorized.
         Instead mark the pattern-stmt that replaces it.  */

      pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info);

      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "last stmt in pattern. don't mark relevant/live.");
      stmt_info = vinfo_for_stmt (pattern_stmt);
      gcc_assert (STMT_VINFO_RELATED_STMT (stmt_info) == stmt);
      save_relevant = STMT_VINFO_RELEVANT (stmt_info);
      save_live_p = STMT_VINFO_LIVE_P (stmt_info);
      stmt = pattern_stmt;
    }

  STMT_VINFO_LIVE_P (stmt_info) |= live_p;
  if (relevant > STMT_VINFO_RELEVANT (stmt_info))
    STMT_VINFO_RELEVANT (stmt_info) = relevant;

  if (STMT_VINFO_RELEVANT (stmt_info) == save_relevant
      && STMT_VINFO_LIVE_P (stmt_info) == save_live_p)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "already marked relevant/live.");
      return;
    }

  VEC_safe_push (gimple, heap, *worklist, stmt);
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
vect_stmt_relevant_p (gimple stmt, loop_vec_info loop_vinfo,
		      enum vect_relevant *relevant, bool *live_p)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  def_operand_p def_p;

  *relevant = vect_unused_in_loop;
  *live_p = false;

  /* cond stmt other than loop exit cond.  */
  if (is_ctrl_stmt (stmt) 
      && STMT_VINFO_TYPE (vinfo_for_stmt (stmt)) != loop_exit_ctrl_vec_info_type) 
    *relevant = vect_used_in_loop;

  /* changing memory.  */
  if (gimple_code (stmt) != GIMPLE_PHI)
    if (!ZERO_SSA_OPERANDS (stmt, SSA_OP_VIRTUAL_DEFS))
      {
	if (vect_print_dump_info (REPORT_DETAILS))
	  fprintf (vect_dump, "vec_stmt_relevant_p: stmt has vdefs.");
	*relevant = vect_used_in_loop;
      }

  /* uses outside the loop.  */
  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, op_iter, SSA_OP_DEF)
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, DEF_FROM_PTR (def_p))
	{
	  basic_block bb = gimple_bb (USE_STMT (use_p));
	  if (!flow_bb_inside_loop_p (loop, bb))
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "vec_stmt_relevant_p: used out of loop.");

	      /* We expect all such uses to be in the loop exit phis
		 (because of loop closed form)   */
	      gcc_assert (gimple_code (USE_STMT (use_p)) == GIMPLE_PHI);
	      gcc_assert (bb == single_exit (loop)->dest);

              *live_p = true;
	    }
	}
    }

  return (*live_p || *relevant);
}


/* 
   Function process_use.

   Inputs:
   - a USE in STMT in a loop represented by LOOP_VINFO
   - LIVE_P, RELEVANT - enum values to be set in the STMT_VINFO of the stmt 
     that defined USE. This is done by calling mark_relevant and passing it
     the WORKLIST (to add DEF_STMT to the WORKLIST in case it is relevant).

   Outputs:
   Generally, LIVE_P and RELEVANT are used to define the liveness and
   relevance info of the DEF_STMT of this USE:
       STMT_VINFO_LIVE_P (DEF_STMT_info) <-- live_p
       STMT_VINFO_RELEVANT (DEF_STMT_info) <-- relevant
   Exceptions:
   - case 1: If USE is used only for address computations (e.g. array indexing),
   which does not need to be directly vectorized, then the liveness/relevance 
   of the respective DEF_STMT is left unchanged.
   - case 2: If STMT is a reduction phi and DEF_STMT is a reduction stmt, we 
   skip DEF_STMT cause it had already been processed.  
   - case 3: If DEF_STMT and STMT are in different nests, then  "relevant" will
   be modified accordingly.

   Return true if everything is as expected. Return false otherwise.  */

static bool
process_use (gimple stmt, tree use, loop_vec_info loop_vinfo, bool live_p, 
	     enum vect_relevant relevant, VEC(gimple,heap) **worklist)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  stmt_vec_info dstmt_vinfo;
  basic_block bb, def_bb;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt;

  /* case 1: we are only interested in uses that need to be vectorized.  Uses 
     that are used for address computation are not considered relevant.  */
  if (!exist_non_indexing_operands_for_use_p (use, stmt))
     return true;

  if (!vect_is_simple_use (use, loop_vinfo, &def_stmt, &def, &dt))
    { 
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
        fprintf (vect_dump, "not vectorized: unsupported use in stmt.");
      return false;
    }

  if (!def_stmt || gimple_nop_p (def_stmt))
    return true;

  def_bb = gimple_bb (def_stmt);
  if (!flow_bb_inside_loop_p (loop, def_bb))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "def_stmt is out of loop.");
      return true;
    }

  /* case 2: A reduction phi (STMT) defined by a reduction stmt (DEF_STMT). 
     DEF_STMT must have already been processed, because this should be the 
     only way that STMT, which is a reduction-phi, was put in the worklist, 
     as there should be no other uses for DEF_STMT in the loop.  So we just 
     check that everything is as expected, and we are done.  */
  dstmt_vinfo = vinfo_for_stmt (def_stmt);
  bb = gimple_bb (stmt);
  if (gimple_code (stmt) == GIMPLE_PHI
      && STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
      && gimple_code (def_stmt) != GIMPLE_PHI
      && STMT_VINFO_DEF_TYPE (dstmt_vinfo) == vect_reduction_def
      && bb->loop_father == def_bb->loop_father)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "reduc-stmt defining reduc-phi in the same nest.");
      if (STMT_VINFO_IN_PATTERN_P (dstmt_vinfo))
	dstmt_vinfo = vinfo_for_stmt (STMT_VINFO_RELATED_STMT (dstmt_vinfo));
      gcc_assert (STMT_VINFO_RELEVANT (dstmt_vinfo) < vect_used_by_reduction);
      gcc_assert (STMT_VINFO_LIVE_P (dstmt_vinfo) 
		  || STMT_VINFO_RELEVANT (dstmt_vinfo) > vect_unused_in_loop);
      return true;
    }

  /* case 3a: outer-loop stmt defining an inner-loop stmt:
	outer-loop-header-bb:
		d = def_stmt
	inner-loop:
		stmt # use (d)
	outer-loop-tail-bb:
		...		  */
  if (flow_loop_nested_p (def_bb->loop_father, bb->loop_father))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "outer-loop def-stmt defining inner-loop stmt.");
      switch (relevant)
	{
	case vect_unused_in_loop:
	  relevant = (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def) ?
			vect_used_by_reduction : vect_unused_in_loop;
	  break;
	case vect_used_in_outer_by_reduction:
	  relevant = vect_used_by_reduction;
	  break;
	case vect_used_in_outer:
	  relevant = vect_used_in_loop;
	  break;
	case vect_used_by_reduction: 
	case vect_used_in_loop:
	  break;

	default:
	  gcc_unreachable ();
	}   
    }

  /* case 3b: inner-loop stmt defining an outer-loop stmt:
	outer-loop-header-bb:
		...
	inner-loop:
		d = def_stmt
	outer-loop-tail-bb:
		stmt # use (d)		*/
  else if (flow_loop_nested_p (bb->loop_father, def_bb->loop_father))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "inner-loop def-stmt defining outer-loop stmt.");
      switch (relevant)
        {
        case vect_unused_in_loop:
          relevant = (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def) ?
                        vect_used_in_outer_by_reduction : vect_unused_in_loop;
          break;

        case vect_used_in_outer_by_reduction:
        case vect_used_in_outer:
          break;

        case vect_used_by_reduction:
          relevant = vect_used_in_outer_by_reduction;
          break;

        case vect_used_in_loop:
          relevant = vect_used_in_outer;
          break;

        default:
          gcc_unreachable ();
        }
    }

  vect_mark_relevant (worklist, def_stmt, relevant, live_p);
  return true;
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
  VEC(gimple,heap) *worklist;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned int nbbs = loop->num_nodes;
  gimple_stmt_iterator si;
  gimple stmt;
  unsigned int i;
  stmt_vec_info stmt_vinfo;
  basic_block bb;
  gimple phi;
  bool live_p;
  enum vect_relevant relevant;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_mark_stmts_to_be_vectorized ===");

  worklist = VEC_alloc (gimple, heap, 64);

  /* 1. Init worklist.  */
  for (i = 0; i < nbbs; i++)
    {
      bb = bbs[i];
      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{ 
	  phi = gsi_stmt (si);
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "init: phi relevant? ");
	      print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
	    }

	  if (vect_stmt_relevant_p (phi, loop_vinfo, &relevant, &live_p))
	    vect_mark_relevant (&worklist, phi, relevant, live_p);
	}
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  stmt = gsi_stmt (si);
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "init: stmt relevant? ");
	      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	    } 

	  if (vect_stmt_relevant_p (stmt, loop_vinfo, &relevant, &live_p))
            vect_mark_relevant (&worklist, stmt, relevant, live_p);
	}
    }

  /* 2. Process_worklist */
  while (VEC_length (gimple, worklist) > 0)
    {
      use_operand_p use_p;
      ssa_op_iter iter;

      stmt = VEC_pop (gimple, worklist);
      if (vect_print_dump_info (REPORT_DETAILS))
	{
          fprintf (vect_dump, "worklist: examine stmt: ");
          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	}

      /* Examine the USEs of STMT. For each USE, mark the stmt that defines it 
	 (DEF_STMT) as relevant/irrelevant and live/dead according to the 
	 liveness and relevance properties of STMT.  */
      stmt_vinfo = vinfo_for_stmt (stmt);
      relevant = STMT_VINFO_RELEVANT (stmt_vinfo);
      live_p = STMT_VINFO_LIVE_P (stmt_vinfo);

      /* Generally, the liveness and relevance properties of STMT are
	 propagated as is to the DEF_STMTs of its USEs:
	  live_p <-- STMT_VINFO_LIVE_P (STMT_VINFO)
	  relevant <-- STMT_VINFO_RELEVANT (STMT_VINFO)

	 One exception is when STMT has been identified as defining a reduction
	 variable; in this case we set the liveness/relevance as follows:
	   live_p = false
	   relevant = vect_used_by_reduction
	 This is because we distinguish between two kinds of relevant stmts -
	 those that are used by a reduction computation, and those that are 
	 (also) used by a regular computation. This allows us later on to 
	 identify stmts that are used solely by a reduction, and therefore the 
	 order of the results that they produce does not have to be kept.

	 Reduction phis are expected to be used by a reduction stmt, or by
	 in an outer loop;  Other reduction stmts are expected to be
	 in the loop, and possibly used by a stmt in an outer loop. 
	 Here are the expected values of "relevant" for reduction phis/stmts:

	 relevance:				phi	stmt
	 vect_unused_in_loop				ok
	 vect_used_in_outer_by_reduction	ok	ok
	 vect_used_in_outer			ok	ok
	 vect_used_by_reduction			ok
	 vect_used_in_loop 						  */

      if (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def)
        {
	  enum vect_relevant tmp_relevant = relevant;
	  switch (tmp_relevant)
	    {
	    case vect_unused_in_loop:
	      gcc_assert (gimple_code (stmt) != GIMPLE_PHI);
	      relevant = vect_used_by_reduction;
	      break;

	    case vect_used_in_outer_by_reduction:
	    case vect_used_in_outer:
	      gcc_assert (gimple_code (stmt) != GIMPLE_ASSIGN
                          || (gimple_assign_rhs_code (stmt) != WIDEN_SUM_EXPR
                              && (gimple_assign_rhs_code (stmt)
                                  != DOT_PROD_EXPR)));
	      break;

	    case vect_used_by_reduction:
	      if (gimple_code (stmt) == GIMPLE_PHI)
		break;
	      /* fall through */
	    case vect_used_in_loop:
	    default:
	      if (vect_print_dump_info (REPORT_DETAILS))
	        fprintf (vect_dump, "unsupported use of reduction.");
	      VEC_free (gimple, heap, worklist);
	      return false;
	    }
	  live_p = false;	
	}

      FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
	{
	  tree op = USE_FROM_PTR (use_p);
	  if (!process_use (stmt, op, loop_vinfo, live_p, relevant, &worklist))
	    {
	      VEC_free (gimple, heap, worklist);
	      return false;
	    }
	}
    } /* while worklist */

  VEC_free (gimple, heap, worklist);
  return true;
}


/* Function vect_can_advance_ivs_p

   In case the number of iterations that LOOP iterates is unknown at compile 
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
  gimple phi;
  gimple_stmt_iterator gsi;

  /* Analyze phi functions of the loop header.  */

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "vect_can_advance_ivs_p:");

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree access_fn = NULL;
      tree evolution_part;

      phi = gsi_stmt (gsi);
      if (vect_print_dump_info (REPORT_DETAILS))
	{
          fprintf (vect_dump, "Analyze phi: ");
          print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
	}

      /* Skip virtual phi's. The data dependences that are associated with
         virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.  */

      if (!is_gimple_reg (SSA_NAME_VAR (PHI_RESULT (phi))))
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "virtual phi. skip.");
	  continue;
	}

      /* Skip reduction phis.  */

      if (STMT_VINFO_DEF_TYPE (vinfo_for_stmt (phi)) == vect_reduction_def)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "reduc phi. skip.");
          continue;
        }

      /* Analyze the evolution function.  */

      access_fn = instantiate_parameters
	(loop, analyze_scalar_evolution (loop, PHI_RESULT (phi)));

      if (!access_fn)
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "No Access function.");
	  return false;
	}

      if (vect_print_dump_info (REPORT_DETAILS))
        {
	  fprintf (vect_dump, "Access function of PHI: ");
	  print_generic_expr (vect_dump, access_fn, TDF_SLIM);
        }

      evolution_part = evolution_part_in_loop_num (access_fn, loop->num);
      
      if (evolution_part == NULL_TREE)
        {
	  if (vect_print_dump_info (REPORT_DETAILS))
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

static gimple
vect_get_loop_niters (struct loop *loop, tree *number_of_iterations)
{
  tree niters;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== get_loop_niters ===");

  niters = number_of_exit_cond_executions (loop);

  if (niters != NULL_TREE
      && niters != chrec_dont_know)
    {
      *number_of_iterations = niters;

      if (vect_print_dump_info (REPORT_DETAILS))
	{
	  fprintf (vect_dump, "==> get_loop_niters:" );
	  print_generic_expr (vect_dump, *number_of_iterations, TDF_SLIM);
	}
    }

  return get_loop_exit_condition (loop);
}


/* Function vect_analyze_loop_1.

   Apply a set of analyses on LOOP, and create a loop_vec_info struct
   for it. The different analyses will record information in the
   loop_vec_info struct.  This is a subset of the analyses applied in
   vect_analyze_loop, to be applied on an inner-loop nested in the loop
   that is now considered for (outer-loop) vectorization.  */

static loop_vec_info
vect_analyze_loop_1 (struct loop *loop)
{
  loop_vec_info loop_vinfo;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "===== analyze_loop_nest_1 =====");

  /* Check the CFG characteristics of the loop (nesting, entry/exit, etc.  */

  loop_vinfo = vect_analyze_loop_form (loop);
  if (!loop_vinfo)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "bad inner-loop form.");
      return NULL;
    }

  return loop_vinfo;
}


/* Function vect_analyze_loop_form.

   Verify that certain CFG restrictions hold, including:
   - the loop has a pre-header
   - the loop has a single entry and exit
   - the loop exit condition is simple enough, and the number of iterations
     can be analyzed (a countable loop).  */

loop_vec_info
vect_analyze_loop_form (struct loop *loop)
{
  loop_vec_info loop_vinfo;
  gimple loop_cond;
  tree number_of_iterations = NULL;
  loop_vec_info inner_loop_vinfo = NULL;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_loop_form ===");

  /* Different restrictions apply when we are considering an inner-most loop,
     vs. an outer (nested) loop.  
     (FORNOW. May want to relax some of these restrictions in the future).  */

  if (!loop->inner)
    {
      /* Inner-most loop.  We currently require that the number of BBs is 
	 exactly 2 (the header and latch).  Vectorizable inner-most loops 
	 look like this:

                        (pre-header)
                           |
                          header <--------+
                           | |            |
                           | +--> latch --+
                           |
                        (exit-bb)  */

      if (loop->num_nodes != 2)
        {
          if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
            fprintf (vect_dump, "not vectorized: too many BBs in loop.");
          return NULL;
        }

      if (empty_block_p (loop->header))
    {
          if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
            fprintf (vect_dump, "not vectorized: empty loop.");
      return NULL;
    }
    }
  else
    {
      struct loop *innerloop = loop->inner;
      edge backedge, entryedge;

      /* Nested loop. We currently require that the loop is doubly-nested,
	 contains a single inner loop, and the number of BBs is exactly 5. 
	 Vectorizable outer-loops look like this:

			(pre-header)
			   |
			  header <---+
			   |         |
		          inner-loop |
			   |         |
			  tail ------+
			   | 
		        (exit-bb)

	 The inner-loop has the properties expected of inner-most loops
	 as described above.  */

      if ((loop->inner)->inner || (loop->inner)->next)
	{
	  if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
	    fprintf (vect_dump, "not vectorized: multiple nested loops.");
	  return NULL;
	}

      /* Analyze the inner-loop.  */
      inner_loop_vinfo = vect_analyze_loop_1 (loop->inner);
      if (!inner_loop_vinfo)
	{
	  if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
            fprintf (vect_dump, "not vectorized: Bad inner loop.");
	  return NULL;
	}

      if (!expr_invariant_in_loop_p (loop,
					LOOP_VINFO_NITERS (inner_loop_vinfo)))
	{
	  if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
	    fprintf (vect_dump,
		     "not vectorized: inner-loop count not invariant.");
	  destroy_loop_vec_info (inner_loop_vinfo, true);
	  return NULL;
	}

      if (loop->num_nodes != 5) 
        {
	  if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
	    fprintf (vect_dump, "not vectorized: too many BBs in loop.");
	  destroy_loop_vec_info (inner_loop_vinfo, true);
	  return NULL;
        }

      gcc_assert (EDGE_COUNT (innerloop->header->preds) == 2);
      backedge = EDGE_PRED (innerloop->header, 1);	  
      entryedge = EDGE_PRED (innerloop->header, 0);
      if (EDGE_PRED (innerloop->header, 0)->src == innerloop->latch)
	{
	  backedge = EDGE_PRED (innerloop->header, 0);
	  entryedge = EDGE_PRED (innerloop->header, 1);	
	}
	
      if (entryedge->src != loop->header
	  || !single_exit (innerloop)
	  || single_exit (innerloop)->dest !=  EDGE_PRED (loop->latch, 0)->src)
	{
	  if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
	    fprintf (vect_dump, "not vectorized: unsupported outerloop form.");
	  destroy_loop_vec_info (inner_loop_vinfo, true);
	  return NULL;
	}

      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "Considering outer-loop vectorization.");
    }
  
  if (!single_exit (loop) 
      || EDGE_COUNT (loop->header->preds) != 2)
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
        {
          if (!single_exit (loop))
            fprintf (vect_dump, "not vectorized: multiple exits.");
          else if (EDGE_COUNT (loop->header->preds) != 2)
            fprintf (vect_dump, "not vectorized: too many incoming edges.");
        }
      if (inner_loop_vinfo)
	destroy_loop_vec_info (inner_loop_vinfo, true);
      return NULL;
    }

  /* We assume that the loop exit condition is at the end of the loop. i.e,
     that the loop is represented as a do-while (with a proper if-guard
     before the loop if needed), where the loop header contains all the
     executable statements, and the latch is empty.  */
  if (!empty_block_p (loop->latch)
        || phi_nodes (loop->latch))
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
        fprintf (vect_dump, "not vectorized: unexpected loop form.");
      if (inner_loop_vinfo)
	destroy_loop_vec_info (inner_loop_vinfo, true);
      return NULL;
    }

  /* Make sure there exists a single-predecessor exit bb:  */
  if (!single_pred_p (single_exit (loop)->dest))
    {
      edge e = single_exit (loop);
      if (!(e->flags & EDGE_ABNORMAL))
	{
	  split_loop_exit_edge (e);
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "split exit edge.");
	}
      else
	{
	  if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
	    fprintf (vect_dump, "not vectorized: abnormal loop exit edge.");
	  if (inner_loop_vinfo)
	    destroy_loop_vec_info (inner_loop_vinfo, true);
	  return NULL;
	}
    }

  loop_cond = vect_get_loop_niters (loop, &number_of_iterations);
  if (!loop_cond)
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
	fprintf (vect_dump, "not vectorized: complicated exit condition.");
      if (inner_loop_vinfo)
	destroy_loop_vec_info (inner_loop_vinfo, true);
      return NULL;
    }
  
  if (!number_of_iterations) 
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
	fprintf (vect_dump, 
		 "not vectorized: number of iterations cannot be computed.");
      if (inner_loop_vinfo)
	destroy_loop_vec_info (inner_loop_vinfo, true);
      return NULL;
    }

  if (chrec_contains_undetermined (number_of_iterations))
    {
      if (vect_print_dump_info (REPORT_BAD_FORM_LOOPS))
        fprintf (vect_dump, "Infinite number of iterations.");
      if (inner_loop_vinfo)
	destroy_loop_vec_info (inner_loop_vinfo, true);
      return NULL;
    }

  if (!NITERS_KNOWN_P (number_of_iterations))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "Symbolic number of iterations is ");
          print_generic_expr (vect_dump, number_of_iterations, TDF_DETAILS);
        }
    }
  else if (TREE_INT_CST_LOW (number_of_iterations) == 0)
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS))
        fprintf (vect_dump, "not vectorized: number of iterations = 0.");
      if (inner_loop_vinfo)
        destroy_loop_vec_info (inner_loop_vinfo, false);
      return NULL;
    }

  loop_vinfo = new_loop_vec_info (loop);
  LOOP_VINFO_NITERS (loop_vinfo) = number_of_iterations;
  LOOP_VINFO_NITERS_UNCHANGED (loop_vinfo) = number_of_iterations;

  STMT_VINFO_TYPE (vinfo_for_stmt (loop_cond)) = loop_exit_ctrl_vec_info_type;

  /* CHECKME: May want to keep it around it in the future.  */
  if (inner_loop_vinfo)
    destroy_loop_vec_info (inner_loop_vinfo, false);

  gcc_assert (!loop->aux);
  loop->aux = loop_vinfo;
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

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "===== analyze_loop_nest =====");

  if (loop_outer (loop) 
      && loop_vec_info_for_loop (loop_outer (loop))
      && LOOP_VINFO_VECTORIZABLE_P (loop_vec_info_for_loop (loop_outer (loop))))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "outer-loop already vectorized.");
      return NULL;
    }

  /* Check the CFG characteristics of the loop (nesting, entry/exit, etc.  */

  loop_vinfo = vect_analyze_loop_form (loop);
  if (!loop_vinfo)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
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
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data references.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Classify all cross-iteration scalar data-flow cycles.
     Cross-iteration cycles caused by virtual phis are analyzed separately.  */

  vect_analyze_scalar_cycles (loop_vinfo);

  vect_pattern_recog (loop_vinfo);

  /* Data-flow analysis to detect stmts that do not need to be vectorized.  */

  ok = vect_mark_stmts_to_be_vectorized (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "unexpected pattern.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Analyze the alignment of the data-refs in the loop.
     Fail if a data reference is found that cannot be vectorized.  */

  ok = vect_analyze_data_refs_alignment (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data alignment.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  ok = vect_determine_vectorization_factor (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "can't determine vectorization factor.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Analyze data dependences between the data-refs in the loop. 
     FORNOW: fail at the first data dependence that we encounter.  */

  ok = vect_analyze_data_ref_dependences (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data dependence.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Analyze the access patterns of the data-refs in the loop (consecutive,
     complex, etc.). FORNOW: Only handle consecutive access pattern.  */

  ok = vect_analyze_data_ref_accesses (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data access.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Prune the list of ddrs to be tested at run-time by versioning for alias.
     It is important to call pruning after vect_analyze_data_ref_accesses,
     since we use grouping information gathered by interleaving analysis.  */
  ok = vect_prune_runtime_alias_test_list (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "too long list of versioning for alias "
			    "run-time tests.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Check the SLP opportunities in the loop, analyze and build SLP trees.  */
  ok = vect_analyze_slp (loop_vinfo);
  if (ok)
    {
      /* Decide which possible SLP instances to SLP.  */
      vect_make_slp_decision (loop_vinfo);

      /* Find stmts that need to be both vectorized and SLPed.  */
      vect_detect_hybrid_slp (loop_vinfo);
    }

  /* This pass will decide on using loop versioning and/or loop peeling in
     order to enhance the alignment of data references in the loop.  */

  ok = vect_enhance_data_refs_alignment (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data alignment.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Scan all the operations in the loop and make sure they are
     vectorizable.  */

  ok = vect_analyze_operations (loop_vinfo);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad operation or unsupported loop bound.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  LOOP_VINFO_VECTORIZABLE_P (loop_vinfo) = 1;

  return loop_vinfo;
}
