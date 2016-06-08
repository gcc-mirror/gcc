/* Loop Vectorization
   Copyright (C) 2003-2016 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com> and
   Ira Rosen <irar@il.ibm.com>

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
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "cfgloop.h"
#include "params.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "gimple-fold.h"
#include "cgraph.h"
#include "tree-cfg.h"

/* Loop Vectorization Pass.

   This pass tries to vectorize loops.

   For example, the vectorizer transforms the following simple loop:

        short a[N]; short b[N]; short c[N]; int i;

        for (i=0; i<N; i++){
          a[i] = b[i] + c[i];
        }

   as if it was manually vectorized by rewriting the source code into:

        typedef int __attribute__((mode(V8HI))) v8hi;
        short a[N];  short b[N]; short c[N];   int i;
        v8hi *pa = (v8hi*)a, *pb = (v8hi*)b, *pc = (v8hi*)c;
        v8hi va, vb, vc;

        for (i=0; i<N/8; i++){
          vb = pb[i];
          vc = pc[i];
          va = vb + vc;
          pa[i] = va;
        }

        The main entry to this pass is vectorize_loops(), in which
   the vectorizer applies a set of analyses on a given set of loops,
   followed by the actual vectorization transformation for the loops that
   had successfully passed the analysis phase.
        Throughout this pass we make a distinction between two types of
   data: scalars (which are represented by SSA_NAMES), and memory references
   ("data-refs").  These two types of data require different handling both
   during analysis and transformation. The types of data-refs that the
   vectorizer currently supports are ARRAY_REFS which base is an array DECL
   (not a pointer), and INDIRECT_REFS through pointers; both array and pointer
   accesses are required to have a simple (consecutive) access pattern.

   Analysis phase:
   ===============
        The driver for the analysis phase is vect_analyze_loop().
   It applies a set of analyses, some of which rely on the scalar evolution
   analyzer (scev) developed by Sebastian Pop.

        During the analysis phase the vectorizer records some information
   per stmt in a "stmt_vec_info" struct which is attached to each stmt in the
   loop, as well as general information about the loop as a whole, which is
   recorded in a "loop_vec_info" struct attached to each loop.

   Transformation phase:
   =====================
        The loop transformation phase scans all the stmts in the loop, and
   creates a vector stmt (or a sequence of stmts) for each scalar stmt S in
   the loop that needs to be vectorized.  It inserts the vector code sequence
   just before the scalar stmt S, and records a pointer to the vector code
   in STMT_VINFO_VEC_STMT (stmt_info) (stmt_info is the stmt_vec_info struct
   attached to S).  This pointer will be used for the vectorization of following
   stmts which use the def of stmt S. Stmt S is removed if it writes to memory;
   otherwise, we rely on dead code elimination for removing it.

        For example, say stmt S1 was vectorized into stmt VS1:

   VS1: vb = px[i];
   S1:  b = x[i];    STMT_VINFO_VEC_STMT (stmt_info (S1)) = VS1
   S2:  a = b;

   To vectorize stmt S2, the vectorizer first finds the stmt that defines
   the operand 'b' (S1), and gets the relevant vector def 'vb' from the
   vector stmt VS1 pointed to by STMT_VINFO_VEC_STMT (stmt_info (S1)).  The
   resulting sequence would be:

   VS1: vb = px[i];
   S1:  b = x[i];       STMT_VINFO_VEC_STMT (stmt_info (S1)) = VS1
   VS2: va = vb;
   S2:  a = b;          STMT_VINFO_VEC_STMT (stmt_info (S2)) = VS2

        Operands that are not SSA_NAMEs, are data-refs that appear in
   load/store operations (like 'x[i]' in S1), and are handled differently.

   Target modeling:
   =================
        Currently the only target specific information that is used is the
   size of the vector (in bytes) - "TARGET_VECTORIZE_UNITS_PER_SIMD_WORD".
   Targets that can support different sizes of vectors, for now will need
   to specify one value for "TARGET_VECTORIZE_UNITS_PER_SIMD_WORD".  More
   flexibility will be added in the future.

        Since we only vectorize operations which vector form can be
   expressed using existing tree codes, to verify that an operation is
   supported, the vectorizer checks the relevant optab at the relevant
   machine_mode (e.g, optab_handler (add_optab, V8HImode)).  If
   the value found is CODE_FOR_nothing, then there's no target support, and
   we can't vectorize the stmt.

   For additional information on this project see:
   http://gcc.gnu.org/projects/tree-ssa/vectorization.html
*/

static void vect_estimate_min_profitable_iters (loop_vec_info, int *, int *);

/* Function vect_determine_vectorization_factor

   Determine the vectorization factor (VF).  VF is the number of data elements
   that are operated upon in parallel in a single iteration of the vectorized
   loop.  For example, when vectorizing a loop that operates on 4byte elements,
   on a target with vector size (VS) 16byte, the VF is set to 4, since 4
   elements can fit in a single vector register.

   We currently support vectorization of loops in which all types operated upon
   are of the same size.  Therefore this function currently sets VF according to
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
  unsigned nbbs = loop->num_nodes;
  unsigned int vectorization_factor = 0;
  tree scalar_type;
  gphi *phi;
  tree vectype;
  unsigned int nunits;
  stmt_vec_info stmt_info;
  unsigned i;
  HOST_WIDE_INT dummy;
  gimple *stmt, *pattern_stmt = NULL;
  gimple_seq pattern_def_seq = NULL;
  gimple_stmt_iterator pattern_def_si = gsi_none ();
  bool analyze_pattern_stmt = false;
  bool bool_result;
  auto_vec<stmt_vec_info> mask_producers;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "=== vect_determine_vectorization_factor ===\n");

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  phi = si.phi ();
	  stmt_info = vinfo_for_stmt (phi);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location, "==> examining phi: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	    }

	  gcc_assert (stmt_info);

	  if (STMT_VINFO_RELEVANT_P (stmt_info)
	      || STMT_VINFO_LIVE_P (stmt_info))
            {
	      gcc_assert (!STMT_VINFO_VECTYPE (stmt_info));
              scalar_type = TREE_TYPE (PHI_RESULT (phi));

	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location,
                                   "get vectype for scalar type:  ");
		  dump_generic_expr (MSG_NOTE, TDF_SLIM, scalar_type);
                  dump_printf (MSG_NOTE, "\n");
		}

	      vectype = get_vectype_for_scalar_type (scalar_type);
	      if (!vectype)
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                       "not vectorized: unsupported "
                                       "data-type ");
		      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
                                         scalar_type);
                      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		    }
		  return false;
		}
	      STMT_VINFO_VECTYPE (stmt_info) = vectype;

	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location, "vectype: ");
		  dump_generic_expr (MSG_NOTE, TDF_SLIM, vectype);
                  dump_printf (MSG_NOTE, "\n");
		}

	      nunits = TYPE_VECTOR_SUBPARTS (vectype);
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location, "nunits = %d\n",
                                 nunits);

	      if (!vectorization_factor
		  || (nunits > vectorization_factor))
		vectorization_factor = nunits;
	    }
	}

      for (gimple_stmt_iterator si = gsi_start_bb (bb);
	   !gsi_end_p (si) || analyze_pattern_stmt;)
        {
          tree vf_vectype;

          if (analyze_pattern_stmt)
	    stmt = pattern_stmt;
          else
            stmt = gsi_stmt (si);

          stmt_info = vinfo_for_stmt (stmt);

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
                               "==> examining statement: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	    }

	  gcc_assert (stmt_info);

	  /* Skip stmts which do not need to be vectorized.  */
	  if ((!STMT_VINFO_RELEVANT_P (stmt_info)
	       && !STMT_VINFO_LIVE_P (stmt_info))
	      || gimple_clobber_p (stmt))
            {
              if (STMT_VINFO_IN_PATTERN_P (stmt_info)
                  && (pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info))
                  && (STMT_VINFO_RELEVANT_P (vinfo_for_stmt (pattern_stmt))
                      || STMT_VINFO_LIVE_P (vinfo_for_stmt (pattern_stmt))))
                {
                  stmt = pattern_stmt;
                  stmt_info = vinfo_for_stmt (pattern_stmt);
                  if (dump_enabled_p ())
                    {
                      dump_printf_loc (MSG_NOTE, vect_location,
                                       "==> examining pattern statement: ");
                      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
                    }
                }
              else
	        {
	          if (dump_enabled_p ())
	            dump_printf_loc (MSG_NOTE, vect_location, "skip.\n");
                  gsi_next (&si);
	          continue;
                }
	    }
          else if (STMT_VINFO_IN_PATTERN_P (stmt_info)
                   && (pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info))
                   && (STMT_VINFO_RELEVANT_P (vinfo_for_stmt (pattern_stmt))
                       || STMT_VINFO_LIVE_P (vinfo_for_stmt (pattern_stmt))))
            analyze_pattern_stmt = true;

	  /* If a pattern statement has def stmts, analyze them too.  */
	  if (is_pattern_stmt_p (stmt_info))
	    {
	      if (pattern_def_seq == NULL)
		{
		  pattern_def_seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_info);
		  pattern_def_si = gsi_start (pattern_def_seq);
		}
	      else if (!gsi_end_p (pattern_def_si))
		gsi_next (&pattern_def_si);
	      if (pattern_def_seq != NULL)
		{
		  gimple *pattern_def_stmt = NULL;
		  stmt_vec_info pattern_def_stmt_info = NULL;

		  while (!gsi_end_p (pattern_def_si))
		    {
		      pattern_def_stmt = gsi_stmt (pattern_def_si);
		      pattern_def_stmt_info
			= vinfo_for_stmt (pattern_def_stmt);
		      if (STMT_VINFO_RELEVANT_P (pattern_def_stmt_info)
			  || STMT_VINFO_LIVE_P (pattern_def_stmt_info))
			break;
		      gsi_next (&pattern_def_si);
		    }

		  if (!gsi_end_p (pattern_def_si))
		    {
		      if (dump_enabled_p ())
			{
			  dump_printf_loc (MSG_NOTE, vect_location,
                                           "==> examining pattern def stmt: ");
			  dump_gimple_stmt (MSG_NOTE, TDF_SLIM,
                                            pattern_def_stmt, 0);
			}

		      stmt = pattern_def_stmt;
		      stmt_info = pattern_def_stmt_info;
		    }
		  else
		    {
		      pattern_def_si = gsi_none ();
		      analyze_pattern_stmt = false;
		    }
		}
	      else
		analyze_pattern_stmt = false;
	    }

	  if (gimple_get_lhs (stmt) == NULL_TREE
	      /* MASK_STORE has no lhs, but is ok.  */
	      && (!is_gimple_call (stmt)
		  || !gimple_call_internal_p (stmt)
		  || gimple_call_internal_fn (stmt) != IFN_MASK_STORE))
	    {
	      if (is_gimple_call (stmt))
		{
		  /* Ignore calls with no lhs.  These must be calls to
		     #pragma omp simd functions, and what vectorization factor
		     it really needs can't be determined until
		     vectorizable_simd_clone_call.  */
		  if (!analyze_pattern_stmt && gsi_end_p (pattern_def_si))
		    {
		      pattern_def_seq = NULL;
		      gsi_next (&si);
		    }
		  continue;
		}
	      if (dump_enabled_p ())
		{
	          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                   "not vectorized: irregular stmt.");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION,  TDF_SLIM, stmt,
                                    0);
		}
	      return false;
	    }

	  if (VECTOR_MODE_P (TYPE_MODE (gimple_expr_type (stmt))))
	    {
	      if (dump_enabled_p ())
	        {
	          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                   "not vectorized: vector stmt in loop:");
	          dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	        }
	      return false;
	    }

	  bool_result = false;

	  if (STMT_VINFO_VECTYPE (stmt_info))
	    {
	      /* The only case when a vectype had been already set is for stmts
	         that contain a dataref, or for "pattern-stmts" (stmts
		 generated by the vectorizer to represent/replace a certain
		 idiom).  */
	      gcc_assert (STMT_VINFO_DATA_REF (stmt_info)
			  || is_pattern_stmt_p (stmt_info)
			  || !gsi_end_p (pattern_def_si));
	      vectype = STMT_VINFO_VECTYPE (stmt_info);
	    }
	  else
	    {
	      gcc_assert (!STMT_VINFO_DATA_REF (stmt_info));
	      if (is_gimple_call (stmt)
		  && gimple_call_internal_p (stmt)
		  && gimple_call_internal_fn (stmt) == IFN_MASK_STORE)
		scalar_type = TREE_TYPE (gimple_call_arg (stmt, 3));
	      else
		scalar_type = TREE_TYPE (gimple_get_lhs (stmt));

	      /* Bool ops don't participate in vectorization factor
		 computation.  For comparison use compared types to
		 compute a factor.  */
	      if (TREE_CODE (scalar_type) == BOOLEAN_TYPE
		  && is_gimple_assign (stmt)
		  && gimple_assign_rhs_code (stmt) != COND_EXPR)
		{
		  if (STMT_VINFO_RELEVANT_P (stmt_info)
		      || STMT_VINFO_LIVE_P (stmt_info))
		    mask_producers.safe_push (stmt_info);
		  bool_result = true;

		  if (gimple_code (stmt) == GIMPLE_ASSIGN
		      && TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
			 == tcc_comparison
		      && TREE_CODE (TREE_TYPE (gimple_assign_rhs1 (stmt)))
			 != BOOLEAN_TYPE)
		    scalar_type = TREE_TYPE (gimple_assign_rhs1 (stmt));
		  else
		    {
		      if (!analyze_pattern_stmt && gsi_end_p (pattern_def_si))
			{
			  pattern_def_seq = NULL;
			  gsi_next (&si);
			}
		      continue;
		    }
		}

	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location,
                                   "get vectype for scalar type:  ");
		  dump_generic_expr (MSG_NOTE, TDF_SLIM, scalar_type);
                  dump_printf (MSG_NOTE, "\n");
		}
	      vectype = get_vectype_for_scalar_type (scalar_type);
	      if (!vectype)
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                       "not vectorized: unsupported "
                                       "data-type ");
		      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
                                         scalar_type);
                      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		    }
		  return false;
		}

	      if (!bool_result)
		STMT_VINFO_VECTYPE (stmt_info) = vectype;

	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location, "vectype: ");
		  dump_generic_expr (MSG_NOTE, TDF_SLIM, vectype);
                  dump_printf (MSG_NOTE, "\n");
		}
            }

	  /* Don't try to compute VF out scalar types if we stmt
	     produces boolean vector.  Use result vectype instead.  */
	  if (VECTOR_BOOLEAN_TYPE_P (vectype))
	    vf_vectype = vectype;
	  else
	    {
	      /* The vectorization factor is according to the smallest
		 scalar type (or the largest vector size, but we only
		 support one vector size per loop).  */
	      if (!bool_result)
		scalar_type = vect_get_smallest_scalar_type (stmt, &dummy,
							     &dummy);
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location,
				   "get vectype for scalar type:  ");
		  dump_generic_expr (MSG_NOTE, TDF_SLIM, scalar_type);
		  dump_printf (MSG_NOTE, "\n");
		}
	      vf_vectype = get_vectype_for_scalar_type (scalar_type);
	    }
	  if (!vf_vectype)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                   "not vectorized: unsupported data-type ");
		  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
                                     scalar_type);
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}
	      return false;
	    }

	  if ((GET_MODE_SIZE (TYPE_MODE (vectype))
	       != GET_MODE_SIZE (TYPE_MODE (vf_vectype))))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                   "not vectorized: different sized vector "
                                   "types in statement, ");
		  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
                                     vectype);
		  dump_printf (MSG_MISSED_OPTIMIZATION, " and ");
		  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
                                     vf_vectype);
                  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}
	      return false;
	    }

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location, "vectype: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, vf_vectype);
              dump_printf (MSG_NOTE, "\n");
	    }

	  nunits = TYPE_VECTOR_SUBPARTS (vf_vectype);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "nunits = %d\n", nunits);
	  if (!vectorization_factor
	      || (nunits > vectorization_factor))
	    vectorization_factor = nunits;

	  if (!analyze_pattern_stmt && gsi_end_p (pattern_def_si))
	    {
	      pattern_def_seq = NULL;
	      gsi_next (&si);
	    }
        }
    }

  /* TODO: Analyze cost. Decide if worth while to vectorize.  */
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "vectorization factor = %d\n",
                     vectorization_factor);
  if (vectorization_factor <= 1)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not vectorized: unsupported data-type\n");
      return false;
    }
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;

  for (i = 0; i < mask_producers.length (); i++)
    {
      tree mask_type = NULL;

      stmt = STMT_VINFO_STMT (mask_producers[i]);

      if (gimple_code (stmt) == GIMPLE_ASSIGN
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_comparison
	  && TREE_CODE (TREE_TYPE (gimple_assign_rhs1 (stmt))) != BOOLEAN_TYPE)
	{
	  scalar_type = TREE_TYPE (gimple_assign_rhs1 (stmt));
	  mask_type = get_mask_type_for_scalar_type (scalar_type);

	  if (!mask_type)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "not vectorized: unsupported mask\n");
	      return false;
	    }
	}
      else
	{
	  tree rhs;
	  ssa_op_iter iter;
	  gimple *def_stmt;
	  enum vect_def_type dt;

	  FOR_EACH_SSA_TREE_OPERAND (rhs, stmt, iter, SSA_OP_USE)
	    {
	      if (!vect_is_simple_use (rhs, mask_producers[i]->vinfo,
				       &def_stmt, &dt, &vectype))
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				       "not vectorized: can't compute mask type "
				       "for statement, ");
		      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION,  TDF_SLIM, stmt,
					0);
		    }
		  return false;
		}

	      /* No vectype probably means external definition.
		 Allow it in case there is another operand which
		 allows to determine mask type.  */
	      if (!vectype)
		continue;

	      if (!mask_type)
		mask_type = vectype;
	      else if (TYPE_VECTOR_SUBPARTS (mask_type)
		       != TYPE_VECTOR_SUBPARTS (vectype))
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				       "not vectorized: different sized masks "
				       "types in statement, ");
		      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					 mask_type);
		      dump_printf (MSG_MISSED_OPTIMIZATION, " and ");
		      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					 vectype);
		      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		    }
		  return false;
		}
	      else if (VECTOR_BOOLEAN_TYPE_P (mask_type)
		       != VECTOR_BOOLEAN_TYPE_P (vectype))
		{
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				       "not vectorized: mixed mask and "
				       "nonmask vector types in statement, ");
		      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					 mask_type);
		      dump_printf (MSG_MISSED_OPTIMIZATION, " and ");
		      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
					 vectype);
		      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		    }
		  return false;
		}
	    }

	  /* We may compare boolean value loaded as vector of integers.
	     Fix mask_type in such case.  */
	  if (mask_type
	      && !VECTOR_BOOLEAN_TYPE_P (mask_type)
	      && gimple_code (stmt) == GIMPLE_ASSIGN
	      && TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_comparison)
	    mask_type = build_same_sized_truth_vector_type (mask_type);
	}

      /* No mask_type should mean loop invariant predicate.
	 This is probably a subject for optimization in
	 if-conversion.  */
      if (!mask_type)
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: can't compute mask type "
			       "for statement, ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION,  TDF_SLIM, stmt,
				0);
	    }
	  return false;
	}

      STMT_VINFO_VECTYPE (mask_producers[i]) = mask_type;
    }

  return true;
}


/* Function vect_is_simple_iv_evolution.

   FORNOW: A simple evolution of an induction variables in the loop is
   considered a polynomial evolution.  */

static bool
vect_is_simple_iv_evolution (unsigned loop_nb, tree access_fn, tree * init,
                             tree * step)
{
  tree init_expr;
  tree step_expr;
  tree evolution_part = evolution_part_in_loop_num (access_fn, loop_nb);
  basic_block bb;

  /* When there is no evolution in this loop, the evolution function
     is not "simple".  */
  if (evolution_part == NULL_TREE)
    return false;

  /* When the evolution is a polynomial of degree >= 2
     the evolution function is not "simple".  */
  if (tree_is_chrec (evolution_part))
    return false;

  step_expr = evolution_part;
  init_expr = unshare_expr (initial_condition_in_loop_num (access_fn, loop_nb));

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "step: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, step_expr);
      dump_printf (MSG_NOTE, ",  init: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, init_expr);
      dump_printf (MSG_NOTE, "\n");
    }

  *init = init_expr;
  *step = step_expr;

  if (TREE_CODE (step_expr) != INTEGER_CST
      && (TREE_CODE (step_expr) != SSA_NAME
	  || ((bb = gimple_bb (SSA_NAME_DEF_STMT (step_expr)))
	      && flow_bb_inside_loop_p (get_loop (cfun, loop_nb), bb))
	  || (!INTEGRAL_TYPE_P (TREE_TYPE (step_expr))
	      && (!SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr))
		  || !flag_associative_math)))
      && (TREE_CODE (step_expr) != REAL_CST
	  || !flag_associative_math))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "step unknown.\n");
      return false;
    }

  return true;
}

/* Function vect_analyze_scalar_cycles_1.

   Examine the cross iteration def-use cycles of scalar variables
   in LOOP.  LOOP_VINFO represents the loop that is now being
   considered for vectorization (can be LOOP, or an outer-loop
   enclosing LOOP).  */

static void
vect_analyze_scalar_cycles_1 (loop_vec_info loop_vinfo, struct loop *loop)
{
  basic_block bb = loop->header;
  tree init, step;
  auto_vec<gimple *, 64> worklist;
  gphi_iterator gsi;
  bool double_reduc;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "=== vect_analyze_scalar_cycles ===\n");

  /* First - identify all inductions.  Reduction detection assumes that all the
     inductions have been identified, therefore, this order must not be
     changed.  */
  for (gsi = gsi_start_phis  (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree access_fn = NULL;
      tree def = PHI_RESULT (phi);
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (phi);

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	}

      /* Skip virtual phi's.  The data dependences that are associated with
         virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.  */
      if (virtual_operand_p (def))
	continue;

      STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_unknown_def_type;

      /* Analyze the evolution function.  */
      access_fn = analyze_scalar_evolution (loop, def);
      if (access_fn)
	{
	  STRIP_NOPS (access_fn);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
                               "Access function of PHI: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, access_fn);
              dump_printf (MSG_NOTE, "\n");
	    }
	  STMT_VINFO_LOOP_PHI_EVOLUTION_BASE_UNCHANGED (stmt_vinfo)
	    = initial_condition_in_loop_num (access_fn, loop->num);
	  STMT_VINFO_LOOP_PHI_EVOLUTION_PART (stmt_vinfo)
	    = evolution_part_in_loop_num (access_fn, loop->num);
	}

      if (!access_fn
	  || !vect_is_simple_iv_evolution (loop->num, access_fn, &init, &step)
	  || (LOOP_VINFO_LOOP (loop_vinfo) != loop
	      && TREE_CODE (step) != INTEGER_CST))
	{
	  worklist.safe_push (phi);
	  continue;
	}

      gcc_assert (STMT_VINFO_LOOP_PHI_EVOLUTION_BASE_UNCHANGED (stmt_vinfo)
		  != NULL_TREE);
      gcc_assert (STMT_VINFO_LOOP_PHI_EVOLUTION_PART (stmt_vinfo) != NULL_TREE);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "Detected induction.\n");
      STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_induction_def;
    }


  /* Second - identify all reductions and nested cycles.  */
  while (worklist.length () > 0)
    {
      gimple *phi = worklist.pop ();
      tree def = PHI_RESULT (phi);
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (phi);
      gimple *reduc_stmt;
      bool nested_cycle;

      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
        }

      gcc_assert (!virtual_operand_p (def)
		  && STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_unknown_def_type);

      nested_cycle = (loop != LOOP_VINFO_LOOP (loop_vinfo));
      reduc_stmt = vect_force_simple_reduction (loop_vinfo, phi, !nested_cycle,
						&double_reduc, false);
      if (reduc_stmt)
        {
          if (double_reduc)
            {
              if (dump_enabled_p ())
                dump_printf_loc (MSG_NOTE, vect_location,
				 "Detected double reduction.\n");

              STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_double_reduction_def;
              STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                    vect_double_reduction_def;
            }
          else
            {
              if (nested_cycle)
                {
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_NOTE, vect_location,
				     "Detected vectorizable nested cycle.\n");

                  STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_nested_cycle;
                  STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                             vect_nested_cycle;
                }
              else
                {
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_NOTE, vect_location,
				     "Detected reduction.\n");

                  STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_reduction_def;
                  STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                           vect_reduction_def;
                  /* Store the reduction cycles for possible vectorization in
                     loop-aware SLP.  */
                  LOOP_VINFO_REDUCTIONS (loop_vinfo).safe_push (reduc_stmt);
                }
            }
        }
      else
        if (dump_enabled_p ())
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Unknown def-use cycle pattern.\n");
    }
}


/* Function vect_analyze_scalar_cycles.

   Examine the cross iteration def-use cycles of scalar variables, by
   analyzing the loop-header PHIs of scalar variables.  Classify each
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

/* Transfer group and reduction information from STMT to its pattern stmt.  */

static void
vect_fixup_reduc_chain (gimple *stmt)
{
  gimple *firstp = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (stmt));
  gimple *stmtp;
  gcc_assert (!GROUP_FIRST_ELEMENT (vinfo_for_stmt (firstp))
	      && GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)));
  GROUP_SIZE (vinfo_for_stmt (firstp)) = GROUP_SIZE (vinfo_for_stmt (stmt));
  do
    {
      stmtp = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (stmt));
      GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmtp)) = firstp;
      stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt));
      if (stmt)
	GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmtp))
	  = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (stmt));
    }
  while (stmt);
  STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmtp)) = vect_reduction_def;
}

/* Fixup scalar cycles that now have their stmts detected as patterns.  */

static void
vect_fixup_scalar_cycles_with_patterns (loop_vec_info loop_vinfo)
{
  gimple *first;
  unsigned i;

  FOR_EACH_VEC_ELT (LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo), i, first)
    if (STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (first)))
      {
	gimple *next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (first));
	while (next)
	  {
	    if (! STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (next)))
	      break;
	    next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next));
	  }
	/* If not all stmt in the chain are patterns try to handle
	   the chain without patterns.  */
	if (! next)
	  {
	    vect_fixup_reduc_chain (first);
	    LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo)[i]
	      = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (first));
	  }
      }
}

/* Function vect_get_loop_niters.

   Determine how many iterations the loop is executed and place it
   in NUMBER_OF_ITERATIONS.  Place the number of latch iterations
   in NUMBER_OF_ITERATIONSM1.

   Return the loop exit condition.  */


static gcond *
vect_get_loop_niters (struct loop *loop, tree *number_of_iterations,
		      tree *number_of_iterationsm1)
{
  tree niters;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== get_loop_niters ===\n");

  niters = number_of_latch_executions (loop);
  *number_of_iterationsm1 = niters;

  /* We want the number of loop header executions which is the number
     of latch executions plus one.
     ???  For UINT_MAX latch executions this number overflows to zero
     for loops like do { n++; } while (n != 0);  */
  if (niters && !chrec_contains_undetermined (niters))
    niters = fold_build2 (PLUS_EXPR, TREE_TYPE (niters), unshare_expr (niters),
			  build_int_cst (TREE_TYPE (niters), 1));
  *number_of_iterations = niters;

  return get_loop_exit_condition (loop);
}


/* Function bb_in_loop_p

   Used as predicate for dfs order traversal of the loop bbs.  */

static bool
bb_in_loop_p (const_basic_block bb, const void *data)
{
  const struct loop *const loop = (const struct loop *)data;
  if (flow_bb_inside_loop_p (loop, bb))
    return true;
  return false;
}


/* Function new_loop_vec_info.

   Create and initialize a new loop_vec_info struct for LOOP, as well as
   stmt_vec_info structs for all the stmts in LOOP.  */

static loop_vec_info
new_loop_vec_info (struct loop *loop)
{
  loop_vec_info res;
  basic_block *bbs;
  gimple_stmt_iterator si;
  unsigned int i, nbbs;

  res = (loop_vec_info) xcalloc (1, sizeof (struct _loop_vec_info));
  res->kind = vec_info::loop;
  LOOP_VINFO_LOOP (res) = loop;

  bbs = get_loop_body (loop);

  /* Create/Update stmt_info for all stmts in the loop.  */
  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *phi = gsi_stmt (si);
	  gimple_set_uid (phi, 0);
	  set_vinfo_for_stmt (phi, new_stmt_vec_info (phi, res));
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  gimple_set_uid (stmt, 0);
	  set_vinfo_for_stmt (stmt, new_stmt_vec_info (stmt, res));
	}
    }

  /* CHECKME: We want to visit all BBs before their successors (except for
     latch blocks, for which this assertion wouldn't hold).  In the simple
     case of the loop forms we allow, a dfs order of the BBs would the same
     as reversed postorder traversal, so we are safe.  */

   free (bbs);
   bbs = XCNEWVEC (basic_block, loop->num_nodes);
   nbbs = dfs_enumerate_from (loop->header, 0, bb_in_loop_p,
                              bbs, loop->num_nodes, loop);
   gcc_assert (nbbs == loop->num_nodes);

  LOOP_VINFO_BBS (res) = bbs;
  LOOP_VINFO_NITERSM1 (res) = NULL;
  LOOP_VINFO_NITERS (res) = NULL;
  LOOP_VINFO_NITERS_UNCHANGED (res) = NULL;
  LOOP_VINFO_COST_MODEL_THRESHOLD (res) = 0;
  LOOP_VINFO_VECTORIZABLE_P (res) = 0;
  LOOP_VINFO_PEELING_FOR_ALIGNMENT (res) = 0;
  LOOP_VINFO_VECT_FACTOR (res) = 0;
  LOOP_VINFO_LOOP_NEST (res) = vNULL;
  LOOP_VINFO_DATAREFS (res) = vNULL;
  LOOP_VINFO_DDRS (res) = vNULL;
  LOOP_VINFO_UNALIGNED_DR (res) = NULL;
  LOOP_VINFO_MAY_MISALIGN_STMTS (res) = vNULL;
  LOOP_VINFO_MAY_ALIAS_DDRS (res) = vNULL;
  LOOP_VINFO_GROUPED_STORES (res) = vNULL;
  LOOP_VINFO_REDUCTIONS (res) = vNULL;
  LOOP_VINFO_REDUCTION_CHAINS (res) = vNULL;
  LOOP_VINFO_SLP_INSTANCES (res) = vNULL;
  LOOP_VINFO_SLP_UNROLLING_FACTOR (res) = 1;
  LOOP_VINFO_TARGET_COST_DATA (res) = init_cost (loop);
  LOOP_VINFO_PEELING_FOR_GAPS (res) = false;
  LOOP_VINFO_PEELING_FOR_NITER (res) = false;
  LOOP_VINFO_OPERANDS_SWAPPED (res) = false;

  return res;
}


/* Function destroy_loop_vec_info.

   Free LOOP_VINFO struct, as well as all the stmt_vec_info structs of all the
   stmts in the loop.  */

void
destroy_loop_vec_info (loop_vec_info loop_vinfo, bool clean_stmts)
{
  struct loop *loop;
  basic_block *bbs;
  int nbbs;
  gimple_stmt_iterator si;
  int j;
  vec<slp_instance> slp_instances;
  slp_instance instance;
  bool swapped;

  if (!loop_vinfo)
    return;

  loop = LOOP_VINFO_LOOP (loop_vinfo);

  bbs = LOOP_VINFO_BBS (loop_vinfo);
  nbbs = clean_stmts ? loop->num_nodes : 0;
  swapped = LOOP_VINFO_OPERANDS_SWAPPED (loop_vinfo);

  for (j = 0; j < nbbs; j++)
    {
      basic_block bb = bbs[j];
      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
        free_stmt_vec_info (gsi_stmt (si));

      for (si = gsi_start_bb (bb); !gsi_end_p (si); )
        {
	  gimple *stmt = gsi_stmt (si);

	  /* We may have broken canonical form by moving a constant
	     into RHS1 of a commutative op.  Fix such occurrences.  */
	  if (swapped && is_gimple_assign (stmt))
	    {
	      enum tree_code code = gimple_assign_rhs_code (stmt);

	      if ((code == PLUS_EXPR
		   || code == POINTER_PLUS_EXPR
		   || code == MULT_EXPR)
		  && CONSTANT_CLASS_P (gimple_assign_rhs1 (stmt)))
		swap_ssa_operands (stmt,
				   gimple_assign_rhs1_ptr (stmt),
				   gimple_assign_rhs2_ptr (stmt));
	    }

	  /* Free stmt_vec_info.  */
	  free_stmt_vec_info (stmt);
          gsi_next (&si);
        }
    }

  free (LOOP_VINFO_BBS (loop_vinfo));
  vect_destroy_datarefs (loop_vinfo);
  free_dependence_relations (LOOP_VINFO_DDRS (loop_vinfo));
  LOOP_VINFO_LOOP_NEST (loop_vinfo).release ();
  LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo).release ();
  LOOP_VINFO_COMP_ALIAS_DDRS (loop_vinfo).release ();
  LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo).release ();
  slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  FOR_EACH_VEC_ELT (slp_instances, j, instance)
    vect_free_slp_instance (instance);

  LOOP_VINFO_SLP_INSTANCES (loop_vinfo).release ();
  LOOP_VINFO_GROUPED_STORES (loop_vinfo).release ();
  LOOP_VINFO_REDUCTIONS (loop_vinfo).release ();
  LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo).release ();

  destroy_cost_data (LOOP_VINFO_TARGET_COST_DATA (loop_vinfo));
  loop_vinfo->scalar_cost_vec.release ();

  free (loop_vinfo);
  loop->aux = NULL;
}


/* Calculate the cost of one scalar iteration of the loop.  */
static void
vect_compute_single_scalar_iteration_cost (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes, factor, scalar_single_iter_cost = 0;
  int innerloop_iters, i;

  /* Count statements in scalar loop.  Using this as scalar cost for a single
     iteration for now.

     TODO: Add outer loop support.

     TODO: Consider assigning different costs to different scalar
     statements.  */

  /* FORNOW.  */
  innerloop_iters = 1;
  if (loop->inner)
    innerloop_iters = 50; /* FIXME */

  for (i = 0; i < nbbs; i++)
    {
      gimple_stmt_iterator si;
      basic_block bb = bbs[i];

      if (bb->loop_father == loop->inner)
        factor = innerloop_iters;
      else
        factor = 1;

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
        {
	  gimple *stmt = gsi_stmt (si);
          stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

          if (!is_gimple_assign (stmt) && !is_gimple_call (stmt))
            continue;

          /* Skip stmts that are not vectorized inside the loop.  */
          if (stmt_info
              && !STMT_VINFO_RELEVANT_P (stmt_info)
              && (!STMT_VINFO_LIVE_P (stmt_info)
                  || !VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (stmt_info)))
	      && !STMT_VINFO_IN_PATTERN_P (stmt_info))
            continue;

	  vect_cost_for_stmt kind;
          if (STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt)))
            {
              if (DR_IS_READ (STMT_VINFO_DATA_REF (vinfo_for_stmt (stmt))))
               kind = scalar_load;
             else
               kind = scalar_store;
            }
          else
            kind = scalar_stmt;

	  scalar_single_iter_cost
	    += record_stmt_cost (&LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo),
				 factor, kind, NULL, 0, vect_prologue);
        }
    }
  LOOP_VINFO_SINGLE_SCALAR_ITERATION_COST (loop_vinfo)
    = scalar_single_iter_cost;
}


/* Function vect_analyze_loop_form_1.

   Verify that certain CFG restrictions hold, including:
   - the loop has a pre-header
   - the loop has a single entry and exit
   - the loop exit condition is simple enough, and the number of iterations
     can be analyzed (a countable loop).  */

bool
vect_analyze_loop_form_1 (struct loop *loop, gcond **loop_cond,
			  tree *number_of_iterationsm1,
			  tree *number_of_iterations, gcond **inner_loop_cond)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_analyze_loop_form ===\n");

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
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: control flow in loop.\n");
          return false;
        }

      if (empty_block_p (loop->header))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: empty loop.\n");
	  return false;
	}
    }
  else
    {
      struct loop *innerloop = loop->inner;
      edge entryedge;

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
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: multiple nested loops.\n");
	  return false;
	}

      if (loop->num_nodes != 5)
        {
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: control flow in loop.\n");
	  return false;
        }

      entryedge = loop_preheader_edge (innerloop);
      if (entryedge->src != loop->header
	  || !single_exit (innerloop)
	  || single_exit (innerloop)->dest != EDGE_PRED (loop->latch, 0)->src)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: unsupported outerloop form.\n");
	  return false;
	}

      /* Analyze the inner-loop.  */
      tree inner_niterm1, inner_niter;
      if (! vect_analyze_loop_form_1 (loop->inner, inner_loop_cond,
				      &inner_niterm1, &inner_niter, NULL))
	{
	  if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: Bad inner loop.\n");
	  return false;
	}

      if (!expr_invariant_in_loop_p (loop, inner_niter))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: inner-loop count not"
                             " invariant.\n");
	  return false;
	}

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "Considering outer-loop vectorization.\n");
    }

  if (!single_exit (loop)
      || EDGE_COUNT (loop->header->preds) != 2)
    {
      if (dump_enabled_p ())
        {
          if (!single_exit (loop))
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: multiple exits.\n");
          else if (EDGE_COUNT (loop->header->preds) != 2)
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: too many incoming edges.\n");
        }
      return false;
    }

  /* We assume that the loop exit condition is at the end of the loop. i.e,
     that the loop is represented as a do-while (with a proper if-guard
     before the loop if needed), where the loop header contains all the
     executable statements, and the latch is empty.  */
  if (!empty_block_p (loop->latch)
      || !gimple_seq_empty_p (phi_nodes (loop->latch)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: latch block not empty.\n");
      return false;
    }

  /* Make sure there exists a single-predecessor exit bb:  */
  if (!single_pred_p (single_exit (loop)->dest))
    {
      edge e = single_exit (loop);
      if (!(e->flags & EDGE_ABNORMAL))
	{
	  split_loop_exit_edge (e);
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "split exit edge.\n");
	}
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: abnormal loop exit edge.\n");
	  return false;
	}
    }

  *loop_cond = vect_get_loop_niters (loop, number_of_iterations,
				     number_of_iterationsm1);
  if (!*loop_cond)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: complicated exit condition.\n");
      return false;
    }

  if (!*number_of_iterations
      || chrec_contains_undetermined (*number_of_iterations))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: number of iterations cannot be "
			 "computed.\n");
      return false;
    }

  if (integer_zerop (*number_of_iterations))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: number of iterations = 0.\n");
      return false;
    }

  return true;
}

/* Analyze LOOP form and return a loop_vec_info if it is of suitable form.  */

loop_vec_info
vect_analyze_loop_form (struct loop *loop)
{
  tree number_of_iterations, number_of_iterationsm1;
  gcond *loop_cond, *inner_loop_cond = NULL;

  if (! vect_analyze_loop_form_1 (loop, &loop_cond, &number_of_iterationsm1,
				  &number_of_iterations, &inner_loop_cond))
    return NULL;

  loop_vec_info loop_vinfo = new_loop_vec_info (loop);
  LOOP_VINFO_NITERSM1 (loop_vinfo) = number_of_iterationsm1;
  LOOP_VINFO_NITERS (loop_vinfo) = number_of_iterations;
  LOOP_VINFO_NITERS_UNCHANGED (loop_vinfo) = number_of_iterations;

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    {
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
			   "Symbolic number of iterations is ");
	  dump_generic_expr (MSG_NOTE, TDF_DETAILS, number_of_iterations);
          dump_printf (MSG_NOTE, "\n");
        }
    }

  STMT_VINFO_TYPE (vinfo_for_stmt (loop_cond)) = loop_exit_ctrl_vec_info_type;
  if (inner_loop_cond)
    STMT_VINFO_TYPE (vinfo_for_stmt (inner_loop_cond))
      = loop_exit_ctrl_vec_info_type;

  gcc_assert (!loop->aux);
  loop->aux = loop_vinfo;
  return loop_vinfo;
}



/* Scan the loop stmts and dependent on whether there are any (non-)SLP
   statements update the vectorization factor.  */

static void
vect_update_vf_for_slp (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  unsigned int vectorization_factor;
  int i;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_update_vf_for_slp ===\n");

  vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  gcc_assert (vectorization_factor != 0);

  /* If all the stmts in the loop can be SLPed, we perform only SLP, and
     vectorization factor of the loop is the unrolling factor required by
     the SLP instances.  If that unrolling factor is 1, we say, that we
     perform pure SLP on loop - cross iteration parallelism is not
     exploited.  */
  bool only_slp_in_loop = true;
  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  if (STMT_VINFO_IN_PATTERN_P (stmt_info)
	      && STMT_VINFO_RELATED_STMT (stmt_info))
	    {
	      stmt = STMT_VINFO_RELATED_STMT (stmt_info);
	      stmt_info = vinfo_for_stmt (stmt);
	    }
	  if ((STMT_VINFO_RELEVANT_P (stmt_info)
	       || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (stmt_info)))
	      && !PURE_SLP_STMT (stmt_info))
	    /* STMT needs both SLP and loop-based vectorization.  */
	    only_slp_in_loop = false;
	}
    }

  if (only_slp_in_loop)
    vectorization_factor = LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo);
  else
    vectorization_factor
      = least_common_multiple (vectorization_factor,
			       LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo));

  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Updating vectorization factor to %d\n",
		     vectorization_factor);
}

/* Function vect_analyze_loop_operations.

   Scan the loop stmts and make sure they are all vectorizable.  */

static bool
vect_analyze_loop_operations (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  int i;
  stmt_vec_info stmt_info;
  bool need_to_vectorize = false;
  bool ok;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_analyze_loop_operations ===\n");

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
        {
          gphi *phi = si.phi ();
          ok = true;

          stmt_info = vinfo_for_stmt (phi);
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_NOTE, vect_location, "examining phi: ");
              dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
            }
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    continue;

          /* Inner-loop loop-closed exit phi in outer-loop vectorization
             (i.e., a phi in the tail of the outer-loop).  */
          if (! is_loop_header_bb_p (bb))
            {
              /* FORNOW: we currently don't support the case that these phis
                 are not used in the outerloop (unless it is double reduction,
                 i.e., this phi is vect_reduction_def), cause this case
                 requires to actually do something here.  */
              if ((!STMT_VINFO_RELEVANT_P (stmt_info)
                   || STMT_VINFO_LIVE_P (stmt_info))
                  && STMT_VINFO_DEF_TYPE (stmt_info)
                     != vect_double_reduction_def)
                {
                  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Unsupported loop-closed phi in "
				     "outer-loop.\n");
                  return false;
                }

              /* If PHI is used in the outer loop, we check that its operand
                 is defined in the inner loop.  */
              if (STMT_VINFO_RELEVANT_P (stmt_info))
                {
                  tree phi_op;
		  gimple *op_def_stmt;

                  if (gimple_phi_num_args (phi) != 1)
                    return false;

                  phi_op = PHI_ARG_DEF (phi, 0);
                  if (TREE_CODE (phi_op) != SSA_NAME)
                    return false;

                  op_def_stmt = SSA_NAME_DEF_STMT (phi_op);
		  if (gimple_nop_p (op_def_stmt)
		      || !flow_bb_inside_loop_p (loop, gimple_bb (op_def_stmt))
		      || !vinfo_for_stmt (op_def_stmt))
                    return false;

                  if (STMT_VINFO_RELEVANT (vinfo_for_stmt (op_def_stmt))
                        != vect_used_in_outer
                      && STMT_VINFO_RELEVANT (vinfo_for_stmt (op_def_stmt))
                           != vect_used_in_outer_by_reduction)
                    return false;
                }

              continue;
            }

          gcc_assert (stmt_info);

          if (STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope
              && STMT_VINFO_DEF_TYPE (stmt_info) != vect_induction_def)
            {
              /* A scalar-dependence cycle that we don't support.  */
              if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "not vectorized: scalar dependence cycle.\n");
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
              if (dump_enabled_p ())
                {
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "not vectorized: relevant phi not "
				   "supported: ");
                  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, phi, 0);
                }
	      return false;
            }
        }

      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
        {
	  gimple *stmt = gsi_stmt (si);
	  if (!gimple_clobber_p (stmt)
	      && !vect_analyze_stmt (stmt, &need_to_vectorize, NULL))
	    return false;
        }
    } /* bbs */

  /* All operations in the loop are either irrelevant (deal with loop
     control, or dead), or only used outside the loop and can be moved
     out of the loop (e.g. invariants, inductions).  The loop can be
     optimized away by scalar optimizations.  We're better off not
     touching this loop.  */
  if (!need_to_vectorize)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "All the computation can be taken out of the loop.\n");
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: redundant loop. no profit to "
			 "vectorize.\n");
      return false;
    }

  return true;
}


/* Function vect_analyze_loop_2.

   Apply a set of analyses on LOOP, and create a loop_vec_info struct
   for it.  The different analyses will record information in the
   loop_vec_info struct.  */
static bool
vect_analyze_loop_2 (loop_vec_info loop_vinfo, bool &fatal)
{
  bool ok;
  int max_vf = MAX_VECTORIZATION_FACTOR;
  int min_vf = 2;
  unsigned int n_stmts = 0;

  /* The first group of checks is independent of the vector size.  */
  fatal = true;

  /* Find all data references in the loop (which correspond to vdefs/vuses)
     and analyze their evolution in the loop.  */

  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);

  loop_p loop = LOOP_VINFO_LOOP (loop_vinfo);
  if (!find_loop_nest (loop, &LOOP_VINFO_LOOP_NEST (loop_vinfo)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: loop nest containing two "
			 "or more consecutive inner loops cannot be "
			 "vectorized\n");
      return false;
    }

  for (unsigned i = 0; i < loop->num_nodes; i++)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (is_gimple_debug (stmt))
	  continue;
	++n_stmts;
	if (!find_data_references_in_stmt (loop, stmt,
					   &LOOP_VINFO_DATAREFS (loop_vinfo)))
	  {
	    if (is_gimple_call (stmt) && loop->safelen)
	      {
		tree fndecl = gimple_call_fndecl (stmt), op;
		if (fndecl != NULL_TREE)
		  {
		    cgraph_node *node = cgraph_node::get (fndecl);
		    if (node != NULL && node->simd_clones != NULL)
		      {
			unsigned int j, n = gimple_call_num_args (stmt);
			for (j = 0; j < n; j++)
			  {
			    op = gimple_call_arg (stmt, j);
			    if (DECL_P (op)
				|| (REFERENCE_CLASS_P (op)
				    && get_base_address (op)))
			      break;
			  }
			op = gimple_call_lhs (stmt);
			/* Ignore #pragma omp declare simd functions
			   if they don't have data references in the
			   call stmt itself.  */
			if (j == n
			    && !(op
				 && (DECL_P (op)
				     || (REFERENCE_CLASS_P (op)
					 && get_base_address (op)))))
			  continue;
		      }
		  }
	      }
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: loop contains function "
			       "calls or data references that cannot "
			       "be analyzed\n");
	    return false;
	  }
      }

  /* Analyze the data references and also adjust the minimal
     vectorization factor according to the loads and stores.  */

  ok = vect_analyze_data_refs (loop_vinfo, &min_vf);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data references.\n");
      return false;
    }

  /* Classify all cross-iteration scalar data-flow cycles.
     Cross-iteration cycles caused by virtual phis are analyzed separately.  */
  vect_analyze_scalar_cycles (loop_vinfo);

  vect_pattern_recog (loop_vinfo);

  vect_fixup_scalar_cycles_with_patterns (loop_vinfo);

  /* Analyze the access patterns of the data-refs in the loop (consecutive,
     complex, etc.). FORNOW: Only handle consecutive access pattern.  */

  ok = vect_analyze_data_ref_accesses (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data access.\n");
      return false;
    }

  /* Data-flow analysis to detect stmts that do not need to be vectorized.  */

  ok = vect_mark_stmts_to_be_vectorized (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "unexpected pattern.\n");
      return false;
    }

  /* While the rest of the analysis below depends on it in some way.  */
  fatal = false;

  /* Analyze data dependences between the data-refs in the loop
     and adjust the maximum vectorization factor according to
     the dependences.
     FORNOW: fail at the first data dependence that we encounter.  */

  ok = vect_analyze_data_ref_dependences (loop_vinfo, &max_vf);
  if (!ok
      || max_vf < min_vf)
    {
      if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "bad data dependence.\n");
      return false;
    }

  ok = vect_determine_vectorization_factor (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't determine vectorization factor.\n");
      return false;
    }
  if (max_vf < LOOP_VINFO_VECT_FACTOR (loop_vinfo))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data dependence.\n");
      return false;
    }

  /* Compute the scalar iteration cost.  */
  vect_compute_single_scalar_iteration_cost (loop_vinfo);

  int saved_vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  HOST_WIDE_INT estimated_niter;
  unsigned th;
  int min_scalar_loop_bound;

  /* Check the SLP opportunities in the loop, analyze and build SLP trees.  */
  ok = vect_analyze_slp (loop_vinfo, n_stmts);
  if (!ok)
    return false;

  /* If there are any SLP instances mark them as pure_slp.  */
  bool slp = vect_make_slp_decision (loop_vinfo);
  if (slp)
    {
      /* Find stmts that need to be both vectorized and SLPed.  */
      vect_detect_hybrid_slp (loop_vinfo);

      /* Update the vectorization factor based on the SLP decision.  */
      vect_update_vf_for_slp (loop_vinfo);
    }

  /* This is the point where we can re-start analysis with SLP forced off.  */
start_over:

  /* Now the vectorization factor is final.  */
  unsigned vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  gcc_assert (vectorization_factor != 0);

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo) && dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "vectorization_factor = %d, niters = "
		     HOST_WIDE_INT_PRINT_DEC "\n", vectorization_factor,
		     LOOP_VINFO_INT_NITERS (loop_vinfo));

  HOST_WIDE_INT max_niter
    = likely_max_stmt_executions_int (LOOP_VINFO_LOOP (loop_vinfo));
  if ((LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
       && (LOOP_VINFO_INT_NITERS (loop_vinfo) < vectorization_factor))
      || (max_niter != -1
	  && (unsigned HOST_WIDE_INT) max_niter < vectorization_factor))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: iteration count smaller than "
			 "vectorization factor.\n");
      return false;
    }

  /* Analyze the alignment of the data-refs in the loop.
     Fail if a data reference is found that cannot be vectorized.  */

  ok = vect_analyze_data_refs_alignment (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data alignment.\n");
      return false;
    }

  /* Prune the list of ddrs to be tested at run-time by versioning for alias.
     It is important to call pruning after vect_analyze_data_ref_accesses,
     since we use grouping information gathered by interleaving analysis.  */
  ok = vect_prune_runtime_alias_test_list (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "number of versioning for alias "
			 "run-time tests exceeds %d "
			 "(--param vect-max-version-for-alias-checks)\n",
			 PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS));
      return false;
    }

  /* This pass will decide on using loop versioning and/or loop peeling in
     order to enhance the alignment of data references in the loop.  */
  ok = vect_enhance_data_refs_alignment (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data alignment.\n");
      return false;
    }

  if (slp)
    {
      /* Analyze operations in the SLP instances.  Note this may
	 remove unsupported SLP instances which makes the above
	 SLP kind detection invalid.  */
      unsigned old_size = LOOP_VINFO_SLP_INSTANCES (loop_vinfo).length ();
      vect_slp_analyze_operations (LOOP_VINFO_SLP_INSTANCES (loop_vinfo),
				   LOOP_VINFO_TARGET_COST_DATA (loop_vinfo));
      if (LOOP_VINFO_SLP_INSTANCES (loop_vinfo).length () != old_size)
	goto again;
    }

  /* Scan all the remaining operations in the loop that are not subject
     to SLP and make sure they are vectorizable.  */
  ok = vect_analyze_loop_operations (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad operation or unsupported loop bound.\n");
      return false;
    }

  /* Analyze cost.  Decide if worth while to vectorize.  */
  int min_profitable_estimate, min_profitable_iters;
  vect_estimate_min_profitable_iters (loop_vinfo, &min_profitable_iters,
				      &min_profitable_estimate);

  if (min_profitable_iters < 0)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: vectorization not profitable.\n");
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: vector version will never be "
			 "profitable.\n");
      goto again;
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

  LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo) = th;

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && LOOP_VINFO_INT_NITERS (loop_vinfo) <= th)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: vectorization not profitable.\n");
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "not vectorized: iteration count smaller than user "
			 "specified loop bound parameter or minimum profitable "
			 "iterations (whichever is more conservative).\n");
      goto again;
    }

  estimated_niter
    = estimated_stmt_executions_int (LOOP_VINFO_LOOP (loop_vinfo));
  if (estimated_niter == -1)
    estimated_niter = max_niter;
  if (estimated_niter != -1
      && ((unsigned HOST_WIDE_INT) estimated_niter
          <= MAX (th, (unsigned)min_profitable_estimate)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: estimated iteration count too "
                         "small.\n");
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "not vectorized: estimated iteration count smaller "
                         "than specified loop bound parameter or minimum "
                         "profitable iterations (whichever is more "
                         "conservative).\n");
      goto again;
    }

  /* Decide whether we need to create an epilogue loop to handle
     remaining scalar iterations.  */
  th = ((LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo) + 1)
        / LOOP_VINFO_VECT_FACTOR (loop_vinfo))
       * LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) > 0)
    {
      if (ctz_hwi (LOOP_VINFO_INT_NITERS (loop_vinfo)
		   - LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo))
	  < exact_log2 (LOOP_VINFO_VECT_FACTOR (loop_vinfo)))
	LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = true;
    }
  else if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo)
	   || (tree_ctz (LOOP_VINFO_NITERS (loop_vinfo))
	       < (unsigned)exact_log2 (LOOP_VINFO_VECT_FACTOR (loop_vinfo))
               /* In case of versioning, check if the maximum number of
                  iterations is greater than th.  If they are identical,
                  the epilogue is unnecessary.  */
	       && ((!LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo)
	            && !LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo))
                   || (unsigned HOST_WIDE_INT) max_niter > th)))
    LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = true;

  /* If an epilogue loop is required make sure we can create one.  */
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo)
      || LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location, "epilog loop required\n");
      if (!vect_can_advance_ivs_p (loop_vinfo)
	  || !slpeel_can_duplicate_loop_p (LOOP_VINFO_LOOP (loop_vinfo),
					   single_exit (LOOP_VINFO_LOOP
							 (loop_vinfo))))
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: can't create required "
			     "epilog loop\n");
          goto again;
        }
    }

  gcc_assert (vectorization_factor
	      == (unsigned)LOOP_VINFO_VECT_FACTOR (loop_vinfo));

  /* Ok to vectorize!  */
  return true;

again:
  /* Try again with SLP forced off but if we didn't do any SLP there is
     no point in re-trying.  */
  if (!slp)
    return false;

  /* If there are reduction chains re-trying will fail anyway.  */
  if (! LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo).is_empty ())
    return false;

  /* Likewise if the grouped loads or stores in the SLP cannot be handled
     via interleaving or lane instructions.  */
  slp_instance instance;
  slp_tree node;
  unsigned i, j;
  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (loop_vinfo), i, instance)
    {
      stmt_vec_info vinfo;
      vinfo = vinfo_for_stmt
	  (SLP_TREE_SCALAR_STMTS (SLP_INSTANCE_TREE (instance))[0]);
      if (! STMT_VINFO_GROUPED_ACCESS (vinfo))
	continue;
      vinfo = vinfo_for_stmt (STMT_VINFO_GROUP_FIRST_ELEMENT (vinfo));
      unsigned int size = STMT_VINFO_GROUP_SIZE (vinfo);
      tree vectype = STMT_VINFO_VECTYPE (vinfo);
      if (! vect_store_lanes_supported (vectype, size)
	  && ! vect_grouped_store_supported (vectype, size))
	return false;
      FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (instance), j, node)
	{
	  vinfo = vinfo_for_stmt (SLP_TREE_SCALAR_STMTS (node)[0]);
	  vinfo = vinfo_for_stmt (STMT_VINFO_GROUP_FIRST_ELEMENT (vinfo));
	  size = STMT_VINFO_GROUP_SIZE (vinfo);
	  vectype = STMT_VINFO_VECTYPE (vinfo);
	  if (! vect_load_lanes_supported (vectype, size)
	      && ! vect_grouped_load_supported (vectype, size))
	    return false;
	}
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "re-trying with SLP disabled\n");

  /* Roll back state appropriately.  No SLP this time.  */
  slp = false;
  /* Restore vectorization factor as it were without SLP.  */
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = saved_vectorization_factor;
  /* Free the SLP instances.  */
  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (loop_vinfo), j, instance)
    vect_free_slp_instance (instance);
  LOOP_VINFO_SLP_INSTANCES (loop_vinfo).release ();
  /* Reset SLP type to loop_vect on all stmts.  */
  for (i = 0; i < LOOP_VINFO_LOOP (loop_vinfo)->num_nodes; ++i)
    {
      basic_block bb = LOOP_VINFO_BBS (loop_vinfo)[i];
      for (gimple_stmt_iterator si = gsi_start_bb (bb);
	   !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info stmt_info = vinfo_for_stmt (gsi_stmt (si));
	  STMT_SLP_TYPE (stmt_info) = loop_vect;
	  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
	    {
	      stmt_info = vinfo_for_stmt (STMT_VINFO_RELATED_STMT (stmt_info));
	      STMT_SLP_TYPE (stmt_info) = loop_vect;
	      for (gimple_stmt_iterator pi
		     = gsi_start (STMT_VINFO_PATTERN_DEF_SEQ (stmt_info));
		   !gsi_end_p (pi); gsi_next (&pi))
		{
		  gimple *pstmt = gsi_stmt (pi);
		  STMT_SLP_TYPE (vinfo_for_stmt (pstmt)) = loop_vect;
		}
	    }
	}
    }
  /* Free optimized alias test DDRS.  */
  LOOP_VINFO_COMP_ALIAS_DDRS (loop_vinfo).release ();
  /* Reset target cost data.  */
  destroy_cost_data (LOOP_VINFO_TARGET_COST_DATA (loop_vinfo));
  LOOP_VINFO_TARGET_COST_DATA (loop_vinfo)
    = init_cost (LOOP_VINFO_LOOP (loop_vinfo));
  /* Reset assorted flags.  */
  LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = false;
  LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) = false;
  LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo) = 0;

  goto start_over;
}

/* Function vect_analyze_loop.

   Apply a set of analyses on LOOP, and create a loop_vec_info struct
   for it.  The different analyses will record information in the
   loop_vec_info struct.  */
loop_vec_info
vect_analyze_loop (struct loop *loop)
{
  loop_vec_info loop_vinfo;
  unsigned int vector_sizes;

  /* Autodetect first vector size we try.  */
  current_vector_size = 0;
  vector_sizes = targetm.vectorize.autovectorize_vector_sizes ();

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "===== analyze_loop_nest =====\n");

  if (loop_outer (loop)
      && loop_vec_info_for_loop (loop_outer (loop))
      && LOOP_VINFO_VECTORIZABLE_P (loop_vec_info_for_loop (loop_outer (loop))))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "outer-loop already vectorized.\n");
      return NULL;
    }

  while (1)
    {
      /* Check the CFG characteristics of the loop (nesting, entry/exit).  */
      loop_vinfo = vect_analyze_loop_form (loop);
      if (!loop_vinfo)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "bad loop form.\n");
	  return NULL;
	}

      bool fatal = false;
      if (vect_analyze_loop_2 (loop_vinfo, fatal))
	{
	  LOOP_VINFO_VECTORIZABLE_P (loop_vinfo) = 1;

	  return loop_vinfo;
	}

      destroy_loop_vec_info (loop_vinfo, true);

      vector_sizes &= ~current_vector_size;
      if (fatal
	  || vector_sizes == 0
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


/* Function reduction_code_for_scalar_code

   Input:
   CODE - tree_code of a reduction operations.

   Output:
   REDUC_CODE - the corresponding tree-code to be used to reduce the
      vector of partial results into a single scalar result, or ERROR_MARK
      if the operation is a supported reduction operation, but does not have
      such a tree-code.

   Return FALSE if CODE currently cannot be vectorized as reduction.  */

static bool
reduction_code_for_scalar_code (enum tree_code code,
                                enum tree_code *reduc_code)
{
  switch (code)
    {
      case MAX_EXPR:
        *reduc_code = REDUC_MAX_EXPR;
        return true;

      case MIN_EXPR:
        *reduc_code = REDUC_MIN_EXPR;
        return true;

      case PLUS_EXPR:
        *reduc_code = REDUC_PLUS_EXPR;
        return true;

      case MULT_EXPR:
      case MINUS_EXPR:
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
      case BIT_AND_EXPR:
        *reduc_code = ERROR_MARK;
        return true;

      default:
       return false;
    }
}


/* Error reporting helper for vect_is_simple_reduction below.  GIMPLE statement
   STMT is printed with a message MSG. */

static void
report_vect_op (int msg_type, gimple *stmt, const char *msg)
{
  dump_printf_loc (msg_type, vect_location, "%s", msg);
  dump_gimple_stmt (msg_type, TDF_SLIM, stmt, 0);
}


/* Detect SLP reduction of the form:

   #a1 = phi <a5, a0>
   a2 = operation (a1)
   a3 = operation (a2)
   a4 = operation (a3)
   a5 = operation (a4)

   #a = phi <a5>

   PHI is the reduction phi node (#a1 = phi <a5, a0> above)
   FIRST_STMT is the first reduction stmt in the chain
   (a2 = operation (a1)).

   Return TRUE if a reduction chain was detected.  */

static bool
vect_is_slp_reduction (loop_vec_info loop_info, gimple *phi,
		       gimple *first_stmt)
{
  struct loop *loop = (gimple_bb (phi))->loop_father;
  struct loop *vect_loop = LOOP_VINFO_LOOP (loop_info);
  enum tree_code code;
  gimple *current_stmt = NULL, *loop_use_stmt = NULL, *first, *next_stmt;
  stmt_vec_info use_stmt_info, current_stmt_info;
  tree lhs;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  int nloop_uses, size = 0, n_out_of_loop_uses;
  bool found = false;

  if (loop != vect_loop)
    return false;

  lhs = PHI_RESULT (phi);
  code = gimple_assign_rhs_code (first_stmt);
  while (1)
    {
      nloop_uses = 0;
      n_out_of_loop_uses = 0;
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
        {
	  gimple *use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;

          /* Check if we got back to the reduction phi.  */
	  if (use_stmt == phi)
            {
	      loop_use_stmt = use_stmt;
              found = true;
              break;
            }

          if (flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
            {
	      loop_use_stmt = use_stmt;
	      nloop_uses++;
            }
           else
             n_out_of_loop_uses++;

           /* There are can be either a single use in the loop or two uses in
              phi nodes.  */
           if (nloop_uses > 1 || (n_out_of_loop_uses && nloop_uses))
             return false;
        }

      if (found)
        break;

      /* We reached a statement with no loop uses.  */
      if (nloop_uses == 0)
	return false;

      /* This is a loop exit phi, and we haven't reached the reduction phi.  */
      if (gimple_code (loop_use_stmt) == GIMPLE_PHI)
        return false;

      if (!is_gimple_assign (loop_use_stmt)
	  || code != gimple_assign_rhs_code (loop_use_stmt)
	  || !flow_bb_inside_loop_p (loop, gimple_bb (loop_use_stmt)))
        return false;

      /* Insert USE_STMT into reduction chain.  */
      use_stmt_info = vinfo_for_stmt (loop_use_stmt);
      if (current_stmt)
        {
          current_stmt_info = vinfo_for_stmt (current_stmt);
	  GROUP_NEXT_ELEMENT (current_stmt_info) = loop_use_stmt;
          GROUP_FIRST_ELEMENT (use_stmt_info)
            = GROUP_FIRST_ELEMENT (current_stmt_info);
        }
      else
	GROUP_FIRST_ELEMENT (use_stmt_info) = loop_use_stmt;

      lhs = gimple_assign_lhs (loop_use_stmt);
      current_stmt = loop_use_stmt;
      size++;
   }

  if (!found || loop_use_stmt != phi || size < 2)
    return false;

  /* Swap the operands, if needed, to make the reduction operand be the second
     operand.  */
  lhs = PHI_RESULT (phi);
  next_stmt = GROUP_FIRST_ELEMENT (vinfo_for_stmt (current_stmt));
  while (next_stmt)
    {
      if (gimple_assign_rhs2 (next_stmt) == lhs)
	{
	  tree op = gimple_assign_rhs1 (next_stmt);
	  gimple *def_stmt = NULL;

          if (TREE_CODE (op) == SSA_NAME)
            def_stmt = SSA_NAME_DEF_STMT (op);

	  /* Check that the other def is either defined in the loop
	     ("vect_internal_def"), or it's an induction (defined by a
	     loop-header phi-node).  */
          if (def_stmt
              && gimple_bb (def_stmt)
	      && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
              && (is_gimple_assign (def_stmt)
                  || is_gimple_call (def_stmt)
                  || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt))
                           == vect_induction_def
                  || (gimple_code (def_stmt) == GIMPLE_PHI
                      && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt))
                                  == vect_internal_def
                      && !is_loop_header_bb_p (gimple_bb (def_stmt)))))
	    {
	      lhs = gimple_assign_lhs (next_stmt);
	      next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
 	      continue;
	    }

	  return false;
	}
      else
	{
          tree op = gimple_assign_rhs2 (next_stmt);
	  gimple *def_stmt = NULL;

          if (TREE_CODE (op) == SSA_NAME)
            def_stmt = SSA_NAME_DEF_STMT (op);

          /* Check that the other def is either defined in the loop
            ("vect_internal_def"), or it's an induction (defined by a
            loop-header phi-node).  */
          if (def_stmt
              && gimple_bb (def_stmt)
	      && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
              && (is_gimple_assign (def_stmt)
                  || is_gimple_call (def_stmt)
                  || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt))
                              == vect_induction_def
                  || (gimple_code (def_stmt) == GIMPLE_PHI
                      && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt))
                                  == vect_internal_def
                      && !is_loop_header_bb_p (gimple_bb (def_stmt)))))
  	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location, "swapping oprnds: ");
		  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, next_stmt, 0);
		}

	      swap_ssa_operands (next_stmt,
	 		         gimple_assign_rhs1_ptr (next_stmt),
                                 gimple_assign_rhs2_ptr (next_stmt));
	      update_stmt (next_stmt);

	      if (CONSTANT_CLASS_P (gimple_assign_rhs1 (next_stmt)))
		LOOP_VINFO_OPERANDS_SWAPPED (loop_info) = true;
	    }
	  else
	    return false;
        }

      lhs = gimple_assign_lhs (next_stmt);
      next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
    }

  /* Save the chain for further analysis in SLP detection.  */
  first = GROUP_FIRST_ELEMENT (vinfo_for_stmt (current_stmt));
  LOOP_VINFO_REDUCTION_CHAINS (loop_info).safe_push (first);
  GROUP_SIZE (vinfo_for_stmt (first)) = size;

  return true;
}


/* Function vect_is_simple_reduction_1

   (1) Detect a cross-iteration def-use cycle that represents a simple
   reduction computation.  We look for the following pattern:

   loop_header:
     a1 = phi < a0, a2 >
     a3 = ...
     a2 = operation (a3, a1)

   or

   a3 = ...
   loop_header:
     a1 = phi < a0, a2 >
     a2 = operation (a3, a1)

   such that:
   1. operation is commutative and associative and it is safe to
      change the order of the computation (if CHECK_REDUCTION is true)
   2. no uses for a2 in the loop (a2 is used out of the loop)
   3. no uses of a1 in the loop besides the reduction operation
   4. no uses of a1 outside the loop.

   Conditions 1,4 are tested here.
   Conditions 2,3 are tested in vect_mark_stmts_to_be_vectorized.

   (2) Detect a cross-iteration def-use cycle in nested loops, i.e.,
   nested cycles, if CHECK_REDUCTION is false.

   (3) Detect cycles of phi nodes in outer-loop vectorization, i.e., double
   reductions:

     a1 = phi < a0, a2 >
     inner loop (def of a3)
     a2 = phi < a3 >

   (4) Detect condition expressions, ie:
     for (int i = 0; i < N; i++)
       if (a[i] < val)
	ret_val = a[i];

*/

static gimple *
vect_is_simple_reduction (loop_vec_info loop_info, gimple *phi,
			  bool check_reduction, bool *double_reduc,
			  bool need_wrapping_integral_overflow,
			  enum vect_reduction_type *v_reduc_type)
{
  struct loop *loop = (gimple_bb (phi))->loop_father;
  struct loop *vect_loop = LOOP_VINFO_LOOP (loop_info);
  edge latch_e = loop_latch_edge (loop);
  tree loop_arg = PHI_ARG_DEF_FROM_EDGE (phi, latch_e);
  gimple *def_stmt, *def1 = NULL, *def2 = NULL, *phi_use_stmt = NULL;
  enum tree_code orig_code, code;
  tree op1, op2, op3 = NULL_TREE, op4 = NULL_TREE;
  tree type;
  int nloop_uses;
  tree name;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  bool phi_def;

  *double_reduc = false;
  *v_reduc_type = TREE_CODE_REDUCTION;

  /* If CHECK_REDUCTION is true, we assume inner-most loop vectorization,
     otherwise, we assume outer loop vectorization.  */
  gcc_assert ((check_reduction && loop == vect_loop)
              || (!check_reduction && flow_loop_nested_p (vect_loop, loop)));

  name = PHI_RESULT (phi);
  /* ???  If there are no uses of the PHI result the inner loop reduction
     won't be detected as possibly double-reduction by vectorizable_reduction
     because that tries to walk the PHI arg from the preheader edge which
     can be constant.  See PR60382.  */
  if (has_zero_uses (name))
    return NULL;
  nloop_uses = 0;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, name)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;

      if (!flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "intermediate value used outside loop.\n");

          return NULL;
        }

      nloop_uses++;
      if (nloop_uses > 1)
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "reduction used in loop.\n");
          return NULL;
        }

      phi_use_stmt = use_stmt;
    }

  if (TREE_CODE (loop_arg) != SSA_NAME)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "reduction: not ssa_name: ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, loop_arg);
          dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}
      return NULL;
    }

  def_stmt = SSA_NAME_DEF_STMT (loop_arg);
  if (!def_stmt)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "reduction: no def_stmt.\n");
      return NULL;
    }

  if (!is_gimple_assign (def_stmt) && gimple_code (def_stmt) != GIMPLE_PHI)
    {
      if (dump_enabled_p ())
	dump_gimple_stmt (MSG_NOTE, TDF_SLIM, def_stmt, 0);
      return NULL;
    }

  if (is_gimple_assign (def_stmt))
    {
      name = gimple_assign_lhs (def_stmt);
      phi_def = false;
    }
  else
    {
      name = PHI_RESULT (def_stmt);
      phi_def = true;
    }

  nloop_uses = 0;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, name)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;
      if (flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
	nloop_uses++;
      if (nloop_uses > 1)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "reduction used in loop.\n");
	  return NULL;
	}
    }

  /* If DEF_STMT is a phi node itself, we expect it to have a single argument
     defined in the inner loop.  */
  if (phi_def)
    {
      op1 = PHI_ARG_DEF (def_stmt, 0);

      if (gimple_phi_num_args (def_stmt) != 1
          || TREE_CODE (op1) != SSA_NAME)
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported phi node definition.\n");

          return NULL;
        }

      def1 = SSA_NAME_DEF_STMT (op1);
      if (gimple_bb (def1)
	  && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
          && loop->inner
          && flow_bb_inside_loop_p (loop->inner, gimple_bb (def1))
          && is_gimple_assign (def1)
	  && flow_bb_inside_loop_p (loop->inner, gimple_bb (phi_use_stmt)))
        {
          if (dump_enabled_p ())
            report_vect_op (MSG_NOTE, def_stmt,
			    "detected double reduction: ");

          *double_reduc = true;
          return def_stmt;
        }

      return NULL;
    }

  code = orig_code = gimple_assign_rhs_code (def_stmt);

  /* We can handle "res -= x[i]", which is non-associative by
     simply rewriting this into "res += -x[i]".  Avoid changing
     gimple instruction for the first simple tests and only do this
     if we're allowed to change code at all.  */
  if (code == MINUS_EXPR
      && (op1 = gimple_assign_rhs1 (def_stmt))
      && TREE_CODE (op1) == SSA_NAME
      && SSA_NAME_DEF_STMT (op1) == phi)
    code = PLUS_EXPR;

  if (code == COND_EXPR)
    {
      if (check_reduction)
	*v_reduc_type = COND_REDUCTION;
    }
  else if (!commutative_tree_code (code) || !associative_tree_code (code))
    {
      if (dump_enabled_p ())
	report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
			"reduction: not commutative/associative: ");
      return NULL;
    }

  if (get_gimple_rhs_class (code) != GIMPLE_BINARY_RHS)
    {
      if (code != COND_EXPR)
        {
	  if (dump_enabled_p ())
	    report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
			    "reduction: not binary operation: ");

          return NULL;
        }

      op3 = gimple_assign_rhs1 (def_stmt);
      if (COMPARISON_CLASS_P (op3))
        {
          op4 = TREE_OPERAND (op3, 1);
          op3 = TREE_OPERAND (op3, 0);
        }

      op1 = gimple_assign_rhs2 (def_stmt);
      op2 = gimple_assign_rhs3 (def_stmt);

      if (TREE_CODE (op1) != SSA_NAME && TREE_CODE (op2) != SSA_NAME)
        {
          if (dump_enabled_p ())
            report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
			    "reduction: uses not ssa_names: ");

          return NULL;
        }
    }
  else
    {
      op1 = gimple_assign_rhs1 (def_stmt);
      op2 = gimple_assign_rhs2 (def_stmt);

      if (TREE_CODE (op1) != SSA_NAME && TREE_CODE (op2) != SSA_NAME)
        {
          if (dump_enabled_p ())
	    report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
			    "reduction: uses not ssa_names: ");

          return NULL;
        }
   }

  type = TREE_TYPE (gimple_assign_lhs (def_stmt));
  if ((TREE_CODE (op1) == SSA_NAME
       && !types_compatible_p (type,TREE_TYPE (op1)))
      || (TREE_CODE (op2) == SSA_NAME
          && !types_compatible_p (type, TREE_TYPE (op2)))
      || (op3 && TREE_CODE (op3) == SSA_NAME
          && !types_compatible_p (type, TREE_TYPE (op3)))
      || (op4 && TREE_CODE (op4) == SSA_NAME
          && !types_compatible_p (type, TREE_TYPE (op4))))
    {
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
			   "reduction: multiple types: operation type: ");
          dump_generic_expr (MSG_NOTE, TDF_SLIM, type);
          dump_printf (MSG_NOTE, ", operands types: ");
          dump_generic_expr (MSG_NOTE, TDF_SLIM,
			     TREE_TYPE (op1));
          dump_printf (MSG_NOTE, ",");
          dump_generic_expr (MSG_NOTE, TDF_SLIM,
			     TREE_TYPE (op2));
          if (op3)
            {
              dump_printf (MSG_NOTE, ",");
              dump_generic_expr (MSG_NOTE, TDF_SLIM,
				 TREE_TYPE (op3));
            }

          if (op4)
            {
              dump_printf (MSG_NOTE, ",");
              dump_generic_expr (MSG_NOTE, TDF_SLIM,
				 TREE_TYPE (op4));
            }
          dump_printf (MSG_NOTE, "\n");
        }

      return NULL;
    }

  /* Check that it's ok to change the order of the computation.
     Generally, when vectorizing a reduction we change the order of the
     computation.  This may change the behavior of the program in some
     cases, so we need to check that this is ok.  One exception is when
     vectorizing an outer-loop: the inner-loop is executed sequentially,
     and therefore vectorizing reductions in the inner-loop during
     outer-loop vectorization is safe.  */

  if (*v_reduc_type != COND_REDUCTION
      && check_reduction)
    {
      /* CHECKME: check for !flag_finite_math_only too?  */
      if (SCALAR_FLOAT_TYPE_P (type) && !flag_associative_math)
	{
	  /* Changing the order of operations changes the semantics.  */
	  if (dump_enabled_p ())
	    report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
			"reduction: unsafe fp math optimization: ");
	  return NULL;
	}
      else if (INTEGRAL_TYPE_P (type))
	{
	  if (!operation_no_trapping_overflow (type, code))
	    {
	      /* Changing the order of operations changes the semantics.  */
	      if (dump_enabled_p ())
		report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
				"reduction: unsafe int math optimization"
				" (overflow traps): ");
	      return NULL;
	    }
	  if (need_wrapping_integral_overflow
	      && !TYPE_OVERFLOW_WRAPS (type)
	      && operation_can_overflow (code))
	    {
	      /* Changing the order of operations changes the semantics.  */
	      if (dump_enabled_p ())
		report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
				"reduction: unsafe int math optimization"
				" (overflow doesn't wrap): ");
	      return NULL;
	    }
	}
      else if (SAT_FIXED_POINT_TYPE_P (type))
	{
	  /* Changing the order of operations changes the semantics.  */
	  if (dump_enabled_p ())
	  report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
			  "reduction: unsafe fixed-point math optimization: ");
	  return NULL;
	}
    }

  /* Reduction is safe. We're dealing with one of the following:
     1) integer arithmetic and no trapv
     2) floating point arithmetic, and special flags permit this optimization
     3) nested cycle (i.e., outer loop vectorization).  */
  if (TREE_CODE (op1) == SSA_NAME)
    def1 = SSA_NAME_DEF_STMT (op1);

  if (TREE_CODE (op2) == SSA_NAME)
    def2 = SSA_NAME_DEF_STMT (op2);

  if (code != COND_EXPR
      && ((!def1 || gimple_nop_p (def1)) && (!def2 || gimple_nop_p (def2))))
    {
      if (dump_enabled_p ())
	report_vect_op (MSG_NOTE, def_stmt, "reduction: no defs for operands: ");
      return NULL;
    }

  /* Check that one def is the reduction def, defined by PHI,
     the other def is either defined in the loop ("vect_internal_def"),
     or it's an induction (defined by a loop-header phi-node).  */

  if (def2 && def2 == phi
      && (code == COND_EXPR
	  || !def1 || gimple_nop_p (def1)
	  || !flow_bb_inside_loop_p (loop, gimple_bb (def1))
          || (def1 && flow_bb_inside_loop_p (loop, gimple_bb (def1))
              && (is_gimple_assign (def1)
		  || is_gimple_call (def1)
  	          || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def1))
                      == vect_induction_def
   	          || (gimple_code (def1) == GIMPLE_PHI
	              && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def1))
                          == vect_internal_def
 	              && !is_loop_header_bb_p (gimple_bb (def1)))))))
    {
      if (dump_enabled_p ())
	report_vect_op (MSG_NOTE, def_stmt, "detected reduction: ");
      return def_stmt;
    }

  if (def1 && def1 == phi
      && (code == COND_EXPR
	  || !def2 || gimple_nop_p (def2)
	  || !flow_bb_inside_loop_p (loop, gimple_bb (def2))
          || (def2 && flow_bb_inside_loop_p (loop, gimple_bb (def2))
 	      && (is_gimple_assign (def2)
		  || is_gimple_call (def2)
	          || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def2))
                      == vect_induction_def
 	          || (gimple_code (def2) == GIMPLE_PHI
		      && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def2))
                          == vect_internal_def
		      && !is_loop_header_bb_p (gimple_bb (def2)))))))
    {
      if (check_reduction
	  && orig_code != MINUS_EXPR)
        {
	  if (code == COND_EXPR)
	    {
	      /* No current known use where this case would be useful.  */
	      if (dump_enabled_p ())
		report_vect_op (MSG_NOTE, def_stmt,
				"detected reduction: cannot currently swap "
				"operands for cond_expr");
	      return NULL;
	    }

          /* Swap operands (just for simplicity - so that the rest of the code
	     can assume that the reduction variable is always the last (second)
	     argument).  */
          if (dump_enabled_p ())
	    report_vect_op (MSG_NOTE, def_stmt,
	  	            "detected reduction: need to swap operands: ");

          swap_ssa_operands (def_stmt, gimple_assign_rhs1_ptr (def_stmt),
 			     gimple_assign_rhs2_ptr (def_stmt));

	  if (CONSTANT_CLASS_P (gimple_assign_rhs1 (def_stmt)))
	    LOOP_VINFO_OPERANDS_SWAPPED (loop_info) = true;
        }
      else
        {
          if (dump_enabled_p ())
            report_vect_op (MSG_NOTE, def_stmt, "detected reduction: ");
        }

      return def_stmt;
    }

  /* Try to find SLP reduction chain.  */
  if (check_reduction && code != COND_EXPR
      && vect_is_slp_reduction (loop_info, phi, def_stmt))
    {
      if (dump_enabled_p ())
        report_vect_op (MSG_NOTE, def_stmt,
			"reduction: detected reduction chain: ");

      return def_stmt;
    }

  if (dump_enabled_p ())
    report_vect_op (MSG_MISSED_OPTIMIZATION, def_stmt,
		    "reduction: unknown pattern: ");
       
  return NULL;
}

/* Wrapper around vect_is_simple_reduction_1, which will modify code
   in-place if it enables detection of more reductions.  Arguments
   as there.  */

gimple *
vect_force_simple_reduction (loop_vec_info loop_info, gimple *phi,
			     bool check_reduction, bool *double_reduc,
			     bool need_wrapping_integral_overflow)
{
  enum vect_reduction_type v_reduc_type;
  return vect_is_simple_reduction (loop_info, phi, check_reduction,
				   double_reduc,
				   need_wrapping_integral_overflow,
				   &v_reduc_type);
}

/* Calculate cost of peeling the loop PEEL_ITERS_PROLOGUE times.  */
int
vect_get_known_peeling_cost (loop_vec_info loop_vinfo, int peel_iters_prologue,
                             int *peel_iters_epilogue,
                             stmt_vector_for_cost *scalar_cost_vec,
			     stmt_vector_for_cost *prologue_cost_vec,
			     stmt_vector_for_cost *epilogue_cost_vec)
{
  int retval = 0;
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    {
      *peel_iters_epilogue = vf/2;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "cost model: epilogue peel iters set to vf/2 "
			 "because loop iterations are unknown .\n");

      /* If peeled iterations are known but number of scalar loop
         iterations are unknown, count a taken branch per peeled loop.  */
      retval = record_stmt_cost (prologue_cost_vec, 1, cond_branch_taken,
				 NULL, 0, vect_prologue);
      retval = record_stmt_cost (prologue_cost_vec, 1, cond_branch_taken,
				 NULL, 0, vect_epilogue);
    }
  else
    {
      int niters = LOOP_VINFO_INT_NITERS (loop_vinfo);
      peel_iters_prologue = niters < peel_iters_prologue ?
                            niters : peel_iters_prologue;
      *peel_iters_epilogue = (niters - peel_iters_prologue) % vf;
      /* If we need to peel for gaps, but no peeling is required, we have to
	 peel VF iterations.  */
      if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) && !*peel_iters_epilogue)
        *peel_iters_epilogue = vf;
    }

  stmt_info_for_cost *si;
  int j;
  if (peel_iters_prologue)
    FOR_EACH_VEC_ELT (*scalar_cost_vec, j, si)
      retval += record_stmt_cost (prologue_cost_vec,
				  si->count * peel_iters_prologue,
				  si->kind, NULL, si->misalign,
				  vect_prologue);
  if (*peel_iters_epilogue)
    FOR_EACH_VEC_ELT (*scalar_cost_vec, j, si)
      retval += record_stmt_cost (epilogue_cost_vec,
				  si->count * *peel_iters_epilogue,
				  si->kind, NULL, si->misalign,
				  vect_epilogue);

  return retval;
}

/* Function vect_estimate_min_profitable_iters

   Return the number of iterations required for the vector version of the
   loop to be profitable relative to the cost of the scalar version of the
   loop.  */

static void
vect_estimate_min_profitable_iters (loop_vec_info loop_vinfo,
				    int *ret_min_profitable_niters,
				    int *ret_min_profitable_estimate)
{
  int min_profitable_iters;
  int min_profitable_estimate;
  int peel_iters_prologue;
  int peel_iters_epilogue;
  unsigned vec_inside_cost = 0;
  int vec_outside_cost = 0;
  unsigned vec_prologue_cost = 0;
  unsigned vec_epilogue_cost = 0;
  int scalar_single_iter_cost = 0;
  int scalar_outside_cost = 0;
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  int npeel = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
  void *target_cost_data = LOOP_VINFO_TARGET_COST_DATA (loop_vinfo);

  /* Cost model disabled.  */
  if (unlimited_cost_model (LOOP_VINFO_LOOP (loop_vinfo)))
    {
      dump_printf_loc (MSG_NOTE, vect_location, "cost model disabled.\n");
      *ret_min_profitable_niters = 0;
      *ret_min_profitable_estimate = 0;
      return;
    }

  /* Requires loop versioning tests to handle misalignment.  */
  if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo))
    {
      /*  FIXME: Make cost depend on complexity of individual check.  */
      unsigned len = LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo).length ();
      (void) add_stmt_cost (target_cost_data, len, vector_stmt, NULL, 0,
			    vect_prologue);
      dump_printf (MSG_NOTE,
                   "cost model: Adding cost of checks for loop "
                   "versioning to treat misalignment.\n");
    }

  /* Requires loop versioning with alias checks.  */
  if (LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    {
      /*  FIXME: Make cost depend on complexity of individual check.  */
      unsigned len = LOOP_VINFO_COMP_ALIAS_DDRS (loop_vinfo).length ();
      (void) add_stmt_cost (target_cost_data, len, vector_stmt, NULL, 0,
			    vect_prologue);
      dump_printf (MSG_NOTE,
                   "cost model: Adding cost of checks for loop "
                   "versioning aliasing.\n");
    }

  if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    (void) add_stmt_cost (target_cost_data, 1, cond_branch_taken, NULL, 0,
			  vect_prologue);

  /* Count statements in scalar loop.  Using this as scalar cost for a single
     iteration for now.

     TODO: Add outer loop support.

     TODO: Consider assigning different costs to different scalar
     statements.  */

  scalar_single_iter_cost
    = LOOP_VINFO_SINGLE_SCALAR_ITERATION_COST (loop_vinfo);

  /* Add additional cost for the peeled instructions in prologue and epilogue
     loop.

     FORNOW: If we don't know the value of peel_iters for prologue or epilogue
     at compile-time - we assume it's vf/2 (the worst would be vf-1).

     TODO: Build an expression that represents peel_iters for prologue and
     epilogue to be used in a run-time test.  */

  if (npeel  < 0)
    {
      peel_iters_prologue = vf/2;
      dump_printf (MSG_NOTE, "cost model: "
                   "prologue peel iters set to vf/2.\n");

      /* If peeling for alignment is unknown, loop bound of main loop becomes
         unknown.  */
      peel_iters_epilogue = vf/2;
      dump_printf (MSG_NOTE, "cost model: "
                   "epilogue peel iters set to vf/2 because "
                   "peeling for alignment is unknown.\n");

      /* If peeled iterations are unknown, count a taken branch and a not taken
         branch per peeled loop. Even if scalar loop iterations are known,
         vector iterations are not known since peeled prologue iterations are
         not known. Hence guards remain the same.  */
      (void) add_stmt_cost (target_cost_data, 1, cond_branch_taken,
			    NULL, 0, vect_prologue);
      (void) add_stmt_cost (target_cost_data, 1, cond_branch_not_taken,
			    NULL, 0, vect_prologue);
      (void) add_stmt_cost (target_cost_data, 1, cond_branch_taken,
			    NULL, 0, vect_epilogue);
      (void) add_stmt_cost (target_cost_data, 1, cond_branch_not_taken,
			    NULL, 0, vect_epilogue);
      stmt_info_for_cost *si;
      int j;
      FOR_EACH_VEC_ELT (LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo), j, si)
	{
	  struct _stmt_vec_info *stmt_info
	    = si->stmt ? vinfo_for_stmt (si->stmt) : NULL;
	  (void) add_stmt_cost (target_cost_data,
				si->count * peel_iters_prologue,
				si->kind, stmt_info, si->misalign,
				vect_prologue);
	  (void) add_stmt_cost (target_cost_data,
				si->count * peel_iters_epilogue,
				si->kind, stmt_info, si->misalign,
				vect_epilogue);
	}
    }
  else
    {
      stmt_vector_for_cost prologue_cost_vec, epilogue_cost_vec;
      stmt_info_for_cost *si;
      int j;
      void *data = LOOP_VINFO_TARGET_COST_DATA (loop_vinfo);

      prologue_cost_vec.create (2);
      epilogue_cost_vec.create (2);
      peel_iters_prologue = npeel;

      (void) vect_get_known_peeling_cost (loop_vinfo, peel_iters_prologue,
					  &peel_iters_epilogue,
					  &LOOP_VINFO_SCALAR_ITERATION_COST
					    (loop_vinfo),
					  &prologue_cost_vec,
					  &epilogue_cost_vec);

      FOR_EACH_VEC_ELT (prologue_cost_vec, j, si)
	{
	  struct _stmt_vec_info *stmt_info
	    = si->stmt ? vinfo_for_stmt (si->stmt) : NULL;
	  (void) add_stmt_cost (data, si->count, si->kind, stmt_info,
				si->misalign, vect_prologue);
	}

      FOR_EACH_VEC_ELT (epilogue_cost_vec, j, si)
	{
	  struct _stmt_vec_info *stmt_info
	    = si->stmt ? vinfo_for_stmt (si->stmt) : NULL;
	  (void) add_stmt_cost (data, si->count, si->kind, stmt_info,
				si->misalign, vect_epilogue);
	}

      prologue_cost_vec.release ();
      epilogue_cost_vec.release ();
    }

  /* FORNOW: The scalar outside cost is incremented in one of the
     following ways:

     1. The vectorizer checks for alignment and aliasing and generates
     a condition that allows dynamic vectorization.  A cost model
     check is ANDED with the versioning condition.  Hence scalar code
     path now has the added cost of the versioning check.

       if (cost > th & versioning_check)
         jmp to vector code

     Hence run-time scalar is incremented by not-taken branch cost.

     2. The vectorizer then checks if a prologue is required.  If the
     cost model check was not done before during versioning, it has to
     be done before the prologue check.

       if (cost <= th)
         prologue = scalar_iters
       if (prologue == 0)
         jmp to vector code
       else
         execute prologue
       if (prologue == num_iters)
	 go to exit

     Hence the run-time scalar cost is incremented by a taken branch,
     plus a not-taken branch, plus a taken branch cost.

     3. The vectorizer then checks if an epilogue is required.  If the
     cost model check was not done before during prologue check, it
     has to be done with the epilogue check.

       if (prologue == 0)
         jmp to vector code
       else
         execute prologue
       if (prologue == num_iters)
	 go to exit
       vector code:
         if ((cost <= th) | (scalar_iters-prologue-epilogue == 0))
           jmp to epilogue

     Hence the run-time scalar cost should be incremented by 2 taken
     branches.

     TODO: The back end may reorder the BBS's differently and reverse
     conditions/branch directions.  Change the estimates below to
     something more reasonable.  */

  /* If the number of iterations is known and we do not do versioning, we can
     decide whether to vectorize at compile time.  Hence the scalar version
     do not carry cost model guard costs.  */
  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    {
      /* Cost model check occurs at versioning.  */
      if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
          || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
	scalar_outside_cost += vect_get_stmt_cost (cond_branch_not_taken);
      else
	{
	  /* Cost model check occurs at prologue generation.  */
	  if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) < 0)
	    scalar_outside_cost += 2 * vect_get_stmt_cost (cond_branch_taken)
	      + vect_get_stmt_cost (cond_branch_not_taken); 
	  /* Cost model check occurs at epilogue generation.  */
	  else
	    scalar_outside_cost += 2 * vect_get_stmt_cost (cond_branch_taken); 
	}
    }

  /* Complete the target-specific cost calculations.  */
  finish_cost (LOOP_VINFO_TARGET_COST_DATA (loop_vinfo), &vec_prologue_cost,
	       &vec_inside_cost, &vec_epilogue_cost);

  vec_outside_cost = (int)(vec_prologue_cost + vec_epilogue_cost);
  
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "Cost model analysis: \n");
      dump_printf (MSG_NOTE, "  Vector inside of loop cost: %d\n",
                   vec_inside_cost);
      dump_printf (MSG_NOTE, "  Vector prologue cost: %d\n",
                   vec_prologue_cost);
      dump_printf (MSG_NOTE, "  Vector epilogue cost: %d\n",
                   vec_epilogue_cost);
      dump_printf (MSG_NOTE, "  Scalar iteration cost: %d\n",
                   scalar_single_iter_cost);
      dump_printf (MSG_NOTE, "  Scalar outside cost: %d\n",
                   scalar_outside_cost);
      dump_printf (MSG_NOTE, "  Vector outside cost: %d\n",
                   vec_outside_cost);
      dump_printf (MSG_NOTE, "  prologue iterations: %d\n",
                   peel_iters_prologue);
      dump_printf (MSG_NOTE, "  epilogue iterations: %d\n",
                   peel_iters_epilogue);
    }

  /* Calculate number of iterations required to make the vector version
     profitable, relative to the loop bodies only.  The following condition
     must hold true:
     SIC * niters + SOC > VIC * ((niters-PL_ITERS-EP_ITERS)/VF) + VOC
     where
     SIC = scalar iteration cost, VIC = vector iteration cost,
     VOC = vector outside cost, VF = vectorization factor,
     PL_ITERS = prologue iterations, EP_ITERS= epilogue iterations
     SOC = scalar outside cost for run time cost model check.  */

  if ((scalar_single_iter_cost * vf) > (int) vec_inside_cost)
    {
      if (vec_outside_cost <= 0)
        min_profitable_iters = 1;
      else
        {
          min_profitable_iters = ((vec_outside_cost - scalar_outside_cost) * vf
				  - vec_inside_cost * peel_iters_prologue
                                  - vec_inside_cost * peel_iters_epilogue)
                                 / ((scalar_single_iter_cost * vf)
                                    - vec_inside_cost);

          if ((scalar_single_iter_cost * vf * min_profitable_iters)
              <= (((int) vec_inside_cost * min_profitable_iters)
                  + (((int) vec_outside_cost - scalar_outside_cost) * vf)))
            min_profitable_iters++;
        }
    }
  /* vector version will never be profitable.  */
  else
    {
      if (LOOP_VINFO_LOOP (loop_vinfo)->force_vectorize)
	warning_at (vect_location, OPT_Wopenmp_simd, "vectorization "
		    "did not happen for a simd loop");

      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "cost model: the vector iteration cost = %d "
			 "divided by the scalar iteration cost = %d "
			 "is greater or equal to the vectorization factor = %d"
                         ".\n",
			 vec_inside_cost, scalar_single_iter_cost, vf);
      *ret_min_profitable_niters = -1;
      *ret_min_profitable_estimate = -1;
      return;
    }

  dump_printf (MSG_NOTE,
	       "  Calculated minimum iters for profitability: %d\n",
	       min_profitable_iters);

  min_profitable_iters =
	min_profitable_iters < vf ? vf : min_profitable_iters;

  /* Because the condition we create is:
     if (niters <= min_profitable_iters)
       then skip the vectorized loop.  */
  min_profitable_iters--;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "  Runtime profitability threshold = %d\n",
                     min_profitable_iters);

  *ret_min_profitable_niters = min_profitable_iters;

  /* Calculate number of iterations required to make the vector version
     profitable, relative to the loop bodies only.

     Non-vectorized variant is SIC * niters and it must win over vector
     variant on the expected loop trip count.  The following condition must hold true:
     SIC * niters > VIC * ((niters-PL_ITERS-EP_ITERS)/VF) + VOC + SOC  */

  if (vec_outside_cost <= 0)
    min_profitable_estimate = 1;
  else
    {
      min_profitable_estimate = ((vec_outside_cost + scalar_outside_cost) * vf
				 - vec_inside_cost * peel_iters_prologue
				 - vec_inside_cost * peel_iters_epilogue)
				 / ((scalar_single_iter_cost * vf)
				   - vec_inside_cost);
    }
  min_profitable_estimate --;
  min_profitable_estimate = MAX (min_profitable_estimate, min_profitable_iters);
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "  Static estimate profitability threshold = %d\n",
		     min_profitable_estimate);

  *ret_min_profitable_estimate = min_profitable_estimate;
}

/* Writes into SEL a mask for a vec_perm, equivalent to a vec_shr by OFFSET
   vector elements (not bits) for a vector of mode MODE.  */
static void
calc_vec_perm_mask_for_shift (enum machine_mode mode, unsigned int offset,
			      unsigned char *sel)
{
  unsigned int i, nelt = GET_MODE_NUNITS (mode);

  for (i = 0; i < nelt; i++)
    sel[i] = (i + offset) & (2*nelt - 1);
}

/* Checks whether the target supports whole-vector shifts for vectors of mode
   MODE.  This is the case if _either_ the platform handles vec_shr_optab, _or_
   it supports vec_perm_const with masks for all necessary shift amounts.  */
static bool
have_whole_vector_shift (enum machine_mode mode)
{
  if (optab_handler (vec_shr_optab, mode) != CODE_FOR_nothing)
    return true;

  if (direct_optab_handler (vec_perm_const_optab, mode) == CODE_FOR_nothing)
    return false;

  unsigned int i, nelt = GET_MODE_NUNITS (mode);
  unsigned char *sel = XALLOCAVEC (unsigned char, nelt);

  for (i = nelt/2; i >= 1; i/=2)
    {
      calc_vec_perm_mask_for_shift (mode, i, sel);
      if (!can_vec_perm_p (mode, false, sel))
	return false;
    }
  return true;
}

/* Return the reduction operand (with index REDUC_INDEX) of STMT.  */

static tree
get_reduction_op (gimple *stmt, int reduc_index)
{
  switch (get_gimple_rhs_class (gimple_assign_rhs_code (stmt)))
    {
    case GIMPLE_SINGLE_RHS:
      gcc_assert (TREE_OPERAND_LENGTH (gimple_assign_rhs1 (stmt))
		  == ternary_op);
      return TREE_OPERAND (gimple_assign_rhs1 (stmt), reduc_index);
    case GIMPLE_UNARY_RHS:
      return gimple_assign_rhs1 (stmt);
    case GIMPLE_BINARY_RHS:
      return (reduc_index
	      ? gimple_assign_rhs2 (stmt) : gimple_assign_rhs1 (stmt));
    case GIMPLE_TERNARY_RHS:
      return gimple_op (stmt, reduc_index + 1);
    default:
      gcc_unreachable ();
    }
}

/* TODO: Close dependency between vect_model_*_cost and vectorizable_*
   functions. Design better to avoid maintenance issues.  */

/* Function vect_model_reduction_cost.

   Models cost for a reduction operation, including the vector ops
   generated within the strip-mine loop, the initial definition before
   the loop, and the epilogue code that must be generated.  */

static bool
vect_model_reduction_cost (stmt_vec_info stmt_info, enum tree_code reduc_code,
			   int ncopies, int reduc_index)
{
  int prologue_cost = 0, epilogue_cost = 0;
  enum tree_code code;
  optab optab;
  tree vectype;
  gimple *stmt, *orig_stmt;
  tree reduction_op;
  machine_mode mode;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  void *target_cost_data;

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      target_cost_data = LOOP_VINFO_TARGET_COST_DATA (loop_vinfo);
    }
  else
    target_cost_data = BB_VINFO_TARGET_COST_DATA (STMT_VINFO_BB_VINFO (stmt_info));

  /* Condition reductions generate two reductions in the loop.  */
  if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) == COND_REDUCTION)
    ncopies *= 2;

  /* Cost of reduction op inside loop.  */
  unsigned inside_cost = add_stmt_cost (target_cost_data, ncopies, vector_stmt,
					stmt_info, 0, vect_body);
  stmt = STMT_VINFO_STMT (stmt_info);

  reduction_op = get_reduction_op (stmt, reduc_index);

  vectype = get_vectype_for_scalar_type (TREE_TYPE (reduction_op));
  if (!vectype)
    {
      if (dump_enabled_p ())
        {
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "unsupported data-type ");
          dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
			     TREE_TYPE (reduction_op));
          dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
        }
      return false;
   }

  mode = TYPE_MODE (vectype);
  orig_stmt = STMT_VINFO_RELATED_STMT (stmt_info);

  if (!orig_stmt)
    orig_stmt = STMT_VINFO_STMT (stmt_info);

  code = gimple_assign_rhs_code (orig_stmt);

  /* Add in cost for initial definition.
     For cond reduction we have four vectors: initial index, step, initial
     result of the data reduction, initial value of the index reduction.  */
  int prologue_stmts = STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
		       == COND_REDUCTION ? 4 : 1;
  prologue_cost += add_stmt_cost (target_cost_data, prologue_stmts,
				  scalar_to_vec, stmt_info, 0,
				  vect_prologue);

  /* Determine cost of epilogue code.

     We have a reduction operator that will reduce the vector in one statement.
     Also requires scalar extract.  */

  if (!loop || !nested_in_vect_loop_p (loop, orig_stmt))
    {
      if (reduc_code != ERROR_MARK)
	{
	  if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) == COND_REDUCTION)
	    {
	      /* An EQ stmt and an COND_EXPR stmt.  */
	      epilogue_cost += add_stmt_cost (target_cost_data, 2,
					      vector_stmt, stmt_info, 0,
					      vect_epilogue);
	      /* Reduction of the max index and a reduction of the found
		 values.  */
	      epilogue_cost += add_stmt_cost (target_cost_data, 2,
					      vec_to_scalar, stmt_info, 0,
					      vect_epilogue);
	      /* A broadcast of the max value.  */
	      epilogue_cost += add_stmt_cost (target_cost_data, 1,
					      scalar_to_vec, stmt_info, 0,
					      vect_epilogue);
	    }
	  else
	    {
	      epilogue_cost += add_stmt_cost (target_cost_data, 1, vector_stmt,
					      stmt_info, 0, vect_epilogue);
	      epilogue_cost += add_stmt_cost (target_cost_data, 1,
					      vec_to_scalar, stmt_info, 0,
					      vect_epilogue);
	    }
	}
      else
	{
	  int vec_size_in_bits = tree_to_uhwi (TYPE_SIZE (vectype));
	  tree bitsize =
	    TYPE_SIZE (TREE_TYPE (gimple_assign_lhs (orig_stmt)));
	  int element_bitsize = tree_to_uhwi (bitsize);
	  int nelements = vec_size_in_bits / element_bitsize;

	  optab = optab_for_tree_code (code, vectype, optab_default);

	  /* We have a whole vector shift available.  */
	  if (VECTOR_MODE_P (mode)
	      && optab_handler (optab, mode) != CODE_FOR_nothing
	      && have_whole_vector_shift (mode))
	    {
	      /* Final reduction via vector shifts and the reduction operator.
		 Also requires scalar extract.  */
	      epilogue_cost += add_stmt_cost (target_cost_data,
					      exact_log2 (nelements) * 2,
					      vector_stmt, stmt_info, 0,
					      vect_epilogue);
	      epilogue_cost += add_stmt_cost (target_cost_data, 1,
					      vec_to_scalar, stmt_info, 0,
					      vect_epilogue);
	    }	  
	  else
	    /* Use extracts and reduction op for final reduction.  For N
	       elements, we have N extracts and N-1 reduction ops.  */
	    epilogue_cost += add_stmt_cost (target_cost_data, 
					    nelements + nelements - 1,
					    vector_stmt, stmt_info, 0,
					    vect_epilogue);
	}
    }

  if (dump_enabled_p ())
    dump_printf (MSG_NOTE, 
                 "vect_model_reduction_cost: inside_cost = %d, "
                 "prologue_cost = %d, epilogue_cost = %d .\n", inside_cost,
                 prologue_cost, epilogue_cost);

  return true;
}


/* Function vect_model_induction_cost.

   Models cost for induction operations.  */

static void
vect_model_induction_cost (stmt_vec_info stmt_info, int ncopies)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  void *target_cost_data = LOOP_VINFO_TARGET_COST_DATA (loop_vinfo);
  unsigned inside_cost, prologue_cost;

  /* loop cost for vec_loop.  */
  inside_cost = add_stmt_cost (target_cost_data, ncopies, vector_stmt,
			       stmt_info, 0, vect_body);

  /* prologue cost for vec_init and vec_step.  */
  prologue_cost = add_stmt_cost (target_cost_data, 2, scalar_to_vec,
				 stmt_info, 0, vect_prologue);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_induction_cost: inside_cost = %d, "
                     "prologue_cost = %d .\n", inside_cost, prologue_cost);
}


/* Function get_initial_def_for_induction

   Input:
   STMT - a stmt that performs an induction operation in the loop.
   IV_PHI - the initial value of the induction variable

   Output:
   Return a vector variable, initialized with the first VF values of
   the induction variable.  E.g., for an iv with IV_PHI='X' and
   evolution S, for a vector of 4 units, we want to return:
   [X, X + S, X + 2*S, X + 3*S].  */

static tree
get_initial_def_for_induction (gimple *iv_phi)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (iv_phi);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree vectype;
  int nunits;
  edge pe = loop_preheader_edge (loop);
  struct loop *iv_loop;
  basic_block new_bb;
  tree new_vec, vec_init, vec_step, t;
  tree new_name;
  gimple *new_stmt;
  gphi *induction_phi;
  tree induc_def, vec_def, vec_dest;
  tree init_expr, step_expr;
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  int i;
  int ncopies;
  tree expr;
  stmt_vec_info phi_info = vinfo_for_stmt (iv_phi);
  bool nested_in_vect_loop = false;
  gimple_seq stmts;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  gimple *exit_phi;
  edge latch_e;
  tree loop_arg;
  gimple_stmt_iterator si;
  basic_block bb = gimple_bb (iv_phi);
  tree stepvectype;
  tree resvectype;

  /* Is phi in an inner-loop, while vectorizing an enclosing outer-loop?  */
  if (nested_in_vect_loop_p (loop, iv_phi))
    {
      nested_in_vect_loop = true;
      iv_loop = loop->inner;
    }
  else
    iv_loop = loop;
  gcc_assert (iv_loop == (gimple_bb (iv_phi))->loop_father);

  latch_e = loop_latch_edge (iv_loop);
  loop_arg = PHI_ARG_DEF_FROM_EDGE (iv_phi, latch_e);

  step_expr = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (phi_info);
  gcc_assert (step_expr != NULL_TREE);

  pe = loop_preheader_edge (iv_loop);
  init_expr = PHI_ARG_DEF_FROM_EDGE (iv_phi,
				     loop_preheader_edge (iv_loop));

  vectype = get_vectype_for_scalar_type (TREE_TYPE (init_expr));
  resvectype = get_vectype_for_scalar_type (TREE_TYPE (PHI_RESULT (iv_phi)));
  gcc_assert (vectype);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  ncopies = vf / nunits;

  gcc_assert (phi_info);
  gcc_assert (ncopies >= 1);

  /* Convert the step to the desired type.  */
  stmts = NULL;
  step_expr = gimple_convert (&stmts, TREE_TYPE (vectype), step_expr);
  if (stmts)
    {
      new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
      gcc_assert (!new_bb);
    }

  /* Find the first insertion point in the BB.  */
  si = gsi_after_labels (bb);

  /* Create the vector that holds the initial_value of the induction.  */
  if (nested_in_vect_loop)
    {
      /* iv_loop is nested in the loop to be vectorized.  init_expr had already
	 been created during vectorization of previous stmts.  We obtain it
	 from the STMT_VINFO_VEC_STMT of the defining stmt.  */
      vec_init = vect_get_vec_def_for_operand (init_expr, iv_phi);
      /* If the initial value is not of proper type, convert it.  */
      if (!useless_type_conversion_p (vectype, TREE_TYPE (vec_init)))
	{
	  new_stmt
	    = gimple_build_assign (vect_get_new_ssa_name (vectype,
							  vect_simple_var,
							  "vec_iv_"),
				   VIEW_CONVERT_EXPR,
				   build1 (VIEW_CONVERT_EXPR, vectype,
					   vec_init));
	  vec_init = gimple_assign_lhs (new_stmt);
	  new_bb = gsi_insert_on_edge_immediate (loop_preheader_edge (iv_loop),
						 new_stmt);
	  gcc_assert (!new_bb);
	  set_vinfo_for_stmt (new_stmt,
			      new_stmt_vec_info (new_stmt, loop_vinfo));
	}
    }
  else
    {
      vec<constructor_elt, va_gc> *v;

      /* iv_loop is the loop to be vectorized. Create:
	 vec_init = [X, X+S, X+2*S, X+3*S] (S = step_expr, X = init_expr)  */
      stmts = NULL;
      new_name = gimple_convert (&stmts, TREE_TYPE (vectype), init_expr);

      vec_alloc (v, nunits);
      bool constant_p = is_gimple_min_invariant (new_name);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, new_name);
      for (i = 1; i < nunits; i++)
	{
	  /* Create: new_name_i = new_name + step_expr  */
	  new_name = gimple_build (&stmts, PLUS_EXPR, TREE_TYPE (new_name),
				   new_name, step_expr);
	  if (!is_gimple_min_invariant (new_name))
	    constant_p = false;
	  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, new_name);
	}
      if (stmts)
	{
	  new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
	  gcc_assert (!new_bb);
	}

      /* Create a vector from [new_name_0, new_name_1, ..., new_name_nunits-1]  */
      if (constant_p)
	new_vec = build_vector_from_ctor (vectype, v);
      else
	new_vec = build_constructor (vectype, v);
      vec_init = vect_init_vector (iv_phi, new_vec, vectype, NULL);
    }


  /* Create the vector that holds the step of the induction.  */
  if (nested_in_vect_loop)
    /* iv_loop is nested in the loop to be vectorized. Generate:
       vec_step = [S, S, S, S]  */
    new_name = step_expr;
  else
    {
      /* iv_loop is the loop to be vectorized. Generate:
	  vec_step = [VF*S, VF*S, VF*S, VF*S]  */
      if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr)))
	{
	  expr = build_int_cst (integer_type_node, vf);
	  expr = fold_convert (TREE_TYPE (step_expr), expr);
	}
      else
	expr = build_int_cst (TREE_TYPE (step_expr), vf);
      new_name = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
			      expr, step_expr);
      if (TREE_CODE (step_expr) == SSA_NAME)
	new_name = vect_init_vector (iv_phi, new_name,
				     TREE_TYPE (step_expr), NULL);
    }

  t = unshare_expr (new_name);
  gcc_assert (CONSTANT_CLASS_P (new_name)
	      || TREE_CODE (new_name) == SSA_NAME);
  stepvectype = get_vectype_for_scalar_type (TREE_TYPE (new_name));
  gcc_assert (stepvectype);
  new_vec = build_vector_from_val (stepvectype, t);
  vec_step = vect_init_vector (iv_phi, new_vec, stepvectype, NULL);


  /* Create the following def-use cycle:
     loop prolog:
         vec_init = ...
	 vec_step = ...
     loop:
         vec_iv = PHI <vec_init, vec_loop>
         ...
         STMT
         ...
         vec_loop = vec_iv + vec_step;  */

  /* Create the induction-phi that defines the induction-operand.  */
  vec_dest = vect_get_new_vect_var (vectype, vect_simple_var, "vec_iv_");
  induction_phi = create_phi_node (vec_dest, iv_loop->header);
  set_vinfo_for_stmt (induction_phi,
		      new_stmt_vec_info (induction_phi, loop_vinfo));
  induc_def = PHI_RESULT (induction_phi);

  /* Create the iv update inside the loop  */
  new_stmt = gimple_build_assign (vec_dest, PLUS_EXPR, induc_def, vec_step);
  vec_def = make_ssa_name (vec_dest, new_stmt);
  gimple_assign_set_lhs (new_stmt, vec_def);
  gsi_insert_before (&si, new_stmt, GSI_SAME_STMT);
  set_vinfo_for_stmt (new_stmt, new_stmt_vec_info (new_stmt, loop_vinfo));

  /* Set the arguments of the phi node:  */
  add_phi_arg (induction_phi, vec_init, pe, UNKNOWN_LOCATION);
  add_phi_arg (induction_phi, vec_def, loop_latch_edge (iv_loop),
	       UNKNOWN_LOCATION);


  /* In case that vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  For more details see documentation
     in vectorizable_operation.  */

  if (ncopies > 1)
    {
      stmt_vec_info prev_stmt_vinfo;
      /* FORNOW. This restriction should be relaxed.  */
      gcc_assert (!nested_in_vect_loop);

      /* Create the vector that holds the step of the induction.  */
      if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr)))
	{
	  expr = build_int_cst (integer_type_node, nunits);
	  expr = fold_convert (TREE_TYPE (step_expr), expr);
	}
      else
	expr = build_int_cst (TREE_TYPE (step_expr), nunits);
      new_name = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
			      expr, step_expr);
      if (TREE_CODE (step_expr) == SSA_NAME)
	new_name = vect_init_vector (iv_phi, new_name,
				     TREE_TYPE (step_expr), NULL);
      t = unshare_expr (new_name);
      gcc_assert (CONSTANT_CLASS_P (new_name)
		  || TREE_CODE (new_name) == SSA_NAME);
      new_vec = build_vector_from_val (stepvectype, t);
      vec_step = vect_init_vector (iv_phi, new_vec, stepvectype, NULL);

      vec_def = induc_def;
      prev_stmt_vinfo = vinfo_for_stmt (induction_phi);
      for (i = 1; i < ncopies; i++)
	{
	  /* vec_i = vec_prev + vec_step  */
	  new_stmt = gimple_build_assign (vec_dest, PLUS_EXPR,
					  vec_def, vec_step);
	  vec_def = make_ssa_name (vec_dest, new_stmt);
	  gimple_assign_set_lhs (new_stmt, vec_def);
 
	  gsi_insert_before (&si, new_stmt, GSI_SAME_STMT);
	  if (!useless_type_conversion_p (resvectype, vectype))
	    {
	      new_stmt
		= gimple_build_assign
			(vect_get_new_vect_var (resvectype, vect_simple_var,
						"vec_iv_"),
			 VIEW_CONVERT_EXPR,
			 build1 (VIEW_CONVERT_EXPR, resvectype,
				 gimple_assign_lhs (new_stmt)));
	      gimple_assign_set_lhs (new_stmt,
				     make_ssa_name
				       (gimple_assign_lhs (new_stmt), new_stmt));
	      gsi_insert_before (&si, new_stmt, GSI_SAME_STMT);
	    }
	  set_vinfo_for_stmt (new_stmt,
			      new_stmt_vec_info (new_stmt, loop_vinfo));
	  STMT_VINFO_RELATED_STMT (prev_stmt_vinfo) = new_stmt;
	  prev_stmt_vinfo = vinfo_for_stmt (new_stmt);
	}
    }

  if (nested_in_vect_loop)
    {
      /* Find the loop-closed exit-phi of the induction, and record
         the final vector of induction results:  */
      exit_phi = NULL;
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, loop_arg)
        {
	  gimple *use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;

	  if (!flow_bb_inside_loop_p (iv_loop, gimple_bb (use_stmt)))
	    {
	      exit_phi = use_stmt;
	      break;
	    }
        }
      if (exit_phi)
	{
	  stmt_vec_info stmt_vinfo = vinfo_for_stmt (exit_phi);
	  /* FORNOW. Currently not supporting the case that an inner-loop induction
	     is not used in the outer-loop (i.e. only outside the outer-loop).  */
	  gcc_assert (STMT_VINFO_RELEVANT_P (stmt_vinfo)
		      && !STMT_VINFO_LIVE_P (stmt_vinfo));

	  STMT_VINFO_VEC_STMT (stmt_vinfo) = new_stmt;
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "vector of inductions after inner-loop:");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, new_stmt, 0);
	    }
	}
    }


  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "transform induction: created def-use cycle: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, induction_phi, 0);
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM,
			SSA_NAME_DEF_STMT (vec_def), 0);
    }

  STMT_VINFO_VEC_STMT (phi_info) = induction_phi;
  if (!useless_type_conversion_p (resvectype, vectype))
    {
      new_stmt = gimple_build_assign (vect_get_new_vect_var (resvectype,
							     vect_simple_var,
							     "vec_iv_"),
				      VIEW_CONVERT_EXPR,
				      build1 (VIEW_CONVERT_EXPR, resvectype,
					      induc_def));
      induc_def = make_ssa_name (gimple_assign_lhs (new_stmt), new_stmt);
      gimple_assign_set_lhs (new_stmt, induc_def);
      si = gsi_after_labels (bb);
      gsi_insert_before (&si, new_stmt, GSI_SAME_STMT);
      set_vinfo_for_stmt (new_stmt,
			  new_stmt_vec_info (new_stmt, loop_vinfo));
      STMT_VINFO_RELATED_STMT (vinfo_for_stmt (new_stmt))
	= STMT_VINFO_RELATED_STMT (vinfo_for_stmt (induction_phi));
    }

  return induc_def;
}


/* Function get_initial_def_for_reduction

   Input:
   STMT - a stmt that performs a reduction operation in the loop.
   INIT_VAL - the initial value of the reduction variable

   Output:
   ADJUSTMENT_DEF - a tree that holds a value to be added to the final result
        of the reduction (used for adjusting the epilog - see below).
   Return a vector variable, initialized according to the operation that STMT
        performs. This vector will be used as the initial value of the
        vector of partial results.

   Option1 (adjust in epilog): Initialize the vector as follows:
     add/bit or/xor:    [0,0,...,0,0]
     mult/bit and:      [1,1,...,1,1]
     min/max/cond_expr: [init_val,init_val,..,init_val,init_val]
   and when necessary (e.g. add/mult case) let the caller know
   that it needs to adjust the result by init_val.

   Option2: Initialize the vector as follows:
     add/bit or/xor:    [init_val,0,0,...,0]
     mult/bit and:      [init_val,1,1,...,1]
     min/max/cond_expr: [init_val,init_val,...,init_val]
   and no adjustments are needed.

   For example, for the following code:

   s = init_val;
   for (i=0;i<n;i++)
     s = s + a[i];

   STMT is 's = s + a[i]', and the reduction variable is 's'.
   For a vector of 4 units, we want to return either [0,0,0,init_val],
   or [0,0,0,0] and let the caller know that it needs to adjust
   the result at the end by 'init_val'.

   FORNOW, we are using the 'adjust in epilog' scheme, because this way the
   initialization vector is simpler (same element in all entries), if
   ADJUSTMENT_DEF is not NULL, and Option2 otherwise.

   A cost model should help decide between these two schemes.  */

tree
get_initial_def_for_reduction (gimple *stmt, tree init_val,
                               tree *adjustment_def)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree scalar_type = TREE_TYPE (init_val);
  tree vectype = get_vectype_for_scalar_type (scalar_type);
  int nunits;
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree def_for_init;
  tree init_def;
  tree *elts;
  int i;
  bool nested_in_vect_loop = false;
  REAL_VALUE_TYPE real_init_val = dconst0;
  int int_init_val = 0;
  gimple *def_stmt = NULL;
  gimple_seq stmts = NULL;

  gcc_assert (vectype);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);

  gcc_assert (POINTER_TYPE_P (scalar_type) || INTEGRAL_TYPE_P (scalar_type)
	      || SCALAR_FLOAT_TYPE_P (scalar_type));

  if (nested_in_vect_loop_p (loop, stmt))
    nested_in_vect_loop = true;
  else
    gcc_assert (loop == (gimple_bb (stmt))->loop_father);

  /* In case of double reduction we only create a vector variable to be put
     in the reduction phi node.  The actual statement creation is done in
     vect_create_epilog_for_reduction.  */
  if (adjustment_def && nested_in_vect_loop
      && TREE_CODE (init_val) == SSA_NAME
      && (def_stmt = SSA_NAME_DEF_STMT (init_val))
      && gimple_code (def_stmt) == GIMPLE_PHI
      && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
      && vinfo_for_stmt (def_stmt)
      && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_stmt))
          == vect_double_reduction_def)
    {
      *adjustment_def = NULL;
      return vect_create_destination_var (init_val, vectype);
    }

  /* In case of a nested reduction do not use an adjustment def as
     that case is not supported by the epilogue generation correctly
     if ncopies is not one.  */
  if (adjustment_def && nested_in_vect_loop)
    {
      *adjustment_def = NULL;
      return vect_get_vec_def_for_operand (init_val, stmt);
    }

  switch (code)
    {
      case WIDEN_SUM_EXPR:
      case DOT_PROD_EXPR:
      case SAD_EXPR:
      case PLUS_EXPR:
      case MINUS_EXPR:
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
      case MULT_EXPR:
      case BIT_AND_EXPR:
        /* ADJUSMENT_DEF is NULL when called from
           vect_create_epilog_for_reduction to vectorize double reduction.  */
        if (adjustment_def)
	  *adjustment_def = init_val;

        if (code == MULT_EXPR)
          {
            real_init_val = dconst1;
            int_init_val = 1;
          }

        if (code == BIT_AND_EXPR)
          int_init_val = -1;

        if (SCALAR_FLOAT_TYPE_P (scalar_type))
          def_for_init = build_real (scalar_type, real_init_val);
        else
          def_for_init = build_int_cst (scalar_type, int_init_val);

        /* Create a vector of '0' or '1' except the first element.  */
	elts = XALLOCAVEC (tree, nunits);
        for (i = nunits - 2; i >= 0; --i)
	  elts[i + 1] = def_for_init;

        /* Option1: the first element is '0' or '1' as well.  */
        if (adjustment_def)
          {
	    elts[0] = def_for_init;
            init_def = build_vector (vectype, elts);
            break;
          }

        /* Option2: the first element is INIT_VAL.  */
	elts[0] = init_val;
        if (TREE_CONSTANT (init_val))
          init_def = build_vector (vectype, elts);
        else
	  {
	    vec<constructor_elt, va_gc> *v;
	    vec_alloc (v, nunits);
	    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, init_val);
	    for (i = 1; i < nunits; ++i)
	      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, elts[i]);
	    init_def = build_constructor (vectype, v);
	  }

        break;

      case MIN_EXPR:
      case MAX_EXPR:
      case COND_EXPR:
	if (adjustment_def)
          {
	    *adjustment_def = NULL_TREE;
	    if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_vinfo) != COND_REDUCTION)
	      {
		init_def = vect_get_vec_def_for_operand (init_val, stmt);
		break;
	      }
	  }
	init_val = gimple_convert (&stmts, TREE_TYPE (vectype), init_val);
	if (! gimple_seq_empty_p (stmts))
	  gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
	init_def = build_vector_from_val (vectype, init_val);
	break;

      default:
        gcc_unreachable ();
    }

  return init_def;
}

/* Function vect_create_epilog_for_reduction

   Create code at the loop-epilog to finalize the result of a reduction
   computation. 
  
   VECT_DEFS is list of vector of partial results, i.e., the lhs's of vector 
     reduction statements. 
   STMT is the scalar reduction stmt that is being vectorized.
   NCOPIES is > 1 in case the vectorization factor (VF) is bigger than the
     number of elements that we can fit in a vectype (nunits).  In this case
     we have to generate more than one vector stmt - i.e - we need to "unroll"
     the vector stmt by a factor VF/nunits.  For more details see documentation
     in vectorizable_operation.
   REDUC_CODE is the tree-code for the epilog reduction.
   REDUCTION_PHIS is a list of the phi-nodes that carry the reduction 
     computation.
   REDUC_INDEX is the index of the operand in the right hand side of the 
     statement that is defined by REDUCTION_PHI.
   DOUBLE_REDUC is TRUE if double reduction phi nodes should be handled.
   SLP_NODE is an SLP node containing a group of reduction statements. The 
     first one in this group is STMT.
   INDUCTION_INDEX is the index of the loop for condition reductions.
     Otherwise it is undefined.

   This function:
   1. Creates the reduction def-use cycles: sets the arguments for 
      REDUCTION_PHIS:
      The loop-entry argument is the vectorized initial-value of the reduction.
      The loop-latch argument is taken from VECT_DEFS - the vector of partial 
      sums.
   2. "Reduces" each vector of partial results VECT_DEFS into a single result,
      by applying the operation specified by REDUC_CODE if available, or by 
      other means (whole-vector shifts or a scalar loop).
      The function also creates a new phi node at the loop exit to preserve
      loop-closed form, as illustrated below.

     The flow at the entry to this function:

        loop:
          vec_def = phi <null, null>            # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT
          s_loop = scalar_stmt                  # (scalar) STMT
        loop_exit:
          s_out0 = phi <s_loop>                 # (scalar) EXIT_PHI
          use <s_out0>
          use <s_out0>

     The above is transformed by this function into:

        loop:
          vec_def = phi <vec_init, VECT_DEF>    # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT
          s_loop = scalar_stmt                  # (scalar) STMT
        loop_exit:
          s_out0 = phi <s_loop>                 # (scalar) EXIT_PHI
          v_out1 = phi <VECT_DEF>               # NEW_EXIT_PHI
          v_out2 = reduce <v_out1>
          s_out3 = extract_field <v_out2, 0>
          s_out4 = adjust_result <s_out3>
          use <s_out4>
          use <s_out4>
*/

static void
vect_create_epilog_for_reduction (vec<tree> vect_defs, gimple *stmt,
				  int ncopies, enum tree_code reduc_code,
				  vec<gimple *> reduction_phis,
                                  int reduc_index, bool double_reduc, 
				  slp_tree slp_node, tree induction_index)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  stmt_vec_info prev_phi_info;
  tree vectype;
  machine_mode mode;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo), *outer_loop = NULL;
  basic_block exit_bb;
  tree scalar_dest;
  tree scalar_type;
  gimple *new_phi = NULL, *phi;
  gimple_stmt_iterator exit_gsi;
  tree vec_dest;
  tree new_temp = NULL_TREE, new_dest, new_name, new_scalar_dest;
  gimple *epilog_stmt = NULL;
  enum tree_code code = gimple_assign_rhs_code (stmt);
  gimple *exit_phi;
  tree bitsize;
  tree adjustment_def = NULL;
  tree vec_initial_def = NULL;
  tree reduction_op, expr, def, initial_def = NULL;
  tree orig_name, scalar_result;
  imm_use_iterator imm_iter, phi_imm_iter;
  use_operand_p use_p, phi_use_p;
  gimple *use_stmt, *orig_stmt, *reduction_phi = NULL;
  bool nested_in_vect_loop = false;
  auto_vec<gimple *> new_phis;
  auto_vec<gimple *> inner_phis;
  enum vect_def_type dt = vect_unknown_def_type;
  int j, i;
  auto_vec<tree> scalar_results;
  unsigned int group_size = 1, k, ratio;
  auto_vec<tree> vec_initial_defs;
  auto_vec<gimple *> phis;
  bool slp_reduc = false;
  tree new_phi_result;
  gimple *inner_phi = NULL;

  if (slp_node)
    group_size = SLP_TREE_SCALAR_STMTS (slp_node).length (); 

  if (nested_in_vect_loop_p (loop, stmt))
    {
      outer_loop = loop;
      loop = loop->inner;
      nested_in_vect_loop = true;
      gcc_assert (!slp_node);
    }

  reduction_op = get_reduction_op (stmt, reduc_index);

  vectype = get_vectype_for_scalar_type (TREE_TYPE (reduction_op));
  gcc_assert (vectype);
  mode = TYPE_MODE (vectype);

  /* 1. Create the reduction def-use cycle:
     Set the arguments of REDUCTION_PHIS, i.e., transform

        loop:
          vec_def = phi <null, null>            # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT
          ...

     into:

        loop:
          vec_def = phi <vec_init, VECT_DEF>    # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT
          ...

     (in case of SLP, do it for all the phis). */

  /* Get the loop-entry arguments.  */
  enum vect_def_type initial_def_dt = vect_unknown_def_type;
  if (slp_node)
    vect_get_vec_defs (reduction_op, NULL_TREE, stmt, &vec_initial_defs,
                       NULL, slp_node, reduc_index);
  else
    {
      /* Get at the scalar def before the loop, that defines the initial value
	 of the reduction variable.  */
      gimple *def_stmt = SSA_NAME_DEF_STMT (reduction_op);
      initial_def = PHI_ARG_DEF_FROM_EDGE (def_stmt,
					   loop_preheader_edge (loop));
      vect_is_simple_use (initial_def, loop_vinfo, &def_stmt, &initial_def_dt);
      vec_initial_def = get_initial_def_for_reduction (stmt, initial_def,
						       &adjustment_def);
      vec_initial_defs.create (1);
      vec_initial_defs.quick_push (vec_initial_def);
    }

  /* Set phi nodes arguments.  */
  FOR_EACH_VEC_ELT (reduction_phis, i, phi)
    {
      tree vec_init_def, def;
      gimple_seq stmts;
      vec_init_def = force_gimple_operand (vec_initial_defs[i], &stmts,
					   true, NULL_TREE);
      gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
      def = vect_defs[i];
      for (j = 0; j < ncopies; j++)
        {
	  if (j != 0)
	    {
	      phi = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (phi));
	      if (nested_in_vect_loop)
		vec_init_def
		  = vect_get_vec_def_for_stmt_copy (initial_def_dt,
						    vec_init_def);
	    }

	  /* Set the loop-entry arg of the reduction-phi.  */

	  if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
	      == INTEGER_INDUC_COND_REDUCTION)
	    {
	      /* Initialise the reduction phi to zero.  This prevents initial
		 values of non-zero interferring with the reduction op.  */
	      gcc_assert (ncopies == 1);
	      gcc_assert (i == 0);

	      tree vec_init_def_type = TREE_TYPE (vec_init_def);
	      tree zero_vec = build_zero_cst (vec_init_def_type);

	      add_phi_arg (as_a <gphi *> (phi), zero_vec,
			   loop_preheader_edge (loop), UNKNOWN_LOCATION);
	    }
	  else
	    add_phi_arg (as_a <gphi *> (phi), vec_init_def,
			 loop_preheader_edge (loop), UNKNOWN_LOCATION);

          /* Set the loop-latch arg for the reduction-phi.  */
          if (j > 0)
            def = vect_get_vec_def_for_stmt_copy (vect_unknown_def_type, def);

          add_phi_arg (as_a <gphi *> (phi), def, loop_latch_edge (loop),
		       UNKNOWN_LOCATION);

          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_NOTE, vect_location,
			       "transform reduction: created def-use cycle: ");
              dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
              dump_gimple_stmt (MSG_NOTE, TDF_SLIM, SSA_NAME_DEF_STMT (def), 0);
            }
        }
    }

  /* 2. Create epilog code.
        The reduction epilog code operates across the elements of the vector
        of partial results computed by the vectorized loop.
        The reduction epilog code consists of:

        step 1: compute the scalar result in a vector (v_out2)
        step 2: extract the scalar result (s_out3) from the vector (v_out2)
        step 3: adjust the scalar result (s_out3) if needed.

        Step 1 can be accomplished using one the following three schemes:
          (scheme 1) using reduc_code, if available.
          (scheme 2) using whole-vector shifts, if available.
          (scheme 3) using a scalar loop. In this case steps 1+2 above are
                     combined.

          The overall epilog code looks like this:

          s_out0 = phi <s_loop>         # original EXIT_PHI
          v_out1 = phi <VECT_DEF>       # NEW_EXIT_PHI
          v_out2 = reduce <v_out1>              # step 1
          s_out3 = extract_field <v_out2, 0>    # step 2
          s_out4 = adjust_result <s_out3>       # step 3

          (step 3 is optional, and steps 1 and 2 may be combined).
          Lastly, the uses of s_out0 are replaced by s_out4.  */


  /* 2.1 Create new loop-exit-phis to preserve loop-closed form:
         v_out1 = phi <VECT_DEF> 
         Store them in NEW_PHIS.  */

  exit_bb = single_exit (loop)->dest;
  prev_phi_info = NULL;
  new_phis.create (vect_defs.length ());
  FOR_EACH_VEC_ELT (vect_defs, i, def)
    {
      for (j = 0; j < ncopies; j++)
        {
	  tree new_def = copy_ssa_name (def);
          phi = create_phi_node (new_def, exit_bb);
          set_vinfo_for_stmt (phi, new_stmt_vec_info (phi, loop_vinfo));
          if (j == 0)
            new_phis.quick_push (phi);
          else
	    {
	      def = vect_get_vec_def_for_stmt_copy (dt, def);
	      STMT_VINFO_RELATED_STMT (prev_phi_info) = phi;
	    }

          SET_PHI_ARG_DEF (phi, single_exit (loop)->dest_idx, def);
          prev_phi_info = vinfo_for_stmt (phi);
        }
    }

  /* The epilogue is created for the outer-loop, i.e., for the loop being
     vectorized.  Create exit phis for the outer loop.  */
  if (double_reduc)
    {
      loop = outer_loop;
      exit_bb = single_exit (loop)->dest;
      inner_phis.create (vect_defs.length ());
      FOR_EACH_VEC_ELT (new_phis, i, phi)
	{
	  tree new_result = copy_ssa_name (PHI_RESULT (phi));
	  gphi *outer_phi = create_phi_node (new_result, exit_bb);
	  SET_PHI_ARG_DEF (outer_phi, single_exit (loop)->dest_idx,
			   PHI_RESULT (phi));
	  set_vinfo_for_stmt (outer_phi, new_stmt_vec_info (outer_phi,
							    loop_vinfo));
	  inner_phis.quick_push (phi);
	  new_phis[i] = outer_phi;
	  prev_phi_info = vinfo_for_stmt (outer_phi);
          while (STMT_VINFO_RELATED_STMT (vinfo_for_stmt (phi)))
            {
	      phi = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (phi));
	      new_result = copy_ssa_name (PHI_RESULT (phi));
	      outer_phi = create_phi_node (new_result, exit_bb);
	      SET_PHI_ARG_DEF (outer_phi, single_exit (loop)->dest_idx,
			       PHI_RESULT (phi));
	      set_vinfo_for_stmt (outer_phi, new_stmt_vec_info (outer_phi,
								loop_vinfo));
	      STMT_VINFO_RELATED_STMT (prev_phi_info) = outer_phi;
	      prev_phi_info = vinfo_for_stmt (outer_phi);
	    }
	}
    }

  exit_gsi = gsi_after_labels (exit_bb);

  /* 2.2 Get the relevant tree-code to use in the epilog for schemes 2,3
         (i.e. when reduc_code is not available) and in the final adjustment
	 code (if needed).  Also get the original scalar reduction variable as
         defined in the loop.  In case STMT is a "pattern-stmt" (i.e. - it
         represents a reduction pattern), the tree-code and scalar-def are
         taken from the original stmt that the pattern-stmt (STMT) replaces.
         Otherwise (it is a regular reduction) - the tree-code and scalar-def
         are taken from STMT.  */

  orig_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
  if (!orig_stmt)
    {
      /* Regular reduction  */
      orig_stmt = stmt;
    }
  else
    {
      /* Reduction pattern  */
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (orig_stmt);
      gcc_assert (STMT_VINFO_IN_PATTERN_P (stmt_vinfo));
      gcc_assert (STMT_VINFO_RELATED_STMT (stmt_vinfo) == stmt);
    }

  code = gimple_assign_rhs_code (orig_stmt);
  /* For MINUS_EXPR the initial vector is [init_val,0,...,0], therefore,
     partial results are added and not subtracted.  */
  if (code == MINUS_EXPR) 
    code = PLUS_EXPR;
  
  scalar_dest = gimple_assign_lhs (orig_stmt);
  scalar_type = TREE_TYPE (scalar_dest);
  scalar_results.create (group_size); 
  new_scalar_dest = vect_create_destination_var (scalar_dest, NULL);
  bitsize = TYPE_SIZE (scalar_type);

  /* In case this is a reduction in an inner-loop while vectorizing an outer
     loop - we don't need to extract a single scalar result at the end of the
     inner-loop (unless it is double reduction, i.e., the use of reduction is
     outside the outer-loop).  The final vector of partial results will be used
     in the vectorized outer-loop, or reduced to a scalar result at the end of
     the outer-loop.  */
  if (nested_in_vect_loop && !double_reduc)
    goto vect_finalize_reduction;

  /* SLP reduction without reduction chain, e.g.,
     # a1 = phi <a2, a0>
     # b1 = phi <b2, b0>
     a2 = operation (a1)
     b2 = operation (b1)  */
  slp_reduc = (slp_node && !GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)));

  /* In case of reduction chain, e.g.,
     # a1 = phi <a3, a0>
     a2 = operation (a1)
     a3 = operation (a2),

     we may end up with more than one vector result.  Here we reduce them to
     one vector.  */
  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      tree first_vect = PHI_RESULT (new_phis[0]);
      tree tmp;
      gassign *new_vec_stmt = NULL;

      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      for (k = 1; k < new_phis.length (); k++)
        {
	  gimple *next_phi = new_phis[k];
          tree second_vect = PHI_RESULT (next_phi);

          tmp = build2 (code, vectype,  first_vect, second_vect);
          new_vec_stmt = gimple_build_assign (vec_dest, tmp);
          first_vect = make_ssa_name (vec_dest, new_vec_stmt);
          gimple_assign_set_lhs (new_vec_stmt, first_vect);
          gsi_insert_before (&exit_gsi, new_vec_stmt, GSI_SAME_STMT);
        }

      new_phi_result = first_vect;
      if (new_vec_stmt)
        {
          new_phis.truncate (0);
          new_phis.safe_push (new_vec_stmt);
        }
    }
  else
    new_phi_result = PHI_RESULT (new_phis[0]);

  if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) == COND_REDUCTION)
    {
      /* For condition reductions, we have a vector (NEW_PHI_RESULT) containing
	 various data values where the condition matched and another vector
	 (INDUCTION_INDEX) containing all the indexes of those matches.  We
	 need to extract the last matching index (which will be the index with
	 highest value) and use this to index into the data vector.
	 For the case where there were no matches, the data vector will contain
	 all default values and the index vector will be all zeros.  */

      /* Get various versions of the type of the vector of indexes.  */
      tree index_vec_type = TREE_TYPE (induction_index);
      gcc_checking_assert (TYPE_UNSIGNED (index_vec_type));
      tree index_scalar_type = TREE_TYPE (index_vec_type);
      tree index_vec_cmp_type = build_same_sized_truth_vector_type
	(index_vec_type);

      /* Get an unsigned integer version of the type of the data vector.  */
      int scalar_precision = GET_MODE_PRECISION (TYPE_MODE (scalar_type));
      tree scalar_type_unsigned = make_unsigned_type (scalar_precision);
      tree vectype_unsigned = build_vector_type
	(scalar_type_unsigned, TYPE_VECTOR_SUBPARTS (vectype));

      /* First we need to create a vector (ZERO_VEC) of zeros and another
	 vector (MAX_INDEX_VEC) filled with the last matching index, which we
	 can create using a MAX reduction and then expanding.
	 In the case where the loop never made any matches, the max index will
	 be zero.  */

      /* Vector of {0, 0, 0,...}.  */
      tree zero_vec = make_ssa_name (vectype);
      tree zero_vec_rhs = build_zero_cst (vectype);
      gimple *zero_vec_stmt = gimple_build_assign (zero_vec, zero_vec_rhs);
      gsi_insert_before (&exit_gsi, zero_vec_stmt, GSI_SAME_STMT);

      /* Find maximum value from the vector of found indexes.  */
      tree max_index = make_ssa_name (index_scalar_type);
      gimple *max_index_stmt = gimple_build_assign (max_index, REDUC_MAX_EXPR,
						    induction_index);
      gsi_insert_before (&exit_gsi, max_index_stmt, GSI_SAME_STMT);

      /* Vector of {max_index, max_index, max_index,...}.  */
      tree max_index_vec = make_ssa_name (index_vec_type);
      tree max_index_vec_rhs = build_vector_from_val (index_vec_type,
						      max_index);
      gimple *max_index_vec_stmt = gimple_build_assign (max_index_vec,
							max_index_vec_rhs);
      gsi_insert_before (&exit_gsi, max_index_vec_stmt, GSI_SAME_STMT);

      /* Next we compare the new vector (MAX_INDEX_VEC) full of max indexes
	 with the vector (INDUCTION_INDEX) of found indexes, choosing values
	 from the data vector (NEW_PHI_RESULT) for matches, 0 (ZERO_VEC)
	 otherwise.  Only one value should match, resulting in a vector
	 (VEC_COND) with one data value and the rest zeros.
	 In the case where the loop never made any matches, every index will
	 match, resulting in a vector with all data values (which will all be
	 the default value).  */

      /* Compare the max index vector to the vector of found indexes to find
	 the position of the max value.  */
      tree vec_compare = make_ssa_name (index_vec_cmp_type);
      gimple *vec_compare_stmt = gimple_build_assign (vec_compare, EQ_EXPR,
						      induction_index,
						      max_index_vec);
      gsi_insert_before (&exit_gsi, vec_compare_stmt, GSI_SAME_STMT);

      /* Use the compare to choose either values from the data vector or
	 zero.  */
      tree vec_cond = make_ssa_name (vectype);
      gimple *vec_cond_stmt = gimple_build_assign (vec_cond, VEC_COND_EXPR,
						   vec_compare, new_phi_result,
						   zero_vec);
      gsi_insert_before (&exit_gsi, vec_cond_stmt, GSI_SAME_STMT);

      /* Finally we need to extract the data value from the vector (VEC_COND)
	 into a scalar (MATCHED_DATA_REDUC).  Logically we want to do a OR
	 reduction, but because this doesn't exist, we can use a MAX reduction
	 instead.  The data value might be signed or a float so we need to cast
	 it first.
	 In the case where the loop never made any matches, the data values are
	 all identical, and so will reduce down correctly.  */

      /* Make the matched data values unsigned.  */
      tree vec_cond_cast = make_ssa_name (vectype_unsigned);
      tree vec_cond_cast_rhs = build1 (VIEW_CONVERT_EXPR, vectype_unsigned,
				       vec_cond);
      gimple *vec_cond_cast_stmt = gimple_build_assign (vec_cond_cast,
							VIEW_CONVERT_EXPR,
							vec_cond_cast_rhs);
      gsi_insert_before (&exit_gsi, vec_cond_cast_stmt, GSI_SAME_STMT);

      /* Reduce down to a scalar value.  */
      tree data_reduc = make_ssa_name (scalar_type_unsigned);
      optab ot = optab_for_tree_code (REDUC_MAX_EXPR, vectype_unsigned,
				      optab_default);
      gcc_assert (optab_handler (ot, TYPE_MODE (vectype_unsigned))
		  != CODE_FOR_nothing);
      gimple *data_reduc_stmt = gimple_build_assign (data_reduc,
						     REDUC_MAX_EXPR,
						     vec_cond_cast);
      gsi_insert_before (&exit_gsi, data_reduc_stmt, GSI_SAME_STMT);

      /* Convert the reduced value back to the result type and set as the
	 result.  */
      tree data_reduc_cast = build1 (VIEW_CONVERT_EXPR, scalar_type,
				     data_reduc);
      epilog_stmt = gimple_build_assign (new_scalar_dest, data_reduc_cast);
      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
      gimple_assign_set_lhs (epilog_stmt, new_temp);
      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
      scalar_results.safe_push (new_temp);
    }

  /* 2.3 Create the reduction code, using one of the three schemes described
         above. In SLP we simply need to extract all the elements from the 
         vector (without reducing them), so we use scalar shifts.  */
  else if (reduc_code != ERROR_MARK && !slp_reduc)
    {
      tree tmp;
      tree vec_elem_type;

      /*** Case 1:  Create:
           v_out2 = reduc_expr <v_out1>  */

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "Reduce using direct vector reduction.\n");

      vec_elem_type = TREE_TYPE (TREE_TYPE (new_phi_result));
      if (!useless_type_conversion_p (scalar_type, vec_elem_type))
	{
          tree tmp_dest =
	      vect_create_destination_var (scalar_dest, vec_elem_type);
	  tmp = build1 (reduc_code, vec_elem_type, new_phi_result);
	  epilog_stmt = gimple_build_assign (tmp_dest, tmp);
	  new_temp = make_ssa_name (tmp_dest, epilog_stmt);
	  gimple_assign_set_lhs (epilog_stmt, new_temp);
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

	  tmp = build1 (NOP_EXPR, scalar_type, new_temp);
	}
      else
	tmp = build1 (reduc_code, scalar_type, new_phi_result);

      epilog_stmt = gimple_build_assign (new_scalar_dest, tmp);
      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
      gimple_assign_set_lhs (epilog_stmt, new_temp);
      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

      if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
	  == INTEGER_INDUC_COND_REDUCTION)
	{
	  /* Earlier we set the initial value to be zero.  Check the result
	     and if it is zero then replace with the original initial
	     value.  */
	  tree zero = build_zero_cst (scalar_type);
	  tree zcompare = build2 (EQ_EXPR, boolean_type_node, new_temp, zero);

	  tmp = make_ssa_name (new_scalar_dest);
	  epilog_stmt = gimple_build_assign (tmp, COND_EXPR, zcompare,
					     initial_def, new_temp);
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	  new_temp = tmp;
	}

      scalar_results.safe_push (new_temp);
    }
  else
    {
      bool reduce_with_shift = have_whole_vector_shift (mode);
      int element_bitsize = tree_to_uhwi (bitsize);
      int vec_size_in_bits = tree_to_uhwi (TYPE_SIZE (vectype));
      tree vec_temp;

      /* Regardless of whether we have a whole vector shift, if we're
         emulating the operation via tree-vect-generic, we don't want
         to use it.  Only the first round of the reduction is likely
         to still be profitable via emulation.  */
      /* ??? It might be better to emit a reduction tree code here, so that
         tree-vect-generic can expand the first round via bit tricks.  */
      if (!VECTOR_MODE_P (mode))
        reduce_with_shift = false;
      else
        {
          optab optab = optab_for_tree_code (code, vectype, optab_default);
          if (optab_handler (optab, mode) == CODE_FOR_nothing)
            reduce_with_shift = false;
        }

      if (reduce_with_shift && !slp_reduc)
        {
          int nelements = vec_size_in_bits / element_bitsize;
          unsigned char *sel = XALLOCAVEC (unsigned char, nelements);

          int elt_offset;

          tree zero_vec = build_zero_cst (vectype);
          /*** Case 2: Create:
             for (offset = nelements/2; offset >= 1; offset/=2)
                {
                  Create:  va' = vec_shift <va, offset>
                  Create:  va = vop <va, va'>
                }  */

          tree rhs;

          if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location,
			     "Reduce using vector shifts\n");

          vec_dest = vect_create_destination_var (scalar_dest, vectype);
          new_temp = new_phi_result;
          for (elt_offset = nelements / 2;
               elt_offset >= 1;
               elt_offset /= 2)
            {
              calc_vec_perm_mask_for_shift (mode, elt_offset, sel);
              tree mask = vect_gen_perm_mask_any (vectype, sel);
	      epilog_stmt = gimple_build_assign (vec_dest, VEC_PERM_EXPR,
						 new_temp, zero_vec, mask);
              new_name = make_ssa_name (vec_dest, epilog_stmt);
              gimple_assign_set_lhs (epilog_stmt, new_name);
              gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

	      epilog_stmt = gimple_build_assign (vec_dest, code, new_name,
						 new_temp);
              new_temp = make_ssa_name (vec_dest, epilog_stmt);
              gimple_assign_set_lhs (epilog_stmt, new_temp);
              gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
            }

	  /* 2.4  Extract the final scalar result.  Create:
	     s_out3 = extract_field <v_out2, bitpos>  */

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "extract scalar result\n");

	  rhs = build3 (BIT_FIELD_REF, scalar_type, new_temp,
			bitsize, bitsize_zero_node);
	  epilog_stmt = gimple_build_assign (new_scalar_dest, rhs);
	  new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
	  gimple_assign_set_lhs (epilog_stmt, new_temp);
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	  scalar_results.safe_push (new_temp);
        }
      else
        {
          /*** Case 3: Create:
             s = extract_field <v_out2, 0>
             for (offset = element_size;
                  offset < vector_size;
                  offset += element_size;)
               {
                 Create:  s' = extract_field <v_out2, offset>
                 Create:  s = op <s, s'>  // For non SLP cases
               }  */

          if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location,
			     "Reduce using scalar code.\n");

          vec_size_in_bits = tree_to_uhwi (TYPE_SIZE (vectype));
          FOR_EACH_VEC_ELT (new_phis, i, new_phi)
            {
              int bit_offset;
              if (gimple_code (new_phi) == GIMPLE_PHI)
                vec_temp = PHI_RESULT (new_phi);
              else
                vec_temp = gimple_assign_lhs (new_phi);
              tree rhs = build3 (BIT_FIELD_REF, scalar_type, vec_temp, bitsize,
                            bitsize_zero_node);
              epilog_stmt = gimple_build_assign (new_scalar_dest, rhs);
              new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
              gimple_assign_set_lhs (epilog_stmt, new_temp);
              gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

              /* In SLP we don't need to apply reduction operation, so we just
                 collect s' values in SCALAR_RESULTS.  */
              if (slp_reduc)
                scalar_results.safe_push (new_temp);

              for (bit_offset = element_bitsize;
                   bit_offset < vec_size_in_bits;
                   bit_offset += element_bitsize)
                {
                  tree bitpos = bitsize_int (bit_offset);
                  tree rhs = build3 (BIT_FIELD_REF, scalar_type, vec_temp,
                                     bitsize, bitpos);

                  epilog_stmt = gimple_build_assign (new_scalar_dest, rhs);
                  new_name = make_ssa_name (new_scalar_dest, epilog_stmt);
                  gimple_assign_set_lhs (epilog_stmt, new_name);
                  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

                  if (slp_reduc)
                    {
                      /* In SLP we don't need to apply reduction operation, so 
                         we just collect s' values in SCALAR_RESULTS.  */
                      new_temp = new_name;
                      scalar_results.safe_push (new_name);
                    }
                  else
                    {
		      epilog_stmt = gimple_build_assign (new_scalar_dest, code,
							 new_name, new_temp);
                      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
                      gimple_assign_set_lhs (epilog_stmt, new_temp);
                      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
                    }
                }
            }

          /* The only case where we need to reduce scalar results in SLP, is
             unrolling.  If the size of SCALAR_RESULTS is greater than
             GROUP_SIZE, we reduce them combining elements modulo 
             GROUP_SIZE.  */
          if (slp_reduc)
            {
              tree res, first_res, new_res;
	      gimple *new_stmt;
            
              /* Reduce multiple scalar results in case of SLP unrolling.  */
              for (j = group_size; scalar_results.iterate (j, &res);
                   j++)
                {
                  first_res = scalar_results[j % group_size];
		  new_stmt = gimple_build_assign (new_scalar_dest, code,
						  first_res, res);
                  new_res = make_ssa_name (new_scalar_dest, new_stmt);
                  gimple_assign_set_lhs (new_stmt, new_res);
                  gsi_insert_before (&exit_gsi, new_stmt, GSI_SAME_STMT);
                  scalar_results[j % group_size] = new_res;
                }
            }
          else
            /* Not SLP - we have one scalar to keep in SCALAR_RESULTS.  */
            scalar_results.safe_push (new_temp);
        }
    }
  
vect_finalize_reduction:

  if (double_reduc)
    loop = loop->inner;

  /* 2.5 Adjust the final result by the initial value of the reduction
	 variable. (When such adjustment is not needed, then
	 'adjustment_def' is zero).  For example, if code is PLUS we create:
	 new_temp = loop_exit_def + adjustment_def  */

  if (adjustment_def)
    {
      gcc_assert (!slp_reduc);
      if (nested_in_vect_loop)
	{
          new_phi = new_phis[0];
	  gcc_assert (TREE_CODE (TREE_TYPE (adjustment_def)) == VECTOR_TYPE);
	  expr = build2 (code, vectype, PHI_RESULT (new_phi), adjustment_def);
	  new_dest = vect_create_destination_var (scalar_dest, vectype);
	}
      else
	{
          new_temp = scalar_results[0];
	  gcc_assert (TREE_CODE (TREE_TYPE (adjustment_def)) != VECTOR_TYPE);
	  expr = build2 (code, scalar_type, new_temp, adjustment_def);
	  new_dest = vect_create_destination_var (scalar_dest, scalar_type);
	}

      epilog_stmt = gimple_build_assign (new_dest, expr);
      new_temp = make_ssa_name (new_dest, epilog_stmt);
      gimple_assign_set_lhs (epilog_stmt, new_temp);
      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
      if (nested_in_vect_loop)
        {
          set_vinfo_for_stmt (epilog_stmt,
                              new_stmt_vec_info (epilog_stmt, loop_vinfo));
          STMT_VINFO_RELATED_STMT (vinfo_for_stmt (epilog_stmt)) =
                STMT_VINFO_RELATED_STMT (vinfo_for_stmt (new_phi));

          if (!double_reduc)
            scalar_results.quick_push (new_temp);
          else
            scalar_results[0] = new_temp;
        }
      else
        scalar_results[0] = new_temp;

      new_phis[0] = epilog_stmt;
    }

  /* 2.6  Handle the loop-exit phis.  Replace the uses of scalar loop-exit
          phis with new adjusted scalar results, i.e., replace use <s_out0>
          with use <s_out4>.        

     Transform:
        loop_exit:
          s_out0 = phi <s_loop>                 # (scalar) EXIT_PHI
          v_out1 = phi <VECT_DEF>               # NEW_EXIT_PHI
          v_out2 = reduce <v_out1>
          s_out3 = extract_field <v_out2, 0>
          s_out4 = adjust_result <s_out3>
          use <s_out0>
          use <s_out0>

     into:

        loop_exit:
          s_out0 = phi <s_loop>                 # (scalar) EXIT_PHI
          v_out1 = phi <VECT_DEF>               # NEW_EXIT_PHI
          v_out2 = reduce <v_out1>
          s_out3 = extract_field <v_out2, 0>
          s_out4 = adjust_result <s_out3>
          use <s_out4>  
          use <s_out4> */


  /* In SLP reduction chain we reduce vector results into one vector if
     necessary, hence we set here GROUP_SIZE to 1.  SCALAR_DEST is the LHS of
     the last stmt in the reduction chain, since we are looking for the loop
     exit phi node.  */
  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      gimple *dest_stmt = SLP_TREE_SCALAR_STMTS (slp_node)[group_size - 1];
      /* Handle reduction patterns.  */
      if (STMT_VINFO_RELATED_STMT (vinfo_for_stmt (dest_stmt)))
	dest_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (dest_stmt));

      scalar_dest = gimple_assign_lhs (dest_stmt);
      group_size = 1;
    }

  /* In SLP we may have several statements in NEW_PHIS and REDUCTION_PHIS (in 
     case that GROUP_SIZE is greater than vectorization factor).  Therefore, we
     need to match SCALAR_RESULTS with corresponding statements.  The first
     (GROUP_SIZE / number of new vector stmts) scalar results correspond to
     the first vector stmt, etc.  
     (RATIO is equal to (GROUP_SIZE / number of new vector stmts)).  */ 
  if (group_size > new_phis.length ())
    {
      ratio = group_size / new_phis.length ();
      gcc_assert (!(group_size % new_phis.length ()));
    }
  else
    ratio = 1;

  for (k = 0; k < group_size; k++)
    {
      if (k % ratio == 0)
        {
          epilog_stmt = new_phis[k / ratio];
          reduction_phi = reduction_phis[k / ratio];
	  if (double_reduc)
	    inner_phi = inner_phis[k / ratio];
        }

      if (slp_reduc)
        {
	  gimple *current_stmt = SLP_TREE_SCALAR_STMTS (slp_node)[k];

          orig_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (current_stmt));
          /* SLP statements can't participate in patterns.  */
          gcc_assert (!orig_stmt);
          scalar_dest = gimple_assign_lhs (current_stmt);
        }

      phis.create (3);
      /* Find the loop-closed-use at the loop exit of the original scalar
         result.  (The reduction result is expected to have two immediate uses -
         one at the latch block, and one at the loop exit).  */
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, scalar_dest)
        if (!flow_bb_inside_loop_p (loop, gimple_bb (USE_STMT (use_p)))
	    && !is_gimple_debug (USE_STMT (use_p)))
          phis.safe_push (USE_STMT (use_p));

      /* While we expect to have found an exit_phi because of loop-closed-ssa
         form we can end up without one if the scalar cycle is dead.  */

      FOR_EACH_VEC_ELT (phis, i, exit_phi)
        {
          if (outer_loop)
            {
              stmt_vec_info exit_phi_vinfo = vinfo_for_stmt (exit_phi);
              gphi *vect_phi;

              /* FORNOW. Currently not supporting the case that an inner-loop
                 reduction is not used in the outer-loop (but only outside the
                 outer-loop), unless it is double reduction.  */
              gcc_assert ((STMT_VINFO_RELEVANT_P (exit_phi_vinfo)
                           && !STMT_VINFO_LIVE_P (exit_phi_vinfo))
                          || double_reduc);

	      if (double_reduc)
		STMT_VINFO_VEC_STMT (exit_phi_vinfo) = inner_phi;
	      else
		STMT_VINFO_VEC_STMT (exit_phi_vinfo) = epilog_stmt;
              if (!double_reduc
                  || STMT_VINFO_DEF_TYPE (exit_phi_vinfo)
                      != vect_double_reduction_def)
                continue;

              /* Handle double reduction:

                 stmt1: s1 = phi <s0, s2>  - double reduction phi (outer loop)
                 stmt2:   s3 = phi <s1, s4> - (regular) reduc phi (inner loop)
                 stmt3:   s4 = use (s3)     - (regular) reduc stmt (inner loop)
                 stmt4: s2 = phi <s4>      - double reduction stmt (outer loop)

                 At that point the regular reduction (stmt2 and stmt3) is
                 already vectorized, as well as the exit phi node, stmt4.
                 Here we vectorize the phi node of double reduction, stmt1, and
                 update all relevant statements.  */

              /* Go through all the uses of s2 to find double reduction phi
                 node, i.e., stmt1 above.  */
              orig_name = PHI_RESULT (exit_phi);
              FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, orig_name)
                {
                  stmt_vec_info use_stmt_vinfo;
                  stmt_vec_info new_phi_vinfo;
                  tree vect_phi_init, preheader_arg, vect_phi_res, init_def;
                  basic_block bb = gimple_bb (use_stmt);
		  gimple *use;

                  /* Check that USE_STMT is really double reduction phi
                     node.  */
                  if (gimple_code (use_stmt) != GIMPLE_PHI
                      || gimple_phi_num_args (use_stmt) != 2
                      || bb->loop_father != outer_loop)
                    continue;
                  use_stmt_vinfo = vinfo_for_stmt (use_stmt);
                  if (!use_stmt_vinfo
                      || STMT_VINFO_DEF_TYPE (use_stmt_vinfo)
                          != vect_double_reduction_def)
		    continue;

                  /* Create vector phi node for double reduction:
                     vs1 = phi <vs0, vs2>
                     vs1 was created previously in this function by a call to
                       vect_get_vec_def_for_operand and is stored in
                       vec_initial_def;
                     vs2 is defined by INNER_PHI, the vectorized EXIT_PHI;
                     vs0 is created here.  */

                  /* Create vector phi node.  */
                  vect_phi = create_phi_node (vec_initial_def, bb);
                  new_phi_vinfo = new_stmt_vec_info (vect_phi,
                                    loop_vec_info_for_loop (outer_loop));
                  set_vinfo_for_stmt (vect_phi, new_phi_vinfo);

                  /* Create vs0 - initial def of the double reduction phi.  */
                  preheader_arg = PHI_ARG_DEF_FROM_EDGE (use_stmt,
                                             loop_preheader_edge (outer_loop));
                  init_def = get_initial_def_for_reduction (stmt,
                                                          preheader_arg, NULL);
                  vect_phi_init = vect_init_vector (use_stmt, init_def,
                                                    vectype, NULL);

                  /* Update phi node arguments with vs0 and vs2.  */
                  add_phi_arg (vect_phi, vect_phi_init,
                               loop_preheader_edge (outer_loop),
                               UNKNOWN_LOCATION);
                  add_phi_arg (vect_phi, PHI_RESULT (inner_phi),
                               loop_latch_edge (outer_loop), UNKNOWN_LOCATION);
                  if (dump_enabled_p ())
                    {
                      dump_printf_loc (MSG_NOTE, vect_location,
				       "created double reduction phi node: ");
                      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, vect_phi, 0);
                    }

                  vect_phi_res = PHI_RESULT (vect_phi);

                  /* Replace the use, i.e., set the correct vs1 in the regular
                     reduction phi node.  FORNOW, NCOPIES is always 1, so the
                     loop is redundant.  */
                  use = reduction_phi;
                  for (j = 0; j < ncopies; j++)
                    {
                      edge pr_edge = loop_preheader_edge (loop);
                      SET_PHI_ARG_DEF (use, pr_edge->dest_idx, vect_phi_res);
                      use = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (use));
                    }
                }
            }
        }

      phis.release ();
      if (nested_in_vect_loop)
        {
          if (double_reduc)
            loop = outer_loop;
          else
            continue;
        }

      phis.create (3);
      /* Find the loop-closed-use at the loop exit of the original scalar
         result.  (The reduction result is expected to have two immediate uses,
         one at the latch block, and one at the loop exit).  For double
         reductions we are looking for exit phis of the outer loop.  */
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, scalar_dest)
        {
          if (!flow_bb_inside_loop_p (loop, gimple_bb (USE_STMT (use_p))))
	    {
	      if (!is_gimple_debug (USE_STMT (use_p)))
		phis.safe_push (USE_STMT (use_p));
	    }
          else
            {
              if (double_reduc && gimple_code (USE_STMT (use_p)) == GIMPLE_PHI)
                {
                  tree phi_res = PHI_RESULT (USE_STMT (use_p));

                  FOR_EACH_IMM_USE_FAST (phi_use_p, phi_imm_iter, phi_res)
                    {
                      if (!flow_bb_inside_loop_p (loop,
                                             gimple_bb (USE_STMT (phi_use_p)))
			  && !is_gimple_debug (USE_STMT (phi_use_p)))
                        phis.safe_push (USE_STMT (phi_use_p));
                    }
                }
            }
        }

      FOR_EACH_VEC_ELT (phis, i, exit_phi)
        {
          /* Replace the uses:  */
          orig_name = PHI_RESULT (exit_phi);
          scalar_result = scalar_results[k];
          FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, orig_name)
            FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
              SET_USE (use_p, scalar_result);
        }

      phis.release ();
    }
}


/* Function is_nonwrapping_integer_induction.

   Check if STMT (which is part of loop LOOP) both increments and
   does not cause overflow.  */

static bool
is_nonwrapping_integer_induction (gimple *stmt, struct loop *loop)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  tree base = STMT_VINFO_LOOP_PHI_EVOLUTION_BASE_UNCHANGED (stmt_vinfo);
  tree step = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (stmt_vinfo);
  tree lhs_type = TREE_TYPE (gimple_phi_result (stmt));
  widest_int ni, max_loop_value, lhs_max;
  bool overflow = false;

  /* Make sure the loop is integer based.  */
  if (TREE_CODE (base) != INTEGER_CST
      || TREE_CODE (step) != INTEGER_CST)
    return false;

  /* Check that the induction increments.  */
  if (tree_int_cst_sgn (step) == -1)
    return false;

  /* Check that the max size of the loop will not wrap.  */

  if (TYPE_OVERFLOW_UNDEFINED (lhs_type))
    return true;

  if (! max_stmt_executions (loop, &ni))
    return false;

  max_loop_value = wi::mul (wi::to_widest (step), ni, TYPE_SIGN (lhs_type),
			    &overflow);
  if (overflow)
    return false;

  max_loop_value = wi::add (wi::to_widest (base), max_loop_value,
			    TYPE_SIGN (lhs_type), &overflow);
  if (overflow)
    return false;

  return (wi::min_precision (max_loop_value, TYPE_SIGN (lhs_type))
	  <= TYPE_PRECISION (lhs_type));
}

/* Function vectorizable_reduction.

   Check if STMT performs a reduction operation that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.

   This function also handles reduction idioms (patterns) that have been
   recognized in advance during vect_pattern_recog.  In this case, STMT may be
   of this form:
     X = pattern_expr (arg0, arg1, ..., X)
   and it's STMT_VINFO_RELATED_STMT points to the last stmt in the original
   sequence that had been detected and replaced by the pattern-stmt (STMT).

   This function also handles reduction of condition expressions, for example:
     for (int i = 0; i < N; i++)
       if (a[i] < value)
	 last = a[i];
   This is handled by vectorising the loop and creating an additional vector
   containing the loop indexes for which "a[i] < value" was true.  In the
   function epilogue this is reduced to a single max value and then used to
   index into the vector of results.

   In some cases of reduction patterns, the type of the reduction variable X is
   different than the type of the other arguments of STMT.
   In such cases, the vectype that is used when transforming STMT into a vector
   stmt is different than the vectype that is used to determine the
   vectorization factor, because it consists of a different number of elements
   than the actual number of elements that are being operated upon in parallel.

   For example, consider an accumulation of shorts into an int accumulator.
   On some targets it's possible to vectorize this pattern operating on 8
   shorts at a time (hence, the vectype for purposes of determining the
   vectorization factor should be V8HI); on the other hand, the vectype that
   is used to create the vector form is actually V4SI (the type of the result).

   Upon entry to this function, STMT_VINFO_VECTYPE records the vectype that
   indicates what is the actual level of parallelism (V8HI in the example), so
   that the right vectorization factor would be derived.  This vectype
   corresponds to the type of arguments to the reduction stmt, and should *NOT*
   be used to create the vectorized stmt.  The right vectype for the vectorized
   stmt is obtained from the type of the result X:
        get_vectype_for_scalar_type (TREE_TYPE (X))

   This means that, contrary to "regular" reductions (or "regular" stmts in
   general), the following equation:
      STMT_VINFO_VECTYPE == get_vectype_for_scalar_type (TREE_TYPE (X))
   does *NOT* necessarily hold for reduction patterns.  */

bool
vectorizable_reduction (gimple *stmt, gimple_stmt_iterator *gsi,
			gimple **vec_stmt, slp_tree slp_node)
{
  tree vec_dest;
  tree scalar_dest;
  tree loop_vec_def0 = NULL_TREE, loop_vec_def1 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype_out = STMT_VINFO_VECTYPE (stmt_info);
  tree vectype_in = NULL_TREE;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  enum tree_code code, orig_code, epilog_reduc_code;
  machine_mode vec_mode;
  int op_type;
  optab optab, reduc_optab;
  tree new_temp = NULL_TREE;
  gimple *def_stmt;
  enum vect_def_type dt;
  gphi *new_phi = NULL;
  tree scalar_type;
  bool is_simple_use;
  gimple *orig_stmt;
  stmt_vec_info orig_stmt_info;
  tree expr = NULL_TREE;
  int i;
  int ncopies;
  int epilog_copies;
  stmt_vec_info prev_stmt_info, prev_phi_info;
  bool single_defuse_cycle = false;
  tree reduc_def = NULL_TREE;
  gimple *new_stmt = NULL;
  int j;
  tree ops[3];
  bool nested_cycle = false, found_nested_cycle_def = false;
  gimple *reduc_def_stmt = NULL;
  bool double_reduc = false, dummy;
  basic_block def_bb;
  struct loop * def_stmt_loop, *outer_loop = NULL;
  tree def_arg;
  gimple *def_arg_stmt;
  auto_vec<tree> vec_oprnds0;
  auto_vec<tree> vec_oprnds1;
  auto_vec<tree> vect_defs;
  auto_vec<gimple *> phis;
  int vec_num;
  tree def0, def1, tem, op0, op1 = NULL_TREE;
  bool first_p = true;
  tree cr_index_scalar_type = NULL_TREE, cr_index_vector_type = NULL_TREE;
  gimple *cond_expr_induction_def_stmt = NULL;

  /* In case of reduction chain we switch to the first stmt in the chain, but
     we don't update STMT_INFO, since only the last stmt is marked as reduction
     and has reduction properties.  */
  if (GROUP_FIRST_ELEMENT (stmt_info)
      && GROUP_FIRST_ELEMENT (stmt_info) != stmt)
    {
      stmt = GROUP_FIRST_ELEMENT (stmt_info);
      first_p = false;
    }

  if (nested_in_vect_loop_p (loop, stmt))
    {
      outer_loop = loop;
      loop = loop->inner;
      nested_cycle = true;
    }

  /* 1. Is vectorizable reduction?  */
  /* Not supportable if the reduction variable is used in the loop, unless
     it's a reduction chain.  */
  if (STMT_VINFO_RELEVANT (stmt_info) > vect_used_in_outer
      && !GROUP_FIRST_ELEMENT (stmt_info))
    return false;

  /* Reductions that are not used even in an enclosing outer-loop,
     are expected to be "live" (used out of the loop).  */
  if (STMT_VINFO_RELEVANT (stmt_info) == vect_unused_in_scope
      && !STMT_VINFO_LIVE_P (stmt_info))
    return false;

  /* Make sure it was already recognized as a reduction computation.  */
  if (STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) != vect_reduction_def
      && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (stmt)) != vect_nested_cycle)
    return false;

  /* 2. Has this been recognized as a reduction pattern?

     Check if STMT represents a pattern that has been recognized
     in earlier analysis stages.  For stmts that represent a pattern,
     the STMT_VINFO_RELATED_STMT field records the last stmt in
     the original sequence that constitutes the pattern.  */

  orig_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (stmt));
  if (orig_stmt)
    {
      orig_stmt_info = vinfo_for_stmt (orig_stmt);
      gcc_assert (STMT_VINFO_IN_PATTERN_P (orig_stmt_info));
      gcc_assert (!STMT_VINFO_IN_PATTERN_P (stmt_info));
    }

  /* 3. Check the operands of the operation.  The first operands are defined
        inside the loop body. The last operand is the reduction variable,
        which is defined by the loop-header-phi.  */

  gcc_assert (is_gimple_assign (stmt));

  /* Flatten RHS.  */
  switch (get_gimple_rhs_class (gimple_assign_rhs_code (stmt)))
    {
    case GIMPLE_SINGLE_RHS:
      op_type = TREE_OPERAND_LENGTH (gimple_assign_rhs1 (stmt));
      if (op_type == ternary_op)
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  ops[0] = TREE_OPERAND (rhs, 0);
	  ops[1] = TREE_OPERAND (rhs, 1);
	  ops[2] = TREE_OPERAND (rhs, 2);
	  code = TREE_CODE (rhs);
	}
      else
	return false;
      break;

    case GIMPLE_BINARY_RHS:
      code = gimple_assign_rhs_code (stmt);
      op_type = TREE_CODE_LENGTH (code);
      gcc_assert (op_type == binary_op);
      ops[0] = gimple_assign_rhs1 (stmt);
      ops[1] = gimple_assign_rhs2 (stmt);
      break;

    case GIMPLE_TERNARY_RHS:
      code = gimple_assign_rhs_code (stmt);
      op_type = TREE_CODE_LENGTH (code);
      gcc_assert (op_type == ternary_op);
      ops[0] = gimple_assign_rhs1 (stmt);
      ops[1] = gimple_assign_rhs2 (stmt);
      ops[2] = gimple_assign_rhs3 (stmt);
      break;

    case GIMPLE_UNARY_RHS:
      return false;

    default:
      gcc_unreachable ();
    }
  /* The default is that the reduction variable is the last in statement.  */
  int reduc_index = op_type - 1;
  if (code == MINUS_EXPR)
    reduc_index = 0;

  if (code == COND_EXPR && slp_node)
    return false;

  scalar_dest = gimple_assign_lhs (stmt);
  scalar_type = TREE_TYPE (scalar_dest);
  if (!POINTER_TYPE_P (scalar_type) && !INTEGRAL_TYPE_P (scalar_type)
      && !SCALAR_FLOAT_TYPE_P (scalar_type))
    return false;

  /* Do not try to vectorize bit-precision reductions.  */
  if ((TYPE_PRECISION (scalar_type)
       != GET_MODE_PRECISION (TYPE_MODE (scalar_type))))
    return false;

  /* All uses but the last are expected to be defined in the loop.
     The last use is the reduction variable.  In case of nested cycle this
     assumption is not true: we use reduc_index to record the index of the
     reduction variable.  */
  for (i = 0; i < op_type; i++)
    {
      if (i == reduc_index)
	continue;

      /* The condition of COND_EXPR is checked in vectorizable_condition().  */
      if (i == 0 && code == COND_EXPR)
        continue;

      is_simple_use = vect_is_simple_use (ops[i], loop_vinfo,
					  &def_stmt, &dt, &tem);
      if (!vectype_in)
	vectype_in = tem;
      gcc_assert (is_simple_use);

      if (dt != vect_internal_def
	  && dt != vect_external_def
	  && dt != vect_constant_def
	  && dt != vect_induction_def
          && !(dt == vect_nested_cycle && nested_cycle))
	return false;

      if (dt == vect_nested_cycle)
        {
          found_nested_cycle_def = true;
          reduc_def_stmt = def_stmt;
          reduc_index = i;
        }

      if (i == 1 && code == COND_EXPR && dt == vect_induction_def)
	cond_expr_induction_def_stmt = def_stmt;
    }

  is_simple_use = vect_is_simple_use (ops[reduc_index], loop_vinfo,
				      &def_stmt, &dt, &tem);
  if (!vectype_in)
    vectype_in = tem;
  gcc_assert (is_simple_use);
  if (!found_nested_cycle_def)
    reduc_def_stmt = def_stmt;

  if (reduc_def_stmt && gimple_code (reduc_def_stmt) != GIMPLE_PHI)
    return false;

  if (!(dt == vect_reduction_def
	|| dt == vect_nested_cycle
	|| ((dt == vect_internal_def || dt == vect_external_def
	     || dt == vect_constant_def || dt == vect_induction_def)
	    && nested_cycle && found_nested_cycle_def)))
    {
      /* For pattern recognized stmts, orig_stmt might be a reduction,
	 but some helper statements for the pattern might not, or
	 might be COND_EXPRs with reduction uses in the condition.  */
      gcc_assert (orig_stmt);
      return false;
    }

  enum vect_reduction_type v_reduc_type;
  gimple *tmp = vect_is_simple_reduction (loop_vinfo, reduc_def_stmt,
					  !nested_cycle, &dummy, false,
					  &v_reduc_type);

  /* If we have a condition reduction, see if we can simplify it further.  */
  if (v_reduc_type == COND_REDUCTION
      && cond_expr_induction_def_stmt != NULL
      && is_nonwrapping_integer_induction (cond_expr_induction_def_stmt, loop))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "condition expression based on integer induction.\n");
      STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) = INTEGER_INDUC_COND_REDUCTION;
    }
  else
   STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) = v_reduc_type;

  if (orig_stmt)
    gcc_assert (tmp == orig_stmt
		|| GROUP_FIRST_ELEMENT (vinfo_for_stmt (tmp)) == orig_stmt);
  else
    /* We changed STMT to be the first stmt in reduction chain, hence we
       check that in this case the first element in the chain is STMT.  */
    gcc_assert (stmt == tmp
		|| GROUP_FIRST_ELEMENT (vinfo_for_stmt (tmp)) == stmt);

  if (STMT_VINFO_LIVE_P (vinfo_for_stmt (reduc_def_stmt)))
    return false;

  if (slp_node)
    ncopies = 1;
  else
    ncopies = (LOOP_VINFO_VECT_FACTOR (loop_vinfo)
               / TYPE_VECTOR_SUBPARTS (vectype_in));

  gcc_assert (ncopies >= 1);

  vec_mode = TYPE_MODE (vectype_in);

  if (code == COND_EXPR)
    {
      /* Only call during the analysis stage, otherwise we'll lose
	 STMT_VINFO_TYPE.  */
      if (!vec_stmt && !vectorizable_condition (stmt, gsi, NULL,
						ops[reduc_index], 0, NULL))
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported condition in reduction\n");
	  return false;
        }
    }
  else
    {
      /* 4. Supportable by target?  */

      if (code == LSHIFT_EXPR || code == RSHIFT_EXPR
	  || code == LROTATE_EXPR || code == RROTATE_EXPR)
	{
	  /* Shifts and rotates are only supported by vectorizable_shifts,
	     not vectorizable_reduction.  */
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported shift or rotation.\n");
	  return false;
	}

      /* 4.1. check support for the operation in the loop  */
      optab = optab_for_tree_code (code, vectype_in, optab_default);
      if (!optab)
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "no optab.\n");

          return false;
        }

      if (optab_handler (optab, vec_mode) == CODE_FOR_nothing)
        {
          if (dump_enabled_p ())
            dump_printf (MSG_NOTE, "op not supported by target.\n");

          if (GET_MODE_SIZE (vec_mode) != UNITS_PER_WORD
              || LOOP_VINFO_VECT_FACTOR (loop_vinfo)
	          < vect_min_worthwhile_factor (code))
            return false;

          if (dump_enabled_p ())
  	    dump_printf (MSG_NOTE, "proceeding using word mode.\n");
        }

      /* Worthwhile without SIMD support?  */
      if (!VECTOR_MODE_P (TYPE_MODE (vectype_in))
          && LOOP_VINFO_VECT_FACTOR (loop_vinfo)
   	     < vect_min_worthwhile_factor (code))
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not worthwhile without SIMD support.\n");

          return false;
        }
    }

  /* 4.2. Check support for the epilog operation.

          If STMT represents a reduction pattern, then the type of the
          reduction variable may be different than the type of the rest
          of the arguments.  For example, consider the case of accumulation
          of shorts into an int accumulator; The original code:
                        S1: int_a = (int) short_a;
          orig_stmt->   S2: int_acc = plus <int_a ,int_acc>;

          was replaced with:
                        STMT: int_acc = widen_sum <short_a, int_acc>

          This means that:
          1. The tree-code that is used to create the vector operation in the
             epilog code (that reduces the partial results) is not the
             tree-code of STMT, but is rather the tree-code of the original
             stmt from the pattern that STMT is replacing.  I.e, in the example
             above we want to use 'widen_sum' in the loop, but 'plus' in the
             epilog.
          2. The type (mode) we use to check available target support
             for the vector operation to be created in the *epilog*, is
             determined by the type of the reduction variable (in the example
             above we'd check this: optab_handler (plus_optab, vect_int_mode])).
             However the type (mode) we use to check available target support
             for the vector operation to be created *inside the loop*, is
             determined by the type of the other arguments to STMT (in the
             example we'd check this: optab_handler (widen_sum_optab,
	     vect_short_mode)).

          This is contrary to "regular" reductions, in which the types of all
          the arguments are the same as the type of the reduction variable.
          For "regular" reductions we can therefore use the same vector type
          (and also the same tree-code) when generating the epilog code and
          when generating the code inside the loop.  */

  if (orig_stmt)
    {
      /* This is a reduction pattern: get the vectype from the type of the
         reduction variable, and get the tree-code from orig_stmt.  */
      gcc_assert (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
		  == TREE_CODE_REDUCTION);
      orig_code = gimple_assign_rhs_code (orig_stmt);
      gcc_assert (vectype_out);
      vec_mode = TYPE_MODE (vectype_out);
    }
  else
    {
      /* Regular reduction: use the same vectype and tree-code as used for
         the vector code inside the loop can be used for the epilog code. */
      orig_code = code;

      if (code == MINUS_EXPR)
	orig_code = PLUS_EXPR;

      /* For simple condition reductions, replace with the actual expression
	 we want to base our reduction around.  */
      if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
	  == INTEGER_INDUC_COND_REDUCTION)
	orig_code = MAX_EXPR;
    }

  if (nested_cycle)
    {
      def_bb = gimple_bb (reduc_def_stmt);
      def_stmt_loop = def_bb->loop_father;
      def_arg = PHI_ARG_DEF_FROM_EDGE (reduc_def_stmt,
                                       loop_preheader_edge (def_stmt_loop));
      if (TREE_CODE (def_arg) == SSA_NAME
          && (def_arg_stmt = SSA_NAME_DEF_STMT (def_arg))
          && gimple_code (def_arg_stmt) == GIMPLE_PHI
          && flow_bb_inside_loop_p (outer_loop, gimple_bb (def_arg_stmt))
          && vinfo_for_stmt (def_arg_stmt)
          && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def_arg_stmt))
              == vect_double_reduction_def)
        double_reduc = true;
    }

  epilog_reduc_code = ERROR_MARK;

  if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) == TREE_CODE_REDUCTION
      || STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
		== INTEGER_INDUC_COND_REDUCTION)
    {
      if (reduction_code_for_scalar_code (orig_code, &epilog_reduc_code))
	{
	  reduc_optab = optab_for_tree_code (epilog_reduc_code, vectype_out,
                                         optab_default);
	  if (!reduc_optab)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "no optab for reduction.\n");

	      epilog_reduc_code = ERROR_MARK;
	    }
	  else if (optab_handler (reduc_optab, vec_mode) == CODE_FOR_nothing)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "reduc op not supported by target.\n");

	      epilog_reduc_code = ERROR_MARK;
	    }

	  /* When epilog_reduc_code is ERROR_MARK then a reduction will be
	     generated in the epilog using multiple expressions.  This does not
	     work for condition reductions.  */
	  if (epilog_reduc_code == ERROR_MARK
	      && STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
			== INTEGER_INDUC_COND_REDUCTION)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "no reduc code for scalar code.\n");
	      return false;
	    }
	}
      else
	{
	  if (!nested_cycle || double_reduc)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "no reduc code for scalar code.\n");

	      return false;
	    }
	}
    }
  else
    {
      int scalar_precision = GET_MODE_PRECISION (TYPE_MODE (scalar_type));
      cr_index_scalar_type = make_unsigned_type (scalar_precision);
      cr_index_vector_type = build_vector_type
	(cr_index_scalar_type, TYPE_VECTOR_SUBPARTS (vectype_out));

      epilog_reduc_code = REDUC_MAX_EXPR;
      optab = optab_for_tree_code (REDUC_MAX_EXPR, cr_index_vector_type,
				   optab_default);
      if (optab_handler (optab, TYPE_MODE (cr_index_vector_type))
	  == CODE_FOR_nothing)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "reduc max op not supported by target.\n");
	  return false;
	}
    }

  if ((double_reduc
       || STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) == COND_REDUCTION
       || STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info)
		== INTEGER_INDUC_COND_REDUCTION)
      && ncopies > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "multiple types in double reduction or condition "
			 "reduction.\n");
      return false;
    }

  /* In case of widenning multiplication by a constant, we update the type
     of the constant to be the type of the other operand.  We check that the
     constant fits the type in the pattern recognition pass.  */
  if (code == DOT_PROD_EXPR
      && !types_compatible_p (TREE_TYPE (ops[0]), TREE_TYPE (ops[1])))
    {
      if (TREE_CODE (ops[0]) == INTEGER_CST)
        ops[0] = fold_convert (TREE_TYPE (ops[1]), ops[0]);
      else if (TREE_CODE (ops[1]) == INTEGER_CST)
        ops[1] = fold_convert (TREE_TYPE (ops[0]), ops[1]);
      else
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "invalid types in dot-prod\n");

          return false;
        }
    }

  if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) == COND_REDUCTION)
    {
      widest_int ni;

      if (! max_loop_iterations (loop, &ni))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "loop count not known, cannot create cond "
			     "reduction.\n");
	  return false;
	}
      /* Convert backedges to iterations.  */
      ni += 1;

      /* The additional index will be the same type as the condition.  Check
	 that the loop can fit into this less one (because we'll use up the
	 zero slot for when there are no matches).  */
      tree max_index = TYPE_MAX_VALUE (cr_index_scalar_type);
      if (wi::geu_p (ni, wi::to_widest (max_index)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "loop size is greater than data size.\n");
	  return false;
	}
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      if (first_p
	  && !vect_model_reduction_cost (stmt_info, epilog_reduc_code, ncopies,
					 reduc_index))
        return false;
      STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
      return true;
    }

  /** Transform.  **/

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform reduction.\n");

  /* FORNOW: Multiple types are not supported for condition.  */
  if (code == COND_EXPR)
    gcc_assert (ncopies == 1);

  /* Create the destination vector  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype_out);

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  For more details see documentation
     in vectorizable_operation.  */

  /* If the reduction is used in an outer loop we need to generate
     VF intermediate results, like so (e.g. for ncopies=2):
	r0 = phi (init, r0)
	r1 = phi (init, r1)
	r0 = x0 + r0;
        r1 = x1 + r1;
    (i.e. we generate VF results in 2 registers).
    In this case we have a separate def-use cycle for each copy, and therefore
    for each copy we get the vector def for the reduction variable from the
    respective phi node created for this copy.

    Otherwise (the reduction is unused in the loop nest), we can combine
    together intermediate results, like so (e.g. for ncopies=2):
	r = phi (init, r)
	r = x0 + r;
	r = x1 + r;
   (i.e. we generate VF/2 results in a single register).
   In this case for each copy we get the vector def for the reduction variable
   from the vectorized reduction operation generated in the previous iteration.
  */

  if (STMT_VINFO_RELEVANT (stmt_info) <= vect_used_only_live)
    {
      single_defuse_cycle = true;
      epilog_copies = 1;
    }
  else
    epilog_copies = ncopies;

  prev_stmt_info = NULL;
  prev_phi_info = NULL;
  if (slp_node)
    vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
  else
    {
      vec_num = 1;
      vec_oprnds0.create (1);
      if (op_type == ternary_op)
        vec_oprnds1.create (1);
    }

  phis.create (vec_num);
  vect_defs.create (vec_num);
  if (!slp_node)
    vect_defs.quick_push (NULL_TREE);

  for (j = 0; j < ncopies; j++)
    {
      if (j == 0 || !single_defuse_cycle)
	{
          for (i = 0; i < vec_num; i++)
            {
              /* Create the reduction-phi that defines the reduction
                 operand.  */
              new_phi = create_phi_node (vec_dest, loop->header);
              set_vinfo_for_stmt (new_phi,
                                  new_stmt_vec_info (new_phi, loop_vinfo));
               if (j == 0 || slp_node)
                 phis.quick_push (new_phi);
            }
        }

      if (code == COND_EXPR)
        {
          gcc_assert (!slp_node);
          vectorizable_condition (stmt, gsi, vec_stmt, 
                                  PHI_RESULT (phis[0]), 
                                  reduc_index, NULL);
          /* Multiple types are not supported for condition.  */
          break;
        }

      /* Handle uses.  */
      if (j == 0)
        {
          op0 = ops[!reduc_index];
          if (op_type == ternary_op)
            {
              if (reduc_index == 0)
                op1 = ops[2];
              else
                op1 = ops[1];
            }

          if (slp_node)
            vect_get_vec_defs (op0, op1, stmt, &vec_oprnds0, &vec_oprnds1,
                               slp_node, -1);
          else
            {
              loop_vec_def0 = vect_get_vec_def_for_operand (ops[!reduc_index],
                                                            stmt);
              vec_oprnds0.quick_push (loop_vec_def0);
              if (op_type == ternary_op)
               {
                 loop_vec_def1 = vect_get_vec_def_for_operand (op1, stmt);
                 vec_oprnds1.quick_push (loop_vec_def1);
               }
            }
        }
      else
        {
          if (!slp_node)
            {
              enum vect_def_type dt;
	      gimple *dummy_stmt;

              vect_is_simple_use (ops[!reduc_index], loop_vinfo,
                                  &dummy_stmt, &dt);
              loop_vec_def0 = vect_get_vec_def_for_stmt_copy (dt,
                                                              loop_vec_def0);
              vec_oprnds0[0] = loop_vec_def0;
              if (op_type == ternary_op)
                {
                  vect_is_simple_use (op1, loop_vinfo, &dummy_stmt, &dt);
                  loop_vec_def1 = vect_get_vec_def_for_stmt_copy (dt,
                                                                loop_vec_def1);
                  vec_oprnds1[0] = loop_vec_def1;
                }
            }

          if (single_defuse_cycle)
            reduc_def = gimple_assign_lhs (new_stmt);

          STMT_VINFO_RELATED_STMT (prev_phi_info) = new_phi;
        }

      FOR_EACH_VEC_ELT (vec_oprnds0, i, def0)
        {
          if (slp_node)
            reduc_def = PHI_RESULT (phis[i]);
          else
            {
              if (!single_defuse_cycle || j == 0)
                reduc_def = PHI_RESULT (new_phi);
            }

          def1 = ((op_type == ternary_op)
                  ? vec_oprnds1[i] : NULL);
          if (op_type == binary_op)
            {
              if (reduc_index == 0)
                expr = build2 (code, vectype_out, reduc_def, def0);
              else
                expr = build2 (code, vectype_out, def0, reduc_def);
            }
          else
            {
              if (reduc_index == 0)
                expr = build3 (code, vectype_out, reduc_def, def0, def1);
              else
                {
                  if (reduc_index == 1)
                    expr = build3 (code, vectype_out, def0, reduc_def, def1);
                  else
                    expr = build3 (code, vectype_out, def0, def1, reduc_def);
                }
            }

          new_stmt = gimple_build_assign (vec_dest, expr);
          new_temp = make_ssa_name (vec_dest, new_stmt);
          gimple_assign_set_lhs (new_stmt, new_temp);
          vect_finish_stmt_generation (stmt, new_stmt, gsi);

          if (slp_node)
            {
              SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
              vect_defs.quick_push (new_temp);
            }
          else
            vect_defs[0] = new_temp;
        }

      if (slp_node)
        continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;

      prev_stmt_info = vinfo_for_stmt (new_stmt);
      prev_phi_info = vinfo_for_stmt (new_phi);
    }

  tree indx_before_incr, indx_after_incr, cond_name = NULL;

  /* Finalize the reduction-phi (set its arguments) and create the
     epilog reduction code.  */
  if ((!single_defuse_cycle || code == COND_EXPR) && !slp_node)
    {
      new_temp = gimple_assign_lhs (*vec_stmt);
      vect_defs[0] = new_temp;

      /* For cond reductions we want to create a new vector (INDEX_COND_EXPR)
	 which is updated with the current index of the loop for every match of
	 the original loop's cond_expr (VEC_STMT).  This results in a vector
	 containing the last time the condition passed for that vector lane.
	 The first match will be a 1 to allow 0 to be used for non-matching
	 indexes.  If there are no matches at all then the vector will be all
	 zeroes.  */
      if (STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info) == COND_REDUCTION)
	{
	  int nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
	  int k;

	  gcc_assert (gimple_assign_rhs_code (*vec_stmt) == VEC_COND_EXPR);

	  /* First we create a simple vector induction variable which starts
	     with the values {1,2,3,...} (SERIES_VECT) and increments by the
	     vector size (STEP).  */

	  /* Create a {1,2,3,...} vector.  */
	  tree *vtemp = XALLOCAVEC (tree, nunits_out);
	  for (k = 0; k < nunits_out; ++k)
	    vtemp[k] = build_int_cst (cr_index_scalar_type, k + 1);
	  tree series_vect = build_vector (cr_index_vector_type, vtemp);

	  /* Create a vector of the step value.  */
	  tree step = build_int_cst (cr_index_scalar_type, nunits_out);
	  tree vec_step = build_vector_from_val (cr_index_vector_type, step);

	  /* Create an induction variable.  */
	  gimple_stmt_iterator incr_gsi;
	  bool insert_after;
	  standard_iv_increment_position (loop, &incr_gsi, &insert_after);
	  create_iv (series_vect, vec_step, NULL_TREE, loop, &incr_gsi,
		     insert_after, &indx_before_incr, &indx_after_incr);

	  /* Next create a new phi node vector (NEW_PHI_TREE) which starts
	     filled with zeros (VEC_ZERO).  */

	  /* Create a vector of 0s.  */
	  tree zero = build_zero_cst (cr_index_scalar_type);
	  tree vec_zero = build_vector_from_val (cr_index_vector_type, zero);

	  /* Create a vector phi node.  */
	  tree new_phi_tree = make_ssa_name (cr_index_vector_type);
	  new_phi = create_phi_node (new_phi_tree, loop->header);
	  set_vinfo_for_stmt (new_phi,
			      new_stmt_vec_info (new_phi, loop_vinfo));
	  add_phi_arg (new_phi, vec_zero, loop_preheader_edge (loop),
		       UNKNOWN_LOCATION);

	  /* Now take the condition from the loops original cond_expr
	     (VEC_STMT) and produce a new cond_expr (INDEX_COND_EXPR) which for
	     every match uses values from the induction variable
	     (INDEX_BEFORE_INCR) otherwise uses values from the phi node
	     (NEW_PHI_TREE).
	     Finally, we update the phi (NEW_PHI_TREE) to take the value of
	     the new cond_expr (INDEX_COND_EXPR).  */

	  /* Duplicate the condition from vec_stmt.  */
	  tree ccompare = unshare_expr (gimple_assign_rhs1 (*vec_stmt));

	  /* Create a conditional, where the condition is taken from vec_stmt
	     (CCOMPARE), then is the induction index (INDEX_BEFORE_INCR) and
	     else is the phi (NEW_PHI_TREE).  */
	  tree index_cond_expr = build3 (VEC_COND_EXPR, cr_index_vector_type,
					 ccompare, indx_before_incr,
					 new_phi_tree);
	  cond_name = make_ssa_name (cr_index_vector_type);
	  gimple *index_condition = gimple_build_assign (cond_name,
							 index_cond_expr);
	  gsi_insert_before (&incr_gsi, index_condition, GSI_SAME_STMT);
	  stmt_vec_info index_vec_info = new_stmt_vec_info (index_condition,
							    loop_vinfo);
	  STMT_VINFO_VECTYPE (index_vec_info) = cr_index_vector_type;
	  set_vinfo_for_stmt (index_condition, index_vec_info);

	  /* Update the phi with the vec cond.  */
	  add_phi_arg (new_phi, cond_name, loop_latch_edge (loop),
		       UNKNOWN_LOCATION);
	}
    }

  vect_create_epilog_for_reduction (vect_defs, stmt, epilog_copies,
                                    epilog_reduc_code, phis, reduc_index,
				    double_reduc, slp_node, cond_name);

  return true;
}

/* Function vect_min_worthwhile_factor.

   For a loop where we could vectorize the operation indicated by CODE,
   return the minimum vectorization factor that makes it worthwhile
   to use generic vectors.  */
int
vect_min_worthwhile_factor (enum tree_code code)
{
  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case NEGATE_EXPR:
      return 4;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_NOT_EXPR:
      return 2;

    default:
      return INT_MAX;
    }
}


/* Function vectorizable_induction

   Check if PHI performs an induction computation that can be vectorized.
   If VEC_STMT is also passed, vectorize the induction PHI: create a vectorized
   phi to replace it, put it in VEC_STMT, and add it to the same basic block.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_induction (gimple *phi,
			gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
			gimple **vec_stmt)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (phi);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;
  tree vec_def;

  gcc_assert (ncopies >= 1);
  /* FORNOW. These restrictions should be relaxed.  */
  if (nested_in_vect_loop_p (loop, phi))
    {
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      gimple *exit_phi;
      edge latch_e;
      tree loop_arg;

      if (ncopies > 1)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "multiple types in nested loop.\n");
	  return false;
	}

      exit_phi = NULL;
      latch_e = loop_latch_edge (loop->inner);
      loop_arg = PHI_ARG_DEF_FROM_EDGE (phi, latch_e);
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, loop_arg)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (is_gimple_debug (use_stmt))
	    continue;

	  if (!flow_bb_inside_loop_p (loop->inner, gimple_bb (use_stmt)))
	    {
	      exit_phi = use_stmt;
	      break;
	    }
	}
      if (exit_phi)
	{
	  stmt_vec_info exit_phi_vinfo  = vinfo_for_stmt (exit_phi);
	  if (!(STMT_VINFO_RELEVANT_P (exit_phi_vinfo)
		&& !STMT_VINFO_LIVE_P (exit_phi_vinfo)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "inner-loop induction only used outside "
				 "of the outer vectorized loop.\n");
	      return false;
	    }
	}
    }

  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  /* FORNOW: SLP not supported.  */
  if (STMT_SLP_TYPE (stmt_info))
    return false;

  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def);

  if (gimple_code (phi) != GIMPLE_PHI)
    return false;

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = induc_vec_info_type;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "=== vectorizable_induction ===\n");
      vect_model_induction_cost (stmt_info, ncopies);
      return true;
    }

  /** Transform.  **/

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform induction phi.\n");

  vec_def = get_initial_def_for_induction (phi);
  *vec_stmt = SSA_NAME_DEF_STMT (vec_def);
  return true;
}

/* Function vectorizable_live_operation.

   STMT computes a value that is used outside the loop.  Check if
   it can be supported.  */

bool
vectorizable_live_operation (gimple *stmt,
			     gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
			     slp_tree slp_node, int slp_index,
			     gimple **vec_stmt)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  imm_use_iterator imm_iter;
  tree lhs, lhs_type, bitsize, vec_bitsize;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;
  gimple *use_stmt;
  auto_vec<tree> vec_oprnds;

  gcc_assert (STMT_VINFO_LIVE_P (stmt_info));

  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def)
    return false;

  /* FORNOW.  CHECKME.  */
  if (nested_in_vect_loop_p (loop, stmt))
    return false;

  /* If STMT is not relevant and it is a simple assignment and its inputs are
     invariant then it can remain in place, unvectorized.  The original last
     scalar value that it computes will be used.  */
  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    {
      gcc_assert (is_simple_and_all_uses_invariant (stmt, loop_vinfo));
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "statement is simple and uses invariant.  Leaving in "
			 "place.\n");
      return true;
    }

  if (!vec_stmt)
    /* No transformation required.  */
    return true;

  /* If stmt has a related stmt, then use that for getting the lhs.  */
  if (is_pattern_stmt_p (stmt_info))
    stmt = STMT_VINFO_RELATED_STMT (stmt_info);

  lhs = (is_a <gphi *> (stmt)) ? gimple_phi_result (stmt)
	: gimple_get_lhs (stmt);
  lhs_type = TREE_TYPE (lhs);

  /* Find all uses of STMT outside the loop - there should be exactly one.  */
  auto_vec<gimple *, 4> worklist;
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, lhs)
    if (!flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
	worklist.safe_push (use_stmt);
  gcc_assert (worklist.length () == 1);

  bitsize = TYPE_SIZE (lhs_type);
  vec_bitsize = TYPE_SIZE (vectype);

  /* Get the vectorized lhs of STMT and the lane to use (counted in bits).  */
  tree vec_lhs, bitstart;
  if (slp_node)
    {
      gcc_assert (slp_index >= 0);

      int num_scalar = SLP_TREE_SCALAR_STMTS (slp_node).length ();
      int num_vec = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
      int scalar_per_vec = num_scalar / num_vec;

      /* There are three possibilites here:
	 1: All scalar stmts fit in a single vector.
	 2: All scalar stmts fit multiple times into a single vector.
	    We must choose the last occurence of stmt in the vector.
	 3: Scalar stmts are split across multiple vectors.
	    We must choose the correct vector and mod the lane accordingly.  */

      /* Get the correct slp vectorized stmt.  */
      int vec_entry = slp_index / scalar_per_vec;
      vec_lhs = gimple_get_lhs (SLP_TREE_VEC_STMTS (slp_node)[vec_entry]);

      /* Get entry to use.  */
      bitstart = build_int_cst (unsigned_type_node,
				scalar_per_vec - (slp_index % scalar_per_vec));
      bitstart = int_const_binop (MULT_EXPR, bitsize, bitstart);
      bitstart = int_const_binop (MINUS_EXPR, vec_bitsize, bitstart);
    }
  else
    {
      enum vect_def_type dt = STMT_VINFO_DEF_TYPE (stmt_info);
      vec_lhs = vect_get_vec_def_for_operand_1 (stmt, dt);

      /* For multiple copies, get the last copy.  */
      for (int i = 1; i < ncopies; ++i)
	vec_lhs = vect_get_vec_def_for_stmt_copy (vect_unknown_def_type,
						  vec_lhs);

      /* Get the last lane in the vector.  */
      bitstart = int_const_binop (MINUS_EXPR, vec_bitsize, bitsize);
    }

  /* Create a new vectorized stmt for the uses of STMT and insert outside the
     loop.  */
  tree new_name = make_ssa_name (lhs_type);
  tree new_tree = build3 (BIT_FIELD_REF, lhs_type, vec_lhs, bitsize, bitstart);
  gimple *new_stmt = gimple_build_assign (new_name, new_tree);
  gsi_insert_on_edge_immediate (single_exit (loop), new_stmt);

  /* Replace all uses of the USE_STMT in the worklist with the newly inserted
     statement.  */
  use_stmt = worklist.pop ();
  replace_uses_by (gimple_phi_result (use_stmt), new_name);
  update_stmt (use_stmt);

  return true;
}

/* Kill any debug uses outside LOOP of SSA names defined in STMT.  */

static void
vect_loop_kill_debug_uses (struct loop *loop, gimple *stmt)
{
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  def_operand_p def_p;
  gimple *ustmt;

  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, op_iter, SSA_OP_DEF)
    {
      FOR_EACH_IMM_USE_STMT (ustmt, imm_iter, DEF_FROM_PTR (def_p))
	{
	  basic_block bb;

	  if (!is_gimple_debug (ustmt))
	    continue;

	  bb = gimple_bb (ustmt);

	  if (!flow_bb_inside_loop_p (loop, bb))
	    {
	      if (gimple_debug_bind_p (ustmt))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
                                     "killing debug use\n");

		  gimple_debug_bind_reset_value (ustmt);
		  update_stmt (ustmt);
		}
	      else
		gcc_unreachable ();
	    }
	}
    }
}


/* This function builds ni_name = number of iterations.  Statements
   are emitted on the loop preheader edge.  */

static tree
vect_build_loop_niters (loop_vec_info loop_vinfo)
{
  tree ni = unshare_expr (LOOP_VINFO_NITERS (loop_vinfo));
  if (TREE_CODE (ni) == INTEGER_CST)
    return ni;
  else
    {
      tree ni_name, var;
      gimple_seq stmts = NULL;
      edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));

      var = create_tmp_var (TREE_TYPE (ni), "niters");
      ni_name = force_gimple_operand (ni, &stmts, false, var);
      if (stmts)
	gsi_insert_seq_on_edge_immediate (pe, stmts);

      return ni_name;
    }
}


/* This function generates the following statements:

   ni_name = number of iterations loop executes
   ratio = ni_name / vf
   ratio_mult_vf_name = ratio * vf

   and places them on the loop preheader edge.  */

static void
vect_generate_tmps_on_preheader (loop_vec_info loop_vinfo,
				 tree ni_name,
				 tree *ratio_mult_vf_name_ptr,
				 tree *ratio_name_ptr)
{
  tree ni_minus_gap_name;
  tree var;
  tree ratio_name;
  tree ratio_mult_vf_name;
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));
  tree log_vf;

  log_vf = build_int_cst (TREE_TYPE (ni_name), exact_log2 (vf));

  /* If epilogue loop is required because of data accesses with gaps, we
     subtract one iteration from the total number of iterations here for
     correct calculation of RATIO.  */
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    {
      ni_minus_gap_name = fold_build2 (MINUS_EXPR, TREE_TYPE (ni_name),
				       ni_name,
			               build_one_cst (TREE_TYPE (ni_name)));
      if (!is_gimple_val (ni_minus_gap_name))
	{
	  var = create_tmp_var (TREE_TYPE (ni_name), "ni_gap");
	  gimple *stmts = NULL;
          ni_minus_gap_name = force_gimple_operand (ni_minus_gap_name, &stmts,
						    true, var);
	  gsi_insert_seq_on_edge_immediate (pe, stmts);
        }
    }
  else
    ni_minus_gap_name = ni_name;

  /* Create: ratio = ni >> log2(vf) */
  /* ???  As we have ni == number of latch executions + 1, ni could
     have overflown to zero.  So avoid computing ratio based on ni
     but compute it using the fact that we know ratio will be at least
     one, thus via (ni - vf) >> log2(vf) + 1.  */
  ratio_name
    = fold_build2 (PLUS_EXPR, TREE_TYPE (ni_name),
		   fold_build2 (RSHIFT_EXPR, TREE_TYPE (ni_name),
				fold_build2 (MINUS_EXPR, TREE_TYPE (ni_name),
					     ni_minus_gap_name,
					     build_int_cst
					       (TREE_TYPE (ni_name), vf)),
				log_vf),
		   build_int_cst (TREE_TYPE (ni_name), 1));
  if (!is_gimple_val (ratio_name))
    {
      var = create_tmp_var (TREE_TYPE (ni_name), "bnd");
      gimple *stmts = NULL;
      ratio_name = force_gimple_operand (ratio_name, &stmts, true, var);
      gsi_insert_seq_on_edge_immediate (pe, stmts);
    }
  *ratio_name_ptr = ratio_name;

  /* Create: ratio_mult_vf = ratio << log2 (vf).  */

  if (ratio_mult_vf_name_ptr)
    {
      ratio_mult_vf_name = fold_build2 (LSHIFT_EXPR, TREE_TYPE (ratio_name),
					ratio_name, log_vf);
      if (!is_gimple_val (ratio_mult_vf_name))
	{
	  var = create_tmp_var (TREE_TYPE (ni_name), "ratio_mult_vf");
	  gimple *stmts = NULL;
	  ratio_mult_vf_name = force_gimple_operand (ratio_mult_vf_name, &stmts,
						     true, var);
	  gsi_insert_seq_on_edge_immediate (pe, stmts);
	}
      *ratio_mult_vf_name_ptr = ratio_mult_vf_name;
    }

  return;
}


/* Function vect_transform_loop.

   The analysis phase has determined that the loop is vectorizable.
   Vectorize the loop - created vectorized stmts to replace the scalar
   stmts in the loop, and update the loop exit condition.  */

void
vect_transform_loop (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  int i;
  tree ratio = NULL;
  int vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  bool grouped_store;
  bool slp_scheduled = false;
  gimple *stmt, *pattern_stmt;
  gimple_seq pattern_def_seq = NULL;
  gimple_stmt_iterator pattern_def_si = gsi_none ();
  bool transform_pattern_stmt = false;
  bool check_profitability = false;
  int th;
  /* Record number of iterations before we started tampering with the profile. */
  gcov_type expected_iterations = expected_loop_iterations_unbounded (loop);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "=== vec_transform_loop ===\n");

  /* If profile is inprecise, we have chance to fix it up.  */
  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    expected_iterations = LOOP_VINFO_INT_NITERS (loop_vinfo);

  /* Use the more conservative vectorization threshold.  If the number
     of iterations is constant assume the cost check has been performed
     by our caller.  If the threshold makes all loops profitable that
     run at least the vectorization factor number of times checking
     is pointless, too.  */
  th = LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo);
  if (th >= LOOP_VINFO_VECT_FACTOR (loop_vinfo) - 1
      && !LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Profitability threshold is %d loop iterations.\n",
                         th);
      check_profitability = true;
    }

  /* Version the loop first, if required, so the profitability check
     comes first.  */

  if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    {
      vect_loop_versioning (loop_vinfo, th, check_profitability);
      check_profitability = false;
    }

  tree ni_name = vect_build_loop_niters (loop_vinfo);
  LOOP_VINFO_NITERS_UNCHANGED (loop_vinfo) = ni_name;

  /* Peel the loop if there are data refs with unknown alignment.
     Only one data ref with unknown store is allowed.  */

  if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo))
    {
      vect_do_peeling_for_alignment (loop_vinfo, ni_name,
				     th, check_profitability);
      check_profitability = false;
      /* The above adjusts LOOP_VINFO_NITERS, so cause ni_name to
	 be re-computed.  */
      ni_name = NULL_TREE;
    }

  /* If the loop has a symbolic number of iterations 'n' (i.e. it's not a
     compile time constant), or it is a constant that doesn't divide by the
     vectorization factor, then an epilog loop needs to be created.
     We therefore duplicate the loop: the original loop will be vectorized,
     and will compute the first (n/VF) iterations.  The second copy of the loop
     will remain scalar and will compute the remaining (n%VF) iterations.
     (VF is the vectorization factor).  */

  if (LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo)
      || LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    {
      tree ratio_mult_vf;
      if (!ni_name)
	ni_name = vect_build_loop_niters (loop_vinfo);
      vect_generate_tmps_on_preheader (loop_vinfo, ni_name, &ratio_mult_vf,
				       &ratio);
      vect_do_peeling_for_loop_bound (loop_vinfo, ni_name, ratio_mult_vf,
				      th, check_profitability);
    }
  else if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    ratio = build_int_cst (TREE_TYPE (LOOP_VINFO_NITERS (loop_vinfo)),
		LOOP_VINFO_INT_NITERS (loop_vinfo) / vectorization_factor);
  else
    {
      if (!ni_name)
	ni_name = vect_build_loop_niters (loop_vinfo);
      vect_generate_tmps_on_preheader (loop_vinfo, ni_name, NULL, &ratio);
    }

  /* 1) Make sure the loop header has exactly two entries
     2) Make sure we have a preheader basic block.  */

  gcc_assert (EDGE_COUNT (loop->header->preds) == 2);

  split_edge (loop_preheader_edge (loop));

  /* FORNOW: the vectorizer supports only loops which body consist
     of one basic block (header + empty latch). When the vectorizer will
     support more involved loop forms, the order by which the BBs are
     traversed need to be reconsidered.  */

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      stmt_vec_info stmt_info;

      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
        {
	  gphi *phi = si.phi ();
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
                               "------>vectorizing phi: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	    }
	  stmt_info = vinfo_for_stmt (phi);
	  if (!stmt_info)
	    continue;

	  if (MAY_HAVE_DEBUG_STMTS && !STMT_VINFO_LIVE_P (stmt_info))
	    vect_loop_kill_debug_uses (loop, phi);

	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
	    continue;

	  if (STMT_VINFO_VECTYPE (stmt_info)
	      && (TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info))
		  != (unsigned HOST_WIDE_INT) vectorization_factor)
	      && dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "multiple-types.\n");

	  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location, "transform phi.\n");
	      vect_transform_stmt (phi, NULL, NULL, NULL, NULL);
	    }
	}

      pattern_stmt = NULL;
      for (gimple_stmt_iterator si = gsi_start_bb (bb);
	   !gsi_end_p (si) || transform_pattern_stmt;)
	{
	  bool is_store;

          if (transform_pattern_stmt)
	    stmt = pattern_stmt;
          else
	    {
	      stmt = gsi_stmt (si);
	      /* During vectorization remove existing clobber stmts.  */
	      if (gimple_clobber_p (stmt))
		{
		  unlink_stmt_vdef (stmt);
		  gsi_remove (&si, true);
		  release_defs (stmt);
		  continue;
		}
	    }

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "------>vectorizing statement: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	    }

	  stmt_info = vinfo_for_stmt (stmt);

	  /* vector stmts created in the outer-loop during vectorization of
	     stmts in an inner-loop may not have a stmt_info, and do not
	     need to be vectorized.  */
	  if (!stmt_info)
	    {
	      gsi_next (&si);
	      continue;
	    }

	  if (MAY_HAVE_DEBUG_STMTS && !STMT_VINFO_LIVE_P (stmt_info))
	    vect_loop_kill_debug_uses (loop, stmt);

	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
            {
              if (STMT_VINFO_IN_PATTERN_P (stmt_info)
                  && (pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info))
                  && (STMT_VINFO_RELEVANT_P (vinfo_for_stmt (pattern_stmt))
                      || STMT_VINFO_LIVE_P (vinfo_for_stmt (pattern_stmt))))
                {
                  stmt = pattern_stmt;
                  stmt_info = vinfo_for_stmt (stmt);
                }
              else
	        {
   	          gsi_next (&si);
	          continue;
                }
	    }
          else if (STMT_VINFO_IN_PATTERN_P (stmt_info)
                   && (pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info))
                   && (STMT_VINFO_RELEVANT_P (vinfo_for_stmt (pattern_stmt))
                       || STMT_VINFO_LIVE_P (vinfo_for_stmt (pattern_stmt))))
            transform_pattern_stmt = true;

	  /* If pattern statement has def stmts, vectorize them too.  */
	  if (is_pattern_stmt_p (stmt_info))
	    {
	      if (pattern_def_seq == NULL)
		{
		  pattern_def_seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_info);
		  pattern_def_si = gsi_start (pattern_def_seq);
		}
	      else if (!gsi_end_p (pattern_def_si))
		gsi_next (&pattern_def_si);
	      if (pattern_def_seq != NULL)
		{
		  gimple *pattern_def_stmt = NULL;
		  stmt_vec_info pattern_def_stmt_info = NULL;

		  while (!gsi_end_p (pattern_def_si))
		    {
		      pattern_def_stmt = gsi_stmt (pattern_def_si);
		      pattern_def_stmt_info
			= vinfo_for_stmt (pattern_def_stmt);
		      if (STMT_VINFO_RELEVANT_P (pattern_def_stmt_info)
			  || STMT_VINFO_LIVE_P (pattern_def_stmt_info))
			break;
		      gsi_next (&pattern_def_si);
		    }

		  if (!gsi_end_p (pattern_def_si))
		    {
		      if (dump_enabled_p ())
			{
			  dump_printf_loc (MSG_NOTE, vect_location,
					   "==> vectorizing pattern def "
					   "stmt: ");
			  dump_gimple_stmt (MSG_NOTE, TDF_SLIM,
					    pattern_def_stmt, 0);
			}

		      stmt = pattern_def_stmt;
		      stmt_info = pattern_def_stmt_info;
		    }
		  else
		    {
		      pattern_def_si = gsi_none ();
		      transform_pattern_stmt = false;
		    }
		}
	      else
		transform_pattern_stmt = false;
            }

	  if (STMT_VINFO_VECTYPE (stmt_info))
	    {
	      unsigned int nunits
		= (unsigned int)
		  TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
	      if (!STMT_SLP_TYPE (stmt_info)
		  && nunits != (unsigned int) vectorization_factor
		  && dump_enabled_p ())
		  /* For SLP VF is set according to unrolling factor, and not
		     to vector size, hence for SLP this print is not valid.  */
		dump_printf_loc (MSG_NOTE, vect_location, "multiple-types.\n");
	    }

	  /* SLP. Schedule all the SLP instances when the first SLP stmt is
	     reached.  */
	  if (STMT_SLP_TYPE (stmt_info))
	    {
	      if (!slp_scheduled)
		{
		  slp_scheduled = true;

		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "=== scheduling SLP instances ===\n");

		  vect_schedule_slp (loop_vinfo);
		}

	      /* Hybrid SLP stmts must be vectorized in addition to SLP.  */
	      if (!vinfo_for_stmt (stmt) || PURE_SLP_STMT (stmt_info))
		{
		  if (!transform_pattern_stmt && gsi_end_p (pattern_def_si))
		    {
		      pattern_def_seq = NULL;
		      gsi_next (&si);
		    }
		  continue;
		}
	    }

	  /* -------- vectorize statement ------------ */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "transform statement.\n");

	  grouped_store = false;
	  is_store = vect_transform_stmt (stmt, &si, &grouped_store, NULL, NULL);
          if (is_store)
            {
	      if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
		{
		  /* Interleaving. If IS_STORE is TRUE, the vectorization of the
		     interleaving chain was completed - free all the stores in
		     the chain.  */
		  gsi_next (&si);
		  vect_remove_stores (GROUP_FIRST_ELEMENT (stmt_info));
		}
	      else
		{
		  /* Free the attached stmt_vec_info and remove the stmt.  */
		  gimple *store = gsi_stmt (si);
		  free_stmt_vec_info (store);
		  unlink_stmt_vdef (store);
		  gsi_remove (&si, true);
		  release_defs (store);
		}

	      /* Stores can only appear at the end of pattern statements.  */
	      gcc_assert (!transform_pattern_stmt);
	      pattern_def_seq = NULL;
	    }
	  else if (!transform_pattern_stmt && gsi_end_p (pattern_def_si))
	    {
	      pattern_def_seq = NULL;
	      gsi_next (&si);
	    }
	}		        /* stmts in BB */
    }				/* BBs in loop */

  slpeel_make_loop_iterate_ntimes (loop, ratio);

  /* Reduce loop iterations by the vectorization factor.  */
  scale_loop_profile (loop, GCOV_COMPUTE_SCALE (1, vectorization_factor),
		      expected_iterations / vectorization_factor);
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    {
      if (loop->nb_iterations_upper_bound != 0)
        loop->nb_iterations_upper_bound = loop->nb_iterations_upper_bound - 1;
      if (loop->nb_iterations_likely_upper_bound != 0)
        loop->nb_iterations_likely_upper_bound
	   = loop->nb_iterations_likely_upper_bound - 1;
    }
  loop->nb_iterations_upper_bound
    = wi::udiv_floor (loop->nb_iterations_upper_bound + 1,
		      vectorization_factor) - 1;
  loop->nb_iterations_likely_upper_bound
    = wi::udiv_floor (loop->nb_iterations_likely_upper_bound + 1,
		      vectorization_factor) - 1;

  if (loop->any_estimate)
    {
      loop->nb_iterations_estimate
        = wi::udiv_floor (loop->nb_iterations_estimate, vectorization_factor);
       if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo)
	   && loop->nb_iterations_estimate != 0)
	 loop->nb_iterations_estimate = loop->nb_iterations_estimate - 1;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "LOOP VECTORIZED\n");
      if (loop->inner)
	dump_printf_loc (MSG_NOTE, vect_location,
			 "OUTER LOOP VECTORIZED\n");
      dump_printf (MSG_NOTE, "\n");
    }

  /* Free SLP instances here because otherwise stmt reference counting
     won't work.  */
  slp_instance instance;
  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (loop_vinfo), i, instance)
    vect_free_slp_instance (instance);
  LOOP_VINFO_SLP_INSTANCES (loop_vinfo).release ();
}

/* The code below is trying to perform simple optimization - revert
   if-conversion for masked stores, i.e. if the mask of a store is zero
   do not perform it and all stored value producers also if possible.
   For example,
     for (i=0; i<n; i++)
       if (c[i])
	{
	  p1[i] += 1;
	  p2[i] = p3[i] +2;
	}
   this transformation will produce the following semi-hammock:

   if (!mask__ifc__42.18_165 == { 0, 0, 0, 0, 0, 0, 0, 0 })
     {
       vect__11.19_170 = MASK_LOAD (vectp_p1.20_168, 0B, mask__ifc__42.18_165);
       vect__12.22_172 = vect__11.19_170 + vect_cst__171;
       MASK_STORE (vectp_p1.23_175, 0B, mask__ifc__42.18_165, vect__12.22_172);
       vect__18.25_182 = MASK_LOAD (vectp_p3.26_180, 0B, mask__ifc__42.18_165);
       vect__19.28_184 = vect__18.25_182 + vect_cst__183;
       MASK_STORE (vectp_p2.29_187, 0B, mask__ifc__42.18_165, vect__19.28_184);
     }
*/

void
optimize_mask_stores (struct loop *loop)
{
  basic_block *bbs = get_loop_body (loop);
  unsigned nbbs = loop->num_nodes;
  unsigned i;
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  auto_vec<gimple *> worklist;

  vect_location = find_loop_location (loop);
  /* Pick up all masked stores in loop if any.  */
  for (i = 0; i < nbbs; i++)
    {
      bb = bbs[i];
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (is_gimple_call (stmt)
	      && gimple_call_internal_p (stmt)
	      && gimple_call_internal_fn (stmt) == IFN_MASK_STORE)
	    worklist.safe_push (stmt);
	}
    }

  free (bbs);
  if (worklist.is_empty ())
    return;

  /* Loop has masked stores.  */
  while (!worklist.is_empty ())
    {
      gimple *last, *last_store;
      edge e, efalse;
      tree mask;
      basic_block store_bb, join_bb;
      gimple_stmt_iterator gsi_to;
      tree vdef, new_vdef;
      gphi *phi;
      tree vectype;
      tree zero;

      last = worklist.pop ();
      mask = gimple_call_arg (last, 2);
      bb = gimple_bb (last);
      /* Create new bb.  */
      e = split_block (bb, last);
      join_bb = e->dest;
      store_bb = create_empty_bb (bb);
      add_bb_to_loop (store_bb, loop);
      e->flags = EDGE_TRUE_VALUE;
      efalse = make_edge (bb, store_bb, EDGE_FALSE_VALUE);
      /* Put STORE_BB to likely part.  */
      efalse->probability = PROB_UNLIKELY;
      store_bb->frequency = PROB_ALWAYS - EDGE_FREQUENCY (efalse);
      make_edge (store_bb, join_bb, EDGE_FALLTHRU);
      if (dom_info_available_p (CDI_DOMINATORS))
	set_immediate_dominator (CDI_DOMINATORS, store_bb, bb);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Create new block %d to sink mask stores.",
			 store_bb->index);
      /* Create vector comparison with boolean result.  */
      vectype = TREE_TYPE (mask);
      zero = build_zero_cst (vectype);
      stmt = gimple_build_cond (EQ_EXPR, mask, zero, NULL_TREE, NULL_TREE);
      gsi = gsi_last_bb (bb);
      gsi_insert_after (&gsi, stmt, GSI_SAME_STMT);
      /* Create new PHI node for vdef of the last masked store:
	 .MEM_2 = VDEF <.MEM_1>
	 will be converted to
	 .MEM.3 = VDEF <.MEM_1>
	 and new PHI node will be created in join bb
	 .MEM_2 = PHI <.MEM_1, .MEM_3>
      */
      vdef = gimple_vdef (last);
      new_vdef = make_ssa_name (gimple_vop (cfun), last);
      gimple_set_vdef (last, new_vdef);
      phi = create_phi_node (vdef, join_bb);
      add_phi_arg (phi, new_vdef, EDGE_SUCC (store_bb, 0), UNKNOWN_LOCATION);

      /* Put all masked stores with the same mask to STORE_BB if possible.  */
      while (true)
	{
	  gimple_stmt_iterator gsi_from;
	  gimple *stmt1 = NULL;

	  /* Move masked store to STORE_BB.  */
	  last_store = last;
	  gsi = gsi_for_stmt (last);
	  gsi_from = gsi;
	  /* Shift GSI to the previous stmt for further traversal.  */
	  gsi_prev (&gsi);
	  gsi_to = gsi_start_bb (store_bb);
	  gsi_move_before (&gsi_from, &gsi_to);
	  /* Setup GSI_TO to the non-empty block start.  */
	  gsi_to = gsi_start_bb (store_bb);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "Move stmt to created bb\n");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, last, 0);
	    }
	  /* Move all stored value producers if possible.  */
	  while (!gsi_end_p (gsi))
	    {
	      tree lhs;
	      imm_use_iterator imm_iter;
	      use_operand_p use_p;
	      bool res;

	      /* Skip debug statements.  */
	      if (is_gimple_debug (gsi_stmt (gsi)))
		{
		  gsi_prev (&gsi);
		  continue;
		}
	      stmt1 = gsi_stmt (gsi);
	      /* Do not consider statements writing to memory or having
		 volatile operand.  */
	      if (gimple_vdef (stmt1)
		  || gimple_has_volatile_ops (stmt1))
		break;
	      gsi_from = gsi;
	      gsi_prev (&gsi);
	      lhs = gimple_get_lhs (stmt1);
	      if (!lhs)
		break;

	      /* LHS of vectorized stmt must be SSA_NAME.  */
	      if (TREE_CODE (lhs) != SSA_NAME)
		break;

	      if (!VECTOR_TYPE_P (TREE_TYPE (lhs)))
		{
		  /* Remove dead scalar statement.  */
		  if (has_zero_uses (lhs))
		    {
		      gsi_remove (&gsi_from, true);
		      continue;
		    }
		}

	      /* Check that LHS does not have uses outside of STORE_BB.  */
	      res = true;
	      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
		{
		  gimple *use_stmt;
		  use_stmt = USE_STMT (use_p);
		  if (is_gimple_debug (use_stmt))
		    continue;
		  if (gimple_bb (use_stmt) != store_bb)
		    {
		      res = false;
		      break;
		    }
		}
	      if (!res)
		break;

	      if (gimple_vuse (stmt1)
		  && gimple_vuse (stmt1) != gimple_vuse (last_store))
		break;

	      /* Can move STMT1 to STORE_BB.  */
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location,
				   "Move stmt to created bb\n");
		  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt1, 0);
		}
	      gsi_move_before (&gsi_from, &gsi_to);
	      /* Shift GSI_TO for further insertion.  */
	      gsi_prev (&gsi_to);
	    }
	  /* Put other masked stores with the same mask to STORE_BB.  */
	  if (worklist.is_empty ()
	      || gimple_call_arg (worklist.last (), 2) != mask
	      || worklist.last () != stmt1)
	    break;
	  last = worklist.pop ();
	}
      add_phi_arg (phi, gimple_vuse (last_store), e, UNKNOWN_LOCATION);
    }
}
