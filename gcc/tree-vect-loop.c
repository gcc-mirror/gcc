/* Loop Vectorization
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software
   Foundation, Inc.
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
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "expr.h"
#include "recog.h"
#include "optabs.h"
#include "params.h"
#include "toplev.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"

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
   ("data-refs"). These two types of data require different handling both
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
   the loop that needs to be vectorized. It inserts the vector code sequence
   just before the scalar stmt S, and records a pointer to the vector code
   in STMT_VINFO_VEC_STMT (stmt_info) (stmt_info is the stmt_vec_info struct
   attached to S). This pointer will be used for the vectorization of following
   stmts which use the def of stmt S. Stmt S is removed if it writes to memory;
   otherwise, we rely on dead code elimination for removing it.

        For example, say stmt S1 was vectorized into stmt VS1:

   VS1: vb = px[i];
   S1:  b = x[i];    STMT_VINFO_VEC_STMT (stmt_info (S1)) = VS1
   S2:  a = b;

   To vectorize stmt S2, the vectorizer first finds the stmt that defines
   the operand 'b' (S1), and gets the relevant vector def 'vb' from the
   vector stmt VS1 pointed to by STMT_VINFO_VEC_STMT (stmt_info (S1)). The
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
   size of the vector (in bytes) - "UNITS_PER_SIMD_WORD". Targets that can
   support different sizes of vectors, for now will need to specify one value
   for "UNITS_PER_SIMD_WORD". More flexibility will be added in the future.

        Since we only vectorize operations which vector form can be
   expressed using existing tree codes, to verify that an operation is
   supported, the vectorizer checks the relevant optab at the relevant
   machine_mode (e.g, optab_handler (add_optab, V8HImode)->insn_code). If
   the value found is CODE_FOR_nothing, then there's no target support, and
   we can't vectorize the stmt.

   For additional information on this project see:
   http://gcc.gnu.org/projects/tree-ssa/vectorization.html
*/

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
		  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
		{
	          fprintf (vect_dump, "not vectorized: irregular stmt.");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}
	      return false;
	    }

	  if (VECTOR_MODE_P (TYPE_MODE (gimple_expr_type (stmt))))
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
	      gcc_assert (!STMT_VINFO_DATA_REF (stmt_info)
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
		  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
        fprintf (vect_dump, "not vectorized: unsupported data-type");
      return false;
    }
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;

  return true;
}


/* Function vect_is_simple_iv_evolution.

   FORNOW: A simple evolution of an induction variables in the loop is
   considered a polynomial evolution with constant step.  */

static bool
vect_is_simple_iv_evolution (unsigned loop_nb, tree access_fn, tree * init,
                             tree * step)
{
  tree init_expr;
  tree step_expr;
  tree evolution_part = evolution_part_in_loop_num (access_fn, loop_nb);

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

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "step: ");
      print_generic_expr (vect_dump, step_expr, TDF_SLIM);
      fprintf (vect_dump, ",  init: ");
      print_generic_expr (vect_dump, init_expr, TDF_SLIM);
    }

  *init = init_expr;
  *step = step_expr;

  if (TREE_CODE (step_expr) != INTEGER_CST)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "step unknown.");
      return false;
    }

  return true;
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
  bool double_reduc;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_scalar_cycles ===");

  /* First - identify all inductions. Reduction detection assumes that all the
     inductions have been identified, therefore, this order must not be
     changed.  */
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


  /* Second - identify all reductions and nested cycles.  */
  while (VEC_length (gimple, worklist) > 0)
    {
      gimple phi = VEC_pop (gimple, worklist);
      tree def = PHI_RESULT (phi);
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (phi);
      gimple reduc_stmt;
      bool nested_cycle;

      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "Analyze phi: ");
          print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
        }

      gcc_assert (is_gimple_reg (SSA_NAME_VAR (def)));
      gcc_assert (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_unknown_def_type);

      nested_cycle = (loop != LOOP_VINFO_LOOP (loop_vinfo));
      reduc_stmt = vect_is_simple_reduction (loop_vinfo, phi, !nested_cycle,
                                             &double_reduc);
      if (reduc_stmt)
        {
          if (double_reduc)
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "Detected double reduction.");

              STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_double_reduction_def;
              STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                    vect_double_reduction_def;
            }
          else
            {
              if (nested_cycle)
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    fprintf (vect_dump, "Detected vectorizable nested cycle.");

                  STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_nested_cycle;
                  STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                             vect_nested_cycle;
                }
              else
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    fprintf (vect_dump, "Detected reduction.");

                  STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_reduction_def;
                  STMT_VINFO_DEF_TYPE (vinfo_for_stmt (reduc_stmt)) =
                                                           vect_reduction_def;
                }
            }
        }
      else
        if (vect_print_dump_info (REPORT_DETAILS))
          fprintf (vect_dump, "Unknown def-use cycle pattern.");
    }

  VEC_free (gimple, heap, worklist);
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
  LOOP_VINFO_LOOP (res) = loop;

  bbs = get_loop_body (loop);

  /* Create/Update stmt_info for all stmts in the loop.  */
  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      /* BBs in a nested inner-loop will have been already processed (because
         we will have called vect_analyze_loop_form for any nested inner-loop).
         Therefore, for stmts in an inner-loop we just want to update the
         STMT_VINFO_LOOP_VINFO field of their stmt_info to point to the new
         loop_info of the outer-loop we are currently considering to vectorize
         (instead of the loop_info of the inner-loop).
         For stmts in other BBs we need to create a stmt_info from scratch.  */
      if (bb->loop_father != loop)
        {
          /* Inner-loop bb.  */
          gcc_assert (loop->inner && bb->loop_father == loop->inner);
          for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
            {
              gimple phi = gsi_stmt (si);
              stmt_vec_info stmt_info = vinfo_for_stmt (phi);
              loop_vec_info inner_loop_vinfo =
                STMT_VINFO_LOOP_VINFO (stmt_info);
              gcc_assert (loop->inner == LOOP_VINFO_LOOP (inner_loop_vinfo));
              STMT_VINFO_LOOP_VINFO (stmt_info) = res;
            }
          for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
           {
              gimple stmt = gsi_stmt (si);
              stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
              loop_vec_info inner_loop_vinfo =
                 STMT_VINFO_LOOP_VINFO (stmt_info);
              gcc_assert (loop->inner == LOOP_VINFO_LOOP (inner_loop_vinfo));
              STMT_VINFO_LOOP_VINFO (stmt_info) = res;
           }
        }
      else
        {
          /* bb in current nest.  */
          for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
            {
              gimple phi = gsi_stmt (si);
              gimple_set_uid (phi, 0);
              set_vinfo_for_stmt (phi, new_stmt_vec_info (phi, res, NULL));
            }

          for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
            {
              gimple stmt = gsi_stmt (si);
              gimple_set_uid (stmt, 0);
              set_vinfo_for_stmt (stmt, new_stmt_vec_info (stmt, res, NULL));
            }
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
  LOOP_VINFO_NITERS (res) = NULL;
  LOOP_VINFO_NITERS_UNCHANGED (res) = NULL;
  LOOP_VINFO_COST_MODEL_MIN_ITERS (res) = 0;
  LOOP_VINFO_VECTORIZABLE_P (res) = 0;
  LOOP_PEELING_FOR_ALIGNMENT (res) = 0;
  LOOP_VINFO_VECT_FACTOR (res) = 0;
  LOOP_VINFO_DATAREFS (res) = VEC_alloc (data_reference_p, heap, 10);
  LOOP_VINFO_DDRS (res) = VEC_alloc (ddr_p, heap, 10 * 10);
  LOOP_VINFO_UNALIGNED_DR (res) = NULL;
  LOOP_VINFO_MAY_MISALIGN_STMTS (res) =
    VEC_alloc (gimple, heap,
               PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIGNMENT_CHECKS));
  LOOP_VINFO_MAY_ALIAS_DDRS (res) =
    VEC_alloc (ddr_p, heap,
               PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS));
  LOOP_VINFO_STRIDED_STORES (res) = VEC_alloc (gimple, heap, 10);
  LOOP_VINFO_SLP_INSTANCES (res) = VEC_alloc (slp_instance, heap, 10);
  LOOP_VINFO_SLP_UNROLLING_FACTOR (res) = 1;

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
  VEC (slp_instance, heap) *slp_instances;
  slp_instance instance;

  if (!loop_vinfo)
    return;

  loop = LOOP_VINFO_LOOP (loop_vinfo);

  bbs = LOOP_VINFO_BBS (loop_vinfo);
  nbbs = loop->num_nodes;

  if (!clean_stmts)
    {
      free (LOOP_VINFO_BBS (loop_vinfo));
      free_data_refs (LOOP_VINFO_DATAREFS (loop_vinfo));
      free_dependence_relations (LOOP_VINFO_DDRS (loop_vinfo));
      VEC_free (gimple, heap, LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo));

      free (loop_vinfo);
      loop->aux = NULL;
      return;
    }

  for (j = 0; j < nbbs; j++)
    {
      basic_block bb = bbs[j];
      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
        free_stmt_vec_info (gsi_stmt (si));

      for (si = gsi_start_bb (bb); !gsi_end_p (si); )
        {
          gimple stmt = gsi_stmt (si);
          stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

          if (stmt_info)
            {
              /* Check if this is a "pattern stmt" (introduced by the
                 vectorizer during the pattern recognition pass).  */
              bool remove_stmt_p = false;
              gimple orig_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
              if (orig_stmt)
                {
                  stmt_vec_info orig_stmt_info = vinfo_for_stmt (orig_stmt);
                  if (orig_stmt_info
                      && STMT_VINFO_IN_PATTERN_P (orig_stmt_info))
                    remove_stmt_p = true;
                }

              /* Free stmt_vec_info.  */
              free_stmt_vec_info (stmt);

              /* Remove dead "pattern stmts".  */
              if (remove_stmt_p)
                gsi_remove (&si, true);
            }
          gsi_next (&si);
        }
    }

  free (LOOP_VINFO_BBS (loop_vinfo));
  free_data_refs (LOOP_VINFO_DATAREFS (loop_vinfo));
  free_dependence_relations (LOOP_VINFO_DDRS (loop_vinfo));
  VEC_free (gimple, heap, LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo));
  VEC_free (ddr_p, heap, LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo));
  slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  for (j = 0; VEC_iterate (slp_instance, slp_instances, j, instance); j++)
    vect_free_slp_instance (instance);

  VEC_free (slp_instance, heap, LOOP_VINFO_SLP_INSTANCES (loop_vinfo));
  VEC_free (gimple, heap, LOOP_VINFO_STRIDED_STORES (loop_vinfo));

  free (loop_vinfo);
  loop->aux = NULL;
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
            fprintf (vect_dump, "not vectorized: control flow in loop.");
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
	    fprintf (vect_dump, "not vectorized: control flow in loop.");
	  destroy_loop_vec_info (inner_loop_vinfo, true);
	  return NULL;
        }

      gcc_assert (EDGE_COUNT (innerloop->header->preds) == 2);
      entryedge = EDGE_PRED (innerloop->header, 0);
      if (EDGE_PRED (innerloop->header, 0)->src == innerloop->latch)
	entryedge = EDGE_PRED (innerloop->header, 1);

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
        || !gimple_seq_empty_p (phi_nodes (loop->latch)))
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
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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


/* Function vect_analyze_loop_operations.

   Scan the loop stmts and make sure they are all vectorizable.  */

static bool
vect_analyze_loop_operations (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  gimple_stmt_iterator si;
  unsigned int vectorization_factor = 0;
  int i;
  gimple phi;
  stmt_vec_info stmt_info;
  bool need_to_vectorize = false;
  int min_profitable_iters;
  int min_scalar_loop_bound;
  unsigned int th;
  bool only_slp_in_loop = true, ok;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_loop_operations ===");

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
                 are not used in the outerloop (unless it is double reduction,
                 i.e., this phi is vect_reduction_def), cause this case
                 requires to actually do something here.  */
              if ((!STMT_VINFO_RELEVANT_P (stmt_info)
                   || STMT_VINFO_LIVE_P (stmt_info))
                  && STMT_VINFO_DEF_TYPE (stmt_info)
                     != vect_double_reduction_def)
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
              if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
                fprintf (vect_dump, "not vectorized: value used after loop.");
              return false;
            }

          if (STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope
              && STMT_VINFO_DEF_TYPE (stmt_info) != vect_induction_def)
            {
              /* A scalar-dependence cycle that we don't support.  */
              if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
              if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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

          gcc_assert (stmt_info);

	  if (!vect_analyze_stmt (stmt, &need_to_vectorize, NULL))
	    return false;

          if ((STMT_VINFO_RELEVANT_P (stmt_info)
               || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (stmt_info)))
              && !PURE_SLP_STMT (stmt_info))

            /* STMT needs both SLP and loop-based vectorization.  */
            only_slp_in_loop = false;
        }
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
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
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
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            fprintf (vect_dump,
                     "not vectorized: can't create epilog loop 1.");
          return false;
        }
      if (!slpeel_can_duplicate_loop_p (loop, single_exit (loop)))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            fprintf (vect_dump,
                     "not vectorized: can't create epilog loop 2.");
          return false;
        }
    }

  return true;
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

  ok = vect_analyze_data_refs (loop_vinfo, NULL);
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

  ok = vect_analyze_data_refs_alignment (loop_vinfo, NULL);
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

  ok = vect_analyze_data_ref_dependences (loop_vinfo, NULL);
  if (!ok)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data dependence.");
      destroy_loop_vec_info (loop_vinfo, true);
      return NULL;
    }

  /* Analyze the access patterns of the data-refs in the loop (consecutive,
     complex, etc.). FORNOW: Only handle consecutive access pattern.  */

  ok = vect_analyze_data_ref_accesses (loop_vinfo, NULL);
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
  ok = vect_analyze_slp (loop_vinfo, NULL);
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

  ok = vect_analyze_loop_operations (loop_vinfo);
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


/* Function reduction_code_for_scalar_code

   Input:
   CODE - tree_code of a reduction operations.

   Output:
   REDUC_CODE - the corresponding tree-code to be used to reduce the
      vector of partial results into a single scalar result (which
      will also reside in a vector) or ERROR_MARK if the operation is
      a supported reduction operation, but does not have such tree-code.

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


/* Error reporting helper for vect_is_simple_reduction below. GIMPLE statement
   STMT is printed with a message MSG. */

static void
report_vect_op (gimple stmt, const char *msg)
{
  fprintf (vect_dump, "%s", msg);
  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
}


/* Function vect_is_simple_reduction

   (1) Detect a cross-iteration def-use cycle that represents a simple
   reduction computation. We look for the following pattern:

   loop_header:
     a1 = phi < a0, a2 >
     a3 = ...
     a2 = operation (a3, a1)

   such that:
   1. operation is commutative and associative and it is safe to
      change the order of the computation (if CHECK_REDUCTION is true)
   2. no uses for a2 in the loop (a2 is used out of the loop)
   3. no uses of a1 in the loop besides the reduction operation.

   Condition 1 is tested here.
   Conditions 2,3 are tested in vect_mark_stmts_to_be_vectorized.

   (2) Detect a cross-iteration def-use cycle in nested loops, i.e.,
   nested cycles, if CHECK_REDUCTION is false.

   (3) Detect cycles of phi nodes in outer-loop vectorization, i.e., double
   reductions:

     a1 = phi < a0, a2 >
     inner loop (def of a3)
     a2 = phi < a3 >
*/

gimple
vect_is_simple_reduction (loop_vec_info loop_info, gimple phi,
                          bool check_reduction, bool *double_reduc)
{
  struct loop *loop = (gimple_bb (phi))->loop_father;
  struct loop *vect_loop = LOOP_VINFO_LOOP (loop_info);
  edge latch_e = loop_latch_edge (loop);
  tree loop_arg = PHI_ARG_DEF_FROM_EDGE (phi, latch_e);
  gimple def_stmt, def1 = NULL, def2 = NULL;
  enum tree_code code;
  tree op1, op2, op3 = NULL_TREE, op4 = NULL_TREE;
  tree type;
  int nloop_uses;
  tree name;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  bool phi_def;

  *double_reduc = false;

  /* If CHECK_REDUCTION is true, we assume inner-most loop vectorization,
     otherwise, we assume outer loop vectorization.  */
  gcc_assert ((check_reduction && loop == vect_loop)
              || (!check_reduction && flow_loop_nested_p (vect_loop, loop)));

  name = PHI_RESULT (phi);
  nloop_uses = 0;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, name)
    {
      gimple use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;
      if (flow_bb_inside_loop_p (loop, gimple_bb (use_stmt))
	  && vinfo_for_stmt (use_stmt)
	  && !is_pattern_stmt_p (vinfo_for_stmt (use_stmt)))
        nloop_uses++;
      if (nloop_uses > 1)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "reduction used in loop.");
          return NULL;
        }
    }

  if (TREE_CODE (loop_arg) != SSA_NAME)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	{
	  fprintf (vect_dump, "reduction: not ssa_name: ");
	  print_generic_expr (vect_dump, loop_arg, TDF_SLIM);
	}
      return NULL;
    }

  def_stmt = SSA_NAME_DEF_STMT (loop_arg);
  if (!def_stmt)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "reduction: no def_stmt.");
      return NULL;
    }

  if (!is_gimple_assign (def_stmt) && gimple_code (def_stmt) != GIMPLE_PHI)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        print_gimple_stmt (vect_dump, def_stmt, 0, TDF_SLIM);
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
      gimple use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;
      if (flow_bb_inside_loop_p (loop, gimple_bb (use_stmt))
	  && vinfo_for_stmt (use_stmt)
	  && !is_pattern_stmt_p (vinfo_for_stmt (use_stmt)))
	nloop_uses++;
      if (nloop_uses > 1)
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "reduction used in loop.");
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
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "unsupported phi node definition.");

          return NULL;
        }

      def1 = SSA_NAME_DEF_STMT (op1);
      if (flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
          && loop->inner
          && flow_bb_inside_loop_p (loop->inner, gimple_bb (def1))
          && is_gimple_assign (def1))
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            report_vect_op (def_stmt, "detected double reduction: ");

          *double_reduc = true;
          return def_stmt;
        }

      return NULL;
    }

  code = gimple_assign_rhs_code (def_stmt);

  if (check_reduction
      && (!commutative_tree_code (code) || !associative_tree_code (code)))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        report_vect_op (def_stmt, "reduction: not commutative/associative: ");
      return NULL;
    }

  if (get_gimple_rhs_class (code) != GIMPLE_BINARY_RHS)
    {
      if (code != COND_EXPR)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
	    report_vect_op (def_stmt, "reduction: not binary operation: ");

          return NULL;
        }

      op3 = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 0);
      if (COMPARISON_CLASS_P (op3))
        {
          op4 = TREE_OPERAND (op3, 1);
          op3 = TREE_OPERAND (op3, 0);
        }

      op1 = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 1);
      op2 = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 2);

      if (TREE_CODE (op1) != SSA_NAME && TREE_CODE (op2) != SSA_NAME)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            report_vect_op (def_stmt, "reduction: uses not ssa_names: ");

          return NULL;
        }
    }
  else
    {
      op1 = gimple_assign_rhs1 (def_stmt);
      op2 = gimple_assign_rhs2 (def_stmt);

      if (TREE_CODE (op1) != SSA_NAME || TREE_CODE (op2) != SSA_NAME)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
	    report_vect_op (def_stmt, "reduction: uses not ssa_names: ");

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
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "reduction: multiple types: operation type: ");
          print_generic_expr (vect_dump, type, TDF_SLIM);
          fprintf (vect_dump, ", operands types: ");
          print_generic_expr (vect_dump, TREE_TYPE (op1), TDF_SLIM);
          fprintf (vect_dump, ",");
          print_generic_expr (vect_dump, TREE_TYPE (op2), TDF_SLIM);
          if (op3)
            {
              fprintf (vect_dump, ",");
              print_generic_expr (vect_dump, TREE_TYPE (op3), TDF_SLIM);
            }

          if (op4)
            {
              fprintf (vect_dump, ",");
              print_generic_expr (vect_dump, TREE_TYPE (op4), TDF_SLIM);
            }
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

  /* CHECKME: check for !flag_finite_math_only too?  */
  if (SCALAR_FLOAT_TYPE_P (type) && !flag_associative_math
      && check_reduction)
    {
      /* Changing the order of operations changes the semantics.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: unsafe fp math optimization: ");
      return NULL;
    }
  else if (INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_TRAPS (type)
	   && check_reduction)
    {
      /* Changing the order of operations changes the semantics.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: unsafe int math optimization: ");
      return NULL;
    }
  else if (SAT_FIXED_POINT_TYPE_P (type) && check_reduction)
    {
      /* Changing the order of operations changes the semantics.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt,
			"reduction: unsafe fixed-point math optimization: ");
      return NULL;
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
      && (!def1 || !def2 || gimple_nop_p (def1) || gimple_nop_p (def2)))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: no defs for operands: ");
      return NULL;
    }

  /* Check that one def is the reduction def, defined by PHI,
     the other def is either defined in the loop ("vect_internal_def"),
     or it's an induction (defined by a loop-header phi-node).  */

  if (def2 && def2 == phi
      && (code == COND_EXPR
          || (def1 && flow_bb_inside_loop_p (loop, gimple_bb (def1))
              && (is_gimple_assign (def1)
  	          || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def1))
                      == vect_induction_def
   	          || (gimple_code (def1) == GIMPLE_PHI
	              && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def1))
                          == vect_internal_def
 	              && !is_loop_header_bb_p (gimple_bb (def1)))))))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "detected reduction: ");
      return def_stmt;
    }
  else if (def1 && def1 == phi
	   && (code == COND_EXPR
               || (def2 && flow_bb_inside_loop_p (loop, gimple_bb (def2))
 	           && (is_gimple_assign (def2)
	               || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def2))
                           == vect_induction_def
 	               || (gimple_code (def2) == GIMPLE_PHI
		           && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def2))
                               == vect_internal_def
		           && !is_loop_header_bb_p (gimple_bb (def2)))))))
    {
      if (check_reduction)
        {
          /* Swap operands (just for simplicity - so that the rest of the code
	     can assume that the reduction variable is always the last (second)
	     argument).  */
          if (vect_print_dump_info (REPORT_DETAILS))
	    report_vect_op (def_stmt,
	  	            "detected reduction: need to swap operands: ");

          swap_tree_operands (def_stmt, gimple_assign_rhs1_ptr (def_stmt),
 			      gimple_assign_rhs2_ptr (def_stmt));
        }
      else
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            report_vect_op (def_stmt, "detected reduction: ");
        }

      return def_stmt;
    }
  else
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: unknown pattern: ");

      return NULL;
    }
}


/* Function vect_estimate_min_profitable_iters

   Return the number of iterations required for the vector version of the
   loop to be profitable relative to the cost of the scalar version of the
   loop.

   TODO: Take profile info into account before making vectorization
   decisions, if available.  */

int
vect_estimate_min_profitable_iters (loop_vec_info loop_vinfo)
{
  int i;
  int min_profitable_iters;
  int peel_iters_prologue;
  int peel_iters_epilogue;
  int vec_inside_cost = 0;
  int vec_outside_cost = 0;
  int scalar_single_iter_cost = 0;
  int scalar_outside_cost = 0;
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  int byte_misalign = LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo);
  int peel_guard_costs = 0;
  int innerloop_iters = 0, factor;
  VEC (slp_instance, heap) *slp_instances;
  slp_instance instance;

  /* Cost model disabled.  */
  if (!flag_vect_cost_model)
    {
      if (vect_print_dump_info (REPORT_COST))
        fprintf (vect_dump, "cost model disabled.");
      return 0;
    }

  /* Requires loop versioning tests to handle misalignment.  */
  if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo))
    {
      /*  FIXME: Make cost depend on complexity of individual check.  */
      vec_outside_cost +=
	VEC_length (gimple, LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo));
      if (vect_print_dump_info (REPORT_COST))
        fprintf (vect_dump, "cost model: Adding cost of checks for loop "
                 "versioning to treat misalignment.\n");
    }

  /* Requires loop versioning with alias checks.  */
  if (LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    {
      /*  FIXME: Make cost depend on complexity of individual check.  */
      vec_outside_cost +=
        VEC_length (ddr_p, LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo));
      if (vect_print_dump_info (REPORT_COST))
        fprintf (vect_dump, "cost model: Adding cost of checks for loop "
                 "versioning aliasing.\n");
    }

  if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    vec_outside_cost += TARG_COND_TAKEN_BRANCH_COST;

  /* Count statements in scalar loop.  Using this as scalar cost for a single
     iteration for now.

     TODO: Add outer loop support.

     TODO: Consider assigning different costs to different scalar
     statements.  */

  /* FORNOW.  */
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
	  gimple stmt = gsi_stmt (si);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  /* Skip stmts that are not vectorized inside the loop.  */
	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && (!STMT_VINFO_LIVE_P (stmt_info)
		  || STMT_VINFO_DEF_TYPE (stmt_info) != vect_reduction_def))
	    continue;
	  scalar_single_iter_cost += cost_for_stmt (stmt) * factor;
	  vec_inside_cost += STMT_VINFO_INSIDE_OF_LOOP_COST (stmt_info) * factor;
	  /* FIXME: for stmts in the inner-loop in outer-loop vectorization,
	     some of the "outside" costs are generated inside the outer-loop.  */
	  vec_outside_cost += STMT_VINFO_OUTSIDE_OF_LOOP_COST (stmt_info);
	}
    }

  /* Add additional cost for the peeled instructions in prologue and epilogue
     loop.

     FORNOW: If we don't know the value of peel_iters for prologue or epilogue
     at compile-time - we assume it's vf/2 (the worst would be vf-1).

     TODO: Build an expression that represents peel_iters for prologue and
     epilogue to be used in a run-time test.  */

  if (byte_misalign < 0)
    {
      peel_iters_prologue = vf/2;
      if (vect_print_dump_info (REPORT_COST))
        fprintf (vect_dump, "cost model: "
                 "prologue peel iters set to vf/2.");

      /* If peeling for alignment is unknown, loop bound of main loop becomes
         unknown.  */
      peel_iters_epilogue = vf/2;
      if (vect_print_dump_info (REPORT_COST))
        fprintf (vect_dump, "cost model: "
                 "epilogue peel iters set to vf/2 because "
                 "peeling for alignment is unknown .");

      /* If peeled iterations are unknown, count a taken branch and a not taken
         branch per peeled loop. Even if scalar loop iterations are known,
         vector iterations are not known since peeled prologue iterations are
         not known. Hence guards remain the same.  */
      peel_guard_costs +=  2 * (TARG_COND_TAKEN_BRANCH_COST
                              + TARG_COND_NOT_TAKEN_BRANCH_COST);
    }
  else
    {
      if (byte_misalign)
	{
	  struct data_reference *dr = LOOP_VINFO_UNALIGNED_DR (loop_vinfo);
	  int element_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr))));
	  tree vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (DR_STMT (dr)));
	  int nelements = TYPE_VECTOR_SUBPARTS (vectype);

	  peel_iters_prologue = nelements - (byte_misalign / element_size);
	}
      else
	peel_iters_prologue = 0;

      if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
        {
          peel_iters_epilogue = vf/2;
          if (vect_print_dump_info (REPORT_COST))
            fprintf (vect_dump, "cost model: "
                     "epilogue peel iters set to vf/2 because "
                     "loop iterations are unknown .");

	  /* If peeled iterations are known but number of scalar loop
	     iterations are unknown, count a taken branch per peeled loop.  */
	  peel_guard_costs +=  2 * TARG_COND_TAKEN_BRANCH_COST;

        }
      else
	{
	  int niters = LOOP_VINFO_INT_NITERS (loop_vinfo);
	  peel_iters_prologue = niters < peel_iters_prologue ?
					niters : peel_iters_prologue;
	  peel_iters_epilogue = (niters - peel_iters_prologue) % vf;
	}
    }

  vec_outside_cost += (peel_iters_prologue * scalar_single_iter_cost)
                      + (peel_iters_epilogue * scalar_single_iter_cost)
                      + peel_guard_costs;

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
     decide whether to vectorize at compile time. Hence the scalar version
     do not carry cost model guard costs.  */
  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    {
      /* Cost model check occurs at versioning.  */
      if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
          || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
	scalar_outside_cost += TARG_COND_NOT_TAKEN_BRANCH_COST;
      else
	{
	  /* Cost model check occurs at prologue generation.  */
	  if (LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo) < 0)
	    scalar_outside_cost += 2 * TARG_COND_TAKEN_BRANCH_COST
	      + TARG_COND_NOT_TAKEN_BRANCH_COST;
	  /* Cost model check occurs at epilogue generation.  */
	  else
	    scalar_outside_cost += 2 * TARG_COND_TAKEN_BRANCH_COST;
	}
    }

  /* Add SLP costs.  */
  slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  for (i = 0; VEC_iterate (slp_instance, slp_instances, i, instance); i++)
    {
      vec_outside_cost += SLP_INSTANCE_OUTSIDE_OF_LOOP_COST (instance);
      vec_inside_cost += SLP_INSTANCE_INSIDE_OF_LOOP_COST (instance);
    }

  /* Calculate number of iterations required to make the vector version
     profitable, relative to the loop bodies only. The following condition
     must hold true:
     SIC * niters + SOC > VIC * ((niters-PL_ITERS-EP_ITERS)/VF) + VOC
     where
     SIC = scalar iteration cost, VIC = vector iteration cost,
     VOC = vector outside cost, VF = vectorization factor,
     PL_ITERS = prologue iterations, EP_ITERS= epilogue iterations
     SOC = scalar outside cost for run time cost model check.  */

  if ((scalar_single_iter_cost * vf) > vec_inside_cost)
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
              <= ((vec_inside_cost * min_profitable_iters)
                  + ((vec_outside_cost - scalar_outside_cost) * vf)))
            min_profitable_iters++;
        }
    }
  /* vector version will never be profitable.  */
  else
    {
      if (vect_print_dump_info (REPORT_COST))
        fprintf (vect_dump, "cost model: vector iteration cost = %d "
                 "is divisible by scalar iteration cost = %d by a factor "
                 "greater than or equal to the vectorization factor = %d .",
                 vec_inside_cost, scalar_single_iter_cost, vf);
      return -1;
    }

  if (vect_print_dump_info (REPORT_COST))
    {
      fprintf (vect_dump, "Cost model analysis: \n");
      fprintf (vect_dump, "  Vector inside of loop cost: %d\n",
	       vec_inside_cost);
      fprintf (vect_dump, "  Vector outside of loop cost: %d\n",
	       vec_outside_cost);
      fprintf (vect_dump, "  Scalar iteration cost: %d\n",
	       scalar_single_iter_cost);
      fprintf (vect_dump, "  Scalar outside cost: %d\n", scalar_outside_cost);
      fprintf (vect_dump, "  prologue iterations: %d\n",
               peel_iters_prologue);
      fprintf (vect_dump, "  epilogue iterations: %d\n",
               peel_iters_epilogue);
      fprintf (vect_dump, "  Calculated minimum iters for profitability: %d\n",
	       min_profitable_iters);
    }

  min_profitable_iters =
	min_profitable_iters < vf ? vf : min_profitable_iters;

  /* Because the condition we create is:
     if (niters <= min_profitable_iters)
       then skip the vectorized loop.  */
  min_profitable_iters--;

  if (vect_print_dump_info (REPORT_COST))
    fprintf (vect_dump, "  Profitability threshold = %d\n",
	     min_profitable_iters);

  return min_profitable_iters;
}


/* TODO: Close dependency between vect_model_*_cost and vectorizable_*
   functions. Design better to avoid maintenance issues.  */

/* Function vect_model_reduction_cost.

   Models cost for a reduction operation, including the vector ops
   generated within the strip-mine loop, the initial definition before
   the loop, and the epilogue code that must be generated.  */

static bool
vect_model_reduction_cost (stmt_vec_info stmt_info, enum tree_code reduc_code,
			   int ncopies)
{
  int outer_cost = 0;
  enum tree_code code;
  optab optab;
  tree vectype;
  gimple stmt, orig_stmt;
  tree reduction_op;
  enum machine_mode mode;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);


  /* Cost of reduction op inside loop.  */
  STMT_VINFO_INSIDE_OF_LOOP_COST (stmt_info) += ncopies * TARG_VEC_STMT_COST;

  stmt = STMT_VINFO_STMT (stmt_info);

  switch (get_gimple_rhs_class (gimple_assign_rhs_code (stmt)))
    {
    case GIMPLE_SINGLE_RHS:
      gcc_assert (TREE_OPERAND_LENGTH (gimple_assign_rhs1 (stmt)) == ternary_op);
      reduction_op = TREE_OPERAND (gimple_assign_rhs1 (stmt), 2);
      break;
    case GIMPLE_UNARY_RHS:
      reduction_op = gimple_assign_rhs1 (stmt);
      break;
    case GIMPLE_BINARY_RHS:
      reduction_op = gimple_assign_rhs2 (stmt);
      break;
    default:
      gcc_unreachable ();
    }

  vectype = get_vectype_for_scalar_type (TREE_TYPE (reduction_op));
  if (!vectype)
    {
      if (vect_print_dump_info (REPORT_COST))
        {
          fprintf (vect_dump, "unsupported data-type ");
          print_generic_expr (vect_dump, TREE_TYPE (reduction_op), TDF_SLIM);
        }
      return false;
   }

  mode = TYPE_MODE (vectype);
  orig_stmt = STMT_VINFO_RELATED_STMT (stmt_info);

  if (!orig_stmt)
    orig_stmt = STMT_VINFO_STMT (stmt_info);

  code = gimple_assign_rhs_code (orig_stmt);

  /* Add in cost for initial definition.  */
  outer_cost += TARG_SCALAR_TO_VEC_COST;

  /* Determine cost of epilogue code.

     We have a reduction operator that will reduce the vector in one statement.
     Also requires scalar extract.  */

  if (!nested_in_vect_loop_p (loop, orig_stmt))
    {
      if (reduc_code != ERROR_MARK)
	outer_cost += TARG_VEC_STMT_COST + TARG_VEC_TO_SCALAR_COST;
      else
	{
	  int vec_size_in_bits = tree_low_cst (TYPE_SIZE (vectype), 1);
	  tree bitsize =
	    TYPE_SIZE (TREE_TYPE (gimple_assign_lhs (orig_stmt)));
	  int element_bitsize = tree_low_cst (bitsize, 1);
	  int nelements = vec_size_in_bits / element_bitsize;

	  optab = optab_for_tree_code (code, vectype, optab_default);

	  /* We have a whole vector shift available.  */
	  if (VECTOR_MODE_P (mode)
	      && optab_handler (optab, mode)->insn_code != CODE_FOR_nothing
	      && optab_handler (vec_shr_optab, mode)->insn_code != CODE_FOR_nothing)
	    /* Final reduction via vector shifts and the reduction operator. Also
	       requires scalar extract.  */
	    outer_cost += ((exact_log2(nelements) * 2) * TARG_VEC_STMT_COST
				+ TARG_VEC_TO_SCALAR_COST);
	  else
	    /* Use extracts and reduction op for final reduction.  For N elements,
               we have N extracts and N-1 reduction ops.  */
	    outer_cost += ((nelements + nelements - 1) * TARG_VEC_STMT_COST);
	}
    }

  STMT_VINFO_OUTSIDE_OF_LOOP_COST (stmt_info) = outer_cost;

  if (vect_print_dump_info (REPORT_COST))
    fprintf (vect_dump, "vect_model_reduction_cost: inside_cost = %d, "
             "outside_cost = %d .", STMT_VINFO_INSIDE_OF_LOOP_COST (stmt_info),
             STMT_VINFO_OUTSIDE_OF_LOOP_COST (stmt_info));

  return true;
}


/* Function vect_model_induction_cost.

   Models cost for induction operations.  */

static void
vect_model_induction_cost (stmt_vec_info stmt_info, int ncopies)
{
  /* loop cost for vec_loop.  */
  STMT_VINFO_INSIDE_OF_LOOP_COST (stmt_info) = ncopies * TARG_VEC_STMT_COST;
  /* prologue cost for vec_init and vec_step.  */
  STMT_VINFO_OUTSIDE_OF_LOOP_COST (stmt_info) = 2 * TARG_SCALAR_TO_VEC_COST;

  if (vect_print_dump_info (REPORT_COST))
    fprintf (vect_dump, "vect_model_induction_cost: inside_cost = %d, "
             "outside_cost = %d .", STMT_VINFO_INSIDE_OF_LOOP_COST (stmt_info),
             STMT_VINFO_OUTSIDE_OF_LOOP_COST (stmt_info));
}


/* Function get_initial_def_for_induction

   Input:
   STMT - a stmt that performs an induction operation in the loop.
   IV_PHI - the initial value of the induction variable

   Output:
   Return a vector variable, initialized with the first VF values of
   the induction variable. E.g., for an iv with IV_PHI='X' and
   evolution S, for a vector of 4 units, we want to return:
   [X, X + S, X + 2*S, X + 3*S].  */

static tree
get_initial_def_for_induction (gimple iv_phi)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (iv_phi);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree scalar_type = TREE_TYPE (gimple_phi_result (iv_phi));
  tree vectype;
  int nunits;
  edge pe = loop_preheader_edge (loop);
  struct loop *iv_loop;
  basic_block new_bb;
  tree vec, vec_init, vec_step, t;
  tree access_fn;
  tree new_var;
  tree new_name;
  gimple init_stmt, induction_phi, new_stmt;
  tree induc_def, vec_def, vec_dest;
  tree init_expr, step_expr;
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  int i;
  bool ok;
  int ncopies;
  tree expr;
  stmt_vec_info phi_info = vinfo_for_stmt (iv_phi);
  bool nested_in_vect_loop = false;
  gimple_seq stmts = NULL;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  gimple exit_phi;
  edge latch_e;
  tree loop_arg;
  gimple_stmt_iterator si;
  basic_block bb = gimple_bb (iv_phi);
  tree stepvectype;

  vectype = get_vectype_for_scalar_type (scalar_type);
  gcc_assert (vectype);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  ncopies = vf / nunits;

  gcc_assert (phi_info);
  gcc_assert (ncopies >= 1);

  /* Find the first insertion point in the BB.  */
  si = gsi_after_labels (bb);

  if (INTEGRAL_TYPE_P (scalar_type))
    step_expr = build_int_cst (scalar_type, 0);
  else if (POINTER_TYPE_P (scalar_type))
    step_expr = build_int_cst (sizetype, 0);
  else
    step_expr = build_real (scalar_type, dconst0);

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

  access_fn = analyze_scalar_evolution (iv_loop, PHI_RESULT (iv_phi));
  gcc_assert (access_fn);
  ok = vect_is_simple_iv_evolution (iv_loop->num, access_fn,
                                    &init_expr, &step_expr);
  gcc_assert (ok);
  pe = loop_preheader_edge (iv_loop);

  /* Create the vector that holds the initial_value of the induction.  */
  if (nested_in_vect_loop)
    {
      /* iv_loop is nested in the loop to be vectorized.  init_expr had already
	 been created during vectorization of previous stmts; We obtain it from
	 the STMT_VINFO_VEC_STMT of the defining stmt. */
      tree iv_def = PHI_ARG_DEF_FROM_EDGE (iv_phi,
                                           loop_preheader_edge (iv_loop));
      vec_init = vect_get_vec_def_for_operand (iv_def, iv_phi, NULL);
    }
  else
    {
      /* iv_loop is the loop to be vectorized. Create:
	 vec_init = [X, X+S, X+2*S, X+3*S] (S = step_expr, X = init_expr)  */
      new_var = vect_get_new_vect_var (scalar_type, vect_scalar_var, "var_");
      add_referenced_var (new_var);

      new_name = force_gimple_operand (init_expr, &stmts, false, new_var);
      if (stmts)
	{
	  new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
	  gcc_assert (!new_bb);
	}

      t = NULL_TREE;
      t = tree_cons (NULL_TREE, init_expr, t);
      for (i = 1; i < nunits; i++)
	{
	  /* Create: new_name_i = new_name + step_expr  */
	  enum tree_code code = POINTER_TYPE_P (scalar_type)
				? POINTER_PLUS_EXPR : PLUS_EXPR;
	  init_stmt = gimple_build_assign_with_ops (code, new_var,
						    new_name, step_expr);
	  new_name = make_ssa_name (new_var, init_stmt);
	  gimple_assign_set_lhs (init_stmt, new_name);

	  new_bb = gsi_insert_on_edge_immediate (pe, init_stmt);
	  gcc_assert (!new_bb);

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "created new init_stmt: ");
	      print_gimple_stmt (vect_dump, init_stmt, 0, TDF_SLIM);
	    }
	  t = tree_cons (NULL_TREE, new_name, t);
	}
      /* Create a vector from [new_name_0, new_name_1, ..., new_name_nunits-1]  */
      vec = build_constructor_from_list (vectype, nreverse (t));
      vec_init = vect_init_vector (iv_phi, vec, vectype, NULL);
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
      expr = build_int_cst (TREE_TYPE (step_expr), vf);
      new_name = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
			      expr, step_expr);
    }

  t = NULL_TREE;
  for (i = 0; i < nunits; i++)
    t = tree_cons (NULL_TREE, unshare_expr (new_name), t);
  gcc_assert (CONSTANT_CLASS_P (new_name));
  stepvectype = get_vectype_for_scalar_type (TREE_TYPE (new_name));
  gcc_assert (stepvectype);
  vec = build_vector (stepvectype, t);
  vec_step = vect_init_vector (iv_phi, vec, stepvectype, NULL);


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
  add_referenced_var (vec_dest);
  induction_phi = create_phi_node (vec_dest, iv_loop->header);
  set_vinfo_for_stmt (induction_phi,
		      new_stmt_vec_info (induction_phi, loop_vinfo, NULL));
  induc_def = PHI_RESULT (induction_phi);

  /* Create the iv update inside the loop  */
  new_stmt = gimple_build_assign_with_ops (PLUS_EXPR, vec_dest,
					   induc_def, vec_step);
  vec_def = make_ssa_name (vec_dest, new_stmt);
  gimple_assign_set_lhs (new_stmt, vec_def);
  gsi_insert_before (&si, new_stmt, GSI_SAME_STMT);
  set_vinfo_for_stmt (new_stmt, new_stmt_vec_info (new_stmt, loop_vinfo,
                                                   NULL));

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
      expr = build_int_cst (TREE_TYPE (step_expr), nunits);
      new_name = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
			      expr, step_expr);
      t = NULL_TREE;
      for (i = 0; i < nunits; i++)
	t = tree_cons (NULL_TREE, unshare_expr (new_name), t);
      gcc_assert (CONSTANT_CLASS_P (new_name));
      vec = build_vector (stepvectype, t);
      vec_step = vect_init_vector (iv_phi, vec, stepvectype, NULL);

      vec_def = induc_def;
      prev_stmt_vinfo = vinfo_for_stmt (induction_phi);
      for (i = 1; i < ncopies; i++)
	{
	  /* vec_i = vec_prev + vec_step  */
	  new_stmt = gimple_build_assign_with_ops (PLUS_EXPR, vec_dest,
						   vec_def, vec_step);
	  vec_def = make_ssa_name (vec_dest, new_stmt);
	  gimple_assign_set_lhs (new_stmt, vec_def);

	  gsi_insert_before (&si, new_stmt, GSI_SAME_STMT);
	  set_vinfo_for_stmt (new_stmt,
			      new_stmt_vec_info (new_stmt, loop_vinfo, NULL));
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
	  if (!flow_bb_inside_loop_p (iv_loop, gimple_bb (USE_STMT (use_p))))
	    {
	      exit_phi = USE_STMT (use_p);
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
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "vector of inductions after inner-loop:");
	      print_gimple_stmt (vect_dump, new_stmt, 0, TDF_SLIM);
	    }
	}
    }


  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "transform induction: created def-use cycle: ");
      print_gimple_stmt (vect_dump, induction_phi, 0, TDF_SLIM);
      fprintf (vect_dump, "\n");
      print_gimple_stmt (vect_dump, SSA_NAME_DEF_STMT (vec_def), 0, TDF_SLIM);
    }

  STMT_VINFO_VEC_STMT (phi_info) = induction_phi;
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
get_initial_def_for_reduction (gimple stmt, tree init_val,
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
  tree t = NULL_TREE;
  int i;
  bool nested_in_vect_loop = false;
  tree init_value;
  REAL_VALUE_TYPE real_init_val = dconst0;
  int int_init_val = 0;
  gimple def_stmt = NULL;

  gcc_assert (vectype);
  nunits = TYPE_VECTOR_SUBPARTS (vectype);

  gcc_assert (POINTER_TYPE_P (scalar_type) || INTEGRAL_TYPE_P (scalar_type)
	      || SCALAR_FLOAT_TYPE_P (scalar_type));

  if (nested_in_vect_loop_p (loop, stmt))
    nested_in_vect_loop = true;
  else
    gcc_assert (loop == (gimple_bb (stmt))->loop_father);

  /* In case of double reduction we only create a vector variable to be put
     in the reduction phi node. The actual statement creation is done in
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

  if (TREE_CONSTANT (init_val))
    {
      if (SCALAR_FLOAT_TYPE_P (scalar_type))
        init_value = build_real (scalar_type, TREE_REAL_CST (init_val));
      else
        init_value = build_int_cst (scalar_type, TREE_INT_CST_LOW (init_val));
    }
  else
    init_value = init_val;

  switch (code)
    {
      case WIDEN_SUM_EXPR:
      case DOT_PROD_EXPR:
      case PLUS_EXPR:
      case MINUS_EXPR:
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
      case MULT_EXPR:
      case BIT_AND_EXPR:
        /* ADJUSMENT_DEF is NULL when called from
           vect_create_epilog_for_reduction to vectorize double reduction.  */
        if (adjustment_def)
          {
            if (nested_in_vect_loop)
              *adjustment_def = vect_get_vec_def_for_operand (init_val, stmt,
                                                              NULL);
            else
              *adjustment_def = init_val;
          }

        if (code == MULT_EXPR || code == BIT_AND_EXPR)
          {
            real_init_val = dconst1;
            int_init_val = 1;
          }

        if (SCALAR_FLOAT_TYPE_P (scalar_type))
          def_for_init = build_real (scalar_type, real_init_val);
        else
          def_for_init = build_int_cst (scalar_type, int_init_val);

        /* Create a vector of '0' or '1' except the first element.  */
        for (i = nunits - 2; i >= 0; --i)
          t = tree_cons (NULL_TREE, def_for_init, t);

        /* Option1: the first element is '0' or '1' as well.  */
        if (adjustment_def)
          {
            t = tree_cons (NULL_TREE, def_for_init, t);
            init_def = build_vector (vectype, t);
            break;
          }

        /* Option2: the first element is INIT_VAL.  */
        t = tree_cons (NULL_TREE, init_value, t);
        if (TREE_CONSTANT (init_val))
          init_def = build_vector (vectype, t);
        else
          init_def = build_constructor_from_list (vectype, t);

        break;

      case MIN_EXPR:
      case MAX_EXPR:
      case COND_EXPR:
        if (adjustment_def)
          {
            *adjustment_def = NULL_TREE;
            init_def = vect_get_vec_def_for_operand (init_val, stmt, NULL);
            break;
          }

        for (i = nunits - 1; i >= 0; --i)
          t = tree_cons (NULL_TREE, init_value, t);

        if (TREE_CONSTANT (init_val))
          init_def = build_vector (vectype, t);
        else
          init_def = build_constructor_from_list (vectype, t);

        break;

      default:
        gcc_unreachable ();
    }

  return init_def;
}


/* Function vect_create_epilog_for_reduction

   Create code at the loop-epilog to finalize the result of a reduction
   computation.

   VECT_DEF is a vector of partial results.
   REDUC_CODE is the tree-code for the epilog reduction.
   NCOPIES is > 1 in case the vectorization factor (VF) is bigger than the
     number of elements that we can fit in a vectype (nunits). In this case
     we have to generate more than one vector stmt - i.e - we need to "unroll"
     the vector stmt by a factor VF/nunits.  For more details see documentation
     in vectorizable_operation.
   STMT is the scalar reduction stmt that is being vectorized.
   REDUCTION_PHI is the phi-node that carries the reduction computation.
   REDUC_INDEX is the index of the operand in the right hand side of the
     statement that is defined by REDUCTION_PHI.
   DOUBLE_REDUC is TRUE if double reduction phi nodes should be handled.

   This function:
   1. Creates the reduction def-use cycle: sets the arguments for
      REDUCTION_PHI:
      The loop-entry argument is the vectorized initial-value of the reduction.
      The loop-latch argument is VECT_DEF - the vector of partial sums.
   2. "Reduces" the vector of partial results VECT_DEF into a single result,
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
vect_create_epilog_for_reduction (tree vect_def, gimple stmt,
				  int ncopies,
				  enum tree_code reduc_code,
				  gimple reduction_phi,
                                  int reduc_index,
                                  bool double_reduc)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  stmt_vec_info prev_phi_info;
  tree vectype;
  enum machine_mode mode;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo), *outer_loop = NULL;
  basic_block exit_bb;
  tree scalar_dest;
  tree scalar_type;
  gimple new_phi = NULL, phi;
  gimple_stmt_iterator exit_gsi;
  tree vec_dest;
  tree new_temp = NULL_TREE;
  tree new_name;
  gimple epilog_stmt = NULL;
  tree new_scalar_dest, new_dest;
  gimple exit_phi;
  tree bitsize, bitpos;
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree adjustment_def;
  tree vec_initial_def, def;
  tree orig_name;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  bool extract_scalar_result = false;
  tree reduction_op, expr;
  gimple orig_stmt;
  gimple use_stmt;
  bool nested_in_vect_loop = false;
  VEC(gimple,heap) *phis = NULL;
  enum vect_def_type dt = vect_unknown_def_type;
  int j, i;

  if (nested_in_vect_loop_p (loop, stmt))
    {
      outer_loop = loop;
      loop = loop->inner;
      nested_in_vect_loop = true;
    }

  switch (get_gimple_rhs_class (gimple_assign_rhs_code (stmt)))
    {
    case GIMPLE_SINGLE_RHS:
      gcc_assert (TREE_OPERAND_LENGTH (gimple_assign_rhs1 (stmt))
                                       == ternary_op);
      reduction_op = TREE_OPERAND (gimple_assign_rhs1 (stmt), reduc_index);
      break;
    case GIMPLE_UNARY_RHS:
      reduction_op = gimple_assign_rhs1 (stmt);
      break;
    case GIMPLE_BINARY_RHS:
      reduction_op = reduc_index ?
                     gimple_assign_rhs2 (stmt) : gimple_assign_rhs1 (stmt);
      break;
    default:
      gcc_unreachable ();
    }

  vectype = get_vectype_for_scalar_type (TREE_TYPE (reduction_op));
  gcc_assert (vectype);
  mode = TYPE_MODE (vectype);

  /*** 1. Create the reduction def-use cycle  ***/

  /* For the case of reduction, vect_get_vec_def_for_operand returns
     the scalar def before the loop, that defines the initial value
     of the reduction variable.  */
  vec_initial_def = vect_get_vec_def_for_operand (reduction_op, stmt,
					          &adjustment_def);

  phi = reduction_phi;
  def = vect_def;
  for (j = 0; j < ncopies; j++)
    {
      /* 1.1 set the loop-entry arg of the reduction-phi:  */
      add_phi_arg (phi, vec_initial_def, loop_preheader_edge (loop),
		   UNKNOWN_LOCATION);

      /* 1.2 set the loop-latch arg for the reduction-phi:  */
      if (j > 0)
        def = vect_get_vec_def_for_stmt_copy (dt, def);
      add_phi_arg (phi, def, loop_latch_edge (loop), UNKNOWN_LOCATION);

      if (vect_print_dump_info (REPORT_DETAILS))
	{
	  fprintf (vect_dump, "transform reduction: created def-use cycle: ");
	  print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
          fprintf (vect_dump, "\n");
          print_gimple_stmt (vect_dump, SSA_NAME_DEF_STMT (def), 0, TDF_SLIM);
	}

      phi = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (phi));
    }

  /*** 2. Create epilog code
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
          Lastly, the uses of s_out0 are replaced by s_out4.

	  ***/

  /* 2.1 Create new loop-exit-phi to preserve loop-closed form:
        v_out1 = phi <v_loop>  */

  exit_bb = single_exit (loop)->dest;
  def = vect_def;
  prev_phi_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      phi = create_phi_node (SSA_NAME_VAR (vect_def), exit_bb);
      set_vinfo_for_stmt (phi, new_stmt_vec_info (phi, loop_vinfo, NULL));
      if (j == 0)
	new_phi = phi;
      else
	{
	  def = vect_get_vec_def_for_stmt_copy (dt, def);
	  STMT_VINFO_RELATED_STMT (prev_phi_info) = phi;
	}
      SET_PHI_ARG_DEF (phi, single_exit (loop)->dest_idx, def);
      prev_phi_info = vinfo_for_stmt (phi);
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
  scalar_dest = gimple_assign_lhs (orig_stmt);
  scalar_type = TREE_TYPE (scalar_dest);
  new_scalar_dest = vect_create_destination_var (scalar_dest, NULL);
  bitsize = TYPE_SIZE (scalar_type);

  /* For MINUS_EXPR the initial vector is [init_val,0,...,0], therefore,
     partial results are added and not subtracted.  */
  if (code == MINUS_EXPR)
    code = PLUS_EXPR;

  /* In case this is a reduction in an inner-loop while vectorizing an outer
     loop - we don't need to extract a single scalar result at the end of the
     inner-loop (unless it is double reduction, i.e., the use of reduction is
     outside the outer-loop). The final vector of partial results will be used
     in the vectorized outer-loop, or reduced to a scalar result at the end of
     the outer-loop.  */
  if (nested_in_vect_loop && !double_reduc)
    goto vect_finalize_reduction;

  /* The epilogue is created for the outer-loop, i.e., for the loop being
     vectorized.  */
  if (double_reduc)
    loop = outer_loop;

  /* FORNOW */
  gcc_assert (ncopies == 1);

  /* 2.3 Create the reduction code, using one of the three schemes described
         above.  */

  if (reduc_code != ERROR_MARK)
    {
      tree tmp;

      /*** Case 1:  Create:
	   v_out2 = reduc_expr <v_out1>  */

      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "Reduce using direct vector reduction.");

      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      tmp = build1 (reduc_code, vectype,  PHI_RESULT (new_phi));
      epilog_stmt = gimple_build_assign (vec_dest, tmp);
      new_temp = make_ssa_name (vec_dest, epilog_stmt);
      gimple_assign_set_lhs (epilog_stmt, new_temp);
      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

      extract_scalar_result = true;
    }
  else
    {
      enum tree_code shift_code = ERROR_MARK;
      bool have_whole_vector_shift = true;
      int bit_offset;
      int element_bitsize = tree_low_cst (bitsize, 1);
      int vec_size_in_bits = tree_low_cst (TYPE_SIZE (vectype), 1);
      tree vec_temp;

      if (optab_handler (vec_shr_optab, mode)->insn_code != CODE_FOR_nothing)
	shift_code = VEC_RSHIFT_EXPR;
      else
	have_whole_vector_shift = false;

      /* Regardless of whether we have a whole vector shift, if we're
	 emulating the operation via tree-vect-generic, we don't want
	 to use it.  Only the first round of the reduction is likely
	 to still be profitable via emulation.  */
      /* ??? It might be better to emit a reduction tree code here, so that
	 tree-vect-generic can expand the first round via bit tricks.  */
      if (!VECTOR_MODE_P (mode))
	have_whole_vector_shift = false;
      else
	{
	  optab optab = optab_for_tree_code (code, vectype, optab_default);
	  if (optab_handler (optab, mode)->insn_code == CODE_FOR_nothing)
	    have_whole_vector_shift = false;
	}

      if (have_whole_vector_shift)
        {
	  /*** Case 2: Create:
	     for (offset = VS/2; offset >= element_size; offset/=2)
	        {
	          Create:  va' = vec_shift <va, offset>
	          Create:  va = vop <va, va'>
	        }  */

	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "Reduce using vector shifts");

	  vec_dest = vect_create_destination_var (scalar_dest, vectype);
	  new_temp = PHI_RESULT (new_phi);

	  for (bit_offset = vec_size_in_bits/2;
	       bit_offset >= element_bitsize;
	       bit_offset /= 2)
	    {
	      tree bitpos = size_int (bit_offset);

	      epilog_stmt = gimple_build_assign_with_ops (shift_code, vec_dest,
							  new_temp, bitpos);
	      new_name = make_ssa_name (vec_dest, epilog_stmt);
	      gimple_assign_set_lhs (epilog_stmt, new_name);
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

	      epilog_stmt = gimple_build_assign_with_ops (code, vec_dest,
							  new_name, new_temp);
	      new_temp = make_ssa_name (vec_dest, epilog_stmt);
	      gimple_assign_set_lhs (epilog_stmt, new_temp);
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	    }

	  extract_scalar_result = true;
	}
      else
        {
	  tree rhs;

	  /*** Case 3: Create:
	     s = extract_field <v_out2, 0>
	     for (offset = element_size;
		  offset < vector_size;
		  offset += element_size;)
	       {
	         Create:  s' = extract_field <v_out2, offset>
	         Create:  s = op <s, s'>
	       }  */

	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "Reduce using scalar code. ");

	  vec_temp = PHI_RESULT (new_phi);
	  vec_size_in_bits = tree_low_cst (TYPE_SIZE (vectype), 1);
	  rhs = build3 (BIT_FIELD_REF, scalar_type, vec_temp, bitsize,
			 bitsize_zero_node);
	  epilog_stmt = gimple_build_assign (new_scalar_dest, rhs);
	  new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
	  gimple_assign_set_lhs (epilog_stmt, new_temp);
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

	  for (bit_offset = element_bitsize;
	       bit_offset < vec_size_in_bits;
	       bit_offset += element_bitsize)
	    {
	      tree bitpos = bitsize_int (bit_offset);
	      tree rhs = build3 (BIT_FIELD_REF, scalar_type, vec_temp, bitsize,
				 bitpos);

	      epilog_stmt = gimple_build_assign (new_scalar_dest, rhs);
	      new_name = make_ssa_name (new_scalar_dest, epilog_stmt);
	      gimple_assign_set_lhs (epilog_stmt, new_name);
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);

	      epilog_stmt = gimple_build_assign_with_ops (code,
							  new_scalar_dest,
							  new_name, new_temp);
	      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
	      gimple_assign_set_lhs (epilog_stmt, new_temp);
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	    }

	  extract_scalar_result = false;
	}
    }

  /* 2.4  Extract the final scalar result.  Create:
         s_out3 = extract_field <v_out2, bitpos>  */

  if (extract_scalar_result)
    {
      tree rhs;

      gcc_assert (!nested_in_vect_loop || double_reduc);
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "extract scalar result");

      if (BYTES_BIG_ENDIAN)
	bitpos = size_binop (MULT_EXPR,
		       bitsize_int (TYPE_VECTOR_SUBPARTS (vectype) - 1),
		       TYPE_SIZE (scalar_type));
      else
	bitpos = bitsize_zero_node;

      rhs = build3 (BIT_FIELD_REF, scalar_type, new_temp, bitsize, bitpos);
      epilog_stmt = gimple_build_assign (new_scalar_dest, rhs);
      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
      gimple_assign_set_lhs (epilog_stmt, new_temp);
      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
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
      if (nested_in_vect_loop)
	{
	  gcc_assert (TREE_CODE (TREE_TYPE (adjustment_def)) == VECTOR_TYPE);
	  expr = build2 (code, vectype, PHI_RESULT (new_phi), adjustment_def);
	  new_dest = vect_create_destination_var (scalar_dest, vectype);
	}
      else
	{
	  gcc_assert (TREE_CODE (TREE_TYPE (adjustment_def)) != VECTOR_TYPE);
	  expr = build2 (code, scalar_type, new_temp, adjustment_def);
	  new_dest = vect_create_destination_var (scalar_dest, scalar_type);
	}

      epilog_stmt = gimple_build_assign (new_dest, expr);
      new_temp = make_ssa_name (new_dest, epilog_stmt);
      gimple_assign_set_lhs (epilog_stmt, new_temp);
      SSA_NAME_DEF_STMT (new_temp) = epilog_stmt;
      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
    }


  /* 2.6  Handle the loop-exit phi  */

  /* Replace uses of s_out0 with uses of s_out3:
     Find the loop-closed-use at the loop exit of the original scalar result.
     (The reduction result is expected to have two immediate uses - one at the
     latch block, and one at the loop exit).  */
  phis = VEC_alloc (gimple, heap, 10);
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, scalar_dest)
    {
      if (!flow_bb_inside_loop_p (loop, gimple_bb (USE_STMT (use_p))))
	{
	  exit_phi = USE_STMT (use_p);
	  VEC_quick_push (gimple, phis, exit_phi);
	}
    }

  /* We expect to have found an exit_phi because of loop-closed-ssa form.  */
  gcc_assert (!VEC_empty (gimple, phis));

  for (i = 0; VEC_iterate (gimple, phis, i, exit_phi); i++)
    {
      if (nested_in_vect_loop)
	{
	  stmt_vec_info stmt_vinfo = vinfo_for_stmt (exit_phi);
          gimple vect_phi;

	  /* FORNOW. Currently not supporting the case that an inner-loop
	     reduction is not used in the outer-loop (but only outside the
	     outer-loop), unless it is double reduction.  */
	  gcc_assert ((STMT_VINFO_RELEVANT_P (stmt_vinfo)
                      && !STMT_VINFO_LIVE_P (stmt_vinfo)) || double_reduc);

	  epilog_stmt = adjustment_def ? epilog_stmt : new_phi;
	  STMT_VINFO_VEC_STMT (stmt_vinfo) = epilog_stmt;
	  set_vinfo_for_stmt (epilog_stmt,
			      new_stmt_vec_info (epilog_stmt, loop_vinfo,
			                         NULL));
	  if (adjustment_def)
	    STMT_VINFO_RELATED_STMT (vinfo_for_stmt (epilog_stmt)) =
		STMT_VINFO_RELATED_STMT (vinfo_for_stmt (new_phi));

          if (!double_reduc
              || STMT_VINFO_DEF_TYPE (stmt_vinfo) != vect_double_reduction_def)
            continue;

          /* Handle double reduction:

             stmt1: s1 = phi <s0, s2>  - double reduction phi (outer loop)
             stmt2:   s3 = phi <s1, s4> - (regular) reduction phi (inner loop)
             stmt3:   s4 = use (s3)     - (regular) reduction stmt (inner loop)
             stmt4: s2 = phi <s4>      - double reduction stmt (outer loop)

             At that point the regular reduction (stmt2 and stmt3) is already
             vectorized, as well as the exit phi node, stmt4.
             Here we vectorize the phi node of double reduction, stmt1, and
             update all relevant statements.  */

          /* Go through all the uses of s2 to find double reduction phi node,
             i.e., stmt1 above.  */
          orig_name = PHI_RESULT (exit_phi);
          FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, orig_name)
            {
              stmt_vec_info use_stmt_vinfo = vinfo_for_stmt (use_stmt);
              stmt_vec_info new_phi_vinfo;
              tree vect_phi_init, preheader_arg, vect_phi_res, init_def;
              basic_block bb = gimple_bb (use_stmt);
              gimple use;

              /* Check that USE_STMT is really double reduction phi node.  */
              if (gimple_code (use_stmt) != GIMPLE_PHI
                  || gimple_phi_num_args (use_stmt) != 2
                  || !use_stmt_vinfo
                  || STMT_VINFO_DEF_TYPE (use_stmt_vinfo)
                      != vect_double_reduction_def
                  || bb->loop_father != outer_loop)
                continue;

              /* Create vector phi node for double reduction:
                 vs1 = phi <vs0, vs2>
                 vs1 was created previously in this function by a call to
                 vect_get_vec_def_for_operand and is stored in vec_initial_def;
                 vs2 is defined by EPILOG_STMT, the vectorized EXIT_PHI;
                 vs0 is created here.  */

              /* Create vector phi node.  */
              vect_phi = create_phi_node (vec_initial_def, bb);
              new_phi_vinfo = new_stmt_vec_info (vect_phi,
                                    loop_vec_info_for_loop (outer_loop), NULL);
              set_vinfo_for_stmt (vect_phi, new_phi_vinfo);

              /* Create vs0 - initial def of the double reduction phi.  */
              preheader_arg = PHI_ARG_DEF_FROM_EDGE (use_stmt,
                                             loop_preheader_edge (outer_loop));
              init_def = get_initial_def_for_reduction (stmt, preheader_arg,
                                                        NULL);
              vect_phi_init = vect_init_vector (use_stmt, init_def, vectype,
                                                NULL);

              /* Update phi node arguments with vs0 and vs2.  */
              add_phi_arg (vect_phi, vect_phi_init,
                           loop_preheader_edge (outer_loop), UNKNOWN_LOCATION);
              add_phi_arg (vect_phi, PHI_RESULT (epilog_stmt),
                           loop_latch_edge (outer_loop), UNKNOWN_LOCATION);
              if (vect_print_dump_info (REPORT_DETAILS))
                {
                  fprintf (vect_dump, "created double reduction phi node: ");
                  print_gimple_stmt (vect_dump, vect_phi, 0, TDF_SLIM);
                }

              vect_phi_res = PHI_RESULT (vect_phi);

              /* Replace the use, i.e., set the correct vs1 in the regular
                 reduction phi node. FORNOW, NCOPIES is always 1, so the loop
                 is redundant.  */
              use = reduction_phi;
              for (j = 0; j < ncopies; j++)
                {
                  edge pr_edge = loop_preheader_edge (loop);
                  SET_PHI_ARG_DEF (use, pr_edge->dest_idx, vect_phi_res);
                  use = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (use));
                }
            }
	}

      /* Replace the uses:  */
      orig_name = PHI_RESULT (exit_phi);
      FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, orig_name)
	FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	  SET_USE (use_p, new_temp);
    }

  VEC_free (gimple, heap, phis);
}


/* Function vectorizable_reduction.

   Check if STMT performs a reduction operation that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.

   This function also handles reduction idioms (patterns) that have been
   recognized in advance during vect_pattern_recog. In this case, STMT may be
   of this form:
     X = pattern_expr (arg0, arg1, ..., X)
   and it's STMT_VINFO_RELATED_STMT points to the last stmt in the original
   sequence that had been detected and replaced by the pattern-stmt (STMT).

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
   that the right vectorization factor would be derived. This vectype
   corresponds to the type of arguments to the reduction stmt, and should *NOT*
   be used to create the vectorized stmt. The right vectype for the vectorized
   stmt is obtained from the type of the result X:
        get_vectype_for_scalar_type (TREE_TYPE (X))

   This means that, contrary to "regular" reductions (or "regular" stmts in
   general), the following equation:
      STMT_VINFO_VECTYPE == get_vectype_for_scalar_type (TREE_TYPE (X))
   does *NOT* necessarily hold for reduction patterns.  */

bool
vectorizable_reduction (gimple stmt, gimple_stmt_iterator *gsi,
			gimple *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree loop_vec_def0 = NULL_TREE, loop_vec_def1 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  enum tree_code code, orig_code, epilog_reduc_code;
  enum machine_mode vec_mode;
  int op_type;
  optab optab, reduc_optab;
  tree new_temp = NULL_TREE;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt;
  gimple new_phi = NULL;
  tree scalar_type;
  bool is_simple_use;
  gimple orig_stmt;
  stmt_vec_info orig_stmt_info;
  tree expr = NULL_TREE;
  int i;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;
  int epilog_copies;
  stmt_vec_info prev_stmt_info, prev_phi_info;
  gimple first_phi = NULL;
  bool single_defuse_cycle = false;
  tree reduc_def = NULL_TREE;
  gimple new_stmt = NULL;
  int j;
  tree ops[3];
  bool nested_cycle = false, found_nested_cycle_def = false;
  gimple reduc_def_stmt = NULL;
  /* The default is that the reduction variable is the last in statement.  */
  int reduc_index = 2;
  bool double_reduc = false, dummy;
  basic_block def_bb;
  struct loop * def_stmt_loop, *outer_loop = NULL;
  tree def_arg;
  gimple def_arg_stmt;

  if (nested_in_vect_loop_p (loop, stmt))
    {
      outer_loop = loop;
      loop = loop->inner;
      nested_cycle = true;
    }

  gcc_assert (ncopies >= 1);

  /* FORNOW: SLP not supported.  */
  if (STMT_SLP_TYPE (stmt_info))
    return false;

  /* 1. Is vectorizable reduction?  */
  /* Not supportable if the reduction variable is used in the loop.  */
  if (STMT_VINFO_RELEVANT (stmt_info) > vect_used_in_outer)
    return false;

  /* Reductions that are not used even in an enclosing outer-loop,
     are expected to be "live" (used out of the loop).  */
  if (STMT_VINFO_RELEVANT (stmt_info) == vect_unused_in_scope
      && !STMT_VINFO_LIVE_P (stmt_info))
    return false;

  /* Make sure it was already recognized as a reduction computation.  */
  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_reduction_def
      && STMT_VINFO_DEF_TYPE (stmt_info) != vect_nested_cycle)
    return false;

  /* 2. Has this been recognized as a reduction pattern?

     Check if STMT represents a pattern that has been recognized
     in earlier analysis stages.  For stmts that represent a pattern,
     the STMT_VINFO_RELATED_STMT field records the last stmt in
     the original sequence that constitutes the pattern.  */

  orig_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
  if (orig_stmt)
    {
      orig_stmt_info = vinfo_for_stmt (orig_stmt);
      gcc_assert (STMT_VINFO_RELATED_STMT (orig_stmt_info) == stmt);
      gcc_assert (STMT_VINFO_IN_PATTERN_P (orig_stmt_info));
      gcc_assert (!STMT_VINFO_IN_PATTERN_P (stmt_info));
    }

  /* 3. Check the operands of the operation. The first operands are defined
        inside the loop body. The last operand is the reduction variable,
        which is defined by the loop-header-phi.  */

  gcc_assert (is_gimple_assign (stmt));

  /* Flatten RHS */
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

    case GIMPLE_UNARY_RHS:
      return false;

    default:
      gcc_unreachable ();
    }

  scalar_dest = gimple_assign_lhs (stmt);
  scalar_type = TREE_TYPE (scalar_dest);
  if (!POINTER_TYPE_P (scalar_type) && !INTEGRAL_TYPE_P (scalar_type)
      && !SCALAR_FLOAT_TYPE_P (scalar_type))
    return false;

  /* All uses but the last are expected to be defined in the loop.
     The last use is the reduction variable. In case of nested cycle this
     assumption is not true: we use reduc_index to record the index of the
     reduction variable.  */
  for (i = 0; i < op_type-1; i++)
    {
      /* The condition of COND_EXPR is checked in vectorizable_condition().  */
      if (i == 0 && code == COND_EXPR)
        continue;

      is_simple_use = vect_is_simple_use (ops[i], loop_vinfo, NULL, &def_stmt,
					  &def, &dt);
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
    }

  is_simple_use = vect_is_simple_use (ops[i], loop_vinfo, NULL, &def_stmt,
                                      &def, &dt);
  gcc_assert (is_simple_use);
  gcc_assert (dt == vect_reduction_def
              || dt == vect_nested_cycle
              || ((dt == vect_internal_def || dt == vect_external_def
                   || dt == vect_constant_def || dt == vect_induction_def)
                   && nested_cycle && found_nested_cycle_def));
  if (!found_nested_cycle_def)
    reduc_def_stmt = def_stmt;

  gcc_assert (gimple_code (reduc_def_stmt) == GIMPLE_PHI);
  if (orig_stmt)
    gcc_assert (orig_stmt == vect_is_simple_reduction (loop_vinfo,
                                                       reduc_def_stmt,
                                                       !nested_cycle,
                                                       &dummy));
  else
    gcc_assert (stmt == vect_is_simple_reduction (loop_vinfo, reduc_def_stmt,
                                                  !nested_cycle, &dummy));

  if (STMT_VINFO_LIVE_P (vinfo_for_stmt (reduc_def_stmt)))
    return false;

  vec_mode = TYPE_MODE (vectype);

  if (code == COND_EXPR)
    {
      if (!vectorizable_condition (stmt, gsi, NULL, ops[reduc_index], 0))
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "unsupported condition in reduction");

            return false;
        }
    }
  else
    {
      /* 4. Supportable by target?  */

      /* 4.1. check support for the operation in the loop  */
      optab = optab_for_tree_code (code, vectype, optab_default);
      if (!optab)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "no optab.");

          return false;
        }

      if (optab_handler (optab, vec_mode)->insn_code == CODE_FOR_nothing)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "op not supported by target.");

          if (GET_MODE_SIZE (vec_mode) != UNITS_PER_WORD
              || LOOP_VINFO_VECT_FACTOR (loop_vinfo)
	          < vect_min_worthwhile_factor (code))
            return false;

          if (vect_print_dump_info (REPORT_DETAILS))
  	    fprintf (vect_dump, "proceeding using word mode.");
        }

      /* Worthwhile without SIMD support?  */
      if (!VECTOR_MODE_P (TYPE_MODE (vectype))
          && LOOP_VINFO_VECT_FACTOR (loop_vinfo)
   	     < vect_min_worthwhile_factor (code))
        {
          if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "not worthwhile without SIMD support.");

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
             stmt from the pattern that STMT is replacing. I.e, in the example
             above we want to use 'widen_sum' in the loop, but 'plus' in the
             epilog.
          2. The type (mode) we use to check available target support
             for the vector operation to be created in the *epilog*, is
             determined by the type of the reduction variable (in the example
             above we'd check this: plus_optab[vect_int_mode]).
             However the type (mode) we use to check available target support
             for the vector operation to be created *inside the loop*, is
             determined by the type of the other arguments to STMT (in the
             example we'd check this: widen_sum_optab[vect_short_mode]).

          This is contrary to "regular" reductions, in which the types of all
          the arguments are the same as the type of the reduction variable.
          For "regular" reductions we can therefore use the same vector type
          (and also the same tree-code) when generating the epilog code and
          when generating the code inside the loop.  */

  if (orig_stmt)
    {
      /* This is a reduction pattern: get the vectype from the type of the
         reduction variable, and get the tree-code from orig_stmt.  */
      orig_code = gimple_assign_rhs_code (orig_stmt);
      vectype = get_vectype_for_scalar_type (TREE_TYPE (def));
      if (!vectype)
	{
          if (vect_print_dump_info (REPORT_DETAILS))
            {
              fprintf (vect_dump, "unsupported data-type ");
              print_generic_expr (vect_dump, TREE_TYPE (def), TDF_SLIM);
            }
          return false;
        }

      vec_mode = TYPE_MODE (vectype);
    }
  else
    {
      /* Regular reduction: use the same vectype and tree-code as used for
         the vector code inside the loop can be used for the epilog code. */
      orig_code = code;
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
  if (reduction_code_for_scalar_code (orig_code, &epilog_reduc_code))
    {
      reduc_optab = optab_for_tree_code (epilog_reduc_code, vectype,
                                         optab_default);
      if (!reduc_optab)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "no optab for reduction.");

          epilog_reduc_code = ERROR_MARK;
        }

      if (reduc_optab
          && optab_handler (reduc_optab, vec_mode)->insn_code
              == CODE_FOR_nothing)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "reduc op not supported by target.");

          epilog_reduc_code = ERROR_MARK;
        }
    }
  else
    {
      if (!nested_cycle || double_reduc)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "no reduc code for scalar code.");

          return false;
        }
    }

  if (double_reduc && ncopies > 1)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "multiple types in double reduction");

      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
      if (!vect_model_reduction_cost (stmt_info, epilog_reduc_code, ncopies))
        return false;
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform reduction.");

  /* FORNOW: Multiple types are not supported for condition.  */
  if (code == COND_EXPR)
    gcc_assert (ncopies == 1);

  /* Create the destination vector  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

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

  if (STMT_VINFO_RELEVANT (stmt_info) == vect_unused_in_scope)
    {
      single_defuse_cycle = true;
      epilog_copies = 1;
    }
  else
    epilog_copies = ncopies;

  prev_stmt_info = NULL;
  prev_phi_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      if (j == 0 || !single_defuse_cycle)
	{
	  /* Create the reduction-phi that defines the reduction-operand.  */
	  new_phi = create_phi_node (vec_dest, loop->header);
	  set_vinfo_for_stmt (new_phi, new_stmt_vec_info (new_phi, loop_vinfo,
	                                                  NULL));
          /* Get the vector def for the reduction variable from the phi
             node.  */
          reduc_def = PHI_RESULT (new_phi);
	}

      if (code == COND_EXPR)
        {
          first_phi = new_phi;
          vectorizable_condition (stmt, gsi, vec_stmt, reduc_def, reduc_index);
          /* Multiple types are not supported for condition.  */
          break;
        }

      /* Handle uses.  */
      if (j == 0)
        {
	  loop_vec_def0 = vect_get_vec_def_for_operand (ops[!reduc_index],
                                                        stmt, NULL);
          if (op_type == ternary_op)
            {
              if (reduc_index == 0)
 	        loop_vec_def1 = vect_get_vec_def_for_operand (ops[2], stmt,
                                                              NULL);
              else
                loop_vec_def1 = vect_get_vec_def_for_operand (ops[1], stmt,
                                                              NULL);
            }

          /* Get the vector def for the reduction variable from the phi
             node.  */
	  first_phi = new_phi;
        }
      else
        {
          enum vect_def_type dt = vect_unknown_def_type; /* Dummy */
          loop_vec_def0 = vect_get_vec_def_for_stmt_copy (dt, loop_vec_def0);
          if (op_type == ternary_op)
            loop_vec_def1 = vect_get_vec_def_for_stmt_copy (dt, loop_vec_def1);

	  if (single_defuse_cycle)
	    reduc_def = gimple_assign_lhs (new_stmt);
	  else
	    reduc_def = PHI_RESULT (new_phi);

	  STMT_VINFO_RELATED_STMT (prev_phi_info) = new_phi;
        }

      /* Arguments are ready. Create the new vector stmt.  */
      if (op_type == binary_op)
        {
          if (reduc_index == 0)
            expr = build2 (code, vectype, reduc_def, loop_vec_def0);
          else
            expr = build2 (code, vectype, loop_vec_def0, reduc_def);
        }
      else
        {
          if (reduc_index == 0)
            expr = build3 (code, vectype, reduc_def, loop_vec_def0,
                           loop_vec_def1);
          else
            {
              if (reduc_index == 1)
                expr = build3 (code, vectype, loop_vec_def0, reduc_def,
                               loop_vec_def1);
              else
                expr = build3 (code, vectype, loop_vec_def0, loop_vec_def1,
	     	               reduc_def);
            }
        }

      new_stmt = gimple_build_assign (vec_dest, expr);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
      vect_finish_stmt_generation (stmt, new_stmt, gsi);

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;

      prev_stmt_info = vinfo_for_stmt (new_stmt);
      prev_phi_info = vinfo_for_stmt (new_phi);
    }

  /* Finalize the reduction-phi (set its arguments) and create the
     epilog reduction code.  */
  if (!single_defuse_cycle || code == COND_EXPR)
    new_temp = gimple_assign_lhs (*vec_stmt);

  vect_create_epilog_for_reduction (new_temp, stmt, epilog_copies,
				    epilog_reduc_code, first_phi, reduc_index,
                                    double_reduc);
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
vectorizable_induction (gimple phi, gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
			gimple *vec_stmt)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (phi);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;
  tree vec_def;

  gcc_assert (ncopies >= 1);
  /* FORNOW. This restriction should be relaxed.  */
  if (nested_in_vect_loop_p (loop, phi) && ncopies > 1)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "multiple types in nested loop.");
      return false;
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
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "=== vectorizable_induction ===");
      vect_model_induction_cost (stmt_info, ncopies);
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform induction phi.");

  vec_def = get_initial_def_for_induction (phi);
  *vec_stmt = SSA_NAME_DEF_STMT (vec_def);
  return true;
}

/* Function vectorizable_live_operation.

   STMT computes a value that is used outside the loop. Check if
   it can be supported.  */

bool
vectorizable_live_operation (gimple stmt,
			     gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
			     gimple *vec_stmt ATTRIBUTE_UNUSED)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int i;
  int op_type;
  tree op;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt;
  enum tree_code code;
  enum gimple_rhs_class rhs_class;

  gcc_assert (STMT_VINFO_LIVE_P (stmt_info));

  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def)
    return false;

  if (!is_gimple_assign (stmt))
    return false;

  if (TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return false;

  /* FORNOW. CHECKME. */
  if (nested_in_vect_loop_p (loop, stmt))
    return false;

  code = gimple_assign_rhs_code (stmt);
  op_type = TREE_CODE_LENGTH (code);
  rhs_class = get_gimple_rhs_class (code);
  gcc_assert (rhs_class != GIMPLE_UNARY_RHS || op_type == unary_op);
  gcc_assert (rhs_class != GIMPLE_BINARY_RHS || op_type == binary_op);

  /* FORNOW: support only if all uses are invariant. This means
     that the scalar operations can remain in place, unvectorized.
     The original last scalar value that they compute will be used.  */

  for (i = 0; i < op_type; i++)
    {
      if (rhs_class == GIMPLE_SINGLE_RHS)
	op = TREE_OPERAND (gimple_op (stmt, 1), i);
      else
	op = gimple_op (stmt, i + 1);
      if (op
          && !vect_is_simple_use (op, loop_vinfo, NULL, &def_stmt, &def, &dt))
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "use not simple.");
          return false;
        }

      if (dt != vect_external_def && dt != vect_constant_def)
        return false;
    }

  /* No transformation is required for the cases we currently support.  */
  return true;
}

/* Kill any debug uses outside LOOP of SSA names defined in STMT.  */

static void
vect_loop_kill_debug_uses (struct loop *loop, gimple stmt)
{
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  def_operand_p def_p;
  gimple ustmt;

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
		  if (vect_print_dump_info (REPORT_DETAILS))
		    fprintf (vect_dump, "killing debug use");

		  gimple_debug_bind_reset_value (ustmt);
		  update_stmt (ustmt);
		}
	      else
		gcc_unreachable ();
	    }
	}
    }
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
  gimple_stmt_iterator si;
  int i;
  tree ratio = NULL;
  int vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  bool strided_store;
  bool slp_scheduled = false;
  unsigned int nunits;
  tree cond_expr = NULL_TREE;
  gimple_seq cond_expr_stmt_list = NULL;
  bool do_peeling_for_loop_bound;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vec_transform_loop ===");

  /* Peel the loop if there are data refs with unknown alignment.
     Only one data ref with unknown store is allowed.  */

  if (LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo))
    vect_do_peeling_for_alignment (loop_vinfo);

  do_peeling_for_loop_bound
    = (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
       || (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
	   && LOOP_VINFO_INT_NITERS (loop_vinfo) % vectorization_factor != 0));

  if (LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo)
      || LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo))
    vect_loop_versioning (loop_vinfo,
			  !do_peeling_for_loop_bound,
			  &cond_expr, &cond_expr_stmt_list);

  /* If the loop has a symbolic number of iterations 'n' (i.e. it's not a
     compile time constant), or it is a constant that doesn't divide by the
     vectorization factor, then an epilog loop needs to be created.
     We therefore duplicate the loop: the original loop will be vectorized,
     and will compute the first (n/VF) iterations. The second copy of the loop
     will remain scalar and will compute the remaining (n%VF) iterations.
     (VF is the vectorization factor).  */

  if (do_peeling_for_loop_bound)
    vect_do_peeling_for_loop_bound (loop_vinfo, &ratio,
				    cond_expr, cond_expr_stmt_list);
  else
    ratio = build_int_cst (TREE_TYPE (LOOP_VINFO_NITERS (loop_vinfo)),
		LOOP_VINFO_INT_NITERS (loop_vinfo) / vectorization_factor);

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
      gimple phi;

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
        {
	  phi = gsi_stmt (si);
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "------>vectorizing phi: ");
	      print_gimple_stmt (vect_dump, phi, 0, TDF_SLIM);
	    }
	  stmt_info = vinfo_for_stmt (phi);
	  if (!stmt_info)
	    continue;

	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
	    {
	      if (MAY_HAVE_DEBUG_STMTS)
		vect_loop_kill_debug_uses (loop, phi);
	      continue;
	    }

	  if ((TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info))
	        != (unsigned HOST_WIDE_INT) vectorization_factor)
	      && vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "multiple-types.");

	  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "transform phi.");
	      vect_transform_stmt (phi, NULL, NULL, NULL, NULL);
	    }
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si);)
	{
	  gimple stmt = gsi_stmt (si);
	  bool is_store;

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "------>vectorizing statement: ");
	      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
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

	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
	    {
	      if (MAY_HAVE_DEBUG_STMTS)
		vect_loop_kill_debug_uses (loop, stmt);
	      gsi_next (&si);
	      continue;
	    }

	  gcc_assert (STMT_VINFO_VECTYPE (stmt_info));
	  nunits =
	    (unsigned int) TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
	  if (!STMT_SLP_TYPE (stmt_info)
	      && nunits != (unsigned int) vectorization_factor
              && vect_print_dump_info (REPORT_DETAILS))
	    /* For SLP VF is set according to unrolling factor, and not to
	       vector size, hence for SLP this print is not valid.  */
            fprintf (vect_dump, "multiple-types.");

	  /* SLP. Schedule all the SLP instances when the first SLP stmt is
	     reached.  */
	  if (STMT_SLP_TYPE (stmt_info))
	    {
	      if (!slp_scheduled)
		{
		  slp_scheduled = true;

		  if (vect_print_dump_info (REPORT_DETAILS))
		    fprintf (vect_dump, "=== scheduling SLP instances ===");

		  vect_schedule_slp (loop_vinfo, NULL);
		}

	      /* Hybrid SLP stmts must be vectorized in addition to SLP.  */
	      if (!vinfo_for_stmt (stmt) || PURE_SLP_STMT (stmt_info))
		{
		  gsi_next (&si);
		  continue;
		}
	    }

	  /* -------- vectorize statement ------------ */
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "transform statement.");

	  strided_store = false;
	  is_store = vect_transform_stmt (stmt, &si, &strided_store, NULL, NULL);
          if (is_store)
            {
	      if (STMT_VINFO_STRIDED_ACCESS (stmt_info))
		{
		  /* Interleaving. If IS_STORE is TRUE, the vectorization of the
		     interleaving chain was completed - free all the stores in
		     the chain.  */
		  vect_remove_stores (DR_GROUP_FIRST_DR (stmt_info));
		  gsi_remove (&si, true);
		  continue;
		}
	      else
		{
		  /* Free the attached stmt_vec_info and remove the stmt.  */
		  free_stmt_vec_info (stmt);
		  gsi_remove (&si, true);
		  continue;
		}
	    }
	  gsi_next (&si);
	}		        /* stmts in BB */
    }				/* BBs in loop */

  slpeel_make_loop_iterate_ntimes (loop, ratio);

  /* The memory tags and pointers in vectorized statements need to
     have their SSA forms updated.  FIXME, why can't this be delayed
     until all the loops have been transformed?  */
  update_ssa (TODO_update_ssa);

  if (vect_print_dump_info (REPORT_VECTORIZED_LOCATIONS))
    fprintf (vect_dump, "LOOP VECTORIZED.");
  if (loop->inner && vect_print_dump_info (REPORT_VECTORIZED_LOCATIONS))
    fprintf (vect_dump, "OUTER LOOP VECTORIZED.");
}
