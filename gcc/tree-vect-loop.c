/* Loop Vectorization
   Copyright (C) 2003-2020 Free Software Foundation, Inc.
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
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "gimple-fold.h"
#include "cgraph.h"
#include "tree-cfg.h"
#include "tree-if-conv.h"
#include "internal-fn.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"
#include "tree-eh.h"

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
static stmt_vec_info vect_is_simple_reduction (loop_vec_info, stmt_vec_info,
					       bool *, bool *);

/* Subroutine of vect_determine_vf_for_stmt that handles only one
   statement.  VECTYPE_MAYBE_SET_P is true if STMT_VINFO_VECTYPE
   may already be set for general statements (not just data refs).  */

static opt_result
vect_determine_vf_for_stmt_1 (stmt_vec_info stmt_info,
			      bool vectype_maybe_set_p,
			      poly_uint64 *vf)
{
  gimple *stmt = stmt_info->stmt;

  if ((!STMT_VINFO_RELEVANT_P (stmt_info)
       && !STMT_VINFO_LIVE_P (stmt_info))
      || gimple_clobber_p (stmt))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "skip.\n");
      return opt_result::success ();
    }

  tree stmt_vectype, nunits_vectype;
  opt_result res = vect_get_vector_types_for_stmt (stmt_info, &stmt_vectype,
						   &nunits_vectype);
  if (!res)
    return res;

  if (stmt_vectype)
    {
      if (STMT_VINFO_VECTYPE (stmt_info))
	/* The only case when a vectype had been already set is for stmts
	   that contain a data ref, or for "pattern-stmts" (stmts generated
	   by the vectorizer to represent/replace a certain idiom).  */
	gcc_assert ((STMT_VINFO_DATA_REF (stmt_info)
		     || vectype_maybe_set_p)
		    && STMT_VINFO_VECTYPE (stmt_info) == stmt_vectype);
      else
	STMT_VINFO_VECTYPE (stmt_info) = stmt_vectype;
    }

  if (nunits_vectype)
    vect_update_max_nunits (vf, nunits_vectype);

  return opt_result::success ();
}

/* Subroutine of vect_determine_vectorization_factor.  Set the vector
   types of STMT_INFO and all attached pattern statements and update
   the vectorization factor VF accordingly.  Return true on success
   or false if something prevented vectorization.  */

static opt_result
vect_determine_vf_for_stmt (stmt_vec_info stmt_info, poly_uint64 *vf)
{
  vec_info *vinfo = stmt_info->vinfo;
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "==> examining statement: %G",
		     stmt_info->stmt);
  opt_result res = vect_determine_vf_for_stmt_1 (stmt_info, false, vf);
  if (!res)
    return res;

  if (STMT_VINFO_IN_PATTERN_P (stmt_info)
      && STMT_VINFO_RELATED_STMT (stmt_info))
    {
      gimple *pattern_def_seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_info);
      stmt_info = STMT_VINFO_RELATED_STMT (stmt_info);

      /* If a pattern statement has def stmts, analyze them too.  */
      for (gimple_stmt_iterator si = gsi_start (pattern_def_seq);
	   !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info def_stmt_info = vinfo->lookup_stmt (gsi_stmt (si));
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "==> examining pattern def stmt: %G",
			     def_stmt_info->stmt);
	  res = vect_determine_vf_for_stmt_1 (def_stmt_info, true, vf);
	  if (!res)
	    return res;
	}

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "==> examining pattern statement: %G",
			 stmt_info->stmt);
      res = vect_determine_vf_for_stmt_1 (stmt_info, true, vf);
      if (!res)
	return res;
    }

  return opt_result::success ();
}

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

static opt_result
vect_determine_vectorization_factor (loop_vec_info loop_vinfo)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned nbbs = loop->num_nodes;
  poly_uint64 vectorization_factor = 1;
  tree scalar_type = NULL_TREE;
  gphi *phi;
  tree vectype;
  stmt_vec_info stmt_info;
  unsigned i;

  DUMP_VECT_SCOPE ("vect_determine_vectorization_factor");

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  phi = si.phi ();
	  stmt_info = loop_vinfo->lookup_stmt (phi);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "==> examining phi: %G",
			     phi);

	  gcc_assert (stmt_info);

	  if (STMT_VINFO_RELEVANT_P (stmt_info)
	      || STMT_VINFO_LIVE_P (stmt_info))
            {
	      gcc_assert (!STMT_VINFO_VECTYPE (stmt_info));
              scalar_type = TREE_TYPE (PHI_RESULT (phi));

	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "get vectype for scalar type:  %T\n",
				 scalar_type);

	      vectype = get_vectype_for_scalar_type (loop_vinfo, scalar_type);
	      if (!vectype)
		return opt_result::failure_at (phi,
					       "not vectorized: unsupported "
					       "data-type %T\n",
					       scalar_type);
	      STMT_VINFO_VECTYPE (stmt_info) = vectype;

	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location, "vectype: %T\n",
				 vectype);

	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location, "nunits = ");
		  dump_dec (MSG_NOTE, TYPE_VECTOR_SUBPARTS (vectype));
		  dump_printf (MSG_NOTE, "\n");
		}

	      vect_update_max_nunits (&vectorization_factor, vectype);
	    }
	}

      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  stmt_info = loop_vinfo->lookup_stmt (gsi_stmt (si));
	  opt_result res
	    = vect_determine_vf_for_stmt (stmt_info, &vectorization_factor);
	  if (!res)
	    return res;
        }
    }

  /* TODO: Analyze cost. Decide if worth while to vectorize.  */
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "vectorization factor = ");
      dump_dec (MSG_NOTE, vectorization_factor);
      dump_printf (MSG_NOTE, "\n");
    }

  if (known_le (vectorization_factor, 1U))
    return opt_result::failure_at (vect_location,
				   "not vectorized: unsupported data-type\n");
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;
  return opt_result::success ();
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
    dump_printf_loc (MSG_NOTE, vect_location, "step: %T,  init: %T\n",
		     step_expr, init_expr);

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

/* Return true if PHI, described by STMT_INFO, is the inner PHI in
   what we are assuming is a double reduction.  For example, given
   a structure like this:

      outer1:
	x_1 = PHI <x_4(outer2), ...>;
	...

      inner:
	x_2 = PHI <x_1(outer1), ...>;
	...
	x_3 = ...;
	...

      outer2:
	x_4 = PHI <x_3(inner)>;
	...

   outer loop analysis would treat x_1 as a double reduction phi and
   this function would then return true for x_2.  */

static bool
vect_inner_phi_in_double_reduction_p (stmt_vec_info stmt_info, gphi *phi)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  use_operand_p use_p;
  ssa_op_iter op_iter;
  FOR_EACH_PHI_ARG (use_p, phi, op_iter, SSA_OP_USE)
    if (stmt_vec_info def_info = loop_vinfo->lookup_def (USE_FROM_PTR (use_p)))
      if (STMT_VINFO_DEF_TYPE (def_info) == vect_double_reduction_def)
	return true;
  return false;
}

/* Function vect_analyze_scalar_cycles_1.

   Examine the cross iteration def-use cycles of scalar variables
   in LOOP.  LOOP_VINFO represents the loop that is now being
   considered for vectorization (can be LOOP, or an outer-loop
   enclosing LOOP).  */

static void
vect_analyze_scalar_cycles_1 (loop_vec_info loop_vinfo, class loop *loop)
{
  basic_block bb = loop->header;
  tree init, step;
  auto_vec<stmt_vec_info, 64> worklist;
  gphi_iterator gsi;
  bool double_reduc, reduc_chain;

  DUMP_VECT_SCOPE ("vect_analyze_scalar_cycles");

  /* First - identify all inductions.  Reduction detection assumes that all the
     inductions have been identified, therefore, this order must not be
     changed.  */
  for (gsi = gsi_start_phis  (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree access_fn = NULL;
      tree def = PHI_RESULT (phi);
      stmt_vec_info stmt_vinfo = loop_vinfo->lookup_stmt (phi);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: %G", phi);

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
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Access function of PHI: %T\n", access_fn);
	  STMT_VINFO_LOOP_PHI_EVOLUTION_BASE_UNCHANGED (stmt_vinfo)
	    = initial_condition_in_loop_num (access_fn, loop->num);
	  STMT_VINFO_LOOP_PHI_EVOLUTION_PART (stmt_vinfo)
	    = evolution_part_in_loop_num (access_fn, loop->num);
	}

      if (!access_fn
	  || vect_inner_phi_in_double_reduction_p (stmt_vinfo, phi)
	  || !vect_is_simple_iv_evolution (loop->num, access_fn, &init, &step)
	  || (LOOP_VINFO_LOOP (loop_vinfo) != loop
	      && TREE_CODE (step) != INTEGER_CST))
	{
	  worklist.safe_push (stmt_vinfo);
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
      stmt_vec_info stmt_vinfo = worklist.pop ();
      gphi *phi = as_a <gphi *> (stmt_vinfo->stmt);
      tree def = PHI_RESULT (phi);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: %G", phi);

      gcc_assert (!virtual_operand_p (def)
		  && STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_unknown_def_type);

      stmt_vec_info reduc_stmt_info
	= vect_is_simple_reduction (loop_vinfo, stmt_vinfo, &double_reduc,
				    &reduc_chain);
      if (reduc_stmt_info)
        {
	  STMT_VINFO_REDUC_DEF (stmt_vinfo) = reduc_stmt_info;
	  STMT_VINFO_REDUC_DEF (reduc_stmt_info) = stmt_vinfo;
	  if (double_reduc)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Detected double reduction.\n");

              STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_double_reduction_def;
	      STMT_VINFO_DEF_TYPE (reduc_stmt_info) = vect_double_reduction_def;
            }
          else
            {
              if (loop != LOOP_VINFO_LOOP (loop_vinfo))
                {
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_NOTE, vect_location,
				     "Detected vectorizable nested cycle.\n");

                  STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_nested_cycle;
                }
              else
                {
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_NOTE, vect_location,
				     "Detected reduction.\n");

                  STMT_VINFO_DEF_TYPE (stmt_vinfo) = vect_reduction_def;
		  STMT_VINFO_DEF_TYPE (reduc_stmt_info) = vect_reduction_def;
                  /* Store the reduction cycles for possible vectorization in
                     loop-aware SLP if it was not detected as reduction
		     chain.  */
		  if (! reduc_chain)
		    LOOP_VINFO_REDUCTIONS (loop_vinfo).safe_push
		      (reduc_stmt_info);
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
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

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

/* Transfer group and reduction information from STMT_INFO to its
   pattern stmt.  */

static void
vect_fixup_reduc_chain (stmt_vec_info stmt_info)
{
  stmt_vec_info firstp = STMT_VINFO_RELATED_STMT (stmt_info);
  stmt_vec_info stmtp;
  gcc_assert (!REDUC_GROUP_FIRST_ELEMENT (firstp)
	      && REDUC_GROUP_FIRST_ELEMENT (stmt_info));
  REDUC_GROUP_SIZE (firstp) = REDUC_GROUP_SIZE (stmt_info);
  do
    {
      stmtp = STMT_VINFO_RELATED_STMT (stmt_info);
      gcc_checking_assert (STMT_VINFO_DEF_TYPE (stmtp)
			   == STMT_VINFO_DEF_TYPE (stmt_info));
      REDUC_GROUP_FIRST_ELEMENT (stmtp) = firstp;
      stmt_info = REDUC_GROUP_NEXT_ELEMENT (stmt_info);
      if (stmt_info)
	REDUC_GROUP_NEXT_ELEMENT (stmtp)
	  = STMT_VINFO_RELATED_STMT (stmt_info);
    }
  while (stmt_info);
}

/* Fixup scalar cycles that now have their stmts detected as patterns.  */

static void
vect_fixup_scalar_cycles_with_patterns (loop_vec_info loop_vinfo)
{
  stmt_vec_info first;
  unsigned i;

  FOR_EACH_VEC_ELT (LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo), i, first)
    if (STMT_VINFO_IN_PATTERN_P (first))
      {
	stmt_vec_info next = REDUC_GROUP_NEXT_ELEMENT (first);
	while (next)
	  {
	    if (! STMT_VINFO_IN_PATTERN_P (next)
		|| STMT_VINFO_REDUC_IDX (STMT_VINFO_RELATED_STMT (next)) == -1)
	      break;
	    next = REDUC_GROUP_NEXT_ELEMENT (next);
	  }
	/* If not all stmt in the chain are patterns or if we failed
	   to update STMT_VINFO_REDUC_IDX try to handle the chain
	   without patterns.  */
	if (! next
	    && STMT_VINFO_REDUC_IDX (STMT_VINFO_RELATED_STMT (first)) != -1)
	  {
	    vect_fixup_reduc_chain (first);
	    LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo)[i]
	      = STMT_VINFO_RELATED_STMT (first);
	  }
      }
}

/* Function vect_get_loop_niters.

   Determine how many iterations the loop is executed and place it
   in NUMBER_OF_ITERATIONS.  Place the number of latch iterations
   in NUMBER_OF_ITERATIONSM1.  Place the condition under which the
   niter information holds in ASSUMPTIONS.

   Return the loop exit condition.  */


static gcond *
vect_get_loop_niters (class loop *loop, tree *assumptions,
		      tree *number_of_iterations, tree *number_of_iterationsm1)
{
  edge exit = single_exit (loop);
  class tree_niter_desc niter_desc;
  tree niter_assumptions, niter, may_be_zero;
  gcond *cond = get_loop_exit_condition (loop);

  *assumptions = boolean_true_node;
  *number_of_iterationsm1 = chrec_dont_know;
  *number_of_iterations = chrec_dont_know;
  DUMP_VECT_SCOPE ("get_loop_niters");

  if (!exit)
    return cond;

  may_be_zero = NULL_TREE;
  if (!number_of_iterations_exit_assumptions (loop, exit, &niter_desc, NULL)
      || chrec_contains_undetermined (niter_desc.niter))
    return cond;

  niter_assumptions = niter_desc.assumptions;
  may_be_zero = niter_desc.may_be_zero;
  niter = niter_desc.niter;

  if (may_be_zero && integer_zerop (may_be_zero))
    may_be_zero = NULL_TREE;

  if (may_be_zero)
    {
      if (COMPARISON_CLASS_P (may_be_zero))
	{
	  /* Try to combine may_be_zero with assumptions, this can simplify
	     computation of niter expression.  */
	  if (niter_assumptions && !integer_nonzerop (niter_assumptions))
	    niter_assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
					     niter_assumptions,
					     fold_build1 (TRUTH_NOT_EXPR,
							  boolean_type_node,
							  may_be_zero));
	  else
	    niter = fold_build3 (COND_EXPR, TREE_TYPE (niter), may_be_zero,
				 build_int_cst (TREE_TYPE (niter), 0),
				 rewrite_to_non_trapping_overflow (niter));

	  may_be_zero = NULL_TREE;
	}
      else if (integer_nonzerop (may_be_zero))
	{
	  *number_of_iterationsm1 = build_int_cst (TREE_TYPE (niter), 0);
	  *number_of_iterations = build_int_cst (TREE_TYPE (niter), 1);
	  return cond;
	}
      else
	return cond;
    }

  *assumptions = niter_assumptions;
  *number_of_iterationsm1 = niter;

  /* We want the number of loop header executions which is the number
     of latch executions plus one.
     ???  For UINT_MAX latch executions this number overflows to zero
     for loops like do { n++; } while (n != 0);  */
  if (niter && !chrec_contains_undetermined (niter))
    niter = fold_build2 (PLUS_EXPR, TREE_TYPE (niter), unshare_expr (niter),
			  build_int_cst (TREE_TYPE (niter), 1));
  *number_of_iterations = niter;

  return cond;
}

/* Function bb_in_loop_p

   Used as predicate for dfs order traversal of the loop bbs.  */

static bool
bb_in_loop_p (const_basic_block bb, const void *data)
{
  const class loop *const loop = (const class loop *)data;
  if (flow_bb_inside_loop_p (loop, bb))
    return true;
  return false;
}


/* Create and initialize a new loop_vec_info struct for LOOP_IN, as well as
   stmt_vec_info structs for all the stmts in LOOP_IN.  */

_loop_vec_info::_loop_vec_info (class loop *loop_in, vec_info_shared *shared)
  : vec_info (vec_info::loop, init_cost (loop_in), shared),
    loop (loop_in),
    bbs (XCNEWVEC (basic_block, loop->num_nodes)),
    num_itersm1 (NULL_TREE),
    num_iters (NULL_TREE),
    num_iters_unchanged (NULL_TREE),
    num_iters_assumptions (NULL_TREE),
    th (0),
    versioning_threshold (0),
    vectorization_factor (0),
    max_vectorization_factor (0),
    mask_skip_niters (NULL_TREE),
    mask_compare_type (NULL_TREE),
    simd_if_cond (NULL_TREE),
    unaligned_dr (NULL),
    peeling_for_alignment (0),
    ptr_mask (0),
    ivexpr_map (NULL),
    scan_map (NULL),
    slp_unrolling_factor (1),
    single_scalar_iteration_cost (0),
    vec_outside_cost (0),
    vec_inside_cost (0),
    vectorizable (false),
    can_fully_mask_p (true),
    fully_masked_p (false),
    peeling_for_gaps (false),
    peeling_for_niter (false),
    no_data_dependencies (false),
    has_mask_store (false),
    scalar_loop_scaling (profile_probability::uninitialized ()),
    scalar_loop (NULL),
    orig_loop_info (NULL)
{
  /* CHECKME: We want to visit all BBs before their successors (except for
     latch blocks, for which this assertion wouldn't hold).  In the simple
     case of the loop forms we allow, a dfs order of the BBs would the same
     as reversed postorder traversal, so we are safe.  */

  unsigned int nbbs = dfs_enumerate_from (loop->header, 0, bb_in_loop_p,
					  bbs, loop->num_nodes, loop);
  gcc_assert (nbbs == loop->num_nodes);

  for (unsigned int i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator si;

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *phi = gsi_stmt (si);
	  gimple_set_uid (phi, 0);
	  add_stmt (phi);
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  gimple_set_uid (stmt, 0);
	  add_stmt (stmt);
	  /* If .GOMP_SIMD_LANE call for the current loop has 3 arguments, the
	     third argument is the #pragma omp simd if (x) condition, when 0,
	     loop shouldn't be vectorized, when non-zero constant, it should
	     be vectorized normally, otherwise versioned with vectorized loop
	     done if the condition is non-zero at runtime.  */
	  if (loop_in->simduid
	      && is_gimple_call (stmt)
	      && gimple_call_internal_p (stmt)
	      && gimple_call_internal_fn (stmt) == IFN_GOMP_SIMD_LANE
	      && gimple_call_num_args (stmt) >= 3
	      && TREE_CODE (gimple_call_arg (stmt, 0)) == SSA_NAME
	      && (loop_in->simduid
		  == SSA_NAME_VAR (gimple_call_arg (stmt, 0))))
	    {
	      tree arg = gimple_call_arg (stmt, 2);
	      if (integer_zerop (arg) || TREE_CODE (arg) == SSA_NAME)
		simd_if_cond = arg;
	      else
		gcc_assert (integer_nonzerop (arg));
	    }
	}
    }

  epilogue_vinfos.create (6);
}

/* Free all levels of MASKS.  */

void
release_vec_loop_masks (vec_loop_masks *masks)
{
  rgroup_masks *rgm;
  unsigned int i;
  FOR_EACH_VEC_ELT (*masks, i, rgm)
    rgm->masks.release ();
  masks->release ();
}

/* Free all memory used by the _loop_vec_info, as well as all the
   stmt_vec_info structs of all the stmts in the loop.  */

_loop_vec_info::~_loop_vec_info ()
{
  free (bbs);

  release_vec_loop_masks (&masks);
  delete ivexpr_map;
  delete scan_map;
  epilogue_vinfos.release ();

  loop->aux = NULL;
}

/* Return an invariant or register for EXPR and emit necessary
   computations in the LOOP_VINFO loop preheader.  */

tree
cse_and_gimplify_to_preheader (loop_vec_info loop_vinfo, tree expr)
{
  if (is_gimple_reg (expr)
      || is_gimple_min_invariant (expr))
    return expr;

  if (! loop_vinfo->ivexpr_map)
    loop_vinfo->ivexpr_map = new hash_map<tree_operand_hash, tree>;
  tree &cached = loop_vinfo->ivexpr_map->get_or_insert (expr);
  if (! cached)
    {
      gimple_seq stmts = NULL;
      cached = force_gimple_operand (unshare_expr (expr),
				     &stmts, true, NULL_TREE);
      if (stmts)
	{
	  edge e = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));
	  gsi_insert_seq_on_edge_immediate (e, stmts);
	}
    }
  return cached;
}

/* Return true if we can use CMP_TYPE as the comparison type to produce
   all masks required to mask LOOP_VINFO.  */

static bool
can_produce_all_loop_masks_p (loop_vec_info loop_vinfo, tree cmp_type)
{
  rgroup_masks *rgm;
  unsigned int i;
  FOR_EACH_VEC_ELT (LOOP_VINFO_MASKS (loop_vinfo), i, rgm)
    if (rgm->mask_type != NULL_TREE
	&& !direct_internal_fn_supported_p (IFN_WHILE_ULT,
					    cmp_type, rgm->mask_type,
					    OPTIMIZE_FOR_SPEED))
      return false;
  return true;
}

/* Calculate the maximum number of scalars per iteration for every
   rgroup in LOOP_VINFO.  */

static unsigned int
vect_get_max_nscalars_per_iter (loop_vec_info loop_vinfo)
{
  unsigned int res = 1;
  unsigned int i;
  rgroup_masks *rgm;
  FOR_EACH_VEC_ELT (LOOP_VINFO_MASKS (loop_vinfo), i, rgm)
    res = MAX (res, rgm->max_nscalars_per_iter);
  return res;
}

/* Each statement in LOOP_VINFO can be masked where necessary.  Check
   whether we can actually generate the masks required.  Return true if so,
   storing the type of the scalar IV in LOOP_VINFO_MASK_COMPARE_TYPE.  */

static bool
vect_verify_full_masking (loop_vec_info loop_vinfo)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  unsigned int min_ni_width;
  unsigned int max_nscalars_per_iter
    = vect_get_max_nscalars_per_iter (loop_vinfo);

  /* Use a normal loop if there are no statements that need masking.
     This only happens in rare degenerate cases: it means that the loop
     has no loads, no stores, and no live-out values.  */
  if (LOOP_VINFO_MASKS (loop_vinfo).is_empty ())
    return false;

  /* Get the maximum number of iterations that is representable
     in the counter type.  */
  tree ni_type = TREE_TYPE (LOOP_VINFO_NITERSM1 (loop_vinfo));
  widest_int max_ni = wi::to_widest (TYPE_MAX_VALUE (ni_type)) + 1;

  /* Get a more refined estimate for the number of iterations.  */
  widest_int max_back_edges;
  if (max_loop_iterations (loop, &max_back_edges))
    max_ni = wi::smin (max_ni, max_back_edges + 1);

  /* Account for rgroup masks, in which each bit is replicated N times.  */
  max_ni *= max_nscalars_per_iter;

  /* Work out how many bits we need to represent the limit.  */
  min_ni_width = wi::min_precision (max_ni, UNSIGNED);

  /* Find a scalar mode for which WHILE_ULT is supported.  */
  opt_scalar_int_mode cmp_mode_iter;
  tree cmp_type = NULL_TREE;
  tree iv_type = NULL_TREE;
  widest_int iv_limit = vect_iv_limit_for_full_masking (loop_vinfo);
  unsigned int iv_precision = UINT_MAX;

  if (iv_limit != -1)
    iv_precision = wi::min_precision (iv_limit * max_nscalars_per_iter,
				      UNSIGNED);

  FOR_EACH_MODE_IN_CLASS (cmp_mode_iter, MODE_INT)
    {
      unsigned int cmp_bits = GET_MODE_BITSIZE (cmp_mode_iter.require ());
      if (cmp_bits >= min_ni_width
	  && targetm.scalar_mode_supported_p (cmp_mode_iter.require ()))
	{
	  tree this_type = build_nonstandard_integer_type (cmp_bits, true);
	  if (this_type
	      && can_produce_all_loop_masks_p (loop_vinfo, this_type))
	    {
	      /* Although we could stop as soon as we find a valid mode,
		 there are at least two reasons why that's not always the
		 best choice:

		 - An IV that's Pmode or wider is more likely to be reusable
		   in address calculations than an IV that's narrower than
		   Pmode.

		 - Doing the comparison in IV_PRECISION or wider allows
		   a natural 0-based IV, whereas using a narrower comparison
		   type requires mitigations against wrap-around.

		 Conversely, if the IV limit is variable, doing the comparison
		 in a wider type than the original type can introduce
		 unnecessary extensions, so picking the widest valid mode
		 is not always a good choice either.

		 Here we prefer the first IV type that's Pmode or wider,
		 and the first comparison type that's IV_PRECISION or wider.
		 (The comparison type must be no wider than the IV type,
		 to avoid extensions in the vector loop.)

		 ??? We might want to try continuing beyond Pmode for ILP32
		 targets if CMP_BITS < IV_PRECISION.  */
	      iv_type = this_type;
	      if (!cmp_type || iv_precision > TYPE_PRECISION (cmp_type))
		cmp_type = this_type;
	      if (cmp_bits >= GET_MODE_BITSIZE (Pmode))
		break;
	    }
	}
    }

  if (!cmp_type)
    return false;

  LOOP_VINFO_MASK_COMPARE_TYPE (loop_vinfo) = cmp_type;
  LOOP_VINFO_MASK_IV_TYPE (loop_vinfo) = iv_type;
  return true;
}

/* Calculate the cost of one scalar iteration of the loop.  */
static void
vect_compute_single_scalar_iteration_cost (loop_vec_info loop_vinfo)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes, factor;
  int innerloop_iters, i;

  DUMP_VECT_SCOPE ("vect_compute_single_scalar_iteration_cost");

  /* Gather costs for statements in the scalar loop.  */

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
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (stmt);

          if (!is_gimple_assign (stmt) && !is_gimple_call (stmt))
            continue;

          /* Skip stmts that are not vectorized inside the loop.  */
	  stmt_vec_info vstmt_info = vect_stmt_to_vectorize (stmt_info);
          if (!STMT_VINFO_RELEVANT_P (vstmt_info)
              && (!STMT_VINFO_LIVE_P (vstmt_info)
                  || !VECTORIZABLE_CYCLE_DEF
			(STMT_VINFO_DEF_TYPE (vstmt_info))))
            continue;

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

	  record_stmt_cost (&LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo),
			    factor, kind, stmt_info, 0, vect_prologue);
        }
    }

  /* Now accumulate cost.  */
  void *target_cost_data = init_cost (loop);
  stmt_info_for_cost *si;
  int j;
  FOR_EACH_VEC_ELT (LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo),
		    j, si)
    (void) add_stmt_cost (target_cost_data, si->count,
			  si->kind, si->stmt_info, si->misalign,
			  vect_body);
  unsigned dummy, body_cost = 0;
  finish_cost (target_cost_data, &dummy, &body_cost, &dummy);
  destroy_cost_data (target_cost_data);
  LOOP_VINFO_SINGLE_SCALAR_ITERATION_COST (loop_vinfo) = body_cost;
}


/* Function vect_analyze_loop_form_1.

   Verify that certain CFG restrictions hold, including:
   - the loop has a pre-header
   - the loop has a single entry and exit
   - the loop exit condition is simple enough
   - the number of iterations can be analyzed, i.e, a countable loop.  The
     niter could be analyzed under some assumptions.  */

opt_result
vect_analyze_loop_form_1 (class loop *loop, gcond **loop_cond,
			  tree *assumptions, tree *number_of_iterationsm1,
			  tree *number_of_iterations, gcond **inner_loop_cond)
{
  DUMP_VECT_SCOPE ("vect_analyze_loop_form");

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
	return opt_result::failure_at (vect_location,
				       "not vectorized:"
				       " control flow in loop.\n");

      if (empty_block_p (loop->header))
	return opt_result::failure_at (vect_location,
				       "not vectorized: empty loop.\n");
    }
  else
    {
      class loop *innerloop = loop->inner;
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
	return opt_result::failure_at (vect_location,
				       "not vectorized:"
				       " multiple nested loops.\n");

      if (loop->num_nodes != 5)
	return opt_result::failure_at (vect_location,
				       "not vectorized:"
				       " control flow in loop.\n");

      entryedge = loop_preheader_edge (innerloop);
      if (entryedge->src != loop->header
	  || !single_exit (innerloop)
	  || single_exit (innerloop)->dest != EDGE_PRED (loop->latch, 0)->src)
	return opt_result::failure_at (vect_location,
				       "not vectorized:"
				       " unsupported outerloop form.\n");

      /* Analyze the inner-loop.  */
      tree inner_niterm1, inner_niter, inner_assumptions;
      opt_result res
	= vect_analyze_loop_form_1 (loop->inner, inner_loop_cond,
				    &inner_assumptions, &inner_niterm1,
				    &inner_niter, NULL);
      if (!res)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: Bad inner loop.\n");
	  return res;
	}

      /* Don't support analyzing niter under assumptions for inner
	 loop.  */
      if (!integer_onep (inner_assumptions))
	return opt_result::failure_at (vect_location,
				       "not vectorized: Bad inner loop.\n");

      if (!expr_invariant_in_loop_p (loop, inner_niter))
	return opt_result::failure_at (vect_location,
				       "not vectorized: inner-loop count not"
				       " invariant.\n");

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "Considering outer-loop vectorization.\n");
    }

  if (!single_exit (loop))
    return opt_result::failure_at (vect_location,
				   "not vectorized: multiple exits.\n");
  if (EDGE_COUNT (loop->header->preds) != 2)
    return opt_result::failure_at (vect_location,
				   "not vectorized:"
				   " too many incoming edges.\n");

  /* We assume that the loop exit condition is at the end of the loop. i.e,
     that the loop is represented as a do-while (with a proper if-guard
     before the loop if needed), where the loop header contains all the
     executable statements, and the latch is empty.  */
  if (!empty_block_p (loop->latch)
      || !gimple_seq_empty_p (phi_nodes (loop->latch)))
    return opt_result::failure_at (vect_location,
				   "not vectorized: latch block not empty.\n");

  /* Make sure the exit is not abnormal.  */
  edge e = single_exit (loop);
  if (e->flags & EDGE_ABNORMAL)
    return opt_result::failure_at (vect_location,
				   "not vectorized:"
				   " abnormal loop exit edge.\n");

  *loop_cond = vect_get_loop_niters (loop, assumptions, number_of_iterations,
				     number_of_iterationsm1);
  if (!*loop_cond)
    return opt_result::failure_at
      (vect_location,
       "not vectorized: complicated exit condition.\n");

  if (integer_zerop (*assumptions)
      || !*number_of_iterations
      || chrec_contains_undetermined (*number_of_iterations))
    return opt_result::failure_at
      (*loop_cond,
       "not vectorized: number of iterations cannot be computed.\n");

  if (integer_zerop (*number_of_iterations))
    return opt_result::failure_at
      (*loop_cond,
       "not vectorized: number of iterations = 0.\n");

  return opt_result::success ();
}

/* Analyze LOOP form and return a loop_vec_info if it is of suitable form.  */

opt_loop_vec_info
vect_analyze_loop_form (class loop *loop, vec_info_shared *shared)
{
  tree assumptions, number_of_iterations, number_of_iterationsm1;
  gcond *loop_cond, *inner_loop_cond = NULL;

  opt_result res
    = vect_analyze_loop_form_1 (loop, &loop_cond,
				&assumptions, &number_of_iterationsm1,
				&number_of_iterations, &inner_loop_cond);
  if (!res)
    return opt_loop_vec_info::propagate_failure (res);

  loop_vec_info loop_vinfo = new _loop_vec_info (loop, shared);
  LOOP_VINFO_NITERSM1 (loop_vinfo) = number_of_iterationsm1;
  LOOP_VINFO_NITERS (loop_vinfo) = number_of_iterations;
  LOOP_VINFO_NITERS_UNCHANGED (loop_vinfo) = number_of_iterations;
  if (!integer_onep (assumptions))
    {
      /* We consider to vectorize this loop by versioning it under
	 some assumptions.  In order to do this, we need to clear
	 existing information computed by scev and niter analyzer.  */
      scev_reset_htab ();
      free_numbers_of_iterations_estimates (loop);
      /* Also set flag for this loop so that following scev and niter
	 analysis are done under the assumptions.  */
      loop_constraint_set (loop, LOOP_C_FINITE);
      /* Also record the assumptions for versioning.  */
      LOOP_VINFO_NITERS_ASSUMPTIONS (loop_vinfo) = assumptions;
    }

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

  stmt_vec_info loop_cond_info = loop_vinfo->lookup_stmt (loop_cond);
  STMT_VINFO_TYPE (loop_cond_info) = loop_exit_ctrl_vec_info_type;
  if (inner_loop_cond)
    {
      stmt_vec_info inner_loop_cond_info
	= loop_vinfo->lookup_stmt (inner_loop_cond);
      STMT_VINFO_TYPE (inner_loop_cond_info) = loop_exit_ctrl_vec_info_type;
    }

  gcc_assert (!loop->aux);
  loop->aux = loop_vinfo;
  return opt_loop_vec_info::success (loop_vinfo);
}



/* Scan the loop stmts and dependent on whether there are any (non-)SLP
   statements update the vectorization factor.  */

static void
vect_update_vf_for_slp (loop_vec_info loop_vinfo)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  poly_uint64 vectorization_factor;
  int i;

  DUMP_VECT_SCOPE ("vect_update_vf_for_slp");

  vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  gcc_assert (known_ne (vectorization_factor, 0U));

  /* If all the stmts in the loop can be SLPed, we perform only SLP, and
     vectorization factor of the loop is the unrolling factor required by
     the SLP instances.  If that unrolling factor is 1, we say, that we
     perform pure SLP on loop - cross iteration parallelism is not
     exploited.  */
  bool only_slp_in_loop = true;
  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (si.phi ());
	  if (!stmt_info)
	    continue;
	  if ((STMT_VINFO_RELEVANT_P (stmt_info)
	       || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (stmt_info)))
	      && !PURE_SLP_STMT (stmt_info))
	    /* STMT needs both SLP and loop-based vectorization.  */
	    only_slp_in_loop = false;
	}
      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (gsi_stmt (si));
	  stmt_info = vect_stmt_to_vectorize (stmt_info);
	  if ((STMT_VINFO_RELEVANT_P (stmt_info)
	       || VECTORIZABLE_CYCLE_DEF (STMT_VINFO_DEF_TYPE (stmt_info)))
	      && !PURE_SLP_STMT (stmt_info))
	    /* STMT needs both SLP and loop-based vectorization.  */
	    only_slp_in_loop = false;
	}
    }

  if (only_slp_in_loop)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Loop contains only SLP stmts\n");
      vectorization_factor = LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo);
    }
  else
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Loop contains SLP and non-SLP stmts\n");
      /* Both the vectorization factor and unroll factor have the form
	 GET_MODE_SIZE (loop_vinfo->vector_mode) * X for some rational X,
	 so they must have a common multiple.  */
      vectorization_factor
	= force_common_multiple (vectorization_factor,
				 LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo));
    }

  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "Updating vectorization factor to ");
      dump_dec (MSG_NOTE, vectorization_factor);
      dump_printf (MSG_NOTE, ".\n");
    }
}

/* Return true if STMT_INFO describes a double reduction phi and if
   the other phi in the reduction is also relevant for vectorization.
   This rejects cases such as:

      outer1:
	x_1 = PHI <x_3(outer2), ...>;
	...

      inner:
	x_2 = ...;
	...

      outer2:
	x_3 = PHI <x_2(inner)>;

   if nothing in x_2 or elsewhere makes x_1 relevant.  */

static bool
vect_active_double_reduction_p (stmt_vec_info stmt_info)
{
  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_double_reduction_def)
    return false;

  return STMT_VINFO_RELEVANT_P (STMT_VINFO_REDUC_DEF (stmt_info));
}

/* Function vect_analyze_loop_operations.

   Scan the loop stmts and make sure they are all vectorizable.  */

static opt_result
vect_analyze_loop_operations (loop_vec_info loop_vinfo)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  int i;
  stmt_vec_info stmt_info;
  bool need_to_vectorize = false;
  bool ok;

  DUMP_VECT_SCOPE ("vect_analyze_loop_operations");

  auto_vec<stmt_info_for_cost> cost_vec;

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
        {
          gphi *phi = si.phi ();
          ok = true;

	  stmt_info = loop_vinfo->lookup_stmt (phi);
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "examining phi: %G", phi);
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
              if (STMT_VINFO_LIVE_P (stmt_info)
		  && !vect_active_double_reduction_p (stmt_info))
		return opt_result::failure_at (phi,
					       "Unsupported loop-closed phi"
					       " in outer-loop.\n");

              /* If PHI is used in the outer loop, we check that its operand
                 is defined in the inner loop.  */
              if (STMT_VINFO_RELEVANT_P (stmt_info))
                {
                  tree phi_op;

                  if (gimple_phi_num_args (phi) != 1)
                    return opt_result::failure_at (phi, "unsupported phi");

                  phi_op = PHI_ARG_DEF (phi, 0);
		  stmt_vec_info op_def_info = loop_vinfo->lookup_def (phi_op);
		  if (!op_def_info)
		    return opt_result::failure_at (phi, "unsupported phi\n");

		  if (STMT_VINFO_RELEVANT (op_def_info) != vect_used_in_outer
		      && (STMT_VINFO_RELEVANT (op_def_info)
			  != vect_used_in_outer_by_reduction))
		    return opt_result::failure_at (phi, "unsupported phi\n");

		  if ((STMT_VINFO_DEF_TYPE (stmt_info) == vect_internal_def
		       || (STMT_VINFO_DEF_TYPE (stmt_info)
			   == vect_double_reduction_def))
		      && !vectorizable_lc_phi (stmt_info, NULL, NULL))
		    return opt_result::failure_at (phi, "unsupported phi\n");
                }

              continue;
            }

          gcc_assert (stmt_info);

          if ((STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope
               || STMT_VINFO_LIVE_P (stmt_info))
              && STMT_VINFO_DEF_TYPE (stmt_info) != vect_induction_def)
	    /* A scalar-dependence cycle that we don't support.  */
	    return opt_result::failure_at (phi,
					   "not vectorized:"
					   " scalar dependence cycle.\n");

          if (STMT_VINFO_RELEVANT_P (stmt_info))
            {
              need_to_vectorize = true;
              if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def
		  && ! PURE_SLP_STMT (stmt_info))
		ok = vectorizable_induction (stmt_info, NULL, NULL, NULL,
					     &cost_vec);
	      else if ((STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def
			|| (STMT_VINFO_DEF_TYPE (stmt_info)
			    == vect_double_reduction_def)
			|| STMT_VINFO_DEF_TYPE (stmt_info) == vect_nested_cycle)
		       && ! PURE_SLP_STMT (stmt_info))
		ok = vectorizable_reduction (stmt_info, NULL, NULL, &cost_vec);
            }

	  /* SLP PHIs are tested by vect_slp_analyze_node_operations.  */
	  if (ok
	      && STMT_VINFO_LIVE_P (stmt_info)
	      && !PURE_SLP_STMT (stmt_info))
	    ok = vectorizable_live_operation (stmt_info, NULL, NULL, NULL,
					      -1, false, &cost_vec);

          if (!ok)
	    return opt_result::failure_at (phi,
					   "not vectorized: relevant phi not "
					   "supported: %G",
					   static_cast <gimple *> (phi));
        }

      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
        {
	  gimple *stmt = gsi_stmt (si);
	  if (!gimple_clobber_p (stmt))
	    {
	      opt_result res
		= vect_analyze_stmt (loop_vinfo->lookup_stmt (stmt),
				     &need_to_vectorize,
				     NULL, NULL, &cost_vec);
	      if (!res)
		return res;
	    }
        }
    } /* bbs */

  add_stmt_costs (loop_vinfo->target_cost_data, &cost_vec);

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
      return opt_result::failure_at
	(vect_location,
	 "not vectorized: redundant loop. no profit to vectorize.\n");
    }

  return opt_result::success ();
}

/* Analyze the cost of the loop described by LOOP_VINFO.  Decide if it
   is worthwhile to vectorize.  Return 1 if definitely yes, 0 if
   definitely no, or -1 if it's worth retrying.  */

static int
vect_analyze_loop_costing (loop_vec_info loop_vinfo)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  unsigned int assumed_vf = vect_vf_for_cost (loop_vinfo);

  /* Only fully-masked loops can have iteration counts less than the
     vectorization factor.  */
  if (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      HOST_WIDE_INT max_niter;

      if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
	max_niter = LOOP_VINFO_INT_NITERS (loop_vinfo);
      else
	max_niter = max_stmt_executions_int (loop);

      if (max_niter != -1
	  && (unsigned HOST_WIDE_INT) max_niter < assumed_vf)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: iteration count smaller than "
			     "vectorization factor.\n");
	  return 0;
	}
    }

  int min_profitable_iters, min_profitable_estimate;
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
      return -1;
    }

  int min_scalar_loop_bound = (param_min_vect_loop_bound
			       * assumed_vf);

  /* Use the cost model only if it is more conservative than user specified
     threshold.  */
  unsigned int th = (unsigned) MAX (min_scalar_loop_bound,
				    min_profitable_iters);

  LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo) = th;

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && LOOP_VINFO_INT_NITERS (loop_vinfo) < th)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: vectorization not profitable.\n");
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "not vectorized: iteration count smaller than user "
			 "specified loop bound parameter or minimum profitable "
			 "iterations (whichever is more conservative).\n");
      return 0;
    }

  /* The static profitablity threshold min_profitable_estimate includes
     the cost of having to check at runtime whether the scalar loop
     should be used instead.  If it turns out that we don't need or want
     such a check, the threshold we should use for the static estimate
     is simply the point at which the vector loop becomes more profitable
     than the scalar loop.  */
  if (min_profitable_estimate > min_profitable_iters
      && !LOOP_REQUIRES_VERSIONING (loop_vinfo)
      && !LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo)
      && !LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo)
      && !vect_apply_runtime_profitability_check_p (loop_vinfo))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "no need for a runtime"
			 " choice between the scalar and vector loops\n");
      min_profitable_estimate = min_profitable_iters;
    }

  HOST_WIDE_INT estimated_niter;

  /* If we are vectorizing an epilogue then we know the maximum number of
     scalar iterations it will cover is at least one lower than the
     vectorization factor of the main loop.  */
  if (LOOP_VINFO_EPILOGUE_P (loop_vinfo))
    estimated_niter
      = vect_vf_for_cost (LOOP_VINFO_ORIG_LOOP_INFO (loop_vinfo)) - 1;
  else
    {
      estimated_niter = estimated_stmt_executions_int (loop);
      if (estimated_niter == -1)
	estimated_niter = likely_max_stmt_executions_int (loop);
    }
  if (estimated_niter != -1
      && ((unsigned HOST_WIDE_INT) estimated_niter
	  < MAX (th, (unsigned) min_profitable_estimate)))
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
      return -1;
    }

  return 1;
}

static opt_result
vect_get_datarefs_in_loop (loop_p loop, basic_block *bbs,
			   vec<data_reference_p> *datarefs,
			   unsigned int *n_stmts)
{
  *n_stmts = 0;
  for (unsigned i = 0; i < loop->num_nodes; i++)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (is_gimple_debug (stmt))
	  continue;
	++(*n_stmts);
	opt_result res = vect_find_stmt_data_reference (loop, stmt, datarefs);
	if (!res)
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
	    return res;
	  }
	/* If dependence analysis will give up due to the limit on the
	   number of datarefs stop here and fail fatally.  */
	if (datarefs->length ()
	    > (unsigned)param_loop_max_datarefs_for_datadeps)
	  return opt_result::failure_at (stmt, "exceeded param "
					 "loop-max-datarefs-for-datadeps\n");
      }
  return opt_result::success ();
}

/* Look for SLP-only access groups and turn each individual access into its own
   group.  */
static void
vect_dissolve_slp_only_groups (loop_vec_info loop_vinfo)
{
  unsigned int i;
  struct data_reference *dr;

  DUMP_VECT_SCOPE ("vect_dissolve_slp_only_groups");

  vec<data_reference_p> datarefs = loop_vinfo->shared->datarefs;
  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      gcc_assert (DR_REF (dr));
      stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (DR_STMT (dr));

      /* Check if the load is a part of an interleaving chain.  */
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
	{
	  stmt_vec_info first_element = DR_GROUP_FIRST_ELEMENT (stmt_info);
	  unsigned int group_size = DR_GROUP_SIZE (first_element);

	  /* Check if SLP-only groups.  */
	  if (!STMT_SLP_TYPE (stmt_info)
	      && STMT_VINFO_SLP_VECT_ONLY (first_element))
	    {
	      /* Dissolve the group.  */
	      STMT_VINFO_SLP_VECT_ONLY (first_element) = false;

	      stmt_vec_info vinfo = first_element;
	      while (vinfo)
		{
		  stmt_vec_info next = DR_GROUP_NEXT_ELEMENT (vinfo);
		  DR_GROUP_FIRST_ELEMENT (vinfo) = vinfo;
		  DR_GROUP_NEXT_ELEMENT (vinfo) = NULL;
		  DR_GROUP_SIZE (vinfo) = 1;
		  if (STMT_VINFO_STRIDED_P (first_element))
		    DR_GROUP_GAP (vinfo) = 0;
		  else
		    DR_GROUP_GAP (vinfo) = group_size - 1;
		  vinfo = next;
		}
	    }
	}
    }
}


/* Decides whether we need to create an epilogue loop to handle
   remaining scalar iterations and sets PEELING_FOR_NITERS accordingly.  */

void
determine_peel_for_niter (loop_vec_info loop_vinfo)
{
  LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = false;

  unsigned HOST_WIDE_INT const_vf;
  HOST_WIDE_INT max_niter
    = likely_max_stmt_executions_int (LOOP_VINFO_LOOP (loop_vinfo));

  unsigned th = LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo);
  if (!th && LOOP_VINFO_ORIG_LOOP_INFO (loop_vinfo))
    th = LOOP_VINFO_COST_MODEL_THRESHOLD (LOOP_VINFO_ORIG_LOOP_INFO
					  (loop_vinfo));

  if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    /* The main loop handles all iterations.  */
    LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = false;
  else if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
	   && LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) >= 0)
    {
      /* Work out the (constant) number of iterations that need to be
	 peeled for reasons other than niters.  */
      unsigned int peel_niter = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
      if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
	peel_niter += 1;
      if (!multiple_p (LOOP_VINFO_INT_NITERS (loop_vinfo) - peel_niter,
		       LOOP_VINFO_VECT_FACTOR (loop_vinfo)))
	LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = true;
    }
  else if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo)
	   /* ??? When peeling for gaps but not alignment, we could
	      try to check whether the (variable) niters is known to be
	      VF * N + 1.  That's something of a niche case though.  */
	   || LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo)
	   || !LOOP_VINFO_VECT_FACTOR (loop_vinfo).is_constant (&const_vf)
	   || ((tree_ctz (LOOP_VINFO_NITERS (loop_vinfo))
		< (unsigned) exact_log2 (const_vf))
	       /* In case of versioning, check if the maximum number of
		  iterations is greater than th.  If they are identical,
		  the epilogue is unnecessary.  */
	       && (!LOOP_REQUIRES_VERSIONING (loop_vinfo)
		   || ((unsigned HOST_WIDE_INT) max_niter
		       > (th / const_vf) * const_vf))))
    LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = true;
}


/* Function vect_analyze_loop_2.

   Apply a set of analyses on LOOP, and create a loop_vec_info struct
   for it.  The different analyses will record information in the
   loop_vec_info struct.  */
static opt_result
vect_analyze_loop_2 (loop_vec_info loop_vinfo, bool &fatal, unsigned *n_stmts)
{
  opt_result ok = opt_result::success ();
  int res;
  unsigned int max_vf = MAX_VECTORIZATION_FACTOR;
  poly_uint64 min_vf = 2;
  loop_vec_info orig_loop_vinfo = NULL;

  /* If we are dealing with an epilogue then orig_loop_vinfo points to the
     loop_vec_info of the first vectorized loop.  */
  if (LOOP_VINFO_EPILOGUE_P (loop_vinfo))
    orig_loop_vinfo = LOOP_VINFO_ORIG_LOOP_INFO (loop_vinfo);
  else
    orig_loop_vinfo = loop_vinfo;
  gcc_assert (orig_loop_vinfo);

  /* The first group of checks is independent of the vector size.  */
  fatal = true;

  if (LOOP_VINFO_SIMD_IF_COND (loop_vinfo)
      && integer_zerop (LOOP_VINFO_SIMD_IF_COND (loop_vinfo)))
    return opt_result::failure_at (vect_location,
				   "not vectorized: simd if(0)\n");

  /* Find all data references in the loop (which correspond to vdefs/vuses)
     and analyze their evolution in the loop.  */

  loop_p loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* Gather the data references and count stmts in the loop.  */
  if (!LOOP_VINFO_DATAREFS (loop_vinfo).exists ())
    {
      opt_result res
	= vect_get_datarefs_in_loop (loop, LOOP_VINFO_BBS (loop_vinfo),
				     &LOOP_VINFO_DATAREFS (loop_vinfo),
				     n_stmts);
      if (!res)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: loop contains function "
			     "calls or data references that cannot "
			     "be analyzed\n");
	  return res;
	}
      loop_vinfo->shared->save_datarefs ();
    }
  else
    loop_vinfo->shared->check_datarefs ();

  /* Analyze the data references and also adjust the minimal
     vectorization factor according to the loads and stores.  */

  ok = vect_analyze_data_refs (loop_vinfo, &min_vf, &fatal);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data references.\n");
      return ok;
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
      return ok;
    }

  /* Data-flow analysis to detect stmts that do not need to be vectorized.  */

  ok = vect_mark_stmts_to_be_vectorized (loop_vinfo, &fatal);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "unexpected pattern.\n");
      return ok;
    }

  /* While the rest of the analysis below depends on it in some way.  */
  fatal = false;

  /* Analyze data dependences between the data-refs in the loop
     and adjust the maximum vectorization factor according to
     the dependences.
     FORNOW: fail at the first data dependence that we encounter.  */

  ok = vect_analyze_data_ref_dependences (loop_vinfo, &max_vf);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data dependence.\n");
      return ok;
    }
  if (max_vf != MAX_VECTORIZATION_FACTOR
      && maybe_lt (max_vf, min_vf))
    return opt_result::failure_at (vect_location, "bad data dependence.\n");
  LOOP_VINFO_MAX_VECT_FACTOR (loop_vinfo) = max_vf;

  ok = vect_determine_vectorization_factor (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't determine vectorization factor.\n");
      return ok;
    }
  if (max_vf != MAX_VECTORIZATION_FACTOR
      && maybe_lt (max_vf, LOOP_VINFO_VECT_FACTOR (loop_vinfo)))
    return opt_result::failure_at (vect_location, "bad data dependence.\n");

  /* Compute the scalar iteration cost.  */
  vect_compute_single_scalar_iteration_cost (loop_vinfo);

  poly_uint64 saved_vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  /* Check the SLP opportunities in the loop, analyze and build SLP trees.  */
  ok = vect_analyze_slp (loop_vinfo, *n_stmts);
  if (!ok)
    return ok;

  /* If there are any SLP instances mark them as pure_slp.  */
  bool slp = vect_make_slp_decision (loop_vinfo);
  if (slp)
    {
      /* Find stmts that need to be both vectorized and SLPed.  */
      vect_detect_hybrid_slp (loop_vinfo);

      /* Update the vectorization factor based on the SLP decision.  */
      vect_update_vf_for_slp (loop_vinfo);
    }

  bool saved_can_fully_mask_p = LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo);

  /* We don't expect to have to roll back to anything other than an empty
     set of rgroups.  */
  gcc_assert (LOOP_VINFO_MASKS (loop_vinfo).is_empty ());

  /* This is the point where we can re-start analysis with SLP forced off.  */
start_over:

  /* Now the vectorization factor is final.  */
  poly_uint64 vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  gcc_assert (known_ne (vectorization_factor, 0U));

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo) && dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "vectorization_factor = ");
      dump_dec (MSG_NOTE, vectorization_factor);
      dump_printf (MSG_NOTE, ", niters = %wd\n",
		   LOOP_VINFO_INT_NITERS (loop_vinfo));
    }

  /* Analyze the alignment of the data-refs in the loop.
     Fail if a data reference is found that cannot be vectorized.  */

  ok = vect_analyze_data_refs_alignment (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data alignment.\n");
      return ok;
    }

  /* Prune the list of ddrs to be tested at run-time by versioning for alias.
     It is important to call pruning after vect_analyze_data_ref_accesses,
     since we use grouping information gathered by interleaving analysis.  */
  ok = vect_prune_runtime_alias_test_list (loop_vinfo);
  if (!ok)
    return ok;

  /* Do not invoke vect_enhance_data_refs_alignment for epilogue
     vectorization, since we do not want to add extra peeling or
     add versioning for alignment.  */
  if (!LOOP_VINFO_EPILOGUE_P (loop_vinfo))
    /* This pass will decide on using loop versioning and/or loop peeling in
       order to enhance the alignment of data references in the loop.  */
    ok = vect_enhance_data_refs_alignment (loop_vinfo);
  else
    ok = vect_verify_datarefs_alignment (loop_vinfo);
  if (!ok)
    return ok;

  if (slp)
    {
      /* Analyze operations in the SLP instances.  Note this may
	 remove unsupported SLP instances which makes the above
	 SLP kind detection invalid.  */
      unsigned old_size = LOOP_VINFO_SLP_INSTANCES (loop_vinfo).length ();
      vect_slp_analyze_operations (loop_vinfo);
      if (LOOP_VINFO_SLP_INSTANCES (loop_vinfo).length () != old_size)
	{
	  ok = opt_result::failure_at (vect_location,
				       "unsupported SLP instances\n");
	  goto again;
	}
    }

  /* Dissolve SLP-only groups.  */
  vect_dissolve_slp_only_groups (loop_vinfo);

  /* Scan all the remaining operations in the loop that are not subject
     to SLP and make sure they are vectorizable.  */
  ok = vect_analyze_loop_operations (loop_vinfo);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad operation or unsupported loop bound.\n");
      return ok;
    }

  /* Decide whether to use a fully-masked loop for this vectorization
     factor.  */
  LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
    = (LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo)
       && vect_verify_full_masking (loop_vinfo));
  if (dump_enabled_p ())
    {
      if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
	dump_printf_loc (MSG_NOTE, vect_location,
			 "using a fully-masked loop.\n");
      else
	dump_printf_loc (MSG_NOTE, vect_location,
			 "not using a fully-masked loop.\n");
    }

  /* If epilog loop is required because of data accesses with gaps,
     one additional iteration needs to be peeled.  Check if there is
     enough iterations for vectorization.  */
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo)
      && LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      && !LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
      tree scalar_niters = LOOP_VINFO_NITERSM1 (loop_vinfo);

      if (known_lt (wi::to_widest (scalar_niters), vf))
	return opt_result::failure_at (vect_location,
				       "loop has no enough iterations to"
				       " support peeling for gaps.\n");
    }

  /* If we're vectorizing an epilogue loop, we either need a fully-masked
     loop or a loop that has a lower VF than the main loop.  */
  if (LOOP_VINFO_EPILOGUE_P (loop_vinfo)
      && !LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
      && maybe_ge (LOOP_VINFO_VECT_FACTOR (loop_vinfo),
		   LOOP_VINFO_VECT_FACTOR (orig_loop_vinfo)))
    return opt_result::failure_at (vect_location,
				   "Vectorization factor too high for"
				   " epilogue loop.\n");

  /* Check the costings of the loop make vectorizing worthwhile.  */
  res = vect_analyze_loop_costing (loop_vinfo);
  if (res < 0)
    {
      ok = opt_result::failure_at (vect_location,
				   "Loop costings may not be worthwhile.\n");
      goto again;
    }
  if (!res)
    return opt_result::failure_at (vect_location,
				   "Loop costings not worthwhile.\n");

  determine_peel_for_niter (loop_vinfo);
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
	  ok = opt_result::failure_at (vect_location,
				       "not vectorized: can't create required "
				       "epilog loop\n");
          goto again;
        }
    }

  /* During peeling, we need to check if number of loop iterations is
     enough for both peeled prolog loop and vector loop.  This check
     can be merged along with threshold check of loop versioning, so
     increase threshold for this case if necessary.

     If we are analyzing an epilogue we still want to check what its
     versioning threshold would be.  If we decide to vectorize the epilogues we
     will want to use the lowest versioning threshold of all epilogues and main
     loop.  This will enable us to enter a vectorized epilogue even when
     versioning the loop.  We can't simply check whether the epilogue requires
     versioning though since we may have skipped some versioning checks when
     analyzing the epilogue.  For instance, checks for alias versioning will be
     skipped when dealing with epilogues as we assume we already checked them
     for the main loop.  So instead we always check the 'orig_loop_vinfo'.  */
  if (LOOP_REQUIRES_VERSIONING (orig_loop_vinfo))
    {
      poly_uint64 niters_th = 0;
      unsigned int th = LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo);

      if (!vect_use_loop_mask_for_alignment_p (loop_vinfo))
	{
	  /* Niters for peeled prolog loop.  */
	  if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) < 0)
	    {
	      dr_vec_info *dr_info = LOOP_VINFO_UNALIGNED_DR (loop_vinfo);
	      tree vectype = STMT_VINFO_VECTYPE (dr_info->stmt);
	      niters_th += TYPE_VECTOR_SUBPARTS (vectype) - 1;
	    }
	  else
	    niters_th += LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
	}

      /* Niters for at least one iteration of vectorized loop.  */
      if (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
	niters_th += LOOP_VINFO_VECT_FACTOR (loop_vinfo);
      /* One additional iteration because of peeling for gap.  */
      if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
	niters_th += 1;

      /*  Use the same condition as vect_transform_loop to decide when to use
	  the cost to determine a versioning threshold.  */
      if (vect_apply_runtime_profitability_check_p (loop_vinfo)
	  && ordered_p (th, niters_th))
	niters_th = ordered_max (poly_uint64 (th), niters_th);

      LOOP_VINFO_VERSIONING_THRESHOLD (loop_vinfo) = niters_th;
    }

  gcc_assert (known_eq (vectorization_factor,
			LOOP_VINFO_VECT_FACTOR (loop_vinfo)));

  /* Ok to vectorize!  */
  return opt_result::success ();

again:
  /* Ensure that "ok" is false (with an opt_problem if dumping is enabled).  */
  gcc_assert (!ok);

  /* Try again with SLP forced off but if we didn't do any SLP there is
     no point in re-trying.  */
  if (!slp)
    return ok;

  /* If there are reduction chains re-trying will fail anyway.  */
  if (! LOOP_VINFO_REDUCTION_CHAINS (loop_vinfo).is_empty ())
    return ok;

  /* Likewise if the grouped loads or stores in the SLP cannot be handled
     via interleaving or lane instructions.  */
  slp_instance instance;
  slp_tree node;
  unsigned i, j;
  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (loop_vinfo), i, instance)
    {
      stmt_vec_info vinfo;
      vinfo = SLP_TREE_SCALAR_STMTS (SLP_INSTANCE_TREE (instance))[0];
      if (! STMT_VINFO_GROUPED_ACCESS (vinfo))
	continue;
      vinfo = DR_GROUP_FIRST_ELEMENT (vinfo);
      unsigned int size = DR_GROUP_SIZE (vinfo);
      tree vectype = STMT_VINFO_VECTYPE (vinfo);
      if (! vect_store_lanes_supported (vectype, size, false)
	 && ! known_eq (TYPE_VECTOR_SUBPARTS (vectype), 1U)
	 && ! vect_grouped_store_supported (vectype, size))
	return opt_result::failure_at (vinfo->stmt,
				       "unsupported grouped store\n");
      FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (instance), j, node)
	{
	  vinfo = SLP_TREE_SCALAR_STMTS (node)[0];
	  vinfo = DR_GROUP_FIRST_ELEMENT (vinfo);
	  bool single_element_p = !DR_GROUP_NEXT_ELEMENT (vinfo);
	  size = DR_GROUP_SIZE (vinfo);
	  vectype = STMT_VINFO_VECTYPE (vinfo);
	  if (! vect_load_lanes_supported (vectype, size, false)
	      && ! vect_grouped_load_supported (vectype, single_element_p,
						size))
	    return opt_result::failure_at (vinfo->stmt,
					   "unsupported grouped load\n");
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
    vect_free_slp_instance (instance, false);
  LOOP_VINFO_SLP_INSTANCES (loop_vinfo).release ();
  /* Reset SLP type to loop_vect on all stmts.  */
  for (i = 0; i < LOOP_VINFO_LOOP (loop_vinfo)->num_nodes; ++i)
    {
      basic_block bb = LOOP_VINFO_BBS (loop_vinfo)[i];
      for (gimple_stmt_iterator si = gsi_start_phis (bb);
	   !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (gsi_stmt (si));
	  STMT_SLP_TYPE (stmt_info) = loop_vect;
	  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def
	      || STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
	    {
	      /* vectorizable_reduction adjusts reduction stmt def-types,
		 restore them to that of the PHI.  */
	      STMT_VINFO_DEF_TYPE (STMT_VINFO_REDUC_DEF (stmt_info))
		= STMT_VINFO_DEF_TYPE (stmt_info);
	      STMT_VINFO_DEF_TYPE (vect_stmt_to_vectorize
					(STMT_VINFO_REDUC_DEF (stmt_info)))
		= STMT_VINFO_DEF_TYPE (stmt_info);
	    }
	}
      for (gimple_stmt_iterator si = gsi_start_bb (bb);
	   !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (gsi_stmt (si));
	  STMT_SLP_TYPE (stmt_info) = loop_vect;
	  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
	    {
	      gimple *pattern_def_seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_info);
	      stmt_info = STMT_VINFO_RELATED_STMT (stmt_info);
	      STMT_SLP_TYPE (stmt_info) = loop_vect;
	      for (gimple_stmt_iterator pi = gsi_start (pattern_def_seq);
		   !gsi_end_p (pi); gsi_next (&pi))
		STMT_SLP_TYPE (loop_vinfo->lookup_stmt (gsi_stmt (pi)))
		  = loop_vect;
	    }
	}
    }
  /* Free optimized alias test DDRS.  */
  LOOP_VINFO_LOWER_BOUNDS (loop_vinfo).truncate (0);
  LOOP_VINFO_COMP_ALIAS_DDRS (loop_vinfo).release ();
  LOOP_VINFO_CHECK_UNEQUAL_ADDRS (loop_vinfo).release ();
  /* Reset target cost data.  */
  destroy_cost_data (LOOP_VINFO_TARGET_COST_DATA (loop_vinfo));
  LOOP_VINFO_TARGET_COST_DATA (loop_vinfo)
    = init_cost (LOOP_VINFO_LOOP (loop_vinfo));
  /* Reset accumulated rgroup information.  */
  release_vec_loop_masks (&LOOP_VINFO_MASKS (loop_vinfo));
  /* Reset assorted flags.  */
  LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo) = false;
  LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) = false;
  LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo) = 0;
  LOOP_VINFO_VERSIONING_THRESHOLD (loop_vinfo) = 0;
  LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = saved_can_fully_mask_p;

  goto start_over;
}

/* Return true if vectorizing a loop using NEW_LOOP_VINFO appears
   to be better than vectorizing it using OLD_LOOP_VINFO.  Assume that
   OLD_LOOP_VINFO is better unless something specifically indicates
   otherwise.

   Note that this deliberately isn't a partial order.  */

static bool
vect_better_loop_vinfo_p (loop_vec_info new_loop_vinfo,
			  loop_vec_info old_loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (new_loop_vinfo);
  gcc_assert (LOOP_VINFO_LOOP (old_loop_vinfo) == loop);

  poly_int64 new_vf = LOOP_VINFO_VECT_FACTOR (new_loop_vinfo);
  poly_int64 old_vf = LOOP_VINFO_VECT_FACTOR (old_loop_vinfo);

  /* Always prefer a VF of loop->simdlen over any other VF.  */
  if (loop->simdlen)
    {
      bool new_simdlen_p = known_eq (new_vf, loop->simdlen);
      bool old_simdlen_p = known_eq (old_vf, loop->simdlen);
      if (new_simdlen_p != old_simdlen_p)
	return new_simdlen_p;
    }

  /* Limit the VFs to what is likely to be the maximum number of iterations,
     to handle cases in which at least one loop_vinfo is fully-masked.  */
  HOST_WIDE_INT estimated_max_niter = likely_max_stmt_executions_int (loop);
  if (estimated_max_niter != -1)
    {
      if (known_le (estimated_max_niter, new_vf))
	new_vf = estimated_max_niter;
      if (known_le (estimated_max_niter, old_vf))
	old_vf = estimated_max_niter;
    }

  /* Check whether the (fractional) cost per scalar iteration is lower
     or higher: new_inside_cost / new_vf vs. old_inside_cost / old_vf.  */
  poly_widest_int rel_new = (new_loop_vinfo->vec_inside_cost
			     * poly_widest_int (old_vf));
  poly_widest_int rel_old = (old_loop_vinfo->vec_inside_cost
			     * poly_widest_int (new_vf));
  if (maybe_lt (rel_old, rel_new))
    return false;
  if (known_lt (rel_new, rel_old))
    return true;

  /* If there's nothing to choose between the loop bodies, see whether
     there's a difference in the prologue and epilogue costs.  */
  if (new_loop_vinfo->vec_outside_cost != old_loop_vinfo->vec_outside_cost)
    return new_loop_vinfo->vec_outside_cost < old_loop_vinfo->vec_outside_cost;

  return false;
}

/* Decide whether to replace OLD_LOOP_VINFO with NEW_LOOP_VINFO.  Return
   true if we should.  */

static bool
vect_joust_loop_vinfos (loop_vec_info new_loop_vinfo,
			loop_vec_info old_loop_vinfo)
{
  if (!vect_better_loop_vinfo_p (new_loop_vinfo, old_loop_vinfo))
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "***** Preferring vector mode %s to vector mode %s\n",
		     GET_MODE_NAME (new_loop_vinfo->vector_mode),
		     GET_MODE_NAME (old_loop_vinfo->vector_mode));
  return true;
}

/* Function vect_analyze_loop.

   Apply a set of analyses on LOOP, and create a loop_vec_info struct
   for it.  The different analyses will record information in the
   loop_vec_info struct.  */
opt_loop_vec_info
vect_analyze_loop (class loop *loop, vec_info_shared *shared)
{
  auto_vector_modes vector_modes;

  /* Autodetect first vector size we try.  */
  unsigned int autovec_flags
    = targetm.vectorize.autovectorize_vector_modes (&vector_modes,
						    loop->simdlen != 0);
  unsigned int mode_i = 0;

  DUMP_VECT_SCOPE ("analyze_loop_nest");

  if (loop_outer (loop)
      && loop_vec_info_for_loop (loop_outer (loop))
      && LOOP_VINFO_VECTORIZABLE_P (loop_vec_info_for_loop (loop_outer (loop))))
    return opt_loop_vec_info::failure_at (vect_location,
					  "outer-loop already vectorized.\n");

  if (!find_loop_nest (loop, &shared->loop_nest))
    return opt_loop_vec_info::failure_at
      (vect_location,
       "not vectorized: loop nest containing two or more consecutive inner"
       " loops cannot be vectorized\n");

  unsigned n_stmts = 0;
  machine_mode autodetected_vector_mode = VOIDmode;
  opt_loop_vec_info first_loop_vinfo = opt_loop_vec_info::success (NULL);
  machine_mode next_vector_mode = VOIDmode;
  poly_uint64 lowest_th = 0;
  unsigned vectorized_loops = 0;
  bool pick_lowest_cost_p = ((autovec_flags & VECT_COMPARE_COSTS)
			     && !unlimited_cost_model (loop));

  bool vect_epilogues = false;
  opt_result res = opt_result::success ();
  unsigned HOST_WIDE_INT simdlen = loop->simdlen;
  while (1)
    {
      /* Check the CFG characteristics of the loop (nesting, entry/exit).  */
      opt_loop_vec_info loop_vinfo = vect_analyze_loop_form (loop, shared);
      if (!loop_vinfo)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "bad loop form.\n");
	  gcc_checking_assert (first_loop_vinfo == NULL);
	  return loop_vinfo;
	}
      loop_vinfo->vector_mode = next_vector_mode;

      bool fatal = false;

      /* When pick_lowest_cost_p is true, we should in principle iterate
	 over all the loop_vec_infos that LOOP_VINFO could replace and
	 try to vectorize LOOP_VINFO under the same conditions.
	 E.g. when trying to replace an epilogue loop, we should vectorize
	 LOOP_VINFO as an epilogue loop with the same VF limit.  When trying
	 to replace the main loop, we should vectorize LOOP_VINFO as a main
	 loop too.

	 However, autovectorize_vector_modes is usually sorted as follows:

	 - Modes that naturally produce lower VFs usually follow modes that
	   naturally produce higher VFs.

	 - When modes naturally produce the same VF, maskable modes
	   usually follow unmaskable ones, so that the maskable mode
	   can be used to vectorize the epilogue of the unmaskable mode.

	 This order is preferred because it leads to the maximum
	 epilogue vectorization opportunities.  Targets should only use
	 a different order if they want to make wide modes available while
	 disparaging them relative to earlier, smaller modes.  The assumption
	 in that case is that the wider modes are more expensive in some
	 way that isn't reflected directly in the costs.

	 There should therefore be few interesting cases in which
	 LOOP_VINFO fails when treated as an epilogue loop, succeeds when
	 treated as a standalone loop, and ends up being genuinely cheaper
	 than FIRST_LOOP_VINFO.  */
      if (vect_epilogues)
	LOOP_VINFO_ORIG_LOOP_INFO (loop_vinfo) = first_loop_vinfo;

      res = vect_analyze_loop_2 (loop_vinfo, fatal, &n_stmts);
      if (mode_i == 0)
	autodetected_vector_mode = loop_vinfo->vector_mode;
      if (dump_enabled_p ())
	{
	  if (res)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "***** Analysis succeeded with vector mode %s\n",
			     GET_MODE_NAME (loop_vinfo->vector_mode));
	  else
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "***** Analysis failed with vector mode %s\n",
			     GET_MODE_NAME (loop_vinfo->vector_mode));
	}

      loop->aux = NULL;

      if (!fatal)
	while (mode_i < vector_modes.length ()
	       && vect_chooses_same_modes_p (loop_vinfo, vector_modes[mode_i]))
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "***** The result for vector mode %s would"
			       " be the same\n",
			       GET_MODE_NAME (vector_modes[mode_i]));
	    mode_i += 1;
	  }

      if (res)
	{
	  LOOP_VINFO_VECTORIZABLE_P (loop_vinfo) = 1;
	  vectorized_loops++;

	  /* Once we hit the desired simdlen for the first time,
	     discard any previous attempts.  */
	  if (simdlen
	      && known_eq (LOOP_VINFO_VECT_FACTOR (loop_vinfo), simdlen))
	    {
	      delete first_loop_vinfo;
	      first_loop_vinfo = opt_loop_vec_info::success (NULL);
	      LOOP_VINFO_ORIG_LOOP_INFO (loop_vinfo) = NULL;
	      simdlen = 0;
	    }
	  else if (pick_lowest_cost_p && first_loop_vinfo)
	    {
	      /* Keep trying to roll back vectorization attempts while the
		 loop_vec_infos they produced were worse than this one.  */
	      vec<loop_vec_info> &vinfos = first_loop_vinfo->epilogue_vinfos;
	      while (!vinfos.is_empty ()
		     && vect_joust_loop_vinfos (loop_vinfo, vinfos.last ()))
		{
		  gcc_assert (vect_epilogues);
		  delete vinfos.pop ();
		}
	      if (vinfos.is_empty ()
		  && vect_joust_loop_vinfos (loop_vinfo, first_loop_vinfo))
		{
		  delete first_loop_vinfo;
		  first_loop_vinfo = opt_loop_vec_info::success (NULL);
		  LOOP_VINFO_ORIG_LOOP_INFO (loop_vinfo) = NULL;
		}
	    }

	  if (first_loop_vinfo == NULL)
	    {
	      first_loop_vinfo = loop_vinfo;
	      lowest_th = LOOP_VINFO_VERSIONING_THRESHOLD (first_loop_vinfo);
	    }
	  else if (vect_epilogues
		   /* For now only allow one epilogue loop.  */
		   && first_loop_vinfo->epilogue_vinfos.is_empty ())
	    {
	      first_loop_vinfo->epilogue_vinfos.safe_push (loop_vinfo);
	      poly_uint64 th = LOOP_VINFO_VERSIONING_THRESHOLD (loop_vinfo);
	      gcc_assert (!LOOP_REQUIRES_VERSIONING (loop_vinfo)
			  || maybe_ne (lowest_th, 0U));
	      /* Keep track of the known smallest versioning
		 threshold.  */
	      if (ordered_p (lowest_th, th))
		lowest_th = ordered_min (lowest_th, th);
	    }
	  else
	    delete loop_vinfo;

	  /* Only vectorize epilogues if PARAM_VECT_EPILOGUES_NOMASK is
	     enabled, SIMDUID is not set, it is the innermost loop and we have
	     either already found the loop's SIMDLEN or there was no SIMDLEN to
	     begin with.
	     TODO: Enable epilogue vectorization for loops with SIMDUID set.  */
	  vect_epilogues = (!simdlen
			    && loop->inner == NULL
			    && param_vect_epilogues_nomask
			    && LOOP_VINFO_PEELING_FOR_NITER (first_loop_vinfo)
			    && !loop->simduid
			    /* For now only allow one epilogue loop, but allow
			       pick_lowest_cost_p to replace it.  */
			    && (first_loop_vinfo->epilogue_vinfos.is_empty ()
				|| pick_lowest_cost_p));

	  /* Commit to first_loop_vinfo if we have no reason to try
	     alternatives.  */
	  if (!simdlen && !vect_epilogues && !pick_lowest_cost_p)
	    break;
	}
      else
	{
	  delete loop_vinfo;
	  if (fatal)
	    {
	      gcc_checking_assert (first_loop_vinfo == NULL);
	      break;
	    }
	}

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

      if (mode_i == vector_modes.length ()
	  || autodetected_vector_mode == VOIDmode)
	break;

      /* Try the next biggest vector size.  */
      next_vector_mode = vector_modes[mode_i++];
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "***** Re-trying analysis with vector mode %s\n",
			 GET_MODE_NAME (next_vector_mode));
    }

  if (first_loop_vinfo)
    {
      loop->aux = (loop_vec_info) first_loop_vinfo;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "***** Choosing vector mode %s\n",
			 GET_MODE_NAME (first_loop_vinfo->vector_mode));
      LOOP_VINFO_VERSIONING_THRESHOLD (first_loop_vinfo) = lowest_th;
      return first_loop_vinfo;
    }

  return opt_loop_vec_info::propagate_failure (res);
}

/* Return true if there is an in-order reduction function for CODE, storing
   it in *REDUC_FN if so.  */

static bool
fold_left_reduction_fn (tree_code code, internal_fn *reduc_fn)
{
  switch (code)
    {
    case PLUS_EXPR:
      *reduc_fn = IFN_FOLD_LEFT_PLUS;
      return true;

    default:
      return false;
    }
}

/* Function reduction_fn_for_scalar_code

   Input:
   CODE - tree_code of a reduction operations.

   Output:
   REDUC_FN - the corresponding internal function to be used to reduce the
      vector of partial results into a single scalar result, or IFN_LAST
      if the operation is a supported reduction operation, but does not have
      such an internal function.

   Return FALSE if CODE currently cannot be vectorized as reduction.  */

static bool
reduction_fn_for_scalar_code (enum tree_code code, internal_fn *reduc_fn)
{
  switch (code)
    {
      case MAX_EXPR:
        *reduc_fn = IFN_REDUC_MAX;
        return true;

      case MIN_EXPR:
        *reduc_fn = IFN_REDUC_MIN;
        return true;

      case PLUS_EXPR:
        *reduc_fn = IFN_REDUC_PLUS;
        return true;

      case BIT_AND_EXPR:
	*reduc_fn = IFN_REDUC_AND;
	return true;

      case BIT_IOR_EXPR:
	*reduc_fn = IFN_REDUC_IOR;
	return true;

      case BIT_XOR_EXPR:
	*reduc_fn = IFN_REDUC_XOR;
	return true;

      case MULT_EXPR:
      case MINUS_EXPR:
        *reduc_fn = IFN_LAST;
        return true;

      default:
       return false;
    }
}

/* If there is a neutral value X such that SLP reduction NODE would not
   be affected by the introduction of additional X elements, return that X,
   otherwise return null.  CODE is the code of the reduction and VECTOR_TYPE
   is the vector type that would hold element X.  REDUC_CHAIN is true if
   the SLP statements perform a single reduction, false if each statement
   performs an independent reduction.  */

static tree
neutral_op_for_slp_reduction (slp_tree slp_node, tree vector_type,
			      tree_code code, bool reduc_chain)
{
  vec<stmt_vec_info> stmts = SLP_TREE_SCALAR_STMTS (slp_node);
  stmt_vec_info stmt_vinfo = stmts[0];
  tree scalar_type = TREE_TYPE (vector_type);
  class loop *loop = gimple_bb (stmt_vinfo->stmt)->loop_father;
  gcc_assert (loop);

  switch (code)
    {
    case WIDEN_SUM_EXPR:
    case DOT_PROD_EXPR:
    case SAD_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      return build_zero_cst (scalar_type);

    case MULT_EXPR:
      return build_one_cst (scalar_type);

    case BIT_AND_EXPR:
      return build_all_ones_cst (scalar_type);

    case MAX_EXPR:
    case MIN_EXPR:
      /* For MIN/MAX the initial values are neutral.  A reduction chain
	 has only a single initial value, so that value is neutral for
	 all statements.  */
      if (reduc_chain)
	return PHI_ARG_DEF_FROM_EDGE (stmt_vinfo->stmt,
				      loop_preheader_edge (loop));
      return NULL_TREE;

    default:
      return NULL_TREE;
    }
}

/* Error reporting helper for vect_is_simple_reduction below.  GIMPLE statement
   STMT is printed with a message MSG. */

static void
report_vect_op (dump_flags_t msg_type, gimple *stmt, const char *msg)
{
  dump_printf_loc (msg_type, vect_location, "%s%G", msg, stmt);
}

/* Return true if we need an in-order reduction for operation CODE
   on type TYPE.  NEED_WRAPPING_INTEGRAL_OVERFLOW is true if integer
   overflow must wrap.  */

bool
needs_fold_left_reduction_p (tree type, tree_code code)
{
  /* CHECKME: check for !flag_finite_math_only too?  */
  if (SCALAR_FLOAT_TYPE_P (type))
    switch (code)
      {
      case MIN_EXPR:
      case MAX_EXPR:
	return false;

      default:
	return !flag_associative_math;
      }

  if (INTEGRAL_TYPE_P (type))
    {
      if (!operation_no_trapping_overflow (type, code))
	return true;
      return false;
    }

  if (SAT_FIXED_POINT_TYPE_P (type))
    return true;

  return false;
}

/* Return true if the reduction PHI in LOOP with latch arg LOOP_ARG and
   has a handled computation expression.  Store the main reduction
   operation in *CODE.  */

static bool
check_reduction_path (dump_user_location_t loc, loop_p loop, gphi *phi,
		      tree loop_arg, enum tree_code *code,
		      vec<std::pair<ssa_op_iter, use_operand_p> > &path)
{
  auto_bitmap visited;
  tree lookfor = PHI_RESULT (phi);
  ssa_op_iter curri;
  use_operand_p curr = op_iter_init_phiuse (&curri, phi, SSA_OP_USE);
  while (USE_FROM_PTR (curr) != loop_arg)
    curr = op_iter_next_use (&curri);
  curri.i = curri.numops;
  do
    {
      path.safe_push (std::make_pair (curri, curr));
      tree use = USE_FROM_PTR (curr);
      if (use == lookfor)
	break;
      gimple *def = SSA_NAME_DEF_STMT (use);
      if (gimple_nop_p (def)
	  || ! flow_bb_inside_loop_p (loop, gimple_bb (def)))
	{
pop:
	  do
	    {
	      std::pair<ssa_op_iter, use_operand_p> x = path.pop ();
	      curri = x.first;
	      curr = x.second;
	      do
		curr = op_iter_next_use (&curri);
	      /* Skip already visited or non-SSA operands (from iterating
	         over PHI args).  */
	      while (curr != NULL_USE_OPERAND_P
		     && (TREE_CODE (USE_FROM_PTR (curr)) != SSA_NAME
			 || ! bitmap_set_bit (visited,
					      SSA_NAME_VERSION
					        (USE_FROM_PTR (curr)))));
	    }
	  while (curr == NULL_USE_OPERAND_P && ! path.is_empty ());
	  if (curr == NULL_USE_OPERAND_P)
	    break;
	}
      else
	{
	  if (gimple_code (def) == GIMPLE_PHI)
	    curr = op_iter_init_phiuse (&curri, as_a <gphi *>(def), SSA_OP_USE);
	  else
	    curr = op_iter_init_use (&curri, def, SSA_OP_USE);
	  while (curr != NULL_USE_OPERAND_P
		 && (TREE_CODE (USE_FROM_PTR (curr)) != SSA_NAME
		     || ! bitmap_set_bit (visited,
					  SSA_NAME_VERSION
					    (USE_FROM_PTR (curr)))))
	    curr = op_iter_next_use (&curri);
	  if (curr == NULL_USE_OPERAND_P)
	    goto pop;
	}
    }
  while (1);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_printf_loc (MSG_NOTE, loc, "reduction path: ");
      unsigned i;
      std::pair<ssa_op_iter, use_operand_p> *x;
      FOR_EACH_VEC_ELT (path, i, x)
	dump_printf (MSG_NOTE, "%T ", USE_FROM_PTR (x->second));
      dump_printf (MSG_NOTE, "\n");
    }

  /* Check whether the reduction path detected is valid.  */
  bool fail = path.length () == 0;
  bool neg = false;
  int sign = -1;
  *code = ERROR_MARK;
  for (unsigned i = 1; i < path.length (); ++i)
    {
      gimple *use_stmt = USE_STMT (path[i].second);
      tree op = USE_FROM_PTR (path[i].second);
      if (! is_gimple_assign (use_stmt)
	  /* The following make sure we can compute the operand index
	     easily plus it mostly disallows chaining via COND_EXPR condition
	     operands.  */
	  || (gimple_assign_rhs1_ptr (use_stmt) != path[i].second->use
	      && (gimple_num_ops (use_stmt) <= 2
		  || gimple_assign_rhs2_ptr (use_stmt) != path[i].second->use)
	      && (gimple_num_ops (use_stmt) <= 3
		  || gimple_assign_rhs3_ptr (use_stmt) != path[i].second->use)))
	{
	  fail = true;
	  break;
	}
      /* Check there's only a single stmt the op is used on inside
         of the loop.  */
      imm_use_iterator imm_iter;
      gimple *op_use_stmt;
      unsigned cnt = 0;
      FOR_EACH_IMM_USE_STMT (op_use_stmt, imm_iter, op)
	if (!is_gimple_debug (op_use_stmt)
	    && flow_bb_inside_loop_p (loop, gimple_bb (op_use_stmt)))
	  {
	    /* We want to allow x + x but not x < 1 ? x : 2.  */
	    if (is_gimple_assign (op_use_stmt)
		&& gimple_assign_rhs_code (op_use_stmt) == COND_EXPR)
	      {
		use_operand_p use_p;
		FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		  cnt++;
	      }
	    else
	      cnt++;
	  }
      if (cnt != 1)
	{
	  fail = true;
	  break;
	}
      tree_code use_code = gimple_assign_rhs_code (use_stmt);
      if (use_code == MINUS_EXPR)
	{
	  use_code = PLUS_EXPR;
	  /* Track whether we negate the reduction value each iteration.  */
	  if (gimple_assign_rhs2 (use_stmt) == op)
	    neg = ! neg;
	}
      if (CONVERT_EXPR_CODE_P (use_code)
	  && tree_nop_conversion_p (TREE_TYPE (gimple_assign_lhs (use_stmt)),
				    TREE_TYPE (gimple_assign_rhs1 (use_stmt))))
	;
      else if (*code == ERROR_MARK)
	{
	  *code = use_code;
	  sign = TYPE_SIGN (TREE_TYPE (gimple_assign_lhs (use_stmt)));
	}
      else if (use_code != *code)
	{
	  fail = true;
	  break;
	}
      else if ((use_code == MIN_EXPR
		|| use_code == MAX_EXPR)
	       && sign != TYPE_SIGN (TREE_TYPE (gimple_assign_lhs (use_stmt))))
	{
	  fail = true;
	  break;
	}
    }
  return ! fail && ! neg && *code != ERROR_MARK;
}

bool
check_reduction_path (dump_user_location_t loc, loop_p loop, gphi *phi,
		      tree loop_arg, enum tree_code code)
{
  auto_vec<std::pair<ssa_op_iter, use_operand_p> > path;
  enum tree_code code_;
  return (check_reduction_path (loc, loop, phi, loop_arg, &code_, path)
	  && code_ == code);
}



/* Function vect_is_simple_reduction

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
      change the order of the computation
   2. no uses for a2 in the loop (a2 is used out of the loop)
   3. no uses of a1 in the loop besides the reduction operation
   4. no uses of a1 outside the loop.

   Conditions 1,4 are tested here.
   Conditions 2,3 are tested in vect_mark_stmts_to_be_vectorized.

   (2) Detect a cross-iteration def-use cycle in nested loops, i.e.,
   nested cycles.

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

static stmt_vec_info
vect_is_simple_reduction (loop_vec_info loop_info, stmt_vec_info phi_info,
			  bool *double_reduc, bool *reduc_chain_p)
{
  gphi *phi = as_a <gphi *> (phi_info->stmt);
  gimple *phi_use_stmt = NULL;
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  *double_reduc = false;
  *reduc_chain_p = false;
  STMT_VINFO_REDUC_TYPE (phi_info) = TREE_CODE_REDUCTION;

  tree phi_name = PHI_RESULT (phi);
  /* ???  If there are no uses of the PHI result the inner loop reduction
     won't be detected as possibly double-reduction by vectorizable_reduction
     because that tries to walk the PHI arg from the preheader edge which
     can be constant.  See PR60382.  */
  if (has_zero_uses (phi_name))
    return NULL;
  class loop *loop = (gimple_bb (phi))->loop_father;
  unsigned nphi_def_loop_uses = 0;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, phi_name)
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

      nphi_def_loop_uses++;
      phi_use_stmt = use_stmt;
    }

  tree latch_def = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));
  if (TREE_CODE (latch_def) != SSA_NAME)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "reduction: not ssa_name: %T\n", latch_def);
      return NULL;
    }

  stmt_vec_info def_stmt_info = loop_info->lookup_def (latch_def);
  if (!def_stmt_info
      || !flow_bb_inside_loop_p (loop, gimple_bb (def_stmt_info->stmt)))
    return NULL;

  bool nested_in_vect_loop
    = flow_loop_nested_p (LOOP_VINFO_LOOP (loop_info), loop);
  unsigned nlatch_def_loop_uses = 0;
  auto_vec<gphi *, 3> lcphis;
  bool inner_loop_of_double_reduc = false;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, latch_def)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;
      if (flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
	nlatch_def_loop_uses++;
      else
	{
	  /* We can have more than one loop-closed PHI.  */
	  lcphis.safe_push (as_a <gphi *> (use_stmt));
	  if (nested_in_vect_loop
	      && (STMT_VINFO_DEF_TYPE (loop_info->lookup_stmt (use_stmt))
		  == vect_double_reduction_def))
	    inner_loop_of_double_reduc = true;
	}
    }

  /* If we are vectorizing an inner reduction we are executing that
     in the original order only in case we are not dealing with a
     double reduction.  */
  if (nested_in_vect_loop && !inner_loop_of_double_reduc)
    {
      if (dump_enabled_p ())
	report_vect_op (MSG_NOTE, def_stmt_info->stmt,
			"detected nested cycle: ");
      return def_stmt_info;
    }

  /* If this isn't a nested cycle or if the nested cycle reduction value
     is used ouside of the inner loop we cannot handle uses of the reduction
     value.  */
  if (nlatch_def_loop_uses > 1 || nphi_def_loop_uses > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "reduction used in loop.\n");
      return NULL;
    }

  /* If DEF_STMT is a phi node itself, we expect it to have a single argument
     defined in the inner loop.  */
  if (gphi *def_stmt = dyn_cast <gphi *> (def_stmt_info->stmt))
    {
      tree op1 = PHI_ARG_DEF (def_stmt, 0);
      if (gimple_phi_num_args (def_stmt) != 1
          || TREE_CODE (op1) != SSA_NAME)
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported phi node definition.\n");

          return NULL;
        }

      gimple *def1 = SSA_NAME_DEF_STMT (op1);
      if (gimple_bb (def1)
	  && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt))
          && loop->inner
          && flow_bb_inside_loop_p (loop->inner, gimple_bb (def1))
          && is_gimple_assign (def1)
	  && is_a <gphi *> (phi_use_stmt)
	  && flow_bb_inside_loop_p (loop->inner, gimple_bb (phi_use_stmt)))
        {
          if (dump_enabled_p ())
            report_vect_op (MSG_NOTE, def_stmt,
			    "detected double reduction: ");

          *double_reduc = true;
	  return def_stmt_info;
        }

      return NULL;
    }

  /* Look for the expression computing latch_def from then loop PHI result.  */
  auto_vec<std::pair<ssa_op_iter, use_operand_p> > path;
  enum tree_code code;
  if (check_reduction_path (vect_location, loop, phi, latch_def, &code,
			    path))
    {
      STMT_VINFO_REDUC_CODE (phi_info) = code;
      if (code == COND_EXPR && !nested_in_vect_loop)
	STMT_VINFO_REDUC_TYPE (phi_info) = COND_REDUCTION;

      /* Fill in STMT_VINFO_REDUC_IDX and gather stmts for an SLP
	 reduction chain for which the additional restriction is that
	 all operations in the chain are the same.  */
      auto_vec<stmt_vec_info, 8> reduc_chain;
      unsigned i;
      bool is_slp_reduc = !nested_in_vect_loop && code != COND_EXPR;
      for (i = path.length () - 1; i >= 1; --i)
	{
	  gimple *stmt = USE_STMT (path[i].second);
	  stmt_vec_info stmt_info = loop_info->lookup_stmt (stmt);
	  STMT_VINFO_REDUC_IDX (stmt_info)
	    = path[i].second->use - gimple_assign_rhs1_ptr (stmt);
	  enum tree_code stmt_code = gimple_assign_rhs_code (stmt);
	  bool leading_conversion = (CONVERT_EXPR_CODE_P (stmt_code)
				     && (i == 1 || i == path.length () - 1));
	  if ((stmt_code != code && !leading_conversion)
	      /* We can only handle the final value in epilogue
		 generation for reduction chains.  */
	      || (i != 1 && !has_single_use (gimple_assign_lhs (stmt))))
	    is_slp_reduc = false;
	  /* For reduction chains we support a trailing/leading
	     conversions.  We do not store those in the actual chain.  */
	  if (leading_conversion)
	    continue;
	  reduc_chain.safe_push (stmt_info);
	}
      if (is_slp_reduc && reduc_chain.length () > 1)
	{
	  for (unsigned i = 0; i < reduc_chain.length () - 1; ++i)
	    {
	      REDUC_GROUP_FIRST_ELEMENT (reduc_chain[i]) = reduc_chain[0];
	      REDUC_GROUP_NEXT_ELEMENT (reduc_chain[i]) = reduc_chain[i+1];
	    }
	  REDUC_GROUP_FIRST_ELEMENT (reduc_chain.last ()) = reduc_chain[0];
	  REDUC_GROUP_NEXT_ELEMENT (reduc_chain.last ()) = NULL;

	  /* Save the chain for further analysis in SLP detection.  */
	  LOOP_VINFO_REDUCTION_CHAINS (loop_info).safe_push (reduc_chain[0]);
	  REDUC_GROUP_SIZE (reduc_chain[0]) = reduc_chain.length ();

	  *reduc_chain_p = true;
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			    "reduction: detected reduction chain\n");
	}
      else if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "reduction: detected reduction\n");

      return def_stmt_info;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "reduction: unknown pattern\n");

  return NULL;
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
  int assumed_vf = vect_vf_for_cost (loop_vinfo);

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    {
      *peel_iters_epilogue = assumed_vf / 2;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "cost model: epilogue peel iters set to vf/2 "
			 "because loop iterations are unknown .\n");

      /* If peeled iterations are known but number of scalar loop
         iterations are unknown, count a taken branch per peeled loop.  */
      retval = record_stmt_cost (prologue_cost_vec, 1, cond_branch_taken,
				 NULL, 0, vect_prologue);
      retval += record_stmt_cost (epilogue_cost_vec, 1, cond_branch_taken,
				  NULL, 0, vect_epilogue);
    }
  else
    {
      int niters = LOOP_VINFO_INT_NITERS (loop_vinfo);
      peel_iters_prologue = niters < peel_iters_prologue ?
                            niters : peel_iters_prologue;
      *peel_iters_epilogue = (niters - peel_iters_prologue) % assumed_vf;
      /* If we need to peel for gaps, but no peeling is required, we have to
	 peel VF iterations.  */
      if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) && !*peel_iters_epilogue)
	*peel_iters_epilogue = assumed_vf;
    }

  stmt_info_for_cost *si;
  int j;
  if (peel_iters_prologue)
    FOR_EACH_VEC_ELT (*scalar_cost_vec, j, si)
      retval += record_stmt_cost (prologue_cost_vec,
				  si->count * peel_iters_prologue,
				  si->kind, si->stmt_info, si->misalign,
				  vect_prologue);
  if (*peel_iters_epilogue)
    FOR_EACH_VEC_ELT (*scalar_cost_vec, j, si)
      retval += record_stmt_cost (epilogue_cost_vec,
				  si->count * *peel_iters_epilogue,
				  si->kind, si->stmt_info, si->misalign,
				  vect_epilogue);

  return retval;
}

/* Function vect_estimate_min_profitable_iters

   Return the number of iterations required for the vector version of the
   loop to be profitable relative to the cost of the scalar version of the
   loop.

   *RET_MIN_PROFITABLE_NITERS is a cost model profitability threshold
   of iterations for vectorization.  -1 value means loop vectorization
   is not profitable.  This returned value may be used for dynamic
   profitability check.

   *RET_MIN_PROFITABLE_ESTIMATE is a profitability threshold to be used
   for static check against estimated number of iterations.  */

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
  int assumed_vf = vect_vf_for_cost (loop_vinfo);
  int npeel = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
  void *target_cost_data = LOOP_VINFO_TARGET_COST_DATA (loop_vinfo);

  /* Cost model disabled.  */
  if (unlimited_cost_model (LOOP_VINFO_LOOP (loop_vinfo)))
    {
      if (dump_enabled_p ())
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
      if (dump_enabled_p ())
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
      len = LOOP_VINFO_CHECK_UNEQUAL_ADDRS (loop_vinfo).length ();
      if (len)
	/* Count LEN - 1 ANDs and LEN comparisons.  */
	(void) add_stmt_cost (target_cost_data, len * 2 - 1, scalar_stmt,
			      NULL, 0, vect_prologue);
      len = LOOP_VINFO_LOWER_BOUNDS (loop_vinfo).length ();
      if (len)
	{
	  /* Count LEN - 1 ANDs and LEN comparisons.  */
	  unsigned int nstmts = len * 2 - 1;
	  /* +1 for each bias that needs adding.  */
	  for (unsigned int i = 0; i < len; ++i)
	    if (!LOOP_VINFO_LOWER_BOUNDS (loop_vinfo)[i].unsigned_p)
	      nstmts += 1;
	  (void) add_stmt_cost (target_cost_data, nstmts, scalar_stmt,
				NULL, 0, vect_prologue);
	}
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE,
		     "cost model: Adding cost of checks for loop "
		     "versioning aliasing.\n");
    }

  /* Requires loop versioning with niter checks.  */
  if (LOOP_REQUIRES_VERSIONING_FOR_NITERS (loop_vinfo))
    {
      /*  FIXME: Make cost depend on complexity of individual check.  */
      (void) add_stmt_cost (target_cost_data, 1, vector_stmt, NULL, 0,
			    vect_prologue);
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE,
		     "cost model: Adding cost of checks for loop "
		     "versioning niters.\n");
    }

  if (LOOP_REQUIRES_VERSIONING (loop_vinfo))
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
     loop.  (For fully-masked loops there will be no peeling.)

     FORNOW: If we don't know the value of peel_iters for prologue or epilogue
     at compile-time - we assume it's vf/2 (the worst would be vf-1).

     TODO: Build an expression that represents peel_iters for prologue and
     epilogue to be used in a run-time test.  */

  if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      peel_iters_prologue = 0;
      peel_iters_epilogue = 0;

      if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
	{
	  /* We need to peel exactly one iteration.  */
	  peel_iters_epilogue += 1;
	  stmt_info_for_cost *si;
	  int j;
	  FOR_EACH_VEC_ELT (LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo),
			    j, si)
	    (void) add_stmt_cost (target_cost_data, si->count,
				  si->kind, si->stmt_info, si->misalign,
				  vect_epilogue);
	}

      /* Calculate how many masks we need to generate.  */
      unsigned int num_masks = 0;
      rgroup_masks *rgm;
      unsigned int num_vectors_m1;
      FOR_EACH_VEC_ELT (LOOP_VINFO_MASKS (loop_vinfo), num_vectors_m1, rgm)
	if (rgm->mask_type)
	  num_masks += num_vectors_m1 + 1;
      gcc_assert (num_masks > 0);

      /* In the worst case, we need to generate each mask in the prologue
	 and in the loop body.  One of the loop body mask instructions
	 replaces the comparison in the scalar loop, and since we don't
	 count the scalar comparison against the scalar body, we shouldn't
	 count that vector instruction against the vector body either.

	 Sometimes we can use unpacks instead of generating prologue
	 masks and sometimes the prologue mask will fold to a constant,
	 so the actual prologue cost might be smaller.  However, it's
	 simpler and safer to use the worst-case cost; if this ends up
	 being the tie-breaker between vectorizing or not, then it's
	 probably better not to vectorize.  */
      (void) add_stmt_cost (target_cost_data, num_masks, vector_stmt,
			    NULL, 0, vect_prologue);
      (void) add_stmt_cost (target_cost_data, num_masks - 1, vector_stmt,
			    NULL, 0, vect_body);
    }
  else if (npeel < 0)
    {
      peel_iters_prologue = assumed_vf / 2;
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "cost model: "
		     "prologue peel iters set to vf/2.\n");

      /* If peeling for alignment is unknown, loop bound of main loop becomes
         unknown.  */
      peel_iters_epilogue = assumed_vf / 2;
      if (dump_enabled_p ())
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
	  (void) add_stmt_cost (target_cost_data,
				si->count * peel_iters_prologue,
				si->kind, si->stmt_info, si->misalign,
				vect_prologue);
	  (void) add_stmt_cost (target_cost_data,
				si->count * peel_iters_epilogue,
				si->kind, si->stmt_info, si->misalign,
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
	(void) add_stmt_cost (data, si->count, si->kind, si->stmt_info,
			      si->misalign, vect_prologue);

      FOR_EACH_VEC_ELT (epilogue_cost_vec, j, si)
	(void) add_stmt_cost (data, si->count, si->kind, si->stmt_info,
			      si->misalign, vect_epilogue);

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
      || LOOP_REQUIRES_VERSIONING (loop_vinfo))
    {
      /* Cost model check occurs at versioning.  */
      if (LOOP_REQUIRES_VERSIONING (loop_vinfo))
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

  /* Stash the costs so that we can compare two loop_vec_infos.  */
  loop_vinfo->vec_inside_cost = vec_inside_cost;
  loop_vinfo->vec_outside_cost = vec_outside_cost;

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
     SIC * niters + SOC > VIC * ((niters - NPEEL) / VF) + VOC
     where
     SIC = scalar iteration cost, VIC = vector iteration cost,
     VOC = vector outside cost, VF = vectorization factor,
     NPEEL = prologue iterations + epilogue iterations,
     SOC = scalar outside cost for run time cost model check.  */

  int saving_per_viter = (scalar_single_iter_cost * assumed_vf
			  - vec_inside_cost);
  if (saving_per_viter <= 0)
    {
      if (LOOP_VINFO_LOOP (loop_vinfo)->force_vectorize)
	warning_at (vect_location.get_location_t (), OPT_Wopenmp_simd,
		    "vectorization did not happen for a simd loop");

      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "cost model: the vector iteration cost = %d "
			 "divided by the scalar iteration cost = %d "
			 "is greater or equal to the vectorization factor = %d"
                         ".\n",
			 vec_inside_cost, scalar_single_iter_cost, assumed_vf);
      *ret_min_profitable_niters = -1;
      *ret_min_profitable_estimate = -1;
      return;
    }

  /* ??? The "if" arm is written to handle all cases; see below for what
     we would do for !LOOP_VINFO_FULLY_MASKED_P.  */
  if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      /* Rewriting the condition above in terms of the number of
	 vector iterations (vniters) rather than the number of
	 scalar iterations (niters) gives:

	 SIC * (vniters * VF + NPEEL) + SOC > VIC * vniters + VOC

	 <==> vniters * (SIC * VF - VIC) > VOC - SIC * NPEEL - SOC

	 For integer N, X and Y when X > 0:

	 N * X > Y <==> N >= (Y /[floor] X) + 1.  */
      int outside_overhead = (vec_outside_cost
			      - scalar_single_iter_cost * peel_iters_prologue
			      - scalar_single_iter_cost * peel_iters_epilogue
			      - scalar_outside_cost);
      /* We're only interested in cases that require at least one
	 vector iteration.  */
      int min_vec_niters = 1;
      if (outside_overhead > 0)
	min_vec_niters = outside_overhead / saving_per_viter + 1;

      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "  Minimum number of vector iterations: %d\n",
		     min_vec_niters);

      if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
	{
	  /* Now that we know the minimum number of vector iterations,
	     find the minimum niters for which the scalar cost is larger:

	     SIC * niters > VIC * vniters + VOC - SOC

	     We know that the minimum niters is no more than
	     vniters * VF + NPEEL, but it might be (and often is) less
	     than that if a partial vector iteration is cheaper than the
	     equivalent scalar code.  */
	  int threshold = (vec_inside_cost * min_vec_niters
			   + vec_outside_cost
			   - scalar_outside_cost);
	  if (threshold <= 0)
	    min_profitable_iters = 1;
	  else
	    min_profitable_iters = threshold / scalar_single_iter_cost + 1;
	}
      else
	/* Convert the number of vector iterations into a number of
	   scalar iterations.  */
	min_profitable_iters = (min_vec_niters * assumed_vf
				+ peel_iters_prologue
				+ peel_iters_epilogue);
    }
  else
    {
      min_profitable_iters = ((vec_outside_cost - scalar_outside_cost)
			      * assumed_vf
			      - vec_inside_cost * peel_iters_prologue
			      - vec_inside_cost * peel_iters_epilogue);
      if (min_profitable_iters <= 0)
        min_profitable_iters = 0;
      else
	{
	  min_profitable_iters /= saving_per_viter;

	  if ((scalar_single_iter_cost * assumed_vf * min_profitable_iters)
	      <= (((int) vec_inside_cost * min_profitable_iters)
		  + (((int) vec_outside_cost - scalar_outside_cost)
		     * assumed_vf)))
	    min_profitable_iters++;
	}
    }

  if (dump_enabled_p ())
    dump_printf (MSG_NOTE,
		 "  Calculated minimum iters for profitability: %d\n",
		 min_profitable_iters);

  if (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
      && min_profitable_iters < (assumed_vf + peel_iters_prologue))
    /* We want the vectorized loop to execute at least once.  */
    min_profitable_iters = assumed_vf + peel_iters_prologue;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "  Runtime profitability threshold = %d\n",
                     min_profitable_iters);

  *ret_min_profitable_niters = min_profitable_iters;

  /* Calculate number of iterations required to make the vector version
     profitable, relative to the loop bodies only.

     Non-vectorized variant is SIC * niters and it must win over vector
     variant on the expected loop trip count.  The following condition must hold true:
     SIC * niters > VIC * ((niters - NPEEL) / VF) + VOC + SOC  */

  if (vec_outside_cost <= 0)
    min_profitable_estimate = 0;
  else if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      /* This is a repeat of the code above, but with + SOC rather
	 than - SOC.  */
      int outside_overhead = (vec_outside_cost
			      - scalar_single_iter_cost * peel_iters_prologue
			      - scalar_single_iter_cost * peel_iters_epilogue
			      + scalar_outside_cost);
      int min_vec_niters = 1;
      if (outside_overhead > 0)
	min_vec_niters = outside_overhead / saving_per_viter + 1;

      if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
	{
	  int threshold = (vec_inside_cost * min_vec_niters
			   + vec_outside_cost
			   + scalar_outside_cost);
	  min_profitable_estimate = threshold / scalar_single_iter_cost + 1;
	}
      else
	min_profitable_estimate = (min_vec_niters * assumed_vf
				   + peel_iters_prologue
				   + peel_iters_epilogue);
    }
  else
    {
      min_profitable_estimate = ((vec_outside_cost + scalar_outside_cost)
				 * assumed_vf
				 - vec_inside_cost * peel_iters_prologue
				 - vec_inside_cost * peel_iters_epilogue)
				 / ((scalar_single_iter_cost * assumed_vf)
				   - vec_inside_cost);
    }
  min_profitable_estimate = MAX (min_profitable_estimate, min_profitable_iters);
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "  Static estimate profitability threshold = %d\n",
		     min_profitable_estimate);

  *ret_min_profitable_estimate = min_profitable_estimate;
}

/* Writes into SEL a mask for a vec_perm, equivalent to a vec_shr by OFFSET
   vector elements (not bits) for a vector with NELT elements.  */
static void
calc_vec_perm_mask_for_shift (unsigned int offset, unsigned int nelt,
			      vec_perm_builder *sel)
{
  /* The encoding is a single stepped pattern.  Any wrap-around is handled
     by vec_perm_indices.  */
  sel->new_vector (nelt, 1, 3);
  for (unsigned int i = 0; i < 3; i++)
    sel->quick_push (i + offset);
}

/* Checks whether the target supports whole-vector shifts for vectors of mode
   MODE.  This is the case if _either_ the platform handles vec_shr_optab, _or_
   it supports vec_perm_const with masks for all necessary shift amounts.  */
static bool
have_whole_vector_shift (machine_mode mode)
{
  if (optab_handler (vec_shr_optab, mode) != CODE_FOR_nothing)
    return true;

  /* Variable-length vectors should be handled via the optab.  */
  unsigned int nelt;
  if (!GET_MODE_NUNITS (mode).is_constant (&nelt))
    return false;

  vec_perm_builder sel;
  vec_perm_indices indices;
  for (unsigned int i = nelt / 2; i >= 1; i /= 2)
    {
      calc_vec_perm_mask_for_shift (i, nelt, &sel);
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (mode, indices, false))
	return false;
    }
  return true;
}

/* TODO: Close dependency between vect_model_*_cost and vectorizable_*
   functions. Design better to avoid maintenance issues.  */

/* Function vect_model_reduction_cost.

   Models cost for a reduction operation, including the vector ops
   generated within the strip-mine loop, the initial definition before
   the loop, and the epilogue code that must be generated.  */

static void
vect_model_reduction_cost (stmt_vec_info stmt_info, internal_fn reduc_fn,
			   vect_reduction_type reduction_type,
			   int ncopies, stmt_vector_for_cost *cost_vec)
{
  int prologue_cost = 0, epilogue_cost = 0, inside_cost;
  enum tree_code code;
  optab optab;
  tree vectype;
  machine_mode mode;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  class loop *loop = NULL;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* Condition reductions generate two reductions in the loop.  */
  if (reduction_type == COND_REDUCTION)
    ncopies *= 2;

  vectype = STMT_VINFO_VECTYPE (stmt_info);
  mode = TYPE_MODE (vectype);
  stmt_vec_info orig_stmt_info = vect_orig_stmt (stmt_info);

  code = gimple_assign_rhs_code (orig_stmt_info->stmt);

  if (reduction_type == EXTRACT_LAST_REDUCTION)
    /* No extra instructions are needed in the prologue.  The loop body
       operations are costed in vectorizable_condition.  */
    inside_cost = 0;
  else if (reduction_type == FOLD_LEFT_REDUCTION)
    {
      /* No extra instructions needed in the prologue.  */
      prologue_cost = 0;

      if (reduc_fn != IFN_LAST)
	/* Count one reduction-like operation per vector.  */
	inside_cost = record_stmt_cost (cost_vec, ncopies, vec_to_scalar,
					stmt_info, 0, vect_body);
      else
	{
	  /* Use NELEMENTS extracts and NELEMENTS scalar ops.  */
	  unsigned int nelements = ncopies * vect_nunits_for_cost (vectype);
	  inside_cost = record_stmt_cost (cost_vec, nelements,
					  vec_to_scalar, stmt_info, 0,
					  vect_body);
	  inside_cost += record_stmt_cost (cost_vec, nelements,
					   scalar_stmt, stmt_info, 0,
					   vect_body);
	}
    }
  else
    {
      /* Add in cost for initial definition.
	 For cond reduction we have four vectors: initial index, step,
	 initial result of the data reduction, initial value of the index
	 reduction.  */
      int prologue_stmts = reduction_type == COND_REDUCTION ? 4 : 1;
      prologue_cost += record_stmt_cost (cost_vec, prologue_stmts,
					 scalar_to_vec, stmt_info, 0,
					 vect_prologue);

      /* Cost of reduction op inside loop.  */
      inside_cost = record_stmt_cost (cost_vec, ncopies, vector_stmt,
				      stmt_info, 0, vect_body);
    }

  /* Determine cost of epilogue code.

     We have a reduction operator that will reduce the vector in one statement.
     Also requires scalar extract.  */

  if (!loop || !nested_in_vect_loop_p (loop, orig_stmt_info))
    {
      if (reduc_fn != IFN_LAST)
	{
	  if (reduction_type == COND_REDUCTION)
	    {
	      /* An EQ stmt and an COND_EXPR stmt.  */
	      epilogue_cost += record_stmt_cost (cost_vec, 2,
						 vector_stmt, stmt_info, 0,
						 vect_epilogue);
	      /* Reduction of the max index and a reduction of the found
		 values.  */
	      epilogue_cost += record_stmt_cost (cost_vec, 2,
						 vec_to_scalar, stmt_info, 0,
						 vect_epilogue);
	      /* A broadcast of the max value.  */
	      epilogue_cost += record_stmt_cost (cost_vec, 1,
						 scalar_to_vec, stmt_info, 0,
						 vect_epilogue);
	    }
	  else
	    {
	      epilogue_cost += record_stmt_cost (cost_vec, 1, vector_stmt,
						 stmt_info, 0, vect_epilogue);
	      epilogue_cost += record_stmt_cost (cost_vec, 1,
						 vec_to_scalar, stmt_info, 0,
						 vect_epilogue);
	    }
	}
      else if (reduction_type == COND_REDUCTION)
	{
	  unsigned estimated_nunits = vect_nunits_for_cost (vectype);
	  /* Extraction of scalar elements.  */
	  epilogue_cost += record_stmt_cost (cost_vec,
					     2 * estimated_nunits,
					     vec_to_scalar, stmt_info, 0,
					     vect_epilogue);
	  /* Scalar max reductions via COND_EXPR / MAX_EXPR.  */
	  epilogue_cost += record_stmt_cost (cost_vec,
					     2 * estimated_nunits - 3,
					     scalar_stmt, stmt_info, 0,
					     vect_epilogue);
	}
      else if (reduction_type == EXTRACT_LAST_REDUCTION
	       || reduction_type == FOLD_LEFT_REDUCTION)
	/* No extra instructions need in the epilogue.  */
	;
      else
	{
	  int vec_size_in_bits = tree_to_uhwi (TYPE_SIZE (vectype));
	  tree bitsize =
	    TYPE_SIZE (TREE_TYPE (gimple_assign_lhs (orig_stmt_info->stmt)));
	  int element_bitsize = tree_to_uhwi (bitsize);
	  int nelements = vec_size_in_bits / element_bitsize;

	  if (code == COND_EXPR)
	    code = MAX_EXPR;

	  optab = optab_for_tree_code (code, vectype, optab_default);

	  /* We have a whole vector shift available.  */
	  if (optab != unknown_optab
	      && VECTOR_MODE_P (mode)
	      && optab_handler (optab, mode) != CODE_FOR_nothing
	      && have_whole_vector_shift (mode))
	    {
	      /* Final reduction via vector shifts and the reduction operator.
		 Also requires scalar extract.  */
	      epilogue_cost += record_stmt_cost (cost_vec,
						 exact_log2 (nelements) * 2,
						 vector_stmt, stmt_info, 0,
						 vect_epilogue);
	      epilogue_cost += record_stmt_cost (cost_vec, 1,
						 vec_to_scalar, stmt_info, 0,
						 vect_epilogue);
	    }	  
	  else
	    /* Use extracts and reduction op for final reduction.  For N
	       elements, we have N extracts and N-1 reduction ops.  */
	    epilogue_cost += record_stmt_cost (cost_vec, 
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
}


/* Function vect_model_induction_cost.

   Models cost for induction operations.  */

static void
vect_model_induction_cost (stmt_vec_info stmt_info, int ncopies,
			   stmt_vector_for_cost *cost_vec)
{
  unsigned inside_cost, prologue_cost;

  if (PURE_SLP_STMT (stmt_info))
    return;

  /* loop cost for vec_loop.  */
  inside_cost = record_stmt_cost (cost_vec, ncopies, vector_stmt,
				  stmt_info, 0, vect_body);

  /* prologue cost for vec_init and vec_step.  */
  prologue_cost = record_stmt_cost (cost_vec, 2, scalar_to_vec,
				    stmt_info, 0, vect_prologue);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_induction_cost: inside_cost = %d, "
                     "prologue_cost = %d .\n", inside_cost, prologue_cost);
}



/* Function get_initial_def_for_reduction

   Input:
   STMT_VINFO - a stmt that performs a reduction operation in the loop.
   INIT_VAL - the initial value of the reduction variable

   Output:
   ADJUSTMENT_DEF - a tree that holds a value to be added to the final result
        of the reduction (used for adjusting the epilog - see below).
   Return a vector variable, initialized according to the operation that
	STMT_VINFO performs. This vector will be used as the initial value
	of the vector of partial results.

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

   STMT_VINFO is 's = s + a[i]', and the reduction variable is 's'.
   For a vector of 4 units, we want to return either [0,0,0,init_val],
   or [0,0,0,0] and let the caller know that it needs to adjust
   the result at the end by 'init_val'.

   FORNOW, we are using the 'adjust in epilog' scheme, because this way the
   initialization vector is simpler (same element in all entries), if
   ADJUSTMENT_DEF is not NULL, and Option2 otherwise.

   A cost model should help decide between these two schemes.  */

static tree
get_initial_def_for_reduction (stmt_vec_info stmt_vinfo,
			       enum tree_code code, tree init_val,
                               tree *adjustment_def)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree scalar_type = TREE_TYPE (init_val);
  tree vectype = get_vectype_for_scalar_type (loop_vinfo, scalar_type);
  tree def_for_init;
  tree init_def;
  REAL_VALUE_TYPE real_init_val = dconst0;
  int int_init_val = 0;
  gimple_seq stmts = NULL;

  gcc_assert (vectype);

  gcc_assert (POINTER_TYPE_P (scalar_type) || INTEGRAL_TYPE_P (scalar_type)
	      || SCALAR_FLOAT_TYPE_P (scalar_type));

  gcc_assert (nested_in_vect_loop_p (loop, stmt_vinfo)
	      || loop == (gimple_bb (stmt_vinfo->stmt))->loop_father);

  /* ADJUSTMENT_DEF is NULL when called from
     vect_create_epilog_for_reduction to vectorize double reduction.  */
  if (adjustment_def)
    *adjustment_def = NULL;

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
      {
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

	if (adjustment_def || operand_equal_p (def_for_init, init_val, 0))
	  {
	    /* Option1: the first element is '0' or '1' as well.  */
	    if (!operand_equal_p (def_for_init, init_val, 0))
	      *adjustment_def = init_val;
	    init_def = gimple_build_vector_from_val (&stmts, vectype,
						     def_for_init);
	  }
	else if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant ())
	  {
	    /* Option2 (variable length): the first element is INIT_VAL.  */
	    init_def = gimple_build_vector_from_val (&stmts, vectype,
						     def_for_init);
	    init_def = gimple_build (&stmts, CFN_VEC_SHL_INSERT,
				     vectype, init_def, init_val);
	  }
	else
	  {
	    /* Option2: the first element is INIT_VAL.  */
	    tree_vector_builder elts (vectype, 1, 2);
	    elts.quick_push (init_val);
	    elts.quick_push (def_for_init);
	    init_def = gimple_build_vector (&stmts, &elts);
	  }
      }
      break;

    case MIN_EXPR:
    case MAX_EXPR:
    case COND_EXPR:
      {
	init_val = gimple_convert (&stmts, TREE_TYPE (vectype), init_val);
	init_def = gimple_build_vector_from_val (&stmts, vectype, init_val);
      }
      break;

    default:
      gcc_unreachable ();
    }

  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
  return init_def;
}

/* Get at the initial defs for the reduction PHIs in SLP_NODE.
   NUMBER_OF_VECTORS is the number of vector defs to create.
   If NEUTRAL_OP is nonnull, introducing extra elements of that
   value will not change the result.  */

static void
get_initial_defs_for_reduction (slp_tree slp_node,
				vec<tree> *vec_oprnds,
				unsigned int number_of_vectors,
				bool reduc_chain, tree neutral_op)
{
  vec<stmt_vec_info> stmts = SLP_TREE_SCALAR_STMTS (slp_node);
  stmt_vec_info stmt_vinfo = stmts[0];
  vec_info *vinfo = stmt_vinfo->vinfo;
  unsigned HOST_WIDE_INT nunits;
  unsigned j, number_of_places_left_in_vector;
  tree vector_type;
  unsigned int group_size = stmts.length ();
  unsigned int i;
  class loop *loop;

  vector_type = STMT_VINFO_VECTYPE (stmt_vinfo);

  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def);

  loop = (gimple_bb (stmt_vinfo->stmt))->loop_father;
  gcc_assert (loop);
  edge pe = loop_preheader_edge (loop);

  gcc_assert (!reduc_chain || neutral_op);

  /* NUMBER_OF_COPIES is the number of times we need to use the same values in
     created vectors. It is greater than 1 if unrolling is performed.

     For example, we have two scalar operands, s1 and s2 (e.g., group of
     strided accesses of size two), while NUNITS is four (i.e., four scalars
     of this type can be packed in a vector).  The output vector will contain
     two copies of each scalar operand: {s1, s2, s1, s2}.  (NUMBER_OF_COPIES
     will be 2).

     If REDUC_GROUP_SIZE > NUNITS, the scalars will be split into several
     vectors containing the operands.

     For example, NUNITS is four as before, and the group size is 8
     (s1, s2, ..., s8).  We will create two vectors {s1, s2, s3, s4} and
     {s5, s6, s7, s8}.  */

  if (!TYPE_VECTOR_SUBPARTS (vector_type).is_constant (&nunits))
    nunits = group_size;

  number_of_places_left_in_vector = nunits;
  bool constant_p = true;
  tree_vector_builder elts (vector_type, nunits, 1);
  elts.quick_grow (nunits);
  gimple_seq ctor_seq = NULL;
  for (j = 0; j < nunits * number_of_vectors; ++j)
    {
      tree op;
      i = j % group_size;
      stmt_vinfo = stmts[i];

      /* Get the def before the loop.  In reduction chain we have only
	 one initial value.  Else we have as many as PHIs in the group.  */
      if (reduc_chain)
	op = j != 0 ? neutral_op : PHI_ARG_DEF_FROM_EDGE (stmt_vinfo->stmt, pe);
      else if (((vec_oprnds->length () + 1) * nunits
		- number_of_places_left_in_vector >= group_size)
	       && neutral_op)
	op = neutral_op;
      else
	op = PHI_ARG_DEF_FROM_EDGE (stmt_vinfo->stmt, pe);

      /* Create 'vect_ = {op0,op1,...,opn}'.  */
      number_of_places_left_in_vector--;
      elts[nunits - number_of_places_left_in_vector - 1] = op;
      if (!CONSTANT_CLASS_P (op))
	constant_p = false;

      if (number_of_places_left_in_vector == 0)
	{
	  tree init;
	  if (constant_p && !neutral_op
	      ? multiple_p (TYPE_VECTOR_SUBPARTS (vector_type), nunits)
	      : known_eq (TYPE_VECTOR_SUBPARTS (vector_type), nunits))
	    /* Build the vector directly from ELTS.  */
	    init = gimple_build_vector (&ctor_seq, &elts);
	  else if (neutral_op)
	    {
	      /* Build a vector of the neutral value and shift the
		 other elements into place.  */
	      init = gimple_build_vector_from_val (&ctor_seq, vector_type,
						   neutral_op);
	      int k = nunits;
	      while (k > 0 && elts[k - 1] == neutral_op)
		k -= 1;
	      while (k > 0)
		{
		  k -= 1;
		  init = gimple_build (&ctor_seq, CFN_VEC_SHL_INSERT,
				       vector_type, init, elts[k]);
		}
	    }
	  else
	    {
	      /* First time round, duplicate ELTS to fill the
		 required number of vectors.  */
	      duplicate_and_interleave (vinfo, &ctor_seq, vector_type, elts,
					number_of_vectors, *vec_oprnds);
	      break;
	    }
	  vec_oprnds->quick_push (init);

	  number_of_places_left_in_vector = nunits;
	  elts.new_vector (vector_type, nunits, 1);
	  elts.quick_grow (nunits);
	  constant_p = true;
	}
    }
  if (ctor_seq != NULL)
    gsi_insert_seq_on_edge_immediate (pe, ctor_seq);
}

/* For a statement STMT_INFO taking part in a reduction operation return
   the stmt_vec_info the meta information is stored on.  */

stmt_vec_info
info_for_reduction (stmt_vec_info stmt_info)
{
  stmt_info = vect_orig_stmt (stmt_info);
  gcc_assert (STMT_VINFO_REDUC_DEF (stmt_info));
  if (!is_a <gphi *> (stmt_info->stmt))
    stmt_info = STMT_VINFO_REDUC_DEF (stmt_info);
  gphi *phi = as_a <gphi *> (stmt_info->stmt);
  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
    {
      if (gimple_phi_num_args (phi) == 1)
	stmt_info = STMT_VINFO_REDUC_DEF (stmt_info);
    }
  else if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_nested_cycle)
    {
      edge pe = loop_preheader_edge (gimple_bb (phi)->loop_father);
      stmt_vec_info info
	  = stmt_info->vinfo->lookup_def (PHI_ARG_DEF_FROM_EDGE (phi, pe));
      if (info && STMT_VINFO_DEF_TYPE (info) == vect_double_reduction_def)
	stmt_info = info;
    }
  return stmt_info;
}

/* Function vect_create_epilog_for_reduction

   Create code at the loop-epilog to finalize the result of a reduction
   computation. 
  
   STMT_INFO is the scalar reduction stmt that is being vectorized.
   SLP_NODE is an SLP node containing a group of reduction statements. The 
     first one in this group is STMT_INFO.
   SLP_NODE_INSTANCE is the SLP node instance containing SLP_NODE
   REDUC_INDEX says which rhs operand of the STMT_INFO is the reduction phi
     (counting from 0)

   This function:
   1. Completes the reduction def-use cycles.
   2. "Reduces" each vector of partial results VECT_DEFS into a single result,
      by calling the function specified by REDUC_FN if available, or by
      other means (whole-vector shifts or a scalar loop).
      The function also creates a new phi node at the loop exit to preserve
      loop-closed form, as illustrated below.

     The flow at the entry to this function:

        loop:
          vec_def = phi <vec_init, null>        # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT_INFO
          s_loop = scalar_stmt                  # (scalar) STMT_INFO
        loop_exit:
          s_out0 = phi <s_loop>                 # (scalar) EXIT_PHI
          use <s_out0>
          use <s_out0>

     The above is transformed by this function into:

        loop:
          vec_def = phi <vec_init, VECT_DEF>    # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT_INFO
          s_loop = scalar_stmt                  # (scalar) STMT_INFO
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
vect_create_epilog_for_reduction (stmt_vec_info stmt_info,
				  slp_tree slp_node,
				  slp_instance slp_node_instance)
{
  stmt_vec_info reduc_info = info_for_reduction (stmt_info);
  gcc_assert (reduc_info->is_reduc_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  /* For double reductions we need to get at the inner loop reduction
     stmt which has the meta info attached.  Our stmt_info is that of the
     loop-closed PHI of the inner loop which we remember as
     def for the reduction PHI generation.  */
  bool double_reduc = false;
  stmt_vec_info rdef_info = stmt_info;
  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
    {
      gcc_assert (!slp_node);
      double_reduc = true;
      stmt_info = loop_vinfo->lookup_def (gimple_phi_arg_def
					    (stmt_info->stmt, 0));
      stmt_info = vect_stmt_to_vectorize (stmt_info);
    }
  gphi *reduc_def_stmt
    = as_a <gphi *> (STMT_VINFO_REDUC_DEF (vect_orig_stmt (stmt_info))->stmt);
  enum tree_code code = STMT_VINFO_REDUC_CODE (reduc_info);
  internal_fn reduc_fn = STMT_VINFO_REDUC_FN (reduc_info);
  stmt_vec_info prev_phi_info;
  tree vectype;
  machine_mode mode;
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo), *outer_loop = NULL;
  basic_block exit_bb;
  tree scalar_dest;
  tree scalar_type;
  gimple *new_phi = NULL, *phi;
  stmt_vec_info phi_info;
  gimple_stmt_iterator exit_gsi;
  tree new_temp = NULL_TREE, new_name, new_scalar_dest;
  gimple *epilog_stmt = NULL;
  gimple *exit_phi;
  tree bitsize;
  tree def;
  tree orig_name, scalar_result;
  imm_use_iterator imm_iter, phi_imm_iter;
  use_operand_p use_p, phi_use_p;
  gimple *use_stmt;
  bool nested_in_vect_loop = false;
  auto_vec<gimple *> new_phis;
  int j, i;
  auto_vec<tree> scalar_results;
  unsigned int group_size = 1, k;
  auto_vec<gimple *> phis;
  bool slp_reduc = false;
  bool direct_slp_reduc;
  tree new_phi_result;
  tree induction_index = NULL_TREE;

  if (slp_node)
    group_size = SLP_TREE_SCALAR_STMTS (slp_node).length (); 

  if (nested_in_vect_loop_p (loop, stmt_info))
    {
      outer_loop = loop;
      loop = loop->inner;
      nested_in_vect_loop = true;
      gcc_assert (!slp_node);
    }
  gcc_assert (!nested_in_vect_loop || double_reduc);

  vectype = STMT_VINFO_REDUC_VECTYPE (reduc_info);
  gcc_assert (vectype);
  mode = TYPE_MODE (vectype);

  tree initial_def = NULL;
  tree induc_val = NULL_TREE;
  tree adjustment_def = NULL;
  if (slp_node)
    ;
  else
    {
      /* Get at the scalar def before the loop, that defines the initial value
	 of the reduction variable.  */
      initial_def = PHI_ARG_DEF_FROM_EDGE (reduc_def_stmt,
					   loop_preheader_edge (loop));
      /* Optimize: for induction condition reduction, if we can't use zero
         for induc_val, use initial_def.  */
      if (STMT_VINFO_REDUC_TYPE (reduc_info) == INTEGER_INDUC_COND_REDUCTION)
	induc_val = STMT_VINFO_VEC_INDUC_COND_INITIAL_VAL (reduc_info);
      else if (double_reduc)
	;
      else if (nested_in_vect_loop)
	;
      else
	adjustment_def = STMT_VINFO_REDUC_EPILOGUE_ADJUSTMENT (reduc_info);
    }

  unsigned vec_num;
  int ncopies;
  if (slp_node)
    {
      vec_num = SLP_TREE_VEC_STMTS (slp_node_instance->reduc_phis).length ();
      ncopies = 1;
    }
  else
    {
      vec_num = 1;
      ncopies = 0;
      phi_info = STMT_VINFO_VEC_STMT (loop_vinfo->lookup_stmt (reduc_def_stmt));
      do
	{
	  ncopies++;
	  phi_info = STMT_VINFO_RELATED_STMT (phi_info);
	}
      while (phi_info);
    }

  /* For cond reductions we want to create a new vector (INDEX_COND_EXPR)
     which is updated with the current index of the loop for every match of
     the original loop's cond_expr (VEC_STMT).  This results in a vector
     containing the last time the condition passed for that vector lane.
     The first match will be a 1 to allow 0 to be used for non-matching
     indexes.  If there are no matches at all then the vector will be all
     zeroes.
   
     PR92772: This algorithm is broken for architectures that support
     masked vectors, but do not provide fold_extract_last.  */
  if (STMT_VINFO_REDUC_TYPE (reduc_info) == COND_REDUCTION)
    {
      auto_vec<std::pair<tree, bool>, 2> ccompares;
      stmt_vec_info cond_info = STMT_VINFO_REDUC_DEF (reduc_info);
      cond_info = vect_stmt_to_vectorize (cond_info);
      while (cond_info != reduc_info)
	{
	  if (gimple_assign_rhs_code (cond_info->stmt) == COND_EXPR)
	    {
	      gimple *vec_stmt = STMT_VINFO_VEC_STMT (cond_info)->stmt;
	      gcc_assert (gimple_assign_rhs_code (vec_stmt) == VEC_COND_EXPR);
	      ccompares.safe_push
		(std::make_pair (unshare_expr (gimple_assign_rhs1 (vec_stmt)),
				 STMT_VINFO_REDUC_IDX (cond_info) == 2));
	    }
	  cond_info
	    = loop_vinfo->lookup_def (gimple_op (cond_info->stmt,
						 1 + STMT_VINFO_REDUC_IDX
							(cond_info)));
	  cond_info = vect_stmt_to_vectorize (cond_info);
	}
      gcc_assert (ccompares.length () != 0);

      tree indx_before_incr, indx_after_incr;
      poly_uint64 nunits_out = TYPE_VECTOR_SUBPARTS (vectype);
      int scalar_precision
	= GET_MODE_PRECISION (SCALAR_TYPE_MODE (TREE_TYPE (vectype)));
      tree cr_index_scalar_type = make_unsigned_type (scalar_precision);
      tree cr_index_vector_type = get_related_vectype_for_scalar_type
	(TYPE_MODE (vectype), cr_index_scalar_type,
	 TYPE_VECTOR_SUBPARTS (vectype));

      /* First we create a simple vector induction variable which starts
	 with the values {1,2,3,...} (SERIES_VECT) and increments by the
	 vector size (STEP).  */

      /* Create a {1,2,3,...} vector.  */
      tree series_vect = build_index_vector (cr_index_vector_type, 1, 1);

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
      loop_vinfo->add_stmt (new_phi);
      add_phi_arg (as_a <gphi *> (new_phi), vec_zero,
		   loop_preheader_edge (loop), UNKNOWN_LOCATION);

      /* Now take the condition from the loops original cond_exprs
	 and produce a new cond_exprs (INDEX_COND_EXPR) which for
	 every match uses values from the induction variable
	 (INDEX_BEFORE_INCR) otherwise uses values from the phi node
	 (NEW_PHI_TREE).
	 Finally, we update the phi (NEW_PHI_TREE) to take the value of
	 the new cond_expr (INDEX_COND_EXPR).  */
      gimple_seq stmts = NULL;
      for (int i = ccompares.length () - 1; i != -1; --i)
	{
	  tree ccompare = ccompares[i].first;
	  if (ccompares[i].second)
	    new_phi_tree = gimple_build (&stmts, VEC_COND_EXPR,
					 cr_index_vector_type,
					 ccompare,
					 indx_before_incr, new_phi_tree);
	  else
	    new_phi_tree = gimple_build (&stmts, VEC_COND_EXPR,
					 cr_index_vector_type,
					 ccompare,
					 new_phi_tree, indx_before_incr);
	}
      gsi_insert_seq_before (&incr_gsi, stmts, GSI_SAME_STMT);
      stmt_vec_info index_vec_info
	= loop_vinfo->add_stmt (SSA_NAME_DEF_STMT (new_phi_tree));
      STMT_VINFO_VECTYPE (index_vec_info) = cr_index_vector_type;

      /* Update the phi with the vec cond.  */
      induction_index = new_phi_tree;
      add_phi_arg (as_a <gphi *> (new_phi), induction_index,
		   loop_latch_edge (loop), UNKNOWN_LOCATION);
    }

  /* 2. Create epilog code.
        The reduction epilog code operates across the elements of the vector
        of partial results computed by the vectorized loop.
        The reduction epilog code consists of:

        step 1: compute the scalar result in a vector (v_out2)
        step 2: extract the scalar result (s_out3) from the vector (v_out2)
        step 3: adjust the scalar result (s_out3) if needed.

        Step 1 can be accomplished using one the following three schemes:
          (scheme 1) using reduc_fn, if available.
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
  if (double_reduc)
    loop = outer_loop;
  exit_bb = single_exit (loop)->dest;
  prev_phi_info = NULL;
  new_phis.create (slp_node ? vec_num : ncopies);
  for (unsigned i = 0; i < vec_num; i++)
    {
      if (slp_node)
	def = gimple_get_lhs (SLP_TREE_VEC_STMTS (slp_node)[i]->stmt);
      else
	def = gimple_get_lhs (STMT_VINFO_VEC_STMT (rdef_info)->stmt);
      for (j = 0; j < ncopies; j++)
        {
	  tree new_def = copy_ssa_name (def);
          phi = create_phi_node (new_def, exit_bb);
	  stmt_vec_info phi_info = loop_vinfo->add_stmt (phi);
          if (j == 0)
            new_phis.quick_push (phi);
          else
	    {
	      def = vect_get_vec_def_for_stmt_copy (loop_vinfo, def);
	      STMT_VINFO_RELATED_STMT (prev_phi_info) = phi_info;
	    }

          SET_PHI_ARG_DEF (phi, single_exit (loop)->dest_idx, def);
	  prev_phi_info = phi_info;
        }
    }

  exit_gsi = gsi_after_labels (exit_bb);

  /* 2.2 Get the relevant tree-code to use in the epilog for schemes 2,3
         (i.e. when reduc_fn is not available) and in the final adjustment
	 code (if needed).  Also get the original scalar reduction variable as
         defined in the loop.  In case STMT is a "pattern-stmt" (i.e. - it
         represents a reduction pattern), the tree-code and scalar-def are
         taken from the original stmt that the pattern-stmt (STMT) replaces.
         Otherwise (it is a regular reduction) - the tree-code and scalar-def
         are taken from STMT.  */

  stmt_vec_info orig_stmt_info = vect_orig_stmt (stmt_info);
  if (orig_stmt_info != stmt_info)
    {
      /* Reduction pattern  */
      gcc_assert (STMT_VINFO_IN_PATTERN_P (orig_stmt_info));
      gcc_assert (STMT_VINFO_RELATED_STMT (orig_stmt_info) == stmt_info);
    }
  
  scalar_dest = gimple_assign_lhs (orig_stmt_info->stmt);
  scalar_type = TREE_TYPE (scalar_dest);
  scalar_results.create (group_size); 
  new_scalar_dest = vect_create_destination_var (scalar_dest, NULL);
  bitsize = TYPE_SIZE (scalar_type);

  /* SLP reduction without reduction chain, e.g.,
     # a1 = phi <a2, a0>
     # b1 = phi <b2, b0>
     a2 = operation (a1)
     b2 = operation (b1)  */
  slp_reduc = (slp_node && !REDUC_GROUP_FIRST_ELEMENT (stmt_info));

  /* True if we should implement SLP_REDUC using native reduction operations
     instead of scalar operations.  */
  direct_slp_reduc = (reduc_fn != IFN_LAST
		      && slp_reduc
		      && !TYPE_VECTOR_SUBPARTS (vectype).is_constant ());

  /* In case of reduction chain, e.g.,
     # a1 = phi <a3, a0>
     a2 = operation (a1)
     a3 = operation (a2),

     we may end up with more than one vector result.  Here we reduce them to
     one vector.  */
  if (REDUC_GROUP_FIRST_ELEMENT (stmt_info) || direct_slp_reduc)
    {
      gimple_seq stmts = NULL;
      tree first_vect = PHI_RESULT (new_phis[0]);
      first_vect = gimple_convert (&stmts, vectype, first_vect);
      for (k = 1; k < new_phis.length (); k++)
        {
	  gimple *next_phi = new_phis[k];
          tree second_vect = PHI_RESULT (next_phi);
	  second_vect = gimple_convert (&stmts, vectype, second_vect);
          first_vect = gimple_build (&stmts, code, vectype,
				     first_vect, second_vect);
        }
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);

      new_phi_result = first_vect;
      new_phis.truncate (0);
      new_phis.safe_push (SSA_NAME_DEF_STMT (first_vect));
    }
  /* Likewise if we couldn't use a single defuse cycle.  */
  else if (ncopies > 1)
    {
      gcc_assert (new_phis.length () == 1);
      gimple_seq stmts = NULL;
      tree first_vect = PHI_RESULT (new_phis[0]);
      first_vect = gimple_convert (&stmts, vectype, first_vect);
      stmt_vec_info next_phi_info = loop_vinfo->lookup_stmt (new_phis[0]);
      for (int k = 1; k < ncopies; ++k)
	{
	  next_phi_info = STMT_VINFO_RELATED_STMT (next_phi_info);
	  tree second_vect = PHI_RESULT (next_phi_info->stmt);
	  second_vect = gimple_convert (&stmts, vectype, second_vect);
	  first_vect = gimple_build (&stmts, code, vectype,
				     first_vect, second_vect);
	}
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);
      new_phi_result = first_vect;
      new_phis.truncate (0);
      new_phis.safe_push (SSA_NAME_DEF_STMT (first_vect));
    }
  else
    new_phi_result = PHI_RESULT (new_phis[0]);

  if (STMT_VINFO_REDUC_TYPE (reduc_info) == COND_REDUCTION
      && reduc_fn != IFN_LAST)
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
      tree index_vec_cmp_type = truth_type_for (index_vec_type);

      /* Get an unsigned integer version of the type of the data vector.  */
      int scalar_precision
	= GET_MODE_PRECISION (SCALAR_TYPE_MODE (scalar_type));
      tree scalar_type_unsigned = make_unsigned_type (scalar_precision);
      tree vectype_unsigned = build_vector_type
	(scalar_type_unsigned, TYPE_VECTOR_SUBPARTS (vectype));

      /* First we need to create a vector (ZERO_VEC) of zeros and another
	 vector (MAX_INDEX_VEC) filled with the last matching index, which we
	 can create using a MAX reduction and then expanding.
	 In the case where the loop never made any matches, the max index will
	 be zero.  */

      /* Vector of {0, 0, 0,...}.  */
      tree zero_vec = build_zero_cst (vectype);

      gimple_seq stmts = NULL;
      new_phi_result = gimple_convert (&stmts, vectype, new_phi_result);
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);

      /* Find maximum value from the vector of found indexes.  */
      tree max_index = make_ssa_name (index_scalar_type);
      gcall *max_index_stmt = gimple_build_call_internal (IFN_REDUC_MAX,
							  1, induction_index);
      gimple_call_set_lhs (max_index_stmt, max_index);
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
      gcall *data_reduc_stmt = gimple_build_call_internal (IFN_REDUC_MAX,
							   1, vec_cond_cast);
      gimple_call_set_lhs (data_reduc_stmt, data_reduc);
      gsi_insert_before (&exit_gsi, data_reduc_stmt, GSI_SAME_STMT);

      /* Convert the reduced value back to the result type and set as the
	 result.  */
      stmts = NULL;
      new_temp = gimple_build (&stmts, VIEW_CONVERT_EXPR, scalar_type,
			       data_reduc);
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);
      scalar_results.safe_push (new_temp);
    }
  else if (STMT_VINFO_REDUC_TYPE (reduc_info) == COND_REDUCTION
	   && reduc_fn == IFN_LAST)
    {
      /* Condition reduction without supported IFN_REDUC_MAX.  Generate
	 idx = 0;
         idx_val = induction_index[0];
	 val = data_reduc[0];
         for (idx = 0, val = init, i = 0; i < nelts; ++i)
	   if (induction_index[i] > idx_val)
	     val = data_reduc[i], idx_val = induction_index[i];
	 return val;  */

      tree data_eltype = TREE_TYPE (TREE_TYPE (new_phi_result));
      tree idx_eltype = TREE_TYPE (TREE_TYPE (induction_index));
      unsigned HOST_WIDE_INT el_size = tree_to_uhwi (TYPE_SIZE (idx_eltype));
      poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (TREE_TYPE (induction_index));
      /* Enforced by vectorizable_reduction, which ensures we have target
	 support before allowing a conditional reduction on variable-length
	 vectors.  */
      unsigned HOST_WIDE_INT v_size = el_size * nunits.to_constant ();
      tree idx_val = NULL_TREE, val = NULL_TREE;
      for (unsigned HOST_WIDE_INT off = 0; off < v_size; off += el_size)
	{
	  tree old_idx_val = idx_val;
	  tree old_val = val;
	  idx_val = make_ssa_name (idx_eltype);
	  epilog_stmt = gimple_build_assign (idx_val, BIT_FIELD_REF,
					     build3 (BIT_FIELD_REF, idx_eltype,
						     induction_index,
						     bitsize_int (el_size),
						     bitsize_int (off)));
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	  val = make_ssa_name (data_eltype);
	  epilog_stmt = gimple_build_assign (val, BIT_FIELD_REF,
					     build3 (BIT_FIELD_REF,
						     data_eltype,
						     new_phi_result,
						     bitsize_int (el_size),
						     bitsize_int (off)));
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	  if (off != 0)
	    {
	      tree new_idx_val = idx_val;
	      if (off != v_size - el_size)
		{
		  new_idx_val = make_ssa_name (idx_eltype);
		  epilog_stmt = gimple_build_assign (new_idx_val,
						     MAX_EXPR, idx_val,
						     old_idx_val);
		  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
		}
	      tree new_val = make_ssa_name (data_eltype);
	      epilog_stmt = gimple_build_assign (new_val,
						 COND_EXPR,
						 build2 (GT_EXPR,
							 boolean_type_node,
							 idx_val,
							 old_idx_val),
						 val, old_val);
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	      idx_val = new_idx_val;
	      val = new_val;
	    }
	}
      /* Convert the reduced value back to the result type and set as the
	 result.  */
      gimple_seq stmts = NULL;
      val = gimple_convert (&stmts, scalar_type, val);
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);
      scalar_results.safe_push (val);
    }

  /* 2.3 Create the reduction code, using one of the three schemes described
         above. In SLP we simply need to extract all the elements from the 
         vector (without reducing them), so we use scalar shifts.  */
  else if (reduc_fn != IFN_LAST && !slp_reduc)
    {
      tree tmp;
      tree vec_elem_type;

      /* Case 1:  Create:
         v_out2 = reduc_expr <v_out1>  */

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
			 "Reduce using direct vector reduction.\n");

      gimple_seq stmts = NULL;
      new_phi_result = gimple_convert (&stmts, vectype, new_phi_result);
      vec_elem_type = TREE_TYPE (TREE_TYPE (new_phi_result));
      new_temp = gimple_build (&stmts, as_combined_fn (reduc_fn),
			       vec_elem_type, new_phi_result);
      new_temp = gimple_convert (&stmts, scalar_type, new_temp);
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);

      if ((STMT_VINFO_REDUC_TYPE (reduc_info) == INTEGER_INDUC_COND_REDUCTION)
	  && induc_val)
	{
	  /* Earlier we set the initial value to be a vector if induc_val
	     values.  Check the result and if it is induc_val then replace
	     with the original initial value, unless induc_val is
	     the same as initial_def already.  */
	  tree zcompare = build2 (EQ_EXPR, boolean_type_node, new_temp,
				  induc_val);

	  tmp = make_ssa_name (new_scalar_dest);
	  epilog_stmt = gimple_build_assign (tmp, COND_EXPR, zcompare,
					     initial_def, new_temp);
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	  new_temp = tmp;
	}

      scalar_results.safe_push (new_temp);
    }
  else if (direct_slp_reduc)
    {
      /* Here we create one vector for each of the REDUC_GROUP_SIZE results,
	 with the elements for other SLP statements replaced with the
	 neutral value.  We can then do a normal reduction on each vector.  */

      /* Enforced by vectorizable_reduction.  */
      gcc_assert (new_phis.length () == 1);
      gcc_assert (pow2p_hwi (group_size));

      slp_tree orig_phis_slp_node = slp_node_instance->reduc_phis;
      vec<stmt_vec_info> orig_phis
	= SLP_TREE_SCALAR_STMTS (orig_phis_slp_node);
      gimple_seq seq = NULL;

      /* Build a vector {0, 1, 2, ...}, with the same number of elements
	 and the same element size as VECTYPE.  */
      tree index = build_index_vector (vectype, 0, 1);
      tree index_type = TREE_TYPE (index);
      tree index_elt_type = TREE_TYPE (index_type);
      tree mask_type = truth_type_for (index_type);

      /* Create a vector that, for each element, identifies which of
	 the REDUC_GROUP_SIZE results should use it.  */
      tree index_mask = build_int_cst (index_elt_type, group_size - 1);
      index = gimple_build (&seq, BIT_AND_EXPR, index_type, index,
			    build_vector_from_val (index_type, index_mask));

      /* Get a neutral vector value.  This is simply a splat of the neutral
	 scalar value if we have one, otherwise the initial scalar value
	 is itself a neutral value.  */
      tree vector_identity = NULL_TREE;
      tree neutral_op = NULL_TREE;
      if (slp_node)
	{
	  stmt_vec_info first = REDUC_GROUP_FIRST_ELEMENT (stmt_info);
	  neutral_op
	    = neutral_op_for_slp_reduction (slp_node_instance->reduc_phis,
					    vectype, code, first != NULL);
	}
      if (neutral_op)
	vector_identity = gimple_build_vector_from_val (&seq, vectype,
							neutral_op);
      for (unsigned int i = 0; i < group_size; ++i)
	{
	  /* If there's no univeral neutral value, we can use the
	     initial scalar value from the original PHI.  This is used
	     for MIN and MAX reduction, for example.  */
	  if (!neutral_op)
	    {
	      tree scalar_value
		= PHI_ARG_DEF_FROM_EDGE (orig_phis[i]->stmt,
					 loop_preheader_edge (loop));
	      scalar_value = gimple_convert (&seq, TREE_TYPE (vectype),
					     scalar_value);
	      vector_identity = gimple_build_vector_from_val (&seq, vectype,
							      scalar_value);
	    }

	  /* Calculate the equivalent of:

	     sel[j] = (index[j] == i);

	     which selects the elements of NEW_PHI_RESULT that should
	     be included in the result.  */
	  tree compare_val = build_int_cst (index_elt_type, i);
	  compare_val = build_vector_from_val (index_type, compare_val);
	  tree sel = gimple_build (&seq, EQ_EXPR, mask_type,
				   index, compare_val);

	  /* Calculate the equivalent of:

	     vec = seq ? new_phi_result : vector_identity;

	     VEC is now suitable for a full vector reduction.  */
	  tree vec = gimple_build (&seq, VEC_COND_EXPR, vectype,
				   sel, new_phi_result, vector_identity);

	  /* Do the reduction and convert it to the appropriate type.  */
	  tree scalar = gimple_build (&seq, as_combined_fn (reduc_fn),
				      TREE_TYPE (vectype), vec);
	  scalar = gimple_convert (&seq, scalar_type, scalar);
	  scalar_results.safe_push (scalar);
	}
      gsi_insert_seq_before (&exit_gsi, seq, GSI_SAME_STMT);
    }
  else
    {
      bool reduce_with_shift;
      tree vec_temp;

      gcc_assert (slp_reduc || new_phis.length () == 1);

      /* See if the target wants to do the final (shift) reduction
	 in a vector mode of smaller size and first reduce upper/lower
	 halves against each other.  */
      enum machine_mode mode1 = mode;
      tree stype = TREE_TYPE (vectype);
      unsigned nunits = TYPE_VECTOR_SUBPARTS (vectype).to_constant ();
      unsigned nunits1 = nunits;
      if ((mode1 = targetm.vectorize.split_reduction (mode)) != mode
	  && new_phis.length () == 1)
	{
	  nunits1 = GET_MODE_NUNITS (mode1).to_constant ();
	  /* For SLP reductions we have to make sure lanes match up, but
	     since we're doing individual element final reduction reducing
	     vector width here is even more important.
	     ???  We can also separate lanes with permutes, for the common
	     case of power-of-two group-size odd/even extracts would work.  */
	  if (slp_reduc && nunits != nunits1)
	    {
	      nunits1 = least_common_multiple (nunits1, group_size);
	      gcc_assert (exact_log2 (nunits1) != -1 && nunits1 <= nunits);
	    }
	}
      if (!slp_reduc
	  && (mode1 = targetm.vectorize.split_reduction (mode)) != mode)
	nunits1 = GET_MODE_NUNITS (mode1).to_constant ();

      tree vectype1 = get_related_vectype_for_scalar_type (TYPE_MODE (vectype),
							   stype, nunits1);
      reduce_with_shift = have_whole_vector_shift (mode1);
      if (!VECTOR_MODE_P (mode1))
	reduce_with_shift = false;
      else
	{
	  optab optab = optab_for_tree_code (code, vectype1, optab_default);
	  if (optab_handler (optab, mode1) == CODE_FOR_nothing)
	    reduce_with_shift = false;
	}

      /* First reduce the vector to the desired vector size we should
	 do shift reduction on by combining upper and lower halves.  */
      new_temp = new_phi_result;
      while (nunits > nunits1)
	{
	  nunits /= 2;
	  vectype1 = get_related_vectype_for_scalar_type (TYPE_MODE (vectype),
							  stype, nunits);
	  unsigned int bitsize = tree_to_uhwi (TYPE_SIZE (vectype1));

	  /* The target has to make sure we support lowpart/highpart
	     extraction, either via direct vector extract or through
	     an integer mode punning.  */
	  tree dst1, dst2;
	  if (convert_optab_handler (vec_extract_optab,
				     TYPE_MODE (TREE_TYPE (new_temp)),
				     TYPE_MODE (vectype1))
	      != CODE_FOR_nothing)
	    {
	      /* Extract sub-vectors directly once vec_extract becomes
		 a conversion optab.  */
	      dst1 = make_ssa_name (vectype1);
	      epilog_stmt
		  = gimple_build_assign (dst1, BIT_FIELD_REF,
					 build3 (BIT_FIELD_REF, vectype1,
						 new_temp, TYPE_SIZE (vectype1),
						 bitsize_int (0)));
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	      dst2 =  make_ssa_name (vectype1);
	      epilog_stmt
		  = gimple_build_assign (dst2, BIT_FIELD_REF,
					 build3 (BIT_FIELD_REF, vectype1,
						 new_temp, TYPE_SIZE (vectype1),
						 bitsize_int (bitsize)));
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	    }
	  else
	    {
	      /* Extract via punning to appropriately sized integer mode
		 vector.  */
	      tree eltype = build_nonstandard_integer_type (bitsize, 1);
	      tree etype = build_vector_type (eltype, 2);
	      gcc_assert (convert_optab_handler (vec_extract_optab,
						 TYPE_MODE (etype),
						 TYPE_MODE (eltype))
			  != CODE_FOR_nothing);
	      tree tem = make_ssa_name (etype);
	      epilog_stmt = gimple_build_assign (tem, VIEW_CONVERT_EXPR,
						 build1 (VIEW_CONVERT_EXPR,
							 etype, new_temp));
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	      new_temp = tem;
	      tem = make_ssa_name (eltype);
	      epilog_stmt
		  = gimple_build_assign (tem, BIT_FIELD_REF,
					 build3 (BIT_FIELD_REF, eltype,
						 new_temp, TYPE_SIZE (eltype),
						 bitsize_int (0)));
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	      dst1 = make_ssa_name (vectype1);
	      epilog_stmt = gimple_build_assign (dst1, VIEW_CONVERT_EXPR,
						 build1 (VIEW_CONVERT_EXPR,
							 vectype1, tem));
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	      tem = make_ssa_name (eltype);
	      epilog_stmt
		  = gimple_build_assign (tem, BIT_FIELD_REF,
					 build3 (BIT_FIELD_REF, eltype,
						 new_temp, TYPE_SIZE (eltype),
						 bitsize_int (bitsize)));
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	      dst2 =  make_ssa_name (vectype1);
	      epilog_stmt = gimple_build_assign (dst2, VIEW_CONVERT_EXPR,
						 build1 (VIEW_CONVERT_EXPR,
							 vectype1, tem));
	      gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	    }

	  new_temp = make_ssa_name (vectype1);
	  epilog_stmt = gimple_build_assign (new_temp, code, dst1, dst2);
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	  new_phis[0] = epilog_stmt;
	}

      if (reduce_with_shift && !slp_reduc)
	{
	  int element_bitsize = tree_to_uhwi (bitsize);
	  /* Enforced by vectorizable_reduction, which disallows SLP reductions
	     for variable-length vectors and also requires direct target support
	     for loop reductions.  */
	  int vec_size_in_bits = tree_to_uhwi (TYPE_SIZE (vectype1));
	  int nelements = vec_size_in_bits / element_bitsize;
	  vec_perm_builder sel;
	  vec_perm_indices indices;

          int elt_offset;

          tree zero_vec = build_zero_cst (vectype1);
          /* Case 2: Create:
             for (offset = nelements/2; offset >= 1; offset/=2)
                {
                  Create:  va' = vec_shift <va, offset>
                  Create:  va = vop <va, va'>
                }  */

          tree rhs;

          if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location,
			     "Reduce using vector shifts\n");

	  gimple_seq stmts = NULL;
	  new_temp = gimple_convert (&stmts, vectype1, new_temp);
          for (elt_offset = nelements / 2;
               elt_offset >= 1;
               elt_offset /= 2)
            {
	      calc_vec_perm_mask_for_shift (elt_offset, nelements, &sel);
	      indices.new_vector (sel, 2, nelements);
	      tree mask = vect_gen_perm_mask_any (vectype1, indices);
	      new_name = gimple_build (&stmts, VEC_PERM_EXPR, vectype1,
				       new_temp, zero_vec, mask);
	      new_temp = gimple_build (&stmts, code,
				       vectype1, new_name, new_temp);
            }
	  gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);

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
          /* Case 3: Create:
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

	  int vec_size_in_bits = tree_to_uhwi (TYPE_SIZE (vectype1));
	  int element_bitsize = tree_to_uhwi (bitsize);
	  tree compute_type = TREE_TYPE (vectype);
	  gimple_seq stmts = NULL;
          FOR_EACH_VEC_ELT (new_phis, i, new_phi)
            {
              int bit_offset;
              if (gimple_code (new_phi) == GIMPLE_PHI)
                vec_temp = PHI_RESULT (new_phi);
              else
                vec_temp = gimple_assign_lhs (new_phi);
	      new_temp = gimple_build (&stmts, BIT_FIELD_REF, compute_type,
				       vec_temp, bitsize, bitsize_zero_node);

              /* In SLP we don't need to apply reduction operation, so we just
                 collect s' values in SCALAR_RESULTS.  */
              if (slp_reduc)
                scalar_results.safe_push (new_temp);

              for (bit_offset = element_bitsize;
                   bit_offset < vec_size_in_bits;
                   bit_offset += element_bitsize)
                {
                  tree bitpos = bitsize_int (bit_offset);
		  new_name = gimple_build (&stmts, BIT_FIELD_REF,
					   compute_type, vec_temp,
					   bitsize, bitpos);
                  if (slp_reduc)
                    {
                      /* In SLP we don't need to apply reduction operation, so 
                         we just collect s' values in SCALAR_RESULTS.  */
                      new_temp = new_name;
                      scalar_results.safe_push (new_name);
                    }
                  else
		    new_temp = gimple_build (&stmts, code, compute_type,
					     new_name, new_temp);
                }
            }

          /* The only case where we need to reduce scalar results in SLP, is
             unrolling.  If the size of SCALAR_RESULTS is greater than
             REDUC_GROUP_SIZE, we reduce them combining elements modulo 
             REDUC_GROUP_SIZE.  */
          if (slp_reduc)
            {
              tree res, first_res, new_res;
            
              /* Reduce multiple scalar results in case of SLP unrolling.  */
              for (j = group_size; scalar_results.iterate (j, &res);
                   j++)
                {
                  first_res = scalar_results[j % group_size];
		  new_res = gimple_build (&stmts, code, compute_type,
					  first_res, res);
                  scalar_results[j % group_size] = new_res;
                }
	      for (k = 0; k < group_size; k++)
		scalar_results[k] = gimple_convert (&stmts, scalar_type,
						    scalar_results[k]);
            }
          else
	    {
	      /* Not SLP - we have one scalar to keep in SCALAR_RESULTS.  */
	      new_temp = gimple_convert (&stmts, scalar_type, new_temp);
	      scalar_results.safe_push (new_temp);
	    }

	  gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);
        }

      if ((STMT_VINFO_REDUC_TYPE (reduc_info) == INTEGER_INDUC_COND_REDUCTION)
	  && induc_val)
	{
	  /* Earlier we set the initial value to be a vector if induc_val
	     values.  Check the result and if it is induc_val then replace
	     with the original initial value, unless induc_val is
	     the same as initial_def already.  */
	  tree zcompare = build2 (EQ_EXPR, boolean_type_node, new_temp,
				  induc_val);

	  tree tmp = make_ssa_name (new_scalar_dest);
	  epilog_stmt = gimple_build_assign (tmp, COND_EXPR, zcompare,
					     initial_def, new_temp);
	  gsi_insert_before (&exit_gsi, epilog_stmt, GSI_SAME_STMT);
	  scalar_results[0] = tmp;
	}
    }
 
  /* 2.5 Adjust the final result by the initial value of the reduction
	 variable. (When such adjustment is not needed, then
	 'adjustment_def' is zero).  For example, if code is PLUS we create:
	 new_temp = loop_exit_def + adjustment_def  */

  if (adjustment_def)
    {
      gcc_assert (!slp_reduc);
      gimple_seq stmts = NULL;
      if (nested_in_vect_loop)
	{
          new_phi = new_phis[0];
	  gcc_assert (VECTOR_TYPE_P (TREE_TYPE (adjustment_def)));
	  adjustment_def = gimple_convert (&stmts, vectype, adjustment_def);
	  new_temp = gimple_build (&stmts, code, vectype,
				   PHI_RESULT (new_phi), adjustment_def);
	}
      else
	{
          new_temp = scalar_results[0];
	  gcc_assert (TREE_CODE (TREE_TYPE (adjustment_def)) != VECTOR_TYPE);
	  adjustment_def = gimple_convert (&stmts, scalar_type, adjustment_def);
	  new_temp = gimple_build (&stmts, code, scalar_type,
				   new_temp, adjustment_def);
	}

      epilog_stmt = gimple_seq_last_stmt (stmts);
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);
      if (nested_in_vect_loop)
        {
	  stmt_vec_info epilog_stmt_info = loop_vinfo->add_stmt (epilog_stmt);
	  STMT_VINFO_RELATED_STMT (epilog_stmt_info)
	    = STMT_VINFO_RELATED_STMT (loop_vinfo->lookup_stmt (new_phi));

          if (!double_reduc)
            scalar_results.quick_push (new_temp);
          else
            scalar_results[0] = new_temp;
        }
      else
        scalar_results[0] = new_temp;

      new_phis[0] = epilog_stmt;
    }

  if (double_reduc)
    loop = loop->inner;

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
     necessary, hence we set here REDUC_GROUP_SIZE to 1.  SCALAR_DEST is the
     LHS of the last stmt in the reduction chain, since we are looking for
     the loop exit phi node.  */
  if (REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    {
      stmt_vec_info dest_stmt_info
	= vect_orig_stmt (SLP_TREE_SCALAR_STMTS (slp_node)[group_size - 1]);
      scalar_dest = gimple_assign_lhs (dest_stmt_info->stmt);
      group_size = 1;
    }

  /* In SLP we may have several statements in NEW_PHIS and REDUCTION_PHIS (in
     case that REDUC_GROUP_SIZE is greater than vectorization factor).
     Therefore, we need to match SCALAR_RESULTS with corresponding statements.
     The first (REDUC_GROUP_SIZE / number of new vector stmts) scalar results
     correspond to the first vector stmt, etc.
     (RATIO is equal to (REDUC_GROUP_SIZE / number of new vector stmts)).  */
  if (group_size > new_phis.length ())
    gcc_assert (!(group_size % new_phis.length ()));

  for (k = 0; k < group_size; k++)
    {
      if (slp_reduc)
        {
	  stmt_vec_info scalar_stmt_info = SLP_TREE_SCALAR_STMTS (slp_node)[k];

	  orig_stmt_info = STMT_VINFO_RELATED_STMT (scalar_stmt_info);
	  /* SLP statements can't participate in patterns.  */
	  gcc_assert (!orig_stmt_info);
	  scalar_dest = gimple_assign_lhs (scalar_stmt_info->stmt);
        }

      if (nested_in_vect_loop)
        {
          if (double_reduc)
            loop = outer_loop;
          else
	    gcc_unreachable ();
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
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		SET_USE (use_p, scalar_result);
	      update_stmt (use_stmt);
	    }
        }

      phis.release ();
    }
}

/* Return a vector of type VECTYPE that is equal to the vector select
   operation "MASK ? VEC : IDENTITY".  Insert the select statements
   before GSI.  */

static tree
merge_with_identity (gimple_stmt_iterator *gsi, tree mask, tree vectype,
		     tree vec, tree identity)
{
  tree cond = make_temp_ssa_name (vectype, NULL, "cond");
  gimple *new_stmt = gimple_build_assign (cond, VEC_COND_EXPR,
					  mask, vec, identity);
  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
  return cond;
}

/* Successively apply CODE to each element of VECTOR_RHS, in left-to-right
   order, starting with LHS.  Insert the extraction statements before GSI and
   associate the new scalar SSA names with variable SCALAR_DEST.
   Return the SSA name for the result.  */

static tree
vect_expand_fold_left (gimple_stmt_iterator *gsi, tree scalar_dest,
		       tree_code code, tree lhs, tree vector_rhs)
{
  tree vectype = TREE_TYPE (vector_rhs);
  tree scalar_type = TREE_TYPE (vectype);
  tree bitsize = TYPE_SIZE (scalar_type);
  unsigned HOST_WIDE_INT vec_size_in_bits = tree_to_uhwi (TYPE_SIZE (vectype));
  unsigned HOST_WIDE_INT element_bitsize = tree_to_uhwi (bitsize);

  for (unsigned HOST_WIDE_INT bit_offset = 0;
       bit_offset < vec_size_in_bits;
       bit_offset += element_bitsize)
    {
      tree bitpos = bitsize_int (bit_offset);
      tree rhs = build3 (BIT_FIELD_REF, scalar_type, vector_rhs,
			 bitsize, bitpos);

      gassign *stmt = gimple_build_assign (scalar_dest, rhs);
      rhs = make_ssa_name (scalar_dest, stmt);
      gimple_assign_set_lhs (stmt, rhs);
      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

      stmt = gimple_build_assign (scalar_dest, code, lhs, rhs);
      tree new_name = make_ssa_name (scalar_dest, stmt);
      gimple_assign_set_lhs (stmt, new_name);
      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
      lhs = new_name;
    }
  return lhs;
}

/* Get a masked internal function equivalent to REDUC_FN.  VECTYPE_IN is the
   type of the vector input.  */

static internal_fn
get_masked_reduction_fn (internal_fn reduc_fn, tree vectype_in)
{
  internal_fn mask_reduc_fn;

  switch (reduc_fn)
    {
    case IFN_FOLD_LEFT_PLUS:
      mask_reduc_fn = IFN_MASK_FOLD_LEFT_PLUS;
      break;

    default:
      return IFN_LAST;
    }

  if (direct_internal_fn_supported_p (mask_reduc_fn, vectype_in,
				      OPTIMIZE_FOR_SPEED))
    return mask_reduc_fn;
  return IFN_LAST;
}

/* Perform an in-order reduction (FOLD_LEFT_REDUCTION).  STMT_INFO is the
   statement that sets the live-out value.  REDUC_DEF_STMT is the phi
   statement.  CODE is the operation performed by STMT_INFO and OPS are
   its scalar operands.  REDUC_INDEX is the index of the operand in
   OPS that is set by REDUC_DEF_STMT.  REDUC_FN is the function that
   implements in-order reduction, or IFN_LAST if we should open-code it.
   VECTYPE_IN is the type of the vector input.  MASKS specifies the masks
   that should be used to control the operation in a fully-masked loop.  */

static bool
vectorize_fold_left_reduction (stmt_vec_info stmt_info,
			       gimple_stmt_iterator *gsi,
			       stmt_vec_info *vec_stmt, slp_tree slp_node,
			       gimple *reduc_def_stmt,
			       tree_code code, internal_fn reduc_fn,
			       tree ops[3], tree vectype_in,
			       int reduc_index, vec_loop_masks *masks)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree vectype_out = STMT_VINFO_VECTYPE (stmt_info);
  stmt_vec_info new_stmt_info = NULL;
  internal_fn mask_reduc_fn = get_masked_reduction_fn (reduc_fn, vectype_in);

  int ncopies;
  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype_in);

  gcc_assert (!nested_in_vect_loop_p (loop, stmt_info));
  gcc_assert (ncopies == 1);
  gcc_assert (TREE_CODE_LENGTH (code) == binary_op);

  if (slp_node)
    gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (vectype_out),
			  TYPE_VECTOR_SUBPARTS (vectype_in)));

  tree op0 = ops[1 - reduc_index];

  int group_size = 1;
  stmt_vec_info scalar_dest_def_info;
  auto_vec<tree> vec_oprnds0;
  if (slp_node)
    {
      auto_vec<vec<tree> > vec_defs (2);
      vect_get_slp_defs (slp_node, &vec_defs);
      vec_oprnds0.safe_splice (vec_defs[1 - reduc_index]);
      vec_defs[0].release ();
      vec_defs[1].release ();
      group_size = SLP_TREE_SCALAR_STMTS (slp_node).length ();
      scalar_dest_def_info = SLP_TREE_SCALAR_STMTS (slp_node)[group_size - 1];
    }
  else
    {
      tree loop_vec_def0 = vect_get_vec_def_for_operand (op0, stmt_info);
      vec_oprnds0.create (1);
      vec_oprnds0.quick_push (loop_vec_def0);
      scalar_dest_def_info = stmt_info;
    }

  tree scalar_dest = gimple_assign_lhs (scalar_dest_def_info->stmt);
  tree scalar_type = TREE_TYPE (scalar_dest);
  tree reduc_var = gimple_phi_result (reduc_def_stmt);

  int vec_num = vec_oprnds0.length ();
  gcc_assert (vec_num == 1 || slp_node);
  tree vec_elem_type = TREE_TYPE (vectype_out);
  gcc_checking_assert (useless_type_conversion_p (scalar_type, vec_elem_type));

  tree vector_identity = NULL_TREE;
  if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    vector_identity = build_zero_cst (vectype_out);

  tree scalar_dest_var = vect_create_destination_var (scalar_dest, NULL);
  int i;
  tree def0;
  FOR_EACH_VEC_ELT (vec_oprnds0, i, def0)
    {
      gimple *new_stmt;
      tree mask = NULL_TREE;
      if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
	mask = vect_get_loop_mask (gsi, masks, vec_num, vectype_in, i);

      /* Handle MINUS by adding the negative.  */
      if (reduc_fn != IFN_LAST && code == MINUS_EXPR)
	{
	  tree negated = make_ssa_name (vectype_out);
	  new_stmt = gimple_build_assign (negated, NEGATE_EXPR, def0);
	  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
	  def0 = negated;
	}

      if (mask && mask_reduc_fn == IFN_LAST)
	def0 = merge_with_identity (gsi, mask, vectype_out, def0,
				    vector_identity);

      /* On the first iteration the input is simply the scalar phi
	 result, and for subsequent iterations it is the output of
	 the preceding operation.  */
      if (reduc_fn != IFN_LAST || (mask && mask_reduc_fn != IFN_LAST))
	{
	  if (mask && mask_reduc_fn != IFN_LAST)
	    new_stmt = gimple_build_call_internal (mask_reduc_fn, 3, reduc_var,
						   def0, mask);
	  else
	    new_stmt = gimple_build_call_internal (reduc_fn, 2, reduc_var,
						   def0);
	  /* For chained SLP reductions the output of the previous reduction
	     operation serves as the input of the next. For the final statement
	     the output cannot be a temporary - we reuse the original
	     scalar destination of the last statement.  */
	  if (i != vec_num - 1)
	    {
	      gimple_set_lhs (new_stmt, scalar_dest_var);
	      reduc_var = make_ssa_name (scalar_dest_var, new_stmt);
	      gimple_set_lhs (new_stmt, reduc_var);
	    }
	}
      else
	{
	  reduc_var = vect_expand_fold_left (gsi, scalar_dest_var, code,
					     reduc_var, def0);
	  new_stmt = SSA_NAME_DEF_STMT (reduc_var);
	  /* Remove the statement, so that we can use the same code paths
	     as for statements that we've just created.  */
	  gimple_stmt_iterator tmp_gsi = gsi_for_stmt (new_stmt);
	  gsi_remove (&tmp_gsi, true);
	}

      if (i == vec_num - 1)
	{
	  gimple_set_lhs (new_stmt, scalar_dest);
	  new_stmt_info = vect_finish_replace_stmt (scalar_dest_def_info,
						    new_stmt);
	}
      else
	new_stmt_info = vect_finish_stmt_generation (scalar_dest_def_info,
						     new_stmt, gsi);

      if (slp_node)
	SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
    }

  if (!slp_node)
    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;

  return true;
}

/* Function is_nonwrapping_integer_induction.

   Check if STMT_VINO (which is part of loop LOOP) both increments and
   does not cause overflow.  */

static bool
is_nonwrapping_integer_induction (stmt_vec_info stmt_vinfo, class loop *loop)
{
  gphi *phi = as_a <gphi *> (stmt_vinfo->stmt);
  tree base = STMT_VINFO_LOOP_PHI_EVOLUTION_BASE_UNCHANGED (stmt_vinfo);
  tree step = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (stmt_vinfo);
  tree lhs_type = TREE_TYPE (gimple_phi_result (phi));
  widest_int ni, max_loop_value, lhs_max;
  wi::overflow_type overflow = wi::OVF_NONE;

  /* Make sure the loop is integer based.  */
  if (TREE_CODE (base) != INTEGER_CST
      || TREE_CODE (step) != INTEGER_CST)
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

/* Check if masking can be supported by inserting a conditional expression.
   CODE is the code for the operation.  COND_FN is the conditional internal
   function, if it exists.  VECTYPE_IN is the type of the vector input.  */
static bool
use_mask_by_cond_expr_p (enum tree_code code, internal_fn cond_fn,
			 tree vectype_in)
{
  if (cond_fn != IFN_LAST
      && direct_internal_fn_supported_p (cond_fn, vectype_in,
					 OPTIMIZE_FOR_SPEED))
    return false;

  switch (code)
    {
    case DOT_PROD_EXPR:
    case SAD_EXPR:
      return true;

    default:
      return false;
    }
}

/* Insert a conditional expression to enable masked vectorization.  CODE is the
   code for the operation.  VOP is the array of operands.  MASK is the loop
   mask.  GSI is a statement iterator used to place the new conditional
   expression.  */
static void
build_vect_cond_expr (enum tree_code code, tree vop[3], tree mask,
		      gimple_stmt_iterator *gsi)
{
  switch (code)
    {
    case DOT_PROD_EXPR:
      {
	tree vectype = TREE_TYPE (vop[1]);
	tree zero = build_zero_cst (vectype);
	tree masked_op1 = make_temp_ssa_name (vectype, NULL, "masked_op1");
	gassign *select = gimple_build_assign (masked_op1, VEC_COND_EXPR,
					       mask, vop[1], zero);
	gsi_insert_before (gsi, select, GSI_SAME_STMT);
	vop[1] = masked_op1;
	break;
      }

    case SAD_EXPR:
      {
	tree vectype = TREE_TYPE (vop[1]);
	tree masked_op1 = make_temp_ssa_name (vectype, NULL, "masked_op1");
	gassign *select = gimple_build_assign (masked_op1, VEC_COND_EXPR,
					       mask, vop[1], vop[0]);
	gsi_insert_before (gsi, select, GSI_SAME_STMT);
	vop[1] = masked_op1;
	break;
      }

    default:
      gcc_unreachable ();
    }
}

/* Function vectorizable_reduction.

   Check if STMT_INFO performs a reduction operation that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.

   This function also handles reduction idioms (patterns) that have been
   recognized in advance during vect_pattern_recog.  In this case, STMT_INFO
   may be of this form:
     X = pattern_expr (arg0, arg1, ..., X)
   and its STMT_VINFO_RELATED_STMT points to the last stmt in the original
   sequence that had been detected and replaced by the pattern-stmt
   (STMT_INFO).

   This function also handles reduction of condition expressions, for example:
     for (int i = 0; i < N; i++)
       if (a[i] < value)
	 last = a[i];
   This is handled by vectorising the loop and creating an additional vector
   containing the loop indexes for which "a[i] < value" was true.  In the
   function epilogue this is reduced to a single max value and then used to
   index into the vector of results.

   In some cases of reduction patterns, the type of the reduction variable X is
   different than the type of the other arguments of STMT_INFO.
   In such cases, the vectype that is used when transforming STMT_INFO into
   a vector stmt is different than the vectype that is used to determine the
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
      get_vectype_for_scalar_type (vinfo, TREE_TYPE (X))

   This means that, contrary to "regular" reductions (or "regular" stmts in
   general), the following equation:
      STMT_VINFO_VECTYPE == get_vectype_for_scalar_type (vinfo, TREE_TYPE (X))
   does *NOT* necessarily hold for reduction patterns.  */

bool
vectorizable_reduction (stmt_vec_info stmt_info, slp_tree slp_node,
			slp_instance slp_node_instance,
			stmt_vector_for_cost *cost_vec)
{
  tree scalar_dest;
  tree vectype_in = NULL_TREE;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  enum vect_def_type cond_reduc_dt = vect_unknown_def_type;
  stmt_vec_info cond_stmt_vinfo = NULL;
  tree scalar_type;
  int i;
  int ncopies;
  bool single_defuse_cycle = false;
  bool nested_cycle = false;
  bool double_reduc = false;
  int vec_num;
  tree tem;
  tree cr_index_scalar_type = NULL_TREE, cr_index_vector_type = NULL_TREE;
  tree cond_reduc_val = NULL_TREE;

  /* Make sure it was already recognized as a reduction computation.  */
  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_reduction_def
      && STMT_VINFO_DEF_TYPE (stmt_info) != vect_double_reduction_def
      && STMT_VINFO_DEF_TYPE (stmt_info) != vect_nested_cycle)
    return false;

  /* The stmt we store reduction analysis meta on.  */
  stmt_vec_info reduc_info = info_for_reduction (stmt_info);
  reduc_info->is_reduc_info = true;

  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_nested_cycle)
    {
      if (is_a <gphi *> (stmt_info->stmt))
	/* Analysis for double-reduction is done on the outer
	   loop PHI, nested cycles have no further restrictions.  */
	STMT_VINFO_TYPE (stmt_info) = cycle_phi_info_type;
      else
	STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
      return true;
    }

  stmt_vec_info orig_stmt_of_analysis = stmt_info;
  stmt_vec_info phi_info = stmt_info;
  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def
      || STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
    {
      if (!is_a <gphi *> (stmt_info->stmt))
	{
	  STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
	  return true;
	}
      if (slp_node)
	{
	  slp_node_instance->reduc_phis = slp_node;
	  /* ???  We're leaving slp_node to point to the PHIs, we only
	     need it to get at the number of vector stmts which wasn't
	     yet initialized for the instance root.  */
	}
      if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def)
	stmt_info = vect_stmt_to_vectorize (STMT_VINFO_REDUC_DEF (stmt_info));
      else /* STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def */
	{
	  use_operand_p use_p;
	  gimple *use_stmt;
	  bool res = single_imm_use (gimple_phi_result (stmt_info->stmt),
				     &use_p, &use_stmt);
	  gcc_assert (res);
	  phi_info = loop_vinfo->lookup_stmt (use_stmt);
	  stmt_info = vect_stmt_to_vectorize (STMT_VINFO_REDUC_DEF (phi_info));
	}
    }

  /* PHIs should not participate in patterns.  */
  gcc_assert (!STMT_VINFO_RELATED_STMT (phi_info));
  gphi *reduc_def_phi = as_a <gphi *> (phi_info->stmt);

  /* Verify following REDUC_IDX from the latch def leads us back to the PHI
     and compute the reduction chain length.  */
  tree reduc_def = PHI_ARG_DEF_FROM_EDGE (reduc_def_phi,
					  loop_latch_edge (loop));
  unsigned reduc_chain_length = 0;
  bool only_slp_reduc_chain = true;
  stmt_info = NULL;
  while (reduc_def != PHI_RESULT (reduc_def_phi))
    {
      stmt_vec_info def = loop_vinfo->lookup_def (reduc_def);
      stmt_vec_info vdef = vect_stmt_to_vectorize (def);
      if (STMT_VINFO_REDUC_IDX (vdef) == -1)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "reduction chain broken by patterns.\n");
	  return false;
	}
      if (!REDUC_GROUP_FIRST_ELEMENT (vdef))
	only_slp_reduc_chain = false;
      /* ???  For epilogue generation live members of the chain need
         to point back to the PHI via their original stmt for
	 info_for_reduction to work.  */
      if (STMT_VINFO_LIVE_P (vdef))
	STMT_VINFO_REDUC_DEF (def) = phi_info;
      gassign *assign = dyn_cast <gassign *> (vdef->stmt);
      if (!assign)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "reduction chain includes calls.\n");
	  return false;
	}
      if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (assign)))
	{
	  if (!tree_nop_conversion_p (TREE_TYPE (gimple_assign_lhs (assign)),
				      TREE_TYPE (gimple_assign_rhs1 (assign))))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "conversion in the reduction chain.\n");
	      return false;
	    }
	}
      else if (!stmt_info)
	/* First non-conversion stmt.  */
	stmt_info = vdef;
      reduc_def = gimple_op (vdef->stmt, 1 + STMT_VINFO_REDUC_IDX (vdef));
      reduc_chain_length++;
    }
  /* PHIs should not participate in patterns.  */
  gcc_assert (!STMT_VINFO_RELATED_STMT (phi_info));

  if (nested_in_vect_loop_p (loop, stmt_info))
    {
      loop = loop->inner;
      nested_cycle = true;
    }

  /* STMT_VINFO_REDUC_DEF doesn't point to the first but the last
     element.  */
  if (slp_node && REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    {
      gcc_assert (!REDUC_GROUP_NEXT_ELEMENT (stmt_info));
      stmt_info = REDUC_GROUP_FIRST_ELEMENT (stmt_info);
    }
  if (REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    gcc_assert (slp_node
		&& REDUC_GROUP_FIRST_ELEMENT (stmt_info) == stmt_info);

  /* 1. Is vectorizable reduction?  */
  /* Not supportable if the reduction variable is used in the loop, unless
     it's a reduction chain.  */
  if (STMT_VINFO_RELEVANT (stmt_info) > vect_used_in_outer
      && !REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    return false;

  /* Reductions that are not used even in an enclosing outer-loop,
     are expected to be "live" (used out of the loop).  */
  if (STMT_VINFO_RELEVANT (stmt_info) == vect_unused_in_scope
      && !STMT_VINFO_LIVE_P (stmt_info))
    return false;

  /* 2. Has this been recognized as a reduction pattern?

     Check if STMT represents a pattern that has been recognized
     in earlier analysis stages.  For stmts that represent a pattern,
     the STMT_VINFO_RELATED_STMT field records the last stmt in
     the original sequence that constitutes the pattern.  */

  stmt_vec_info orig_stmt_info = STMT_VINFO_RELATED_STMT (stmt_info);
  if (orig_stmt_info)
    {
      gcc_assert (STMT_VINFO_IN_PATTERN_P (orig_stmt_info));
      gcc_assert (!STMT_VINFO_IN_PATTERN_P (stmt_info));
    }

  /* 3. Check the operands of the operation.  The first operands are defined
        inside the loop body. The last operand is the reduction variable,
        which is defined by the loop-header-phi.  */

  tree vectype_out = STMT_VINFO_VECTYPE (stmt_info);
  STMT_VINFO_REDUC_VECTYPE (reduc_info) = vectype_out;
  gassign *stmt = as_a <gassign *> (stmt_info->stmt);
  enum tree_code code = gimple_assign_rhs_code (stmt);
  bool lane_reduc_code_p
    = (code == DOT_PROD_EXPR || code == WIDEN_SUM_EXPR || code == SAD_EXPR);
  int op_type = TREE_CODE_LENGTH (code);

  scalar_dest = gimple_assign_lhs (stmt);
  scalar_type = TREE_TYPE (scalar_dest);
  if (!POINTER_TYPE_P (scalar_type) && !INTEGRAL_TYPE_P (scalar_type)
      && !SCALAR_FLOAT_TYPE_P (scalar_type))
    return false;

  /* Do not try to vectorize bit-precision reductions.  */
  if (!type_has_mode_precision_p (scalar_type))
    return false;

  /* For lane-reducing ops we're reducing the number of reduction PHIs
     which means the only use of that may be in the lane-reducing operation.  */
  if (lane_reduc_code_p
      && reduc_chain_length != 1
      && !only_slp_reduc_chain)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "lane-reducing reduction with extra stmts.\n");
      return false;
    }

  /* All uses but the last are expected to be defined in the loop.
     The last use is the reduction variable.  In case of nested cycle this
     assumption is not true: we use reduc_index to record the index of the
     reduction variable.  */
  reduc_def = PHI_RESULT (reduc_def_phi);
  for (i = 0; i < op_type; i++)
    {
      tree op = gimple_op (stmt, i + 1);
      /* The condition of COND_EXPR is checked in vectorizable_condition().  */
      if (i == 0 && code == COND_EXPR)
        continue;

      stmt_vec_info def_stmt_info;
      enum vect_def_type dt;
      if (!vect_is_simple_use (op, loop_vinfo, &dt, &tem,
			       &def_stmt_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "use not simple.\n");
	  return false;
	}
      if (i == STMT_VINFO_REDUC_IDX (stmt_info))
	continue;

      /* There should be only one cycle def in the stmt, the one
         leading to reduc_def.  */
      if (VECTORIZABLE_CYCLE_DEF (dt))
	return false;

      /* To properly compute ncopies we are interested in the widest
	 non-reduction input type in case we're looking at a widening
	 accumulation that we later handle in vect_transform_reduction.  */
      if (lane_reduc_code_p
	  && tem
	  && (!vectype_in
	      || (GET_MODE_SIZE (SCALAR_TYPE_MODE (TREE_TYPE (vectype_in)))
		  < GET_MODE_SIZE (SCALAR_TYPE_MODE (TREE_TYPE (tem))))))
	vectype_in = tem;

      if (code == COND_EXPR)
	{
	  /* Record how the non-reduction-def value of COND_EXPR is defined.  */
	  if (dt == vect_constant_def)
	    {
	      cond_reduc_dt = dt;
	      cond_reduc_val = op;
	    }
	  if (dt == vect_induction_def
	      && def_stmt_info
	      && is_nonwrapping_integer_induction (def_stmt_info, loop))
	    {
	      cond_reduc_dt = dt;
	      cond_stmt_vinfo = def_stmt_info;
	    }
	}
    }
  if (!vectype_in)
    vectype_in = STMT_VINFO_VECTYPE (phi_info);
  STMT_VINFO_REDUC_VECTYPE_IN (reduc_info) = vectype_in;

  enum vect_reduction_type v_reduc_type = STMT_VINFO_REDUC_TYPE (phi_info);
  STMT_VINFO_REDUC_TYPE (reduc_info) = v_reduc_type;
  /* If we have a condition reduction, see if we can simplify it further.  */
  if (v_reduc_type == COND_REDUCTION)
    {
      if (slp_node)
	return false;

      /* When the condition uses the reduction value in the condition, fail.  */
      if (STMT_VINFO_REDUC_IDX (stmt_info) == 0)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "condition depends on previous iteration\n");
	  return false;
	}

      if (reduc_chain_length == 1
	  && direct_internal_fn_supported_p (IFN_FOLD_EXTRACT_LAST,
					     vectype_in, OPTIMIZE_FOR_SPEED))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "optimizing condition reduction with"
			     " FOLD_EXTRACT_LAST.\n");
	  STMT_VINFO_REDUC_TYPE (reduc_info) = EXTRACT_LAST_REDUCTION;
	}
      else if (cond_reduc_dt == vect_induction_def)
	{
	  tree base
	    = STMT_VINFO_LOOP_PHI_EVOLUTION_BASE_UNCHANGED (cond_stmt_vinfo);
	  tree step = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (cond_stmt_vinfo);

	  gcc_assert (TREE_CODE (base) == INTEGER_CST
		      && TREE_CODE (step) == INTEGER_CST);
	  cond_reduc_val = NULL_TREE;
	  enum tree_code cond_reduc_op_code = ERROR_MARK;
	  tree res = PHI_RESULT (STMT_VINFO_STMT (cond_stmt_vinfo));
	  if (!types_compatible_p (TREE_TYPE (res), TREE_TYPE (base)))
	    ;
	  /* Find a suitable value, for MAX_EXPR below base, for MIN_EXPR
	     above base; punt if base is the minimum value of the type for
	     MAX_EXPR or maximum value of the type for MIN_EXPR for now.  */
	  else if (tree_int_cst_sgn (step) == -1)
	    {
	      cond_reduc_op_code = MIN_EXPR;
	      if (tree_int_cst_sgn (base) == -1)
		cond_reduc_val = build_int_cst (TREE_TYPE (base), 0);
	      else if (tree_int_cst_lt (base,
					TYPE_MAX_VALUE (TREE_TYPE (base))))
		cond_reduc_val
		  = int_const_binop (PLUS_EXPR, base, integer_one_node);
	    }
	  else
	    {
	      cond_reduc_op_code = MAX_EXPR;
	      if (tree_int_cst_sgn (base) == 1)
		cond_reduc_val = build_int_cst (TREE_TYPE (base), 0);
	      else if (tree_int_cst_lt (TYPE_MIN_VALUE (TREE_TYPE (base)),
					base))
		cond_reduc_val
		  = int_const_binop (MINUS_EXPR, base, integer_one_node);
	    }
	  if (cond_reduc_val)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "condition expression based on "
				 "integer induction.\n");
	      STMT_VINFO_REDUC_CODE (reduc_info) = cond_reduc_op_code;
	      STMT_VINFO_VEC_INDUC_COND_INITIAL_VAL (reduc_info)
		= cond_reduc_val;
	      STMT_VINFO_REDUC_TYPE (reduc_info) = INTEGER_INDUC_COND_REDUCTION;
	    }
	}
      else if (cond_reduc_dt == vect_constant_def)
	{
	  enum vect_def_type cond_initial_dt;
	  tree cond_initial_val
	    = PHI_ARG_DEF_FROM_EDGE (reduc_def_phi, loop_preheader_edge (loop));

	  gcc_assert (cond_reduc_val != NULL_TREE);
	  vect_is_simple_use (cond_initial_val, loop_vinfo, &cond_initial_dt);
	  if (cond_initial_dt == vect_constant_def
	      && types_compatible_p (TREE_TYPE (cond_initial_val),
				     TREE_TYPE (cond_reduc_val)))
	    {
	      tree e = fold_binary (LE_EXPR, boolean_type_node,
				    cond_initial_val, cond_reduc_val);
	      if (e && (integer_onep (e) || integer_zerop (e)))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "condition expression based on "
				     "compile time constant.\n");
		  /* Record reduction code at analysis stage.  */
		  STMT_VINFO_REDUC_CODE (reduc_info)
		    = integer_onep (e) ? MAX_EXPR : MIN_EXPR;
		  STMT_VINFO_REDUC_TYPE (reduc_info) = CONST_COND_REDUCTION;
		}
	    }
	}
    }

  if (STMT_VINFO_LIVE_P (phi_info))
    return false;

  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype_in);

  gcc_assert (ncopies >= 1);

  poly_uint64 nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);

  if (nested_cycle)
    {
      gcc_assert (STMT_VINFO_DEF_TYPE (reduc_info)
		  == vect_double_reduction_def);
      double_reduc = true;
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

  enum tree_code orig_code = STMT_VINFO_REDUC_CODE (phi_info);
  STMT_VINFO_REDUC_CODE (reduc_info) = orig_code;

  vect_reduction_type reduction_type = STMT_VINFO_REDUC_TYPE (reduc_info);
  if (reduction_type == TREE_CODE_REDUCTION)
    {
      /* Check whether it's ok to change the order of the computation.
	 Generally, when vectorizing a reduction we change the order of the
	 computation.  This may change the behavior of the program in some
	 cases, so we need to check that this is ok.  One exception is when
	 vectorizing an outer-loop: the inner-loop is executed sequentially,
	 and therefore vectorizing reductions in the inner-loop during
	 outer-loop vectorization is safe.  */
      if (needs_fold_left_reduction_p (scalar_type, orig_code))
	{
	  /* When vectorizing a reduction chain w/o SLP the reduction PHI
	     is not directy used in stmt.  */
	  if (!only_slp_reduc_chain
	      && reduc_chain_length != 1)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "in-order reduction chain without SLP.\n");
	      return false;
	    }
	  STMT_VINFO_REDUC_TYPE (reduc_info)
	    = reduction_type = FOLD_LEFT_REDUCTION;
	}
      else if (!commutative_tree_code (orig_code)
	       || !associative_tree_code (orig_code))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			    "reduction: not commutative/associative");
	  return false;
	}
    }

  if ((double_reduc || reduction_type != TREE_CODE_REDUCTION)
      && ncopies > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "multiple types in double reduction or condition "
			 "reduction or fold-left reduction.\n");
      return false;
    }

  internal_fn reduc_fn = IFN_LAST;
  if (reduction_type == TREE_CODE_REDUCTION
      || reduction_type == FOLD_LEFT_REDUCTION
      || reduction_type == INTEGER_INDUC_COND_REDUCTION
      || reduction_type == CONST_COND_REDUCTION)
    {
      if (reduction_type == FOLD_LEFT_REDUCTION
	  ? fold_left_reduction_fn (orig_code, &reduc_fn)
	  : reduction_fn_for_scalar_code (orig_code, &reduc_fn))
	{
	  if (reduc_fn != IFN_LAST
	      && !direct_internal_fn_supported_p (reduc_fn, vectype_out,
						  OPTIMIZE_FOR_SPEED))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "reduc op not supported by target.\n");

	      reduc_fn = IFN_LAST;
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
  else if (reduction_type == COND_REDUCTION)
    {
      int scalar_precision
	= GET_MODE_PRECISION (SCALAR_TYPE_MODE (scalar_type));
      cr_index_scalar_type = make_unsigned_type (scalar_precision);
      cr_index_vector_type = build_vector_type (cr_index_scalar_type,
						nunits_out);

      if (direct_internal_fn_supported_p (IFN_REDUC_MAX, cr_index_vector_type,
					  OPTIMIZE_FOR_SPEED))
	reduc_fn = IFN_REDUC_MAX;
    }
  STMT_VINFO_REDUC_FN (reduc_info) = reduc_fn;

  if (reduction_type != EXTRACT_LAST_REDUCTION
      && (!nested_cycle || double_reduc)
      && reduc_fn == IFN_LAST
      && !nunits_out.is_constant ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "missing target support for reduction on"
			 " variable-length vectors.\n");
      return false;
    }

  /* For SLP reductions, see if there is a neutral value we can use.  */
  tree neutral_op = NULL_TREE;
  if (slp_node)
    neutral_op = neutral_op_for_slp_reduction
      (slp_node_instance->reduc_phis, vectype_out, orig_code,
       REDUC_GROUP_FIRST_ELEMENT (stmt_info) != NULL);

  if (double_reduc && reduction_type == FOLD_LEFT_REDUCTION)
    {
      /* We can't support in-order reductions of code such as this:

	   for (int i = 0; i < n1; ++i)
	     for (int j = 0; j < n2; ++j)
	       l += a[j];

	 since GCC effectively transforms the loop when vectorizing:

	   for (int i = 0; i < n1 / VF; ++i)
	     for (int j = 0; j < n2; ++j)
	       for (int k = 0; k < VF; ++k)
		 l += a[j];

	 which is a reassociation of the original operation.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "in-order double reduction not supported.\n");

      return false;
    }

  if (reduction_type == FOLD_LEFT_REDUCTION
      && slp_node
      && !REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    {
      /* We cannot use in-order reductions in this case because there is
	 an implicit reassociation of the operations involved.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "in-order unchained SLP reductions not supported.\n");
      return false;
    }

  /* For double reductions, and for SLP reductions with a neutral value,
     we construct a variable-length initial vector by loading a vector
     full of the neutral value and then shift-and-inserting the start
     values into the low-numbered elements.  */
  if ((double_reduc || neutral_op)
      && !nunits_out.is_constant ()
      && !direct_internal_fn_supported_p (IFN_VEC_SHL_INSERT,
					  vectype_out, OPTIMIZE_FOR_SPEED))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "reduction on variable-length vectors requires"
			 " target support for a vector-shift-and-insert"
			 " operation.\n");
      return false;
    }

  /* Check extra constraints for variable-length unchained SLP reductions.  */
  if (STMT_SLP_TYPE (stmt_info)
      && !REDUC_GROUP_FIRST_ELEMENT (stmt_info)
      && !nunits_out.is_constant ())
    {
      /* We checked above that we could build the initial vector when
	 there's a neutral element value.  Check here for the case in
	 which each SLP statement has its own initial value and in which
	 that value needs to be repeated for every instance of the
	 statement within the initial vector.  */
      unsigned int group_size = SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
      if (!neutral_op
	  && !can_duplicate_and_interleave_p (loop_vinfo, group_size,
					      TREE_TYPE (vectype_out)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported form of SLP reduction for"
			     " variable-length vectors: cannot build"
			     " initial vector.\n");
	  return false;
	}
      /* The epilogue code relies on the number of elements being a multiple
	 of the group size.  The duplicate-and-interleave approach to setting
	 up the initial vector does too.  */
      if (!multiple_p (nunits_out, group_size))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported form of SLP reduction for"
			     " variable-length vectors: the vector size"
			     " is not a multiple of the number of results.\n");
	  return false;
	}
    }

  if (reduction_type == COND_REDUCTION)
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

   This only works when we see both the reduction PHI and its only consumer
   in vectorizable_reduction and there are no intermediate stmts
   participating.  */
  if (ncopies > 1
      && (STMT_VINFO_RELEVANT (stmt_info) <= vect_used_only_live)
      && reduc_chain_length == 1)
    single_defuse_cycle = true;

  if (single_defuse_cycle || lane_reduc_code_p)
    {
      gcc_assert (code != COND_EXPR);

      /* 4. Supportable by target?  */
      bool ok = true;

      /* 4.1. check support for the operation in the loop  */
      optab optab = optab_for_tree_code (code, vectype_in, optab_vector);
      if (!optab)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "no optab.\n");
	  ok = false;
        }

      machine_mode vec_mode = TYPE_MODE (vectype_in);
      if (ok && optab_handler (optab, vec_mode) == CODE_FOR_nothing)
        {
          if (dump_enabled_p ())
            dump_printf (MSG_NOTE, "op not supported by target.\n");
	  if (maybe_ne (GET_MODE_SIZE (vec_mode), UNITS_PER_WORD)
	      || !vect_worthwhile_without_simd_p (loop_vinfo, code))
	    ok = false;
	  else
	    if (dump_enabled_p ())
	      dump_printf (MSG_NOTE, "proceeding using word mode.\n");
        }

      /* Worthwhile without SIMD support?  */
      if (ok
	  && !VECTOR_MODE_P (TYPE_MODE (vectype_in))
	  && !vect_worthwhile_without_simd_p (loop_vinfo, code))
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not worthwhile without SIMD support.\n");
	  ok = false;
        }

      /* lane-reducing operations have to go through vect_transform_reduction.
         For the other cases try without the single cycle optimization.  */
      if (!ok)
	{
	  if (lane_reduc_code_p)
	    return false;
	  else
	    single_defuse_cycle = false;
	}
    }
  STMT_VINFO_FORCE_SINGLE_CYCLE (reduc_info) = single_defuse_cycle;

  /* If the reduction stmt is one of the patterns that have lane
     reduction embedded we cannot handle the case of ! single_defuse_cycle.  */
  if ((ncopies > 1 && ! single_defuse_cycle)
      && lane_reduc_code_p)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "multi def-use cycle not possible for lane-reducing "
			 "reduction operation\n");
      return false;
    }

  if (slp_node)
    vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
  else
    vec_num = 1;

  vect_model_reduction_cost (stmt_info, reduc_fn, reduction_type, ncopies,
			     cost_vec);
  if (dump_enabled_p ()
      && reduction_type == FOLD_LEFT_REDUCTION)
    dump_printf_loc (MSG_NOTE, vect_location,
		     "using an in-order (fold-left) reduction.\n");
  STMT_VINFO_TYPE (orig_stmt_of_analysis) = cycle_phi_info_type;
  /* All but single defuse-cycle optimized, lane-reducing and fold-left
     reductions go through their own vectorizable_* routines.  */
  if (!single_defuse_cycle
      && code != DOT_PROD_EXPR
      && code != WIDEN_SUM_EXPR
      && code != SAD_EXPR
      && reduction_type != FOLD_LEFT_REDUCTION)
    {
      stmt_vec_info tem
	= vect_stmt_to_vectorize (STMT_VINFO_REDUC_DEF (phi_info));
      if (slp_node && REDUC_GROUP_FIRST_ELEMENT (tem))
	{
	  gcc_assert (!REDUC_GROUP_NEXT_ELEMENT (tem));
	  tem = REDUC_GROUP_FIRST_ELEMENT (tem);
	}
      STMT_VINFO_DEF_TYPE (vect_orig_stmt (tem)) = vect_internal_def;
      STMT_VINFO_DEF_TYPE (tem) = vect_internal_def;
    }
  else if (loop_vinfo && LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo))
    {
      vec_loop_masks *masks = &LOOP_VINFO_MASKS (loop_vinfo);
      internal_fn cond_fn = get_conditional_internal_fn (code);

      if (reduction_type != FOLD_LEFT_REDUCTION
	  && !use_mask_by_cond_expr_p (code, cond_fn, vectype_in)
	  && (cond_fn == IFN_LAST
	      || !direct_internal_fn_supported_p (cond_fn, vectype_in,
						  OPTIMIZE_FOR_SPEED)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "can't use a fully-masked loop because no"
			     " conditional operation is available.\n");
	  LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
	}
      else if (reduction_type == FOLD_LEFT_REDUCTION
	       && reduc_fn == IFN_LAST
	       && !expand_vec_cond_expr_p (vectype_in,
					   truth_type_for (vectype_in),
					   SSA_NAME))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "can't use a fully-masked loop because no"
			     " conditional operation is available.\n");
	  LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
	}
      else
	vect_record_loop_mask (loop_vinfo, masks, ncopies * vec_num,
			       vectype_in, NULL);
    }
  return true;
}

/* Transform the definition stmt STMT_INFO of a reduction PHI backedge
   value.  */

bool
vect_transform_reduction (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			  stmt_vec_info *vec_stmt, slp_tree slp_node)
{
  tree vectype_out = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int i;
  int ncopies;
  int j;
  int vec_num;

  stmt_vec_info reduc_info = info_for_reduction (stmt_info);
  gcc_assert (reduc_info->is_reduc_info);

  if (nested_in_vect_loop_p (loop, stmt_info))
    {
      loop = loop->inner;
      gcc_assert (STMT_VINFO_DEF_TYPE (reduc_info) == vect_double_reduction_def);
    }

  gassign *stmt = as_a <gassign *> (stmt_info->stmt);
  enum tree_code code = gimple_assign_rhs_code (stmt);
  int op_type = TREE_CODE_LENGTH (code);

  /* Flatten RHS.  */
  tree ops[3];
  switch (get_gimple_rhs_class (code))
    {
    case GIMPLE_TERNARY_RHS:
      ops[2] = gimple_assign_rhs3 (stmt);
      /* Fall thru.  */
    case GIMPLE_BINARY_RHS:
      ops[0] = gimple_assign_rhs1 (stmt);
      ops[1] = gimple_assign_rhs2 (stmt);
      break;
    default:
      gcc_unreachable ();
    }

  /* All uses but the last are expected to be defined in the loop.
     The last use is the reduction variable.  In case of nested cycle this
     assumption is not true: we use reduc_index to record the index of the
     reduction variable.  */
  stmt_vec_info phi_info = STMT_VINFO_REDUC_DEF (vect_orig_stmt (stmt_info));
  gphi *reduc_def_phi = as_a <gphi *> (phi_info->stmt);
  int reduc_index = STMT_VINFO_REDUC_IDX (stmt_info);
  tree vectype_in = STMT_VINFO_REDUC_VECTYPE_IN (reduc_info);

  if (slp_node)
    {
      ncopies = 1;
      vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
    }
  else
    {
      ncopies = vect_get_num_copies (loop_vinfo, vectype_in);
      vec_num = 1;
    }

  internal_fn cond_fn = get_conditional_internal_fn (code);
  vec_loop_masks *masks = &LOOP_VINFO_MASKS (loop_vinfo);
  bool mask_by_cond_expr = use_mask_by_cond_expr_p (code, cond_fn, vectype_in);

  /* Transform.  */
  stmt_vec_info new_stmt_info = NULL;
  stmt_vec_info prev_stmt_info;
  tree new_temp = NULL_TREE;
  auto_vec<tree> vec_oprnds0;
  auto_vec<tree> vec_oprnds1;
  auto_vec<tree> vec_oprnds2;
  tree def0;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform reduction.\n");

  /* FORNOW: Multiple types are not supported for condition.  */
  if (code == COND_EXPR)
    gcc_assert (ncopies == 1);

  bool masked_loop_p = LOOP_VINFO_FULLY_MASKED_P (loop_vinfo);

  vect_reduction_type reduction_type = STMT_VINFO_REDUC_TYPE (reduc_info);
  if (reduction_type == FOLD_LEFT_REDUCTION)
    {
      internal_fn reduc_fn = STMT_VINFO_REDUC_FN (reduc_info);
      return vectorize_fold_left_reduction
	  (stmt_info, gsi, vec_stmt, slp_node, reduc_def_phi, code,
	   reduc_fn, ops, vectype_in, reduc_index, masks);
    }

  bool single_defuse_cycle = STMT_VINFO_FORCE_SINGLE_CYCLE (reduc_info);
  gcc_assert (single_defuse_cycle
	      || code == DOT_PROD_EXPR
	      || code == WIDEN_SUM_EXPR
	      || code == SAD_EXPR);

  /* Create the destination vector  */
  tree scalar_dest = gimple_assign_lhs (stmt);
  tree vec_dest = vect_create_destination_var (scalar_dest, vectype_out);

  prev_stmt_info = NULL;
  if (!slp_node)
    {
      vec_oprnds0.create (1);
      vec_oprnds1.create (1);
      if (op_type == ternary_op)
        vec_oprnds2.create (1);
    }

  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
        {
	  if (slp_node)
	    {
	      /* Get vec defs for all the operands except the reduction index,
		 ensuring the ordering of the ops in the vector is kept.  */
	      auto_vec<vec<tree>, 3> vec_defs;
	      vect_get_slp_defs (slp_node, &vec_defs);
	      vec_oprnds0.safe_splice (vec_defs[0]);
	      vec_defs[0].release ();
	      vec_oprnds1.safe_splice (vec_defs[1]);
	      vec_defs[1].release ();
	      if (op_type == ternary_op)
		{
		  vec_oprnds2.safe_splice (vec_defs[2]);
		  vec_defs[2].release ();
		}
	    }
          else
	    {
              vec_oprnds0.quick_push
		(vect_get_vec_def_for_operand (ops[0], stmt_info));
              vec_oprnds1.quick_push
		(vect_get_vec_def_for_operand (ops[1], stmt_info));
              if (op_type == ternary_op)
		vec_oprnds2.quick_push 
		  (vect_get_vec_def_for_operand (ops[2], stmt_info));
	    }
        }
      else
        {
          if (!slp_node)
            {
	      gcc_assert (reduc_index != -1 || ! single_defuse_cycle);

	      if (single_defuse_cycle && reduc_index == 0)
		vec_oprnds0[0] = gimple_get_lhs (new_stmt_info->stmt);
	      else
		vec_oprnds0[0]
		  = vect_get_vec_def_for_stmt_copy (loop_vinfo,
						    vec_oprnds0[0]);
	      if (single_defuse_cycle && reduc_index == 1)
		vec_oprnds1[0] = gimple_get_lhs (new_stmt_info->stmt);
	      else
		vec_oprnds1[0]
		  = vect_get_vec_def_for_stmt_copy (loop_vinfo,
						    vec_oprnds1[0]);
	      if (op_type == ternary_op)
		{
		  if (single_defuse_cycle && reduc_index == 2)
		    vec_oprnds2[0] = gimple_get_lhs (new_stmt_info->stmt);
		  else
		    vec_oprnds2[0] 
		      = vect_get_vec_def_for_stmt_copy (loop_vinfo,
							vec_oprnds2[0]);
		}
            }
        }

      FOR_EACH_VEC_ELT (vec_oprnds0, i, def0)
        {
	  tree vop[3] = { def0, vec_oprnds1[i], NULL_TREE };
	  if (masked_loop_p && !mask_by_cond_expr)
	    {
	      /* Make sure that the reduction accumulator is vop[0].  */
	      if (reduc_index == 1)
		{
		  gcc_assert (commutative_tree_code (code));
		  std::swap (vop[0], vop[1]);
		}
	      tree mask = vect_get_loop_mask (gsi, masks, vec_num * ncopies,
					      vectype_in, i * ncopies + j);
	      gcall *call = gimple_build_call_internal (cond_fn, 4, mask,
							vop[0], vop[1],
							vop[0]);
	      new_temp = make_ssa_name (vec_dest, call);
	      gimple_call_set_lhs (call, new_temp);
	      gimple_call_set_nothrow (call, true);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, call, gsi);
	    }
	  else
	    {
	      if (op_type == ternary_op)
		vop[2] = vec_oprnds2[i];

	      if (masked_loop_p && mask_by_cond_expr)
		{
		  tree mask = vect_get_loop_mask (gsi, masks,
						  vec_num * ncopies,
						  vectype_in, i * ncopies + j);
		  build_vect_cond_expr (code, vop, mask, gsi);
		}

	      gassign *new_stmt = gimple_build_assign (vec_dest, code,
						       vop[0], vop[1], vop[2]);
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      gimple_assign_set_lhs (new_stmt, new_temp);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	    }

          if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
        }

      if (slp_node || single_defuse_cycle)
        continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

      prev_stmt_info = new_stmt_info;
    }

  if (single_defuse_cycle && !slp_node)
    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;

  return true;
}

/* Transform phase of a cycle PHI.  */

bool
vect_transform_cycle_phi (stmt_vec_info stmt_info, stmt_vec_info *vec_stmt,
			  slp_tree slp_node, slp_instance slp_node_instance)
{
  tree vectype_out = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int i;
  int ncopies;
  stmt_vec_info prev_phi_info;
  int j;
  bool nested_cycle = false;
  int vec_num;

  if (nested_in_vect_loop_p (loop, stmt_info))
    {
      loop = loop->inner;
      nested_cycle = true;
    }

  stmt_vec_info reduc_stmt_info = STMT_VINFO_REDUC_DEF (stmt_info);
  reduc_stmt_info = vect_stmt_to_vectorize (reduc_stmt_info);
  stmt_vec_info reduc_info = info_for_reduction (stmt_info);
  gcc_assert (reduc_info->is_reduc_info);

  if (STMT_VINFO_REDUC_TYPE (reduc_info) == EXTRACT_LAST_REDUCTION
      || STMT_VINFO_REDUC_TYPE (reduc_info) == FOLD_LEFT_REDUCTION)
    /* Leave the scalar phi in place.  */
    return true;

  tree vectype_in = STMT_VINFO_REDUC_VECTYPE_IN (reduc_info);
  /* For a nested cycle we do not fill the above.  */
  if (!vectype_in)
    vectype_in = STMT_VINFO_VECTYPE (stmt_info);
  gcc_assert (vectype_in);

  if (slp_node)
    {
      /* The size vect_schedule_slp_instance computes is off for us.  */
      vec_num = vect_get_num_vectors
	  (LOOP_VINFO_VECT_FACTOR (loop_vinfo)
	   * SLP_TREE_SCALAR_STMTS (slp_node).length (), vectype_in);
      ncopies = 1;
    }
  else
    {
      vec_num = 1;
      ncopies = vect_get_num_copies (loop_vinfo, vectype_in);
    }

  /* Check whether we should use a single PHI node and accumulate
     vectors to one before the backedge.  */
  if (STMT_VINFO_FORCE_SINGLE_CYCLE (reduc_info))
    ncopies = 1;

  /* Create the destination vector  */
  gphi *phi = as_a <gphi *> (stmt_info->stmt);
  tree vec_dest = vect_create_destination_var (gimple_phi_result (phi),
					       vectype_out);

  /* Get the loop-entry arguments.  */
  tree vec_initial_def;
  auto_vec<tree> vec_initial_defs;
  if (slp_node)
    {
      vec_initial_defs.reserve (vec_num);
      gcc_assert (slp_node == slp_node_instance->reduc_phis);
      stmt_vec_info first = REDUC_GROUP_FIRST_ELEMENT (reduc_stmt_info);
      tree neutral_op
	= neutral_op_for_slp_reduction (slp_node, vectype_out,
					STMT_VINFO_REDUC_CODE (reduc_info),
					first != NULL);
      get_initial_defs_for_reduction (slp_node_instance->reduc_phis,
				      &vec_initial_defs, vec_num,
				      first != NULL, neutral_op);
    }
  else
    {
      /* Get at the scalar def before the loop, that defines the initial
	 value of the reduction variable.  */
      tree initial_def = PHI_ARG_DEF_FROM_EDGE (phi,
						loop_preheader_edge (loop));
      /* Optimize: if initial_def is for REDUC_MAX smaller than the base
	 and we can't use zero for induc_val, use initial_def.  Similarly
	 for REDUC_MIN and initial_def larger than the base.  */
      if (STMT_VINFO_REDUC_TYPE (reduc_info) == INTEGER_INDUC_COND_REDUCTION)
	{
	  tree induc_val = STMT_VINFO_VEC_INDUC_COND_INITIAL_VAL (reduc_info);
	  if (TREE_CODE (initial_def) == INTEGER_CST
	      && !integer_zerop (induc_val)
	      && ((STMT_VINFO_REDUC_CODE (reduc_info) == MAX_EXPR
		   && tree_int_cst_lt (initial_def, induc_val))
		  || (STMT_VINFO_REDUC_CODE (reduc_info) == MIN_EXPR
		      && tree_int_cst_lt (induc_val, initial_def))))
	    {
	      induc_val = initial_def;
	      /* Communicate we used the initial_def to epilouge
		 generation.  */
	      STMT_VINFO_VEC_INDUC_COND_INITIAL_VAL (reduc_info) = NULL_TREE;
	    }
	  vec_initial_def = build_vector_from_val (vectype_out, induc_val);
	}
      else if (nested_cycle)
	{
	  /* Do not use an adjustment def as that case is not supported
	     correctly if ncopies is not one.  */
	  vec_initial_def = vect_get_vec_def_for_operand (initial_def,
							  reduc_stmt_info);
	}
      else
	{
	  tree adjustment_def = NULL_TREE;
	  tree *adjustment_defp = &adjustment_def;
	  enum tree_code code = STMT_VINFO_REDUC_CODE (reduc_info);
	  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
	    adjustment_defp = NULL;
	  vec_initial_def
	    = get_initial_def_for_reduction (reduc_stmt_info, code,
					     initial_def, adjustment_defp);
	  STMT_VINFO_REDUC_EPILOGUE_ADJUSTMENT (reduc_info) = adjustment_def;
	}
      vec_initial_defs.create (1);
      vec_initial_defs.quick_push (vec_initial_def);
    }

  /* Generate the reduction PHIs upfront.  */
  prev_phi_info = NULL;
  for (i = 0; i < vec_num; i++)
    {
      tree vec_init_def = vec_initial_defs[i];
      for (j = 0; j < ncopies; j++)
	{
	  /* Create the reduction-phi that defines the reduction
	     operand.  */
	  gphi *new_phi = create_phi_node (vec_dest, loop->header);
	  stmt_vec_info new_phi_info = loop_vinfo->add_stmt (new_phi);

	  /* Set the loop-entry arg of the reduction-phi.  */
	  if (j != 0 && nested_cycle)
	    vec_init_def = vect_get_vec_def_for_stmt_copy (loop_vinfo,
							   vec_init_def);
	  add_phi_arg (new_phi, vec_init_def, loop_preheader_edge (loop),
		       UNKNOWN_LOCATION);

	  /* The loop-latch arg is set in epilogue processing.  */

	  if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_phi_info);
	  else
	    {
	      if (j == 0)
		STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_phi_info;
	      else
		STMT_VINFO_RELATED_STMT (prev_phi_info) = new_phi_info;
	      prev_phi_info = new_phi_info;
	    }
	}
    }

  return true;
}

/* Vectorizes LC PHIs.  */

bool
vectorizable_lc_phi (stmt_vec_info stmt_info, stmt_vec_info *vec_stmt,
		     slp_tree slp_node)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  if (!loop_vinfo
      || !is_a <gphi *> (stmt_info->stmt)
      || gimple_phi_num_args (stmt_info->stmt) != 1)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && STMT_VINFO_DEF_TYPE (stmt_info) != vect_double_reduction_def)
    return false;

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = lc_phi_info_type;
      return true;
    }

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree scalar_dest = gimple_phi_result (stmt_info->stmt);
  basic_block bb = gimple_bb (stmt_info->stmt);
  edge e = single_pred_edge (bb);
  tree vec_dest = vect_create_destination_var (scalar_dest, vectype);
  vec<tree> vec_oprnds = vNULL;
  vect_get_vec_defs (gimple_phi_arg_def (stmt_info->stmt, 0), NULL_TREE,
		     stmt_info, &vec_oprnds, NULL, slp_node);
  if (slp_node)
    {
      unsigned vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
      gcc_assert (vec_oprnds.length () == vec_num);
      for (unsigned i = 0; i < vec_num; i++)
	{
	  /* Create the vectorized LC PHI node.  */
	  gphi *new_phi = create_phi_node (vec_dest, bb);
	  add_phi_arg (new_phi, vec_oprnds[i], e, UNKNOWN_LOCATION);
	  stmt_vec_info new_phi_info = loop_vinfo->add_stmt (new_phi);
	  SLP_TREE_VEC_STMTS (slp_node).quick_push (new_phi_info);
	}
    }
  else
    {
      unsigned ncopies = vect_get_num_copies (loop_vinfo, vectype);
      stmt_vec_info prev_phi_info = NULL;
      for (unsigned i = 0; i < ncopies; i++)
	{
	  if (i != 0)
	    vect_get_vec_defs_for_stmt_copy (loop_vinfo, &vec_oprnds, NULL);
	  /* Create the vectorized LC PHI node.  */
	  gphi *new_phi = create_phi_node (vec_dest, bb);
	  add_phi_arg (new_phi, vec_oprnds[0], e, UNKNOWN_LOCATION);
	  stmt_vec_info new_phi_info = loop_vinfo->add_stmt (new_phi);
	  if (i == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_phi_info;
	  else
	    STMT_VINFO_RELATED_STMT (prev_phi_info) = new_phi_info;
	  prev_phi_info = new_phi_info;
	}
    }
  vec_oprnds.release ();

  return true;
}


/* Function vect_min_worthwhile_factor.

   For a loop where we could vectorize the operation indicated by CODE,
   return the minimum vectorization factor that makes it worthwhile
   to use generic vectors.  */
static unsigned int
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

/* Return true if VINFO indicates we are doing loop vectorization and if
   it is worth decomposing CODE operations into scalar operations for
   that loop's vectorization factor.  */

bool
vect_worthwhile_without_simd_p (vec_info *vinfo, tree_code code)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  unsigned HOST_WIDE_INT value;
  return (loop_vinfo
	  && LOOP_VINFO_VECT_FACTOR (loop_vinfo).is_constant (&value)
	  && value >= vect_min_worthwhile_factor (code));
}

/* Function vectorizable_induction

   Check if STMT_INFO performs an induction computation that can be vectorized.
   If VEC_STMT is also passed, vectorize the induction PHI: create a vectorized
   phi to replace it, put it in VEC_STMT, and add it to the same basic block.
   Return true if STMT_INFO is vectorizable in this way.  */

bool
vectorizable_induction (stmt_vec_info stmt_info,
			gimple_stmt_iterator *gsi ATTRIBUTE_UNUSED,
			stmt_vec_info *vec_stmt, slp_tree slp_node,
			stmt_vector_for_cost *cost_vec)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  unsigned ncopies;
  bool nested_in_vect_loop = false;
  class loop *iv_loop;
  tree vec_def;
  edge pe = loop_preheader_edge (loop);
  basic_block new_bb;
  tree new_vec, vec_init, vec_step, t;
  tree new_name;
  gimple *new_stmt;
  gphi *induction_phi;
  tree induc_def, vec_dest;
  tree init_expr, step_expr;
  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  unsigned i;
  tree expr;
  gimple_seq stmts;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  gimple *exit_phi;
  edge latch_e;
  tree loop_arg;
  gimple_stmt_iterator si;

  gphi *phi = dyn_cast <gphi *> (stmt_info->stmt);
  if (!phi)
    return false;

  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  /* Make sure it was recognized as induction computation.  */
  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_induction_def)
    return false;

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);
  gcc_assert (ncopies >= 1);

  /* FORNOW. These restrictions should be relaxed.  */
  if (nested_in_vect_loop_p (loop, stmt_info))
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

      /* FORNOW: outer loop induction with SLP not supported.  */
      if (STMT_SLP_TYPE (stmt_info))
	return false;

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
	  stmt_vec_info exit_phi_vinfo = loop_vinfo->lookup_stmt (exit_phi);
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

      nested_in_vect_loop = true;
      iv_loop = loop->inner;
    }
  else
    iv_loop = loop;
  gcc_assert (iv_loop == (gimple_bb (phi))->loop_father);

  if (slp_node && !nunits.is_constant ())
    {
      /* The current SLP code creates the initial value element-by-element.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "SLP induction not supported for variable-length"
			 " vectors.\n");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = induc_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_induction");
      vect_model_induction_cost (stmt_info, ncopies, cost_vec);
      return true;
    }

  /* Transform.  */

  /* Compute a vector variable, initialized with the first VF values of
     the induction variable.  E.g., for an iv with IV_PHI='X' and
     evolution S, for a vector of 4 units, we want to compute:
     [X, X + S, X + 2*S, X + 3*S].  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform induction phi.\n");

  latch_e = loop_latch_edge (iv_loop);
  loop_arg = PHI_ARG_DEF_FROM_EDGE (phi, latch_e);

  step_expr = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (stmt_info);
  gcc_assert (step_expr != NULL_TREE);
  tree step_vectype = get_same_sized_vectype (TREE_TYPE (step_expr), vectype);

  pe = loop_preheader_edge (iv_loop);
  init_expr = PHI_ARG_DEF_FROM_EDGE (phi,
				     loop_preheader_edge (iv_loop));

  stmts = NULL;
  if (!nested_in_vect_loop)
    {
      /* Convert the initial value to the IV update type.  */
      tree new_type = TREE_TYPE (step_expr);
      init_expr = gimple_convert (&stmts, new_type, init_expr);

      /* If we are using the loop mask to "peel" for alignment then we need
	 to adjust the start value here.  */
      tree skip_niters = LOOP_VINFO_MASK_SKIP_NITERS (loop_vinfo);
      if (skip_niters != NULL_TREE)
	{
	  if (FLOAT_TYPE_P (vectype))
	    skip_niters = gimple_build (&stmts, FLOAT_EXPR, new_type,
					skip_niters);
	  else
	    skip_niters = gimple_convert (&stmts, new_type, skip_niters);
	  tree skip_step = gimple_build (&stmts, MULT_EXPR, new_type,
					 skip_niters, step_expr);
	  init_expr = gimple_build (&stmts, MINUS_EXPR, new_type,
				    init_expr, skip_step);
	}
    }

  if (stmts)
    {
      new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
      gcc_assert (!new_bb);
    }

  /* Find the first insertion point in the BB.  */
  basic_block bb = gimple_bb (phi);
  si = gsi_after_labels (bb);

  /* For SLP induction we have to generate several IVs as for example
     with group size 3 we need [i, i, i, i + S] [i + S, i + S, i + 2*S, i + 2*S]
     [i + 2*S, i + 3*S, i + 3*S, i + 3*S].  The step is the same uniform
     [VF*S, VF*S, VF*S, VF*S] for all.  */
  if (slp_node)
    {
      /* Enforced above.  */
      unsigned int const_nunits = nunits.to_constant ();

      /* Generate [VF*S, VF*S, ... ].  */
      if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr)))
	{
	  expr = build_int_cst (integer_type_node, vf);
	  expr = fold_convert (TREE_TYPE (step_expr), expr);
	}
      else
	expr = build_int_cst (TREE_TYPE (step_expr), vf);
      new_name = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
			      expr, step_expr);
      if (! CONSTANT_CLASS_P (new_name))
	new_name = vect_init_vector (stmt_info, new_name,
				     TREE_TYPE (step_expr), NULL);
      new_vec = build_vector_from_val (step_vectype, new_name);
      vec_step = vect_init_vector (stmt_info, new_vec, step_vectype, NULL);

      /* Now generate the IVs.  */
      unsigned group_size = SLP_TREE_SCALAR_STMTS (slp_node).length ();
      unsigned nvects = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
      unsigned elts = const_nunits * nvects;
      unsigned nivs = least_common_multiple (group_size,
					     const_nunits) / const_nunits;
      gcc_assert (elts % group_size == 0);
      tree elt = init_expr;
      unsigned ivn;
      for (ivn = 0; ivn < nivs; ++ivn)
	{
	  tree_vector_builder elts (step_vectype, const_nunits, 1);
	  stmts = NULL;
	  for (unsigned eltn = 0; eltn < const_nunits; ++eltn)
	    {
	      if (ivn*const_nunits + eltn >= group_size
		  && (ivn * const_nunits + eltn) % group_size == 0)
		elt = gimple_build (&stmts, PLUS_EXPR, TREE_TYPE (elt),
				    elt, step_expr);
	      elts.quick_push (elt);
	    }
	  vec_init = gimple_build_vector (&stmts, &elts);
	  vec_init = gimple_convert (&stmts, vectype, vec_init);
	  if (stmts)
	    {
	      new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
	      gcc_assert (!new_bb);
	    }

	  /* Create the induction-phi that defines the induction-operand.  */
	  vec_dest = vect_get_new_vect_var (vectype, vect_simple_var, "vec_iv_");
	  induction_phi = create_phi_node (vec_dest, iv_loop->header);
	  stmt_vec_info induction_phi_info
	    = loop_vinfo->add_stmt (induction_phi);
	  induc_def = PHI_RESULT (induction_phi);

	  /* Create the iv update inside the loop  */
	  gimple_seq stmts = NULL;
	  vec_def = gimple_convert (&stmts, step_vectype, induc_def);
	  vec_def = gimple_build (&stmts,
				  PLUS_EXPR, step_vectype, vec_def, vec_step);
	  vec_def = gimple_convert (&stmts, vectype, vec_def);
	  loop_vinfo->add_stmt (SSA_NAME_DEF_STMT (vec_def));
	  gsi_insert_seq_before (&si, stmts, GSI_SAME_STMT);

	  /* Set the arguments of the phi node:  */
	  add_phi_arg (induction_phi, vec_init, pe, UNKNOWN_LOCATION);
	  add_phi_arg (induction_phi, vec_def, loop_latch_edge (iv_loop),
		       UNKNOWN_LOCATION);

	  SLP_TREE_VEC_STMTS (slp_node).quick_push (induction_phi_info);
	}

      /* Re-use IVs when we can.  */
      if (ivn < nvects)
	{
	  unsigned vfp
	    = least_common_multiple (group_size, const_nunits) / group_size;
	  /* Generate [VF'*S, VF'*S, ... ].  */
	  if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr)))
	    {
	      expr = build_int_cst (integer_type_node, vfp);
	      expr = fold_convert (TREE_TYPE (step_expr), expr);
	    }
	  else
	    expr = build_int_cst (TREE_TYPE (step_expr), vfp);
	  new_name = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
				  expr, step_expr);
	  if (! CONSTANT_CLASS_P (new_name))
	    new_name = vect_init_vector (stmt_info, new_name,
					 TREE_TYPE (step_expr), NULL);
	  new_vec = build_vector_from_val (step_vectype, new_name);
	  vec_step = vect_init_vector (stmt_info, new_vec, step_vectype, NULL);
	  for (; ivn < nvects; ++ivn)
	    {
	      gimple *iv = SLP_TREE_VEC_STMTS (slp_node)[ivn - nivs]->stmt;
	      tree def;
	      if (gimple_code (iv) == GIMPLE_PHI)
		def = gimple_phi_result (iv);
	      else
		def = gimple_assign_lhs (iv);
	      gimple_seq stmts = NULL;
	      def = gimple_convert (&stmts, step_vectype, def);
	      def = gimple_build (&stmts,
				  PLUS_EXPR, step_vectype, def, vec_step);
	      def = gimple_convert (&stmts, vectype, def);
	      if (gimple_code (iv) == GIMPLE_PHI)
		gsi_insert_seq_before (&si, stmts, GSI_SAME_STMT);
	      else
		{
		  gimple_stmt_iterator tgsi = gsi_for_stmt (iv);
		  gsi_insert_seq_after (&tgsi, stmts, GSI_CONTINUE_LINKING);
		}
	      SLP_TREE_VEC_STMTS (slp_node).quick_push
		(loop_vinfo->add_stmt (SSA_NAME_DEF_STMT (def)));
	    }
	}

      return true;
    }

  /* Create the vector that holds the initial_value of the induction.  */
  if (nested_in_vect_loop)
    {
      /* iv_loop is nested in the loop to be vectorized.  init_expr had already
	 been created during vectorization of previous stmts.  We obtain it
	 from the STMT_VINFO_VEC_STMT of the defining stmt.  */
      vec_init = vect_get_vec_def_for_operand (init_expr, stmt_info);
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
	  loop_vinfo->add_stmt (new_stmt);
	}
    }
  else
    {
      /* iv_loop is the loop to be vectorized. Create:
	 vec_init = [X, X+S, X+2*S, X+3*S] (S = step_expr, X = init_expr)  */
      stmts = NULL;
      new_name = gimple_convert (&stmts, TREE_TYPE (step_expr), init_expr);

      unsigned HOST_WIDE_INT const_nunits;
      if (nunits.is_constant (&const_nunits))
	{
	  tree_vector_builder elts (step_vectype, const_nunits, 1);
	  elts.quick_push (new_name);
	  for (i = 1; i < const_nunits; i++)
	    {
	      /* Create: new_name_i = new_name + step_expr  */
	      new_name = gimple_build (&stmts, PLUS_EXPR, TREE_TYPE (new_name),
				       new_name, step_expr);
	      elts.quick_push (new_name);
	    }
	  /* Create a vector from [new_name_0, new_name_1, ...,
	     new_name_nunits-1]  */
	  vec_init = gimple_build_vector (&stmts, &elts);
	}
      else if (INTEGRAL_TYPE_P (TREE_TYPE (step_expr)))
	/* Build the initial value directly from a VEC_SERIES_EXPR.  */
	vec_init = gimple_build (&stmts, VEC_SERIES_EXPR, step_vectype,
				 new_name, step_expr);
      else
	{
	  /* Build:
	        [base, base, base, ...]
		+ (vectype) [0, 1, 2, ...] * [step, step, step, ...].  */
	  gcc_assert (SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr)));
	  gcc_assert (flag_associative_math);
	  tree index = build_index_vector (step_vectype, 0, 1);
	  tree base_vec = gimple_build_vector_from_val (&stmts, step_vectype,
							new_name);
	  tree step_vec = gimple_build_vector_from_val (&stmts, step_vectype,
							step_expr);
	  vec_init = gimple_build (&stmts, FLOAT_EXPR, step_vectype, index);
	  vec_init = gimple_build (&stmts, MULT_EXPR, step_vectype,
				   vec_init, step_vec);
	  vec_init = gimple_build (&stmts, PLUS_EXPR, step_vectype,
				   vec_init, base_vec);
	}
      vec_init = gimple_convert (&stmts, vectype, vec_init);

      if (stmts)
	{
	  new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
	  gcc_assert (!new_bb);
	}
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
      gimple_seq seq = NULL;
      if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr)))
	{
	  expr = build_int_cst (integer_type_node, vf);
	  expr = gimple_build (&seq, FLOAT_EXPR, TREE_TYPE (step_expr), expr);
	}
      else
	expr = build_int_cst (TREE_TYPE (step_expr), vf);
      new_name = gimple_build (&seq, MULT_EXPR, TREE_TYPE (step_expr),
			       expr, step_expr);
      if (seq)
	{
	  new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
	  gcc_assert (!new_bb);
	}
    }

  t = unshare_expr (new_name);
  gcc_assert (CONSTANT_CLASS_P (new_name)
	      || TREE_CODE (new_name) == SSA_NAME);
  new_vec = build_vector_from_val (step_vectype, t);
  vec_step = vect_init_vector (stmt_info, new_vec, step_vectype, NULL);


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
  stmt_vec_info induction_phi_info = loop_vinfo->add_stmt (induction_phi);
  induc_def = PHI_RESULT (induction_phi);

  /* Create the iv update inside the loop  */
  stmts = NULL;
  vec_def = gimple_convert (&stmts, step_vectype, induc_def);
  vec_def = gimple_build (&stmts, PLUS_EXPR, step_vectype, vec_def, vec_step);
  vec_def = gimple_convert (&stmts, vectype, vec_def);
  gsi_insert_seq_before (&si, stmts, GSI_SAME_STMT);
  new_stmt = SSA_NAME_DEF_STMT (vec_def);
  stmt_vec_info new_stmt_info = loop_vinfo->add_stmt (new_stmt);

  /* Set the arguments of the phi node:  */
  add_phi_arg (induction_phi, vec_init, pe, UNKNOWN_LOCATION);
  add_phi_arg (induction_phi, vec_def, loop_latch_edge (iv_loop),
	       UNKNOWN_LOCATION);

  STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = induction_phi_info;

  /* In case that vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  For more details see documentation
     in vectorizable_operation.  */

  if (ncopies > 1)
    {
      gimple_seq seq = NULL;
      stmt_vec_info prev_stmt_vinfo;
      /* FORNOW. This restriction should be relaxed.  */
      gcc_assert (!nested_in_vect_loop);

      /* Create the vector that holds the step of the induction.  */
      if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (step_expr)))
	{
	  expr = build_int_cst (integer_type_node, nunits);
	  expr = gimple_build (&seq, FLOAT_EXPR, TREE_TYPE (step_expr), expr);
	}
      else
	expr = build_int_cst (TREE_TYPE (step_expr), nunits);
      new_name = gimple_build (&seq, MULT_EXPR, TREE_TYPE (step_expr),
			       expr, step_expr);
      if (seq)
	{
	  new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
	  gcc_assert (!new_bb);
	}

      t = unshare_expr (new_name);
      gcc_assert (CONSTANT_CLASS_P (new_name)
		  || TREE_CODE (new_name) == SSA_NAME);
      new_vec = build_vector_from_val (step_vectype, t);
      vec_step = vect_init_vector (stmt_info, new_vec, step_vectype, NULL);

      vec_def = induc_def;
      prev_stmt_vinfo = induction_phi_info;
      for (i = 1; i < ncopies; i++)
	{
	  /* vec_i = vec_prev + vec_step  */
	  gimple_seq stmts = NULL;
	  vec_def = gimple_convert (&stmts, step_vectype, vec_def);
	  vec_def = gimple_build (&stmts,
				  PLUS_EXPR, step_vectype, vec_def, vec_step);
	  vec_def = gimple_convert (&stmts, vectype, vec_def);
 
	  gsi_insert_seq_before (&si, stmts, GSI_SAME_STMT);
	  new_stmt = SSA_NAME_DEF_STMT (vec_def);
	  new_stmt_info = loop_vinfo->add_stmt (new_stmt);
	  STMT_VINFO_RELATED_STMT (prev_stmt_vinfo) = new_stmt_info;
	  prev_stmt_vinfo = new_stmt_info;
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
	  stmt_vec_info stmt_vinfo = loop_vinfo->lookup_stmt (exit_phi);
	  /* FORNOW. Currently not supporting the case that an inner-loop induction
	     is not used in the outer-loop (i.e. only outside the outer-loop).  */
	  gcc_assert (STMT_VINFO_RELEVANT_P (stmt_vinfo)
		      && !STMT_VINFO_LIVE_P (stmt_vinfo));

	  STMT_VINFO_VEC_STMT (stmt_vinfo) = new_stmt_info;
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vector of inductions after inner-loop:%G",
			     new_stmt);
	}
    }


  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "transform induction: created def-use cycle: %G%G",
		     induction_phi, SSA_NAME_DEF_STMT (vec_def));

  return true;
}

/* Function vectorizable_live_operation.

   STMT_INFO computes a value that is used outside the loop.  Check if
   it can be supported.  */

bool
vectorizable_live_operation (stmt_vec_info stmt_info,
			     gimple_stmt_iterator *gsi,
			     slp_tree slp_node, slp_instance slp_node_instance,
			     int slp_index, bool vec_stmt_p,
			     stmt_vector_for_cost *)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  imm_use_iterator imm_iter;
  tree lhs, lhs_type, bitsize, vec_bitsize;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies;
  gimple *use_stmt;
  auto_vec<tree> vec_oprnds;
  int vec_entry = 0;
  poly_uint64 vec_index = 0;

  gcc_assert (STMT_VINFO_LIVE_P (stmt_info));

  /* If a stmt of a reduction is live, vectorize it via
     vect_create_epilog_for_reduction.  vectorizable_reduction assessed
     validity so just trigger the transform here.  */
  if (STMT_VINFO_REDUC_DEF (vect_orig_stmt (stmt_info)))
    {
      if (!vec_stmt_p)
	return true;
      if (slp_node)
	{
	  /* For reduction chains the meta-info is attached to
	     the group leader.  */
	  if (REDUC_GROUP_FIRST_ELEMENT (stmt_info))
	    stmt_info = REDUC_GROUP_FIRST_ELEMENT (stmt_info);
	  /* For SLP reductions we vectorize the epilogue for
	     all involved stmts together.  */
	  else if (slp_index != 0)
	    return true;
	}
      stmt_vec_info reduc_info = info_for_reduction (stmt_info);
      gcc_assert (reduc_info->is_reduc_info);
      if (STMT_VINFO_REDUC_TYPE (reduc_info) == FOLD_LEFT_REDUCTION
	  || STMT_VINFO_REDUC_TYPE (reduc_info) == EXTRACT_LAST_REDUCTION)
	return true;
      vect_create_epilog_for_reduction (stmt_info, slp_node,
					slp_node_instance);
      return true;
    }

  /* FORNOW.  CHECKME.  */
  if (nested_in_vect_loop_p (loop, stmt_info))
    return false;

  /* If STMT is not relevant and it is a simple assignment and its inputs are
     invariant then it can remain in place, unvectorized.  The original last
     scalar value that it computes will be used.  */
  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    {
      gcc_assert (is_simple_and_all_uses_invariant (stmt_info, loop_vinfo));
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "statement is simple and uses invariant.  Leaving in "
			 "place.\n");
      return true;
    }

  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  if (slp_node)
    {
      gcc_assert (slp_index >= 0);

      int num_scalar = SLP_TREE_SCALAR_STMTS (slp_node).length ();
      int num_vec = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);

      /* Get the last occurrence of the scalar index from the concatenation of
	 all the slp vectors. Calculate which slp vector it is and the index
	 within.  */
      poly_uint64 pos = (num_vec * nunits) - num_scalar + slp_index;

      /* Calculate which vector contains the result, and which lane of
	 that vector we need.  */
      if (!can_div_trunc_p (pos, nunits, &vec_entry, &vec_index))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Cannot determine which vector holds the"
			     " final result.\n");
	  return false;
	}
    }

  if (!vec_stmt_p)
    {
      /* No transformation required.  */
      if (LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo))
	{
	  if (!direct_internal_fn_supported_p (IFN_EXTRACT_LAST, vectype,
					       OPTIMIZE_FOR_SPEED))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "can't use a fully-masked loop because "
				 "the target doesn't support extract last "
				 "reduction.\n");
	      LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
	    }
	  else if (slp_node)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "can't use a fully-masked loop because an "
				 "SLP statement is live after the loop.\n");
	      LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
	    }
	  else if (ncopies > 1)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "can't use a fully-masked loop because"
				 " ncopies is greater than 1.\n");
	      LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
	    }
	  else
	    {
	      gcc_assert (ncopies == 1 && !slp_node);
	      vect_record_loop_mask (loop_vinfo,
				     &LOOP_VINFO_MASKS (loop_vinfo),
				     1, vectype, NULL);
	    }
	}
      return true;
    }

  /* Use the lhs of the original scalar statement.  */
  gimple *stmt = vect_orig_stmt (stmt_info)->stmt;

  lhs = (is_a <gphi *> (stmt)) ? gimple_phi_result (stmt)
	: gimple_get_lhs (stmt);
  lhs_type = TREE_TYPE (lhs);

  bitsize = (VECTOR_BOOLEAN_TYPE_P (vectype)
	     ? bitsize_int (TYPE_PRECISION (TREE_TYPE (vectype)))
	     : TYPE_SIZE (TREE_TYPE (vectype)));
  vec_bitsize = TYPE_SIZE (vectype);

  /* Get the vectorized lhs of STMT and the lane to use (counted in bits).  */
  tree vec_lhs, bitstart;
  if (slp_node)
    {
      gcc_assert (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo));

      /* Get the correct slp vectorized stmt.  */
      gimple *vec_stmt = SLP_TREE_VEC_STMTS (slp_node)[vec_entry]->stmt;
      if (gphi *phi = dyn_cast <gphi *> (vec_stmt))
	vec_lhs = gimple_phi_result (phi);
      else
	vec_lhs = gimple_get_lhs (vec_stmt);

      /* Get entry to use.  */
      bitstart = bitsize_int (vec_index);
      bitstart = int_const_binop (MULT_EXPR, bitsize, bitstart);
    }
  else
    {
      enum vect_def_type dt = STMT_VINFO_DEF_TYPE (stmt_info);
      vec_lhs = vect_get_vec_def_for_operand_1 (stmt_info, dt);
      gcc_checking_assert (ncopies == 1
			   || !LOOP_VINFO_FULLY_MASKED_P (loop_vinfo));

      /* For multiple copies, get the last copy.  */
      for (int i = 1; i < ncopies; ++i)
	vec_lhs = vect_get_vec_def_for_stmt_copy (loop_vinfo, vec_lhs);

      /* Get the last lane in the vector.  */
      bitstart = int_const_binop (MINUS_EXPR, vec_bitsize, bitsize);
    }

  /* Ensure the VEC_LHS for lane extraction stmts satisfy loop-closed PHI
     requirement, insert one phi node for it.  It looks like:
	 loop;
       BB:
	 # lhs' = PHI <lhs>
     ==>
	 loop;
       BB:
	 # vec_lhs' = PHI <vec_lhs>
	 new_tree = lane_extract <vec_lhs', ...>;
	 lhs' = new_tree;  */

  basic_block exit_bb = single_exit (loop)->dest;
  gcc_assert (single_pred_p (exit_bb));

  tree vec_lhs_phi = copy_ssa_name (vec_lhs);
  gimple *phi = create_phi_node (vec_lhs_phi, exit_bb);
  SET_PHI_ARG_DEF (phi, single_exit (loop)->dest_idx, vec_lhs);

  gimple_seq stmts = NULL;
  tree new_tree;
  if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      /* Emit:

	   SCALAR_RES = EXTRACT_LAST <VEC_LHS, MASK>

	 where VEC_LHS is the vectorized live-out result and MASK is
	 the loop mask for the final iteration.  */
      gcc_assert (ncopies == 1 && !slp_node);
      tree scalar_type = TREE_TYPE (STMT_VINFO_VECTYPE (stmt_info));
      tree mask = vect_get_loop_mask (gsi, &LOOP_VINFO_MASKS (loop_vinfo), 1,
				      vectype, 0);
      tree scalar_res = gimple_build (&stmts, CFN_EXTRACT_LAST, scalar_type,
				      mask, vec_lhs_phi);

      /* Convert the extracted vector element to the required scalar type.  */
      new_tree = gimple_convert (&stmts, lhs_type, scalar_res);
    }
  else
    {
      tree bftype = TREE_TYPE (vectype);
      if (VECTOR_BOOLEAN_TYPE_P (vectype))
	bftype = build_nonstandard_integer_type (tree_to_uhwi (bitsize), 1);
      new_tree = build3 (BIT_FIELD_REF, bftype, vec_lhs_phi, bitsize, bitstart);
      new_tree = force_gimple_operand (fold_convert (lhs_type, new_tree),
				       &stmts, true, NULL_TREE);
    }

  if (stmts)
    {
      gimple_stmt_iterator exit_gsi = gsi_after_labels (exit_bb);
      gsi_insert_seq_before (&exit_gsi, stmts, GSI_SAME_STMT);

      /* Remove existing phi from lhs and create one copy from new_tree.  */
      tree lhs_phi = NULL_TREE;
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (exit_bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *phi = gsi_stmt (gsi);
	  if ((gimple_phi_arg_def (phi, 0) == lhs))
	    {
	      remove_phi_node (&gsi, false);
	      lhs_phi = gimple_phi_result (phi);
	      gimple *copy = gimple_build_assign (lhs_phi, new_tree);
	      gsi_insert_before (&exit_gsi, copy, GSI_SAME_STMT);
	      break;
	    }
	}
    }

  /* Replace use of lhs with newly computed result.  If the use stmt is a
     single arg PHI, just replace all uses of PHI result.  It's necessary
     because lcssa PHI defining lhs may be before newly inserted stmt.  */
  use_operand_p use_p;
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, lhs)
    if (!flow_bb_inside_loop_p (loop, gimple_bb (use_stmt))
	&& !is_gimple_debug (use_stmt))
    {
      if (gimple_code (use_stmt) == GIMPLE_PHI
	  && gimple_phi_num_args (use_stmt) == 1)
	{
	  replace_uses_by (gimple_phi_result (use_stmt), new_tree);
	}
      else
	{
	  FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	    SET_USE (use_p, new_tree);
	}
      update_stmt (use_stmt);
    }

  return true;
}

/* Kill any debug uses outside LOOP of SSA names defined in STMT_INFO.  */

static void
vect_loop_kill_debug_uses (class loop *loop, stmt_vec_info stmt_info)
{
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  def_operand_p def_p;
  gimple *ustmt;

  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt_info->stmt, op_iter, SSA_OP_DEF)
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

/* Given loop represented by LOOP_VINFO, return true if computation of
   LOOP_VINFO_NITERS (= LOOP_VINFO_NITERSM1 + 1) doesn't overflow, false
   otherwise.  */

static bool
loop_niters_no_overflow (loop_vec_info loop_vinfo)
{
  /* Constant case.  */
  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
    {
      tree cst_niters = LOOP_VINFO_NITERS (loop_vinfo);
      tree cst_nitersm1 = LOOP_VINFO_NITERSM1 (loop_vinfo);

      gcc_assert (TREE_CODE (cst_niters) == INTEGER_CST);
      gcc_assert (TREE_CODE (cst_nitersm1) == INTEGER_CST);
      if (wi::to_widest (cst_nitersm1) < wi::to_widest (cst_niters))
	return true;
    }

  widest_int max;
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  /* Check the upper bound of loop niters.  */
  if (get_max_loop_iterations (loop, &max))
    {
      tree type = TREE_TYPE (LOOP_VINFO_NITERS (loop_vinfo));
      signop sgn = TYPE_SIGN (type);
      widest_int type_max = widest_int::from (wi::max_value (type), sgn);
      if (max < type_max)
	return true;
    }
  return false;
}

/* Return a mask type with half the number of elements as OLD_TYPE,
   given that it should have mode NEW_MODE.  */

tree
vect_halve_mask_nunits (tree old_type, machine_mode new_mode)
{
  poly_uint64 nunits = exact_div (TYPE_VECTOR_SUBPARTS (old_type), 2);
  return build_truth_vector_type_for_mode (nunits, new_mode);
}

/* Return a mask type with twice as many elements as OLD_TYPE,
   given that it should have mode NEW_MODE.  */

tree
vect_double_mask_nunits (tree old_type, machine_mode new_mode)
{
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (old_type) * 2;
  return build_truth_vector_type_for_mode (nunits, new_mode);
}

/* Record that a fully-masked version of LOOP_VINFO would need MASKS to
   contain a sequence of NVECTORS masks that each control a vector of type
   VECTYPE.  If SCALAR_MASK is nonnull, the fully-masked loop would AND
   these vector masks with the vector version of SCALAR_MASK.  */

void
vect_record_loop_mask (loop_vec_info loop_vinfo, vec_loop_masks *masks,
		       unsigned int nvectors, tree vectype, tree scalar_mask)
{
  gcc_assert (nvectors != 0);
  if (masks->length () < nvectors)
    masks->safe_grow_cleared (nvectors);
  rgroup_masks *rgm = &(*masks)[nvectors - 1];
  /* The number of scalars per iteration and the number of vectors are
     both compile-time constants.  */
  unsigned int nscalars_per_iter
    = exact_div (nvectors * TYPE_VECTOR_SUBPARTS (vectype),
		 LOOP_VINFO_VECT_FACTOR (loop_vinfo)).to_constant ();

  if (scalar_mask)
    {
      scalar_cond_masked_key cond (scalar_mask, nvectors);
      loop_vinfo->scalar_cond_masked_set.add (cond);
    }

  if (rgm->max_nscalars_per_iter < nscalars_per_iter)
    {
      rgm->max_nscalars_per_iter = nscalars_per_iter;
      rgm->mask_type = truth_type_for (vectype);
    }
}

/* Given a complete set of masks MASKS, extract mask number INDEX
   for an rgroup that operates on NVECTORS vectors of type VECTYPE,
   where 0 <= INDEX < NVECTORS.  Insert any set-up statements before GSI.

   See the comment above vec_loop_masks for more details about the mask
   arrangement.  */

tree
vect_get_loop_mask (gimple_stmt_iterator *gsi, vec_loop_masks *masks,
		    unsigned int nvectors, tree vectype, unsigned int index)
{
  rgroup_masks *rgm = &(*masks)[nvectors - 1];
  tree mask_type = rgm->mask_type;

  /* Populate the rgroup's mask array, if this is the first time we've
     used it.  */
  if (rgm->masks.is_empty ())
    {
      rgm->masks.safe_grow_cleared (nvectors);
      for (unsigned int i = 0; i < nvectors; ++i)
	{
	  tree mask = make_temp_ssa_name (mask_type, NULL, "loop_mask");
	  /* Provide a dummy definition until the real one is available.  */
	  SSA_NAME_DEF_STMT (mask) = gimple_build_nop ();
	  rgm->masks[i] = mask;
	}
    }

  tree mask = rgm->masks[index];
  if (maybe_ne (TYPE_VECTOR_SUBPARTS (mask_type),
		TYPE_VECTOR_SUBPARTS (vectype)))
    {
      /* A loop mask for data type X can be reused for data type Y
	 if X has N times more elements than Y and if Y's elements
	 are N times bigger than X's.  In this case each sequence
	 of N elements in the loop mask will be all-zero or all-one.
	 We can then view-convert the mask so that each sequence of
	 N elements is replaced by a single element.  */
      gcc_assert (multiple_p (TYPE_VECTOR_SUBPARTS (mask_type),
			      TYPE_VECTOR_SUBPARTS (vectype)));
      gimple_seq seq = NULL;
      mask_type = truth_type_for (vectype);
      mask = gimple_build (&seq, VIEW_CONVERT_EXPR, mask_type, mask);
      if (seq)
	gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);
    }
  return mask;
}

/* Scale profiling counters by estimation for LOOP which is vectorized
   by factor VF.  */

static void
scale_profile_for_vect_loop (class loop *loop, unsigned vf)
{
  edge preheader = loop_preheader_edge (loop);
  /* Reduce loop iterations by the vectorization factor.  */
  gcov_type new_est_niter = niter_for_unrolled_loop (loop, vf);
  profile_count freq_h = loop->header->count, freq_e = preheader->count ();

  if (freq_h.nonzero_p ())
    {
      profile_probability p;

      /* Avoid dropping loop body profile counter to 0 because of zero count
	 in loop's preheader.  */
      if (!(freq_e == profile_count::zero ()))
        freq_e = freq_e.force_nonzero ();
      p = freq_e.apply_scale (new_est_niter + 1, 1).probability_in (freq_h);
      scale_loop_frequencies (loop, p);
    }

  edge exit_e = single_exit (loop);
  exit_e->probability = profile_probability::always ()
				 .apply_scale (1, new_est_niter + 1);

  edge exit_l = single_pred_edge (loop->latch);
  profile_probability prob = exit_l->probability;
  exit_l->probability = exit_e->probability.invert ();
  if (prob.initialized_p () && exit_l->probability.initialized_p ())
    scale_bbs_frequencies (&loop->latch, 1, exit_l->probability / prob);
}

/* Vectorize STMT_INFO if relevant, inserting any new instructions before GSI.
   When vectorizing STMT_INFO as a store, set *SEEN_STORE to its
   stmt_vec_info.  */

static void
vect_transform_loop_stmt (loop_vec_info loop_vinfo, stmt_vec_info stmt_info,
			  gimple_stmt_iterator *gsi, stmt_vec_info *seen_store)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "------>vectorizing statement: %G", stmt_info->stmt);

  if (MAY_HAVE_DEBUG_BIND_STMTS && !STMT_VINFO_LIVE_P (stmt_info))
    vect_loop_kill_debug_uses (loop, stmt_info);

  if (!STMT_VINFO_RELEVANT_P (stmt_info)
      && !STMT_VINFO_LIVE_P (stmt_info))
    return;

  if (STMT_VINFO_VECTYPE (stmt_info))
    {
      poly_uint64 nunits
	= TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
      if (!STMT_SLP_TYPE (stmt_info)
	  && maybe_ne (nunits, vf)
	  && dump_enabled_p ())
	/* For SLP VF is set according to unrolling factor, and not
	   to vector size, hence for SLP this print is not valid.  */
	dump_printf_loc (MSG_NOTE, vect_location, "multiple-types.\n");
    }

  /* Pure SLP statements have already been vectorized.  We still need
     to apply loop vectorization to hybrid SLP statements.  */
  if (PURE_SLP_STMT (stmt_info))
    return;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform statement.\n");

  if (vect_transform_stmt (stmt_info, gsi, NULL, NULL))
    *seen_store = stmt_info;
}

/* Helper function to pass to simplify_replace_tree to enable replacing tree's
   in the hash_map with its corresponding values.  */

static tree
find_in_mapping (tree t, void *context)
{
  hash_map<tree,tree>* mapping = (hash_map<tree, tree>*) context;

  tree *value = mapping->get (t);
  return value ? *value : t;
}

/* Update EPILOGUE's loop_vec_info.  EPILOGUE was constructed as a copy of the
   original loop that has now been vectorized.

   The inits of the data_references need to be advanced with the number of
   iterations of the main loop.  This has been computed in vect_do_peeling and
   is stored in parameter ADVANCE.  We first restore the data_references
   initial offset with the values recored in ORIG_DRS_INIT.

   Since the loop_vec_info of this EPILOGUE was constructed for the original
   loop, its stmt_vec_infos all point to the original statements.  These need
   to be updated to point to their corresponding copies as well as the SSA_NAMES
   in their PATTERN_DEF_SEQs and RELATED_STMTs.

   The data_reference's connections also need to be updated.  Their
   corresponding dr_vec_info need to be reconnected to the EPILOGUE's
   stmt_vec_infos, their statements need to point to their corresponding copy,
   if they are gather loads or scatter stores then their reference needs to be
   updated to point to its corresponding copy and finally we set
   'base_misaligned' to false as we have already peeled for alignment in the
   prologue of the main loop.  */

static void
update_epilogue_loop_vinfo (class loop *epilogue, tree advance)
{
  loop_vec_info epilogue_vinfo = loop_vec_info_for_loop (epilogue);
  auto_vec<gimple *> stmt_worklist;
  hash_map<tree,tree> mapping;
  gimple *orig_stmt, *new_stmt;
  gimple_stmt_iterator epilogue_gsi;
  gphi_iterator epilogue_phi_gsi;
  stmt_vec_info stmt_vinfo = NULL, related_vinfo;
  basic_block *epilogue_bbs = get_loop_body (epilogue);
  unsigned i;

  LOOP_VINFO_BBS (epilogue_vinfo) = epilogue_bbs;

  /* Advance data_reference's with the number of iterations of the previous
     loop and its prologue.  */
  vect_update_inits_of_drs (epilogue_vinfo, advance, PLUS_EXPR);


  /* The EPILOGUE loop is a copy of the original loop so they share the same
     gimple UIDs.  In this loop we update the loop_vec_info of the EPILOGUE to
     point to the copied statements.  We also create a mapping of all LHS' in
     the original loop and all the LHS' in the EPILOGUE and create worklists to
     update teh STMT_VINFO_PATTERN_DEF_SEQs and STMT_VINFO_RELATED_STMTs.  */
  for (unsigned i = 0; i < epilogue->num_nodes; ++i)
    {
      for (epilogue_phi_gsi = gsi_start_phis (epilogue_bbs[i]);
	   !gsi_end_p (epilogue_phi_gsi); gsi_next (&epilogue_phi_gsi))
	{
	  new_stmt = epilogue_phi_gsi.phi ();

	  gcc_assert (gimple_uid (new_stmt) > 0);
	  stmt_vinfo
	    = epilogue_vinfo->stmt_vec_infos[gimple_uid (new_stmt) - 1];

	  orig_stmt = STMT_VINFO_STMT (stmt_vinfo);
	  STMT_VINFO_STMT (stmt_vinfo) = new_stmt;

	  mapping.put (gimple_phi_result (orig_stmt),
		       gimple_phi_result (new_stmt));
	  /* PHI nodes can not have patterns or related statements.  */
	  gcc_assert (STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo) == NULL
		      && STMT_VINFO_RELATED_STMT (stmt_vinfo) == NULL);
	}

      for (epilogue_gsi = gsi_start_bb (epilogue_bbs[i]);
	   !gsi_end_p (epilogue_gsi); gsi_next (&epilogue_gsi))
	{
	  new_stmt = gsi_stmt (epilogue_gsi);

	  gcc_assert (gimple_uid (new_stmt) > 0);
	  stmt_vinfo
	    = epilogue_vinfo->stmt_vec_infos[gimple_uid (new_stmt) - 1];

	  orig_stmt = STMT_VINFO_STMT (stmt_vinfo);
	  STMT_VINFO_STMT (stmt_vinfo) = new_stmt;

	  if (tree old_lhs = gimple_get_lhs (orig_stmt))
	    mapping.put (old_lhs, gimple_get_lhs (new_stmt));

	  if (STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo))
	    {
	      gimple_seq seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo);
	      for (gimple_stmt_iterator gsi = gsi_start (seq);
		   !gsi_end_p (gsi); gsi_next (&gsi))
		stmt_worklist.safe_push (gsi_stmt (gsi));
	    }

	  related_vinfo = STMT_VINFO_RELATED_STMT (stmt_vinfo);
	  if (related_vinfo != NULL && related_vinfo != stmt_vinfo)
	    {
	      gimple *stmt = STMT_VINFO_STMT (related_vinfo);
	      stmt_worklist.safe_push (stmt);
	      /* Set BB such that the assert in
		'get_initial_def_for_reduction' is able to determine that
		the BB of the related stmt is inside this loop.  */
	      gimple_set_bb (stmt,
			     gimple_bb (new_stmt));
	      related_vinfo = STMT_VINFO_RELATED_STMT (related_vinfo);
	      gcc_assert (related_vinfo == NULL
			  || related_vinfo == stmt_vinfo);
	    }
	}
    }

  /* The PATTERN_DEF_SEQs and RELATED_STMTs in the epilogue were constructed
     using the original main loop and thus need to be updated to refer to the
     cloned variables used in the epilogue.  */
  for (unsigned i = 0; i < stmt_worklist.length (); ++i)
    {
      gimple *stmt = stmt_worklist[i];
      tree *new_op;

      for (unsigned j = 1; j < gimple_num_ops (stmt); ++j)
	{
	  tree op = gimple_op (stmt, j);
	  if ((new_op = mapping.get(op)))
	    gimple_set_op (stmt, j, *new_op);
	  else
	    {
	      /* PR92429: The last argument of simplify_replace_tree disables
		 folding when replacing arguments.  This is required as
		 otherwise you might end up with different statements than the
		 ones analyzed in vect_loop_analyze, leading to different
		 vectorization.  */
	      op = simplify_replace_tree (op, NULL_TREE, NULL_TREE,
					  &find_in_mapping, &mapping, false);
	      gimple_set_op (stmt, j, op);
	    }
	}
    }

  struct data_reference *dr;
  vec<data_reference_p> datarefs = epilogue_vinfo->shared->datarefs;
  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      orig_stmt = DR_STMT (dr);
      gcc_assert (gimple_uid (orig_stmt) > 0);
      stmt_vinfo = epilogue_vinfo->stmt_vec_infos[gimple_uid (orig_stmt) - 1];
      /* Data references for gather loads and scatter stores do not use the
	 updated offset we set using ADVANCE.  Instead we have to make sure the
	 reference in the data references point to the corresponding copy of
	 the original in the epilogue.  */
      if (STMT_VINFO_MEMORY_ACCESS_TYPE (vect_stmt_to_vectorize (stmt_vinfo))
	  == VMAT_GATHER_SCATTER)
	{
	  DR_REF (dr)
	    = simplify_replace_tree (DR_REF (dr), NULL_TREE, NULL_TREE,
				     &find_in_mapping, &mapping);
	  DR_BASE_ADDRESS (dr)
	    = simplify_replace_tree (DR_BASE_ADDRESS (dr), NULL_TREE, NULL_TREE,
				     &find_in_mapping, &mapping);
	}
      DR_STMT (dr) = STMT_VINFO_STMT (stmt_vinfo);
      stmt_vinfo->dr_aux.stmt = stmt_vinfo;
      /* The vector size of the epilogue is smaller than that of the main loop
	 so the alignment is either the same or lower. This means the dr will
	 thus by definition be aligned.  */
      STMT_VINFO_DR_INFO (stmt_vinfo)->base_misaligned = false;
    }

  epilogue_vinfo->shared->datarefs_copy.release ();
  epilogue_vinfo->shared->save_datarefs ();
}

/* Function vect_transform_loop.

   The analysis phase has determined that the loop is vectorizable.
   Vectorize the loop - created vectorized stmts to replace the scalar
   stmts in the loop, and update the loop exit condition.
   Returns scalar epilogue loop if any.  */

class loop *
vect_transform_loop (loop_vec_info loop_vinfo, gimple *loop_vectorized_call)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  class loop *epilogue = NULL;
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  int i;
  tree niters_vector = NULL_TREE;
  tree step_vector = NULL_TREE;
  tree niters_vector_mult_vf = NULL_TREE;
  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  unsigned int lowest_vf = constant_lower_bound (vf);
  gimple *stmt;
  bool check_profitability = false;
  unsigned int th;

  DUMP_VECT_SCOPE ("vec_transform_loop");

  loop_vinfo->shared->check_datarefs ();

  /* Use the more conservative vectorization threshold.  If the number
     of iterations is constant assume the cost check has been performed
     by our caller.  If the threshold makes all loops profitable that
     run at least the (estimated) vectorization factor number of times
     checking is pointless, too.  */
  th = LOOP_VINFO_COST_MODEL_THRESHOLD (loop_vinfo);
  if (vect_apply_runtime_profitability_check_p (loop_vinfo))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Profitability threshold is %d loop iterations.\n",
			 th);
      check_profitability = true;
    }

  /* Make sure there exists a single-predecessor exit bb.  Do this before 
     versioning.   */
  edge e = single_exit (loop);
  if (! single_pred_p (e->dest))
    {
      split_loop_exit_edge (e, true);
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "split exit edge\n");
    }

  /* Version the loop first, if required, so the profitability check
     comes first.  */

  if (LOOP_REQUIRES_VERSIONING (loop_vinfo))
    {
      class loop *sloop
	= vect_loop_versioning (loop_vinfo, loop_vectorized_call);
      sloop->force_vectorize = false;
      check_profitability = false;
    }

  /* Make sure there exists a single-predecessor exit bb also on the
     scalar loop copy.  Do this after versioning but before peeling
     so CFG structure is fine for both scalar and if-converted loop
     to make slpeel_duplicate_current_defs_from_edges face matched
     loop closed PHI nodes on the exit.  */
  if (LOOP_VINFO_SCALAR_LOOP (loop_vinfo))
    {
      e = single_exit (LOOP_VINFO_SCALAR_LOOP (loop_vinfo));
      if (! single_pred_p (e->dest))
	{
	  split_loop_exit_edge (e, true);
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "split exit edge of scalar loop\n");
	}
    }

  tree niters = vect_build_loop_niters (loop_vinfo);
  LOOP_VINFO_NITERS_UNCHANGED (loop_vinfo) = niters;
  tree nitersm1 = unshare_expr (LOOP_VINFO_NITERSM1 (loop_vinfo));
  bool niters_no_overflow = loop_niters_no_overflow (loop_vinfo);
  tree advance;
  drs_init_vec orig_drs_init;

  epilogue = vect_do_peeling (loop_vinfo, niters, nitersm1, &niters_vector,
			      &step_vector, &niters_vector_mult_vf, th,
			      check_profitability, niters_no_overflow,
			      &advance);

  if (LOOP_VINFO_SCALAR_LOOP (loop_vinfo)
      && LOOP_VINFO_SCALAR_LOOP_SCALING (loop_vinfo).initialized_p ())
    scale_loop_frequencies (LOOP_VINFO_SCALAR_LOOP (loop_vinfo),
			    LOOP_VINFO_SCALAR_LOOP_SCALING (loop_vinfo));

  if (niters_vector == NULL_TREE)
    {
      if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
	  && !LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
	  && known_eq (lowest_vf, vf))
	{
	  niters_vector
	    = build_int_cst (TREE_TYPE (LOOP_VINFO_NITERS (loop_vinfo)),
			     LOOP_VINFO_INT_NITERS (loop_vinfo) / lowest_vf);
	  step_vector = build_one_cst (TREE_TYPE (niters));
	}
      else
	vect_gen_vector_loop_niters (loop_vinfo, niters, &niters_vector,
				     &step_vector, niters_no_overflow);
    }

  /* 1) Make sure the loop header has exactly two entries
     2) Make sure we have a preheader basic block.  */

  gcc_assert (EDGE_COUNT (loop->header->preds) == 2);

  split_edge (loop_preheader_edge (loop));

  if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
      && vect_use_loop_mask_for_alignment_p (loop_vinfo))
    /* This will deal with any possible peeling.  */
    vect_prepare_for_masked_peels (loop_vinfo);

  /* Schedule the SLP instances first, then handle loop vectorization
     below.  */
  if (!loop_vinfo->slp_instances.is_empty ())
    {
      DUMP_VECT_SCOPE ("scheduling SLP instances");
      vect_schedule_slp (loop_vinfo);
    }

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
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "------>vectorizing phi: %G", phi);
	  stmt_info = loop_vinfo->lookup_stmt (phi);
	  if (!stmt_info)
	    continue;

	  if (MAY_HAVE_DEBUG_BIND_STMTS && !STMT_VINFO_LIVE_P (stmt_info))
	    vect_loop_kill_debug_uses (loop, stmt_info);

	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
	    continue;

	  if (STMT_VINFO_VECTYPE (stmt_info)
	      && (maybe_ne
		  (TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info)), vf))
	      && dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "multiple-types.\n");

	  if ((STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def
	       || STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def
	       || STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def
	       || STMT_VINFO_DEF_TYPE (stmt_info) == vect_nested_cycle
	       || STMT_VINFO_DEF_TYPE (stmt_info) == vect_internal_def)
	      && ! PURE_SLP_STMT (stmt_info))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location, "transform phi.\n");
	      vect_transform_stmt (stmt_info, NULL, NULL, NULL);
	    }
	}

      for (gimple_stmt_iterator si = gsi_start_bb (bb);
	   !gsi_end_p (si);)
	{
	  stmt = gsi_stmt (si);
	  /* During vectorization remove existing clobber stmts.  */
	  if (gimple_clobber_p (stmt))
	    {
	      unlink_stmt_vdef (stmt);
	      gsi_remove (&si, true);
	      release_defs (stmt);
	    }
	  else
	    {
	      stmt_info = loop_vinfo->lookup_stmt (stmt);

	      /* vector stmts created in the outer-loop during vectorization of
		 stmts in an inner-loop may not have a stmt_info, and do not
		 need to be vectorized.  */
	      stmt_vec_info seen_store = NULL;
	      if (stmt_info)
		{
		  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
		    {
		      gimple *def_seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_info);
		      for (gimple_stmt_iterator subsi = gsi_start (def_seq);
			   !gsi_end_p (subsi); gsi_next (&subsi))
			{
			  stmt_vec_info pat_stmt_info
			    = loop_vinfo->lookup_stmt (gsi_stmt (subsi));
			  vect_transform_loop_stmt (loop_vinfo, pat_stmt_info,
						    &si, &seen_store);
			}
		      stmt_vec_info pat_stmt_info
			= STMT_VINFO_RELATED_STMT (stmt_info);
		      vect_transform_loop_stmt (loop_vinfo, pat_stmt_info, &si,
						&seen_store);
		    }
		  vect_transform_loop_stmt (loop_vinfo, stmt_info, &si,
					    &seen_store);
		}
	      gsi_next (&si);
	      if (seen_store)
		{
		  if (STMT_VINFO_GROUPED_ACCESS (seen_store))
		    /* Interleaving.  If IS_STORE is TRUE, the
		       vectorization of the interleaving chain was
		       completed - free all the stores in the chain.  */
		    vect_remove_stores (DR_GROUP_FIRST_ELEMENT (seen_store));
		  else
		    /* Free the attached stmt_vec_info and remove the stmt.  */
		    loop_vinfo->remove_stmt (stmt_info);
		}
	    }
	}

      /* Stub out scalar statements that must not survive vectorization.
	 Doing this here helps with grouped statements, or statements that
	 are involved in patterns.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gcall *call = dyn_cast <gcall *> (gsi_stmt (gsi));
	  if (call && gimple_call_internal_p (call, IFN_MASK_LOAD))
	    {
	      tree lhs = gimple_get_lhs (call);
	      if (!VECTOR_TYPE_P (TREE_TYPE (lhs)))
		{
		  tree zero = build_zero_cst (TREE_TYPE (lhs));
		  gimple *new_stmt = gimple_build_assign (lhs, zero);
		  gsi_replace (&gsi, new_stmt, true);
		}
	    }
	}
    }				/* BBs in loop */

  /* The vectorization factor is always > 1, so if we use an IV increment of 1.
     a zero NITERS becomes a nonzero NITERS_VECTOR.  */
  if (integer_onep (step_vector))
    niters_no_overflow = true;
  vect_set_loop_condition (loop, loop_vinfo, niters_vector, step_vector,
			   niters_vector_mult_vf, !niters_no_overflow);

  unsigned int assumed_vf = vect_vf_for_cost (loop_vinfo);
  scale_profile_for_vect_loop (loop, assumed_vf);

  /* True if the final iteration might not handle a full vector's
     worth of scalar iterations.  */
  bool final_iter_may_be_partial = LOOP_VINFO_FULLY_MASKED_P (loop_vinfo);
  /* The minimum number of iterations performed by the epilogue.  This
     is 1 when peeling for gaps because we always need a final scalar
     iteration.  */
  int min_epilogue_iters = LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) ? 1 : 0;
  /* +1 to convert latch counts to loop iteration counts,
     -min_epilogue_iters to remove iterations that cannot be performed
       by the vector code.  */
  int bias_for_lowest = 1 - min_epilogue_iters;
  int bias_for_assumed = bias_for_lowest;
  int alignment_npeels = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
  if (alignment_npeels && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      /* When the amount of peeling is known at compile time, the first
	 iteration will have exactly alignment_npeels active elements.
	 In the worst case it will have at least one.  */
      int min_first_active = (alignment_npeels > 0 ? alignment_npeels : 1);
      bias_for_lowest += lowest_vf - min_first_active;
      bias_for_assumed += assumed_vf - min_first_active;
    }
  /* In these calculations the "- 1" converts loop iteration counts
     back to latch counts.  */
  if (loop->any_upper_bound)
    loop->nb_iterations_upper_bound
      = (final_iter_may_be_partial
	 ? wi::udiv_ceil (loop->nb_iterations_upper_bound + bias_for_lowest,
			  lowest_vf) - 1
	 : wi::udiv_floor (loop->nb_iterations_upper_bound + bias_for_lowest,
			   lowest_vf) - 1);
  if (loop->any_likely_upper_bound)
    loop->nb_iterations_likely_upper_bound
      = (final_iter_may_be_partial
	 ? wi::udiv_ceil (loop->nb_iterations_likely_upper_bound
			  + bias_for_lowest, lowest_vf) - 1
	 : wi::udiv_floor (loop->nb_iterations_likely_upper_bound
			   + bias_for_lowest, lowest_vf) - 1);
  if (loop->any_estimate)
    loop->nb_iterations_estimate
      = (final_iter_may_be_partial
	 ? wi::udiv_ceil (loop->nb_iterations_estimate + bias_for_assumed,
			  assumed_vf) - 1
	 : wi::udiv_floor (loop->nb_iterations_estimate + bias_for_assumed,
			   assumed_vf) - 1);

  if (dump_enabled_p ())
    {
      if (!LOOP_VINFO_EPILOGUE_P (loop_vinfo))
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "LOOP VECTORIZED\n");
	  if (loop->inner)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "OUTER LOOP VECTORIZED\n");
	  dump_printf (MSG_NOTE, "\n");
	}
      else
	dump_printf_loc (MSG_NOTE, vect_location,
			 "LOOP EPILOGUE VECTORIZED (MODE=%s)\n",
			 GET_MODE_NAME (loop_vinfo->vector_mode));
    }

  /* Loops vectorized with a variable factor won't benefit from
     unrolling/peeling.  */
  if (!vf.is_constant ())
    {
      loop->unroll = 1;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "Disabling unrolling due to"
			 " variable-length vectorization factor\n");
    }
  /* Free SLP instances here because otherwise stmt reference counting
     won't work.  */
  slp_instance instance;
  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (loop_vinfo), i, instance)
    vect_free_slp_instance (instance, true);
  LOOP_VINFO_SLP_INSTANCES (loop_vinfo).release ();
  /* Clear-up safelen field since its value is invalid after vectorization
     since vectorized loop can have loop-carried dependencies.  */
  loop->safelen = 0;

  if (epilogue)
    {
      update_epilogue_loop_vinfo (epilogue, advance);

      epilogue->simduid = loop->simduid;
      epilogue->force_vectorize = loop->force_vectorize;
      epilogue->dont_vectorize = false;
    }

  return epilogue;
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
optimize_mask_stores (class loop *loop)
{
  basic_block *bbs = get_loop_body (loop);
  unsigned nbbs = loop->num_nodes;
  unsigned i;
  basic_block bb;
  class loop *bb_loop;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  auto_vec<gimple *> worklist;
  auto_purge_vect_location sentinel;

  vect_location = find_loop_location (loop);
  /* Pick up all masked stores in loop if any.  */
  for (i = 0; i < nbbs; i++)
    {
      bb = bbs[i];
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (gimple_call_internal_p (stmt, IFN_MASK_STORE))
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
      /* Create then_bb and if-then structure in CFG, then_bb belongs to
	 the same loop as if_bb.  It could be different to LOOP when two
	 level loop-nest is vectorized and mask_store belongs to the inner
	 one.  */
      e = split_block (bb, last);
      bb_loop = bb->loop_father;
      gcc_assert (loop == bb_loop || flow_loop_nested_p (loop, bb_loop));
      join_bb = e->dest;
      store_bb = create_empty_bb (bb);
      add_bb_to_loop (store_bb, bb_loop);
      e->flags = EDGE_TRUE_VALUE;
      efalse = make_edge (bb, store_bb, EDGE_FALSE_VALUE);
      /* Put STORE_BB to likely part.  */
      efalse->probability = profile_probability::unlikely ();
      store_bb->count = efalse->count ();
      make_single_succ_edge (store_bb, join_bb, EDGE_FALLTHRU);
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
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Move stmt to created bb\n%G", last);
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
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Move stmt to created bb\n%G", stmt1);
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

/* Decide whether it is possible to use a zero-based induction variable
   when vectorizing LOOP_VINFO with a fully-masked loop.  If it is,
   return the value that the induction variable must be able to hold
   in order to ensure that the loop ends with an all-false mask.
   Return -1 otherwise.  */
widest_int
vect_iv_limit_for_full_masking (loop_vec_info loop_vinfo)
{
  tree niters_skip = LOOP_VINFO_MASK_SKIP_NITERS (loop_vinfo);
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  unsigned HOST_WIDE_INT max_vf = vect_max_vf (loop_vinfo);

  /* Calculate the value that the induction variable must be able
     to hit in order to ensure that we end the loop with an all-false mask.
     This involves adding the maximum number of inactive trailing scalar
     iterations.  */
  widest_int iv_limit = -1;
  if (max_loop_iterations (loop, &iv_limit))
    {
      if (niters_skip)
	{
	  /* Add the maximum number of skipped iterations to the
	     maximum iteration count.  */
	  if (TREE_CODE (niters_skip) == INTEGER_CST)
	    iv_limit += wi::to_widest (niters_skip);
	  else
	    iv_limit += max_vf - 1;
	}
      else if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo))
	/* Make a conservatively-correct assumption.  */
	iv_limit += max_vf - 1;

      /* IV_LIMIT is the maximum number of latch iterations, which is also
	 the maximum in-range IV value.  Round this value down to the previous
	 vector alignment boundary and then add an extra full iteration.  */
      poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
      iv_limit = (iv_limit & -(int) known_alignment (vf)) + max_vf;
    }
  return iv_limit;
}

