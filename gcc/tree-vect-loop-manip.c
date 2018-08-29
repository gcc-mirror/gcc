/* Vectorizer Specific Loop Manipulations
   Copyright (C) 2003-2018 Free Software Foundation, Inc.
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
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "fold-const.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "tree-ssa-loop-ivopts.h"
#include "gimple-fold.h"
#include "tree-ssa-loop-niter.h"
#include "internal-fn.h"
#include "stor-layout.h"
#include "optabs-query.h"
#include "vec-perm-indices.h"

/*************************************************************************
  Simple Loop Peeling Utilities

  Utilities to support loop peeling for vectorization purposes.
 *************************************************************************/


/* Renames the use *OP_P.  */

static void
rename_use_op (use_operand_p op_p)
{
  tree new_name;

  if (TREE_CODE (USE_FROM_PTR (op_p)) != SSA_NAME)
    return;

  new_name = get_current_def (USE_FROM_PTR (op_p));

  /* Something defined outside of the loop.  */
  if (!new_name)
    return;

  /* An ordinary ssa name defined in the loop.  */

  SET_USE (op_p, new_name);
}


/* Renames the variables in basic block BB.  Allow renaming  of PHI arguments
   on edges incoming from outer-block header if RENAME_FROM_OUTER_LOOP is
   true.  */

static void
rename_variables_in_bb (basic_block bb, bool rename_from_outer_loop)
{
  gimple *stmt;
  use_operand_p use_p;
  ssa_op_iter iter;
  edge e;
  edge_iterator ei;
  struct loop *loop = bb->loop_father;
  struct loop *outer_loop = NULL;

  if (rename_from_outer_loop)
    {
      gcc_assert (loop);
      outer_loop = loop_outer (loop);
    }

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	rename_use_op (use_p);
    }

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (!flow_bb_inside_loop_p (loop, e->src))
	{
	  if (!rename_from_outer_loop)
	    continue;
	  if (e->src != outer_loop->header)
	    {
	      if (outer_loop->inner->next)
		{
		  /* If outer_loop has 2 inner loops, allow there to
		     be an extra basic block which decides which of the
		     two loops to use using LOOP_VECTORIZED.  */
		  if (!single_pred_p (e->src)
		      || single_pred (e->src) != outer_loop->header)
		    continue;
		}
	    }
	}
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
        rename_use_op (PHI_ARG_DEF_PTR_FROM_EDGE (gsi.phi (), e));
    }
}


struct adjust_info
{
  tree from, to;
  basic_block bb;
};

/* A stack of values to be adjusted in debug stmts.  We have to
   process them LIFO, so that the closest substitution applies.  If we
   processed them FIFO, without the stack, we might substitute uses
   with a PHI DEF that would soon become non-dominant, and when we got
   to the suitable one, it wouldn't have anything to substitute any
   more.  */
static vec<adjust_info, va_heap> adjust_vec;

/* Adjust any debug stmts that referenced AI->from values to use the
   loop-closed AI->to, if the references are dominated by AI->bb and
   not by the definition of AI->from.  */

static void
adjust_debug_stmts_now (adjust_info *ai)
{
  basic_block bbphi = ai->bb;
  tree orig_def = ai->from;
  tree new_def = ai->to;
  imm_use_iterator imm_iter;
  gimple *stmt;
  basic_block bbdef = gimple_bb (SSA_NAME_DEF_STMT (orig_def));

  gcc_assert (dom_info_available_p (CDI_DOMINATORS));

  /* Adjust any debug stmts that held onto non-loop-closed
     references.  */
  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, orig_def)
    {
      use_operand_p use_p;
      basic_block bbuse;

      if (!is_gimple_debug (stmt))
	continue;

      gcc_assert (gimple_debug_bind_p (stmt));

      bbuse = gimple_bb (stmt);

      if ((bbuse == bbphi
	   || dominated_by_p (CDI_DOMINATORS, bbuse, bbphi))
	  && !(bbuse == bbdef
	       || dominated_by_p (CDI_DOMINATORS, bbuse, bbdef)))
	{
	  if (new_def)
	    FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	      SET_USE (use_p, new_def);
	  else
	    {
	      gimple_debug_bind_reset_value (stmt);
	      update_stmt (stmt);
	    }
	}
    }
}

/* Adjust debug stmts as scheduled before.  */

static void
adjust_vec_debug_stmts (void)
{
  if (!MAY_HAVE_DEBUG_BIND_STMTS)
    return;

  gcc_assert (adjust_vec.exists ());

  while (!adjust_vec.is_empty ())
    {
      adjust_debug_stmts_now (&adjust_vec.last ());
      adjust_vec.pop ();
    }
}

/* Adjust any debug stmts that referenced FROM values to use the
   loop-closed TO, if the references are dominated by BB and not by
   the definition of FROM.  If adjust_vec is non-NULL, adjustments
   will be postponed until adjust_vec_debug_stmts is called.  */

static void
adjust_debug_stmts (tree from, tree to, basic_block bb)
{
  adjust_info ai;

  if (MAY_HAVE_DEBUG_BIND_STMTS
      && TREE_CODE (from) == SSA_NAME
      && ! SSA_NAME_IS_DEFAULT_DEF (from)
      && ! virtual_operand_p (from))
    {
      ai.from = from;
      ai.to = to;
      ai.bb = bb;

      if (adjust_vec.exists ())
	adjust_vec.safe_push (ai);
      else
	adjust_debug_stmts_now (&ai);
    }
}

/* Change E's phi arg in UPDATE_PHI to NEW_DEF, and record information
   to adjust any debug stmts that referenced the old phi arg,
   presumably non-loop-closed references left over from other
   transformations.  */

static void
adjust_phi_and_debug_stmts (gimple *update_phi, edge e, tree new_def)
{
  tree orig_def = PHI_ARG_DEF_FROM_EDGE (update_phi, e);

  SET_PHI_ARG_DEF (update_phi, e->dest_idx, new_def);

  if (MAY_HAVE_DEBUG_BIND_STMTS)
    adjust_debug_stmts (orig_def, PHI_RESULT (update_phi),
			gimple_bb (update_phi));
}

/* Define one loop mask MASK from loop LOOP.  INIT_MASK is the value that
   the mask should have during the first iteration and NEXT_MASK is the
   value that it should have on subsequent iterations.  */

static void
vect_set_loop_mask (struct loop *loop, tree mask, tree init_mask,
		    tree next_mask)
{
  gphi *phi = create_phi_node (mask, loop->header);
  add_phi_arg (phi, init_mask, loop_preheader_edge (loop), UNKNOWN_LOCATION);
  add_phi_arg (phi, next_mask, loop_latch_edge (loop), UNKNOWN_LOCATION);
}

/* Add SEQ to the end of LOOP's preheader block.  */

static void
add_preheader_seq (struct loop *loop, gimple_seq seq)
{
  if (seq)
    {
      edge pe = loop_preheader_edge (loop);
      basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
      gcc_assert (!new_bb);
    }
}

/* Add SEQ to the beginning of LOOP's header block.  */

static void
add_header_seq (struct loop *loop, gimple_seq seq)
{
  if (seq)
    {
      gimple_stmt_iterator gsi = gsi_after_labels (loop->header);
      gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
    }
}

/* Return true if the target can interleave elements of two vectors.
   OFFSET is 0 if the first half of the vectors should be interleaved
   or 1 if the second half should.  When returning true, store the
   associated permutation in INDICES.  */

static bool
interleave_supported_p (vec_perm_indices *indices, tree vectype,
			unsigned int offset)
{
  poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (vectype);
  poly_uint64 base = exact_div (nelts, 2) * offset;
  vec_perm_builder sel (nelts, 2, 3);
  for (unsigned int i = 0; i < 3; ++i)
    {
      sel.quick_push (base + i);
      sel.quick_push (base + i + nelts);
    }
  indices->new_vector (sel, 2, nelts);
  return can_vec_perm_const_p (TYPE_MODE (vectype), *indices);
}

/* Try to use permutes to define the masks in DEST_RGM using the masks
   in SRC_RGM, given that the former has twice as many masks as the
   latter.  Return true on success, adding any new statements to SEQ.  */

static bool
vect_maybe_permute_loop_masks (gimple_seq *seq, rgroup_masks *dest_rgm,
			       rgroup_masks *src_rgm)
{
  tree src_masktype = src_rgm->mask_type;
  tree dest_masktype = dest_rgm->mask_type;
  machine_mode src_mode = TYPE_MODE (src_masktype);
  if (dest_rgm->max_nscalars_per_iter <= src_rgm->max_nscalars_per_iter
      && optab_handler (vec_unpacku_hi_optab, src_mode) != CODE_FOR_nothing
      && optab_handler (vec_unpacku_lo_optab, src_mode) != CODE_FOR_nothing)
    {
      /* Unpacking the source masks gives at least as many mask bits as
	 we need.  We can then VIEW_CONVERT any excess bits away.  */
      tree unpack_masktype = vect_halve_mask_nunits (src_masktype);
      for (unsigned int i = 0; i < dest_rgm->masks.length (); ++i)
	{
	  tree src = src_rgm->masks[i / 2];
	  tree dest = dest_rgm->masks[i];
	  tree_code code = ((i & 1) == (BYTES_BIG_ENDIAN ? 0 : 1)
			    ? VEC_UNPACK_HI_EXPR
			    : VEC_UNPACK_LO_EXPR);
	  gassign *stmt;
	  if (dest_masktype == unpack_masktype)
	    stmt = gimple_build_assign (dest, code, src);
	  else
	    {
	      tree temp = make_ssa_name (unpack_masktype);
	      stmt = gimple_build_assign (temp, code, src);
	      gimple_seq_add_stmt (seq, stmt);
	      stmt = gimple_build_assign (dest, VIEW_CONVERT_EXPR,
					  build1 (VIEW_CONVERT_EXPR,
						  dest_masktype, temp));
	    }
	  gimple_seq_add_stmt (seq, stmt);
	}
      return true;
    }
  vec_perm_indices indices[2];
  if (dest_masktype == src_masktype
      && interleave_supported_p (&indices[0], src_masktype, 0)
      && interleave_supported_p (&indices[1], src_masktype, 1))
    {
      /* The destination requires twice as many mask bits as the source, so
	 we can use interleaving permutes to double up the number of bits.  */
      tree masks[2];
      for (unsigned int i = 0; i < 2; ++i)
	masks[i] = vect_gen_perm_mask_checked (src_masktype, indices[i]);
      for (unsigned int i = 0; i < dest_rgm->masks.length (); ++i)
	{
	  tree src = src_rgm->masks[i / 2];
	  tree dest = dest_rgm->masks[i];
	  gimple *stmt = gimple_build_assign (dest, VEC_PERM_EXPR,
					      src, src, masks[i & 1]);
	  gimple_seq_add_stmt (seq, stmt);
	}
      return true;
    }
  return false;
}

/* Helper for vect_set_loop_condition_masked.  Generate definitions for
   all the masks in RGM and return a mask that is nonzero when the loop
   needs to iterate.  Add any new preheader statements to PREHEADER_SEQ.
   Use LOOP_COND_GSI to insert code before the exit gcond.

   RGM belongs to loop LOOP.  The loop originally iterated NITERS
   times and has been vectorized according to LOOP_VINFO.  Each iteration
   of the vectorized loop handles VF iterations of the scalar loop.

   If NITERS_SKIP is nonnull, the first iteration of the vectorized loop
   starts with NITERS_SKIP dummy iterations of the scalar loop before
   the real work starts.  The mask elements for these dummy iterations
   must be 0, to ensure that the extra iterations do not have an effect.

   It is known that:

     NITERS * RGM->max_nscalars_per_iter

   does not overflow.  However, MIGHT_WRAP_P says whether an induction
   variable that starts at 0 and has step:

     VF * RGM->max_nscalars_per_iter

   might overflow before hitting a value above:

     (NITERS + NITERS_SKIP) * RGM->max_nscalars_per_iter

   This means that we cannot guarantee that such an induction variable
   would ever hit a value that produces a set of all-false masks for RGM.  */

static tree
vect_set_loop_masks_directly (struct loop *loop, loop_vec_info loop_vinfo,
			      gimple_seq *preheader_seq,
			      gimple_stmt_iterator loop_cond_gsi,
			      rgroup_masks *rgm, tree vf,
			      tree niters, tree niters_skip,
			      bool might_wrap_p)
{
  tree compare_type = LOOP_VINFO_MASK_COMPARE_TYPE (loop_vinfo);
  tree mask_type = rgm->mask_type;
  unsigned int nscalars_per_iter = rgm->max_nscalars_per_iter;
  poly_uint64 nscalars_per_mask = TYPE_VECTOR_SUBPARTS (mask_type);

  /* Calculate the maximum number of scalar values that the rgroup
     handles in total, the number that it handles for each iteration
     of the vector loop, and the number that it should skip during the
     first iteration of the vector loop.  */
  tree nscalars_total = niters;
  tree nscalars_step = vf;
  tree nscalars_skip = niters_skip;
  if (nscalars_per_iter != 1)
    {
      /* We checked before choosing to use a fully-masked loop that these
	 multiplications don't overflow.  */
      tree factor = build_int_cst (compare_type, nscalars_per_iter);
      nscalars_total = gimple_build (preheader_seq, MULT_EXPR, compare_type,
				     nscalars_total, factor);
      nscalars_step = gimple_build (preheader_seq, MULT_EXPR, compare_type,
				    nscalars_step, factor);
      if (nscalars_skip)
	nscalars_skip = gimple_build (preheader_seq, MULT_EXPR, compare_type,
				      nscalars_skip, factor);
    }

  /* Create an induction variable that counts the number of scalars
     processed.  */
  tree index_before_incr, index_after_incr;
  gimple_stmt_iterator incr_gsi;
  bool insert_after;
  tree zero_index = build_int_cst (compare_type, 0);
  standard_iv_increment_position (loop, &incr_gsi, &insert_after);
  create_iv (zero_index, nscalars_step, NULL_TREE, loop, &incr_gsi,
	     insert_after, &index_before_incr, &index_after_incr);

  tree test_index, test_limit, first_limit;
  gimple_stmt_iterator *test_gsi;
  if (might_wrap_p)
    {
      /* In principle the loop should stop iterating once the incremented
	 IV reaches a value greater than or equal to:

	   NSCALARS_TOTAL +[infinite-prec] NSCALARS_SKIP

	 However, there's no guarantee that this addition doesn't overflow
	 the comparison type, or that the IV hits a value above it before
	 wrapping around.  We therefore adjust the limit down by one
	 IV step:

	   (NSCALARS_TOTAL +[infinite-prec] NSCALARS_SKIP)
	   -[infinite-prec] NSCALARS_STEP

	 and compare the IV against this limit _before_ incrementing it.
	 Since the comparison type is unsigned, we actually want the
	 subtraction to saturate at zero:

	   (NSCALARS_TOTAL +[infinite-prec] NSCALARS_SKIP)
	   -[sat] NSCALARS_STEP

	 And since NSCALARS_SKIP < NSCALARS_STEP, we can reassociate this as:

	   NSCALARS_TOTAL -[sat] (NSCALARS_STEP - NSCALARS_SKIP)

	 where the rightmost subtraction can be done directly in
	 COMPARE_TYPE.  */
      test_index = index_before_incr;
      tree adjust = nscalars_step;
      if (nscalars_skip)
	adjust = gimple_build (preheader_seq, MINUS_EXPR, compare_type,
			       adjust, nscalars_skip);
      test_limit = gimple_build (preheader_seq, MAX_EXPR, compare_type,
				 nscalars_total, adjust);
      test_limit = gimple_build (preheader_seq, MINUS_EXPR, compare_type,
				 test_limit, adjust);
      test_gsi = &incr_gsi;

      /* Get a safe limit for the first iteration.  */
      if (nscalars_skip)
	{
	  /* The first vector iteration can handle at most NSCALARS_STEP
	     scalars.  NSCALARS_STEP <= CONST_LIMIT, and adding
	     NSCALARS_SKIP to that cannot overflow.  */
	  tree const_limit = build_int_cst (compare_type,
					    LOOP_VINFO_VECT_FACTOR (loop_vinfo)
					    * nscalars_per_iter);
	  first_limit = gimple_build (preheader_seq, MIN_EXPR, compare_type,
				      nscalars_total, const_limit);
	  first_limit = gimple_build (preheader_seq, PLUS_EXPR, compare_type,
				      first_limit, nscalars_skip);
	}
      else
	/* For the first iteration it doesn't matter whether the IV hits
	   a value above NSCALARS_TOTAL.  That only matters for the latch
	   condition.  */
	first_limit = nscalars_total;
    }
  else
    {
      /* Test the incremented IV, which will always hit a value above
	 the bound before wrapping.  */
      test_index = index_after_incr;
      test_limit = nscalars_total;
      if (nscalars_skip)
	test_limit = gimple_build (preheader_seq, PLUS_EXPR, compare_type,
				   test_limit, nscalars_skip);
      test_gsi = &loop_cond_gsi;

      first_limit = test_limit;
    }

  /* Provide a definition of each mask in the group.  */
  tree next_mask = NULL_TREE;
  tree mask;
  unsigned int i;
  FOR_EACH_VEC_ELT_REVERSE (rgm->masks, i, mask)
    {
      /* Previous masks will cover BIAS scalars.  This mask covers the
	 next batch.  */
      poly_uint64 bias = nscalars_per_mask * i;
      tree bias_tree = build_int_cst (compare_type, bias);
      gimple *tmp_stmt;

      /* See whether the first iteration of the vector loop is known
	 to have a full mask.  */
      poly_uint64 const_limit;
      bool first_iteration_full
	= (poly_int_tree_p (first_limit, &const_limit)
	   && known_ge (const_limit, (i + 1) * nscalars_per_mask));

      /* Rather than have a new IV that starts at BIAS and goes up to
	 TEST_LIMIT, prefer to use the same 0-based IV for each mask
	 and adjust the bound down by BIAS.  */
      tree this_test_limit = test_limit;
      if (i != 0)
	{
	  this_test_limit = gimple_build (preheader_seq, MAX_EXPR,
					  compare_type, this_test_limit,
					  bias_tree);
	  this_test_limit = gimple_build (preheader_seq, MINUS_EXPR,
					  compare_type, this_test_limit,
					  bias_tree);
	}

      /* Create the initial mask.  First include all scalars that
	 are within the loop limit.  */
      tree init_mask = NULL_TREE;
      if (!first_iteration_full)
	{
	  tree start, end;
	  if (first_limit == test_limit)
	    {
	      /* Use a natural test between zero (the initial IV value)
		 and the loop limit.  The "else" block would be valid too,
		 but this choice can avoid the need to load BIAS_TREE into
		 a register.  */
	      start = zero_index;
	      end = this_test_limit;
	    }
	  else
	    {
	      /* FIRST_LIMIT is the maximum number of scalars handled by the
		 first iteration of the vector loop.  Test the portion
		 associated with this mask.  */
	      start = bias_tree;
	      end = first_limit;
	    }

	  init_mask = make_temp_ssa_name (mask_type, NULL, "max_mask");
	  tmp_stmt = vect_gen_while (init_mask, start, end);
	  gimple_seq_add_stmt (preheader_seq, tmp_stmt);
	}

      /* Now AND out the bits that are within the number of skipped
	 scalars.  */
      poly_uint64 const_skip;
      if (nscalars_skip
	  && !(poly_int_tree_p (nscalars_skip, &const_skip)
	       && known_le (const_skip, bias)))
	{
	  tree unskipped_mask = vect_gen_while_not (preheader_seq, mask_type,
						    bias_tree, nscalars_skip);
	  if (init_mask)
	    init_mask = gimple_build (preheader_seq, BIT_AND_EXPR, mask_type,
				      init_mask, unskipped_mask);
	  else
	    init_mask = unskipped_mask;
	}

      if (!init_mask)
	/* First iteration is full.  */
	init_mask = build_minus_one_cst (mask_type);

      /* Get the mask value for the next iteration of the loop.  */
      next_mask = make_temp_ssa_name (mask_type, NULL, "next_mask");
      gcall *call = vect_gen_while (next_mask, test_index, this_test_limit);
      gsi_insert_before (test_gsi, call, GSI_SAME_STMT);

      vect_set_loop_mask (loop, mask, init_mask, next_mask);
    }
  return next_mask;
}

/* Make LOOP iterate NITERS times using masking and WHILE_ULT calls.
   LOOP_VINFO describes the vectorization of LOOP.  NITERS is the
   number of iterations of the original scalar loop that should be
   handled by the vector loop.  NITERS_MAYBE_ZERO and FINAL_IV are
   as for vect_set_loop_condition.

   Insert the branch-back condition before LOOP_COND_GSI and return the
   final gcond.  */

static gcond *
vect_set_loop_condition_masked (struct loop *loop, loop_vec_info loop_vinfo,
				tree niters, tree final_iv,
				bool niters_maybe_zero,
				gimple_stmt_iterator loop_cond_gsi)
{
  gimple_seq preheader_seq = NULL;
  gimple_seq header_seq = NULL;

  tree compare_type = LOOP_VINFO_MASK_COMPARE_TYPE (loop_vinfo);
  unsigned int compare_precision = TYPE_PRECISION (compare_type);
  unsigned HOST_WIDE_INT max_vf = vect_max_vf (loop_vinfo);
  tree orig_niters = niters;

  /* Type of the initial value of NITERS.  */
  tree ni_actual_type = TREE_TYPE (niters);
  unsigned int ni_actual_precision = TYPE_PRECISION (ni_actual_type);

  /* Convert NITERS to the same size as the compare.  */
  if (compare_precision > ni_actual_precision
      && niters_maybe_zero)
    {
      /* We know that there is always at least one iteration, so if the
	 count is zero then it must have wrapped.  Cope with this by
	 subtracting 1 before the conversion and adding 1 to the result.  */
      gcc_assert (TYPE_UNSIGNED (ni_actual_type));
      niters = gimple_build (&preheader_seq, PLUS_EXPR, ni_actual_type,
			     niters, build_minus_one_cst (ni_actual_type));
      niters = gimple_convert (&preheader_seq, compare_type, niters);
      niters = gimple_build (&preheader_seq, PLUS_EXPR, compare_type,
			     niters, build_one_cst (compare_type));
    }
  else
    niters = gimple_convert (&preheader_seq, compare_type, niters);

  /* Convert skip_niters to the right type.  */
  tree niters_skip = LOOP_VINFO_MASK_SKIP_NITERS (loop_vinfo);

  /* Now calculate the value that the induction variable must be able
     to hit in order to ensure that we end the loop with an all-false mask.
     This involves adding the maximum number of inactive trailing scalar
     iterations.  */
  widest_int iv_limit;
  bool known_max_iters = max_loop_iterations (loop, &iv_limit);
  if (known_max_iters)
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
      /* IV_LIMIT is the maximum number of latch iterations, which is also
	 the maximum in-range IV value.  Round this value down to the previous
	 vector alignment boundary and then add an extra full iteration.  */
      poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
      iv_limit = (iv_limit & -(int) known_alignment (vf)) + max_vf;
    }

  /* Get the vectorization factor in tree form.  */
  tree vf = build_int_cst (compare_type,
			   LOOP_VINFO_VECT_FACTOR (loop_vinfo));

  /* Iterate over all the rgroups and fill in their masks.  We could use
     the first mask from any rgroup for the loop condition; here we
     arbitrarily pick the last.  */
  tree test_mask = NULL_TREE;
  rgroup_masks *rgm;
  unsigned int i;
  vec_loop_masks *masks = &LOOP_VINFO_MASKS (loop_vinfo);
  FOR_EACH_VEC_ELT (*masks, i, rgm)
    if (!rgm->masks.is_empty ())
      {
	/* First try using permutes.  This adds a single vector
	   instruction to the loop for each mask, but needs no extra
	   loop invariants or IVs.  */
	unsigned int nmasks = i + 1;
	if ((nmasks & 1) == 0)
	  {
	    rgroup_masks *half_rgm = &(*masks)[nmasks / 2 - 1];
	    if (!half_rgm->masks.is_empty ()
		&& vect_maybe_permute_loop_masks (&header_seq, rgm, half_rgm))
	      continue;
	  }

	/* See whether zero-based IV would ever generate all-false masks
	   before wrapping around.  */
	bool might_wrap_p
	  = (!known_max_iters
	     || (wi::min_precision (iv_limit * rgm->max_nscalars_per_iter,
				    UNSIGNED)
		 > compare_precision));

	/* Set up all masks for this group.  */
	test_mask = vect_set_loop_masks_directly (loop, loop_vinfo,
						  &preheader_seq,
						  loop_cond_gsi, rgm, vf,
						  niters, niters_skip,
						  might_wrap_p);
      }

  /* Emit all accumulated statements.  */
  add_preheader_seq (loop, preheader_seq);
  add_header_seq (loop, header_seq);

  /* Get a boolean result that tells us whether to iterate.  */
  edge exit_edge = single_exit (loop);
  tree_code code = (exit_edge->flags & EDGE_TRUE_VALUE) ? EQ_EXPR : NE_EXPR;
  tree zero_mask = build_zero_cst (TREE_TYPE (test_mask));
  gcond *cond_stmt = gimple_build_cond (code, test_mask, zero_mask,
					NULL_TREE, NULL_TREE);
  gsi_insert_before (&loop_cond_gsi, cond_stmt, GSI_SAME_STMT);

  /* The loop iterates (NITERS - 1) / VF + 1 times.
     Subtract one from this to get the latch count.  */
  tree step = build_int_cst (compare_type,
			     LOOP_VINFO_VECT_FACTOR (loop_vinfo));
  tree niters_minus_one = fold_build2 (PLUS_EXPR, compare_type, niters,
				       build_minus_one_cst (compare_type));
  loop->nb_iterations = fold_build2 (TRUNC_DIV_EXPR, compare_type,
				     niters_minus_one, step);

  if (final_iv)
    {
      gassign *assign = gimple_build_assign (final_iv, orig_niters);
      gsi_insert_on_edge_immediate (single_exit (loop), assign);
    }

  return cond_stmt;
}

/* Like vect_set_loop_condition, but handle the case in which there
   are no loop masks.  */

static gcond *
vect_set_loop_condition_unmasked (struct loop *loop, tree niters,
				  tree step, tree final_iv,
				  bool niters_maybe_zero,
				  gimple_stmt_iterator loop_cond_gsi)
{
  tree indx_before_incr, indx_after_incr;
  gcond *cond_stmt;
  gcond *orig_cond;
  edge pe = loop_preheader_edge (loop);
  edge exit_edge = single_exit (loop);
  gimple_stmt_iterator incr_gsi;
  bool insert_after;
  enum tree_code code;
  tree niters_type = TREE_TYPE (niters);

  orig_cond = get_loop_exit_condition (loop);
  gcc_assert (orig_cond);
  loop_cond_gsi = gsi_for_stmt (orig_cond);

  tree init, limit;
  if (!niters_maybe_zero && integer_onep (step))
    {
      /* In this case we can use a simple 0-based IV:

	 A:
	   x = 0;
	   do
	     {
	       ...
	       x += 1;
	     }
	   while (x < NITERS);  */
      code = (exit_edge->flags & EDGE_TRUE_VALUE) ? GE_EXPR : LT_EXPR;
      init = build_zero_cst (niters_type);
      limit = niters;
    }
  else
    {
      /* The following works for all values of NITERS except 0:

	 B:
	   x = 0;
	   do
	     {
	       ...
	       x += STEP;
	     }
	   while (x <= NITERS - STEP);

	 so that the loop continues to iterate if x + STEP - 1 < NITERS
	 but stops if x + STEP - 1 >= NITERS.

	 However, if NITERS is zero, x never hits a value above NITERS - STEP
	 before wrapping around.  There are two obvious ways of dealing with
	 this:

	 - start at STEP - 1 and compare x before incrementing it
	 - start at -1 and compare x after incrementing it

	 The latter is simpler and is what we use.  The loop in this case
	 looks like:

	 C:
	   x = -1;
	   do
	     {
	       ...
	       x += STEP;
	     }
	   while (x < NITERS - STEP);

	 In both cases the loop limit is NITERS - STEP.  */
      gimple_seq seq = NULL;
      limit = force_gimple_operand (niters, &seq, true, NULL_TREE);
      limit = gimple_build (&seq, MINUS_EXPR, TREE_TYPE (limit), limit, step);
      if (seq)
	{
	  basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
	  gcc_assert (!new_bb);
	}
      if (niters_maybe_zero)
	{
	  /* Case C.  */
	  code = (exit_edge->flags & EDGE_TRUE_VALUE) ? GE_EXPR : LT_EXPR;
	  init = build_all_ones_cst (niters_type);
	}
      else
	{
	  /* Case B.  */
	  code = (exit_edge->flags & EDGE_TRUE_VALUE) ? GT_EXPR : LE_EXPR;
	  init = build_zero_cst (niters_type);
	}
    }

  standard_iv_increment_position (loop, &incr_gsi, &insert_after);
  create_iv (init, step, NULL_TREE, loop,
             &incr_gsi, insert_after, &indx_before_incr, &indx_after_incr);
  indx_after_incr = force_gimple_operand_gsi (&loop_cond_gsi, indx_after_incr,
					      true, NULL_TREE, true,
					      GSI_SAME_STMT);
  limit = force_gimple_operand_gsi (&loop_cond_gsi, limit, true, NULL_TREE,
				     true, GSI_SAME_STMT);

  cond_stmt = gimple_build_cond (code, indx_after_incr, limit, NULL_TREE,
				 NULL_TREE);

  gsi_insert_before (&loop_cond_gsi, cond_stmt, GSI_SAME_STMT);

  /* Record the number of latch iterations.  */
  if (limit == niters)
    /* Case A: the loop iterates NITERS times.  Subtract one to get the
       latch count.  */
    loop->nb_iterations = fold_build2 (MINUS_EXPR, niters_type, niters,
				       build_int_cst (niters_type, 1));
  else
    /* Case B or C: the loop iterates (NITERS - STEP) / STEP + 1 times.
       Subtract one from this to get the latch count.  */
    loop->nb_iterations = fold_build2 (TRUNC_DIV_EXPR, niters_type,
				       limit, step);

  if (final_iv)
    {
      gassign *assign = gimple_build_assign (final_iv, MINUS_EXPR,
					     indx_after_incr, init);
      gsi_insert_on_edge_immediate (single_exit (loop), assign);
    }

  return cond_stmt;
}

/* If we're using fully-masked loops, make LOOP iterate:

      N == (NITERS - 1) / STEP + 1

   times.  When NITERS is zero, this is equivalent to making the loop
   execute (1 << M) / STEP times, where M is the precision of NITERS.
   NITERS_MAYBE_ZERO is true if this last case might occur.

   If we're not using fully-masked loops, make LOOP iterate:

      N == (NITERS - STEP) / STEP + 1

   times, where NITERS is known to be outside the range [1, STEP - 1].
   This is equivalent to making the loop execute NITERS / STEP times
   when NITERS is nonzero and (1 << M) / STEP times otherwise.
   NITERS_MAYBE_ZERO again indicates whether this last case might occur.

   If FINAL_IV is nonnull, it is an SSA name that should be set to
   N * STEP on exit from the loop.

   Assumption: the exit-condition of LOOP is the last stmt in the loop.  */

void
vect_set_loop_condition (struct loop *loop, loop_vec_info loop_vinfo,
			 tree niters, tree step, tree final_iv,
			 bool niters_maybe_zero)
{
  gcond *cond_stmt;
  gcond *orig_cond = get_loop_exit_condition (loop);
  gimple_stmt_iterator loop_cond_gsi = gsi_for_stmt (orig_cond);

  if (loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    cond_stmt = vect_set_loop_condition_masked (loop, loop_vinfo, niters,
						final_iv, niters_maybe_zero,
						loop_cond_gsi);
  else
    cond_stmt = vect_set_loop_condition_unmasked (loop, niters, step,
						  final_iv, niters_maybe_zero,
						  loop_cond_gsi);

  /* Remove old loop exit test.  */
  stmt_vec_info orig_cond_info;
  if (loop_vinfo
      && (orig_cond_info = loop_vinfo->lookup_stmt (orig_cond)))
    loop_vinfo->remove_stmt (orig_cond_info);
  else
    gsi_remove (&loop_cond_gsi, true);

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "New loop exit condition: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, cond_stmt, 0);
    }
}

/* Helper routine of slpeel_tree_duplicate_loop_to_edge_cfg.
   For all PHI arguments in FROM->dest and TO->dest from those
   edges ensure that TO->dest PHI arguments have current_def
   to that in from.  */

static void
slpeel_duplicate_current_defs_from_edges (edge from, edge to)
{
  gimple_stmt_iterator gsi_from, gsi_to;

  for (gsi_from = gsi_start_phis (from->dest),
       gsi_to = gsi_start_phis (to->dest);
       !gsi_end_p (gsi_from) && !gsi_end_p (gsi_to);)
    {
      gimple *from_phi = gsi_stmt (gsi_from);
      gimple *to_phi = gsi_stmt (gsi_to);
      tree from_arg = PHI_ARG_DEF_FROM_EDGE (from_phi, from);
      tree to_arg = PHI_ARG_DEF_FROM_EDGE (to_phi, to);
      if (virtual_operand_p (from_arg))
	{
	  gsi_next (&gsi_from);
	  continue;
	}
      if (virtual_operand_p (to_arg))
	{
	  gsi_next (&gsi_to);
	  continue;
	}
      if (TREE_CODE (from_arg) != SSA_NAME)
	gcc_assert (operand_equal_p (from_arg, to_arg, 0));
      else
	{
	  if (get_current_def (to_arg) == NULL_TREE)
	    set_current_def (to_arg, get_current_def (from_arg));
	}
      gsi_next (&gsi_from);
      gsi_next (&gsi_to);
    }

  gphi *from_phi = get_virtual_phi (from->dest);
  gphi *to_phi = get_virtual_phi (to->dest);
  if (from_phi)
    set_current_def (PHI_ARG_DEF_FROM_EDGE (to_phi, to),
		     get_current_def (PHI_ARG_DEF_FROM_EDGE (from_phi, from)));
}


/* Given LOOP this function generates a new copy of it and puts it
   on E which is either the entry or exit of LOOP.  If SCALAR_LOOP is
   non-NULL, assume LOOP and SCALAR_LOOP are equivalent and copy the
   basic blocks from SCALAR_LOOP instead of LOOP, but to either the
   entry or exit of LOOP.  */

struct loop *
slpeel_tree_duplicate_loop_to_edge_cfg (struct loop *loop,
					struct loop *scalar_loop, edge e)
{
  struct loop *new_loop;
  basic_block *new_bbs, *bbs, *pbbs;
  bool at_exit;
  bool was_imm_dom;
  basic_block exit_dest;
  edge exit, new_exit;
  bool duplicate_outer_loop = false;

  exit = single_exit (loop);
  at_exit = (e == exit);
  if (!at_exit && e != loop_preheader_edge (loop))
    return NULL;

  if (scalar_loop == NULL)
    scalar_loop = loop;

  bbs = XNEWVEC (basic_block, scalar_loop->num_nodes + 1);
  pbbs = bbs + 1;
  get_loop_body_with_size (scalar_loop, pbbs, scalar_loop->num_nodes);
  /* Allow duplication of outer loops.  */
  if (scalar_loop->inner)
    duplicate_outer_loop = true;
  /* Check whether duplication is possible.  */
  if (!can_copy_bbs_p (pbbs, scalar_loop->num_nodes))
    {
      free (bbs);
      return NULL;
    }

  /* Generate new loop structure.  */
  new_loop = duplicate_loop (scalar_loop, loop_outer (scalar_loop));
  duplicate_subloops (scalar_loop, new_loop);

  exit_dest = exit->dest;
  was_imm_dom = (get_immediate_dominator (CDI_DOMINATORS,
					  exit_dest) == loop->header ?
		 true : false);

  /* Also copy the pre-header, this avoids jumping through hoops to
     duplicate the loop entry PHI arguments.  Create an empty
     pre-header unconditionally for this.  */
  basic_block preheader = split_edge (loop_preheader_edge (scalar_loop));
  edge entry_e = single_pred_edge (preheader);
  bbs[0] = preheader;
  new_bbs = XNEWVEC (basic_block, scalar_loop->num_nodes + 1);

  exit = single_exit (scalar_loop);
  copy_bbs (bbs, scalar_loop->num_nodes + 1, new_bbs,
	    &exit, 1, &new_exit, NULL,
	    at_exit ? loop->latch : e->src, true);
  exit = single_exit (loop);
  basic_block new_preheader = new_bbs[0];

  add_phi_args_after_copy (new_bbs, scalar_loop->num_nodes + 1, NULL);

  if (scalar_loop != loop)
    {
      /* If we copied from SCALAR_LOOP rather than LOOP, SSA_NAMEs from
	 SCALAR_LOOP will have current_def set to SSA_NAMEs in the new_loop,
	 but LOOP will not.  slpeel_update_phi_nodes_for_guard{1,2} expects
	 the LOOP SSA_NAMEs (on the exit edge and edge from latch to
	 header) to have current_def set, so copy them over.  */
      slpeel_duplicate_current_defs_from_edges (single_exit (scalar_loop),
						exit);
      slpeel_duplicate_current_defs_from_edges (EDGE_SUCC (scalar_loop->latch,
							   0),
						EDGE_SUCC (loop->latch, 0));
    }

  if (at_exit) /* Add the loop copy at exit.  */
    {
      if (scalar_loop != loop)
	{
	  gphi_iterator gsi;
	  new_exit = redirect_edge_and_branch (new_exit, exit_dest);

	  for (gsi = gsi_start_phis (exit_dest); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gphi *phi = gsi.phi ();
	      tree orig_arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
	      location_t orig_locus
		= gimple_phi_arg_location_from_edge (phi, e);

	      add_phi_arg (phi, orig_arg, new_exit, orig_locus);
	    }
	}
      redirect_edge_and_branch_force (e, new_preheader);
      flush_pending_stmts (e);
      set_immediate_dominator (CDI_DOMINATORS, new_preheader, e->src);
      if (was_imm_dom || duplicate_outer_loop)
	set_immediate_dominator (CDI_DOMINATORS, exit_dest, new_exit->src);

      /* And remove the non-necessary forwarder again.  Keep the other
         one so we have a proper pre-header for the loop at the exit edge.  */
      redirect_edge_pred (single_succ_edge (preheader),
			  single_pred (preheader));
      delete_basic_block (preheader);
      set_immediate_dominator (CDI_DOMINATORS, scalar_loop->header,
			       loop_preheader_edge (scalar_loop)->src);
    }
  else /* Add the copy at entry.  */
    {
      if (scalar_loop != loop)
	{
	  /* Remove the non-necessary forwarder of scalar_loop again.  */
	  redirect_edge_pred (single_succ_edge (preheader),
			      single_pred (preheader));
	  delete_basic_block (preheader);
	  set_immediate_dominator (CDI_DOMINATORS, scalar_loop->header,
				   loop_preheader_edge (scalar_loop)->src);
	  preheader = split_edge (loop_preheader_edge (loop));
	  entry_e = single_pred_edge (preheader);
	}

      redirect_edge_and_branch_force (entry_e, new_preheader);
      flush_pending_stmts (entry_e);
      set_immediate_dominator (CDI_DOMINATORS, new_preheader, entry_e->src);

      redirect_edge_and_branch_force (new_exit, preheader);
      flush_pending_stmts (new_exit);
      set_immediate_dominator (CDI_DOMINATORS, preheader, new_exit->src);

      /* And remove the non-necessary forwarder again.  Keep the other
         one so we have a proper pre-header for the loop at the exit edge.  */
      redirect_edge_pred (single_succ_edge (new_preheader),
			  single_pred (new_preheader));
      delete_basic_block (new_preheader);
      set_immediate_dominator (CDI_DOMINATORS, new_loop->header,
			       loop_preheader_edge (new_loop)->src);
    }

  /* Skip new preheader since it's deleted if copy loop is added at entry.  */
  for (unsigned i = (at_exit ? 0 : 1); i < scalar_loop->num_nodes + 1; i++)
    rename_variables_in_bb (new_bbs[i], duplicate_outer_loop);

  if (scalar_loop != loop)
    {
      /* Update new_loop->header PHIs, so that on the preheader
	 edge they are the ones from loop rather than scalar_loop.  */
      gphi_iterator gsi_orig, gsi_new;
      edge orig_e = loop_preheader_edge (loop);
      edge new_e = loop_preheader_edge (new_loop);

      for (gsi_orig = gsi_start_phis (loop->header),
	   gsi_new = gsi_start_phis (new_loop->header);
	   !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_new);
	   gsi_next (&gsi_orig), gsi_next (&gsi_new))
	{
	  gphi *orig_phi = gsi_orig.phi ();
	  gphi *new_phi = gsi_new.phi ();
	  tree orig_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, orig_e);
	  location_t orig_locus
	    = gimple_phi_arg_location_from_edge (orig_phi, orig_e);

	  add_phi_arg (new_phi, orig_arg, new_e, orig_locus);
	}
    }

  free (new_bbs);
  free (bbs);

  checking_verify_dominators (CDI_DOMINATORS);

  return new_loop;
}


/* Given the condition expression COND, put it as the last statement of
   GUARD_BB; set both edges' probability; set dominator of GUARD_TO to
   DOM_BB; return the skip edge.  GUARD_TO is the target basic block to
   skip the loop.  PROBABILITY is the skip edge's probability.  Mark the
   new edge as irreducible if IRREDUCIBLE_P is true.  */

static edge
slpeel_add_loop_guard (basic_block guard_bb, tree cond,
		       basic_block guard_to, basic_block dom_bb,
		       profile_probability probability, bool irreducible_p)
{
  gimple_stmt_iterator gsi;
  edge new_e, enter_e;
  gcond *cond_stmt;
  gimple_seq gimplify_stmt_list = NULL;

  enter_e = EDGE_SUCC (guard_bb, 0);
  enter_e->flags &= ~EDGE_FALLTHRU;
  enter_e->flags |= EDGE_FALSE_VALUE;
  gsi = gsi_last_bb (guard_bb);

  cond = force_gimple_operand_1 (cond, &gimplify_stmt_list, is_gimple_condexpr,
				 NULL_TREE);
  if (gimplify_stmt_list)
    gsi_insert_seq_after (&gsi, gimplify_stmt_list, GSI_NEW_STMT);

  cond_stmt = gimple_build_cond_from_tree (cond, NULL_TREE, NULL_TREE);
  gsi = gsi_last_bb (guard_bb);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);

  /* Add new edge to connect guard block to the merge/loop-exit block.  */
  new_e = make_edge (guard_bb, guard_to, EDGE_TRUE_VALUE);

  new_e->probability = probability;
  if (irreducible_p)
    new_e->flags |= EDGE_IRREDUCIBLE_LOOP;

  enter_e->probability = probability.invert ();
  set_immediate_dominator (CDI_DOMINATORS, guard_to, dom_bb);

  /* Split enter_e to preserve LOOPS_HAVE_PREHEADERS.  */
  if (enter_e->dest->loop_father->header == enter_e->dest)
    split_edge (enter_e);

  return new_e;
}


/* This function verifies that the following restrictions apply to LOOP:
   (1) it consists of exactly 2 basic blocks - header, and an empty latch
       for innermost loop and 5 basic blocks for outer-loop.
   (2) it is single entry, single exit
   (3) its exit condition is the last stmt in the header
   (4) E is the entry/exit edge of LOOP.
 */

bool
slpeel_can_duplicate_loop_p (const struct loop *loop, const_edge e)
{
  edge exit_e = single_exit (loop);
  edge entry_e = loop_preheader_edge (loop);
  gcond *orig_cond = get_loop_exit_condition (loop);
  gimple_stmt_iterator loop_exit_gsi = gsi_last_bb (exit_e->src);
  unsigned int num_bb = loop->inner? 5 : 2;

  /* All loops have an outer scope; the only case loop->outer is NULL is for
     the function itself.  */
  if (!loop_outer (loop)
      || loop->num_nodes != num_bb
      || !empty_block_p (loop->latch)
      || !single_exit (loop)
      /* Verify that new loop exit condition can be trivially modified.  */
      || (!orig_cond || orig_cond != gsi_stmt (loop_exit_gsi))
      || (e != exit_e && e != entry_e))
    return false;

  return true;
}

/* If the loop has a virtual PHI, but exit bb doesn't, create a virtual PHI
   in the exit bb and rename all the uses after the loop.  This simplifies
   the *guard[12] routines, which assume loop closed SSA form for all PHIs
   (but normally loop closed SSA form doesn't require virtual PHIs to be
   in the same form).  Doing this early simplifies the checking what
   uses should be renamed.  */

static void
create_lcssa_for_virtual_phi (struct loop *loop)
{
  gphi_iterator gsi;
  edge exit_e = single_exit (loop);

  for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    if (virtual_operand_p (gimple_phi_result (gsi_stmt (gsi))))
      {
	gphi *phi = gsi.phi ();
	for (gsi = gsi_start_phis (exit_e->dest);
	     !gsi_end_p (gsi); gsi_next (&gsi))
	  if (virtual_operand_p (gimple_phi_result (gsi_stmt (gsi))))
	    break;
	if (gsi_end_p (gsi))
	  {
	    tree new_vop = copy_ssa_name (PHI_RESULT (phi));
	    gphi *new_phi = create_phi_node (new_vop, exit_e->dest);
	    tree vop = PHI_ARG_DEF_FROM_EDGE (phi, EDGE_SUCC (loop->latch, 0));
	    imm_use_iterator imm_iter;
	    gimple *stmt;
	    use_operand_p use_p;

	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_vop)
	      = SSA_NAME_OCCURS_IN_ABNORMAL_PHI (vop);
	    add_phi_arg (new_phi, vop, exit_e, UNKNOWN_LOCATION);
	    gimple_phi_set_result (new_phi, new_vop);
	    FOR_EACH_IMM_USE_STMT (stmt, imm_iter, vop)
	      if (stmt != new_phi
		  && !flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
		FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		  SET_USE (use_p, new_vop);
	  }
	break;
      }

}

/* Function vect_get_loop_location.

   Extract the location of the loop in the source code.
   If the loop is not well formed for vectorization, an estimated
   location is calculated.
   Return the loop location if succeed and NULL if not.  */

dump_user_location_t
find_loop_location (struct loop *loop)
{
  gimple *stmt = NULL;
  basic_block bb;
  gimple_stmt_iterator si;

  if (!loop)
    return dump_user_location_t ();

  stmt = get_loop_exit_condition (loop);

  if (stmt
      && LOCATION_LOCUS (gimple_location (stmt)) > BUILTINS_LOCATION)
    return stmt;

  /* If we got here the loop is probably not "well formed",
     try to estimate the loop location */

  if (!loop->header)
    return dump_user_location_t ();

  bb = loop->header;

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      stmt = gsi_stmt (si);
      if (LOCATION_LOCUS (gimple_location (stmt)) > BUILTINS_LOCATION)
        return stmt;
    }

  return dump_user_location_t ();
}

/* Return true if the phi described by STMT_INFO defines an IV of the
   loop to be vectorized.  */

static bool
iv_phi_p (stmt_vec_info stmt_info)
{
  gphi *phi = as_a <gphi *> (stmt_info->stmt);
  if (virtual_operand_p (PHI_RESULT (phi)))
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def
      || STMT_VINFO_DEF_TYPE (stmt_info) == vect_double_reduction_def)
    return false;

  return true;
}

/* Function vect_can_advance_ivs_p

   In case the number of iterations that LOOP iterates is unknown at compile
   time, an epilog loop will be generated, and the loop induction variables
   (IVs) will be "advanced" to the value they are supposed to take just before
   the epilog loop.  Here we check that the access function of the loop IVs
   and the expression that represents the loop bound are simple enough.
   These restrictions will be relaxed in the future.  */

bool
vect_can_advance_ivs_p (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block bb = loop->header;
  gphi_iterator gsi;

  /* Analyze phi functions of the loop header.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "vect_can_advance_ivs_p:\n");
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree evolution_part;

      gphi *phi = gsi.phi ();
      stmt_vec_info phi_info = loop_vinfo->lookup_stmt (phi);
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi_info->stmt, 0);
	}

      /* Skip virtual phi's. The data dependences that are associated with
	 virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.

	 Skip reduction phis.  */
      if (!iv_phi_p (phi_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "reduc or virtual phi. skip.\n");
	  continue;
	}

      /* Analyze the evolution function.  */

      evolution_part = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (phi_info);
      if (evolution_part == NULL_TREE)
        {
	  if (dump_enabled_p ())
	    dump_printf (MSG_MISSED_OPTIMIZATION,
			 "No access function or evolution.\n");
	  return false;
        }

      /* FORNOW: We do not transform initial conditions of IVs
	 which evolution functions are not invariants in the loop.  */

      if (!expr_invariant_in_loop_p (loop, evolution_part))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "evolution not invariant in loop.\n");
	  return false;
	}

      /* FORNOW: We do not transform initial conditions of IVs
	 which evolution functions are a polynomial of degree >= 2.  */

      if (tree_is_chrec (evolution_part))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "evolution is chrec.\n");
	  return false;
	}
    }

  return true;
}


/*   Function vect_update_ivs_after_vectorizer.

     "Advance" the induction variables of LOOP to the value they should take
     after the execution of LOOP.  This is currently necessary because the
     vectorizer does not handle induction variables that are used after the
     loop.  Such a situation occurs when the last iterations of LOOP are
     peeled, because:
     1. We introduced new uses after LOOP for IVs that were not originally used
        after LOOP: the IVs of LOOP are now used by an epilog loop.
     2. LOOP is going to be vectorized; this means that it will iterate N/VF
        times, whereas the loop IVs should be bumped N times.

     Input:
     - LOOP - a loop that is going to be vectorized. The last few iterations
              of LOOP were peeled.
     - NITERS - the number of iterations that LOOP executes (before it is
                vectorized). i.e, the number of times the ivs should be bumped.
     - UPDATE_E - a successor edge of LOOP->exit that is on the (only) path
                  coming out from LOOP on which there are uses of the LOOP ivs
		  (this is the path from LOOP->exit to epilog_loop->preheader).

                  The new definitions of the ivs are placed in LOOP->exit.
                  The phi args associated with the edge UPDATE_E in the bb
                  UPDATE_E->dest are updated accordingly.

     Assumption 1: Like the rest of the vectorizer, this function assumes
     a single loop exit that has a single predecessor.

     Assumption 2: The phi nodes in the LOOP header and in update_bb are
     organized in the same order.

     Assumption 3: The access function of the ivs is simple enough (see
     vect_can_advance_ivs_p).  This assumption will be relaxed in the future.

     Assumption 4: Exactly one of the successors of LOOP exit-bb is on a path
     coming out of LOOP on which the ivs of LOOP are used (this is the path
     that leads to the epilog loop; other paths skip the epilog loop).  This
     path starts with the edge UPDATE_E, and its destination (denoted update_bb)
     needs to have its phis updated.
 */

static void
vect_update_ivs_after_vectorizer (loop_vec_info loop_vinfo,
				  tree niters, edge update_e)
{
  gphi_iterator gsi, gsi1;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block update_bb = update_e->dest;
  basic_block exit_bb = single_exit (loop)->dest;

  /* Make sure there exists a single-predecessor exit bb:  */
  gcc_assert (single_pred_p (exit_bb));
  gcc_assert (single_succ_edge (exit_bb) == update_e);

  for (gsi = gsi_start_phis (loop->header), gsi1 = gsi_start_phis (update_bb);
       !gsi_end_p (gsi) && !gsi_end_p (gsi1);
       gsi_next (&gsi), gsi_next (&gsi1))
    {
      tree init_expr;
      tree step_expr, off;
      tree type;
      tree var, ni, ni_name;
      gimple_stmt_iterator last_gsi;

      gphi *phi = gsi.phi ();
      gphi *phi1 = gsi1.phi ();
      stmt_vec_info phi_info = loop_vinfo->lookup_stmt (phi);
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "vect_update_ivs_after_vectorizer: phi: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	}

      /* Skip reduction and virtual phis.  */
      if (!iv_phi_p (phi_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "reduc or virtual phi. skip.\n");
	  continue;
	}

      type = TREE_TYPE (gimple_phi_result (phi));
      step_expr = STMT_VINFO_LOOP_PHI_EVOLUTION_PART (phi_info);
      step_expr = unshare_expr (step_expr);

      /* FORNOW: We do not support IVs whose evolution function is a polynomial
         of degree >= 2 or exponential.  */
      gcc_assert (!tree_is_chrec (step_expr));

      init_expr = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));

      off = fold_build2 (MULT_EXPR, TREE_TYPE (step_expr),
			 fold_convert (TREE_TYPE (step_expr), niters),
			 step_expr);
      if (POINTER_TYPE_P (type))
	ni = fold_build_pointer_plus (init_expr, off);
      else
	ni = fold_build2 (PLUS_EXPR, type,
			  init_expr, fold_convert (type, off));

      var = create_tmp_var (type, "tmp");

      last_gsi = gsi_last_bb (exit_bb);
      gimple_seq new_stmts = NULL;
      ni_name = force_gimple_operand (ni, &new_stmts, false, var);
      /* Exit_bb shouldn't be empty.  */
      if (!gsi_end_p (last_gsi))
	gsi_insert_seq_after (&last_gsi, new_stmts, GSI_SAME_STMT);
      else
	gsi_insert_seq_before (&last_gsi, new_stmts, GSI_SAME_STMT);

      /* Fix phi expressions in the successor bb.  */
      adjust_phi_and_debug_stmts (phi1, update_e, ni_name);
    }
}

/* Return a gimple value containing the misalignment (measured in vector
   elements) for the loop described by LOOP_VINFO, i.e. how many elements
   it is away from a perfectly aligned address.  Add any new statements
   to SEQ.  */

static tree
get_misalign_in_elems (gimple **seq, loop_vec_info loop_vinfo)
{
  dr_vec_info *dr_info = LOOP_VINFO_UNALIGNED_DR (loop_vinfo);
  stmt_vec_info stmt_info = dr_info->stmt;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

  unsigned int target_align = DR_TARGET_ALIGNMENT (dr_info);
  gcc_assert (target_align != 0);

  bool negative = tree_int_cst_compare (DR_STEP (dr_info->dr),
					size_zero_node) < 0;
  tree offset = (negative
		 ? size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1)
		 : size_zero_node);
  tree start_addr = vect_create_addr_base_for_vector_ref (stmt_info, seq,
							  offset);
  tree type = unsigned_type_for (TREE_TYPE (start_addr));
  tree target_align_minus_1 = build_int_cst (type, target_align - 1);
  HOST_WIDE_INT elem_size
    = int_cst_value (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
  tree elem_size_log = build_int_cst (type, exact_log2 (elem_size));

  /* Create:  misalign_in_bytes = addr & (target_align - 1).  */
  tree int_start_addr = fold_convert (type, start_addr);
  tree misalign_in_bytes = fold_build2 (BIT_AND_EXPR, type, int_start_addr,
					target_align_minus_1);

  /* Create:  misalign_in_elems = misalign_in_bytes / element_size.  */
  tree misalign_in_elems = fold_build2 (RSHIFT_EXPR, type, misalign_in_bytes,
					elem_size_log);

  return misalign_in_elems;
}

/* Function vect_gen_prolog_loop_niters

   Generate the number of iterations which should be peeled as prolog for the
   loop represented by LOOP_VINFO.  It is calculated as the misalignment of
   DR - the data reference recorded in LOOP_VINFO_UNALIGNED_DR (LOOP_VINFO).
   As a result, after the execution of this loop, the data reference DR will
   refer to an aligned location.  The following computation is generated:

   If the misalignment of DR is known at compile time:
     addr_mis = int mis = DR_MISALIGNMENT (dr);
   Else, compute address misalignment in bytes:
     addr_mis = addr & (target_align - 1)

   prolog_niters = ((VF - addr_mis/elem_size)&(VF-1))/step

   (elem_size = element type size; an element is the scalar element whose type
   is the inner type of the vectype)

   The computations will be emitted at the end of BB.  We also compute and
   store upper bound (included) of the result in BOUND.

   When the step of the data-ref in the loop is not 1 (as in interleaved data
   and SLP), the number of iterations of the prolog must be divided by the step
   (which is equal to the size of interleaved group).

   The above formulas assume that VF == number of elements in the vector. This
   may not hold when there are multiple-types in the loop.
   In this case, for some data-references in the loop the VF does not represent
   the number of elements that fit in the vector.  Therefore, instead of VF we
   use TYPE_VECTOR_SUBPARTS.  */

static tree
vect_gen_prolog_loop_niters (loop_vec_info loop_vinfo,
			     basic_block bb, int *bound)
{
  dr_vec_info *dr_info = LOOP_VINFO_UNALIGNED_DR (loop_vinfo);
  tree var;
  tree niters_type = TREE_TYPE (LOOP_VINFO_NITERS (loop_vinfo));
  gimple_seq stmts = NULL, new_stmts = NULL;
  tree iters, iters_name;
  stmt_vec_info stmt_info = dr_info->stmt;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  unsigned int target_align = DR_TARGET_ALIGNMENT (dr_info);

  if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) > 0)
    {
      int npeel = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "known peeling = %d.\n", npeel);

      iters = build_int_cst (niters_type, npeel);
      *bound = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);
    }
  else
    {
      tree misalign_in_elems = get_misalign_in_elems (&stmts, loop_vinfo);
      tree type = TREE_TYPE (misalign_in_elems);
      HOST_WIDE_INT elem_size
	= int_cst_value (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
      HOST_WIDE_INT align_in_elems = target_align / elem_size;
      tree align_in_elems_minus_1 = build_int_cst (type, align_in_elems - 1);
      tree align_in_elems_tree = build_int_cst (type, align_in_elems);

      /* Create:  (niters_type) ((align_in_elems - misalign_in_elems)
				 & (align_in_elems - 1)).  */
      bool negative = tree_int_cst_compare (DR_STEP (dr_info->dr),
					    size_zero_node) < 0;
      if (negative)
	iters = fold_build2 (MINUS_EXPR, type, misalign_in_elems,
			     align_in_elems_tree);
      else
	iters = fold_build2 (MINUS_EXPR, type, align_in_elems_tree,
			     misalign_in_elems);
      iters = fold_build2 (BIT_AND_EXPR, type, iters, align_in_elems_minus_1);
      iters = fold_convert (niters_type, iters);
      *bound = align_in_elems - 1;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "niters for prolog loop: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, iters);
      dump_printf (MSG_NOTE, "\n");
    }

  var = create_tmp_var (niters_type, "prolog_loop_niters");
  iters_name = force_gimple_operand (iters, &new_stmts, false, var);

  if (new_stmts)
    gimple_seq_add_seq (&stmts, new_stmts);
  if (stmts)
    {
      gcc_assert (single_succ_p (bb));
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      if (gsi_end_p (gsi))
	gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
      else
	gsi_insert_seq_after (&gsi, stmts, GSI_SAME_STMT);
    }
  return iters_name;
}


/* Function vect_update_init_of_dr

   If CODE is PLUS, the vector loop starts NITERS iterations after the
   scalar one, otherwise CODE is MINUS and the vector loop starts NITERS
   iterations before the scalar one (using masking to skip inactive
   elements).  This function updates the information recorded in DR to
   account for the difference.  Specifically, it updates the OFFSET
   field of DR.  */

static void
vect_update_init_of_dr (struct data_reference *dr, tree niters, tree_code code)
{
  tree offset = DR_OFFSET (dr);

  niters = fold_build2 (MULT_EXPR, sizetype,
			fold_convert (sizetype, niters),
			fold_convert (sizetype, DR_STEP (dr)));
  offset = fold_build2 (code, sizetype,
			fold_convert (sizetype, offset), niters);
  DR_OFFSET (dr) = offset;
}


/* Function vect_update_inits_of_drs

   Apply vect_update_inits_of_dr to all accesses in LOOP_VINFO.
   CODE and NITERS are as for vect_update_inits_of_dr.  */

static void
vect_update_inits_of_drs (loop_vec_info loop_vinfo, tree niters,
			  tree_code code)
{
  unsigned int i;
  vec<data_reference_p> datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct data_reference *dr;

  DUMP_VECT_SCOPE ("vect_update_inits_of_dr");

  /* Adjust niters to sizetype and insert stmts on loop preheader edge.  */
  if (!types_compatible_p (sizetype, TREE_TYPE (niters)))
    {
      gimple_seq seq;
      edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));
      tree var = create_tmp_var (sizetype, "prolog_loop_adjusted_niters");

      niters = fold_convert (sizetype, niters);
      niters = force_gimple_operand (niters, &seq, false, var);
      if (seq)
	{
	  basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
	  gcc_assert (!new_bb);
	}
    }

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      dr_vec_info *dr_info = loop_vinfo->lookup_dr (dr);
      if (!STMT_VINFO_GATHER_SCATTER_P (dr_info->stmt))
	vect_update_init_of_dr (dr, niters, code);
    }
}

/* For the information recorded in LOOP_VINFO prepare the loop for peeling
   by masking.  This involves calculating the number of iterations to
   be peeled and then aligning all memory references appropriately.  */

void
vect_prepare_for_masked_peels (loop_vec_info loop_vinfo)
{
  tree misalign_in_elems;
  tree type = LOOP_VINFO_MASK_COMPARE_TYPE (loop_vinfo);

  gcc_assert (vect_use_loop_mask_for_alignment_p (loop_vinfo));

  /* From the information recorded in LOOP_VINFO get the number of iterations
     that need to be skipped via masking.  */
  if (LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) > 0)
    {
      poly_int64 misalign = (LOOP_VINFO_VECT_FACTOR (loop_vinfo)
			     - LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo));
      misalign_in_elems = build_int_cst (type, misalign);
    }
  else
    {
      gimple_seq seq1 = NULL, seq2 = NULL;
      misalign_in_elems = get_misalign_in_elems (&seq1, loop_vinfo);
      misalign_in_elems = fold_convert (type, misalign_in_elems);
      misalign_in_elems = force_gimple_operand (misalign_in_elems,
						&seq2, true, NULL_TREE);
      gimple_seq_add_seq (&seq1, seq2);
      if (seq1)
	{
	  edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));
	  basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, seq1);
	  gcc_assert (!new_bb);
	}
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "misalignment for fully-masked loop: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, misalign_in_elems);
      dump_printf (MSG_NOTE, "\n");
    }

  LOOP_VINFO_MASK_SKIP_NITERS (loop_vinfo) = misalign_in_elems;

  vect_update_inits_of_drs (loop_vinfo, misalign_in_elems, MINUS_EXPR);
}

/* This function builds ni_name = number of iterations.  Statements
   are emitted on the loop preheader edge.  If NEW_VAR_P is not NULL, set
   it to TRUE if new ssa_var is generated.  */

tree
vect_build_loop_niters (loop_vec_info loop_vinfo, bool *new_var_p)
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
	{
	  gsi_insert_seq_on_edge_immediate (pe, stmts);
	  if (new_var_p != NULL)
	    *new_var_p = true;
	}

      return ni_name;
    }
}

/* Calculate the number of iterations above which vectorized loop will be
   preferred than scalar loop.  NITERS_PROLOG is the number of iterations
   of prolog loop.  If it's integer const, the integer number is also passed
   in INT_NITERS_PROLOG.  BOUND_PROLOG is the upper bound (inclusive) of the
   number of iterations of the prolog loop.  BOUND_EPILOG is the corresponding
   value for the epilog loop.  If CHECK_PROFITABILITY is true, TH is the
   threshold below which the scalar (rather than vectorized) loop will be
   executed.  This function stores the upper bound (inclusive) of the result
   in BOUND_SCALAR.  */

static tree
vect_gen_scalar_loop_niters (tree niters_prolog, int int_niters_prolog,
			     int bound_prolog, poly_int64 bound_epilog, int th,
			     poly_uint64 *bound_scalar,
			     bool check_profitability)
{
  tree type = TREE_TYPE (niters_prolog);
  tree niters = fold_build2 (PLUS_EXPR, type, niters_prolog,
			     build_int_cst (type, bound_epilog));

  *bound_scalar = bound_prolog + bound_epilog;
  if (check_profitability)
    {
      /* TH indicates the minimum niters of vectorized loop, while we
	 compute the maximum niters of scalar loop.  */
      th--;
      /* Peeling for constant times.  */
      if (int_niters_prolog >= 0)
	{
	  *bound_scalar = upper_bound (int_niters_prolog + bound_epilog, th);
	  return build_int_cst (type, *bound_scalar);
	}
      /* Peeling an unknown number of times.  Note that both BOUND_PROLOG
	 and BOUND_EPILOG are inclusive upper bounds.  */
      if (known_ge (th, bound_prolog + bound_epilog))
	{
	  *bound_scalar = th;
	  return build_int_cst (type, th);
	}
      /* Need to do runtime comparison.  */
      else if (maybe_gt (th, bound_epilog))
	{
	  *bound_scalar = upper_bound (*bound_scalar, th);
	  return fold_build2 (MAX_EXPR, type,
			      build_int_cst (type, th), niters);
	}
    }
  return niters;
}

/* NITERS is the number of times that the original scalar loop executes
   after peeling.  Work out the maximum number of iterations N that can
   be handled by the vectorized form of the loop and then either:

   a) set *STEP_VECTOR_PTR to the vectorization factor and generate:

	niters_vector = N

   b) set *STEP_VECTOR_PTR to one and generate:

        niters_vector = N / vf

   In both cases, store niters_vector in *NITERS_VECTOR_PTR and add
   any new statements on the loop preheader edge.  NITERS_NO_OVERFLOW
   is true if NITERS doesn't overflow (i.e. if NITERS is always nonzero).  */

void
vect_gen_vector_loop_niters (loop_vec_info loop_vinfo, tree niters,
			     tree *niters_vector_ptr, tree *step_vector_ptr,
			     bool niters_no_overflow)
{
  tree ni_minus_gap, var;
  tree niters_vector, step_vector, type = TREE_TYPE (niters);
  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  edge pe = loop_preheader_edge (LOOP_VINFO_LOOP (loop_vinfo));
  tree log_vf = NULL_TREE;

  /* If epilogue loop is required because of data accesses with gaps, we
     subtract one iteration from the total number of iterations here for
     correct calculation of RATIO.  */
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    {
      ni_minus_gap = fold_build2 (MINUS_EXPR, type, niters,
				  build_one_cst (type));
      if (!is_gimple_val (ni_minus_gap))
	{
	  var = create_tmp_var (type, "ni_gap");
	  gimple *stmts = NULL;
	  ni_minus_gap = force_gimple_operand (ni_minus_gap, &stmts,
					       true, var);
	  gsi_insert_seq_on_edge_immediate (pe, stmts);
	}
    }
  else
    ni_minus_gap = niters;

  unsigned HOST_WIDE_INT const_vf;
  if (vf.is_constant (&const_vf)
      && !LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      /* Create: niters >> log2(vf) */
      /* If it's known that niters == number of latch executions + 1 doesn't
	 overflow, we can generate niters >> log2(vf); otherwise we generate
	 (niters - vf) >> log2(vf) + 1 by using the fact that we know ratio
	 will be at least one.  */
      log_vf = build_int_cst (type, exact_log2 (const_vf));
      if (niters_no_overflow)
	niters_vector = fold_build2 (RSHIFT_EXPR, type, ni_minus_gap, log_vf);
      else
	niters_vector
	  = fold_build2 (PLUS_EXPR, type,
			 fold_build2 (RSHIFT_EXPR, type,
				      fold_build2 (MINUS_EXPR, type,
						   ni_minus_gap,
						   build_int_cst (type, vf)),
				      log_vf),
			 build_int_cst (type, 1));
      step_vector = build_one_cst (type);
    }
  else
    {
      niters_vector = ni_minus_gap;
      step_vector = build_int_cst (type, vf);
    }

  if (!is_gimple_val (niters_vector))
    {
      var = create_tmp_var (type, "bnd");
      gimple_seq stmts = NULL;
      niters_vector = force_gimple_operand (niters_vector, &stmts, true, var);
      gsi_insert_seq_on_edge_immediate (pe, stmts);
      /* Peeling algorithm guarantees that vector loop bound is at least ONE,
	 we set range information to make niters analyzer's life easier.  */
      if (stmts != NULL && log_vf)
	set_range_info (niters_vector, VR_RANGE,
			wi::to_wide (build_int_cst (type, 1)),
			wi::to_wide (fold_build2 (RSHIFT_EXPR, type,
						  TYPE_MAX_VALUE (type),
						  log_vf)));
    }
  *niters_vector_ptr = niters_vector;
  *step_vector_ptr = step_vector;

  return;
}

/* Given NITERS_VECTOR which is the number of iterations for vectorized
   loop specified by LOOP_VINFO after vectorization, compute the number
   of iterations before vectorization (niters_vector * vf) and store it
   to NITERS_VECTOR_MULT_VF_PTR.  */

static void
vect_gen_vector_loop_niters_mult_vf (loop_vec_info loop_vinfo,
				     tree niters_vector,
				     tree *niters_vector_mult_vf_ptr)
{
  /* We should be using a step_vector of VF if VF is variable.  */
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo).to_constant ();
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree type = TREE_TYPE (niters_vector);
  tree log_vf = build_int_cst (type, exact_log2 (vf));
  basic_block exit_bb = single_exit (loop)->dest;

  gcc_assert (niters_vector_mult_vf_ptr != NULL);
  tree niters_vector_mult_vf = fold_build2 (LSHIFT_EXPR, type,
					    niters_vector, log_vf);
  if (!is_gimple_val (niters_vector_mult_vf))
    {
      tree var = create_tmp_var (type, "niters_vector_mult_vf");
      gimple_seq stmts = NULL;
      niters_vector_mult_vf = force_gimple_operand (niters_vector_mult_vf,
						    &stmts, true, var);
      gimple_stmt_iterator gsi = gsi_start_bb (exit_bb);
      gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
    }
  *niters_vector_mult_vf_ptr = niters_vector_mult_vf;
}

/* Function slpeel_tree_duplicate_loop_to_edge_cfg duplciates FIRST/SECOND
   from SECOND/FIRST and puts it at the original loop's preheader/exit
   edge, the two loops are arranged as below:

       preheader_a:
     first_loop:
       header_a:
	 i_1 = PHI<i_0, i_2>;
	 ...
	 i_2 = i_1 + 1;
	 if (cond_a)
	   goto latch_a;
	 else
	   goto between_bb;
       latch_a:
	 goto header_a;

       between_bb:
	 ;; i_x = PHI<i_2>;   ;; LCSSA phi node to be created for FIRST,

     second_loop:
       header_b:
	 i_3 = PHI<i_0, i_4>; ;; Use of i_0 to be replaced with i_x,
				 or with i_2 if no LCSSA phi is created
				 under condition of CREATE_LCSSA_FOR_IV_PHIS.
	 ...
	 i_4 = i_3 + 1;
	 if (cond_b)
	   goto latch_b;
	 else
	   goto exit_bb;
       latch_b:
	 goto header_b;

       exit_bb:

   This function creates loop closed SSA for the first loop; update the
   second loop's PHI nodes by replacing argument on incoming edge with the
   result of newly created lcssa PHI nodes.  IF CREATE_LCSSA_FOR_IV_PHIS
   is false, Loop closed ssa phis will only be created for non-iv phis for
   the first loop.

   This function assumes exit bb of the first loop is preheader bb of the
   second loop, i.e, between_bb in the example code.  With PHIs updated,
   the second loop will execute rest iterations of the first.  */

static void
slpeel_update_phi_nodes_for_loops (loop_vec_info loop_vinfo,
				   struct loop *first, struct loop *second,
				   bool create_lcssa_for_iv_phis)
{
  gphi_iterator gsi_update, gsi_orig;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  edge first_latch_e = EDGE_SUCC (first->latch, 0);
  edge second_preheader_e = loop_preheader_edge (second);
  basic_block between_bb = single_exit (first)->dest;

  gcc_assert (between_bb == second_preheader_e->src);
  gcc_assert (single_pred_p (between_bb) && single_succ_p (between_bb));
  /* Either the first loop or the second is the loop to be vectorized.  */
  gcc_assert (loop == first || loop == second);

  for (gsi_orig = gsi_start_phis (first->header),
       gsi_update = gsi_start_phis (second->header);
       !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_update);
       gsi_next (&gsi_orig), gsi_next (&gsi_update))
    {
      gphi *orig_phi = gsi_orig.phi ();
      gphi *update_phi = gsi_update.phi ();

      tree arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, first_latch_e);
      /* Generate lcssa PHI node for the first loop.  */
      gphi *vect_phi = (loop == first) ? orig_phi : update_phi;
      stmt_vec_info vect_phi_info = loop_vinfo->lookup_stmt (vect_phi);
      if (create_lcssa_for_iv_phis || !iv_phi_p (vect_phi_info))
	{
	  tree new_res = copy_ssa_name (PHI_RESULT (orig_phi));
	  gphi *lcssa_phi = create_phi_node (new_res, between_bb);
	  add_phi_arg (lcssa_phi, arg, single_exit (first), UNKNOWN_LOCATION);
	  arg = new_res;
	}

      /* Update PHI node in the second loop by replacing arg on the loop's
	 incoming edge.  */
      adjust_phi_and_debug_stmts (update_phi, second_preheader_e, arg);
    }
}

/* Function slpeel_add_loop_guard adds guard skipping from the beginning
   of SKIP_LOOP to the beginning of UPDATE_LOOP.  GUARD_EDGE and MERGE_EDGE
   are two pred edges of the merge point before UPDATE_LOOP.  The two loops
   appear like below:

       guard_bb:
	 if (cond)
	   goto merge_bb;
	 else
	   goto skip_loop;

     skip_loop:
       header_a:
	 i_1 = PHI<i_0, i_2>;
	 ...
	 i_2 = i_1 + 1;
	 if (cond_a)
	   goto latch_a;
	 else
	   goto exit_a;
       latch_a:
	 goto header_a;

       exit_a:
	 i_5 = PHI<i_2>;

       merge_bb:
	 ;; PHI (i_x = PHI<i_0, i_5>) to be created at merge point.

     update_loop:
       header_b:
	 i_3 = PHI<i_5, i_4>;  ;; Use of i_5 to be replaced with i_x.
	 ...
	 i_4 = i_3 + 1;
	 if (cond_b)
	   goto latch_b;
	 else
	   goto exit_bb;
       latch_b:
	 goto header_b;

       exit_bb:

   This function creates PHI nodes at merge_bb and replaces the use of i_5
   in the update_loop's PHI node with the result of new PHI result.  */

static void
slpeel_update_phi_nodes_for_guard1 (struct loop *skip_loop,
				    struct loop *update_loop,
				    edge guard_edge, edge merge_edge)
{
  source_location merge_loc, guard_loc;
  edge orig_e = loop_preheader_edge (skip_loop);
  edge update_e = loop_preheader_edge (update_loop);
  gphi_iterator gsi_orig, gsi_update;

  for ((gsi_orig = gsi_start_phis (skip_loop->header),
	gsi_update = gsi_start_phis (update_loop->header));
       !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_update);
       gsi_next (&gsi_orig), gsi_next (&gsi_update))
    {
      gphi *orig_phi = gsi_orig.phi ();
      gphi *update_phi = gsi_update.phi ();

      /* Generate new phi node at merge bb of the guard.  */
      tree new_res = copy_ssa_name (PHI_RESULT (orig_phi));
      gphi *new_phi = create_phi_node (new_res, guard_edge->dest);

      /* Merge bb has two incoming edges: GUARD_EDGE and MERGE_EDGE.  Set the
	 args in NEW_PHI for these edges.  */
      tree merge_arg = PHI_ARG_DEF_FROM_EDGE (update_phi, update_e);
      tree guard_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, orig_e);
      merge_loc = gimple_phi_arg_location_from_edge (update_phi, update_e);
      guard_loc = gimple_phi_arg_location_from_edge (orig_phi, orig_e);
      add_phi_arg (new_phi, merge_arg, merge_edge, merge_loc);
      add_phi_arg (new_phi, guard_arg, guard_edge, guard_loc);

      /* Update phi in UPDATE_PHI.  */
      adjust_phi_and_debug_stmts (update_phi, update_e, new_res);
    }
}

/* LCSSA_PHI is a lcssa phi of EPILOG loop which is copied from LOOP,
   this function searches for the corresponding lcssa phi node in exit
   bb of LOOP.  If it is found, return the phi result; otherwise return
   NULL.  */

static tree
find_guard_arg (struct loop *loop, struct loop *epilog ATTRIBUTE_UNUSED,
		gphi *lcssa_phi)
{
  gphi_iterator gsi;
  edge e = single_exit (loop);

  gcc_assert (single_pred_p (e->dest));
  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (operand_equal_p (PHI_ARG_DEF (phi, 0),
			   PHI_ARG_DEF (lcssa_phi, 0), 0))
	return PHI_RESULT (phi);
    }
  return NULL_TREE;
}

/* LOOP and EPILOG are two consecutive loops in CFG and EPILOG is copied
   from LOOP.  Function slpeel_add_loop_guard adds guard skipping from a
   point between the two loops to the end of EPILOG.  Edges GUARD_EDGE
   and MERGE_EDGE are the two pred edges of merge_bb at the end of EPILOG.
   The CFG looks like:

     loop:
       header_a:
	 i_1 = PHI<i_0, i_2>;
	 ...
	 i_2 = i_1 + 1;
	 if (cond_a)
	   goto latch_a;
	 else
	   goto exit_a;
       latch_a:
	 goto header_a;

       exit_a:

       guard_bb:
	 if (cond)
	   goto merge_bb;
	 else
	   goto epilog_loop;

       ;; fall_through_bb

     epilog_loop:
       header_b:
	 i_3 = PHI<i_2, i_4>;
	 ...
	 i_4 = i_3 + 1;
	 if (cond_b)
	   goto latch_b;
	 else
	   goto merge_bb;
       latch_b:
	 goto header_b;

       merge_bb:
	 ; PHI node (i_y = PHI<i_2, i_4>) to be created at merge point.

       exit_bb:
	 i_x = PHI<i_4>;  ;Use of i_4 to be replaced with i_y in merge_bb.

   For each name used out side EPILOG (i.e - for each name that has a lcssa
   phi in exit_bb) we create a new PHI in merge_bb.  The new PHI has two
   args corresponding to GUARD_EDGE and MERGE_EDGE.  Arg for MERGE_EDGE is
   the arg of the original PHI in exit_bb, arg for GUARD_EDGE is defined
   by LOOP and is found in the exit bb of LOOP.  Arg of the original PHI
   in exit_bb will also be updated.  */

static void
slpeel_update_phi_nodes_for_guard2 (struct loop *loop, struct loop *epilog,
				    edge guard_edge, edge merge_edge)
{
  gphi_iterator gsi;
  basic_block merge_bb = guard_edge->dest;

  gcc_assert (single_succ_p (merge_bb));
  edge e = single_succ_edge (merge_bb);
  basic_block exit_bb = e->dest;
  gcc_assert (single_pred_p (exit_bb));
  gcc_assert (single_pred (exit_bb) == single_exit (epilog)->dest);

  for (gsi = gsi_start_phis (exit_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *update_phi = gsi.phi ();
      tree old_arg = PHI_ARG_DEF (update_phi, 0);
      /* This loop-closed-phi actually doesn't represent a use out of the
	 loop - the phi arg is a constant.  */
      if (TREE_CODE (old_arg) != SSA_NAME)
	continue;

      tree merge_arg = get_current_def (old_arg);
      if (!merge_arg)
	merge_arg = old_arg;

      tree guard_arg = find_guard_arg (loop, epilog, update_phi);
      /* If the var is live after loop but not a reduction, we simply
	 use the old arg.  */
      if (!guard_arg)
	guard_arg = old_arg;

      /* Create new phi node in MERGE_BB:  */
      tree new_res = copy_ssa_name (PHI_RESULT (update_phi));
      gphi *merge_phi = create_phi_node (new_res, merge_bb);

      /* MERGE_BB has two incoming edges: GUARD_EDGE and MERGE_EDGE, Set
	 the two PHI args in merge_phi for these edges.  */
      add_phi_arg (merge_phi, merge_arg, merge_edge, UNKNOWN_LOCATION);
      add_phi_arg (merge_phi, guard_arg, guard_edge, UNKNOWN_LOCATION);

      /* Update the original phi in exit_bb.  */
      adjust_phi_and_debug_stmts (update_phi, e, new_res);
    }
}

/* EPILOG loop is duplicated from the original loop for vectorizing,
   the arg of its loop closed ssa PHI needs to be updated.  */

static void
slpeel_update_phi_nodes_for_lcssa (struct loop *epilog)
{
  gphi_iterator gsi;
  basic_block exit_bb = single_exit (epilog)->dest;

  gcc_assert (single_pred_p (exit_bb));
  edge e = EDGE_PRED (exit_bb, 0);
  for (gsi = gsi_start_phis (exit_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    rename_use_op (PHI_ARG_DEF_PTR_FROM_EDGE (gsi.phi (), e));
}

/* Function vect_do_peeling.

   Input:
   - LOOP_VINFO: Represent a loop to be vectorized, which looks like:

       preheader:
     LOOP:
       header_bb:
	 loop_body
	 if (exit_loop_cond) goto exit_bb
	 else                goto header_bb
       exit_bb:

   - NITERS: The number of iterations of the loop.
   - NITERSM1: The number of iterations of the loop's latch.
   - NITERS_NO_OVERFLOW: No overflow in computing NITERS.
   - TH, CHECK_PROFITABILITY: Threshold of niters to vectorize loop if
			      CHECK_PROFITABILITY is true.
   Output:
   - *NITERS_VECTOR and *STEP_VECTOR describe how the main loop should
     iterate after vectorization; see vect_set_loop_condition for details.
   - *NITERS_VECTOR_MULT_VF_VAR is either null or an SSA name that
     should be set to the number of scalar iterations handled by the
     vector loop.  The SSA name is only used on exit from the loop.

   This function peels prolog and epilog from the loop, adds guards skipping
   PROLOG and EPILOG for various conditions.  As a result, the changed CFG
   would look like:

       guard_bb_1:
	 if (prefer_scalar_loop) goto merge_bb_1
	 else                    goto guard_bb_2

       guard_bb_2:
         if (skip_prolog) goto merge_bb_2
         else             goto prolog_preheader

       prolog_preheader:
     PROLOG:
       prolog_header_bb:
	 prolog_body
	 if (exit_prolog_cond) goto prolog_exit_bb
	 else                  goto prolog_header_bb
       prolog_exit_bb:

       merge_bb_2:

       vector_preheader:
     VECTOR LOOP:
       vector_header_bb:
	 vector_body
	 if (exit_vector_cond) goto vector_exit_bb
	 else                  goto vector_header_bb
       vector_exit_bb:

       guard_bb_3:
	 if (skip_epilog) goto merge_bb_3
	 else             goto epilog_preheader

       merge_bb_1:

       epilog_preheader:
     EPILOG:
       epilog_header_bb:
	 epilog_body
	 if (exit_epilog_cond) goto merge_bb_3
	 else                  goto epilog_header_bb

       merge_bb_3:

   Note this function peels prolog and epilog only if it's necessary,
   as well as guards.
   Returns created epilogue or NULL.

   TODO: Guard for prefer_scalar_loop should be emitted along with
   versioning conditions if loop versioning is needed.  */


struct loop *
vect_do_peeling (loop_vec_info loop_vinfo, tree niters, tree nitersm1,
		 tree *niters_vector, tree *step_vector,
		 tree *niters_vector_mult_vf_var, int th,
		 bool check_profitability, bool niters_no_overflow)
{
  edge e, guard_e;
  tree type = TREE_TYPE (niters), guard_cond;
  basic_block guard_bb, guard_to;
  profile_probability prob_prolog, prob_vector, prob_epilog;
  int estimated_vf;
  int prolog_peeling = 0;
  if (!vect_use_loop_mask_for_alignment_p (loop_vinfo))
    prolog_peeling = LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo);

  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  poly_uint64 bound_epilog = 0;
  if (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
      && LOOP_VINFO_PEELING_FOR_NITER (loop_vinfo))
    bound_epilog += vf - 1;
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    bound_epilog += 1;
  bool epilog_peeling = maybe_ne (bound_epilog, 0U);
  poly_uint64 bound_scalar = bound_epilog;

  if (!prolog_peeling && !epilog_peeling)
    return NULL;

  prob_vector = profile_probability::guessed_always ().apply_scale (9, 10);
  estimated_vf = vect_vf_for_cost (loop_vinfo);
  if (estimated_vf == 2)
    estimated_vf = 3;
  prob_prolog = prob_epilog = profile_probability::guessed_always ()
			.apply_scale (estimated_vf - 1, estimated_vf);

  struct loop *prolog, *epilog = NULL, *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct loop *first_loop = loop;
  bool irred_flag = loop_preheader_edge (loop)->flags & EDGE_IRREDUCIBLE_LOOP;
  create_lcssa_for_virtual_phi (loop);
  update_ssa (TODO_update_ssa_only_virtuals);

  if (MAY_HAVE_DEBUG_BIND_STMTS)
    {
      gcc_assert (!adjust_vec.exists ());
      adjust_vec.create (32);
    }
  initialize_original_copy_tables ();

  /* Record the anchor bb at which the guard should be placed if the scalar
     loop might be preferred.  */
  basic_block anchor = loop_preheader_edge (loop)->src;

  /* Generate the number of iterations for the prolog loop.  We do this here
     so that we can also get the upper bound on the number of iterations.  */
  tree niters_prolog;
  int bound_prolog = 0;
  if (prolog_peeling)
    niters_prolog = vect_gen_prolog_loop_niters (loop_vinfo, anchor,
						 &bound_prolog);
  else
    niters_prolog = build_int_cst (type, 0);

  /* Prolog loop may be skipped.  */
  bool skip_prolog = (prolog_peeling != 0);
  /* Skip to epilog if scalar loop may be preferred.  It's only needed
     when we peel for epilog loop and when it hasn't been checked with
     loop versioning.  */
  bool skip_vector = (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
		      ? maybe_lt (LOOP_VINFO_INT_NITERS (loop_vinfo),
				  bound_prolog + bound_epilog)
		      : !LOOP_REQUIRES_VERSIONING (loop_vinfo));
  /* Epilog loop must be executed if the number of iterations for epilog
     loop is known at compile time, otherwise we need to add a check at
     the end of vector loop and skip to the end of epilog loop.  */
  bool skip_epilog = (prolog_peeling < 0
		      || !LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
		      || !vf.is_constant ());
  /* PEELING_FOR_GAPS is special because epilog loop must be executed.  */
  if (LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo))
    skip_epilog = false;

  if (skip_vector)
    {
      split_edge (loop_preheader_edge (loop));

      /* Due to the order in which we peel prolog and epilog, we first
	 propagate probability to the whole loop.  The purpose is to
	 avoid adjusting probabilities of both prolog and vector loops
	 separately.  Note in this case, the probability of epilog loop
	 needs to be scaled back later.  */
      basic_block bb_before_loop = loop_preheader_edge (loop)->src;
      if (prob_vector.initialized_p ())
	{
	  scale_bbs_frequencies (&bb_before_loop, 1, prob_vector);
	  scale_loop_profile (loop, prob_vector, 0);
	}
    }

  dump_user_location_t loop_loc = find_loop_location (loop);
  struct loop *scalar_loop = LOOP_VINFO_SCALAR_LOOP (loop_vinfo);
  if (prolog_peeling)
    {
      e = loop_preheader_edge (loop);
      if (!slpeel_can_duplicate_loop_p (loop, e))
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "loop can't be duplicated to preheader edge.\n");
	  gcc_unreachable ();
	}
      /* Peel prolog and put it on preheader edge of loop.  */
      prolog = slpeel_tree_duplicate_loop_to_edge_cfg (loop, scalar_loop, e);
      if (!prolog)
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "slpeel_tree_duplicate_loop_to_edge_cfg failed.\n");
	  gcc_unreachable ();
	}
      slpeel_update_phi_nodes_for_loops (loop_vinfo, prolog, loop, true);
      first_loop = prolog;
      reset_original_copy_tables ();

      /* Update the number of iterations for prolog loop.  */
      tree step_prolog = build_one_cst (TREE_TYPE (niters_prolog));
      vect_set_loop_condition (prolog, NULL, niters_prolog,
			       step_prolog, NULL_TREE, false);

      /* Skip the prolog loop.  */
      if (skip_prolog)
	{
	  guard_cond = fold_build2 (EQ_EXPR, boolean_type_node,
				    niters_prolog, build_int_cst (type, 0));
	  guard_bb = loop_preheader_edge (prolog)->src;
	  basic_block bb_after_prolog = loop_preheader_edge (loop)->src;
	  guard_to = split_edge (loop_preheader_edge (loop));
	  guard_e = slpeel_add_loop_guard (guard_bb, guard_cond,
					   guard_to, guard_bb,
					   prob_prolog.invert (),
					   irred_flag);
	  e = EDGE_PRED (guard_to, 0);
	  e = (e != guard_e ? e : EDGE_PRED (guard_to, 1));
	  slpeel_update_phi_nodes_for_guard1 (prolog, loop, guard_e, e);

	  scale_bbs_frequencies (&bb_after_prolog, 1, prob_prolog);
	  scale_loop_profile (prolog, prob_prolog, bound_prolog);
	}
      /* Update init address of DRs.  */
      vect_update_inits_of_drs (loop_vinfo, niters_prolog, PLUS_EXPR);
      /* Update niters for vector loop.  */
      LOOP_VINFO_NITERS (loop_vinfo)
	= fold_build2 (MINUS_EXPR, type, niters, niters_prolog);
      LOOP_VINFO_NITERSM1 (loop_vinfo)
	= fold_build2 (MINUS_EXPR, type,
		       LOOP_VINFO_NITERSM1 (loop_vinfo), niters_prolog);
      bool new_var_p = false;
      niters = vect_build_loop_niters (loop_vinfo, &new_var_p);
      /* It's guaranteed that vector loop bound before vectorization is at
	 least VF, so set range information for newly generated var.  */
      if (new_var_p)
	set_range_info (niters, VR_RANGE,
			wi::to_wide (build_int_cst (type, vf)),
			wi::to_wide (TYPE_MAX_VALUE (type)));

      /* Prolog iterates at most bound_prolog times, latch iterates at
	 most bound_prolog - 1 times.  */
      record_niter_bound (prolog, bound_prolog - 1, false, true);
      delete_update_ssa ();
      adjust_vec_debug_stmts ();
      scev_reset ();
    }

  if (epilog_peeling)
    {
      e = single_exit (loop);
      if (!slpeel_can_duplicate_loop_p (loop, e))
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "loop can't be duplicated to exit edge.\n");
	  gcc_unreachable ();
	}
      /* Peel epilog and put it on exit edge of loop.  */
      epilog = slpeel_tree_duplicate_loop_to_edge_cfg (loop, scalar_loop, e);
      if (!epilog)
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loop_loc,
			   "slpeel_tree_duplicate_loop_to_edge_cfg failed.\n");
	  gcc_unreachable ();
	}
      slpeel_update_phi_nodes_for_loops (loop_vinfo, loop, epilog, false);

      /* Scalar version loop may be preferred.  In this case, add guard
	 and skip to epilog.  Note this only happens when the number of
	 iterations of loop is unknown at compile time, otherwise this
	 won't be vectorized.  */
      if (skip_vector)
	{
	  /* Additional epilogue iteration is peeled if gap exists.  */
	  tree t = vect_gen_scalar_loop_niters (niters_prolog, prolog_peeling,
						bound_prolog, bound_epilog,
						th, &bound_scalar,
						check_profitability);
	  /* Build guard against NITERSM1 since NITERS may overflow.  */
	  guard_cond = fold_build2 (LT_EXPR, boolean_type_node, nitersm1, t);
	  guard_bb = anchor;
	  guard_to = split_edge (loop_preheader_edge (epilog));
	  guard_e = slpeel_add_loop_guard (guard_bb, guard_cond,
					   guard_to, guard_bb,
					   prob_vector.invert (),
					   irred_flag);
	  e = EDGE_PRED (guard_to, 0);
	  e = (e != guard_e ? e : EDGE_PRED (guard_to, 1));
	  slpeel_update_phi_nodes_for_guard1 (first_loop, epilog, guard_e, e);

	  /* Simply propagate profile info from guard_bb to guard_to which is
	     a merge point of control flow.  */
	  guard_to->count = guard_bb->count;

	  /* Scale probability of epilog loop back.
	     FIXME: We should avoid scaling down and back up.  Profile may
	     get lost if we scale down to 0.  */
	  basic_block *bbs = get_loop_body (epilog);
	  for (unsigned int i = 0; i < epilog->num_nodes; i++)
	    bbs[i]->count = bbs[i]->count.apply_scale
				 (bbs[i]->count,
				  bbs[i]->count.apply_probability
				    (prob_vector));
	  free (bbs);
	}

      basic_block bb_before_epilog = loop_preheader_edge (epilog)->src;
      tree niters_vector_mult_vf;
      /* If loop is peeled for non-zero constant times, now niters refers to
	 orig_niters - prolog_peeling, it won't overflow even the orig_niters
	 overflows.  */
      niters_no_overflow |= (prolog_peeling > 0);
      vect_gen_vector_loop_niters (loop_vinfo, niters,
				   niters_vector, step_vector,
				   niters_no_overflow);
      if (!integer_onep (*step_vector))
	{
	  /* On exit from the loop we will have an easy way of calcalating
	     NITERS_VECTOR / STEP * STEP.  Install a dummy definition
	     until then.  */
	  niters_vector_mult_vf = make_ssa_name (TREE_TYPE (*niters_vector));
	  SSA_NAME_DEF_STMT (niters_vector_mult_vf) = gimple_build_nop ();
	  *niters_vector_mult_vf_var = niters_vector_mult_vf;
	}
      else
	vect_gen_vector_loop_niters_mult_vf (loop_vinfo, *niters_vector,
					     &niters_vector_mult_vf);
      /* Update IVs of original loop as if they were advanced by
	 niters_vector_mult_vf steps.  */
      gcc_checking_assert (vect_can_advance_ivs_p (loop_vinfo));
      edge update_e = skip_vector ? e : loop_preheader_edge (epilog);
      vect_update_ivs_after_vectorizer (loop_vinfo, niters_vector_mult_vf,
					update_e);

      if (skip_epilog)
	{
	  guard_cond = fold_build2 (EQ_EXPR, boolean_type_node,
				    niters, niters_vector_mult_vf);
	  guard_bb = single_exit (loop)->dest;
	  guard_to = split_edge (single_exit (epilog));
	  guard_e = slpeel_add_loop_guard (guard_bb, guard_cond, guard_to,
					   skip_vector ? anchor : guard_bb,
					   prob_epilog.invert (),
					   irred_flag);
	  slpeel_update_phi_nodes_for_guard2 (loop, epilog, guard_e,
					      single_exit (epilog));
	  /* Only need to handle basic block before epilog loop if it's not
	     the guard_bb, which is the case when skip_vector is true.  */
	  if (guard_bb != bb_before_epilog)
	    {
	      prob_epilog = prob_vector * prob_epilog + prob_vector.invert ();

	      scale_bbs_frequencies (&bb_before_epilog, 1, prob_epilog);
	    }
	  scale_loop_profile (epilog, prob_epilog, 0);
	}
      else
	slpeel_update_phi_nodes_for_lcssa (epilog);

      unsigned HOST_WIDE_INT bound;
      if (bound_scalar.is_constant (&bound))
	{
	  gcc_assert (bound != 0);
	  /* -1 to convert loop iterations to latch iterations.  */
	  record_niter_bound (epilog, bound - 1, false, true);
	}

      delete_update_ssa ();
      adjust_vec_debug_stmts ();
      scev_reset ();
    }
  adjust_vec.release ();
  free_original_copy_tables ();

  return epilog;
}

/* Function vect_create_cond_for_niters_checks.

   Create a conditional expression that represents the run-time checks for
   loop's niter.  The loop is guaranteed to terminate if the run-time
   checks hold.

   Input:
   COND_EXPR  - input conditional expression.  New conditions will be chained
		with logical AND operation.  If it is NULL, then the function
		is used to return the number of alias checks.
   LOOP_VINFO - field LOOP_VINFO_MAY_ALIAS_STMTS contains the list of ddrs
		to be checked.

   Output:
   COND_EXPR - conditional expression.

   The returned COND_EXPR is the conditional expression to be used in the
   if statement that controls which version of the loop gets executed at
   runtime.  */

static void
vect_create_cond_for_niters_checks (loop_vec_info loop_vinfo, tree *cond_expr)
{
  tree part_cond_expr = LOOP_VINFO_NITERS_ASSUMPTIONS (loop_vinfo);

  if (*cond_expr)
    *cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			      *cond_expr, part_cond_expr);
  else
    *cond_expr = part_cond_expr;
}

/* Set *COND_EXPR to a tree that is true when both the original *COND_EXPR
   and PART_COND_EXPR are true.  Treat a null *COND_EXPR as "true".  */

static void
chain_cond_expr (tree *cond_expr, tree part_cond_expr)
{
  if (*cond_expr)
    *cond_expr = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			      *cond_expr, part_cond_expr);
  else
    *cond_expr = part_cond_expr;
}

/* Function vect_create_cond_for_align_checks.

   Create a conditional expression that represents the alignment checks for
   all of data references (array element references) whose alignment must be
   checked at runtime.

   Input:
   COND_EXPR  - input conditional expression.  New conditions will be chained
                with logical AND operation.
   LOOP_VINFO - two fields of the loop information are used.
                LOOP_VINFO_PTR_MASK is the mask used to check the alignment.
                LOOP_VINFO_MAY_MISALIGN_STMTS contains the refs to be checked.

   Output:
   COND_EXPR_STMT_LIST - statements needed to construct the conditional
                         expression.
   The returned value is the conditional expression to be used in the if
   statement that controls which version of the loop gets executed at runtime.

   The algorithm makes two assumptions:
     1) The number of bytes "n" in a vector is a power of 2.
     2) An address "a" is aligned if a%n is zero and that this
        test can be done as a&(n-1) == 0.  For example, for 16
        byte vectors the test is a&0xf == 0.  */

static void
vect_create_cond_for_align_checks (loop_vec_info loop_vinfo,
                                   tree *cond_expr,
				   gimple_seq *cond_expr_stmt_list)
{
  vec<stmt_vec_info> may_misalign_stmts
    = LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo);
  stmt_vec_info stmt_info;
  int mask = LOOP_VINFO_PTR_MASK (loop_vinfo);
  tree mask_cst;
  unsigned int i;
  tree int_ptrsize_type;
  char tmp_name[20];
  tree or_tmp_name = NULL_TREE;
  tree and_tmp_name;
  gimple *and_stmt;
  tree ptrsize_zero;
  tree part_cond_expr;

  /* Check that mask is one less than a power of 2, i.e., mask is
     all zeros followed by all ones.  */
  gcc_assert ((mask != 0) && ((mask & (mask+1)) == 0));

  int_ptrsize_type = signed_type_for (ptr_type_node);

  /* Create expression (mask & (dr_1 || ... || dr_n)) where dr_i is the address
     of the first vector of the i'th data reference. */

  FOR_EACH_VEC_ELT (may_misalign_stmts, i, stmt_info)
    {
      gimple_seq new_stmt_list = NULL;
      tree addr_base;
      tree addr_tmp_name;
      tree new_or_tmp_name;
      gimple *addr_stmt, *or_stmt;
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      bool negative = tree_int_cst_compare
	(DR_STEP (STMT_VINFO_DATA_REF (stmt_info)), size_zero_node) < 0;
      tree offset = negative
	? size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1) : size_zero_node;

      /* create: addr_tmp = (int)(address_of_first_vector) */
      addr_base =
	vect_create_addr_base_for_vector_ref (stmt_info, &new_stmt_list,
					      offset);
      if (new_stmt_list != NULL)
	gimple_seq_add_seq (cond_expr_stmt_list, new_stmt_list);

      sprintf (tmp_name, "addr2int%d", i);
      addr_tmp_name = make_temp_ssa_name (int_ptrsize_type, NULL, tmp_name);
      addr_stmt = gimple_build_assign (addr_tmp_name, NOP_EXPR, addr_base);
      gimple_seq_add_stmt (cond_expr_stmt_list, addr_stmt);

      /* The addresses are OR together.  */

      if (or_tmp_name != NULL_TREE)
        {
          /* create: or_tmp = or_tmp | addr_tmp */
          sprintf (tmp_name, "orptrs%d", i);
	  new_or_tmp_name = make_temp_ssa_name (int_ptrsize_type, NULL, tmp_name);
	  or_stmt = gimple_build_assign (new_or_tmp_name, BIT_IOR_EXPR,
					 or_tmp_name, addr_tmp_name);
	  gimple_seq_add_stmt (cond_expr_stmt_list, or_stmt);
          or_tmp_name = new_or_tmp_name;
        }
      else
        or_tmp_name = addr_tmp_name;

    } /* end for i */

  mask_cst = build_int_cst (int_ptrsize_type, mask);

  /* create: and_tmp = or_tmp & mask  */
  and_tmp_name = make_temp_ssa_name (int_ptrsize_type, NULL, "andmask");

  and_stmt = gimple_build_assign (and_tmp_name, BIT_AND_EXPR,
				  or_tmp_name, mask_cst);
  gimple_seq_add_stmt (cond_expr_stmt_list, and_stmt);

  /* Make and_tmp the left operand of the conditional test against zero.
     if and_tmp has a nonzero bit then some address is unaligned.  */
  ptrsize_zero = build_int_cst (int_ptrsize_type, 0);
  part_cond_expr = fold_build2 (EQ_EXPR, boolean_type_node,
				and_tmp_name, ptrsize_zero);
  chain_cond_expr (cond_expr, part_cond_expr);
}

/* If LOOP_VINFO_CHECK_UNEQUAL_ADDRS contains <A1, B1>, ..., <An, Bn>,
   create a tree representation of: (&A1 != &B1) && ... && (&An != &Bn).
   Set *COND_EXPR to a tree that is true when both the original *COND_EXPR
   and this new condition are true.  Treat a null *COND_EXPR as "true".  */

static void
vect_create_cond_for_unequal_addrs (loop_vec_info loop_vinfo, tree *cond_expr)
{
  vec<vec_object_pair> pairs = LOOP_VINFO_CHECK_UNEQUAL_ADDRS (loop_vinfo);
  unsigned int i;
  vec_object_pair *pair;
  FOR_EACH_VEC_ELT (pairs, i, pair)
    {
      tree addr1 = build_fold_addr_expr (pair->first);
      tree addr2 = build_fold_addr_expr (pair->second);
      tree part_cond_expr = fold_build2 (NE_EXPR, boolean_type_node,
					 addr1, addr2);
      chain_cond_expr (cond_expr, part_cond_expr);
    }
}

/* Create an expression that is true when all lower-bound conditions for
   the vectorized loop are met.  Chain this condition with *COND_EXPR.  */

static void
vect_create_cond_for_lower_bounds (loop_vec_info loop_vinfo, tree *cond_expr)
{
  vec<vec_lower_bound> lower_bounds = LOOP_VINFO_LOWER_BOUNDS (loop_vinfo);
  for (unsigned int i = 0; i < lower_bounds.length (); ++i)
    {
      tree expr = lower_bounds[i].expr;
      tree type = unsigned_type_for (TREE_TYPE (expr));
      expr = fold_convert (type, expr);
      poly_uint64 bound = lower_bounds[i].min_value;
      if (!lower_bounds[i].unsigned_p)
	{
	  expr = fold_build2 (PLUS_EXPR, type, expr,
			      build_int_cstu (type, bound - 1));
	  bound += bound - 1;
	}
      tree part_cond_expr = fold_build2 (GE_EXPR, boolean_type_node, expr,
					 build_int_cstu (type, bound));
      chain_cond_expr (cond_expr, part_cond_expr);
    }
}

/* Function vect_create_cond_for_alias_checks.

   Create a conditional expression that represents the run-time checks for
   overlapping of address ranges represented by a list of data references
   relations passed as input.

   Input:
   COND_EXPR  - input conditional expression.  New conditions will be chained
                with logical AND operation.  If it is NULL, then the function
                is used to return the number of alias checks.
   LOOP_VINFO - field LOOP_VINFO_MAY_ALIAS_STMTS contains the list of ddrs
	        to be checked.

   Output:
   COND_EXPR - conditional expression.

   The returned COND_EXPR is the conditional expression to be used in the if
   statement that controls which version of the loop gets executed at runtime.
*/

void
vect_create_cond_for_alias_checks (loop_vec_info loop_vinfo, tree * cond_expr)
{
  vec<dr_with_seg_len_pair_t> comp_alias_ddrs =
    LOOP_VINFO_COMP_ALIAS_DDRS (loop_vinfo);

  if (comp_alias_ddrs.is_empty ())
    return;

  create_runtime_alias_checks (LOOP_VINFO_LOOP (loop_vinfo),
			       &comp_alias_ddrs, cond_expr);
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created %u versioning for alias checks.\n",
		     comp_alias_ddrs.length ());
}


/* Function vect_loop_versioning.

   If the loop has data references that may or may not be aligned or/and
   has data reference relations whose independence was not proven then
   two versions of the loop need to be generated, one which is vectorized
   and one which isn't.  A test is then generated to control which of the
   loops is executed.  The test checks for the alignment of all of the
   data references that may or may not be aligned.  An additional
   sequence of runtime tests is generated for each pairs of DDRs whose
   independence was not proven.  The vectorized version of loop is
   executed only if both alias and alignment tests are passed.

   The test generated to check which version of loop is executed
   is modified to also check for profitability as indicated by the
   cost model threshold TH.

   The versioning precondition(s) are placed in *COND_EXPR and
   *COND_EXPR_STMT_LIST.  */

void
vect_loop_versioning (loop_vec_info loop_vinfo,
		      unsigned int th, bool check_profitability,
		      poly_uint64 versioning_threshold)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo), *nloop;
  struct loop *scalar_loop = LOOP_VINFO_SCALAR_LOOP (loop_vinfo);
  basic_block condition_bb;
  gphi_iterator gsi;
  gimple_stmt_iterator cond_exp_gsi;
  basic_block merge_bb;
  basic_block new_exit_bb;
  edge new_exit_e, e;
  gphi *orig_phi, *new_phi;
  tree cond_expr = NULL_TREE;
  gimple_seq cond_expr_stmt_list = NULL;
  tree arg;
  profile_probability prob = profile_probability::likely ();
  gimple_seq gimplify_stmt_list = NULL;
  tree scalar_loop_iters = LOOP_VINFO_NITERSM1 (loop_vinfo);
  bool version_align = LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo);
  bool version_alias = LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo);
  bool version_niter = LOOP_REQUIRES_VERSIONING_FOR_NITERS (loop_vinfo);

  if (check_profitability)
    cond_expr = fold_build2 (GE_EXPR, boolean_type_node, scalar_loop_iters,
			     build_int_cst (TREE_TYPE (scalar_loop_iters),
					    th - 1));
  if (maybe_ne (versioning_threshold, 0U))
    {
      tree expr = fold_build2 (GE_EXPR, boolean_type_node, scalar_loop_iters,
			       build_int_cst (TREE_TYPE (scalar_loop_iters),
					      versioning_threshold - 1));
      if (cond_expr)
	cond_expr = fold_build2 (BIT_AND_EXPR, boolean_type_node,
				 expr, cond_expr);
      else
	cond_expr = expr;
    }

  if (version_niter)
    vect_create_cond_for_niters_checks (loop_vinfo, &cond_expr);

  if (cond_expr)
    cond_expr = force_gimple_operand_1 (cond_expr, &cond_expr_stmt_list,
					is_gimple_condexpr, NULL_TREE);

  if (version_align)
    vect_create_cond_for_align_checks (loop_vinfo, &cond_expr,
				       &cond_expr_stmt_list);

  if (version_alias)
    {
      vect_create_cond_for_unequal_addrs (loop_vinfo, &cond_expr);
      vect_create_cond_for_lower_bounds (loop_vinfo, &cond_expr);
      vect_create_cond_for_alias_checks (loop_vinfo, &cond_expr);
    }

  cond_expr = force_gimple_operand_1 (unshare_expr (cond_expr),
				      &gimplify_stmt_list,
				      is_gimple_condexpr, NULL_TREE);
  gimple_seq_add_seq (&cond_expr_stmt_list, gimplify_stmt_list);

  initialize_original_copy_tables ();
  if (scalar_loop)
    {
      edge scalar_e;
      basic_block preheader, scalar_preheader;

      /* We don't want to scale SCALAR_LOOP's frequencies, we need to
	 scale LOOP's frequencies instead.  */
      nloop = loop_version (scalar_loop, cond_expr, &condition_bb,
			    prob, prob.invert (), prob, prob.invert (), true);
      scale_loop_frequencies (loop, prob);
      /* CONDITION_BB was created above SCALAR_LOOP's preheader,
	 while we need to move it above LOOP's preheader.  */
      e = loop_preheader_edge (loop);
      scalar_e = loop_preheader_edge (scalar_loop);
      /* The vector loop preheader might not be empty, since new
	 invariants could have been created while analyzing the loop.  */
      gcc_assert (single_pred_p (e->src));
      gcc_assert (empty_block_p (scalar_e->src)
		  && single_pred_p (scalar_e->src));
      gcc_assert (single_pred_p (condition_bb));
      preheader = e->src;
      scalar_preheader = scalar_e->src;
      scalar_e = find_edge (condition_bb, scalar_preheader);
      e = single_pred_edge (preheader);
      redirect_edge_and_branch_force (single_pred_edge (condition_bb),
				      scalar_preheader);
      redirect_edge_and_branch_force (scalar_e, preheader);
      redirect_edge_and_branch_force (e, condition_bb);
      set_immediate_dominator (CDI_DOMINATORS, condition_bb,
			       single_pred (condition_bb));
      set_immediate_dominator (CDI_DOMINATORS, scalar_preheader,
			       single_pred (scalar_preheader));
      set_immediate_dominator (CDI_DOMINATORS, preheader,
			       condition_bb);
    }
  else
    nloop = loop_version (loop, cond_expr, &condition_bb,
			  prob, prob.invert (), prob, prob.invert (), true);

  if (version_niter)
    {
      /* The versioned loop could be infinite, we need to clear existing
	 niter information which is copied from the original loop.  */
      gcc_assert (loop_constraint_set_p (loop, LOOP_C_FINITE));
      vect_free_loop_info_assumptions (nloop);
      /* And set constraint LOOP_C_INFINITE for niter analyzer.  */
      loop_constraint_set (loop, LOOP_C_INFINITE);
    }

  if (LOCATION_LOCUS (vect_location.get_location_t ()) != UNKNOWN_LOCATION
      && dump_enabled_p ())
    {
      if (version_alias)
        dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
                         "loop versioned for vectorization because of "
			 "possible aliasing\n");
      if (version_align)
        dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
                         "loop versioned for vectorization to enhance "
			 "alignment\n");

    }
  free_original_copy_tables ();

  /* Loop versioning violates an assumption we try to maintain during
     vectorization - that the loop exit block has a single predecessor.
     After versioning, the exit block of both loop versions is the same
     basic block (i.e. it has two predecessors). Just in order to simplify
     following transformations in the vectorizer, we fix this situation
     here by adding a new (empty) block on the exit-edge of the loop,
     with the proper loop-exit phis to maintain loop-closed-form.
     If loop versioning wasn't done from loop, but scalar_loop instead,
     merge_bb will have already just a single successor.  */

  merge_bb = single_exit (loop)->dest;
  if (scalar_loop == NULL || EDGE_COUNT (merge_bb->preds) >= 2)
    {
      gcc_assert (EDGE_COUNT (merge_bb->preds) >= 2);
      new_exit_bb = split_edge (single_exit (loop));
      new_exit_e = single_exit (loop);
      e = EDGE_SUCC (new_exit_bb, 0);

      for (gsi = gsi_start_phis (merge_bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  tree new_res;
	  orig_phi = gsi.phi ();
	  new_res = copy_ssa_name (PHI_RESULT (orig_phi));
	  new_phi = create_phi_node (new_res, new_exit_bb);
	  arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, e);
	  add_phi_arg (new_phi, arg, new_exit_e,
		       gimple_phi_arg_location_from_edge (orig_phi, e));
	  adjust_phi_and_debug_stmts (orig_phi, e, PHI_RESULT (new_phi));
	}
    }

  /* End loop-exit-fixes after versioning.  */

  if (cond_expr_stmt_list)
    {
      cond_exp_gsi = gsi_last_bb (condition_bb);
      gsi_insert_seq_before (&cond_exp_gsi, cond_expr_stmt_list,
			     GSI_SAME_STMT);
    }
  update_ssa (TODO_update_ssa);
}
