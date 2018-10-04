/* Statement Analysis and Transformation for Vectorization
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
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "insn-config.h"
#include "recog.h"		/* FIXME: for insn_data */
#include "cgraph.h"
#include "dumpfile.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "cfgloop.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "builtins.h"
#include "internal-fn.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"
#include "tree-ssa-loop-niter.h"
#include "gimple-fold.h"

/* For lang_hooks.types.type_for_mode.  */
#include "langhooks.h"

/* Return the vectorized type for the given statement.  */

tree
stmt_vectype (struct _stmt_vec_info *stmt_info)
{
  return STMT_VINFO_VECTYPE (stmt_info);
}

/* Return TRUE iff the given statement is in an inner loop relative to
   the loop being vectorized.  */
bool
stmt_in_inner_loop_p (struct _stmt_vec_info *stmt_info)
{
  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  basic_block bb = gimple_bb (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop* loop;

  if (!loop_vinfo)
    return false;

  loop = LOOP_VINFO_LOOP (loop_vinfo);

  return (bb->loop_father == loop->inner);
}

/* Record the cost of a statement, either by directly informing the 
   target model or by saving it in a vector for later processing.
   Return a preliminary estimate of the statement's cost.  */

unsigned
record_stmt_cost (stmt_vector_for_cost *body_cost_vec, int count,
		  enum vect_cost_for_stmt kind, stmt_vec_info stmt_info,
		  int misalign, enum vect_cost_model_location where)
{
  if ((kind == vector_load || kind == unaligned_load)
      && STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    kind = vector_gather_load;
  if ((kind == vector_store || kind == unaligned_store)
      && STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    kind = vector_scatter_store;

  stmt_info_for_cost si = { count, kind, where, stmt_info, misalign };
  body_cost_vec->safe_push (si);

  tree vectype = stmt_info ? stmt_vectype (stmt_info) : NULL_TREE;
  return (unsigned)
      (builtin_vectorization_cost (kind, vectype, misalign) * count);
}

/* Return a variable of type ELEM_TYPE[NELEMS].  */

static tree
create_vector_array (tree elem_type, unsigned HOST_WIDE_INT nelems)
{
  return create_tmp_var (build_array_type_nelts (elem_type, nelems),
			 "vect_array");
}

/* ARRAY is an array of vectors created by create_vector_array.
   Return an SSA_NAME for the vector in index N.  The reference
   is part of the vectorization of STMT_INFO and the vector is associated
   with scalar destination SCALAR_DEST.  */

static tree
read_vector_array (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		   tree scalar_dest, tree array, unsigned HOST_WIDE_INT n)
{
  tree vect_type, vect, vect_name, array_ref;
  gimple *new_stmt;

  gcc_assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
  vect_type = TREE_TYPE (TREE_TYPE (array));
  vect = vect_create_destination_var (scalar_dest, vect_type);
  array_ref = build4 (ARRAY_REF, vect_type, array,
		      build_int_cst (size_type_node, n),
		      NULL_TREE, NULL_TREE);

  new_stmt = gimple_build_assign (vect, array_ref);
  vect_name = make_ssa_name (vect, new_stmt);
  gimple_assign_set_lhs (new_stmt, vect_name);
  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

  return vect_name;
}

/* ARRAY is an array of vectors created by create_vector_array.
   Emit code to store SSA_NAME VECT in index N of the array.
   The store is part of the vectorization of STMT_INFO.  */

static void
write_vector_array (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    tree vect, tree array, unsigned HOST_WIDE_INT n)
{
  tree array_ref;
  gimple *new_stmt;

  array_ref = build4 (ARRAY_REF, TREE_TYPE (vect), array,
		      build_int_cst (size_type_node, n),
		      NULL_TREE, NULL_TREE);

  new_stmt = gimple_build_assign (array_ref, vect);
  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
}

/* PTR is a pointer to an array of type TYPE.  Return a representation
   of *PTR.  The memory reference replaces those in FIRST_DR
   (and its group).  */

static tree
create_array_ref (tree type, tree ptr, tree alias_ptr_type)
{
  tree mem_ref;

  mem_ref = build2 (MEM_REF, type, ptr, build_int_cst (alias_ptr_type, 0));
  /* Arrays have the same alignment as their type.  */
  set_ptr_info_alignment (get_ptr_info (ptr), TYPE_ALIGN_UNIT (type), 0);
  return mem_ref;
}

/* Add a clobber of variable VAR to the vectorization of STMT_INFO.
   Emit the clobber before *GSI.  */

static void
vect_clobber_variable (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		       tree var)
{
  tree clobber = build_clobber (TREE_TYPE (var));
  gimple *new_stmt = gimple_build_assign (var, clobber);
  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
}

/* Utility functions used by vect_mark_stmts_to_be_vectorized.  */

/* Function vect_mark_relevant.

   Mark STMT_INFO as "relevant for vectorization" and add it to WORKLIST.  */

static void
vect_mark_relevant (vec<stmt_vec_info> *worklist, stmt_vec_info stmt_info,
		    enum vect_relevant relevant, bool live_p)
{
  enum vect_relevant save_relevant = STMT_VINFO_RELEVANT (stmt_info);
  bool save_live_p = STMT_VINFO_LIVE_P (stmt_info);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "mark relevant %d, live %d: %G", relevant, live_p,
		     stmt_info->stmt);

  /* If this stmt is an original stmt in a pattern, we might need to mark its
     related pattern stmt instead of the original stmt.  However, such stmts
     may have their own uses that are not in any pattern, in such cases the
     stmt itself should be marked.  */
  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
    {
      /* This is the last stmt in a sequence that was detected as a
	 pattern that can potentially be vectorized.  Don't mark the stmt
	 as relevant/live because it's not going to be vectorized.
	 Instead mark the pattern-stmt that replaces it.  */

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "last stmt in pattern. don't mark"
			 " relevant/live.\n");
      stmt_vec_info old_stmt_info = stmt_info;
      stmt_info = STMT_VINFO_RELATED_STMT (stmt_info);
      gcc_assert (STMT_VINFO_RELATED_STMT (stmt_info) == old_stmt_info);
      save_relevant = STMT_VINFO_RELEVANT (stmt_info);
      save_live_p = STMT_VINFO_LIVE_P (stmt_info);
    }

  STMT_VINFO_LIVE_P (stmt_info) |= live_p;
  if (relevant > STMT_VINFO_RELEVANT (stmt_info))
    STMT_VINFO_RELEVANT (stmt_info) = relevant;

  if (STMT_VINFO_RELEVANT (stmt_info) == save_relevant
      && STMT_VINFO_LIVE_P (stmt_info) == save_live_p)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "already marked relevant/live.\n");
      return;
    }

  worklist->safe_push (stmt_info);
}


/* Function is_simple_and_all_uses_invariant

   Return true if STMT_INFO is simple and all uses of it are invariant.  */

bool
is_simple_and_all_uses_invariant (stmt_vec_info stmt_info,
				  loop_vec_info loop_vinfo)
{
  tree op;
  ssa_op_iter iter;

  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
    {
      enum vect_def_type dt = vect_uninitialized_def;

      if (!vect_is_simple_use (op, loop_vinfo, &dt))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "use not simple.\n");
	  return false;
	}

      if (dt != vect_external_def && dt != vect_constant_def)
	return false;
    }
  return true;
}

/* Function vect_stmt_relevant_p.

   Return true if STMT_INFO, in the loop that is represented by LOOP_VINFO,
   is "relevant for vectorization".

   A stmt is considered "relevant for vectorization" if:
   - it has uses outside the loop.
   - it has vdefs (it alters memory).
   - control stmts in the loop (except for the exit condition).

   CHECKME: what other side effects would the vectorizer allow?  */

static bool
vect_stmt_relevant_p (stmt_vec_info stmt_info, loop_vec_info loop_vinfo,
		      enum vect_relevant *relevant, bool *live_p)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  def_operand_p def_p;

  *relevant = vect_unused_in_scope;
  *live_p = false;

  /* cond stmt other than loop exit cond.  */
  if (is_ctrl_stmt (stmt_info->stmt)
      && STMT_VINFO_TYPE (stmt_info) != loop_exit_ctrl_vec_info_type)
    *relevant = vect_used_in_scope;

  /* changing memory.  */
  if (gimple_code (stmt_info->stmt) != GIMPLE_PHI)
    if (gimple_vdef (stmt_info->stmt)
	&& !gimple_clobber_p (stmt_info->stmt))
      {
	if (dump_enabled_p ())
	  dump_printf_loc (MSG_NOTE, vect_location,
                           "vec_stmt_relevant_p: stmt has vdefs.\n");
	*relevant = vect_used_in_scope;
      }

  /* uses outside the loop.  */
  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt_info->stmt, op_iter, SSA_OP_DEF)
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, DEF_FROM_PTR (def_p))
	{
	  basic_block bb = gimple_bb (USE_STMT (use_p));
	  if (!flow_bb_inside_loop_p (loop, bb))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
                                 "vec_stmt_relevant_p: used out of loop.\n");

	      if (is_gimple_debug (USE_STMT (use_p)))
		continue;

	      /* We expect all such uses to be in the loop exit phis
		 (because of loop closed form)   */
	      gcc_assert (gimple_code (USE_STMT (use_p)) == GIMPLE_PHI);
	      gcc_assert (bb == single_exit (loop)->dest);

              *live_p = true;
	    }
	}
    }

  if (*live_p && *relevant == vect_unused_in_scope
      && !is_simple_and_all_uses_invariant (stmt_info, loop_vinfo))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "vec_stmt_relevant_p: stmt live but not relevant.\n");
      *relevant = vect_used_only_live;
    }

  return (*live_p || *relevant);
}


/* Function exist_non_indexing_operands_for_use_p

   USE is one of the uses attached to STMT_INFO.  Check if USE is
   used in STMT_INFO for anything other than indexing an array.  */

static bool
exist_non_indexing_operands_for_use_p (tree use, stmt_vec_info stmt_info)
{
  tree operand;

  /* USE corresponds to some operand in STMT.  If there is no data
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

  gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
  if (!assign || !gimple_assign_copy_p (assign))
    {
      gcall *call = dyn_cast <gcall *> (stmt_info->stmt);
      if (call && gimple_call_internal_p (call))
	{
	  internal_fn ifn = gimple_call_internal_fn (call);
	  int mask_index = internal_fn_mask_index (ifn);
	  if (mask_index >= 0
	      && use == gimple_call_arg (call, mask_index))
	    return true;
	  int stored_value_index = internal_fn_stored_value_index (ifn);
	  if (stored_value_index >= 0
	      && use == gimple_call_arg (call, stored_value_index))
	    return true;
	  if (internal_gather_scatter_fn_p (ifn)
	      && use == gimple_call_arg (call, 1))
	    return true;
	}
      return false;
    }

  if (TREE_CODE (gimple_assign_lhs (assign)) == SSA_NAME)
    return false;
  operand = gimple_assign_rhs1 (assign);
  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  if (operand == use)
    return true;

  return false;
}


/*
   Function process_use.

   Inputs:
   - a USE in STMT_VINFO in a loop represented by LOOP_VINFO
   - RELEVANT - enum value to be set in the STMT_VINFO of the stmt
     that defined USE.  This is done by calling mark_relevant and passing it
     the WORKLIST (to add DEF_STMT to the WORKLIST in case it is relevant).
   - FORCE is true if exist_non_indexing_operands_for_use_p check shouldn't
     be performed.

   Outputs:
   Generally, LIVE_P and RELEVANT are used to define the liveness and
   relevance info of the DEF_STMT of this USE:
       STMT_VINFO_LIVE_P (DEF_stmt_vinfo) <-- live_p
       STMT_VINFO_RELEVANT (DEF_stmt_vinfo) <-- relevant
   Exceptions:
   - case 1: If USE is used only for address computations (e.g. array indexing),
   which does not need to be directly vectorized, then the liveness/relevance
   of the respective DEF_STMT is left unchanged.
   - case 2: If STMT_VINFO is a reduction phi and DEF_STMT is a reduction stmt,
   we skip DEF_STMT cause it had already been processed.
   - case 3: If DEF_STMT and STMT_VINFO are in different nests, then
   "relevant" will be modified accordingly.

   Return true if everything is as expected. Return false otherwise.  */

static opt_result
process_use (stmt_vec_info stmt_vinfo, tree use, loop_vec_info loop_vinfo,
	     enum vect_relevant relevant, vec<stmt_vec_info> *worklist,
	     bool force)
{
  stmt_vec_info dstmt_vinfo;
  basic_block bb, def_bb;
  enum vect_def_type dt;

  /* case 1: we are only interested in uses that need to be vectorized.  Uses
     that are used for address computation are not considered relevant.  */
  if (!force && !exist_non_indexing_operands_for_use_p (use, stmt_vinfo))
    return opt_result::success ();

  if (!vect_is_simple_use (use, loop_vinfo, &dt, &dstmt_vinfo))
    return opt_result::failure_at (stmt_vinfo->stmt,
				   "not vectorized:"
				   " unsupported use in stmt.\n");

  if (!dstmt_vinfo)
    return opt_result::success ();

  def_bb = gimple_bb (dstmt_vinfo->stmt);

  /* case 2: A reduction phi (STMT) defined by a reduction stmt (DSTMT_VINFO).
     DSTMT_VINFO must have already been processed, because this should be the
     only way that STMT, which is a reduction-phi, was put in the worklist,
     as there should be no other uses for DSTMT_VINFO in the loop.  So we just
     check that everything is as expected, and we are done.  */
  bb = gimple_bb (stmt_vinfo->stmt);
  if (gimple_code (stmt_vinfo->stmt) == GIMPLE_PHI
      && STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
      && gimple_code (dstmt_vinfo->stmt) != GIMPLE_PHI
      && STMT_VINFO_DEF_TYPE (dstmt_vinfo) == vect_reduction_def
      && bb->loop_father == def_bb->loop_father)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "reduc-stmt defining reduc-phi in the same nest.\n");
      gcc_assert (STMT_VINFO_RELEVANT (dstmt_vinfo) < vect_used_by_reduction);
      gcc_assert (STMT_VINFO_LIVE_P (dstmt_vinfo)
		  || STMT_VINFO_RELEVANT (dstmt_vinfo) > vect_unused_in_scope);
      return opt_result::success ();
    }

  /* case 3a: outer-loop stmt defining an inner-loop stmt:
	outer-loop-header-bb:
		d = dstmt_vinfo
	inner-loop:
		stmt # use (d)
	outer-loop-tail-bb:
		...		  */
  if (flow_loop_nested_p (def_bb->loop_father, bb->loop_father))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "outer-loop def-stmt defining inner-loop stmt.\n");

      switch (relevant)
	{
	case vect_unused_in_scope:
	  relevant = (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_nested_cycle) ?
		      vect_used_in_scope : vect_unused_in_scope;
	  break;

	case vect_used_in_outer_by_reduction:
          gcc_assert (STMT_VINFO_DEF_TYPE (stmt_vinfo) != vect_reduction_def);
	  relevant = vect_used_by_reduction;
	  break;

	case vect_used_in_outer:
          gcc_assert (STMT_VINFO_DEF_TYPE (stmt_vinfo) != vect_reduction_def);
	  relevant = vect_used_in_scope;
	  break;

	case vect_used_in_scope:
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* case 3b: inner-loop stmt defining an outer-loop stmt:
	outer-loop-header-bb:
		...
	inner-loop:
		d = dstmt_vinfo
	outer-loop-tail-bb (or outer-loop-exit-bb in double reduction):
		stmt # use (d)		*/
  else if (flow_loop_nested_p (bb->loop_father, def_bb->loop_father))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "inner-loop def-stmt defining outer-loop stmt.\n");

      switch (relevant)
        {
        case vect_unused_in_scope:
          relevant = (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
            || STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_double_reduction_def) ?
                      vect_used_in_outer_by_reduction : vect_unused_in_scope;
          break;

        case vect_used_by_reduction:
	case vect_used_only_live:
          relevant = vect_used_in_outer_by_reduction;
          break;

        case vect_used_in_scope:
          relevant = vect_used_in_outer;
          break;

        default:
          gcc_unreachable ();
        }
    }
  /* We are also not interested in uses on loop PHI backedges that are
     inductions.  Otherwise we'll needlessly vectorize the IV increment
     and cause hybrid SLP for SLP inductions.  Unless the PHI is live
     of course.  */
  else if (gimple_code (stmt_vinfo->stmt) == GIMPLE_PHI
	   && STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_induction_def
	   && ! STMT_VINFO_LIVE_P (stmt_vinfo)
	   && (PHI_ARG_DEF_FROM_EDGE (stmt_vinfo->stmt,
				      loop_latch_edge (bb->loop_father))
	       == use))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "induction value on backedge.\n");
      return opt_result::success ();
    }


  vect_mark_relevant (worklist, dstmt_vinfo, relevant, false);
  return opt_result::success ();
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

opt_result
vect_mark_stmts_to_be_vectorized (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned int nbbs = loop->num_nodes;
  gimple_stmt_iterator si;
  unsigned int i;
  basic_block bb;
  bool live_p;
  enum vect_relevant relevant;

  DUMP_VECT_SCOPE ("vect_mark_stmts_to_be_vectorized");

  auto_vec<stmt_vec_info, 64> worklist;

  /* 1. Init worklist.  */
  for (i = 0; i < nbbs; i++)
    {
      bb = bbs[i];
      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info phi_info = loop_vinfo->lookup_stmt (gsi_stmt (si));
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "init: phi relevant? %G",
			     phi_info->stmt);

	  if (vect_stmt_relevant_p (phi_info, loop_vinfo, &relevant, &live_p))
	    vect_mark_relevant (&worklist, phi_info, relevant, live_p);
	}
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (gsi_stmt (si));
	  if (dump_enabled_p ())
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "init: stmt relevant? %G", stmt_info->stmt);

	  if (vect_stmt_relevant_p (stmt_info, loop_vinfo, &relevant, &live_p))
	    vect_mark_relevant (&worklist, stmt_info, relevant, live_p);
	}
    }

  /* 2. Process_worklist */
  while (worklist.length () > 0)
    {
      use_operand_p use_p;
      ssa_op_iter iter;

      stmt_vec_info stmt_vinfo = worklist.pop ();
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "worklist: examine stmt: %G", stmt_vinfo->stmt);

      /* Examine the USEs of STMT. For each USE, mark the stmt that defines it
	 (DEF_STMT) as relevant/irrelevant according to the relevance property
	 of STMT.  */
      relevant = STMT_VINFO_RELEVANT (stmt_vinfo);

      /* Generally, the relevance property of STMT (in STMT_VINFO_RELEVANT) is
	 propagated as is to the DEF_STMTs of its USEs.

	 One exception is when STMT has been identified as defining a reduction
	 variable; in this case we set the relevance to vect_used_by_reduction.
	 This is because we distinguish between two kinds of relevant stmts -
	 those that are used by a reduction computation, and those that are
	 (also) used by a regular computation.  This allows us later on to
	 identify stmts that are used solely by a reduction, and therefore the
	 order of the results that they produce does not have to be kept.  */

      switch (STMT_VINFO_DEF_TYPE (stmt_vinfo))
        {
          case vect_reduction_def:
	    gcc_assert (relevant != vect_unused_in_scope);
	    if (relevant != vect_unused_in_scope
		&& relevant != vect_used_in_scope
		&& relevant != vect_used_by_reduction
		&& relevant != vect_used_only_live)
	      return opt_result::failure_at
		(stmt_vinfo->stmt, "unsupported use of reduction.\n");
	    break;

          case vect_nested_cycle:
	    if (relevant != vect_unused_in_scope
		&& relevant != vect_used_in_outer_by_reduction
		&& relevant != vect_used_in_outer)
	      return opt_result::failure_at
		(stmt_vinfo->stmt, "unsupported use of nested cycle.\n");
            break;

          case vect_double_reduction_def:
	    if (relevant != vect_unused_in_scope
		&& relevant != vect_used_by_reduction
		&& relevant != vect_used_only_live)
	      return opt_result::failure_at
		(stmt_vinfo->stmt, "unsupported use of double reduction.\n");
            break;

          default:
            break;
        }

      if (is_pattern_stmt_p (stmt_vinfo))
        {
          /* Pattern statements are not inserted into the code, so
             FOR_EACH_PHI_OR_STMT_USE optimizes their operands out, and we
             have to scan the RHS or function arguments instead.  */
	  if (gassign *assign = dyn_cast <gassign *> (stmt_vinfo->stmt))
	    {
	      enum tree_code rhs_code = gimple_assign_rhs_code (assign);
	      tree op = gimple_assign_rhs1 (assign);

	      i = 1;
	      if (rhs_code == COND_EXPR && COMPARISON_CLASS_P (op))
		{
		  opt_result res
		    = process_use (stmt_vinfo, TREE_OPERAND (op, 0),
				   loop_vinfo, relevant, &worklist, false);
		  if (!res)
		    return res;
		  res = process_use (stmt_vinfo, TREE_OPERAND (op, 1),
				     loop_vinfo, relevant, &worklist, false);
		  if (!res)
		    return res;
		  i = 2;
		}
	      for (; i < gimple_num_ops (assign); i++)
		{
		  op = gimple_op (assign, i);
                  if (TREE_CODE (op) == SSA_NAME)
		    {
		      opt_result res
			= process_use (stmt_vinfo, op, loop_vinfo, relevant,
				       &worklist, false);
		      if (!res)
			return res;
		    }
                 }
            }
	  else if (gcall *call = dyn_cast <gcall *> (stmt_vinfo->stmt))
	    {
	      for (i = 0; i < gimple_call_num_args (call); i++)
		{
		  tree arg = gimple_call_arg (call, i);
		  opt_result res
		    = process_use (stmt_vinfo, arg, loop_vinfo, relevant,
				   &worklist, false);
		  if (!res)
		    return res;
		}
	    }
        }
      else
	FOR_EACH_PHI_OR_STMT_USE (use_p, stmt_vinfo->stmt, iter, SSA_OP_USE)
          {
            tree op = USE_FROM_PTR (use_p);
	    opt_result res
	      = process_use (stmt_vinfo, op, loop_vinfo, relevant,
			     &worklist, false);
	    if (!res)
	      return res;
          }

      if (STMT_VINFO_GATHER_SCATTER_P (stmt_vinfo))
	{
	  gather_scatter_info gs_info;
	  if (!vect_check_gather_scatter (stmt_vinfo, loop_vinfo, &gs_info))
	    gcc_unreachable ();
	  opt_result res
	    = process_use (stmt_vinfo, gs_info.offset, loop_vinfo, relevant,
			   &worklist, true);
	  if (!res)
	    return res;
	}
    } /* while worklist */

  return opt_result::success ();
}

/* Compute the prologue cost for invariant or constant operands.  */

static unsigned
vect_prologue_cost_for_slp_op (slp_tree node, stmt_vec_info stmt_info,
			       unsigned opno, enum vect_def_type dt,
			       stmt_vector_for_cost *cost_vec)
{
  gimple *stmt = SLP_TREE_SCALAR_STMTS (node)[0]->stmt;
  tree op = gimple_op (stmt, opno);
  unsigned prologue_cost = 0;

  /* Without looking at the actual initializer a vector of
     constants can be implemented as load from the constant pool.
     When all elements are the same we can use a splat.  */
  tree vectype = get_vectype_for_scalar_type (TREE_TYPE (op));
  unsigned group_size = SLP_TREE_SCALAR_STMTS (node).length ();
  unsigned num_vects_to_check;
  unsigned HOST_WIDE_INT const_nunits;
  unsigned nelt_limit;
  if (TYPE_VECTOR_SUBPARTS (vectype).is_constant (&const_nunits)
      && ! multiple_p (const_nunits, group_size))
    {
      num_vects_to_check = SLP_TREE_NUMBER_OF_VEC_STMTS (node);
      nelt_limit = const_nunits;
    }
  else
    {
      /* If either the vector has variable length or the vectors
	 are composed of repeated whole groups we only need to
	 cost construction once.  All vectors will be the same.  */
      num_vects_to_check = 1;
      nelt_limit = group_size;
    }
  tree elt = NULL_TREE;
  unsigned nelt = 0;
  for (unsigned j = 0; j < num_vects_to_check * nelt_limit; ++j)
    {
      unsigned si = j % group_size;
      if (nelt == 0)
	elt = gimple_op (SLP_TREE_SCALAR_STMTS (node)[si]->stmt, opno);
      /* ???  We're just tracking whether all operands of a single
	 vector initializer are the same, ideally we'd check if
	 we emitted the same one already.  */
      else if (elt != gimple_op (SLP_TREE_SCALAR_STMTS (node)[si]->stmt,
				 opno))
	elt = NULL_TREE;
      nelt++;
      if (nelt == nelt_limit)
	{
	  /* ???  We need to pass down stmt_info for a vector type
	     even if it points to the wrong stmt.  */
	  prologue_cost += record_stmt_cost
	      (cost_vec, 1,
	       dt == vect_external_def
	       ? (elt ? scalar_to_vec : vec_construct)
	       : vector_load,
	       stmt_info, 0, vect_prologue);
	  nelt = 0;
	}
    }

  return prologue_cost;
}

/* Function vect_model_simple_cost.

   Models cost for simple operations, i.e. those that only emit ncopies of a
   single op.  Right now, this does not account for multiple insns that could
   be generated for the single vector op.  We will handle that shortly.  */

static void
vect_model_simple_cost (stmt_vec_info stmt_info, int ncopies,
			enum vect_def_type *dt,
			int ndts,
			slp_tree node,
			stmt_vector_for_cost *cost_vec)
{
  int inside_cost = 0, prologue_cost = 0;

  gcc_assert (cost_vec != NULL);

  /* ???  Somehow we need to fix this at the callers.  */
  if (node)
    ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (node);

  if (node)
    {
      /* Scan operands and account for prologue cost of constants/externals.
	 ???  This over-estimates cost for multiple uses and should be
	 re-engineered.  */
      gimple *stmt = SLP_TREE_SCALAR_STMTS (node)[0]->stmt;
      tree lhs = gimple_get_lhs (stmt);
      for (unsigned i = 0; i < gimple_num_ops (stmt); ++i)
	{
	  tree op = gimple_op (stmt, i);
	  enum vect_def_type dt;
	  if (!op || op == lhs)
	    continue;
	  if (vect_is_simple_use (op, stmt_info->vinfo, &dt)
	      && (dt == vect_constant_def || dt == vect_external_def))
	    prologue_cost += vect_prologue_cost_for_slp_op (node, stmt_info,
							    i, dt, cost_vec);
	}
    }
  else
    /* Cost the "broadcast" of a scalar operand in to a vector operand.
       Use scalar_to_vec to cost the broadcast, as elsewhere in the vector
       cost model.  */
    for (int i = 0; i < ndts; i++)
      if (dt[i] == vect_constant_def || dt[i] == vect_external_def)
	prologue_cost += record_stmt_cost (cost_vec, 1, scalar_to_vec,
					   stmt_info, 0, vect_prologue);

  /* Adjust for two-operator SLP nodes.  */
  if (node && SLP_TREE_TWO_OPERATORS (node))
    {
      ncopies *= 2;
      inside_cost += record_stmt_cost (cost_vec, ncopies, vec_perm,
				       stmt_info, 0, vect_body);
    }

  /* Pass the inside-of-loop statements to the target-specific cost model.  */
  inside_cost += record_stmt_cost (cost_vec, ncopies, vector_stmt,
				   stmt_info, 0, vect_body);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_simple_cost: inside_cost = %d, "
                     "prologue_cost = %d .\n", inside_cost, prologue_cost);
}


/* Model cost for type demotion and promotion operations.  PWR is normally
   zero for single-step promotions and demotions.  It will be one if 
   two-step promotion/demotion is required, and so on.  Each additional
   step doubles the number of instructions required.  */

static void
vect_model_promotion_demotion_cost (stmt_vec_info stmt_info,
				    enum vect_def_type *dt, int pwr,
				    stmt_vector_for_cost *cost_vec)
{
  int i, tmp;
  int inside_cost = 0, prologue_cost = 0;

  for (i = 0; i < pwr + 1; i++)
    {
      tmp = (STMT_VINFO_TYPE (stmt_info) == type_promotion_vec_info_type) ?
	(i + 1) : i;
      inside_cost += record_stmt_cost (cost_vec, vect_pow2 (tmp),
				       vec_promote_demote, stmt_info, 0,
				       vect_body);
    }

  /* FORNOW: Assuming maximum 2 args per stmts.  */
  for (i = 0; i < 2; i++)
    if (dt[i] == vect_constant_def || dt[i] == vect_external_def)
      prologue_cost += record_stmt_cost (cost_vec, 1, vector_stmt,
					 stmt_info, 0, vect_prologue);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_promotion_demotion_cost: inside_cost = %d, "
                     "prologue_cost = %d .\n", inside_cost, prologue_cost);
}

/* Function vect_model_store_cost

   Models cost for stores.  In the case of grouped accesses, one access
   has the overhead of the grouped access attributed to it.  */

static void
vect_model_store_cost (stmt_vec_info stmt_info, int ncopies,
		       enum vect_def_type dt,
		       vect_memory_access_type memory_access_type,
		       vec_load_store_type vls_type, slp_tree slp_node,
		       stmt_vector_for_cost *cost_vec)
{
  unsigned int inside_cost = 0, prologue_cost = 0;
  stmt_vec_info first_stmt_info = stmt_info;
  bool grouped_access_p = STMT_VINFO_GROUPED_ACCESS (stmt_info);

  /* ???  Somehow we need to fix this at the callers.  */
  if (slp_node)
    ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);

  if (vls_type == VLS_STORE_INVARIANT)
    {
      if (slp_node)
	prologue_cost += vect_prologue_cost_for_slp_op (slp_node, stmt_info,
							1, dt, cost_vec);
      else
	prologue_cost += record_stmt_cost (cost_vec, 1, scalar_to_vec,
					   stmt_info, 0, vect_prologue);
    }

  /* Grouped stores update all elements in the group at once,
     so we want the DR for the first statement.  */
  if (!slp_node && grouped_access_p)
    first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);

  /* True if we should include any once-per-group costs as well as
     the cost of the statement itself.  For SLP we only get called
     once per group anyhow.  */
  bool first_stmt_p = (first_stmt_info == stmt_info);

  /* We assume that the cost of a single store-lanes instruction is
     equivalent to the cost of DR_GROUP_SIZE separate stores.  If a grouped
     access is instead being provided by a permute-and-store operation,
     include the cost of the permutes.  */
  if (first_stmt_p
      && memory_access_type == VMAT_CONTIGUOUS_PERMUTE)
    {
      /* Uses a high and low interleave or shuffle operations for each
	 needed permute.  */
      int group_size = DR_GROUP_SIZE (first_stmt_info);
      int nstmts = ncopies * ceil_log2 (group_size) * group_size;
      inside_cost = record_stmt_cost (cost_vec, nstmts, vec_perm,
				      stmt_info, 0, vect_body);

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "vect_model_store_cost: strided group_size = %d .\n",
                         group_size);
    }

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  /* Costs of the stores.  */
  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_GATHER_SCATTER)
    {
      /* N scalar stores plus extracting the elements.  */
      unsigned int assumed_nunits = vect_nunits_for_cost (vectype);
      inside_cost += record_stmt_cost (cost_vec,
				       ncopies * assumed_nunits,
				       scalar_store, stmt_info, 0, vect_body);
    }
  else
    vect_get_store_cost (stmt_info, ncopies, &inside_cost, cost_vec);

  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_STRIDED_SLP)
    {
      /* N scalar stores plus extracting the elements.  */
      unsigned int assumed_nunits = vect_nunits_for_cost (vectype);
      inside_cost += record_stmt_cost (cost_vec,
				       ncopies * assumed_nunits,
				       vec_to_scalar, stmt_info, 0, vect_body);
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_store_cost: inside_cost = %d, "
                     "prologue_cost = %d .\n", inside_cost, prologue_cost);
}


/* Calculate cost of DR's memory access.  */
void
vect_get_store_cost (stmt_vec_info stmt_info, int ncopies,
		     unsigned int *inside_cost,
		     stmt_vector_for_cost *body_cost_vec)
{
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  int alignment_support_scheme
    = vect_supportable_dr_alignment (dr_info, false);

  switch (alignment_support_scheme)
    {
    case dr_aligned:
      {
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies,
					  vector_store, stmt_info, 0,
					  vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_store_cost: aligned.\n");
        break;
      }

    case dr_unaligned_supported:
      {
        /* Here, we assign an additional cost for the unaligned store.  */
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies,
					  unaligned_store, stmt_info,
					  DR_MISALIGNMENT (dr_info),
					  vect_body);
        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_store_cost: unaligned supported by "
                           "hardware.\n");
        break;
      }

    case dr_unaligned_unsupported:
      {
        *inside_cost = VECT_MAX_COST;

        if (dump_enabled_p ())
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "vect_model_store_cost: unsupported access.\n");
        break;
      }

    default:
      gcc_unreachable ();
    }
}


/* Function vect_model_load_cost

   Models cost for loads.  In the case of grouped accesses, one access has
   the overhead of the grouped access attributed to it.  Since unaligned
   accesses are supported for loads, we also account for the costs of the
   access scheme chosen.  */

static void
vect_model_load_cost (stmt_vec_info stmt_info, unsigned ncopies,
		      vect_memory_access_type memory_access_type,
		      slp_instance instance,
		      slp_tree slp_node,
		      stmt_vector_for_cost *cost_vec)
{
  unsigned int inside_cost = 0, prologue_cost = 0;
  bool grouped_access_p = STMT_VINFO_GROUPED_ACCESS (stmt_info);

  gcc_assert (cost_vec);

  /* ???  Somehow we need to fix this at the callers.  */
  if (slp_node)
    ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);

  if (slp_node && SLP_TREE_LOAD_PERMUTATION (slp_node).exists ())
    {
      /* If the load is permuted then the alignment is determined by
	 the first group element not by the first scalar stmt DR.  */
      stmt_vec_info first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      /* Record the cost for the permutation.  */
      unsigned n_perms;
      unsigned assumed_nunits
	= vect_nunits_for_cost (STMT_VINFO_VECTYPE (first_stmt_info));
      unsigned slp_vf = (ncopies * assumed_nunits) / instance->group_size; 
      vect_transform_slp_perm_load (slp_node, vNULL, NULL,
				    slp_vf, instance, true,
				    &n_perms);
      inside_cost += record_stmt_cost (cost_vec, n_perms, vec_perm,
				       first_stmt_info, 0, vect_body);
      /* And adjust the number of loads performed.  This handles
	 redundancies as well as loads that are later dead.  */
      auto_sbitmap perm (DR_GROUP_SIZE (first_stmt_info));
      bitmap_clear (perm);
      for (unsigned i = 0;
	   i < SLP_TREE_LOAD_PERMUTATION (slp_node).length (); ++i)
	bitmap_set_bit (perm, SLP_TREE_LOAD_PERMUTATION (slp_node)[i]);
      ncopies = 0;
      bool load_seen = false;
      for (unsigned i = 0; i < DR_GROUP_SIZE (first_stmt_info); ++i)
	{
	  if (i % assumed_nunits == 0)
	    {
	      if (load_seen)
		ncopies++;
	      load_seen = false;
	    }
	  if (bitmap_bit_p (perm, i))
	    load_seen = true;
	}
      if (load_seen)
	ncopies++;
      gcc_assert (ncopies
		  <= (DR_GROUP_SIZE (first_stmt_info)
		      - DR_GROUP_GAP (first_stmt_info)
		      + assumed_nunits - 1) / assumed_nunits);
    }

  /* Grouped loads read all elements in the group at once,
     so we want the DR for the first statement.  */
  stmt_vec_info first_stmt_info = stmt_info;
  if (!slp_node && grouped_access_p)
    first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);

  /* True if we should include any once-per-group costs as well as
     the cost of the statement itself.  For SLP we only get called
     once per group anyhow.  */
  bool first_stmt_p = (first_stmt_info == stmt_info);

  /* We assume that the cost of a single load-lanes instruction is
     equivalent to the cost of DR_GROUP_SIZE separate loads.  If a grouped
     access is instead being provided by a load-and-permute operation,
     include the cost of the permutes.  */
  if (first_stmt_p
      && memory_access_type == VMAT_CONTIGUOUS_PERMUTE)
    {
      /* Uses an even and odd extract operations or shuffle operations
	 for each needed permute.  */
      int group_size = DR_GROUP_SIZE (first_stmt_info);
      int nstmts = ncopies * ceil_log2 (group_size) * group_size;
      inside_cost += record_stmt_cost (cost_vec, nstmts, vec_perm,
				       stmt_info, 0, vect_body);

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "vect_model_load_cost: strided group_size = %d .\n",
                         group_size);
    }

  /* The loads themselves.  */
  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_GATHER_SCATTER)
    {
      /* N scalar loads plus gathering them into a vector.  */
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      unsigned int assumed_nunits = vect_nunits_for_cost (vectype);
      inside_cost += record_stmt_cost (cost_vec,
				       ncopies * assumed_nunits,
				       scalar_load, stmt_info, 0, vect_body);
    }
  else
    vect_get_load_cost (stmt_info, ncopies, first_stmt_p,
			&inside_cost, &prologue_cost, 
			cost_vec, cost_vec, true);
  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_STRIDED_SLP)
    inside_cost += record_stmt_cost (cost_vec, ncopies, vec_construct,
				     stmt_info, 0, vect_body);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_load_cost: inside_cost = %d, "
                     "prologue_cost = %d .\n", inside_cost, prologue_cost);
}


/* Calculate cost of DR's memory access.  */
void
vect_get_load_cost (stmt_vec_info stmt_info, int ncopies,
		    bool add_realign_cost, unsigned int *inside_cost,
		    unsigned int *prologue_cost,
		    stmt_vector_for_cost *prologue_cost_vec,
		    stmt_vector_for_cost *body_cost_vec,
		    bool record_prologue_costs)
{
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  int alignment_support_scheme
    = vect_supportable_dr_alignment (dr_info, false);

  switch (alignment_support_scheme)
    {
    case dr_aligned:
      {
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies, vector_load,
					  stmt_info, 0, vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_load_cost: aligned.\n");

        break;
      }
    case dr_unaligned_supported:
      {
        /* Here, we assign an additional cost for the unaligned load.  */
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies,
					  unaligned_load, stmt_info,
					  DR_MISALIGNMENT (dr_info),
					  vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_load_cost: unaligned supported by "
                           "hardware.\n");

        break;
      }
    case dr_explicit_realign:
      {
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies * 2,
					  vector_load, stmt_info, 0, vect_body);
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies,
					  vec_perm, stmt_info, 0, vect_body);

        /* FIXME: If the misalignment remains fixed across the iterations of
           the containing loop, the following cost should be added to the
           prologue costs.  */
        if (targetm.vectorize.builtin_mask_for_load)
	  *inside_cost += record_stmt_cost (body_cost_vec, 1, vector_stmt,
					    stmt_info, 0, vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_load_cost: explicit realign\n");

        break;
      }
    case dr_explicit_realign_optimized:
      {
        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_load_cost: unaligned software "
                           "pipelined.\n");

        /* Unaligned software pipeline has a load of an address, an initial
           load, and possibly a mask operation to "prime" the loop.  However,
           if this is an access in a group of loads, which provide grouped
           access, then the above cost should only be considered for one
           access in the group.  Inside the loop, there is a load op
           and a realignment op.  */

        if (add_realign_cost && record_prologue_costs)
          {
	    *prologue_cost += record_stmt_cost (prologue_cost_vec, 2,
						vector_stmt, stmt_info,
						0, vect_prologue);
            if (targetm.vectorize.builtin_mask_for_load)
	      *prologue_cost += record_stmt_cost (prologue_cost_vec, 1,
						  vector_stmt, stmt_info,
						  0, vect_prologue);
          }

	*inside_cost += record_stmt_cost (body_cost_vec, ncopies, vector_load,
					  stmt_info, 0, vect_body);
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies, vec_perm,
					  stmt_info, 0, vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_load_cost: explicit realign optimized"
                           "\n");

        break;
      }

    case dr_unaligned_unsupported:
      {
        *inside_cost = VECT_MAX_COST;

        if (dump_enabled_p ())
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "vect_model_load_cost: unsupported access.\n");
        break;
      }

    default:
      gcc_unreachable ();
    }
}

/* Insert the new stmt NEW_STMT at *GSI or at the appropriate place in
   the loop preheader for the vectorized stmt STMT_VINFO.  */

static void
vect_init_vector_1 (stmt_vec_info stmt_vinfo, gimple *new_stmt,
		    gimple_stmt_iterator *gsi)
{
  if (gsi)
    vect_finish_stmt_generation (stmt_vinfo, new_stmt, gsi);
  else
    {
      loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);

      if (loop_vinfo)
        {
          struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
	  basic_block new_bb;
	  edge pe;

	  if (nested_in_vect_loop_p (loop, stmt_vinfo))
	    loop = loop->inner;

	  pe = loop_preheader_edge (loop);
          new_bb = gsi_insert_on_edge_immediate (pe, new_stmt);
          gcc_assert (!new_bb);
	}
      else
       {
          bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_vinfo);
          basic_block bb;
          gimple_stmt_iterator gsi_bb_start;

          gcc_assert (bb_vinfo);
          bb = BB_VINFO_BB (bb_vinfo);
          gsi_bb_start = gsi_after_labels (bb);
          gsi_insert_before (&gsi_bb_start, new_stmt, GSI_SAME_STMT);
       }
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created new init_stmt: %G", new_stmt);
}

/* Function vect_init_vector.

   Insert a new stmt (INIT_STMT) that initializes a new variable of type
   TYPE with the value VAL.  If TYPE is a vector type and VAL does not have
   vector type a vector with all elements equal to VAL is created first.
   Place the initialization at BSI if it is not NULL.  Otherwise, place the
   initialization at the loop preheader.
   Return the DEF of INIT_STMT.
   It will be used in the vectorization of STMT_INFO.  */

tree
vect_init_vector (stmt_vec_info stmt_info, tree val, tree type,
		  gimple_stmt_iterator *gsi)
{
  gimple *init_stmt;
  tree new_temp;

  /* We abuse this function to push sth to a SSA name with initial 'val'.  */
  if (! useless_type_conversion_p (type, TREE_TYPE (val)))
    {
      gcc_assert (TREE_CODE (type) == VECTOR_TYPE);
      if (! types_compatible_p (TREE_TYPE (type), TREE_TYPE (val)))
	{
	  /* Scalar boolean value should be transformed into
	     all zeros or all ones value before building a vector.  */
	  if (VECTOR_BOOLEAN_TYPE_P (type))
	    {
	      tree true_val = build_all_ones_cst (TREE_TYPE (type));
	      tree false_val = build_zero_cst (TREE_TYPE (type));

	      if (CONSTANT_CLASS_P (val))
		val = integer_zerop (val) ? false_val : true_val;
	      else
		{
		  new_temp = make_ssa_name (TREE_TYPE (type));
		  init_stmt = gimple_build_assign (new_temp, COND_EXPR,
						   val, true_val, false_val);
		  vect_init_vector_1 (stmt_info, init_stmt, gsi);
		  val = new_temp;
		}
	    }
	  else if (CONSTANT_CLASS_P (val))
	    val = fold_convert (TREE_TYPE (type), val);
	  else
	    {
	      new_temp = make_ssa_name (TREE_TYPE (type));
	      if (! INTEGRAL_TYPE_P (TREE_TYPE (val)))
		init_stmt = gimple_build_assign (new_temp,
						 fold_build1 (VIEW_CONVERT_EXPR,
							      TREE_TYPE (type),
							      val));
	      else
		init_stmt = gimple_build_assign (new_temp, NOP_EXPR, val);
	      vect_init_vector_1 (stmt_info, init_stmt, gsi);
	      val = new_temp;
	    }
	}
      val = build_vector_from_val (type, val);
    }

  new_temp = vect_get_new_ssa_name (type, vect_simple_var, "cst_");
  init_stmt = gimple_build_assign  (new_temp, val);
  vect_init_vector_1 (stmt_info, init_stmt, gsi);
  return new_temp;
}

/* Function vect_get_vec_def_for_operand_1.

   For a defining stmt DEF_STMT_INFO of a scalar stmt, return a vector def
   with type DT that will be used in the vectorized stmt.  */

tree
vect_get_vec_def_for_operand_1 (stmt_vec_info def_stmt_info,
				enum vect_def_type dt)
{
  tree vec_oprnd;
  stmt_vec_info vec_stmt_info;

  switch (dt)
    {
    /* operand is a constant or a loop invariant.  */
    case vect_constant_def:
    case vect_external_def:
      /* Code should use vect_get_vec_def_for_operand.  */
      gcc_unreachable ();

    /* operand is defined inside the loop.  */
    case vect_internal_def:
      {
        /* Get the def from the vectorized stmt.  */
	vec_stmt_info = STMT_VINFO_VEC_STMT (def_stmt_info);
	/* Get vectorized pattern statement.  */
	if (!vec_stmt_info
	    && STMT_VINFO_IN_PATTERN_P (def_stmt_info)
	    && !STMT_VINFO_RELEVANT (def_stmt_info))
	  vec_stmt_info = (STMT_VINFO_VEC_STMT
			   (STMT_VINFO_RELATED_STMT (def_stmt_info)));
	gcc_assert (vec_stmt_info);
	if (gphi *phi = dyn_cast <gphi *> (vec_stmt_info->stmt))
	  vec_oprnd = PHI_RESULT (phi);
	else
	  vec_oprnd = gimple_get_lhs (vec_stmt_info->stmt);
	return vec_oprnd;
      }

    /* operand is defined by a loop header phi.  */
    case vect_reduction_def:
    case vect_double_reduction_def:
    case vect_nested_cycle:
    case vect_induction_def:
      {
	gcc_assert (gimple_code (def_stmt_info->stmt) == GIMPLE_PHI);

	/* Get the def from the vectorized stmt.  */
	vec_stmt_info = STMT_VINFO_VEC_STMT (def_stmt_info);
	if (gphi *phi = dyn_cast <gphi *> (vec_stmt_info->stmt))
	  vec_oprnd = PHI_RESULT (phi);
	else
	  vec_oprnd = gimple_get_lhs (vec_stmt_info->stmt);
	return vec_oprnd;
      }

    default:
      gcc_unreachable ();
    }
}


/* Function vect_get_vec_def_for_operand.

   OP is an operand in STMT_VINFO.  This function returns a (vector) def
   that will be used in the vectorized stmt for STMT_VINFO.

   In the case that OP is an SSA_NAME which is defined in the loop, then
   STMT_VINFO_VEC_STMT of the defining stmt holds the relevant def.

   In case OP is an invariant or constant, a new stmt that creates a vector def
   needs to be introduced.  VECTYPE may be used to specify a required type for
   vector invariant.  */

tree
vect_get_vec_def_for_operand (tree op, stmt_vec_info stmt_vinfo, tree vectype)
{
  gimple *def_stmt;
  enum vect_def_type dt;
  bool is_simple_use;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "vect_get_vec_def_for_operand: %T\n", op);

  stmt_vec_info def_stmt_info;
  is_simple_use = vect_is_simple_use (op, loop_vinfo, &dt,
				      &def_stmt_info, &def_stmt);
  gcc_assert (is_simple_use);
  if (def_stmt && dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "  def_stmt =  %G", def_stmt);

  if (dt == vect_constant_def || dt == vect_external_def)
    {
      tree stmt_vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
      tree vector_type;

      if (vectype)
	vector_type = vectype;
      else if (VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (op))
	       && VECTOR_BOOLEAN_TYPE_P (stmt_vectype))
	vector_type = build_same_sized_truth_vector_type (stmt_vectype);
      else
	vector_type = get_vectype_for_scalar_type (TREE_TYPE (op));

      gcc_assert (vector_type);
      return vect_init_vector (stmt_vinfo, op, vector_type, NULL);
    }
  else
    return vect_get_vec_def_for_operand_1 (def_stmt_info, dt);
}


/* Function vect_get_vec_def_for_stmt_copy

   Return a vector-def for an operand.  This function is used when the
   vectorized stmt to be created (by the caller to this function) is a "copy"
   created in case the vectorized result cannot fit in one vector, and several
   copies of the vector-stmt are required.  In this case the vector-def is
   retrieved from the vector stmt recorded in the STMT_VINFO_RELATED_STMT field
   of the stmt that defines VEC_OPRND.  VINFO describes the vectorization.

   Context:
        In case the vectorization factor (VF) is bigger than the number
   of elements that can fit in a vectype (nunits), we have to generate
   more than one vector stmt to vectorize the scalar stmt.  This situation
   arises when there are multiple data-types operated upon in the loop; the
   smallest data-type determines the VF, and as a result, when vectorizing
   stmts operating on wider types we need to create 'VF/nunits' "copies" of the
   vector stmt (each computing a vector of 'nunits' results, and together
   computing 'VF' results in each iteration).  This function is called when
   vectorizing such a stmt (e.g. vectorizing S2 in the illustration below, in
   which VF=16 and nunits=4, so the number of copies required is 4):

   scalar stmt:         vectorized into:        STMT_VINFO_RELATED_STMT

   S1: x = load         VS1.0:  vx.0 = memref0      VS1.1
                        VS1.1:  vx.1 = memref1      VS1.2
                        VS1.2:  vx.2 = memref2      VS1.3
                        VS1.3:  vx.3 = memref3

   S2: z = x + ...      VSnew.0:  vz0 = vx.0 + ...  VSnew.1
                        VSnew.1:  vz1 = vx.1 + ...  VSnew.2
                        VSnew.2:  vz2 = vx.2 + ...  VSnew.3
                        VSnew.3:  vz3 = vx.3 + ...

   The vectorization of S1 is explained in vectorizable_load.
   The vectorization of S2:
        To create the first vector-stmt out of the 4 copies - VSnew.0 -
   the function 'vect_get_vec_def_for_operand' is called to
   get the relevant vector-def for each operand of S2.  For operand x it
   returns  the vector-def 'vx.0'.

        To create the remaining copies of the vector-stmt (VSnew.j), this
   function is called to get the relevant vector-def for each operand.  It is
   obtained from the respective VS1.j stmt, which is recorded in the
   STMT_VINFO_RELATED_STMT field of the stmt that defines VEC_OPRND.

        For example, to obtain the vector-def 'vx.1' in order to create the
   vector stmt 'VSnew.1', this function is called with VEC_OPRND='vx.0'.
   Given 'vx0' we obtain the stmt that defines it ('VS1.0'); from the
   STMT_VINFO_RELATED_STMT field of 'VS1.0' we obtain the next copy - 'VS1.1',
   and return its def ('vx.1').
   Overall, to create the above sequence this function will be called 3 times:
	vx.1 = vect_get_vec_def_for_stmt_copy (vinfo, vx.0);
	vx.2 = vect_get_vec_def_for_stmt_copy (vinfo, vx.1);
	vx.3 = vect_get_vec_def_for_stmt_copy (vinfo, vx.2);  */

tree
vect_get_vec_def_for_stmt_copy (vec_info *vinfo, tree vec_oprnd)
{
  stmt_vec_info def_stmt_info = vinfo->lookup_def (vec_oprnd);
  if (!def_stmt_info)
    /* Do nothing; can reuse same def.  */
    return vec_oprnd;

  def_stmt_info = STMT_VINFO_RELATED_STMT (def_stmt_info);
  gcc_assert (def_stmt_info);
  if (gphi *phi = dyn_cast <gphi *> (def_stmt_info->stmt))
    vec_oprnd = PHI_RESULT (phi);
  else
    vec_oprnd = gimple_get_lhs (def_stmt_info->stmt);
  return vec_oprnd;
}


/* Get vectorized definitions for the operands to create a copy of an original
   stmt.  See vect_get_vec_def_for_stmt_copy () for details.  */

void
vect_get_vec_defs_for_stmt_copy (vec_info *vinfo,
				 vec<tree> *vec_oprnds0,
				 vec<tree> *vec_oprnds1)
{
  tree vec_oprnd = vec_oprnds0->pop ();

  vec_oprnd = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd);
  vec_oprnds0->quick_push (vec_oprnd);

  if (vec_oprnds1 && vec_oprnds1->length ())
    {
      vec_oprnd = vec_oprnds1->pop ();
      vec_oprnd = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd);
      vec_oprnds1->quick_push (vec_oprnd);
    }
}


/* Get vectorized definitions for OP0 and OP1.  */

void
vect_get_vec_defs (tree op0, tree op1, stmt_vec_info stmt_info,
		   vec<tree> *vec_oprnds0,
		   vec<tree> *vec_oprnds1,
		   slp_tree slp_node)
{
  if (slp_node)
    {
      int nops = (op1 == NULL_TREE) ? 1 : 2;
      auto_vec<tree> ops (nops);
      auto_vec<vec<tree> > vec_defs (nops);

      ops.quick_push (op0);
      if (op1)
        ops.quick_push (op1);

      vect_get_slp_defs (ops, slp_node, &vec_defs);

      *vec_oprnds0 = vec_defs[0];
      if (op1)
	*vec_oprnds1 = vec_defs[1];
    }
  else
    {
      tree vec_oprnd;

      vec_oprnds0->create (1);
      vec_oprnd = vect_get_vec_def_for_operand (op0, stmt_info);
      vec_oprnds0->quick_push (vec_oprnd);

      if (op1)
	{
	  vec_oprnds1->create (1);
	  vec_oprnd = vect_get_vec_def_for_operand (op1, stmt_info);
	  vec_oprnds1->quick_push (vec_oprnd);
	}
    }
}

/* Helper function called by vect_finish_replace_stmt and
   vect_finish_stmt_generation.  Set the location of the new
   statement and create and return a stmt_vec_info for it.  */

static stmt_vec_info
vect_finish_stmt_generation_1 (stmt_vec_info stmt_info, gimple *vec_stmt)
{
  vec_info *vinfo = stmt_info->vinfo;

  stmt_vec_info vec_stmt_info = vinfo->add_stmt (vec_stmt);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "add new stmt: %G", vec_stmt);

  gimple_set_location (vec_stmt, gimple_location (stmt_info->stmt));

  /* While EH edges will generally prevent vectorization, stmt might
     e.g. be in a must-not-throw region.  Ensure newly created stmts
     that could throw are part of the same region.  */
  int lp_nr = lookup_stmt_eh_lp (stmt_info->stmt);
  if (lp_nr != 0 && stmt_could_throw_p (vec_stmt))
    add_stmt_to_eh_lp (vec_stmt, lp_nr);

  return vec_stmt_info;
}

/* Replace the scalar statement STMT_INFO with a new vector statement VEC_STMT,
   which sets the same scalar result as STMT_INFO did.  Create and return a
   stmt_vec_info for VEC_STMT.  */

stmt_vec_info
vect_finish_replace_stmt (stmt_vec_info stmt_info, gimple *vec_stmt)
{
  gcc_assert (gimple_get_lhs (stmt_info->stmt) == gimple_get_lhs (vec_stmt));

  gimple_stmt_iterator gsi = gsi_for_stmt (stmt_info->stmt);
  gsi_replace (&gsi, vec_stmt, false);

  return vect_finish_stmt_generation_1 (stmt_info, vec_stmt);
}

/* Add VEC_STMT to the vectorized implementation of STMT_INFO and insert it
   before *GSI.  Create and return a stmt_vec_info for VEC_STMT.  */

stmt_vec_info
vect_finish_stmt_generation (stmt_vec_info stmt_info, gimple *vec_stmt,
			     gimple_stmt_iterator *gsi)
{
  gcc_assert (gimple_code (stmt_info->stmt) != GIMPLE_LABEL);

  if (!gsi_end_p (*gsi)
      && gimple_has_mem_ops (vec_stmt))
    {
      gimple *at_stmt = gsi_stmt (*gsi);
      tree vuse = gimple_vuse (at_stmt);
      if (vuse && TREE_CODE (vuse) == SSA_NAME)
	{
	  tree vdef = gimple_vdef (at_stmt);
	  gimple_set_vuse (vec_stmt, gimple_vuse (at_stmt));
	  /* If we have an SSA vuse and insert a store, update virtual
	     SSA form to avoid triggering the renamer.  Do so only
	     if we can easily see all uses - which is what almost always
	     happens with the way vectorized stmts are inserted.  */
	  if ((vdef && TREE_CODE (vdef) == SSA_NAME)
	      && ((is_gimple_assign (vec_stmt)
		   && !is_gimple_reg (gimple_assign_lhs (vec_stmt)))
		  || (is_gimple_call (vec_stmt)
		      && !(gimple_call_flags (vec_stmt)
			   & (ECF_CONST|ECF_PURE|ECF_NOVOPS)))))
	    {
	      tree new_vdef = copy_ssa_name (vuse, vec_stmt);
	      gimple_set_vdef (vec_stmt, new_vdef);
	      SET_USE (gimple_vuse_op (at_stmt), new_vdef);
	    }
	}
    }
  gsi_insert_before (gsi, vec_stmt, GSI_SAME_STMT);
  return vect_finish_stmt_generation_1 (stmt_info, vec_stmt);
}

/* We want to vectorize a call to combined function CFN with function
   decl FNDECL, using VECTYPE_OUT as the type of the output and VECTYPE_IN
   as the types of all inputs.  Check whether this is possible using
   an internal function, returning its code if so or IFN_LAST if not.  */

static internal_fn
vectorizable_internal_function (combined_fn cfn, tree fndecl,
				tree vectype_out, tree vectype_in)
{
  internal_fn ifn;
  if (internal_fn_p (cfn))
    ifn = as_internal_fn (cfn);
  else
    ifn = associated_internal_fn (fndecl);
  if (ifn != IFN_LAST && direct_internal_fn_p (ifn))
    {
      const direct_internal_fn_info &info = direct_internal_fn (ifn);
      if (info.vectorizable)
	{
	  tree type0 = (info.type0 < 0 ? vectype_out : vectype_in);
	  tree type1 = (info.type1 < 0 ? vectype_out : vectype_in);
	  if (direct_internal_fn_supported_p (ifn, tree_pair (type0, type1),
					      OPTIMIZE_FOR_SPEED))
	    return ifn;
	}
    }
  return IFN_LAST;
}


static tree permute_vec_elements (tree, tree, tree, stmt_vec_info,
				  gimple_stmt_iterator *);

/* Check whether a load or store statement in the loop described by
   LOOP_VINFO is possible in a fully-masked loop.  This is testing
   whether the vectorizer pass has the appropriate support, as well as
   whether the target does.

   VLS_TYPE says whether the statement is a load or store and VECTYPE
   is the type of the vector being loaded or stored.  MEMORY_ACCESS_TYPE
   says how the load or store is going to be implemented and GROUP_SIZE
   is the number of load or store statements in the containing group.
   If the access is a gather load or scatter store, GS_INFO describes
   its arguments.

   Clear LOOP_VINFO_CAN_FULLY_MASK_P if a fully-masked loop is not
   supported, otherwise record the required mask types.  */

static void
check_load_store_masking (loop_vec_info loop_vinfo, tree vectype,
			  vec_load_store_type vls_type, int group_size,
			  vect_memory_access_type memory_access_type,
			  gather_scatter_info *gs_info)
{
  /* Invariant loads need no special support.  */
  if (memory_access_type == VMAT_INVARIANT)
    return;

  vec_loop_masks *masks = &LOOP_VINFO_MASKS (loop_vinfo);
  machine_mode vecmode = TYPE_MODE (vectype);
  bool is_load = (vls_type == VLS_LOAD);
  if (memory_access_type == VMAT_LOAD_STORE_LANES)
    {
      if (is_load
	  ? !vect_load_lanes_supported (vectype, group_size, true)
	  : !vect_store_lanes_supported (vectype, group_size, true))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "can't use a fully-masked loop because the"
			     " target doesn't have an appropriate masked"
			     " load/store-lanes instruction.\n");
	  LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
	  return;
	}
      unsigned int ncopies = vect_get_num_copies (loop_vinfo, vectype);
      vect_record_loop_mask (loop_vinfo, masks, ncopies, vectype);
      return;
    }

  if (memory_access_type == VMAT_GATHER_SCATTER)
    {
      internal_fn ifn = (is_load
			 ? IFN_MASK_GATHER_LOAD
			 : IFN_MASK_SCATTER_STORE);
      tree offset_type = TREE_TYPE (gs_info->offset);
      if (!internal_gather_scatter_fn_supported_p (ifn, vectype,
						   gs_info->memory_type,
						   TYPE_SIGN (offset_type),
						   gs_info->scale))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "can't use a fully-masked loop because the"
			     " target doesn't have an appropriate masked"
			     " gather load or scatter store instruction.\n");
	  LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
	  return;
	}
      unsigned int ncopies = vect_get_num_copies (loop_vinfo, vectype);
      vect_record_loop_mask (loop_vinfo, masks, ncopies, vectype);
      return;
    }

  if (memory_access_type != VMAT_CONTIGUOUS
      && memory_access_type != VMAT_CONTIGUOUS_PERMUTE)
    {
      /* Element X of the data must come from iteration i * VF + X of the
	 scalar loop.  We need more work to support other mappings.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't use a fully-masked loop because an access"
			 " isn't contiguous.\n");
      LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
      return;
    }

  machine_mode mask_mode;
  if (!(targetm.vectorize.get_mask_mode
	(GET_MODE_NUNITS (vecmode),
	 GET_MODE_SIZE (vecmode)).exists (&mask_mode))
      || !can_vec_mask_load_store_p (vecmode, mask_mode, is_load))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't use a fully-masked loop because the target"
			 " doesn't have the appropriate masked load or"
			 " store.\n");
      LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo) = false;
      return;
    }
  /* We might load more scalars than we need for permuting SLP loads.
     We checked in get_group_load_store_type that the extra elements
     don't leak into a new vector.  */
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  unsigned int nvectors;
  if (can_div_away_from_zero_p (group_size * vf, nunits, &nvectors))
    vect_record_loop_mask (loop_vinfo, masks, nvectors, vectype);
  else
    gcc_unreachable ();
}

/* Return the mask input to a masked load or store.  VEC_MASK is the vectorized
   form of the scalar mask condition and LOOP_MASK, if nonnull, is the mask
   that needs to be applied to all loads and stores in a vectorized loop.
   Return VEC_MASK if LOOP_MASK is null, otherwise return VEC_MASK & LOOP_MASK.

   MASK_TYPE is the type of both masks.  If new statements are needed,
   insert them before GSI.  */

static tree
prepare_load_store_mask (tree mask_type, tree loop_mask, tree vec_mask,
			 gimple_stmt_iterator *gsi)
{
  gcc_assert (useless_type_conversion_p (mask_type, TREE_TYPE (vec_mask)));
  if (!loop_mask)
    return vec_mask;

  gcc_assert (TREE_TYPE (loop_mask) == mask_type);
  tree and_res = make_temp_ssa_name (mask_type, NULL, "vec_mask_and");
  gimple *and_stmt = gimple_build_assign (and_res, BIT_AND_EXPR,
					  vec_mask, loop_mask);
  gsi_insert_before (gsi, and_stmt, GSI_SAME_STMT);
  return and_res;
}

/* Determine whether we can use a gather load or scatter store to vectorize
   strided load or store STMT_INFO by truncating the current offset to a
   smaller width.  We need to be able to construct an offset vector:

     { 0, X, X*2, X*3, ... }

   without loss of precision, where X is STMT_INFO's DR_STEP.

   Return true if this is possible, describing the gather load or scatter
   store in GS_INFO.  MASKED_P is true if the load or store is conditional.  */

static bool
vect_truncate_gather_scatter_offset (stmt_vec_info stmt_info,
				     loop_vec_info loop_vinfo, bool masked_p,
				     gather_scatter_info *gs_info)
{
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  data_reference *dr = dr_info->dr;
  tree step = DR_STEP (dr);
  if (TREE_CODE (step) != INTEGER_CST)
    {
      /* ??? Perhaps we could use range information here?  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "cannot truncate variable step.\n");
      return false;
    }

  /* Get the number of bits in an element.  */
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  scalar_mode element_mode = SCALAR_TYPE_MODE (TREE_TYPE (vectype));
  unsigned int element_bits = GET_MODE_BITSIZE (element_mode);

  /* Set COUNT to the upper limit on the number of elements - 1.
     Start with the maximum vectorization factor.  */
  unsigned HOST_WIDE_INT count = vect_max_vf (loop_vinfo) - 1;

  /* Try lowering COUNT to the number of scalar latch iterations.  */
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  widest_int max_iters;
  if (max_loop_iterations (loop, &max_iters)
      && max_iters < count)
    count = max_iters.to_shwi ();

  /* Try scales of 1 and the element size.  */
  int scales[] = { 1, vect_get_scalar_dr_size (dr_info) };
  wi::overflow_type overflow = wi::OVF_NONE;
  for (int i = 0; i < 2; ++i)
    {
      int scale = scales[i];
      widest_int factor;
      if (!wi::multiple_of_p (wi::to_widest (step), scale, SIGNED, &factor))
	continue;

      /* See whether we can calculate (COUNT - 1) * STEP / SCALE
	 in OFFSET_BITS bits.  */
      widest_int range = wi::mul (count, factor, SIGNED, &overflow);
      if (overflow)
	continue;
      signop sign = range >= 0 ? UNSIGNED : SIGNED;
      if (wi::min_precision (range, sign) > element_bits)
	{
	  overflow = wi::OVF_UNKNOWN;
	  continue;
	}

      /* See whether the target supports the operation.  */
      tree memory_type = TREE_TYPE (DR_REF (dr));
      if (!vect_gather_scatter_fn_p (DR_IS_READ (dr), masked_p, vectype,
				     memory_type, element_bits, sign, scale,
				     &gs_info->ifn, &gs_info->element_type))
	continue;

      tree offset_type = build_nonstandard_integer_type (element_bits,
							 sign == UNSIGNED);

      gs_info->decl = NULL_TREE;
      /* Logically the sum of DR_BASE_ADDRESS, DR_INIT and DR_OFFSET,
	 but we don't need to store that here.  */
      gs_info->base = NULL_TREE;
      gs_info->offset = fold_convert (offset_type, step);
      gs_info->offset_dt = vect_constant_def;
      gs_info->offset_vectype = NULL_TREE;
      gs_info->scale = scale;
      gs_info->memory_type = memory_type;
      return true;
    }

  if (overflow && dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "truncating gather/scatter offset to %d bits"
		     " might change its value.\n", element_bits);

  return false;
}

/* Return true if we can use gather/scatter internal functions to
   vectorize STMT_INFO, which is a grouped or strided load or store.
   MASKED_P is true if load or store is conditional.  When returning
   true, fill in GS_INFO with the information required to perform the
   operation.  */

static bool
vect_use_strided_gather_scatters_p (stmt_vec_info stmt_info,
				    loop_vec_info loop_vinfo, bool masked_p,
				    gather_scatter_info *gs_info)
{
  if (!vect_check_gather_scatter (stmt_info, loop_vinfo, gs_info)
      || gs_info->decl)
    return vect_truncate_gather_scatter_offset (stmt_info, loop_vinfo,
						masked_p, gs_info);

  scalar_mode element_mode = SCALAR_TYPE_MODE (gs_info->element_type);
  unsigned int element_bits = GET_MODE_BITSIZE (element_mode);
  tree offset_type = TREE_TYPE (gs_info->offset);
  unsigned int offset_bits = TYPE_PRECISION (offset_type);

  /* Enforced by vect_check_gather_scatter.  */
  gcc_assert (element_bits >= offset_bits);

  /* If the elements are wider than the offset, convert the offset to the
     same width, without changing its sign.  */
  if (element_bits > offset_bits)
    {
      bool unsigned_p = TYPE_UNSIGNED (offset_type);
      offset_type = build_nonstandard_integer_type (element_bits, unsigned_p);
      gs_info->offset = fold_convert (offset_type, gs_info->offset);
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "using gather/scatter for strided/grouped access,"
		     " scale = %d\n", gs_info->scale);

  return true;
}

/* STMT_INFO is a non-strided load or store, meaning that it accesses
   elements with a known constant step.  Return -1 if that step
   is negative, 0 if it is zero, and 1 if it is greater than zero.  */

static int
compare_step_with_zero (stmt_vec_info stmt_info)
{
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  return tree_int_cst_compare (vect_dr_behavior (dr_info)->step,
			       size_zero_node);
}

/* If the target supports a permute mask that reverses the elements in
   a vector of type VECTYPE, return that mask, otherwise return null.  */

static tree
perm_mask_for_reverse (tree vectype)
{
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* The encoding has a single stepped pattern.  */
  vec_perm_builder sel (nunits, 1, 3);
  for (int i = 0; i < 3; ++i)
    sel.quick_push (nunits - 1 - i);

  vec_perm_indices indices (sel, 1, nunits);
  if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
    return NULL_TREE;
  return vect_gen_perm_mask_checked (vectype, indices);
}

/* STMT_INFO is either a masked or unconditional store.  Return the value
   being stored.  */

tree
vect_get_store_rhs (stmt_vec_info stmt_info)
{
  if (gassign *assign = dyn_cast <gassign *> (stmt_info->stmt))
    {
      gcc_assert (gimple_assign_single_p (assign));
      return gimple_assign_rhs1 (assign);
    }
  if (gcall *call = dyn_cast <gcall *> (stmt_info->stmt))
    {
      internal_fn ifn = gimple_call_internal_fn (call);
      int index = internal_fn_stored_value_index (ifn);
      gcc_assert (index >= 0);
      return gimple_call_arg (call, index);
    }
  gcc_unreachable ();
}

/* A subroutine of get_load_store_type, with a subset of the same
   arguments.  Handle the case where STMT_INFO is part of a grouped load
   or store.

   For stores, the statements in the group are all consecutive
   and there is no gap at the end.  For loads, the statements in the
   group might not be consecutive; there can be gaps between statements
   as well as at the end.  */

static bool
get_group_load_store_type (stmt_vec_info stmt_info, tree vectype, bool slp,
			   bool masked_p, vec_load_store_type vls_type,
			   vect_memory_access_type *memory_access_type,
			   gather_scatter_info *gs_info)
{
  vec_info *vinfo = stmt_info->vinfo;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = loop_vinfo ? LOOP_VINFO_LOOP (loop_vinfo) : NULL;
  stmt_vec_info first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
  dr_vec_info *first_dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
  unsigned int group_size = DR_GROUP_SIZE (first_stmt_info);
  bool single_element_p = (stmt_info == first_stmt_info
			   && !DR_GROUP_NEXT_ELEMENT (stmt_info));
  unsigned HOST_WIDE_INT gap = DR_GROUP_GAP (first_stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* True if the vectorized statements would access beyond the last
     statement in the group.  */
  bool overrun_p = false;

  /* True if we can cope with such overrun by peeling for gaps, so that
     there is at least one final scalar iteration after the vector loop.  */
  bool can_overrun_p = (!masked_p
			&& vls_type == VLS_LOAD
			&& loop_vinfo
			&& !loop->inner);

  /* There can only be a gap at the end of the group if the stride is
     known at compile time.  */
  gcc_assert (!STMT_VINFO_STRIDED_P (first_stmt_info) || gap == 0);

  /* Stores can't yet have gaps.  */
  gcc_assert (slp || vls_type == VLS_LOAD || gap == 0);

  if (slp)
    {
      if (STMT_VINFO_STRIDED_P (first_stmt_info))
	{
	  /* Try to use consecutive accesses of DR_GROUP_SIZE elements,
	     separated by the stride, until we have a complete vector.
	     Fall back to scalar accesses if that isn't possible.  */
	  if (multiple_p (nunits, group_size))
	    *memory_access_type = VMAT_STRIDED_SLP;
	  else
	    *memory_access_type = VMAT_ELEMENTWISE;
	}
      else
	{
	  overrun_p = loop_vinfo && gap != 0;
	  if (overrun_p && vls_type != VLS_LOAD)
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Grouped store with gaps requires"
			       " non-consecutive accesses\n");
	      return false;
	    }
	  /* An overrun is fine if the trailing elements are smaller
	     than the alignment boundary B.  Every vector access will
	     be a multiple of B and so we are guaranteed to access a
	     non-gap element in the same B-sized block.  */
	  if (overrun_p
	      && gap < (vect_known_alignment_in_bytes (first_dr_info)
			/ vect_get_scalar_dr_size (first_dr_info)))
	    overrun_p = false;
	  if (overrun_p && !can_overrun_p)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Peeling for outer loop is not supported\n");
	      return false;
	    }
	  *memory_access_type = VMAT_CONTIGUOUS;
	}
    }
  else
    {
      /* We can always handle this case using elementwise accesses,
	 but see if something more efficient is available.  */
      *memory_access_type = VMAT_ELEMENTWISE;

      /* If there is a gap at the end of the group then these optimizations
	 would access excess elements in the last iteration.  */
      bool would_overrun_p = (gap != 0);
      /* An overrun is fine if the trailing elements are smaller than the
	 alignment boundary B.  Every vector access will be a multiple of B
	 and so we are guaranteed to access a non-gap element in the
	 same B-sized block.  */
      if (would_overrun_p
	  && !masked_p
	  && gap < (vect_known_alignment_in_bytes (first_dr_info)
		    / vect_get_scalar_dr_size (first_dr_info)))
	would_overrun_p = false;

      if (!STMT_VINFO_STRIDED_P (first_stmt_info)
	  && (can_overrun_p || !would_overrun_p)
	  && compare_step_with_zero (stmt_info) > 0)
	{
	  /* First cope with the degenerate case of a single-element
	     vector.  */
	  if (known_eq (TYPE_VECTOR_SUBPARTS (vectype), 1U))
	    *memory_access_type = VMAT_CONTIGUOUS;

	  /* Otherwise try using LOAD/STORE_LANES.  */
	  if (*memory_access_type == VMAT_ELEMENTWISE
	      && (vls_type == VLS_LOAD
		  ? vect_load_lanes_supported (vectype, group_size, masked_p)
		  : vect_store_lanes_supported (vectype, group_size,
						masked_p)))
	    {
	      *memory_access_type = VMAT_LOAD_STORE_LANES;
	      overrun_p = would_overrun_p;
	    }

	  /* If that fails, try using permuting loads.  */
	  if (*memory_access_type == VMAT_ELEMENTWISE
	      && (vls_type == VLS_LOAD
		  ? vect_grouped_load_supported (vectype, single_element_p,
						 group_size)
		  : vect_grouped_store_supported (vectype, group_size)))
	    {
	      *memory_access_type = VMAT_CONTIGUOUS_PERMUTE;
	      overrun_p = would_overrun_p;
	    }
	}

      /* As a last resort, trying using a gather load or scatter store.

	 ??? Although the code can handle all group sizes correctly,
	 it probably isn't a win to use separate strided accesses based
	 on nearby locations.  Or, even if it's a win over scalar code,
	 it might not be a win over vectorizing at a lower VF, if that
	 allows us to use contiguous accesses.  */
      if (*memory_access_type == VMAT_ELEMENTWISE
	  && single_element_p
	  && loop_vinfo
	  && vect_use_strided_gather_scatters_p (stmt_info, loop_vinfo,
						 masked_p, gs_info))
	*memory_access_type = VMAT_GATHER_SCATTER;
    }

  if (vls_type != VLS_LOAD && first_stmt_info == stmt_info)
    {
      /* STMT is the leader of the group. Check the operands of all the
	 stmts of the group.  */
      stmt_vec_info next_stmt_info = DR_GROUP_NEXT_ELEMENT (stmt_info);
      while (next_stmt_info)
	{
	  tree op = vect_get_store_rhs (next_stmt_info);
	  enum vect_def_type dt;
	  if (!vect_is_simple_use (op, vinfo, &dt))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "use not simple.\n");
	      return false;
	    }
	  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
	}
    }

  if (overrun_p)
    {
      gcc_assert (can_overrun_p);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Data access with gaps requires scalar "
			 "epilogue loop\n");
      LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) = true;
    }

  return true;
}

/* A subroutine of get_load_store_type, with a subset of the same
   arguments.  Handle the case where STMT_INFO is a load or store that
   accesses consecutive elements with a negative step.  */

static vect_memory_access_type
get_negative_load_store_type (stmt_vec_info stmt_info, tree vectype,
			      vec_load_store_type vls_type,
			      unsigned int ncopies)
{
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  dr_alignment_support alignment_support_scheme;

  if (ncopies > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "multiple types with negative step.\n");
      return VMAT_ELEMENTWISE;
    }

  alignment_support_scheme = vect_supportable_dr_alignment (dr_info, false);
  if (alignment_support_scheme != dr_aligned
      && alignment_support_scheme != dr_unaligned_supported)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "negative step but alignment required.\n");
      return VMAT_ELEMENTWISE;
    }

  if (vls_type == VLS_STORE_INVARIANT)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "negative step with invariant source;"
			 " no permute needed.\n");
      return VMAT_CONTIGUOUS_DOWN;
    }

  if (!perm_mask_for_reverse (vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "negative step and reversing not supported.\n");
      return VMAT_ELEMENTWISE;
    }

  return VMAT_CONTIGUOUS_REVERSE;
}

/* Analyze load or store statement STMT_INFO of type VLS_TYPE.  Return true
   if there is a memory access type that the vectorized form can use,
   storing it in *MEMORY_ACCESS_TYPE if so.  If we decide to use gathers
   or scatters, fill in GS_INFO accordingly.

   SLP says whether we're performing SLP rather than loop vectorization.
   MASKED_P is true if the statement is conditional on a vectorized mask.
   VECTYPE is the vector type that the vectorized statements will use.
   NCOPIES is the number of vector statements that will be needed.  */

static bool
get_load_store_type (stmt_vec_info stmt_info, tree vectype, bool slp,
		     bool masked_p, vec_load_store_type vls_type,
		     unsigned int ncopies,
		     vect_memory_access_type *memory_access_type,
		     gather_scatter_info *gs_info)
{
  vec_info *vinfo = stmt_info->vinfo;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    {
      *memory_access_type = VMAT_GATHER_SCATTER;
      if (!vect_check_gather_scatter (stmt_info, loop_vinfo, gs_info))
	gcc_unreachable ();
      else if (!vect_is_simple_use (gs_info->offset, vinfo,
				    &gs_info->offset_dt,
				    &gs_info->offset_vectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "%s index use not simple.\n",
			     vls_type == VLS_LOAD ? "gather" : "scatter");
	  return false;
	}
    }
  else if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      if (!get_group_load_store_type (stmt_info, vectype, slp, masked_p,
				      vls_type, memory_access_type, gs_info))
	return false;
    }
  else if (STMT_VINFO_STRIDED_P (stmt_info))
    {
      gcc_assert (!slp);
      if (loop_vinfo
	  && vect_use_strided_gather_scatters_p (stmt_info, loop_vinfo,
						 masked_p, gs_info))
	*memory_access_type = VMAT_GATHER_SCATTER;
      else
	*memory_access_type = VMAT_ELEMENTWISE;
    }
  else
    {
      int cmp = compare_step_with_zero (stmt_info);
      if (cmp < 0)
	*memory_access_type = get_negative_load_store_type
	  (stmt_info, vectype, vls_type, ncopies);
      else if (cmp == 0)
	{
	  gcc_assert (vls_type == VLS_LOAD);
	  *memory_access_type = VMAT_INVARIANT;
	}
      else
	*memory_access_type = VMAT_CONTIGUOUS;
    }

  if ((*memory_access_type == VMAT_ELEMENTWISE
       || *memory_access_type == VMAT_STRIDED_SLP)
      && !nunits.is_constant ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Not using elementwise accesses due to variable "
			 "vectorization factor.\n");
      return false;
    }

  /* FIXME: At the moment the cost model seems to underestimate the
     cost of using elementwise accesses.  This check preserves the
     traditional behavior until that can be fixed.  */
  stmt_vec_info first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
  if (!first_stmt_info)
    first_stmt_info = stmt_info;
  if (*memory_access_type == VMAT_ELEMENTWISE
      && !STMT_VINFO_STRIDED_P (first_stmt_info)
      && !(stmt_info == DR_GROUP_FIRST_ELEMENT (stmt_info)
	   && !DR_GROUP_NEXT_ELEMENT (stmt_info)
	   && !pow2p_hwi (DR_GROUP_SIZE (stmt_info))))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not falling back to elementwise accesses\n");
      return false;
    }
  return true;
}

/* Return true if boolean argument MASK is suitable for vectorizing
   conditional load or store STMT_INFO.  When returning true, store the type
   of the definition in *MASK_DT_OUT and the type of the vectorized mask
   in *MASK_VECTYPE_OUT.  */

static bool
vect_check_load_store_mask (stmt_vec_info stmt_info, tree mask,
			    vect_def_type *mask_dt_out,
			    tree *mask_vectype_out)
{
  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (mask)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mask argument is not a boolean.\n");
      return false;
    }

  if (TREE_CODE (mask) != SSA_NAME)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mask argument is not an SSA name.\n");
      return false;
    }

  enum vect_def_type mask_dt;
  tree mask_vectype;
  if (!vect_is_simple_use (mask, stmt_info->vinfo, &mask_dt, &mask_vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mask use not simple.\n");
      return false;
    }

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (!mask_vectype)
    mask_vectype = get_mask_type_for_scalar_type (TREE_TYPE (vectype));

  if (!mask_vectype || !VECTOR_BOOLEAN_TYPE_P (mask_vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "could not find an appropriate vector mask type.\n");
      return false;
    }

  if (maybe_ne (TYPE_VECTOR_SUBPARTS (mask_vectype),
		TYPE_VECTOR_SUBPARTS (vectype)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "vector mask type %T",
			 " does not match vector data type %T.\n",
			 mask_vectype, vectype);

      return false;
    }

  *mask_dt_out = mask_dt;
  *mask_vectype_out = mask_vectype;
  return true;
}

/* Return true if stored value RHS is suitable for vectorizing store
   statement STMT_INFO.  When returning true, store the type of the
   definition in *RHS_DT_OUT, the type of the vectorized store value in
   *RHS_VECTYPE_OUT and the type of the store in *VLS_TYPE_OUT.  */

static bool
vect_check_store_rhs (stmt_vec_info stmt_info, tree rhs,
		      vect_def_type *rhs_dt_out, tree *rhs_vectype_out,
		      vec_load_store_type *vls_type_out)
{
  /* In the case this is a store from a constant make sure
     native_encode_expr can handle it.  */
  if (CONSTANT_CLASS_P (rhs) && native_encode_expr (rhs, NULL, 64) == 0)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "cannot encode constant as a byte sequence.\n");
      return false;
    }

  enum vect_def_type rhs_dt;
  tree rhs_vectype;
  if (!vect_is_simple_use (rhs, stmt_info->vinfo, &rhs_dt, &rhs_vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "use not simple.\n");
      return false;
    }

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (rhs_vectype && !useless_type_conversion_p (vectype, rhs_vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "incompatible vector types.\n");
      return false;
    }

  *rhs_dt_out = rhs_dt;
  *rhs_vectype_out = rhs_vectype;
  if (rhs_dt == vect_constant_def || rhs_dt == vect_external_def)
    *vls_type_out = VLS_STORE_INVARIANT;
  else
    *vls_type_out = VLS_STORE;
  return true;
}

/* Build an all-ones vector mask of type MASKTYPE while vectorizing STMT_INFO.
   Note that we support masks with floating-point type, in which case the
   floats are interpreted as a bitmask.  */

static tree
vect_build_all_ones_mask (stmt_vec_info stmt_info, tree masktype)
{
  if (TREE_CODE (masktype) == INTEGER_TYPE)
    return build_int_cst (masktype, -1);
  else if (TREE_CODE (TREE_TYPE (masktype)) == INTEGER_TYPE)
    {
      tree mask = build_int_cst (TREE_TYPE (masktype), -1);
      mask = build_vector_from_val (masktype, mask);
      return vect_init_vector (stmt_info, mask, masktype, NULL);
    }
  else if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (masktype)))
    {
      REAL_VALUE_TYPE r;
      long tmp[6];
      for (int j = 0; j < 6; ++j)
	tmp[j] = -1;
      real_from_target (&r, tmp, TYPE_MODE (TREE_TYPE (masktype)));
      tree mask = build_real (TREE_TYPE (masktype), r);
      mask = build_vector_from_val (masktype, mask);
      return vect_init_vector (stmt_info, mask, masktype, NULL);
    }
  gcc_unreachable ();
}

/* Build an all-zero merge value of type VECTYPE while vectorizing
   STMT_INFO as a gather load.  */

static tree
vect_build_zero_merge_argument (stmt_vec_info stmt_info, tree vectype)
{
  tree merge;
  if (TREE_CODE (TREE_TYPE (vectype)) == INTEGER_TYPE)
    merge = build_int_cst (TREE_TYPE (vectype), 0);
  else if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (vectype)))
    {
      REAL_VALUE_TYPE r;
      long tmp[6];
      for (int j = 0; j < 6; ++j)
	tmp[j] = 0;
      real_from_target (&r, tmp, TYPE_MODE (TREE_TYPE (vectype)));
      merge = build_real (TREE_TYPE (vectype), r);
    }
  else
    gcc_unreachable ();
  merge = build_vector_from_val (vectype, merge);
  return vect_init_vector (stmt_info, merge, vectype, NULL);
}

/* Build a gather load call while vectorizing STMT_INFO.  Insert new
   instructions before GSI and add them to VEC_STMT.  GS_INFO describes
   the gather load operation.  If the load is conditional, MASK is the
   unvectorized condition and MASK_DT is its definition type, otherwise
   MASK is null.  */

static void
vect_build_gather_load_calls (stmt_vec_info stmt_info,
			      gimple_stmt_iterator *gsi,
			      stmt_vec_info *vec_stmt,
			      gather_scatter_info *gs_info,
			      tree mask)
{
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = vect_get_num_copies (loop_vinfo, vectype);
  edge pe = loop_preheader_edge (loop);
  enum { NARROW, NONE, WIDEN } modifier;
  poly_uint64 gather_off_nunits
    = TYPE_VECTOR_SUBPARTS (gs_info->offset_vectype);

  tree arglist = TYPE_ARG_TYPES (TREE_TYPE (gs_info->decl));
  tree rettype = TREE_TYPE (TREE_TYPE (gs_info->decl));
  tree srctype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree ptrtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree idxtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree masktype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree scaletype = TREE_VALUE (arglist);
  gcc_checking_assert (types_compatible_p (srctype, rettype)
		       && (!mask || types_compatible_p (srctype, masktype)));

  tree perm_mask = NULL_TREE;
  tree mask_perm_mask = NULL_TREE;
  if (known_eq (nunits, gather_off_nunits))
    modifier = NONE;
  else if (known_eq (nunits * 2, gather_off_nunits))
    {
      modifier = WIDEN;

      /* Currently widening gathers and scatters are only supported for
	 fixed-length vectors.  */
      int count = gather_off_nunits.to_constant ();
      vec_perm_builder sel (count, count, 1);
      for (int i = 0; i < count; ++i)
	sel.quick_push (i | (count / 2));

      vec_perm_indices indices (sel, 1, count);
      perm_mask = vect_gen_perm_mask_checked (gs_info->offset_vectype,
					      indices);
    }
  else if (known_eq (nunits, gather_off_nunits * 2))
    {
      modifier = NARROW;

      /* Currently narrowing gathers and scatters are only supported for
	 fixed-length vectors.  */
      int count = nunits.to_constant ();
      vec_perm_builder sel (count, count, 1);
      sel.quick_grow (count);
      for (int i = 0; i < count; ++i)
	sel[i] = i < count / 2 ? i : i + count / 2;
      vec_perm_indices indices (sel, 2, count);
      perm_mask = vect_gen_perm_mask_checked (vectype, indices);

      ncopies *= 2;

      if (mask)
	{
	  for (int i = 0; i < count; ++i)
	    sel[i] = i | (count / 2);
	  indices.new_vector (sel, 2, count);
	  mask_perm_mask = vect_gen_perm_mask_checked (masktype, indices);
	}
    }
  else
    gcc_unreachable ();

  tree scalar_dest = gimple_get_lhs (stmt_info->stmt);
  tree vec_dest = vect_create_destination_var (scalar_dest, vectype);

  tree ptr = fold_convert (ptrtype, gs_info->base);
  if (!is_gimple_min_invariant (ptr))
    {
      gimple_seq seq;
      ptr = force_gimple_operand (ptr, &seq, true, NULL_TREE);
      basic_block new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
      gcc_assert (!new_bb);
    }

  tree scale = build_int_cst (scaletype, gs_info->scale);

  tree vec_oprnd0 = NULL_TREE;
  tree vec_mask = NULL_TREE;
  tree src_op = NULL_TREE;
  tree mask_op = NULL_TREE;
  tree prev_res = NULL_TREE;
  stmt_vec_info prev_stmt_info = NULL;

  if (!mask)
    {
      src_op = vect_build_zero_merge_argument (stmt_info, rettype);
      mask_op = vect_build_all_ones_mask (stmt_info, masktype);
    }

  for (int j = 0; j < ncopies; ++j)
    {
      tree op, var;
      if (modifier == WIDEN && (j & 1))
	op = permute_vec_elements (vec_oprnd0, vec_oprnd0,
				   perm_mask, stmt_info, gsi);
      else if (j == 0)
	op = vec_oprnd0
	  = vect_get_vec_def_for_operand (gs_info->offset, stmt_info);
      else
	op = vec_oprnd0 = vect_get_vec_def_for_stmt_copy (loop_vinfo,
							  vec_oprnd0);

      if (!useless_type_conversion_p (idxtype, TREE_TYPE (op)))
	{
	  gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (op)),
				TYPE_VECTOR_SUBPARTS (idxtype)));
	  var = vect_get_new_ssa_name (idxtype, vect_simple_var);
	  op = build1 (VIEW_CONVERT_EXPR, idxtype, op);
	  gassign *new_stmt = gimple_build_assign (var, VIEW_CONVERT_EXPR, op);
	  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	  op = var;
	}

      if (mask)
	{
	  if (mask_perm_mask && (j & 1))
	    mask_op = permute_vec_elements (mask_op, mask_op,
					    mask_perm_mask, stmt_info, gsi);
	  else
	    {
	      if (j == 0)
		vec_mask = vect_get_vec_def_for_operand (mask, stmt_info);
	      else
		vec_mask = vect_get_vec_def_for_stmt_copy (loop_vinfo,
							   vec_mask);

	      mask_op = vec_mask;
	      if (!useless_type_conversion_p (masktype, TREE_TYPE (vec_mask)))
		{
		  gcc_assert
		    (known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (mask_op)),
			       TYPE_VECTOR_SUBPARTS (masktype)));
		  var = vect_get_new_ssa_name (masktype, vect_simple_var);
		  mask_op = build1 (VIEW_CONVERT_EXPR, masktype, mask_op);
		  gassign *new_stmt
		    = gimple_build_assign (var, VIEW_CONVERT_EXPR, mask_op);
		  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		  mask_op = var;
		}
	    }
	  src_op = mask_op;
	}

      gcall *new_call = gimple_build_call (gs_info->decl, 5, src_op, ptr, op,
					   mask_op, scale);

      stmt_vec_info new_stmt_info;
      if (!useless_type_conversion_p (vectype, rettype))
	{
	  gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (vectype),
				TYPE_VECTOR_SUBPARTS (rettype)));
	  op = vect_get_new_ssa_name (rettype, vect_simple_var);
	  gimple_call_set_lhs (new_call, op);
	  vect_finish_stmt_generation (stmt_info, new_call, gsi);
	  var = make_ssa_name (vec_dest);
	  op = build1 (VIEW_CONVERT_EXPR, vectype, op);
	  gassign *new_stmt = gimple_build_assign (var, VIEW_CONVERT_EXPR, op);
	  new_stmt_info
	    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	}
      else
	{
	  var = make_ssa_name (vec_dest, new_call);
	  gimple_call_set_lhs (new_call, var);
	  new_stmt_info
	    = vect_finish_stmt_generation (stmt_info, new_call, gsi);
	}

      if (modifier == NARROW)
	{
	  if ((j & 1) == 0)
	    {
	      prev_res = var;
	      continue;
	    }
	  var = permute_vec_elements (prev_res, var, perm_mask,
				      stmt_info, gsi);
	  new_stmt_info = loop_vinfo->lookup_def (var);
	}

      if (prev_stmt_info == NULL)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
      prev_stmt_info = new_stmt_info;
    }
}

/* Prepare the base and offset in GS_INFO for vectorization.
   Set *DATAREF_PTR to the loop-invariant base address and *VEC_OFFSET
   to the vectorized offset argument for the first copy of STMT_INFO.
   STMT_INFO is the statement described by GS_INFO and LOOP is the
   containing loop.  */

static void
vect_get_gather_scatter_ops (struct loop *loop, stmt_vec_info stmt_info,
			     gather_scatter_info *gs_info,
			     tree *dataref_ptr, tree *vec_offset)
{
  gimple_seq stmts = NULL;
  *dataref_ptr = force_gimple_operand (gs_info->base, &stmts, true, NULL_TREE);
  if (stmts != NULL)
    {
      basic_block new_bb;
      edge pe = loop_preheader_edge (loop);
      new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
      gcc_assert (!new_bb);
    }
  tree offset_type = TREE_TYPE (gs_info->offset);
  tree offset_vectype = get_vectype_for_scalar_type (offset_type);
  *vec_offset = vect_get_vec_def_for_operand (gs_info->offset, stmt_info,
					      offset_vectype);
}

/* Prepare to implement a grouped or strided load or store using
   the gather load or scatter store operation described by GS_INFO.
   STMT_INFO is the load or store statement.

   Set *DATAREF_BUMP to the amount that should be added to the base
   address after each copy of the vectorized statement.  Set *VEC_OFFSET
   to an invariant offset vector in which element I has the value
   I * DR_STEP / SCALE.  */

static void
vect_get_strided_load_store_ops (stmt_vec_info stmt_info,
				 loop_vec_info loop_vinfo,
				 gather_scatter_info *gs_info,
				 tree *dataref_bump, tree *vec_offset)
{
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  gimple_seq stmts;

  tree bump = size_binop (MULT_EXPR,
			  fold_convert (sizetype, DR_STEP (dr)),
			  size_int (TYPE_VECTOR_SUBPARTS (vectype)));
  *dataref_bump = force_gimple_operand (bump, &stmts, true, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);

  /* The offset given in GS_INFO can have pointer type, so use the element
     type of the vector instead.  */
  tree offset_type = TREE_TYPE (gs_info->offset);
  tree offset_vectype = get_vectype_for_scalar_type (offset_type);
  offset_type = TREE_TYPE (offset_vectype);

  /* Calculate X = DR_STEP / SCALE and convert it to the appropriate type.  */
  tree step = size_binop (EXACT_DIV_EXPR, DR_STEP (dr),
			  ssize_int (gs_info->scale));
  step = fold_convert (offset_type, step);
  step = force_gimple_operand (step, &stmts, true, NULL_TREE);

  /* Create {0, X, X*2, X*3, ...}.  */
  *vec_offset = gimple_build (&stmts, VEC_SERIES_EXPR, offset_vectype,
			      build_zero_cst (offset_type), step);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
}

/* Return the amount that should be added to a vector pointer to move
   to the next or previous copy of AGGR_TYPE.  DR_INFO is the data reference
   being vectorized and MEMORY_ACCESS_TYPE describes the type of
   vectorization.  */

static tree
vect_get_data_ptr_increment (dr_vec_info *dr_info, tree aggr_type,
			     vect_memory_access_type memory_access_type)
{
  if (memory_access_type == VMAT_INVARIANT)
    return size_zero_node;

  tree iv_step = TYPE_SIZE_UNIT (aggr_type);
  tree step = vect_dr_behavior (dr_info)->step;
  if (tree_int_cst_sgn (step) == -1)
    iv_step = fold_build1 (NEGATE_EXPR, TREE_TYPE (iv_step), iv_step);
  return iv_step;
}

/* Check and perform vectorization of BUILT_IN_BSWAP{16,32,64}.  */

static bool
vectorizable_bswap (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    stmt_vec_info *vec_stmt, slp_tree slp_node,
		    tree vectype_in, stmt_vector_for_cost *cost_vec)
{
  tree op, vectype;
  gcall *stmt = as_a <gcall *> (stmt_info->stmt);
  vec_info *vinfo = stmt_info->vinfo;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  unsigned ncopies;

  op = gimple_call_arg (stmt, 0);
  vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);

  tree char_vectype = get_same_sized_vectype (char_type_node, vectype_in);
  if (! char_vectype)
    return false;

  poly_uint64 num_bytes = TYPE_VECTOR_SUBPARTS (char_vectype);
  unsigned word_bytes;
  if (!constant_multiple_p (num_bytes, nunits, &word_bytes))
    return false;

  /* The encoding uses one stepped pattern for each byte in the word.  */
  vec_perm_builder elts (num_bytes, word_bytes, 3);
  for (unsigned i = 0; i < 3; ++i)
    for (unsigned j = 0; j < word_bytes; ++j)
      elts.quick_push ((i + 1) * word_bytes - j - 1);

  vec_perm_indices indices (elts, 1, num_bytes);
  if (!can_vec_perm_const_p (TYPE_MODE (char_vectype), indices))
    return false;

  if (! vec_stmt)
    {
      STMT_VINFO_TYPE (stmt_info) = call_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_bswap");
      if (! slp_node)
	{
	  record_stmt_cost (cost_vec,
			    1, vector_stmt, stmt_info, 0, vect_prologue);
	  record_stmt_cost (cost_vec,
			    ncopies, vec_perm, stmt_info, 0, vect_body);
	}
      return true;
    }

  tree bswap_vconst = vec_perm_indices_to_tree (char_vectype, indices);

  /* Transform.  */
  vec<tree> vec_oprnds = vNULL;
  stmt_vec_info new_stmt_info = NULL;
  stmt_vec_info prev_stmt_info = NULL;
  for (unsigned j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
	vect_get_vec_defs (op, NULL, stmt_info, &vec_oprnds, NULL, slp_node);
      else
	vect_get_vec_defs_for_stmt_copy (vinfo, &vec_oprnds, NULL);

      /* Arguments are ready. create the new vector stmt.  */
      unsigned i;
      tree vop;
      FOR_EACH_VEC_ELT (vec_oprnds, i, vop)
       {
	 gimple *new_stmt;
	 tree tem = make_ssa_name (char_vectype);
	 new_stmt = gimple_build_assign (tem, build1 (VIEW_CONVERT_EXPR,
						      char_vectype, vop));
	 vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	 tree tem2 = make_ssa_name (char_vectype);
	 new_stmt = gimple_build_assign (tem2, VEC_PERM_EXPR,
					 tem, tem, bswap_vconst);
	 vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	 tem = make_ssa_name (vectype);
	 new_stmt = gimple_build_assign (tem, build1 (VIEW_CONVERT_EXPR,
						      vectype, tem2));
	 new_stmt_info
	   = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
         if (slp_node)
	   SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
       }

      if (slp_node)
        continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

      prev_stmt_info = new_stmt_info;
    }

  vec_oprnds.release ();
  return true;
}

/* Return true if vector types VECTYPE_IN and VECTYPE_OUT have
   integer elements and if we can narrow VECTYPE_IN to VECTYPE_OUT
   in a single step.  On success, store the binary pack code in
   *CONVERT_CODE.  */

static bool
simple_integer_narrowing (tree vectype_out, tree vectype_in,
			  tree_code *convert_code)
{
  if (!INTEGRAL_TYPE_P (TREE_TYPE (vectype_out))
      || !INTEGRAL_TYPE_P (TREE_TYPE (vectype_in)))
    return false;

  tree_code code;
  int multi_step_cvt = 0;
  auto_vec <tree, 8> interm_types;
  if (!supportable_narrowing_operation (NOP_EXPR, vectype_out, vectype_in,
					&code, &multi_step_cvt,
					&interm_types)
      || multi_step_cvt)
    return false;

  *convert_code = code;
  return true;
}

/* Function vectorizable_call.

   Check if STMT_INFO performs a function call that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_call (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		   stmt_vec_info *vec_stmt, slp_tree slp_node,
		   stmt_vector_for_cost *cost_vec)
{
  gcall *stmt;
  tree vec_dest;
  tree scalar_dest;
  tree op;
  tree vec_oprnd0 = NULL_TREE, vec_oprnd1 = NULL_TREE;
  stmt_vec_info prev_stmt_info;
  tree vectype_out, vectype_in;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec_info *vinfo = stmt_info->vinfo;
  tree fndecl, new_temp, rhs_type;
  enum vect_def_type dt[4]
    = { vect_unknown_def_type, vect_unknown_def_type, vect_unknown_def_type,
	vect_unknown_def_type };
  int ndts = ARRAY_SIZE (dt);
  int ncopies, j;
  auto_vec<tree, 8> vargs;
  auto_vec<tree, 8> orig_vargs;
  enum { NARROW, NONE, WIDEN } modifier;
  size_t i, nargs;
  tree lhs;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  /* Is STMT_INFO a vectorizable call?   */
  stmt = dyn_cast <gcall *> (stmt_info->stmt);
  if (!stmt)
    return false;

  if (gimple_call_internal_p (stmt)
      && (internal_load_fn_p (gimple_call_internal_fn (stmt))
	  || internal_store_fn_p (gimple_call_internal_fn (stmt))))
    /* Handled by vectorizable_load and vectorizable_store.  */
    return false;

  if (gimple_call_lhs (stmt) == NULL_TREE
      || TREE_CODE (gimple_call_lhs (stmt)) != SSA_NAME)
    return false;

  gcc_checking_assert (!stmt_can_throw_internal (stmt));

  vectype_out = STMT_VINFO_VECTYPE (stmt_info);

  /* Process function arguments.  */
  rhs_type = NULL_TREE;
  vectype_in = NULL_TREE;
  nargs = gimple_call_num_args (stmt);

  /* Bail out if the function has more than three arguments, we do not have
     interesting builtin functions to vectorize with more than two arguments
     except for fma.  No arguments is also not good.  */
  if (nargs == 0 || nargs > 4)
    return false;

  /* Ignore the argument of IFN_GOMP_SIMD_LANE, it is magic.  */
  combined_fn cfn = gimple_call_combined_fn (stmt);
  if (cfn == CFN_GOMP_SIMD_LANE)
    {
      nargs = 0;
      rhs_type = unsigned_type_node;
    }

  int mask_opno = -1;
  if (internal_fn_p (cfn))
    mask_opno = internal_fn_mask_index (as_internal_fn (cfn));

  for (i = 0; i < nargs; i++)
    {
      tree opvectype;

      op = gimple_call_arg (stmt, i);
      if (!vect_is_simple_use (op, vinfo, &dt[i], &opvectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "use not simple.\n");
	  return false;
	}

      /* Skip the mask argument to an internal function.  This operand
	 has been converted via a pattern if necessary.  */
      if ((int) i == mask_opno)
	continue;

      /* We can only handle calls with arguments of the same type.  */
      if (rhs_type
	  && !types_compatible_p (rhs_type, TREE_TYPE (op)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "argument types differ.\n");
	  return false;
	}
      if (!rhs_type)
	rhs_type = TREE_TYPE (op);

      if (!vectype_in)
	vectype_in = opvectype;
      else if (opvectype
	       && opvectype != vectype_in)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "argument vector types differ.\n");
	  return false;
	}
    }
  /* If all arguments are external or constant defs use a vector type with
     the same size as the output vector type.  */
  if (!vectype_in)
    vectype_in = get_same_sized_vectype (rhs_type, vectype_out);
  if (vec_stmt)
    gcc_assert (vectype_in);
  if (!vectype_in)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "no vectype for scalar type %T\n", rhs_type);

      return false;
    }

  /* FORNOW */
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype_in);
  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  if (known_eq (nunits_in * 2, nunits_out))
    modifier = NARROW;
  else if (known_eq (nunits_out, nunits_in))
    modifier = NONE;
  else if (known_eq (nunits_out * 2, nunits_in))
    modifier = WIDEN;
  else
    return false;

  /* We only handle functions that do not read or clobber memory.  */
  if (gimple_vuse (stmt))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "function reads from or writes to memory.\n");
      return false;
    }

  /* For now, we only vectorize functions if a target specific builtin
     is available.  TODO -- in some cases, it might be profitable to
     insert the calls for pieces of the vector, in order to be able
     to vectorize other operations in the loop.  */
  fndecl = NULL_TREE;
  internal_fn ifn = IFN_LAST;
  tree callee = gimple_call_fndecl (stmt);

  /* First try using an internal function.  */
  tree_code convert_code = ERROR_MARK;
  if (cfn != CFN_LAST
      && (modifier == NONE
	  || (modifier == NARROW
	      && simple_integer_narrowing (vectype_out, vectype_in,
					   &convert_code))))
    ifn = vectorizable_internal_function (cfn, callee, vectype_out,
					  vectype_in);

  /* If that fails, try asking for a target-specific built-in function.  */
  if (ifn == IFN_LAST)
    {
      if (cfn != CFN_LAST)
	fndecl = targetm.vectorize.builtin_vectorized_function
	  (cfn, vectype_out, vectype_in);
      else if (callee)
	fndecl = targetm.vectorize.builtin_md_vectorized_function
	  (callee, vectype_out, vectype_in);
    }

  if (ifn == IFN_LAST && !fndecl)
    {
      if (cfn == CFN_GOMP_SIMD_LANE
	  && !slp_node
	  && loop_vinfo
	  && LOOP_VINFO_LOOP (loop_vinfo)->simduid
	  && TREE_CODE (gimple_call_arg (stmt, 0)) == SSA_NAME
	  && LOOP_VINFO_LOOP (loop_vinfo)->simduid
	     == SSA_NAME_VAR (gimple_call_arg (stmt, 0)))
	{
	  /* We can handle IFN_GOMP_SIMD_LANE by returning a
	     { 0, 1, 2, ... vf - 1 } vector.  */
	  gcc_assert (nargs == 0);
	}
      else if (modifier == NONE
	       && (gimple_call_builtin_p (stmt, BUILT_IN_BSWAP16)
		   || gimple_call_builtin_p (stmt, BUILT_IN_BSWAP32)
		   || gimple_call_builtin_p (stmt, BUILT_IN_BSWAP64)))
	return vectorizable_bswap (stmt_info, gsi, vec_stmt, slp_node,
				   vectype_in, cost_vec);
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "function is not vectorizable.\n");
	  return false;
	}
    }

  if (slp_node)
    ncopies = 1;
  else if (modifier == NARROW && ifn == IFN_LAST)
    ncopies = vect_get_num_copies (loop_vinfo, vectype_out);
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype_in);

  /* Sanity check: make sure that at least one copy of the vectorized stmt
     needs to be generated.  */
  gcc_assert (ncopies >= 1);

  vec_loop_masks *masks = (loop_vinfo ? &LOOP_VINFO_MASKS (loop_vinfo) : NULL);
  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = call_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_call");
      vect_model_simple_cost (stmt_info, ncopies, dt, ndts, slp_node, cost_vec);
      if (ifn != IFN_LAST && modifier == NARROW && !slp_node)
	record_stmt_cost (cost_vec, ncopies / 2,
			  vec_promote_demote, stmt_info, 0, vect_body);

      if (loop_vinfo && mask_opno >= 0)
	{
	  unsigned int nvectors = (slp_node
				   ? SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node)
				   : ncopies);
	  vect_record_loop_mask (loop_vinfo, masks, nvectors, vectype_out);
	}
      return true;
    }

  /* Transform.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform call.\n");

  /* Handle def.  */
  scalar_dest = gimple_call_lhs (stmt);
  vec_dest = vect_create_destination_var (scalar_dest, vectype_out);

  bool masked_loop_p = loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo);

  stmt_vec_info new_stmt_info = NULL;
  prev_stmt_info = NULL;
  if (modifier == NONE || ifn != IFN_LAST)
    {
      tree prev_res = NULL_TREE;
      vargs.safe_grow (nargs);
      orig_vargs.safe_grow (nargs);
      for (j = 0; j < ncopies; ++j)
	{
	  /* Build argument list for the vectorized call.  */
	  if (slp_node)
	    {
	      auto_vec<vec<tree> > vec_defs (nargs);
	      vec<tree> vec_oprnds0;

	      for (i = 0; i < nargs; i++)
		vargs[i] = gimple_call_arg (stmt, i);
	      vect_get_slp_defs (vargs, slp_node, &vec_defs);
	      vec_oprnds0 = vec_defs[0];

	      /* Arguments are ready.  Create the new vector stmt.  */
	      FOR_EACH_VEC_ELT (vec_oprnds0, i, vec_oprnd0)
		{
		  size_t k;
		  for (k = 0; k < nargs; k++)
		    {
		      vec<tree> vec_oprndsk = vec_defs[k];
		      vargs[k] = vec_oprndsk[i];
		    }
		  if (modifier == NARROW)
		    {
		      /* We don't define any narrowing conditional functions
			 at present.  */
		      gcc_assert (mask_opno < 0);
		      tree half_res = make_ssa_name (vectype_in);
		      gcall *call
			= gimple_build_call_internal_vec (ifn, vargs);
		      gimple_call_set_lhs (call, half_res);
		      gimple_call_set_nothrow (call, true);
		      new_stmt_info
			= vect_finish_stmt_generation (stmt_info, call, gsi);
		      if ((i & 1) == 0)
			{
			  prev_res = half_res;
			  continue;
			}
		      new_temp = make_ssa_name (vec_dest);
		      gimple *new_stmt
			= gimple_build_assign (new_temp, convert_code,
					       prev_res, half_res);
		      new_stmt_info
			= vect_finish_stmt_generation (stmt_info, new_stmt,
						       gsi);
		    }
		  else
		    {
		      if (mask_opno >= 0 && masked_loop_p)
			{
			  unsigned int vec_num = vec_oprnds0.length ();
			  /* Always true for SLP.  */
			  gcc_assert (ncopies == 1);
			  tree mask = vect_get_loop_mask (gsi, masks, vec_num,
							  vectype_out, i);
			  vargs[mask_opno] = prepare_load_store_mask
			    (TREE_TYPE (mask), mask, vargs[mask_opno], gsi);
			}

		      gcall *call;
		      if (ifn != IFN_LAST)
			call = gimple_build_call_internal_vec (ifn, vargs);
		      else
			call = gimple_build_call_vec (fndecl, vargs);
		      new_temp = make_ssa_name (vec_dest, call);
		      gimple_call_set_lhs (call, new_temp);
		      gimple_call_set_nothrow (call, true);
		      new_stmt_info
			= vect_finish_stmt_generation (stmt_info, call, gsi);
		    }
		  SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
		}

	      for (i = 0; i < nargs; i++)
		{
		  vec<tree> vec_oprndsi = vec_defs[i];
		  vec_oprndsi.release ();
		}
	      continue;
	    }

	  for (i = 0; i < nargs; i++)
	    {
	      op = gimple_call_arg (stmt, i);
	      if (j == 0)
		vec_oprnd0
		  = vect_get_vec_def_for_operand (op, stmt_info);
	      else
		vec_oprnd0
		  = vect_get_vec_def_for_stmt_copy (vinfo, orig_vargs[i]);

	      orig_vargs[i] = vargs[i] = vec_oprnd0;
	    }

	  if (mask_opno >= 0 && masked_loop_p)
	    {
	      tree mask = vect_get_loop_mask (gsi, masks, ncopies,
					      vectype_out, j);
	      vargs[mask_opno]
		= prepare_load_store_mask (TREE_TYPE (mask), mask,
					   vargs[mask_opno], gsi);
	    }

	  if (cfn == CFN_GOMP_SIMD_LANE)
	    {
	      tree cst = build_index_vector (vectype_out, j * nunits_out, 1);
	      tree new_var
		= vect_get_new_ssa_name (vectype_out, vect_simple_var, "cst_");
	      gimple *init_stmt = gimple_build_assign (new_var, cst);
	      vect_init_vector_1 (stmt_info, init_stmt, NULL);
	      new_temp = make_ssa_name (vec_dest);
	      gimple *new_stmt = gimple_build_assign (new_temp, new_var);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	    }
	  else if (modifier == NARROW)
	    {
	      /* We don't define any narrowing conditional functions at
		 present.  */
	      gcc_assert (mask_opno < 0);
	      tree half_res = make_ssa_name (vectype_in);
	      gcall *call = gimple_build_call_internal_vec (ifn, vargs);
	      gimple_call_set_lhs (call, half_res);
	      gimple_call_set_nothrow (call, true);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, call, gsi);
	      if ((j & 1) == 0)
		{
		  prev_res = half_res;
		  continue;
		}
	      new_temp = make_ssa_name (vec_dest);
	      gassign *new_stmt = gimple_build_assign (new_temp, convert_code,
						       prev_res, half_res);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	    }
	  else
	    {
	      gcall *call;
	      if (ifn != IFN_LAST)
		call = gimple_build_call_internal_vec (ifn, vargs);
	      else
		call = gimple_build_call_vec (fndecl, vargs);
	      new_temp = make_ssa_name (vec_dest, call);
	      gimple_call_set_lhs (call, new_temp);
	      gimple_call_set_nothrow (call, true);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, call, gsi);
	    }

	  if (j == (modifier == NARROW ? 1 : 0))
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

	  prev_stmt_info = new_stmt_info;
	}
    }
  else if (modifier == NARROW)
    {
      /* We don't define any narrowing conditional functions at present.  */
      gcc_assert (mask_opno < 0);
      for (j = 0; j < ncopies; ++j)
	{
	  /* Build argument list for the vectorized call.  */
	  if (j == 0)
	    vargs.create (nargs * 2);
	  else
	    vargs.truncate (0);

	  if (slp_node)
	    {
	      auto_vec<vec<tree> > vec_defs (nargs);
	      vec<tree> vec_oprnds0;

	      for (i = 0; i < nargs; i++)
		vargs.quick_push (gimple_call_arg (stmt, i));
	      vect_get_slp_defs (vargs, slp_node, &vec_defs);
	      vec_oprnds0 = vec_defs[0];

	      /* Arguments are ready.  Create the new vector stmt.  */
	      for (i = 0; vec_oprnds0.iterate (i, &vec_oprnd0); i += 2)
		{
		  size_t k;
		  vargs.truncate (0);
		  for (k = 0; k < nargs; k++)
		    {
		      vec<tree> vec_oprndsk = vec_defs[k];
		      vargs.quick_push (vec_oprndsk[i]);
		      vargs.quick_push (vec_oprndsk[i + 1]);
		    }
		  gcall *call;
		  if (ifn != IFN_LAST)
		    call = gimple_build_call_internal_vec (ifn, vargs);
		  else
		    call = gimple_build_call_vec (fndecl, vargs);
		  new_temp = make_ssa_name (vec_dest, call);
		  gimple_call_set_lhs (call, new_temp);
		  gimple_call_set_nothrow (call, true);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, call, gsi);
		  SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
		}

	      for (i = 0; i < nargs; i++)
		{
		  vec<tree> vec_oprndsi = vec_defs[i];
		  vec_oprndsi.release ();
		}
	      continue;
	    }

	  for (i = 0; i < nargs; i++)
	    {
	      op = gimple_call_arg (stmt, i);
	      if (j == 0)
		{
		  vec_oprnd0
		    = vect_get_vec_def_for_operand (op, stmt_info);
		  vec_oprnd1
		    = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd0);
		}
	      else
		{
		  vec_oprnd1 = gimple_call_arg (new_stmt_info->stmt,
						2 * i + 1);
		  vec_oprnd0
		    = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd1);
		  vec_oprnd1
		    = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd0);
		}

	      vargs.quick_push (vec_oprnd0);
	      vargs.quick_push (vec_oprnd1);
	    }

	  gcall *new_stmt = gimple_build_call_vec (fndecl, vargs);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  gimple_call_set_lhs (new_stmt, new_temp);
	  new_stmt_info
	    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = new_stmt_info;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

	  prev_stmt_info = new_stmt_info;
	}

      *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
    }
  else
    /* No current target implements this case.  */
    return false;

  vargs.release ();

  /* The call in STMT might prevent it from being removed in dce.
     We however cannot remove it here, due to the way the ssa name
     it defines is mapped to the new definition.  So just replace
     rhs of the statement with something harmless.  */

  if (slp_node)
    return true;

  stmt_info = vect_orig_stmt (stmt_info);
  lhs = gimple_get_lhs (stmt_info->stmt);

  gassign *new_stmt
    = gimple_build_assign (lhs, build_zero_cst (TREE_TYPE (lhs)));
  vinfo->replace_stmt (gsi, stmt_info, new_stmt);

  return true;
}


struct simd_call_arg_info
{
  tree vectype;
  tree op;
  HOST_WIDE_INT linear_step;
  enum vect_def_type dt;
  unsigned int align;
  bool simd_lane_linear;
};

/* Helper function of vectorizable_simd_clone_call.  If OP, an SSA_NAME,
   is linear within simd lane (but not within whole loop), note it in
   *ARGINFO.  */

static void
vect_simd_lane_linear (tree op, struct loop *loop,
		       struct simd_call_arg_info *arginfo)
{
  gimple *def_stmt = SSA_NAME_DEF_STMT (op);

  if (!is_gimple_assign (def_stmt)
      || gimple_assign_rhs_code (def_stmt) != POINTER_PLUS_EXPR
      || !is_gimple_min_invariant (gimple_assign_rhs1 (def_stmt)))
    return;

  tree base = gimple_assign_rhs1 (def_stmt);
  HOST_WIDE_INT linear_step = 0;
  tree v = gimple_assign_rhs2 (def_stmt);
  while (TREE_CODE (v) == SSA_NAME)
    {
      tree t;
      def_stmt = SSA_NAME_DEF_STMT (v);
      if (is_gimple_assign (def_stmt))
	switch (gimple_assign_rhs_code (def_stmt))
	  {
	  case PLUS_EXPR:
	    t = gimple_assign_rhs2 (def_stmt);
	    if (linear_step || TREE_CODE (t) != INTEGER_CST)
	      return;
	    base = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (base), base, t);
	    v = gimple_assign_rhs1 (def_stmt);
	    continue;
	  case MULT_EXPR:
	    t = gimple_assign_rhs2 (def_stmt);
	    if (linear_step || !tree_fits_shwi_p (t) || integer_zerop (t))
	      return;
	    linear_step = tree_to_shwi (t);
	    v = gimple_assign_rhs1 (def_stmt);
	    continue;
	  CASE_CONVERT:
	    t = gimple_assign_rhs1 (def_stmt);
	    if (TREE_CODE (TREE_TYPE (t)) != INTEGER_TYPE
		|| (TYPE_PRECISION (TREE_TYPE (v))
		    < TYPE_PRECISION (TREE_TYPE (t))))
	      return;
	    if (!linear_step)
	      linear_step = 1;
	    v = t;
	    continue;
	  default:
	    return;
	  }
      else if (gimple_call_internal_p (def_stmt, IFN_GOMP_SIMD_LANE)
	       && loop->simduid
	       && TREE_CODE (gimple_call_arg (def_stmt, 0)) == SSA_NAME
	       && (SSA_NAME_VAR (gimple_call_arg (def_stmt, 0))
		   == loop->simduid))
	{
	  if (!linear_step)
	    linear_step = 1;
	  arginfo->linear_step = linear_step;
	  arginfo->op = base;
	  arginfo->simd_lane_linear = true;
	  return;
	}
    }
}

/* Return the number of elements in vector type VECTYPE, which is associated
   with a SIMD clone.  At present these vectors always have a constant
   length.  */

static unsigned HOST_WIDE_INT
simd_clone_subparts (tree vectype)
{
  return TYPE_VECTOR_SUBPARTS (vectype).to_constant ();
}

/* Function vectorizable_simd_clone_call.

   Check if STMT_INFO performs a function call that can be vectorized
   by calling a simd clone of the function.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_simd_clone_call (stmt_vec_info stmt_info,
			      gimple_stmt_iterator *gsi,
			      stmt_vec_info *vec_stmt, slp_tree slp_node,
			      stmt_vector_for_cost *)
{
  tree vec_dest;
  tree scalar_dest;
  tree op, type;
  tree vec_oprnd0 = NULL_TREE;
  stmt_vec_info prev_stmt_info;
  tree vectype;
  unsigned int nunits;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec_info *vinfo = stmt_info->vinfo;
  struct loop *loop = loop_vinfo ? LOOP_VINFO_LOOP (loop_vinfo) : NULL;
  tree fndecl, new_temp;
  int ncopies, j;
  auto_vec<simd_call_arg_info> arginfo;
  vec<tree> vargs = vNULL;
  size_t i, nargs;
  tree lhs, rtype, ratype;
  vec<constructor_elt, va_gc> *ret_ctor_elts = NULL;

  /* Is STMT a vectorizable call?   */
  gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt);
  if (!stmt)
    return false;

  fndecl = gimple_call_fndecl (stmt);
  if (fndecl == NULL_TREE)
    return false;

  struct cgraph_node *node = cgraph_node::get (fndecl);
  if (node == NULL || node->simd_clones == NULL)
    return false;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  if (gimple_call_lhs (stmt)
      && TREE_CODE (gimple_call_lhs (stmt)) != SSA_NAME)
    return false;

  gcc_checking_assert (!stmt_can_throw_internal (stmt));

  vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (loop_vinfo && nested_in_vect_loop_p (loop, stmt_info))
    return false;

  /* FORNOW */
  if (slp_node)
    return false;

  /* Process function arguments.  */
  nargs = gimple_call_num_args (stmt);

  /* Bail out if the function has zero arguments.  */
  if (nargs == 0)
    return false;

  arginfo.reserve (nargs, true);

  for (i = 0; i < nargs; i++)
    {
      simd_call_arg_info thisarginfo;
      affine_iv iv;

      thisarginfo.linear_step = 0;
      thisarginfo.align = 0;
      thisarginfo.op = NULL_TREE;
      thisarginfo.simd_lane_linear = false;

      op = gimple_call_arg (stmt, i);
      if (!vect_is_simple_use (op, vinfo, &thisarginfo.dt,
			       &thisarginfo.vectype)
	  || thisarginfo.dt == vect_uninitialized_def)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "use not simple.\n");
	  return false;
	}

      if (thisarginfo.dt == vect_constant_def
	  || thisarginfo.dt == vect_external_def)
	gcc_assert (thisarginfo.vectype == NULL_TREE);
      else
	gcc_assert (thisarginfo.vectype != NULL_TREE);

      /* For linear arguments, the analyze phase should have saved
	 the base and step in STMT_VINFO_SIMD_CLONE_INFO.  */
      if (i * 3 + 4 <= STMT_VINFO_SIMD_CLONE_INFO (stmt_info).length ()
	  && STMT_VINFO_SIMD_CLONE_INFO (stmt_info)[i * 3 + 2])
	{
	  gcc_assert (vec_stmt);
	  thisarginfo.linear_step
	    = tree_to_shwi (STMT_VINFO_SIMD_CLONE_INFO (stmt_info)[i * 3 + 2]);
	  thisarginfo.op
	    = STMT_VINFO_SIMD_CLONE_INFO (stmt_info)[i * 3 + 1];
	  thisarginfo.simd_lane_linear
	    = (STMT_VINFO_SIMD_CLONE_INFO (stmt_info)[i * 3 + 3]
	       == boolean_true_node);
	  /* If loop has been peeled for alignment, we need to adjust it.  */
	  tree n1 = LOOP_VINFO_NITERS_UNCHANGED (loop_vinfo);
	  tree n2 = LOOP_VINFO_NITERS (loop_vinfo);
	  if (n1 != n2 && !thisarginfo.simd_lane_linear)
	    {
	      tree bias = fold_build2 (MINUS_EXPR, TREE_TYPE (n1), n1, n2);
	      tree step = STMT_VINFO_SIMD_CLONE_INFO (stmt_info)[i * 3 + 2];
	      tree opt = TREE_TYPE (thisarginfo.op);
	      bias = fold_convert (TREE_TYPE (step), bias);
	      bias = fold_build2 (MULT_EXPR, TREE_TYPE (step), bias, step);
	      thisarginfo.op
		= fold_build2 (POINTER_TYPE_P (opt)
			       ? POINTER_PLUS_EXPR : PLUS_EXPR, opt,
			       thisarginfo.op, bias);
	    }
	}
      else if (!vec_stmt
	       && thisarginfo.dt != vect_constant_def
	       && thisarginfo.dt != vect_external_def
	       && loop_vinfo
	       && TREE_CODE (op) == SSA_NAME
	       && simple_iv (loop, loop_containing_stmt (stmt), op,
			     &iv, false)
	       && tree_fits_shwi_p (iv.step))
	{
	  thisarginfo.linear_step = tree_to_shwi (iv.step);
	  thisarginfo.op = iv.base;
	}
      else if ((thisarginfo.dt == vect_constant_def
		|| thisarginfo.dt == vect_external_def)
	       && POINTER_TYPE_P (TREE_TYPE (op)))
	thisarginfo.align = get_pointer_alignment (op) / BITS_PER_UNIT;
      /* Addresses of array elements indexed by GOMP_SIMD_LANE are
	 linear too.  */
      if (POINTER_TYPE_P (TREE_TYPE (op))
	  && !thisarginfo.linear_step
	  && !vec_stmt
	  && thisarginfo.dt != vect_constant_def
	  && thisarginfo.dt != vect_external_def
	  && loop_vinfo
	  && !slp_node
	  && TREE_CODE (op) == SSA_NAME)
	vect_simd_lane_linear (op, loop, &thisarginfo);

      arginfo.quick_push (thisarginfo);
    }

  unsigned HOST_WIDE_INT vf;
  if (!LOOP_VINFO_VECT_FACTOR (loop_vinfo).is_constant (&vf))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not considering SIMD clones; not yet supported"
			 " for variable-width vectors.\n");
      return false;
    }

  unsigned int badness = 0;
  struct cgraph_node *bestn = NULL;
  if (STMT_VINFO_SIMD_CLONE_INFO (stmt_info).exists ())
    bestn = cgraph_node::get (STMT_VINFO_SIMD_CLONE_INFO (stmt_info)[0]);
  else
    for (struct cgraph_node *n = node->simd_clones; n != NULL;
	 n = n->simdclone->next_clone)
      {
	unsigned int this_badness = 0;
	if (n->simdclone->simdlen > vf
	    || n->simdclone->nargs != nargs)
	  continue;
	if (n->simdclone->simdlen < vf)
	  this_badness += (exact_log2 (vf)
			   - exact_log2 (n->simdclone->simdlen)) * 1024;
	if (n->simdclone->inbranch)
	  this_badness += 2048;
	int target_badness = targetm.simd_clone.usable (n);
	if (target_badness < 0)
	  continue;
	this_badness += target_badness * 512;
	/* FORNOW: Have to add code to add the mask argument.  */
	if (n->simdclone->inbranch)
	  continue;
	for (i = 0; i < nargs; i++)
	  {
	    switch (n->simdclone->args[i].arg_type)
	      {
	      case SIMD_CLONE_ARG_TYPE_VECTOR:
		if (!useless_type_conversion_p
			(n->simdclone->args[i].orig_type,
			 TREE_TYPE (gimple_call_arg (stmt, i))))
		  i = -1;
		else if (arginfo[i].dt == vect_constant_def
			 || arginfo[i].dt == vect_external_def
			 || arginfo[i].linear_step)
		  this_badness += 64;
		break;
	      case SIMD_CLONE_ARG_TYPE_UNIFORM:
		if (arginfo[i].dt != vect_constant_def
		    && arginfo[i].dt != vect_external_def)
		  i = -1;
		break;
	      case SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP:
	      case SIMD_CLONE_ARG_TYPE_LINEAR_REF_CONSTANT_STEP:
		if (arginfo[i].dt == vect_constant_def
		    || arginfo[i].dt == vect_external_def
		    || (arginfo[i].linear_step
			!= n->simdclone->args[i].linear_step))
		  i = -1;
		break;
	      case SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP:
	      case SIMD_CLONE_ARG_TYPE_LINEAR_VAL_CONSTANT_STEP:
	      case SIMD_CLONE_ARG_TYPE_LINEAR_UVAL_CONSTANT_STEP:
	      case SIMD_CLONE_ARG_TYPE_LINEAR_REF_VARIABLE_STEP:
	      case SIMD_CLONE_ARG_TYPE_LINEAR_VAL_VARIABLE_STEP:
	      case SIMD_CLONE_ARG_TYPE_LINEAR_UVAL_VARIABLE_STEP:
		/* FORNOW */
		i = -1;
		break;
	      case SIMD_CLONE_ARG_TYPE_MASK:
		gcc_unreachable ();
	      }
	    if (i == (size_t) -1)
	      break;
	    if (n->simdclone->args[i].alignment > arginfo[i].align)
	      {
		i = -1;
		break;
	      }
	    if (arginfo[i].align)
	      this_badness += (exact_log2 (arginfo[i].align)
			       - exact_log2 (n->simdclone->args[i].alignment));
	  }
	if (i == (size_t) -1)
	  continue;
	if (bestn == NULL || this_badness < badness)
	  {
	    bestn = n;
	    badness = this_badness;
	  }
      }

  if (bestn == NULL)
    return false;

  for (i = 0; i < nargs; i++)
    if ((arginfo[i].dt == vect_constant_def
	 || arginfo[i].dt == vect_external_def)
	&& bestn->simdclone->args[i].arg_type == SIMD_CLONE_ARG_TYPE_VECTOR)
      {
	arginfo[i].vectype
	  = get_vectype_for_scalar_type (TREE_TYPE (gimple_call_arg (stmt,
								     i)));
	if (arginfo[i].vectype == NULL
	    || (simd_clone_subparts (arginfo[i].vectype)
		> bestn->simdclone->simdlen))
	  return false;
      }

  fndecl = bestn->decl;
  nunits = bestn->simdclone->simdlen;
  ncopies = vf / nunits;

  /* If the function isn't const, only allow it in simd loops where user
     has asserted that at least nunits consecutive iterations can be
     performed using SIMD instructions.  */
  if ((loop == NULL || (unsigned) loop->safelen < nunits)
      && gimple_vuse (stmt))
    return false;

  /* Sanity check: make sure that at least one copy of the vectorized stmt
     needs to be generated.  */
  gcc_assert (ncopies >= 1);

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_SIMD_CLONE_INFO (stmt_info).safe_push (bestn->decl);
      for (i = 0; i < nargs; i++)
	if ((bestn->simdclone->args[i].arg_type
	     == SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP)
	    || (bestn->simdclone->args[i].arg_type
		== SIMD_CLONE_ARG_TYPE_LINEAR_REF_CONSTANT_STEP))
	  {
	    STMT_VINFO_SIMD_CLONE_INFO (stmt_info).safe_grow_cleared (i * 3
									+ 1);
	    STMT_VINFO_SIMD_CLONE_INFO (stmt_info).safe_push (arginfo[i].op);
	    tree lst = POINTER_TYPE_P (TREE_TYPE (arginfo[i].op))
		       ? size_type_node : TREE_TYPE (arginfo[i].op);
	    tree ls = build_int_cst (lst, arginfo[i].linear_step);
	    STMT_VINFO_SIMD_CLONE_INFO (stmt_info).safe_push (ls);
	    tree sll = arginfo[i].simd_lane_linear
		       ? boolean_true_node : boolean_false_node;
	    STMT_VINFO_SIMD_CLONE_INFO (stmt_info).safe_push (sll);
	  }
      STMT_VINFO_TYPE (stmt_info) = call_simd_clone_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_simd_clone_call");
/*      vect_model_simple_cost (stmt_info, ncopies, dt, slp_node, cost_vec); */
      return true;
    }

  /* Transform.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform call.\n");

  /* Handle def.  */
  scalar_dest = gimple_call_lhs (stmt);
  vec_dest = NULL_TREE;
  rtype = NULL_TREE;
  ratype = NULL_TREE;
  if (scalar_dest)
    {
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      rtype = TREE_TYPE (TREE_TYPE (fndecl));
      if (TREE_CODE (rtype) == ARRAY_TYPE)
	{
	  ratype = rtype;
	  rtype = TREE_TYPE (ratype);
	}
    }

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; ++j)
    {
      /* Build argument list for the vectorized call.  */
      if (j == 0)
	vargs.create (nargs);
      else
	vargs.truncate (0);

      for (i = 0; i < nargs; i++)
	{
	  unsigned int k, l, m, o;
	  tree atype;
	  op = gimple_call_arg (stmt, i);
	  switch (bestn->simdclone->args[i].arg_type)
	    {
	    case SIMD_CLONE_ARG_TYPE_VECTOR:
	      atype = bestn->simdclone->args[i].vector_type;
	      o = nunits / simd_clone_subparts (atype);
	      for (m = j * o; m < (j + 1) * o; m++)
		{
		  if (simd_clone_subparts (atype)
		      < simd_clone_subparts (arginfo[i].vectype))
		    {
		      poly_uint64 prec = GET_MODE_BITSIZE (TYPE_MODE (atype));
		      k = (simd_clone_subparts (arginfo[i].vectype)
			   / simd_clone_subparts (atype));
		      gcc_assert ((k & (k - 1)) == 0);
		      if (m == 0)
			vec_oprnd0
			  = vect_get_vec_def_for_operand (op, stmt_info);
		      else
			{
			  vec_oprnd0 = arginfo[i].op;
			  if ((m & (k - 1)) == 0)
			    vec_oprnd0
			      = vect_get_vec_def_for_stmt_copy (vinfo,
								vec_oprnd0);
			}
		      arginfo[i].op = vec_oprnd0;
		      vec_oprnd0
			= build3 (BIT_FIELD_REF, atype, vec_oprnd0,
				  bitsize_int (prec),
				  bitsize_int ((m & (k - 1)) * prec));
		      gassign *new_stmt
			= gimple_build_assign (make_ssa_name (atype),
					       vec_oprnd0);
		      vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		      vargs.safe_push (gimple_assign_lhs (new_stmt));
		    }
		  else
		    {
		      k = (simd_clone_subparts (atype)
			   / simd_clone_subparts (arginfo[i].vectype));
		      gcc_assert ((k & (k - 1)) == 0);
		      vec<constructor_elt, va_gc> *ctor_elts;
		      if (k != 1)
			vec_alloc (ctor_elts, k);
		      else
			ctor_elts = NULL;
		      for (l = 0; l < k; l++)
			{
			  if (m == 0 && l == 0)
			    vec_oprnd0
			      = vect_get_vec_def_for_operand (op, stmt_info);
			  else
			    vec_oprnd0
			      = vect_get_vec_def_for_stmt_copy (vinfo,
								arginfo[i].op);
			  arginfo[i].op = vec_oprnd0;
			  if (k == 1)
			    break;
			  CONSTRUCTOR_APPEND_ELT (ctor_elts, NULL_TREE,
						  vec_oprnd0);
			}
		      if (k == 1)
			vargs.safe_push (vec_oprnd0);
		      else
			{
			  vec_oprnd0 = build_constructor (atype, ctor_elts);
			  gassign *new_stmt
			    = gimple_build_assign (make_ssa_name (atype),
						   vec_oprnd0);
			  vect_finish_stmt_generation (stmt_info, new_stmt,
						       gsi);
			  vargs.safe_push (gimple_assign_lhs (new_stmt));
			}
		    }
		}
	      break;
	    case SIMD_CLONE_ARG_TYPE_UNIFORM:
	      vargs.safe_push (op);
	      break;
	    case SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP:
	    case SIMD_CLONE_ARG_TYPE_LINEAR_REF_CONSTANT_STEP:
	      if (j == 0)
		{
		  gimple_seq stmts;
		  arginfo[i].op
		    = force_gimple_operand (arginfo[i].op, &stmts, true,
					    NULL_TREE);
		  if (stmts != NULL)
		    {
		      basic_block new_bb;
		      edge pe = loop_preheader_edge (loop);
		      new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
		      gcc_assert (!new_bb);
		    }
		  if (arginfo[i].simd_lane_linear)
		    {
		      vargs.safe_push (arginfo[i].op);
		      break;
		    }
		  tree phi_res = copy_ssa_name (op);
		  gphi *new_phi = create_phi_node (phi_res, loop->header);
		  loop_vinfo->add_stmt (new_phi);
		  add_phi_arg (new_phi, arginfo[i].op,
			       loop_preheader_edge (loop), UNKNOWN_LOCATION);
		  enum tree_code code
		    = POINTER_TYPE_P (TREE_TYPE (op))
		      ? POINTER_PLUS_EXPR : PLUS_EXPR;
		  tree type = POINTER_TYPE_P (TREE_TYPE (op))
			      ? sizetype : TREE_TYPE (op);
		  widest_int cst
		    = wi::mul (bestn->simdclone->args[i].linear_step,
			       ncopies * nunits);
		  tree tcst = wide_int_to_tree (type, cst);
		  tree phi_arg = copy_ssa_name (op);
		  gassign *new_stmt
		    = gimple_build_assign (phi_arg, code, phi_res, tcst);
		  gimple_stmt_iterator si = gsi_after_labels (loop->header);
		  gsi_insert_after (&si, new_stmt, GSI_NEW_STMT);
		  loop_vinfo->add_stmt (new_stmt);
		  add_phi_arg (new_phi, phi_arg, loop_latch_edge (loop),
			       UNKNOWN_LOCATION);
		  arginfo[i].op = phi_res;
		  vargs.safe_push (phi_res);
		}
	      else
		{
		  enum tree_code code
		    = POINTER_TYPE_P (TREE_TYPE (op))
		      ? POINTER_PLUS_EXPR : PLUS_EXPR;
		  tree type = POINTER_TYPE_P (TREE_TYPE (op))
			      ? sizetype : TREE_TYPE (op);
		  widest_int cst
		    = wi::mul (bestn->simdclone->args[i].linear_step,
			       j * nunits);
		  tree tcst = wide_int_to_tree (type, cst);
		  new_temp = make_ssa_name (TREE_TYPE (op));
		  gassign *new_stmt
		    = gimple_build_assign (new_temp, code,
					   arginfo[i].op, tcst);
		  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		  vargs.safe_push (new_temp);
		}
	      break;
	    case SIMD_CLONE_ARG_TYPE_LINEAR_VAL_CONSTANT_STEP:
	    case SIMD_CLONE_ARG_TYPE_LINEAR_UVAL_CONSTANT_STEP:
	    case SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP:
	    case SIMD_CLONE_ARG_TYPE_LINEAR_REF_VARIABLE_STEP:
	    case SIMD_CLONE_ARG_TYPE_LINEAR_VAL_VARIABLE_STEP:
	    case SIMD_CLONE_ARG_TYPE_LINEAR_UVAL_VARIABLE_STEP:
	    default:
	      gcc_unreachable ();
	    }
	}

      gcall *new_call = gimple_build_call_vec (fndecl, vargs);
      if (vec_dest)
	{
	  gcc_assert (ratype || simd_clone_subparts (rtype) == nunits);
	  if (ratype)
	    new_temp = create_tmp_var (ratype);
	  else if (simd_clone_subparts (vectype)
		   == simd_clone_subparts (rtype))
	    new_temp = make_ssa_name (vec_dest, new_call);
	  else
	    new_temp = make_ssa_name (rtype, new_call);
	  gimple_call_set_lhs (new_call, new_temp);
	}
      stmt_vec_info new_stmt_info
	= vect_finish_stmt_generation (stmt_info, new_call, gsi);

      if (vec_dest)
	{
	  if (simd_clone_subparts (vectype) < nunits)
	    {
	      unsigned int k, l;
	      poly_uint64 prec = GET_MODE_BITSIZE (TYPE_MODE (vectype));
	      poly_uint64 bytes = GET_MODE_SIZE (TYPE_MODE (vectype));
	      k = nunits / simd_clone_subparts (vectype);
	      gcc_assert ((k & (k - 1)) == 0);
	      for (l = 0; l < k; l++)
		{
		  tree t;
		  if (ratype)
		    {
		      t = build_fold_addr_expr (new_temp);
		      t = build2 (MEM_REF, vectype, t,
				  build_int_cst (TREE_TYPE (t), l * bytes));
		    }
		  else
		    t = build3 (BIT_FIELD_REF, vectype, new_temp,
				bitsize_int (prec), bitsize_int (l * prec));
		  gimple *new_stmt
		    = gimple_build_assign (make_ssa_name (vectype), t);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

		  if (j == 0 && l == 0)
		    STMT_VINFO_VEC_STMT (stmt_info)
		      = *vec_stmt = new_stmt_info;
		  else
		    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

		  prev_stmt_info = new_stmt_info;
		}

	      if (ratype)
		vect_clobber_variable (stmt_info, gsi, new_temp);
	      continue;
	    }
	  else if (simd_clone_subparts (vectype) > nunits)
	    {
	      unsigned int k = (simd_clone_subparts (vectype)
				/ simd_clone_subparts (rtype));
	      gcc_assert ((k & (k - 1)) == 0);
	      if ((j & (k - 1)) == 0)
		vec_alloc (ret_ctor_elts, k);
	      if (ratype)
		{
		  unsigned int m, o = nunits / simd_clone_subparts (rtype);
		  for (m = 0; m < o; m++)
		    {
		      tree tem = build4 (ARRAY_REF, rtype, new_temp,
					 size_int (m), NULL_TREE, NULL_TREE);
		      gimple *new_stmt
			= gimple_build_assign (make_ssa_name (rtype), tem);
		      new_stmt_info
			= vect_finish_stmt_generation (stmt_info, new_stmt,
						       gsi);
		      CONSTRUCTOR_APPEND_ELT (ret_ctor_elts, NULL_TREE,
					      gimple_assign_lhs (new_stmt));
		    }
		  vect_clobber_variable (stmt_info, gsi, new_temp);
		}
	      else
		CONSTRUCTOR_APPEND_ELT (ret_ctor_elts, NULL_TREE, new_temp);
	      if ((j & (k - 1)) != k - 1)
		continue;
	      vec_oprnd0 = build_constructor (vectype, ret_ctor_elts);
	      gimple *new_stmt
		= gimple_build_assign (make_ssa_name (vec_dest), vec_oprnd0);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

	      if ((unsigned) j == k - 1)
		STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	      else
		STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

	      prev_stmt_info = new_stmt_info;
	      continue;
	    }
	  else if (ratype)
	    {
	      tree t = build_fold_addr_expr (new_temp);
	      t = build2 (MEM_REF, vectype, t,
			  build_int_cst (TREE_TYPE (t), 0));
	      gimple *new_stmt
		= gimple_build_assign (make_ssa_name (vec_dest), t);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	      vect_clobber_variable (stmt_info, gsi, new_temp);
	    }
	}

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

      prev_stmt_info = new_stmt_info;
    }

  vargs.release ();

  /* The call in STMT might prevent it from being removed in dce.
     We however cannot remove it here, due to the way the ssa name
     it defines is mapped to the new definition.  So just replace
     rhs of the statement with something harmless.  */

  if (slp_node)
    return true;

  gimple *new_stmt;
  if (scalar_dest)
    {
      type = TREE_TYPE (scalar_dest);
      lhs = gimple_call_lhs (vect_orig_stmt (stmt_info)->stmt);
      new_stmt = gimple_build_assign (lhs, build_zero_cst (type));
    }
  else
    new_stmt = gimple_build_nop ();
  vinfo->replace_stmt (gsi, vect_orig_stmt (stmt_info), new_stmt);
  unlink_stmt_vdef (stmt);

  return true;
}


/* Function vect_gen_widened_results_half

   Create a vector stmt whose code, type, number of arguments, and result
   variable are CODE, OP_TYPE, and VEC_DEST, and its arguments are
   VEC_OPRND0 and VEC_OPRND1.  The new vector stmt is to be inserted at BSI.
   In the case that CODE is a CALL_EXPR, this means that a call to DECL
   needs to be created (DECL is a function-decl of a target-builtin).
   STMT_INFO is the original scalar stmt that we are vectorizing.  */

static gimple *
vect_gen_widened_results_half (enum tree_code code,
			       tree decl,
                               tree vec_oprnd0, tree vec_oprnd1, int op_type,
			       tree vec_dest, gimple_stmt_iterator *gsi,
			       stmt_vec_info stmt_info)
{
  gimple *new_stmt;
  tree new_temp;

  /* Generate half of the widened result:  */
  if (code == CALL_EXPR)
    {
      /* Target specific support  */
      if (op_type == binary_op)
	new_stmt = gimple_build_call (decl, 2, vec_oprnd0, vec_oprnd1);
      else
	new_stmt = gimple_build_call (decl, 1, vec_oprnd0);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_call_set_lhs (new_stmt, new_temp);
    }
  else
    {
      /* Generic support */
      gcc_assert (op_type == TREE_CODE_LENGTH (code));
      if (op_type != binary_op)
	vec_oprnd1 = NULL;
      new_stmt = gimple_build_assign (vec_dest, code, vec_oprnd0, vec_oprnd1);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
    }
  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

  return new_stmt;
}


/* Get vectorized definitions for loop-based vectorization of STMT_INFO.
   For the first operand we call vect_get_vec_def_for_operand (with OPRND
   containing scalar operand), and for the rest we get a copy with
   vect_get_vec_def_for_stmt_copy() using the previous vector definition
   (stored in OPRND). See vect_get_vec_def_for_stmt_copy() for details.
   The vectors are collected into VEC_OPRNDS.  */

static void
vect_get_loop_based_defs (tree *oprnd, stmt_vec_info stmt_info,
			  vec<tree> *vec_oprnds, int multi_step_cvt)
{
  vec_info *vinfo = stmt_info->vinfo;
  tree vec_oprnd;

  /* Get first vector operand.  */
  /* All the vector operands except the very first one (that is scalar oprnd)
     are stmt copies.  */
  if (TREE_CODE (TREE_TYPE (*oprnd)) != VECTOR_TYPE)
    vec_oprnd = vect_get_vec_def_for_operand (*oprnd, stmt_info);
  else
    vec_oprnd = vect_get_vec_def_for_stmt_copy (vinfo, *oprnd);

  vec_oprnds->quick_push (vec_oprnd);

  /* Get second vector operand.  */
  vec_oprnd = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd);
  vec_oprnds->quick_push (vec_oprnd);

  *oprnd = vec_oprnd;

  /* For conversion in multiple steps, continue to get operands
     recursively.  */
  if (multi_step_cvt)
    vect_get_loop_based_defs (oprnd, stmt_info, vec_oprnds,
			      multi_step_cvt - 1);
}


/* Create vectorized demotion statements for vector operands from VEC_OPRNDS.
   For multi-step conversions store the resulting vectors and call the function
   recursively.  */

static void
vect_create_vectorized_demotion_stmts (vec<tree> *vec_oprnds,
				       int multi_step_cvt,
				       stmt_vec_info stmt_info,
				       vec<tree> vec_dsts,
				       gimple_stmt_iterator *gsi,
				       slp_tree slp_node, enum tree_code code,
				       stmt_vec_info *prev_stmt_info)
{
  unsigned int i;
  tree vop0, vop1, new_tmp, vec_dest;

  vec_dest = vec_dsts.pop ();

  for (i = 0; i < vec_oprnds->length (); i += 2)
    {
      /* Create demotion operation.  */
      vop0 = (*vec_oprnds)[i];
      vop1 = (*vec_oprnds)[i + 1];
      gassign *new_stmt = gimple_build_assign (vec_dest, code, vop0, vop1);
      new_tmp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_tmp);
      stmt_vec_info new_stmt_info
	= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

      if (multi_step_cvt)
	/* Store the resulting vector for next recursive call.  */
	(*vec_oprnds)[i/2] = new_tmp;
      else
	{
	  /* This is the last step of the conversion sequence. Store the
	     vectors in SLP_NODE or in vector info of the scalar statement
	     (or in STMT_VINFO_RELATED_STMT chain).  */
	  if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
	  else
	    {
	      if (!*prev_stmt_info)
		STMT_VINFO_VEC_STMT (stmt_info) = new_stmt_info;
	      else
		STMT_VINFO_RELATED_STMT (*prev_stmt_info) = new_stmt_info;

	      *prev_stmt_info = new_stmt_info;
	    }
	}
    }

  /* For multi-step demotion operations we first generate demotion operations
     from the source type to the intermediate types, and then combine the
     results (stored in VEC_OPRNDS) in demotion operation to the destination
     type.  */
  if (multi_step_cvt)
    {
      /* At each level of recursion we have half of the operands we had at the
	 previous level.  */
      vec_oprnds->truncate ((i+1)/2);
      vect_create_vectorized_demotion_stmts (vec_oprnds, multi_step_cvt - 1,
					     stmt_info, vec_dsts, gsi,
					     slp_node, VEC_PACK_TRUNC_EXPR,
					     prev_stmt_info);
    }

  vec_dsts.quick_push (vec_dest);
}


/* Create vectorized promotion statements for vector operands from VEC_OPRNDS0
   and VEC_OPRNDS1, for a binary operation associated with scalar statement
   STMT_INFO.  For multi-step conversions store the resulting vectors and
   call the function recursively.  */

static void
vect_create_vectorized_promotion_stmts (vec<tree> *vec_oprnds0,
					vec<tree> *vec_oprnds1,
					stmt_vec_info stmt_info, tree vec_dest,
					gimple_stmt_iterator *gsi,
					enum tree_code code1,
					enum tree_code code2, tree decl1,
					tree decl2, int op_type)
{
  int i;
  tree vop0, vop1, new_tmp1, new_tmp2;
  gimple *new_stmt1, *new_stmt2;
  vec<tree> vec_tmp = vNULL;

  vec_tmp.create (vec_oprnds0->length () * 2);
  FOR_EACH_VEC_ELT (*vec_oprnds0, i, vop0)
    {
      if (op_type == binary_op)
	vop1 = (*vec_oprnds1)[i];
      else
	vop1 = NULL_TREE;

      /* Generate the two halves of promotion operation.  */
      new_stmt1 = vect_gen_widened_results_half (code1, decl1, vop0, vop1,
						 op_type, vec_dest, gsi,
						 stmt_info);
      new_stmt2 = vect_gen_widened_results_half (code2, decl2, vop0, vop1,
						 op_type, vec_dest, gsi,
						 stmt_info);
      if (is_gimple_call (new_stmt1))
	{
	  new_tmp1 = gimple_call_lhs (new_stmt1);
	  new_tmp2 = gimple_call_lhs (new_stmt2);
	}
      else
	{
	  new_tmp1 = gimple_assign_lhs (new_stmt1);
	  new_tmp2 = gimple_assign_lhs (new_stmt2);
	}

      /* Store the results for the next step.  */
      vec_tmp.quick_push (new_tmp1);
      vec_tmp.quick_push (new_tmp2);
    }

  vec_oprnds0->release ();
  *vec_oprnds0 = vec_tmp;
}


/* Check if STMT_INFO performs a conversion operation that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_conversion (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			 stmt_vec_info *vec_stmt, slp_tree slp_node,
			 stmt_vector_for_cost *cost_vec)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL_TREE;
  tree vec_oprnd0 = NULL_TREE, vec_oprnd1 = NULL_TREE;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code, code1 = ERROR_MARK, code2 = ERROR_MARK;
  enum tree_code codecvt1 = ERROR_MARK, codecvt2 = ERROR_MARK;
  tree decl1 = NULL_TREE, decl2 = NULL_TREE;
  tree new_temp;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 2;
  stmt_vec_info prev_stmt_info;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  tree vectype_out, vectype_in;
  int ncopies, i, j;
  tree lhs_type, rhs_type;
  enum { NARROW, NONE, WIDEN } modifier;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  tree vop0;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec_info *vinfo = stmt_info->vinfo;
  int multi_step_cvt = 0;
  vec<tree> interm_types = vNULL;
  tree last_oprnd, intermediate_type, cvt_type = NULL_TREE;
  int op_type;
  unsigned short fltsz;

  /* Is STMT a vectorizable conversion?   */

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  if (TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (!CONVERT_EXPR_CODE_P (code)
      && code != FIX_TRUNC_EXPR
      && code != FLOAT_EXPR
      && code != WIDEN_MULT_EXPR
      && code != WIDEN_LSHIFT_EXPR)
    return false;

  op_type = TREE_CODE_LENGTH (code);

  /* Check types of lhs and rhs.  */
  scalar_dest = gimple_assign_lhs (stmt);
  lhs_type = TREE_TYPE (scalar_dest);
  vectype_out = STMT_VINFO_VECTYPE (stmt_info);

  op0 = gimple_assign_rhs1 (stmt);
  rhs_type = TREE_TYPE (op0);

  if ((code != FIX_TRUNC_EXPR && code != FLOAT_EXPR)
      && !((INTEGRAL_TYPE_P (lhs_type)
	    && INTEGRAL_TYPE_P (rhs_type))
	   || (SCALAR_FLOAT_TYPE_P (lhs_type)
	       && SCALAR_FLOAT_TYPE_P (rhs_type))))
    return false;

  if (!VECTOR_BOOLEAN_TYPE_P (vectype_out)
      && ((INTEGRAL_TYPE_P (lhs_type)
	   && !type_has_mode_precision_p (lhs_type))
	  || (INTEGRAL_TYPE_P (rhs_type)
	      && !type_has_mode_precision_p (rhs_type))))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "type conversion to/from bit-precision unsupported."
                         "\n");
      return false;
    }

  /* Check the operands of the operation.  */
  if (!vect_is_simple_use (op0, vinfo, &dt[0], &vectype_in))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }
  if (op_type == binary_op)
    {
      bool ok;

      op1 = gimple_assign_rhs2 (stmt);
      gcc_assert (code == WIDEN_MULT_EXPR || code == WIDEN_LSHIFT_EXPR);
      /* For WIDEN_MULT_EXPR, if OP0 is a constant, use the type of
	 OP1.  */
      if (CONSTANT_CLASS_P (op0))
	ok = vect_is_simple_use (op1, vinfo, &dt[1], &vectype_in);
      else
	ok = vect_is_simple_use (op1, vinfo, &dt[1]);

      if (!ok)
	{
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.\n");
	  return false;
	}
    }

  /* If op0 is an external or constant defs use a vector type of
     the same size as the output vector type.  */
  if (!vectype_in)
    vectype_in = get_same_sized_vectype (rhs_type, vectype_out);
  if (vec_stmt)
    gcc_assert (vectype_in);
  if (!vectype_in)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "no vectype for scalar type %T\n", rhs_type);

      return false;
    }

  if (VECTOR_BOOLEAN_TYPE_P (vectype_out)
      && !VECTOR_BOOLEAN_TYPE_P (vectype_in))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't convert between boolean and non "
			 "boolean vectors %T\n", rhs_type);

      return false;
    }

  nunits_in = TYPE_VECTOR_SUBPARTS (vectype_in);
  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  if (known_eq (nunits_out, nunits_in))
    modifier = NONE;
  else if (multiple_p (nunits_out, nunits_in))
    modifier = NARROW;
  else
    {
      gcc_checking_assert (multiple_p (nunits_in, nunits_out));
      modifier = WIDEN;
    }

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node)
    ncopies = 1;
  else if (modifier == NARROW)
    ncopies = vect_get_num_copies (loop_vinfo, vectype_out);
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype_in);

  /* Sanity check: make sure that at least one copy of the vectorized stmt
     needs to be generated.  */
  gcc_assert (ncopies >= 1);

  bool found_mode = false;
  scalar_mode lhs_mode = SCALAR_TYPE_MODE (lhs_type);
  scalar_mode rhs_mode = SCALAR_TYPE_MODE (rhs_type);
  opt_scalar_mode rhs_mode_iter;

  /* Supportable by target?  */
  switch (modifier)
    {
    case NONE:
      if (code != FIX_TRUNC_EXPR && code != FLOAT_EXPR)
	return false;
      if (supportable_convert_operation (code, vectype_out, vectype_in,
					 &decl1, &code1))
	break;
      /* FALLTHRU */
    unsupported:
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "conversion not supported by target.\n");
      return false;

    case WIDEN:
      if (supportable_widening_operation (code, stmt_info, vectype_out,
					  vectype_in, &code1, &code2,
					  &multi_step_cvt, &interm_types))
	{
	  /* Binary widening operation can only be supported directly by the
	     architecture.  */
	  gcc_assert (!(multi_step_cvt && op_type == binary_op));
	  break;
	}

      if (code != FLOAT_EXPR
	  || GET_MODE_SIZE (lhs_mode) <= GET_MODE_SIZE (rhs_mode))
	goto unsupported;

      fltsz = GET_MODE_SIZE (lhs_mode);
      FOR_EACH_2XWIDER_MODE (rhs_mode_iter, rhs_mode)
	{
	  rhs_mode = rhs_mode_iter.require ();
	  if (GET_MODE_SIZE (rhs_mode) > fltsz)
	    break;

	  cvt_type
	    = build_nonstandard_integer_type (GET_MODE_BITSIZE (rhs_mode), 0);
	  cvt_type = get_same_sized_vectype (cvt_type, vectype_in);
	  if (cvt_type == NULL_TREE)
	    goto unsupported;

	  if (GET_MODE_SIZE (rhs_mode) == fltsz)
	    {
	      if (!supportable_convert_operation (code, vectype_out,
						  cvt_type, &decl1, &codecvt1))
		goto unsupported;
	    }
	  else if (!supportable_widening_operation (code, stmt_info,
						    vectype_out, cvt_type,
						    &codecvt1, &codecvt2,
						    &multi_step_cvt,
						    &interm_types))
	    continue;
	  else
	    gcc_assert (multi_step_cvt == 0);

	  if (supportable_widening_operation (NOP_EXPR, stmt_info, cvt_type,
					      vectype_in, &code1, &code2,
					      &multi_step_cvt, &interm_types))
	    {
	      found_mode = true;
	      break;
	    }
	}

      if (!found_mode)
	goto unsupported;

      if (GET_MODE_SIZE (rhs_mode) == fltsz)
	codecvt2 = ERROR_MARK;
      else
	{
	  multi_step_cvt++;
	  interm_types.safe_push (cvt_type);
	  cvt_type = NULL_TREE;
	}
      break;

    case NARROW:
      gcc_assert (op_type == unary_op);
      if (supportable_narrowing_operation (code, vectype_out, vectype_in,
					   &code1, &multi_step_cvt,
					   &interm_types))
	break;

      if (code != FIX_TRUNC_EXPR
	  || GET_MODE_SIZE (lhs_mode) >= GET_MODE_SIZE (rhs_mode))
	goto unsupported;

      cvt_type
	= build_nonstandard_integer_type (GET_MODE_BITSIZE (rhs_mode), 0);
      cvt_type = get_same_sized_vectype (cvt_type, vectype_in);
      if (cvt_type == NULL_TREE)
	goto unsupported;
      if (!supportable_convert_operation (code, cvt_type, vectype_in,
					  &decl1, &codecvt1))
	goto unsupported;
      if (supportable_narrowing_operation (NOP_EXPR, vectype_out, cvt_type,
					   &code1, &multi_step_cvt,
					   &interm_types))
	break;
      goto unsupported;

    default:
      gcc_unreachable ();
    }

  if (!vec_stmt)		/* transformation not required.  */
    {
      DUMP_VECT_SCOPE ("vectorizable_conversion");
      if (code == FIX_TRUNC_EXPR || code == FLOAT_EXPR)
        {
	  STMT_VINFO_TYPE (stmt_info) = type_conversion_vec_info_type;
	  vect_model_simple_cost (stmt_info, ncopies, dt, ndts, slp_node,
				  cost_vec);
	}
      else if (modifier == NARROW)
	{
	  STMT_VINFO_TYPE (stmt_info) = type_demotion_vec_info_type;
	  vect_model_promotion_demotion_cost (stmt_info, dt, multi_step_cvt,
					      cost_vec);
	}
      else
	{
	  STMT_VINFO_TYPE (stmt_info) = type_promotion_vec_info_type;
	  vect_model_promotion_demotion_cost (stmt_info, dt, multi_step_cvt,
					      cost_vec);
	}
      interm_types.release ();
      return true;
    }

  /* Transform.  */
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform conversion. ncopies = %d.\n", ncopies);

  if (op_type == binary_op)
    {
      if (CONSTANT_CLASS_P (op0))
	op0 = fold_convert (TREE_TYPE (op1), op0);
      else if (CONSTANT_CLASS_P (op1))
	op1 = fold_convert (TREE_TYPE (op0), op1);
    }

  /* In case of multi-step conversion, we first generate conversion operations
     to the intermediate types, and then from that types to the final one.
     We create vector destinations for the intermediate type (TYPES) received
     from supportable_*_operation, and store them in the correct order
     for future use in vect_create_vectorized_*_stmts ().  */
  auto_vec<tree> vec_dsts (multi_step_cvt + 1);
  vec_dest = vect_create_destination_var (scalar_dest,
					  (cvt_type && modifier == WIDEN)
					  ? cvt_type : vectype_out);
  vec_dsts.quick_push (vec_dest);

  if (multi_step_cvt)
    {
      for (i = interm_types.length () - 1;
	   interm_types.iterate (i, &intermediate_type); i--)
	{
	  vec_dest = vect_create_destination_var (scalar_dest,
						  intermediate_type);
	  vec_dsts.quick_push (vec_dest);
	}
    }

  if (cvt_type)
    vec_dest = vect_create_destination_var (scalar_dest,
					    modifier == WIDEN
					    ? vectype_out : cvt_type);

  if (!slp_node)
    {
      if (modifier == WIDEN)
	{
	  vec_oprnds0.create (multi_step_cvt ? vect_pow2 (multi_step_cvt) : 1);
	  if (op_type == binary_op)
	    vec_oprnds1.create (1);
	}
      else if (modifier == NARROW)
	vec_oprnds0.create (
		   2 * (multi_step_cvt ? vect_pow2 (multi_step_cvt) : 1));
    }
  else if (code == WIDEN_LSHIFT_EXPR)
    vec_oprnds1.create (slp_node->vec_stmts_size);

  last_oprnd = op0;
  prev_stmt_info = NULL;
  switch (modifier)
    {
    case NONE:
      for (j = 0; j < ncopies; j++)
	{
	  if (j == 0)
	    vect_get_vec_defs (op0, NULL, stmt_info, &vec_oprnds0,
			       NULL, slp_node);
	  else
	    vect_get_vec_defs_for_stmt_copy (vinfo, &vec_oprnds0, NULL);

	  FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	    {
	      stmt_vec_info new_stmt_info;
	      /* Arguments are ready, create the new vector stmt.  */
	      if (code1 == CALL_EXPR)
		{
		  gcall *new_stmt = gimple_build_call (decl1, 1, vop0);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_call_set_lhs (new_stmt, new_temp);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		}
	      else
		{
		  gcc_assert (TREE_CODE_LENGTH (code1) == unary_op);
		  gassign *new_stmt
		    = gimple_build_assign (vec_dest, code1, vop0);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_assign_set_lhs (new_stmt, new_temp);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		}

	      if (slp_node)
		SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
	      else
		{
		  if (!prev_stmt_info)
		    STMT_VINFO_VEC_STMT (stmt_info)
		      = *vec_stmt = new_stmt_info;
		  else
		    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
		  prev_stmt_info = new_stmt_info;
		}
	    }
	}
      break;

    case WIDEN:
      /* In case the vectorization factor (VF) is bigger than the number
	 of elements that we can fit in a vectype (nunits), we have to
	 generate more than one vector stmt - i.e - we need to "unroll"
	 the vector stmt by a factor VF/nunits.  */
      for (j = 0; j < ncopies; j++)
	{
	  /* Handle uses.  */
	  if (j == 0)
	    {
	      if (slp_node)
		{
		  if (code == WIDEN_LSHIFT_EXPR)
		    {
		      unsigned int k;

		      vec_oprnd1 = op1;
		      /* Store vec_oprnd1 for every vector stmt to be created
			 for SLP_NODE.  We check during the analysis that all
			 the shift arguments are the same.  */
		      for (k = 0; k < slp_node->vec_stmts_size - 1; k++)
			vec_oprnds1.quick_push (vec_oprnd1);

		      vect_get_vec_defs (op0, NULL_TREE, stmt_info,
					 &vec_oprnds0, NULL, slp_node);
		    }
		  else
		    vect_get_vec_defs (op0, op1, stmt_info, &vec_oprnds0,
				       &vec_oprnds1, slp_node);
		}
	      else
		{
		  vec_oprnd0 = vect_get_vec_def_for_operand (op0, stmt_info);
		  vec_oprnds0.quick_push (vec_oprnd0);
		  if (op_type == binary_op)
		    {
		      if (code == WIDEN_LSHIFT_EXPR)
			vec_oprnd1 = op1;
		      else
			vec_oprnd1
			  = vect_get_vec_def_for_operand (op1, stmt_info);
		      vec_oprnds1.quick_push (vec_oprnd1);
		    }
		}
	    }
	  else
	    {
	      vec_oprnd0 = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd0);
	      vec_oprnds0.truncate (0);
	      vec_oprnds0.quick_push (vec_oprnd0);
	      if (op_type == binary_op)
		{
		  if (code == WIDEN_LSHIFT_EXPR)
		    vec_oprnd1 = op1;
		  else
		    vec_oprnd1 = vect_get_vec_def_for_stmt_copy (vinfo,
								 vec_oprnd1);
		  vec_oprnds1.truncate (0);
		  vec_oprnds1.quick_push (vec_oprnd1);
		}
	    }

	  /* Arguments are ready.  Create the new vector stmts.  */
	  for (i = multi_step_cvt; i >= 0; i--)
	    {
	      tree this_dest = vec_dsts[i];
	      enum tree_code c1 = code1, c2 = code2;
	      if (i == 0 && codecvt2 != ERROR_MARK)
		{
		  c1 = codecvt1;
		  c2 = codecvt2;
		}
	      vect_create_vectorized_promotion_stmts (&vec_oprnds0,
						      &vec_oprnds1, stmt_info,
						      this_dest, gsi,
						      c1, c2, decl1, decl2,
						      op_type);
	    }

	  FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	    {
	      stmt_vec_info new_stmt_info;
	      if (cvt_type)
		{
		  if (codecvt1 == CALL_EXPR)
		    {
		      gcall *new_stmt = gimple_build_call (decl1, 1, vop0);
		      new_temp = make_ssa_name (vec_dest, new_stmt);
		      gimple_call_set_lhs (new_stmt, new_temp);
		      new_stmt_info
			= vect_finish_stmt_generation (stmt_info, new_stmt,
						       gsi);
		    }
		  else
		    {
		      gcc_assert (TREE_CODE_LENGTH (codecvt1) == unary_op);
		      new_temp = make_ssa_name (vec_dest);
		      gassign *new_stmt
			= gimple_build_assign (new_temp, codecvt1, vop0);
		      new_stmt_info
			= vect_finish_stmt_generation (stmt_info, new_stmt,
						       gsi);
		    }
		}
	      else
		new_stmt_info = vinfo->lookup_def (vop0);

	      if (slp_node)
		SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
	      else
		{
		  if (!prev_stmt_info)
		    STMT_VINFO_VEC_STMT (stmt_info) = new_stmt_info;
		  else
		    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
		  prev_stmt_info = new_stmt_info;
		}
	    }
	}

      *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
      break;

    case NARROW:
      /* In case the vectorization factor (VF) is bigger than the number
	 of elements that we can fit in a vectype (nunits), we have to
	 generate more than one vector stmt - i.e - we need to "unroll"
	 the vector stmt by a factor VF/nunits.  */
      for (j = 0; j < ncopies; j++)
	{
	  /* Handle uses.  */
	  if (slp_node)
	    vect_get_vec_defs (op0, NULL_TREE, stmt_info, &vec_oprnds0, NULL,
			       slp_node);
	  else
	    {
	      vec_oprnds0.truncate (0);
	      vect_get_loop_based_defs (&last_oprnd, stmt_info, &vec_oprnds0,
					vect_pow2 (multi_step_cvt) - 1);
	    }

	  /* Arguments are ready.  Create the new vector stmts.  */
	  if (cvt_type)
	    FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	      {
		if (codecvt1 == CALL_EXPR)
		  {
		    gcall *new_stmt = gimple_build_call (decl1, 1, vop0);
		    new_temp = make_ssa_name (vec_dest, new_stmt);
		    gimple_call_set_lhs (new_stmt, new_temp);
		    vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		  }
		else
		  {
		    gcc_assert (TREE_CODE_LENGTH (codecvt1) == unary_op);
		    new_temp = make_ssa_name (vec_dest);
		    gassign *new_stmt
		      = gimple_build_assign (new_temp, codecvt1, vop0);
		    vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		  }

		vec_oprnds0[i] = new_temp;
	      }

	  vect_create_vectorized_demotion_stmts (&vec_oprnds0, multi_step_cvt,
						 stmt_info, vec_dsts, gsi,
						 slp_node, code1,
						 &prev_stmt_info);
	}

      *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
      break;
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  interm_types.release ();

  return true;
}


/* Function vectorizable_assignment.

   Check if STMT_INFO performs an assignment (copy) that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_assignment (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			 stmt_vec_info *vec_stmt, slp_tree slp_node,
			 stmt_vector_for_cost *cost_vec)
{
  tree vec_dest;
  tree scalar_dest;
  tree op;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  tree new_temp;
  enum vect_def_type dt[1] = {vect_unknown_def_type};
  int ndts = 1;
  int ncopies;
  int i, j;
  vec<tree> vec_oprnds = vNULL;
  tree vop;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec_info *vinfo = stmt_info->vinfo;
  stmt_vec_info prev_stmt_info = NULL;
  enum tree_code code;
  tree vectype_in;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  /* Is vectorizable assignment?  */
  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  scalar_dest = gimple_assign_lhs (stmt);
  if (TREE_CODE (scalar_dest) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (gimple_assign_single_p (stmt)
      || code == PAREN_EXPR
      || CONVERT_EXPR_CODE_P (code))
    op = gimple_assign_rhs1 (stmt);
  else
    return false;

  if (code == VIEW_CONVERT_EXPR)
    op = TREE_OPERAND (op, 0);

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);

  if (!vect_is_simple_use (op, vinfo, &dt[0], &vectype_in))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }

  /* We can handle NOP_EXPR conversions that do not change the number
     of elements or the vector size.  */
  if ((CONVERT_EXPR_CODE_P (code)
       || code == VIEW_CONVERT_EXPR)
      && (!vectype_in
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (vectype_in), nunits)
	  || maybe_ne (GET_MODE_SIZE (TYPE_MODE (vectype)),
		       GET_MODE_SIZE (TYPE_MODE (vectype_in)))))
    return false;

  /* We do not handle bit-precision changes.  */
  if ((CONVERT_EXPR_CODE_P (code)
       || code == VIEW_CONVERT_EXPR)
      && INTEGRAL_TYPE_P (TREE_TYPE (scalar_dest))
      && (!type_has_mode_precision_p (TREE_TYPE (scalar_dest))
	  || !type_has_mode_precision_p (TREE_TYPE (op)))
      /* But a conversion that does not change the bit-pattern is ok.  */
      && !((TYPE_PRECISION (TREE_TYPE (scalar_dest))
	    > TYPE_PRECISION (TREE_TYPE (op)))
	   && TYPE_UNSIGNED (TREE_TYPE (op)))
      /* Conversion between boolean types of different sizes is
	 a simple assignment in case their vectypes are same
	 boolean vectors.  */
      && (!VECTOR_BOOLEAN_TYPE_P (vectype)
	  || !VECTOR_BOOLEAN_TYPE_P (vectype_in)))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "type conversion to/from bit-precision "
                         "unsupported.\n");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = assignment_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_assignment");
      vect_model_simple_cost (stmt_info, ncopies, dt, ndts, slp_node, cost_vec);
      return true;
    }

  /* Transform.  */
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform assignment.\n");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle use.  */
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
	vect_get_vec_defs (op, NULL, stmt_info, &vec_oprnds, NULL, slp_node);
      else
	vect_get_vec_defs_for_stmt_copy (vinfo, &vec_oprnds, NULL);

      /* Arguments are ready. create the new vector stmt.  */
      stmt_vec_info new_stmt_info = NULL;
      FOR_EACH_VEC_ELT (vec_oprnds, i, vop)
       {
	 if (CONVERT_EXPR_CODE_P (code)
	     || code == VIEW_CONVERT_EXPR)
	   vop = build1 (VIEW_CONVERT_EXPR, vectype, vop);
	 gassign *new_stmt = gimple_build_assign (vec_dest, vop);
         new_temp = make_ssa_name (vec_dest, new_stmt);
         gimple_assign_set_lhs (new_stmt, new_temp);
	 new_stmt_info
	   = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
         if (slp_node)
	   SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
       }

      if (slp_node)
        continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

      prev_stmt_info = new_stmt_info;
    }

  vec_oprnds.release ();
  return true;
}


/* Return TRUE if CODE (a shift operation) is supported for SCALAR_TYPE
   either as shift by a scalar or by a vector.  */

bool
vect_supportable_shift (enum tree_code code, tree scalar_type)
{

  machine_mode vec_mode;
  optab optab;
  int icode;
  tree vectype;

  vectype = get_vectype_for_scalar_type (scalar_type);
  if (!vectype)
    return false;

  optab = optab_for_tree_code (code, vectype, optab_scalar);
  if (!optab
      || optab_handler (optab, TYPE_MODE (vectype)) == CODE_FOR_nothing)
    {
      optab = optab_for_tree_code (code, vectype, optab_vector);
      if (!optab
          || (optab_handler (optab, TYPE_MODE (vectype))
                      == CODE_FOR_nothing))
        return false;
    }

  vec_mode = TYPE_MODE (vectype);
  icode = (int) optab_handler (optab, vec_mode);
  if (icode == CODE_FOR_nothing)
    return false;

  return true;
}


/* Function vectorizable_shift.

   Check if STMT_INFO performs a shift operation that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_shift (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    stmt_vec_info *vec_stmt, slp_tree slp_node,
		    stmt_vector_for_cost *cost_vec)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL;
  tree vec_oprnd1 = NULL_TREE;
  tree vectype;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code;
  machine_mode vec_mode;
  tree new_temp;
  optab optab;
  int icode;
  machine_mode optab_op2_mode;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 2;
  stmt_vec_info prev_stmt_info;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  tree vectype_out;
  tree op1_vectype;
  int ncopies;
  int j, i;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  tree vop0, vop1;
  unsigned int k;
  bool scalar_shift_arg = true;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec_info *vinfo = stmt_info->vinfo;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  /* Is STMT a vectorizable binary/unary operation?   */
  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  if (TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (!(code == LSHIFT_EXPR || code == RSHIFT_EXPR || code == LROTATE_EXPR
      || code == RROTATE_EXPR))
    return false;

  scalar_dest = gimple_assign_lhs (stmt);
  vectype_out = STMT_VINFO_VECTYPE (stmt_info);
  if (!type_has_mode_precision_p (TREE_TYPE (scalar_dest)))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "bit-precision shifts not supported.\n");
      return false;
    }

  op0 = gimple_assign_rhs1 (stmt);
  if (!vect_is_simple_use (op0, vinfo, &dt[0], &vectype))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }
  /* If op0 is an external or constant def use a vector type with
     the same size as the output vector type.  */
  if (!vectype)
    vectype = get_same_sized_vectype (TREE_TYPE (op0), vectype_out);
  if (vec_stmt)
    gcc_assert (vectype);
  if (!vectype)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "no vectype for scalar type\n");
      return false;
    }

  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype);
  if (maybe_ne (nunits_out, nunits_in))
    return false;

  op1 = gimple_assign_rhs2 (stmt);
  stmt_vec_info op1_def_stmt_info;
  if (!vect_is_simple_use (op1, vinfo, &dt[1], &op1_vectype,
			   &op1_def_stmt_info))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);

  /* Determine whether the shift amount is a vector, or scalar.  If the
     shift/rotate amount is a vector, use the vector/vector shift optabs.  */

  if ((dt[1] == vect_internal_def
       || dt[1] == vect_induction_def)
      && !slp_node)
    scalar_shift_arg = false;
  else if (dt[1] == vect_constant_def
	   || dt[1] == vect_external_def
	   || dt[1] == vect_internal_def)
    {
      /* In SLP, need to check whether the shift count is the same,
	 in loops if it is a constant or invariant, it is always
	 a scalar shift.  */
      if (slp_node)
	{
	  vec<stmt_vec_info> stmts = SLP_TREE_SCALAR_STMTS (slp_node);
	  stmt_vec_info slpstmt_info;

	  FOR_EACH_VEC_ELT (stmts, k, slpstmt_info)
	    {
	      gassign *slpstmt = as_a <gassign *> (slpstmt_info->stmt);
	      if (!operand_equal_p (gimple_assign_rhs2 (slpstmt), op1, 0))
		scalar_shift_arg = false;
	    }
	}

      /* If the shift amount is computed by a pattern stmt we cannot
         use the scalar amount directly thus give up and use a vector
	 shift.  */
      if (op1_def_stmt_info && is_pattern_stmt_p (op1_def_stmt_info))
	scalar_shift_arg = false;
    }
  else
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "operand mode requires invariant argument.\n");
      return false;
    }

  /* Vector shifted by vector.  */
  if (!scalar_shift_arg)
    {
      optab = optab_for_tree_code (code, vectype, optab_vector);
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "vector/vector shift/rotate found.\n");

      if (!op1_vectype)
	op1_vectype = get_same_sized_vectype (TREE_TYPE (op1), vectype_out);
      if (op1_vectype == NULL_TREE
	  || TYPE_MODE (op1_vectype) != TYPE_MODE (vectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "unusable type for last operand in"
                             " vector/vector shift/rotate.\n");
	  return false;
	}
    }
  /* See if the machine has a vector shifted by scalar insn and if not
     then see if it has a vector shifted by vector insn.  */
  else
    {
      optab = optab_for_tree_code (code, vectype, optab_scalar);
      if (optab
          && optab_handler (optab, TYPE_MODE (vectype)) != CODE_FOR_nothing)
        {
          if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location,
                             "vector/scalar shift/rotate found.\n");
        }
      else
        {
          optab = optab_for_tree_code (code, vectype, optab_vector);
          if (optab
               && (optab_handler (optab, TYPE_MODE (vectype))
                      != CODE_FOR_nothing))
            {
	      scalar_shift_arg = false;

              if (dump_enabled_p ())
                dump_printf_loc (MSG_NOTE, vect_location,
                                 "vector/vector shift/rotate found.\n");

              /* Unlike the other binary operators, shifts/rotates have
                 the rhs being int, instead of the same type as the lhs,
                 so make sure the scalar is the right type if we are
		 dealing with vectors of long long/long/short/char.  */
              if (dt[1] == vect_constant_def)
                op1 = fold_convert (TREE_TYPE (vectype), op1);
	      else if (!useless_type_conversion_p (TREE_TYPE (vectype),
						   TREE_TYPE (op1)))
		{
		  if (slp_node
		      && TYPE_MODE (TREE_TYPE (vectype))
			 != TYPE_MODE (TREE_TYPE (op1)))
		    {
                      if (dump_enabled_p ())
                        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                         "unusable type for last operand in"
                                         " vector/vector shift/rotate.\n");
		      return false;
		    }
		  if (vec_stmt && !slp_node)
		    {
		      op1 = fold_convert (TREE_TYPE (vectype), op1);
		      op1 = vect_init_vector (stmt_info, op1,
					      TREE_TYPE (vectype), NULL);
		    }
		}
            }
        }
    }

  /* Supportable by target?  */
  if (!optab)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "no optab.\n");
      return false;
    }
  vec_mode = TYPE_MODE (vectype);
  icode = (int) optab_handler (optab, vec_mode);
  if (icode == CODE_FOR_nothing)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "op not supported by target.\n");
      /* Check only during analysis.  */
      if (maybe_ne (GET_MODE_SIZE (vec_mode), UNITS_PER_WORD)
	  || (!vec_stmt
	      && !vect_worthwhile_without_simd_p (vinfo, code)))
        return false;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "proceeding using word mode.\n");
    }

  /* Worthwhile without SIMD support?  Check only during analysis.  */
  if (!vec_stmt
      && !VECTOR_MODE_P (TYPE_MODE (vectype))
      && !vect_worthwhile_without_simd_p (vinfo, code))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not worthwhile without SIMD support.\n");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = shift_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_shift");
      vect_model_simple_cost (stmt_info, ncopies, dt, ndts, slp_node, cost_vec);
      return true;
    }

  /* Transform.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform binary/unary operation.\n");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
        {
          if (scalar_shift_arg)
            {
              /* Vector shl and shr insn patterns can be defined with scalar
                 operand 2 (shift operand).  In this case, use constant or loop
                 invariant op1 directly, without extending it to vector mode
                 first.  */
              optab_op2_mode = insn_data[icode].operand[2].mode;
              if (!VECTOR_MODE_P (optab_op2_mode))
                {
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_NOTE, vect_location,
                                     "operand 1 using scalar mode.\n");
                  vec_oprnd1 = op1;
                  vec_oprnds1.create (slp_node ? slp_node->vec_stmts_size : 1);
                  vec_oprnds1.quick_push (vec_oprnd1);
                  if (slp_node)
                    {
                      /* Store vec_oprnd1 for every vector stmt to be created
                         for SLP_NODE.  We check during the analysis that all
                         the shift arguments are the same.
                         TODO: Allow different constants for different vector
                         stmts generated for an SLP instance.  */
                      for (k = 0; k < slp_node->vec_stmts_size - 1; k++)
                        vec_oprnds1.quick_push (vec_oprnd1);
                    }
                }
            }

          /* vec_oprnd1 is available if operand 1 should be of a scalar-type
             (a special case for certain kind of vector shifts); otherwise,
             operand 1 should be of a vector type (the usual case).  */
          if (vec_oprnd1)
	    vect_get_vec_defs (op0, NULL_TREE, stmt_info, &vec_oprnds0, NULL,
			       slp_node);
          else
	    vect_get_vec_defs (op0, op1, stmt_info, &vec_oprnds0, &vec_oprnds1,
			       slp_node);
        }
      else
	vect_get_vec_defs_for_stmt_copy (vinfo, &vec_oprnds0, &vec_oprnds1);

      /* Arguments are ready.  Create the new vector stmt.  */
      stmt_vec_info new_stmt_info = NULL;
      FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
        {
          vop1 = vec_oprnds1[i];
	  gassign *new_stmt = gimple_build_assign (vec_dest, code, vop0, vop1);
          new_temp = make_ssa_name (vec_dest, new_stmt);
          gimple_assign_set_lhs (new_stmt, new_temp);
	  new_stmt_info
	    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
          if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
        }

      if (slp_node)
        continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
      prev_stmt_info = new_stmt_info;
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();

  return true;
}


/* Function vectorizable_operation.

   Check if STMT_INFO performs a binary, unary or ternary operation that can
   be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_operation (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			stmt_vec_info *vec_stmt, slp_tree slp_node,
			stmt_vector_for_cost *cost_vec)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL_TREE, op2 = NULL_TREE;
  tree vectype;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code, orig_code;
  machine_mode vec_mode;
  tree new_temp;
  int op_type;
  optab optab;
  bool target_support_p;
  enum vect_def_type dt[3]
    = {vect_unknown_def_type, vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 3;
  stmt_vec_info prev_stmt_info;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  tree vectype_out;
  int ncopies;
  int j, i;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  vec<tree> vec_oprnds2 = vNULL;
  tree vop0, vop1, vop2;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec_info *vinfo = stmt_info->vinfo;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  /* Is STMT a vectorizable binary/unary operation?   */
  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  if (TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return false;

  orig_code = code = gimple_assign_rhs_code (stmt);

  /* For pointer addition and subtraction, we should use the normal
     plus and minus for the vector operation.  */
  if (code == POINTER_PLUS_EXPR)
    code = PLUS_EXPR;
  if (code == POINTER_DIFF_EXPR)
    code = MINUS_EXPR;

  /* Support only unary or binary operations.  */
  op_type = TREE_CODE_LENGTH (code);
  if (op_type != unary_op && op_type != binary_op && op_type != ternary_op)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "num. args = %d (not unary/binary/ternary op).\n",
                         op_type);
      return false;
    }

  scalar_dest = gimple_assign_lhs (stmt);
  vectype_out = STMT_VINFO_VECTYPE (stmt_info);

  /* Most operations cannot handle bit-precision types without extra
     truncations.  */
  if (!VECTOR_BOOLEAN_TYPE_P (vectype_out)
      && !type_has_mode_precision_p (TREE_TYPE (scalar_dest))
      /* Exception are bitwise binary operations.  */
      && code != BIT_IOR_EXPR
      && code != BIT_XOR_EXPR
      && code != BIT_AND_EXPR)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "bit-precision arithmetic not supported.\n");
      return false;
    }

  op0 = gimple_assign_rhs1 (stmt);
  if (!vect_is_simple_use (op0, vinfo, &dt[0], &vectype))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }
  /* If op0 is an external or constant def use a vector type with
     the same size as the output vector type.  */
  if (!vectype)
    {
      /* For boolean type we cannot determine vectype by
	 invariant value (don't know whether it is a vector
	 of booleans or vector of integers).  We use output
	 vectype because operations on boolean don't change
	 type.  */
      if (VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (op0)))
	{
	  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (scalar_dest)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "not supported operation on bool value.\n");
	      return false;
	    }
	  vectype = vectype_out;
	}
      else
	vectype = get_same_sized_vectype (TREE_TYPE (op0), vectype_out);
    }
  if (vec_stmt)
    gcc_assert (vectype);
  if (!vectype)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "no vectype for scalar type %T\n",
			 TREE_TYPE (op0));

      return false;
    }

  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype);
  if (maybe_ne (nunits_out, nunits_in))
    return false;

  if (op_type == binary_op || op_type == ternary_op)
    {
      op1 = gimple_assign_rhs2 (stmt);
      if (!vect_is_simple_use (op1, vinfo, &dt[1]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.\n");
	  return false;
	}
    }
  if (op_type == ternary_op)
    {
      op2 = gimple_assign_rhs3 (stmt);
      if (!vect_is_simple_use (op2, vinfo, &dt[2]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.\n");
	  return false;
	}
    }

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);

  /* Shifts are handled in vectorizable_shift ().  */
  if (code == LSHIFT_EXPR || code == RSHIFT_EXPR || code == LROTATE_EXPR
      || code == RROTATE_EXPR)
   return false;

  /* Supportable by target?  */

  vec_mode = TYPE_MODE (vectype);
  if (code == MULT_HIGHPART_EXPR)
    target_support_p = can_mult_highpart_p (vec_mode, TYPE_UNSIGNED (vectype));
  else
    {
      optab = optab_for_tree_code (code, vectype, optab_default);
      if (!optab)
	{
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "no optab.\n");
	  return false;
	}
      target_support_p = (optab_handler (optab, vec_mode)
			  != CODE_FOR_nothing);
    }

  if (!target_support_p)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "op not supported by target.\n");
      /* Check only during analysis.  */
      if (maybe_ne (GET_MODE_SIZE (vec_mode), UNITS_PER_WORD)
	  || (!vec_stmt && !vect_worthwhile_without_simd_p (vinfo, code)))
        return false;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "proceeding using word mode.\n");
    }

  /* Worthwhile without SIMD support?  Check only during analysis.  */
  if (!VECTOR_MODE_P (vec_mode)
      && !vec_stmt
      && !vect_worthwhile_without_simd_p (vinfo, code))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not worthwhile without SIMD support.\n");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = op_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_operation");
      vect_model_simple_cost (stmt_info, ncopies, dt, ndts, slp_node, cost_vec);
      return true;
    }

  /* Transform.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform binary/unary operation.\n");

  /* POINTER_DIFF_EXPR has pointer arguments which are vectorized as
     vectors with unsigned elements, but the result is signed.  So, we
     need to compute the MINUS_EXPR into vectype temporary and
     VIEW_CONVERT_EXPR it into the final vectype_out result.  */
  tree vec_cvt_dest = NULL_TREE;
  if (orig_code == POINTER_DIFF_EXPR)
    {
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      vec_cvt_dest = vect_create_destination_var (scalar_dest, vectype_out);
    }
  /* Handle def.  */
  else
    vec_dest = vect_create_destination_var (scalar_dest, vectype_out);

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  In doing so, we record a pointer
     from one copy of the vector stmt to the next, in the field
     STMT_VINFO_RELATED_STMT.  This is necessary in order to allow following
     stages to find the correct vector defs to be used when vectorizing
     stmts that use the defs of the current stmt.  The example below
     illustrates the vectorization process when VF=16 and nunits=4 (i.e.,
     we need to create 4 vectorized stmts):

     before vectorization:
                                RELATED_STMT    VEC_STMT
        S1:     x = memref      -               -
        S2:     z = x + 1       -               -

     step 1: vectorize stmt S1 (done in vectorizable_load. See more details
             there):
                                RELATED_STMT    VEC_STMT
        VS1_0:  vx0 = memref0   VS1_1           -
        VS1_1:  vx1 = memref1   VS1_2           -
        VS1_2:  vx2 = memref2   VS1_3           -
        VS1_3:  vx3 = memref3   -               -
        S1:     x = load        -               VS1_0
        S2:     z = x + 1       -               -

     step2: vectorize stmt S2 (done here):
        To vectorize stmt S2 we first need to find the relevant vector
        def for the first operand 'x'.  This is, as usual, obtained from
        the vector stmt recorded in the STMT_VINFO_VEC_STMT of the stmt
        that defines 'x' (S1).  This way we find the stmt VS1_0, and the
        relevant vector def 'vx0'.  Having found 'vx0' we can generate
        the vector stmt VS2_0, and as usual, record it in the
        STMT_VINFO_VEC_STMT of stmt S2.
        When creating the second copy (VS2_1), we obtain the relevant vector
        def from the vector stmt recorded in the STMT_VINFO_RELATED_STMT of
        stmt VS1_0.  This way we find the stmt VS1_1 and the relevant
        vector def 'vx1'.  Using 'vx1' we create stmt VS2_1 and record a
        pointer to it in the STMT_VINFO_RELATED_STMT of the vector stmt VS2_0.
        Similarly when creating stmts VS2_2 and VS2_3.  This is the resulting
        chain of stmts and pointers:
                                RELATED_STMT    VEC_STMT
        VS1_0:  vx0 = memref0   VS1_1           -
        VS1_1:  vx1 = memref1   VS1_2           -
        VS1_2:  vx2 = memref2   VS1_3           -
        VS1_3:  vx3 = memref3   -               -
        S1:     x = load        -               VS1_0
        VS2_0:  vz0 = vx0 + v1  VS2_1           -
        VS2_1:  vz1 = vx1 + v1  VS2_2           -
        VS2_2:  vz2 = vx2 + v1  VS2_3           -
        VS2_3:  vz3 = vx3 + v1  -               -
        S2:     z = x + 1       -               VS2_0  */

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
	{
	  if (op_type == binary_op)
	    vect_get_vec_defs (op0, op1, stmt_info, &vec_oprnds0, &vec_oprnds1,
			       slp_node);
	  else if (op_type == ternary_op)
	    {
	      if (slp_node)
		{
		  auto_vec<tree> ops(3);
		  ops.quick_push (op0);
		  ops.quick_push (op1);
		  ops.quick_push (op2);
		  auto_vec<vec<tree> > vec_defs(3);
		  vect_get_slp_defs (ops, slp_node, &vec_defs);
		  vec_oprnds0 = vec_defs[0];
		  vec_oprnds1 = vec_defs[1];
		  vec_oprnds2 = vec_defs[2];
		}
	      else
		{
		  vect_get_vec_defs (op0, op1, stmt_info, &vec_oprnds0,
				     &vec_oprnds1, NULL);
		  vect_get_vec_defs (op2, NULL_TREE, stmt_info, &vec_oprnds2,
				     NULL, NULL);
		}
	    }
	  else
	    vect_get_vec_defs (op0, NULL_TREE, stmt_info, &vec_oprnds0, NULL,
			       slp_node);
	}
      else
	{
	  vect_get_vec_defs_for_stmt_copy (vinfo, &vec_oprnds0, &vec_oprnds1);
	  if (op_type == ternary_op)
	    {
	      tree vec_oprnd = vec_oprnds2.pop ();
	      vec_oprnds2.quick_push (vect_get_vec_def_for_stmt_copy (vinfo,
							           vec_oprnd));
	    }
	}

      /* Arguments are ready.  Create the new vector stmt.  */
      stmt_vec_info new_stmt_info = NULL;
      FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
        {
	  vop1 = ((op_type == binary_op || op_type == ternary_op)
		  ? vec_oprnds1[i] : NULL_TREE);
	  vop2 = ((op_type == ternary_op)
		  ? vec_oprnds2[i] : NULL_TREE);
	  gassign *new_stmt = gimple_build_assign (vec_dest, code,
						   vop0, vop1, vop2);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  gimple_assign_set_lhs (new_stmt, new_temp);
	  new_stmt_info
	    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	  if (vec_cvt_dest)
	    {
	      new_temp = build1 (VIEW_CONVERT_EXPR, vectype_out, new_temp);
	      gassign *new_stmt
		= gimple_build_assign (vec_cvt_dest, VIEW_CONVERT_EXPR,
				       new_temp);
	      new_temp = make_ssa_name (vec_cvt_dest, new_stmt);
	      gimple_assign_set_lhs (new_stmt, new_temp);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	    }
          if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
        }

      if (slp_node)
        continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
      prev_stmt_info = new_stmt_info;
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  vec_oprnds2.release ();

  return true;
}

/* A helper function to ensure data reference DR_INFO's base alignment.  */

static void
ensure_base_align (dr_vec_info *dr_info)
{
  if (dr_info->misalignment == DR_MISALIGNMENT_UNINITIALIZED)
    return;

  if (dr_info->base_misaligned)
    {
      tree base_decl = dr_info->base_decl;

      unsigned int align_base_to
	= DR_TARGET_ALIGNMENT (dr_info) * BITS_PER_UNIT;

      if (decl_in_symtab_p (base_decl))
	symtab_node::get (base_decl)->increase_alignment (align_base_to);
      else
	{
	  SET_DECL_ALIGN (base_decl, align_base_to);
          DECL_USER_ALIGN (base_decl) = 1;
	}
      dr_info->base_misaligned = false;
    }
}


/* Function get_group_alias_ptr_type.

   Return the alias type for the group starting at FIRST_STMT_INFO.  */

static tree
get_group_alias_ptr_type (stmt_vec_info first_stmt_info)
{
  struct data_reference *first_dr, *next_dr;

  first_dr = STMT_VINFO_DATA_REF (first_stmt_info);
  stmt_vec_info next_stmt_info = DR_GROUP_NEXT_ELEMENT (first_stmt_info);
  while (next_stmt_info)
    {
      next_dr = STMT_VINFO_DATA_REF (next_stmt_info);
      if (get_alias_set (DR_REF (first_dr))
	  != get_alias_set (DR_REF (next_dr)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "conflicting alias set types.\n");
	  return ptr_type_node;
	}
      next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
    }
  return reference_alias_ptr_type (DR_REF (first_dr));
}


/* Function vectorizable_store.

   Check if STMT_INFO defines a non scalar data-ref (array/pointer/structure)
   that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_store (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    stmt_vec_info *vec_stmt, slp_tree slp_node,
		    stmt_vector_for_cost *cost_vec)
{
  tree data_ref;
  tree op;
  tree vec_oprnd = NULL_TREE;
  tree elem_type;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  machine_mode vec_mode;
  tree dummy;
  enum dr_alignment_support alignment_support_scheme;
  enum vect_def_type rhs_dt = vect_unknown_def_type;
  enum vect_def_type mask_dt = vect_unknown_def_type;
  stmt_vec_info prev_stmt_info = NULL;
  tree dataref_ptr = NULL_TREE;
  tree dataref_offset = NULL_TREE;
  gimple *ptr_incr = NULL;
  int ncopies;
  int j;
  stmt_vec_info first_stmt_info;
  bool grouped_store;
  unsigned int group_size, i;
  vec<tree> oprnds = vNULL;
  vec<tree> result_chain = vNULL;
  tree offset = NULL_TREE;
  vec<tree> vec_oprnds = vNULL;
  bool slp = (slp_node != NULL);
  unsigned int vec_num;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec_info *vinfo = stmt_info->vinfo;
  tree aggr_type;
  gather_scatter_info gs_info;
  poly_uint64 vf;
  vec_load_store_type vls_type;
  tree ref_type;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  /* Is vectorizable store? */

  tree mask = NULL_TREE, mask_vectype = NULL_TREE;
  if (gassign *assign = dyn_cast <gassign *> (stmt_info->stmt))
    {
      tree scalar_dest = gimple_assign_lhs (assign);
      if (TREE_CODE (scalar_dest) == VIEW_CONVERT_EXPR
	  && is_pattern_stmt_p (stmt_info))
	scalar_dest = TREE_OPERAND (scalar_dest, 0);
      if (TREE_CODE (scalar_dest) != ARRAY_REF
	  && TREE_CODE (scalar_dest) != BIT_FIELD_REF
	  && TREE_CODE (scalar_dest) != INDIRECT_REF
	  && TREE_CODE (scalar_dest) != COMPONENT_REF
	  && TREE_CODE (scalar_dest) != IMAGPART_EXPR
	  && TREE_CODE (scalar_dest) != REALPART_EXPR
	  && TREE_CODE (scalar_dest) != MEM_REF)
	return false;
    }
  else
    {
      gcall *call = dyn_cast <gcall *> (stmt_info->stmt);
      if (!call || !gimple_call_internal_p (call))
	return false;

      internal_fn ifn = gimple_call_internal_fn (call);
      if (!internal_store_fn_p (ifn))
	return false;

      if (slp_node != NULL)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "SLP of masked stores not supported.\n");
	  return false;
	}

      int mask_index = internal_fn_mask_index (ifn);
      if (mask_index >= 0)
	{
	  mask = gimple_call_arg (call, mask_index);
	  if (!vect_check_load_store_mask (stmt_info, mask, &mask_dt,
					   &mask_vectype))
	    return false;
	}
    }

  op = vect_get_store_rhs (stmt_info);

  /* Cannot have hybrid store SLP -- that would mean storing to the
     same location twice.  */
  gcc_assert (slp == PURE_SLP_STMT (stmt_info));

  tree vectype = STMT_VINFO_VECTYPE (stmt_info), rhs_vectype = NULL_TREE;
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
    }
  else
    vf = 1;

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);

  /* FORNOW.  This restriction should be relaxed.  */
  if (loop && nested_in_vect_loop_p (loop, stmt_info) && ncopies > 1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "multiple types in nested loop.\n");
      return false;
    }

  if (!vect_check_store_rhs (stmt_info, op, &rhs_dt, &rhs_vectype, &vls_type))
    return false;

  elem_type = TREE_TYPE (vectype);
  vec_mode = TYPE_MODE (vectype);

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  vect_memory_access_type memory_access_type;
  if (!get_load_store_type (stmt_info, vectype, slp, mask, vls_type, ncopies,
			    &memory_access_type, &gs_info))
    return false;

  if (mask)
    {
      if (memory_access_type == VMAT_CONTIGUOUS)
	{
	  if (!VECTOR_MODE_P (vec_mode)
	      || !can_vec_mask_load_store_p (vec_mode,
					     TYPE_MODE (mask_vectype), false))
	    return false;
	}
      else if (memory_access_type != VMAT_LOAD_STORE_LANES
	       && (memory_access_type != VMAT_GATHER_SCATTER || gs_info.decl))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported access type for masked store.\n");
	  return false;
	}
    }
  else
    {
      /* FORNOW. In some cases can vectorize even if data-type not supported
	 (e.g. - array initialization with 0).  */
      if (optab_handler (mov_optab, vec_mode) == CODE_FOR_nothing)
	return false;
    }

  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info), *first_dr_info = NULL;
  grouped_store = (STMT_VINFO_GROUPED_ACCESS (stmt_info)
		   && memory_access_type != VMAT_GATHER_SCATTER
		   && (slp || memory_access_type != VMAT_CONTIGUOUS));
  if (grouped_store)
    {
      first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      first_dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
      group_size = DR_GROUP_SIZE (first_stmt_info);
    }
  else
    {
      first_stmt_info = stmt_info;
      first_dr_info = dr_info;
      group_size = vec_num = 1;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) = memory_access_type;

      if (loop_vinfo
	  && LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo))
	check_load_store_masking (loop_vinfo, vectype, vls_type, group_size,
				  memory_access_type, &gs_info);

      STMT_VINFO_TYPE (stmt_info) = store_vec_info_type;
      vect_model_store_cost (stmt_info, ncopies, rhs_dt, memory_access_type,
			     vls_type, slp_node, cost_vec);
      return true;
    }
  gcc_assert (memory_access_type == STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info));

  /* Transform.  */

  ensure_base_align (dr_info);

  if (memory_access_type == VMAT_GATHER_SCATTER && gs_info.decl)
    {
      tree vec_oprnd0 = NULL_TREE, vec_oprnd1 = NULL_TREE, src;
      tree arglist = TYPE_ARG_TYPES (TREE_TYPE (gs_info.decl));
      tree rettype, srctype, ptrtype, idxtype, masktype, scaletype;
      tree ptr, mask, var, scale, perm_mask = NULL_TREE;
      edge pe = loop_preheader_edge (loop);
      gimple_seq seq;
      basic_block new_bb;
      enum { NARROW, NONE, WIDEN } modifier;
      poly_uint64 scatter_off_nunits
	= TYPE_VECTOR_SUBPARTS (gs_info.offset_vectype);

      if (known_eq (nunits, scatter_off_nunits))
	modifier = NONE;
      else if (known_eq (nunits * 2, scatter_off_nunits))
	{
	  modifier = WIDEN;

	  /* Currently gathers and scatters are only supported for
	     fixed-length vectors.  */
	  unsigned int count = scatter_off_nunits.to_constant ();
	  vec_perm_builder sel (count, count, 1);
	  for (i = 0; i < (unsigned int) count; ++i)
	    sel.quick_push (i | (count / 2));

	  vec_perm_indices indices (sel, 1, count);
	  perm_mask = vect_gen_perm_mask_checked (gs_info.offset_vectype,
						  indices);
	  gcc_assert (perm_mask != NULL_TREE);
	}
      else if (known_eq (nunits, scatter_off_nunits * 2))
	{
	  modifier = NARROW;

	  /* Currently gathers and scatters are only supported for
	     fixed-length vectors.  */
	  unsigned int count = nunits.to_constant ();
	  vec_perm_builder sel (count, count, 1);
	  for (i = 0; i < (unsigned int) count; ++i)
	    sel.quick_push (i | (count / 2));

	  vec_perm_indices indices (sel, 2, count);
	  perm_mask = vect_gen_perm_mask_checked (vectype, indices);
	  gcc_assert (perm_mask != NULL_TREE);
	  ncopies *= 2;
	}
      else
	gcc_unreachable ();

      rettype = TREE_TYPE (TREE_TYPE (gs_info.decl));
      ptrtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      masktype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      idxtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      srctype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      scaletype = TREE_VALUE (arglist);

      gcc_checking_assert (TREE_CODE (masktype) == INTEGER_TYPE
			   && TREE_CODE (rettype) == VOID_TYPE);

      ptr = fold_convert (ptrtype, gs_info.base);
      if (!is_gimple_min_invariant (ptr))
	{
	  ptr = force_gimple_operand (ptr, &seq, true, NULL_TREE);
	  new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
	  gcc_assert (!new_bb);
	}

      /* Currently we support only unconditional scatter stores,
	 so mask should be all ones.  */
      mask = build_int_cst (masktype, -1);
      mask = vect_init_vector (stmt_info, mask, masktype, NULL);

      scale = build_int_cst (scaletype, gs_info.scale);

      prev_stmt_info = NULL;
      for (j = 0; j < ncopies; ++j)
	{
	  if (j == 0)
	    {
	      src = vec_oprnd1
		= vect_get_vec_def_for_operand (op, stmt_info);
	      op = vec_oprnd0
		= vect_get_vec_def_for_operand (gs_info.offset, stmt_info);
	    }
	  else if (modifier != NONE && (j & 1))
	    {
	      if (modifier == WIDEN)
		{
		  src = vec_oprnd1
		    = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd1);
		  op = permute_vec_elements (vec_oprnd0, vec_oprnd0, perm_mask,
					     stmt_info, gsi);
		}
	      else if (modifier == NARROW)
		{
		  src = permute_vec_elements (vec_oprnd1, vec_oprnd1, perm_mask,
					      stmt_info, gsi);
		  op = vec_oprnd0
		    = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd0);
		}
	      else
		gcc_unreachable ();
	    }
	  else
	    {
	      src = vec_oprnd1
		= vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd1);
	      op = vec_oprnd0
		= vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnd0);
	    }

	  if (!useless_type_conversion_p (srctype, TREE_TYPE (src)))
	    {
	      gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (src)),
				    TYPE_VECTOR_SUBPARTS (srctype)));
	      var = vect_get_new_ssa_name (srctype, vect_simple_var);
	      src = build1 (VIEW_CONVERT_EXPR, srctype, src);
	      gassign *new_stmt
		= gimple_build_assign (var, VIEW_CONVERT_EXPR, src);
	      vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	      src = var;
	    }

	  if (!useless_type_conversion_p (idxtype, TREE_TYPE (op)))
	    {
	      gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (op)),
				    TYPE_VECTOR_SUBPARTS (idxtype)));
	      var = vect_get_new_ssa_name (idxtype, vect_simple_var);
	      op = build1 (VIEW_CONVERT_EXPR, idxtype, op);
	      gassign *new_stmt
		= gimple_build_assign (var, VIEW_CONVERT_EXPR, op);
	      vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	      op = var;
	    }

	  gcall *new_stmt
	    = gimple_build_call (gs_info.decl, 5, ptr, mask, op, src, scale);
	  stmt_vec_info new_stmt_info
	    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

	  if (prev_stmt_info == NULL)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
	  prev_stmt_info = new_stmt_info;
	}
      return true;
    }

  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    DR_GROUP_STORE_COUNT (DR_GROUP_FIRST_ELEMENT (stmt_info))++;

  if (grouped_store)
    {
      /* FORNOW */
      gcc_assert (!loop || !nested_in_vect_loop_p (loop, stmt_info));

      /* We vectorize all the stmts of the interleaving group when we
	 reach the last stmt in the group.  */
      if (DR_GROUP_STORE_COUNT (first_stmt_info)
	  < DR_GROUP_SIZE (first_stmt_info)
	  && !slp)
	{
	  *vec_stmt = NULL;
	  return true;
	}

      if (slp)
        {
          grouped_store = false;
          /* VEC_NUM is the number of vect stmts to be created for this 
             group.  */
          vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
	  first_stmt_info = SLP_TREE_SCALAR_STMTS (slp_node)[0];
	  gcc_assert (DR_GROUP_FIRST_ELEMENT (first_stmt_info)
		      == first_stmt_info);
	  first_dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
	  op = vect_get_store_rhs (first_stmt_info);
        } 
      else
        /* VEC_NUM is the number of vect stmts to be created for this 
           group.  */
	vec_num = group_size;

      ref_type = get_group_alias_ptr_type (first_stmt_info);
    }
  else
    ref_type = reference_alias_ptr_type (DR_REF (first_dr_info->dr));

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform store. ncopies = %d\n", ncopies);

  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_STRIDED_SLP)
    {
      gimple_stmt_iterator incr_gsi;
      bool insert_after;
      gimple *incr;
      tree offvar;
      tree ivstep;
      tree running_off;
      tree stride_base, stride_step, alias_off;
      tree vec_oprnd;
      unsigned int g;
      /* Checked by get_load_store_type.  */
      unsigned int const_nunits = nunits.to_constant ();

      gcc_assert (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo));
      gcc_assert (!nested_in_vect_loop_p (loop, stmt_info));

      stride_base
	= fold_build_pointer_plus
	    (DR_BASE_ADDRESS (first_dr_info->dr),
	     size_binop (PLUS_EXPR,
			 convert_to_ptrofftype (DR_OFFSET (first_dr_info->dr)),
			 convert_to_ptrofftype (DR_INIT (first_dr_info->dr))));
      stride_step = fold_convert (sizetype, DR_STEP (first_dr_info->dr));

      /* For a store with loop-invariant (but other than power-of-2)
         stride (i.e. not a grouped access) like so:

	   for (i = 0; i < n; i += stride)
	     array[i] = ...;

	 we generate a new induction variable and new stores from
	 the components of the (vectorized) rhs:

	   for (j = 0; ; j += VF*stride)
	     vectemp = ...;
	     tmp1 = vectemp[0];
	     array[j] = tmp1;
	     tmp2 = vectemp[1];
	     array[j + stride] = tmp2;
	     ...
         */

      unsigned nstores = const_nunits;
      unsigned lnel = 1;
      tree ltype = elem_type;
      tree lvectype = vectype;
      if (slp)
	{
	  if (group_size < const_nunits
	      && const_nunits % group_size == 0)
	    {
	      nstores = const_nunits / group_size;
	      lnel = group_size;
	      ltype = build_vector_type (elem_type, group_size);
	      lvectype = vectype;

	      /* First check if vec_extract optab doesn't support extraction
		 of vector elts directly.  */
	      scalar_mode elmode = SCALAR_TYPE_MODE (elem_type);
	      machine_mode vmode;
	      if (!mode_for_vector (elmode, group_size).exists (&vmode)
		  || !VECTOR_MODE_P (vmode)
		  || !targetm.vector_mode_supported_p (vmode)
		  || (convert_optab_handler (vec_extract_optab,
					     TYPE_MODE (vectype), vmode)
		      == CODE_FOR_nothing))
		{
		  /* Try to avoid emitting an extract of vector elements
		     by performing the extracts using an integer type of the
		     same size, extracting from a vector of those and then
		     re-interpreting it as the original vector type if
		     supported.  */
		  unsigned lsize
		    = group_size * GET_MODE_BITSIZE (elmode);
		  elmode = int_mode_for_size (lsize, 0).require ();
		  unsigned int lnunits = const_nunits / group_size;
		  /* If we can't construct such a vector fall back to
		     element extracts from the original vector type and
		     element size stores.  */
		  if (mode_for_vector (elmode, lnunits).exists (&vmode)
		      && VECTOR_MODE_P (vmode)
		      && targetm.vector_mode_supported_p (vmode)
		      && (convert_optab_handler (vec_extract_optab,
						 vmode, elmode)
			  != CODE_FOR_nothing))
		    {
		      nstores = lnunits;
		      lnel = group_size;
		      ltype = build_nonstandard_integer_type (lsize, 1);
		      lvectype = build_vector_type (ltype, nstores);
		    }
		  /* Else fall back to vector extraction anyway.
		     Fewer stores are more important than avoiding spilling
		     of the vector we extract from.  Compared to the
		     construction case in vectorizable_load no store-forwarding
		     issue exists here for reasonable archs.  */
		}
	    }
	  else if (group_size >= const_nunits
		   && group_size % const_nunits == 0)
	    {
	      nstores = 1;
	      lnel = const_nunits;
	      ltype = vectype;
	      lvectype = vectype;
	    }
	  ltype = build_aligned_type (ltype, TYPE_ALIGN (elem_type));
	  ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
	}

      ivstep = stride_step;
      ivstep = fold_build2 (MULT_EXPR, TREE_TYPE (ivstep), ivstep,
			    build_int_cst (TREE_TYPE (ivstep), vf));

      standard_iv_increment_position (loop, &incr_gsi, &insert_after);

      stride_base = cse_and_gimplify_to_preheader (loop_vinfo, stride_base);
      ivstep = cse_and_gimplify_to_preheader (loop_vinfo, ivstep);
      create_iv (stride_base, ivstep, NULL,
		 loop, &incr_gsi, insert_after,
		 &offvar, NULL);
      incr = gsi_stmt (incr_gsi);
      loop_vinfo->add_stmt (incr);

      stride_step = cse_and_gimplify_to_preheader (loop_vinfo, stride_step);

      prev_stmt_info = NULL;
      alias_off = build_int_cst (ref_type, 0);
      stmt_vec_info next_stmt_info = first_stmt_info;
      for (g = 0; g < group_size; g++)
	{
	  running_off = offvar;
	  if (g)
	    {
	      tree size = TYPE_SIZE_UNIT (ltype);
	      tree pos = fold_build2 (MULT_EXPR, sizetype, size_int (g),
				      size);
	      tree newoff = copy_ssa_name (running_off, NULL);
	      incr = gimple_build_assign (newoff, POINTER_PLUS_EXPR,
					  running_off, pos);
	      vect_finish_stmt_generation (stmt_info, incr, gsi);
	      running_off = newoff;
	    }
	  unsigned int group_el = 0;
	  unsigned HOST_WIDE_INT
	    elsz = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
	  for (j = 0; j < ncopies; j++)
	    {
	      /* We've set op and dt above, from vect_get_store_rhs,
		 and first_stmt_info == stmt_info.  */
	      if (j == 0)
		{
		  if (slp)
		    {
		      vect_get_vec_defs (op, NULL_TREE, stmt_info,
					 &vec_oprnds, NULL, slp_node);
		      vec_oprnd = vec_oprnds[0];
		    }
		  else
		    {
		      op = vect_get_store_rhs (next_stmt_info);
		      vec_oprnd = vect_get_vec_def_for_operand
			(op, next_stmt_info);
		    }
		}
	      else
		{
		  if (slp)
		    vec_oprnd = vec_oprnds[j];
		  else
		    vec_oprnd = vect_get_vec_def_for_stmt_copy (vinfo,
								vec_oprnd);
		}
	      /* Pun the vector to extract from if necessary.  */
	      if (lvectype != vectype)
		{
		  tree tem = make_ssa_name (lvectype);
		  gimple *pun
		    = gimple_build_assign (tem, build1 (VIEW_CONVERT_EXPR,
							lvectype, vec_oprnd));
		  vect_finish_stmt_generation (stmt_info, pun, gsi);
		  vec_oprnd = tem;
		}
	      for (i = 0; i < nstores; i++)
		{
		  tree newref, newoff;
		  gimple *incr, *assign;
		  tree size = TYPE_SIZE (ltype);
		  /* Extract the i'th component.  */
		  tree pos = fold_build2 (MULT_EXPR, bitsizetype,
					  bitsize_int (i), size);
		  tree elem = fold_build3 (BIT_FIELD_REF, ltype, vec_oprnd,
					   size, pos);

		  elem = force_gimple_operand_gsi (gsi, elem, true,
						   NULL_TREE, true,
						   GSI_SAME_STMT);

		  tree this_off = build_int_cst (TREE_TYPE (alias_off),
						 group_el * elsz);
		  newref = build2 (MEM_REF, ltype,
				   running_off, this_off);
		  vect_copy_ref_info (newref, DR_REF (first_dr_info->dr));

		  /* And store it to *running_off.  */
		  assign = gimple_build_assign (newref, elem);
		  stmt_vec_info assign_info
		    = vect_finish_stmt_generation (stmt_info, assign, gsi);

		  group_el += lnel;
		  if (! slp
		      || group_el == group_size)
		    {
		      newoff = copy_ssa_name (running_off, NULL);
		      incr = gimple_build_assign (newoff, POINTER_PLUS_EXPR,
						  running_off, stride_step);
		      vect_finish_stmt_generation (stmt_info, incr, gsi);

		      running_off = newoff;
		      group_el = 0;
		    }
		  if (g == group_size - 1
		      && !slp)
		    {
		      if (j == 0 && i == 0)
			STMT_VINFO_VEC_STMT (stmt_info)
			    = *vec_stmt = assign_info;
		      else
			STMT_VINFO_RELATED_STMT (prev_stmt_info) = assign_info;
		      prev_stmt_info = assign_info;
		    }
		}
	    }
	  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
	  if (slp)
	    break;
	}

      vec_oprnds.release ();
      return true;
    }

  auto_vec<tree> dr_chain (group_size);
  oprnds.create (group_size);

  alignment_support_scheme
    = vect_supportable_dr_alignment (first_dr_info, false);
  gcc_assert (alignment_support_scheme);
  vec_loop_masks *loop_masks
    = (loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
       ? &LOOP_VINFO_MASKS (loop_vinfo)
       : NULL);
  /* Targets with store-lane instructions must not require explicit
     realignment.  vect_supportable_dr_alignment always returns either
     dr_aligned or dr_unaligned_supported for masked operations.  */
  gcc_assert ((memory_access_type != VMAT_LOAD_STORE_LANES
	       && !mask
	       && !loop_masks)
	      || alignment_support_scheme == dr_aligned
	      || alignment_support_scheme == dr_unaligned_supported);

  if (memory_access_type == VMAT_CONTIGUOUS_DOWN
      || memory_access_type == VMAT_CONTIGUOUS_REVERSE)
    offset = size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1);

  tree bump;
  tree vec_offset = NULL_TREE;
  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    {
      aggr_type = NULL_TREE;
      bump = NULL_TREE;
    }
  else if (memory_access_type == VMAT_GATHER_SCATTER)
    {
      aggr_type = elem_type;
      vect_get_strided_load_store_ops (stmt_info, loop_vinfo, &gs_info,
				       &bump, &vec_offset);
    }
  else
    {
      if (memory_access_type == VMAT_LOAD_STORE_LANES)
	aggr_type = build_array_type_nelts (elem_type, vec_num * nunits);
      else
	aggr_type = vectype;
      bump = vect_get_data_ptr_increment (dr_info, aggr_type,
					  memory_access_type);
    }

  if (mask)
    LOOP_VINFO_HAS_MASK_STORE (loop_vinfo) = true;

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  For more details see documentation in
     vect_get_vec_def_for_copy_stmt.  */

  /* In case of interleaving (non-unit grouped access):

        S1:  &base + 2 = x2
        S2:  &base = x0
        S3:  &base + 1 = x1
        S4:  &base + 3 = x3

     We create vectorized stores starting from base address (the access of the
     first stmt in the chain (S2 in the above example), when the last store stmt
     of the chain (S4) is reached:

        VS1: &base = vx2
	VS2: &base + vec_size*1 = vx0
	VS3: &base + vec_size*2 = vx1
	VS4: &base + vec_size*3 = vx3

     Then permutation statements are generated:

	VS5: vx5 = VEC_PERM_EXPR < vx0, vx3, {0, 8, 1, 9, 2, 10, 3, 11} >
	VS6: vx6 = VEC_PERM_EXPR < vx0, vx3, {4, 12, 5, 13, 6, 14, 7, 15} >
	...

     And they are put in STMT_VINFO_VEC_STMT of the corresponding scalar stmts
     (the order of the data-refs in the output of vect_permute_store_chain
     corresponds to the order of scalar stmts in the interleaving chain - see
     the documentation of vect_permute_store_chain()).

     In case of both multiple types and interleaving, above vector stores and
     permutation stmts are created for every copy.  The result vector stmts are
     put in STMT_VINFO_VEC_STMT for the first copy and in the corresponding
     STMT_VINFO_RELATED_STMT for the next copies.
  */

  prev_stmt_info = NULL;
  tree vec_mask = NULL_TREE;
  for (j = 0; j < ncopies; j++)
    {
      stmt_vec_info new_stmt_info;
      if (j == 0)
	{
          if (slp)
            {
	      /* Get vectorized arguments for SLP_NODE.  */
	      vect_get_vec_defs (op, NULL_TREE, stmt_info, &vec_oprnds,
				 NULL, slp_node);

              vec_oprnd = vec_oprnds[0];
            }
          else
            {
	      /* For interleaved stores we collect vectorized defs for all the
		 stores in the group in DR_CHAIN and OPRNDS. DR_CHAIN is then
		 used as an input to vect_permute_store_chain(), and OPRNDS as
		 an input to vect_get_vec_def_for_stmt_copy() for the next copy.

		 If the store is not grouped, DR_GROUP_SIZE is 1, and DR_CHAIN and
		 OPRNDS are of size 1.  */
	      stmt_vec_info next_stmt_info = first_stmt_info;
	      for (i = 0; i < group_size; i++)
		{
		  /* Since gaps are not supported for interleaved stores,
		     DR_GROUP_SIZE is the exact number of stmts in the chain.
		     Therefore, NEXT_STMT_INFO can't be NULL_TREE.  In case
		     that there is no interleaving, DR_GROUP_SIZE is 1,
		     and only one iteration of the loop will be executed.  */
		  op = vect_get_store_rhs (next_stmt_info);
		  vec_oprnd = vect_get_vec_def_for_operand
		    (op, next_stmt_info);
		  dr_chain.quick_push (vec_oprnd);
		  oprnds.quick_push (vec_oprnd);
		  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
		}
	      if (mask)
		vec_mask = vect_get_vec_def_for_operand (mask, stmt_info,
							 mask_vectype);
	    }

	  /* We should have catched mismatched types earlier.  */
	  gcc_assert (useless_type_conversion_p (vectype,
						 TREE_TYPE (vec_oprnd)));
	  bool simd_lane_access_p
	    = STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info);
	  if (simd_lane_access_p
	      && TREE_CODE (DR_BASE_ADDRESS (first_dr_info->dr)) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (first_dr_info->dr), 0))
	      && integer_zerop (DR_OFFSET (first_dr_info->dr))
	      && integer_zerop (DR_INIT (first_dr_info->dr))
	      && alias_sets_conflict_p (get_alias_set (aggr_type),
					get_alias_set (TREE_TYPE (ref_type))))
	    {
	      dataref_ptr = unshare_expr (DR_BASE_ADDRESS (first_dr_info->dr));
	      dataref_offset = build_int_cst (ref_type, 0);
	    }
	  else if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
	    vect_get_gather_scatter_ops (loop, stmt_info, &gs_info,
					 &dataref_ptr, &vec_offset);
	  else
	    dataref_ptr
	      = vect_create_data_ref_ptr (first_stmt_info, aggr_type,
					  simd_lane_access_p ? loop : NULL,
					  offset, &dummy, gsi, &ptr_incr,
					  simd_lane_access_p, NULL_TREE, bump);
	}
      else
	{
	  /* For interleaved stores we created vectorized defs for all the
	     defs stored in OPRNDS in the previous iteration (previous copy).
	     DR_CHAIN is then used as an input to vect_permute_store_chain(),
	     and OPRNDS as an input to vect_get_vec_def_for_stmt_copy() for the
	     next copy.
	     If the store is not grouped, DR_GROUP_SIZE is 1, and DR_CHAIN and
	     OPRNDS are of size 1.  */
	  for (i = 0; i < group_size; i++)
	    {
	      op = oprnds[i];
	      vec_oprnd = vect_get_vec_def_for_stmt_copy (vinfo, op);
	      dr_chain[i] = vec_oprnd;
	      oprnds[i] = vec_oprnd;
	    }
	  if (mask)
	    vec_mask = vect_get_vec_def_for_stmt_copy (vinfo, vec_mask);
	  if (dataref_offset)
	    dataref_offset
	      = int_const_binop (PLUS_EXPR, dataref_offset, bump);
	  else if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
	    vec_offset = vect_get_vec_def_for_stmt_copy (vinfo, vec_offset);
	  else
	    dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					   stmt_info, bump);
	}

      if (memory_access_type == VMAT_LOAD_STORE_LANES)
	{
	  tree vec_array;

	  /* Get an array into which we can store the individual vectors.  */
	  vec_array = create_vector_array (vectype, vec_num);

	  /* Invalidate the current contents of VEC_ARRAY.  This should
	     become an RTL clobber too, which prevents the vector registers
	     from being upward-exposed.  */
	  vect_clobber_variable (stmt_info, gsi, vec_array);

	  /* Store the individual vectors into the array.  */
	  for (i = 0; i < vec_num; i++)
	    {
	      vec_oprnd = dr_chain[i];
	      write_vector_array (stmt_info, gsi, vec_oprnd, vec_array, i);
	    }

	  tree final_mask = NULL;
	  if (loop_masks)
	    final_mask = vect_get_loop_mask (gsi, loop_masks, ncopies,
					     vectype, j);
	  if (vec_mask)
	    final_mask = prepare_load_store_mask (mask_vectype, final_mask,
						  vec_mask, gsi);

	  gcall *call;
	  if (final_mask)
	    {
	      /* Emit:
		   MASK_STORE_LANES (DATAREF_PTR, ALIAS_PTR, VEC_MASK,
				     VEC_ARRAY).  */
	      unsigned int align = TYPE_ALIGN_UNIT (TREE_TYPE (vectype));
	      tree alias_ptr = build_int_cst (ref_type, align);
	      call = gimple_build_call_internal (IFN_MASK_STORE_LANES, 4,
						 dataref_ptr, alias_ptr,
						 final_mask, vec_array);
	    }
	  else
	    {
	      /* Emit:
		   MEM_REF[...all elements...] = STORE_LANES (VEC_ARRAY).  */
	      data_ref = create_array_ref (aggr_type, dataref_ptr, ref_type);
	      call = gimple_build_call_internal (IFN_STORE_LANES, 1,
						 vec_array);
	      gimple_call_set_lhs (call, data_ref);
	    }
	  gimple_call_set_nothrow (call, true);
	  new_stmt_info = vect_finish_stmt_generation (stmt_info, call, gsi);

	  /* Record that VEC_ARRAY is now dead.  */
	  vect_clobber_variable (stmt_info, gsi, vec_array);
	}
      else
	{
	  new_stmt_info = NULL;
	  if (grouped_store)
	    {
	      if (j == 0)
		result_chain.create (group_size);
	      /* Permute.  */
	      vect_permute_store_chain (dr_chain, group_size, stmt_info, gsi,
					&result_chain);
	    }

	  stmt_vec_info next_stmt_info = first_stmt_info;
	  for (i = 0; i < vec_num; i++)
	    {
	      unsigned align, misalign;

	      tree final_mask = NULL_TREE;
	      if (loop_masks)
		final_mask = vect_get_loop_mask (gsi, loop_masks,
						 vec_num * ncopies,
						 vectype, vec_num * j + i);
	      if (vec_mask)
		final_mask = prepare_load_store_mask (mask_vectype, final_mask,
						      vec_mask, gsi);

	      if (memory_access_type == VMAT_GATHER_SCATTER)
		{
		  tree scale = size_int (gs_info.scale);
		  gcall *call;
		  if (loop_masks)
		    call = gimple_build_call_internal
		      (IFN_MASK_SCATTER_STORE, 5, dataref_ptr, vec_offset,
		       scale, vec_oprnd, final_mask);
		  else
		    call = gimple_build_call_internal
		      (IFN_SCATTER_STORE, 4, dataref_ptr, vec_offset,
		       scale, vec_oprnd);
		  gimple_call_set_nothrow (call, true);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, call, gsi);
		  break;
		}

	      if (i > 0)
		/* Bump the vector pointer.  */
		dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					       stmt_info, bump);

	      if (slp)
		vec_oprnd = vec_oprnds[i];
	      else if (grouped_store)
		/* For grouped stores vectorized defs are interleaved in
		   vect_permute_store_chain().  */
		vec_oprnd = result_chain[i];

	      align = DR_TARGET_ALIGNMENT (first_dr_info);
	      if (aligned_access_p (first_dr_info))
		misalign = 0;
	      else if (DR_MISALIGNMENT (first_dr_info) == -1)
		{
		  align = dr_alignment (vect_dr_behavior (first_dr_info));
		  misalign = 0;
		}
	      else
		misalign = DR_MISALIGNMENT (first_dr_info);
	      if (dataref_offset == NULL_TREE
		  && TREE_CODE (dataref_ptr) == SSA_NAME)
		set_ptr_info_alignment (get_ptr_info (dataref_ptr), align,
					misalign);

	      if (memory_access_type == VMAT_CONTIGUOUS_REVERSE)
		{
		  tree perm_mask = perm_mask_for_reverse (vectype);
		  tree perm_dest = vect_create_destination_var
		    (vect_get_store_rhs (stmt_info), vectype);
		  tree new_temp = make_ssa_name (perm_dest);

		  /* Generate the permute statement.  */
		  gimple *perm_stmt 
		    = gimple_build_assign (new_temp, VEC_PERM_EXPR, vec_oprnd,
					   vec_oprnd, perm_mask);
		  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);

		  perm_stmt = SSA_NAME_DEF_STMT (new_temp);
		  vec_oprnd = new_temp;
		}

	      /* Arguments are ready.  Create the new vector stmt.  */
	      if (final_mask)
		{
		  align = least_bit_hwi (misalign | align);
		  tree ptr = build_int_cst (ref_type, align);
		  gcall *call
		    = gimple_build_call_internal (IFN_MASK_STORE, 4,
						  dataref_ptr, ptr,
						  final_mask, vec_oprnd);
		  gimple_call_set_nothrow (call, true);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, call, gsi);
		}
	      else
		{
		  data_ref = fold_build2 (MEM_REF, vectype,
					  dataref_ptr,
					  dataref_offset
					  ? dataref_offset
					  : build_int_cst (ref_type, 0));
		  if (aligned_access_p (first_dr_info))
		    ;
		  else if (DR_MISALIGNMENT (first_dr_info) == -1)
		    TREE_TYPE (data_ref)
		      = build_aligned_type (TREE_TYPE (data_ref),
					    align * BITS_PER_UNIT);
		  else
		    TREE_TYPE (data_ref)
		      = build_aligned_type (TREE_TYPE (data_ref),
					    TYPE_ALIGN (elem_type));
		  vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
		  gassign *new_stmt
		    = gimple_build_assign (data_ref, vec_oprnd);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		}

	      if (slp)
		continue;

	      next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
	      if (!next_stmt_info)
		break;
	    }
	}
      if (!slp)
	{
	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
	  prev_stmt_info = new_stmt_info;
	}
    }

  oprnds.release ();
  result_chain.release ();
  vec_oprnds.release ();

  return true;
}

/* Given a vector type VECTYPE, turns permutation SEL into the equivalent
   VECTOR_CST mask.  No checks are made that the target platform supports the
   mask, so callers may wish to test can_vec_perm_const_p separately, or use
   vect_gen_perm_mask_checked.  */

tree
vect_gen_perm_mask_any (tree vectype, const vec_perm_indices &sel)
{
  tree mask_type;

  poly_uint64 nunits = sel.length ();
  gcc_assert (known_eq (nunits, TYPE_VECTOR_SUBPARTS (vectype)));

  mask_type = build_vector_type (ssizetype, nunits);
  return vec_perm_indices_to_tree (mask_type, sel);
}

/* Checked version of vect_gen_perm_mask_any.  Asserts can_vec_perm_const_p,
   i.e. that the target supports the pattern _for arbitrary input vectors_.  */

tree
vect_gen_perm_mask_checked (tree vectype, const vec_perm_indices &sel)
{
  gcc_assert (can_vec_perm_const_p (TYPE_MODE (vectype), sel));
  return vect_gen_perm_mask_any (vectype, sel);
}

/* Given a vector variable X and Y, that was generated for the scalar
   STMT_INFO, generate instructions to permute the vector elements of X and Y
   using permutation mask MASK_VEC, insert them at *GSI and return the
   permuted vector variable.  */

static tree
permute_vec_elements (tree x, tree y, tree mask_vec, stmt_vec_info stmt_info,
		      gimple_stmt_iterator *gsi)
{
  tree vectype = TREE_TYPE (x);
  tree perm_dest, data_ref;
  gimple *perm_stmt;

  tree scalar_dest = gimple_get_lhs (stmt_info->stmt);
  if (TREE_CODE (scalar_dest) == SSA_NAME)
    perm_dest = vect_create_destination_var (scalar_dest, vectype);
  else
    perm_dest = vect_get_new_vect_var (vectype, vect_simple_var, NULL);
  data_ref = make_ssa_name (perm_dest);

  /* Generate the permute statement.  */
  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, x, y, mask_vec);
  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);

  return data_ref;
}

/* Hoist the definitions of all SSA uses on STMT_INFO out of the loop LOOP,
   inserting them on the loops preheader edge.  Returns true if we
   were successful in doing so (and thus STMT_INFO can be moved then),
   otherwise returns false.  */

static bool
hoist_defs_of_uses (stmt_vec_info stmt_info, struct loop *loop)
{
  ssa_op_iter i;
  tree op;
  bool any = false;

  FOR_EACH_SSA_TREE_OPERAND (op, stmt_info->stmt, i, SSA_OP_USE)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
      if (!gimple_nop_p (def_stmt)
	  && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt)))
	{
	  /* Make sure we don't need to recurse.  While we could do
	     so in simple cases when there are more complex use webs
	     we don't have an easy way to preserve stmt order to fulfil
	     dependencies within them.  */
	  tree op2;
	  ssa_op_iter i2;
	  if (gimple_code (def_stmt) == GIMPLE_PHI)
	    return false;
	  FOR_EACH_SSA_TREE_OPERAND (op2, def_stmt, i2, SSA_OP_USE)
	    {
	      gimple *def_stmt2 = SSA_NAME_DEF_STMT (op2);
	      if (!gimple_nop_p (def_stmt2)
		  && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt2)))
		return false;
	    }
	  any = true;
	}
    }

  if (!any)
    return true;

  FOR_EACH_SSA_TREE_OPERAND (op, stmt_info->stmt, i, SSA_OP_USE)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
      if (!gimple_nop_p (def_stmt)
	  && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt)))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (def_stmt);
	  gsi_remove (&gsi, false);
	  gsi_insert_on_edge_immediate (loop_preheader_edge (loop), def_stmt);
	}
    }

  return true;
}

/* vectorizable_load.

   Check if STMT_INFO reads a non scalar data-ref (array/pointer/structure)
   that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_load (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		   stmt_vec_info *vec_stmt, slp_tree slp_node,
		   slp_instance slp_node_instance,
		   stmt_vector_for_cost *cost_vec)
{
  tree scalar_dest;
  tree vec_dest = NULL;
  tree data_ref = NULL;
  stmt_vec_info prev_stmt_info;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  struct loop *containing_loop = gimple_bb (stmt_info->stmt)->loop_father;
  bool nested_in_vect_loop = false;
  tree elem_type;
  tree new_temp;
  machine_mode mode;
  tree dummy;
  enum dr_alignment_support alignment_support_scheme;
  tree dataref_ptr = NULL_TREE;
  tree dataref_offset = NULL_TREE;
  gimple *ptr_incr = NULL;
  int ncopies;
  int i, j;
  unsigned int group_size;
  poly_uint64 group_gap_adj;
  tree msq = NULL_TREE, lsq;
  tree offset = NULL_TREE;
  tree byte_offset = NULL_TREE;
  tree realignment_token = NULL_TREE;
  gphi *phi = NULL;
  vec<tree> dr_chain = vNULL;
  bool grouped_load = false;
  stmt_vec_info first_stmt_info;
  stmt_vec_info first_stmt_info_for_drptr = NULL;
  bool compute_in_loop = false;
  struct loop *at_loop;
  int vec_num;
  bool slp = (slp_node != NULL);
  bool slp_perm = false;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  poly_uint64 vf;
  tree aggr_type;
  gather_scatter_info gs_info;
  vec_info *vinfo = stmt_info->vinfo;
  tree ref_type;
  enum vect_def_type mask_dt = vect_unknown_def_type;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  tree mask = NULL_TREE, mask_vectype = NULL_TREE;
  if (gassign *assign = dyn_cast <gassign *> (stmt_info->stmt))
    {
      scalar_dest = gimple_assign_lhs (assign);
      if (TREE_CODE (scalar_dest) != SSA_NAME)
	return false;

      tree_code code = gimple_assign_rhs_code (assign);
      if (code != ARRAY_REF
	  && code != BIT_FIELD_REF
	  && code != INDIRECT_REF
	  && code != COMPONENT_REF
	  && code != IMAGPART_EXPR
	  && code != REALPART_EXPR
	  && code != MEM_REF
	  && TREE_CODE_CLASS (code) != tcc_declaration)
	return false;
    }
  else
    {
      gcall *call = dyn_cast <gcall *> (stmt_info->stmt);
      if (!call || !gimple_call_internal_p (call))
	return false;

      internal_fn ifn = gimple_call_internal_fn (call);
      if (!internal_load_fn_p (ifn))
	return false;

      scalar_dest = gimple_call_lhs (call);
      if (!scalar_dest)
	return false;

      if (slp_node != NULL)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "SLP of masked loads not supported.\n");
	  return false;
	}

      int mask_index = internal_fn_mask_index (ifn);
      if (mask_index >= 0)
	{
	  mask = gimple_call_arg (call, mask_index);
	  if (!vect_check_load_store_mask (stmt_info, mask, &mask_dt,
					   &mask_vectype))
	    return false;
	}
    }

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (loop, stmt_info);
      vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
    }
  else
    vf = 1;

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);

  /* FORNOW. This restriction should be relaxed.  */
  if (nested_in_vect_loop && ncopies > 1)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "multiple types in nested loop.\n");
      return false;
    }

  /* Invalidate assumptions made by dependence analysis when vectorization
     on the unrolled body effectively re-orders stmts.  */
  if (ncopies > 1
      && STMT_VINFO_MIN_NEG_DIST (stmt_info) != 0
      && maybe_gt (LOOP_VINFO_VECT_FACTOR (loop_vinfo),
		   STMT_VINFO_MIN_NEG_DIST (stmt_info)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "cannot perform implicit CSE when unrolling "
			 "with negative dependence distance\n");
      return false;
    }

  elem_type = TREE_TYPE (vectype);
  mode = TYPE_MODE (vectype);

  /* FORNOW. In some cases can vectorize even if data-type not supported
    (e.g. - data copies).  */
  if (optab_handler (mov_optab, mode) == CODE_FOR_nothing)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "Aligned load, but unsupported type.\n");
      return false;
    }

  /* Check if the load is a part of an interleaving chain.  */
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      grouped_load = true;
      /* FORNOW */
      gcc_assert (!nested_in_vect_loop);
      gcc_assert (!STMT_VINFO_GATHER_SCATTER_P (stmt_info));

      first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      group_size = DR_GROUP_SIZE (first_stmt_info);

      if (slp && SLP_TREE_LOAD_PERMUTATION (slp_node).exists ())
	slp_perm = true;

      /* Invalidate assumptions made by dependence analysis when vectorization
	 on the unrolled body effectively re-orders stmts.  */
      if (!PURE_SLP_STMT (stmt_info)
	  && STMT_VINFO_MIN_NEG_DIST (stmt_info) != 0
	  && maybe_gt (LOOP_VINFO_VECT_FACTOR (loop_vinfo),
		       STMT_VINFO_MIN_NEG_DIST (stmt_info)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "cannot perform implicit CSE when performing "
			     "group loads with negative dependence distance\n");
	  return false;
	}

      /* Similarly when the stmt is a load that is both part of a SLP
         instance and a loop vectorized stmt via the same-dr mechanism
	 we have to give up.  */
      if (DR_GROUP_SAME_DR_STMT (stmt_info)
	  && (STMT_SLP_TYPE (stmt_info)
	      != STMT_SLP_TYPE (DR_GROUP_SAME_DR_STMT (stmt_info))))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "conflicting SLP types for CSEd load\n");
	  return false;
	}
    }
  else
    group_size = 1;

  vect_memory_access_type memory_access_type;
  if (!get_load_store_type (stmt_info, vectype, slp, mask, VLS_LOAD, ncopies,
			    &memory_access_type, &gs_info))
    return false;

  if (mask)
    {
      if (memory_access_type == VMAT_CONTIGUOUS)
	{
	  machine_mode vec_mode = TYPE_MODE (vectype);
	  if (!VECTOR_MODE_P (vec_mode)
	      || !can_vec_mask_load_store_p (vec_mode,
					     TYPE_MODE (mask_vectype), true))
	    return false;
	}
      else if (memory_access_type == VMAT_GATHER_SCATTER && gs_info.decl)
	{
	  tree arglist = TYPE_ARG_TYPES (TREE_TYPE (gs_info.decl));
	  tree masktype
	    = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (arglist))));
	  if (TREE_CODE (masktype) == INTEGER_TYPE)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "masked gather with integer mask not"
				 " supported.");
	      return false;
	    }
	}
      else if (memory_access_type != VMAT_LOAD_STORE_LANES
	       && memory_access_type != VMAT_GATHER_SCATTER)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported access type for masked load.\n");
	  return false;
	}
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      if (!slp)
	STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) = memory_access_type;

      if (loop_vinfo
	  && LOOP_VINFO_CAN_FULLY_MASK_P (loop_vinfo))
	check_load_store_masking (loop_vinfo, vectype, VLS_LOAD, group_size,
				  memory_access_type, &gs_info);

      STMT_VINFO_TYPE (stmt_info) = load_vec_info_type;
      vect_model_load_cost (stmt_info, ncopies, memory_access_type,
			    slp_node_instance, slp_node, cost_vec);
      return true;
    }

  if (!slp)
    gcc_assert (memory_access_type
		== STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info));

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform load. ncopies = %d\n", ncopies);

  /* Transform.  */

  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info), *first_dr_info = NULL;
  ensure_base_align (dr_info);

  if (memory_access_type == VMAT_GATHER_SCATTER && gs_info.decl)
    {
      vect_build_gather_load_calls (stmt_info, gsi, vec_stmt, &gs_info, mask);
      return true;
    }

  if (memory_access_type == VMAT_INVARIANT)
    {
      gcc_assert (!grouped_load && !mask && !bb_vinfo);
      /* If we have versioned for aliasing or the loop doesn't
	 have any data dependencies that would preclude this,
	 then we are sure this is a loop invariant load and
	 thus we can insert it on the preheader edge.  */
      bool hoist_p = (LOOP_VINFO_NO_DATA_DEPENDENCIES (loop_vinfo)
		      && !nested_in_vect_loop
		      && hoist_defs_of_uses (stmt_info, loop));
      if (hoist_p)
	{
	  gassign *stmt = as_a <gassign *> (stmt_info->stmt);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "hoisting out of the vectorized loop: %G", stmt);
	  scalar_dest = copy_ssa_name (scalar_dest);
	  tree rhs = unshare_expr (gimple_assign_rhs1 (stmt));
	  gsi_insert_on_edge_immediate
	    (loop_preheader_edge (loop),
	     gimple_build_assign (scalar_dest, rhs));
	}
      /* These copies are all equivalent, but currently the representation
	 requires a separate STMT_VINFO_VEC_STMT for each one.  */
      prev_stmt_info = NULL;
      gimple_stmt_iterator gsi2 = *gsi;
      gsi_next (&gsi2);
      for (j = 0; j < ncopies; j++)
	{
	  stmt_vec_info new_stmt_info;
	  if (hoist_p)
	    {
	      new_temp = vect_init_vector (stmt_info, scalar_dest,
					   vectype, NULL);
	      gimple *new_stmt = SSA_NAME_DEF_STMT (new_temp);
	      new_stmt_info = vinfo->add_stmt (new_stmt);
	    }
	  else
	    {
	      new_temp = vect_init_vector (stmt_info, scalar_dest,
					   vectype, &gsi2);
	      new_stmt_info = vinfo->lookup_def (new_temp);
	    }
	  if (slp)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
	  else if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
	  prev_stmt_info = new_stmt_info;
	}
      return true;
    }

  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_STRIDED_SLP)
    {
      gimple_stmt_iterator incr_gsi;
      bool insert_after;
      gimple *incr;
      tree offvar;
      tree ivstep;
      tree running_off;
      vec<constructor_elt, va_gc> *v = NULL;
      tree stride_base, stride_step, alias_off;
      /* Checked by get_load_store_type.  */
      unsigned int const_nunits = nunits.to_constant ();
      unsigned HOST_WIDE_INT cst_offset = 0;

      gcc_assert (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo));
      gcc_assert (!nested_in_vect_loop);

      if (grouped_load)
	{
	  first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
	  first_dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
	}
      else
	{
	  first_stmt_info = stmt_info;
	  first_dr_info = dr_info;
	}
      if (slp && grouped_load)
	{
	  group_size = DR_GROUP_SIZE (first_stmt_info);
	  ref_type = get_group_alias_ptr_type (first_stmt_info);
	}
      else
	{
	  if (grouped_load)
	    cst_offset
	      = (tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (vectype)))
		 * vect_get_place_in_interleaving_chain (stmt_info,
							 first_stmt_info));
	  group_size = 1;
	  ref_type = reference_alias_ptr_type (DR_REF (dr_info->dr));
	}

      stride_base
	= fold_build_pointer_plus
	    (DR_BASE_ADDRESS (first_dr_info->dr),
	     size_binop (PLUS_EXPR,
			 convert_to_ptrofftype (DR_OFFSET (first_dr_info->dr)),
			 convert_to_ptrofftype (DR_INIT (first_dr_info->dr))));
      stride_step = fold_convert (sizetype, DR_STEP (first_dr_info->dr));

      /* For a load with loop-invariant (but other than power-of-2)
         stride (i.e. not a grouped access) like so:

	   for (i = 0; i < n; i += stride)
	     ... = array[i];

	 we generate a new induction variable and new accesses to
	 form a new vector (or vectors, depending on ncopies):

	   for (j = 0; ; j += VF*stride)
	     tmp1 = array[j];
	     tmp2 = array[j + stride];
	     ...
	     vectemp = {tmp1, tmp2, ...}
         */

      ivstep = fold_build2 (MULT_EXPR, TREE_TYPE (stride_step), stride_step,
			    build_int_cst (TREE_TYPE (stride_step), vf));

      standard_iv_increment_position (loop, &incr_gsi, &insert_after);

      stride_base = cse_and_gimplify_to_preheader (loop_vinfo, stride_base);
      ivstep = cse_and_gimplify_to_preheader (loop_vinfo, ivstep);
      create_iv (stride_base, ivstep, NULL,
		 loop, &incr_gsi, insert_after,
		 &offvar, NULL);
      incr = gsi_stmt (incr_gsi);
      loop_vinfo->add_stmt (incr);

      stride_step = cse_and_gimplify_to_preheader (loop_vinfo, stride_step);

      prev_stmt_info = NULL;
      running_off = offvar;
      alias_off = build_int_cst (ref_type, 0);
      int nloads = const_nunits;
      int lnel = 1;
      tree ltype = TREE_TYPE (vectype);
      tree lvectype = vectype;
      auto_vec<tree> dr_chain;
      if (memory_access_type == VMAT_STRIDED_SLP)
	{
	  if (group_size < const_nunits)
	    {
	      /* First check if vec_init optab supports construction from
		 vector elts directly.  */
	      scalar_mode elmode = SCALAR_TYPE_MODE (TREE_TYPE (vectype));
	      machine_mode vmode;
	      if (mode_for_vector (elmode, group_size).exists (&vmode)
		  && VECTOR_MODE_P (vmode)
		  && targetm.vector_mode_supported_p (vmode)
		  && (convert_optab_handler (vec_init_optab,
					     TYPE_MODE (vectype), vmode)
		      != CODE_FOR_nothing))
		{
		  nloads = const_nunits / group_size;
		  lnel = group_size;
		  ltype = build_vector_type (TREE_TYPE (vectype), group_size);
		}
	      else
		{
		  /* Otherwise avoid emitting a constructor of vector elements
		     by performing the loads using an integer type of the same
		     size, constructing a vector of those and then
		     re-interpreting it as the original vector type.
		     This avoids a huge runtime penalty due to the general
		     inability to perform store forwarding from smaller stores
		     to a larger load.  */
		  unsigned lsize
		    = group_size * TYPE_PRECISION (TREE_TYPE (vectype));
		  elmode = int_mode_for_size (lsize, 0).require ();
		  unsigned int lnunits = const_nunits / group_size;
		  /* If we can't construct such a vector fall back to
		     element loads of the original vector type.  */
		  if (mode_for_vector (elmode, lnunits).exists (&vmode)
		      && VECTOR_MODE_P (vmode)
		      && targetm.vector_mode_supported_p (vmode)
		      && (convert_optab_handler (vec_init_optab, vmode, elmode)
			  != CODE_FOR_nothing))
		    {
		      nloads = lnunits;
		      lnel = group_size;
		      ltype = build_nonstandard_integer_type (lsize, 1);
		      lvectype = build_vector_type (ltype, nloads);
		    }
		}
	    }
	  else
	    {
	      nloads = 1;
	      lnel = const_nunits;
	      ltype = vectype;
	    }
	  ltype = build_aligned_type (ltype, TYPE_ALIGN (TREE_TYPE (vectype)));
	}
      /* Load vector(1) scalar_type if it's 1 element-wise vectype.  */
      else if (nloads == 1)
	ltype = vectype;

      if (slp)
	{
	  /* For SLP permutation support we need to load the whole group,
	     not only the number of vector stmts the permutation result
	     fits in.  */
	  if (slp_perm)
	    {
	      /* We don't yet generate SLP_TREE_LOAD_PERMUTATIONs for
		 variable VF.  */
	      unsigned int const_vf = vf.to_constant ();
	      ncopies = CEIL (group_size * const_vf, const_nunits);
	      dr_chain.create (ncopies);
	    }
	  else
	    ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
	}
      unsigned int group_el = 0;
      unsigned HOST_WIDE_INT
	elsz = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
      for (j = 0; j < ncopies; j++)
	{
	  if (nloads > 1)
	    vec_alloc (v, nloads);
	  stmt_vec_info new_stmt_info = NULL;
	  for (i = 0; i < nloads; i++)
	    {
	      tree this_off = build_int_cst (TREE_TYPE (alias_off),
					     group_el * elsz + cst_offset);
	      tree data_ref = build2 (MEM_REF, ltype, running_off, this_off);
	      vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
	      gassign *new_stmt
		= gimple_build_assign (make_ssa_name (ltype), data_ref);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	      if (nloads > 1)
		CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
					gimple_assign_lhs (new_stmt));

	      group_el += lnel;
	      if (! slp
		  || group_el == group_size)
		{
		  tree newoff = copy_ssa_name (running_off);
		  gimple *incr = gimple_build_assign (newoff, POINTER_PLUS_EXPR,
						      running_off, stride_step);
		  vect_finish_stmt_generation (stmt_info, incr, gsi);

		  running_off = newoff;
		  group_el = 0;
		}
	    }
	  if (nloads > 1)
	    {
	      tree vec_inv = build_constructor (lvectype, v);
	      new_temp = vect_init_vector (stmt_info, vec_inv, lvectype, gsi);
	      new_stmt_info = vinfo->lookup_def (new_temp);
	      if (lvectype != vectype)
		{
		  gassign *new_stmt
		    = gimple_build_assign (make_ssa_name (vectype),
					   VIEW_CONVERT_EXPR,
					   build1 (VIEW_CONVERT_EXPR,
						   vectype, new_temp));
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		}
	    }

	  if (slp)
	    {
	      if (slp_perm)
		dr_chain.quick_push (gimple_assign_lhs (new_stmt_info->stmt));
	      else
		SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
	    }
	  else
	    {
	      if (j == 0)
		STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	      else
		STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
	      prev_stmt_info = new_stmt_info;
	    }
	}
      if (slp_perm)
	{
	  unsigned n_perms;
	  vect_transform_slp_perm_load (slp_node, dr_chain, gsi, vf,
					slp_node_instance, false, &n_perms);
	}
      return true;
    }

  if (memory_access_type == VMAT_GATHER_SCATTER
      || (!slp && memory_access_type == VMAT_CONTIGUOUS))
    grouped_load = false;

  if (grouped_load)
    {
      first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      group_size = DR_GROUP_SIZE (first_stmt_info);
      /* For SLP vectorization we directly vectorize a subchain
         without permutation.  */
      if (slp && ! SLP_TREE_LOAD_PERMUTATION (slp_node).exists ())
	first_stmt_info = SLP_TREE_SCALAR_STMTS (slp_node)[0];
      /* For BB vectorization always use the first stmt to base
	 the data ref pointer on.  */
      if (bb_vinfo)
	first_stmt_info_for_drptr = SLP_TREE_SCALAR_STMTS (slp_node)[0];

      /* Check if the chain of loads is already vectorized.  */
      if (STMT_VINFO_VEC_STMT (first_stmt_info)
	  /* For SLP we would need to copy over SLP_TREE_VEC_STMTS.
	     ???  But we can only do so if there is exactly one
	     as we have no way to get at the rest.  Leave the CSE
	     opportunity alone.
	     ???  With the group load eventually participating
	     in multiple different permutations (having multiple
	     slp nodes which refer to the same group) the CSE
	     is even wrong code.  See PR56270.  */
	  && !slp)
	{
	  *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
	  return true;
	}
      first_dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
      group_gap_adj = 0;

      /* VEC_NUM is the number of vect stmts to be created for this group.  */
      if (slp)
	{
	  grouped_load = false;
	  /* If an SLP permutation is from N elements to N elements,
	     and if one vector holds a whole number of N, we can load
	     the inputs to the permutation in the same way as an
	     unpermuted sequence.  In other cases we need to load the
	     whole group, not only the number of vector stmts the
	     permutation result fits in.  */
	  if (slp_perm
	      && (group_size != SLP_INSTANCE_GROUP_SIZE (slp_node_instance)
		  || !multiple_p (nunits, group_size)))
	    {
	      /* We don't yet generate such SLP_TREE_LOAD_PERMUTATIONs for
		 variable VF; see vect_transform_slp_perm_load.  */
	      unsigned int const_vf = vf.to_constant ();
	      unsigned int const_nunits = nunits.to_constant ();
	      vec_num = CEIL (group_size * const_vf, const_nunits);
	      group_gap_adj = vf * group_size - nunits * vec_num;
	    }
	  else
	    {
	      vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
	      group_gap_adj
		= group_size - SLP_INSTANCE_GROUP_SIZE (slp_node_instance);
	    }
    	}
      else
	vec_num = group_size;

      ref_type = get_group_alias_ptr_type (first_stmt_info);
    }
  else
    {
      first_stmt_info = stmt_info;
      first_dr_info = dr_info;
      group_size = vec_num = 1;
      group_gap_adj = 0;
      ref_type = reference_alias_ptr_type (DR_REF (first_dr_info->dr));
    }

  alignment_support_scheme
    = vect_supportable_dr_alignment (first_dr_info, false);
  gcc_assert (alignment_support_scheme);
  vec_loop_masks *loop_masks
    = (loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
       ? &LOOP_VINFO_MASKS (loop_vinfo)
       : NULL);
  /* Targets with store-lane instructions must not require explicit
     realignment.  vect_supportable_dr_alignment always returns either
     dr_aligned or dr_unaligned_supported for masked operations.  */
  gcc_assert ((memory_access_type != VMAT_LOAD_STORE_LANES
	       && !mask
	       && !loop_masks)
	      || alignment_support_scheme == dr_aligned
	      || alignment_support_scheme == dr_unaligned_supported);

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  In doing so, we record a pointer
     from one copy of the vector stmt to the next, in the field
     STMT_VINFO_RELATED_STMT.  This is necessary in order to allow following
     stages to find the correct vector defs to be used when vectorizing
     stmts that use the defs of the current stmt.  The example below
     illustrates the vectorization process when VF=16 and nunits=4 (i.e., we
     need to create 4 vectorized stmts):

     before vectorization:
                                RELATED_STMT    VEC_STMT
        S1:     x = memref      -               -
        S2:     z = x + 1       -               -

     step 1: vectorize stmt S1:
        We first create the vector stmt VS1_0, and, as usual, record a
        pointer to it in the STMT_VINFO_VEC_STMT of the scalar stmt S1.
        Next, we create the vector stmt VS1_1, and record a pointer to
        it in the STMT_VINFO_RELATED_STMT of the vector stmt VS1_0.
        Similarly, for VS1_2 and VS1_3.  This is the resulting chain of
        stmts and pointers:
                                RELATED_STMT    VEC_STMT
        VS1_0:  vx0 = memref0   VS1_1           -
        VS1_1:  vx1 = memref1   VS1_2           -
        VS1_2:  vx2 = memref2   VS1_3           -
        VS1_3:  vx3 = memref3   -               -
        S1:     x = load        -               VS1_0
        S2:     z = x + 1       -               -

     See in documentation in vect_get_vec_def_for_stmt_copy for how the
     information we recorded in RELATED_STMT field is used to vectorize
     stmt S2.  */

  /* In case of interleaving (non-unit grouped access):

     S1:  x2 = &base + 2
     S2:  x0 = &base
     S3:  x1 = &base + 1
     S4:  x3 = &base + 3

     Vectorized loads are created in the order of memory accesses
     starting from the access of the first stmt of the chain:

     VS1: vx0 = &base
     VS2: vx1 = &base + vec_size*1
     VS3: vx3 = &base + vec_size*2
     VS4: vx4 = &base + vec_size*3

     Then permutation statements are generated:

     VS5: vx5 = VEC_PERM_EXPR < vx0, vx1, { 0, 2, ..., i*2 } >
     VS6: vx6 = VEC_PERM_EXPR < vx0, vx1, { 1, 3, ..., i*2+1 } >
       ...

     And they are put in STMT_VINFO_VEC_STMT of the corresponding scalar stmts
     (the order of the data-refs in the output of vect_permute_load_chain
     corresponds to the order of scalar stmts in the interleaving chain - see
     the documentation of vect_permute_load_chain()).
     The generation of permutation stmts and recording them in
     STMT_VINFO_VEC_STMT is done in vect_transform_grouped_load().

     In case of both multiple types and interleaving, the vector loads and
     permutation stmts above are created for every copy.  The result vector
     stmts are put in STMT_VINFO_VEC_STMT for the first copy and in the
     corresponding STMT_VINFO_RELATED_STMT for the next copies.  */

  /* If the data reference is aligned (dr_aligned) or potentially unaligned
     on a target that supports unaligned accesses (dr_unaligned_supported)
     we generate the following code:
         p = initial_addr;
         indx = 0;
         loop {
	   p = p + indx * vectype_size;
           vec_dest = *(p);
           indx = indx + 1;
         }

     Otherwise, the data reference is potentially unaligned on a target that
     does not support unaligned accesses (dr_explicit_realign_optimized) -
     then generate the following code, in which the data in each iteration is
     obtained by two vector loads, one from the previous iteration, and one
     from the current iteration:
         p1 = initial_addr;
         msq_init = *(floor(p1))
         p2 = initial_addr + VS - 1;
         realignment_token = call target_builtin;
         indx = 0;
         loop {
           p2 = p2 + indx * vectype_size
           lsq = *(floor(p2))
           vec_dest = realign_load (msq, lsq, realignment_token)
           indx = indx + 1;
           msq = lsq;
         }   */

  /* If the misalignment remains the same throughout the execution of the
     loop, we can create the init_addr and permutation mask at the loop
     preheader.  Otherwise, it needs to be created inside the loop.
     This can only occur when vectorizing memory accesses in the inner-loop
     nested within an outer-loop that is being vectorized.  */

  if (nested_in_vect_loop
      && !multiple_p (DR_STEP_ALIGNMENT (dr_info->dr),
		      GET_MODE_SIZE (TYPE_MODE (vectype))))
    {
      gcc_assert (alignment_support_scheme != dr_explicit_realign_optimized);
      compute_in_loop = true;
    }

  if ((alignment_support_scheme == dr_explicit_realign_optimized
       || alignment_support_scheme == dr_explicit_realign)
      && !compute_in_loop)
    {
      msq = vect_setup_realignment (first_stmt_info, gsi, &realignment_token,
				    alignment_support_scheme, NULL_TREE,
				    &at_loop);
      if (alignment_support_scheme == dr_explicit_realign_optimized)
	{
	  phi = as_a <gphi *> (SSA_NAME_DEF_STMT (msq));
	  byte_offset = size_binop (MINUS_EXPR, TYPE_SIZE_UNIT (vectype),
				    size_one_node);
	}
    }
  else
    at_loop = loop;

  if (memory_access_type == VMAT_CONTIGUOUS_REVERSE)
    offset = size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1);

  tree bump;
  tree vec_offset = NULL_TREE;
  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    {
      aggr_type = NULL_TREE;
      bump = NULL_TREE;
    }
  else if (memory_access_type == VMAT_GATHER_SCATTER)
    {
      aggr_type = elem_type;
      vect_get_strided_load_store_ops (stmt_info, loop_vinfo, &gs_info,
				       &bump, &vec_offset);
    }
  else
    {
      if (memory_access_type == VMAT_LOAD_STORE_LANES)
	aggr_type = build_array_type_nelts (elem_type, vec_num * nunits);
      else
	aggr_type = vectype;
      bump = vect_get_data_ptr_increment (dr_info, aggr_type,
					  memory_access_type);
    }

  tree vec_mask = NULL_TREE;
  prev_stmt_info = NULL;
  poly_uint64 group_elt = 0;
  for (j = 0; j < ncopies; j++)
    {
      stmt_vec_info new_stmt_info = NULL;
      /* 1. Create the vector or array pointer update chain.  */
      if (j == 0)
	{
	  bool simd_lane_access_p
	    = STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info);
	  if (simd_lane_access_p
	      && TREE_CODE (DR_BASE_ADDRESS (first_dr_info->dr)) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (first_dr_info->dr), 0))
	      && integer_zerop (DR_OFFSET (first_dr_info->dr))
	      && integer_zerop (DR_INIT (first_dr_info->dr))
	      && alias_sets_conflict_p (get_alias_set (aggr_type),
					get_alias_set (TREE_TYPE (ref_type)))
	      && (alignment_support_scheme == dr_aligned
		  || alignment_support_scheme == dr_unaligned_supported))
	    {
	      dataref_ptr = unshare_expr (DR_BASE_ADDRESS (first_dr_info->dr));
	      dataref_offset = build_int_cst (ref_type, 0);
	    }
	  else if (first_stmt_info_for_drptr
		   && first_stmt_info != first_stmt_info_for_drptr)
	    {
	      dataref_ptr
		= vect_create_data_ref_ptr (first_stmt_info_for_drptr,
					    aggr_type, at_loop, offset, &dummy,
					    gsi, &ptr_incr, simd_lane_access_p,
					    byte_offset, bump);
	      /* Adjust the pointer by the difference to first_stmt.  */
	      data_reference_p ptrdr
		= STMT_VINFO_DATA_REF (first_stmt_info_for_drptr);
	      tree diff
		= fold_convert (sizetype,
				size_binop (MINUS_EXPR,
					    DR_INIT (first_dr_info->dr),
					    DR_INIT (ptrdr)));
	      dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					     stmt_info, diff);
	    }
	  else if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
	    vect_get_gather_scatter_ops (loop, stmt_info, &gs_info,
					 &dataref_ptr, &vec_offset);
	  else
	    dataref_ptr
	      = vect_create_data_ref_ptr (first_stmt_info, aggr_type, at_loop,
					  offset, &dummy, gsi, &ptr_incr,
					  simd_lane_access_p,
					  byte_offset, bump);
	  if (mask)
	    vec_mask = vect_get_vec_def_for_operand (mask, stmt_info,
						     mask_vectype);
	}
      else
	{
	  if (dataref_offset)
	    dataref_offset = int_const_binop (PLUS_EXPR, dataref_offset,
					      bump);
	  else if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
	    vec_offset = vect_get_vec_def_for_stmt_copy (vinfo, vec_offset);
	  else
	    dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					   stmt_info, bump);
	  if (mask)
	    vec_mask = vect_get_vec_def_for_stmt_copy (vinfo, vec_mask);
	}

      if (grouped_load || slp_perm)
	dr_chain.create (vec_num);

      if (memory_access_type == VMAT_LOAD_STORE_LANES)
	{
	  tree vec_array;

	  vec_array = create_vector_array (vectype, vec_num);

	  tree final_mask = NULL_TREE;
	  if (loop_masks)
	    final_mask = vect_get_loop_mask (gsi, loop_masks, ncopies,
					     vectype, j);
	  if (vec_mask)
	    final_mask = prepare_load_store_mask (mask_vectype, final_mask,
						  vec_mask, gsi);

	  gcall *call;
	  if (final_mask)
	    {
	      /* Emit:
		   VEC_ARRAY = MASK_LOAD_LANES (DATAREF_PTR, ALIAS_PTR,
		                                VEC_MASK).  */
	      unsigned int align = TYPE_ALIGN_UNIT (TREE_TYPE (vectype));
	      tree alias_ptr = build_int_cst (ref_type, align);
	      call = gimple_build_call_internal (IFN_MASK_LOAD_LANES, 3,
						 dataref_ptr, alias_ptr,
						 final_mask);
	    }
	  else
	    {
	      /* Emit:
		   VEC_ARRAY = LOAD_LANES (MEM_REF[...all elements...]).  */
	      data_ref = create_array_ref (aggr_type, dataref_ptr, ref_type);
	      call = gimple_build_call_internal (IFN_LOAD_LANES, 1, data_ref);
	    }
	  gimple_call_set_lhs (call, vec_array);
	  gimple_call_set_nothrow (call, true);
	  new_stmt_info = vect_finish_stmt_generation (stmt_info, call, gsi);

	  /* Extract each vector into an SSA_NAME.  */
	  for (i = 0; i < vec_num; i++)
	    {
	      new_temp = read_vector_array (stmt_info, gsi, scalar_dest,
					    vec_array, i);
	      dr_chain.quick_push (new_temp);
	    }

	  /* Record the mapping between SSA_NAMEs and statements.  */
	  vect_record_grouped_load_vectors (stmt_info, dr_chain);

	  /* Record that VEC_ARRAY is now dead.  */
	  vect_clobber_variable (stmt_info, gsi, vec_array);
	}
      else
	{
	  for (i = 0; i < vec_num; i++)
	    {
	      tree final_mask = NULL_TREE;
	      if (loop_masks
		  && memory_access_type != VMAT_INVARIANT)
		final_mask = vect_get_loop_mask (gsi, loop_masks,
						 vec_num * ncopies,
						 vectype, vec_num * j + i);
	      if (vec_mask)
		final_mask = prepare_load_store_mask (mask_vectype, final_mask,
						      vec_mask, gsi);

	      if (i > 0)
		dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					       stmt_info, bump);

	      /* 2. Create the vector-load in the loop.  */
	      gimple *new_stmt = NULL;
	      switch (alignment_support_scheme)
		{
		case dr_aligned:
		case dr_unaligned_supported:
		  {
		    unsigned int align, misalign;

		    if (memory_access_type == VMAT_GATHER_SCATTER)
		      {
			tree scale = size_int (gs_info.scale);
			gcall *call;
			if (loop_masks)
			  call = gimple_build_call_internal
			    (IFN_MASK_GATHER_LOAD, 4, dataref_ptr,
			     vec_offset, scale, final_mask);
			else
			  call = gimple_build_call_internal
			    (IFN_GATHER_LOAD, 3, dataref_ptr,
			     vec_offset, scale);
			gimple_call_set_nothrow (call, true);
			new_stmt = call;
			data_ref = NULL_TREE;
			break;
		      }

		    align = DR_TARGET_ALIGNMENT (dr_info);
		    if (alignment_support_scheme == dr_aligned)
		      {
			gcc_assert (aligned_access_p (first_dr_info));
			misalign = 0;
		      }
		    else if (DR_MISALIGNMENT (first_dr_info) == -1)
		      {
			align = dr_alignment
			  (vect_dr_behavior (first_dr_info));
			misalign = 0;
		      }
		    else
		      misalign = DR_MISALIGNMENT (first_dr_info);
		    if (dataref_offset == NULL_TREE
			&& TREE_CODE (dataref_ptr) == SSA_NAME)
		      set_ptr_info_alignment (get_ptr_info (dataref_ptr),
					      align, misalign);

		    if (final_mask)
		      {
			align = least_bit_hwi (misalign | align);
			tree ptr = build_int_cst (ref_type, align);
			gcall *call
			  = gimple_build_call_internal (IFN_MASK_LOAD, 3,
							dataref_ptr, ptr,
							final_mask);
			gimple_call_set_nothrow (call, true);
			new_stmt = call;
			data_ref = NULL_TREE;
		      }
		    else
		      {
			data_ref
			  = fold_build2 (MEM_REF, vectype, dataref_ptr,
					 dataref_offset
					 ? dataref_offset
					 : build_int_cst (ref_type, 0));
			if (alignment_support_scheme == dr_aligned)
			  ;
			else if (DR_MISALIGNMENT (first_dr_info) == -1)
			  TREE_TYPE (data_ref)
			    = build_aligned_type (TREE_TYPE (data_ref),
						  align * BITS_PER_UNIT);
			else
			  TREE_TYPE (data_ref)
			    = build_aligned_type (TREE_TYPE (data_ref),
						  TYPE_ALIGN (elem_type));
		      }
		    break;
		  }
		case dr_explicit_realign:
		  {
		    tree ptr, bump;

		    tree vs = size_int (TYPE_VECTOR_SUBPARTS (vectype));

		    if (compute_in_loop)
		      msq = vect_setup_realignment (first_stmt_info, gsi,
						    &realignment_token,
						    dr_explicit_realign,
						    dataref_ptr, NULL);

		    if (TREE_CODE (dataref_ptr) == SSA_NAME)
		      ptr = copy_ssa_name (dataref_ptr);
		    else
		      ptr = make_ssa_name (TREE_TYPE (dataref_ptr));
		    unsigned int align = DR_TARGET_ALIGNMENT (first_dr_info);
		    new_stmt = gimple_build_assign
				 (ptr, BIT_AND_EXPR, dataref_ptr,
				  build_int_cst
				  (TREE_TYPE (dataref_ptr),
				   -(HOST_WIDE_INT) align));
		    vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		    data_ref
		      = build2 (MEM_REF, vectype, ptr,
				build_int_cst (ref_type, 0));
		    vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
		    vec_dest = vect_create_destination_var (scalar_dest,
							    vectype);
		    new_stmt = gimple_build_assign (vec_dest, data_ref);
		    new_temp = make_ssa_name (vec_dest, new_stmt);
		    gimple_assign_set_lhs (new_stmt, new_temp);
		    gimple_set_vdef (new_stmt, gimple_vdef (stmt_info->stmt));
		    gimple_set_vuse (new_stmt, gimple_vuse (stmt_info->stmt));
		    vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		    msq = new_temp;

		    bump = size_binop (MULT_EXPR, vs,
				       TYPE_SIZE_UNIT (elem_type));
		    bump = size_binop (MINUS_EXPR, bump, size_one_node);
		    ptr = bump_vector_ptr (dataref_ptr, NULL, gsi,
					   stmt_info, bump);
		    new_stmt = gimple_build_assign
				 (NULL_TREE, BIT_AND_EXPR, ptr,
				  build_int_cst
				  (TREE_TYPE (ptr), -(HOST_WIDE_INT) align));
		    ptr = copy_ssa_name (ptr, new_stmt);
		    gimple_assign_set_lhs (new_stmt, ptr);
		    vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		    data_ref
		      = build2 (MEM_REF, vectype, ptr,
				build_int_cst (ref_type, 0));
		    break;
		  }
		case dr_explicit_realign_optimized:
		  {
		    if (TREE_CODE (dataref_ptr) == SSA_NAME)
		      new_temp = copy_ssa_name (dataref_ptr);
		    else
		      new_temp = make_ssa_name (TREE_TYPE (dataref_ptr));
		    unsigned int align = DR_TARGET_ALIGNMENT (first_dr_info);
		    new_stmt = gimple_build_assign
		      (new_temp, BIT_AND_EXPR, dataref_ptr,
		       build_int_cst (TREE_TYPE (dataref_ptr),
				     -(HOST_WIDE_INT) align));
		    vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		    data_ref
		      = build2 (MEM_REF, vectype, new_temp,
				build_int_cst (ref_type, 0));
		    break;
		  }
		default:
		  gcc_unreachable ();
		}
	      vec_dest = vect_create_destination_var (scalar_dest, vectype);
	      /* DATA_REF is null if we've already built the statement.  */
	      if (data_ref)
		{
		  vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
		  new_stmt = gimple_build_assign (vec_dest, data_ref);
		}
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      gimple_set_lhs (new_stmt, new_temp);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

	      /* 3. Handle explicit realignment if necessary/supported.
		 Create in loop:
		   vec_dest = realign_load (msq, lsq, realignment_token)  */
	      if (alignment_support_scheme == dr_explicit_realign_optimized
		  || alignment_support_scheme == dr_explicit_realign)
		{
		  lsq = gimple_assign_lhs (new_stmt);
		  if (!realignment_token)
		    realignment_token = dataref_ptr;
		  vec_dest = vect_create_destination_var (scalar_dest, vectype);
		  new_stmt = gimple_build_assign (vec_dest, REALIGN_LOAD_EXPR,
						  msq, lsq, realignment_token);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_assign_set_lhs (new_stmt, new_temp);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);

		  if (alignment_support_scheme == dr_explicit_realign_optimized)
		    {
		      gcc_assert (phi);
		      if (i == vec_num - 1 && j == ncopies - 1)
			add_phi_arg (phi, lsq,
				     loop_latch_edge (containing_loop),
				     UNKNOWN_LOCATION);
		      msq = lsq;
		    }
		}

	      if (memory_access_type == VMAT_CONTIGUOUS_REVERSE)
		{
		  tree perm_mask = perm_mask_for_reverse (vectype);
		  new_temp = permute_vec_elements (new_temp, new_temp,
						   perm_mask, stmt_info, gsi);
		  new_stmt_info = vinfo->lookup_def (new_temp);
		}

	      /* Collect vector loads and later create their permutation in
		 vect_transform_grouped_load ().  */
	      if (grouped_load || slp_perm)
		dr_chain.quick_push (new_temp);

	      /* Store vector loads in the corresponding SLP_NODE.  */
	      if (slp && !slp_perm)
		SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);

	      /* With SLP permutation we load the gaps as well, without
	         we need to skip the gaps after we manage to fully load
		 all elements.  group_gap_adj is DR_GROUP_SIZE here.  */
	      group_elt += nunits;
	      if (maybe_ne (group_gap_adj, 0U)
		  && !slp_perm
		  && known_eq (group_elt, group_size - group_gap_adj))
		{
		  poly_wide_int bump_val
		    = (wi::to_wide (TYPE_SIZE_UNIT (elem_type))
		       * group_gap_adj);
		  tree bump = wide_int_to_tree (sizetype, bump_val);
		  dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
						 stmt_info, bump);
		  group_elt = 0;
		}
	    }
	  /* Bump the vector pointer to account for a gap or for excess
	     elements loaded for a permuted SLP load.  */
	  if (maybe_ne (group_gap_adj, 0U) && slp_perm)
	    {
	      poly_wide_int bump_val
		= (wi::to_wide (TYPE_SIZE_UNIT (elem_type))
		   * group_gap_adj);
	      tree bump = wide_int_to_tree (sizetype, bump_val);
	      dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					     stmt_info, bump);
	    }
	}

      if (slp && !slp_perm)
	continue;

      if (slp_perm)
        {
	  unsigned n_perms;
          if (!vect_transform_slp_perm_load (slp_node, dr_chain, gsi, vf,
                                             slp_node_instance, false,
					     &n_perms))
            {
              dr_chain.release ();
              return false;
            }
        }
      else
        {
          if (grouped_load)
  	    {
	      if (memory_access_type != VMAT_LOAD_STORE_LANES)
		vect_transform_grouped_load (stmt_info, dr_chain,
					     group_size, gsi);
	      *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
	    }
          else
	    {
	      if (j == 0)
	        STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	      else
	        STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
	      prev_stmt_info = new_stmt_info;
	    }
        }
      dr_chain.release ();
    }

  return true;
}

/* Function vect_is_simple_cond.

   Input:
   LOOP - the loop that is being vectorized.
   COND - Condition that is checked for simple use.

   Output:
   *COMP_VECTYPE - the vector type for the comparison.
   *DTS - The def types for the arguments of the comparison

   Returns whether a COND can be vectorized.  Checks whether
   condition operands are supportable using vec_is_simple_use.  */

static bool
vect_is_simple_cond (tree cond, vec_info *vinfo,
		     tree *comp_vectype, enum vect_def_type *dts,
		     tree vectype)
{
  tree lhs, rhs;
  tree vectype1 = NULL_TREE, vectype2 = NULL_TREE;

  /* Mask case.  */
  if (TREE_CODE (cond) == SSA_NAME
      && VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (cond)))
    {
      if (!vect_is_simple_use (cond, vinfo, &dts[0], comp_vectype)
	  || !*comp_vectype
	  || !VECTOR_BOOLEAN_TYPE_P (*comp_vectype))
	return false;
      return true;
    }

  if (!COMPARISON_CLASS_P (cond))
    return false;

  lhs = TREE_OPERAND (cond, 0);
  rhs = TREE_OPERAND (cond, 1);

  if (TREE_CODE (lhs) == SSA_NAME)
    {
      if (!vect_is_simple_use (lhs, vinfo, &dts[0], &vectype1))
	return false;
    }
  else if (TREE_CODE (lhs) == INTEGER_CST || TREE_CODE (lhs) == REAL_CST
	   || TREE_CODE (lhs) == FIXED_CST)
    dts[0] = vect_constant_def;
  else
    return false;

  if (TREE_CODE (rhs) == SSA_NAME)
    {
      if (!vect_is_simple_use (rhs, vinfo, &dts[1], &vectype2))
	return false;
    }
  else if (TREE_CODE (rhs) == INTEGER_CST || TREE_CODE (rhs) == REAL_CST
	   || TREE_CODE (rhs) == FIXED_CST)
    dts[1] = vect_constant_def;
  else
    return false;

  if (vectype1 && vectype2
      && maybe_ne (TYPE_VECTOR_SUBPARTS (vectype1),
		   TYPE_VECTOR_SUBPARTS (vectype2)))
    return false;

  *comp_vectype = vectype1 ? vectype1 : vectype2;
  /* Invariant comparison.  */
  if (! *comp_vectype && vectype)
    {
      tree scalar_type = TREE_TYPE (lhs);
      /* If we can widen the comparison to match vectype do so.  */
      if (INTEGRAL_TYPE_P (scalar_type)
	  && tree_int_cst_lt (TYPE_SIZE (scalar_type),
			      TYPE_SIZE (TREE_TYPE (vectype))))
	scalar_type = build_nonstandard_integer_type
	  (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (vectype))),
	   TYPE_UNSIGNED (scalar_type));
      *comp_vectype = get_vectype_for_scalar_type (scalar_type);
    }

  return true;
}

/* vectorizable_condition.

   Check if STMT_INFO is conditional modify expression that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt using VEC_COND_EXPR  to replace it, put it in VEC_STMT, and insert it
   at GSI.

   When STMT_INFO is vectorized as a nested cycle, REDUC_DEF is the vector
   variable to be used at REDUC_INDEX (in then clause if REDUC_INDEX is 1,
   and in else clause if it is 2).

   Return true if STMT_INFO is vectorizable in this way.  */

bool
vectorizable_condition (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			stmt_vec_info *vec_stmt, tree reduc_def,
			int reduc_index, slp_tree slp_node,
			stmt_vector_for_cost *cost_vec)
{
  vec_info *vinfo = stmt_info->vinfo;
  tree scalar_dest = NULL_TREE;
  tree vec_dest = NULL_TREE;
  tree cond_expr, cond_expr0 = NULL_TREE, cond_expr1 = NULL_TREE;
  tree then_clause, else_clause;
  tree comp_vectype = NULL_TREE;
  tree vec_cond_lhs = NULL_TREE, vec_cond_rhs = NULL_TREE;
  tree vec_then_clause = NULL_TREE, vec_else_clause = NULL_TREE;
  tree vec_compare;
  tree new_temp;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum vect_def_type dts[4]
    = {vect_unknown_def_type, vect_unknown_def_type,
       vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 4;
  int ncopies;
  enum tree_code code, cond_code, bitop1 = NOP_EXPR, bitop2 = NOP_EXPR;
  stmt_vec_info prev_stmt_info = NULL;
  int i, j;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  vec<tree> vec_oprnds2 = vNULL;
  vec<tree> vec_oprnds3 = vNULL;
  tree vec_cmp_type;
  bool masked = false;

  if (reduc_index && STMT_SLP_TYPE (stmt_info))
    return false;

  vect_reduction_type reduction_type
    = STMT_VINFO_VEC_REDUCTION_TYPE (stmt_info);
  if (reduction_type == TREE_CODE_REDUCTION)
    {
      if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
	return false;

      if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
	  && !(STMT_VINFO_DEF_TYPE (stmt_info) == vect_nested_cycle
	       && reduc_def))
	return false;

      /* FORNOW: not yet supported.  */
      if (STMT_VINFO_LIVE_P (stmt_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "value used after loop.\n");
	  return false;
	}
    }

  /* Is vectorizable conditional operation?  */
  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (code != COND_EXPR)
    return false;

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree vectype1 = NULL_TREE, vectype2 = NULL_TREE;

  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);
  if (reduc_index && ncopies > 1)
    return false; /* FORNOW */

  cond_expr = gimple_assign_rhs1 (stmt);
  then_clause = gimple_assign_rhs2 (stmt);
  else_clause = gimple_assign_rhs3 (stmt);

  if (!vect_is_simple_cond (cond_expr, stmt_info->vinfo,
			    &comp_vectype, &dts[0], slp_node ? NULL : vectype)
      || !comp_vectype)
    return false;

  if (!vect_is_simple_use (then_clause, stmt_info->vinfo, &dts[2], &vectype1))
    return false;
  if (!vect_is_simple_use (else_clause, stmt_info->vinfo, &dts[3], &vectype2))
    return false;

  if (vectype1 && !useless_type_conversion_p (vectype, vectype1))
    return false;

  if (vectype2 && !useless_type_conversion_p (vectype, vectype2))
    return false;

  masked = !COMPARISON_CLASS_P (cond_expr);
  vec_cmp_type = build_same_sized_truth_vector_type (comp_vectype);

  if (vec_cmp_type == NULL_TREE)
    return false;

  cond_code = TREE_CODE (cond_expr);
  if (!masked)
    {
      cond_expr0 = TREE_OPERAND (cond_expr, 0);
      cond_expr1 = TREE_OPERAND (cond_expr, 1);
    }

  if (!masked && VECTOR_BOOLEAN_TYPE_P (comp_vectype))
    {
      /* Boolean values may have another representation in vectors
	 and therefore we prefer bit operations over comparison for
	 them (which also works for scalar masks).  We store opcodes
	 to use in bitop1 and bitop2.  Statement is vectorized as
	 BITOP2 (rhs1 BITOP1 rhs2) or rhs1 BITOP2 (BITOP1 rhs2)
	 depending on bitop1 and bitop2 arity.  */
      switch (cond_code)
	{
	case GT_EXPR:
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_AND_EXPR;
	  break;
	case GE_EXPR:
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_IOR_EXPR;
	  break;
	case LT_EXPR:
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_AND_EXPR;
	  std::swap (cond_expr0, cond_expr1);
	  break;
	case LE_EXPR:
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_IOR_EXPR;
	  std::swap (cond_expr0, cond_expr1);
	  break;
	case NE_EXPR:
	  bitop1 = BIT_XOR_EXPR;
	  break;
	case EQ_EXPR:
	  bitop1 = BIT_XOR_EXPR;
	  bitop2 = BIT_NOT_EXPR;
	  break;
	default:
	  return false;
	}
      cond_code = SSA_NAME;
    }

  if (!vec_stmt)
    {
      if (bitop1 != NOP_EXPR)
	{
	  machine_mode mode = TYPE_MODE (comp_vectype);
	  optab optab;

	  optab = optab_for_tree_code (bitop1, comp_vectype, optab_default);
	  if (!optab || optab_handler (optab, mode) == CODE_FOR_nothing)
	    return false;

	  if (bitop2 != NOP_EXPR)
	    {
	      optab = optab_for_tree_code (bitop2, comp_vectype,
					   optab_default);
	      if (!optab || optab_handler (optab, mode) == CODE_FOR_nothing)
		return false;
	    }
	}
      if (expand_vec_cond_expr_p (vectype, comp_vectype,
				     cond_code))
	{
	  STMT_VINFO_TYPE (stmt_info) = condition_vec_info_type;
	  vect_model_simple_cost (stmt_info, ncopies, dts, ndts, slp_node,
				  cost_vec);
	  return true;
	}
      return false;
    }

  /* Transform.  */

  if (!slp_node)
    {
      vec_oprnds0.create (1);
      vec_oprnds1.create (1);
      vec_oprnds2.create (1);
      vec_oprnds3.create (1);
    }

  /* Handle def.  */
  scalar_dest = gimple_assign_lhs (stmt);
  if (reduction_type != EXTRACT_LAST_REDUCTION)
    vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle cond expr.  */
  for (j = 0; j < ncopies; j++)
    {
      stmt_vec_info new_stmt_info = NULL;
      if (j == 0)
	{
          if (slp_node)
            {
              auto_vec<tree, 4> ops;
	      auto_vec<vec<tree>, 4> vec_defs;

	      if (masked)
		ops.safe_push (cond_expr);
	      else
		{
		  ops.safe_push (cond_expr0);
		  ops.safe_push (cond_expr1);
		}
              ops.safe_push (then_clause);
              ops.safe_push (else_clause);
              vect_get_slp_defs (ops, slp_node, &vec_defs);
	      vec_oprnds3 = vec_defs.pop ();
	      vec_oprnds2 = vec_defs.pop ();
	      if (!masked)
		vec_oprnds1 = vec_defs.pop ();
	      vec_oprnds0 = vec_defs.pop ();
            }
          else
            {
	      if (masked)
		{
		  vec_cond_lhs
		    = vect_get_vec_def_for_operand (cond_expr, stmt_info,
						    comp_vectype);
		  vect_is_simple_use (cond_expr, stmt_info->vinfo, &dts[0]);
		}
	      else
		{
		  vec_cond_lhs
		    = vect_get_vec_def_for_operand (cond_expr0,
						    stmt_info, comp_vectype);
		  vect_is_simple_use (cond_expr0, loop_vinfo, &dts[0]);

		  vec_cond_rhs
		    = vect_get_vec_def_for_operand (cond_expr1,
						    stmt_info, comp_vectype);
		  vect_is_simple_use (cond_expr1, loop_vinfo, &dts[1]);
		}
	      if (reduc_index == 1)
		vec_then_clause = reduc_def;
	      else
		{
		  vec_then_clause = vect_get_vec_def_for_operand (then_clause,
								  stmt_info);
		  vect_is_simple_use (then_clause, loop_vinfo, &dts[2]);
		}
	      if (reduc_index == 2)
		vec_else_clause = reduc_def;
	      else
		{
		  vec_else_clause = vect_get_vec_def_for_operand (else_clause,
								  stmt_info);
		  vect_is_simple_use (else_clause, loop_vinfo, &dts[3]);
		}
	    }
	}
      else
	{
	  vec_cond_lhs
	    = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnds0.pop ());
	  if (!masked)
	    vec_cond_rhs
	      = vect_get_vec_def_for_stmt_copy (vinfo, vec_oprnds1.pop ());

	  vec_then_clause = vect_get_vec_def_for_stmt_copy (vinfo,
							    vec_oprnds2.pop ());
	  vec_else_clause = vect_get_vec_def_for_stmt_copy (vinfo,
							    vec_oprnds3.pop ());
	}

      if (!slp_node)
        {
	  vec_oprnds0.quick_push (vec_cond_lhs);
	  if (!masked)
	    vec_oprnds1.quick_push (vec_cond_rhs);
	  vec_oprnds2.quick_push (vec_then_clause);
	  vec_oprnds3.quick_push (vec_else_clause);
	}

      /* Arguments are ready.  Create the new vector stmt.  */
      FOR_EACH_VEC_ELT (vec_oprnds0, i, vec_cond_lhs)
        {
          vec_then_clause = vec_oprnds2[i];
          vec_else_clause = vec_oprnds3[i];

	  if (masked)
	    vec_compare = vec_cond_lhs;
	  else
	    {
	      vec_cond_rhs = vec_oprnds1[i];
	      if (bitop1 == NOP_EXPR)
		vec_compare = build2 (cond_code, vec_cmp_type,
				      vec_cond_lhs, vec_cond_rhs);
	      else
		{
		  new_temp = make_ssa_name (vec_cmp_type);
		  gassign *new_stmt;
		  if (bitop1 == BIT_NOT_EXPR)
		    new_stmt = gimple_build_assign (new_temp, bitop1,
						    vec_cond_rhs);
		  else
		    new_stmt
		      = gimple_build_assign (new_temp, bitop1, vec_cond_lhs,
					     vec_cond_rhs);
		  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		  if (bitop2 == NOP_EXPR)
		    vec_compare = new_temp;
		  else if (bitop2 == BIT_NOT_EXPR)
		    {
		      /* Instead of doing ~x ? y : z do x ? z : y.  */
		      vec_compare = new_temp;
		      std::swap (vec_then_clause, vec_else_clause);
		    }
		  else
		    {
		      vec_compare = make_ssa_name (vec_cmp_type);
		      new_stmt
			= gimple_build_assign (vec_compare, bitop2,
					       vec_cond_lhs, new_temp);
		      vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		    }
		}
	    }
	  if (reduction_type == EXTRACT_LAST_REDUCTION)
	    {
	      if (!is_gimple_val (vec_compare))
		{
		  tree vec_compare_name = make_ssa_name (vec_cmp_type);
		  gassign *new_stmt = gimple_build_assign (vec_compare_name,
							   vec_compare);
		  vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		  vec_compare = vec_compare_name;
		}
	      gcc_assert (reduc_index == 2);
	      gcall *new_stmt = gimple_build_call_internal
		(IFN_FOLD_EXTRACT_LAST, 3, else_clause, vec_compare,
		 vec_then_clause);
	      gimple_call_set_lhs (new_stmt, scalar_dest);
	      SSA_NAME_DEF_STMT (scalar_dest) = new_stmt;
	      if (stmt_info->stmt == gsi_stmt (*gsi))
		new_stmt_info = vect_finish_replace_stmt (stmt_info, new_stmt);
	      else
		{
		  /* In this case we're moving the definition to later in the
		     block.  That doesn't matter because the only uses of the
		     lhs are in phi statements.  */
		  gimple_stmt_iterator old_gsi
		    = gsi_for_stmt (stmt_info->stmt);
		  gsi_remove (&old_gsi, true);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		}
	    }
	  else
	    {
	      new_temp = make_ssa_name (vec_dest);
	      gassign *new_stmt
		= gimple_build_assign (new_temp, VEC_COND_EXPR, vec_compare,
				       vec_then_clause, vec_else_clause);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	    }
          if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
        }

        if (slp_node)
          continue;

	if (j == 0)
	  STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
	else
	  STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

	prev_stmt_info = new_stmt_info;
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  vec_oprnds2.release ();
  vec_oprnds3.release ();

  return true;
}

/* vectorizable_comparison.

   Check if STMT_INFO is comparison expression that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   comparison, put it in VEC_STMT, and insert it at GSI.

   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_comparison (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			 stmt_vec_info *vec_stmt, tree reduc_def,
			 slp_tree slp_node, stmt_vector_for_cost *cost_vec)
{
  vec_info *vinfo = stmt_info->vinfo;
  tree lhs, rhs1, rhs2;
  tree vectype1 = NULL_TREE, vectype2 = NULL_TREE;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree vec_rhs1 = NULL_TREE, vec_rhs2 = NULL_TREE;
  tree new_temp;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum vect_def_type dts[2] = {vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 2;
  poly_uint64 nunits;
  int ncopies;
  enum tree_code code, bitop1 = NOP_EXPR, bitop2 = NOP_EXPR;
  stmt_vec_info prev_stmt_info = NULL;
  int i, j;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  tree mask_type;
  tree mask;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (!vectype || !VECTOR_BOOLEAN_TYPE_P (vectype))
    return false;

  mask_type = vectype;
  nunits = TYPE_VECTOR_SUBPARTS (vectype);

  if (slp_node)
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, vectype);

  gcc_assert (ncopies >= 1);
  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && !(STMT_VINFO_DEF_TYPE (stmt_info) == vect_nested_cycle
	   && reduc_def))
    return false;

  if (STMT_VINFO_LIVE_P (stmt_info))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "value used after loop.\n");
      return false;
    }

  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (TREE_CODE_CLASS (code) != tcc_comparison)
    return false;

  rhs1 = gimple_assign_rhs1 (stmt);
  rhs2 = gimple_assign_rhs2 (stmt);

  if (!vect_is_simple_use (rhs1, stmt_info->vinfo, &dts[0], &vectype1))
    return false;

  if (!vect_is_simple_use (rhs2, stmt_info->vinfo, &dts[1], &vectype2))
    return false;

  if (vectype1 && vectype2
      && maybe_ne (TYPE_VECTOR_SUBPARTS (vectype1),
		   TYPE_VECTOR_SUBPARTS (vectype2)))
    return false;

  vectype = vectype1 ? vectype1 : vectype2;

  /* Invariant comparison.  */
  if (!vectype)
    {
      vectype = get_vectype_for_scalar_type (TREE_TYPE (rhs1));
      if (maybe_ne (TYPE_VECTOR_SUBPARTS (vectype), nunits))
	return false;
    }
  else if (maybe_ne (nunits, TYPE_VECTOR_SUBPARTS (vectype)))
    return false;

  /* Can't compare mask and non-mask types.  */
  if (vectype1 && vectype2
      && (VECTOR_BOOLEAN_TYPE_P (vectype1) ^ VECTOR_BOOLEAN_TYPE_P (vectype2)))
    return false;

  /* Boolean values may have another representation in vectors
     and therefore we prefer bit operations over comparison for
     them (which also works for scalar masks).  We store opcodes
     to use in bitop1 and bitop2.  Statement is vectorized as
       BITOP2 (rhs1 BITOP1 rhs2) or
       rhs1 BITOP2 (BITOP1 rhs2)
     depending on bitop1 and bitop2 arity.  */
  if (VECTOR_BOOLEAN_TYPE_P (vectype))
    {
      if (code == GT_EXPR)
	{
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_AND_EXPR;
	}
      else if (code == GE_EXPR)
	{
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_IOR_EXPR;
	}
      else if (code == LT_EXPR)
	{
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_AND_EXPR;
	  std::swap (rhs1, rhs2);
	  std::swap (dts[0], dts[1]);
	}
      else if (code == LE_EXPR)
	{
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_IOR_EXPR;
	  std::swap (rhs1, rhs2);
	  std::swap (dts[0], dts[1]);
	}
      else
	{
	  bitop1 = BIT_XOR_EXPR;
	  if (code == EQ_EXPR)
	    bitop2 = BIT_NOT_EXPR;
	}
    }

  if (!vec_stmt)
    {
      if (bitop1 == NOP_EXPR)
	{
	  if (!expand_vec_cmp_expr_p (vectype, mask_type, code))
	    return false;
	}
      else
	{
	  machine_mode mode = TYPE_MODE (vectype);
	  optab optab;

	  optab = optab_for_tree_code (bitop1, vectype, optab_default);
	  if (!optab || optab_handler (optab, mode) == CODE_FOR_nothing)
	    return false;

	  if (bitop2 != NOP_EXPR)
	    {
	      optab = optab_for_tree_code (bitop2, vectype, optab_default);
	      if (!optab || optab_handler (optab, mode) == CODE_FOR_nothing)
		return false;
	    }
	}

      STMT_VINFO_TYPE (stmt_info) = comparison_vec_info_type;
      vect_model_simple_cost (stmt_info, ncopies * (1 + (bitop2 != NOP_EXPR)),
			      dts, ndts, slp_node, cost_vec);
      return true;
    }

  /* Transform.  */
  if (!slp_node)
    {
      vec_oprnds0.create (1);
      vec_oprnds1.create (1);
    }

  /* Handle def.  */
  lhs = gimple_assign_lhs (stmt);
  mask = vect_create_destination_var (lhs, mask_type);

  /* Handle cmp expr.  */
  for (j = 0; j < ncopies; j++)
    {
      stmt_vec_info new_stmt_info = NULL;
      if (j == 0)
	{
	  if (slp_node)
	    {
	      auto_vec<tree, 2> ops;
	      auto_vec<vec<tree>, 2> vec_defs;

	      ops.safe_push (rhs1);
	      ops.safe_push (rhs2);
	      vect_get_slp_defs (ops, slp_node, &vec_defs);
	      vec_oprnds1 = vec_defs.pop ();
	      vec_oprnds0 = vec_defs.pop ();
	    }
	  else
	    {
	      vec_rhs1 = vect_get_vec_def_for_operand (rhs1, stmt_info,
						       vectype);
	      vec_rhs2 = vect_get_vec_def_for_operand (rhs2, stmt_info,
						       vectype);
	    }
	}
      else
	{
	  vec_rhs1 = vect_get_vec_def_for_stmt_copy (vinfo,
						     vec_oprnds0.pop ());
	  vec_rhs2 = vect_get_vec_def_for_stmt_copy (vinfo,
						     vec_oprnds1.pop ());
	}

      if (!slp_node)
	{
	  vec_oprnds0.quick_push (vec_rhs1);
	  vec_oprnds1.quick_push (vec_rhs2);
	}

      /* Arguments are ready.  Create the new vector stmt.  */
      FOR_EACH_VEC_ELT (vec_oprnds0, i, vec_rhs1)
	{
	  vec_rhs2 = vec_oprnds1[i];

	  new_temp = make_ssa_name (mask);
	  if (bitop1 == NOP_EXPR)
	    {
	      gassign *new_stmt = gimple_build_assign (new_temp, code,
						       vec_rhs1, vec_rhs2);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	    }
	  else
	    {
	      gassign *new_stmt;
	      if (bitop1 == BIT_NOT_EXPR)
		new_stmt = gimple_build_assign (new_temp, bitop1, vec_rhs2);
	      else
		new_stmt = gimple_build_assign (new_temp, bitop1, vec_rhs1,
						vec_rhs2);
	      new_stmt_info
		= vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
	      if (bitop2 != NOP_EXPR)
		{
		  tree res = make_ssa_name (mask);
		  if (bitop2 == BIT_NOT_EXPR)
		    new_stmt = gimple_build_assign (res, bitop2, new_temp);
		  else
		    new_stmt = gimple_build_assign (res, bitop2, vec_rhs1,
						    new_temp);
		  new_stmt_info
		    = vect_finish_stmt_generation (stmt_info, new_stmt, gsi);
		}
	    }
	  if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt_info);
	}

      if (slp_node)
	continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt_info;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;

      prev_stmt_info = new_stmt_info;
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();

  return true;
}

/* If SLP_NODE is nonnull, return true if vectorizable_live_operation
   can handle all live statements in the node.  Otherwise return true
   if STMT_INFO is not live or if vectorizable_live_operation can handle it.
   GSI and VEC_STMT are as for vectorizable_live_operation.  */

static bool
can_vectorize_live_stmts (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			  slp_tree slp_node, stmt_vec_info *vec_stmt,
			  stmt_vector_for_cost *cost_vec)
{
  if (slp_node)
    {
      stmt_vec_info slp_stmt_info;
      unsigned int i;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (slp_node), i, slp_stmt_info)
	{
	  if (STMT_VINFO_LIVE_P (slp_stmt_info)
	      && !vectorizable_live_operation (slp_stmt_info, gsi, slp_node, i,
					       vec_stmt, cost_vec))
	    return false;
	}
    }
  else if (STMT_VINFO_LIVE_P (stmt_info)
	   && !vectorizable_live_operation (stmt_info, gsi, slp_node, -1,
					    vec_stmt, cost_vec))
    return false;

  return true;
}

/* Make sure the statement is vectorizable.  */

opt_result
vect_analyze_stmt (stmt_vec_info stmt_info, bool *need_to_vectorize,
		   slp_tree node, slp_instance node_instance,
		   stmt_vector_for_cost *cost_vec)
{
  vec_info *vinfo = stmt_info->vinfo;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  enum vect_relevant relevance = STMT_VINFO_RELEVANT (stmt_info);
  bool ok;
  gimple_seq pattern_def_seq;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "==> examining statement: %G",
		     stmt_info->stmt);

  if (gimple_has_volatile_ops (stmt_info->stmt))
    return opt_result::failure_at (stmt_info->stmt,
				   "not vectorized:"
				   " stmt has volatile operands: %G\n",
				   stmt_info->stmt);

  if (STMT_VINFO_IN_PATTERN_P (stmt_info)
      && node == NULL
      && (pattern_def_seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_info)))
    {
      gimple_stmt_iterator si;

      for (si = gsi_start (pattern_def_seq); !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info pattern_def_stmt_info
	    = vinfo->lookup_stmt (gsi_stmt (si));
	  if (STMT_VINFO_RELEVANT_P (pattern_def_stmt_info)
	      || STMT_VINFO_LIVE_P (pattern_def_stmt_info))
	    {
	      /* Analyze def stmt of STMT if it's a pattern stmt.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "==> examining pattern def statement: %G",
				 pattern_def_stmt_info->stmt);

	      opt_result res
		= vect_analyze_stmt (pattern_def_stmt_info,
				     need_to_vectorize, node, node_instance,
				     cost_vec);
	      if (!res)
		return res;
	    }
	}
    }

  /* Skip stmts that do not need to be vectorized. In loops this is expected
     to include:
     - the COND_EXPR which is the loop exit condition
     - any LABEL_EXPRs in the loop
     - computations that are used only for array indexing or loop control.
     In basic blocks we only analyze statements that are a part of some SLP
     instance, therefore, all the statements are relevant.

     Pattern statement needs to be analyzed instead of the original statement
     if the original statement is not relevant.  Otherwise, we analyze both
     statements.  In basic blocks we are called from some SLP instance
     traversal, don't analyze pattern stmts instead, the pattern stmts
     already will be part of SLP instance.  */

  stmt_vec_info pattern_stmt_info = STMT_VINFO_RELATED_STMT (stmt_info);
  if (!STMT_VINFO_RELEVANT_P (stmt_info)
      && !STMT_VINFO_LIVE_P (stmt_info))
    {
      if (STMT_VINFO_IN_PATTERN_P (stmt_info)
	  && pattern_stmt_info
	  && (STMT_VINFO_RELEVANT_P (pattern_stmt_info)
	      || STMT_VINFO_LIVE_P (pattern_stmt_info)))
        {
          /* Analyze PATTERN_STMT instead of the original stmt.  */
	  stmt_info = pattern_stmt_info;
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "==> examining pattern statement: %G",
			     stmt_info->stmt);
        }
      else
        {
          if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location, "irrelevant.\n");

          return opt_result::success ();
        }
    }
  else if (STMT_VINFO_IN_PATTERN_P (stmt_info)
	   && node == NULL
	   && pattern_stmt_info
	   && (STMT_VINFO_RELEVANT_P (pattern_stmt_info)
	       || STMT_VINFO_LIVE_P (pattern_stmt_info)))
    {
      /* Analyze PATTERN_STMT too.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "==> examining pattern statement: %G",
			 pattern_stmt_info->stmt);

      opt_result res
	= vect_analyze_stmt (pattern_stmt_info, need_to_vectorize, node,
			     node_instance, cost_vec);
      if (!res)
	return res;
   }

  switch (STMT_VINFO_DEF_TYPE (stmt_info))
    {
      case vect_internal_def:
        break;

      case vect_reduction_def:
      case vect_nested_cycle:
         gcc_assert (!bb_vinfo
		     && (relevance == vect_used_in_outer
			 || relevance == vect_used_in_outer_by_reduction
			 || relevance == vect_used_by_reduction
			 || relevance == vect_unused_in_scope
			 || relevance == vect_used_only_live));
         break;

      case vect_induction_def:
	gcc_assert (!bb_vinfo);
	break;

      case vect_constant_def:
      case vect_external_def:
      case vect_unknown_def_type:
      default:
        gcc_unreachable ();
    }

  if (STMT_VINFO_RELEVANT_P (stmt_info))
    {
      tree type = gimple_expr_type (stmt_info->stmt);
      gcc_assert (!VECTOR_MODE_P (TYPE_MODE (type)));
      gcall *call = dyn_cast <gcall *> (stmt_info->stmt);
      gcc_assert (STMT_VINFO_VECTYPE (stmt_info)
		  || (call && gimple_call_lhs (call) == NULL_TREE));
      *need_to_vectorize = true;
    }

  if (PURE_SLP_STMT (stmt_info) && !node)
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "handled only by SLP analysis\n");
      return opt_result::success ();
    }

  ok = true;
  if (!bb_vinfo
      && (STMT_VINFO_RELEVANT_P (stmt_info)
	  || STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def))
    ok = (vectorizable_simd_clone_call (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_conversion (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_shift (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_operation (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_assignment (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_load (stmt_info, NULL, NULL, node, node_instance,
				cost_vec)
	  || vectorizable_call (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_store (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_reduction (stmt_info, NULL, NULL, node,
				     node_instance, cost_vec)
	  || vectorizable_induction (stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_condition (stmt_info, NULL, NULL, NULL, 0, node,
				     cost_vec)
	  || vectorizable_comparison (stmt_info, NULL, NULL, NULL, node,
				      cost_vec));
  else
    {
      if (bb_vinfo)
	ok = (vectorizable_simd_clone_call (stmt_info, NULL, NULL, node,
					    cost_vec)
	      || vectorizable_conversion (stmt_info, NULL, NULL, node,
					  cost_vec)
	      || vectorizable_shift (stmt_info, NULL, NULL, node, cost_vec)
	      || vectorizable_operation (stmt_info, NULL, NULL, node, cost_vec)
	      || vectorizable_assignment (stmt_info, NULL, NULL, node,
					  cost_vec)
	      || vectorizable_load (stmt_info, NULL, NULL, node, node_instance,
				    cost_vec)
	      || vectorizable_call (stmt_info, NULL, NULL, node, cost_vec)
	      || vectorizable_store (stmt_info, NULL, NULL, node, cost_vec)
	      || vectorizable_condition (stmt_info, NULL, NULL, NULL, 0, node,
					 cost_vec)
	      || vectorizable_comparison (stmt_info, NULL, NULL, NULL, node,
					  cost_vec));
    }

  if (!ok)
    return opt_result::failure_at (stmt_info->stmt,
				   "not vectorized:"
				   " relevant stmt not supported: %G",
				   stmt_info->stmt);

  /* Stmts that are (also) "live" (i.e. - that are used out of the loop)
      need extra handling, except for vectorizable reductions.  */
  if (!bb_vinfo
      && STMT_VINFO_TYPE (stmt_info) != reduc_vec_info_type
      && !can_vectorize_live_stmts (stmt_info, NULL, node, NULL, cost_vec))
    return opt_result::failure_at (stmt_info->stmt,
				   "not vectorized:"
				   " live stmt not supported: %G",
				   stmt_info->stmt);

  return opt_result::success ();
}


/* Function vect_transform_stmt.

   Create a vectorized stmt to replace STMT_INFO, and insert it at BSI.  */

bool
vect_transform_stmt (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		     slp_tree slp_node, slp_instance slp_node_instance)
{
  vec_info *vinfo = stmt_info->vinfo;
  bool is_store = false;
  stmt_vec_info vec_stmt = NULL;
  bool done;

  gcc_assert (slp_node || !PURE_SLP_STMT (stmt_info));
  stmt_vec_info old_vec_stmt_info = STMT_VINFO_VEC_STMT (stmt_info);

  bool nested_p = (STMT_VINFO_LOOP_VINFO (stmt_info)
		   && nested_in_vect_loop_p
		        (LOOP_VINFO_LOOP (STMT_VINFO_LOOP_VINFO (stmt_info)),
			 stmt_info));

  gimple *stmt = stmt_info->stmt;
  switch (STMT_VINFO_TYPE (stmt_info))
    {
    case type_demotion_vec_info_type:
    case type_promotion_vec_info_type:
    case type_conversion_vec_info_type:
      done = vectorizable_conversion (stmt_info, gsi, &vec_stmt, slp_node,
				      NULL);
      gcc_assert (done);
      break;

    case induc_vec_info_type:
      done = vectorizable_induction (stmt_info, gsi, &vec_stmt, slp_node,
				     NULL);
      gcc_assert (done);
      break;

    case shift_vec_info_type:
      done = vectorizable_shift (stmt_info, gsi, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      break;

    case op_vec_info_type:
      done = vectorizable_operation (stmt_info, gsi, &vec_stmt, slp_node,
				     NULL);
      gcc_assert (done);
      break;

    case assignment_vec_info_type:
      done = vectorizable_assignment (stmt_info, gsi, &vec_stmt, slp_node,
				      NULL);
      gcc_assert (done);
      break;

    case load_vec_info_type:
      done = vectorizable_load (stmt_info, gsi, &vec_stmt, slp_node,
                                slp_node_instance, NULL);
      gcc_assert (done);
      break;

    case store_vec_info_type:
      done = vectorizable_store (stmt_info, gsi, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info) && !slp_node)
	{
	  /* In case of interleaving, the whole chain is vectorized when the
	     last store in the chain is reached.  Store stmts before the last
	     one are skipped, and there vec_stmt_info shouldn't be freed
	     meanwhile.  */
	  stmt_vec_info group_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
	  if (DR_GROUP_STORE_COUNT (group_info) == DR_GROUP_SIZE (group_info))
	    is_store = true;
	}
      else
	is_store = true;
      break;

    case condition_vec_info_type:
      done = vectorizable_condition (stmt_info, gsi, &vec_stmt, NULL, 0,
				     slp_node, NULL);
      gcc_assert (done);
      break;

    case comparison_vec_info_type:
      done = vectorizable_comparison (stmt_info, gsi, &vec_stmt, NULL,
				      slp_node, NULL);
      gcc_assert (done);
      break;

    case call_vec_info_type:
      done = vectorizable_call (stmt_info, gsi, &vec_stmt, slp_node, NULL);
      stmt = gsi_stmt (*gsi);
      break;

    case call_simd_clone_vec_info_type:
      done = vectorizable_simd_clone_call (stmt_info, gsi, &vec_stmt,
					   slp_node, NULL);
      stmt = gsi_stmt (*gsi);
      break;

    case reduc_vec_info_type:
      done = vectorizable_reduction (stmt_info, gsi, &vec_stmt, slp_node,
				     slp_node_instance, NULL);
      gcc_assert (done);
      break;

    default:
      if (!STMT_VINFO_LIVE_P (stmt_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "stmt not supported.\n");
	  gcc_unreachable ();
	}
    }

  /* Verify SLP vectorization doesn't mess with STMT_VINFO_VEC_STMT.
     This would break hybrid SLP vectorization.  */
  if (slp_node)
    gcc_assert (!vec_stmt
		&& STMT_VINFO_VEC_STMT (stmt_info) == old_vec_stmt_info);

  /* Handle inner-loop stmts whose DEF is used in the loop-nest that
     is being vectorized, but outside the immediately enclosing loop.  */
  if (vec_stmt
      && nested_p
      && STMT_VINFO_TYPE (stmt_info) != reduc_vec_info_type
      && (STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_outer
          || STMT_VINFO_RELEVANT (stmt_info) ==
                                           vect_used_in_outer_by_reduction))
    {
      struct loop *innerloop = LOOP_VINFO_LOOP (
                                STMT_VINFO_LOOP_VINFO (stmt_info))->inner;
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      tree scalar_dest;

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "Record the vdef for outer-loop vectorization.\n");

      /* Find the relevant loop-exit phi-node, and reord the vec_stmt there
        (to be used when vectorizing outer-loop stmts that use the DEF of
        STMT).  */
      if (gimple_code (stmt) == GIMPLE_PHI)
        scalar_dest = PHI_RESULT (stmt);
      else
        scalar_dest = gimple_get_lhs (stmt);

      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, scalar_dest)
	if (!flow_bb_inside_loop_p (innerloop, gimple_bb (USE_STMT (use_p))))
	  {
	    stmt_vec_info exit_phi_info
	      = vinfo->lookup_stmt (USE_STMT (use_p));
	    STMT_VINFO_VEC_STMT (exit_phi_info) = vec_stmt;
	  }
    }

  /* Handle stmts whose DEF is used outside the loop-nest that is
     being vectorized.  */
  if (STMT_VINFO_TYPE (stmt_info) != reduc_vec_info_type)
    {
      done = can_vectorize_live_stmts (stmt_info, gsi, slp_node, &vec_stmt,
				       NULL);
      gcc_assert (done);
    }

  if (vec_stmt)
    STMT_VINFO_VEC_STMT (stmt_info) = vec_stmt;

  return is_store;
}


/* Remove a group of stores (for SLP or interleaving), free their
   stmt_vec_info.  */

void
vect_remove_stores (stmt_vec_info first_stmt_info)
{
  vec_info *vinfo = first_stmt_info->vinfo;
  stmt_vec_info next_stmt_info = first_stmt_info;

  while (next_stmt_info)
    {
      stmt_vec_info tmp = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
      next_stmt_info = vect_orig_stmt (next_stmt_info);
      /* Free the attached stmt_vec_info and remove the stmt.  */
      vinfo->remove_stmt (next_stmt_info);
      next_stmt_info = tmp;
    }
}

/* Function get_vectype_for_scalar_type_and_size.

   Returns the vector type corresponding to SCALAR_TYPE  and SIZE as supported
   by the target.  */

tree
get_vectype_for_scalar_type_and_size (tree scalar_type, poly_uint64 size)
{
  tree orig_scalar_type = scalar_type;
  scalar_mode inner_mode;
  machine_mode simd_mode;
  poly_uint64 nunits;
  tree vectype;

  if (!is_int_mode (TYPE_MODE (scalar_type), &inner_mode)
      && !is_float_mode (TYPE_MODE (scalar_type), &inner_mode))
    return NULL_TREE;

  unsigned int nbytes = GET_MODE_SIZE (inner_mode);

  /* For vector types of elements whose mode precision doesn't
     match their types precision we use a element type of mode
     precision.  The vectorization routines will have to make sure
     they support the proper result truncation/extension.
     We also make sure to build vector types with INTEGER_TYPE
     component type only.  */
  if (INTEGRAL_TYPE_P (scalar_type)
      && (GET_MODE_BITSIZE (inner_mode) != TYPE_PRECISION (scalar_type)
	  || TREE_CODE (scalar_type) != INTEGER_TYPE))
    scalar_type = build_nonstandard_integer_type (GET_MODE_BITSIZE (inner_mode),
						  TYPE_UNSIGNED (scalar_type));

  /* We shouldn't end up building VECTOR_TYPEs of non-scalar components.
     When the component mode passes the above test simply use a type
     corresponding to that mode.  The theory is that any use that
     would cause problems with this will disable vectorization anyway.  */
  else if (!SCALAR_FLOAT_TYPE_P (scalar_type)
	   && !INTEGRAL_TYPE_P (scalar_type))
    scalar_type = lang_hooks.types.type_for_mode (inner_mode, 1);

  /* We can't build a vector type of elements with alignment bigger than
     their size.  */
  else if (nbytes < TYPE_ALIGN_UNIT (scalar_type))
    scalar_type = lang_hooks.types.type_for_mode (inner_mode, 
						  TYPE_UNSIGNED (scalar_type));

  /* If we felt back to using the mode fail if there was
     no scalar type for it.  */
  if (scalar_type == NULL_TREE)
    return NULL_TREE;

  /* If no size was supplied use the mode the target prefers.   Otherwise
     lookup a vector mode of the specified size.  */
  if (known_eq (size, 0U))
    simd_mode = targetm.vectorize.preferred_simd_mode (inner_mode);
  else if (!multiple_p (size, nbytes, &nunits)
	   || !mode_for_vector (inner_mode, nunits).exists (&simd_mode))
    return NULL_TREE;
  /* NOTE: nunits == 1 is allowed to support single element vector types.  */
  if (!multiple_p (GET_MODE_SIZE (simd_mode), nbytes, &nunits))
    return NULL_TREE;

  vectype = build_vector_type (scalar_type, nunits);

  if (!VECTOR_MODE_P (TYPE_MODE (vectype))
      && !INTEGRAL_MODE_P (TYPE_MODE (vectype)))
    return NULL_TREE;

  /* Re-attach the address-space qualifier if we canonicalized the scalar
     type.  */
  if (TYPE_ADDR_SPACE (orig_scalar_type) != TYPE_ADDR_SPACE (vectype))
    return build_qualified_type
	     (vectype, KEEP_QUAL_ADDR_SPACE (TYPE_QUALS (orig_scalar_type)));

  return vectype;
}

poly_uint64 current_vector_size;

/* Function get_vectype_for_scalar_type.

   Returns the vector type corresponding to SCALAR_TYPE as supported
   by the target.  */

tree
get_vectype_for_scalar_type (tree scalar_type)
{
  tree vectype;
  vectype = get_vectype_for_scalar_type_and_size (scalar_type,
						  current_vector_size);
  if (vectype
      && known_eq (current_vector_size, 0U))
    current_vector_size = GET_MODE_SIZE (TYPE_MODE (vectype));
  return vectype;
}

/* Function get_mask_type_for_scalar_type.

   Returns the mask type corresponding to a result of comparison
   of vectors of specified SCALAR_TYPE as supported by target.  */

tree
get_mask_type_for_scalar_type (tree scalar_type)
{
  tree vectype = get_vectype_for_scalar_type (scalar_type);

  if (!vectype)
    return NULL;

  return build_truth_vector_type (TYPE_VECTOR_SUBPARTS (vectype),
				  current_vector_size);
}

/* Function get_same_sized_vectype

   Returns a vector type corresponding to SCALAR_TYPE of size
   VECTOR_TYPE if supported by the target.  */

tree
get_same_sized_vectype (tree scalar_type, tree vector_type)
{
  if (VECT_SCALAR_BOOLEAN_TYPE_P (scalar_type))
    return build_same_sized_truth_vector_type (vector_type);

  return get_vectype_for_scalar_type_and_size
	   (scalar_type, GET_MODE_SIZE (TYPE_MODE (vector_type)));
}

/* Function vect_is_simple_use.

   Input:
   VINFO - the vect info of the loop or basic block that is being vectorized.
   OPERAND - operand in the loop or bb.
   Output:
   DEF_STMT_INFO_OUT (optional) - information about the defining stmt in
     case OPERAND is an SSA_NAME that is defined in the vectorizable region
   DEF_STMT_OUT (optional) - the defining stmt in case OPERAND is an SSA_NAME;
     the definition could be anywhere in the function
   DT - the type of definition

   Returns whether a stmt with OPERAND can be vectorized.
   For loops, supportable operands are constants, loop invariants, and operands
   that are defined by the current iteration of the loop.  Unsupportable
   operands are those that are defined by a previous iteration of the loop (as
   is the case in reduction/induction computations).
   For basic blocks, supportable operands are constants and bb invariants.
   For now, operands defined outside the basic block are not supported.  */

bool
vect_is_simple_use (tree operand, vec_info *vinfo, enum vect_def_type *dt,
		    stmt_vec_info *def_stmt_info_out, gimple **def_stmt_out)
{
  if (def_stmt_info_out)
    *def_stmt_info_out = NULL;
  if (def_stmt_out)
    *def_stmt_out = NULL;
  *dt = vect_unknown_def_type;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "vect_is_simple_use: operand ");
      if (TREE_CODE (operand) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (operand))
	dump_gimple_expr (MSG_NOTE, TDF_SLIM, SSA_NAME_DEF_STMT (operand), 0);
      else
	dump_generic_expr (MSG_NOTE, TDF_SLIM, operand);
    }

  if (CONSTANT_CLASS_P (operand))
    *dt = vect_constant_def;
  else if (is_gimple_min_invariant (operand))
    *dt = vect_external_def;
  else if (TREE_CODE (operand) != SSA_NAME)
    *dt = vect_unknown_def_type;
  else if (SSA_NAME_IS_DEFAULT_DEF (operand))
    *dt = vect_external_def;
  else
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (operand);
      stmt_vec_info stmt_vinfo = vinfo->lookup_def (operand);
      if (!stmt_vinfo)
	*dt = vect_external_def;
      else
	{
	  stmt_vinfo = vect_stmt_to_vectorize (stmt_vinfo);
	  def_stmt = stmt_vinfo->stmt;
	  switch (gimple_code (def_stmt))
	    {
	    case GIMPLE_PHI:
	    case GIMPLE_ASSIGN:
	    case GIMPLE_CALL:
	      *dt = STMT_VINFO_DEF_TYPE (stmt_vinfo);
	      break;
	    default:
	      *dt = vect_unknown_def_type;
	      break;
	    }
	  if (def_stmt_info_out)
	    *def_stmt_info_out = stmt_vinfo;
	}
      if (def_stmt_out)
	*def_stmt_out = def_stmt;
    }

  if (dump_enabled_p ())
    {
      dump_printf (MSG_NOTE, ", type of def: ");
      switch (*dt)
	{
	case vect_uninitialized_def:
	  dump_printf (MSG_NOTE, "uninitialized\n");
	  break;
	case vect_constant_def:
	  dump_printf (MSG_NOTE, "constant\n");
	  break;
	case vect_external_def:
	  dump_printf (MSG_NOTE, "external\n");
	  break;
	case vect_internal_def:
	  dump_printf (MSG_NOTE, "internal\n");
	  break;
	case vect_induction_def:
	  dump_printf (MSG_NOTE, "induction\n");
	  break;
	case vect_reduction_def:
	  dump_printf (MSG_NOTE, "reduction\n");
	  break;
	case vect_double_reduction_def:
	  dump_printf (MSG_NOTE, "double reduction\n");
	  break;
	case vect_nested_cycle:
	  dump_printf (MSG_NOTE, "nested cycle\n");
	  break;
	case vect_unknown_def_type:
	  dump_printf (MSG_NOTE, "unknown\n");
	  break;
	}
    }

  if (*dt == vect_unknown_def_type)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "Unsupported pattern.\n");
      return false;
    }

  return true;
}

/* Function vect_is_simple_use.

   Same as vect_is_simple_use but also determines the vector operand
   type of OPERAND and stores it to *VECTYPE.  If the definition of
   OPERAND is vect_uninitialized_def, vect_constant_def or
   vect_external_def *VECTYPE will be set to NULL_TREE and the caller
   is responsible to compute the best suited vector type for the
   scalar operand.  */

bool
vect_is_simple_use (tree operand, vec_info *vinfo, enum vect_def_type *dt,
		    tree *vectype, stmt_vec_info *def_stmt_info_out,
		    gimple **def_stmt_out)
{
  stmt_vec_info def_stmt_info;
  gimple *def_stmt;
  if (!vect_is_simple_use (operand, vinfo, dt, &def_stmt_info, &def_stmt))
    return false;

  if (def_stmt_out)
    *def_stmt_out = def_stmt;
  if (def_stmt_info_out)
    *def_stmt_info_out = def_stmt_info;

  /* Now get a vector type if the def is internal, otherwise supply
     NULL_TREE and leave it up to the caller to figure out a proper
     type for the use stmt.  */
  if (*dt == vect_internal_def
      || *dt == vect_induction_def
      || *dt == vect_reduction_def
      || *dt == vect_double_reduction_def
      || *dt == vect_nested_cycle)
    {
      *vectype = STMT_VINFO_VECTYPE (def_stmt_info);
      gcc_assert (*vectype != NULL_TREE);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "vect_is_simple_use: vectype %T\n", *vectype);
    }
  else if (*dt == vect_uninitialized_def
	   || *dt == vect_constant_def
	   || *dt == vect_external_def)
    *vectype = NULL_TREE;
  else
    gcc_unreachable ();

  return true;
}


/* Function supportable_widening_operation

   Check whether an operation represented by the code CODE is a
   widening operation that is supported by the target platform in
   vector form (i.e., when operating on arguments of type VECTYPE_IN
   producing a result of type VECTYPE_OUT).

   Widening operations we currently support are NOP (CONVERT), FLOAT,
   FIX_TRUNC and WIDEN_MULT.  This function checks if these operations
   are supported by the target platform either directly (via vector
   tree-codes), or via target builtins.

   Output:
   - CODE1 and CODE2 are codes of vector operations to be used when
   vectorizing the operation, if available.
   - MULTI_STEP_CVT determines the number of required intermediate steps in
   case of multi-step conversion (like char->short->int - in that case
   MULTI_STEP_CVT will be 1).
   - INTERM_TYPES contains the intermediate type required to perform the
   widening operation (short in the above example).  */

bool
supportable_widening_operation (enum tree_code code, stmt_vec_info stmt_info,
				tree vectype_out, tree vectype_in,
                                enum tree_code *code1, enum tree_code *code2,
                                int *multi_step_cvt,
                                vec<tree> *interm_types)
{
  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *vect_loop = NULL;
  machine_mode vec_mode;
  enum insn_code icode1, icode2;
  optab optab1, optab2;
  tree vectype = vectype_in;
  tree wide_vectype = vectype_out;
  enum tree_code c1, c2;
  int i;
  tree prev_type, intermediate_type;
  machine_mode intermediate_mode, prev_mode;
  optab optab3, optab4;

  *multi_step_cvt = 0;
  if (loop_info)
    vect_loop = LOOP_VINFO_LOOP (loop_info);

  switch (code)
    {
    case WIDEN_MULT_EXPR:
      /* The result of a vectorized widening operation usually requires
	 two vectors (because the widened results do not fit into one vector).
	 The generated vector results would normally be expected to be
	 generated in the same order as in the original scalar computation,
	 i.e. if 8 results are generated in each vector iteration, they are
	 to be organized as follows:
		vect1: [res1,res2,res3,res4],
		vect2: [res5,res6,res7,res8].

	 However, in the special case that the result of the widening
	 operation is used in a reduction computation only, the order doesn't
	 matter (because when vectorizing a reduction we change the order of
	 the computation).  Some targets can take advantage of this and
	 generate more efficient code.  For example, targets like Altivec,
	 that support widen_mult using a sequence of {mult_even,mult_odd}
	 generate the following vectors:
		vect1: [res1,res3,res5,res7],
		vect2: [res2,res4,res6,res8].

	 When vectorizing outer-loops, we execute the inner-loop sequentially
	 (each vectorized inner-loop iteration contributes to VF outer-loop
	 iterations in parallel).  We therefore don't allow to change the
	 order of the computation in the inner-loop during outer-loop
	 vectorization.  */
      /* TODO: Another case in which order doesn't *really* matter is when we
	 widen and then contract again, e.g. (short)((int)x * y >> 8).
	 Normally, pack_trunc performs an even/odd permute, whereas the 
	 repack from an even/odd expansion would be an interleave, which
	 would be significantly simpler for e.g. AVX2.  */
      /* In any case, in order to avoid duplicating the code below, recurse
	 on VEC_WIDEN_MULT_EVEN_EXPR.  If it succeeds, all the return values
	 are properly set up for the caller.  If we fail, we'll continue with
	 a VEC_WIDEN_MULT_LO/HI_EXPR check.  */
      if (vect_loop
	  && STMT_VINFO_RELEVANT (stmt_info) == vect_used_by_reduction
	  && !nested_in_vect_loop_p (vect_loop, stmt_info)
	  && supportable_widening_operation (VEC_WIDEN_MULT_EVEN_EXPR,
					     stmt_info, vectype_out,
					     vectype_in, code1, code2,
					     multi_step_cvt, interm_types))
        {
          /* Elements in a vector with vect_used_by_reduction property cannot
             be reordered if the use chain with this property does not have the
             same operation.  One such an example is s += a * b, where elements
             in a and b cannot be reordered.  Here we check if the vector defined
             by STMT is only directly used in the reduction statement.  */
	  tree lhs = gimple_assign_lhs (stmt_info->stmt);
	  stmt_vec_info use_stmt_info = loop_info->lookup_single_use (lhs);
	  if (use_stmt_info
	      && STMT_VINFO_DEF_TYPE (use_stmt_info) == vect_reduction_def)
	    return true;
        }
      c1 = VEC_WIDEN_MULT_LO_EXPR;
      c2 = VEC_WIDEN_MULT_HI_EXPR;
      break;

    case DOT_PROD_EXPR:
      c1 = DOT_PROD_EXPR;
      c2 = DOT_PROD_EXPR;
      break;

    case SAD_EXPR:
      c1 = SAD_EXPR;
      c2 = SAD_EXPR;
      break;

    case VEC_WIDEN_MULT_EVEN_EXPR:
      /* Support the recursion induced just above.  */
      c1 = VEC_WIDEN_MULT_EVEN_EXPR;
      c2 = VEC_WIDEN_MULT_ODD_EXPR;
      break;

    case WIDEN_LSHIFT_EXPR:
      c1 = VEC_WIDEN_LSHIFT_LO_EXPR;
      c2 = VEC_WIDEN_LSHIFT_HI_EXPR;
      break;

    CASE_CONVERT:
      c1 = VEC_UNPACK_LO_EXPR;
      c2 = VEC_UNPACK_HI_EXPR;
      break;

    case FLOAT_EXPR:
      c1 = VEC_UNPACK_FLOAT_LO_EXPR;
      c2 = VEC_UNPACK_FLOAT_HI_EXPR;
      break;

    case FIX_TRUNC_EXPR:
      c1 = VEC_UNPACK_FIX_TRUNC_LO_EXPR;
      c2 = VEC_UNPACK_FIX_TRUNC_HI_EXPR;
      break;

    default:
      gcc_unreachable ();
    }

  if (BYTES_BIG_ENDIAN && c1 != VEC_WIDEN_MULT_EVEN_EXPR)
    std::swap (c1, c2);

  if (code == FIX_TRUNC_EXPR)
    {
      /* The signedness is determined from output operand.  */
      optab1 = optab_for_tree_code (c1, vectype_out, optab_default);
      optab2 = optab_for_tree_code (c2, vectype_out, optab_default);
    }
  else
    {
      optab1 = optab_for_tree_code (c1, vectype, optab_default);
      optab2 = optab_for_tree_code (c2, vectype, optab_default);
    }

  if (!optab1 || !optab2)
    return false;

  vec_mode = TYPE_MODE (vectype);
  if ((icode1 = optab_handler (optab1, vec_mode)) == CODE_FOR_nothing
       || (icode2 = optab_handler (optab2, vec_mode)) == CODE_FOR_nothing)
    return false;

  *code1 = c1;
  *code2 = c2;

  if (insn_data[icode1].operand[0].mode == TYPE_MODE (wide_vectype)
      && insn_data[icode2].operand[0].mode == TYPE_MODE (wide_vectype))
      /* For scalar masks we may have different boolean
	 vector types having the same QImode.  Thus we
	 add additional check for elements number.  */
    return (!VECTOR_BOOLEAN_TYPE_P (vectype)
	    || known_eq (TYPE_VECTOR_SUBPARTS (vectype),
			 TYPE_VECTOR_SUBPARTS (wide_vectype) * 2));

  /* Check if it's a multi-step conversion that can be done using intermediate
     types.  */

  prev_type = vectype;
  prev_mode = vec_mode;

  if (!CONVERT_EXPR_CODE_P (code))
    return false;

  /* We assume here that there will not be more than MAX_INTERM_CVT_STEPS
     intermediate steps in promotion sequence.  We try
     MAX_INTERM_CVT_STEPS to get to NARROW_VECTYPE, and fail if we do
     not.  */
  interm_types->create (MAX_INTERM_CVT_STEPS);
  for (i = 0; i < MAX_INTERM_CVT_STEPS; i++)
    {
      intermediate_mode = insn_data[icode1].operand[0].mode;
      if (VECTOR_BOOLEAN_TYPE_P (prev_type))
	{
	  intermediate_type = vect_halve_mask_nunits (prev_type);
	  if (intermediate_mode != TYPE_MODE (intermediate_type))
	    return false;
	}
      else
	intermediate_type
	  = lang_hooks.types.type_for_mode (intermediate_mode,
					    TYPE_UNSIGNED (prev_type));

      optab3 = optab_for_tree_code (c1, intermediate_type, optab_default);
      optab4 = optab_for_tree_code (c2, intermediate_type, optab_default);

      if (!optab3 || !optab4
          || (icode1 = optab_handler (optab1, prev_mode)) == CODE_FOR_nothing
	  || insn_data[icode1].operand[0].mode != intermediate_mode
	  || (icode2 = optab_handler (optab2, prev_mode)) == CODE_FOR_nothing
	  || insn_data[icode2].operand[0].mode != intermediate_mode
	  || ((icode1 = optab_handler (optab3, intermediate_mode))
	      == CODE_FOR_nothing)
	  || ((icode2 = optab_handler (optab4, intermediate_mode))
	      == CODE_FOR_nothing))
	break;

      interm_types->quick_push (intermediate_type);
      (*multi_step_cvt)++;

      if (insn_data[icode1].operand[0].mode == TYPE_MODE (wide_vectype)
	  && insn_data[icode2].operand[0].mode == TYPE_MODE (wide_vectype))
	return (!VECTOR_BOOLEAN_TYPE_P (vectype)
		|| known_eq (TYPE_VECTOR_SUBPARTS (intermediate_type),
			     TYPE_VECTOR_SUBPARTS (wide_vectype) * 2));

      prev_type = intermediate_type;
      prev_mode = intermediate_mode;
    }

  interm_types->release ();
  return false;
}


/* Function supportable_narrowing_operation

   Check whether an operation represented by the code CODE is a
   narrowing operation that is supported by the target platform in
   vector form (i.e., when operating on arguments of type VECTYPE_IN
   and producing a result of type VECTYPE_OUT).

   Narrowing operations we currently support are NOP (CONVERT), FIX_TRUNC
   and FLOAT.  This function checks if these operations are supported by
   the target platform directly via vector tree-codes.

   Output:
   - CODE1 is the code of a vector operation to be used when
   vectorizing the operation, if available.
   - MULTI_STEP_CVT determines the number of required intermediate steps in
   case of multi-step conversion (like int->short->char - in that case
   MULTI_STEP_CVT will be 1).
   - INTERM_TYPES contains the intermediate type required to perform the
   narrowing operation (short in the above example).   */

bool
supportable_narrowing_operation (enum tree_code code,
				 tree vectype_out, tree vectype_in,
				 enum tree_code *code1, int *multi_step_cvt,
                                 vec<tree> *interm_types)
{
  machine_mode vec_mode;
  enum insn_code icode1;
  optab optab1, interm_optab;
  tree vectype = vectype_in;
  tree narrow_vectype = vectype_out;
  enum tree_code c1;
  tree intermediate_type, prev_type;
  machine_mode intermediate_mode, prev_mode;
  int i;
  bool uns;

  *multi_step_cvt = 0;
  switch (code)
    {
    CASE_CONVERT:
      c1 = VEC_PACK_TRUNC_EXPR;
      break;

    case FIX_TRUNC_EXPR:
      c1 = VEC_PACK_FIX_TRUNC_EXPR;
      break;

    case FLOAT_EXPR:
      c1 = VEC_PACK_FLOAT_EXPR;
      break;

    default:
      gcc_unreachable ();
    }

  if (code == FIX_TRUNC_EXPR)
    /* The signedness is determined from output operand.  */
    optab1 = optab_for_tree_code (c1, vectype_out, optab_default);
  else
    optab1 = optab_for_tree_code (c1, vectype, optab_default);

  if (!optab1)
    return false;

  vec_mode = TYPE_MODE (vectype);
  if ((icode1 = optab_handler (optab1, vec_mode)) == CODE_FOR_nothing)
    return false;

  *code1 = c1;

  if (insn_data[icode1].operand[0].mode == TYPE_MODE (narrow_vectype))
    /* For scalar masks we may have different boolean
       vector types having the same QImode.  Thus we
       add additional check for elements number.  */
    return (!VECTOR_BOOLEAN_TYPE_P (vectype)
	    || known_eq (TYPE_VECTOR_SUBPARTS (vectype) * 2,
			 TYPE_VECTOR_SUBPARTS (narrow_vectype)));

  if (code == FLOAT_EXPR)
    return false;

  /* Check if it's a multi-step conversion that can be done using intermediate
     types.  */
  prev_mode = vec_mode;
  prev_type = vectype;
  if (code == FIX_TRUNC_EXPR)
    uns = TYPE_UNSIGNED (vectype_out);
  else
    uns = TYPE_UNSIGNED (vectype);

  /* For multi-step FIX_TRUNC_EXPR prefer signed floating to integer
     conversion over unsigned, as unsigned FIX_TRUNC_EXPR is often more
     costly than signed.  */
  if (code == FIX_TRUNC_EXPR && uns)
    {
      enum insn_code icode2;

      intermediate_type
	= lang_hooks.types.type_for_mode (TYPE_MODE (vectype_out), 0);
      interm_optab
	= optab_for_tree_code (c1, intermediate_type, optab_default);
      if (interm_optab != unknown_optab
	  && (icode2 = optab_handler (optab1, vec_mode)) != CODE_FOR_nothing
	  && insn_data[icode1].operand[0].mode
	     == insn_data[icode2].operand[0].mode)
	{
	  uns = false;
	  optab1 = interm_optab;
	  icode1 = icode2;
	}
    }

  /* We assume here that there will not be more than MAX_INTERM_CVT_STEPS
     intermediate steps in promotion sequence.  We try
     MAX_INTERM_CVT_STEPS to get to NARROW_VECTYPE, and fail if we do not.  */
  interm_types->create (MAX_INTERM_CVT_STEPS);
  for (i = 0; i < MAX_INTERM_CVT_STEPS; i++)
    {
      intermediate_mode = insn_data[icode1].operand[0].mode;
      if (VECTOR_BOOLEAN_TYPE_P (prev_type))
	{
	  intermediate_type = vect_double_mask_nunits (prev_type);
	  if (intermediate_mode != TYPE_MODE (intermediate_type))
	    return false;
	}
      else
	intermediate_type
	  = lang_hooks.types.type_for_mode (intermediate_mode, uns);
      interm_optab
	= optab_for_tree_code (VEC_PACK_TRUNC_EXPR, intermediate_type,
			       optab_default);
      if (!interm_optab
	  || ((icode1 = optab_handler (optab1, prev_mode)) == CODE_FOR_nothing)
	  || insn_data[icode1].operand[0].mode != intermediate_mode
	  || ((icode1 = optab_handler (interm_optab, intermediate_mode))
	      == CODE_FOR_nothing))
	break;

      interm_types->quick_push (intermediate_type);
      (*multi_step_cvt)++;

      if (insn_data[icode1].operand[0].mode == TYPE_MODE (narrow_vectype))
	return (!VECTOR_BOOLEAN_TYPE_P (vectype)
		|| known_eq (TYPE_VECTOR_SUBPARTS (intermediate_type) * 2,
			     TYPE_VECTOR_SUBPARTS (narrow_vectype)));

      prev_mode = intermediate_mode;
      prev_type = intermediate_type;
      optab1 = interm_optab;
    }

  interm_types->release ();
  return false;
}

/* Generate and return a statement that sets vector mask MASK such that
   MASK[I] is true iff J + START_INDEX < END_INDEX for all J <= I.  */

gcall *
vect_gen_while (tree mask, tree start_index, tree end_index)
{
  tree cmp_type = TREE_TYPE (start_index);
  tree mask_type = TREE_TYPE (mask);
  gcc_checking_assert (direct_internal_fn_supported_p (IFN_WHILE_ULT,
						       cmp_type, mask_type,
						       OPTIMIZE_FOR_SPEED));
  gcall *call = gimple_build_call_internal (IFN_WHILE_ULT, 3,
					    start_index, end_index,
					    build_zero_cst (mask_type));
  gimple_call_set_lhs (call, mask);
  return call;
}

/* Generate a vector mask of type MASK_TYPE for which index I is false iff
   J + START_INDEX < END_INDEX for all J <= I.  Add the statements to SEQ.  */

tree
vect_gen_while_not (gimple_seq *seq, tree mask_type, tree start_index,
		    tree end_index)
{
  tree tmp = make_ssa_name (mask_type);
  gcall *call = vect_gen_while (tmp, start_index, end_index);
  gimple_seq_add_stmt (seq, call);
  return gimple_build (seq, BIT_NOT_EXPR, mask_type, tmp);
}

/* Try to compute the vector types required to vectorize STMT_INFO,
   returning true on success and false if vectorization isn't possible.

   On success:

   - Set *STMT_VECTYPE_OUT to:
     - NULL_TREE if the statement doesn't need to be vectorized;
     - boolean_type_node if the statement is a boolean operation whose
       vector type can only be determined once all the other vector types
       are known; and
     - the equivalent of STMT_VINFO_VECTYPE otherwise.

   - Set *NUNITS_VECTYPE_OUT to the vector type that contains the maximum
     number of units needed to vectorize STMT_INFO, or NULL_TREE if the
     statement does not help to determine the overall number of units.  */

opt_result
vect_get_vector_types_for_stmt (stmt_vec_info stmt_info,
				tree *stmt_vectype_out,
				tree *nunits_vectype_out)
{
  gimple *stmt = stmt_info->stmt;

  *stmt_vectype_out = NULL_TREE;
  *nunits_vectype_out = NULL_TREE;

  if (gimple_get_lhs (stmt) == NULL_TREE
      /* MASK_STORE has no lhs, but is ok.  */
      && !gimple_call_internal_p (stmt, IFN_MASK_STORE))
    {
      if (is_a <gcall *> (stmt))
	{
	  /* Ignore calls with no lhs.  These must be calls to
	     #pragma omp simd functions, and what vectorization factor
	     it really needs can't be determined until
	     vectorizable_simd_clone_call.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "defer to SIMD clone analysis.\n");
	  return opt_result::success ();
	}

      return opt_result::failure_at (stmt,
				     "not vectorized: irregular stmt.%G", stmt);
    }

  if (VECTOR_MODE_P (TYPE_MODE (gimple_expr_type (stmt))))
    return opt_result::failure_at (stmt,
				   "not vectorized: vector stmt in loop:%G",
				   stmt);

  tree vectype;
  tree scalar_type = NULL_TREE;
  if (STMT_VINFO_VECTYPE (stmt_info))
    *stmt_vectype_out = vectype = STMT_VINFO_VECTYPE (stmt_info);
  else
    {
      gcc_assert (!STMT_VINFO_DATA_REF (stmt_info));
      if (gimple_call_internal_p (stmt, IFN_MASK_STORE))
	scalar_type = TREE_TYPE (gimple_call_arg (stmt, 3));
      else
	scalar_type = TREE_TYPE (gimple_get_lhs (stmt));

      /* Pure bool ops don't participate in number-of-units computation.
	 For comparisons use the types being compared.  */
      if (VECT_SCALAR_BOOLEAN_TYPE_P (scalar_type)
	  && is_gimple_assign (stmt)
	  && gimple_assign_rhs_code (stmt) != COND_EXPR)
	{
	  *stmt_vectype_out = boolean_type_node;

	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  if (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_comparison
	      && !VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (rhs1)))
	    scalar_type = TREE_TYPE (rhs1);
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "pure bool operation.\n");
	      return opt_result::success ();
	    }
	}

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "get vectype for scalar type:  %T\n", scalar_type);
      vectype = get_vectype_for_scalar_type (scalar_type);
      if (!vectype)
	return opt_result::failure_at (stmt,
				       "not vectorized:"
				       " unsupported data-type %T\n",
				       scalar_type);

      if (!*stmt_vectype_out)
	*stmt_vectype_out = vectype;

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "vectype: %T\n", vectype);
    }

  /* Don't try to compute scalar types if the stmt produces a boolean
     vector; use the existing vector type instead.  */
  tree nunits_vectype;
  if (VECTOR_BOOLEAN_TYPE_P (vectype))
    nunits_vectype = vectype;
  else
    {
      /* The number of units is set according to the smallest scalar
	 type (or the largest vector size, but we only support one
	 vector size per vectorization).  */
      if (*stmt_vectype_out != boolean_type_node)
	{
	  HOST_WIDE_INT dummy;
	  scalar_type = vect_get_smallest_scalar_type (stmt_info,
						       &dummy, &dummy);
	}
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "get vectype for scalar type:  %T\n", scalar_type);
      nunits_vectype = get_vectype_for_scalar_type (scalar_type);
    }
  if (!nunits_vectype)
    return opt_result::failure_at (stmt,
				   "not vectorized: unsupported data-type %T\n",
				   scalar_type);

  if (maybe_ne (GET_MODE_SIZE (TYPE_MODE (vectype)),
		GET_MODE_SIZE (TYPE_MODE (nunits_vectype))))
    return opt_result::failure_at (stmt,
				   "not vectorized: different sized vector "
				   "types in statement, %T and %T\n",
				   vectype, nunits_vectype);

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "vectype: %T\n",
		       nunits_vectype);

      dump_printf_loc (MSG_NOTE, vect_location, "nunits = ");
      dump_dec (MSG_NOTE, TYPE_VECTOR_SUBPARTS (nunits_vectype));
      dump_printf (MSG_NOTE, "\n");
    }

  *nunits_vectype_out = nunits_vectype;
  return opt_result::success ();
}

/* Try to determine the correct vector type for STMT_INFO, which is a
   statement that produces a scalar boolean result.  Return the vector
   type on success, otherwise return NULL_TREE.  */

opt_tree
vect_get_mask_type_for_stmt (stmt_vec_info stmt_info)
{
  gimple *stmt = stmt_info->stmt;
  tree mask_type = NULL;
  tree vectype, scalar_type;

  if (is_gimple_assign (stmt)
      && TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_comparison
      && !VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (stmt))))
    {
      scalar_type = TREE_TYPE (gimple_assign_rhs1 (stmt));
      mask_type = get_mask_type_for_scalar_type (scalar_type);

      if (!mask_type)
	return opt_tree::failure_at (stmt,
				     "not vectorized: unsupported mask\n");
    }
  else
    {
      tree rhs;
      ssa_op_iter iter;
      enum vect_def_type dt;

      FOR_EACH_SSA_TREE_OPERAND (rhs, stmt, iter, SSA_OP_USE)
	{
	  if (!vect_is_simple_use (rhs, stmt_info->vinfo, &dt, &vectype))
	    return opt_tree::failure_at (stmt,
					 "not vectorized:can't compute mask"
					 " type for statement, %G", stmt);

	  /* No vectype probably means external definition.
	     Allow it in case there is another operand which
	     allows to determine mask type.  */
	  if (!vectype)
	    continue;

	  if (!mask_type)
	    mask_type = vectype;
	  else if (maybe_ne (TYPE_VECTOR_SUBPARTS (mask_type),
			     TYPE_VECTOR_SUBPARTS (vectype)))
	    return opt_tree::failure_at (stmt,
					 "not vectorized: different sized mask"
					 " types in statement, %T and %T\n",
					 mask_type, vectype);
	  else if (VECTOR_BOOLEAN_TYPE_P (mask_type)
		   != VECTOR_BOOLEAN_TYPE_P (vectype))
	    return opt_tree::failure_at (stmt,
					 "not vectorized: mixed mask and "
					 "nonmask vector types in statement, "
					 "%T and %T\n",
					 mask_type, vectype);
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
     This is probably a subject for optimization in if-conversion.  */
  if (!mask_type)
    return opt_tree::failure_at (stmt,
				 "not vectorized: can't compute mask type "
				 "for statement: %G", stmt);

  return opt_tree::success (mask_type);
}
