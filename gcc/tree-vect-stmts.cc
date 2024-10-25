/* Statement Analysis and Transformation for Vectorization
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
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
#include "explow.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "builtins.h"
#include "internal-fn.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"
#include "gimple-range.h"
#include "tree-ssa-loop-niter.h"
#include "gimple-fold.h"
#include "regs.h"
#include "attribs.h"
#include "optabs-libfuncs.h"

/* For lang_hooks.types.type_for_mode.  */
#include "langhooks.h"

/* Return the vectorized type for the given statement.  */

tree
stmt_vectype (class _stmt_vec_info *stmt_info)
{
  return STMT_VINFO_VECTYPE (stmt_info);
}

/* Return TRUE iff the given statement is in an inner loop relative to
   the loop being vectorized.  */
bool
stmt_in_inner_loop_p (vec_info *vinfo, class _stmt_vec_info *stmt_info)
{
  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  basic_block bb = gimple_bb (stmt);
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  class loop* loop;

  if (!loop_vinfo)
    return false;

  loop = LOOP_VINFO_LOOP (loop_vinfo);

  return (bb->loop_father == loop->inner);
}

/* Record the cost of a statement, either by directly informing the
   target model or by saving it in a vector for later processing.
   Return a preliminary estimate of the statement's cost.  */

static unsigned
record_stmt_cost (stmt_vector_for_cost *body_cost_vec, int count,
		  enum vect_cost_for_stmt kind,
		  stmt_vec_info stmt_info, slp_tree node,
		  tree vectype, int misalign,
		  enum vect_cost_model_location where)
{
  if ((kind == vector_load || kind == unaligned_load)
      && (stmt_info && STMT_VINFO_GATHER_SCATTER_P (stmt_info)))
    kind = vector_gather_load;
  if ((kind == vector_store || kind == unaligned_store)
      && (stmt_info && STMT_VINFO_GATHER_SCATTER_P (stmt_info)))
    kind = vector_scatter_store;

  stmt_info_for_cost si
    = { count, kind, where, stmt_info, node, vectype, misalign };
  body_cost_vec->safe_push (si);

  return (unsigned)
      (builtin_vectorization_cost (kind, vectype, misalign) * count);
}

unsigned
record_stmt_cost (stmt_vector_for_cost *body_cost_vec, int count,
		  enum vect_cost_for_stmt kind, stmt_vec_info stmt_info,
		  tree vectype, int misalign,
		  enum vect_cost_model_location where)
{
  return record_stmt_cost (body_cost_vec, count, kind, stmt_info, NULL,
			   vectype, misalign, where);
}

unsigned
record_stmt_cost (stmt_vector_for_cost *body_cost_vec, int count,
		  enum vect_cost_for_stmt kind, slp_tree node,
		  tree vectype, int misalign,
		  enum vect_cost_model_location where)
{
  return record_stmt_cost (body_cost_vec, count, kind, NULL, node,
			   vectype, misalign, where);
}

unsigned
record_stmt_cost (stmt_vector_for_cost *body_cost_vec, int count,
		  enum vect_cost_for_stmt kind,
		  enum vect_cost_model_location where)
{
  gcc_assert (kind == cond_branch_taken || kind == cond_branch_not_taken
	      || kind == scalar_stmt);
  return record_stmt_cost (body_cost_vec, count, kind, NULL, NULL,
			   NULL_TREE, 0, where);
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
read_vector_array (vec_info *vinfo,
		   stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
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
  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

  return vect_name;
}

/* ARRAY is an array of vectors created by create_vector_array.
   Emit code to store SSA_NAME VECT in index N of the array.
   The store is part of the vectorization of STMT_INFO.  */

static void
write_vector_array (vec_info *vinfo,
		    stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    tree vect, tree array, unsigned HOST_WIDE_INT n)
{
  tree array_ref;
  gimple *new_stmt;

  array_ref = build4 (ARRAY_REF, TREE_TYPE (vect), array,
		      build_int_cst (size_type_node, n),
		      NULL_TREE, NULL_TREE);

  new_stmt = gimple_build_assign (array_ref, vect);
  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
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
vect_clobber_variable (vec_info *vinfo, stmt_vec_info stmt_info,
		       gimple_stmt_iterator *gsi, tree var)
{
  tree clobber = build_clobber (TREE_TYPE (var));
  gimple *new_stmt = gimple_build_assign (var, clobber);
  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
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

      if (live_p && relevant == vect_unused_in_scope)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vec_stmt_relevant_p: forcing live pattern stmt "
			     "relevant.\n");
	  relevant = vect_used_only_live;
	}

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "mark relevant %d, live %d: %G", relevant, live_p,
			 stmt_info->stmt);
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
   - it is an induction and we have multiple exits.

   CHECKME: what other side effects would the vectorizer allow?  */

static bool
vect_stmt_relevant_p (stmt_vec_info stmt_info, loop_vec_info loop_vinfo,
		      enum vect_relevant *relevant, bool *live_p)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  def_operand_p def_p;

  *relevant = vect_unused_in_scope;
  *live_p = false;

  /* cond stmt other than loop exit cond.  */
  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  if (is_ctrl_stmt (stmt)
      && LOOP_VINFO_LOOP_IV_COND (loop_vinfo) != stmt
      && (!loop->inner || gimple_bb (stmt)->loop_father == loop))
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
	      if (is_gimple_debug (USE_STMT (use_p)))
		continue;

	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
                                 "vec_stmt_relevant_p: used out of loop.\n");

	      /* We expect all such uses to be in the loop exit phis
		 (because of loop closed form)   */
	      gcc_assert (gimple_code (USE_STMT (use_p)) == GIMPLE_PHI);

              *live_p = true;
	    }
	}
    }

  /* Check if it's an induction and multiple exits.  In this case there will be
     a usage later on after peeling which is needed for the alternate exit.  */
  if (LOOP_VINFO_EARLY_BREAKS (loop_vinfo)
      && STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def)
    {
      if (dump_enabled_p ())
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "vec_stmt_relevant_p: induction forced for "
			   "early break.\n");
      LOOP_VINFO_EARLY_BREAKS_LIVE_IVS (loop_vinfo).safe_push (stmt_info);
      *live_p = true;

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

  basic_block def_bb = gimple_bb (dstmt_vinfo->stmt);
  basic_block bb = gimple_bb (stmt_vinfo->stmt);

  /* case 2: A reduction phi (STMT) defined by a reduction stmt (DSTMT_VINFO).
     We have to force the stmt live since the epilogue loop needs it to
     continue computing the reduction.  */
  if (gimple_code (stmt_vinfo->stmt) == GIMPLE_PHI
      && STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
      && gimple_code (dstmt_vinfo->stmt) != GIMPLE_PHI
      && STMT_VINFO_DEF_TYPE (dstmt_vinfo) == vect_reduction_def
      && bb->loop_father == def_bb->loop_father)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "reduc-stmt defining reduc-phi in the same nest.\n");
      vect_mark_relevant (worklist, dstmt_vinfo, relevant, true);
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
vect_mark_stmts_to_be_vectorized (loop_vec_info loop_vinfo, bool *fatal)
{
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
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
	  if (is_gimple_debug (gsi_stmt (si)))
	    continue;
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
	  else if (gcond *cond = dyn_cast <gcond *> (stmt_vinfo->stmt))
	    {
	      tree_code rhs_code = gimple_cond_code (cond);
	      gcc_assert (TREE_CODE_CLASS (rhs_code) == tcc_comparison);
	      opt_result res
		= process_use (stmt_vinfo, gimple_cond_lhs (cond),
			       loop_vinfo, relevant, &worklist, false);
	      if (!res)
		return res;
	      res = process_use (stmt_vinfo, gimple_cond_rhs (cond),
				loop_vinfo, relevant, &worklist, false);
	      if (!res)
		return res;
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
	  else
	    gcc_unreachable ();
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
	    {
	      if (fatal)
		*fatal = false;
	      return res;
	    }
	}
    } /* while worklist */

  return opt_result::success ();
}

/* Function vect_model_simple_cost.

   Models cost for simple operations, i.e. those that only emit ncopies of a
   single op.  Right now, this does not account for multiple insns that could
   be generated for the single vector op.  We will handle that shortly.  */

static void
vect_model_simple_cost (vec_info *,
			stmt_vec_info stmt_info, int ncopies,
			enum vect_def_type *dt,
			int ndts,
			slp_tree node,
			stmt_vector_for_cost *cost_vec,
			vect_cost_for_stmt kind = vector_stmt)
{
  int inside_cost = 0, prologue_cost = 0;

  gcc_assert (cost_vec != NULL);

  /* ???  Somehow we need to fix this at the callers.  */
  if (node)
    ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (node);

  if (!node)
    /* Cost the "broadcast" of a scalar operand in to a vector operand.
       Use scalar_to_vec to cost the broadcast, as elsewhere in the vector
       cost model.  */
    for (int i = 0; i < ndts; i++)
      if (dt[i] == vect_constant_def || dt[i] == vect_external_def)
	prologue_cost += record_stmt_cost (cost_vec, 1, scalar_to_vec,
					   stmt_info, 0, vect_prologue);

  /* Pass the inside-of-loop statements to the target-specific cost model.  */
  inside_cost += record_stmt_cost (cost_vec, ncopies, kind,
				   stmt_info, 0, vect_body);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_simple_cost: inside_cost = %d, "
                     "prologue_cost = %d .\n", inside_cost, prologue_cost);
}


/* Model cost for type demotion and promotion operations.  PWR is
   normally zero for single-step promotions and demotions.  It will be
   one if two-step promotion/demotion is required, and so on.  NCOPIES
   is the number of vector results (and thus number of instructions)
   for the narrowest end of the operation chain.  Each additional
   step doubles the number of instructions required.  If WIDEN_ARITH
   is true the stmt is doing widening arithmetic.  */

static void
vect_model_promotion_demotion_cost (stmt_vec_info stmt_info,
				    enum vect_def_type *dt,
				    unsigned int ncopies, int pwr,
				    stmt_vector_for_cost *cost_vec,
				    bool widen_arith)
{
  int i;
  int inside_cost = 0, prologue_cost = 0;

  for (i = 0; i < pwr + 1; i++)
    {
      inside_cost += record_stmt_cost (cost_vec, ncopies,
				       widen_arith
				       ? vector_stmt : vec_promote_demote,
				       stmt_info, 0, vect_body);
      ncopies *= 2;
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

/* Returns true if the current function returns DECL.  */

static bool
cfun_returns (tree decl)
{
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      greturn *ret = safe_dyn_cast <greturn *> (*gsi_last_bb (e->src));
      if (!ret)
	continue;
      if (gimple_return_retval (ret) == decl)
	return true;
      /* We often end up with an aggregate copy to the result decl,
         handle that case as well.  First skip intermediate clobbers
	 though.  */
      gimple *def = ret;
      do
	{
	  def = SSA_NAME_DEF_STMT (gimple_vuse (def));
	}
      while (gimple_clobber_p (def));
      if (is_a <gassign *> (def)
	  && gimple_assign_lhs (def) == gimple_return_retval (ret)
	  && gimple_assign_rhs1 (def) == decl)
	return true;
    }
  return false;
}

/* Calculate cost of DR's memory access.  */
void
vect_get_store_cost (vec_info *, stmt_vec_info stmt_info, int ncopies,
		     dr_alignment_support alignment_support_scheme,
		     int misalignment,
		     unsigned int *inside_cost,
		     stmt_vector_for_cost *body_cost_vec)
{
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
					  misalignment, vect_body);
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

/* Calculate cost of DR's memory access.  */
void
vect_get_load_cost (vec_info *, stmt_vec_info stmt_info, int ncopies,
		    dr_alignment_support alignment_support_scheme,
		    int misalignment,
		    bool add_realign_cost, unsigned int *inside_cost,
		    unsigned int *prologue_cost,
		    stmt_vector_for_cost *prologue_cost_vec,
		    stmt_vector_for_cost *body_cost_vec,
		    bool record_prologue_costs)
{
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
					  misalignment, vect_body);

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
vect_init_vector_1 (vec_info *vinfo, stmt_vec_info stmt_vinfo, gimple *new_stmt,
		    gimple_stmt_iterator *gsi)
{
  if (gsi)
    vect_finish_stmt_generation (vinfo, stmt_vinfo, new_stmt, gsi);
  else
    vinfo->insert_on_entry (stmt_vinfo, new_stmt);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created new init_stmt: %G", new_stmt);
}

/* Function vect_init_vector.

   Insert a new stmt (INIT_STMT) that initializes a new variable of type
   TYPE with the value VAL.  If TYPE is a vector type and VAL does not have
   vector type a vector with all elements equal to VAL is created first.
   Place the initialization at GSI if it is not NULL.  Otherwise, place the
   initialization at the loop preheader.
   Return the DEF of INIT_STMT.
   It will be used in the vectorization of STMT_INFO.  */

tree
vect_init_vector (vec_info *vinfo, stmt_vec_info stmt_info, tree val, tree type,
		  gimple_stmt_iterator *gsi)
{
  gimple *init_stmt;
  tree new_temp;

  /* We abuse this function to push sth to a SSA name with initial 'val'.  */
  if (! useless_type_conversion_p (type, TREE_TYPE (val)))
    {
      gcc_assert (VECTOR_TYPE_P (type));
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
		  vect_init_vector_1 (vinfo, stmt_info, init_stmt, gsi);
		  val = new_temp;
		}
	    }
	  else
	    {
	      gimple_seq stmts = NULL;
	      if (! INTEGRAL_TYPE_P (TREE_TYPE (val)))
		val = gimple_build (&stmts, VIEW_CONVERT_EXPR,
				    TREE_TYPE (type), val);
	      else
		/* ???  Condition vectorization expects us to do
		   promotion of invariant/external defs.  */
		val = gimple_convert (&stmts, TREE_TYPE (type), val);
	      for (gimple_stmt_iterator gsi2 = gsi_start (stmts);
		   !gsi_end_p (gsi2); )
		{
		  init_stmt = gsi_stmt (gsi2);
		  gsi_remove (&gsi2, false);
		  vect_init_vector_1 (vinfo, stmt_info, init_stmt, gsi);
		}
	    }
	}
      val = build_vector_from_val (type, val);
    }

  new_temp = vect_get_new_ssa_name (type, vect_simple_var, "cst_");
  init_stmt = gimple_build_assign (new_temp, val);
  vect_init_vector_1 (vinfo, stmt_info, init_stmt, gsi);
  return new_temp;
}


/* Function vect_get_vec_defs_for_operand.

   OP is an operand in STMT_VINFO.  This function returns a vector of
   NCOPIES defs that will be used in the vectorized stmts for STMT_VINFO.

   In the case that OP is an SSA_NAME which is defined in the loop, then
   STMT_VINFO_VEC_STMTS of the defining stmt holds the relevant defs.

   In case OP is an invariant or constant, a new stmt that creates a vector def
   needs to be introduced.  VECTYPE may be used to specify a required type for
   vector invariant.  */

void
vect_get_vec_defs_for_operand (vec_info *vinfo, stmt_vec_info stmt_vinfo,
			       unsigned ncopies,
			       tree op, vec<tree> *vec_oprnds, tree vectype)
{
  gimple *def_stmt;
  enum vect_def_type dt;
  bool is_simple_use;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "vect_get_vec_defs_for_operand: %T\n", op);

  stmt_vec_info def_stmt_info;
  is_simple_use = vect_is_simple_use (op, loop_vinfo, &dt,
				      &def_stmt_info, &def_stmt);
  gcc_assert (is_simple_use);
  if (def_stmt && dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "  def_stmt =  %G", def_stmt);

  vec_oprnds->create (ncopies);
  if (dt == vect_constant_def || dt == vect_external_def)
    {
      tree stmt_vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
      tree vector_type;

      if (vectype)
	vector_type = vectype;
      else if (VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (op))
	       && VECTOR_BOOLEAN_TYPE_P (stmt_vectype))
	vector_type = truth_type_for (stmt_vectype);
      else
	vector_type = get_vectype_for_scalar_type (loop_vinfo, TREE_TYPE (op));

      gcc_assert (vector_type);
      tree vop = vect_init_vector (vinfo, stmt_vinfo, op, vector_type, NULL);
      while (ncopies--)
	vec_oprnds->quick_push (vop);
    }
  else
    {
      def_stmt_info = vect_stmt_to_vectorize (def_stmt_info);
      gcc_assert (STMT_VINFO_VEC_STMTS (def_stmt_info).length () == ncopies);
      for (unsigned i = 0; i < ncopies; ++i)
	vec_oprnds->quick_push (gimple_get_lhs
				  (STMT_VINFO_VEC_STMTS (def_stmt_info)[i]));
    }
}


/* Get vectorized definitions for OP0 and OP1.  */

void
vect_get_vec_defs (vec_info *vinfo, stmt_vec_info stmt_info, slp_tree slp_node,
		   unsigned ncopies,
		   tree op0, tree vectype0, vec<tree> *vec_oprnds0,
		   tree op1, tree vectype1, vec<tree> *vec_oprnds1,
		   tree op2, tree vectype2, vec<tree> *vec_oprnds2,
		   tree op3, tree vectype3, vec<tree> *vec_oprnds3)
{
  if (slp_node)
    {
      if (op0)
	vect_get_slp_defs (SLP_TREE_CHILDREN (slp_node)[0], vec_oprnds0);
      if (op1)
	vect_get_slp_defs (SLP_TREE_CHILDREN (slp_node)[1], vec_oprnds1);
      if (op2)
	vect_get_slp_defs (SLP_TREE_CHILDREN (slp_node)[2], vec_oprnds2);
      if (op3)
	vect_get_slp_defs (SLP_TREE_CHILDREN (slp_node)[3], vec_oprnds3);
    }
  else
    {
      if (op0)
	vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies,
				       op0, vec_oprnds0, vectype0);
      if (op1)
	vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies,
				       op1, vec_oprnds1, vectype1);
      if (op2)
	vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies,
				       op2, vec_oprnds2, vectype2);
      if (op3)
	vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies,
				       op3, vec_oprnds3, vectype3);
    }
}

void
vect_get_vec_defs (vec_info *vinfo, stmt_vec_info stmt_info, slp_tree slp_node,
		   unsigned ncopies,
		   tree op0, vec<tree> *vec_oprnds0,
		   tree op1, vec<tree> *vec_oprnds1,
		   tree op2, vec<tree> *vec_oprnds2,
		   tree op3, vec<tree> *vec_oprnds3)
{
  vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		     op0, NULL_TREE, vec_oprnds0,
		     op1, NULL_TREE, vec_oprnds1,
		     op2, NULL_TREE, vec_oprnds2,
		     op3, NULL_TREE, vec_oprnds3);
}

/* Helper function called by vect_finish_replace_stmt and
   vect_finish_stmt_generation.  Set the location of the new
   statement and create and return a stmt_vec_info for it.  */

static void
vect_finish_stmt_generation_1 (vec_info *,
			       stmt_vec_info stmt_info, gimple *vec_stmt)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "add new stmt: %G", vec_stmt);

  if (stmt_info)
    {
      gimple_set_location (vec_stmt, gimple_location (stmt_info->stmt));

      /* While EH edges will generally prevent vectorization, stmt might
	 e.g. be in a must-not-throw region.  Ensure newly created stmts
	 that could throw are part of the same region.  */
      int lp_nr = lookup_stmt_eh_lp (stmt_info->stmt);
      if (lp_nr != 0 && stmt_could_throw_p (cfun, vec_stmt))
	add_stmt_to_eh_lp (vec_stmt, lp_nr);
    }
  else
    gcc_assert (!stmt_could_throw_p (cfun, vec_stmt));
}

/* Replace the scalar statement STMT_INFO with a new vector statement VEC_STMT,
   which sets the same scalar result as STMT_INFO did.  Create and return a
   stmt_vec_info for VEC_STMT.  */

void
vect_finish_replace_stmt (vec_info *vinfo,
			  stmt_vec_info stmt_info, gimple *vec_stmt)
{
  gimple *scalar_stmt = vect_orig_stmt (stmt_info)->stmt;
  gcc_assert (gimple_get_lhs (scalar_stmt) == gimple_get_lhs (vec_stmt));

  gimple_stmt_iterator gsi = gsi_for_stmt (scalar_stmt);
  gsi_replace (&gsi, vec_stmt, true);

  vect_finish_stmt_generation_1 (vinfo, stmt_info, vec_stmt);
}

/* Add VEC_STMT to the vectorized implementation of STMT_INFO and insert it
   before *GSI.  Create and return a stmt_vec_info for VEC_STMT.  */

void
vect_finish_stmt_generation (vec_info *vinfo,
			     stmt_vec_info stmt_info, gimple *vec_stmt,
			     gimple_stmt_iterator *gsi)
{
  gcc_assert (!stmt_info || gimple_code (stmt_info->stmt) != GIMPLE_LABEL);

  if (!gsi_end_p (*gsi)
      && gimple_has_mem_ops (vec_stmt))
    {
      gimple *at_stmt = gsi_stmt (*gsi);
      tree vuse = gimple_vuse (at_stmt);
      if (vuse && TREE_CODE (vuse) == SSA_NAME)
	{
	  tree vdef = gimple_vdef (at_stmt);
	  gimple_set_vuse (vec_stmt, gimple_vuse (at_stmt));
	  gimple_set_modified (vec_stmt, true);
	  /* If we have an SSA vuse and insert a store, update virtual
	     SSA form to avoid triggering the renamer.  Do so only
	     if we can easily see all uses - which is what almost always
	     happens with the way vectorized stmts are inserted.  */
	  if ((vdef && TREE_CODE (vdef) == SSA_NAME)
	      && ((is_gimple_assign (vec_stmt)
		   && !is_gimple_reg (gimple_assign_lhs (vec_stmt)))
		  || (is_gimple_call (vec_stmt)
		      && (!(gimple_call_flags (vec_stmt)
			    & (ECF_CONST|ECF_PURE|ECF_NOVOPS))
			  || (gimple_call_lhs (vec_stmt)
			      && !is_gimple_reg (gimple_call_lhs (vec_stmt)))))))
	    {
	      tree new_vdef = copy_ssa_name (vuse, vec_stmt);
	      gimple_set_vdef (vec_stmt, new_vdef);
	      SET_USE (gimple_vuse_op (at_stmt), new_vdef);
	    }
	}
    }
  gsi_insert_before (gsi, vec_stmt, GSI_SAME_STMT);
  vect_finish_stmt_generation_1 (vinfo, stmt_info, vec_stmt);
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
	  bool same_size_p = TYPE_SIZE (vectype_in) == TYPE_SIZE (vectype_out);
	  tree type0 = (info.type0 < 0 ? vectype_out : vectype_in);
	  tree type1 = (info.type1 < 0 ? vectype_out : vectype_in);

	  /* The type size of both the vectype_in and vectype_out should be
	     exactly the same when vectype_out isn't participating the optab.
	     While there is no restriction for type size when vectype_out
	     is part of the optab query.  */
	  if (type0 != vectype_out && type1 != vectype_out && !same_size_p)
	    return IFN_LAST;

	  if (direct_internal_fn_supported_p (ifn, tree_pair (type0, type1),
					      OPTIMIZE_FOR_SPEED))
	    return ifn;
	}
    }
  return IFN_LAST;
}


static tree permute_vec_elements (vec_info *, tree, tree, tree, stmt_vec_info,
				  gimple_stmt_iterator *);

/* Check whether a load or store statement in the loop described by
   LOOP_VINFO is possible in a loop using partial vectors.  This is
   testing whether the vectorizer pass has the appropriate support,
   as well as whether the target does.

   VLS_TYPE says whether the statement is a load or store and VECTYPE
   is the type of the vector being loaded or stored.  SLP_NODE is the SLP
   node that contains the statement, or null if none.  MEMORY_ACCESS_TYPE
   says how the load or store is going to be implemented and GROUP_SIZE
   is the number of load or store statements in the containing group.
   If the access is a gather load or scatter store, GS_INFO describes
   its arguments.  If the load or store is conditional, SCALAR_MASK is the
   condition under which it occurs.

   Clear LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P if a loop using partial
   vectors is not supported, otherwise record the required rgroup control
   types.  */

static void
check_load_store_for_partial_vectors (loop_vec_info loop_vinfo, tree vectype,
				      slp_tree slp_node,
				      vec_load_store_type vls_type,
				      int group_size,
				      vect_memory_access_type
				      memory_access_type,
				      gather_scatter_info *gs_info,
				      tree scalar_mask)
{
  /* Invariant loads need no special support.  */
  if (memory_access_type == VMAT_INVARIANT)
    return;

  unsigned int nvectors = vect_get_num_copies (loop_vinfo, slp_node, vectype);
  vec_loop_masks *masks = &LOOP_VINFO_MASKS (loop_vinfo);
  vec_loop_lens *lens = &LOOP_VINFO_LENS (loop_vinfo);
  machine_mode vecmode = TYPE_MODE (vectype);
  bool is_load = (vls_type == VLS_LOAD);
  if (memory_access_type == VMAT_LOAD_STORE_LANES)
    {
      if (slp_node)
	nvectors /= group_size;
      internal_fn ifn
	= (is_load ? vect_load_lanes_supported (vectype, group_size, true)
		   : vect_store_lanes_supported (vectype, group_size, true));
      if (ifn == IFN_MASK_LEN_LOAD_LANES || ifn == IFN_MASK_LEN_STORE_LANES)
	vect_record_loop_len (loop_vinfo, lens, nvectors, vectype, 1);
      else if (ifn == IFN_MASK_LOAD_LANES || ifn == IFN_MASK_STORE_LANES)
	vect_record_loop_mask (loop_vinfo, masks, nvectors, vectype,
			       scalar_mask);
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "can't operate on partial vectors because"
			     " the target doesn't have an appropriate"
			     " load/store-lanes instruction.\n");
	  LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
	}
      return;
    }

  if (memory_access_type == VMAT_GATHER_SCATTER)
    {
      internal_fn ifn = (is_load
			 ? IFN_MASK_GATHER_LOAD
			 : IFN_MASK_SCATTER_STORE);
      internal_fn len_ifn = (is_load
			     ? IFN_MASK_LEN_GATHER_LOAD
			     : IFN_MASK_LEN_SCATTER_STORE);
      if (internal_gather_scatter_fn_supported_p (len_ifn, vectype,
						  gs_info->memory_type,
						  gs_info->offset_vectype,
						  gs_info->scale))
	vect_record_loop_len (loop_vinfo, lens, nvectors, vectype, 1);
      else if (internal_gather_scatter_fn_supported_p (ifn, vectype,
						       gs_info->memory_type,
						       gs_info->offset_vectype,
						       gs_info->scale))
	vect_record_loop_mask (loop_vinfo, masks, nvectors, vectype,
			       scalar_mask);
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "can't operate on partial vectors because"
			     " the target doesn't have an appropriate"
			     " gather load or scatter store instruction.\n");
	  LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
	}
      return;
    }

  if (memory_access_type != VMAT_CONTIGUOUS
      && memory_access_type != VMAT_CONTIGUOUS_PERMUTE)
    {
      /* Element X of the data must come from iteration i * VF + X of the
	 scalar loop.  We need more work to support other mappings.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't operate on partial vectors because an"
			 " access isn't contiguous.\n");
      LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
      return;
    }

  if (!VECTOR_MODE_P (vecmode))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't operate on partial vectors when emulating"
			 " vector operations.\n");
      LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
      return;
    }

  /* We might load more scalars than we need for permuting SLP loads.
     We checked in get_group_load_store_type that the extra elements
     don't leak into a new vector.  */
  auto group_memory_nvectors = [](poly_uint64 size, poly_uint64 nunits)
  {
    unsigned int nvectors;
    if (can_div_away_from_zero_p (size, nunits, &nvectors))
      return nvectors;
    gcc_unreachable ();
  };

  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  machine_mode mask_mode;
  machine_mode vmode;
  bool using_partial_vectors_p = false;
  if (get_len_load_store_mode (vecmode, is_load).exists (&vmode))
    {
      nvectors = group_memory_nvectors (group_size * vf, nunits);
      unsigned factor = (vecmode == vmode) ? 1 : GET_MODE_UNIT_SIZE (vecmode);
      vect_record_loop_len (loop_vinfo, lens, nvectors, vectype, factor);
      using_partial_vectors_p = true;
    }
  else if (targetm.vectorize.get_mask_mode (vecmode).exists (&mask_mode)
	   && can_vec_mask_load_store_p (vecmode, mask_mode, is_load))
    {
      nvectors = group_memory_nvectors (group_size * vf, nunits);
      vect_record_loop_mask (loop_vinfo, masks, nvectors, vectype, scalar_mask);
      using_partial_vectors_p = true;
    }

  if (!using_partial_vectors_p)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't operate on partial vectors because the"
			 " target doesn't have the appropriate partial"
			 " vectorization load or store.\n");
      LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
    }
}

/* Return the mask input to a masked load or store.  VEC_MASK is the vectorized
   form of the scalar mask condition and LOOP_MASK, if nonnull, is the mask
   that needs to be applied to all loads and stores in a vectorized loop.
   Return VEC_MASK if LOOP_MASK is null or if VEC_MASK is already masked,
   otherwise return VEC_MASK & LOOP_MASK.

   MASK_TYPE is the type of both masks.  If new statements are needed,
   insert them before GSI.  */

tree
prepare_vec_mask (loop_vec_info loop_vinfo, tree mask_type, tree loop_mask,
		  tree vec_mask, gimple_stmt_iterator *gsi)
{
  gcc_assert (useless_type_conversion_p (mask_type, TREE_TYPE (vec_mask)));
  if (!loop_mask)
    return vec_mask;

  gcc_assert (TREE_TYPE (loop_mask) == mask_type);

  if (loop_vinfo->vec_cond_masked_set.contains ({ vec_mask, loop_mask }))
    return vec_mask;

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
  class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  widest_int max_iters;
  if (max_loop_iterations (loop, &max_iters)
      && max_iters < count)
    count = max_iters.to_shwi ();

  /* Try scales of 1 and the element size.  */
  unsigned int scales[] = { 1, vect_get_scalar_dr_size (dr_info) };
  wi::overflow_type overflow = wi::OVF_NONE;
  for (int i = 0; i < 2; ++i)
    {
      unsigned int scale = scales[i];
      widest_int factor;
      if (!wi::multiple_of_p (wi::to_widest (step), scale, SIGNED, &factor))
	continue;

      /* Determine the minimum precision of (COUNT - 1) * STEP / SCALE.  */
      widest_int range = wi::mul (count, factor, SIGNED, &overflow);
      if (overflow)
	continue;
      signop sign = range >= 0 ? UNSIGNED : SIGNED;
      unsigned int min_offset_bits = wi::min_precision (range, sign);

      /* Find the narrowest viable offset type.  */
      unsigned int offset_bits = 1U << ceil_log2 (min_offset_bits);
      tree offset_type = build_nonstandard_integer_type (offset_bits,
							 sign == UNSIGNED);

      /* See whether the target supports the operation with an offset
	 no narrower than OFFSET_TYPE.  */
      tree memory_type = TREE_TYPE (DR_REF (dr));
      if (!vect_gather_scatter_fn_p (loop_vinfo, DR_IS_READ (dr), masked_p,
				     vectype, memory_type, offset_type, scale,
				     &gs_info->ifn, &gs_info->offset_vectype)
	  || gs_info->ifn == IFN_LAST)
	continue;

      gs_info->decl = NULL_TREE;
      /* Logically the sum of DR_BASE_ADDRESS, DR_INIT and DR_OFFSET,
	 but we don't need to store that here.  */
      gs_info->base = NULL_TREE;
      gs_info->element_type = TREE_TYPE (vectype);
      gs_info->offset = fold_convert (offset_type, step);
      gs_info->offset_dt = vect_constant_def;
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
      || gs_info->ifn == IFN_LAST)
    return vect_truncate_gather_scatter_offset (stmt_info, loop_vinfo,
						masked_p, gs_info);

  tree old_offset_type = TREE_TYPE (gs_info->offset);
  tree new_offset_type = TREE_TYPE (gs_info->offset_vectype);

  gcc_assert (TYPE_PRECISION (new_offset_type)
	      >= TYPE_PRECISION (old_offset_type));
  gs_info->offset = fold_convert (new_offset_type, gs_info->offset);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "using gather/scatter for strided/grouped access,"
		     " scale = %d\n", gs_info->scale);

  return true;
}

/* STMT_INFO is a non-strided load or store, meaning that it accesses
   elements with a known constant step.  Return -1 if that step
   is negative, 0 if it is zero, and 1 if it is greater than zero.  */

int
compare_step_with_zero (vec_info *vinfo, stmt_vec_info stmt_info)
{
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  return tree_int_cst_compare (vect_dr_behavior (vinfo, dr_info)->step,
			       size_zero_node);
}

/* If the target supports a permute mask that reverses the elements in
   a vector of type VECTYPE, return that mask, otherwise return null.  */

tree
perm_mask_for_reverse (tree vectype)
{
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* The encoding has a single stepped pattern.  */
  vec_perm_builder sel (nunits, 1, 3);
  for (int i = 0; i < 3; ++i)
    sel.quick_push (nunits - 1 - i);

  vec_perm_indices indices (sel, 1, nunits);
  if (!can_vec_perm_const_p (TYPE_MODE (vectype), TYPE_MODE (vectype),
			     indices))
    return NULL_TREE;
  return vect_gen_perm_mask_checked (vectype, indices);
}

/* A subroutine of get_load_store_type, with a subset of the same
   arguments.  Handle the case where STMT_INFO is a load or store that
   accesses consecutive elements with a negative step.  Sets *POFFSET
   to the offset to be applied to the DR for the first access.  */

static vect_memory_access_type
get_negative_load_store_type (vec_info *vinfo,
			      stmt_vec_info stmt_info, tree vectype,
			      vec_load_store_type vls_type,
			      unsigned int ncopies, poly_int64 *poffset)
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

  /* For backward running DRs the first access in vectype actually is
     N-1 elements before the address of the DR.  */
  *poffset = ((-TYPE_VECTOR_SUBPARTS (vectype) + 1)
	      * TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (vectype))));

  int misalignment = dr_misalignment (dr_info, vectype, *poffset);
  alignment_support_scheme
    = vect_supportable_dr_alignment (vinfo, dr_info, vectype, misalignment);
  if (alignment_support_scheme != dr_aligned
      && alignment_support_scheme != dr_unaligned_supported)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "negative step but alignment required.\n");
      *poffset = 0;
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
      *poffset = 0;
      return VMAT_ELEMENTWISE;
    }

  return VMAT_CONTIGUOUS_REVERSE;
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

/* Function VECTOR_VECTOR_COMPOSITION_TYPE

   This function returns a vector type which can be composed with NETLS pieces,
   whose type is recorded in PTYPE.  VTYPE should be a vector type, and has the
   same vector size as the return vector.  It checks target whether supports
   pieces-size vector mode for construction firstly, if target fails to, check
   pieces-size scalar mode for construction further.  It returns NULL_TREE if
   fails to find the available composition.

   For example, for (vtype=V16QI, nelts=4), we can probably get:
     - V16QI with PTYPE V4QI.
     - V4SI with PTYPE SI.
     - NULL_TREE.  */

static tree
vector_vector_composition_type (tree vtype, poly_uint64 nelts, tree *ptype)
{
  gcc_assert (VECTOR_TYPE_P (vtype));
  gcc_assert (known_gt (nelts, 0U));

  machine_mode vmode = TYPE_MODE (vtype);
  if (!VECTOR_MODE_P (vmode))
    return NULL_TREE;

  /* When we are asked to compose the vector from its components let
     that happen directly.  */
  if (known_eq (TYPE_VECTOR_SUBPARTS (vtype), nelts))
    {
      *ptype = TREE_TYPE (vtype);
      return vtype;
    }

  poly_uint64 vbsize = GET_MODE_BITSIZE (vmode);
  unsigned int pbsize;
  if (constant_multiple_p (vbsize, nelts, &pbsize))
    {
      /* First check if vec_init optab supports construction from
	 vector pieces directly.  */
      scalar_mode elmode = SCALAR_TYPE_MODE (TREE_TYPE (vtype));
      poly_uint64 inelts = pbsize / GET_MODE_BITSIZE (elmode);
      machine_mode rmode;
      if (related_vector_mode (vmode, elmode, inelts).exists (&rmode)
	  && (convert_optab_handler (vec_init_optab, vmode, rmode)
	      != CODE_FOR_nothing))
	{
	  *ptype = build_vector_type (TREE_TYPE (vtype), inelts);
	  return vtype;
	}

      /* Otherwise check if exists an integer type of the same piece size and
	 if vec_init optab supports construction from it directly.  */
      if (int_mode_for_size (pbsize, 0).exists (&elmode)
	  && related_vector_mode (vmode, elmode, nelts).exists (&rmode)
	  && (convert_optab_handler (vec_init_optab, rmode, elmode)
	      != CODE_FOR_nothing))
	{
	  *ptype = build_nonstandard_integer_type (pbsize, 1);
	  return build_vector_type (*ptype, nelts);
	}
    }

  return NULL_TREE;
}

/* A subroutine of get_load_store_type, with a subset of the same
   arguments.  Handle the case where STMT_INFO is part of a grouped load
   or store.

   For stores, the statements in the group are all consecutive
   and there is no gap at the end.  For loads, the statements in the
   group might not be consecutive; there can be gaps between statements
   as well as at the end.  */

static bool
get_group_load_store_type (vec_info *vinfo, stmt_vec_info stmt_info,
			   tree vectype, slp_tree slp_node,
			   bool masked_p, vec_load_store_type vls_type,
			   vect_memory_access_type *memory_access_type,
			   poly_int64 *poffset,
			   dr_alignment_support *alignment_support_scheme,
			   int *misalignment,
			   gather_scatter_info *gs_info,
			   internal_fn *lanes_ifn)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  class loop *loop = loop_vinfo ? LOOP_VINFO_LOOP (loop_vinfo) : NULL;
  stmt_vec_info first_stmt_info;
  unsigned int group_size;
  unsigned HOST_WIDE_INT gap;
  bool single_element_p;
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      group_size = DR_GROUP_SIZE (first_stmt_info);
      gap = DR_GROUP_GAP (first_stmt_info);
      single_element_p = (stmt_info == first_stmt_info
			  && !DR_GROUP_NEXT_ELEMENT (stmt_info));
    }
  else
    {
      first_stmt_info = stmt_info;
      group_size = 1;
      gap = 0;
      single_element_p = true;
    }
  dr_vec_info *first_dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
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
  gcc_assert (slp_node || vls_type == VLS_LOAD || gap == 0);

  if (slp_node)
    {
      /* For SLP vectorization we directly vectorize a subchain
	 without permutation.  */
      if (! SLP_TREE_LOAD_PERMUTATION (slp_node).exists ())
	first_dr_info
	  = STMT_VINFO_DR_INFO (SLP_TREE_SCALAR_STMTS (slp_node)[0]);
      if (STMT_VINFO_STRIDED_P (first_stmt_info))
	/* Try to use consecutive accesses of as many elements as possible,
	   separated by the stride, until we have a complete vector.
	   Fall back to scalar accesses if that isn't possible.  */
	*memory_access_type = VMAT_STRIDED_SLP;
      else
	{
	  int cmp = compare_step_with_zero (vinfo, stmt_info);
	  if (cmp < 0)
	    {
	      if (single_element_p)
		/* ???  The VMAT_CONTIGUOUS_REVERSE code generation is
		   only correct for single element "interleaving" SLP.  */
		*memory_access_type = get_negative_load_store_type
			     (vinfo, stmt_info, vectype, vls_type, 1, poffset);
	      else
		{
		  /* Try to use consecutive accesses of DR_GROUP_SIZE elements,
		     separated by the stride, until we have a complete vector.
		     Fall back to scalar accesses if that isn't possible.  */
		  if (multiple_p (nunits, group_size))
		    *memory_access_type = VMAT_STRIDED_SLP;
		  else
		    *memory_access_type = VMAT_ELEMENTWISE;
		}
	    }
	  else if (cmp == 0 && loop_vinfo)
	    {
	      gcc_assert (vls_type == VLS_LOAD);
	      *memory_access_type = VMAT_INVARIANT;
	      /* Invariant accesses perform only component accesses, alignment
		 is irrelevant for them.  */
	      *alignment_support_scheme = dr_unaligned_supported;
	    }
	  /* Try using LOAD/STORE_LANES.  */
	  else if (slp_node->ldst_lanes
		   && (*lanes_ifn
			 = (vls_type == VLS_LOAD
			    ? vect_load_lanes_supported (vectype, group_size, masked_p)
			    : vect_store_lanes_supported (vectype, group_size,
							  masked_p))) != IFN_LAST)
	    *memory_access_type = VMAT_LOAD_STORE_LANES;
	  else
	    *memory_access_type = VMAT_CONTIGUOUS;

	  /* If this is single-element interleaving with an element
	     distance that leaves unused vector loads around punt - we
	     at least create very sub-optimal code in that case (and
	     blow up memory, see PR65518).  */
	  if (loop_vinfo
	      && *memory_access_type == VMAT_CONTIGUOUS
	      && single_element_p
	      && maybe_gt (group_size, TYPE_VECTOR_SUBPARTS (vectype)))
	    {
	      if (SLP_TREE_LANES (slp_node) == 1)
		{
		  *memory_access_type = VMAT_ELEMENTWISE;
		  overrun_p = false;
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "single-element interleaving not supported "
				     "for not adjacent vector loads, using "
				     "elementwise access\n");
		}
	      else
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "single-element interleaving not supported "
				     "for not adjacent vector loads\n");
		  return false;
		}
	    }

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
	      && gap < (vect_known_alignment_in_bytes (first_dr_info,
						       vectype)
			/ vect_get_scalar_dr_size (first_dr_info)))
	    overrun_p = false;

	  /* When we have a contiguous access across loop iterations
	     but the access in the loop doesn't cover the full vector
	     we can end up with no gap recorded but still excess
	     elements accessed, see PR103116.  Make sure we peel for
	     gaps if necessary and sufficient and give up if not.

	     If there is a combination of the access not covering the full
	     vector and a gap recorded then we may need to peel twice.  */
	  if (loop_vinfo
	      && (*memory_access_type == VMAT_CONTIGUOUS
		  || *memory_access_type == VMAT_CONTIGUOUS_REVERSE)
	      && SLP_TREE_LOAD_PERMUTATION (slp_node).exists ()
	      && !multiple_p (group_size * LOOP_VINFO_VECT_FACTOR (loop_vinfo),
			      nunits))
	    overrun_p = true;

	  /* If the gap splits the vector in half and the target
	     can do half-vector operations avoid the epilogue peeling
	     by simply loading half of the vector only.  Usually
	     the construction with an upper zero half will be elided.  */
	  dr_alignment_support alss;
	  int misalign = dr_misalignment (first_dr_info, vectype);
	  tree half_vtype;
	  poly_uint64 remain;
	  unsigned HOST_WIDE_INT tem, num;
	  if (overrun_p
	      && !masked_p
	      && *memory_access_type != VMAT_LOAD_STORE_LANES
	      && (((alss = vect_supportable_dr_alignment (vinfo, first_dr_info,
							  vectype, misalign)))
		   == dr_aligned
		  || alss == dr_unaligned_supported)
	      && can_div_trunc_p (group_size
				  * LOOP_VINFO_VECT_FACTOR (loop_vinfo) - gap,
				  nunits, &tem, &remain)
	      && (known_eq (remain, 0u)
		  || (known_ne (remain, 0u)
		      && constant_multiple_p (nunits, remain, &num)
		      && (vector_vector_composition_type (vectype, num,
							  &half_vtype)
			  != NULL_TREE))))
	    overrun_p = false;

	  if (overrun_p && !can_overrun_p)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Peeling for outer loop is not supported\n");
	      return false;
	    }

	  /* Peeling for gaps assumes that a single scalar iteration
	     is enough to make sure the last vector iteration doesn't
	     access excess elements.  */
	  if (overrun_p
	      && (!can_div_trunc_p (group_size
				    * LOOP_VINFO_VECT_FACTOR (loop_vinfo) - gap,
				    nunits, &tem, &remain)
		  || maybe_lt (remain + group_size, nunits)))
	    {
	      /* But peeling a single scalar iteration is enough if
		 we can use the next power-of-two sized partial
		 access and that is sufficiently small to be covered
		 by the single scalar iteration.  */
	      unsigned HOST_WIDE_INT cnunits, cvf, cremain, cpart_size;
	      if (!nunits.is_constant (&cnunits)
		  || !LOOP_VINFO_VECT_FACTOR (loop_vinfo).is_constant (&cvf)
		  || (((cremain = group_size * cvf - gap % cnunits), true)
		      && ((cpart_size = (1 << ceil_log2 (cremain))) != cnunits)
		      && (cremain + group_size < cpart_size
			  || vector_vector_composition_type
			       (vectype, cnunits / cpart_size,
				&half_vtype) == NULL_TREE)))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "peeling for gaps insufficient for "
				     "access\n");
		  return false;
		}
	    }
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
	  && gap < (vect_known_alignment_in_bytes (first_dr_info, vectype)
		    / vect_get_scalar_dr_size (first_dr_info)))
	would_overrun_p = false;

      if (!STMT_VINFO_STRIDED_P (first_stmt_info)
	  && (can_overrun_p || !would_overrun_p)
	  && compare_step_with_zero (vinfo, stmt_info) > 0)
	{
	  /* First cope with the degenerate case of a single-element
	     vector.  */
	  if (known_eq (TYPE_VECTOR_SUBPARTS (vectype), 1U))
	    ;

	  else
	    {
	      /* Otherwise try using LOAD/STORE_LANES.  */
	      *lanes_ifn
		= vls_type == VLS_LOAD
		    ? vect_load_lanes_supported (vectype, group_size, masked_p)
		    : vect_store_lanes_supported (vectype, group_size,
						  masked_p);
	      if (*lanes_ifn != IFN_LAST)
		{
		  *memory_access_type = VMAT_LOAD_STORE_LANES;
		  overrun_p = would_overrun_p;
		}

	      /* If that fails, try using permuting loads.  */
	      else if (vls_type == VLS_LOAD
			 ? vect_grouped_load_supported (vectype,
							single_element_p,
							group_size)
			 : vect_grouped_store_supported (vectype, group_size))
		{
		  *memory_access_type = VMAT_CONTIGUOUS_PERMUTE;
		  overrun_p = would_overrun_p;
		}
	    }
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

  if (*memory_access_type == VMAT_GATHER_SCATTER
      || *memory_access_type == VMAT_ELEMENTWISE)
    {
      *alignment_support_scheme = dr_unaligned_supported;
      *misalignment = DR_MISALIGNMENT_UNKNOWN;
    }
  else
    {
      *misalignment = dr_misalignment (first_dr_info, vectype, *poffset);
      *alignment_support_scheme
	= vect_supportable_dr_alignment (vinfo, first_dr_info, vectype,
					 *misalignment);
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

/* Analyze load or store statement STMT_INFO of type VLS_TYPE.  Return true
   if there is a memory access type that the vectorized form can use,
   storing it in *MEMORY_ACCESS_TYPE if so.  If we decide to use gathers
   or scatters, fill in GS_INFO accordingly.  In addition
   *ALIGNMENT_SUPPORT_SCHEME is filled out and false is returned if
   the target does not support the alignment scheme.  *MISALIGNMENT
   is set according to the alignment of the access (including
   DR_MISALIGNMENT_UNKNOWN when it is unknown).

   SLP says whether we're performing SLP rather than loop vectorization.
   MASKED_P is true if the statement is conditional on a vectorized mask.
   VECTYPE is the vector type that the vectorized statements will use.
   NCOPIES is the number of vector statements that will be needed.  */

static bool
get_load_store_type (vec_info  *vinfo, stmt_vec_info stmt_info,
		     tree vectype, slp_tree slp_node,
		     bool masked_p, vec_load_store_type vls_type,
		     unsigned int ncopies,
		     vect_memory_access_type *memory_access_type,
		     poly_int64 *poffset,
		     dr_alignment_support *alignment_support_scheme,
		     int *misalignment,
		     gather_scatter_info *gs_info,
		     internal_fn *lanes_ifn)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  *misalignment = DR_MISALIGNMENT_UNKNOWN;
  *poffset = 0;
  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    {
      *memory_access_type = VMAT_GATHER_SCATTER;
      if (!vect_check_gather_scatter (stmt_info, loop_vinfo, gs_info))
	gcc_unreachable ();
      /* When using internal functions, we rely on pattern recognition
	 to convert the type of the offset to the type that the target
	 requires, with the result being a call to an internal function.
	 If that failed for some reason (e.g. because another pattern
	 took priority), just handle cases in which the offset already
	 has the right type.  */
      else if (gs_info->ifn != IFN_LAST
	       && !is_gimple_call (stmt_info->stmt)
	       && !tree_nop_conversion_p (TREE_TYPE (gs_info->offset),
					  TREE_TYPE (gs_info->offset_vectype)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "%s offset requires a conversion\n",
			     vls_type == VLS_LOAD ? "gather" : "scatter");
	  return false;
	}
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
      else if (gs_info->ifn == IFN_LAST && !gs_info->decl)
	{
	  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant ()
	      || !TYPE_VECTOR_SUBPARTS (gs_info->offset_vectype).is_constant ()
	      || !constant_multiple_p (TYPE_VECTOR_SUBPARTS
					 (gs_info->offset_vectype),
				       TYPE_VECTOR_SUBPARTS (vectype)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "unsupported vector types for emulated "
				 "gather.\n");
	      return false;
	    }
	}
      /* Gather-scatter accesses perform only component accesses, alignment
	 is irrelevant for them.  */
      *alignment_support_scheme = dr_unaligned_supported;
    }
  else if (STMT_VINFO_GROUPED_ACCESS (stmt_info) || slp_node)
    {
      if (!get_group_load_store_type (vinfo, stmt_info, vectype, slp_node,
				      masked_p,
				      vls_type, memory_access_type, poffset,
				      alignment_support_scheme,
				      misalignment, gs_info, lanes_ifn))
	return false;
    }
  else if (STMT_VINFO_STRIDED_P (stmt_info))
    {
      gcc_assert (!slp_node);
      if (loop_vinfo
	  && vect_use_strided_gather_scatters_p (stmt_info, loop_vinfo,
						 masked_p, gs_info))
	*memory_access_type = VMAT_GATHER_SCATTER;
      else
	*memory_access_type = VMAT_ELEMENTWISE;
      /* Alignment is irrelevant here.  */
      *alignment_support_scheme = dr_unaligned_supported;
    }
  else
    {
      int cmp = compare_step_with_zero (vinfo, stmt_info);
      if (cmp == 0)
	{
	  gcc_assert (vls_type == VLS_LOAD);
	  *memory_access_type = VMAT_INVARIANT;
	  /* Invariant accesses perform only component accesses, alignment
	     is irrelevant for them.  */
	  *alignment_support_scheme = dr_unaligned_supported;
	}
      else
	{
	  if (cmp < 0)
	    *memory_access_type = get_negative_load_store_type
	       (vinfo, stmt_info, vectype, vls_type, ncopies, poffset);
	  else
	    *memory_access_type = VMAT_CONTIGUOUS;
	  *misalignment = dr_misalignment (STMT_VINFO_DR_INFO (stmt_info),
					   vectype, *poffset);
	  *alignment_support_scheme
	    = vect_supportable_dr_alignment (vinfo,
					     STMT_VINFO_DR_INFO (stmt_info),
					     vectype, *misalignment);
	}
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

  if (*alignment_support_scheme == dr_unaligned_unsupported)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "unsupported unaligned access\n");
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

/* Return true if boolean argument at MASK_INDEX is suitable for vectorizing
   conditional operation STMT_INFO.  When returning true, store the mask
   in *MASK, the type of its definition in *MASK_DT_OUT, the type of the
   vectorized mask in *MASK_VECTYPE_OUT and the SLP node corresponding
   to the mask in *MASK_NODE if MASK_NODE is not NULL.  */

static bool
vect_check_scalar_mask (vec_info *vinfo, stmt_vec_info stmt_info,
			slp_tree slp_node, unsigned mask_index,
			tree *mask, slp_tree *mask_node,
			vect_def_type *mask_dt_out, tree *mask_vectype_out)
{
  enum vect_def_type mask_dt;
  tree mask_vectype;
  slp_tree mask_node_1;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node, mask_index,
			   mask, &mask_node_1, &mask_dt, &mask_vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mask use not simple.\n");
      return false;
    }

  if ((mask_dt == vect_constant_def || mask_dt == vect_external_def)
      && !VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (*mask)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mask argument is not a boolean.\n");
      return false;
    }

  /* If the caller is not prepared for adjusting an external/constant
     SLP mask vector type fail.  */
  if (slp_node
      && !mask_node
      && SLP_TREE_DEF_TYPE (mask_node_1) != vect_internal_def)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "SLP mask argument is not vectorized.\n");
      return false;
    }

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (!mask_vectype)
    mask_vectype = get_mask_type_for_scalar_type (vinfo, TREE_TYPE (vectype),
						  mask_node_1);

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
			 "vector mask type %T"
			 " does not match vector data type %T.\n",
			 mask_vectype, vectype);

      return false;
    }

  *mask_dt_out = mask_dt;
  *mask_vectype_out = mask_vectype;
  if (mask_node)
    *mask_node = mask_node_1;
  return true;
}

/* Return true if stored value is suitable for vectorizing store
   statement STMT_INFO.  When returning true, store the scalar stored
   in *RHS and *RHS_NODE, the type of the definition in *RHS_DT_OUT,
   the type of the vectorized store value in
   *RHS_VECTYPE_OUT and the type of the store in *VLS_TYPE_OUT.  */

static bool
vect_check_store_rhs (vec_info *vinfo, stmt_vec_info stmt_info,
		      slp_tree slp_node, tree *rhs, slp_tree *rhs_node,
		      vect_def_type *rhs_dt_out, tree *rhs_vectype_out,
		      vec_load_store_type *vls_type_out)
{
  int op_no = 0;
  if (gcall *call = dyn_cast <gcall *> (stmt_info->stmt))
    {
      if (gimple_call_internal_p (call)
	  && internal_store_fn_p (gimple_call_internal_fn (call)))
	op_no = internal_fn_stored_value_index (gimple_call_internal_fn (call));
    }
  if (slp_node)
    op_no = vect_slp_child_index_for_operand
	      (stmt_info->stmt, op_no, STMT_VINFO_GATHER_SCATTER_P (stmt_info));

  enum vect_def_type rhs_dt;
  tree rhs_vectype;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node, op_no,
			   rhs, rhs_node, &rhs_dt, &rhs_vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "use not simple.\n");
      return false;
    }

  /* In the case this is a store from a constant make sure
     native_encode_expr can handle it.  */
  if (rhs_dt == vect_constant_def
      && CONSTANT_CLASS_P (*rhs) && native_encode_expr (*rhs, NULL, 64) == 0)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "cannot encode constant as a byte sequence.\n");
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
vect_build_all_ones_mask (vec_info *vinfo,
			  stmt_vec_info stmt_info, tree masktype)
{
  if (TREE_CODE (masktype) == INTEGER_TYPE)
    return build_int_cst (masktype, -1);
  else if (VECTOR_BOOLEAN_TYPE_P (masktype)
	   || TREE_CODE (TREE_TYPE (masktype)) == INTEGER_TYPE)
    {
      tree mask = build_int_cst (TREE_TYPE (masktype), -1);
      mask = build_vector_from_val (masktype, mask);
      return vect_init_vector (vinfo, stmt_info, mask, masktype, NULL);
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
      return vect_init_vector (vinfo, stmt_info, mask, masktype, NULL);
    }
  gcc_unreachable ();
}

/* Build an all-zero merge value of type VECTYPE while vectorizing
   STMT_INFO as a gather load.  */

static tree
vect_build_zero_merge_argument (vec_info *vinfo,
				stmt_vec_info stmt_info, tree vectype)
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
  return vect_init_vector (vinfo, stmt_info, merge, vectype, NULL);
}

/* Build a gather load call while vectorizing STMT_INFO.  Insert new
   instructions before GSI and add them to VEC_STMT.  GS_INFO describes
   the gather load operation.  If the load is conditional, MASK is the
   vectorized condition, otherwise MASK is null.  PTR is the base
   pointer and OFFSET is the vectorized offset.  */

static gimple *
vect_build_one_gather_load_call (vec_info *vinfo, stmt_vec_info stmt_info,
				 gimple_stmt_iterator *gsi,
				 gather_scatter_info *gs_info,
				 tree ptr, tree offset, tree mask)
{
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree arglist = TYPE_ARG_TYPES (TREE_TYPE (gs_info->decl));
  tree rettype = TREE_TYPE (TREE_TYPE (gs_info->decl));
  tree srctype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  /* ptrtype */ arglist = TREE_CHAIN (arglist);
  tree idxtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree masktype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree scaletype = TREE_VALUE (arglist);
  tree var;
  gcc_checking_assert (types_compatible_p (srctype, rettype)
		       && (!mask
			   || TREE_CODE (masktype) == INTEGER_TYPE
			   || types_compatible_p (srctype, masktype)));

  tree op = offset;
  if (!useless_type_conversion_p (idxtype, TREE_TYPE (op)))
    {
      gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (op)),
			    TYPE_VECTOR_SUBPARTS (idxtype)));
      var = vect_get_new_ssa_name (idxtype, vect_simple_var);
      op = build1 (VIEW_CONVERT_EXPR, idxtype, op);
      gassign *new_stmt = gimple_build_assign (var, VIEW_CONVERT_EXPR, op);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      op = var;
    }

  tree src_op = NULL_TREE;
  tree mask_op = NULL_TREE;
  if (mask)
    {
      if (!useless_type_conversion_p (masktype, TREE_TYPE (mask)))
	{
	  tree utype, optype = TREE_TYPE (mask);
	  if (VECTOR_TYPE_P (masktype)
	      || TYPE_MODE (masktype) == TYPE_MODE (optype))
	    utype = masktype;
	  else
	    utype = lang_hooks.types.type_for_mode (TYPE_MODE (optype), 1);
	  var = vect_get_new_ssa_name (utype, vect_scalar_var);
	  tree mask_arg = build1 (VIEW_CONVERT_EXPR, utype, mask);
	  gassign *new_stmt
	      = gimple_build_assign (var, VIEW_CONVERT_EXPR, mask_arg);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	  mask_arg = var;
	  if (!useless_type_conversion_p (masktype, utype))
	    {
	      gcc_assert (TYPE_PRECISION (utype)
			  <= TYPE_PRECISION (masktype));
	      var = vect_get_new_ssa_name (masktype, vect_scalar_var);
	      new_stmt = gimple_build_assign (var, NOP_EXPR, mask_arg);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      mask_arg = var;
	    }
	  src_op = build_zero_cst (srctype);
	  mask_op = mask_arg;
	}
      else
	{
	  src_op = mask;
	  mask_op = mask;
	}
    }
  else
    {
      src_op = vect_build_zero_merge_argument (vinfo, stmt_info, rettype);
      mask_op = vect_build_all_ones_mask (vinfo, stmt_info, masktype);
    }

  tree scale = build_int_cst (scaletype, gs_info->scale);
  gimple *new_stmt = gimple_build_call (gs_info->decl, 5, src_op, ptr, op,
					mask_op, scale);

  if (!useless_type_conversion_p (vectype, rettype))
    {
      gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (vectype),
			    TYPE_VECTOR_SUBPARTS (rettype)));
      op = vect_get_new_ssa_name (rettype, vect_simple_var);
      gimple_call_set_lhs (new_stmt, op);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      op = build1 (VIEW_CONVERT_EXPR, vectype, op);
      new_stmt = gimple_build_assign (NULL_TREE, VIEW_CONVERT_EXPR, op);
    }

  return new_stmt;
}

/* Build a scatter store call while vectorizing STMT_INFO.  Insert new
   instructions before GSI.  GS_INFO describes the scatter store operation.
   PTR is the base pointer, OFFSET the vectorized offsets and OPRND the
   vectorized data to store.
   If the store is conditional, MASK is the vectorized condition, otherwise
   MASK is null.  */

static gimple *
vect_build_one_scatter_store_call (vec_info *vinfo, stmt_vec_info stmt_info,
				   gimple_stmt_iterator *gsi,
				   gather_scatter_info *gs_info,
				   tree ptr, tree offset, tree oprnd, tree mask)
{
  tree rettype = TREE_TYPE (TREE_TYPE (gs_info->decl));
  tree arglist = TYPE_ARG_TYPES (TREE_TYPE (gs_info->decl));
  /* tree ptrtype = TREE_VALUE (arglist); */ arglist = TREE_CHAIN (arglist);
  tree masktype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree idxtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree srctype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
  tree scaletype = TREE_VALUE (arglist);
  gcc_checking_assert (TREE_CODE (masktype) == INTEGER_TYPE
		       && TREE_CODE (rettype) == VOID_TYPE);

  tree mask_arg = NULL_TREE;
  if (mask)
    {
      mask_arg = mask;
      tree optype = TREE_TYPE (mask_arg);
      tree utype;
      if (TYPE_MODE (masktype) == TYPE_MODE (optype))
	utype = masktype;
      else
	utype = lang_hooks.types.type_for_mode (TYPE_MODE (optype), 1);
      tree var = vect_get_new_ssa_name (utype, vect_scalar_var);
      mask_arg = build1 (VIEW_CONVERT_EXPR, utype, mask_arg);
      gassign *new_stmt
	= gimple_build_assign (var, VIEW_CONVERT_EXPR, mask_arg);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      mask_arg = var;
      if (!useless_type_conversion_p (masktype, utype))
	{
	  gcc_assert (TYPE_PRECISION (utype) <= TYPE_PRECISION (masktype));
	  tree var = vect_get_new_ssa_name (masktype, vect_scalar_var);
	  new_stmt = gimple_build_assign (var, NOP_EXPR, mask_arg);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	  mask_arg = var;
	}
    }
  else
    {
      mask_arg = build_int_cst (masktype, -1);
      mask_arg = vect_init_vector (vinfo, stmt_info, mask_arg, masktype, NULL);
    }

  tree src = oprnd;
  if (!useless_type_conversion_p (srctype, TREE_TYPE (src)))
    {
      gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (src)),
			    TYPE_VECTOR_SUBPARTS (srctype)));
      tree var = vect_get_new_ssa_name (srctype, vect_simple_var);
      src = build1 (VIEW_CONVERT_EXPR, srctype, src);
      gassign *new_stmt = gimple_build_assign (var, VIEW_CONVERT_EXPR, src);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      src = var;
    }

  tree op = offset;
  if (!useless_type_conversion_p (idxtype, TREE_TYPE (op)))
    {
      gcc_assert (known_eq (TYPE_VECTOR_SUBPARTS (TREE_TYPE (op)),
			    TYPE_VECTOR_SUBPARTS (idxtype)));
      tree var = vect_get_new_ssa_name (idxtype, vect_simple_var);
      op = build1 (VIEW_CONVERT_EXPR, idxtype, op);
      gassign *new_stmt = gimple_build_assign (var, VIEW_CONVERT_EXPR, op);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      op = var;
    }

  tree scale = build_int_cst (scaletype, gs_info->scale);
  gcall *new_stmt
    = gimple_build_call (gs_info->decl, 5, ptr, mask_arg, op, src, scale);
  return new_stmt;
}

/* Prepare the base and offset in GS_INFO for vectorization.
   Set *DATAREF_PTR to the loop-invariant base address and *VEC_OFFSET
   to the vectorized offset argument for the first copy of STMT_INFO.
   STMT_INFO is the statement described by GS_INFO and LOOP is the
   containing loop.  */

static void
vect_get_gather_scatter_ops (loop_vec_info loop_vinfo,
			     class loop *loop, stmt_vec_info stmt_info,
			     slp_tree slp_node, gather_scatter_info *gs_info,
			     tree *dataref_ptr, vec<tree> *vec_offset)
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
  if (slp_node)
    vect_get_slp_defs (SLP_TREE_CHILDREN (slp_node)[0], vec_offset);
  else
    {
      unsigned ncopies
	= vect_get_num_copies (loop_vinfo, gs_info->offset_vectype);
      vect_get_vec_defs_for_operand (loop_vinfo, stmt_info, ncopies,
				     gs_info->offset, vec_offset,
				     gs_info->offset_vectype);
    }
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
				 gimple_stmt_iterator *gsi,
				 gather_scatter_info *gs_info,
				 tree *dataref_bump, tree *vec_offset,
				 vec_loop_lens *loop_lens)
{
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo))
    {
      /* _31 = .SELECT_VL (ivtmp_29, POLY_INT_CST [4, 4]);
	 ivtmp_8 = _31 * 16 (step in bytes);
	 .MASK_LEN_SCATTER_STORE (vectp_a.9_7, ... );
	 vectp_a.9_26 = vectp_a.9_7 + ivtmp_8;  */
      tree loop_len
	= vect_get_loop_len (loop_vinfo, gsi, loop_lens, 1, vectype, 0, 0);
      tree tmp
	= fold_build2 (MULT_EXPR, sizetype,
		       fold_convert (sizetype, unshare_expr (DR_STEP (dr))),
		       loop_len);
      *dataref_bump = force_gimple_operand_gsi (gsi, tmp, true, NULL_TREE, true,
						GSI_SAME_STMT);
    }
  else
    {
      tree bump
	= size_binop (MULT_EXPR,
		      fold_convert (sizetype, unshare_expr (DR_STEP (dr))),
		      size_int (TYPE_VECTOR_SUBPARTS (vectype)));
      *dataref_bump = cse_and_gimplify_to_preheader (loop_vinfo, bump);
    }

  /* The offset given in GS_INFO can have pointer type, so use the element
     type of the vector instead.  */
  tree offset_type = TREE_TYPE (gs_info->offset_vectype);

  /* Calculate X = DR_STEP / SCALE and convert it to the appropriate type.  */
  tree step = size_binop (EXACT_DIV_EXPR, unshare_expr (DR_STEP (dr)),
			  ssize_int (gs_info->scale));
  step = fold_convert (offset_type, step);

  /* Create {0, X, X*2, X*3, ...}.  */
  tree offset = fold_build2 (VEC_SERIES_EXPR, gs_info->offset_vectype,
			     build_zero_cst (offset_type), step);
  *vec_offset = cse_and_gimplify_to_preheader (loop_vinfo, offset);
}

/* Prepare the pointer IVs which needs to be updated by a variable amount.
   Such variable amount is the outcome of .SELECT_VL. In this case, we can
   allow each iteration process the flexible number of elements as long as
   the number <= vf elments.

   Return data reference according to SELECT_VL.
   If new statements are needed, insert them before GSI.  */

static tree
vect_get_loop_variant_data_ptr_increment (
  vec_info *vinfo, tree aggr_type, gimple_stmt_iterator *gsi,
  vec_loop_lens *loop_lens, dr_vec_info *dr_info,
  vect_memory_access_type memory_access_type)
{
  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (vinfo);
  tree step = vect_dr_behavior (vinfo, dr_info)->step;

  /* gather/scatter never reach here.  */
  gcc_assert (memory_access_type != VMAT_GATHER_SCATTER);

  /* When we support SELECT_VL pattern, we dynamic adjust
     the memory address by .SELECT_VL result.

     The result of .SELECT_VL is the number of elements to
     be processed of each iteration. So the memory address
     adjustment operation should be:

     addr = addr + .SELECT_VL (ARG..) * step;
  */
  tree loop_len
    = vect_get_loop_len (loop_vinfo, gsi, loop_lens, 1, aggr_type, 0, 0);
  tree len_type = TREE_TYPE (loop_len);
  /* Since the outcome of .SELECT_VL is element size, we should adjust
     it into bytesize so that it can be used in address pointer variable
     amount IVs adjustment.  */
  tree tmp = fold_build2 (MULT_EXPR, len_type, loop_len,
			  wide_int_to_tree (len_type, wi::to_widest (step)));
  tree bump = make_temp_ssa_name (len_type, NULL, "ivtmp");
  gassign *assign = gimple_build_assign (bump, tmp);
  gsi_insert_before (gsi, assign, GSI_SAME_STMT);
  return bump;
}

/* Return the amount that should be added to a vector pointer to move
   to the next or previous copy of AGGR_TYPE.  DR_INFO is the data reference
   being vectorized and MEMORY_ACCESS_TYPE describes the type of
   vectorization.  */

static tree
vect_get_data_ptr_increment (vec_info *vinfo, gimple_stmt_iterator *gsi,
			     dr_vec_info *dr_info, tree aggr_type,
			     vect_memory_access_type memory_access_type,
			     vec_loop_lens *loop_lens = nullptr)
{
  if (memory_access_type == VMAT_INVARIANT)
    return size_zero_node;

  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (vinfo);
  if (loop_vinfo && LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo))
    return vect_get_loop_variant_data_ptr_increment (vinfo, aggr_type, gsi,
						     loop_lens, dr_info,
						     memory_access_type);

  tree iv_step = TYPE_SIZE_UNIT (aggr_type);
  tree step = vect_dr_behavior (vinfo, dr_info)->step;
  if (tree_int_cst_sgn (step) == -1)
    iv_step = fold_build1 (NEGATE_EXPR, TREE_TYPE (iv_step), iv_step);
  return iv_step;
}

/* Check and perform vectorization of BUILT_IN_BSWAP{16,32,64,128}.  */

static bool
vectorizable_bswap (vec_info *vinfo,
		    stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    gimple **vec_stmt, slp_tree slp_node,
		    slp_tree *slp_op,
		    tree vectype_in, stmt_vector_for_cost *cost_vec)
{
  tree op, vectype;
  gcall *stmt = as_a <gcall *> (stmt_info->stmt);
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
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

  if (TYPE_SIZE (vectype_in) != TYPE_SIZE (vectype))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mismatched vector sizes %T and %T\n",
			 vectype_in, vectype);
      return false;
    }

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
  machine_mode vmode = TYPE_MODE (char_vectype);
  if (!can_vec_perm_const_p (vmode, vmode, indices))
    return false;

  if (! vec_stmt)
    {
      if (slp_node
	  && !vect_maybe_update_slp_op_vectype (slp_op[0], vectype_in))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}

      STMT_VINFO_TYPE (stmt_info) = call_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_bswap");
      record_stmt_cost (cost_vec,
			1, vector_stmt, stmt_info, 0, vect_prologue);
      record_stmt_cost (cost_vec,
			slp_node
			? SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node) : ncopies,
			vec_perm, stmt_info, 0, vect_body);
      return true;
    }

  tree bswap_vconst = vec_perm_indices_to_tree (char_vectype, indices);

  /* Transform.  */
  vec<tree> vec_oprnds = vNULL;
  vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		     op, &vec_oprnds);
  /* Arguments are ready. create the new vector stmt.  */
  unsigned i;
  tree vop;
  FOR_EACH_VEC_ELT (vec_oprnds, i, vop)
    {
      gimple *new_stmt;
      tree tem = make_ssa_name (char_vectype);
      new_stmt = gimple_build_assign (tem, build1 (VIEW_CONVERT_EXPR,
						   char_vectype, vop));
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      tree tem2 = make_ssa_name (char_vectype);
      new_stmt = gimple_build_assign (tem2, VEC_PERM_EXPR,
				      tem, tem, bswap_vconst);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      tem = make_ssa_name (vectype);
      new_stmt = gimple_build_assign (tem, build1 (VIEW_CONVERT_EXPR,
						   vectype, tem2));
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      if (slp_node)
	slp_node->push_vec_def (new_stmt);
      else
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
    }

  if (!slp_node)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

  vec_oprnds.release ();
  return true;
}

/* Return true if vector types VECTYPE_IN and VECTYPE_OUT have
   integer elements and if we can narrow VECTYPE_IN to VECTYPE_OUT
   in a single step.  On success, store the binary pack code in
   *CONVERT_CODE.  */

static bool
simple_integer_narrowing (tree vectype_out, tree vectype_in,
			  code_helper *convert_code)
{
  if (!INTEGRAL_TYPE_P (TREE_TYPE (vectype_out))
      || !INTEGRAL_TYPE_P (TREE_TYPE (vectype_in)))
    return false;

  code_helper code;
  int multi_step_cvt = 0;
  auto_vec <tree, 8> interm_types;
  if (!supportable_narrowing_operation (NOP_EXPR, vectype_out, vectype_in,
					&code, &multi_step_cvt, &interm_types)
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
vectorizable_call (vec_info *vinfo,
		   stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		   gimple **vec_stmt, slp_tree slp_node,
		   stmt_vector_for_cost *cost_vec)
{
  gcall *stmt;
  tree vec_dest;
  tree scalar_dest;
  tree op;
  tree vec_oprnd0 = NULL_TREE, vec_oprnd1 = NULL_TREE;
  tree vectype_out, vectype_in;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
  tree fndecl, new_temp, rhs_type;
  enum vect_def_type dt[4]
    = { vect_unknown_def_type, vect_unknown_def_type, vect_unknown_def_type,
	vect_unknown_def_type };
  tree vectypes[ARRAY_SIZE (dt)] = {};
  slp_tree slp_op[ARRAY_SIZE (dt)] = {};
  int ndts = ARRAY_SIZE (dt);
  int ncopies, j;
  auto_vec<tree, 8> vargs;
  enum { NARROW, NONE, WIDEN } modifier;
  size_t i, nargs;
  tree lhs;
  tree clz_ctz_arg1 = NULL_TREE;

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

  gcc_checking_assert (!stmt_can_throw_internal (cfun, stmt));

  vectype_out = STMT_VINFO_VECTYPE (stmt_info);

  /* Process function arguments.  */
  rhs_type = NULL_TREE;
  vectype_in = NULL_TREE;
  nargs = gimple_call_num_args (stmt);

  /* Bail out if the function has more than four arguments, we do not have
     interesting builtin functions to vectorize with more than two arguments
     except for fma.  No arguments is also not good.  */
  if (nargs == 0 || nargs > 4)
    return false;

  /* Ignore the arguments of IFN_GOMP_SIMD_LANE, they are magic.  */
  combined_fn cfn = gimple_call_combined_fn (stmt);
  if (cfn == CFN_GOMP_SIMD_LANE)
    {
      nargs = 0;
      rhs_type = unsigned_type_node;
    }
  /* Similarly pretend IFN_CLZ and IFN_CTZ only has one argument, the second
     argument just says whether it is well-defined at zero or not and what
     value should be returned for it.  */
  if ((cfn == CFN_CLZ || cfn == CFN_CTZ) && nargs == 2)
    {
      nargs = 1;
      clz_ctz_arg1 = gimple_call_arg (stmt, 1);
    }

  int mask_opno = -1;
  if (internal_fn_p (cfn))
    mask_opno = internal_fn_mask_index (as_internal_fn (cfn));

  for (i = 0; i < nargs; i++)
    {
      if ((int) i == mask_opno)
	{
	  if (!vect_check_scalar_mask (vinfo, stmt_info, slp_node, mask_opno,
				       &op, &slp_op[i], &dt[i], &vectypes[i]))
	    return false;
	  continue;
	}

      if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			       i, &op, &slp_op[i], &dt[i], &vectypes[i]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "use not simple.\n");
	  return false;
	}

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
	vectype_in = vectypes[i];
      else if (vectypes[i]
	       && !types_compatible_p (vectypes[i], vectype_in))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "argument vector types differ.\n");
	  return false;
	}
    }
  /* If all arguments are external or constant defs, infer the vector type
     from the scalar type.  */
  if (!vectype_in)
    vectype_in = get_vectype_for_scalar_type (vinfo, rhs_type, slp_node);
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
      != VECTOR_BOOLEAN_TYPE_P (vectype_in))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mixed mask and nonmask vector types\n");
      return false;
    }

  if (vect_emulated_vector_p (vectype_in) || vect_emulated_vector_p (vectype_out))
  {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "use emulated vector type for call\n");
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
  code_helper convert_code = MAX_TREE_CODES;
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
      else if (callee && fndecl_built_in_p (callee, BUILT_IN_MD))
	fndecl = targetm.vectorize.builtin_md_vectorized_function
	  (callee, vectype_out, vectype_in);
    }

  if (ifn == IFN_LAST && !fndecl)
    {
      if (cfn == CFN_GOMP_SIMD_LANE
	  && (!slp_node || SLP_TREE_LANES (slp_node) == 1)
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
		   || gimple_call_builtin_p (stmt, BUILT_IN_BSWAP64)
		   || gimple_call_builtin_p (stmt, BUILT_IN_BSWAP128)))
	return vectorizable_bswap (vinfo, stmt_info, gsi, vec_stmt, slp_node,
				   slp_op, vectype_in, cost_vec);
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

  int reduc_idx = STMT_VINFO_REDUC_IDX (stmt_info);
  internal_fn cond_fn = get_conditional_internal_fn (ifn);
  internal_fn cond_len_fn = get_len_internal_fn (ifn);
  int len_opno = internal_fn_len_index (cond_len_fn);
  vec_loop_masks *masks = (loop_vinfo ? &LOOP_VINFO_MASKS (loop_vinfo) : NULL);
  vec_loop_lens *lens = (loop_vinfo ? &LOOP_VINFO_LENS (loop_vinfo) : NULL);
  if (!vec_stmt) /* transformation not required.  */
    {
      if (slp_node)
	for (i = 0; i < nargs; ++i)
	  if (!vect_maybe_update_slp_op_vectype (slp_op[i],
						 vectypes[i]
						 ? vectypes[i] : vectype_in))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "incompatible vector types for invariants\n");
	      return false;
	    }
      STMT_VINFO_TYPE (stmt_info) = call_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_call");
      vect_model_simple_cost (vinfo, stmt_info,
			      ncopies, dt, ndts, slp_node, cost_vec);
      if (ifn != IFN_LAST && modifier == NARROW && !slp_node)
	record_stmt_cost (cost_vec, ncopies / 2,
			  vec_promote_demote, stmt_info, 0, vect_body);

      if (loop_vinfo
	  && LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo)
	  && (reduc_idx >= 0 || mask_opno >= 0))
	{
	  if (reduc_idx >= 0
	      && (cond_fn == IFN_LAST
		  || !direct_internal_fn_supported_p (cond_fn, vectype_out,
						      OPTIMIZE_FOR_SPEED))
	      && (cond_len_fn == IFN_LAST
		  || !direct_internal_fn_supported_p (cond_len_fn, vectype_out,
						      OPTIMIZE_FOR_SPEED)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "can't use a fully-masked loop because no"
				 " conditional operation is available.\n");
	      LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
	    }
	  else
	    {
	      unsigned int nvectors
		= (slp_node
		   ? SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node)
		   : ncopies);
	      tree scalar_mask = NULL_TREE;
	      if (mask_opno >= 0)
		scalar_mask = gimple_call_arg (stmt_info->stmt, mask_opno);
	      if (cond_len_fn != IFN_LAST
		  && direct_internal_fn_supported_p (cond_len_fn, vectype_out,
						     OPTIMIZE_FOR_SPEED))
		vect_record_loop_len (loop_vinfo, lens, nvectors, vectype_out,
				      1);
	      else
		vect_record_loop_mask (loop_vinfo, masks, nvectors, vectype_out,
				       scalar_mask);
	    }
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
  bool len_loop_p = loop_vinfo && LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo);
  unsigned int vect_nargs = nargs;
  if (len_loop_p)
    {
      if (len_opno >= 0)
	{
	  ifn = cond_len_fn;
	  /* COND_* -> COND_LEN_* takes 2 extra arguments:LEN,BIAS.  */
	  vect_nargs += 2;
	}
      else if (reduc_idx >= 0)
	gcc_unreachable ();
    }
  else if (masked_loop_p && reduc_idx >= 0)
    {
      ifn = cond_fn;
      vect_nargs += 2;
    }
  if (clz_ctz_arg1)
    ++vect_nargs;

  if (modifier == NONE || ifn != IFN_LAST)
    {
      tree prev_res = NULL_TREE;
      vargs.safe_grow (vect_nargs, true);
      auto_vec<vec<tree> > vec_defs (nargs);
      for (j = 0; j < ncopies; ++j)
	{
	  /* Build argument list for the vectorized call.  */
	  if (slp_node)
	    {
	      if (cfn == CFN_GOMP_SIMD_LANE)
		{
		  for (i = 0; i < SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node); ++i)
		    {
		      /* ???  For multi-lane SLP we'd need to build
			 { 0, 0, .., 1, 1, ... }.  */
		      tree cst = build_index_vector (vectype_out,
						     i * nunits_out, 1);
		      tree new_var
			  = vect_get_new_ssa_name (vectype_out, vect_simple_var,
						   "cst_");
		      gimple *init_stmt = gimple_build_assign (new_var, cst);
		      vect_init_vector_1 (vinfo, stmt_info, init_stmt, NULL);
		      new_temp = make_ssa_name (vec_dest);
		      gimple *new_stmt
			= gimple_build_assign (new_temp, new_var);
		      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt,
						   gsi);
		      slp_node->push_vec_def (new_stmt);
		    }
		  continue;
		}

	      vec<tree> vec_oprnds0;
	      vect_get_slp_defs (vinfo, slp_node, &vec_defs);
	      vec_oprnds0 = vec_defs[0];

	      /* Arguments are ready.  Create the new vector stmt.  */
	      FOR_EACH_VEC_ELT (vec_oprnds0, i, vec_oprnd0)
		{
		  int varg = 0;
		  if (masked_loop_p && reduc_idx >= 0)
		    {
		      unsigned int vec_num = vec_oprnds0.length ();
		      /* Always true for SLP.  */
		      gcc_assert (ncopies == 1);
		      vargs[varg++] = vect_get_loop_mask (loop_vinfo,
							  gsi, masks, vec_num,
							  vectype_out, i);
		    }
		  size_t k;
		  for (k = 0; k < nargs; k++)
		    {
		      vec<tree> vec_oprndsk = vec_defs[k];
		      vargs[varg++] = vec_oprndsk[i];
		    }
		  if (masked_loop_p && reduc_idx >= 0)
		    vargs[varg++] = vargs[reduc_idx + 1];
		  if (clz_ctz_arg1)
		    vargs[varg++] = clz_ctz_arg1;

		  gimple *new_stmt;
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
		      vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
		      if ((i & 1) == 0)
			{
			  prev_res = half_res;
			  continue;
			}
		      new_temp = make_ssa_name (vec_dest);
		      new_stmt = vect_gimple_build (new_temp, convert_code,
						    prev_res, half_res);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		    }
		  else
		    {
		      if (len_opno >= 0 && len_loop_p)
			{
			  unsigned int vec_num = vec_oprnds0.length ();
			  /* Always true for SLP.  */
			  gcc_assert (ncopies == 1);
			  tree len
			    = vect_get_loop_len (loop_vinfo, gsi, lens, vec_num,
						 vectype_out, i, 1);
			  signed char biasval
			    = LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
			  tree bias = build_int_cst (intQI_type_node, biasval);
			  vargs[len_opno] = len;
			  vargs[len_opno + 1] = bias;
			}
		      else if (mask_opno >= 0 && masked_loop_p)
			{
			  unsigned int vec_num = vec_oprnds0.length ();
			  /* Always true for SLP.  */
			  gcc_assert (ncopies == 1);
			  tree mask = vect_get_loop_mask (loop_vinfo,
							  gsi, masks, vec_num,
							  vectype_out, i);
			  vargs[mask_opno] = prepare_vec_mask
			    (loop_vinfo, TREE_TYPE (mask), mask,
			     vargs[mask_opno], gsi);
			}

		      gcall *call;
		      if (ifn != IFN_LAST)
			call = gimple_build_call_internal_vec (ifn, vargs);
		      else
			call = gimple_build_call_vec (fndecl, vargs);
		      new_temp = make_ssa_name (vec_dest, call);
		      gimple_call_set_lhs (call, new_temp);
		      gimple_call_set_nothrow (call, true);
		      vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
		      new_stmt = call;
		    }
		  slp_node->push_vec_def (new_stmt);
		}
	      continue;
	    }

	  int varg = 0;
	  if (masked_loop_p && reduc_idx >= 0)
	    vargs[varg++] = vect_get_loop_mask (loop_vinfo, gsi, masks, ncopies,
						vectype_out, j);
	  for (i = 0; i < nargs; i++)
	    {
	      op = gimple_call_arg (stmt, i);
	      if (j == 0)
		{
		  vec_defs.quick_push (vNULL);
		  vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies,
						 op, &vec_defs[i],
						 vectypes[i]);
		}
	      vargs[varg++] = vec_defs[i][j];
	    }
	  if (masked_loop_p && reduc_idx >= 0)
	    vargs[varg++] = vargs[reduc_idx + 1];
	  if (clz_ctz_arg1)
	    vargs[varg++] = clz_ctz_arg1;

	  if (len_opno >= 0 && len_loop_p)
	    {
	      tree len = vect_get_loop_len (loop_vinfo, gsi, lens, ncopies,
					    vectype_out, j, 1);
	      signed char biasval
		= LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
	      tree bias = build_int_cst (intQI_type_node, biasval);
	      vargs[len_opno] = len;
	      vargs[len_opno + 1] = bias;
	    }
	  else if (mask_opno >= 0 && masked_loop_p)
	    {
	      tree mask = vect_get_loop_mask (loop_vinfo, gsi, masks, ncopies,
					      vectype_out, j);
	      vargs[mask_opno]
		= prepare_vec_mask (loop_vinfo, TREE_TYPE (mask), mask,
				    vargs[mask_opno], gsi);
	    }

	  gimple *new_stmt;
	  if (cfn == CFN_GOMP_SIMD_LANE)
	    {
	      tree cst = build_index_vector (vectype_out, j * nunits_out, 1);
	      tree new_var
		= vect_get_new_ssa_name (vectype_out, vect_simple_var, "cst_");
	      gimple *init_stmt = gimple_build_assign (new_var, cst);
	      vect_init_vector_1 (vinfo, stmt_info, init_stmt, NULL);
	      new_temp = make_ssa_name (vec_dest);
	      new_stmt = gimple_build_assign (new_temp, new_var);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
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
	      vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
	      if ((j & 1) == 0)
		{
		  prev_res = half_res;
		  continue;
		}
	      new_temp = make_ssa_name (vec_dest);
	      new_stmt = vect_gimple_build (new_temp, convert_code, prev_res,
					    half_res);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
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
	      vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
	      new_stmt = call;
	    }

	  if (j == (modifier == NARROW ? 1 : 0))
	    *vec_stmt = new_stmt;
	  STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}
      for (i = 0; i < nargs; i++)
	{
	  vec<tree> vec_oprndsi = vec_defs[i];
	  vec_oprndsi.release ();
	}
    }
  else if (modifier == NARROW)
    {
      auto_vec<vec<tree> > vec_defs (nargs);
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
	      vec<tree> vec_oprnds0;

	      vect_get_slp_defs (vinfo, slp_node, &vec_defs);
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
		  vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
		  slp_node->push_vec_def (call);
		}
	      continue;
	    }

	  for (i = 0; i < nargs; i++)
	    {
	      op = gimple_call_arg (stmt, i);
	      if (j == 0)
		{
		  vec_defs.quick_push (vNULL);
		  vect_get_vec_defs_for_operand (vinfo, stmt_info, 2 * ncopies,
						 op, &vec_defs[i], vectypes[i]);
		}
	      vec_oprnd0 = vec_defs[i][2*j];
	      vec_oprnd1 = vec_defs[i][2*j+1];

	      vargs.quick_push (vec_oprnd0);
	      vargs.quick_push (vec_oprnd1);
	    }

	  gcall *new_stmt = gimple_build_call_vec (fndecl, vargs);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  gimple_call_set_lhs (new_stmt, new_temp);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

	  STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}

      if (!slp_node)
	*vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

      for (i = 0; i < nargs; i++)
	{
	  vec<tree> vec_oprndsi = vec_defs[i];
	  vec_oprndsi.release ();
	}
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
vect_simd_lane_linear (tree op, class loop *loop,
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

/* Function vectorizable_simd_clone_call.

   Check if STMT_INFO performs a function call that can be vectorized
   by calling a simd clone of the function.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_simd_clone_call (vec_info *vinfo, stmt_vec_info stmt_info,
			      gimple_stmt_iterator *gsi,
			      gimple **vec_stmt, slp_tree slp_node,
			      stmt_vector_for_cost *)
{
  tree vec_dest;
  tree scalar_dest;
  tree op, type;
  tree vec_oprnd0 = NULL_TREE;
  tree vectype;
  poly_uint64 nunits;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
  class loop *loop = loop_vinfo ? LOOP_VINFO_LOOP (loop_vinfo) : NULL;
  tree fndecl, new_temp;
  int ncopies, j;
  auto_vec<simd_call_arg_info> arginfo;
  vec<tree> vargs = vNULL;
  size_t i, nargs;
  tree lhs, rtype, ratype;
  vec<constructor_elt, va_gc> *ret_ctor_elts = NULL;
  int masked_call_offset = 0;

  /* Is STMT a vectorizable call?   */
  gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt);
  if (!stmt)
    return false;

  fndecl = gimple_call_fndecl (stmt);
  if (fndecl == NULL_TREE
      && gimple_call_internal_p (stmt, IFN_MASK_CALL))
    {
      fndecl = gimple_call_arg (stmt, 0);
      gcc_checking_assert (TREE_CODE (fndecl) == ADDR_EXPR);
      fndecl = TREE_OPERAND (fndecl, 0);
      gcc_checking_assert (TREE_CODE (fndecl) == FUNCTION_DECL);
      masked_call_offset = 1;
    }
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

  gcc_checking_assert (!stmt_can_throw_internal (cfun, stmt));

  vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (loop_vinfo && nested_in_vect_loop_p (loop, stmt_info))
    return false;

  /* Process function arguments.  */
  nargs = gimple_call_num_args (stmt) - masked_call_offset;

  /* Bail out if the function has zero arguments.  */
  if (nargs == 0)
    return false;

  vec<tree>& simd_clone_info = (slp_node ? SLP_TREE_SIMD_CLONE_INFO (slp_node)
				: STMT_VINFO_SIMD_CLONE_INFO (stmt_info));
  if (!vec_stmt)
    simd_clone_info.truncate (0);
  arginfo.reserve (nargs, true);
  auto_vec<slp_tree> slp_op;
  slp_op.safe_grow_cleared (nargs);

  for (i = 0; i < nargs; i++)
    {
      simd_call_arg_info thisarginfo;
      affine_iv iv;

      thisarginfo.linear_step = 0;
      thisarginfo.align = 0;
      thisarginfo.op = NULL_TREE;
      thisarginfo.simd_lane_linear = false;

      int op_no = i + masked_call_offset;
      if (slp_node)
	op_no = vect_slp_child_index_for_operand (stmt, op_no, false);
      if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			       op_no, &op, &slp_op[i],
			       &thisarginfo.dt, &thisarginfo.vectype)
	  || thisarginfo.dt == vect_uninitialized_def)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "use not simple.\n");
	  return false;
	}

      if (thisarginfo.dt == vect_constant_def
	  || thisarginfo.dt == vect_external_def)
	{
	  /* With SLP we determine the vector type of constants/externals
	     at analysis time, handling conflicts via
	     vect_maybe_update_slp_op_vectype.  At transform time
	     we have a vector type recorded for SLP.  */
	  gcc_assert (!vec_stmt
		      || !slp_node
		      || thisarginfo.vectype != NULL_TREE);
	  if (!vec_stmt)
	    thisarginfo.vectype = get_vectype_for_scalar_type (vinfo,
							       TREE_TYPE (op),
							       slp_node);
	}
      else
	gcc_assert (thisarginfo.vectype != NULL_TREE);

      /* For linear arguments, the analyze phase should have saved
	 the base and step in {STMT_VINFO,SLP_TREE}_SIMD_CLONE_INFO.  */
      if (vec_stmt
	  && i * 3 + 4 <= simd_clone_info.length ()
	  && simd_clone_info[i * 3 + 2])
	{
	  thisarginfo.linear_step = tree_to_shwi (simd_clone_info[i * 3 + 2]);
	  thisarginfo.op = simd_clone_info[i * 3 + 1];
	  thisarginfo.simd_lane_linear
	    = (simd_clone_info[i * 3 + 3] == boolean_true_node);
	  /* If loop has been peeled for alignment, we need to adjust it.  */
	  tree n1 = LOOP_VINFO_NITERS_UNCHANGED (loop_vinfo);
	  tree n2 = LOOP_VINFO_NITERS (loop_vinfo);
	  if (n1 != n2 && !thisarginfo.simd_lane_linear)
	    {
	      tree bias = fold_build2 (MINUS_EXPR, TREE_TYPE (n1), n1, n2);
	      tree step = simd_clone_info[i * 3 + 2];
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
	  && TREE_CODE (op) == SSA_NAME)
	vect_simd_lane_linear (op, loop, &thisarginfo);

      arginfo.quick_push (thisarginfo);
    }

  poly_uint64 vf = loop_vinfo ? LOOP_VINFO_VECT_FACTOR (loop_vinfo) : 1;
  unsigned group_size = slp_node ? SLP_TREE_LANES (slp_node) : 1;
  unsigned int badness = 0;
  struct cgraph_node *bestn = NULL;
  if (vec_stmt)
    bestn = cgraph_node::get (simd_clone_info[0]);
  else
    for (struct cgraph_node *n = node->simd_clones; n != NULL;
	 n = n->simdclone->next_clone)
      {
	unsigned int this_badness = 0;
	unsigned int num_calls;
	/* The number of arguments in the call and the number of parameters in
	   the simdclone should match.  However, when the simdclone is
	   'inbranch', it could have one more paramater than nargs when using
	   an inbranch simdclone to call a non-inbranch call, either in a
	   non-masked loop using a all true constant mask, or inside a masked
	   loop using it's mask.  */
	size_t simd_nargs = n->simdclone->nargs;
	if (!masked_call_offset && n->simdclone->inbranch)
	  simd_nargs--;
	if (!constant_multiple_p (vf * group_size, n->simdclone->simdlen,
				  &num_calls)
	    || (!n->simdclone->inbranch && (masked_call_offset > 0))
	    || (nargs != simd_nargs))
	  continue;
	if (num_calls != 1)
	  this_badness += floor_log2 (num_calls) * 4096;
	if (n->simdclone->inbranch)
	  this_badness += 8192;
	int target_badness = targetm.simd_clone.usable (n);
	if (target_badness < 0)
	  continue;
	this_badness += target_badness * 512;
	for (i = 0; i < nargs; i++)
	  {
	    switch (n->simdclone->args[i].arg_type)
	      {
	      case SIMD_CLONE_ARG_TYPE_VECTOR:
		if (!useless_type_conversion_p
			(n->simdclone->args[i].orig_type,
			 TREE_TYPE (gimple_call_arg (stmt,
						     i + masked_call_offset))))
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
		/* While we can create a traditional data vector from
		   an incoming integer mode mask we have no good way to
		   force generate an integer mode mask from a traditional
		   boolean vector input.  */
		if (SCALAR_INT_MODE_P (n->simdclone->mask_mode)
		    && !SCALAR_INT_MODE_P (TYPE_MODE (arginfo[i].vectype)))
		  i = -1;
		else if (!SCALAR_INT_MODE_P (n->simdclone->mask_mode)
			 && SCALAR_INT_MODE_P (TYPE_MODE (arginfo[i].vectype)))
		  this_badness += 2048;
		break;
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
	if (masked_call_offset == 0
	    && n->simdclone->inbranch
	    && n->simdclone->nargs > nargs)
	  {
	    gcc_assert (n->simdclone->args[n->simdclone->nargs - 1].arg_type ==
			SIMD_CLONE_ARG_TYPE_MASK);
	    /* Penalize using a masked SIMD clone in a non-masked loop, that is
	       not in a branch, as we'd have to construct an all-true mask.  */
	    if (!loop_vinfo || !LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
	      this_badness += 64;
	  }
	if (bestn == NULL || this_badness < badness)
	  {
	    bestn = n;
	    badness = this_badness;
	  }
      }

  if (bestn == NULL)
    return false;

  unsigned int num_mask_args = 0;
  if (SCALAR_INT_MODE_P (bestn->simdclone->mask_mode))
    for (i = 0; i < nargs; i++)
      if (bestn->simdclone->args[i].arg_type == SIMD_CLONE_ARG_TYPE_MASK)
	num_mask_args++;

  for (i = 0; i < nargs; i++)
    {
      if ((arginfo[i].dt == vect_constant_def
	   || arginfo[i].dt == vect_external_def)
	  && bestn->simdclone->args[i].arg_type == SIMD_CLONE_ARG_TYPE_VECTOR)
	{
	  tree arg_type = TREE_TYPE (gimple_call_arg (stmt,
						      i + masked_call_offset));
	  arginfo[i].vectype = get_vectype_for_scalar_type (vinfo, arg_type,
							    slp_node);
	  if (arginfo[i].vectype == NULL
	      || !constant_multiple_p (bestn->simdclone->simdlen,
				       TYPE_VECTOR_SUBPARTS (arginfo[i].vectype)))
	    return false;
	}

      if (bestn->simdclone->args[i].arg_type == SIMD_CLONE_ARG_TYPE_VECTOR
	  && VECTOR_BOOLEAN_TYPE_P (bestn->simdclone->args[i].vector_type))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "vector mask arguments are not supported.\n");
	  return false;
	}

      if (bestn->simdclone->args[i].arg_type == SIMD_CLONE_ARG_TYPE_MASK)
	{
	  tree clone_arg_vectype = bestn->simdclone->args[i].vector_type;
	  if (bestn->simdclone->mask_mode == VOIDmode)
	    {
	      if (maybe_ne (TYPE_VECTOR_SUBPARTS (clone_arg_vectype),
			    TYPE_VECTOR_SUBPARTS (arginfo[i].vectype)))
		{
		  /* FORNOW we only have partial support for vector-type masks
		     that can't hold all of simdlen. */
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				     vect_location,
				     "in-branch vector clones are not yet"
				     " supported for mismatched vector sizes.\n");
		  return false;
		}
	      if (!expand_vec_cond_expr_p (clone_arg_vectype,
					   arginfo[i].vectype))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				     vect_location,
				     "cannot compute mask argument for"
				     " in-branch vector clones.\n");
		  return false;
		}
	    }
	  else if (SCALAR_INT_MODE_P (bestn->simdclone->mask_mode))
	    {
	      if (!SCALAR_INT_MODE_P (TYPE_MODE (arginfo[i].vectype))
		  || maybe_ne (exact_div (bestn->simdclone->simdlen,
					  num_mask_args),
			       TYPE_VECTOR_SUBPARTS (arginfo[i].vectype)))
		{
		  /* FORNOW we only have partial support for integer-type masks
		     that represent the same number of lanes as the
		     vectorized mask inputs. */
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				     vect_location,
				     "in-branch vector clones are not yet "
				     "supported for mismatched vector sizes.\n");
		  return false;
		}
	    }
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				 vect_location,
				 "in-branch vector clones not supported"
				 " on this target.\n");
	      return false;
	    }
	}
    }

  fndecl = bestn->decl;
  nunits = bestn->simdclone->simdlen;
  if (slp_node)
    ncopies = vector_unroll_factor (vf * group_size, nunits);
  else
    ncopies = vector_unroll_factor (vf, nunits);

  /* If the function isn't const, only allow it in simd loops where user
     has asserted that at least nunits consecutive iterations can be
     performed using SIMD instructions.  */
  if ((loop == NULL || maybe_lt ((unsigned) loop->safelen, nunits))
      && gimple_vuse (stmt))
    return false;

  /* Sanity check: make sure that at least one copy of the vectorized stmt
     needs to be generated.  */
  gcc_assert (ncopies >= 1);

  if (!vec_stmt) /* transformation not required.  */
    {
      if (slp_node)
	for (unsigned i = 0; i < nargs; ++i)
	  if (!vect_maybe_update_slp_op_vectype (slp_op[i], arginfo[i].vectype))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "incompatible vector types for invariants\n");
	      return false;
	    }
      /* When the original call is pure or const but the SIMD ABI dictates
	 an aggregate return we will have to use a virtual definition and
	 in a loop eventually even need to add a virtual PHI.  That's
	 not straight-forward so allow to fix this up via renaming.  */
      if (gimple_call_lhs (stmt)
	  && !gimple_vdef (stmt)
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (bestn->decl))) == ARRAY_TYPE)
	vinfo->any_known_not_updated_vssa = true;
      /* ???  For SLP code-gen we end up inserting after the last
	 vector argument def rather than at the original call position
	 so automagic virtual operand updating doesn't work.  */
      if (gimple_vuse (stmt) && slp_node)
	vinfo->any_known_not_updated_vssa = true;
      simd_clone_info.safe_push (bestn->decl);
      for (i = 0; i < bestn->simdclone->nargs; i++)
	{
	  switch (bestn->simdclone->args[i].arg_type)
	    {
	    default:
	      continue;
	    case SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP:
	    case SIMD_CLONE_ARG_TYPE_LINEAR_REF_CONSTANT_STEP:
	      {
		simd_clone_info.safe_grow_cleared (i * 3 + 1, true);
		simd_clone_info.safe_push (arginfo[i].op);
		tree lst = POINTER_TYPE_P (TREE_TYPE (arginfo[i].op))
			   ? size_type_node : TREE_TYPE (arginfo[i].op);
		tree ls = build_int_cst (lst, arginfo[i].linear_step);
		simd_clone_info.safe_push (ls);
		tree sll = arginfo[i].simd_lane_linear
			   ? boolean_true_node : boolean_false_node;
		simd_clone_info.safe_push (sll);
	      }
	      break;
	    case SIMD_CLONE_ARG_TYPE_MASK:
	      if (loop_vinfo
		  && LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo))
		{
		  unsigned nmasks
		    = exact_div (ncopies * bestn->simdclone->simdlen,
				 TYPE_VECTOR_SUBPARTS (vectype)).to_constant ();
		  vect_record_loop_mask (loop_vinfo,
					 &LOOP_VINFO_MASKS (loop_vinfo),
					 nmasks, vectype, op);
		}

	      break;
	    }
	}

      if (!bestn->simdclone->inbranch && loop_vinfo)
	{
	  if (dump_enabled_p ()
	      && LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo))
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "can't use a fully-masked loop because a"
			     " non-masked simd clone was selected.\n");
	  LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
	}

      STMT_VINFO_TYPE (stmt_info) = call_simd_clone_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_simd_clone_call");
/*      vect_model_simple_cost (vinfo, stmt_info, ncopies,
				dt, slp_node, cost_vec); */
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

  auto_vec<vec<tree> > vec_oprnds;
  auto_vec<unsigned> vec_oprnds_i;
  vec_oprnds_i.safe_grow_cleared (nargs, true);
  if (slp_node)
    {
      vec_oprnds.reserve_exact (nargs);
      vect_get_slp_defs (vinfo, slp_node, &vec_oprnds);
    }
  else
    vec_oprnds.safe_grow_cleared (nargs, true);
  for (j = 0; j < ncopies; ++j)
    {
      poly_uint64 callee_nelements;
      poly_uint64 caller_nelements;
      /* Build argument list for the vectorized call.  */
      if (j == 0)
	vargs.create (nargs);
      else
	vargs.truncate (0);

      for (i = 0; i < nargs; i++)
	{
	  unsigned int k, l, m, o;
	  tree atype;
	  op = gimple_call_arg (stmt, i + masked_call_offset);
	  switch (bestn->simdclone->args[i].arg_type)
	    {
	    case SIMD_CLONE_ARG_TYPE_VECTOR:
	      atype = bestn->simdclone->args[i].vector_type;
	      caller_nelements = TYPE_VECTOR_SUBPARTS (arginfo[i].vectype);
	      callee_nelements = TYPE_VECTOR_SUBPARTS (atype);
	      o = vector_unroll_factor (nunits, callee_nelements);
	      for (m = j * o; m < (j + 1) * o; m++)
		{
		  if (known_lt (callee_nelements, caller_nelements))
		    {
		      poly_uint64 prec = GET_MODE_BITSIZE (TYPE_MODE (atype));
		      if (!constant_multiple_p (caller_nelements,
						callee_nelements, &k))
			gcc_unreachable ();

		      gcc_assert ((k & (k - 1)) == 0);
		      if (m == 0)
			{
			  if (!slp_node)
			    vect_get_vec_defs_for_operand (vinfo, stmt_info,
							   ncopies * o / k, op,
							   &vec_oprnds[i]);
			  vec_oprnds_i[i] = 0;
			  vec_oprnd0 = vec_oprnds[i][vec_oprnds_i[i]++];
			}
		      else
			{
			  vec_oprnd0 = arginfo[i].op;
			  if ((m & (k - 1)) == 0)
			    vec_oprnd0 = vec_oprnds[i][vec_oprnds_i[i]++];
			}
		      arginfo[i].op = vec_oprnd0;
		      vec_oprnd0
			= build3 (BIT_FIELD_REF, atype, vec_oprnd0,
				  bitsize_int (prec),
				  bitsize_int ((m & (k - 1)) * prec));
		      gassign *new_stmt
			= gimple_build_assign (make_ssa_name (atype),
					       vec_oprnd0);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		      vargs.safe_push (gimple_assign_lhs (new_stmt));
		    }
		  else
		    {
		      if (!constant_multiple_p (callee_nelements,
						caller_nelements, &k))
			gcc_unreachable ();
		      gcc_assert ((k & (k - 1)) == 0);
		      vec<constructor_elt, va_gc> *ctor_elts;
		      if (k != 1)
			vec_alloc (ctor_elts, k);
		      else
			ctor_elts = NULL;
		      for (l = 0; l < k; l++)
			{
			  if (m == 0 && l == 0)
			    {
			      if (!slp_node)
				vect_get_vec_defs_for_operand (vinfo, stmt_info,
							       k * o * ncopies,
							       op,
							       &vec_oprnds[i]);
			      vec_oprnds_i[i] = 0;
			      vec_oprnd0 = vec_oprnds[i][vec_oprnds_i[i]++];
			    }
			  else
			    vec_oprnd0 = vec_oprnds[i][vec_oprnds_i[i]++];
			  arginfo[i].op = vec_oprnd0;
			  if (k == 1)
			    break;
			  CONSTRUCTOR_APPEND_ELT (ctor_elts, NULL_TREE,
						  vec_oprnd0);
			}
		      if (k == 1)
			if (!useless_type_conversion_p (TREE_TYPE (vec_oprnd0),
						       atype))
			  {
			    vec_oprnd0 = build1 (VIEW_CONVERT_EXPR, atype,
						 vec_oprnd0);
			    gassign *new_stmt
			      = gimple_build_assign (make_ssa_name (atype),
						     vec_oprnd0);
			    vect_finish_stmt_generation (vinfo, stmt_info,
							 new_stmt, gsi);
			    vargs.safe_push (gimple_get_lhs (new_stmt));
			  }
			else
			  vargs.safe_push (vec_oprnd0);
		      else
			{
			  vec_oprnd0 = build_constructor (atype, ctor_elts);
			  gassign *new_stmt
			    = gimple_build_assign (make_ssa_name (atype),
						   vec_oprnd0);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			  vargs.safe_push (gimple_assign_lhs (new_stmt));
			}
		    }
		}
	      break;
	    case SIMD_CLONE_ARG_TYPE_MASK:
	      if (bestn->simdclone->mask_mode == VOIDmode)
		{
		  atype = bestn->simdclone->args[i].vector_type;
		  tree elt_type = TREE_TYPE (atype);
		  tree one = fold_convert (elt_type, integer_one_node);
		  tree zero = fold_convert (elt_type, integer_zero_node);
		  callee_nelements = TYPE_VECTOR_SUBPARTS (atype);
		  caller_nelements = TYPE_VECTOR_SUBPARTS (arginfo[i].vectype);
		  o = vector_unroll_factor (nunits, callee_nelements);
		  for (m = j * o; m < (j + 1) * o; m++)
		    {
		      if (maybe_lt (callee_nelements, caller_nelements))
			{
			  /* The mask type has fewer elements than simdlen.  */

			  /* FORNOW */
			  gcc_unreachable ();
			}
		      else if (known_eq (callee_nelements, caller_nelements))
			{
			  /* The SIMD clone function has the same number of
			     elements as the current function.  */
			  if (m == 0)
			    {
			      if (!slp_node)
				vect_get_vec_defs_for_operand (vinfo, stmt_info,
							       o * ncopies,
							       op,
							       &vec_oprnds[i]);
			      vec_oprnds_i[i] = 0;
			    }
			  vec_oprnd0 = vec_oprnds[i][vec_oprnds_i[i]++];
			  if (loop_vinfo
			      && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
			    {
			      vec_loop_masks *loop_masks
				= &LOOP_VINFO_MASKS (loop_vinfo);
			      tree loop_mask
				= vect_get_loop_mask (loop_vinfo, gsi,
						      loop_masks, ncopies,
						      vectype, j);
			      vec_oprnd0
				= prepare_vec_mask (loop_vinfo,
						    TREE_TYPE (loop_mask),
						    loop_mask, vec_oprnd0,
						    gsi);
			      loop_vinfo->vec_cond_masked_set.add ({ vec_oprnd0,
								     loop_mask });

			    }
			  vec_oprnd0
			    = build3 (VEC_COND_EXPR, atype, vec_oprnd0,
				      build_vector_from_val (atype, one),
				      build_vector_from_val (atype, zero));
			  gassign *new_stmt
			    = gimple_build_assign (make_ssa_name (atype),
						   vec_oprnd0);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			  vargs.safe_push (gimple_assign_lhs (new_stmt));
			}
		      else
			{
			  /* The mask type has more elements than simdlen.  */

			  /* FORNOW */
			  gcc_unreachable ();
			}
		    }
		}
	      else if (SCALAR_INT_MODE_P (bestn->simdclone->mask_mode))
		{
		  atype = bestn->simdclone->args[i].vector_type;
		  /* Guess the number of lanes represented by atype.  */
		  poly_uint64 atype_subparts
		    = exact_div (bestn->simdclone->simdlen,
				 num_mask_args);
		  o = vector_unroll_factor (nunits, atype_subparts);
		  for (m = j * o; m < (j + 1) * o; m++)
		    {
		      if (m == 0)
			{
			  if (!slp_node)
			    vect_get_vec_defs_for_operand (vinfo, stmt_info,
							   o * ncopies,
							   op,
							   &vec_oprnds[i]);
			  vec_oprnds_i[i] = 0;
			}
		      if (maybe_lt (atype_subparts,
				    TYPE_VECTOR_SUBPARTS (arginfo[i].vectype)))
			{
			  /* The mask argument has fewer elements than the
			     input vector.  */
			  /* FORNOW */
			  gcc_unreachable ();
			}
		      else if (known_eq (atype_subparts,
					 TYPE_VECTOR_SUBPARTS (arginfo[i].vectype)))
			{
			  /* The vector mask argument matches the input
			     in the number of lanes, but not necessarily
			     in the mode.  */
			  vec_oprnd0 = vec_oprnds[i][vec_oprnds_i[i]++];
			  tree st = lang_hooks.types.type_for_mode
				      (TYPE_MODE (TREE_TYPE (vec_oprnd0)), 1);
			  vec_oprnd0 = build1 (VIEW_CONVERT_EXPR, st,
					       vec_oprnd0);
			  gassign *new_stmt
			    = gimple_build_assign (make_ssa_name (st),
						   vec_oprnd0);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			  if (!types_compatible_p (atype, st))
			    {
			      new_stmt
				= gimple_build_assign (make_ssa_name (atype),
						       NOP_EXPR,
						       gimple_assign_lhs
							 (new_stmt));
			      vect_finish_stmt_generation (vinfo, stmt_info,
							   new_stmt, gsi);
			    }
			  vargs.safe_push (gimple_assign_lhs (new_stmt));
			}
		      else
			{
			  /* The mask argument has more elements than the
			     input vector.  */
			  /* FORNOW */
			  gcc_unreachable ();
			}
		    }
		}
	      else
		gcc_unreachable ();
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
		    = force_gimple_operand (unshare_expr (arginfo[i].op),
					    &stmts, true, NULL_TREE);
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
		  add_phi_arg (new_phi, arginfo[i].op,
			       loop_preheader_edge (loop), UNKNOWN_LOCATION);
		  enum tree_code code
		    = POINTER_TYPE_P (TREE_TYPE (op))
		      ? POINTER_PLUS_EXPR : PLUS_EXPR;
		  tree type = POINTER_TYPE_P (TREE_TYPE (op))
			      ? sizetype : TREE_TYPE (op);
		  poly_widest_int cst
		    = wi::mul (bestn->simdclone->args[i].linear_step,
			       ncopies * nunits);
		  tree tcst = wide_int_to_tree (type, cst);
		  tree phi_arg = copy_ssa_name (op);
		  gassign *new_stmt
		    = gimple_build_assign (phi_arg, code, phi_res, tcst);
		  gimple_stmt_iterator si = gsi_after_labels (loop->header);
		  gsi_insert_after (&si, new_stmt, GSI_NEW_STMT);
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
		  poly_widest_int cst
		    = wi::mul (bestn->simdclone->args[i].linear_step,
			       j * nunits);
		  tree tcst = wide_int_to_tree (type, cst);
		  new_temp = make_ssa_name (TREE_TYPE (op));
		  gassign *new_stmt
		    = gimple_build_assign (new_temp, code,
					   arginfo[i].op, tcst);
		  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
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

      if (masked_call_offset == 0
	  && bestn->simdclone->inbranch
	  && bestn->simdclone->nargs > nargs)
	{
	  unsigned long m, o;
	  size_t mask_i = bestn->simdclone->nargs - 1;
	  tree mask;
	  gcc_assert (bestn->simdclone->args[mask_i].arg_type ==
		      SIMD_CLONE_ARG_TYPE_MASK);

	  tree masktype = bestn->simdclone->args[mask_i].vector_type;
	  if (SCALAR_INT_MODE_P (bestn->simdclone->mask_mode))
	    /* Guess the number of lanes represented by masktype.  */
	    callee_nelements = exact_div (bestn->simdclone->simdlen,
					  bestn->simdclone->nargs - nargs);
	  else
	    callee_nelements = TYPE_VECTOR_SUBPARTS (masktype);
	  o = vector_unroll_factor (nunits, callee_nelements);
	  for (m = j * o; m < (j + 1) * o; m++)
	    {
	      if (loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
		{
		  vec_loop_masks *loop_masks = &LOOP_VINFO_MASKS (loop_vinfo);
		  mask = vect_get_loop_mask (loop_vinfo, gsi, loop_masks,
					     ncopies, vectype, j);
		}
	      else
		mask = vect_build_all_ones_mask (vinfo, stmt_info, masktype);

	      gassign *new_stmt;
	      if (SCALAR_INT_MODE_P (bestn->simdclone->mask_mode))
		{
		  /* This means we are dealing with integer mask modes.
		     First convert to an integer type with the same size as
		     the current vector type.  */
		  unsigned HOST_WIDE_INT intermediate_size
		      = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (mask)));
		  tree mid_int_type =
		      build_nonstandard_integer_type (intermediate_size, 1);
		  mask = build1 (VIEW_CONVERT_EXPR, mid_int_type, mask);
		  new_stmt
		      = gimple_build_assign (make_ssa_name (mid_int_type),
					     mask);
		  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
		  /* Then zero-extend to the mask mode.  */
		  mask = fold_build1 (NOP_EXPR, masktype,
				      gimple_get_lhs (new_stmt));
		}
	      else if (bestn->simdclone->mask_mode == VOIDmode)
		{
		  tree one = fold_convert (TREE_TYPE (masktype),
					   integer_one_node);
		  tree zero = fold_convert (TREE_TYPE (masktype),
					    integer_zero_node);
		  mask = build3 (VEC_COND_EXPR, masktype, mask,
				 build_vector_from_val (masktype, one),
				 build_vector_from_val (masktype, zero));
		}
	      else
		gcc_unreachable ();

	      new_stmt = gimple_build_assign (make_ssa_name (masktype), mask);
	      vect_finish_stmt_generation (vinfo, stmt_info,
					   new_stmt, gsi);
	      mask = gimple_assign_lhs (new_stmt);
	      vargs.safe_push (mask);
	    }
	}

      gcall *new_call = gimple_build_call_vec (fndecl, vargs);
      if (vec_dest)
	{
	  gcc_assert (ratype
		      || known_eq (TYPE_VECTOR_SUBPARTS (rtype), nunits));
	  if (ratype)
	    new_temp = create_tmp_var (ratype);
	  else if (useless_type_conversion_p (vectype, rtype))
	    new_temp = make_ssa_name (vec_dest, new_call);
	  else
	    new_temp = make_ssa_name (rtype, new_call);
	  gimple_call_set_lhs (new_call, new_temp);
	}
      vect_finish_stmt_generation (vinfo, stmt_info, new_call, gsi);
      gimple *new_stmt = new_call;

      if (vec_dest)
	{
	  if (!multiple_p (TYPE_VECTOR_SUBPARTS (vectype), nunits))
	    {
	      unsigned int k, l;
	      poly_uint64 prec = GET_MODE_BITSIZE (TYPE_MODE (vectype));
	      poly_uint64 bytes = GET_MODE_SIZE (TYPE_MODE (vectype));
	      k = vector_unroll_factor (nunits,
					TYPE_VECTOR_SUBPARTS (vectype));
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
		  new_stmt = gimple_build_assign (make_ssa_name (vectype), t);
		  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

		  if (j == 0 && l == 0)
		    *vec_stmt = new_stmt;
		  if (slp_node)
		    SLP_TREE_VEC_DEFS (slp_node)
		      .quick_push (gimple_assign_lhs (new_stmt));
		  else
		    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
		}

	      if (ratype)
		vect_clobber_variable (vinfo, stmt_info, gsi, new_temp);
	      continue;
	    }
	  else if (!multiple_p (nunits, TYPE_VECTOR_SUBPARTS (vectype)))
	    {
	      unsigned int k;
	      if (!constant_multiple_p (TYPE_VECTOR_SUBPARTS (vectype),
					TYPE_VECTOR_SUBPARTS (rtype), &k))
		gcc_unreachable ();
	      gcc_assert ((k & (k - 1)) == 0);
	      if ((j & (k - 1)) == 0)
		vec_alloc (ret_ctor_elts, k);
	      if (ratype)
		{
		  unsigned int m, o;
		  o = vector_unroll_factor (nunits,
					    TYPE_VECTOR_SUBPARTS (rtype));
		  for (m = 0; m < o; m++)
		    {
		      tree tem = build4 (ARRAY_REF, rtype, new_temp,
					 size_int (m), NULL_TREE, NULL_TREE);
		      new_stmt = gimple_build_assign (make_ssa_name (rtype),
						      tem);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		      CONSTRUCTOR_APPEND_ELT (ret_ctor_elts, NULL_TREE,
					      gimple_assign_lhs (new_stmt));
		    }
		  vect_clobber_variable (vinfo, stmt_info, gsi, new_temp);
		}
	      else
		CONSTRUCTOR_APPEND_ELT (ret_ctor_elts, NULL_TREE, new_temp);
	      if ((j & (k - 1)) != k - 1)
		continue;
	      vec_oprnd0 = build_constructor (vectype, ret_ctor_elts);
	      new_stmt
		= gimple_build_assign (make_ssa_name (vec_dest), vec_oprnd0);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

	      if ((unsigned) j == k - 1)
		*vec_stmt = new_stmt;
	      if (slp_node)
		SLP_TREE_VEC_DEFS (slp_node)
		  .quick_push (gimple_assign_lhs (new_stmt));
	      else
		STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	      continue;
	    }
	  else if (ratype)
	    {
	      tree t = build_fold_addr_expr (new_temp);
	      t = build2 (MEM_REF, vectype, t,
			  build_int_cst (TREE_TYPE (t), 0));
	      new_stmt = gimple_build_assign (make_ssa_name (vec_dest), t);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      vect_clobber_variable (vinfo, stmt_info, gsi, new_temp);
	    }
	  else if (!useless_type_conversion_p (vectype, rtype))
	    {
	      vec_oprnd0 = build1 (VIEW_CONVERT_EXPR, vectype, new_temp);
	      new_stmt
		= gimple_build_assign (make_ssa_name (vec_dest), vec_oprnd0);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }
	}

      if (j == 0)
	*vec_stmt = new_stmt;
      if (slp_node)
	SLP_TREE_VEC_DEFS (slp_node).quick_push (gimple_get_lhs (new_stmt));
      else
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
    }

  for (i = 0; i < nargs; ++i)
    {
      vec<tree> oprndsi = vec_oprnds[i];
      oprndsi.release ();
    }
  vargs.release ();

  /* Mark the clone as no longer being a candidate for GC.  */
  bestn->gc_candidate = false;

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
   VEC_OPRND0 and VEC_OPRND1.  The new vector stmt is to be inserted at GSI.
   In the case that CODE is a CALL_EXPR, this means that a call to DECL
   needs to be created (DECL is a function-decl of a target-builtin).
   STMT_INFO is the original scalar stmt that we are vectorizing.  */

static gimple *
vect_gen_widened_results_half (vec_info *vinfo, code_helper ch,
                               tree vec_oprnd0, tree vec_oprnd1, int op_type,
			       tree vec_dest, gimple_stmt_iterator *gsi,
			       stmt_vec_info stmt_info)
{
  gimple *new_stmt;
  tree new_temp;

  /* Generate half of the widened result:  */
  if (op_type != binary_op)
    vec_oprnd1 = NULL;
  new_stmt = vect_gimple_build (vec_dest, ch, vec_oprnd0, vec_oprnd1);
  new_temp = make_ssa_name (vec_dest, new_stmt);
  gimple_set_lhs (new_stmt, new_temp);
  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

  return new_stmt;
}


/* Create vectorized demotion statements for vector operands from VEC_OPRNDS.
   For multi-step conversions store the resulting vectors and call the function
   recursively. When NARROW_SRC_P is true, there's still a conversion after
   narrowing, don't store the vectors in the SLP_NODE or in vector info of
   the scalar statement(or in STMT_VINFO_RELATED_STMT chain).  */

static void
vect_create_vectorized_demotion_stmts (vec_info *vinfo, vec<tree> *vec_oprnds,
				       int multi_step_cvt,
				       stmt_vec_info stmt_info,
				       vec<tree> &vec_dsts,
				       gimple_stmt_iterator *gsi,
				       slp_tree slp_node, code_helper code,
				       bool narrow_src_p)
{
  unsigned int i;
  tree vop0, vop1, new_tmp, vec_dest;

  vec_dest = vec_dsts.pop ();

  for (i = 0; i < vec_oprnds->length (); i += 2)
    {
      /* Create demotion operation.  */
      vop0 = (*vec_oprnds)[i];
      vop1 = (*vec_oprnds)[i + 1];
      gimple *new_stmt = vect_gimple_build (vec_dest, code, vop0, vop1);
      new_tmp = make_ssa_name (vec_dest, new_stmt);
      gimple_set_lhs (new_stmt, new_tmp);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      if (multi_step_cvt || narrow_src_p)
	/* Store the resulting vector for next recursive call,
	   or return the resulting vector_tmp for NARROW FLOAT_EXPR.  */
	(*vec_oprnds)[i/2] = new_tmp;
      else
	{
	  /* This is the last step of the conversion sequence. Store the
	     vectors in SLP_NODE or in vector info of the scalar statement
	     (or in STMT_VINFO_RELATED_STMT chain).  */
	  if (slp_node)
	    slp_node->push_vec_def (new_stmt);
	  else
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
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
      vect_create_vectorized_demotion_stmts (vinfo, vec_oprnds,
					     multi_step_cvt - 1,
					     stmt_info, vec_dsts, gsi,
					     slp_node, VEC_PACK_TRUNC_EXPR,
					     narrow_src_p);
    }

  vec_dsts.quick_push (vec_dest);
}


/* Create vectorized promotion statements for vector operands from VEC_OPRNDS0
   and VEC_OPRNDS1, for a binary operation associated with scalar statement
   STMT_INFO.  For multi-step conversions store the resulting vectors and
   call the function recursively.  */

static void
vect_create_vectorized_promotion_stmts (vec_info *vinfo,
					vec<tree> *vec_oprnds0,
					vec<tree> *vec_oprnds1,
					stmt_vec_info stmt_info, tree vec_dest,
					gimple_stmt_iterator *gsi,
					code_helper ch1,
					code_helper ch2, int op_type)
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
      new_stmt1 = vect_gen_widened_results_half (vinfo, ch1, vop0, vop1,
						 op_type, vec_dest, gsi,
						 stmt_info);
      new_stmt2 = vect_gen_widened_results_half (vinfo, ch2, vop0, vop1,
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

/* Create vectorized promotion stmts for widening stmts using only half the
   potential vector size for input.  */
static void
vect_create_half_widening_stmts (vec_info *vinfo,
					vec<tree> *vec_oprnds0,
					vec<tree> *vec_oprnds1,
					stmt_vec_info stmt_info, tree vec_dest,
					gimple_stmt_iterator *gsi,
					code_helper code1,
					int op_type)
{
  int i;
  tree vop0, vop1;
  gimple *new_stmt1;
  gimple *new_stmt2;
  gimple *new_stmt3;
  vec<tree> vec_tmp = vNULL;

  vec_tmp.create (vec_oprnds0->length ());
  FOR_EACH_VEC_ELT (*vec_oprnds0, i, vop0)
    {
      tree new_tmp1, new_tmp2, new_tmp3, out_type;

      gcc_assert (op_type == binary_op);
      vop1 = (*vec_oprnds1)[i];

      /* Widen the first vector input.  */
      out_type = TREE_TYPE (vec_dest);
      new_tmp1 = make_ssa_name (out_type);
      new_stmt1 = gimple_build_assign (new_tmp1, NOP_EXPR, vop0);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt1, gsi);
      if (VECTOR_TYPE_P (TREE_TYPE (vop1)))
	{
	  /* Widen the second vector input.  */
	  new_tmp2 = make_ssa_name (out_type);
	  new_stmt2 = gimple_build_assign (new_tmp2, NOP_EXPR, vop1);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt2, gsi);
	  /* Perform the operation.  With both vector inputs widened.  */
	  new_stmt3 = vect_gimple_build (vec_dest, code1, new_tmp1, new_tmp2);
	}
      else
	{
	  /* Perform the operation.  With the single vector input widened.  */
	  new_stmt3 = vect_gimple_build (vec_dest, code1, new_tmp1, vop1);
	}

      new_tmp3 = make_ssa_name (vec_dest, new_stmt3);
      gimple_assign_set_lhs (new_stmt3, new_tmp3);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt3, gsi);

      /* Store the results for the next step.  */
      vec_tmp.quick_push (new_tmp3);
    }

  vec_oprnds0->release ();
  *vec_oprnds0 = vec_tmp;
}


/* Check if STMT_INFO performs a conversion operation that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_conversion (vec_info *vinfo,
			 stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			 gimple **vec_stmt, slp_tree slp_node,
			 stmt_vector_for_cost *cost_vec)
{
  tree vec_dest, cvt_op = NULL_TREE;
  tree scalar_dest;
  tree op0, op1 = NULL_TREE;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  tree_code tc1;
  code_helper code, code1, code2;
  code_helper codecvt1 = ERROR_MARK, codecvt2 = ERROR_MARK;
  tree new_temp;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 2;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  tree vectype_out, vectype_in;
  int ncopies, i;
  tree lhs_type, rhs_type;
  /* For conversions between floating point and integer, there're 2 NARROW
     cases. NARROW_SRC is for FLOAT_EXPR, means
     integer --DEMOTION--> integer --FLOAT_EXPR--> floating point.
     This is safe when the range of the source integer can fit into the lower
     precision. NARROW_DST is for FIX_TRUNC_EXPR, means
     floating point --FIX_TRUNC_EXPR--> integer --DEMOTION--> INTEGER.
     For other conversions, when there's narrowing, NARROW_DST is used as
     default.  */
  enum { NARROW_SRC, NARROW_DST, NONE, WIDEN } modifier;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  tree vop0;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
  int multi_step_cvt = 0;
  vec<tree> interm_types = vNULL;
  tree intermediate_type, cvt_type = NULL_TREE;
  int op_type;
  unsigned short fltsz;

  /* Is STMT a vectorizable conversion?   */

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  gimple* stmt = stmt_info->stmt;
  if (!(is_gimple_assign (stmt) || is_gimple_call (stmt)))
    return false;

  if (gimple_get_lhs (stmt) == NULL_TREE
      || TREE_CODE (gimple_get_lhs (stmt)) != SSA_NAME)
    return false;

  if (TREE_CODE (gimple_get_lhs (stmt)) != SSA_NAME)
    return false;

  if (is_gimple_assign (stmt))
    {
      code = gimple_assign_rhs_code (stmt);
      op_type = TREE_CODE_LENGTH ((tree_code) code);
    }
  else if (gimple_call_internal_p (stmt))
    {
      code = gimple_call_internal_fn (stmt);
      op_type = gimple_call_num_args (stmt);
    }
  else
    return false;

  bool widen_arith = (code == WIDEN_MULT_EXPR
		 || code == WIDEN_LSHIFT_EXPR
		 || widening_fn_p (code));

  if (!widen_arith
      && !CONVERT_EXPR_CODE_P (code)
      && code != FIX_TRUNC_EXPR
      && code != FLOAT_EXPR)
    return false;

  /* Check types of lhs and rhs.  */
  scalar_dest = gimple_get_lhs (stmt);
  lhs_type = TREE_TYPE (scalar_dest);
  vectype_out = STMT_VINFO_VECTYPE (stmt_info);

  /* Check the operands of the operation.  */
  slp_tree slp_op0, slp_op1 = NULL;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			   0, &op0, &slp_op0, &dt[0], &vectype_in))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "use not simple.\n");
      return false;
    }

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

  if (op_type == binary_op)
    {
      gcc_assert (code == WIDEN_MULT_EXPR
		  || code == WIDEN_LSHIFT_EXPR
		  || widening_fn_p (code));

      op1 = is_gimple_assign (stmt) ? gimple_assign_rhs2 (stmt) :
				     gimple_call_arg (stmt, 0);
      tree vectype1_in;
      if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 1,
			       &op1, &slp_op1, &dt[1], &vectype1_in))
	{
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.\n");
	  return false;
	}
      /* For WIDEN_MULT_EXPR, if OP0 is a constant, use the type of
	 OP1.  */
      if (!vectype_in)
	vectype_in = vectype1_in;
    }

  /* If op0 is an external or constant def, infer the vector type
     from the scalar type.  */
  if (!vectype_in)
    vectype_in = get_vectype_for_scalar_type (vinfo, rhs_type, slp_node);
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
    if (widen_arith)
      modifier = WIDEN;
    else
      modifier = NONE;
  else if (multiple_p (nunits_out, nunits_in))
    modifier = NARROW_DST;
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
  else if (modifier == NARROW_DST)
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
  vec<std::pair<tree, tree_code> > converts = vNULL;

  /* Supportable by target?  */
  switch (modifier)
    {
    case NONE:
      if (code != FIX_TRUNC_EXPR
	  && code != FLOAT_EXPR
	  && !CONVERT_EXPR_CODE_P (code))
	return false;
      gcc_assert (code.is_tree_code ());
      if (supportable_indirect_convert_operation (code,
						  vectype_out,
						  vectype_in,
						  &converts,
						  op0))
	{
	  gcc_assert (converts.length () <= 2);
	  if (converts.length () == 1)
	    code1 = converts[0].second;
	  else
	    {
	      cvt_type = NULL_TREE;
	      multi_step_cvt = converts.length () - 1;
	      codecvt1 = converts[0].second;
	      code1 = converts[1].second;
	      interm_types.safe_push (converts[0].first);
	    }
	  break;
	}

      /* FALLTHRU */
    unsupported:
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "conversion not supported by target.\n");
      return false;

    case WIDEN:
      if (known_eq (nunits_in, nunits_out))
	{
	  if (!(code.is_tree_code ()
		&& supportable_half_widening_operation ((tree_code) code,
							vectype_out, vectype_in,
							&tc1)))
	    goto unsupported;
	  code1 = tc1;
	  gcc_assert (!(multi_step_cvt && op_type == binary_op));
	  break;
	}
      if (supportable_widening_operation (vinfo, code, stmt_info,
					       vectype_out, vectype_in, &code1,
					       &code2, &multi_step_cvt,
					       &interm_types))
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
	      tc1 = ERROR_MARK;
	      gcc_assert (code.is_tree_code ());
	      if (!supportable_convert_operation ((tree_code) code, vectype_out,
						  cvt_type, &tc1))
		goto unsupported;
	      codecvt1 = tc1;
	    }
	  else if (!supportable_widening_operation (vinfo, code,
						    stmt_info, vectype_out,
						    cvt_type, &codecvt1,
						    &codecvt2, &multi_step_cvt,
						    &interm_types))
	    continue;
	  else
	    gcc_assert (multi_step_cvt == 0);

	  if (supportable_widening_operation (vinfo, NOP_EXPR, stmt_info,
					      cvt_type,
					      vectype_in, &code1,
					      &code2, &multi_step_cvt,
					      &interm_types))
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

    case NARROW_DST:
      gcc_assert (op_type == unary_op);
      if (supportable_narrowing_operation (code, vectype_out, vectype_in,
					   &code1, &multi_step_cvt,
					   &interm_types))
	break;

      if (GET_MODE_SIZE (lhs_mode) >= GET_MODE_SIZE (rhs_mode))
	goto unsupported;

      if (code == FIX_TRUNC_EXPR)
	{
	  cvt_type
	    = build_nonstandard_integer_type (GET_MODE_BITSIZE (rhs_mode), 0);
	  cvt_type = get_same_sized_vectype (cvt_type, vectype_in);
	  if (cvt_type == NULL_TREE)
	    goto unsupported;
	  if (supportable_convert_operation ((tree_code) code, cvt_type, vectype_in,
					      &tc1))
	    codecvt1 = tc1;
	  else
	    goto unsupported;
	  if (supportable_narrowing_operation (NOP_EXPR, vectype_out, cvt_type,
					       &code1, &multi_step_cvt,
					       &interm_types))
	    break;
	}
      /* If op0 can be represented with low precision integer,
	 truncate it to cvt_type and the do FLOAT_EXPR.  */
      else if (code == FLOAT_EXPR)
	{
	  wide_int op_min_value, op_max_value;
	  if (!vect_get_range_info (op0, &op_min_value, &op_max_value))
	    goto unsupported;

	  cvt_type
	    = build_nonstandard_integer_type (GET_MODE_BITSIZE (lhs_mode), 0);
	  if (cvt_type == NULL_TREE
	      || (wi::min_precision (op_max_value, SIGNED)
		  > TYPE_PRECISION (cvt_type))
	      || (wi::min_precision (op_min_value, SIGNED)
		  > TYPE_PRECISION (cvt_type)))
	    goto unsupported;

	  cvt_type = get_same_sized_vectype (cvt_type, vectype_out);
	  if (cvt_type == NULL_TREE)
	    goto unsupported;
	  if (!supportable_narrowing_operation (NOP_EXPR, cvt_type, vectype_in,
						&code1, &multi_step_cvt,
						&interm_types))
	    goto unsupported;
	  if (supportable_convert_operation ((tree_code) code, vectype_out,
					     cvt_type, &tc1))
	    {
	      codecvt1 = tc1;
	      modifier = NARROW_SRC;
	      break;
	    }
	}

      goto unsupported;

    default:
      gcc_unreachable ();
    }

  if (!vec_stmt)		/* transformation not required.  */
    {
      if (slp_node
	  && (!vect_maybe_update_slp_op_vectype (slp_op0, vectype_in)
	      || !vect_maybe_update_slp_op_vectype (slp_op1, vectype_in)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}
      DUMP_VECT_SCOPE ("vectorizable_conversion");
      if (modifier == NONE)
        {
	  STMT_VINFO_TYPE (stmt_info) = type_conversion_vec_info_type;
	  vect_model_simple_cost (vinfo, stmt_info,
				  ncopies * (1 + multi_step_cvt),
				  dt, ndts, slp_node, cost_vec);
	}
      else if (modifier == NARROW_SRC || modifier == NARROW_DST)
	{
	  STMT_VINFO_TYPE (stmt_info) = type_demotion_vec_info_type;
	  /* The final packing step produces one vector result per copy.  */
	  unsigned int nvectors
	    = (slp_node ? SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node) : ncopies);
	  vect_model_promotion_demotion_cost (stmt_info, dt, nvectors,
					      multi_step_cvt, cost_vec,
					      widen_arith);
	}
      else
	{
	  STMT_VINFO_TYPE (stmt_info) = type_promotion_vec_info_type;
	  /* The initial unpacking step produces two vector results
	     per copy.  MULTI_STEP_CVT is 0 for a single conversion,
	     so >> MULTI_STEP_CVT divides by 2^(number of steps - 1).  */
	  unsigned int nvectors
	    = (slp_node
	       ? SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node) >> multi_step_cvt
	       : ncopies * 2);
	  vect_model_promotion_demotion_cost (stmt_info, dt, nvectors,
					      multi_step_cvt, cost_vec,
					      widen_arith);
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
  bool widen_or_narrow_float_p
    = cvt_type && (modifier == WIDEN || modifier == NARROW_SRC);
  vec_dest = vect_create_destination_var (scalar_dest,
					  widen_or_narrow_float_p
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
					    widen_or_narrow_float_p
					    ? vectype_out : cvt_type);

  int ninputs = 1;
  if (!slp_node)
    {
      if (modifier == WIDEN)
	;
      else if (modifier == NARROW_SRC || modifier == NARROW_DST)
	{
	  if (multi_step_cvt)
	    ninputs = vect_pow2 (multi_step_cvt);
	  ninputs *= 2;
	}
    }

  switch (modifier)
    {
    case NONE:
      vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
			 op0, vectype_in, &vec_oprnds0);
      /* vec_dest is intermediate type operand when multi_step_cvt.  */
      if (multi_step_cvt)
	{
	  cvt_op = vec_dest;
	  vec_dest = vec_dsts[0];
	}

      FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	{
	  /* Arguments are ready, create the new vector stmt.  */
	  gimple* new_stmt;
	  if (multi_step_cvt)
	    {
	      gcc_assert (multi_step_cvt == 1);
	      new_stmt = vect_gimple_build (cvt_op, codecvt1, vop0);
	      new_temp = make_ssa_name (cvt_op, new_stmt);
	      gimple_assign_set_lhs (new_stmt, new_temp);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      vop0 = new_temp;
	    }
	  new_stmt = vect_gimple_build (vec_dest, code1, vop0);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  gimple_set_lhs (new_stmt, new_temp);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

	  if (slp_node)
	    slp_node->push_vec_def (new_stmt);
	  else
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}
      break;

    case WIDEN:
      /* In case the vectorization factor (VF) is bigger than the number
	 of elements that we can fit in a vectype (nunits), we have to
	 generate more than one vector stmt - i.e - we need to "unroll"
	 the vector stmt by a factor VF/nunits.  */
      vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies * ninputs,
			 op0, vectype_in, &vec_oprnds0,
			 code == WIDEN_LSHIFT_EXPR ? NULL_TREE : op1,
			 vectype_in, &vec_oprnds1);
      if (code == WIDEN_LSHIFT_EXPR)
	{
	  int oprnds_size = vec_oprnds0.length ();
	  vec_oprnds1.create (oprnds_size);
	  for (i = 0; i < oprnds_size; ++i)
	    vec_oprnds1.quick_push (op1);
	}
      /* Arguments are ready.  Create the new vector stmts.  */
      for (i = multi_step_cvt; i >= 0; i--)
	{
	  tree this_dest = vec_dsts[i];
	  code_helper c1 = code1, c2 = code2;
	  if (i == 0 && codecvt2 != ERROR_MARK)
	    {
	      c1 = codecvt1;
	      c2 = codecvt2;
	    }
	  if (known_eq (nunits_out, nunits_in))
	    vect_create_half_widening_stmts (vinfo, &vec_oprnds0, &vec_oprnds1,
					     stmt_info, this_dest, gsi, c1,
					     op_type);
	  else
	    vect_create_vectorized_promotion_stmts (vinfo, &vec_oprnds0,
						    &vec_oprnds1, stmt_info,
						    this_dest, gsi,
						    c1, c2, op_type);
	}

      FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	{
	  gimple *new_stmt;
	  if (cvt_type)
	    {
	      new_temp = make_ssa_name (vec_dest);
	      new_stmt = vect_gimple_build (new_temp, codecvt1, vop0);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }
	  else
	    new_stmt = SSA_NAME_DEF_STMT (vop0);

	  if (slp_node)
	    slp_node->push_vec_def (new_stmt);
	  else
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}
      break;

    case NARROW_SRC:
    case NARROW_DST:
      /* In case the vectorization factor (VF) is bigger than the number
	 of elements that we can fit in a vectype (nunits), we have to
	 generate more than one vector stmt - i.e - we need to "unroll"
	 the vector stmt by a factor VF/nunits.  */
      vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies * ninputs,
			 op0, vectype_in, &vec_oprnds0);
      /* Arguments are ready.  Create the new vector stmts.  */
      if (cvt_type && modifier == NARROW_DST)
	FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	  {
	    new_temp = make_ssa_name (vec_dest);
	    gimple *new_stmt = vect_gimple_build (new_temp, codecvt1, vop0);
	    vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    vec_oprnds0[i] = new_temp;
	  }

      vect_create_vectorized_demotion_stmts (vinfo, &vec_oprnds0,
					     multi_step_cvt,
					     stmt_info, vec_dsts, gsi,
					     slp_node, code1,
					     modifier == NARROW_SRC);
      /* After demoting op0 to cvt_type, convert it to dest.  */
      if (cvt_type && code == FLOAT_EXPR)
	{
	  for (unsigned int i = 0; i != vec_oprnds0.length() / 2;  i++)
	    {
	      /* Arguments are ready, create the new vector stmt.  */
	      gcc_assert (TREE_CODE_LENGTH ((tree_code) codecvt1) == unary_op);
	      gimple *new_stmt
		= vect_gimple_build (vec_dest, codecvt1, vec_oprnds0[i]);
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      gimple_set_lhs (new_stmt, new_temp);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

	      /* This is the last step of the conversion sequence. Store the
		 vectors in SLP_NODE or in vector info of the scalar statement
		 (or in STMT_VINFO_RELATED_STMT chain).  */
	      if (slp_node)
		slp_node->push_vec_def (new_stmt);
	      else
		STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	    }
	}
      break;
    }
  if (!slp_node)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  interm_types.release ();

  return true;
}

/* Return true if we can assume from the scalar form of STMT_INFO that
   neither the scalar nor the vector forms will generate code.  STMT_INFO
   is known not to involve a data reference.  */

bool
vect_nop_conversion_p (stmt_vec_info stmt_info)
{
  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  tree lhs = gimple_assign_lhs (stmt);
  tree_code code = gimple_assign_rhs_code (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);

  if (code == SSA_NAME || code == VIEW_CONVERT_EXPR)
    return true;

  if (CONVERT_EXPR_CODE_P (code))
    return tree_nop_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs));

  return false;
}

/* Function vectorizable_assignment.

   Check if STMT_INFO performs an assignment (copy) that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_assignment (vec_info *vinfo,
			 stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			 gimple **vec_stmt, slp_tree slp_node,
			 stmt_vector_for_cost *cost_vec)
{
  tree vec_dest;
  tree scalar_dest;
  tree op;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  tree new_temp;
  enum vect_def_type dt[1] = {vect_unknown_def_type};
  int ndts = 1;
  int ncopies;
  int i;
  vec<tree> vec_oprnds = vNULL;
  tree vop;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
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

  if (STMT_VINFO_DATA_REF (stmt_info))
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (!(gimple_assign_single_p (stmt)
	|| code == PAREN_EXPR
	|| CONVERT_EXPR_CODE_P (code)))
    return false;

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

  slp_tree slp_op;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 0, &op, &slp_op,
			   &dt[0], &vectype_in))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }
  if (!vectype_in)
    vectype_in = get_vectype_for_scalar_type (vinfo, TREE_TYPE (op), slp_node);

  /* We can handle VIEW_CONVERT conversions that do not change the number
     of elements or the vector size or other conversions when the component
     types are nop-convertible.  */
  if (!vectype_in
      || maybe_ne (TYPE_VECTOR_SUBPARTS (vectype_in), nunits)
      || (code == VIEW_CONVERT_EXPR
	  && maybe_ne (GET_MODE_SIZE (TYPE_MODE (vectype)),
		       GET_MODE_SIZE (TYPE_MODE (vectype_in))))
      || (CONVERT_EXPR_CODE_P (code)
	  && !tree_nop_conversion_p (TREE_TYPE (vectype),
				     TREE_TYPE (vectype_in))))
    return false;

  if (VECTOR_BOOLEAN_TYPE_P (vectype) != VECTOR_BOOLEAN_TYPE_P (vectype_in))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "can't convert between boolean and non "
			 "boolean vectors %T\n", TREE_TYPE (op));

      return false;
    }

  /* We do not handle bit-precision changes.  */
  if ((CONVERT_EXPR_CODE_P (code)
       || code == VIEW_CONVERT_EXPR)
      && ((INTEGRAL_TYPE_P (TREE_TYPE (scalar_dest))
	   && !type_has_mode_precision_p (TREE_TYPE (scalar_dest)))
	  || (INTEGRAL_TYPE_P (TREE_TYPE (op))
	      && !type_has_mode_precision_p (TREE_TYPE (op))))
      /* But a conversion that does not change the bit-pattern is ok.  */
      && !(INTEGRAL_TYPE_P (TREE_TYPE (scalar_dest))
	   && INTEGRAL_TYPE_P (TREE_TYPE (op))
	   && (((TYPE_PRECISION (TREE_TYPE (scalar_dest))
	       > TYPE_PRECISION (TREE_TYPE (op)))
	     && TYPE_UNSIGNED (TREE_TYPE (op)))
	       || (TYPE_PRECISION (TREE_TYPE (scalar_dest))
		   == TYPE_PRECISION (TREE_TYPE (op))))))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "type conversion to/from bit-precision "
			 "unsupported.\n");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      if (slp_node
	  && !vect_maybe_update_slp_op_vectype (slp_op, vectype_in))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}
      STMT_VINFO_TYPE (stmt_info) = assignment_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_assignment");
      if (!vect_nop_conversion_p (stmt_info))
	vect_model_simple_cost (vinfo, stmt_info, ncopies, dt, ndts, slp_node,
				cost_vec);
      return true;
    }

  /* Transform.  */
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform assignment.\n");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle use.  */
  vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies, op, &vec_oprnds);

  /* Arguments are ready. create the new vector stmt.  */
  FOR_EACH_VEC_ELT (vec_oprnds, i, vop)
    {
      if (CONVERT_EXPR_CODE_P (code)
	  || code == VIEW_CONVERT_EXPR)
	vop = build1 (VIEW_CONVERT_EXPR, vectype, vop);
      gassign *new_stmt = gimple_build_assign (vec_dest, vop);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      if (slp_node)
	slp_node->push_vec_def (new_stmt);
      else
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
    }
  if (!slp_node)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

  vec_oprnds.release ();
  return true;
}


/* Return TRUE if CODE (a shift operation) is supported for SCALAR_TYPE
   either as shift by a scalar or by a vector.  */

bool
vect_supportable_shift (vec_info *vinfo, enum tree_code code, tree scalar_type)
{

  machine_mode vec_mode;
  optab optab;
  int icode;
  tree vectype;

  vectype = get_vectype_for_scalar_type (vinfo, scalar_type);
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
vectorizable_shift (vec_info *vinfo,
		    stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    gimple **vec_stmt, slp_tree slp_node,
		    stmt_vector_for_cost *cost_vec)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL;
  tree vec_oprnd1 = NULL_TREE;
  tree vectype;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  enum tree_code code;
  machine_mode vec_mode;
  tree new_temp;
  optab optab;
  int icode;
  machine_mode optab_op2_mode;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 2;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  tree vectype_out;
  tree op1_vectype;
  int ncopies;
  int i;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  tree vop0, vop1;
  unsigned int k;
  bool scalar_shift_arg = true;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
  bool incompatible_op1_vectype_p = false;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && STMT_VINFO_DEF_TYPE (stmt_info) != vect_nested_cycle
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

  slp_tree slp_op0;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			   0, &op0, &slp_op0, &dt[0], &vectype))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }
  /* If op0 is an external or constant def, infer the vector type
     from the scalar type.  */
  if (!vectype)
    vectype = get_vectype_for_scalar_type (vinfo, TREE_TYPE (op0), slp_node);
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

  stmt_vec_info op1_def_stmt_info;
  slp_tree slp_op1;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 1, &op1, &slp_op1,
			   &dt[1], &op1_vectype, &op1_def_stmt_info))
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
       || dt[1] == vect_induction_def
       || dt[1] == vect_nested_cycle)
      && (!slp_node || SLP_TREE_LANES (slp_node) == 1))
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
	    if (slpstmt_info)
	      {
		gassign *slpstmt = as_a <gassign *> (slpstmt_info->stmt);
		if (!operand_equal_p (gimple_assign_rhs2 (slpstmt), op1, 0))
		  scalar_shift_arg = false;
	      }

	  /* For internal SLP defs we have to make sure we see scalar stmts
	     for all vector elements.
	     ???  For different vectors we could resort to a different
	     scalar shift operand but code-generation below simply always
	     takes the first.  */
	  if (dt[1] == vect_internal_def
	      && maybe_ne (nunits_out * SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node),
			   stmts.length ()))
	    scalar_shift_arg = false;
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
  bool was_scalar_shift_arg = scalar_shift_arg;
  if (!scalar_shift_arg)
    {
      optab = optab_for_tree_code (code, vectype, optab_vector);
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "vector/vector shift/rotate found.\n");

      if (!op1_vectype)
	op1_vectype = get_vectype_for_scalar_type (vinfo, TREE_TYPE (op1),
						   slp_op1);
      incompatible_op1_vectype_p
	= (op1_vectype == NULL_TREE
	   || maybe_ne (TYPE_VECTOR_SUBPARTS (op1_vectype),
			TYPE_VECTOR_SUBPARTS (vectype))
	   || TYPE_MODE (op1_vectype) != TYPE_MODE (vectype));
      if (incompatible_op1_vectype_p
	  && (!slp_node
	      || SLP_TREE_DEF_TYPE (slp_op1) != vect_constant_def
	      || slp_op1->refcnt != 1))
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

	      if (!op1_vectype)
		op1_vectype = get_vectype_for_scalar_type (vinfo,
							   TREE_TYPE (op1),
							   slp_op1);

              /* Unlike the other binary operators, shifts/rotates have
                 the rhs being int, instead of the same type as the lhs,
                 so make sure the scalar is the right type if we are
		 dealing with vectors of long long/long/short/char.  */
	      incompatible_op1_vectype_p
		= (!op1_vectype
		   || !tree_nop_conversion_p (TREE_TYPE (vectype),
					      TREE_TYPE (op1)));
	      if (incompatible_op1_vectype_p
		  && dt[1] == vect_internal_def)
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "unusable type for last operand in"
				     " vector/vector shift/rotate.\n");
		  return false;
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
      return false;
    }
  /* vector lowering cannot optimize vector shifts using word arithmetic.  */
  if (vect_emulated_vector_p (vectype))
    return false;

  if (!vec_stmt) /* transformation not required.  */
    {
      if (slp_node
	  && (!vect_maybe_update_slp_op_vectype (slp_op0, vectype)
	      || ((!scalar_shift_arg || dt[1] == vect_internal_def)
		  && (!incompatible_op1_vectype_p
		      || dt[1] == vect_constant_def)
		  && !vect_maybe_update_slp_op_vectype
			(slp_op1,
			 incompatible_op1_vectype_p ? vectype : op1_vectype))))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}
      /* Now adjust the constant shift amount in place.  */
      if (slp_node
	  && incompatible_op1_vectype_p
	  && dt[1] == vect_constant_def)
	{
	  for (unsigned i = 0;
	       i < SLP_TREE_SCALAR_OPS (slp_op1).length (); ++i)
	    {
	      SLP_TREE_SCALAR_OPS (slp_op1)[i]
		= fold_convert (TREE_TYPE (vectype),
				SLP_TREE_SCALAR_OPS (slp_op1)[i]);
	      gcc_assert ((TREE_CODE (SLP_TREE_SCALAR_OPS (slp_op1)[i])
			   == INTEGER_CST));
	    }
	}
      STMT_VINFO_TYPE (stmt_info) = shift_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_shift");
      vect_model_simple_cost (vinfo, stmt_info, ncopies, dt,
			      scalar_shift_arg ? 1 : ndts, slp_node, cost_vec);
      return true;
    }

  /* Transform.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform binary/unary operation.\n");

  if (incompatible_op1_vectype_p && !slp_node)
    {
      gcc_assert (!scalar_shift_arg && was_scalar_shift_arg);
      op1 = fold_convert (TREE_TYPE (vectype), op1);
      if (dt[1] != vect_constant_def)
	op1 = vect_init_vector (vinfo, stmt_info, op1,
				TREE_TYPE (vectype), NULL);
    }

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  if (scalar_shift_arg && dt[1] != vect_internal_def)
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
	  vec_oprnds1.create (slp_node ? slp_node->vec_stmts_size : ncopies);
	  vec_oprnds1.quick_push (vec_oprnd1);
	      /* Store vec_oprnd1 for every vector stmt to be created.
		 We check during the analysis that all the shift arguments
		 are the same.
		 TODO: Allow different constants for different vector
		 stmts generated for an SLP instance.  */
	  for (k = 0;
	       k < (slp_node ? slp_node->vec_stmts_size - 1 : ncopies - 1); k++)
	    vec_oprnds1.quick_push (vec_oprnd1);
	}
    }
  else if (!scalar_shift_arg && slp_node && incompatible_op1_vectype_p)
    {
      if (was_scalar_shift_arg)
	{
	  /* If the argument was the same in all lanes create
	     the correctly typed vector shift amount directly.  */
	  op1 = fold_convert (TREE_TYPE (vectype), op1);
	  op1 = vect_init_vector (vinfo, stmt_info, op1, TREE_TYPE (vectype),
				  !loop_vinfo ? gsi : NULL);
	  vec_oprnd1 = vect_init_vector (vinfo, stmt_info, op1, vectype,
					 !loop_vinfo ? gsi : NULL);
	  vec_oprnds1.create (slp_node->vec_stmts_size);
	  for (k = 0; k < slp_node->vec_stmts_size; k++)
	    vec_oprnds1.quick_push (vec_oprnd1);
	}
      else if (dt[1] == vect_constant_def)
	/* The constant shift amount has been adjusted in place.  */
	;
      else
	gcc_assert (TYPE_MODE (op1_vectype) == TYPE_MODE (vectype));
    }

  /* vec_oprnd1 is available if operand 1 should be of a scalar-type
     (a special case for certain kind of vector shifts); otherwise,
     operand 1 should be of a vector type (the usual case).  */
  vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		     op0, &vec_oprnds0,
		     vec_oprnd1 ? NULL_TREE : op1, &vec_oprnds1);

  /* Arguments are ready.  Create the new vector stmt.  */
  FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
    {
      /* For internal defs where we need to use a scalar shift arg
	 extract the first lane.  */
      if (scalar_shift_arg && dt[1] == vect_internal_def)
	{
	  vop1 = vec_oprnds1[0];
	  new_temp = make_ssa_name (TREE_TYPE (TREE_TYPE (vop1)));
	  gassign *new_stmt
	    = gimple_build_assign (new_temp,
				   build3 (BIT_FIELD_REF, TREE_TYPE (new_temp),
					   vop1,
					   TYPE_SIZE (TREE_TYPE (new_temp)),
					   bitsize_zero_node));
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	  vop1 = new_temp;
	}
      else
	vop1 = vec_oprnds1[i];
      gassign *new_stmt = gimple_build_assign (vec_dest, code, vop0, vop1);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
      if (slp_node)
	slp_node->push_vec_def (new_stmt);
      else
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
    }

  if (!slp_node)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

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
vectorizable_operation (vec_info *vinfo,
			stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			gimple **vec_stmt, slp_tree slp_node,
			stmt_vector_for_cost *cost_vec)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL_TREE, op2 = NULL_TREE;
  tree vectype;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  enum tree_code code, orig_code;
  machine_mode vec_mode;
  tree new_temp;
  int op_type;
  optab optab;
  bool target_support_p;
  enum vect_def_type dt[3]
    = {vect_unknown_def_type, vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 3;
  poly_uint64 nunits_in;
  poly_uint64 nunits_out;
  tree vectype_out;
  unsigned int ncopies;
  int vec_num;
  int i;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  vec<tree> vec_oprnds2 = vNULL;
  tree vop0, vop1, vop2;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  /* Is STMT a vectorizable binary/unary operation?   */
  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  /* Loads and stores are handled in vectorizable_{load,store}.  */
  if (STMT_VINFO_DATA_REF (stmt_info))
    return false;

  orig_code = code = gimple_assign_rhs_code (stmt);

  /* Shifts are handled in vectorizable_shift.  */
  if (code == LSHIFT_EXPR
      || code == RSHIFT_EXPR
      || code == LROTATE_EXPR
      || code == RROTATE_EXPR)
   return false;

  /* Comparisons are handled in vectorizable_comparison.  */
  if (TREE_CODE_CLASS (code) == tcc_comparison)
    return false;

  /* Conditions are handled in vectorizable_condition.  */
  if (code == COND_EXPR)
    return false;

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
  bool mask_op_p = VECTOR_BOOLEAN_TYPE_P (vectype_out);
  if (!mask_op_p
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

  slp_tree slp_op0;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			   0, &op0, &slp_op0, &dt[0], &vectype))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.\n");
      return false;
    }
  bool is_invariant = (dt[0] == vect_external_def
		       || dt[0] == vect_constant_def);
  /* If op0 is an external or constant def, infer the vector type
     from the scalar type.  */
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
	vectype = get_vectype_for_scalar_type (vinfo, TREE_TYPE (op0),
					       slp_node);
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
  if (maybe_ne (nunits_out, nunits_in)
      || !tree_nop_conversion_p (TREE_TYPE (vectype_out), TREE_TYPE (vectype)))
    return false;

  tree vectype2 = NULL_TREE, vectype3 = NULL_TREE;
  slp_tree slp_op1 = NULL, slp_op2 = NULL;
  if (op_type == binary_op || op_type == ternary_op)
    {
      if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			       1, &op1, &slp_op1, &dt[1], &vectype2))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.\n");
	  return false;
	}
      is_invariant &= (dt[1] == vect_external_def
		       || dt[1] == vect_constant_def);
      if (vectype2
	  && (maybe_ne (nunits_out, TYPE_VECTOR_SUBPARTS (vectype2))
	      || !tree_nop_conversion_p (TREE_TYPE (vectype_out),
					 TREE_TYPE (vectype2))))
	return false;
    }
  if (op_type == ternary_op)
    {
      if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			       2, &op2, &slp_op2, &dt[2], &vectype3))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.\n");
	  return false;
	}
      is_invariant &= (dt[2] == vect_external_def
		       || dt[2] == vect_constant_def);
      if (vectype3
	  && (maybe_ne (nunits_out, TYPE_VECTOR_SUBPARTS (vectype3))
	      || !tree_nop_conversion_p (TREE_TYPE (vectype_out),
					 TREE_TYPE (vectype3))))
	return false;
    }

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node)
    {
      ncopies = 1;
      vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
    }
  else
    {
      ncopies = vect_get_num_copies (loop_vinfo, vectype);
      vec_num = 1;
    }

  gcc_assert (ncopies >= 1);

  /* Reject attempts to combine mask types with nonmask types, e.g. if
     we have an AND between a (nonmask) boolean loaded from memory and
     a (mask) boolean result of a comparison.

     TODO: We could easily fix these cases up using pattern statements.  */
  if (VECTOR_BOOLEAN_TYPE_P (vectype) != mask_op_p
      || (vectype2 && VECTOR_BOOLEAN_TYPE_P (vectype2) != mask_op_p)
      || (vectype3 && VECTOR_BOOLEAN_TYPE_P (vectype3) != mask_op_p))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "mixed mask and nonmask vector types\n");
      return false;
    }

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
      target_support_p = (optab_handler (optab, vec_mode) != CODE_FOR_nothing
			  || optab_libfunc (optab, vec_mode));
    }

  bool using_emulated_vectors_p = vect_emulated_vector_p (vectype);
  if (!target_support_p || using_emulated_vectors_p)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "op not supported by target.\n");
      /* When vec_mode is not a vector mode and we verified ops we
	 do not have to lower like AND are natively supported let
	 those through even when the mode isn't word_mode.  For
	 ops we have to lower the lowering code assumes we are
	 dealing with word_mode.  */
      if (!INTEGRAL_TYPE_P (TREE_TYPE (vectype))
	  || (((code == PLUS_EXPR || code == MINUS_EXPR || code == NEGATE_EXPR)
	    || !target_support_p)
	   && maybe_ne (GET_MODE_SIZE (vec_mode), UNITS_PER_WORD))
	  /* Check only during analysis.  */
	  || (!vec_stmt && !vect_can_vectorize_without_simd_p (code)))
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "using word mode not possible.\n");
	  return false;
	}
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "proceeding using word mode.\n");
      using_emulated_vectors_p = true;
    }

  int reduc_idx = STMT_VINFO_REDUC_IDX (stmt_info);
  vec_loop_masks *masks = (loop_vinfo ? &LOOP_VINFO_MASKS (loop_vinfo) : NULL);
  vec_loop_lens *lens = (loop_vinfo ? &LOOP_VINFO_LENS (loop_vinfo) : NULL);
  internal_fn cond_fn = get_conditional_internal_fn (code);
  internal_fn cond_len_fn = get_conditional_len_internal_fn (code);

  /* If operating on inactive elements could generate spurious traps,
     we need to restrict the operation to active lanes.  Note that this
     specifically doesn't apply to unhoisted invariants, since they
     operate on the same value for every lane.

     Similarly, if this operation is part of a reduction, a fully-masked
     loop should only change the active lanes of the reduction chain,
     keeping the inactive lanes as-is.  */
  bool mask_out_inactive = ((!is_invariant && gimple_could_trap_p (stmt))
			    || reduc_idx >= 0);

  if (!vec_stmt) /* transformation not required.  */
    {
      if (loop_vinfo
	  && LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo)
	  && mask_out_inactive)
	{
	  if (cond_len_fn != IFN_LAST
	      && direct_internal_fn_supported_p (cond_len_fn, vectype,
						 OPTIMIZE_FOR_SPEED))
	    vect_record_loop_len (loop_vinfo, lens, ncopies * vec_num, vectype,
				  1);
	  else if (cond_fn != IFN_LAST
		   && direct_internal_fn_supported_p (cond_fn, vectype,
						      OPTIMIZE_FOR_SPEED))
	    vect_record_loop_mask (loop_vinfo, masks, ncopies * vec_num,
				   vectype, NULL);
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "can't use a fully-masked loop because no"
				 " conditional operation is available.\n");
	      LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
	    }
	}

      /* Put types on constant and invariant SLP children.  */
      if (slp_node
	  && (!vect_maybe_update_slp_op_vectype (slp_op0, vectype)
	      || !vect_maybe_update_slp_op_vectype (slp_op1, vectype)
	      || !vect_maybe_update_slp_op_vectype (slp_op2, vectype)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}

      STMT_VINFO_TYPE (stmt_info) = op_vec_info_type;
      DUMP_VECT_SCOPE ("vectorizable_operation");
      vect_model_simple_cost (vinfo, stmt_info,
			      ncopies, dt, ndts, slp_node, cost_vec);
      if (using_emulated_vectors_p)
	{
	  /* The above vect_model_simple_cost call handles constants
	     in the prologue and (mis-)costs one of the stmts as
	     vector stmt.  See below for the actual lowering that will
	     be applied.  */
	  unsigned n
	    = slp_node ? SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node) : ncopies;
	  switch (code)
	    {
	    case PLUS_EXPR:
	      n *= 5;
	      break;
	    case MINUS_EXPR:
	      n *= 6;
	      break;
	    case NEGATE_EXPR:
	      n *= 4;
	      break;
	    default:
	      /* Bit operations do not have extra cost and are accounted
		 as vector stmt by vect_model_simple_cost.  */
	      n = 0;
	      break;
	    }
	  if (n != 0)
	    {
	      /* We also need to materialize two large constants.  */
	      record_stmt_cost (cost_vec, 2, scalar_stmt, stmt_info,
				0, vect_prologue);
	      record_stmt_cost (cost_vec, n, scalar_stmt, stmt_info,
				0, vect_body);
	    }
	}
      return true;
    }

  /* Transform.  */

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform binary/unary operation.\n");

  bool masked_loop_p = loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo);
  bool len_loop_p = loop_vinfo && LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo);

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

  vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		     op0, &vec_oprnds0, op1, &vec_oprnds1, op2, &vec_oprnds2);
  /* Arguments are ready.  Create the new vector stmt.  */
  FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
    {
      gimple *new_stmt = NULL;
      vop1 = ((op_type == binary_op || op_type == ternary_op)
	      ? vec_oprnds1[i] : NULL_TREE);
      vop2 = ((op_type == ternary_op) ? vec_oprnds2[i] : NULL_TREE);
      if (using_emulated_vectors_p
	  && (code == PLUS_EXPR || code == MINUS_EXPR || code == NEGATE_EXPR))
	{
	  /* Lower the operation.  This follows vector lowering.  */
	  unsigned int width = vector_element_bits (vectype);
	  tree inner_type = TREE_TYPE (vectype);
	  tree word_type
	    = build_nonstandard_integer_type (GET_MODE_BITSIZE (word_mode), 1);
	  HOST_WIDE_INT max = GET_MODE_MASK (TYPE_MODE (inner_type));
	  tree low_bits = build_replicated_int_cst (word_type, width, max >> 1);
	  tree high_bits
	    = build_replicated_int_cst (word_type, width, max & ~(max >> 1));
	  tree wvop0 = make_ssa_name (word_type);
	  new_stmt = gimple_build_assign (wvop0, VIEW_CONVERT_EXPR,
					  build1 (VIEW_CONVERT_EXPR,
						  word_type, vop0));
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	  tree result_low, signs;
	  if (code == PLUS_EXPR || code == MINUS_EXPR)
	    {
	      tree wvop1 = make_ssa_name (word_type);
	      new_stmt = gimple_build_assign (wvop1, VIEW_CONVERT_EXPR,
					      build1 (VIEW_CONVERT_EXPR,
						      word_type, vop1));
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      signs = make_ssa_name (word_type);
	      new_stmt = gimple_build_assign (signs,
					      BIT_XOR_EXPR, wvop0, wvop1);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      tree b_low = make_ssa_name (word_type);
	      new_stmt = gimple_build_assign (b_low,
					      BIT_AND_EXPR, wvop1, low_bits);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      tree a_low = make_ssa_name (word_type);
	      if (code == PLUS_EXPR)
		new_stmt = gimple_build_assign (a_low,
						BIT_AND_EXPR, wvop0, low_bits);
	      else
		new_stmt = gimple_build_assign (a_low,
						BIT_IOR_EXPR, wvop0, high_bits);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      if (code == MINUS_EXPR)
		{
		  new_stmt = gimple_build_assign (NULL_TREE,
						  BIT_NOT_EXPR, signs);
		  signs = make_ssa_name (word_type);
		  gimple_assign_set_lhs (new_stmt, signs);
		  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
		}
	      new_stmt = gimple_build_assign (NULL_TREE,
					      BIT_AND_EXPR, signs, high_bits);
	      signs = make_ssa_name (word_type);
	      gimple_assign_set_lhs (new_stmt, signs);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      result_low = make_ssa_name (word_type);
	      new_stmt = gimple_build_assign (result_low, code, a_low, b_low);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }
	  else
	    {
	      tree a_low = make_ssa_name (word_type);
	      new_stmt = gimple_build_assign (a_low,
					      BIT_AND_EXPR, wvop0, low_bits);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      signs = make_ssa_name (word_type);
	      new_stmt = gimple_build_assign (signs, BIT_NOT_EXPR, wvop0);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      new_stmt = gimple_build_assign (NULL_TREE,
					      BIT_AND_EXPR, signs, high_bits);
	      signs = make_ssa_name (word_type);
	      gimple_assign_set_lhs (new_stmt, signs);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      result_low = make_ssa_name (word_type);
	      new_stmt = gimple_build_assign (result_low,
					      MINUS_EXPR, high_bits, a_low);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }
	  new_stmt = gimple_build_assign (NULL_TREE, BIT_XOR_EXPR, result_low,
					  signs);
	  result_low = make_ssa_name (word_type);
	  gimple_assign_set_lhs (new_stmt, result_low);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	  new_stmt = gimple_build_assign (NULL_TREE, VIEW_CONVERT_EXPR,
					  build1 (VIEW_CONVERT_EXPR,
						  vectype, result_low));
	  new_temp = make_ssa_name (vectype);
	  gimple_assign_set_lhs (new_stmt, new_temp);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	}
      else if ((masked_loop_p || len_loop_p) && mask_out_inactive)
	{
	  tree mask;
	  if (masked_loop_p)
	    mask = vect_get_loop_mask (loop_vinfo, gsi, masks,
				       vec_num * ncopies, vectype, i);
	  else
	    /* Dummy mask.  */
	    mask = build_minus_one_cst (truth_type_for (vectype));
	  auto_vec<tree> vops (6);
	  vops.quick_push (mask);
	  vops.quick_push (vop0);
	  if (vop1)
	    vops.quick_push (vop1);
	  if (vop2)
	    vops.quick_push (vop2);
	  if (reduc_idx >= 0)
	    {
	      /* Perform the operation on active elements only and take
		 inactive elements from the reduction chain input.  */
	      gcc_assert (!vop2);
	      vops.quick_push (reduc_idx == 1 ? vop1 : vop0);
	    }
	  else
	    {
	      auto else_value = targetm.preferred_else_value
		(cond_fn, vectype, vops.length () - 1, &vops[1]);
	      vops.quick_push (else_value);
	    }
	  if (len_loop_p)
	    {
	      tree len = vect_get_loop_len (loop_vinfo, gsi, lens,
					    vec_num * ncopies, vectype, i, 1);
	      signed char biasval
		= LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
	      tree bias = build_int_cst (intQI_type_node, biasval);
	      vops.quick_push (len);
	      vops.quick_push (bias);
	    }
	  gcall *call
	    = gimple_build_call_internal_vec (masked_loop_p ? cond_fn
							    : cond_len_fn,
					      vops);
	  new_temp = make_ssa_name (vec_dest, call);
	  gimple_call_set_lhs (call, new_temp);
	  gimple_call_set_nothrow (call, true);
	  vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
	  new_stmt = call;
	}
      else
	{
	  tree mask = NULL_TREE;
	  /* When combining two masks check if either of them is elsewhere
	     combined with a loop mask, if that's the case we can mark that the
	     new combined mask doesn't need to be combined with a loop mask.  */
	  if (masked_loop_p
	      && code == BIT_AND_EXPR
	      && VECTOR_BOOLEAN_TYPE_P (vectype))
	    {
	      if (loop_vinfo->scalar_cond_masked_set.contains ({ op0,
								 ncopies}))
		{
		  mask = vect_get_loop_mask (loop_vinfo, gsi, masks,
					     vec_num * ncopies, vectype, i);

		  vop0 = prepare_vec_mask (loop_vinfo, TREE_TYPE (mask), mask,
					   vop0, gsi);
		}

	      if (loop_vinfo->scalar_cond_masked_set.contains ({ op1,
								 ncopies }))
		{
		  mask = vect_get_loop_mask (loop_vinfo, gsi, masks,
					     vec_num * ncopies, vectype, i);

		  vop1 = prepare_vec_mask (loop_vinfo, TREE_TYPE (mask), mask,
					   vop1, gsi);
		}
	    }

	  new_stmt = gimple_build_assign (vec_dest, code, vop0, vop1, vop2);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  gimple_assign_set_lhs (new_stmt, new_temp);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	  if (using_emulated_vectors_p)
	    suppress_warning (new_stmt, OPT_Wvector_operation_performance);

	  /* Enter the combined value into the vector cond hash so we don't
	     AND it with a loop mask again.  */
	  if (mask)
	    loop_vinfo->vec_cond_masked_set.add ({ new_temp, mask });
	}

      if (vec_cvt_dest)
	{
	  new_temp = build1 (VIEW_CONVERT_EXPR, vectype_out, new_temp);
	  new_stmt = gimple_build_assign (vec_cvt_dest, VIEW_CONVERT_EXPR,
					  new_temp);
	  new_temp = make_ssa_name (vec_cvt_dest, new_stmt);
	  gimple_assign_set_lhs (new_stmt, new_temp);
	  vect_finish_stmt_generation (vinfo, stmt_info,
				       new_stmt, gsi);
	}

      if (slp_node)
	slp_node->push_vec_def (new_stmt);
      else
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
    }

  if (!slp_node)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  vec_oprnds2.release ();

  return true;
}

/* A helper function to ensure data reference DR_INFO's base alignment.  */

static void
ensure_base_align (dr_vec_info *dr_info)
{
  /* Alignment is only analyzed for the first element of a DR group,
     use that to look at base alignment we need to enforce.  */
  if (STMT_VINFO_GROUPED_ACCESS (dr_info->stmt))
    dr_info = STMT_VINFO_DR_INFO (DR_GROUP_FIRST_ELEMENT (dr_info->stmt));

  gcc_assert (dr_info->misalignment != DR_MISALIGNMENT_UNINITIALIZED);

  if (dr_info->base_misaligned)
    {
      tree base_decl = dr_info->base_decl;

      // We should only be able to increase the alignment of a base object if
      // we know what its new alignment should be at compile time.
      unsigned HOST_WIDE_INT align_base_to =
	DR_TARGET_ALIGNMENT (dr_info).to_constant () * BITS_PER_UNIT;

      if (decl_in_symtab_p (base_decl))
	symtab_node::get (base_decl)->increase_alignment (align_base_to);
      else if (DECL_ALIGN (base_decl) < align_base_to)
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


/* Function scan_operand_equal_p.

   Helper function for check_scan_store.  Compare two references
   with .GOMP_SIMD_LANE bases.  */

static bool
scan_operand_equal_p (tree ref1, tree ref2)
{
  tree ref[2] = { ref1, ref2 };
  poly_int64 bitsize[2], bitpos[2];
  tree offset[2], base[2];
  for (int i = 0; i < 2; ++i)
    {
      machine_mode mode;
      int unsignedp, reversep, volatilep = 0;
      base[i] = get_inner_reference (ref[i], &bitsize[i], &bitpos[i],
      				     &offset[i], &mode, &unsignedp,
      				     &reversep, &volatilep);
      if (reversep || volatilep || maybe_ne (bitpos[i], 0))
	return false;
      if (TREE_CODE (base[i]) == MEM_REF
	  && offset[i] == NULL_TREE
	  && TREE_CODE (TREE_OPERAND (base[i], 0)) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (TREE_OPERAND (base[i], 0));
	  if (is_gimple_assign (def_stmt)
	      && gimple_assign_rhs_code (def_stmt) == POINTER_PLUS_EXPR
	      && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == ADDR_EXPR
	      && TREE_CODE (gimple_assign_rhs2 (def_stmt)) == SSA_NAME)
	    {
	      if (maybe_ne (mem_ref_offset (base[i]), 0))
		return false;
	      base[i] = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 0);
	      offset[i] = gimple_assign_rhs2 (def_stmt);
	    }
	}
    }

  if (!operand_equal_p (base[0], base[1], 0))
    return false;
  if (maybe_ne (bitsize[0], bitsize[1]))
    return false;
  if (offset[0] != offset[1])
    {
      if (!offset[0] || !offset[1])
	return false;
      if (!operand_equal_p (offset[0], offset[1], 0))
	{
	  tree step[2];
	  for (int i = 0; i < 2; ++i)
	    {
	      step[i] = integer_one_node;
	      if (TREE_CODE (offset[i]) == SSA_NAME)
		{
		  gimple *def_stmt = SSA_NAME_DEF_STMT (offset[i]);
		  if (is_gimple_assign (def_stmt)
		      && gimple_assign_rhs_code (def_stmt) == MULT_EXPR
		      && (TREE_CODE (gimple_assign_rhs2 (def_stmt))
			  == INTEGER_CST))
		    {
		      step[i] = gimple_assign_rhs2 (def_stmt);
		      offset[i] = gimple_assign_rhs1 (def_stmt);
		    }
		}
	      else if (TREE_CODE (offset[i]) == MULT_EXPR)
		{
		  step[i] = TREE_OPERAND (offset[i], 1);
		  offset[i] = TREE_OPERAND (offset[i], 0);
		}
	      tree rhs1 = NULL_TREE;
	      if (TREE_CODE (offset[i]) == SSA_NAME)
		{
		  gimple *def_stmt = SSA_NAME_DEF_STMT (offset[i]);
		  if (gimple_assign_cast_p (def_stmt))
		    rhs1 = gimple_assign_rhs1 (def_stmt);
		}
	      else if (CONVERT_EXPR_P (offset[i]))
		rhs1 = TREE_OPERAND (offset[i], 0);
	      if (rhs1
		  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		  && INTEGRAL_TYPE_P (TREE_TYPE (offset[i]))
		  && (TYPE_PRECISION (TREE_TYPE (offset[i]))
		      >= TYPE_PRECISION (TREE_TYPE (rhs1))))
		offset[i] = rhs1;
	    }
	  if (!operand_equal_p (offset[0], offset[1], 0)
	      || !operand_equal_p (step[0], step[1], 0))
	    return false;
	}
    }
  return true;
}


enum scan_store_kind {
  /* Normal permutation.  */
  scan_store_kind_perm,

  /* Whole vector left shift permutation with zero init.  */
  scan_store_kind_lshift_zero,

  /* Whole vector left shift permutation and VEC_COND_EXPR.  */
  scan_store_kind_lshift_cond
};

/* Function check_scan_store.

   Verify if we can perform the needed permutations or whole vector shifts.
   Return -1 on failure, otherwise exact log2 of vectype's nunits.
   USE_WHOLE_VECTOR is a vector of enum scan_store_kind which operation
   to do at each step.  */

static int
scan_store_can_perm_p (tree vectype, tree init,
		       vec<enum scan_store_kind> *use_whole_vector = NULL)
{
  enum machine_mode vec_mode = TYPE_MODE (vectype);
  unsigned HOST_WIDE_INT nunits;
  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&nunits))
    return -1;
  int units_log2 = exact_log2 (nunits);
  if (units_log2 <= 0)
    return -1;

  int i;
  enum scan_store_kind whole_vector_shift_kind = scan_store_kind_perm;
  for (i = 0; i <= units_log2; ++i)
    {
      unsigned HOST_WIDE_INT j, k;
      enum scan_store_kind kind = scan_store_kind_perm;
      vec_perm_builder sel (nunits, nunits, 1);
      sel.quick_grow (nunits);
      if (i == units_log2)
	{
	  for (j = 0; j < nunits; ++j)
	    sel[j] = nunits - 1;
	}
      else
	{
	  for (j = 0; j < (HOST_WIDE_INT_1U << i); ++j)
	    sel[j] = j;
	  for (k = 0; j < nunits; ++j, ++k)
	    sel[j] = nunits + k;
	}
      vec_perm_indices indices (sel, i == units_log2 ? 1 : 2, nunits);
      if (!can_vec_perm_const_p (vec_mode, vec_mode, indices))
	{
	  if (i == units_log2)
	    return -1;

	  if (whole_vector_shift_kind == scan_store_kind_perm)
	    {
	      if (optab_handler (vec_shl_optab, vec_mode) == CODE_FOR_nothing)
		return -1;
	      whole_vector_shift_kind = scan_store_kind_lshift_zero;
	      /* Whole vector shifts shift in zeros, so if init is all zero
		 constant, there is no need to do anything further.  */
	      if ((TREE_CODE (init) != INTEGER_CST
		   && TREE_CODE (init) != REAL_CST)
		  || !initializer_zerop (init))
		{
		  tree masktype = truth_type_for (vectype);
		  if (!expand_vec_cond_expr_p (vectype, masktype))
		    return -1;
		  whole_vector_shift_kind = scan_store_kind_lshift_cond;
		}
	    }
	  kind = whole_vector_shift_kind;
	}
      if (use_whole_vector)
	{
	  if (kind != scan_store_kind_perm && use_whole_vector->is_empty ())
	    use_whole_vector->safe_grow_cleared (i, true);
	  if (kind != scan_store_kind_perm || !use_whole_vector->is_empty ())
	    use_whole_vector->safe_push (kind);
	}
    }

  return units_log2;
}


/* Function check_scan_store.

   Check magic stores for #pragma omp scan {in,ex}clusive reductions.  */

static bool
check_scan_store (vec_info *vinfo, stmt_vec_info stmt_info, tree vectype,
		  enum vect_def_type rhs_dt, slp_tree slp_node, tree mask,
		  vect_memory_access_type memory_access_type)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  tree ref_type;

  gcc_assert (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) > 1);
  if ((slp_node && SLP_TREE_LANES (slp_node) > 1)
      || mask
      || memory_access_type != VMAT_CONTIGUOUS
      || TREE_CODE (DR_BASE_ADDRESS (dr_info->dr)) != ADDR_EXPR
      || !VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (dr_info->dr), 0))
      || loop_vinfo == NULL
      || LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
      || STMT_VINFO_GROUPED_ACCESS (stmt_info)
      || !integer_zerop (get_dr_vinfo_offset (vinfo, dr_info))
      || !integer_zerop (DR_INIT (dr_info->dr))
      || !(ref_type = reference_alias_ptr_type (DR_REF (dr_info->dr)))
      || !alias_sets_conflict_p (get_alias_set (vectype),
				 get_alias_set (TREE_TYPE (ref_type))))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "unsupported OpenMP scan store.\n");
      return false;
    }

  /* We need to pattern match code built by OpenMP lowering and simplified
     by following optimizations into something we can handle.
     #pragma omp simd reduction(inscan,+:r)
     for (...)
       {
	 r += something ();
	 #pragma omp scan inclusive (r)
	 use (r);
       }
     shall have body with:
       // Initialization for input phase, store the reduction initializer:
       _20 = .GOMP_SIMD_LANE (simduid.3_14(D), 0);
       _21 = .GOMP_SIMD_LANE (simduid.3_14(D), 1);
       D.2042[_21] = 0;
       // Actual input phase:
       ...
       r.0_5 = D.2042[_20];
       _6 = _4 + r.0_5;
       D.2042[_20] = _6;
       // Initialization for scan phase:
       _25 = .GOMP_SIMD_LANE (simduid.3_14(D), 2);
       _26 = D.2043[_25];
       _27 = D.2042[_25];
       _28 = _26 + _27;
       D.2043[_25] = _28;
       D.2042[_25] = _28;
       // Actual scan phase:
       ...
       r.1_8 = D.2042[_20];
       ...
     The "omp simd array" variable D.2042 holds the privatized copy used
     inside of the loop and D.2043 is another one that holds copies of
     the current original list item.  The separate GOMP_SIMD_LANE ifn
     kinds are there in order to allow optimizing the initializer store
     and combiner sequence, e.g. if it is originally some C++ish user
     defined reduction, but allow the vectorizer to pattern recognize it
     and turn into the appropriate vectorized scan.

     For exclusive scan, this is slightly different:
     #pragma omp simd reduction(inscan,+:r)
     for (...)
       {
	 use (r);
	 #pragma omp scan exclusive (r)
	 r += something ();
       }
     shall have body with:
       // Initialization for input phase, store the reduction initializer:
       _20 = .GOMP_SIMD_LANE (simduid.3_14(D), 0);
       _21 = .GOMP_SIMD_LANE (simduid.3_14(D), 1);
       D.2042[_21] = 0;
       // Actual input phase:
       ...
       r.0_5 = D.2042[_20];
       _6 = _4 + r.0_5;
       D.2042[_20] = _6;
       // Initialization for scan phase:
       _25 = .GOMP_SIMD_LANE (simduid.3_14(D), 3);
       _26 = D.2043[_25];
       D.2044[_25] = _26;
       _27 = D.2042[_25];
       _28 = _26 + _27;
       D.2043[_25] = _28;
       // Actual scan phase:
       ...
       r.1_8 = D.2044[_20];
       ...  */

  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 2)
    {
      /* Match the D.2042[_21] = 0; store above.  Just require that
	 it is a constant or external definition store.  */
      if (rhs_dt != vect_constant_def && rhs_dt != vect_external_def)
	{
	 fail_init:
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported OpenMP scan initializer store.\n");
	  return false;
	}

      if (! loop_vinfo->scan_map)
	loop_vinfo->scan_map = new hash_map<tree, tree>;
      tree var = TREE_OPERAND (DR_BASE_ADDRESS (dr_info->dr), 0);
      tree &cached = loop_vinfo->scan_map->get_or_insert (var);
      if (cached)
	goto fail_init;
      cached = gimple_assign_rhs1 (STMT_VINFO_STMT (stmt_info));

      /* These stores can be vectorized normally.  */
      return true;
    }

  if (rhs_dt != vect_internal_def)
    {
     fail:
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "unsupported OpenMP scan combiner pattern.\n");
      return false;
    }

  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  tree rhs = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (rhs) != SSA_NAME)
    goto fail;

  gimple *other_store_stmt = NULL;
  tree var = TREE_OPERAND (DR_BASE_ADDRESS (dr_info->dr), 0);
  bool inscan_var_store
    = lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var)) != NULL;

  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4)
    {
      if (!inscan_var_store)
	{
	  use_operand_p use_p;
	  imm_use_iterator iter;
	  FOR_EACH_IMM_USE_FAST (use_p, iter, rhs)
	    {
	      gimple *use_stmt = USE_STMT (use_p);
	      if (use_stmt == stmt || is_gimple_debug (use_stmt))
		continue;
	      if (gimple_bb (use_stmt) != gimple_bb (stmt)
		  || !is_gimple_assign (use_stmt)
		  || gimple_assign_rhs_class (use_stmt) != GIMPLE_BINARY_RHS
		  || other_store_stmt
		  || TREE_CODE (gimple_assign_lhs (use_stmt)) != SSA_NAME)
		goto fail;
	      other_store_stmt = use_stmt;
	    }
	  if (other_store_stmt == NULL)
	    goto fail;
	  rhs = gimple_assign_lhs (other_store_stmt);
	  if (!single_imm_use (rhs, &use_p, &other_store_stmt))
	    goto fail;
	}
    }
  else if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 3)
    {
      use_operand_p use_p;
      imm_use_iterator iter;
      FOR_EACH_IMM_USE_FAST (use_p, iter, rhs)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (use_stmt == stmt || is_gimple_debug (use_stmt))
	    continue;
	  if (other_store_stmt)
	    goto fail;
	  other_store_stmt = use_stmt;
	}
    }
  else
    goto fail;

  gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
  if (gimple_bb (def_stmt) != gimple_bb (stmt)
      || !is_gimple_assign (def_stmt)
      || gimple_assign_rhs_class (def_stmt) != GIMPLE_BINARY_RHS)
    goto fail;

  enum tree_code code = gimple_assign_rhs_code (def_stmt);
  /* For pointer addition, we should use the normal plus for the vector
     operation.  */
  switch (code)
    {
    case POINTER_PLUS_EXPR:
      code = PLUS_EXPR;
      break;
    case MULT_HIGHPART_EXPR:
      goto fail;
    default:
      break;
    }
  if (TREE_CODE_LENGTH (code) != binary_op || !commutative_tree_code (code))
    goto fail;

  tree rhs1 = gimple_assign_rhs1 (def_stmt);
  tree rhs2 = gimple_assign_rhs2 (def_stmt);
  if (TREE_CODE (rhs1) != SSA_NAME || TREE_CODE (rhs2) != SSA_NAME)
    goto fail;

  gimple *load1_stmt = SSA_NAME_DEF_STMT (rhs1);
  gimple *load2_stmt = SSA_NAME_DEF_STMT (rhs2);
  if (gimple_bb (load1_stmt) != gimple_bb (stmt)
      || !gimple_assign_load_p (load1_stmt)
      || gimple_bb (load2_stmt) != gimple_bb (stmt)
      || !gimple_assign_load_p (load2_stmt))
    goto fail;

  stmt_vec_info load1_stmt_info = loop_vinfo->lookup_stmt (load1_stmt);
  stmt_vec_info load2_stmt_info = loop_vinfo->lookup_stmt (load2_stmt);
  if (load1_stmt_info == NULL
      || load2_stmt_info == NULL
      || (STMT_VINFO_SIMD_LANE_ACCESS_P (load1_stmt_info)
	  != STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info))
      || (STMT_VINFO_SIMD_LANE_ACCESS_P (load2_stmt_info)
	  != STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info)))
    goto fail;

  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4 && inscan_var_store)
    {
      dr_vec_info *load1_dr_info = STMT_VINFO_DR_INFO (load1_stmt_info);
      if (TREE_CODE (DR_BASE_ADDRESS (load1_dr_info->dr)) != ADDR_EXPR
	  || !VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (load1_dr_info->dr), 0)))
	goto fail;
      tree var1 = TREE_OPERAND (DR_BASE_ADDRESS (load1_dr_info->dr), 0);
      tree lrhs;
      if (lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var1)))
	lrhs = rhs1;
      else
	lrhs = rhs2;
      use_operand_p use_p;
      imm_use_iterator iter;
      FOR_EACH_IMM_USE_FAST (use_p, iter, lrhs)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (use_stmt == def_stmt || is_gimple_debug (use_stmt))
	    continue;
	  if (other_store_stmt)
	    goto fail;
	  other_store_stmt = use_stmt;
	}
    }

  if (other_store_stmt == NULL)
    goto fail;
  if (gimple_bb (other_store_stmt) != gimple_bb (stmt)
      || !gimple_store_p (other_store_stmt))
    goto fail;

  stmt_vec_info other_store_stmt_info
    = loop_vinfo->lookup_stmt (other_store_stmt);
  if (other_store_stmt_info == NULL
      || (STMT_VINFO_SIMD_LANE_ACCESS_P (other_store_stmt_info)
	  != STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info)))
    goto fail;

  gimple *stmt1 = stmt;
  gimple *stmt2 = other_store_stmt;
  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4 && !inscan_var_store)
    std::swap (stmt1, stmt2);
  if (scan_operand_equal_p (gimple_assign_lhs (stmt1),
			    gimple_assign_rhs1 (load2_stmt)))
    {
      std::swap (rhs1, rhs2);
      std::swap (load1_stmt, load2_stmt);
      std::swap (load1_stmt_info, load2_stmt_info);
    }
  if (!scan_operand_equal_p (gimple_assign_lhs (stmt1),
			     gimple_assign_rhs1 (load1_stmt)))
    goto fail;

  tree var3 = NULL_TREE;
  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 3
      && !scan_operand_equal_p (gimple_assign_lhs (stmt2),
				gimple_assign_rhs1 (load2_stmt)))
    goto fail;
  else if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4)
    {
      dr_vec_info *load2_dr_info = STMT_VINFO_DR_INFO (load2_stmt_info);
      if (TREE_CODE (DR_BASE_ADDRESS (load2_dr_info->dr)) != ADDR_EXPR
	  || !VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (load2_dr_info->dr), 0)))
	goto fail;
      var3 = TREE_OPERAND (DR_BASE_ADDRESS (load2_dr_info->dr), 0);
      if (!lookup_attribute ("omp simd array", DECL_ATTRIBUTES (var3))
	  || lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var3))
	  || lookup_attribute ("omp simd inscan exclusive",
			       DECL_ATTRIBUTES (var3)))
	goto fail;
    }

  dr_vec_info *other_dr_info = STMT_VINFO_DR_INFO (other_store_stmt_info);
  if (TREE_CODE (DR_BASE_ADDRESS (other_dr_info->dr)) != ADDR_EXPR
      || !VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (other_dr_info->dr), 0)))
    goto fail;

  tree var1 = TREE_OPERAND (DR_BASE_ADDRESS (dr_info->dr), 0);
  tree var2 = TREE_OPERAND (DR_BASE_ADDRESS (other_dr_info->dr), 0);
  if (!lookup_attribute ("omp simd array", DECL_ATTRIBUTES (var1))
      || !lookup_attribute ("omp simd array", DECL_ATTRIBUTES (var2))
      || (!lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var1)))
	 == (!lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var2))))
    goto fail;

  if (lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var1)))
    std::swap (var1, var2);

  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4)
    {
      if (!lookup_attribute ("omp simd inscan exclusive",
			     DECL_ATTRIBUTES (var1)))
	goto fail;
      var1 = var3;
    }

  if (loop_vinfo->scan_map == NULL)
    goto fail;
  tree *init = loop_vinfo->scan_map->get (var1);
  if (init == NULL)
    goto fail;

  /* The IL is as expected, now check if we can actually vectorize it.
     Inclusive scan:
       _26 = D.2043[_25];
       _27 = D.2042[_25];
       _28 = _26 + _27;
       D.2043[_25] = _28;
       D.2042[_25] = _28;
     should be vectorized as (where _40 is the vectorized rhs
     from the D.2042[_21] = 0; store):
       _30 = MEM <vector(8) int> [(int *)&D.2043];
       _31 = MEM <vector(8) int> [(int *)&D.2042];
       _32 = VEC_PERM_EXPR <_40, _31, { 0, 8, 9, 10, 11, 12, 13, 14 }>;
       _33 = _31 + _32;
       // _33 = { _31[0], _31[0]+_31[1], _31[1]+_31[2], ..., _31[6]+_31[7] };
       _34 = VEC_PERM_EXPR <_40, _33, { 0, 1, 8, 9, 10, 11, 12, 13 }>;
       _35 = _33 + _34;
       // _35 = { _31[0], _31[0]+_31[1], _31[0]+.._31[2], _31[0]+.._31[3],
       //         _31[1]+.._31[4], ... _31[4]+.._31[7] };
       _36 = VEC_PERM_EXPR <_40, _35, { 0, 1, 2, 3, 8, 9, 10, 11 }>;
       _37 = _35 + _36;
       // _37 = { _31[0], _31[0]+_31[1], _31[0]+.._31[2], _31[0]+.._31[3],
       //         _31[0]+.._31[4], ... _31[0]+.._31[7] };
       _38 = _30 + _37;
       _39 = VEC_PERM_EXPR <_38, _38, { 7, 7, 7, 7, 7, 7, 7, 7 }>;
       MEM <vector(8) int> [(int *)&D.2043] = _39;
       MEM <vector(8) int> [(int *)&D.2042] = _38;
     Exclusive scan:
       _26 = D.2043[_25];
       D.2044[_25] = _26;
       _27 = D.2042[_25];
       _28 = _26 + _27;
       D.2043[_25] = _28;
     should be vectorized as (where _40 is the vectorized rhs
     from the D.2042[_21] = 0; store):
       _30 = MEM <vector(8) int> [(int *)&D.2043];
       _31 = MEM <vector(8) int> [(int *)&D.2042];
       _32 = VEC_PERM_EXPR <_40, _31, { 0, 8, 9, 10, 11, 12, 13, 14 }>;
       _33 = VEC_PERM_EXPR <_40, _32, { 0, 8, 9, 10, 11, 12, 13, 14 }>;
       _34 = _32 + _33;
       // _34 = { 0, _31[0], _31[0]+_31[1], _31[1]+_31[2], _31[2]+_31[3],
       //         _31[3]+_31[4], ... _31[5]+.._31[6] };
       _35 = VEC_PERM_EXPR <_40, _34, { 0, 1, 8, 9, 10, 11, 12, 13 }>;
       _36 = _34 + _35;
       // _36 = { 0, _31[0], _31[0]+_31[1], _31[0]+.._31[2], _31[0]+.._31[3],
       //         _31[1]+.._31[4], ... _31[3]+.._31[6] };
       _37 = VEC_PERM_EXPR <_40, _36, { 0, 1, 2, 3, 8, 9, 10, 11 }>;
       _38 = _36 + _37;
       // _38 = { 0, _31[0], _31[0]+_31[1], _31[0]+.._31[2], _31[0]+.._31[3],
       //         _31[0]+.._31[4], ... _31[0]+.._31[6] };
       _39 = _30 + _38;
       _50 = _31 + _39;
       _51 = VEC_PERM_EXPR <_50, _50, { 7, 7, 7, 7, 7, 7, 7, 7 }>;
       MEM <vector(8) int> [(int *)&D.2044] = _39;
       MEM <vector(8) int> [(int *)&D.2042] = _51;  */
  enum machine_mode vec_mode = TYPE_MODE (vectype);
  optab optab = optab_for_tree_code (code, vectype, optab_default);
  if (!optab || optab_handler (optab, vec_mode) == CODE_FOR_nothing)
    goto fail;

  int units_log2 = scan_store_can_perm_p (vectype, *init);
  if (units_log2 == -1)
    goto fail;

  return true;
}


/* Function vectorizable_scan_store.

   Helper of vectorizable_score, arguments like on vectorizable_store.
   Handle only the transformation, checking is done in check_scan_store.  */

static bool
vectorizable_scan_store (vec_info *vinfo, stmt_vec_info stmt_info,
			 slp_tree slp_node, gimple_stmt_iterator *gsi,
			 gimple **vec_stmt, int ncopies)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  tree ref_type = reference_alias_ptr_type (DR_REF (dr_info->dr));
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "transform scan store. ncopies = %d\n", ncopies);

  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  tree rhs = gimple_assign_rhs1 (stmt);
  gcc_assert (TREE_CODE (rhs) == SSA_NAME);

  tree var = TREE_OPERAND (DR_BASE_ADDRESS (dr_info->dr), 0);
  bool inscan_var_store
    = lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var)) != NULL;

  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4 && !inscan_var_store)
    {
      use_operand_p use_p;
      imm_use_iterator iter;
      FOR_EACH_IMM_USE_FAST (use_p, iter, rhs)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (use_stmt == stmt || is_gimple_debug (use_stmt))
	    continue;
	  rhs = gimple_assign_lhs (use_stmt);
	  break;
	}
    }

  gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
  enum tree_code code = gimple_assign_rhs_code (def_stmt);
  if (code == POINTER_PLUS_EXPR)
    code = PLUS_EXPR;
  gcc_assert (TREE_CODE_LENGTH (code) == binary_op
	      && commutative_tree_code (code));
  tree rhs1 = gimple_assign_rhs1 (def_stmt);
  tree rhs2 = gimple_assign_rhs2 (def_stmt);
  gcc_assert (TREE_CODE (rhs1) == SSA_NAME && TREE_CODE (rhs2) == SSA_NAME);
  gimple *load1_stmt = SSA_NAME_DEF_STMT (rhs1);
  gimple *load2_stmt = SSA_NAME_DEF_STMT (rhs2);
  stmt_vec_info load1_stmt_info = loop_vinfo->lookup_stmt (load1_stmt);
  stmt_vec_info load2_stmt_info = loop_vinfo->lookup_stmt (load2_stmt);
  dr_vec_info *load1_dr_info = STMT_VINFO_DR_INFO (load1_stmt_info);
  dr_vec_info *load2_dr_info = STMT_VINFO_DR_INFO (load2_stmt_info);
  tree var1 = TREE_OPERAND (DR_BASE_ADDRESS (load1_dr_info->dr), 0);
  tree var2 = TREE_OPERAND (DR_BASE_ADDRESS (load2_dr_info->dr), 0);

  if (lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var1)))
    {
      std::swap (rhs1, rhs2);
      std::swap (var1, var2);
      std::swap (load1_dr_info, load2_dr_info);
    }

  tree *init = loop_vinfo->scan_map->get (var1);
  gcc_assert (init);

  unsigned HOST_WIDE_INT nunits;
  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&nunits))
    gcc_unreachable ();
  auto_vec<enum scan_store_kind, 16> use_whole_vector;
  int units_log2 = scan_store_can_perm_p (vectype, *init, &use_whole_vector);
  gcc_assert (units_log2 > 0);
  auto_vec<tree, 16> perms;
  perms.quick_grow (units_log2 + 1);
  tree zero_vec = NULL_TREE, masktype = NULL_TREE;
  for (int i = 0; i <= units_log2; ++i)
    {
      unsigned HOST_WIDE_INT j, k;
      vec_perm_builder sel (nunits, nunits, 1);
      sel.quick_grow (nunits);
      if (i == units_log2)
	for (j = 0; j < nunits; ++j)
	  sel[j] = nunits - 1;
      else
	{
	  for (j = 0; j < (HOST_WIDE_INT_1U << i); ++j)
	    sel[j] = j;
	  for (k = 0; j < nunits; ++j, ++k)
	    sel[j] = nunits + k;
	}
      vec_perm_indices indices (sel, i == units_log2 ? 1 : 2, nunits);
      if (!use_whole_vector.is_empty ()
	  && use_whole_vector[i] != scan_store_kind_perm)
	{
	  if (zero_vec == NULL_TREE)
	    zero_vec = build_zero_cst (vectype);
	  if (masktype == NULL_TREE
	      && use_whole_vector[i] == scan_store_kind_lshift_cond)
	    masktype = truth_type_for (vectype);
	  perms[i] = vect_gen_perm_mask_any (vectype, indices);
	}
      else
	perms[i] = vect_gen_perm_mask_checked (vectype, indices);
    }

  tree vec_oprnd1 = NULL_TREE;
  tree vec_oprnd2 = NULL_TREE;
  tree vec_oprnd3 = NULL_TREE;
  tree dataref_ptr = DR_BASE_ADDRESS (dr_info->dr);
  tree dataref_offset = build_int_cst (ref_type, 0);
  tree bump = vect_get_data_ptr_increment (vinfo, gsi, dr_info,
					   vectype, VMAT_CONTIGUOUS);
  tree ldataref_ptr = NULL_TREE;
  tree orig = NULL_TREE;
  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4 && !inscan_var_store)
    ldataref_ptr = DR_BASE_ADDRESS (load1_dr_info->dr);
  /* The initialization is invariant.  */
  vec_oprnd1 = vect_init_vector (vinfo, stmt_info, *init, vectype, NULL);
  auto_vec<tree> vec_oprnds2;
  auto_vec<tree> vec_oprnds3;
  if (ldataref_ptr == NULL)
    {
      /* We want to lookup the vector operands of the reduction, not those
	 of the store - for SLP we have to use the proper SLP node for the
	 lookup, which should be the single child of the scan store.  */
      vect_get_vec_defs (vinfo, stmt_info, SLP_TREE_CHILDREN (slp_node)[0],
			 ncopies, rhs1, &vec_oprnds2, rhs2, &vec_oprnds3);
      /* ???  For SLP we do not key the def on 'rhs1' or 'rhs2' but get
	 them in SLP child order.  So we have to swap here with logic
	 similar to above.  */
      stmt_vec_info load
	= SLP_TREE_SCALAR_STMTS (SLP_TREE_CHILDREN
				   (SLP_TREE_CHILDREN (slp_node)[0])[0])[0];
      dr_vec_info *dr_info = STMT_VINFO_DR_INFO (load);
      tree var = TREE_OPERAND (DR_BASE_ADDRESS (dr_info->dr), 0);
      if (lookup_attribute ("omp simd inscan", DECL_ATTRIBUTES (var)))
	for (unsigned i = 0; i < vec_oprnds2.length (); ++i)
	  std::swap (vec_oprnds2[i], vec_oprnds3[i]);;
    }
  else
    vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		       rhs2, &vec_oprnds3);
  for (unsigned j = 0; j < vec_oprnds3.length (); j++)
    {
      if (ldataref_ptr == NULL)
	vec_oprnd2 = vec_oprnds2[j];
      vec_oprnd3 = vec_oprnds3[j];
      if (j == 0)
	orig = vec_oprnd3;
      else if (!inscan_var_store)
	dataref_offset = int_const_binop (PLUS_EXPR, dataref_offset, bump);

      if (ldataref_ptr)
	{
	  vec_oprnd2 = make_ssa_name (vectype);
	  tree data_ref = fold_build2 (MEM_REF, vectype,
				       unshare_expr (ldataref_ptr),
				       dataref_offset);
	  vect_copy_ref_info (data_ref, DR_REF (load1_dr_info->dr));
	  gimple *g = gimple_build_assign (vec_oprnd2, data_ref);
	  vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
	  if (! slp_node)
	    {
	      STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);
	      *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];
	    }
	}

      tree v = vec_oprnd2;
      for (int i = 0; i < units_log2; ++i)
	{
	  tree new_temp = make_ssa_name (vectype);
	  gimple *g = gimple_build_assign (new_temp, VEC_PERM_EXPR,
					   (zero_vec
					    && (use_whole_vector[i]
						!= scan_store_kind_perm))
					   ? zero_vec : vec_oprnd1, v,
					   perms[i]);
	  vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
	  if (! slp_node)
	    {
	      STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);
	      *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];
	    }

	  if (zero_vec && use_whole_vector[i] == scan_store_kind_lshift_cond)
	    {
	      /* Whole vector shift shifted in zero bits, but if *init
		 is not initializer_zerop, we need to replace those elements
		 with elements from vec_oprnd1.  */
	      tree_vector_builder vb (masktype, nunits, 1);
	      for (unsigned HOST_WIDE_INT k = 0; k < nunits; ++k)
		vb.quick_push (k < (HOST_WIDE_INT_1U << i)
			       ? boolean_false_node : boolean_true_node);

	      tree new_temp2 = make_ssa_name (vectype);
	      g = gimple_build_assign (new_temp2, VEC_COND_EXPR, vb.build (),
				       new_temp, vec_oprnd1);
	      vect_finish_stmt_generation (vinfo, stmt_info,
							   g, gsi);
	      if (! slp_node)
		STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);
	      new_temp = new_temp2;
	    }

	  /* For exclusive scan, perform the perms[i] permutation once
	     more.  */
	  if (i == 0
	      && STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4
	      && v == vec_oprnd2)
	    {
	      v = new_temp;
	      --i;
	      continue;
	    }

	  tree new_temp2 = make_ssa_name (vectype);
	  g = gimple_build_assign (new_temp2, code, v, new_temp);
	  vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
	  if (! slp_node)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);

	  v = new_temp2;
	}

      tree new_temp = make_ssa_name (vectype);
      gimple *g = gimple_build_assign (new_temp, code, orig, v);
      vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
      if (! slp_node)
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);

      tree last_perm_arg = new_temp;
      /* For exclusive scan, new_temp computed above is the exclusive scan
	 prefix sum.  Turn it into inclusive prefix sum for the broadcast
	 of the last element into orig.  */
      if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) == 4)
	{
	  last_perm_arg = make_ssa_name (vectype);
	  g = gimple_build_assign (last_perm_arg, code, new_temp, vec_oprnd2);
	  vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
	  if (! slp_node)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);
	}

      orig = make_ssa_name (vectype);
      g = gimple_build_assign (orig, VEC_PERM_EXPR, last_perm_arg,
			       last_perm_arg, perms[units_log2]);
      vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
      if (! slp_node)
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);

      if (!inscan_var_store)
	{
	  tree data_ref = fold_build2 (MEM_REF, vectype,
				       unshare_expr (dataref_ptr),
				       dataref_offset);
	  vect_copy_ref_info (data_ref, DR_REF (dr_info->dr));
	  g = gimple_build_assign (data_ref, new_temp);
	  vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
	  if (! slp_node)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);
	}
    }

  if (inscan_var_store)
    for (unsigned j = 0; j < vec_oprnds3.length (); j++)
      {
	if (j != 0)
	  dataref_offset = int_const_binop (PLUS_EXPR, dataref_offset, bump);

	tree data_ref = fold_build2 (MEM_REF, vectype,
				     unshare_expr (dataref_ptr),
				     dataref_offset);
	vect_copy_ref_info (data_ref, DR_REF (dr_info->dr));
	gimple *g = gimple_build_assign (data_ref, orig);
	vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
	if (! slp_node)
	  STMT_VINFO_VEC_STMTS (stmt_info).safe_push (g);
      }
  return true;
}


/* Function vectorizable_store.

   Check if STMT_INFO defines a non scalar data-ref (array/pointer/structure)
   that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_store (vec_info *vinfo,
		    stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		    gimple **vec_stmt, slp_tree slp_node,
		    stmt_vector_for_cost *cost_vec)
{
  tree data_ref;
  tree vec_oprnd = NULL_TREE;
  tree elem_type;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  class loop *loop = NULL;
  machine_mode vec_mode;
  tree dummy;
  enum vect_def_type rhs_dt = vect_unknown_def_type;
  enum vect_def_type mask_dt = vect_unknown_def_type;
  tree dataref_ptr = NULL_TREE;
  tree dataref_offset = NULL_TREE;
  gimple *ptr_incr = NULL;
  int ncopies;
  int j;
  stmt_vec_info first_stmt_info;
  bool grouped_store;
  unsigned int group_size, i;
  bool slp = (slp_node != NULL);
  unsigned int vec_num;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
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
  slp_tree mask_node = NULL;
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

      int mask_index = internal_fn_mask_index (ifn);
      if (mask_index >= 0 && slp_node)
	mask_index = vect_slp_child_index_for_operand
		    (call, mask_index, STMT_VINFO_GATHER_SCATTER_P (stmt_info));
      if (mask_index >= 0
	  && !vect_check_scalar_mask (vinfo, stmt_info, slp_node, mask_index,
				      &mask, &mask_node, &mask_dt,
				      &mask_vectype))
	return false;
    }

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
  if (loop
      && nested_in_vect_loop_p (loop, stmt_info)
      && (ncopies > 1 || (slp && SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node) > 1)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "multiple types in nested loop.\n");
      return false;
    }

  tree op;
  slp_tree op_node;
  if (!vect_check_store_rhs (vinfo, stmt_info, slp_node,
			     &op, &op_node, &rhs_dt, &rhs_vectype, &vls_type))
    return false;

  elem_type = TREE_TYPE (vectype);
  vec_mode = TYPE_MODE (vectype);

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  vect_memory_access_type memory_access_type;
  enum dr_alignment_support alignment_support_scheme;
  int misalignment;
  poly_int64 poffset;
  internal_fn lanes_ifn;
  if (!get_load_store_type (vinfo, stmt_info, vectype, slp_node, mask, vls_type,
			    ncopies, &memory_access_type, &poffset,
			    &alignment_support_scheme, &misalignment, &gs_info,
			    &lanes_ifn))
    return false;

  if (slp_node
      && slp_node->ldst_lanes
      && memory_access_type != VMAT_LOAD_STORE_LANES)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "discovered store-lane but cannot use it.\n");
      return false;
    }

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
	       && (memory_access_type != VMAT_GATHER_SCATTER
		   || (gs_info.decl && !VECTOR_BOOLEAN_TYPE_P (mask_vectype))))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported access type for masked store.\n");
	  return false;
	}
      else if (memory_access_type == VMAT_GATHER_SCATTER
	       && gs_info.ifn == IFN_LAST
	       && !gs_info.decl)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported masked emulated scatter.\n");
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

  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) > 1 && !vec_stmt)
    {
      if (!check_scan_store (vinfo, stmt_info, vectype, rhs_dt, slp_node, mask,
			     memory_access_type))
	return false;
    }

  bool costing_p = !vec_stmt;
  if (costing_p) /* transformation not required.  */
    {
      STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) = memory_access_type;
      if (slp_node)
	SLP_TREE_MEMORY_ACCESS_TYPE (slp_node) = memory_access_type;

      if (loop_vinfo
	  && LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo))
	check_load_store_for_partial_vectors (loop_vinfo, vectype, slp_node,
					      vls_type, group_size,
					      memory_access_type, &gs_info,
					      mask);

      if (slp_node
	  && (!vect_maybe_update_slp_op_vectype (op_node, vectype)
	      || (mask
		  && !vect_maybe_update_slp_op_vectype (mask_node,
							mask_vectype))))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}

      if (dump_enabled_p ()
	  && memory_access_type != VMAT_ELEMENTWISE
	  && memory_access_type != VMAT_GATHER_SCATTER
	  && alignment_support_scheme != dr_aligned)
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Vectorizing an unaligned access.\n");

      STMT_VINFO_TYPE (stmt_info) = store_vec_info_type;

      /* As function vect_transform_stmt shows, for interleaving stores
	 the whole chain is vectorized when the last store in the chain
	 is reached, the other stores in the group are skipped.  So we
	 want to only cost the last one here, but it's not trivial to
	 get the last, as it's equivalent to use the first one for
	 costing, use the first one instead.  */
      if (grouped_store
	  && !slp
	  && first_stmt_info != stmt_info)
	return true;
    }
  if (slp_node)
    gcc_assert (memory_access_type == SLP_TREE_MEMORY_ACCESS_TYPE (stmt_info));
  else
    gcc_assert (memory_access_type == STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info));

  /* Transform.  */

  ensure_base_align (dr_info);

  if (STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) >= 3)
    {
      gcc_assert (memory_access_type == VMAT_CONTIGUOUS);
      gcc_assert (!slp || SLP_TREE_LANES (slp_node) == 1);
      if (costing_p)
	{
	  unsigned int inside_cost = 0, prologue_cost = 0;
	  if (vls_type == VLS_STORE_INVARIANT)
	    prologue_cost += record_stmt_cost (cost_vec, 1, scalar_to_vec,
					       stmt_info, 0, vect_prologue);
	  vect_get_store_cost (vinfo, stmt_info, ncopies,
			       alignment_support_scheme, misalignment,
			       &inside_cost, cost_vec);

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vect_model_store_cost: inside_cost = %d, "
			     "prologue_cost = %d .\n",
			     inside_cost, prologue_cost);

	  return true;
	}
      return vectorizable_scan_store (vinfo, stmt_info, slp_node,
				      gsi, vec_stmt, ncopies);
    }

  if (grouped_store || slp)
    {
      /* FORNOW */
      gcc_assert (!grouped_store
		  || !loop
		  || !nested_in_vect_loop_p (loop, stmt_info));

      if (slp)
        {
          grouped_store = false;
          /* VEC_NUM is the number of vect stmts to be created for this
             group.  */
          vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
	  first_stmt_info = SLP_TREE_SCALAR_STMTS (slp_node)[0];
	  gcc_assert (!STMT_VINFO_GROUPED_ACCESS (first_stmt_info)
		      || (DR_GROUP_FIRST_ELEMENT (first_stmt_info)
			  == first_stmt_info));
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

  if (!costing_p && dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform store. ncopies = %d\n",
		     ncopies);

  /* Check if we need to update prologue cost for invariant,
     and update it accordingly if so.  If it's not for
     interleaving store, we can just check vls_type; but if
     it's for interleaving store, need to check the def_type
     of the stored value since the current vls_type is just
     for first_stmt_info.  */
  auto update_prologue_cost = [&](unsigned *prologue_cost, tree store_rhs)
  {
    gcc_assert (costing_p);
    if (slp)
      return;
    if (grouped_store)
      {
	gcc_assert (store_rhs);
	enum vect_def_type cdt;
	gcc_assert (vect_is_simple_use (store_rhs, vinfo, &cdt));
	if (cdt != vect_constant_def && cdt != vect_external_def)
	  return;
      }
    else if (vls_type != VLS_STORE_INVARIANT)
      return;
    *prologue_cost += record_stmt_cost (cost_vec, 1, scalar_to_vec, stmt_info,
					0, vect_prologue);
  };

  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_STRIDED_SLP)
    {
      unsigned inside_cost = 0, prologue_cost = 0;
      gimple_stmt_iterator incr_gsi;
      bool insert_after;
      gimple *incr;
      tree offvar;
      tree ivstep;
      tree running_off;
      tree stride_base, stride_step, alias_off;
      tree vec_oprnd = NULL_TREE;
      tree dr_offset;
      unsigned int g;
      /* Checked by get_load_store_type.  */
      unsigned int const_nunits = nunits.to_constant ();

      gcc_assert (!LOOP_VINFO_FULLY_MASKED_P (loop_vinfo));
      gcc_assert (!nested_in_vect_loop_p (loop, stmt_info));

      dr_offset = get_dr_vinfo_offset (vinfo, first_dr_info);
      stride_base
	= fold_build_pointer_plus
	    (DR_BASE_ADDRESS (first_dr_info->dr),
	     size_binop (PLUS_EXPR,
			 convert_to_ptrofftype (dr_offset),
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
	  HOST_WIDE_INT n = gcd (group_size, const_nunits);
	  if (n == const_nunits)
	    {
	      int mis_align = dr_misalignment (first_dr_info, vectype);
	      dr_alignment_support dr_align
		= vect_supportable_dr_alignment (vinfo, dr_info, vectype,
						 mis_align);
	      if (dr_align == dr_aligned
		  || dr_align == dr_unaligned_supported)
		{
		  nstores = 1;
		  lnel = const_nunits;
		  ltype = vectype;
		  lvectype = vectype;
		  alignment_support_scheme = dr_align;
		  misalignment = mis_align;
		}
	    }
	  else if (n > 1)
	    {
	      nstores = const_nunits / n;
	      lnel = n;
	      ltype = build_vector_type (elem_type, n);
	      lvectype = vectype;

	      /* First check if vec_extract optab doesn't support extraction
		 of vector elts directly.  */
	      scalar_mode elmode = SCALAR_TYPE_MODE (elem_type);
	      machine_mode vmode;
	      if (!VECTOR_MODE_P (TYPE_MODE (vectype))
		  || !related_vector_mode (TYPE_MODE (vectype), elmode,
					   n).exists (&vmode)
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
		    = n * GET_MODE_BITSIZE (elmode);
		  unsigned int lnunits = const_nunits / n;
		  /* If we can't construct such a vector fall back to
		     element extracts from the original vector type and
		     element size stores.  */
		  if (int_mode_for_size (lsize, 0).exists (&elmode)
		      && VECTOR_MODE_P (TYPE_MODE (vectype))
		      && related_vector_mode (TYPE_MODE (vectype), elmode,
					      lnunits).exists (&vmode)
		      && (convert_optab_handler (vec_extract_optab,
						 vmode, elmode)
			  != CODE_FOR_nothing))
		    {
		      nstores = lnunits;
		      lnel = n;
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
	  ltype = build_aligned_type (ltype, TYPE_ALIGN (elem_type));
	  ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
	}

      if (!costing_p)
	{
	  ivstep = stride_step;
	  ivstep = fold_build2 (MULT_EXPR, TREE_TYPE (ivstep), ivstep,
				build_int_cst (TREE_TYPE (ivstep), vf));

	  standard_iv_increment_position (loop, &incr_gsi, &insert_after);

	  stride_base = cse_and_gimplify_to_preheader (loop_vinfo, stride_base);
	  ivstep = cse_and_gimplify_to_preheader (loop_vinfo, ivstep);
	  create_iv (stride_base, PLUS_EXPR, ivstep, NULL, loop, &incr_gsi,
		     insert_after, &offvar, NULL);
	  incr = gsi_stmt (incr_gsi);

	  stride_step = cse_and_gimplify_to_preheader (loop_vinfo, stride_step);
	}

      alias_off = build_int_cst (ref_type, 0);
      stmt_vec_info next_stmt_info = first_stmt_info;
      auto_vec<tree> vec_oprnds;
      /* For costing some adjacent vector stores, we'd like to cost with
	 the total number of them once instead of cost each one by one. */
      unsigned int n_adjacent_stores = 0;
      for (g = 0; g < group_size; g++)
	{
	  running_off = offvar;
	  if (!costing_p)
	    {
	      if (g)
		{
		  tree size = TYPE_SIZE_UNIT (ltype);
		  tree pos
		    = fold_build2 (MULT_EXPR, sizetype, size_int (g), size);
		  tree newoff = copy_ssa_name (running_off, NULL);
		  incr = gimple_build_assign (newoff, POINTER_PLUS_EXPR,
					      running_off, pos);
		  vect_finish_stmt_generation (vinfo, stmt_info, incr, gsi);
		  running_off = newoff;
		}
	    }
	  if (!slp)
	    op = vect_get_store_rhs (next_stmt_info);
	  if (!costing_p)
	    vect_get_vec_defs (vinfo, next_stmt_info, slp_node, ncopies, op,
			       &vec_oprnds);
	  else
	    update_prologue_cost (&prologue_cost, op);
	  unsigned int group_el = 0;
	  unsigned HOST_WIDE_INT
	    elsz = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
	  for (j = 0; j < ncopies; j++)
	    {
	      if (!costing_p)
		{
		  vec_oprnd = vec_oprnds[j];
		  /* Pun the vector to extract from if necessary.  */
		  if (lvectype != vectype)
		    {
		      tree tem = make_ssa_name (lvectype);
		      tree cvt
			= build1 (VIEW_CONVERT_EXPR, lvectype, vec_oprnd);
		      gimple *pun = gimple_build_assign (tem, cvt);
		      vect_finish_stmt_generation (vinfo, stmt_info, pun, gsi);
		      vec_oprnd = tem;
		    }
		}
	      for (i = 0; i < nstores; i++)
		{
		  if (costing_p)
		    {
		      /* Only need vector extracting when there are more
			 than one stores.  */
		      if (nstores > 1)
			inside_cost
			  += record_stmt_cost (cost_vec, 1, vec_to_scalar,
					       stmt_info, 0, vect_body);
		      /* Take a single lane vector type store as scalar
			 store to avoid ICE like 110776.  */
		      if (VECTOR_TYPE_P (ltype)
			  && known_ne (TYPE_VECTOR_SUBPARTS (ltype), 1U))
			n_adjacent_stores++;
		      else
			inside_cost
			  += record_stmt_cost (cost_vec, 1, scalar_store,
					       stmt_info, 0, vect_body);
		      continue;
		    }
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
		  vect_finish_stmt_generation (vinfo, stmt_info, assign, gsi);

		  group_el += lnel;
		  if (! slp
		      || group_el == group_size)
		    {
		      newoff = copy_ssa_name (running_off, NULL);
		      incr = gimple_build_assign (newoff, POINTER_PLUS_EXPR,
						  running_off, stride_step);
		      vect_finish_stmt_generation (vinfo, stmt_info, incr, gsi);

		      running_off = newoff;
		      group_el = 0;
		    }
		  if (g == group_size - 1
		      && !slp)
		    {
		      if (j == 0 && i == 0)
			*vec_stmt = assign;
		      STMT_VINFO_VEC_STMTS (stmt_info).safe_push (assign);
		    }
		}
	    }
	  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
	  vec_oprnds.truncate(0);
	  if (slp)
	    break;
	}

      if (costing_p)
	{
	  if (n_adjacent_stores > 0)
	    vect_get_store_cost (vinfo, stmt_info, n_adjacent_stores,
				 alignment_support_scheme, misalignment,
				 &inside_cost, cost_vec);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vect_model_store_cost: inside_cost = %d, "
			     "prologue_cost = %d .\n",
			     inside_cost, prologue_cost);
	}

      return true;
    }

  gcc_assert (alignment_support_scheme);
  vec_loop_masks *loop_masks
    = (loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
       ? &LOOP_VINFO_MASKS (loop_vinfo)
       : NULL);
  vec_loop_lens *loop_lens
    = (loop_vinfo && LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo)
       ? &LOOP_VINFO_LENS (loop_vinfo)
       : NULL);

  /* The vect_transform_stmt and vect_analyze_stmt will go here but there
     are some difference here.  We cannot enable both the lens and masks
     during transform but it is allowed during analysis.
     Shouldn't go with length-based approach if fully masked.  */
  if (cost_vec == NULL)
    /* The cost_vec is NULL during transfrom.  */
    gcc_assert ((!loop_lens || !loop_masks));

  /* Targets with store-lane instructions must not require explicit
     realignment.  vect_supportable_dr_alignment always returns either
     dr_aligned or dr_unaligned_supported for masked operations.  */
  gcc_assert ((memory_access_type != VMAT_LOAD_STORE_LANES
	       && !mask
	       && !loop_masks)
	      || alignment_support_scheme == dr_aligned
	      || alignment_support_scheme == dr_unaligned_supported);

  tree offset = NULL_TREE;
  if (!known_eq (poffset, 0))
    offset = size_int (poffset);

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
      if (!costing_p)
	vect_get_strided_load_store_ops (stmt_info, loop_vinfo, gsi, &gs_info,
					 &bump, &vec_offset, loop_lens);
    }
  else
    {
      if (memory_access_type == VMAT_LOAD_STORE_LANES)
	aggr_type = build_array_type_nelts (elem_type, group_size * nunits);
      else
	aggr_type = vectype;
      if (!costing_p)
	bump = vect_get_data_ptr_increment (vinfo, gsi, dr_info, aggr_type,
					    memory_access_type, loop_lens);
    }

  if (mask && !costing_p)
    LOOP_VINFO_HAS_MASK_STORE (loop_vinfo) = true;

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  */

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

  auto_vec<tree> dr_chain (group_size);
  auto_vec<tree> vec_masks;
  tree vec_mask = NULL;
  auto_delete_vec<auto_vec<tree>> gvec_oprnds (group_size);
  for (i = 0; i < group_size; i++)
    gvec_oprnds.quick_push (new auto_vec<tree> ());

  if (memory_access_type == VMAT_LOAD_STORE_LANES)
    {
      if (costing_p && slp_node)
	/* Update all incoming store operand nodes, the general handling
	   above only handles the mask and the first store operand node.  */
	for (slp_tree child : SLP_TREE_CHILDREN (slp_node))
	  if (child != mask_node
	      && !vect_maybe_update_slp_op_vectype (child, vectype))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "incompatible vector types for invariants\n");
	      return false;
	    }
      unsigned inside_cost = 0, prologue_cost = 0;
      /* For costing some adjacent vector stores, we'd like to cost with
	 the total number of them once instead of cost each one by one. */
      unsigned int n_adjacent_stores = 0;
      if (slp)
	ncopies = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node) / group_size;
      for (j = 0; j < ncopies; j++)
	{
	  gimple *new_stmt;
	  if (j == 0)
	    {
	      /* For interleaved stores we collect vectorized defs for all
		 the stores in the group in DR_CHAIN. DR_CHAIN is then used
		 as an input to vect_permute_store_chain().  */
	      stmt_vec_info next_stmt_info = first_stmt_info;
	      for (i = 0; i < group_size; i++)
		{
		  /* Since gaps are not supported for interleaved stores,
		     DR_GROUP_SIZE is the exact number of stmts in the
		     chain. Therefore, NEXT_STMT_INFO can't be NULL_TREE.  */
		  op = vect_get_store_rhs (next_stmt_info);
		  if (costing_p)
		    update_prologue_cost (&prologue_cost, op);
		  else if (!slp)
		    {
		      vect_get_vec_defs_for_operand (vinfo, next_stmt_info,
						     ncopies, op,
						     gvec_oprnds[i]);
		      vec_oprnd = (*gvec_oprnds[i])[0];
		      dr_chain.quick_push (vec_oprnd);
		    }
		  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
		}

	      if (!costing_p)
		{
		  if (mask)
		    {
		      if (slp_node)
			vect_get_slp_defs (mask_node, &vec_masks);
		      else
			vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies,
						       mask, &vec_masks,
						       mask_vectype);
		      vec_mask = vec_masks[0];
		    }

		  dataref_ptr
		    = vect_create_data_ref_ptr (vinfo, first_stmt_info,
						aggr_type, NULL, offset, &dummy,
						gsi, &ptr_incr, false, bump);
		}
	    }
	  else if (!costing_p)
	    {
	      gcc_assert (!LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo));
	      /* DR_CHAIN is then used as an input to
		 vect_permute_store_chain().  */
	      if (!slp)
		{
		  /* We should have caught mismatched types earlier.  */
		  gcc_assert (
		    useless_type_conversion_p (vectype, TREE_TYPE (vec_oprnd)));
		  for (i = 0; i < group_size; i++)
		    {
		      vec_oprnd = (*gvec_oprnds[i])[j];
		      dr_chain[i] = vec_oprnd;
		    }
		}
	      if (mask)
		vec_mask = vec_masks[j];
	      dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					     stmt_info, bump);
	    }

	  if (costing_p)
	    {
	      n_adjacent_stores += group_size;
	      continue;
	    }

	  /* Get an array into which we can store the individual vectors.  */
	  tree vec_array = create_vector_array (vectype, group_size);

	  /* Invalidate the current contents of VEC_ARRAY.  This should
	     become an RTL clobber too, which prevents the vector registers
	     from being upward-exposed.  */
	  vect_clobber_variable (vinfo, stmt_info, gsi, vec_array);

	  /* Store the individual vectors into the array.  */
	  for (i = 0; i < group_size; i++)
	    {
	      if (slp)
		{
		  slp_tree child;
		  if (i == 0 || !mask_node)
		    child = SLP_TREE_CHILDREN (slp_node)[i];
		  else
		    child = SLP_TREE_CHILDREN (slp_node)[i + 1];
		  vec_oprnd = SLP_TREE_VEC_DEFS (child)[j];
		}
	      else
		vec_oprnd = dr_chain[i];
	      write_vector_array (vinfo, stmt_info, gsi, vec_oprnd, vec_array,
				  i);
	    }

	  tree final_mask = NULL;
	  tree final_len = NULL;
	  tree bias = NULL;
	  if (loop_masks)
	    final_mask = vect_get_loop_mask (loop_vinfo, gsi, loop_masks,
					     ncopies, vectype, j);
	  if (vec_mask)
	    final_mask = prepare_vec_mask (loop_vinfo, mask_vectype, final_mask,
					   vec_mask, gsi);

	  if (lanes_ifn == IFN_MASK_LEN_STORE_LANES)
	    {
	      if (loop_lens)
		final_len = vect_get_loop_len (loop_vinfo, gsi, loop_lens,
					       ncopies, vectype, j, 1);
	      else
		final_len = size_int (TYPE_VECTOR_SUBPARTS (vectype));
	      signed char biasval
		= LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
	      bias = build_int_cst (intQI_type_node, biasval);
	      if (!final_mask)
		{
		  mask_vectype = truth_type_for (vectype);
		  final_mask = build_minus_one_cst (mask_vectype);
		}
	    }

	  gcall *call;
	  if (final_len && final_mask)
	    {
	      /* Emit:
		   MASK_LEN_STORE_LANES (DATAREF_PTR, ALIAS_PTR, VEC_MASK,
					 LEN, BIAS, VEC_ARRAY).  */
	      unsigned int align = TYPE_ALIGN (TREE_TYPE (vectype));
	      tree alias_ptr = build_int_cst (ref_type, align);
	      call = gimple_build_call_internal (IFN_MASK_LEN_STORE_LANES, 6,
						 dataref_ptr, alias_ptr,
						 final_mask, final_len, bias,
						 vec_array);
	    }
	  else if (final_mask)
	    {
	      /* Emit:
		   MASK_STORE_LANES (DATAREF_PTR, ALIAS_PTR, VEC_MASK,
				     VEC_ARRAY).  */
	      unsigned int align = TYPE_ALIGN (TREE_TYPE (vectype));
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
	      call = gimple_build_call_internal (IFN_STORE_LANES, 1, vec_array);
	      gimple_call_set_lhs (call, data_ref);
	    }
	  gimple_call_set_nothrow (call, true);
	  vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
	  new_stmt = call;

	  /* Record that VEC_ARRAY is now dead.  */
	  vect_clobber_variable (vinfo, stmt_info, gsi, vec_array);
	  if (j == 0 && !slp)
	    *vec_stmt = new_stmt;
	  if (!slp)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}

      if (costing_p)
	{
	  if (n_adjacent_stores > 0)
	    vect_get_store_cost (vinfo, stmt_info, n_adjacent_stores,
				 alignment_support_scheme, misalignment,
				 &inside_cost, cost_vec);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vect_model_store_cost: inside_cost = %d, "
			     "prologue_cost = %d .\n",
			     inside_cost, prologue_cost);
	}

      return true;
    }

  if (memory_access_type == VMAT_GATHER_SCATTER)
    {
      gcc_assert (!grouped_store);
      auto_vec<tree> vec_offsets;
      unsigned int inside_cost = 0, prologue_cost = 0;
      for (j = 0; j < ncopies; j++)
	{
	  gimple *new_stmt;
	  if (j == 0)
	    {
	      if (costing_p && vls_type == VLS_STORE_INVARIANT)
		prologue_cost += record_stmt_cost (cost_vec, 1, scalar_to_vec,
						   stmt_info, 0, vect_prologue);
	      else if (!costing_p)
		{
		  /* Since the store is not grouped, DR_GROUP_SIZE is 1, and
		     DR_CHAIN is of size 1.  */
		  gcc_assert (group_size == 1);
		  if (slp_node)
		    vect_get_slp_defs (op_node, gvec_oprnds[0]);
		  else
		    vect_get_vec_defs_for_operand (vinfo, first_stmt_info,
						   ncopies, op, gvec_oprnds[0]);
		  if (mask)
		    {
		      if (slp_node)
			vect_get_slp_defs (mask_node, &vec_masks);
		      else
			vect_get_vec_defs_for_operand (vinfo, stmt_info,
						       ncopies,
						       mask, &vec_masks,
						       mask_vectype);
		    }

		  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
		    vect_get_gather_scatter_ops (loop_vinfo, loop, stmt_info,
						 slp_node, &gs_info,
						 &dataref_ptr, &vec_offsets);
		  else
		    dataref_ptr
		      = vect_create_data_ref_ptr (vinfo, first_stmt_info,
						  aggr_type, NULL, offset,
						  &dummy, gsi, &ptr_incr, false,
						  bump);
		}
	    }
	  else if (!costing_p)
	    {
	      gcc_assert (!LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo));
	      if (!STMT_VINFO_GATHER_SCATTER_P (stmt_info))
		dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr,
					       gsi, stmt_info, bump);
	    }

	  new_stmt = NULL;
	  for (i = 0; i < vec_num; ++i)
	    {
	      if (!costing_p)
		{
		  vec_oprnd = (*gvec_oprnds[0])[vec_num * j + i];
		  if (mask)
		    vec_mask = vec_masks[vec_num * j + i];
		  /* We should have catched mismatched types earlier.  */
		  gcc_assert (useless_type_conversion_p (vectype,
							 TREE_TYPE (vec_oprnd)));
		}
	      unsigned HOST_WIDE_INT align;
	      tree final_mask = NULL_TREE;
	      tree final_len = NULL_TREE;
	      tree bias = NULL_TREE;
	      if (!costing_p)
		{
		  if (loop_masks)
		    final_mask = vect_get_loop_mask (loop_vinfo, gsi,
						     loop_masks, ncopies,
						     vectype, j);
		  if (vec_mask)
		    final_mask = prepare_vec_mask (loop_vinfo, mask_vectype,
						   final_mask, vec_mask, gsi);
		}

	      if (gs_info.ifn != IFN_LAST)
		{
		  if (costing_p)
		    {
		      unsigned int cnunits = vect_nunits_for_cost (vectype);
		      inside_cost
			  += record_stmt_cost (cost_vec, cnunits, scalar_store,
					       stmt_info, 0, vect_body);
		      continue;
		    }

		  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
		    vec_offset = vec_offsets[vec_num * j + i];
		  tree scale = size_int (gs_info.scale);

		  if (gs_info.ifn == IFN_MASK_LEN_SCATTER_STORE)
		    {
		      if (loop_lens)
			final_len = vect_get_loop_len (loop_vinfo, gsi,
						       loop_lens, ncopies,
						       vectype, j, 1);
		      else
			final_len = size_int (TYPE_VECTOR_SUBPARTS (vectype));
		      signed char biasval
			= LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
		      bias = build_int_cst (intQI_type_node, biasval);
		      if (!final_mask)
			{
			  mask_vectype = truth_type_for (vectype);
			  final_mask = build_minus_one_cst (mask_vectype);
			}
		    }

		  gcall *call;
		  if (final_len && final_mask)
		    call = gimple_build_call_internal
			     (IFN_MASK_LEN_SCATTER_STORE, 7, dataref_ptr,
			      vec_offset, scale, vec_oprnd, final_mask,
			      final_len, bias);
		  else if (final_mask)
		    call = gimple_build_call_internal
			     (IFN_MASK_SCATTER_STORE, 5, dataref_ptr,
			      vec_offset, scale, vec_oprnd, final_mask);
		  else
		    call = gimple_build_call_internal (IFN_SCATTER_STORE, 4,
						       dataref_ptr, vec_offset,
						       scale, vec_oprnd);
		  gimple_call_set_nothrow (call, true);
		  vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
		  new_stmt = call;
		}
	      else if (gs_info.decl)
		{
		  /* The builtin decls path for scatter is legacy, x86 only.  */
		  gcc_assert (nunits.is_constant ()
			      && (!final_mask
				  || SCALAR_INT_MODE_P
				       (TYPE_MODE (TREE_TYPE (final_mask)))));
		  if (costing_p)
		    {
		      unsigned int cnunits = vect_nunits_for_cost (vectype);
		      inside_cost
			+= record_stmt_cost (cost_vec, cnunits, scalar_store,
					     stmt_info, 0, vect_body);
		      continue;
		    }
		  poly_uint64 offset_nunits
		    = TYPE_VECTOR_SUBPARTS (gs_info.offset_vectype);
		  if (known_eq (nunits, offset_nunits))
		    {
		      new_stmt = vect_build_one_scatter_store_call
				   (vinfo, stmt_info, gsi, &gs_info,
				    dataref_ptr, vec_offsets[vec_num * j + i],
				    vec_oprnd, final_mask);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		    }
		  else if (known_eq (nunits, offset_nunits * 2))
		    {
		      /* We have a offset vector with half the number of
			 lanes but the builtins will store full vectype
			 data from the lower lanes.  */
		      new_stmt = vect_build_one_scatter_store_call
				   (vinfo, stmt_info, gsi, &gs_info,
				    dataref_ptr,
				    vec_offsets[2 * vec_num * j + 2 * i],
				    vec_oprnd, final_mask);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		      int count = nunits.to_constant ();
		      vec_perm_builder sel (count, count, 1);
		      sel.quick_grow (count);
		      for (int i = 0; i < count; ++i)
			sel[i] = i | (count / 2);
		      vec_perm_indices indices (sel, 2, count);
		      tree perm_mask
			= vect_gen_perm_mask_checked (vectype, indices);
		      new_stmt = gimple_build_assign (NULL_TREE, VEC_PERM_EXPR,
						      vec_oprnd, vec_oprnd,
						      perm_mask);
		      vec_oprnd = make_ssa_name (vectype);
		      gimple_set_lhs (new_stmt, vec_oprnd);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		      if (final_mask)
			{
			  new_stmt = gimple_build_assign (NULL_TREE,
							  VEC_UNPACK_HI_EXPR,
							  final_mask);
			  final_mask = make_ssa_name
				      (truth_type_for (gs_info.offset_vectype));
			  gimple_set_lhs (new_stmt, final_mask);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			}
		      new_stmt = vect_build_one_scatter_store_call
				   (vinfo, stmt_info, gsi, &gs_info,
				    dataref_ptr,
				    vec_offsets[2 * vec_num * j + 2 * i + 1],
				    vec_oprnd, final_mask);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		    }
		  else if (known_eq (nunits * 2, offset_nunits))
		    {
		      /* We have a offset vector with double the number of
			 lanes.  Select the low/high part accordingly.  */
		      vec_offset = vec_offsets[(vec_num * j + i) / 2];
		      if ((vec_num * j + i) & 1)
			{
			  int count = offset_nunits.to_constant ();
			  vec_perm_builder sel (count, count, 1);
			  sel.quick_grow (count);
			  for (int i = 0; i < count; ++i)
			    sel[i] = i | (count / 2);
			  vec_perm_indices indices (sel, 2, count);
			  tree perm_mask = vect_gen_perm_mask_checked
					     (TREE_TYPE (vec_offset), indices);
			  new_stmt = gimple_build_assign (NULL_TREE,
							  VEC_PERM_EXPR,
							  vec_offset,
							  vec_offset,
							  perm_mask);
			  vec_offset = make_ssa_name (TREE_TYPE (vec_offset));
			  gimple_set_lhs (new_stmt, vec_offset);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			}
		      new_stmt = vect_build_one_scatter_store_call
				   (vinfo, stmt_info, gsi, &gs_info,
				    dataref_ptr, vec_offset,
				    vec_oprnd, final_mask);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		    }
		  else
		    gcc_unreachable ();
		}
	      else
		{
		  /* Emulated scatter.  */
		  gcc_assert (!final_mask);
		  if (costing_p)
		    {
		      unsigned int cnunits = vect_nunits_for_cost (vectype);
		      /* For emulated scatter N offset vector element extracts
			 (we assume the scalar scaling and ptr + offset add is
			 consumed by the load).  */
		      inside_cost
			+= record_stmt_cost (cost_vec, cnunits, vec_to_scalar,
					     stmt_info, 0, vect_body);
		      /* N scalar stores plus extracting the elements.  */
		      inside_cost
			+= record_stmt_cost (cost_vec, cnunits, vec_to_scalar,
					     stmt_info, 0, vect_body);
		      inside_cost
			+= record_stmt_cost (cost_vec, cnunits, scalar_store,
					     stmt_info, 0, vect_body);
		      continue;
		    }

		  unsigned HOST_WIDE_INT const_nunits = nunits.to_constant ();
		  unsigned HOST_WIDE_INT const_offset_nunits
		    = TYPE_VECTOR_SUBPARTS (gs_info.offset_vectype).to_constant ();
		  vec<constructor_elt, va_gc> *ctor_elts;
		  vec_alloc (ctor_elts, const_nunits);
		  gimple_seq stmts = NULL;
		  tree elt_type = TREE_TYPE (vectype);
		  unsigned HOST_WIDE_INT elt_size
		    = tree_to_uhwi (TYPE_SIZE (elt_type));
		  /* We support offset vectors with more elements
		     than the data vector for now.  */
		  unsigned HOST_WIDE_INT factor
		    = const_offset_nunits / const_nunits;
		  vec_offset = vec_offsets[(vec_num * j + i) / factor];
		  unsigned elt_offset
		    = ((vec_num * j + i) % factor) * const_nunits;
		  tree idx_type = TREE_TYPE (TREE_TYPE (vec_offset));
		  tree scale = size_int (gs_info.scale);
		  align = get_object_alignment (DR_REF (first_dr_info->dr));
		  tree ltype = build_aligned_type (TREE_TYPE (vectype), align);
		  for (unsigned k = 0; k < const_nunits; ++k)
		    {
		      /* Compute the offsetted pointer.  */
		      tree boff = size_binop (MULT_EXPR, TYPE_SIZE (idx_type),
					      bitsize_int (k + elt_offset));
		      tree idx
			= gimple_build (&stmts, BIT_FIELD_REF, idx_type,
					vec_offset, TYPE_SIZE (idx_type), boff);
		      idx = gimple_convert (&stmts, sizetype, idx);
		      idx = gimple_build (&stmts, MULT_EXPR, sizetype,
					  idx, scale);
		      tree ptr
			= gimple_build (&stmts, PLUS_EXPR,
					TREE_TYPE (dataref_ptr),
					dataref_ptr, idx);
		      ptr = gimple_convert (&stmts, ptr_type_node, ptr);
		      /* Extract the element to be stored.  */
		      tree elt
			= gimple_build (&stmts, BIT_FIELD_REF,
					TREE_TYPE (vectype),
					vec_oprnd, TYPE_SIZE (elt_type),
					bitsize_int (k * elt_size));
		      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
		      stmts = NULL;
		      tree ref
			= build2 (MEM_REF, ltype, ptr,
				  build_int_cst (ref_type, 0));
		      new_stmt = gimple_build_assign (ref, elt);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);
		    }
		  if (slp)
		    slp_node->push_vec_def (new_stmt);
		}
	    }
	  if (!slp && !costing_p)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}

      if (!slp && !costing_p)
	*vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

      if (costing_p && dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "vect_model_store_cost: inside_cost = %d, "
			 "prologue_cost = %d .\n",
			 inside_cost, prologue_cost);

      return true;
    }

  gcc_assert (memory_access_type == VMAT_CONTIGUOUS
	      || memory_access_type == VMAT_CONTIGUOUS_DOWN
	      || memory_access_type == VMAT_CONTIGUOUS_PERMUTE
	      || memory_access_type == VMAT_CONTIGUOUS_REVERSE);

  unsigned inside_cost = 0, prologue_cost = 0;
  /* For costing some adjacent vector stores, we'd like to cost with
     the total number of them once instead of cost each one by one. */
  unsigned int n_adjacent_stores = 0;
  auto_vec<tree> result_chain (group_size);
  auto_vec<tree, 1> vec_oprnds;
  for (j = 0; j < ncopies; j++)
    {
      gimple *new_stmt;
      if (j == 0)
	{
	  if (slp && !costing_p)
	    {
	      /* Get vectorized arguments for SLP_NODE.  */
	      vect_get_vec_defs (vinfo, stmt_info, slp_node, 1, op,
				 &vec_oprnds, mask, &vec_masks);
	      vec_oprnd = vec_oprnds[0];
	      if (mask)
		vec_mask = vec_masks[0];
	    }
	  else
	    {
	      /* For interleaved stores we collect vectorized defs for all the
		 stores in the group in DR_CHAIN. DR_CHAIN is then used as an
		 input to vect_permute_store_chain().

		 If the store is not grouped, DR_GROUP_SIZE is 1, and DR_CHAIN
		 is of size 1.  */
	      stmt_vec_info next_stmt_info = first_stmt_info;
	      for (i = 0; i < group_size; i++)
		{
		  /* Since gaps are not supported for interleaved stores,
		     DR_GROUP_SIZE is the exact number of stmts in the chain.
		     Therefore, NEXT_STMT_INFO can't be NULL_TREE.  In case
		     that there is no interleaving, DR_GROUP_SIZE is 1,
		     and only one iteration of the loop will be executed.  */
		  op = vect_get_store_rhs (next_stmt_info);
		  if (costing_p)
		    update_prologue_cost (&prologue_cost, op);
		  else
		    {
		      vect_get_vec_defs_for_operand (vinfo, next_stmt_info,
						     ncopies, op,
						     gvec_oprnds[i]);
		      vec_oprnd = (*gvec_oprnds[i])[0];
		      dr_chain.quick_push (vec_oprnd);
		    }
		  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
		}
	      if (mask && !costing_p)
		{
		  vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies,
						 mask, &vec_masks,
						 mask_vectype);
		  vec_mask = vec_masks[0];
		}
	    }

	  /* We should have catched mismatched types earlier.  */
	  gcc_assert (costing_p
		      || useless_type_conversion_p (vectype,
						    TREE_TYPE (vec_oprnd)));
	  bool simd_lane_access_p
	    = STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) != 0;
	  if (!costing_p
	      && simd_lane_access_p
	      && !loop_masks
	      && TREE_CODE (DR_BASE_ADDRESS (first_dr_info->dr)) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (first_dr_info->dr), 0))
	      && integer_zerop (get_dr_vinfo_offset (vinfo, first_dr_info))
	      && integer_zerop (DR_INIT (first_dr_info->dr))
	      && alias_sets_conflict_p (get_alias_set (aggr_type),
					get_alias_set (TREE_TYPE (ref_type))))
	    {
	      dataref_ptr = unshare_expr (DR_BASE_ADDRESS (first_dr_info->dr));
	      dataref_offset = build_int_cst (ref_type, 0);
	    }
	  else if (!costing_p)
	    dataref_ptr
	      = vect_create_data_ref_ptr (vinfo, first_stmt_info, aggr_type,
					  simd_lane_access_p ? loop : NULL,
					  offset, &dummy, gsi, &ptr_incr,
					  simd_lane_access_p, bump);
	}
      else if (!costing_p)
	{
	  gcc_assert (!LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo));
	  /* DR_CHAIN is then used as an input to vect_permute_store_chain().
	     If the store is not grouped, DR_GROUP_SIZE is 1, and DR_CHAIN is
	     of size 1.  */
	  for (i = 0; i < group_size; i++)
	    {
	      vec_oprnd = (*gvec_oprnds[i])[j];
	      dr_chain[i] = vec_oprnd;
	    }
	  if (mask)
	    vec_mask = vec_masks[j];
	  if (dataref_offset)
	    dataref_offset = int_const_binop (PLUS_EXPR, dataref_offset, bump);
	  else
	    dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					   stmt_info, bump);
	}

      new_stmt = NULL;
      if (grouped_store)
	{
	  /* Permute.  */
	  gcc_assert (memory_access_type == VMAT_CONTIGUOUS_PERMUTE);
	  if (costing_p)
	    {
	      int group_size = DR_GROUP_SIZE (first_stmt_info);
	      int nstmts = ceil_log2 (group_size) * group_size;
	      inside_cost += record_stmt_cost (cost_vec, nstmts, vec_perm,
					       stmt_info, 0, vect_body);
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "vect_model_store_cost: "
				 "strided group_size = %d .\n",
				 group_size);
	    }
	  else
	    vect_permute_store_chain (vinfo, dr_chain, group_size, stmt_info,
				      gsi, &result_chain);
	}

      stmt_vec_info next_stmt_info = first_stmt_info;
      for (i = 0; i < vec_num; i++)
	{
	  if (!costing_p)
	    {
	      if (slp)
		vec_oprnd = vec_oprnds[i];
	      else if (grouped_store)
		/* For grouped stores vectorized defs are interleaved in
		   vect_permute_store_chain().  */
		vec_oprnd = result_chain[i];
	    }

	  if (memory_access_type == VMAT_CONTIGUOUS_REVERSE)
	    {
	      if (costing_p)
		inside_cost += record_stmt_cost (cost_vec, 1, vec_perm,
						 stmt_info, 0, vect_body);
	      else
		{
		  tree perm_mask = perm_mask_for_reverse (vectype);
		  tree perm_dest = vect_create_destination_var (
		    vect_get_store_rhs (stmt_info), vectype);
		  tree new_temp = make_ssa_name (perm_dest);

		  /* Generate the permute statement.  */
		  gimple *perm_stmt
		    = gimple_build_assign (new_temp, VEC_PERM_EXPR, vec_oprnd,
					   vec_oprnd, perm_mask);
		  vect_finish_stmt_generation (vinfo, stmt_info, perm_stmt,
					       gsi);

		  perm_stmt = SSA_NAME_DEF_STMT (new_temp);
		  vec_oprnd = new_temp;
		}
	    }

	  if (costing_p)
	    {
	      n_adjacent_stores++;

	      if (!slp)
		{
		  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
		  if (!next_stmt_info)
		    break;
		}

	      continue;
	    }

	  tree final_mask = NULL_TREE;
	  tree final_len = NULL_TREE;
	  tree bias = NULL_TREE;
	  if (loop_masks)
	    final_mask = vect_get_loop_mask (loop_vinfo, gsi, loop_masks,
					     vec_num * ncopies, vectype,
					     vec_num * j + i);
	  if (slp && vec_mask)
	    vec_mask = vec_masks[i];
	  if (vec_mask)
	    final_mask = prepare_vec_mask (loop_vinfo, mask_vectype, final_mask,
					   vec_mask, gsi);

	  if (i > 0)
	    /* Bump the vector pointer.  */
	    dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					   stmt_info, bump);

	  unsigned misalign;
	  unsigned HOST_WIDE_INT align;
	  align = known_alignment (DR_TARGET_ALIGNMENT (first_dr_info));
	  if (alignment_support_scheme == dr_aligned)
	    misalign = 0;
	  else if (misalignment == DR_MISALIGNMENT_UNKNOWN)
	    {
	      align = dr_alignment (vect_dr_behavior (vinfo, first_dr_info));
	      misalign = 0;
	    }
	  else
	    misalign = misalignment;
	  if (dataref_offset == NULL_TREE
	      && TREE_CODE (dataref_ptr) == SSA_NAME)
	    set_ptr_info_alignment (get_ptr_info (dataref_ptr), align,
				    misalign);
	  align = least_bit_hwi (misalign | align);

	  /* Compute IFN when LOOP_LENS or final_mask valid.  */
	  machine_mode vmode = TYPE_MODE (vectype);
	  machine_mode new_vmode = vmode;
	  internal_fn partial_ifn = IFN_LAST;
	  if (loop_lens)
	    {
	      opt_machine_mode new_ovmode
		= get_len_load_store_mode (vmode, false, &partial_ifn);
	      new_vmode = new_ovmode.require ();
	      unsigned factor
		= (new_ovmode == vmode) ? 1 : GET_MODE_UNIT_SIZE (vmode);
	      final_len = vect_get_loop_len (loop_vinfo, gsi, loop_lens,
					     vec_num * ncopies, vectype,
					     vec_num * j + i, factor);
	    }
	  else if (final_mask)
	    {
	      if (!can_vec_mask_load_store_p (
		    vmode, TYPE_MODE (TREE_TYPE (final_mask)), false,
		    &partial_ifn))
		gcc_unreachable ();
	    }

	  if (partial_ifn == IFN_MASK_LEN_STORE)
	    {
	      if (!final_len)
		{
		  /* Pass VF value to 'len' argument of
		     MASK_LEN_STORE if LOOP_LENS is invalid.  */
		  final_len = size_int (TYPE_VECTOR_SUBPARTS (vectype));
		}
	      if (!final_mask)
		{
		  /* Pass all ones value to 'mask' argument of
		     MASK_LEN_STORE if final_mask is invalid.  */
		  mask_vectype = truth_type_for (vectype);
		  final_mask = build_minus_one_cst (mask_vectype);
		}
	    }
	  if (final_len)
	    {
	      signed char biasval
		= LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);

	      bias = build_int_cst (intQI_type_node, biasval);
	    }

	  /* Arguments are ready.  Create the new vector stmt.  */
	  if (final_len)
	    {
	      gcall *call;
	      tree ptr = build_int_cst (ref_type, align * BITS_PER_UNIT);
	      /* Need conversion if it's wrapped with VnQI.  */
	      if (vmode != new_vmode)
		{
		  tree new_vtype
		    = build_vector_type_for_mode (unsigned_intQI_type_node,
						  new_vmode);
		  tree var = vect_get_new_ssa_name (new_vtype, vect_simple_var);
		  vec_oprnd = build1 (VIEW_CONVERT_EXPR, new_vtype, vec_oprnd);
		  gassign *new_stmt
		    = gimple_build_assign (var, VIEW_CONVERT_EXPR, vec_oprnd);
		  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
		  vec_oprnd = var;
		}

	      if (partial_ifn == IFN_MASK_LEN_STORE)
		call = gimple_build_call_internal (IFN_MASK_LEN_STORE, 6,
						   dataref_ptr, ptr, final_mask,
						   final_len, bias, vec_oprnd);
	      else
		call = gimple_build_call_internal (IFN_LEN_STORE, 5,
						   dataref_ptr, ptr, final_len,
						   bias, vec_oprnd);
	      gimple_call_set_nothrow (call, true);
	      vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
	      new_stmt = call;
	    }
	  else if (final_mask)
	    {
	      tree ptr = build_int_cst (ref_type, align * BITS_PER_UNIT);
	      gcall *call
		= gimple_build_call_internal (IFN_MASK_STORE, 4, dataref_ptr,
					      ptr, final_mask, vec_oprnd);
	      gimple_call_set_nothrow (call, true);
	      vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);
	      new_stmt = call;
	    }
	  else
	    {
	      data_ref
		= fold_build2 (MEM_REF, vectype, dataref_ptr,
			       dataref_offset ? dataref_offset
					      : build_int_cst (ref_type, 0));
	      if (alignment_support_scheme == dr_aligned)
		;
	      else
		TREE_TYPE (data_ref)
		  = build_aligned_type (TREE_TYPE (data_ref),
					align * BITS_PER_UNIT);
	      vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
	      new_stmt = gimple_build_assign (data_ref, vec_oprnd);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }

	  if (slp)
	    continue;

	  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
	  if (!next_stmt_info)
	    break;
	}
      if (!slp && !costing_p)
	{
	  if (j == 0)
	    *vec_stmt = new_stmt;
	  STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}
    }

  if (costing_p)
    {
      if (n_adjacent_stores > 0)
	vect_get_store_cost (vinfo, stmt_info, n_adjacent_stores,
			     alignment_support_scheme, misalignment,
			     &inside_cost, cost_vec);

      /* When vectorizing a store into the function result assign
	 a penalty if the function returns in a multi-register location.
	 In this case we assume we'll end up with having to spill the
	 vector result and do piecewise loads as a conservative estimate.  */
      tree base = get_base_address (STMT_VINFO_DATA_REF (stmt_info)->ref);
      if (base
	  && (TREE_CODE (base) == RESULT_DECL
	      || (DECL_P (base) && cfun_returns (base)))
	  && !aggregate_value_p (base, cfun->decl))
	{
	  rtx reg = hard_function_value (TREE_TYPE (base), cfun->decl, 0, 1);
	  /* ???  Handle PARALLEL in some way.  */
	  if (REG_P (reg))
	    {
	      int nregs = hard_regno_nregs (REGNO (reg), GET_MODE (reg));
	      /* Assume that a single reg-reg move is possible and cheap,
		 do not account for vector to gp register move cost.  */
	      if (nregs > 1)
		{
		  /* Spill.  */
		  prologue_cost
		    += record_stmt_cost (cost_vec, ncopies, vector_store,
					 stmt_info, 0, vect_epilogue);
		  /* Loads.  */
		  prologue_cost
		    += record_stmt_cost (cost_vec, ncopies * nregs, scalar_load,
					 stmt_info, 0, vect_epilogue);
		}
	    }
	}
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "vect_model_store_cost: inside_cost = %d, "
			 "prologue_cost = %d .\n",
			 inside_cost, prologue_cost);
    }

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
  machine_mode vmode = TYPE_MODE (vectype);
  gcc_assert (can_vec_perm_const_p (vmode, vmode, sel));
  return vect_gen_perm_mask_any (vectype, sel);
}

/* Given a vector variable X and Y, that was generated for the scalar
   STMT_INFO, generate instructions to permute the vector elements of X and Y
   using permutation mask MASK_VEC, insert them at *GSI and return the
   permuted vector variable.  */

static tree
permute_vec_elements (vec_info *vinfo,
		      tree x, tree y, tree mask_vec, stmt_vec_info stmt_info,
		      gimple_stmt_iterator *gsi)
{
  tree vectype = TREE_TYPE (x);
  tree perm_dest, data_ref;
  gimple *perm_stmt;

  tree scalar_dest = gimple_get_lhs (stmt_info->stmt);
  if (scalar_dest && TREE_CODE (scalar_dest) == SSA_NAME)
    perm_dest = vect_create_destination_var (scalar_dest, vectype);
  else
    perm_dest = vect_get_new_vect_var (vectype, vect_simple_var, NULL);
  data_ref = make_ssa_name (perm_dest);

  /* Generate the permute statement.  */
  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, x, y, mask_vec);
  vect_finish_stmt_generation (vinfo, stmt_info, perm_stmt, gsi);

  return data_ref;
}

/* Hoist the definitions of all SSA uses on STMT_INFO out of the loop LOOP,
   inserting them on the loops preheader edge.  Returns true if we
   were successful in doing so (and thus STMT_INFO can be moved then),
   otherwise returns false.  HOIST_P indicates if we want to hoist the
   definitions of all SSA uses, it would be false when we are costing.  */

static bool
hoist_defs_of_uses (gimple *stmt, class loop *loop, bool hoist_p)
{
  ssa_op_iter i;
  use_operand_p use_p;
  auto_vec<use_operand_p, 8> to_hoist;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, i, SSA_OP_USE)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (USE_FROM_PTR (use_p));
      if (!gimple_nop_p (def_stmt)
	  && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt)))
	{
	  /* Make sure we don't need to recurse.  While we could do
	     so in simple cases when there are more complex use webs
	     we don't have an easy way to preserve stmt order to fulfil
	     dependencies within them.  */
	  tree op2;
	  ssa_op_iter i2;
	  if (gimple_code (def_stmt) == GIMPLE_PHI
	      || (single_ssa_def_operand (def_stmt, SSA_OP_DEF)
		  == NULL_DEF_OPERAND_P))
	    return false;
	  FOR_EACH_SSA_TREE_OPERAND (op2, def_stmt, i2, SSA_OP_USE)
	    {
	      gimple *def_stmt2 = SSA_NAME_DEF_STMT (op2);
	      if (!gimple_nop_p (def_stmt2)
		  && flow_bb_inside_loop_p (loop, gimple_bb (def_stmt2)))
		return false;
	    }
	  to_hoist.safe_push (use_p);
	}
    }

  if (to_hoist.is_empty ())
    return true;

  if (!hoist_p)
    return true;

  /* Instead of moving defs we copy them so we can zero their UID to not
     confuse dominance queries in the preheader.  */
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  for (use_operand_p use_p : to_hoist)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (USE_FROM_PTR (use_p));
      gimple *copy = gimple_copy (def_stmt);
      gimple_set_uid (copy, 0);
      def_operand_p def_p = single_ssa_def_operand (def_stmt, SSA_OP_DEF);
      tree new_def = duplicate_ssa_name (DEF_FROM_PTR (def_p), copy);
      update_stmt (copy);
      def_p = single_ssa_def_operand (copy, SSA_OP_DEF);
      SET_DEF (def_p, new_def);
      SET_USE (use_p, new_def);
      gsi_insert_before (&gsi, copy, GSI_SAME_STMT);
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
vectorizable_load (vec_info *vinfo,
		   stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		   gimple **vec_stmt, slp_tree slp_node,
		   stmt_vector_for_cost *cost_vec)
{
  tree scalar_dest;
  tree vec_dest = NULL;
  tree data_ref = NULL;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  class loop *loop = NULL;
  class loop *containing_loop = gimple_bb (stmt_info->stmt)->loop_father;
  bool nested_in_vect_loop = false;
  tree elem_type;
  /* Avoid false positive uninitialized warning, see PR110652.  */
  tree new_temp = NULL_TREE;
  machine_mode mode;
  tree dummy;
  tree dataref_ptr = NULL_TREE;
  tree dataref_offset = NULL_TREE;
  gimple *ptr_incr = NULL;
  int ncopies;
  int i, j;
  unsigned int group_size;
  poly_uint64 group_gap_adj;
  tree msq = NULL_TREE, lsq;
  tree realignment_token = NULL_TREE;
  gphi *phi = NULL;
  vec<tree> dr_chain = vNULL;
  bool grouped_load = false;
  stmt_vec_info first_stmt_info;
  stmt_vec_info first_stmt_info_for_drptr = NULL;
  bool compute_in_loop = false;
  class loop *at_loop;
  int vec_num;
  bool slp = (slp_node != NULL);
  bool slp_perm = false;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
  poly_uint64 vf;
  tree aggr_type;
  gather_scatter_info gs_info;
  tree ref_type;
  enum vect_def_type mask_dt = vect_unknown_def_type;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def
      && ! vec_stmt)
    return false;

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  tree mask = NULL_TREE, mask_vectype = NULL_TREE;
  int mask_index = -1;
  slp_tree slp_op = NULL;
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

      mask_index = internal_fn_mask_index (ifn);
      if (mask_index >= 0 && slp_node)
	mask_index = vect_slp_child_index_for_operand
		    (call, mask_index, STMT_VINFO_GATHER_SCATTER_P (stmt_info));
      if (mask_index >= 0
	  && !vect_check_scalar_mask (vinfo, stmt_info, slp_node, mask_index,
				      &mask, &slp_op, &mask_dt, &mask_vectype))
	return false;
    }

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
  if (nested_in_vect_loop
      && (ncopies > 1 || (slp && SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node) > 1)))
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

      /* Refuse non-SLP vectorization of SLP-only groups.  */
      if (!slp && STMT_VINFO_SLP_VECT_ONLY (first_stmt_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "cannot vectorize load in non-SLP mode.\n");
	  return false;
	}

      /* Invalidate assumptions made by dependence analysis when vectorization
	 on the unrolled body effectively re-orders stmts.  */
      if (STMT_VINFO_MIN_NEG_DIST (stmt_info) != 0
	  && maybe_gt (LOOP_VINFO_VECT_FACTOR (loop_vinfo),
		       STMT_VINFO_MIN_NEG_DIST (stmt_info)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "cannot perform implicit CSE when performing "
			     "group loads with negative dependence distance\n");
	  return false;
	}
    }
  else
    group_size = 1;

  vect_memory_access_type memory_access_type;
  enum dr_alignment_support alignment_support_scheme;
  int misalignment;
  poly_int64 poffset;
  internal_fn lanes_ifn;
  if (!get_load_store_type (vinfo, stmt_info, vectype, slp_node, mask, VLS_LOAD,
			    ncopies, &memory_access_type, &poffset,
			    &alignment_support_scheme, &misalignment, &gs_info,
			    &lanes_ifn))
    return false;

  /* ???  The following checks should really be part of
     get_group_load_store_type.  */
  if (slp
      && SLP_TREE_LOAD_PERMUTATION (slp_node).exists ()
      && !((memory_access_type == VMAT_ELEMENTWISE
	    || memory_access_type == VMAT_GATHER_SCATTER)
	   && SLP_TREE_LANES (slp_node) == 1))
    {
      slp_perm = true;

      if (!loop_vinfo)
	{
	  /* In BB vectorization we may not actually use a loaded vector
	     accessing elements in excess of DR_GROUP_SIZE.  */
	  stmt_vec_info group_info = SLP_TREE_SCALAR_STMTS (slp_node)[0];
	  group_info = DR_GROUP_FIRST_ELEMENT (group_info);
	  unsigned HOST_WIDE_INT nunits;
	  unsigned j, k, maxk = 0;
	  FOR_EACH_VEC_ELT (SLP_TREE_LOAD_PERMUTATION (slp_node), j, k)
	      if (k > maxk)
		maxk = k;
	  tree vectype = SLP_TREE_VECTYPE (slp_node);
	  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&nunits)
	      || maxk >= (DR_GROUP_SIZE (group_info) & ~(nunits - 1)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "BB vectorization with gaps at the end of "
				 "a load is not supported\n");
	      return false;
	    }
	}

      auto_vec<tree> tem;
      unsigned n_perms;
      if (!vect_transform_slp_perm_load (vinfo, slp_node, tem, NULL, vf,
					 true, &n_perms))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION,
			     vect_location,
			     "unsupported load permutation\n");
	  return false;
	}
    }

  if (slp_node
      && slp_node->ldst_lanes
      && memory_access_type != VMAT_LOAD_STORE_LANES)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "discovered load-lane but cannot use it.\n");
      return false;
    }

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
      else if (memory_access_type != VMAT_LOAD_STORE_LANES
	       && memory_access_type != VMAT_GATHER_SCATTER)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported access type for masked load.\n");
	  return false;
	}
      else if (memory_access_type == VMAT_GATHER_SCATTER
	       && gs_info.ifn == IFN_LAST
	       && !gs_info.decl)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported masked emulated gather.\n");
	  return false;
	}
      else if (memory_access_type == VMAT_ELEMENTWISE
	       || memory_access_type == VMAT_STRIDED_SLP)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported masked strided access.\n");
	  return false;
	}
    }

  bool costing_p = !vec_stmt;

  if (costing_p) /* transformation not required.  */
    {
      if (slp_node
	  && mask
	  && !vect_maybe_update_slp_op_vectype (slp_op,
						mask_vectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}

      if (!slp)
	STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) = memory_access_type;
      else
	SLP_TREE_MEMORY_ACCESS_TYPE (slp_node) = memory_access_type;

      if (loop_vinfo
	  && LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo))
	check_load_store_for_partial_vectors (loop_vinfo, vectype, slp_node,
					      VLS_LOAD, group_size,
					      memory_access_type, &gs_info,
					      mask);

      if (dump_enabled_p ()
	  && memory_access_type != VMAT_ELEMENTWISE
	  && memory_access_type != VMAT_GATHER_SCATTER
	  && alignment_support_scheme != dr_aligned)
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Vectorizing an unaligned access.\n");

      if (memory_access_type == VMAT_LOAD_STORE_LANES)
	vinfo->any_known_not_updated_vssa = true;

      STMT_VINFO_TYPE (stmt_info) = load_vec_info_type;
    }

  if (!slp)
    gcc_assert (memory_access_type
		== STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info));
  else
    gcc_assert (memory_access_type
		== SLP_TREE_MEMORY_ACCESS_TYPE (slp_node));

  if (dump_enabled_p () && !costing_p)
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform load. ncopies = %d\n", ncopies);

  /* Transform.  */

  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info), *first_dr_info = NULL;
  ensure_base_align (dr_info);

  if (memory_access_type == VMAT_INVARIANT)
    {
      gcc_assert (!grouped_load && !mask && !bb_vinfo);
      /* If we have versioned for aliasing or the loop doesn't
	 have any data dependencies that would preclude this,
	 then we are sure this is a loop invariant load and
	 thus we can insert it on the preheader edge.
	 TODO: hoist_defs_of_uses should ideally be computed
	 once at analysis time, remembered and used in the
	 transform time.  */
      bool hoist_p = (LOOP_VINFO_NO_DATA_DEPENDENCIES (loop_vinfo)
		      && !nested_in_vect_loop
		      && hoist_defs_of_uses (stmt_info->stmt, loop, false));
      if (costing_p)
	{
	  enum vect_cost_model_location cost_loc
	    = hoist_p ? vect_prologue : vect_body;
	  unsigned int cost = record_stmt_cost (cost_vec, 1, scalar_load,
						stmt_info, 0, cost_loc);
	  cost += record_stmt_cost (cost_vec, 1, scalar_to_vec, stmt_info, 0,
				    cost_loc);
	  unsigned int prologue_cost = hoist_p ? cost : 0;
	  unsigned int inside_cost = hoist_p ? 0 : cost;
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vect_model_load_cost: inside_cost = %d, "
			     "prologue_cost = %d .\n",
			     inside_cost, prologue_cost);
	  return true;
	}
      if (hoist_p)
	{
	  gassign *stmt = as_a <gassign *> (stmt_info->stmt);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "hoisting out of the vectorized loop: %G",
			     (gimple *) stmt);
	  scalar_dest = copy_ssa_name (scalar_dest);
	  tree rhs = unshare_expr (gimple_assign_rhs1 (stmt));
	  edge pe = loop_preheader_edge (loop);
	  gphi *vphi = get_virtual_phi (loop->header);
	  tree vuse;
	  if (vphi)
	    vuse = PHI_ARG_DEF_FROM_EDGE (vphi, pe);
	  else
	    vuse = gimple_vuse (gsi_stmt (*gsi));
	  gimple *new_stmt = gimple_build_assign (scalar_dest, rhs);
	  gimple_set_vuse (new_stmt, vuse);
	  gsi_insert_on_edge_immediate (pe, new_stmt);
	  hoist_defs_of_uses (new_stmt, loop, true);
	}
      /* These copies are all equivalent.  */
      if (hoist_p)
	new_temp = vect_init_vector (vinfo, stmt_info, scalar_dest,
				     vectype, NULL);
      else
	{
	  gimple_stmt_iterator gsi2 = *gsi;
	  gsi_next (&gsi2);
	  new_temp = vect_init_vector (vinfo, stmt_info, scalar_dest,
				       vectype, &gsi2);
	}
      gimple *new_stmt = SSA_NAME_DEF_STMT (new_temp);
      if (slp)
	for (j = 0; j < (int) SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node); ++j)
	  slp_node->push_vec_def (new_stmt);
      else
	{
	  for (j = 0; j < ncopies; ++j)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	  *vec_stmt = new_stmt;
	}
      return true;
    }

  if (memory_access_type == VMAT_ELEMENTWISE
      || memory_access_type == VMAT_STRIDED_SLP)
    {
      gimple_stmt_iterator incr_gsi;
      bool insert_after;
      tree offvar;
      tree ivstep;
      tree running_off;
      vec<constructor_elt, va_gc> *v = NULL;
      tree stride_base, stride_step, alias_off;
      /* Checked by get_load_store_type.  */
      unsigned int const_nunits = nunits.to_constant ();
      unsigned HOST_WIDE_INT cst_offset = 0;
      tree dr_offset;
      unsigned int inside_cost = 0;

      gcc_assert (!LOOP_VINFO_USING_PARTIAL_VECTORS_P (loop_vinfo));
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

      if (slp && grouped_load
	  && memory_access_type == VMAT_STRIDED_SLP)
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

      if (!costing_p)
	{
	  dr_offset = get_dr_vinfo_offset (vinfo, first_dr_info);
	  stride_base = fold_build_pointer_plus (
	    DR_BASE_ADDRESS (first_dr_info->dr),
	    size_binop (PLUS_EXPR, convert_to_ptrofftype (dr_offset),
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
	  create_iv (stride_base, PLUS_EXPR, ivstep, NULL,
		     loop, &incr_gsi, insert_after,
		     &offvar, NULL);

	  stride_step = cse_and_gimplify_to_preheader (loop_vinfo, stride_step);
	}

      running_off = offvar;
      alias_off = build_int_cst (ref_type, 0);
      int nloads = const_nunits;
      int lnel = 1;
      tree ltype = TREE_TYPE (vectype);
      tree lvectype = vectype;
      auto_vec<tree> dr_chain;
      if (memory_access_type == VMAT_STRIDED_SLP)
	{
	  HOST_WIDE_INT n = gcd (group_size, const_nunits);
	  /* Use the target vector type if the group size is a multiple
	     of it.  */
	  if (n == const_nunits)
	    {
	      nloads = 1;
	      lnel = const_nunits;
	      ltype = vectype;
	    }
	  /* Else use the biggest vector we can load the group without
	     accessing excess elements.  */
	  else if (n > 1)
	    {
	      tree ptype;
	      tree vtype
		= vector_vector_composition_type (vectype, const_nunits / n,
						  &ptype);
	      if (vtype != NULL_TREE)
		{
		  nloads = const_nunits / n;
		  lnel = n;
		  lvectype = vtype;
		  ltype = ptype;
		}
	    }
	  /* Else fall back to the default element-wise access.  */
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
      unsigned int n_groups = 0;
      /* For costing some adjacent vector loads, we'd like to cost with
	 the total number of them once instead of cost each one by one. */
      unsigned int n_adjacent_loads = 0;
      for (j = 0; j < ncopies; j++)
	{
	  if (nloads > 1 && !costing_p)
	    vec_alloc (v, nloads);
	  gimple *new_stmt = NULL;
	  for (i = 0; i < nloads; i++)
	    {
	      if (costing_p)
		{
		  /* For VMAT_ELEMENTWISE, just cost it as scalar_load to
		     avoid ICE, see PR110776.  */
		  if (VECTOR_TYPE_P (ltype)
		      && memory_access_type != VMAT_ELEMENTWISE)
		    n_adjacent_loads++;
		  else
		    inside_cost += record_stmt_cost (cost_vec, 1, scalar_load,
						     stmt_info, 0, vect_body);
		  continue;
		}
	      tree this_off = build_int_cst (TREE_TYPE (alias_off),
					     group_el * elsz + cst_offset);
	      tree data_ref = build2 (MEM_REF, ltype, running_off, this_off);
	      vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
	      new_stmt = gimple_build_assign (make_ssa_name (ltype), data_ref);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      if (nloads > 1)
		CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
					gimple_assign_lhs (new_stmt));

	      group_el += lnel;
	      if (! slp
		  || group_el == group_size)
		{
		  n_groups++;
		  /* When doing SLP make sure to not load elements from
		     the next vector iteration, those will not be accessed
		     so just use the last element again.  See PR107451.  */
		  if (!slp || known_lt (n_groups, vf))
		    {
		      tree newoff = copy_ssa_name (running_off);
		      gimple *incr
			= gimple_build_assign (newoff, POINTER_PLUS_EXPR,
					       running_off, stride_step);
		      vect_finish_stmt_generation (vinfo, stmt_info, incr, gsi);
		      running_off = newoff;
		    }
		  group_el = 0;
		}
	    }

	  if (nloads > 1)
	    {
	      if (costing_p)
		inside_cost += record_stmt_cost (cost_vec, 1, vec_construct,
						 stmt_info, 0, vect_body);
	      else
		{
		  tree vec_inv = build_constructor (lvectype, v);
		  new_temp = vect_init_vector (vinfo, stmt_info, vec_inv,
					       lvectype, gsi);
		  new_stmt = SSA_NAME_DEF_STMT (new_temp);
		  if (lvectype != vectype)
		    {
		      new_stmt
			= gimple_build_assign (make_ssa_name (vectype),
					       VIEW_CONVERT_EXPR,
					       build1 (VIEW_CONVERT_EXPR,
						       vectype, new_temp));
		      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt,
						   gsi);
		    }
		}
	    }

	  if (!costing_p)
	    {
	      if (slp)
		{
		  if (slp_perm)
		    dr_chain.quick_push (gimple_assign_lhs (new_stmt));
		  else
		    slp_node->push_vec_def (new_stmt);
		}
	      else
		{
		  if (j == 0)
		    *vec_stmt = new_stmt;
		  STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
		}
	    }
	}
      if (slp_perm)
	{
	  unsigned n_perms;
	  if (costing_p)
	    {
	      unsigned n_loads;
	      vect_transform_slp_perm_load (vinfo, slp_node, vNULL, NULL, vf,
					    true, &n_perms, &n_loads);
	      inside_cost += record_stmt_cost (cost_vec, n_perms, vec_perm,
					       first_stmt_info, 0, vect_body);
	    }
	  else
	    vect_transform_slp_perm_load (vinfo, slp_node, dr_chain, gsi, vf,
					  false, &n_perms);
	}

      if (costing_p)
	{
	  if (n_adjacent_loads > 0)
	    vect_get_load_cost (vinfo, stmt_info, n_adjacent_loads,
				alignment_support_scheme, misalignment, false,
				&inside_cost, nullptr, cost_vec, cost_vec,
				true);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vect_model_load_cost: inside_cost = %u, "
			     "prologue_cost = 0 .\n",
			     inside_cost);
	}

      return true;
    }

  if (memory_access_type == VMAT_GATHER_SCATTER
      || (!slp && memory_access_type == VMAT_CONTIGUOUS))
    grouped_load = false;

  if (grouped_load
      || (slp && SLP_TREE_LOAD_PERMUTATION (slp_node).exists ()))
    {
      if (grouped_load)
	{
	  first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
	  group_size = DR_GROUP_SIZE (first_stmt_info);
	}
      else
	{
	  first_stmt_info = stmt_info;
	  group_size = 1;
	}
      /* For SLP vectorization we directly vectorize a subchain
         without permutation.  */
      if (slp && ! SLP_TREE_LOAD_PERMUTATION (slp_node).exists ())
	first_stmt_info = SLP_TREE_SCALAR_STMTS (slp_node)[0];
      /* For BB vectorization always use the first stmt to base
	 the data ref pointer on.  */
      if (bb_vinfo)
	first_stmt_info_for_drptr
	  = vect_find_first_scalar_stmt_in_slp (slp_node);

      /* Check if the chain of loads is already vectorized.  */
      if (STMT_VINFO_VEC_STMTS (first_stmt_info).exists ()
	  /* For SLP we would need to copy over SLP_TREE_VEC_DEFS.
	     ???  But we can only do so if there is exactly one
	     as we have no way to get at the rest.  Leave the CSE
	     opportunity alone.
	     ???  With the group load eventually participating
	     in multiple different permutations (having multiple
	     slp nodes which refer to the same group) the CSE
	     is even wrong code.  See PR56270.  */
	  && !slp)
	{
	  *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];
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
	  unsigned scalar_lanes = SLP_TREE_LANES (slp_node);
	  if (nested_in_vect_loop)
	    /* We do not support grouped accesses in a nested loop,
	       instead the access is contiguous but it might be
	       permuted.  No gap adjustment is needed though.  */
	    vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
	  else if (slp_perm
		   && (group_size != scalar_lanes
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
		= group_size - scalar_lanes;
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
      if (slp)
	vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
    }

  gcc_assert (alignment_support_scheme);
  vec_loop_masks *loop_masks
    = (loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
       ? &LOOP_VINFO_MASKS (loop_vinfo)
       : NULL);
  vec_loop_lens *loop_lens
    = (loop_vinfo && LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo)
       ? &LOOP_VINFO_LENS (loop_vinfo)
       : NULL);

  /* The vect_transform_stmt and vect_analyze_stmt will go here but there
     are some difference here.  We cannot enable both the lens and masks
     during transform but it is allowed during analysis.
     Shouldn't go with length-based approach if fully masked.  */
  if (cost_vec == NULL)
    /* The cost_vec is NULL during transfrom.  */
    gcc_assert ((!loop_lens || !loop_masks));

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
  */

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

  bool diff_first_stmt_info
    = first_stmt_info_for_drptr && first_stmt_info != first_stmt_info_for_drptr;

  tree offset = NULL_TREE;
  if ((alignment_support_scheme == dr_explicit_realign_optimized
       || alignment_support_scheme == dr_explicit_realign)
      && !compute_in_loop)
    {
      /* If we have different first_stmt_info, we can't set up realignment
	 here, since we can't guarantee first_stmt_info DR has been
	 initialized yet, use first_stmt_info_for_drptr DR by bumping the
	 distance from first_stmt_info DR instead as below.  */
      if (!costing_p)
	{
	  if (!diff_first_stmt_info)
	    msq = vect_setup_realignment (vinfo, first_stmt_info, gsi,
					  &realignment_token,
					  alignment_support_scheme, NULL_TREE,
					  &at_loop);
	  if (alignment_support_scheme == dr_explicit_realign_optimized)
	    {
	      phi = as_a<gphi *> (SSA_NAME_DEF_STMT (msq));
	      offset = size_binop (MINUS_EXPR, TYPE_SIZE_UNIT (vectype),
				   size_one_node);
	      gcc_assert (!first_stmt_info_for_drptr);
	    }
	}
    }
  else
    at_loop = loop;

  if (!known_eq (poffset, 0))
    offset = (offset
	      ? size_binop (PLUS_EXPR, offset, size_int (poffset))
	      : size_int (poffset));

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
      if (!costing_p)
	vect_get_strided_load_store_ops (stmt_info, loop_vinfo, gsi, &gs_info,
					 &bump, &vec_offset, loop_lens);
    }
  else
    {
      if (memory_access_type == VMAT_LOAD_STORE_LANES)
	aggr_type = build_array_type_nelts (elem_type, group_size * nunits);
      else
	aggr_type = vectype;
      if (!costing_p)
	bump = vect_get_data_ptr_increment (vinfo, gsi, dr_info, aggr_type,
					    memory_access_type, loop_lens);
    }

  auto_vec<tree> vec_offsets;
  auto_vec<tree> vec_masks;
  if (mask && !costing_p)
    {
      if (slp_node)
	vect_get_slp_defs (SLP_TREE_CHILDREN (slp_node)[mask_index],
			   &vec_masks);
      else
	vect_get_vec_defs_for_operand (vinfo, stmt_info, ncopies, mask,
				       &vec_masks, mask_vectype);
    }

  tree vec_mask = NULL_TREE;
  if (memory_access_type == VMAT_LOAD_STORE_LANES)
    {
      gcc_assert (alignment_support_scheme == dr_aligned
		  || alignment_support_scheme == dr_unaligned_supported);

      unsigned int inside_cost = 0, prologue_cost = 0;
      /* For costing some adjacent vector loads, we'd like to cost with
	 the total number of them once instead of cost each one by one. */
      unsigned int n_adjacent_loads = 0;
      if (slp_node)
	ncopies = slp_node->vec_stmts_size / group_size;
      for (j = 0; j < ncopies; j++)
	{
	  if (costing_p)
	    {
	      /* An IFN_LOAD_LANES will load all its vector results,
		 regardless of which ones we actually need.  Account
		 for the cost of unused results.  */
	      if (first_stmt_info == stmt_info)
		{
		  unsigned int gaps = DR_GROUP_SIZE (first_stmt_info);
		  stmt_vec_info next_stmt_info = first_stmt_info;
		  do
		    {
		      gaps -= 1;
		      next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
		    }
		  while (next_stmt_info);
		  if (gaps)
		    {
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_NOTE, vect_location,
					 "vect_model_load_cost: %d "
					 "unused vectors.\n",
					 gaps);
		      vect_get_load_cost (vinfo, stmt_info, gaps,
					  alignment_support_scheme,
					  misalignment, false, &inside_cost,
					  &prologue_cost, cost_vec, cost_vec,
					  true);
		    }
		}
	      n_adjacent_loads++;
	      continue;
	    }

	  /* 1. Create the vector or array pointer update chain.  */
	  if (j == 0)
	    dataref_ptr
	      = vect_create_data_ref_ptr (vinfo, first_stmt_info, aggr_type,
					  at_loop, offset, &dummy, gsi,
					  &ptr_incr, false, bump);
	  else
	    {
	      gcc_assert (!LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo));
	      dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					     stmt_info, bump);
	    }
	  if (mask)
	    vec_mask = vec_masks[j];

	  tree vec_array = create_vector_array (vectype, group_size);

	  tree final_mask = NULL_TREE;
	  tree final_len = NULL_TREE;
	  tree bias = NULL_TREE;
	  if (loop_masks)
	    final_mask = vect_get_loop_mask (loop_vinfo, gsi, loop_masks,
					     ncopies, vectype, j);
	  if (vec_mask)
	    final_mask = prepare_vec_mask (loop_vinfo, mask_vectype, final_mask,
					   vec_mask, gsi);

	  if (lanes_ifn == IFN_MASK_LEN_LOAD_LANES)
	    {
	      if (loop_lens)
		final_len = vect_get_loop_len (loop_vinfo, gsi, loop_lens,
					       ncopies, vectype, j, 1);
	      else
		final_len = size_int (TYPE_VECTOR_SUBPARTS (vectype));
	      signed char biasval
		= LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
	      bias = build_int_cst (intQI_type_node, biasval);
	      if (!final_mask)
		{
		  mask_vectype = truth_type_for (vectype);
		  final_mask = build_minus_one_cst (mask_vectype);
		}
	    }

	  gcall *call;
	  if (final_len && final_mask)
	    {
	      /* Emit:
		   VEC_ARRAY = MASK_LEN_LOAD_LANES (DATAREF_PTR, ALIAS_PTR,
						    VEC_MASK, LEN, BIAS).  */
	      unsigned int align = TYPE_ALIGN (TREE_TYPE (vectype));
	      tree alias_ptr = build_int_cst (ref_type, align);
	      call = gimple_build_call_internal (IFN_MASK_LEN_LOAD_LANES, 5,
						 dataref_ptr, alias_ptr,
						 final_mask, final_len, bias);
	    }
	  else if (final_mask)
	    {
	      /* Emit:
		   VEC_ARRAY = MASK_LOAD_LANES (DATAREF_PTR, ALIAS_PTR,
						VEC_MASK).  */
	      unsigned int align = TYPE_ALIGN (TREE_TYPE (vectype));
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
	  vect_finish_stmt_generation (vinfo, stmt_info, call, gsi);

	  if (!slp)
	    dr_chain.create (group_size);
	  /* Extract each vector into an SSA_NAME.  */
	  for (unsigned i = 0; i < group_size; i++)
	    {
	      new_temp = read_vector_array (vinfo, stmt_info, gsi, scalar_dest,
					    vec_array, i);
	      if (slp)
		slp_node->push_vec_def (new_temp);
	      else
		dr_chain.quick_push (new_temp);
	    }

	  if (!slp)
	    /* Record the mapping between SSA_NAMEs and statements.  */
	    vect_record_grouped_load_vectors (vinfo, stmt_info, dr_chain);

	  /* Record that VEC_ARRAY is now dead.  */
	  vect_clobber_variable (vinfo, stmt_info, gsi, vec_array);

	  if (!slp)
	    dr_chain.release ();

	  if (!slp_node)
	    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];
	}

      if (costing_p)
	{
	  if (n_adjacent_loads > 0)
	    vect_get_load_cost (vinfo, stmt_info, n_adjacent_loads,
				alignment_support_scheme, misalignment, false,
				&inside_cost, &prologue_cost, cost_vec,
				cost_vec, true);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "vect_model_load_cost: inside_cost = %u, "
			     "prologue_cost = %u .\n",
			     inside_cost, prologue_cost);
	}

      return true;
    }

  if (memory_access_type == VMAT_GATHER_SCATTER)
    {
      gcc_assert (alignment_support_scheme == dr_aligned
		  || alignment_support_scheme == dr_unaligned_supported);
      gcc_assert (!grouped_load && !slp_perm);

      unsigned int inside_cost = 0, prologue_cost = 0;
      for (j = 0; j < ncopies; j++)
	{
	  /* 1. Create the vector or array pointer update chain.  */
	  if (j == 0 && !costing_p)
	    {
	      if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
		vect_get_gather_scatter_ops (loop_vinfo, loop, stmt_info,
					     slp_node, &gs_info, &dataref_ptr,
					     &vec_offsets);
	      else
		dataref_ptr
		  = vect_create_data_ref_ptr (vinfo, first_stmt_info, aggr_type,
					      at_loop, offset, &dummy, gsi,
					      &ptr_incr, false, bump);
	    }
	  else if (!costing_p)
	    {
	      gcc_assert (!LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo));
	      if (!STMT_VINFO_GATHER_SCATTER_P (stmt_info))
		dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr,
					       gsi, stmt_info, bump);
	    }

	  gimple *new_stmt = NULL;
	  for (i = 0; i < vec_num; i++)
	    {
	      tree final_mask = NULL_TREE;
	      tree final_len = NULL_TREE;
	      tree bias = NULL_TREE;
	      if (!costing_p)
		{
		  if (mask)
		    vec_mask = vec_masks[vec_num * j + i];
		  if (loop_masks)
		    final_mask
		      = vect_get_loop_mask (loop_vinfo, gsi, loop_masks,
					    vec_num * ncopies, vectype,
					    vec_num * j + i);
		  if (vec_mask)
		    final_mask = prepare_vec_mask (loop_vinfo, mask_vectype,
						   final_mask, vec_mask, gsi);

		  if (i > 0 && !STMT_VINFO_GATHER_SCATTER_P (stmt_info))
		    dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr,
						   gsi, stmt_info, bump);
		}

	      /* 2. Create the vector-load in the loop.  */
	      unsigned HOST_WIDE_INT align;
	      if (gs_info.ifn != IFN_LAST)
		{
		  if (costing_p)
		    {
		      unsigned int cnunits = vect_nunits_for_cost (vectype);
		      inside_cost
			= record_stmt_cost (cost_vec, cnunits, scalar_load,
					    stmt_info, 0, vect_body);
		      continue;
		    }
		  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
		    vec_offset = vec_offsets[vec_num * j + i];
		  tree zero = build_zero_cst (vectype);
		  tree scale = size_int (gs_info.scale);

		  if (gs_info.ifn == IFN_MASK_LEN_GATHER_LOAD)
		    {
		      if (loop_lens)
			final_len
			  = vect_get_loop_len (loop_vinfo, gsi, loop_lens,
					       vec_num * ncopies, vectype,
					       vec_num * j + i, 1);
		      else
			final_len
			  = build_int_cst (sizetype,
					   TYPE_VECTOR_SUBPARTS (vectype));
		      signed char biasval
			= LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
		      bias = build_int_cst (intQI_type_node, biasval);
		      if (!final_mask)
			{
			  mask_vectype = truth_type_for (vectype);
			  final_mask = build_minus_one_cst (mask_vectype);
			}
		    }

		  gcall *call;
		  if (final_len && final_mask)
		    call
		      = gimple_build_call_internal (IFN_MASK_LEN_GATHER_LOAD, 7,
						    dataref_ptr, vec_offset,
						    scale, zero, final_mask,
						    final_len, bias);
		  else if (final_mask)
		    call = gimple_build_call_internal (IFN_MASK_GATHER_LOAD, 5,
						       dataref_ptr, vec_offset,
						       scale, zero, final_mask);
		  else
		    call = gimple_build_call_internal (IFN_GATHER_LOAD, 4,
						       dataref_ptr, vec_offset,
						       scale, zero);
		  gimple_call_set_nothrow (call, true);
		  new_stmt = call;
		  data_ref = NULL_TREE;
		}
	      else if (gs_info.decl)
		{
		  /* The builtin decls path for gather is legacy, x86 only.  */
		  gcc_assert (!final_len && nunits.is_constant ());
		  if (costing_p)
		    {
		      unsigned int cnunits = vect_nunits_for_cost (vectype);
		      inside_cost
			= record_stmt_cost (cost_vec, cnunits, scalar_load,
					    stmt_info, 0, vect_body);
		      continue;
		    }
		  poly_uint64 offset_nunits
		    = TYPE_VECTOR_SUBPARTS (gs_info.offset_vectype);
		  if (known_eq (nunits, offset_nunits))
		    {
		      new_stmt = vect_build_one_gather_load_call
				   (vinfo, stmt_info, gsi, &gs_info,
				    dataref_ptr, vec_offsets[vec_num * j + i],
				    final_mask);
		      data_ref = NULL_TREE;
		    }
		  else if (known_eq (nunits, offset_nunits * 2))
		    {
		      /* We have a offset vector with half the number of
			 lanes but the builtins will produce full vectype
			 data with just the lower lanes filled.  */
		      new_stmt = vect_build_one_gather_load_call
			  (vinfo, stmt_info, gsi, &gs_info,
			   dataref_ptr, vec_offsets[2 * vec_num * j + 2 * i],
			   final_mask);
		      tree low = make_ssa_name (vectype);
		      gimple_set_lhs (new_stmt, low);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);

		      /* now put upper half of final_mask in final_mask low. */
		      if (final_mask
			  && !SCALAR_INT_MODE_P
				(TYPE_MODE (TREE_TYPE (final_mask))))
			{
			  int count = nunits.to_constant ();
			  vec_perm_builder sel (count, count, 1);
			  sel.quick_grow (count);
			  for (int i = 0; i < count; ++i)
			    sel[i] = i | (count / 2);
			  vec_perm_indices indices (sel, 2, count);
			  tree perm_mask = vect_gen_perm_mask_checked
					     (TREE_TYPE (final_mask), indices);
			  new_stmt = gimple_build_assign (NULL_TREE,
							  VEC_PERM_EXPR,
							  final_mask,
							  final_mask,
							  perm_mask);
			  final_mask = make_ssa_name (TREE_TYPE (final_mask));
			  gimple_set_lhs (new_stmt, final_mask);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			}
		      else if (final_mask)
			{
			  new_stmt = gimple_build_assign (NULL_TREE,
							  VEC_UNPACK_HI_EXPR,
							  final_mask);
			  final_mask = make_ssa_name
			    (truth_type_for (gs_info.offset_vectype));
			  gimple_set_lhs (new_stmt, final_mask);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			}

		      new_stmt = vect_build_one_gather_load_call
				   (vinfo, stmt_info, gsi, &gs_info,
				    dataref_ptr,
				    vec_offsets[2 * vec_num * j + 2 * i + 1],
				    final_mask);
		      tree high = make_ssa_name (vectype);
		      gimple_set_lhs (new_stmt, high);
		      vect_finish_stmt_generation (vinfo, stmt_info,
						   new_stmt, gsi);

		      /* compose low + high.  */
		      int count = nunits.to_constant ();
		      vec_perm_builder sel (count, count, 1);
		      sel.quick_grow (count);
		      for (int i = 0; i < count; ++i)
			sel[i] = i < count / 2 ? i : i + count / 2;
		      vec_perm_indices indices (sel, 2, count);
		      tree perm_mask
			= vect_gen_perm_mask_checked (vectype, indices);
		      new_stmt = gimple_build_assign (NULL_TREE,
						      VEC_PERM_EXPR,
						      low, high, perm_mask);
		      data_ref = NULL_TREE;
		    }
		  else if (known_eq (nunits * 2, offset_nunits))
		    {
		      /* We have a offset vector with double the number of
			 lanes.  Select the low/high part accordingly.  */
		      vec_offset = vec_offsets[(vec_num * j + i) / 2];
		      if ((vec_num * j + i) & 1)
			{
			  int count = offset_nunits.to_constant ();
			  vec_perm_builder sel (count, count, 1);
			  sel.quick_grow (count);
			  for (int i = 0; i < count; ++i)
			    sel[i] = i | (count / 2);
			  vec_perm_indices indices (sel, 2, count);
			  tree perm_mask = vect_gen_perm_mask_checked
					     (TREE_TYPE (vec_offset), indices);
			  new_stmt = gimple_build_assign (NULL_TREE,
							  VEC_PERM_EXPR,
							  vec_offset,
							  vec_offset,
							  perm_mask);
			  vec_offset = make_ssa_name (TREE_TYPE (vec_offset));
			  gimple_set_lhs (new_stmt, vec_offset);
			  vect_finish_stmt_generation (vinfo, stmt_info,
						       new_stmt, gsi);
			}
		      new_stmt = vect_build_one_gather_load_call
				   (vinfo, stmt_info, gsi, &gs_info,
				    dataref_ptr, vec_offset, final_mask);
		      data_ref = NULL_TREE;
		    }
		  else
		    gcc_unreachable ();
		}
	      else
		{
		  /* Emulated gather-scatter.  */
		  gcc_assert (!final_mask);
		  unsigned HOST_WIDE_INT const_nunits = nunits.to_constant ();
		  if (costing_p)
		    {
		      /* For emulated gathers N offset vector element
			 offset add is consumed by the load).  */
		      inside_cost = record_stmt_cost (cost_vec, const_nunits,
						      vec_to_scalar, stmt_info,
						      slp_node, 0, vect_body);
		      /* N scalar loads plus gathering them into a
			 vector.  */
		      inside_cost
			= record_stmt_cost (cost_vec, const_nunits, scalar_load,
					    stmt_info, 0, vect_body);
		      inside_cost
			= record_stmt_cost (cost_vec, 1, vec_construct,
					    stmt_info, slp_node, 0, vect_body);
		      continue;
		    }
		  unsigned HOST_WIDE_INT const_offset_nunits
		    = TYPE_VECTOR_SUBPARTS (gs_info.offset_vectype)
			.to_constant ();
		  vec<constructor_elt, va_gc> *ctor_elts;
		  vec_alloc (ctor_elts, const_nunits);
		  gimple_seq stmts = NULL;
		  /* We support offset vectors with more elements
		     than the data vector for now.  */
		  unsigned HOST_WIDE_INT factor
		    = const_offset_nunits / const_nunits;
		  vec_offset = vec_offsets[(vec_num * j + i) / factor];
		  unsigned elt_offset
		    = ((vec_num * j + i) % factor) * const_nunits;
		  tree idx_type = TREE_TYPE (TREE_TYPE (vec_offset));
		  tree scale = size_int (gs_info.scale);
		  align = get_object_alignment (DR_REF (first_dr_info->dr));
		  tree ltype = build_aligned_type (TREE_TYPE (vectype), align);
		  for (unsigned k = 0; k < const_nunits; ++k)
		    {
		      tree boff = size_binop (MULT_EXPR, TYPE_SIZE (idx_type),
					      bitsize_int (k + elt_offset));
		      tree idx
			= gimple_build (&stmts, BIT_FIELD_REF, idx_type,
					vec_offset, TYPE_SIZE (idx_type), boff);
		      idx = gimple_convert (&stmts, sizetype, idx);
		      idx = gimple_build (&stmts, MULT_EXPR, sizetype, idx,
					  scale);
		      tree ptr = gimple_build (&stmts, PLUS_EXPR,
					       TREE_TYPE (dataref_ptr),
					       dataref_ptr, idx);
		      ptr = gimple_convert (&stmts, ptr_type_node, ptr);
		      tree elt = make_ssa_name (TREE_TYPE (vectype));
		      tree ref = build2 (MEM_REF, ltype, ptr,
					 build_int_cst (ref_type, 0));
		      new_stmt = gimple_build_assign (elt, ref);
		      gimple_set_vuse (new_stmt, gimple_vuse (gsi_stmt (*gsi)));
		      gimple_seq_add_stmt (&stmts, new_stmt);
		      CONSTRUCTOR_APPEND_ELT (ctor_elts, NULL_TREE, elt);
		    }
		  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
		  new_stmt = gimple_build_assign (
		    NULL_TREE, build_constructor (vectype, ctor_elts));
		  data_ref = NULL_TREE;
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
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

	      /* Store vector loads in the corresponding SLP_NODE.  */
	      if (slp)
		slp_node->push_vec_def (new_stmt);
	    }

	  if (!slp && !costing_p)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}

      if (!slp && !costing_p)
	*vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

      if (costing_p && dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "vect_model_load_cost: inside_cost = %u, "
			 "prologue_cost = %u .\n",
			 inside_cost, prologue_cost);
      return true;
    }

  poly_uint64 group_elt = 0;
  unsigned int inside_cost = 0, prologue_cost = 0;
  /* For costing some adjacent vector loads, we'd like to cost with
     the total number of them once instead of cost each one by one. */
  unsigned int n_adjacent_loads = 0;
  for (j = 0; j < ncopies; j++)
    {
      /* 1. Create the vector or array pointer update chain.  */
      if (j == 0 && !costing_p)
	{
	  bool simd_lane_access_p
	    = STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) != 0;
	  if (simd_lane_access_p
	      && TREE_CODE (DR_BASE_ADDRESS (first_dr_info->dr)) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (first_dr_info->dr), 0))
	      && integer_zerop (get_dr_vinfo_offset (vinfo, first_dr_info))
	      && integer_zerop (DR_INIT (first_dr_info->dr))
	      && alias_sets_conflict_p (get_alias_set (aggr_type),
					get_alias_set (TREE_TYPE (ref_type)))
	      && (alignment_support_scheme == dr_aligned
		  || alignment_support_scheme == dr_unaligned_supported))
	    {
	      dataref_ptr = unshare_expr (DR_BASE_ADDRESS (first_dr_info->dr));
	      dataref_offset = build_int_cst (ref_type, 0);
	    }
	  else if (diff_first_stmt_info)
	    {
	      dataref_ptr
		= vect_create_data_ref_ptr (vinfo, first_stmt_info_for_drptr,
					    aggr_type, at_loop, offset, &dummy,
					    gsi, &ptr_incr, simd_lane_access_p,
					    bump);
	      /* Adjust the pointer by the difference to first_stmt.  */
	      data_reference_p ptrdr
		= STMT_VINFO_DATA_REF (first_stmt_info_for_drptr);
	      tree diff
		= fold_convert (sizetype,
				size_binop (MINUS_EXPR,
					    DR_INIT (first_dr_info->dr),
					    DR_INIT (ptrdr)));
	      dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					     stmt_info, diff);
	      if (alignment_support_scheme == dr_explicit_realign)
		{
		  msq = vect_setup_realignment (vinfo,
						first_stmt_info_for_drptr, gsi,
						&realignment_token,
						alignment_support_scheme,
						dataref_ptr, &at_loop);
		  gcc_assert (!compute_in_loop);
		}
	    }
	  else
	    dataref_ptr
	      = vect_create_data_ref_ptr (vinfo, first_stmt_info, aggr_type,
					  at_loop,
					  offset, &dummy, gsi, &ptr_incr,
					  simd_lane_access_p, bump);
	}
      else if (!costing_p)
	{
	  gcc_assert (!LOOP_VINFO_USING_SELECT_VL_P (loop_vinfo));
	  if (dataref_offset)
	    dataref_offset = int_const_binop (PLUS_EXPR, dataref_offset,
					      bump);
	  else
	    dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					   stmt_info, bump);
	}

      if (grouped_load || slp_perm)
	dr_chain.create (vec_num);

      gimple *new_stmt = NULL;
      for (i = 0; i < vec_num; i++)
	{
	  tree final_mask = NULL_TREE;
	  tree final_len = NULL_TREE;
	  tree bias = NULL_TREE;
	  if (!costing_p)
	    {
	      if (mask)
		vec_mask = vec_masks[vec_num * j + i];
	      if (loop_masks)
		final_mask = vect_get_loop_mask (loop_vinfo, gsi, loop_masks,
						 vec_num * ncopies, vectype,
						 vec_num * j + i);
	      if (vec_mask)
		final_mask = prepare_vec_mask (loop_vinfo, mask_vectype,
					       final_mask, vec_mask, gsi);

	      if (i > 0)
		dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr,
					       gsi, stmt_info, bump);
	    }

	  /* 2. Create the vector-load in the loop.  */
	  switch (alignment_support_scheme)
	    {
	    case dr_aligned:
	    case dr_unaligned_supported:
	      {
		if (costing_p)
		  break;

		unsigned int misalign;
		unsigned HOST_WIDE_INT align;
		align = known_alignment (DR_TARGET_ALIGNMENT (first_dr_info));
		if (alignment_support_scheme == dr_aligned)
		  misalign = 0;
		else if (misalignment == DR_MISALIGNMENT_UNKNOWN)
		  {
		    align
		      = dr_alignment (vect_dr_behavior (vinfo, first_dr_info));
		    misalign = 0;
		  }
		else
		  misalign = misalignment;
		if (dataref_offset == NULL_TREE
		    && TREE_CODE (dataref_ptr) == SSA_NAME)
		  set_ptr_info_alignment (get_ptr_info (dataref_ptr), align,
					  misalign);
		align = least_bit_hwi (misalign | align);

		/* Compute IFN when LOOP_LENS or final_mask valid.  */
		machine_mode vmode = TYPE_MODE (vectype);
		machine_mode new_vmode = vmode;
		internal_fn partial_ifn = IFN_LAST;
		if (loop_lens)
		  {
		    opt_machine_mode new_ovmode
		      = get_len_load_store_mode (vmode, true, &partial_ifn);
		    new_vmode = new_ovmode.require ();
		    unsigned factor
		      = (new_ovmode == vmode) ? 1 : GET_MODE_UNIT_SIZE (vmode);
		    final_len = vect_get_loop_len (loop_vinfo, gsi, loop_lens,
						   vec_num * ncopies, vectype,
						   vec_num * j + i, factor);
		  }
		else if (final_mask)
		  {
		    if (!can_vec_mask_load_store_p (
			  vmode, TYPE_MODE (TREE_TYPE (final_mask)), true,
			  &partial_ifn))
		      gcc_unreachable ();
		  }

		if (partial_ifn == IFN_MASK_LEN_LOAD)
		  {
		    if (!final_len)
		      {
			/* Pass VF value to 'len' argument of
			   MASK_LEN_LOAD if LOOP_LENS is invalid.  */
			final_len = size_int (TYPE_VECTOR_SUBPARTS (vectype));
		      }
		    if (!final_mask)
		      {
			/* Pass all ones value to 'mask' argument of
			   MASK_LEN_LOAD if final_mask is invalid.  */
			mask_vectype = truth_type_for (vectype);
			final_mask = build_minus_one_cst (mask_vectype);
		      }
		  }
		if (final_len)
		  {
		    signed char biasval
		      = LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);

		    bias = build_int_cst (intQI_type_node, biasval);
		  }

		if (final_len)
		  {
		    tree ptr = build_int_cst (ref_type, align * BITS_PER_UNIT);
		    gcall *call;
		    if (partial_ifn == IFN_MASK_LEN_LOAD)
		      call = gimple_build_call_internal (IFN_MASK_LEN_LOAD, 5,
							 dataref_ptr, ptr,
							 final_mask, final_len,
							 bias);
		    else
		      call = gimple_build_call_internal (IFN_LEN_LOAD, 4,
							 dataref_ptr, ptr,
							 final_len, bias);
		    gimple_call_set_nothrow (call, true);
		    new_stmt = call;
		    data_ref = NULL_TREE;

		    /* Need conversion if it's wrapped with VnQI.  */
		    if (vmode != new_vmode)
		      {
			tree new_vtype = build_vector_type_for_mode (
			  unsigned_intQI_type_node, new_vmode);
			tree var
			  = vect_get_new_ssa_name (new_vtype, vect_simple_var);
			gimple_set_lhs (call, var);
			vect_finish_stmt_generation (vinfo, stmt_info, call,
						     gsi);
			tree op = build1 (VIEW_CONVERT_EXPR, vectype, var);
			new_stmt = gimple_build_assign (vec_dest,
							VIEW_CONVERT_EXPR, op);
		      }
		  }
		else if (final_mask)
		  {
		    tree ptr = build_int_cst (ref_type, align * BITS_PER_UNIT);
		    gcall *call = gimple_build_call_internal (IFN_MASK_LOAD, 3,
							      dataref_ptr, ptr,
							      final_mask);
		    gimple_call_set_nothrow (call, true);
		    new_stmt = call;
		    data_ref = NULL_TREE;
		  }
		else
		  {
		    tree ltype = vectype;
		    tree new_vtype = NULL_TREE;
		    unsigned HOST_WIDE_INT gap = DR_GROUP_GAP (first_stmt_info);
		    unsigned int vect_align
		      = vect_known_alignment_in_bytes (first_dr_info, vectype);
		    /* Try to use a single smaller load when we are about
		       to load excess elements compared to the unrolled
		       scalar loop.  */
		    if (known_gt ((vec_num * j + i + 1) * nunits,
				       (group_size * vf - gap)))
		      {
			poly_uint64 remain = ((group_size * vf - gap)
					      - (vec_num * j + i) * nunits);
			if (known_ge ((vec_num * j + i + 1) * nunits
				      - (group_size * vf - gap), nunits))
			  /* DR will be unused.  */
			  ltype = NULL_TREE;
			else if (known_ge (vect_align,
					   tree_to_poly_uint64
					     (TYPE_SIZE_UNIT (vectype))))
			  /* Aligned access to excess elements is OK if
			     at least one element is accessed in the
			     scalar loop.  */
			  ;
			else if (known_gt (vect_align,
					   ((nunits - remain)
					    * vect_get_scalar_dr_size
						(first_dr_info))))
			  /* Aligned access to the gap area when there's
			     at least one element in it is OK.  */
			  ;
			else
			  {
			    /* remain should now be > 0 and < nunits.  */
			    unsigned num;
			    if (known_ne (remain, 0u)
				&& constant_multiple_p (nunits, remain, &num))
			      {
				tree ptype;
				new_vtype
				  = vector_vector_composition_type (vectype,
								    num,
								    &ptype);
				if (new_vtype)
				  ltype = ptype;
			      }
			    /* Else use multiple loads or a masked load?  */
			    /* For loop vectorization we now should have
			       an alternate type or LOOP_VINFO_PEELING_FOR_GAPS
			       set.  */
			    if (loop_vinfo)
			      gcc_assert (new_vtype
					  || LOOP_VINFO_PEELING_FOR_GAPS
					       (loop_vinfo));
			    /* But still reduce the access size to the next
			       required power-of-two so peeling a single
			       scalar iteration is sufficient.  */
			    unsigned HOST_WIDE_INT cremain;
			    if (remain.is_constant (&cremain))
			      {
				unsigned HOST_WIDE_INT cpart_size
				  = 1 << ceil_log2 (cremain);
				if (known_gt (nunits, cpart_size)
				    && constant_multiple_p (nunits, cpart_size,
							    &num))
				  {
				    tree ptype;
				    new_vtype
				      = vector_vector_composition_type (vectype,
									num,
									&ptype);
				    if (new_vtype)
				      ltype = ptype;
				  }
			      }
			  }
		      }
		    tree offset
		      = (dataref_offset ? dataref_offset
					: build_int_cst (ref_type, 0));
		    if (!ltype)
		      ;
		    else if (ltype != vectype
			     && memory_access_type == VMAT_CONTIGUOUS_REVERSE)
		      {
			poly_uint64 gap_offset
			  = (tree_to_poly_uint64 (TYPE_SIZE_UNIT (vectype))
			     - tree_to_poly_uint64 (TYPE_SIZE_UNIT (ltype)));
			tree gapcst = build_int_cstu (ref_type, gap_offset);
			offset = size_binop (PLUS_EXPR, offset, gapcst);
		      }
		    if (ltype)
		      {
			data_ref
			  = fold_build2 (MEM_REF, ltype, dataref_ptr, offset);
			if (alignment_support_scheme == dr_aligned)
			  ;
			else
			  TREE_TYPE (data_ref)
			    = build_aligned_type (TREE_TYPE (data_ref),
						  align * BITS_PER_UNIT);
		      }
		    if (!ltype)
		      data_ref = build_constructor (vectype, NULL);
		    else if (ltype != vectype)
		      {
			vect_copy_ref_info (data_ref,
					    DR_REF (first_dr_info->dr));
			tree tem = make_ssa_name (ltype);
			new_stmt = gimple_build_assign (tem, data_ref);
			vect_finish_stmt_generation (vinfo, stmt_info, new_stmt,
						     gsi);
			data_ref = NULL;
			vec<constructor_elt, va_gc> *v;
			/* We've computed 'num' above to statically two
			   or via constant_multiple_p.  */
			unsigned num
			  = (exact_div (tree_to_poly_uint64
					  (TYPE_SIZE_UNIT (vectype)),
					tree_to_poly_uint64
					  (TYPE_SIZE_UNIT (ltype)))
			     .to_constant ());
			vec_alloc (v, num);
			if (memory_access_type == VMAT_CONTIGUOUS_REVERSE)
			  {
			    while (--num)
			      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
						      build_zero_cst (ltype));
			    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, tem);
			  }
			else
			  {
			    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, tem);
			    while (--num)
			      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE,
						      build_zero_cst (ltype));
			  }
			gcc_assert (new_vtype != NULL_TREE);
			if (new_vtype == vectype)
			  new_stmt = gimple_build_assign (
			    vec_dest, build_constructor (vectype, v));
			else
			  {
			    tree new_vname = make_ssa_name (new_vtype);
			    new_stmt = gimple_build_assign (
			      new_vname, build_constructor (new_vtype, v));
			    vect_finish_stmt_generation (vinfo, stmt_info,
							 new_stmt, gsi);
			    new_stmt = gimple_build_assign (
			      vec_dest,
			      build1 (VIEW_CONVERT_EXPR, vectype, new_vname));
			  }
		      }
		  }
		break;
	      }
	    case dr_explicit_realign:
	      {
		if (costing_p)
		  break;
		tree ptr, bump;

		tree vs = size_int (TYPE_VECTOR_SUBPARTS (vectype));

		if (compute_in_loop)
		  msq = vect_setup_realignment (vinfo, first_stmt_info, gsi,
						&realignment_token,
						dr_explicit_realign,
						dataref_ptr, NULL);

		if (TREE_CODE (dataref_ptr) == SSA_NAME)
		  ptr = copy_ssa_name (dataref_ptr);
		else
		  ptr = make_ssa_name (TREE_TYPE (dataref_ptr));
		// For explicit realign the target alignment should be
		// known at compile time.
		unsigned HOST_WIDE_INT align
		  = DR_TARGET_ALIGNMENT (first_dr_info).to_constant ();
		new_stmt = gimple_build_assign (
		  ptr, BIT_AND_EXPR, dataref_ptr,
		  build_int_cst (TREE_TYPE (dataref_ptr),
				 -(HOST_WIDE_INT) align));
		vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
		data_ref
		  = build2 (MEM_REF, vectype, ptr, build_int_cst (ref_type, 0));
		vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
		vec_dest = vect_create_destination_var (scalar_dest, vectype);
		new_stmt = gimple_build_assign (vec_dest, data_ref);
		new_temp = make_ssa_name (vec_dest, new_stmt);
		gimple_assign_set_lhs (new_stmt, new_temp);
		gimple_move_vops (new_stmt, stmt_info->stmt);
		vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
		msq = new_temp;

		bump = size_binop (MULT_EXPR, vs, TYPE_SIZE_UNIT (elem_type));
		bump = size_binop (MINUS_EXPR, bump, size_one_node);
		ptr = bump_vector_ptr (vinfo, dataref_ptr, NULL, gsi, stmt_info,
				       bump);
		new_stmt = gimple_build_assign (
		  NULL_TREE, BIT_AND_EXPR, ptr,
		  build_int_cst (TREE_TYPE (ptr), -(HOST_WIDE_INT) align));
		if (TREE_CODE (ptr) == SSA_NAME)
		  ptr = copy_ssa_name (ptr, new_stmt);
		else
		  ptr = make_ssa_name (TREE_TYPE (ptr), new_stmt);
		gimple_assign_set_lhs (new_stmt, ptr);
		vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
		data_ref
		  = build2 (MEM_REF, vectype, ptr, build_int_cst (ref_type, 0));
		break;
	      }
	    case dr_explicit_realign_optimized:
	      {
		if (costing_p)
		  break;
		if (TREE_CODE (dataref_ptr) == SSA_NAME)
		  new_temp = copy_ssa_name (dataref_ptr);
		else
		  new_temp = make_ssa_name (TREE_TYPE (dataref_ptr));
		// We should only be doing this if we know the target
		// alignment at compile time.
		unsigned HOST_WIDE_INT align
		  = DR_TARGET_ALIGNMENT (first_dr_info).to_constant ();
		new_stmt = gimple_build_assign (
		  new_temp, BIT_AND_EXPR, dataref_ptr,
		  build_int_cst (TREE_TYPE (dataref_ptr),
				 -(HOST_WIDE_INT) align));
		vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
		data_ref = build2 (MEM_REF, vectype, new_temp,
				   build_int_cst (ref_type, 0));
		break;
	      }
	    default:
	      gcc_unreachable ();
	    }

	  /* One common place to cost the above vect load for different
	     alignment support schemes.  */
	  if (costing_p)
	    {
	      /* For VMAT_CONTIGUOUS_PERMUTE if it's grouped load, we
		 only need to take care of the first stmt, whose
		 stmt_info is first_stmt_info, vec_num iterating on it
		 will cover the cost for the remaining, it's consistent
		 with transforming.  For the prologue cost for realign,
		 we only need to count it once for the whole group.  */
	      bool first_stmt_info_p = first_stmt_info == stmt_info;
	      bool add_realign_cost = first_stmt_info_p && i == 0;
	      if (memory_access_type == VMAT_CONTIGUOUS
		  || memory_access_type == VMAT_CONTIGUOUS_REVERSE
		  || (memory_access_type == VMAT_CONTIGUOUS_PERMUTE
		      && (!grouped_load || first_stmt_info_p)))
		{
		  /* Leave realign cases alone to keep them simple.  */
		  if (alignment_support_scheme == dr_explicit_realign_optimized
		      || alignment_support_scheme == dr_explicit_realign)
		    vect_get_load_cost (vinfo, stmt_info, 1,
					alignment_support_scheme, misalignment,
					add_realign_cost, &inside_cost,
					&prologue_cost, cost_vec, cost_vec,
					true);
		  else
		    n_adjacent_loads++;
		}
	    }
	  else
	    {
	      vec_dest = vect_create_destination_var (scalar_dest, vectype);
	      /* DATA_REF is null if we've already built the statement.  */
	      if (data_ref)
		{
		  vect_copy_ref_info (data_ref, DR_REF (first_dr_info->dr));
		  new_stmt = gimple_build_assign (vec_dest, data_ref);
		}
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      gimple_set_lhs (new_stmt, new_temp);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }

	  /* 3. Handle explicit realignment if necessary/supported.
	     Create in loop:
	       vec_dest = realign_load (msq, lsq, realignment_token)  */
	  if (!costing_p
	      && (alignment_support_scheme == dr_explicit_realign_optimized
		  || alignment_support_scheme == dr_explicit_realign))
	    {
	      lsq = gimple_assign_lhs (new_stmt);
	      if (!realignment_token)
		realignment_token = dataref_ptr;
	      vec_dest = vect_create_destination_var (scalar_dest, vectype);
	      new_stmt = gimple_build_assign (vec_dest, REALIGN_LOAD_EXPR, msq,
					      lsq, realignment_token);
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      gimple_assign_set_lhs (new_stmt, new_temp);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);

	      if (alignment_support_scheme == dr_explicit_realign_optimized)
		{
		  gcc_assert (phi);
		  if (i == vec_num - 1 && j == ncopies - 1)
		    add_phi_arg (phi, lsq, loop_latch_edge (containing_loop),
				 UNKNOWN_LOCATION);
		  msq = lsq;
		}
	    }

	  if (memory_access_type == VMAT_CONTIGUOUS_REVERSE)
	    {
	      if (costing_p)
		inside_cost = record_stmt_cost (cost_vec, 1, vec_perm,
						stmt_info, 0, vect_body);
	      else
		{
		  tree perm_mask = perm_mask_for_reverse (vectype);
		  new_temp = permute_vec_elements (vinfo, new_temp, new_temp,
						   perm_mask, stmt_info, gsi);
		  new_stmt = SSA_NAME_DEF_STMT (new_temp);
		}
	    }

	  /* Collect vector loads and later create their permutation in
	     vect_transform_grouped_load ().  */
	  if (!costing_p && (grouped_load || slp_perm))
	    dr_chain.quick_push (new_temp);

	  /* Store vector loads in the corresponding SLP_NODE.  */
	  if (!costing_p && slp && !slp_perm)
	    slp_node->push_vec_def (new_stmt);

	  /* With SLP permutation we load the gaps as well, without
	     we need to skip the gaps after we manage to fully load
	     all elements.  group_gap_adj is DR_GROUP_SIZE here.  */
	  group_elt += nunits;
	  if (!costing_p
	      && maybe_ne (group_gap_adj, 0U)
	      && !slp_perm
	      && known_eq (group_elt, group_size - group_gap_adj))
	    {
	      poly_wide_int bump_val
		= (wi::to_wide (TYPE_SIZE_UNIT (elem_type)) * group_gap_adj);
	      if (tree_int_cst_sgn (vect_dr_behavior (vinfo, dr_info)->step)
		  == -1)
		bump_val = -bump_val;
	      tree bump = wide_int_to_tree (sizetype, bump_val);
	      dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					     stmt_info, bump);
	      group_elt = 0;
	    }
	}
      /* Bump the vector pointer to account for a gap or for excess
	 elements loaded for a permuted SLP load.  */
      if (!costing_p
	  && maybe_ne (group_gap_adj, 0U)
	  && slp_perm)
	{
	  poly_wide_int bump_val
	    = (wi::to_wide (TYPE_SIZE_UNIT (elem_type)) * group_gap_adj);
	  if (tree_int_cst_sgn (vect_dr_behavior (vinfo, dr_info)->step) == -1)
	    bump_val = -bump_val;
	  tree bump = wide_int_to_tree (sizetype, bump_val);
	  dataref_ptr = bump_vector_ptr (vinfo, dataref_ptr, ptr_incr, gsi,
					 stmt_info, bump);
	}

      if (slp && !slp_perm)
	continue;

      if (slp_perm)
	{
	  unsigned n_perms;
	  /* For SLP we know we've seen all possible uses of dr_chain so
	     direct vect_transform_slp_perm_load to DCE the unused parts.
	     ???  This is a hack to prevent compile-time issues as seen
	     in PR101120 and friends.  */
	  if (costing_p)
	    {
	      vect_transform_slp_perm_load (vinfo, slp_node, vNULL, nullptr, vf,
					    true, &n_perms, nullptr);
	      inside_cost = record_stmt_cost (cost_vec, n_perms, vec_perm,
					      stmt_info, 0, vect_body);
	    }
	  else
	    {
	      bool ok = vect_transform_slp_perm_load (vinfo, slp_node, dr_chain,
						      gsi, vf, false, &n_perms,
						      nullptr, true);
	      gcc_assert (ok);
	    }
	}
      else
	{
	  if (grouped_load)
	    {
	      gcc_assert (memory_access_type == VMAT_CONTIGUOUS_PERMUTE);
	      /* We assume that the cost of a single load-lanes instruction
		 is equivalent to the cost of DR_GROUP_SIZE separate loads.
		 If a grouped access is instead being provided by a
		 load-and-permute operation, include the cost of the
		 permutes.  */
	      if (costing_p && first_stmt_info == stmt_info)
		{
		  /* Uses an even and odd extract operations or shuffle
		     operations for each needed permute.  */
		  int group_size = DR_GROUP_SIZE (first_stmt_info);
		  int nstmts = ceil_log2 (group_size) * group_size;
		  inside_cost += record_stmt_cost (cost_vec, nstmts, vec_perm,
						   stmt_info, 0, vect_body);

		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "vect_model_load_cost:"
				     "strided group_size = %d .\n",
				     group_size);
		}
	      else if (!costing_p)
		{
		  vect_transform_grouped_load (vinfo, stmt_info, dr_chain,
					       group_size, gsi);
		  *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];
		}
	    }
	  else if (!costing_p)
	    STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
	}
      dr_chain.release ();
    }
  if (!slp && !costing_p)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

  if (costing_p)
    {
      gcc_assert (memory_access_type == VMAT_CONTIGUOUS
		  || memory_access_type == VMAT_CONTIGUOUS_REVERSE
		  || memory_access_type == VMAT_CONTIGUOUS_PERMUTE);
      if (n_adjacent_loads > 0)
	vect_get_load_cost (vinfo, stmt_info, n_adjacent_loads,
			    alignment_support_scheme, misalignment, false,
			    &inside_cost, &prologue_cost, cost_vec, cost_vec,
			    true);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "vect_model_load_cost: inside_cost = %u, "
			 "prologue_cost = %u .\n",
			 inside_cost, prologue_cost);
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
vect_is_simple_cond (tree cond, vec_info *vinfo, stmt_vec_info stmt_info,
		     slp_tree slp_node, tree *comp_vectype,
		     enum vect_def_type *dts, tree vectype)
{
  tree lhs, rhs;
  tree vectype1 = NULL_TREE, vectype2 = NULL_TREE;
  slp_tree slp_op;

  /* Mask case.  */
  if (TREE_CODE (cond) == SSA_NAME
      && VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (cond)))
    {
      if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 0, &cond,
			       &slp_op, &dts[0], comp_vectype)
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
      if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 0,
			       &lhs, &slp_op, &dts[0], &vectype1))
	return false;
    }
  else if (TREE_CODE (lhs) == INTEGER_CST || TREE_CODE (lhs) == REAL_CST
	   || TREE_CODE (lhs) == FIXED_CST)
    dts[0] = vect_constant_def;
  else
    return false;

  if (TREE_CODE (rhs) == SSA_NAME)
    {
      if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 1,
			       &rhs, &slp_op, &dts[1], &vectype2))
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
  if (! *comp_vectype)
    {
      tree scalar_type = TREE_TYPE (lhs);
      if (VECT_SCALAR_BOOLEAN_TYPE_P (scalar_type))
	*comp_vectype = truth_type_for (vectype);
      else
	{
	  /* If we can widen the comparison to match vectype do so.  */
	  if (INTEGRAL_TYPE_P (scalar_type)
	      && !slp_node
	      && tree_int_cst_lt (TYPE_SIZE (scalar_type),
				  TYPE_SIZE (TREE_TYPE (vectype))))
	    scalar_type = build_nonstandard_integer_type
	      (vector_element_bits (vectype), TYPE_UNSIGNED (scalar_type));
	  *comp_vectype = get_vectype_for_scalar_type (vinfo, scalar_type,
						       slp_node);
	}
    }

  return true;
}

/* vectorizable_condition.

   Check if STMT_INFO is conditional modify expression that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   stmt using VEC_COND_EXPR  to replace it, put it in VEC_STMT, and insert it
   at GSI.

   When STMT_INFO is vectorized as a nested cycle, for_reduction is true.

   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_condition (vec_info *vinfo,
			stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			gimple **vec_stmt,
			slp_tree slp_node, stmt_vector_for_cost *cost_vec)
{
  tree scalar_dest = NULL_TREE;
  tree vec_dest = NULL_TREE;
  tree cond_expr, cond_expr0 = NULL_TREE, cond_expr1 = NULL_TREE;
  tree then_clause, else_clause;
  tree comp_vectype = NULL_TREE;
  tree vec_cond_lhs = NULL_TREE, vec_cond_rhs = NULL_TREE;
  tree vec_then_clause = NULL_TREE, vec_else_clause = NULL_TREE;
  tree vec_compare;
  tree new_temp;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  enum vect_def_type dts[4]
    = {vect_unknown_def_type, vect_unknown_def_type,
       vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 4;
  int ncopies;
  int vec_num;
  enum tree_code code, cond_code, bitop1 = NOP_EXPR, bitop2 = NOP_EXPR;
  int i;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  vec<tree> vec_oprnds2 = vNULL;
  vec<tree> vec_oprnds3 = vNULL;
  tree vec_cmp_type;
  bool masked = false;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  /* Is vectorizable conditional operation?  */
  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (code != COND_EXPR)
    return false;

  stmt_vec_info reduc_info = NULL;
  int reduc_index = -1;
  vect_reduction_type reduction_type = TREE_CODE_REDUCTION;
  bool for_reduction
    = STMT_VINFO_REDUC_DEF (vect_orig_stmt (stmt_info)) != NULL;
  if (for_reduction)
    {
      if (slp_node && SLP_TREE_LANES (slp_node) > 1)
	return false;
      reduc_info = info_for_reduction (vinfo, stmt_info);
      reduction_type = STMT_VINFO_REDUC_TYPE (reduc_info);
      reduc_index = STMT_VINFO_REDUC_IDX (stmt_info);
      gcc_assert (reduction_type != EXTRACT_LAST_REDUCTION
		  || reduc_index != -1);
    }
  else
    {
      if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
	return false;
    }

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree vectype1 = NULL_TREE, vectype2 = NULL_TREE;

  if (slp_node)
    {
      ncopies = 1;
      vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
    }
  else
    {
      ncopies = vect_get_num_copies (loop_vinfo, vectype);
      vec_num = 1;
    }

  gcc_assert (ncopies >= 1);
  if (for_reduction && ncopies > 1)
    return false; /* FORNOW */

  cond_expr = gimple_assign_rhs1 (stmt);

  if (!vect_is_simple_cond (cond_expr, vinfo, stmt_info, slp_node,
			    &comp_vectype, &dts[0], vectype)
      || !comp_vectype)
    return false;

  unsigned op_adjust = COMPARISON_CLASS_P (cond_expr) ? 1 : 0;
  slp_tree then_slp_node, else_slp_node;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 1 + op_adjust,
			   &then_clause, &then_slp_node, &dts[2], &vectype1))
    return false;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node, 2 + op_adjust,
			   &else_clause, &else_slp_node, &dts[3], &vectype2))
    return false;

  if (vectype1 && !useless_type_conversion_p (vectype, vectype1))
    return false;

  if (vectype2 && !useless_type_conversion_p (vectype, vectype2))
    return false;

  masked = !COMPARISON_CLASS_P (cond_expr);
  vec_cmp_type = truth_type_for (comp_vectype);

  if (vec_cmp_type == NULL_TREE)
    return false;

  cond_code = TREE_CODE (cond_expr);
  if (!masked)
    {
      cond_expr0 = TREE_OPERAND (cond_expr, 0);
      cond_expr1 = TREE_OPERAND (cond_expr, 1);
    }

  /* For conditional reductions, the "then" value needs to be the candidate
     value calculated by this iteration while the "else" value needs to be
     the result carried over from previous iterations.  If the COND_EXPR
     is the other way around, we need to swap it.  */
  bool must_invert_cmp_result = false;
  if (reduction_type == EXTRACT_LAST_REDUCTION && reduc_index == 1)
    {
      if (masked)
	must_invert_cmp_result = true;
      else
	{
	  bool honor_nans = HONOR_NANS (TREE_TYPE (cond_expr0));
	  tree_code new_code = invert_tree_comparison (cond_code, honor_nans);
	  if (new_code == ERROR_MARK)
	    must_invert_cmp_result = true;
	  else
	    {
	      cond_code = new_code;
	      /* Make sure we don't accidentally use the old condition.  */
	      cond_expr = NULL_TREE;
	    }
	}
      /* ???  The vectorized operand query below doesn't allow swapping
	 this way for SLP.  */
      if (slp_node)
	return false;
      std::swap (then_clause, else_clause);
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

  if (TREE_CODE_CLASS (cond_code) == tcc_comparison
      && reduction_type == EXTRACT_LAST_REDUCTION
      && !expand_vec_cmp_expr_p (comp_vectype, vec_cmp_type, cond_code))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "reduction comparison operation not supported.\n");
      return false;
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

      vect_cost_for_stmt kind = vector_stmt;
      if (reduction_type == EXTRACT_LAST_REDUCTION)
	/* Count one reduction-like operation per vector.  */
	kind = vec_to_scalar;
      else if (!expand_vec_cond_expr_p (vectype, comp_vectype, cond_code)
	       && (masked
		   || (!expand_vec_cmp_expr_p (comp_vectype, vec_cmp_type,
					       cond_code)
		       || !expand_vec_cond_expr_p (vectype, vec_cmp_type))))
	return false;

      if (slp_node
	  && (!vect_maybe_update_slp_op_vectype
		 (SLP_TREE_CHILDREN (slp_node)[0], comp_vectype)
	      || (op_adjust == 1
		  && !vect_maybe_update_slp_op_vectype
			(SLP_TREE_CHILDREN (slp_node)[1], comp_vectype))
	      || !vect_maybe_update_slp_op_vectype (then_slp_node, vectype)
	      || !vect_maybe_update_slp_op_vectype (else_slp_node, vectype)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}

      if (loop_vinfo && for_reduction
	  && LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo))
	{
	  if (reduction_type == EXTRACT_LAST_REDUCTION)
	    {
	      if (direct_internal_fn_supported_p (IFN_LEN_FOLD_EXTRACT_LAST,
						  vectype, OPTIMIZE_FOR_SPEED))
		vect_record_loop_len (loop_vinfo,
				      &LOOP_VINFO_LENS (loop_vinfo),
				      ncopies * vec_num, vectype, 1);
	      else
		vect_record_loop_mask (loop_vinfo,
				       &LOOP_VINFO_MASKS (loop_vinfo),
				       ncopies * vec_num, vectype, NULL);
	    }
	  /* Extra inactive lanes should be safe for vect_nested_cycle.  */
	  else if (STMT_VINFO_DEF_TYPE (reduc_info) != vect_nested_cycle)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "conditional reduction prevents the use"
				 " of partial vectors.\n");
	      LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo) = false;
	    }
	}

      STMT_VINFO_TYPE (stmt_info) = condition_vec_info_type;
      vect_model_simple_cost (vinfo, stmt_info, ncopies, dts, ndts, slp_node,
			      cost_vec, kind);
      return true;
    }

  /* Transform.  */

  /* Handle def.  */
  scalar_dest = gimple_assign_lhs (stmt);
  if (reduction_type != EXTRACT_LAST_REDUCTION)
    vec_dest = vect_create_destination_var (scalar_dest, vectype);

  bool swap_cond_operands = false;

  /* See whether another part of the vectorized code applies a loop
     mask to the condition, or to its inverse.  */

  vec_loop_masks *masks = NULL;
  vec_loop_lens *lens = NULL;
  if (loop_vinfo && LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo))
    {
      if (reduction_type == EXTRACT_LAST_REDUCTION)
	lens = &LOOP_VINFO_LENS (loop_vinfo);
    }
  else if (loop_vinfo && LOOP_VINFO_FULLY_MASKED_P (loop_vinfo))
    {
      if (reduction_type == EXTRACT_LAST_REDUCTION)
	masks = &LOOP_VINFO_MASKS (loop_vinfo);
      else
	{
	  scalar_cond_masked_key cond (cond_expr, ncopies);
	  if (loop_vinfo->scalar_cond_masked_set.contains (cond))
	    masks = &LOOP_VINFO_MASKS (loop_vinfo);
	  else
	    {
	      bool honor_nans = HONOR_NANS (TREE_TYPE (cond.op0));
	      tree_code orig_code = cond.code;
	      cond.code = invert_tree_comparison (cond.code, honor_nans);
	      if (!masked && loop_vinfo->scalar_cond_masked_set.contains (cond))
		{
		  masks = &LOOP_VINFO_MASKS (loop_vinfo);
		  cond_code = cond.code;
		  swap_cond_operands = true;
		}
	      else
		{
		  /* Try the inverse of the current mask.  We check if the
		     inverse mask is live and if so we generate a negate of
		     the current mask such that we still honor NaNs.  */
		  cond.inverted_p = true;
		  cond.code = orig_code;
		  if (loop_vinfo->scalar_cond_masked_set.contains (cond))
		    {
		      masks = &LOOP_VINFO_MASKS (loop_vinfo);
		      cond_code = cond.code;
		      swap_cond_operands = true;
		      must_invert_cmp_result = true;
		    }
		}
	    }
	}
    }

  /* Handle cond expr.  */
  if (masked)
    vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		       cond_expr, comp_vectype, &vec_oprnds0,
		       then_clause, vectype, &vec_oprnds2,
		       reduction_type != EXTRACT_LAST_REDUCTION
		       ? else_clause : NULL, vectype, &vec_oprnds3);
  else
    vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		       cond_expr0, comp_vectype, &vec_oprnds0,
		       cond_expr1, comp_vectype, &vec_oprnds1,
		       then_clause, vectype, &vec_oprnds2,
		       reduction_type != EXTRACT_LAST_REDUCTION
		       ? else_clause : NULL, vectype, &vec_oprnds3);

  if (reduction_type == EXTRACT_LAST_REDUCTION)
    vec_else_clause = else_clause;

  /* Arguments are ready.  Create the new vector stmt.  */
  FOR_EACH_VEC_ELT (vec_oprnds0, i, vec_cond_lhs)
    {
      vec_then_clause = vec_oprnds2[i];
      if (reduction_type != EXTRACT_LAST_REDUCTION)
	vec_else_clause = vec_oprnds3[i];

      if (swap_cond_operands)
	std::swap (vec_then_clause, vec_else_clause);

      if (masked)
	vec_compare = vec_cond_lhs;
      else
	{
	  vec_cond_rhs = vec_oprnds1[i];
	  if (bitop1 == NOP_EXPR)
	    {
	      gimple_seq stmts = NULL;
	      vec_compare = gimple_build (&stmts, cond_code, vec_cmp_type,
					   vec_cond_lhs, vec_cond_rhs);
	      gsi_insert_before (gsi, stmts, GSI_SAME_STMT);
	    }
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
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      if (bitop2 == NOP_EXPR)
		vec_compare = new_temp;
	      else if (bitop2 == BIT_NOT_EXPR
		       && reduction_type != EXTRACT_LAST_REDUCTION)
		{
		  /* Instead of doing ~x ? y : z do x ? z : y.  */
		  vec_compare = new_temp;
		  std::swap (vec_then_clause, vec_else_clause);
		}
	      else
		{
		  vec_compare = make_ssa_name (vec_cmp_type);
		  if (bitop2 == BIT_NOT_EXPR)
		    new_stmt
		      = gimple_build_assign (vec_compare, bitop2, new_temp);
		  else
		    new_stmt
		      = gimple_build_assign (vec_compare, bitop2,
					     vec_cond_lhs, new_temp);
		  vect_finish_stmt_generation (vinfo, stmt_info,
					       new_stmt, gsi);
		}
	    }
	}

      /* If we decided to apply a loop mask to the result of the vector
	 comparison, AND the comparison with the mask now.  Later passes
	 should then be able to reuse the AND results between mulitple
	 vector statements.

	 For example:
	 for (int i = 0; i < 100; ++i)
	 x[i] = y[i] ? z[i] : 10;

	 results in following optimized GIMPLE:

	 mask__35.8_43 = vect__4.7_41 != { 0, ... };
	 vec_mask_and_46 = loop_mask_40 & mask__35.8_43;
	 _19 = &MEM[base: z_12(D), index: ivtmp_56, step: 4, offset: 0B];
	 vect_iftmp.11_47 = .MASK_LOAD (_19, 4B, vec_mask_and_46);
	 vect_iftmp.12_52 = VEC_COND_EXPR <vec_mask_and_46,
	 vect_iftmp.11_47, { 10, ... }>;

	 instead of using a masked and unmasked forms of
	 vec != { 0, ... } (masked in the MASK_LOAD,
	 unmasked in the VEC_COND_EXPR).  */

      /* Force vec_compare to be an SSA_NAME rather than a comparison,
	 in cases where that's necessary.  */

      tree len = NULL_TREE, bias = NULL_TREE;
      if (masks || lens || reduction_type == EXTRACT_LAST_REDUCTION)
	{
	  if (!is_gimple_val (vec_compare))
	    {
	      tree vec_compare_name = make_ssa_name (vec_cmp_type);
	      gassign *new_stmt = gimple_build_assign (vec_compare_name,
						       vec_compare);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      vec_compare = vec_compare_name;
	    }

	  if (must_invert_cmp_result)
	    {
	      tree vec_compare_name = make_ssa_name (vec_cmp_type);
	      gassign *new_stmt = gimple_build_assign (vec_compare_name,
						       BIT_NOT_EXPR,
						       vec_compare);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      vec_compare = vec_compare_name;
	    }

	  if (direct_internal_fn_supported_p (IFN_LEN_FOLD_EXTRACT_LAST,
					      vectype, OPTIMIZE_FOR_SPEED))
	    {
	      if (lens)
		{
		  len = vect_get_loop_len (loop_vinfo, gsi, lens,
					   vec_num * ncopies, vectype, i, 1);
		  signed char biasval
		    = LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS (loop_vinfo);
		  bias = build_int_cst (intQI_type_node, biasval);
		}
	      else
		{
		  len = size_int (TYPE_VECTOR_SUBPARTS (vectype));
		  bias = build_int_cst (intQI_type_node, 0);
		}
	    }
	  if (masks)
	    {
	      tree loop_mask
		= vect_get_loop_mask (loop_vinfo, gsi, masks, vec_num * ncopies,
				      vectype, i);
	      tree tmp2 = make_ssa_name (vec_cmp_type);
	      gassign *g
		= gimple_build_assign (tmp2, BIT_AND_EXPR, vec_compare,
				       loop_mask);
	      vect_finish_stmt_generation (vinfo, stmt_info, g, gsi);
	      vec_compare = tmp2;
	    }
	}

      gimple *new_stmt;
      if (reduction_type == EXTRACT_LAST_REDUCTION)
	{
	  gimple *old_stmt = vect_orig_stmt (stmt_info)->stmt;
	  tree lhs = gimple_get_lhs (old_stmt);
	  if ((unsigned)i != vec_oprnds0.length () - 1)
	    lhs = copy_ssa_name (lhs);
	  if (len)
	    new_stmt = gimple_build_call_internal
		(IFN_LEN_FOLD_EXTRACT_LAST, 5, vec_else_clause, vec_compare,
		 vec_then_clause, len, bias);
	  else
	    new_stmt = gimple_build_call_internal
		(IFN_FOLD_EXTRACT_LAST, 3, vec_else_clause, vec_compare,
		 vec_then_clause);
	  gimple_call_set_lhs (new_stmt, lhs);
	  SSA_NAME_DEF_STMT (lhs) = new_stmt;
	  if ((unsigned)i != vec_oprnds0.length () - 1)
	    {
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	      vec_else_clause = lhs;
	    }
	  else if (old_stmt == gsi_stmt (*gsi))
	    vect_finish_replace_stmt (vinfo, stmt_info, new_stmt);
	  else
	    {
	      /* In this case we're moving the definition to later in the
		 block.  That doesn't matter because the only uses of the
		 lhs are in phi statements.  */
	      gimple_stmt_iterator old_gsi = gsi_for_stmt (old_stmt);
	      gsi_remove (&old_gsi, true);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }
	}
      else
	{
	  new_temp = make_ssa_name (vec_dest);
	  new_stmt = gimple_build_assign (new_temp, VEC_COND_EXPR, vec_compare,
					  vec_then_clause, vec_else_clause);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	}
      if (slp_node)
	slp_node->push_vec_def (new_stmt);
      else
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
    }

  if (!slp_node)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  vec_oprnds2.release ();
  vec_oprnds3.release ();

  return true;
}

/* Helper of vectorizable_comparison.

   Check if STMT_INFO is comparison expression CODE that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   comparison, put it in VEC_STMT, and insert it at GSI.

   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_comparison_1 (vec_info *vinfo, tree vectype,
			   stmt_vec_info stmt_info, tree_code code,
			   gimple_stmt_iterator *gsi, gimple **vec_stmt,
			   slp_tree slp_node, stmt_vector_for_cost *cost_vec)
{
  tree lhs, rhs1, rhs2;
  tree vectype1 = NULL_TREE, vectype2 = NULL_TREE;
  tree vec_rhs1 = NULL_TREE, vec_rhs2 = NULL_TREE;
  tree new_temp;
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  enum vect_def_type dts[2] = {vect_unknown_def_type, vect_unknown_def_type};
  int ndts = 2;
  poly_uint64 nunits;
  int ncopies;
  enum tree_code bitop1 = NOP_EXPR, bitop2 = NOP_EXPR;
  int i;
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  tree mask_type;
  tree mask = NULL_TREE;

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

  if (TREE_CODE_CLASS (code) != tcc_comparison)
    return false;

  slp_tree slp_rhs1, slp_rhs2;
  if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			   0, &rhs1, &slp_rhs1, &dts[0], &vectype1))
    return false;

  if (!vect_is_simple_use (vinfo, stmt_info, slp_node,
			   1, &rhs2, &slp_rhs2, &dts[1], &vectype2))
    return false;

  if (vectype1 && vectype2
      && maybe_ne (TYPE_VECTOR_SUBPARTS (vectype1),
		   TYPE_VECTOR_SUBPARTS (vectype2)))
    return false;

  vectype = vectype1 ? vectype1 : vectype2;

  /* Invariant comparison.  */
  if (!vectype)
    {
      vectype = get_vectype_for_scalar_type (vinfo, TREE_TYPE (rhs1), slp_node);
      if (!vectype || maybe_ne (TYPE_VECTOR_SUBPARTS (vectype), nunits))
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
  bool swap_p = false;
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
	  swap_p = true;
	}
      else if (code == LE_EXPR)
	{
	  bitop1 = BIT_NOT_EXPR;
	  bitop2 = BIT_IOR_EXPR;
	  swap_p = true;
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

      /* Put types on constant and invariant SLP children.  */
      if (slp_node
	  && (!vect_maybe_update_slp_op_vectype (slp_rhs1, vectype)
	      || !vect_maybe_update_slp_op_vectype (slp_rhs2, vectype)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "incompatible vector types for invariants\n");
	  return false;
	}

      vect_model_simple_cost (vinfo, stmt_info,
			      ncopies * (1 + (bitop2 != NOP_EXPR)),
			      dts, ndts, slp_node, cost_vec);
      return true;
    }

  /* Transform.  */

  /* Handle def.  */
  lhs = gimple_get_lhs (STMT_VINFO_STMT (stmt_info));
  if (lhs)
    mask = vect_create_destination_var (lhs, mask_type);

  vect_get_vec_defs (vinfo, stmt_info, slp_node, ncopies,
		     rhs1, vectype, &vec_oprnds0,
		     rhs2, vectype, &vec_oprnds1);
  if (swap_p)
    std::swap (vec_oprnds0, vec_oprnds1);

  /* Arguments are ready.  Create the new vector stmt.  */
  FOR_EACH_VEC_ELT (vec_oprnds0, i, vec_rhs1)
    {
      gimple *new_stmt;
      vec_rhs2 = vec_oprnds1[i];

      if (lhs)
	new_temp = make_ssa_name (mask);
      else
	new_temp = make_temp_ssa_name (mask_type, NULL, "cmp");
      if (bitop1 == NOP_EXPR)
	{
	  new_stmt = gimple_build_assign (new_temp, code,
					  vec_rhs1, vec_rhs2);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	}
      else
	{
	  if (bitop1 == BIT_NOT_EXPR)
	    new_stmt = gimple_build_assign (new_temp, bitop1, vec_rhs2);
	  else
	    new_stmt = gimple_build_assign (new_temp, bitop1, vec_rhs1,
					    vec_rhs2);
	  vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	  if (bitop2 != NOP_EXPR)
	    {
	      tree res = make_ssa_name (mask);
	      if (bitop2 == BIT_NOT_EXPR)
		new_stmt = gimple_build_assign (res, bitop2, new_temp);
	      else
		new_stmt = gimple_build_assign (res, bitop2, vec_rhs1,
						new_temp);
	      vect_finish_stmt_generation (vinfo, stmt_info, new_stmt, gsi);
	    }
	}
      if (slp_node)
	slp_node->push_vec_def (new_stmt);
      else
	STMT_VINFO_VEC_STMTS (stmt_info).safe_push (new_stmt);
    }

  if (!slp_node)
    *vec_stmt = STMT_VINFO_VEC_STMTS (stmt_info)[0];

  vec_oprnds0.release ();
  vec_oprnds1.release ();

  return true;
}

/* vectorizable_comparison.

   Check if STMT_INFO is comparison expression that can be vectorized.
   If VEC_STMT is also passed, vectorize STMT_INFO: create a vectorized
   comparison, put it in VEC_STMT, and insert it at GSI.

   Return true if STMT_INFO is vectorizable in this way.  */

static bool
vectorizable_comparison (vec_info *vinfo,
			 stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
			 gimple **vec_stmt,
			 slp_tree slp_node, stmt_vector_for_cost *cost_vec)
{
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!stmt)
    return false;

  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (!vectorizable_comparison_1 (vinfo, vectype, stmt_info, code, gsi,
				  vec_stmt, slp_node, cost_vec))
    return false;

  if (!vec_stmt)
    STMT_VINFO_TYPE (stmt_info) = comparison_vec_info_type;

  return true;
}

/* Check to see if the current early break given in STMT_INFO is valid for
   vectorization.  */

bool
vectorizable_early_exit (vec_info *vinfo, stmt_vec_info stmt_info,
			 gimple_stmt_iterator *gsi, gimple **vec_stmt,
			 slp_tree slp_node, stmt_vector_for_cost *cost_vec)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  if (!loop_vinfo
      || !is_a <gcond *> (STMT_VINFO_STMT (stmt_info)))
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_condition_def)
    return false;

  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  DUMP_VECT_SCOPE ("vectorizable_early_exit");

  auto code = gimple_cond_code (STMT_VINFO_STMT (stmt_info));

  tree vectype = NULL_TREE;
  slp_tree slp_op0;
  tree op0;
  enum vect_def_type dt0;

  /* Early break gcond kind SLP trees can be root only and have no children,
     for instance in the case where the argument is an external.  If that's
     the case there is no operand to analyse use of.  */
  if ((!slp_node || !SLP_TREE_CHILDREN (slp_node).is_empty ())
      && !vect_is_simple_use (vinfo, stmt_info, slp_node, 0, &op0, &slp_op0, &dt0,
			      &vectype))
    {
      if (dump_enabled_p ())
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "use not simple.\n");
	return false;
    }

  /* For SLP we don't want to use the type of the operands of the SLP node, when
     vectorizing using SLP slp_node will be the children of the gcond and we
     want to use the type of the direct children which since the gcond is root
     will be the current node, rather than a child node as vect_is_simple_use
     assumes.  */
  if (slp_node)
    vectype = SLP_TREE_VECTYPE (slp_node);

  if (!vectype)
    return false;

  machine_mode mode = TYPE_MODE (vectype);
  int ncopies, vec_num;

  if (slp_node)
    {
      ncopies = 1;
      vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
    }
  else
    {
      ncopies = vect_get_num_copies (loop_vinfo, vectype);
      vec_num = 1;
    }

  vec_loop_masks *masks = &LOOP_VINFO_MASKS (loop_vinfo);
  vec_loop_lens *lens = &LOOP_VINFO_LENS (loop_vinfo);
  bool masked_loop_p = LOOP_VINFO_FULLY_MASKED_P (loop_vinfo);
  bool len_loop_p = LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo);

  /* Now build the new conditional.  Pattern gimple_conds get dropped during
     codegen so we must replace the original insn.  */
  gimple *orig_stmt = STMT_VINFO_STMT (vect_orig_stmt (stmt_info));
  gcond *cond_stmt = as_a <gcond *>(orig_stmt);
  /* When vectorizing we assume that if the branch edge is taken that we're
     exiting the loop.  This is not however always the case as the compiler will
     rewrite conditions to always be a comparison against 0.  To do this it
     sometimes flips the edges.  This is fine for scalar,  but for vector we
     then have to flip the test, as we're still assuming that if you take the
     branch edge that we found the exit condition.  i.e. we need to know whether
     we are generating a `forall` or an `exist` condition.  */
  auto new_code = NE_EXPR;
  auto reduc_optab = ior_optab;
  auto reduc_op = BIT_IOR_EXPR;
  tree cst = build_zero_cst (vectype);
  edge exit_true_edge = EDGE_SUCC (gimple_bb (cond_stmt), 0);
  if (exit_true_edge->flags & EDGE_FALSE_VALUE)
    exit_true_edge = EDGE_SUCC (gimple_bb (cond_stmt), 1);
  gcc_assert (exit_true_edge->flags & EDGE_TRUE_VALUE);
  if (flow_bb_inside_loop_p (LOOP_VINFO_LOOP (loop_vinfo),
			     exit_true_edge->dest))
    {
      new_code = EQ_EXPR;
      reduc_optab = and_optab;
      reduc_op = BIT_AND_EXPR;
      cst = build_minus_one_cst (vectype);
    }

  /* Analyze only.  */
  if (!vec_stmt)
    {
      if (direct_optab_handler (cbranch_optab, mode) == CODE_FOR_nothing)
	{
	  if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "can't vectorize early exit because the "
			       "target doesn't support flag setting vector "
			       "comparisons.\n");
	  return false;
	}

      if (ncopies > 1
	  && direct_optab_handler (reduc_optab, mode) == CODE_FOR_nothing)
	{
	  if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "can't vectorize early exit because the "
			       "target does not support boolean vector %s "
			       "for type %T.\n",
			       reduc_optab == ior_optab ? "OR" : "AND",
			       vectype);
	  return false;
	}

      if (!vectorizable_comparison_1 (vinfo, vectype, stmt_info, code, gsi,
				      vec_stmt, slp_node, cost_vec))
	return false;

      if (LOOP_VINFO_CAN_USE_PARTIAL_VECTORS_P (loop_vinfo))
	{
	  if (direct_internal_fn_supported_p (IFN_VCOND_MASK_LEN, vectype,
					      OPTIMIZE_FOR_SPEED))
	    vect_record_loop_len (loop_vinfo, lens, ncopies * vec_num,
				  vectype, 1);
	  else
	    vect_record_loop_mask (loop_vinfo, masks, ncopies * vec_num,
				   vectype, NULL);
	}

      return true;
    }

  /* Tranform.  */

  tree new_temp = NULL_TREE;
  gimple *new_stmt = NULL;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform early-exit.\n");

  /* For SLP we don't do codegen of the body starting from the gcond, the gconds are
     roots and so by the time we get to them we have already codegened the SLP tree
     and so we shouldn't try to do so again.  The arguments have already been
     vectorized.  It's not very clean to do this here, But the masking code below is
     complex and this keeps it all in one place to ease fixes and backports.  Once we
     drop the non-SLP loop vect or split vectorizable_* this can be simplified.  */
  if (!slp_node)
    {
      if (!vectorizable_comparison_1 (vinfo, vectype, stmt_info, code, gsi,
				      vec_stmt, slp_node, cost_vec))
	gcc_unreachable ();
    }

  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  basic_block cond_bb = gimple_bb (stmt);
  gimple_stmt_iterator  cond_gsi = gsi_last_bb (cond_bb);

  auto_vec<tree> stmts;

  if (slp_node)
    stmts.safe_splice (SLP_TREE_VEC_DEFS (slp_node));
  else
    {
      auto vec_stmts = STMT_VINFO_VEC_STMTS (stmt_info);
      stmts.reserve_exact (vec_stmts.length ());
      for (auto stmt : vec_stmts)
	stmts.quick_push (gimple_assign_lhs (stmt));
    }

  /* Determine if we need to reduce the final value.  */
  if (stmts.length () > 1)
    {
      /* We build the reductions in a way to maintain as much parallelism as
	 possible.  */
      auto_vec<tree> workset (stmts.length ());

      /* Mask the statements as we queue them up.  Normally we loop over
	 vec_num,  but since we inspect the exact results of vectorization
	 we don't need to and instead can just use the stmts themselves.  */
      if (masked_loop_p)
	for (unsigned i = 0; i < stmts.length (); i++)
	  {
	    tree stmt_mask
	      = vect_get_loop_mask (loop_vinfo, gsi, masks, ncopies * vec_num,
				    vectype, i);
	    stmt_mask
	      = prepare_vec_mask (loop_vinfo, TREE_TYPE (stmt_mask), stmt_mask,
				  stmts[i], &cond_gsi);
	    workset.quick_push (stmt_mask);
	  }
      else if (len_loop_p)
	for (unsigned i = 0; i < stmts.length (); i++)
	  {
	    tree len_mask = vect_gen_loop_len_mask (loop_vinfo, gsi, &cond_gsi,
						    lens, ncopies * vec_num,
						    vectype, stmts[i], i, 1);

	    workset.quick_push (len_mask);
	  }
      else
	workset.splice (stmts);

      while (workset.length () > 1)
	{
	  new_temp = make_temp_ssa_name (vectype, NULL, "vexit_reduc");
	  tree arg0 = workset.pop ();
	  tree arg1 = workset.pop ();
	  new_stmt = gimple_build_assign (new_temp, reduc_op, arg0, arg1);
	  vect_finish_stmt_generation (loop_vinfo, stmt_info, new_stmt,
				       &cond_gsi);
	  workset.quick_insert (0, new_temp);
	}
    }
  else
    {
      new_temp = stmts[0];
      if (masked_loop_p)
	{
	  tree mask
	    = vect_get_loop_mask (loop_vinfo, gsi, masks, ncopies, vectype, 0);
	  new_temp = prepare_vec_mask (loop_vinfo, TREE_TYPE (mask), mask,
				       new_temp, &cond_gsi);
	}
      else if (len_loop_p)
	new_temp = vect_gen_loop_len_mask (loop_vinfo, gsi, &cond_gsi, lens,
					   ncopies, vectype, new_temp, 0, 1);
    }

  gcc_assert (new_temp);

  gimple_cond_set_condition (cond_stmt, new_code, new_temp, cst);
  update_stmt (orig_stmt);

  if (slp_node)
    SLP_TREE_VEC_DEFS (slp_node).truncate (0);
   else
    STMT_VINFO_VEC_STMTS (stmt_info).truncate (0);

  if (!slp_node)
    *vec_stmt = orig_stmt;

  return true;
}

/* If SLP_NODE is nonnull, return true if vectorizable_live_operation
   can handle all live statements in the node.  Otherwise return true
   if STMT_INFO is not live or if vectorizable_live_operation can handle it.
   VEC_STMT_P is as for vectorizable_live_operation.  */

static bool
can_vectorize_live_stmts (vec_info *vinfo, stmt_vec_info stmt_info,
			  slp_tree slp_node, slp_instance slp_node_instance,
			  bool vec_stmt_p,
			  stmt_vector_for_cost *cost_vec)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  if (slp_node)
    {
      stmt_vec_info slp_stmt_info;
      unsigned int i;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (slp_node), i, slp_stmt_info)
	{
	  if (slp_stmt_info
	      && (STMT_VINFO_LIVE_P (slp_stmt_info)
		  || (loop_vinfo
		      && LOOP_VINFO_EARLY_BREAKS (loop_vinfo)
		      && STMT_VINFO_DEF_TYPE (slp_stmt_info)
		      == vect_induction_def))
	      && !vectorizable_live_operation (vinfo, slp_stmt_info, slp_node,
					       slp_node_instance, i,
					       vec_stmt_p, cost_vec))
	    return false;
	}
    }
  else if ((STMT_VINFO_LIVE_P (stmt_info)
	    || (LOOP_VINFO_EARLY_BREAKS (loop_vinfo)
		&& STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def))
	   && !vectorizable_live_operation (vinfo, stmt_info,
					    slp_node, slp_node_instance, -1,
					    vec_stmt_p, cost_vec))
    return false;

  return true;
}

/* Make sure the statement is vectorizable.  */

opt_result
vect_analyze_stmt (vec_info *vinfo,
		   stmt_vec_info stmt_info, bool *need_to_vectorize,
		   slp_tree node, slp_instance node_instance,
		   stmt_vector_for_cost *cost_vec)
{
  bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo);
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
		= vect_analyze_stmt (vinfo, pattern_def_stmt_info,
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

	  if (node)
	    return opt_result::failure_at (stmt_info->stmt,
					   "not vectorized:"
					   " irrelevant stmt as SLP node %p "
					   "representative.\n",
					   (void *)node);
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
	= vect_analyze_stmt (vinfo, pattern_stmt_info, need_to_vectorize, node,
			     node_instance, cost_vec);
      if (!res)
	return res;
    }

  switch (STMT_VINFO_DEF_TYPE (stmt_info))
    {
      case vect_internal_def:
      case vect_condition_def:
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

      case vect_double_reduction_def:
	gcc_assert (!bb_vinfo && node);
	break;

      case vect_induction_def:
      case vect_first_order_recurrence:
	gcc_assert (!bb_vinfo);
	break;

      case vect_constant_def:
      case vect_external_def:
      case vect_unknown_def_type:
      default:
        gcc_unreachable ();
    }

  tree saved_vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (node)
    STMT_VINFO_VECTYPE (stmt_info) = SLP_TREE_VECTYPE (node);

  if (STMT_VINFO_RELEVANT_P (stmt_info))
    {
      gcall *call = dyn_cast <gcall *> (stmt_info->stmt);
      gcc_assert (STMT_VINFO_VECTYPE (stmt_info)
		  || gimple_code (stmt_info->stmt) == GIMPLE_COND
		  || (call && gimple_call_lhs (call) == NULL_TREE));
      *need_to_vectorize = true;
    }

  if (PURE_SLP_STMT (stmt_info) && !node)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "handled only by SLP analysis\n");
      return opt_result::success ();
    }

  /* When we arrive here with a non-SLP statement and we are supposed
     to use SLP for everything fail vectorization.  */
  if (!node && param_vect_force_slp)
    return opt_result::failure_at (stmt_info->stmt,
				   "needs non-SLP handling\n");

  ok = true;
  if (!bb_vinfo
      && (STMT_VINFO_RELEVANT_P (stmt_info)
	  || STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def))
    /* Prefer vectorizable_call over vectorizable_simd_clone_call so
       -mveclibabi= takes preference over library functions with
       the simd attribute.  */
    ok = (vectorizable_call (vinfo, stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_simd_clone_call (vinfo, stmt_info, NULL, NULL, node,
					   cost_vec)
	  || vectorizable_conversion (vinfo, stmt_info,
				      NULL, NULL, node, cost_vec)
	  || vectorizable_operation (vinfo, stmt_info,
				     NULL, NULL, node, cost_vec)
	  || vectorizable_assignment (vinfo, stmt_info,
				      NULL, NULL, node, cost_vec)
	  || vectorizable_load (vinfo, stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_store (vinfo, stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_lane_reducing (as_a <loop_vec_info> (vinfo),
					 stmt_info, node, cost_vec)
	  || vectorizable_reduction (as_a <loop_vec_info> (vinfo), stmt_info,
				     node, node_instance, cost_vec)
	  || vectorizable_induction (as_a <loop_vec_info> (vinfo), stmt_info,
				     NULL, node, cost_vec)
	  || vectorizable_shift (vinfo, stmt_info, NULL, NULL, node, cost_vec)
	  || vectorizable_condition (vinfo, stmt_info,
				     NULL, NULL, node, cost_vec)
	  || vectorizable_comparison (vinfo, stmt_info, NULL, NULL, node,
				      cost_vec)
	  || vectorizable_lc_phi (as_a <loop_vec_info> (vinfo),
				  stmt_info, NULL, node)
	  || vectorizable_recurr (as_a <loop_vec_info> (vinfo),
				   stmt_info, NULL, node, cost_vec)
	  || vectorizable_early_exit (vinfo, stmt_info, NULL, NULL, node,
				      cost_vec));
  else
    {
      if (bb_vinfo)
	ok = (vectorizable_call (vinfo, stmt_info, NULL, NULL, node, cost_vec)
	      || vectorizable_simd_clone_call (vinfo, stmt_info,
					       NULL, NULL, node, cost_vec)
	      || vectorizable_conversion (vinfo, stmt_info, NULL, NULL, node,
					  cost_vec)
	      || vectorizable_shift (vinfo, stmt_info,
				     NULL, NULL, node, cost_vec)
	      || vectorizable_operation (vinfo, stmt_info,
					 NULL, NULL, node, cost_vec)
	      || vectorizable_assignment (vinfo, stmt_info, NULL, NULL, node,
					  cost_vec)
	      || vectorizable_load (vinfo, stmt_info,
				    NULL, NULL, node, cost_vec)
	      || vectorizable_store (vinfo, stmt_info,
				     NULL, NULL, node, cost_vec)
	      || vectorizable_condition (vinfo, stmt_info,
					 NULL, NULL, node, cost_vec)
	      || vectorizable_comparison (vinfo, stmt_info, NULL, NULL, node,
					  cost_vec)
	      || vectorizable_phi (vinfo, stmt_info, NULL, node, cost_vec)
	      || vectorizable_early_exit (vinfo, stmt_info, NULL, NULL, node,
					  cost_vec));

    }

  if (node)
    STMT_VINFO_VECTYPE (stmt_info) = saved_vectype;

  if (!ok)
    return opt_result::failure_at (stmt_info->stmt,
				   "not vectorized:"
				   " relevant stmt not supported: %G",
				   stmt_info->stmt);

  /* Stmts that are (also) "live" (i.e. - that are used out of the loop)
      need extra handling, except for vectorizable reductions.  */
  if (!bb_vinfo
      && STMT_VINFO_TYPE (stmt_info) != reduc_vec_info_type
      && STMT_VINFO_TYPE (stmt_info) != lc_phi_info_type
      && !can_vectorize_live_stmts (as_a <loop_vec_info> (vinfo),
				    stmt_info, node, node_instance,
				    false, cost_vec))
    return opt_result::failure_at (stmt_info->stmt,
				   "not vectorized:"
				   " live stmt not supported: %G",
				   stmt_info->stmt);

  return opt_result::success ();
}


/* Function vect_transform_stmt.

   Create a vectorized stmt to replace STMT_INFO, and insert it at GSI.  */

bool
vect_transform_stmt (vec_info *vinfo,
		     stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
		     slp_tree slp_node, slp_instance slp_node_instance)
{
  bool is_store = false;
  gimple *vec_stmt = NULL;
  bool done;

  gcc_assert (slp_node || !PURE_SLP_STMT (stmt_info));

  tree saved_vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (slp_node)
    STMT_VINFO_VECTYPE (stmt_info) = SLP_TREE_VECTYPE (slp_node);

  switch (STMT_VINFO_TYPE (stmt_info))
    {
    case type_demotion_vec_info_type:
    case type_promotion_vec_info_type:
    case type_conversion_vec_info_type:
      done = vectorizable_conversion (vinfo, stmt_info,
				      gsi, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      break;

    case induc_vec_info_type:
      done = vectorizable_induction (as_a <loop_vec_info> (vinfo),
				     stmt_info, &vec_stmt, slp_node,
				     NULL);
      gcc_assert (done);
      break;

    case shift_vec_info_type:
      done = vectorizable_shift (vinfo, stmt_info,
				 gsi, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      break;

    case op_vec_info_type:
      done = vectorizable_operation (vinfo, stmt_info, gsi, &vec_stmt, slp_node,
				     NULL);
      gcc_assert (done);
      break;

    case assignment_vec_info_type:
      done = vectorizable_assignment (vinfo, stmt_info,
				      gsi, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      break;

    case load_vec_info_type:
      done = vectorizable_load (vinfo, stmt_info, gsi, &vec_stmt, slp_node,
				NULL);
      gcc_assert (done);
      break;

    case store_vec_info_type:
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
	  && !slp_node
	  && (++DR_GROUP_STORE_COUNT (DR_GROUP_FIRST_ELEMENT (stmt_info))
	      < DR_GROUP_SIZE (DR_GROUP_FIRST_ELEMENT (stmt_info))))
	/* In case of interleaving, the whole chain is vectorized when the
	   last store in the chain is reached.  Store stmts before the last
	   one are skipped, and there vec_stmt_info shouldn't be freed
	   meanwhile.  */
	;
      else
	{
	  done = vectorizable_store (vinfo, stmt_info,
				     gsi, &vec_stmt, slp_node, NULL);
	  gcc_assert (done);
	  is_store = true;
	}
      break;

    case condition_vec_info_type:
      done = vectorizable_condition (vinfo, stmt_info,
				     gsi, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      break;

    case comparison_vec_info_type:
      done = vectorizable_comparison (vinfo, stmt_info, gsi, &vec_stmt,
				      slp_node, NULL);
      gcc_assert (done);
      break;

    case call_vec_info_type:
      done = vectorizable_call (vinfo, stmt_info,
				gsi, &vec_stmt, slp_node, NULL);
      break;

    case call_simd_clone_vec_info_type:
      done = vectorizable_simd_clone_call (vinfo, stmt_info, gsi, &vec_stmt,
					   slp_node, NULL);
      break;

    case reduc_vec_info_type:
      done = vect_transform_reduction (as_a <loop_vec_info> (vinfo), stmt_info,
				       gsi, &vec_stmt, slp_node);
      gcc_assert (done);
      break;

    case cycle_phi_info_type:
      done = vect_transform_cycle_phi (as_a <loop_vec_info> (vinfo), stmt_info,
				       &vec_stmt, slp_node, slp_node_instance);
      gcc_assert (done);
      break;

    case lc_phi_info_type:
      done = vectorizable_lc_phi (as_a <loop_vec_info> (vinfo),
				  stmt_info, &vec_stmt, slp_node);
      gcc_assert (done);
      break;

    case recurr_info_type:
      done = vectorizable_recurr (as_a <loop_vec_info> (vinfo),
				  stmt_info, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      break;

    case phi_info_type:
      done = vectorizable_phi (vinfo, stmt_info, &vec_stmt, slp_node, NULL);
      gcc_assert (done);
      break;

    case loop_exit_ctrl_vec_info_type:
      done = vectorizable_early_exit (vinfo, stmt_info, gsi, &vec_stmt,
				      slp_node, NULL);
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
      done = true;
    }

  if (!slp_node && vec_stmt)
    gcc_assert (STMT_VINFO_VEC_STMTS (stmt_info).exists ());

  if (STMT_VINFO_TYPE (stmt_info) != store_vec_info_type)
    {
      /* Handle stmts whose DEF is used outside the loop-nest that is
	 being vectorized.  */
      done = can_vectorize_live_stmts (vinfo, stmt_info, slp_node,
				       slp_node_instance, true, NULL);
      gcc_assert (done);
    }

  if (slp_node)
    STMT_VINFO_VECTYPE (stmt_info) = saved_vectype;

  return is_store;
}


/* Remove a group of stores (for SLP or interleaving), free their
   stmt_vec_info.  */

void
vect_remove_stores (vec_info *vinfo, stmt_vec_info first_stmt_info)
{
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

/* If NUNITS is nonzero, return a vector type that contains NUNITS
   elements of type SCALAR_TYPE, or null if the target doesn't support
   such a type.

   If NUNITS is zero, return a vector type that contains elements of
   type SCALAR_TYPE, choosing whichever vector size the target prefers.

   If PREVAILING_MODE is VOIDmode, we have not yet chosen a vector mode
   for this vectorization region and want to "autodetect" the best choice.
   Otherwise, PREVAILING_MODE is a previously-chosen vector TYPE_MODE
   and we want the new type to be interoperable with it.   PREVAILING_MODE
   in this case can be a scalar integer mode or a vector mode; when it
   is a vector mode, the function acts like a tree-level version of
   related_vector_mode.  */

tree
get_related_vectype_for_scalar_type (machine_mode prevailing_mode,
				     tree scalar_type, poly_uint64 nunits)
{
  tree orig_scalar_type = scalar_type;
  scalar_mode inner_mode;
  machine_mode simd_mode;
  tree vectype;

  if ((!INTEGRAL_TYPE_P (scalar_type)
       && !POINTER_TYPE_P (scalar_type)
       && !SCALAR_FLOAT_TYPE_P (scalar_type))
      || (!is_int_mode (TYPE_MODE (scalar_type), &inner_mode)
	  && !is_float_mode (TYPE_MODE (scalar_type), &inner_mode)))
    return NULL_TREE;

  unsigned int nbytes = GET_MODE_SIZE (inner_mode);

  /* Interoperability between modes requires one to be a constant multiple
     of the other, so that the number of vectors required for each operation
     is a compile-time constant.  */
  if (prevailing_mode != VOIDmode
      && !constant_multiple_p (nunits * nbytes,
			       GET_MODE_SIZE (prevailing_mode))
      && !constant_multiple_p (GET_MODE_SIZE (prevailing_mode),
			       nunits * nbytes))
    return NULL_TREE;

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

  /* If no prevailing mode was supplied, use the mode the target prefers.
     Otherwise lookup a vector mode based on the prevailing mode.  */
  if (prevailing_mode == VOIDmode)
    {
      gcc_assert (known_eq (nunits, 0U));
      simd_mode = targetm.vectorize.preferred_simd_mode (inner_mode);
      if (SCALAR_INT_MODE_P (simd_mode))
	{
	  /* Traditional behavior is not to take the integer mode
	     literally, but simply to use it as a way of determining
	     the vector size.  It is up to mode_for_vector to decide
	     what the TYPE_MODE should be.

	     Note that nunits == 1 is allowed in order to support single
	     element vector types.  */
	  if (!multiple_p (GET_MODE_SIZE (simd_mode), nbytes, &nunits)
	      || !mode_for_vector (inner_mode, nunits).exists (&simd_mode))
	    return NULL_TREE;
	}
    }
  else if (SCALAR_INT_MODE_P (prevailing_mode)
	   || !related_vector_mode (prevailing_mode,
				    inner_mode, nunits).exists (&simd_mode))
    {
      /* Fall back to using mode_for_vector, mostly in the hope of being
	 able to use an integer mode.  */
      if (known_eq (nunits, 0U)
	  && !multiple_p (GET_MODE_SIZE (prevailing_mode), nbytes, &nunits))
	return NULL_TREE;

      if (!mode_for_vector (inner_mode, nunits).exists (&simd_mode))
	return NULL_TREE;
    }

  vectype = build_vector_type_for_mode (scalar_type, simd_mode);

  /* In cases where the mode was chosen by mode_for_vector, check that
     the target actually supports the chosen mode, or that it at least
     allows the vector mode to be replaced by a like-sized integer.  */
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

/* Function get_vectype_for_scalar_type.

   Returns the vector type corresponding to SCALAR_TYPE as supported
   by the target.  If GROUP_SIZE is nonzero and we're performing BB
   vectorization, make sure that the number of elements in the vector
   is no bigger than GROUP_SIZE.  */

tree
get_vectype_for_scalar_type (vec_info *vinfo, tree scalar_type,
			     unsigned int group_size)
{
  /* For BB vectorization, we should always have a group size once we've
     constructed the SLP tree; the only valid uses of zero GROUP_SIZEs
     are tentative requests during things like early data reference
     analysis and pattern recognition.  */
  if (is_a <bb_vec_info> (vinfo))
    gcc_assert (vinfo->slp_instances.is_empty () || group_size != 0);
  else
    group_size = 0;

  tree vectype = get_related_vectype_for_scalar_type (vinfo->vector_mode,
						      scalar_type);
  if (vectype && vinfo->vector_mode == VOIDmode)
    vinfo->vector_mode = TYPE_MODE (vectype);

  /* Register the natural choice of vector type, before the group size
     has been applied.  */
  if (vectype)
    vinfo->used_vector_modes.add (TYPE_MODE (vectype));

  /* If the natural choice of vector type doesn't satisfy GROUP_SIZE,
     try again with an explicit number of elements.  */
  if (vectype
      && group_size
      && maybe_ge (TYPE_VECTOR_SUBPARTS (vectype), group_size))
    {
      /* Start with the biggest number of units that fits within
	 GROUP_SIZE and halve it until we find a valid vector type.
	 Usually either the first attempt will succeed or all will
	 fail (in the latter case because GROUP_SIZE is too small
	 for the target), but it's possible that a target could have
	 a hole between supported vector types.

	 If GROUP_SIZE is not a power of 2, this has the effect of
	 trying the largest power of 2 that fits within the group,
	 even though the group is not a multiple of that vector size.
	 The BB vectorizer will then try to carve up the group into
	 smaller pieces.  */
      unsigned int nunits = 1 << floor_log2 (group_size);
      do
	{
	  vectype = get_related_vectype_for_scalar_type (vinfo->vector_mode,
							 scalar_type, nunits);
	  nunits /= 2;
	}
      while (nunits > 1 && !vectype);
    }

  return vectype;
}

/* Return the vector type corresponding to SCALAR_TYPE as supported
   by the target.  NODE, if nonnull, is the SLP tree node that will
   use the returned vector type.  */

tree
get_vectype_for_scalar_type (vec_info *vinfo, tree scalar_type, slp_tree node)
{
  unsigned int group_size = 0;
  if (node)
    group_size = SLP_TREE_LANES (node);
  return get_vectype_for_scalar_type (vinfo, scalar_type, group_size);
}

/* Function get_mask_type_for_scalar_type.

   Returns the mask type corresponding to a result of comparison
   of vectors of specified SCALAR_TYPE as supported by target.
   If GROUP_SIZE is nonzero and we're performing BB vectorization,
   make sure that the number of elements in the vector is no bigger
   than GROUP_SIZE.  */

tree
get_mask_type_for_scalar_type (vec_info *vinfo, tree scalar_type,
			       unsigned int group_size)
{
  tree vectype = get_vectype_for_scalar_type (vinfo, scalar_type, group_size);

  if (!vectype)
    return NULL;

  return truth_type_for (vectype);
}

/* Function get_mask_type_for_scalar_type.

   Returns the mask type corresponding to a result of comparison
   of vectors of specified SCALAR_TYPE as supported by target.
   NODE, if nonnull, is the SLP tree node that will use the returned
   vector type.  */

tree
get_mask_type_for_scalar_type (vec_info *vinfo, tree scalar_type,
			       slp_tree node)
{
  tree vectype = get_vectype_for_scalar_type (vinfo, scalar_type, node);

  if (!vectype)
    return NULL;

  return truth_type_for (vectype);
}

/* Function get_same_sized_vectype

   Returns a vector type corresponding to SCALAR_TYPE of size
   VECTOR_TYPE if supported by the target.  */

tree
get_same_sized_vectype (tree scalar_type, tree vector_type)
{
  if (VECT_SCALAR_BOOLEAN_TYPE_P (scalar_type))
    return truth_type_for (vector_type);

  poly_uint64 nunits;
  if (!multiple_p (GET_MODE_SIZE (TYPE_MODE (vector_type)),
		   GET_MODE_SIZE (TYPE_MODE (scalar_type)), &nunits))
    return NULL_TREE;

  return get_related_vectype_for_scalar_type (TYPE_MODE (vector_type),
					      scalar_type, nunits);
}

/* Return true if replacing LOOP_VINFO->vector_mode with VECTOR_MODE
   would not change the chosen vector modes.  */

bool
vect_chooses_same_modes_p (vec_info *vinfo, machine_mode vector_mode)
{
  for (vec_info::mode_set::iterator i = vinfo->used_vector_modes.begin ();
       i != vinfo->used_vector_modes.end (); ++i)
    if (!VECTOR_MODE_P (*i)
	|| related_vector_mode (vector_mode, GET_MODE_INNER (*i), 0) != *i)
      return false;
  return true;
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
	  *dt = STMT_VINFO_DEF_TYPE (stmt_vinfo);
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
	case vect_first_order_recurrence:
	  dump_printf (MSG_NOTE, "first order recurrence\n");
	  break;
	case vect_condition_def:
	  dump_printf (MSG_NOTE, "control flow\n");
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
      || *dt == vect_nested_cycle
      || *dt == vect_first_order_recurrence)
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

/* Function vect_is_simple_use.

   Same as vect_is_simple_use but determines the operand by operand
   position OPERAND from either STMT or SLP_NODE, filling in *OP
   and *SLP_DEF (when SLP_NODE is not NULL).  */

bool
vect_is_simple_use (vec_info *vinfo, stmt_vec_info stmt, slp_tree slp_node,
		    unsigned operand, tree *op, slp_tree *slp_def,
		    enum vect_def_type *dt,
		    tree *vectype, stmt_vec_info *def_stmt_info_out)
{
  if (slp_node)
    {
      slp_tree child = SLP_TREE_CHILDREN (slp_node)[operand];
      *slp_def = child;
      *vectype = SLP_TREE_VECTYPE (child);
      if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	{
	  /* ???  VEC_PERM nodes might be intermediate and their lane value
	     have no representative (nor do we build a VEC_PERM stmt for
	     the actual operation).  Note for two-operator nodes we set
	     a representative but leave scalar stmts empty as we'd only
	     have one for a subset of lanes.  Ideally no caller would
	     require *op for internal defs.  */
	  if (SLP_TREE_REPRESENTATIVE (child))
	    {
	      *op = gimple_get_lhs (SLP_TREE_REPRESENTATIVE (child)->stmt);
	      return vect_is_simple_use (*op, vinfo, dt, def_stmt_info_out);
	    }
	  else
	    {
	      gcc_assert (SLP_TREE_CODE (child) == VEC_PERM_EXPR);
	      *op = error_mark_node;
	      *dt = vect_internal_def;
	      if (def_stmt_info_out)
		*def_stmt_info_out = NULL;
	      return true;
	    }
	}
      else
	{
	  if (def_stmt_info_out)
	    *def_stmt_info_out = NULL;
	  *op = SLP_TREE_SCALAR_OPS (child)[0];
	  *dt = SLP_TREE_DEF_TYPE (child);
	  return true;
	}
    }
  else
    {
      *slp_def = NULL;
      if (gassign *ass = dyn_cast <gassign *> (stmt->stmt))
	{
	  if (gimple_assign_rhs_code (ass) == COND_EXPR
	      && COMPARISON_CLASS_P (gimple_assign_rhs1 (ass)))
	    {
	      if (operand < 2)
		*op = TREE_OPERAND (gimple_assign_rhs1 (ass), operand);
	      else
		*op = gimple_op (ass, operand);
	    }
	  else if (gimple_assign_rhs_code (ass) == VIEW_CONVERT_EXPR)
	    *op = TREE_OPERAND (gimple_assign_rhs1 (ass), 0);
	  else
	    *op = gimple_op (ass, operand + 1);
	}
      else if (gcond *cond = dyn_cast <gcond *> (stmt->stmt))
	*op = gimple_op (cond, operand);
      else if (gcall *call = dyn_cast <gcall *> (stmt->stmt))
	*op = gimple_call_arg (call, operand);
      else
	gcc_unreachable ();
      return vect_is_simple_use (*op, vinfo, dt, vectype, def_stmt_info_out);
    }
}

/* If OP is not NULL and is external or constant update its vector
   type with VECTYPE.  Returns true if successful or false if not,
   for example when conflicting vector types are present.  */

bool
vect_maybe_update_slp_op_vectype (slp_tree op, tree vectype)
{
  if (!op || SLP_TREE_DEF_TYPE (op) == vect_internal_def)
    return true;
  if (SLP_TREE_VECTYPE (op))
    return types_compatible_p (SLP_TREE_VECTYPE (op), vectype);
  /* For external defs refuse to produce VECTOR_BOOLEAN_TYPE_P, those
     should be handled by patters.  Allow vect_constant_def for now
     as well as the trivial single-lane uniform vect_external_def case
     both of which we code-generate reasonably.  */
  if (VECTOR_BOOLEAN_TYPE_P (vectype)
      && SLP_TREE_DEF_TYPE (op) == vect_external_def
      && SLP_TREE_LANES (op) > 1)
    return false;
  SLP_TREE_VECTYPE (op) = vectype;
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
supportable_widening_operation (vec_info *vinfo,
				code_helper code,
				stmt_vec_info stmt_info,
				tree vectype_out, tree vectype_in,
				code_helper *code1,
				code_helper *code2,
                                int *multi_step_cvt,
                                vec<tree> *interm_types)
{
  loop_vec_info loop_info = dyn_cast <loop_vec_info> (vinfo);
  class loop *vect_loop = NULL;
  machine_mode vec_mode;
  enum insn_code icode1, icode2;
  optab optab1 = unknown_optab, optab2 = unknown_optab;
  tree vectype = vectype_in;
  tree wide_vectype = vectype_out;
  tree_code c1 = MAX_TREE_CODES, c2 = MAX_TREE_CODES;
  int i;
  tree prev_type, intermediate_type;
  machine_mode intermediate_mode, prev_mode;
  optab optab3, optab4;

  *multi_step_cvt = 0;
  if (loop_info)
    vect_loop = LOOP_VINFO_LOOP (loop_info);

  switch (code.safe_as_tree_code ())
    {
    case MAX_TREE_CODES:
      /* Don't set c1 and c2 if code is not a tree_code.  */
      break;

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
	  && !nested_in_vect_loop_p (vect_loop, stmt_info)
	  && supportable_widening_operation (vinfo, VEC_WIDEN_MULT_EVEN_EXPR,
					     stmt_info, vectype_out,
					     vectype_in, code1,
					     code2, multi_step_cvt,
					     interm_types))
        {
          /* Elements in a vector with vect_used_by_reduction property cannot
             be reordered if the use chain with this property does not have the
             same operation.  One such an example is s += a * b, where elements
             in a and b cannot be reordered.  Here we check if the vector defined
             by STMT is only directly used in the reduction statement.  */
	  tree lhs = gimple_assign_lhs (vect_orig_stmt (stmt_info)->stmt);
	  stmt_vec_info use_stmt_info = loop_info->lookup_single_use (lhs);
	  if (use_stmt_info && STMT_VINFO_REDUC_DEF (use_stmt_info))
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
  else if (CONVERT_EXPR_CODE_P (code.safe_as_tree_code ())
	   && VECTOR_BOOLEAN_TYPE_P (wide_vectype)
	   && VECTOR_BOOLEAN_TYPE_P (vectype)
	   && TYPE_MODE (wide_vectype) == TYPE_MODE (vectype)
	   && SCALAR_INT_MODE_P (TYPE_MODE (vectype)))
    {
      /* If the input and result modes are the same, a different optab
	 is needed where we pass in the number of units in vectype.  */
      optab1 = vec_unpacks_sbool_lo_optab;
      optab2 = vec_unpacks_sbool_hi_optab;
    }

  vec_mode = TYPE_MODE (vectype);
  if (widening_fn_p (code))
     {
       /* If this is an internal fn then we must check whether the target
	  supports either a low-high split or an even-odd split.  */
      internal_fn ifn = as_internal_fn ((combined_fn) code);

      internal_fn lo, hi, even, odd;
      lookup_hilo_internal_fn (ifn, &lo, &hi);
      *code1 = as_combined_fn (lo);
      *code2 = as_combined_fn (hi);
      optab1 = direct_internal_fn_optab (lo, {vectype, vectype});
      optab2 = direct_internal_fn_optab (hi, {vectype, vectype});

      /* If we don't support low-high, then check for even-odd.  */
      if (!optab1
	  || (icode1 = optab_handler (optab1, vec_mode)) == CODE_FOR_nothing
	  || !optab2
	  || (icode2 = optab_handler (optab2, vec_mode)) == CODE_FOR_nothing)
	{
	  lookup_evenodd_internal_fn (ifn, &even, &odd);
	  *code1 = as_combined_fn (even);
	  *code2 = as_combined_fn (odd);
	  optab1 = direct_internal_fn_optab (even, {vectype, vectype});
	  optab2 = direct_internal_fn_optab (odd, {vectype, vectype});
	}
    }
  else if (code.is_tree_code ())
    {
      if (code == FIX_TRUNC_EXPR)
	{
	  /* The signedness is determined from output operand.  */
	  optab1 = optab_for_tree_code (c1, vectype_out, optab_default);
	  optab2 = optab_for_tree_code (c2, vectype_out, optab_default);
	}
      else if (CONVERT_EXPR_CODE_P ((tree_code) code.safe_as_tree_code ())
	       && VECTOR_BOOLEAN_TYPE_P (wide_vectype)
	       && VECTOR_BOOLEAN_TYPE_P (vectype)
	       && TYPE_MODE (wide_vectype) == TYPE_MODE (vectype)
	       && SCALAR_INT_MODE_P (TYPE_MODE (vectype)))
	{
	  /* If the input and result modes are the same, a different optab
	     is needed where we pass in the number of units in vectype.  */
	  optab1 = vec_unpacks_sbool_lo_optab;
	  optab2 = vec_unpacks_sbool_hi_optab;
	}
      else
	{
	  optab1 = optab_for_tree_code (c1, vectype, optab_default);
	  optab2 = optab_for_tree_code (c2, vectype, optab_default);
	}
      *code1 = c1;
      *code2 = c2;
    }

  if (!optab1 || !optab2)
    return false;

  if ((icode1 = optab_handler (optab1, vec_mode)) == CODE_FOR_nothing
       || (icode2 = optab_handler (optab2, vec_mode)) == CODE_FOR_nothing)
    return false;


  if (insn_data[icode1].operand[0].mode == TYPE_MODE (wide_vectype)
      && insn_data[icode2].operand[0].mode == TYPE_MODE (wide_vectype))
    {
      if (!VECTOR_BOOLEAN_TYPE_P (vectype))
	return true;
      /* For scalar masks we may have different boolean
	 vector types having the same QImode.  Thus we
	 add additional check for elements number.  */
      if (known_eq (TYPE_VECTOR_SUBPARTS (vectype),
		    TYPE_VECTOR_SUBPARTS (wide_vectype) * 2))
	return true;
    }

  /* Check if it's a multi-step conversion that can be done using intermediate
     types.  */

  prev_type = vectype;
  prev_mode = vec_mode;

  if (!CONVERT_EXPR_CODE_P (code.safe_as_tree_code ()))
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
	intermediate_type
	  = vect_halve_mask_nunits (prev_type, intermediate_mode);
      else if (VECTOR_MODE_P (intermediate_mode))
	{
	  tree intermediate_element_type
	    = lang_hooks.types.type_for_mode (GET_MODE_INNER (intermediate_mode),
					      TYPE_UNSIGNED (prev_type));
	  intermediate_type
	    = build_vector_type_for_mode (intermediate_element_type,
					  intermediate_mode);
	}
      else
	intermediate_type
	  = lang_hooks.types.type_for_mode (intermediate_mode,
					    TYPE_UNSIGNED (prev_type));

      if (VECTOR_BOOLEAN_TYPE_P (intermediate_type)
	  && VECTOR_BOOLEAN_TYPE_P (prev_type)
	  && intermediate_mode == prev_mode
	  && SCALAR_INT_MODE_P (prev_mode))
	{
	  /* If the input and result modes are the same, a different optab
	     is needed where we pass in the number of units in vectype.  */
	  optab3 = vec_unpacks_sbool_lo_optab;
	  optab4 = vec_unpacks_sbool_hi_optab;
	}
      else
	{
	  optab3 = optab_for_tree_code (c1, intermediate_type, optab_default);
	  optab4 = optab_for_tree_code (c2, intermediate_type, optab_default);
	}

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
	{
	  if (!VECTOR_BOOLEAN_TYPE_P (vectype))
	    return true;
	  if (known_eq (TYPE_VECTOR_SUBPARTS (intermediate_type),
			TYPE_VECTOR_SUBPARTS (wide_vectype) * 2))
	    return true;
	}

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
supportable_narrowing_operation (code_helper code,
				 tree vectype_out, tree vectype_in,
				 code_helper *code1, int *multi_step_cvt,
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
  unsigned HOST_WIDE_INT n_elts;
  bool uns;

  if (!code.is_tree_code ())
    return false;

  *multi_step_cvt = 0;
  switch ((tree_code) code)
    {
    CASE_CONVERT:
      c1 = VEC_PACK_TRUNC_EXPR;
      if (VECTOR_BOOLEAN_TYPE_P (narrow_vectype)
	  && VECTOR_BOOLEAN_TYPE_P (vectype)
	  && SCALAR_INT_MODE_P (TYPE_MODE (vectype))
	  && TYPE_VECTOR_SUBPARTS (vectype).is_constant (&n_elts)
	  && n_elts < BITS_PER_UNIT)
	optab1 = vec_pack_sbool_trunc_optab;
      else
	optab1 = optab_for_tree_code (c1, vectype, optab_default);
      break;

    case FIX_TRUNC_EXPR:
      c1 = VEC_PACK_FIX_TRUNC_EXPR;
      /* The signedness is determined from output operand.  */
      optab1 = optab_for_tree_code (c1, vectype_out, optab_default);
      break;

    case FLOAT_EXPR:
      c1 = VEC_PACK_FLOAT_EXPR;
      optab1 = optab_for_tree_code (c1, vectype, optab_default);
      break;

    default:
      gcc_unreachable ();
    }

  if (!optab1)
    return false;

  vec_mode = TYPE_MODE (vectype);
  if ((icode1 = optab_handler (optab1, vec_mode)) == CODE_FOR_nothing)
    return false;

  *code1 = c1;

  if (insn_data[icode1].operand[0].mode == TYPE_MODE (narrow_vectype))
    {
      if (!VECTOR_BOOLEAN_TYPE_P (vectype))
	return true;
      /* For scalar masks we may have different boolean
	 vector types having the same QImode.  Thus we
	 add additional check for elements number.  */
      if (known_eq (TYPE_VECTOR_SUBPARTS (vectype) * 2,
		    TYPE_VECTOR_SUBPARTS (narrow_vectype)))
	return true;
    }

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
	intermediate_type
	  = vect_double_mask_nunits (prev_type, intermediate_mode);
      else
	intermediate_type
	  = lang_hooks.types.type_for_mode (intermediate_mode, uns);
      if (VECTOR_BOOLEAN_TYPE_P (intermediate_type)
	  && VECTOR_BOOLEAN_TYPE_P (prev_type)
	  && SCALAR_INT_MODE_P (prev_mode)
	  && TYPE_VECTOR_SUBPARTS (intermediate_type).is_constant (&n_elts)
	  && n_elts < BITS_PER_UNIT)
	interm_optab = vec_pack_sbool_trunc_optab;
      else
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
	{
	  if (!VECTOR_BOOLEAN_TYPE_P (vectype))
	    return true;
	  if (known_eq (TYPE_VECTOR_SUBPARTS (intermediate_type) * 2,
			TYPE_VECTOR_SUBPARTS (narrow_vectype)))
	    return true;
	}

      prev_mode = intermediate_mode;
      prev_type = intermediate_type;
      optab1 = interm_optab;
    }

  interm_types->release ();
  return false;
}

/* Function supportable_indirect_convert_operation

   Check whether an operation represented by the code CODE is single or multi
   operations that are supported by the target platform in
   vector form (i.e., when operating on arguments of type VECTYPE_IN
   producing a result of type VECTYPE_OUT).

   Convert operations we currently support directly are FIX_TRUNC and FLOAT.
   This function checks if these operations are supported
   by the target platform directly (via vector tree-codes).

   Output:
   - converts contains some pairs to perform the convert operation,
   the pair's first is the intermediate type, and its second is the code of
   a vector operation to be used when converting the operation from the
   previous type to the intermediate type. */
bool
supportable_indirect_convert_operation (code_helper code,
					tree vectype_out,
					tree vectype_in,
					vec<std::pair<tree, tree_code> > *converts,
					tree op0)
{
  bool found_mode = false;
  scalar_mode lhs_mode = GET_MODE_INNER (TYPE_MODE (vectype_out));
  scalar_mode rhs_mode = GET_MODE_INNER (TYPE_MODE (vectype_in));
  opt_scalar_mode mode_iter;
  tree_code tc1, tc2, code1, code2;

  tree cvt_type = NULL_TREE;
  poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (vectype_in);

  if (supportable_convert_operation ((tree_code) code,
				     vectype_out,
				     vectype_in,
				     &tc1))
    {
      converts->safe_push (std::make_pair (vectype_out, tc1));
      return true;
    }

  /* For conversions between float and integer types try whether
     we can use intermediate signed integer types to support the
     conversion.  */
  if (GET_MODE_SIZE (lhs_mode) != GET_MODE_SIZE (rhs_mode)
      && (code == FLOAT_EXPR
	  || (code == FIX_TRUNC_EXPR && !flag_trapping_math)))
    {
      bool demotion = GET_MODE_SIZE (rhs_mode) > GET_MODE_SIZE (lhs_mode);
      bool float_expr_p = code == FLOAT_EXPR;
      unsigned short target_size;
      scalar_mode intermediate_mode;
      if (demotion)
	{
	  intermediate_mode = lhs_mode;
	  target_size = GET_MODE_SIZE (rhs_mode);
	}
      else
	{
	  target_size = GET_MODE_SIZE (lhs_mode);
	  if (!int_mode_for_size
	      (GET_MODE_BITSIZE (rhs_mode), 0).exists (&intermediate_mode))
	    return false;
	}
      code1 = float_expr_p ? (tree_code) code : NOP_EXPR;
      code2 = float_expr_p ? NOP_EXPR : (tree_code) code;
      opt_scalar_mode mode_iter;
      FOR_EACH_2XWIDER_MODE (mode_iter, intermediate_mode)
	{
	  intermediate_mode = mode_iter.require ();

	  if (GET_MODE_SIZE (intermediate_mode) > target_size)
	    break;

	  scalar_mode cvt_mode;
	  if (!int_mode_for_size
	      (GET_MODE_BITSIZE (intermediate_mode), 0).exists (&cvt_mode))
	    break;

	  cvt_type = build_nonstandard_integer_type
	    (GET_MODE_BITSIZE (cvt_mode), 0);

	  /* Check if the intermediate type can hold OP0's range.
	     When converting from float to integer this is not necessary
	     because values that do not fit the (smaller) target type are
	     unspecified anyway.  */
	  if (demotion && float_expr_p)
	    {
	      wide_int op_min_value, op_max_value;
	      /* For vector form, it looks like op0 doesn't have RANGE_INFO.
		 In the future, if it is supported, changes may need to be made
		 to this part, such as checking the RANGE of each element
		 in the vector.  */
	      if (TREE_CODE (op0) != SSA_NAME
		  || !SSA_NAME_RANGE_INFO (op0)
		  || !vect_get_range_info (op0, &op_min_value,
					   &op_max_value))
		break;

	      if (cvt_type == NULL_TREE
		  || (wi::min_precision (op_max_value, SIGNED)
		      > TYPE_PRECISION (cvt_type))
		  || (wi::min_precision (op_min_value, SIGNED)
		      > TYPE_PRECISION (cvt_type)))
		continue;
	    }

	  cvt_type = get_related_vectype_for_scalar_type (TYPE_MODE (vectype_in),
							  cvt_type,
							  nelts);
	  /* This should only happened for SLP as long as loop vectorizer
	     only supports same-sized vector.  */
	  if (cvt_type == NULL_TREE
	      || maybe_ne (TYPE_VECTOR_SUBPARTS (cvt_type), nelts)
	      || !supportable_convert_operation ((tree_code) code1,
						 vectype_out,
						 cvt_type, &tc1)
	      || !supportable_convert_operation ((tree_code) code2,
						 cvt_type,
						 vectype_in, &tc2))
	    continue;

	  found_mode = true;
	  break;
	}

      if (found_mode)
	{
	  converts->safe_push (std::make_pair (cvt_type, tc2));
	  if (TYPE_MODE (cvt_type) != TYPE_MODE (vectype_out))
	    converts->safe_push (std::make_pair (vectype_out, tc1));
	  return true;
	}
    }
  return false;
}

/* Generate and return a vector mask of MASK_TYPE such that
   mask[I] is true iff J + START_INDEX < END_INDEX for all J <= I.
   Add the statements to SEQ.  */

tree
vect_gen_while (gimple_seq *seq, tree mask_type, tree start_index,
		tree end_index, const char *name)
{
  tree cmp_type = TREE_TYPE (start_index);
  gcc_checking_assert (direct_internal_fn_supported_p (IFN_WHILE_ULT,
						       cmp_type, mask_type,
						       OPTIMIZE_FOR_SPEED));
  gcall *call = gimple_build_call_internal (IFN_WHILE_ULT, 3,
					    start_index, end_index,
					    build_zero_cst (mask_type));
  tree tmp;
  if (name)
    tmp = make_temp_ssa_name (mask_type, NULL, name);
  else
    tmp = make_ssa_name (mask_type);
  gimple_call_set_lhs (call, tmp);
  gimple_seq_add_stmt (seq, call);
  return tmp;
}

/* Generate a vector mask of type MASK_TYPE for which index I is false iff
   J + START_INDEX < END_INDEX for all J <= I.  Add the statements to SEQ.  */

tree
vect_gen_while_not (gimple_seq *seq, tree mask_type, tree start_index,
		    tree end_index)
{
  tree tmp = vect_gen_while (seq, mask_type, start_index, end_index);
  return gimple_build (seq, BIT_NOT_EXPR, mask_type, tmp);
}

/* Try to compute the vector types required to vectorize STMT_INFO,
   returning true on success and false if vectorization isn't possible.
   If GROUP_SIZE is nonzero and we're performing BB vectorization,
   take sure that the number of elements in the vectors is no bigger
   than GROUP_SIZE.

   On success:

   - Set *STMT_VECTYPE_OUT to:
     - NULL_TREE if the statement doesn't need to be vectorized;
     - the equivalent of STMT_VINFO_VECTYPE otherwise.

   - Set *NUNITS_VECTYPE_OUT to the vector type that contains the maximum
     number of units needed to vectorize STMT_INFO, or NULL_TREE if the
     statement does not help to determine the overall number of units.  */

opt_result
vect_get_vector_types_for_stmt (vec_info *vinfo, stmt_vec_info stmt_info,
				tree *stmt_vectype_out,
				tree *nunits_vectype_out,
				unsigned int group_size)
{
  gimple *stmt = stmt_info->stmt;

  /* For BB vectorization, we should always have a group size once we've
     constructed the SLP tree; the only valid uses of zero GROUP_SIZEs
     are tentative requests during things like early data reference
     analysis and pattern recognition.  */
  if (is_a <bb_vec_info> (vinfo))
    gcc_assert (vinfo->slp_instances.is_empty () || group_size != 0);
  else
    group_size = 0;

  *stmt_vectype_out = NULL_TREE;
  *nunits_vectype_out = NULL_TREE;

  if (gimple_get_lhs (stmt) == NULL_TREE
      /* Allow vector conditionals through here.  */
      && !is_a <gcond *> (stmt)
      /* MASK_STORE and friends have no lhs, but are ok.  */
      && !(is_gimple_call (stmt)
	   && gimple_call_internal_p (stmt)
	   && internal_store_fn_p (gimple_call_internal_fn (stmt))))
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
				     "not vectorized: irregular stmt: %G", stmt);
    }

  tree vectype;
  tree scalar_type = NULL_TREE;
  if (group_size == 0 && STMT_VINFO_VECTYPE (stmt_info))
    {
      vectype = STMT_VINFO_VECTYPE (stmt_info);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "precomputed vectype: %T\n", vectype);
    }
  else if (vect_use_mask_type_p (stmt_info))
    {
      unsigned int precision = stmt_info->mask_precision;
      scalar_type = build_nonstandard_integer_type (precision, 1);
      vectype = get_mask_type_for_scalar_type (vinfo, scalar_type, group_size);
      if (!vectype)
	return opt_result::failure_at (stmt, "not vectorized: unsupported"
				       " data-type %T\n", scalar_type);
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "vectype: %T\n", vectype);
    }
  else
    {
      /* If we got here with a gcond it means that the target had no available vector
	 mode for the scalar type.  We can't vectorize so abort.  */
      if (is_a <gcond *> (stmt))
	return opt_result::failure_at (stmt,
				       "not vectorized:"
				       " unsupported data-type for gcond %T\n",
				       scalar_type);

      if (data_reference *dr = STMT_VINFO_DATA_REF (stmt_info))
	scalar_type = TREE_TYPE (DR_REF (dr));
      else
	scalar_type = TREE_TYPE (gimple_get_lhs (stmt));

      if (dump_enabled_p ())
	{
	  if (group_size)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "get vectype for scalar type (group size %d):"
			     " %T\n", group_size, scalar_type);
	  else
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "get vectype for scalar type: %T\n", scalar_type);
	}
      vectype = get_vectype_for_scalar_type (vinfo, scalar_type, group_size);
      if (!vectype)
	return opt_result::failure_at (stmt,
				       "not vectorized:"
				       " unsupported data-type %T\n",
				       scalar_type);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "vectype: %T\n", vectype);
    }

  if (scalar_type && VECTOR_MODE_P (TYPE_MODE (scalar_type)))
    return opt_result::failure_at (stmt,
				   "not vectorized: vector stmt in loop:%G",
				   stmt);

  *stmt_vectype_out = vectype;

  /* Don't try to compute scalar types if the stmt produces a boolean
     vector; use the existing vector type instead.  */
  tree nunits_vectype = vectype;
  if (!VECTOR_BOOLEAN_TYPE_P (vectype))
    {
      /* The number of units is set according to the smallest scalar
	 type (or the largest vector size, but we only support one
	 vector size per vectorization).  */
      scalar_type = vect_get_smallest_scalar_type (stmt_info,
						   TREE_TYPE (vectype));
      if (!types_compatible_p (scalar_type, TREE_TYPE (vectype)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "get vectype for smallest scalar type: %T\n",
			     scalar_type);
	  nunits_vectype = get_vectype_for_scalar_type (vinfo, scalar_type,
							group_size);
	  if (!nunits_vectype)
	    return opt_result::failure_at
	      (stmt, "not vectorized: unsupported data-type %T\n",
	       scalar_type);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location, "nunits vectype: %T\n",
			     nunits_vectype);
	}
    }

  if (!multiple_p (TYPE_VECTOR_SUBPARTS (nunits_vectype),
		   TYPE_VECTOR_SUBPARTS (*stmt_vectype_out)))
    return opt_result::failure_at (stmt,
				   "Not vectorized: Incompatible number "
				   "of vector subparts between %T and %T\n",
				   nunits_vectype, *stmt_vectype_out);

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "nunits = ");
      dump_dec (MSG_NOTE, TYPE_VECTOR_SUBPARTS (nunits_vectype));
      dump_printf (MSG_NOTE, "\n");
    }

  *nunits_vectype_out = nunits_vectype;
  return opt_result::success ();
}

/* Generate and return statement sequence that sets vector length LEN that is:

   min_of_start_and_end = min (START_INDEX, END_INDEX);
   left_len = END_INDEX - min_of_start_and_end;
   rhs = min (left_len, LEN_LIMIT);
   LEN = rhs;

   Note: the cost of the code generated by this function is modeled
   by vect_estimate_min_profitable_iters, so changes here may need
   corresponding changes there.  */

gimple_seq
vect_gen_len (tree len, tree start_index, tree end_index, tree len_limit)
{
  gimple_seq stmts = NULL;
  tree len_type = TREE_TYPE (len);
  gcc_assert (TREE_TYPE (start_index) == len_type);

  tree min = gimple_build (&stmts, MIN_EXPR, len_type, start_index, end_index);
  tree left_len = gimple_build (&stmts, MINUS_EXPR, len_type, end_index, min);
  tree rhs = gimple_build (&stmts, MIN_EXPR, len_type, left_len, len_limit);
  gimple* stmt = gimple_build_assign (len, rhs);
  gimple_seq_add_stmt (&stmts, stmt);

  return stmts;
}

