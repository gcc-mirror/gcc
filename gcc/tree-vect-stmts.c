/* Statement Analysis and Transformation for Vectorization
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
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
#include "dumpfile.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "target.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "cfgloop.h"
#include "expr.h"
#include "recog.h"		/* FIXME: for insn_data */
#include "optabs.h"
#include "diagnostic-core.h"
#include "tree-vectorizer.h"
#include "dumpfile.h"

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
  gimple stmt = STMT_VINFO_STMT (stmt_info);
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
  if (body_cost_vec)
    {
      tree vectype = stmt_info ? stmt_vectype (stmt_info) : NULL_TREE;
      add_stmt_info_to_vec (body_cost_vec, count, kind,
			    stmt_info ? STMT_VINFO_STMT (stmt_info) : NULL,
			    misalign);
      return (unsigned)
	(builtin_vectorization_cost (kind, vectype, misalign) * count);
	 
    }
  else
    {
      loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
      bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
      void *target_cost_data;

      if (loop_vinfo)
	target_cost_data = LOOP_VINFO_TARGET_COST_DATA (loop_vinfo);
      else
	target_cost_data = BB_VINFO_TARGET_COST_DATA (bb_vinfo);

      return add_stmt_cost (target_cost_data, count, kind, stmt_info,
			    misalign, where);
    }
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
   is part of the vectorization of STMT and the vector is associated
   with scalar destination SCALAR_DEST.  */

static tree
read_vector_array (gimple stmt, gimple_stmt_iterator *gsi, tree scalar_dest,
		   tree array, unsigned HOST_WIDE_INT n)
{
  tree vect_type, vect, vect_name, array_ref;
  gimple new_stmt;

  gcc_assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
  vect_type = TREE_TYPE (TREE_TYPE (array));
  vect = vect_create_destination_var (scalar_dest, vect_type);
  array_ref = build4 (ARRAY_REF, vect_type, array,
		      build_int_cst (size_type_node, n),
		      NULL_TREE, NULL_TREE);

  new_stmt = gimple_build_assign (vect, array_ref);
  vect_name = make_ssa_name (vect, new_stmt);
  gimple_assign_set_lhs (new_stmt, vect_name);
  vect_finish_stmt_generation (stmt, new_stmt, gsi);

  return vect_name;
}

/* ARRAY is an array of vectors created by create_vector_array.
   Emit code to store SSA_NAME VECT in index N of the array.
   The store is part of the vectorization of STMT.  */

static void
write_vector_array (gimple stmt, gimple_stmt_iterator *gsi, tree vect,
		    tree array, unsigned HOST_WIDE_INT n)
{
  tree array_ref;
  gimple new_stmt;

  array_ref = build4 (ARRAY_REF, TREE_TYPE (vect), array,
		      build_int_cst (size_type_node, n),
		      NULL_TREE, NULL_TREE);

  new_stmt = gimple_build_assign (array_ref, vect);
  vect_finish_stmt_generation (stmt, new_stmt, gsi);
}

/* PTR is a pointer to an array of type TYPE.  Return a representation
   of *PTR.  The memory reference replaces those in FIRST_DR
   (and its group).  */

static tree
create_array_ref (tree type, tree ptr, struct data_reference *first_dr)
{
  tree mem_ref, alias_ptr_type;

  alias_ptr_type = reference_alias_ptr_type (DR_REF (first_dr));
  mem_ref = build2 (MEM_REF, type, ptr, build_int_cst (alias_ptr_type, 0));
  /* Arrays have the same alignment as their type.  */
  set_ptr_info_alignment (get_ptr_info (ptr), TYPE_ALIGN_UNIT (type), 0);
  return mem_ref;
}

/* Utility functions used by vect_mark_stmts_to_be_vectorized.  */

/* Function vect_mark_relevant.

   Mark STMT as "relevant for vectorization" and add it to WORKLIST.  */

static void
vect_mark_relevant (vec<gimple> *worklist, gimple stmt,
		    enum vect_relevant relevant, bool live_p,
		    bool used_in_pattern)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  enum vect_relevant save_relevant = STMT_VINFO_RELEVANT (stmt_info);
  bool save_live_p = STMT_VINFO_LIVE_P (stmt_info);
  gimple pattern_stmt;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "mark relevant %d, live %d.", relevant, live_p);

  /* If this stmt is an original stmt in a pattern, we might need to mark its
     related pattern stmt instead of the original stmt.  However, such stmts
     may have their own uses that are not in any pattern, in such cases the
     stmt itself should be marked.  */
  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
    {
      bool found = false;
      if (!used_in_pattern)
        {
          imm_use_iterator imm_iter;
          use_operand_p use_p;
          gimple use_stmt;
          tree lhs;
	  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
	  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

          if (is_gimple_assign (stmt))
            lhs = gimple_assign_lhs (stmt);
          else
            lhs = gimple_call_lhs (stmt);

          /* This use is out of pattern use, if LHS has other uses that are
             pattern uses, we should mark the stmt itself, and not the pattern
             stmt.  */
	  if (TREE_CODE (lhs) == SSA_NAME)
	    FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
	      {
		if (is_gimple_debug (USE_STMT (use_p)))
		  continue;
		use_stmt = USE_STMT (use_p);

		if (!flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
		  continue;

		if (vinfo_for_stmt (use_stmt)
		    && STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (use_stmt)))
		  {
		    found = true;
		    break;
		  }
	      }
        }

      if (!found)
        {
          /* This is the last stmt in a sequence that was detected as a
             pattern that can potentially be vectorized.  Don't mark the stmt
             as relevant/live because it's not going to be vectorized.
             Instead mark the pattern-stmt that replaces it.  */

          pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info);

          if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location,
                             "last stmt in pattern. don't mark"
                             " relevant/live.");
          stmt_info = vinfo_for_stmt (pattern_stmt);
          gcc_assert (STMT_VINFO_RELATED_STMT (stmt_info) == stmt);
          save_relevant = STMT_VINFO_RELEVANT (stmt_info);
          save_live_p = STMT_VINFO_LIVE_P (stmt_info);
          stmt = pattern_stmt;
        }
    }

  STMT_VINFO_LIVE_P (stmt_info) |= live_p;
  if (relevant > STMT_VINFO_RELEVANT (stmt_info))
    STMT_VINFO_RELEVANT (stmt_info) = relevant;

  if (STMT_VINFO_RELEVANT (stmt_info) == save_relevant
      && STMT_VINFO_LIVE_P (stmt_info) == save_live_p)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "already marked relevant/live.");
      return;
    }

  worklist->safe_push (stmt);
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

  *relevant = vect_unused_in_scope;
  *live_p = false;

  /* cond stmt other than loop exit cond.  */
  if (is_ctrl_stmt (stmt)
      && STMT_VINFO_TYPE (vinfo_for_stmt (stmt))
         != loop_exit_ctrl_vec_info_type)
    *relevant = vect_used_in_scope;

  /* changing memory.  */
  if (gimple_code (stmt) != GIMPLE_PHI)
    if (gimple_vdef (stmt))
      {
	if (dump_enabled_p ())
	  dump_printf_loc (MSG_NOTE, vect_location,
                           "vec_stmt_relevant_p: stmt has vdefs.");
	*relevant = vect_used_in_scope;
      }

  /* uses outside the loop.  */
  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, op_iter, SSA_OP_DEF)
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, DEF_FROM_PTR (def_p))
	{
	  basic_block bb = gimple_bb (USE_STMT (use_p));
	  if (!flow_bb_inside_loop_p (loop, bb))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
                                 "vec_stmt_relevant_p: used out of loop.");

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

  return (*live_p || *relevant);
}


/* Function exist_non_indexing_operands_for_use_p

   USE is one of the uses attached to STMT.  Check if USE is
   used in STMT for anything other than indexing an array.  */

static bool
exist_non_indexing_operands_for_use_p (tree use, gimple stmt)
{
  tree operand;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

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

  if (!gimple_assign_copy_p (stmt))
    return false;
  if (TREE_CODE (gimple_assign_lhs (stmt)) == SSA_NAME)
    return false;
  operand = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  if (operand == use)
    return true;

  return false;
}


/*
   Function process_use.

   Inputs:
   - a USE in STMT in a loop represented by LOOP_VINFO
   - LIVE_P, RELEVANT - enum values to be set in the STMT_VINFO of the stmt
     that defined USE.  This is done by calling mark_relevant and passing it
     the WORKLIST (to add DEF_STMT to the WORKLIST in case it is relevant).
   - FORCE is true if exist_non_indexing_operands_for_use_p check shouldn't
     be performed.

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
	     enum vect_relevant relevant, vec<gimple> *worklist,
	     bool force)
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
  if (!force && !exist_non_indexing_operands_for_use_p (use, stmt))
     return true;

  if (!vect_is_simple_use (use, stmt, loop_vinfo, NULL, &def_stmt, &def, &dt))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not vectorized: unsupported use in stmt.");
      return false;
    }

  if (!def_stmt || gimple_nop_p (def_stmt))
    return true;

  def_bb = gimple_bb (def_stmt);
  if (!flow_bb_inside_loop_p (loop, def_bb))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "def_stmt is out of loop.");
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
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "reduc-stmt defining reduc-phi in the same nest.");
      if (STMT_VINFO_IN_PATTERN_P (dstmt_vinfo))
	dstmt_vinfo = vinfo_for_stmt (STMT_VINFO_RELATED_STMT (dstmt_vinfo));
      gcc_assert (STMT_VINFO_RELEVANT (dstmt_vinfo) < vect_used_by_reduction);
      gcc_assert (STMT_VINFO_LIVE_P (dstmt_vinfo)
		  || STMT_VINFO_RELEVANT (dstmt_vinfo) > vect_unused_in_scope);
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
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "outer-loop def-stmt defining inner-loop stmt.");

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
		d = def_stmt
	outer-loop-tail-bb (or outer-loop-exit-bb in double reduction):
		stmt # use (d)		*/
  else if (flow_loop_nested_p (bb->loop_father, def_bb->loop_father))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "inner-loop def-stmt defining outer-loop stmt.");

      switch (relevant)
        {
        case vect_unused_in_scope:
          relevant = (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
            || STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_double_reduction_def) ?
                      vect_used_in_outer_by_reduction : vect_unused_in_scope;
          break;

        case vect_used_by_reduction:
          relevant = vect_used_in_outer_by_reduction;
          break;

        case vect_used_in_scope:
          relevant = vect_used_in_outer;
          break;

        default:
          gcc_unreachable ();
        }
    }

  vect_mark_relevant (worklist, def_stmt, relevant, live_p,
                      is_pattern_stmt_p (stmt_vinfo));
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

bool
vect_mark_stmts_to_be_vectorized (loop_vec_info loop_vinfo)
{
  vec<gimple> worklist;
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
  enum vect_relevant relevant, tmp_relevant;
  enum vect_def_type def_type;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "=== vect_mark_stmts_to_be_vectorized ===");

  worklist.create (64);

  /* 1. Init worklist.  */
  for (i = 0; i < nbbs; i++)
    {
      bb = bbs[i];
      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  phi = gsi_stmt (si);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location, "init: phi relevant? ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	    }

	  if (vect_stmt_relevant_p (phi, loop_vinfo, &relevant, &live_p))
	    vect_mark_relevant (&worklist, phi, relevant, live_p, false);
	}
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  stmt = gsi_stmt (si);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location, "init: stmt relevant? ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	    }

	  if (vect_stmt_relevant_p (stmt, loop_vinfo, &relevant, &live_p))
            vect_mark_relevant (&worklist, stmt, relevant, live_p, false);
	}
    }

  /* 2. Process_worklist */
  while (worklist.length () > 0)
    {
      use_operand_p use_p;
      ssa_op_iter iter;

      stmt = worklist.pop ();
      if (dump_enabled_p ())
	{
          dump_printf_loc (MSG_NOTE, vect_location, "worklist: examine stmt: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
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
	 (also) used by a regular computation.  This allows us later on to
	 identify stmts that are used solely by a reduction, and therefore the
	 order of the results that they produce does not have to be kept.  */

      def_type = STMT_VINFO_DEF_TYPE (stmt_vinfo);
      tmp_relevant = relevant;
      switch (def_type)
        {
          case vect_reduction_def:
	    switch (tmp_relevant)
	      {
	        case vect_unused_in_scope:
	          relevant = vect_used_by_reduction;
	          break;

	        case vect_used_by_reduction:
	          if (gimple_code (stmt) == GIMPLE_PHI)
                    break;
  	          /* fall through */

	        default:
	          if (dump_enabled_p ())
	            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                     "unsupported use of reduction.");
  	          worklist.release ();
	          return false;
	      }

	    live_p = false;
	    break;

          case vect_nested_cycle:
            if (tmp_relevant != vect_unused_in_scope
                && tmp_relevant != vect_used_in_outer_by_reduction
                && tmp_relevant != vect_used_in_outer)
              {
                if (dump_enabled_p ())
                  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                   "unsupported use of nested cycle.");

                worklist.release ();
                return false;
              }

            live_p = false;
            break;

          case vect_double_reduction_def:
            if (tmp_relevant != vect_unused_in_scope
                && tmp_relevant != vect_used_by_reduction)
              {
                if (dump_enabled_p ())
                  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                   "unsupported use of double reduction.");

                worklist.release ();
                return false;
              }

            live_p = false;
            break;

          default:
            break;
        }

      if (is_pattern_stmt_p (stmt_vinfo))
        {
          /* Pattern statements are not inserted into the code, so
             FOR_EACH_PHI_OR_STMT_USE optimizes their operands out, and we
             have to scan the RHS or function arguments instead.  */
          if (is_gimple_assign (stmt))
            {
	      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
	      tree op = gimple_assign_rhs1 (stmt);

	      i = 1;
	      if (rhs_code == COND_EXPR && COMPARISON_CLASS_P (op))
		{
		  if (!process_use (stmt, TREE_OPERAND (op, 0), loop_vinfo,
				    live_p, relevant, &worklist, false)
		      || !process_use (stmt, TREE_OPERAND (op, 1), loop_vinfo,
				       live_p, relevant, &worklist, false))
		    {
		      worklist.release ();
		      return false;
		    }
		  i = 2;
		}
	      for (; i < gimple_num_ops (stmt); i++)
                {
		  op = gimple_op (stmt, i);
                  if (!process_use (stmt, op, loop_vinfo, live_p, relevant,
				    &worklist, false))
                    {
                      worklist.release ();
                      return false;
                    }
                 }
            }
          else if (is_gimple_call (stmt))
            {
              for (i = 0; i < gimple_call_num_args (stmt); i++)
                {
                  tree arg = gimple_call_arg (stmt, i);
                  if (!process_use (stmt, arg, loop_vinfo, live_p, relevant,
				    &worklist, false))
                    {
                      worklist.release ();
                      return false;
                    }
                }
            }
        }
      else
        FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
          {
            tree op = USE_FROM_PTR (use_p);
            if (!process_use (stmt, op, loop_vinfo, live_p, relevant,
			      &worklist, false))
              {
                worklist.release ();
                return false;
              }
          }

      if (STMT_VINFO_GATHER_P (stmt_vinfo))
	{
	  tree off;
	  tree decl = vect_check_gather (stmt, loop_vinfo, NULL, &off, NULL);
	  gcc_assert (decl);
	  if (!process_use (stmt, off, loop_vinfo, live_p, relevant,
			    &worklist, true))
	    {
	      worklist.release ();
	      return false;
	    }
	}
    } /* while worklist */

  worklist.release ();
  return true;
}


/* Function vect_model_simple_cost.

   Models cost for simple operations, i.e. those that only emit ncopies of a
   single op.  Right now, this does not account for multiple insns that could
   be generated for the single vector op.  We will handle that shortly.  */

void
vect_model_simple_cost (stmt_vec_info stmt_info, int ncopies,
			enum vect_def_type *dt,
			stmt_vector_for_cost *prologue_cost_vec,
			stmt_vector_for_cost *body_cost_vec)
{
  int i;
  int inside_cost = 0, prologue_cost = 0;

  /* The SLP costs were already calculated during SLP tree build.  */
  if (PURE_SLP_STMT (stmt_info))
    return;

  /* FORNOW: Assuming maximum 2 args per stmts.  */
  for (i = 0; i < 2; i++)
    if (dt[i] == vect_constant_def || dt[i] == vect_external_def)
      prologue_cost += record_stmt_cost (prologue_cost_vec, 1, vector_stmt,
					 stmt_info, 0, vect_prologue);

  /* Pass the inside-of-loop statements to the target-specific cost model.  */
  inside_cost = record_stmt_cost (body_cost_vec, ncopies, vector_stmt,
				  stmt_info, 0, vect_body);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_simple_cost: inside_cost = %d, "
                     "prologue_cost = %d .", inside_cost, prologue_cost);
}


/* Model cost for type demotion and promotion operations.  PWR is normally
   zero for single-step promotions and demotions.  It will be one if 
   two-step promotion/demotion is required, and so on.  Each additional
   step doubles the number of instructions required.  */

static void
vect_model_promotion_demotion_cost (stmt_vec_info stmt_info,
				    enum vect_def_type *dt, int pwr)
{
  int i, tmp;
  int inside_cost = 0, prologue_cost = 0;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  void *target_cost_data;

  /* The SLP costs were already calculated during SLP tree build.  */
  if (PURE_SLP_STMT (stmt_info))
    return;

  if (loop_vinfo)
    target_cost_data = LOOP_VINFO_TARGET_COST_DATA (loop_vinfo);
  else
    target_cost_data = BB_VINFO_TARGET_COST_DATA (bb_vinfo);

  for (i = 0; i < pwr + 1; i++)
    {
      tmp = (STMT_VINFO_TYPE (stmt_info) == type_promotion_vec_info_type) ?
	(i + 1) : i;
      inside_cost += add_stmt_cost (target_cost_data, vect_pow2 (tmp),
				    vec_promote_demote, stmt_info, 0,
				    vect_body);
    }

  /* FORNOW: Assuming maximum 2 args per stmts.  */
  for (i = 0; i < 2; i++)
    if (dt[i] == vect_constant_def || dt[i] == vect_external_def)
      prologue_cost += add_stmt_cost (target_cost_data, 1, vector_stmt,
				      stmt_info, 0, vect_prologue);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_promotion_demotion_cost: inside_cost = %d, "
                     "prologue_cost = %d .", inside_cost, prologue_cost);
}

/* Function vect_cost_group_size

   For grouped load or store, return the group_size only if it is the first
   load or store of a group, else return 1.  This ensures that group size is
   only returned once per group.  */

static int
vect_cost_group_size (stmt_vec_info stmt_info)
{
  gimple first_stmt = GROUP_FIRST_ELEMENT (stmt_info);

  if (first_stmt == STMT_VINFO_STMT (stmt_info))
    return GROUP_SIZE (stmt_info);

  return 1;
}


/* Function vect_model_store_cost

   Models cost for stores.  In the case of grouped accesses, one access
   has the overhead of the grouped access attributed to it.  */

void
vect_model_store_cost (stmt_vec_info stmt_info, int ncopies,
		       bool store_lanes_p, enum vect_def_type dt,
		       slp_tree slp_node,
		       stmt_vector_for_cost *prologue_cost_vec,
		       stmt_vector_for_cost *body_cost_vec)
{
  int group_size;
  unsigned int inside_cost = 0, prologue_cost = 0;
  struct data_reference *first_dr;
  gimple first_stmt;

  /* The SLP costs were already calculated during SLP tree build.  */
  if (PURE_SLP_STMT (stmt_info))
    return;

  if (dt == vect_constant_def || dt == vect_external_def)
    prologue_cost += record_stmt_cost (prologue_cost_vec, 1, scalar_to_vec,
				       stmt_info, 0, vect_prologue);

  /* Grouped access?  */
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      if (slp_node)
        {
          first_stmt = SLP_TREE_SCALAR_STMTS (slp_node)[0];
          group_size = 1;
        }
      else
        {
          first_stmt = GROUP_FIRST_ELEMENT (stmt_info);
          group_size = vect_cost_group_size (stmt_info);
        }

      first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt));
    }
  /* Not a grouped access.  */
  else
    {
      group_size = 1;
      first_dr = STMT_VINFO_DATA_REF (stmt_info);
    }

  /* We assume that the cost of a single store-lanes instruction is
     equivalent to the cost of GROUP_SIZE separate stores.  If a grouped
     access is instead being provided by a permute-and-store operation,
     include the cost of the permutes.  */
  if (!store_lanes_p && group_size > 1)
    {
      /* Uses a high and low interleave operation for each needed permute.  */
      
      int nstmts = ncopies * exact_log2 (group_size) * group_size;
      inside_cost = record_stmt_cost (body_cost_vec, nstmts, vec_perm,
				      stmt_info, 0, vect_body);

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "vect_model_store_cost: strided group_size = %d .",
                         group_size);
    }

  /* Costs of the stores.  */
  vect_get_store_cost (first_dr, ncopies, &inside_cost, body_cost_vec);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_store_cost: inside_cost = %d, "
                     "prologue_cost = %d .", inside_cost, prologue_cost);
}


/* Calculate cost of DR's memory access.  */
void
vect_get_store_cost (struct data_reference *dr, int ncopies,
		     unsigned int *inside_cost,
		     stmt_vector_for_cost *body_cost_vec)
{
  int alignment_support_scheme = vect_supportable_dr_alignment (dr, false);
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

  switch (alignment_support_scheme)
    {
    case dr_aligned:
      {
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies,
					  vector_store, stmt_info, 0,
					  vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_store_cost: aligned.");
        break;
      }

    case dr_unaligned_supported:
      {
        /* Here, we assign an additional cost for the unaligned store.  */
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies,
					  unaligned_store, stmt_info,
					  DR_MISALIGNMENT (dr), vect_body);
        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_store_cost: unaligned supported by "
                           "hardware.");
        break;
      }

    case dr_unaligned_unsupported:
      {
        *inside_cost = VECT_MAX_COST;

        if (dump_enabled_p ())
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "vect_model_store_cost: unsupported access.");
        break;
      }

    default:
      gcc_unreachable ();
    }
}


/* Function vect_model_load_cost

   Models cost for loads.  In the case of grouped accesses, the last access
   has the overhead of the grouped access attributed to it.  Since unaligned
   accesses are supported for loads, we also account for the costs of the
   access scheme chosen.  */

void
vect_model_load_cost (stmt_vec_info stmt_info, int ncopies,
		      bool load_lanes_p, slp_tree slp_node,
		      stmt_vector_for_cost *prologue_cost_vec,
		      stmt_vector_for_cost *body_cost_vec)
{
  int group_size;
  gimple first_stmt;
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info), *first_dr;
  unsigned int inside_cost = 0, prologue_cost = 0;

  /* The SLP costs were already calculated during SLP tree build.  */
  if (PURE_SLP_STMT (stmt_info))
    return;

  /* Grouped accesses?  */
  first_stmt = GROUP_FIRST_ELEMENT (stmt_info);
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info) && first_stmt && !slp_node)
    {
      group_size = vect_cost_group_size (stmt_info);
      first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt));
    }
  /* Not a grouped access.  */
  else
    {
      group_size = 1;
      first_dr = dr;
    }

  /* We assume that the cost of a single load-lanes instruction is
     equivalent to the cost of GROUP_SIZE separate loads.  If a grouped
     access is instead being provided by a load-and-permute operation,
     include the cost of the permutes.  */
  if (!load_lanes_p && group_size > 1)
    {
      /* Uses an even and odd extract operations for each needed permute.  */
      int nstmts = ncopies * exact_log2 (group_size) * group_size;
      inside_cost += record_stmt_cost (body_cost_vec, nstmts, vec_perm,
				       stmt_info, 0, vect_body);

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location, 
                         "vect_model_load_cost: strided group_size = %d .",
                         group_size);
    }

  /* The loads themselves.  */
  if (STMT_VINFO_STRIDE_LOAD_P (stmt_info))
    {
      /* N scalar loads plus gathering them into a vector.  */
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      inside_cost += record_stmt_cost (body_cost_vec,
				       ncopies * TYPE_VECTOR_SUBPARTS (vectype),
				       scalar_load, stmt_info, 0, vect_body);
      inside_cost += record_stmt_cost (body_cost_vec, ncopies, vec_construct,
				       stmt_info, 0, vect_body);
    }
  else
    vect_get_load_cost (first_dr, ncopies,
			((!STMT_VINFO_GROUPED_ACCESS (stmt_info))
			 || group_size > 1 || slp_node),
			&inside_cost, &prologue_cost, 
			prologue_cost_vec, body_cost_vec, true);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_model_load_cost: inside_cost = %d, "
                     "prologue_cost = %d .", inside_cost, prologue_cost);
}


/* Calculate cost of DR's memory access.  */
void
vect_get_load_cost (struct data_reference *dr, int ncopies,
		    bool add_realign_cost, unsigned int *inside_cost,
		    unsigned int *prologue_cost,
		    stmt_vector_for_cost *prologue_cost_vec,
		    stmt_vector_for_cost *body_cost_vec,
		    bool record_prologue_costs)
{
  int alignment_support_scheme = vect_supportable_dr_alignment (dr, false);
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

  switch (alignment_support_scheme)
    {
    case dr_aligned:
      {
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies, vector_load,
					  stmt_info, 0, vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_load_cost: aligned.");

        break;
      }
    case dr_unaligned_supported:
      {
        /* Here, we assign an additional cost for the unaligned load.  */
	*inside_cost += record_stmt_cost (body_cost_vec, ncopies,
					  unaligned_load, stmt_info,
					  DR_MISALIGNMENT (dr), vect_body);

        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "vect_model_load_cost: unaligned supported by "
                           "hardware.");

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
                           "vect_model_load_cost: explicit realign");

        break;
      }
    case dr_explicit_realign_optimized:
      {
        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location, 
                           "vect_model_load_cost: unaligned software "
                           "pipelined.");

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
                           "vect_model_load_cost: explicit realign optimized");

        break;
      }

    case dr_unaligned_unsupported:
      {
        *inside_cost = VECT_MAX_COST;

        if (dump_enabled_p ())
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "vect_model_load_cost: unsupported access.");
        break;
      }

    default:
      gcc_unreachable ();
    }
}

/* Insert the new stmt NEW_STMT at *GSI or at the appropriate place in
   the loop preheader for the vectorized stmt STMT.  */

static void
vect_init_vector_1 (gimple stmt, gimple new_stmt, gimple_stmt_iterator *gsi)
{
  if (gsi)
    vect_finish_stmt_generation (stmt, new_stmt, gsi);
  else
    {
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
      loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);

      if (loop_vinfo)
        {
          struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
	  basic_block new_bb;
	  edge pe;

          if (nested_in_vect_loop_p (loop, stmt))
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
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "created new init_stmt: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, new_stmt, 0);
    }
}

/* Function vect_init_vector.

   Insert a new stmt (INIT_STMT) that initializes a new variable of type
   TYPE with the value VAL.  If TYPE is a vector type and VAL does not have
   vector type a vector with all elements equal to VAL is created first.
   Place the initialization at BSI if it is not NULL.  Otherwise, place the
   initialization at the loop preheader.
   Return the DEF of INIT_STMT.
   It will be used in the vectorization of STMT.  */

tree
vect_init_vector (gimple stmt, tree val, tree type, gimple_stmt_iterator *gsi)
{
  tree new_var;
  gimple init_stmt;
  tree vec_oprnd;
  tree new_temp;

  if (TREE_CODE (type) == VECTOR_TYPE
      && TREE_CODE (TREE_TYPE (val)) != VECTOR_TYPE)
    {
      if (!types_compatible_p (TREE_TYPE (type), TREE_TYPE (val)))
	{
	  if (CONSTANT_CLASS_P (val))
	    val = fold_unary (VIEW_CONVERT_EXPR, TREE_TYPE (type), val);
	  else
	    {
	      new_temp = make_ssa_name (TREE_TYPE (type), NULL);
	      init_stmt = gimple_build_assign_with_ops (NOP_EXPR,
							new_temp, val,
							NULL_TREE);
	      vect_init_vector_1 (stmt, init_stmt, gsi);
	      val = new_temp;
	    }
	}
      val = build_vector_from_val (type, val);
    }

  new_var = vect_get_new_vect_var (type, vect_simple_var, "cst_");
  init_stmt = gimple_build_assign  (new_var, val);
  new_temp = make_ssa_name (new_var, init_stmt);
  gimple_assign_set_lhs (init_stmt, new_temp);
  vect_init_vector_1 (stmt, init_stmt, gsi);
  vec_oprnd = gimple_assign_lhs (init_stmt);
  return vec_oprnd;
}


/* Function vect_get_vec_def_for_operand.

   OP is an operand in STMT.  This function returns a (vector) def that will be
   used in the vectorized stmt for STMT.

   In the case that OP is an SSA_NAME which is defined in the loop, then
   STMT_VINFO_VEC_STMT of the defining stmt holds the relevant def.

   In case OP is an invariant or constant, a new stmt that creates a vector def
   needs to be introduced.  */

tree
vect_get_vec_def_for_operand (tree op, gimple stmt, tree *scalar_def)
{
  tree vec_oprnd;
  gimple vec_stmt;
  gimple def_stmt;
  stmt_vec_info def_stmt_info = NULL;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  unsigned int nunits;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  tree def;
  enum vect_def_type dt;
  bool is_simple_use;
  tree vector_type;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "vect_get_vec_def_for_operand: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, op);
    }

  is_simple_use = vect_is_simple_use (op, stmt, loop_vinfo, NULL,
				      &def_stmt, &def, &dt);
  gcc_assert (is_simple_use);
  if (dump_enabled_p ())
    {
      int loc_printed = 0;
      if (def)
        {
          dump_printf_loc (MSG_NOTE, vect_location, "def =  ");
          loc_printed = 1;
          dump_generic_expr (MSG_NOTE, TDF_SLIM, def);
        }
      if (def_stmt)
        {
          if (loc_printed)
            dump_printf (MSG_NOTE, "  def_stmt =  ");
          else
            dump_printf_loc (MSG_NOTE, vect_location, "  def_stmt =  ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, def_stmt, 0);
        }
    }

  switch (dt)
    {
    /* Case 1: operand is a constant.  */
    case vect_constant_def:
      {
	vector_type = get_vectype_for_scalar_type (TREE_TYPE (op));
	gcc_assert (vector_type);
	nunits = TYPE_VECTOR_SUBPARTS (vector_type);

	if (scalar_def)
	  *scalar_def = op;

        /* Create 'vect_cst_ = {cst,cst,...,cst}'  */
        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location,
                           "Create vector_cst. nunits = %d", nunits);

        return vect_init_vector (stmt, op, vector_type, NULL);
      }

    /* Case 2: operand is defined outside the loop - loop invariant.  */
    case vect_external_def:
      {
	vector_type = get_vectype_for_scalar_type (TREE_TYPE (def));
	gcc_assert (vector_type);

	if (scalar_def)
	  *scalar_def = def;

        /* Create 'vec_inv = {inv,inv,..,inv}'  */
        if (dump_enabled_p ())
          dump_printf_loc (MSG_NOTE, vect_location, "Create vector_inv.");

        return vect_init_vector (stmt, def, vector_type, NULL);
      }

    /* Case 3: operand is defined inside the loop.  */
    case vect_internal_def:
      {
	if (scalar_def)
	  *scalar_def = NULL/* FIXME tuples: def_stmt*/;

        /* Get the def from the vectorized stmt.  */
        def_stmt_info = vinfo_for_stmt (def_stmt);

        vec_stmt = STMT_VINFO_VEC_STMT (def_stmt_info);
        /* Get vectorized pattern statement.  */
        if (!vec_stmt
            && STMT_VINFO_IN_PATTERN_P (def_stmt_info)
            && !STMT_VINFO_RELEVANT (def_stmt_info))
          vec_stmt = STMT_VINFO_VEC_STMT (vinfo_for_stmt (
                       STMT_VINFO_RELATED_STMT (def_stmt_info)));
        gcc_assert (vec_stmt);
	if (gimple_code (vec_stmt) == GIMPLE_PHI)
	  vec_oprnd = PHI_RESULT (vec_stmt);
	else if (is_gimple_call (vec_stmt))
	  vec_oprnd = gimple_call_lhs (vec_stmt);
	else
	  vec_oprnd = gimple_assign_lhs (vec_stmt);
        return vec_oprnd;
      }

    /* Case 4: operand is defined by a loop header phi - reduction  */
    case vect_reduction_def:
    case vect_double_reduction_def:
    case vect_nested_cycle:
      {
	struct loop *loop;

	gcc_assert (gimple_code (def_stmt) == GIMPLE_PHI);
	loop = (gimple_bb (def_stmt))->loop_father;

        /* Get the def before the loop  */
        op = PHI_ARG_DEF_FROM_EDGE (def_stmt, loop_preheader_edge (loop));
        return get_initial_def_for_reduction (stmt, op, scalar_def);
     }

    /* Case 5: operand is defined by loop-header phi - induction.  */
    case vect_induction_def:
      {
	gcc_assert (gimple_code (def_stmt) == GIMPLE_PHI);

        /* Get the def from the vectorized stmt.  */
        def_stmt_info = vinfo_for_stmt (def_stmt);
        vec_stmt = STMT_VINFO_VEC_STMT (def_stmt_info);
	if (gimple_code (vec_stmt) == GIMPLE_PHI)
	  vec_oprnd = PHI_RESULT (vec_stmt);
	else
	  vec_oprnd = gimple_get_lhs (vec_stmt);
        return vec_oprnd;
      }

    default:
      gcc_unreachable ();
    }
}


/* Function vect_get_vec_def_for_stmt_copy

   Return a vector-def for an operand.  This function is used when the
   vectorized stmt to be created (by the caller to this function) is a "copy"
   created in case the vectorized result cannot fit in one vector, and several
   copies of the vector-stmt are required.  In this case the vector-def is
   retrieved from the vector stmt recorded in the STMT_VINFO_RELATED_STMT field
   of the stmt that defines VEC_OPRND.
   DT is the type of the vector def VEC_OPRND.

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
        vx.1 = vect_get_vec_def_for_stmt_copy (dt, vx.0);
        vx.2 = vect_get_vec_def_for_stmt_copy (dt, vx.1);
        vx.3 = vect_get_vec_def_for_stmt_copy (dt, vx.2);  */

tree
vect_get_vec_def_for_stmt_copy (enum vect_def_type dt, tree vec_oprnd)
{
  gimple vec_stmt_for_operand;
  stmt_vec_info def_stmt_info;

  /* Do nothing; can reuse same def.  */
  if (dt == vect_external_def || dt == vect_constant_def )
    return vec_oprnd;

  vec_stmt_for_operand = SSA_NAME_DEF_STMT (vec_oprnd);
  def_stmt_info = vinfo_for_stmt (vec_stmt_for_operand);
  gcc_assert (def_stmt_info);
  vec_stmt_for_operand = STMT_VINFO_RELATED_STMT (def_stmt_info);
  gcc_assert (vec_stmt_for_operand);
  vec_oprnd = gimple_get_lhs (vec_stmt_for_operand);
  if (gimple_code (vec_stmt_for_operand) == GIMPLE_PHI)
    vec_oprnd = PHI_RESULT (vec_stmt_for_operand);
  else
    vec_oprnd = gimple_get_lhs (vec_stmt_for_operand);
  return vec_oprnd;
}


/* Get vectorized definitions for the operands to create a copy of an original
   stmt.  See vect_get_vec_def_for_stmt_copy () for details.  */

static void
vect_get_vec_defs_for_stmt_copy (enum vect_def_type *dt,
				 vec<tree> *vec_oprnds0,
				 vec<tree> *vec_oprnds1)
{
  tree vec_oprnd = vec_oprnds0->pop ();

  vec_oprnd = vect_get_vec_def_for_stmt_copy (dt[0], vec_oprnd);
  vec_oprnds0->quick_push (vec_oprnd);

  if (vec_oprnds1 && vec_oprnds1->length ())
    {
      vec_oprnd = vec_oprnds1->pop ();
      vec_oprnd = vect_get_vec_def_for_stmt_copy (dt[1], vec_oprnd);
      vec_oprnds1->quick_push (vec_oprnd);
    }
}


/* Get vectorized definitions for OP0 and OP1.
   REDUC_INDEX is the index of reduction operand in case of reduction,
   and -1 otherwise.  */

void
vect_get_vec_defs (tree op0, tree op1, gimple stmt,
		   vec<tree> *vec_oprnds0,
		   vec<tree> *vec_oprnds1,
		   slp_tree slp_node, int reduc_index)
{
  if (slp_node)
    {
      int nops = (op1 == NULL_TREE) ? 1 : 2;
      vec<tree> ops;
      ops.create (nops);
      vec<vec<tree> > vec_defs;
      vec_defs.create (nops);

      ops.quick_push (op0);
      if (op1)
        ops.quick_push (op1);

      vect_get_slp_defs (ops, slp_node, &vec_defs, reduc_index);

      *vec_oprnds0 = vec_defs[0];
      if (op1)
	*vec_oprnds1 = vec_defs[1];

      ops.release ();
      vec_defs.release ();
    }
  else
    {
      tree vec_oprnd;

      vec_oprnds0->create (1);
      vec_oprnd = vect_get_vec_def_for_operand (op0, stmt, NULL);
      vec_oprnds0->quick_push (vec_oprnd);

      if (op1)
	{
	  vec_oprnds1->create (1);
	  vec_oprnd = vect_get_vec_def_for_operand (op1, stmt, NULL);
	  vec_oprnds1->quick_push (vec_oprnd);
	}
    }
}


/* Function vect_finish_stmt_generation.

   Insert a new stmt.  */

void
vect_finish_stmt_generation (gimple stmt, gimple vec_stmt,
			     gimple_stmt_iterator *gsi)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);

  gcc_assert (gimple_code (stmt) != GIMPLE_LABEL);

  if (!gsi_end_p (*gsi)
      && gimple_has_mem_ops (vec_stmt))
    {
      gimple at_stmt = gsi_stmt (*gsi);
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

  set_vinfo_for_stmt (vec_stmt, new_stmt_vec_info (vec_stmt, loop_vinfo,
                                                   bb_vinfo));

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "add new stmt: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, vec_stmt, 0);
    }

  gimple_set_location (vec_stmt, gimple_location (stmt));
}

/* Checks if CALL can be vectorized in type VECTYPE.  Returns
   a function declaration if the target has a vectorized version
   of the function, or NULL_TREE if the function cannot be vectorized.  */

tree
vectorizable_function (gimple call, tree vectype_out, tree vectype_in)
{
  tree fndecl = gimple_call_fndecl (call);

  /* We only handle functions that do not read or clobber memory -- i.e.
     const or novops ones.  */
  if (!(gimple_call_flags (call) & (ECF_CONST | ECF_NOVOPS)))
    return NULL_TREE;

  if (!fndecl
      || TREE_CODE (fndecl) != FUNCTION_DECL
      || !DECL_BUILT_IN (fndecl))
    return NULL_TREE;

  return targetm.vectorize.builtin_vectorized_function (fndecl, vectype_out,
						        vectype_in);
}

/* Function vectorizable_call.

   Check if STMT performs a function call that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_call (gimple stmt, gimple_stmt_iterator *gsi, gimple *vec_stmt,
		   slp_tree slp_node)
{
  tree vec_dest;
  tree scalar_dest;
  tree op, type;
  tree vec_oprnd0 = NULL_TREE, vec_oprnd1 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt), prev_stmt_info;
  tree vectype_out, vectype_in;
  int nunits_in;
  int nunits_out;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  tree fndecl, new_temp, def, rhs_type;
  gimple def_stmt;
  enum vect_def_type dt[3]
    = {vect_unknown_def_type, vect_unknown_def_type, vect_unknown_def_type};
  gimple new_stmt = NULL;
  int ncopies, j;
  vec<tree> vargs = vNULL;
  enum { NARROW, NONE, WIDEN } modifier;
  size_t i, nargs;
  tree lhs;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  /* Is STMT a vectorizable call?   */
  if (!is_gimple_call (stmt))
    return false;

  if (TREE_CODE (gimple_call_lhs (stmt)) != SSA_NAME)
    return false;

  if (stmt_can_throw_internal (stmt))
    return false;

  vectype_out = STMT_VINFO_VECTYPE (stmt_info);

  /* Process function arguments.  */
  rhs_type = NULL_TREE;
  vectype_in = NULL_TREE;
  nargs = gimple_call_num_args (stmt);

  /* Bail out if the function has more than three arguments, we do not have
     interesting builtin functions to vectorize with more than two arguments
     except for fma.  No arguments is also not good.  */
  if (nargs == 0 || nargs > 3)
    return false;

  /* Ignore the argument of IFN_GOMP_SIMD_LANE, it is magic.  */
  if (gimple_call_internal_p (stmt)
      && gimple_call_internal_fn (stmt) == IFN_GOMP_SIMD_LANE)
    {
      nargs = 0;
      rhs_type = unsigned_type_node;
    }

  for (i = 0; i < nargs; i++)
    {
      tree opvectype;

      op = gimple_call_arg (stmt, i);

      /* We can only handle calls with arguments of the same type.  */
      if (rhs_type
	  && !types_compatible_p (rhs_type, TREE_TYPE (op)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "argument types differ.");
	  return false;
	}
      if (!rhs_type)
	rhs_type = TREE_TYPE (op);

      if (!vect_is_simple_use_1 (op, stmt, loop_vinfo, bb_vinfo,
				 &def_stmt, &def, &dt[i], &opvectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.");
	  return false;
	}

      if (!vectype_in)
	vectype_in = opvectype;
      else if (opvectype
	       && opvectype != vectype_in)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "argument vector types differ.");
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
        {
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "no vectype for scalar type ");
          dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, rhs_type);
        }

      return false;
    }

  /* FORNOW */
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype_in);
  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  if (nunits_in == nunits_out / 2)
    modifier = NARROW;
  else if (nunits_out == nunits_in)
    modifier = NONE;
  else if (nunits_out == nunits_in / 2)
    modifier = WIDEN;
  else
    return false;

  /* For now, we only vectorize functions if a target specific builtin
     is available.  TODO -- in some cases, it might be profitable to
     insert the calls for pieces of the vector, in order to be able
     to vectorize other operations in the loop.  */
  fndecl = vectorizable_function (stmt, vectype_out, vectype_in);
  if (fndecl == NULL_TREE)
    {
      if (gimple_call_internal_p (stmt)
	  && gimple_call_internal_fn (stmt) == IFN_GOMP_SIMD_LANE
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
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "function is not vectorizable.");
	  return false;
	}
    }

  gcc_assert (!gimple_vuse (stmt));

  if (slp_node || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else if (modifier == NARROW)
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_out;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_in;

  /* Sanity check: make sure that at least one copy of the vectorized stmt
     needs to be generated.  */
  gcc_assert (ncopies >= 1);

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = call_vec_info_type;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location, "=== vectorizable_call ===");
      vect_model_simple_cost (stmt_info, ncopies, dt, NULL, NULL);
      return true;
    }

  /** Transform.  **/

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform call.");

  /* Handle def.  */
  scalar_dest = gimple_call_lhs (stmt);
  vec_dest = vect_create_destination_var (scalar_dest, vectype_out);

  prev_stmt_info = NULL;
  switch (modifier)
    {
    case NONE:
      for (j = 0; j < ncopies; ++j)
	{
	  /* Build argument list for the vectorized call.  */
	  if (j == 0)
	    vargs.create (nargs);
	  else
	    vargs.truncate (0);

	  if (slp_node)
	    {
	      vec<vec<tree> > vec_defs;
	      vec_defs.create (nargs);
	      vec<tree> vec_oprnds0;

	      for (i = 0; i < nargs; i++)
		vargs.quick_push (gimple_call_arg (stmt, i));
	      vect_get_slp_defs (vargs, slp_node, &vec_defs, -1);
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
		  new_stmt = gimple_build_call_vec (fndecl, vargs);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_call_set_lhs (new_stmt, new_temp);
		  vect_finish_stmt_generation (stmt, new_stmt, gsi);
		  SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
		}

	      for (i = 0; i < nargs; i++)
		{
		  vec<tree> vec_oprndsi = vec_defs[i];
		  vec_oprndsi.release ();
		}
	      vec_defs.release ();
	      continue;
	    }

	  for (i = 0; i < nargs; i++)
	    {
	      op = gimple_call_arg (stmt, i);
	      if (j == 0)
		vec_oprnd0
		  = vect_get_vec_def_for_operand (op, stmt, NULL);
	      else
		{
		  vec_oprnd0 = gimple_call_arg (new_stmt, i);
		  vec_oprnd0
                    = vect_get_vec_def_for_stmt_copy (dt[i], vec_oprnd0);
		}

	      vargs.quick_push (vec_oprnd0);
	    }

	  if (gimple_call_internal_p (stmt)
	      && gimple_call_internal_fn (stmt) == IFN_GOMP_SIMD_LANE)
	    {
	      tree *v = XALLOCAVEC (tree, nunits_out);
	      int k;
	      for (k = 0; k < nunits_out; ++k)
		v[k] = build_int_cst (unsigned_type_node, j * nunits_out + k);
	      tree cst = build_vector (vectype_out, v);
	      tree new_var
		= vect_get_new_vect_var (vectype_out, vect_simple_var, "cst_");
	      gimple init_stmt = gimple_build_assign (new_var, cst);
	      new_temp = make_ssa_name (new_var, init_stmt);
	      gimple_assign_set_lhs (init_stmt, new_temp);
	      vect_init_vector_1 (stmt, init_stmt, NULL);
	      new_temp = make_ssa_name (vec_dest, NULL);
	      new_stmt = gimple_build_assign (new_temp,
					      gimple_assign_lhs (init_stmt));
	    }
	  else
	    {
	      new_stmt = gimple_build_call_vec (fndecl, vargs);
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      gimple_call_set_lhs (new_stmt, new_temp);
	    }
	  vect_finish_stmt_generation (stmt, new_stmt, gsi);

	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;

	  prev_stmt_info = vinfo_for_stmt (new_stmt);
	}

      break;

    case NARROW:
      for (j = 0; j < ncopies; ++j)
	{
	  /* Build argument list for the vectorized call.  */
	  if (j == 0)
	    vargs.create (nargs * 2);
	  else
	    vargs.truncate (0);

	  if (slp_node)
	    {
	      vec<vec<tree> > vec_defs;
	      vec_defs.create (nargs);
	      vec<tree> vec_oprnds0;

	      for (i = 0; i < nargs; i++)
		vargs.quick_push (gimple_call_arg (stmt, i));
	      vect_get_slp_defs (vargs, slp_node, &vec_defs, -1);
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
		  new_stmt = gimple_build_call_vec (fndecl, vargs);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_call_set_lhs (new_stmt, new_temp);
		  vect_finish_stmt_generation (stmt, new_stmt, gsi);
		  SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
		}

	      for (i = 0; i < nargs; i++)
		{
		  vec<tree> vec_oprndsi = vec_defs[i];
		  vec_oprndsi.release ();
		}
	      vec_defs.release ();
	      continue;
	    }

	  for (i = 0; i < nargs; i++)
	    {
	      op = gimple_call_arg (stmt, i);
	      if (j == 0)
		{
		  vec_oprnd0
		    = vect_get_vec_def_for_operand (op, stmt, NULL);
		  vec_oprnd1
		    = vect_get_vec_def_for_stmt_copy (dt[i], vec_oprnd0);
		}
	      else
		{
		  vec_oprnd1 = gimple_call_arg (new_stmt, 2*i + 1);
		  vec_oprnd0
		    = vect_get_vec_def_for_stmt_copy (dt[i], vec_oprnd1);
		  vec_oprnd1
		    = vect_get_vec_def_for_stmt_copy (dt[i], vec_oprnd0);
		}

	      vargs.quick_push (vec_oprnd0);
	      vargs.quick_push (vec_oprnd1);
	    }

	  new_stmt = gimple_build_call_vec (fndecl, vargs);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  gimple_call_set_lhs (new_stmt, new_temp);
	  vect_finish_stmt_generation (stmt, new_stmt, gsi);

	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = new_stmt;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;

	  prev_stmt_info = vinfo_for_stmt (new_stmt);
	}

      *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);

      break;

    case WIDEN:
      /* No current target implements this case.  */
      return false;
    }

  vargs.release ();

  /* Update the exception handling table with the vector stmt if necessary.  */
  if (maybe_clean_or_replace_eh_stmt (stmt, *vec_stmt))
    gimple_purge_dead_eh_edges (gimple_bb (stmt));

  /* The call in STMT might prevent it from being removed in dce.
     We however cannot remove it here, due to the way the ssa name
     it defines is mapped to the new definition.  So just replace
     rhs of the statement with something harmless.  */

  if (slp_node)
    return true;

  type = TREE_TYPE (scalar_dest);
  if (is_pattern_stmt_p (stmt_info))
    lhs = gimple_call_lhs (STMT_VINFO_RELATED_STMT (stmt_info));
  else
    lhs = gimple_call_lhs (stmt);
  new_stmt = gimple_build_assign (lhs, build_zero_cst (type));
  set_vinfo_for_stmt (new_stmt, stmt_info);
  set_vinfo_for_stmt (stmt, NULL);
  STMT_VINFO_STMT (stmt_info) = new_stmt;
  gsi_replace (gsi, new_stmt, false);
  SSA_NAME_DEF_STMT (gimple_assign_lhs (new_stmt)) = new_stmt;

  return true;
}


/* Function vect_gen_widened_results_half

   Create a vector stmt whose code, type, number of arguments, and result
   variable are CODE, OP_TYPE, and VEC_DEST, and its arguments are
   VEC_OPRND0 and VEC_OPRND1.  The new vector stmt is to be inserted at BSI.
   In the case that CODE is a CALL_EXPR, this means that a call to DECL
   needs to be created (DECL is a function-decl of a target-builtin).
   STMT is the original scalar stmt that we are vectorizing.  */

static gimple
vect_gen_widened_results_half (enum tree_code code,
			       tree decl,
                               tree vec_oprnd0, tree vec_oprnd1, int op_type,
			       tree vec_dest, gimple_stmt_iterator *gsi,
			       gimple stmt)
{
  gimple new_stmt;
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
      new_stmt = gimple_build_assign_with_ops (code, vec_dest, vec_oprnd0,
					       vec_oprnd1);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
    }
  vect_finish_stmt_generation (stmt, new_stmt, gsi);

  return new_stmt;
}


/* Get vectorized definitions for loop-based vectorization.  For the first
   operand we call vect_get_vec_def_for_operand() (with OPRND containing
   scalar operand), and for the rest we get a copy with
   vect_get_vec_def_for_stmt_copy() using the previous vector definition
   (stored in OPRND). See vect_get_vec_def_for_stmt_copy() for details.
   The vectors are collected into VEC_OPRNDS.  */

static void
vect_get_loop_based_defs (tree *oprnd, gimple stmt, enum vect_def_type dt,
			  vec<tree> *vec_oprnds, int multi_step_cvt)
{
  tree vec_oprnd;

  /* Get first vector operand.  */
  /* All the vector operands except the very first one (that is scalar oprnd)
     are stmt copies.  */
  if (TREE_CODE (TREE_TYPE (*oprnd)) != VECTOR_TYPE)
    vec_oprnd = vect_get_vec_def_for_operand (*oprnd, stmt, NULL);
  else
    vec_oprnd = vect_get_vec_def_for_stmt_copy (dt, *oprnd);

  vec_oprnds->quick_push (vec_oprnd);

  /* Get second vector operand.  */
  vec_oprnd = vect_get_vec_def_for_stmt_copy (dt, vec_oprnd);
  vec_oprnds->quick_push (vec_oprnd);

  *oprnd = vec_oprnd;

  /* For conversion in multiple steps, continue to get operands
     recursively.  */
  if (multi_step_cvt)
    vect_get_loop_based_defs (oprnd, stmt, dt, vec_oprnds,  multi_step_cvt - 1);
}


/* Create vectorized demotion statements for vector operands from VEC_OPRNDS.
   For multi-step conversions store the resulting vectors and call the function
   recursively.  */

static void
vect_create_vectorized_demotion_stmts (vec<tree> *vec_oprnds,
				       int multi_step_cvt, gimple stmt,
				       vec<tree> vec_dsts,
				       gimple_stmt_iterator *gsi,
				       slp_tree slp_node, enum tree_code code,
				       stmt_vec_info *prev_stmt_info)
{
  unsigned int i;
  tree vop0, vop1, new_tmp, vec_dest;
  gimple new_stmt;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

  vec_dest = vec_dsts.pop ();

  for (i = 0; i < vec_oprnds->length (); i += 2)
    {
      /* Create demotion operation.  */
      vop0 = (*vec_oprnds)[i];
      vop1 = (*vec_oprnds)[i + 1];
      new_stmt = gimple_build_assign_with_ops (code, vec_dest, vop0, vop1);
      new_tmp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_tmp);
      vect_finish_stmt_generation (stmt, new_stmt, gsi);

      if (multi_step_cvt)
	/* Store the resulting vector for next recursive call.  */
	(*vec_oprnds)[i/2] = new_tmp;
      else
	{
	  /* This is the last step of the conversion sequence. Store the
	     vectors in SLP_NODE or in vector info of the scalar statement
	     (or in STMT_VINFO_RELATED_STMT chain).  */
	  if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
	  else
	    {
	      if (!*prev_stmt_info)
		STMT_VINFO_VEC_STMT (stmt_info) = new_stmt;
	      else
		STMT_VINFO_RELATED_STMT (*prev_stmt_info) = new_stmt;

	      *prev_stmt_info = vinfo_for_stmt (new_stmt);
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
					     stmt, vec_dsts, gsi, slp_node,
					     VEC_PACK_TRUNC_EXPR,
					     prev_stmt_info);
    }

  vec_dsts.quick_push (vec_dest);
}


/* Create vectorized promotion statements for vector operands from VEC_OPRNDS0
   and VEC_OPRNDS1 (for binary operations).  For multi-step conversions store
   the resulting vectors and call the function recursively.  */

static void
vect_create_vectorized_promotion_stmts (vec<tree> *vec_oprnds0,
					vec<tree> *vec_oprnds1,
					gimple stmt, tree vec_dest,
					gimple_stmt_iterator *gsi,
					enum tree_code code1,
					enum tree_code code2, tree decl1,
					tree decl2, int op_type)
{
  int i;
  tree vop0, vop1, new_tmp1, new_tmp2;
  gimple new_stmt1, new_stmt2;
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
						 op_type, vec_dest, gsi, stmt);
      new_stmt2 = vect_gen_widened_results_half (code2, decl2, vop0, vop1,
						 op_type, vec_dest, gsi, stmt);
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


/* Check if STMT performs a conversion operation, that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at GSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_conversion (gimple stmt, gimple_stmt_iterator *gsi,
			 gimple *vec_stmt, slp_tree slp_node)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL_TREE;
  tree vec_oprnd0 = NULL_TREE, vec_oprnd1 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code, code1 = ERROR_MARK, code2 = ERROR_MARK;
  enum tree_code codecvt1 = ERROR_MARK, codecvt2 = ERROR_MARK;
  tree decl1 = NULL_TREE, decl2 = NULL_TREE;
  tree new_temp;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  gimple new_stmt = NULL;
  stmt_vec_info prev_stmt_info;
  int nunits_in;
  int nunits_out;
  tree vectype_out, vectype_in;
  int ncopies, i, j;
  tree lhs_type, rhs_type;
  enum { NARROW, NONE, WIDEN } modifier;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  tree vop0;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  int multi_step_cvt = 0;
  vec<tree> vec_dsts = vNULL;
  vec<tree> interm_types = vNULL;
  tree last_oprnd, intermediate_type, cvt_type = NULL_TREE;
  int op_type;
  enum machine_mode rhs_mode;
  unsigned short fltsz;

  /* Is STMT a vectorizable conversion?   */

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  if (!is_gimple_assign (stmt))
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

  if ((INTEGRAL_TYPE_P (lhs_type)
       && (TYPE_PRECISION (lhs_type)
	   != GET_MODE_PRECISION (TYPE_MODE (lhs_type))))
      || (INTEGRAL_TYPE_P (rhs_type)
	  && (TYPE_PRECISION (rhs_type)
	      != GET_MODE_PRECISION (TYPE_MODE (rhs_type)))))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "type conversion to/from bit-precision unsupported.");
      return false;
    }

  /* Check the operands of the operation.  */
  if (!vect_is_simple_use_1 (op0, stmt, loop_vinfo, bb_vinfo,
			     &def_stmt, &def, &dt[0], &vectype_in))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.");
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
	ok = vect_is_simple_use_1 (op1, stmt, loop_vinfo, bb_vinfo,
				   &def_stmt, &def, &dt[1], &vectype_in);
      else
	ok = vect_is_simple_use (op1, stmt, loop_vinfo, bb_vinfo, &def_stmt,
				 &def, &dt[1]);

      if (!ok)
	{
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.");
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
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "no vectype for scalar type ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, rhs_type);
	}

      return false;
    }

  nunits_in = TYPE_VECTOR_SUBPARTS (vectype_in);
  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  if (nunits_in < nunits_out)
    modifier = NARROW;
  else if (nunits_out == nunits_in)
    modifier = NONE;
  else
    modifier = WIDEN;

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else if (modifier == NARROW)
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_out;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_in;

  /* Sanity check: make sure that at least one copy of the vectorized stmt
     needs to be generated.  */
  gcc_assert (ncopies >= 1);

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
                         "conversion not supported by target.");
      return false;

    case WIDEN:
      if (supportable_widening_operation (code, stmt, vectype_out, vectype_in,
					  &code1, &code2, &multi_step_cvt,
					  &interm_types))
	{
	  /* Binary widening operation can only be supported directly by the
	     architecture.  */
	  gcc_assert (!(multi_step_cvt && op_type == binary_op));
	  break;
	}

      if (code != FLOAT_EXPR
	  || (GET_MODE_SIZE (TYPE_MODE (lhs_type))
	      <= GET_MODE_SIZE (TYPE_MODE (rhs_type))))
	goto unsupported;

      rhs_mode = TYPE_MODE (rhs_type);
      fltsz = GET_MODE_SIZE (TYPE_MODE (lhs_type));
      for (rhs_mode = GET_MODE_2XWIDER_MODE (TYPE_MODE (rhs_type));
	   rhs_mode != VOIDmode && GET_MODE_SIZE (rhs_mode) <= fltsz;
	   rhs_mode = GET_MODE_2XWIDER_MODE (rhs_mode))
	{
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
	  else if (!supportable_widening_operation (code, stmt, vectype_out,
						    cvt_type, &codecvt1,
						    &codecvt2, &multi_step_cvt,
						    &interm_types))
	    continue;
	  else
	    gcc_assert (multi_step_cvt == 0);

	  if (supportable_widening_operation (NOP_EXPR, stmt, cvt_type,
					      vectype_in, &code1, &code2,
					      &multi_step_cvt, &interm_types))
	    break;
	}

      if (rhs_mode == VOIDmode || GET_MODE_SIZE (rhs_mode) > fltsz)
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
	  || (GET_MODE_SIZE (TYPE_MODE (lhs_type))
	      >= GET_MODE_SIZE (TYPE_MODE (rhs_type))))
	goto unsupported;

      rhs_mode = TYPE_MODE (rhs_type);
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
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "=== vectorizable_conversion ===");
      if (code == FIX_TRUNC_EXPR || code == FLOAT_EXPR)
        {
	  STMT_VINFO_TYPE (stmt_info) = type_conversion_vec_info_type;
	  vect_model_simple_cost (stmt_info, ncopies, dt, NULL, NULL);
	}
      else if (modifier == NARROW)
	{
	  STMT_VINFO_TYPE (stmt_info) = type_demotion_vec_info_type;
	  vect_model_promotion_demotion_cost (stmt_info, dt, multi_step_cvt);
	}
      else
	{
	  STMT_VINFO_TYPE (stmt_info) = type_promotion_vec_info_type;
	  vect_model_promotion_demotion_cost (stmt_info, dt, multi_step_cvt);
	}
      interm_types.release ();
      return true;
    }

  /** Transform.  **/
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform conversion. ncopies = %d.", ncopies);

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
  vec_dsts.create (multi_step_cvt + 1);
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
	  vec_oprnds0.create (multi_step_cvt ? vect_pow2(multi_step_cvt) : 1);
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
	    vect_get_vec_defs (op0, NULL, stmt, &vec_oprnds0, NULL, slp_node,
			       -1);
	  else
	    vect_get_vec_defs_for_stmt_copy (dt, &vec_oprnds0, NULL);

	  FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	    {
	      /* Arguments are ready, create the new vector stmt.  */
	      if (code1 == CALL_EXPR)
		{
		  new_stmt = gimple_build_call (decl1, 1, vop0);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_call_set_lhs (new_stmt, new_temp);
		}
	      else
		{
		  gcc_assert (TREE_CODE_LENGTH (code1) == unary_op);
		  new_stmt = gimple_build_assign_with_ops (code1, vec_dest,
							   vop0, NULL);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_assign_set_lhs (new_stmt, new_temp);
		}

	      vect_finish_stmt_generation (stmt, new_stmt, gsi);
	      if (slp_node)
		SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
	    }

	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
	  prev_stmt_info = vinfo_for_stmt (new_stmt);
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

		      vect_get_vec_defs (op0, NULL_TREE, stmt, &vec_oprnds0, NULL,
					 slp_node, -1);
		    }
		  else
		    vect_get_vec_defs (op0, op1, stmt, &vec_oprnds0,
				       &vec_oprnds1, slp_node, -1);
		}
	      else
		{
		  vec_oprnd0 = vect_get_vec_def_for_operand (op0, stmt, NULL);
		  vec_oprnds0.quick_push (vec_oprnd0);
		  if (op_type == binary_op)
		    {
		      if (code == WIDEN_LSHIFT_EXPR)
			vec_oprnd1 = op1;
		      else
			vec_oprnd1 = vect_get_vec_def_for_operand (op1, stmt,
								   NULL);
		      vec_oprnds1.quick_push (vec_oprnd1);
		    }
		}
	    }
	  else
	    {
	      vec_oprnd0 = vect_get_vec_def_for_stmt_copy (dt[0], vec_oprnd0);
	      vec_oprnds0.truncate (0);
	      vec_oprnds0.quick_push (vec_oprnd0);
	      if (op_type == binary_op)
		{
		  if (code == WIDEN_LSHIFT_EXPR)
		    vec_oprnd1 = op1;
		  else
		    vec_oprnd1 = vect_get_vec_def_for_stmt_copy (dt[1],
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
						      &vec_oprnds1,
						      stmt, this_dest, gsi,
						      c1, c2, decl1, decl2,
						      op_type);
	    }

	  FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	    {
	      if (cvt_type)
		{
		  if (codecvt1 == CALL_EXPR)
		    {
		      new_stmt = gimple_build_call (decl1, 1, vop0);
		      new_temp = make_ssa_name (vec_dest, new_stmt);
		      gimple_call_set_lhs (new_stmt, new_temp);
		    }
		  else
		    {
		      gcc_assert (TREE_CODE_LENGTH (codecvt1) == unary_op);
		      new_temp = make_ssa_name (vec_dest, NULL);
		      new_stmt = gimple_build_assign_with_ops (codecvt1,
							       new_temp,
							       vop0, NULL);
		    }

		  vect_finish_stmt_generation (stmt, new_stmt, gsi);
		}
	      else
		new_stmt = SSA_NAME_DEF_STMT (vop0);

	      if (slp_node)
		SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
	      else
		{
		  if (!prev_stmt_info)
		    STMT_VINFO_VEC_STMT (stmt_info) = new_stmt;
		  else
		    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
		  prev_stmt_info = vinfo_for_stmt (new_stmt);
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
	    vect_get_vec_defs (op0, NULL_TREE, stmt, &vec_oprnds0, NULL,
			       slp_node, -1);
	  else
	    {
	      vec_oprnds0.truncate (0);
	      vect_get_loop_based_defs (&last_oprnd, stmt, dt[0], &vec_oprnds0,
					vect_pow2 (multi_step_cvt) - 1);
	    }

	  /* Arguments are ready.  Create the new vector stmts.  */
	  if (cvt_type)
	    FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
	      {
		if (codecvt1 == CALL_EXPR)
		  {
		    new_stmt = gimple_build_call (decl1, 1, vop0);
		    new_temp = make_ssa_name (vec_dest, new_stmt);
		    gimple_call_set_lhs (new_stmt, new_temp);
		  }
		else
		  {
		    gcc_assert (TREE_CODE_LENGTH (codecvt1) == unary_op);
		    new_temp = make_ssa_name (vec_dest, NULL);
		    new_stmt = gimple_build_assign_with_ops (codecvt1, new_temp,
							     vop0, NULL);
		  }

		vect_finish_stmt_generation (stmt, new_stmt, gsi);
		vec_oprnds0[i] = new_temp;
	      }

	  vect_create_vectorized_demotion_stmts (&vec_oprnds0, multi_step_cvt,
						 stmt, vec_dsts, gsi,
						 slp_node, code1,
						 &prev_stmt_info);
	}

      *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
      break;
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  vec_dsts.release ();
  interm_types.release ();

  return true;
}


/* Function vectorizable_assignment.

   Check if STMT performs an assignment (copy) that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_assignment (gimple stmt, gimple_stmt_iterator *gsi,
			 gimple *vec_stmt, slp_tree slp_node)
{
  tree vec_dest;
  tree scalar_dest;
  tree op;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  tree new_temp;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  unsigned int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies;
  int i, j;
  vec<tree> vec_oprnds = vNULL;
  tree vop;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  gimple new_stmt = NULL;
  stmt_vec_info prev_stmt_info = NULL;
  enum tree_code code;
  tree vectype_in;

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node. Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;

  gcc_assert (ncopies >= 1);

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  /* Is vectorizable assignment?  */
  if (!is_gimple_assign (stmt))
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

  if (!vect_is_simple_use_1 (op, stmt, loop_vinfo, bb_vinfo,
			     &def_stmt, &def, &dt[0], &vectype_in))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.");
      return false;
    }

  /* We can handle NOP_EXPR conversions that do not change the number
     of elements or the vector size.  */
  if ((CONVERT_EXPR_CODE_P (code)
       || code == VIEW_CONVERT_EXPR)
      && (!vectype_in
	  || TYPE_VECTOR_SUBPARTS (vectype_in) != nunits
	  || (GET_MODE_SIZE (TYPE_MODE (vectype))
	      != GET_MODE_SIZE (TYPE_MODE (vectype_in)))))
    return false;

  /* We do not handle bit-precision changes.  */
  if ((CONVERT_EXPR_CODE_P (code)
       || code == VIEW_CONVERT_EXPR)
      && INTEGRAL_TYPE_P (TREE_TYPE (scalar_dest))
      && ((TYPE_PRECISION (TREE_TYPE (scalar_dest))
	   != GET_MODE_PRECISION (TYPE_MODE (TREE_TYPE (scalar_dest))))
	  || ((TYPE_PRECISION (TREE_TYPE (op))
	       != GET_MODE_PRECISION (TYPE_MODE (TREE_TYPE (op))))))
      /* But a conversion that does not change the bit-pattern is ok.  */
      && !((TYPE_PRECISION (TREE_TYPE (scalar_dest))
	    > TYPE_PRECISION (TREE_TYPE (op)))
	   && TYPE_UNSIGNED (TREE_TYPE (op))))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "type conversion to/from bit-precision "
                         "unsupported.");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = assignment_vec_info_type;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "=== vectorizable_assignment ===");
      vect_model_simple_cost (stmt_info, ncopies, dt, NULL, NULL);
      return true;
    }

  /** Transform.  **/
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "transform assignment.");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle use.  */
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
        vect_get_vec_defs (op, NULL, stmt, &vec_oprnds, NULL, slp_node, -1);
      else
        vect_get_vec_defs_for_stmt_copy (dt, &vec_oprnds, NULL);

      /* Arguments are ready. create the new vector stmt.  */
      FOR_EACH_VEC_ELT (vec_oprnds, i, vop)
       {
	 if (CONVERT_EXPR_CODE_P (code)
	     || code == VIEW_CONVERT_EXPR)
	   vop = build1 (VIEW_CONVERT_EXPR, vectype, vop);
         new_stmt = gimple_build_assign (vec_dest, vop);
         new_temp = make_ssa_name (vec_dest, new_stmt);
         gimple_assign_set_lhs (new_stmt, new_temp);
         vect_finish_stmt_generation (stmt, new_stmt, gsi);
         if (slp_node)
           SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
       }

      if (slp_node)
        continue;

      if (j == 0)
        STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
      else
        STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;

      prev_stmt_info = vinfo_for_stmt (new_stmt);
    }

  vec_oprnds.release ();
  return true;
}


/* Return TRUE if CODE (a shift operation) is supported for SCALAR_TYPE
   either as shift by a scalar or by a vector.  */

bool
vect_supportable_shift (enum tree_code code, tree scalar_type)
{

  enum machine_mode vec_mode;
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

   Check if STMT performs a shift operation that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_shift (gimple stmt, gimple_stmt_iterator *gsi,
                    gimple *vec_stmt, slp_tree slp_node)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL;
  tree vec_oprnd1 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code;
  enum machine_mode vec_mode;
  tree new_temp;
  optab optab;
  int icode;
  enum machine_mode optab_op2_mode;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt[2] = {vect_unknown_def_type, vect_unknown_def_type};
  gimple new_stmt = NULL;
  stmt_vec_info prev_stmt_info;
  int nunits_in;
  int nunits_out;
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
  int vf;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  /* Is STMT a vectorizable binary/unary operation?   */
  if (!is_gimple_assign (stmt))
    return false;

  if (TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (!(code == LSHIFT_EXPR || code == RSHIFT_EXPR || code == LROTATE_EXPR
      || code == RROTATE_EXPR))
    return false;

  scalar_dest = gimple_assign_lhs (stmt);
  vectype_out = STMT_VINFO_VECTYPE (stmt_info);
  if (TYPE_PRECISION (TREE_TYPE (scalar_dest))
      != GET_MODE_PRECISION (TYPE_MODE (TREE_TYPE (scalar_dest))))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "bit-precision shifts not supported.");
      return false;
    }

  op0 = gimple_assign_rhs1 (stmt);
  if (!vect_is_simple_use_1 (op0, stmt, loop_vinfo, bb_vinfo,
                             &def_stmt, &def, &dt[0], &vectype))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.");
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
                         "no vectype for scalar type ");
      return false;
    }

  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype);
  if (nunits_out != nunits_in)
    return false;

  op1 = gimple_assign_rhs2 (stmt);
  if (!vect_is_simple_use_1 (op1, stmt, loop_vinfo, bb_vinfo, &def_stmt,
			     &def, &dt[1], &op1_vectype))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.");
      return false;
    }

  if (loop_vinfo)
    vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  else
    vf = 1;

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_in;

  gcc_assert (ncopies >= 1);

  /* Determine whether the shift amount is a vector, or scalar.  If the
     shift/rotate amount is a vector, use the vector/vector shift optabs.  */

  if (dt[1] == vect_internal_def && !slp_node)
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
	  vec<gimple> stmts = SLP_TREE_SCALAR_STMTS (slp_node);
	  gimple slpstmt;

	  FOR_EACH_VEC_ELT (stmts, k, slpstmt)
	    if (!operand_equal_p (gimple_assign_rhs2 (slpstmt), op1, 0))
	      scalar_shift_arg = false;
	}
    }
  else
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "operand mode requires invariant argument.");
      return false;
    }

  /* Vector shifted by vector.  */
  if (!scalar_shift_arg)
    {
      optab = optab_for_tree_code (code, vectype, optab_vector);
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "vector/vector shift/rotate found.");

      if (!op1_vectype)
	op1_vectype = get_same_sized_vectype (TREE_TYPE (op1), vectype_out);
      if (op1_vectype == NULL_TREE
	  || TYPE_MODE (op1_vectype) != TYPE_MODE (vectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "unusable type for last operand in"
                             " vector/vector shift/rotate.");
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
                             "vector/scalar shift/rotate found.");
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
                                 "vector/vector shift/rotate found.");

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
                                         " vector/vector shift/rotate.");
			return false;
		    }
		  if (vec_stmt && !slp_node)
		    {
		      op1 = fold_convert (TREE_TYPE (vectype), op1);
		      op1 = vect_init_vector (stmt, op1,
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
                         "no optab.");
      return false;
    }
  vec_mode = TYPE_MODE (vectype);
  icode = (int) optab_handler (optab, vec_mode);
  if (icode == CODE_FOR_nothing)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "op not supported by target.");
      /* Check only during analysis.  */
      if (GET_MODE_SIZE (vec_mode) != UNITS_PER_WORD
          || (vf < vect_min_worthwhile_factor (code)
              && !vec_stmt))
        return false;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location, "proceeding using word mode.");
    }

  /* Worthwhile without SIMD support?  Check only during analysis.  */
  if (!VECTOR_MODE_P (TYPE_MODE (vectype))
      && vf < vect_min_worthwhile_factor (code)
      && !vec_stmt)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not worthwhile without SIMD support.");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = shift_vec_info_type;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location, "=== vectorizable_shift ===");
      vect_model_simple_cost (stmt_info, ncopies, dt, NULL, NULL);
      return true;
    }

  /** Transform.  **/

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform binary/unary operation.");

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
                                     "operand 1 using scalar mode.");
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
            vect_get_vec_defs (op0, NULL_TREE, stmt, &vec_oprnds0, NULL,
                               slp_node, -1);
          else
            vect_get_vec_defs (op0, op1, stmt, &vec_oprnds0, &vec_oprnds1,
                               slp_node, -1);
        }
      else
        vect_get_vec_defs_for_stmt_copy (dt, &vec_oprnds0, &vec_oprnds1);

      /* Arguments are ready.  Create the new vector stmt.  */
      FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
        {
          vop1 = vec_oprnds1[i];
          new_stmt = gimple_build_assign_with_ops (code, vec_dest, vop0, vop1);
          new_temp = make_ssa_name (vec_dest, new_stmt);
          gimple_assign_set_lhs (new_stmt, new_temp);
          vect_finish_stmt_generation (stmt, new_stmt, gsi);
          if (slp_node)
            SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
        }

      if (slp_node)
        continue;

      if (j == 0)
        STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
      else
        STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
      prev_stmt_info = vinfo_for_stmt (new_stmt);
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();

  return true;
}


static tree permute_vec_elements (tree, tree, tree, gimple,
				  gimple_stmt_iterator *);


/* Function vectorizable_operation.

   Check if STMT performs a binary, unary or ternary operation that can
   be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_operation (gimple stmt, gimple_stmt_iterator *gsi,
			gimple *vec_stmt, slp_tree slp_node)
{
  tree vec_dest;
  tree scalar_dest;
  tree op0, op1 = NULL_TREE, op2 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code;
  enum machine_mode vec_mode;
  tree new_temp;
  int op_type;
  optab optab;
  int icode;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt[3]
    = {vect_unknown_def_type, vect_unknown_def_type, vect_unknown_def_type};
  gimple new_stmt = NULL;
  stmt_vec_info prev_stmt_info;
  int nunits_in;
  int nunits_out;
  tree vectype_out;
  int ncopies;
  int j, i;
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  vec<tree> vec_oprnds2 = vNULL;
  tree vop0, vop1, vop2;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  int vf;

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  /* Is STMT a vectorizable binary/unary operation?   */
  if (!is_gimple_assign (stmt))
    return false;

  if (TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);

  /* For pointer addition, we should use the normal plus for
     the vector addition.  */
  if (code == POINTER_PLUS_EXPR)
    code = PLUS_EXPR;

  /* Support only unary or binary operations.  */
  op_type = TREE_CODE_LENGTH (code);
  if (op_type != unary_op && op_type != binary_op && op_type != ternary_op)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "num. args = %d (not unary/binary/ternary op).",
                         op_type);
      return false;
    }

  scalar_dest = gimple_assign_lhs (stmt);
  vectype_out = STMT_VINFO_VECTYPE (stmt_info);

  /* Most operations cannot handle bit-precision types without extra
     truncations.  */
  if ((TYPE_PRECISION (TREE_TYPE (scalar_dest))
       != GET_MODE_PRECISION (TYPE_MODE (TREE_TYPE (scalar_dest))))
      /* Exception are bitwise binary operations.  */
      && code != BIT_IOR_EXPR
      && code != BIT_XOR_EXPR
      && code != BIT_AND_EXPR)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "bit-precision arithmetic not supported.");
      return false;
    }

  op0 = gimple_assign_rhs1 (stmt);
  if (!vect_is_simple_use_1 (op0, stmt, loop_vinfo, bb_vinfo,
			     &def_stmt, &def, &dt[0], &vectype))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.");
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
        {
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "no vectype for scalar type ");
          dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
                             TREE_TYPE (op0));
        }

      return false;
    }

  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype);
  if (nunits_out != nunits_in)
    return false;

  if (op_type == binary_op || op_type == ternary_op)
    {
      op1 = gimple_assign_rhs2 (stmt);
      if (!vect_is_simple_use (op1, stmt, loop_vinfo, bb_vinfo, &def_stmt,
			       &def, &dt[1]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.");
	  return false;
	}
    }
  if (op_type == ternary_op)
    {
      op2 = gimple_assign_rhs3 (stmt);
      if (!vect_is_simple_use (op2, stmt, loop_vinfo, bb_vinfo, &def_stmt,
			       &def, &dt[2]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "use not simple.");
	  return false;
	}
    }

  if (loop_vinfo)
    vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  else
    vf = 1;

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp_node || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_in;

  gcc_assert (ncopies >= 1);

  /* Shifts are handled in vectorizable_shift ().  */
  if (code == LSHIFT_EXPR || code == RSHIFT_EXPR || code == LROTATE_EXPR
      || code == RROTATE_EXPR)
   return false;

  /* Supportable by target?  */

  vec_mode = TYPE_MODE (vectype);
  if (code == MULT_HIGHPART_EXPR)
    {
      if (can_mult_highpart_p (vec_mode, TYPE_UNSIGNED (vectype)))
	icode = LAST_INSN_CODE;
      else
	icode = CODE_FOR_nothing;
    }
  else
    {
      optab = optab_for_tree_code (code, vectype, optab_default);
      if (!optab)
	{
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "no optab.");
	  return false;
	}
      icode = (int) optab_handler (optab, vec_mode);
    }

  if (icode == CODE_FOR_nothing)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "op not supported by target.");
      /* Check only during analysis.  */
      if (GET_MODE_SIZE (vec_mode) != UNITS_PER_WORD
	  || (!vec_stmt && vf < vect_min_worthwhile_factor (code)))
        return false;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "proceeding using word mode.");
    }

  /* Worthwhile without SIMD support?  Check only during analysis.  */
  if (!VECTOR_MODE_P (vec_mode)
      && !vec_stmt
      && vf < vect_min_worthwhile_factor (code))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not worthwhile without SIMD support.");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = op_vec_info_type;
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "=== vectorizable_operation ===");
      vect_model_simple_cost (stmt_info, ncopies, dt, NULL, NULL);
      return true;
    }

  /** Transform.  **/

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform binary/unary operation.");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

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
	  if (op_type == binary_op || op_type == ternary_op)
	    vect_get_vec_defs (op0, op1, stmt, &vec_oprnds0, &vec_oprnds1,
			       slp_node, -1);
	  else
	    vect_get_vec_defs (op0, NULL_TREE, stmt, &vec_oprnds0, NULL,
			       slp_node, -1);
	  if (op_type == ternary_op)
	    {
	      vec_oprnds2.create (1);
	      vec_oprnds2.quick_push (vect_get_vec_def_for_operand (op2,
		                                                    stmt,
								    NULL));
	    }
	}
      else
	{
	  vect_get_vec_defs_for_stmt_copy (dt, &vec_oprnds0, &vec_oprnds1);
	  if (op_type == ternary_op)
	    {
	      tree vec_oprnd = vec_oprnds2.pop ();
	      vec_oprnds2.quick_push (vect_get_vec_def_for_stmt_copy (dt[2],
							           vec_oprnd));
	    }
	}

      /* Arguments are ready.  Create the new vector stmt.  */
      FOR_EACH_VEC_ELT (vec_oprnds0, i, vop0)
        {
	  vop1 = ((op_type == binary_op || op_type == ternary_op)
		  ? vec_oprnds1[i] : NULL_TREE);
	  vop2 = ((op_type == ternary_op)
		  ? vec_oprnds2[i] : NULL_TREE);
	  new_stmt = gimple_build_assign_with_ops (code, vec_dest,
						   vop0, vop1, vop2);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  gimple_assign_set_lhs (new_stmt, new_temp);
	  vect_finish_stmt_generation (stmt, new_stmt, gsi);
          if (slp_node)
	    SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
        }

      if (slp_node)
        continue;

      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
      prev_stmt_info = vinfo_for_stmt (new_stmt);
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  vec_oprnds2.release ();

  return true;
}

/* A helper function to ensure data reference DR's base alignment
   for STMT_INFO.  */

static void
ensure_base_align (stmt_vec_info stmt_info, struct data_reference *dr)
{
  if (!dr->aux)
    return;

  if (((dataref_aux *)dr->aux)->base_misaligned)
    {
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      tree base_decl = ((dataref_aux *)dr->aux)->base_decl;

      DECL_ALIGN (base_decl) = TYPE_ALIGN (vectype);
      DECL_USER_ALIGN (base_decl) = 1;
      ((dataref_aux *)dr->aux)->base_misaligned = false;
    }
}


/* Function vectorizable_store.

   Check if STMT defines a non scalar data-ref (array/pointer/structure) that
   can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_store (gimple stmt, gimple_stmt_iterator *gsi, gimple *vec_stmt,
                    slp_tree slp_node)
{
  tree scalar_dest;
  tree data_ref;
  tree op;
  tree vec_oprnd = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info), *first_dr = NULL;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree elem_type;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  enum machine_mode vec_mode;
  tree dummy;
  enum dr_alignment_support alignment_support_scheme;
  tree def;
  gimple def_stmt;
  enum vect_def_type dt;
  stmt_vec_info prev_stmt_info = NULL;
  tree dataref_ptr = NULL_TREE;
  tree dataref_offset = NULL_TREE;
  gimple ptr_incr = NULL;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies;
  int j;
  gimple next_stmt, first_stmt = NULL;
  bool grouped_store = false;
  bool store_lanes_p = false;
  unsigned int group_size, i;
  vec<tree> dr_chain = vNULL;
  vec<tree> oprnds = vNULL;
  vec<tree> result_chain = vNULL;
  bool inv_p;
  vec<tree> vec_oprnds = vNULL;
  bool slp = (slp_node != NULL);
  unsigned int vec_num;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  tree aggr_type;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node. Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;

  gcc_assert (ncopies >= 1);

  /* FORNOW. This restriction should be relaxed.  */
  if (loop && nested_in_vect_loop_p (loop, stmt) && ncopies > 1)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "multiple types in nested loop.");
      return false;
    }

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  /* Is vectorizable store? */

  if (!is_gimple_assign (stmt))
    return false;

  scalar_dest = gimple_assign_lhs (stmt);
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

  gcc_assert (gimple_assign_single_p (stmt));
  op = gimple_assign_rhs1 (stmt);
  if (!vect_is_simple_use (op, stmt, loop_vinfo, bb_vinfo, &def_stmt,
			   &def, &dt))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "use not simple.");
      return false;
    }

  elem_type = TREE_TYPE (vectype);
  vec_mode = TYPE_MODE (vectype);

  /* FORNOW. In some cases can vectorize even if data-type not supported
     (e.g. - array initialization with 0).  */
  if (optab_handler (mov_optab, vec_mode) == CODE_FOR_nothing)
    return false;

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  if (tree_int_cst_compare (loop && nested_in_vect_loop_p (loop, stmt)
			    ? STMT_VINFO_DR_STEP (stmt_info) : DR_STEP (dr),
			    size_zero_node) < 0)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "negative step for store.");
      return false;
    }

  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      grouped_store = true;
      first_stmt = GROUP_FIRST_ELEMENT (stmt_info);
      if (!slp && !PURE_SLP_STMT (stmt_info))
	{
	  group_size = GROUP_SIZE (vinfo_for_stmt (first_stmt));
	  if (vect_store_lanes_supported (vectype, group_size))
	    store_lanes_p = true;
	  else if (!vect_grouped_store_supported (vectype, group_size))
	    return false;
	}

      if (first_stmt == stmt)
	{
          /* STMT is the leader of the group. Check the operands of all the
             stmts of the group.  */
          next_stmt = GROUP_NEXT_ELEMENT (stmt_info);
          while (next_stmt)
            {
	      gcc_assert (gimple_assign_single_p (next_stmt));
	      op = gimple_assign_rhs1 (next_stmt);
              if (!vect_is_simple_use (op, next_stmt, loop_vinfo, bb_vinfo,
				       &def_stmt, &def, &dt))
                {
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                     "use not simple.");
                  return false;
                }
              next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
            }
        }
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = store_vec_info_type;
      vect_model_store_cost (stmt_info, ncopies, store_lanes_p, dt,
			     NULL, NULL, NULL);
      return true;
    }

  /** Transform.  **/

  ensure_base_align (stmt_info, dr);

  if (grouped_store)
    {
      first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt));
      group_size = GROUP_SIZE (vinfo_for_stmt (first_stmt));

      GROUP_STORE_COUNT (vinfo_for_stmt (first_stmt))++;

      /* FORNOW */
      gcc_assert (!loop || !nested_in_vect_loop_p (loop, stmt));

      /* We vectorize all the stmts of the interleaving group when we
	 reach the last stmt in the group.  */
      if (GROUP_STORE_COUNT (vinfo_for_stmt (first_stmt))
	  < GROUP_SIZE (vinfo_for_stmt (first_stmt))
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
          first_stmt = SLP_TREE_SCALAR_STMTS (slp_node)[0]; 
          first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt));
	  op = gimple_assign_rhs1 (first_stmt);
        } 
      else
        /* VEC_NUM is the number of vect stmts to be created for this 
           group.  */
	vec_num = group_size;
    }
  else
    {
      first_stmt = stmt;
      first_dr = dr;
      group_size = vec_num = 1;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform store. ncopies = %d", ncopies);

  dr_chain.create (group_size);
  oprnds.create (group_size);

  alignment_support_scheme = vect_supportable_dr_alignment (first_dr, false);
  gcc_assert (alignment_support_scheme);
  /* Targets with store-lane instructions must not require explicit
     realignment.  */
  gcc_assert (!store_lanes_p
	      || alignment_support_scheme == dr_aligned
	      || alignment_support_scheme == dr_unaligned_supported);

  if (store_lanes_p)
    aggr_type = build_array_type_nelts (elem_type, vec_num * nunits);
  else
    aggr_type = vectype;

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
  for (j = 0; j < ncopies; j++)
    {
      gimple new_stmt;

      if (j == 0)
	{
          if (slp)
            {
	      /* Get vectorized arguments for SLP_NODE.  */
              vect_get_vec_defs (op, NULL_TREE, stmt, &vec_oprnds,
                                 NULL, slp_node, -1);

              vec_oprnd = vec_oprnds[0];
            }
          else
            {
	      /* For interleaved stores we collect vectorized defs for all the
		 stores in the group in DR_CHAIN and OPRNDS. DR_CHAIN is then
		 used as an input to vect_permute_store_chain(), and OPRNDS as
		 an input to vect_get_vec_def_for_stmt_copy() for the next copy.

		 If the store is not grouped, GROUP_SIZE is 1, and DR_CHAIN and
		 OPRNDS are of size 1.  */
	      next_stmt = first_stmt;
	      for (i = 0; i < group_size; i++)
		{
		  /* Since gaps are not supported for interleaved stores,
		     GROUP_SIZE is the exact number of stmts in the chain.
		     Therefore, NEXT_STMT can't be NULL_TREE.  In case that
		     there is no interleaving, GROUP_SIZE is 1, and only one
		     iteration of the loop will be executed.  */
		  gcc_assert (next_stmt
			      && gimple_assign_single_p (next_stmt));
		  op = gimple_assign_rhs1 (next_stmt);

		  vec_oprnd = vect_get_vec_def_for_operand (op, next_stmt,
							    NULL);
		  dr_chain.quick_push (vec_oprnd);
		  oprnds.quick_push (vec_oprnd);
		  next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
		}
	    }

	  /* We should have catched mismatched types earlier.  */
	  gcc_assert (useless_type_conversion_p (vectype,
						 TREE_TYPE (vec_oprnd)));
	  bool simd_lane_access_p
	    = STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info);
	  if (simd_lane_access_p
	      && TREE_CODE (DR_BASE_ADDRESS (first_dr)) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (first_dr), 0))
	      && integer_zerop (DR_OFFSET (first_dr))
	      && integer_zerop (DR_INIT (first_dr))
	      && alias_sets_conflict_p (get_alias_set (aggr_type),
					get_alias_set (DR_REF (first_dr))))
	    {
	      dataref_ptr = unshare_expr (DR_BASE_ADDRESS (first_dr));
	      dataref_offset = build_int_cst (reference_alias_ptr_type
					      (DR_REF (first_dr)), 0);
	    }
	  else
	    dataref_ptr
	      = vect_create_data_ref_ptr (first_stmt, aggr_type,
					  simd_lane_access_p ? loop : NULL,
					  NULL_TREE, &dummy, gsi, &ptr_incr,
					  simd_lane_access_p, &inv_p);
	  gcc_assert (bb_vinfo || !inv_p);
	}
      else
	{
	  /* For interleaved stores we created vectorized defs for all the
	     defs stored in OPRNDS in the previous iteration (previous copy).
	     DR_CHAIN is then used as an input to vect_permute_store_chain(),
	     and OPRNDS as an input to vect_get_vec_def_for_stmt_copy() for the
	     next copy.
	     If the store is not grouped, GROUP_SIZE is 1, and DR_CHAIN and
	     OPRNDS are of size 1.  */
	  for (i = 0; i < group_size; i++)
	    {
	      op = oprnds[i];
	      vect_is_simple_use (op, NULL, loop_vinfo, bb_vinfo, &def_stmt,
				  &def, &dt);
	      vec_oprnd = vect_get_vec_def_for_stmt_copy (dt, op);
	      dr_chain[i] = vec_oprnd;
	      oprnds[i] = vec_oprnd;
	    }
	  if (dataref_offset)
	    dataref_offset
	      = int_const_binop (PLUS_EXPR, dataref_offset,
				 TYPE_SIZE_UNIT (aggr_type));
	  else
	    dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi, stmt,
					   TYPE_SIZE_UNIT (aggr_type));
	}

      if (store_lanes_p)
	{
	  tree vec_array;

	  /* Combine all the vectors into an array.  */
	  vec_array = create_vector_array (vectype, vec_num);
	  for (i = 0; i < vec_num; i++)
	    {
	      vec_oprnd = dr_chain[i];
	      write_vector_array (stmt, gsi, vec_oprnd, vec_array, i);
	    }

	  /* Emit:
	       MEM_REF[...all elements...] = STORE_LANES (VEC_ARRAY).  */
	  data_ref = create_array_ref (aggr_type, dataref_ptr, first_dr);
	  new_stmt = gimple_build_call_internal (IFN_STORE_LANES, 1, vec_array);
	  gimple_call_set_lhs (new_stmt, data_ref);
	  vect_finish_stmt_generation (stmt, new_stmt, gsi);
	}
      else
	{
	  new_stmt = NULL;
	  if (grouped_store)
	    {
	      if (j == 0)
		result_chain.create (group_size);
	      /* Permute.  */
	      vect_permute_store_chain (dr_chain, group_size, stmt, gsi,
					&result_chain);
	    }

	  next_stmt = first_stmt;
	  for (i = 0; i < vec_num; i++)
	    {
	      unsigned align, misalign;

	      if (i > 0)
		/* Bump the vector pointer.  */
		dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					       stmt, NULL_TREE);

	      if (slp)
		vec_oprnd = vec_oprnds[i];
	      else if (grouped_store)
		/* For grouped stores vectorized defs are interleaved in
		   vect_permute_store_chain().  */
		vec_oprnd = result_chain[i];

	      data_ref = build2 (MEM_REF, TREE_TYPE (vec_oprnd), dataref_ptr,
				 dataref_offset
				 ? dataref_offset
				 : build_int_cst (reference_alias_ptr_type
						  (DR_REF (first_dr)), 0));
	      align = TYPE_ALIGN_UNIT (vectype);
	      if (aligned_access_p (first_dr))
		misalign = 0;
	      else if (DR_MISALIGNMENT (first_dr) == -1)
		{
		  TREE_TYPE (data_ref)
		    = build_aligned_type (TREE_TYPE (data_ref),
					  TYPE_ALIGN (elem_type));
		  align = TYPE_ALIGN_UNIT (elem_type);
		  misalign = 0;
		}
	      else
		{
		  TREE_TYPE (data_ref)
		    = build_aligned_type (TREE_TYPE (data_ref),
					  TYPE_ALIGN (elem_type));
		  misalign = DR_MISALIGNMENT (first_dr);
		}
	      if (dataref_offset == NULL_TREE)
		set_ptr_info_alignment (get_ptr_info (dataref_ptr), align,
					misalign);

	      /* Arguments are ready.  Create the new vector stmt.  */
	      new_stmt = gimple_build_assign (data_ref, vec_oprnd);
	      vect_finish_stmt_generation (stmt, new_stmt, gsi);

	      if (slp)
		continue;

	      next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
	      if (!next_stmt)
		break;
	    }
	}
      if (!slp)
	{
	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
	  prev_stmt_info = vinfo_for_stmt (new_stmt);
	}
    }

  dr_chain.release ();
  oprnds.release ();
  result_chain.release ();
  vec_oprnds.release ();

  return true;
}

/* Given a vector type VECTYPE and permutation SEL returns
   the VECTOR_CST mask that implements the permutation of the
   vector elements.  If that is impossible to do, returns NULL.  */

tree
vect_gen_perm_mask (tree vectype, unsigned char *sel)
{
  tree mask_elt_type, mask_type, mask_vec, *mask_elts;
  int i, nunits;

  nunits = TYPE_VECTOR_SUBPARTS (vectype);

  if (!can_vec_perm_p (TYPE_MODE (vectype), false, sel))
    return NULL;

  mask_elt_type = lang_hooks.types.type_for_mode
		    (int_mode_for_mode (TYPE_MODE (TREE_TYPE (vectype))), 1);
  mask_type = get_vectype_for_scalar_type (mask_elt_type);

  mask_elts = XALLOCAVEC (tree, nunits);
  for (i = nunits - 1; i >= 0; i--)
    mask_elts[i] = build_int_cst (mask_elt_type, sel[i]);
  mask_vec = build_vector (mask_type, mask_elts);

  return mask_vec;
}

/* Given a vector type VECTYPE returns the VECTOR_CST mask that implements
   reversal of the vector elements.  If that is impossible to do,
   returns NULL.  */

static tree
perm_mask_for_reverse (tree vectype)
{
  int i, nunits;
  unsigned char *sel;

  nunits = TYPE_VECTOR_SUBPARTS (vectype);
  sel = XALLOCAVEC (unsigned char, nunits);

  for (i = 0; i < nunits; ++i)
    sel[i] = nunits - 1 - i;

  return vect_gen_perm_mask (vectype, sel);
}

/* Given a vector variable X and Y, that was generated for the scalar
   STMT, generate instructions to permute the vector elements of X and Y
   using permutation mask MASK_VEC, insert them at *GSI and return the
   permuted vector variable.  */

static tree
permute_vec_elements (tree x, tree y, tree mask_vec, gimple stmt,
		      gimple_stmt_iterator *gsi)
{
  tree vectype = TREE_TYPE (x);
  tree perm_dest, data_ref;
  gimple perm_stmt;

  perm_dest = vect_create_destination_var (gimple_assign_lhs (stmt), vectype);
  data_ref = make_ssa_name (perm_dest, NULL);

  /* Generate the permute statement.  */
  perm_stmt = gimple_build_assign_with_ops (VEC_PERM_EXPR, data_ref,
					    x, y, mask_vec);
  vect_finish_stmt_generation (stmt, perm_stmt, gsi);

  return data_ref;
}

/* vectorizable_load.

   Check if STMT reads a non scalar data-ref (array/pointer/structure) that
   can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_load (gimple stmt, gimple_stmt_iterator *gsi, gimple *vec_stmt,
                   slp_tree slp_node, slp_instance slp_node_instance)
{
  tree scalar_dest;
  tree vec_dest = NULL;
  tree data_ref = NULL;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  stmt_vec_info prev_stmt_info;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  struct loop *containing_loop = (gimple_bb (stmt))->loop_father;
  bool nested_in_vect_loop = false;
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info), *first_dr = NULL;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree elem_type;
  tree new_temp;
  enum machine_mode mode;
  gimple new_stmt = NULL;
  tree dummy;
  enum dr_alignment_support alignment_support_scheme;
  tree dataref_ptr = NULL_TREE;
  tree dataref_offset = NULL_TREE;
  gimple ptr_incr = NULL;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies;
  int i, j, group_size, group_gap;
  tree msq = NULL_TREE, lsq;
  tree offset = NULL_TREE;
  tree realignment_token = NULL_TREE;
  gimple phi = NULL;
  vec<tree> dr_chain = vNULL;
  bool grouped_load = false;
  bool load_lanes_p = false;
  gimple first_stmt;
  bool inv_p;
  bool negative = false;
  bool compute_in_loop = false;
  struct loop *at_loop;
  int vec_num;
  bool slp = (slp_node != NULL);
  bool slp_perm = false;
  enum tree_code code;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  int vf;
  tree aggr_type;
  tree gather_base = NULL_TREE, gather_off = NULL_TREE;
  tree gather_off_vectype = NULL_TREE, gather_decl = NULL_TREE;
  int gather_scale = 1;
  enum vect_def_type gather_dt = vect_unknown_def_type;

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (loop, stmt);
      vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
    }
  else
    vf = 1;

  /* Multiple types in SLP are handled by creating the appropriate number of
     vectorized stmts for each SLP node.  Hence, NCOPIES is always 1 in
     case of SLP.  */
  if (slp || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;

  gcc_assert (ncopies >= 1);

  /* FORNOW. This restriction should be relaxed.  */
  if (nested_in_vect_loop && ncopies > 1)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "multiple types in nested loop.");
      return false;
    }

  if (!STMT_VINFO_RELEVANT_P (stmt_info) && !bb_vinfo)
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_internal_def)
    return false;

  /* Is vectorizable load? */
  if (!is_gimple_assign (stmt))
    return false;

  scalar_dest = gimple_assign_lhs (stmt);
  if (TREE_CODE (scalar_dest) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (code != ARRAY_REF
      && code != BIT_FIELD_REF
      && code != INDIRECT_REF
      && code != COMPONENT_REF
      && code != IMAGPART_EXPR
      && code != REALPART_EXPR
      && code != MEM_REF
      && TREE_CODE_CLASS (code) != tcc_declaration)
    return false;

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  elem_type = TREE_TYPE (vectype);
  mode = TYPE_MODE (vectype);

  /* FORNOW. In some cases can vectorize even if data-type not supported
    (e.g. - data copies).  */
  if (optab_handler (mov_optab, mode) == CODE_FOR_nothing)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "Aligned load, but unsupported type.");
      return false;
    }

  /* Check if the load is a part of an interleaving chain.  */
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      grouped_load = true;
      /* FORNOW */
      gcc_assert (! nested_in_vect_loop && !STMT_VINFO_GATHER_P (stmt_info));

      first_stmt = GROUP_FIRST_ELEMENT (stmt_info);
      if (!slp && !PURE_SLP_STMT (stmt_info))
	{
	  group_size = GROUP_SIZE (vinfo_for_stmt (first_stmt));
	  if (vect_load_lanes_supported (vectype, group_size))
	    load_lanes_p = true;
	  else if (!vect_grouped_load_supported (vectype, group_size))
	    return false;
	}
    }


  if (STMT_VINFO_GATHER_P (stmt_info))
    {
      gimple def_stmt;
      tree def;
      gather_decl = vect_check_gather (stmt, loop_vinfo, &gather_base,
				       &gather_off, &gather_scale);
      gcc_assert (gather_decl);
      if (!vect_is_simple_use_1 (gather_off, NULL, loop_vinfo, bb_vinfo,
				 &def_stmt, &def, &gather_dt,
				 &gather_off_vectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "gather index use not simple.");
	  return false;
	}
    }
  else if (STMT_VINFO_STRIDE_LOAD_P (stmt_info))
    ;
  else
    {
      negative = tree_int_cst_compare (nested_in_vect_loop
				       ? STMT_VINFO_DR_STEP (stmt_info)
				       : DR_STEP (dr),
				       size_zero_node) < 0;
      if (negative && ncopies > 1)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "multiple types with negative step.");
	  return false;
	}

      if (negative)
	{
	  if (grouped_load)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "negative step for group load not supported");
	      return false;
	    }
	  alignment_support_scheme = vect_supportable_dr_alignment (dr, false);
	  if (alignment_support_scheme != dr_aligned
	      && alignment_support_scheme != dr_unaligned_supported)
	    {
              if (dump_enabled_p ())
                dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                 "negative step but alignment required.");
	      return false;
	    }
	  if (!perm_mask_for_reverse (vectype))
	    {
              if (dump_enabled_p ())
                dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                 "negative step and reversing not supported.");
	      return false;
	    }
	}
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = load_vec_info_type;
      vect_model_load_cost (stmt_info, ncopies, load_lanes_p, NULL, NULL, NULL);
      return true;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "transform load. ncopies = %d", ncopies);

  /** Transform.  **/

  ensure_base_align (stmt_info, dr);

  if (STMT_VINFO_GATHER_P (stmt_info))
    {
      tree vec_oprnd0 = NULL_TREE, op;
      tree arglist = TYPE_ARG_TYPES (TREE_TYPE (gather_decl));
      tree rettype, srctype, ptrtype, idxtype, masktype, scaletype;
      tree ptr, mask, var, scale, perm_mask = NULL_TREE, prev_res = NULL_TREE;
      edge pe = loop_preheader_edge (loop);
      gimple_seq seq;
      basic_block new_bb;
      enum { NARROW, NONE, WIDEN } modifier;
      int gather_off_nunits = TYPE_VECTOR_SUBPARTS (gather_off_vectype);

      if (nunits == gather_off_nunits)
	modifier = NONE;
      else if (nunits == gather_off_nunits / 2)
	{
	  unsigned char *sel = XALLOCAVEC (unsigned char, gather_off_nunits);
	  modifier = WIDEN;

	  for (i = 0; i < gather_off_nunits; ++i)
	    sel[i] = i | nunits;

	  perm_mask = vect_gen_perm_mask (gather_off_vectype, sel);
	  gcc_assert (perm_mask != NULL_TREE);
	}
      else if (nunits == gather_off_nunits * 2)
	{
	  unsigned char *sel = XALLOCAVEC (unsigned char, nunits);
	  modifier = NARROW;

	  for (i = 0; i < nunits; ++i)
	    sel[i] = i < gather_off_nunits
		     ? i : i + nunits - gather_off_nunits;

	  perm_mask = vect_gen_perm_mask (vectype, sel);
	  gcc_assert (perm_mask != NULL_TREE);
	  ncopies *= 2;
	}
      else
	gcc_unreachable ();

      rettype = TREE_TYPE (TREE_TYPE (gather_decl));
      srctype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      ptrtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      idxtype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      masktype = TREE_VALUE (arglist); arglist = TREE_CHAIN (arglist);
      scaletype = TREE_VALUE (arglist);
      gcc_checking_assert (types_compatible_p (srctype, rettype)
			   && types_compatible_p (srctype, masktype));

      vec_dest = vect_create_destination_var (scalar_dest, vectype);

      ptr = fold_convert (ptrtype, gather_base);
      if (!is_gimple_min_invariant (ptr))
	{
	  ptr = force_gimple_operand (ptr, &seq, true, NULL_TREE);
	  new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
	  gcc_assert (!new_bb);
	}

      /* Currently we support only unconditional gather loads,
	 so mask should be all ones.  */
      if (TREE_CODE (TREE_TYPE (masktype)) == INTEGER_TYPE)
	mask = build_int_cst (TREE_TYPE (masktype), -1);
      else if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (masktype)))
	{
	  REAL_VALUE_TYPE r;
	  long tmp[6];
	  for (j = 0; j < 6; ++j)
	    tmp[j] = -1;
	  real_from_target (&r, tmp, TYPE_MODE (TREE_TYPE (masktype)));
	  mask = build_real (TREE_TYPE (masktype), r);
	}
      else
	gcc_unreachable ();
      mask = build_vector_from_val (masktype, mask);
      mask = vect_init_vector (stmt, mask, masktype, NULL);

      scale = build_int_cst (scaletype, gather_scale);

      prev_stmt_info = NULL;
      for (j = 0; j < ncopies; ++j)
	{
	  if (modifier == WIDEN && (j & 1))
	    op = permute_vec_elements (vec_oprnd0, vec_oprnd0,
				       perm_mask, stmt, gsi);
	  else if (j == 0)
	    op = vec_oprnd0
	      = vect_get_vec_def_for_operand (gather_off, stmt, NULL);
	  else
	    op = vec_oprnd0
	      = vect_get_vec_def_for_stmt_copy (gather_dt, vec_oprnd0);

	  if (!useless_type_conversion_p (idxtype, TREE_TYPE (op)))
	    {
	      gcc_assert (TYPE_VECTOR_SUBPARTS (TREE_TYPE (op))
			  == TYPE_VECTOR_SUBPARTS (idxtype));
	      var = vect_get_new_vect_var (idxtype, vect_simple_var, NULL);
	      var = make_ssa_name (var, NULL);
	      op = build1 (VIEW_CONVERT_EXPR, idxtype, op);
	      new_stmt
		= gimple_build_assign_with_ops (VIEW_CONVERT_EXPR, var,
						op, NULL_TREE);
	      vect_finish_stmt_generation (stmt, new_stmt, gsi);
	      op = var;
	    }

	  new_stmt
	    = gimple_build_call (gather_decl, 5, mask, ptr, op, mask, scale);

	  if (!useless_type_conversion_p (vectype, rettype))
	    {
	      gcc_assert (TYPE_VECTOR_SUBPARTS (vectype)
			  == TYPE_VECTOR_SUBPARTS (rettype));
	      var = vect_get_new_vect_var (rettype, vect_simple_var, NULL);
	      op = make_ssa_name (var, new_stmt);
	      gimple_call_set_lhs (new_stmt, op);
	      vect_finish_stmt_generation (stmt, new_stmt, gsi);
	      var = make_ssa_name (vec_dest, NULL);
	      op = build1 (VIEW_CONVERT_EXPR, vectype, op);
	      new_stmt
		= gimple_build_assign_with_ops (VIEW_CONVERT_EXPR, var, op,
						NULL_TREE);
	    }
	  else
	    {
	      var = make_ssa_name (vec_dest, new_stmt);
	      gimple_call_set_lhs (new_stmt, var);
	    }

	  vect_finish_stmt_generation (stmt, new_stmt, gsi);

	  if (modifier == NARROW)
	    {
	      if ((j & 1) == 0)
		{
		  prev_res = var;
		  continue;
		}
	      var = permute_vec_elements (prev_res, var,
					  perm_mask, stmt, gsi);
	      new_stmt = SSA_NAME_DEF_STMT (var);
	    }

	  if (prev_stmt_info == NULL)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
	  prev_stmt_info = vinfo_for_stmt (new_stmt);
	}
      return true;
    }
  else if (STMT_VINFO_STRIDE_LOAD_P (stmt_info))
    {
      gimple_stmt_iterator incr_gsi;
      bool insert_after;
      gimple incr;
      tree offvar;
      tree ivstep;
      tree running_off;
      vec<constructor_elt, va_gc> *v = NULL;
      gimple_seq stmts = NULL;
      tree stride_base, stride_step, alias_off;

      gcc_assert (!nested_in_vect_loop);

      stride_base
	= fold_build_pointer_plus
	    (unshare_expr (DR_BASE_ADDRESS (dr)),
	     size_binop (PLUS_EXPR,
			 convert_to_ptrofftype (unshare_expr (DR_OFFSET (dr))),
			 convert_to_ptrofftype (DR_INIT(dr))));
      stride_step = fold_convert (sizetype, unshare_expr (DR_STEP (dr)));

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

      ivstep = stride_step;
      ivstep = fold_build2 (MULT_EXPR, TREE_TYPE (ivstep), ivstep,
			    build_int_cst (TREE_TYPE (ivstep), vf));

      standard_iv_increment_position (loop, &incr_gsi, &insert_after);

      create_iv (stride_base, ivstep, NULL,
		 loop, &incr_gsi, insert_after,
		 &offvar, NULL);
      incr = gsi_stmt (incr_gsi);
      set_vinfo_for_stmt (incr, new_stmt_vec_info (incr, loop_vinfo, NULL));

      stride_step = force_gimple_operand (stride_step, &stmts, true, NULL_TREE);
      if (stmts)
	gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);

      prev_stmt_info = NULL;
      running_off = offvar;
      alias_off = build_int_cst (reference_alias_ptr_type (DR_REF (dr)), 0);
      for (j = 0; j < ncopies; j++)
	{
	  tree vec_inv;

	  vec_alloc (v, nunits);
	  for (i = 0; i < nunits; i++)
	    {
	      tree newref, newoff;
	      gimple incr;
	      newref = build2 (MEM_REF, TREE_TYPE (vectype),
			       running_off, alias_off);

	      newref = force_gimple_operand_gsi (gsi, newref, true,
						 NULL_TREE, true,
						 GSI_SAME_STMT);
	      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, newref);
	      newoff = copy_ssa_name (running_off, NULL);
	      incr = gimple_build_assign_with_ops (POINTER_PLUS_EXPR, newoff,
						   running_off, stride_step);
	      vect_finish_stmt_generation (stmt, incr, gsi);

	      running_off = newoff;
	    }

	  vec_inv = build_constructor (vectype, v);
	  new_temp = vect_init_vector (stmt, vec_inv, vectype, gsi);
	  new_stmt = SSA_NAME_DEF_STMT (new_temp);

	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
	  prev_stmt_info = vinfo_for_stmt (new_stmt);
	}
      return true;
    }

  if (grouped_load)
    {
      first_stmt = GROUP_FIRST_ELEMENT (stmt_info);
      if (slp
          && !SLP_TREE_LOAD_PERMUTATION (slp_node).exists ()
	  && first_stmt != SLP_TREE_SCALAR_STMTS (slp_node)[0])
        first_stmt = SLP_TREE_SCALAR_STMTS (slp_node)[0];

      /* Check if the chain of loads is already vectorized.  */
      if (STMT_VINFO_VEC_STMT (vinfo_for_stmt (first_stmt))
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
      first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt));
      group_size = GROUP_SIZE (vinfo_for_stmt (first_stmt));

      /* VEC_NUM is the number of vect stmts to be created for this group.  */
      if (slp)
	{
	  grouped_load = false;
	  vec_num = SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node);
          if (SLP_TREE_LOAD_PERMUTATION (slp_node).exists ())
            slp_perm = true;
	  group_gap = GROUP_GAP (vinfo_for_stmt (first_stmt));
    	}
      else
	{
	  vec_num = group_size;
	  group_gap = 0;
	}
    }
  else
    {
      first_stmt = stmt;
      first_dr = dr;
      group_size = vec_num = 1;
      group_gap = 0;
    }

  alignment_support_scheme = vect_supportable_dr_alignment (first_dr, false);
  gcc_assert (alignment_support_scheme);
  /* Targets with load-lane instructions must not require explicit
     realignment.  */
  gcc_assert (!load_lanes_p
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
      && (TREE_INT_CST_LOW (DR_STEP (dr))
	  % GET_MODE_SIZE (TYPE_MODE (vectype)) != 0))
    {
      gcc_assert (alignment_support_scheme != dr_explicit_realign_optimized);
      compute_in_loop = true;
    }

  if ((alignment_support_scheme == dr_explicit_realign_optimized
       || alignment_support_scheme == dr_explicit_realign)
      && !compute_in_loop)
    {
      msq = vect_setup_realignment (first_stmt, gsi, &realignment_token,
				    alignment_support_scheme, NULL_TREE,
				    &at_loop);
      if (alignment_support_scheme == dr_explicit_realign_optimized)
	{
	  phi = SSA_NAME_DEF_STMT (msq);
	  offset = size_int (TYPE_VECTOR_SUBPARTS (vectype) - 1);
	}
    }
  else
    at_loop = loop;

  if (negative)
    offset = size_int (-TYPE_VECTOR_SUBPARTS (vectype) + 1);

  if (load_lanes_p)
    aggr_type = build_array_type_nelts (elem_type, vec_num * nunits);
  else
    aggr_type = vectype;

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      /* 1. Create the vector or array pointer update chain.  */
      if (j == 0)
	{
	  bool simd_lane_access_p
	    = STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info);
	  if (simd_lane_access_p
	      && TREE_CODE (DR_BASE_ADDRESS (first_dr)) == ADDR_EXPR
	      && VAR_P (TREE_OPERAND (DR_BASE_ADDRESS (first_dr), 0))
	      && integer_zerop (DR_OFFSET (first_dr))
	      && integer_zerop (DR_INIT (first_dr))
	      && alias_sets_conflict_p (get_alias_set (aggr_type),
					get_alias_set (DR_REF (first_dr)))
	      && (alignment_support_scheme == dr_aligned
		  || alignment_support_scheme == dr_unaligned_supported))
	    {
	      dataref_ptr = unshare_expr (DR_BASE_ADDRESS (first_dr));
	      dataref_offset = build_int_cst (reference_alias_ptr_type
					      (DR_REF (first_dr)), 0);
	    }
	  else
	    dataref_ptr
	      = vect_create_data_ref_ptr (first_stmt, aggr_type, at_loop,
					  offset, &dummy, gsi, &ptr_incr,
					  simd_lane_access_p, &inv_p);
	}
      else if (dataref_offset)
	dataref_offset = int_const_binop (PLUS_EXPR, dataref_offset,
					  TYPE_SIZE_UNIT (aggr_type));
      else
        dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi, stmt,
				       TYPE_SIZE_UNIT (aggr_type));

      if (grouped_load || slp_perm)
	dr_chain.create (vec_num);

      if (load_lanes_p)
	{
	  tree vec_array;

	  vec_array = create_vector_array (vectype, vec_num);

	  /* Emit:
	       VEC_ARRAY = LOAD_LANES (MEM_REF[...all elements...]).  */
	  data_ref = create_array_ref (aggr_type, dataref_ptr, first_dr);
	  new_stmt = gimple_build_call_internal (IFN_LOAD_LANES, 1, data_ref);
	  gimple_call_set_lhs (new_stmt, vec_array);
	  vect_finish_stmt_generation (stmt, new_stmt, gsi);

	  /* Extract each vector into an SSA_NAME.  */
	  for (i = 0; i < vec_num; i++)
	    {
	      new_temp = read_vector_array (stmt, gsi, scalar_dest,
					    vec_array, i);
	      dr_chain.quick_push (new_temp);
	    }

	  /* Record the mapping between SSA_NAMEs and statements.  */
	  vect_record_grouped_load_vectors (stmt, dr_chain);
	}
      else
	{
	  for (i = 0; i < vec_num; i++)
	    {
	      if (i > 0)
		dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					       stmt, NULL_TREE);

	      /* 2. Create the vector-load in the loop.  */
	      switch (alignment_support_scheme)
		{
		case dr_aligned:
		case dr_unaligned_supported:
		  {
		    unsigned int align, misalign;

		    data_ref
		      = build2 (MEM_REF, vectype, dataref_ptr,
				dataref_offset
				? dataref_offset
				: build_int_cst (reference_alias_ptr_type
						 (DR_REF (first_dr)), 0));
		    align = TYPE_ALIGN_UNIT (vectype);
		    if (alignment_support_scheme == dr_aligned)
		      {
			gcc_assert (aligned_access_p (first_dr));
			misalign = 0;
		      }
		    else if (DR_MISALIGNMENT (first_dr) == -1)
		      {
			TREE_TYPE (data_ref)
			  = build_aligned_type (TREE_TYPE (data_ref),
						TYPE_ALIGN (elem_type));
			align = TYPE_ALIGN_UNIT (elem_type);
			misalign = 0;
		      }
		    else
		      {
			TREE_TYPE (data_ref)
			  = build_aligned_type (TREE_TYPE (data_ref),
						TYPE_ALIGN (elem_type));
			misalign = DR_MISALIGNMENT (first_dr);
		      }
		    if (dataref_offset == NULL_TREE)
		      set_ptr_info_alignment (get_ptr_info (dataref_ptr),
					      align, misalign);
		    break;
		  }
		case dr_explicit_realign:
		  {
		    tree ptr, bump;
		    tree vs_minus_1;

		    vs_minus_1 = size_int (TYPE_VECTOR_SUBPARTS (vectype) - 1);

		    if (compute_in_loop)
		      msq = vect_setup_realignment (first_stmt, gsi,
						    &realignment_token,
						    dr_explicit_realign,
						    dataref_ptr, NULL);

		    ptr = copy_ssa_name (dataref_ptr, NULL);
		    new_stmt = gimple_build_assign_with_ops
				 (BIT_AND_EXPR, ptr, dataref_ptr,
				  build_int_cst
				  (TREE_TYPE (dataref_ptr),
				   -(HOST_WIDE_INT)TYPE_ALIGN_UNIT (vectype)));
		    vect_finish_stmt_generation (stmt, new_stmt, gsi);
		    data_ref
		      = build2 (MEM_REF, vectype, ptr,
				build_int_cst (reference_alias_ptr_type
						 (DR_REF (first_dr)), 0));
		    vec_dest = vect_create_destination_var (scalar_dest,
							    vectype);
		    new_stmt = gimple_build_assign (vec_dest, data_ref);
		    new_temp = make_ssa_name (vec_dest, new_stmt);
		    gimple_assign_set_lhs (new_stmt, new_temp);
		    gimple_set_vdef (new_stmt, gimple_vdef (stmt));
		    gimple_set_vuse (new_stmt, gimple_vuse (stmt));
		    vect_finish_stmt_generation (stmt, new_stmt, gsi);
		    msq = new_temp;

		    bump = size_binop (MULT_EXPR, vs_minus_1,
				       TYPE_SIZE_UNIT (elem_type));
		    ptr = bump_vector_ptr (dataref_ptr, NULL, gsi, stmt, bump);
		    new_stmt = gimple_build_assign_with_ops
				 (BIT_AND_EXPR, NULL_TREE, ptr,
				  build_int_cst
				  (TREE_TYPE (ptr),
				   -(HOST_WIDE_INT)TYPE_ALIGN_UNIT (vectype)));
		    ptr = copy_ssa_name (dataref_ptr, new_stmt);
		    gimple_assign_set_lhs (new_stmt, ptr);
		    vect_finish_stmt_generation (stmt, new_stmt, gsi);
		    data_ref
		      = build2 (MEM_REF, vectype, ptr,
				build_int_cst (reference_alias_ptr_type
						 (DR_REF (first_dr)), 0));
		    break;
		  }
		case dr_explicit_realign_optimized:
		  new_temp = copy_ssa_name (dataref_ptr, NULL);
		  new_stmt = gimple_build_assign_with_ops
			       (BIT_AND_EXPR, new_temp, dataref_ptr,
				build_int_cst
				  (TREE_TYPE (dataref_ptr),
				   -(HOST_WIDE_INT)TYPE_ALIGN_UNIT (vectype)));
		  vect_finish_stmt_generation (stmt, new_stmt, gsi);
		  data_ref
		    = build2 (MEM_REF, vectype, new_temp,
			      build_int_cst (reference_alias_ptr_type
					       (DR_REF (first_dr)), 0));
		  break;
		default:
		  gcc_unreachable ();
		}
	      vec_dest = vect_create_destination_var (scalar_dest, vectype);
	      new_stmt = gimple_build_assign (vec_dest, data_ref);
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      gimple_assign_set_lhs (new_stmt, new_temp);
	      vect_finish_stmt_generation (stmt, new_stmt, gsi);

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
		  new_stmt
		    = gimple_build_assign_with_ops (REALIGN_LOAD_EXPR,
						    vec_dest, msq, lsq,
						    realignment_token);
		  new_temp = make_ssa_name (vec_dest, new_stmt);
		  gimple_assign_set_lhs (new_stmt, new_temp);
		  vect_finish_stmt_generation (stmt, new_stmt, gsi);

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

	      /* 4. Handle invariant-load.  */
	      if (inv_p && !bb_vinfo)
		{
		  gimple_stmt_iterator gsi2 = *gsi;
		  gcc_assert (!grouped_load);
		  gsi_next (&gsi2);
		  new_temp = vect_init_vector (stmt, scalar_dest,
					       vectype, &gsi2);
		  new_stmt = SSA_NAME_DEF_STMT (new_temp);
		}

	      if (negative)
		{
		  tree perm_mask = perm_mask_for_reverse (vectype);
		  new_temp = permute_vec_elements (new_temp, new_temp,
						   perm_mask, stmt, gsi);
		  new_stmt = SSA_NAME_DEF_STMT (new_temp);
		}

	      /* Collect vector loads and later create their permutation in
		 vect_transform_grouped_load ().  */
	      if (grouped_load || slp_perm)
		dr_chain.quick_push (new_temp);

	      /* Store vector loads in the corresponding SLP_NODE.  */
	      if (slp && !slp_perm)
		SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
	    }
	  /* Bump the vector pointer to account for a gap.  */
	  if (slp && group_gap != 0)
	    {
	      tree bump = size_binop (MULT_EXPR,
				      TYPE_SIZE_UNIT (elem_type),
				      size_int (group_gap));
	      dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, gsi,
					     stmt, bump);
	    }
	}

      if (slp && !slp_perm)
	continue;

      if (slp_perm)
        {
          if (!vect_transform_slp_perm_load (slp_node, dr_chain, gsi, vf,
                                             slp_node_instance, false))
            {
              dr_chain.release ();
              return false;
            }
        }
      else
        {
          if (grouped_load)
  	    {
	      if (!load_lanes_p)
		vect_transform_grouped_load (stmt, dr_chain, group_size, gsi);
	      *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
	    }
          else
	    {
	      if (j == 0)
	        STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
	      else
	        STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
	      prev_stmt_info = vinfo_for_stmt (new_stmt);
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

   Returns whether a COND can be vectorized.  Checks whether
   condition operands are supportable using vec_is_simple_use.  */

static bool
vect_is_simple_cond (tree cond, gimple stmt, loop_vec_info loop_vinfo,
		     bb_vec_info bb_vinfo, tree *comp_vectype)
{
  tree lhs, rhs;
  tree def;
  enum vect_def_type dt;
  tree vectype1 = NULL_TREE, vectype2 = NULL_TREE;

  if (!COMPARISON_CLASS_P (cond))
    return false;

  lhs = TREE_OPERAND (cond, 0);
  rhs = TREE_OPERAND (cond, 1);

  if (TREE_CODE (lhs) == SSA_NAME)
    {
      gimple lhs_def_stmt = SSA_NAME_DEF_STMT (lhs);
      if (!vect_is_simple_use_1 (lhs, stmt, loop_vinfo, bb_vinfo,
				 &lhs_def_stmt, &def, &dt, &vectype1))
	return false;
    }
  else if (TREE_CODE (lhs) != INTEGER_CST && TREE_CODE (lhs) != REAL_CST
	   && TREE_CODE (lhs) != FIXED_CST)
    return false;

  if (TREE_CODE (rhs) == SSA_NAME)
    {
      gimple rhs_def_stmt = SSA_NAME_DEF_STMT (rhs);
      if (!vect_is_simple_use_1 (rhs, stmt, loop_vinfo, bb_vinfo,
				 &rhs_def_stmt, &def, &dt, &vectype2))
	return false;
    }
  else if (TREE_CODE (rhs) != INTEGER_CST && TREE_CODE (rhs) != REAL_CST
	   && TREE_CODE (rhs) != FIXED_CST)
    return false;

  *comp_vectype = vectype1 ? vectype1 : vectype2;
  return true;
}

/* vectorizable_condition.

   Check if STMT is conditional modify expression that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt using VEC_COND_EXPR  to replace it, put it in VEC_STMT, and insert it
   at GSI.

   When STMT is vectorized as nested cycle, REDUC_DEF is the vector variable
   to be used at REDUC_INDEX (in then clause if REDUC_INDEX is 1, and in
   else caluse if it is 2).

   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_condition (gimple stmt, gimple_stmt_iterator *gsi,
			gimple *vec_stmt, tree reduc_def, int reduc_index,
			slp_tree slp_node)
{
  tree scalar_dest = NULL_TREE;
  tree vec_dest = NULL_TREE;
  tree cond_expr, then_clause, else_clause;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree comp_vectype = NULL_TREE;
  tree vec_cond_lhs = NULL_TREE, vec_cond_rhs = NULL_TREE;
  tree vec_then_clause = NULL_TREE, vec_else_clause = NULL_TREE;
  tree vec_compare, vec_cond_expr;
  tree new_temp;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  tree def;
  enum vect_def_type dt, dts[4];
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies;
  enum tree_code code;
  stmt_vec_info prev_stmt_info = NULL;
  int i, j;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  vec<tree> vec_oprnds0 = vNULL;
  vec<tree> vec_oprnds1 = vNULL;
  vec<tree> vec_oprnds2 = vNULL;
  vec<tree> vec_oprnds3 = vNULL;
  tree vec_cmp_type;

  if (slp_node || PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else
    ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;

  gcc_assert (ncopies >= 1);
  if (reduc_index && ncopies > 1)
    return false; /* FORNOW */

  if (reduc_index && STMT_SLP_TYPE (stmt_info))
    return false;

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
                         "value used after loop.");
      return false;
    }

  /* Is vectorizable conditional operation?  */
  if (!is_gimple_assign (stmt))
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (code != COND_EXPR)
    return false;

  cond_expr = gimple_assign_rhs1 (stmt);
  then_clause = gimple_assign_rhs2 (stmt);
  else_clause = gimple_assign_rhs3 (stmt);

  if (!vect_is_simple_cond (cond_expr, stmt, loop_vinfo, bb_vinfo,
			    &comp_vectype)
      || !comp_vectype)
    return false;

  if (TREE_CODE (then_clause) == SSA_NAME)
    {
      gimple then_def_stmt = SSA_NAME_DEF_STMT (then_clause);
      if (!vect_is_simple_use (then_clause, stmt, loop_vinfo, bb_vinfo,
			       &then_def_stmt, &def, &dt))
	return false;
    }
  else if (TREE_CODE (then_clause) != INTEGER_CST
	   && TREE_CODE (then_clause) != REAL_CST
	   && TREE_CODE (then_clause) != FIXED_CST)
    return false;

  if (TREE_CODE (else_clause) == SSA_NAME)
    {
      gimple else_def_stmt = SSA_NAME_DEF_STMT (else_clause);
      if (!vect_is_simple_use (else_clause, stmt, loop_vinfo, bb_vinfo,
			       &else_def_stmt, &def, &dt))
	return false;
    }
  else if (TREE_CODE (else_clause) != INTEGER_CST
	   && TREE_CODE (else_clause) != REAL_CST
	   && TREE_CODE (else_clause) != FIXED_CST)
    return false;

  unsigned int prec = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (vectype)));
  /* The result of a vector comparison should be signed type.  */
  tree cmp_type = build_nonstandard_integer_type (prec, 0);
  vec_cmp_type = get_same_sized_vectype (cmp_type, vectype);
  if (vec_cmp_type == NULL_TREE)
    return false;

  if (!vec_stmt)
    {
      STMT_VINFO_TYPE (stmt_info) = condition_vec_info_type;
      return expand_vec_cond_expr_p (vectype, comp_vectype);
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
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle cond expr.  */
  for (j = 0; j < ncopies; j++)
    {
      gimple new_stmt = NULL;
      if (j == 0)
	{
          if (slp_node)
            {
              vec<tree> ops;
	      ops.create (4);
	      vec<vec<tree> > vec_defs;

	      vec_defs.create (4);
              ops.safe_push (TREE_OPERAND (cond_expr, 0));
              ops.safe_push (TREE_OPERAND (cond_expr, 1));
              ops.safe_push (then_clause);
              ops.safe_push (else_clause);
              vect_get_slp_defs (ops, slp_node, &vec_defs, -1);
	      vec_oprnds3 = vec_defs.pop ();
	      vec_oprnds2 = vec_defs.pop ();
	      vec_oprnds1 = vec_defs.pop ();
	      vec_oprnds0 = vec_defs.pop ();

              ops.release ();
              vec_defs.release ();
            }
          else
            {
	      gimple gtemp;
	      vec_cond_lhs =
	      vect_get_vec_def_for_operand (TREE_OPERAND (cond_expr, 0),
					    stmt, NULL);
	      vect_is_simple_use (TREE_OPERAND (cond_expr, 0), stmt,
				  loop_vinfo, NULL, &gtemp, &def, &dts[0]);

	      vec_cond_rhs =
		vect_get_vec_def_for_operand (TREE_OPERAND (cond_expr, 1),
						stmt, NULL);
	      vect_is_simple_use (TREE_OPERAND (cond_expr, 1), stmt,
				  loop_vinfo, NULL, &gtemp, &def, &dts[1]);
	      if (reduc_index == 1)
		vec_then_clause = reduc_def;
	      else
		{
		  vec_then_clause = vect_get_vec_def_for_operand (then_clause,
		 		  			      stmt, NULL);
	          vect_is_simple_use (then_clause, stmt, loop_vinfo,
					  NULL, &gtemp, &def, &dts[2]);
		}
	      if (reduc_index == 2)
		vec_else_clause = reduc_def;
	      else
		{
		  vec_else_clause = vect_get_vec_def_for_operand (else_clause,
							      stmt, NULL);
		  vect_is_simple_use (else_clause, stmt, loop_vinfo,
				  NULL, &gtemp, &def, &dts[3]);
		}
	    }
	}
      else
	{
	  vec_cond_lhs = vect_get_vec_def_for_stmt_copy (dts[0],
							 vec_oprnds0.pop ());
	  vec_cond_rhs = vect_get_vec_def_for_stmt_copy (dts[1],
							 vec_oprnds1.pop ());
	  vec_then_clause = vect_get_vec_def_for_stmt_copy (dts[2],
							    vec_oprnds2.pop ());
	  vec_else_clause = vect_get_vec_def_for_stmt_copy (dts[3],
							    vec_oprnds3.pop ());
	}

      if (!slp_node)
        {
	  vec_oprnds0.quick_push (vec_cond_lhs);
	  vec_oprnds1.quick_push (vec_cond_rhs);
	  vec_oprnds2.quick_push (vec_then_clause);
	  vec_oprnds3.quick_push (vec_else_clause);
	}

      /* Arguments are ready.  Create the new vector stmt.  */
      FOR_EACH_VEC_ELT (vec_oprnds0, i, vec_cond_lhs)
        {
          vec_cond_rhs = vec_oprnds1[i];
          vec_then_clause = vec_oprnds2[i];
          vec_else_clause = vec_oprnds3[i];

	  vec_compare = build2 (TREE_CODE (cond_expr), vec_cmp_type,
				vec_cond_lhs, vec_cond_rhs);
          vec_cond_expr = build3 (VEC_COND_EXPR, vectype,
 		         vec_compare, vec_then_clause, vec_else_clause);

          new_stmt = gimple_build_assign (vec_dest, vec_cond_expr);
          new_temp = make_ssa_name (vec_dest, new_stmt);
          gimple_assign_set_lhs (new_stmt, new_temp);
          vect_finish_stmt_generation (stmt, new_stmt, gsi);
          if (slp_node)
            SLP_TREE_VEC_STMTS (slp_node).quick_push (new_stmt);
        }

        if (slp_node)
          continue;

        if (j == 0)
          STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
        else
          STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;

        prev_stmt_info = vinfo_for_stmt (new_stmt);
    }

  vec_oprnds0.release ();
  vec_oprnds1.release ();
  vec_oprnds2.release ();
  vec_oprnds3.release ();

  return true;
}


/* Make sure the statement is vectorizable.  */

bool
vect_analyze_stmt (gimple stmt, bool *need_to_vectorize, slp_tree node)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  enum vect_relevant relevance = STMT_VINFO_RELEVANT (stmt_info);
  bool ok;
  tree scalar_type, vectype;
  gimple pattern_stmt;
  gimple_seq pattern_def_seq;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "==> examining statement: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
    }

  if (gimple_has_volatile_ops (stmt))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not vectorized: stmt has volatile operands");

      return false;
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

  pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
  if (!STMT_VINFO_RELEVANT_P (stmt_info)
      && !STMT_VINFO_LIVE_P (stmt_info))
    {
      if (STMT_VINFO_IN_PATTERN_P (stmt_info)
          && pattern_stmt
          && (STMT_VINFO_RELEVANT_P (vinfo_for_stmt (pattern_stmt))
              || STMT_VINFO_LIVE_P (vinfo_for_stmt (pattern_stmt))))
        {
          /* Analyze PATTERN_STMT instead of the original stmt.  */
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
            dump_printf_loc (MSG_NOTE, vect_location, "irrelevant.");

          return true;
        }
    }
  else if (STMT_VINFO_IN_PATTERN_P (stmt_info)
	   && node == NULL
           && pattern_stmt
           && (STMT_VINFO_RELEVANT_P (vinfo_for_stmt (pattern_stmt))
               || STMT_VINFO_LIVE_P (vinfo_for_stmt (pattern_stmt))))
    {
      /* Analyze PATTERN_STMT too.  */
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
                           "==> examining pattern statement: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
        }

      if (!vect_analyze_stmt (pattern_stmt, need_to_vectorize, node))
        return false;
   }

  if (is_pattern_stmt_p (stmt_info)
      && node == NULL
      && (pattern_def_seq = STMT_VINFO_PATTERN_DEF_SEQ (stmt_info)))
    {
      gimple_stmt_iterator si;

      for (si = gsi_start (pattern_def_seq); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple pattern_def_stmt = gsi_stmt (si);
	  if (STMT_VINFO_RELEVANT_P (vinfo_for_stmt (pattern_def_stmt))
	      || STMT_VINFO_LIVE_P (vinfo_for_stmt (pattern_def_stmt)))
	    {
	      /* Analyze def stmt of STMT if it's a pattern stmt.  */
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location,
                                   "==> examining pattern def statement: ");
		  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, pattern_def_stmt, 0);
		}

	      if (!vect_analyze_stmt (pattern_def_stmt,
				      need_to_vectorize, node))
		return false;
	    }
	}
    }

  switch (STMT_VINFO_DEF_TYPE (stmt_info))
    {
      case vect_internal_def:
        break;

      case vect_reduction_def:
      case vect_nested_cycle:
         gcc_assert (!bb_vinfo && (relevance == vect_used_in_outer
                     || relevance == vect_used_in_outer_by_reduction
                     || relevance == vect_unused_in_scope));
         break;

      case vect_induction_def:
      case vect_constant_def:
      case vect_external_def:
      case vect_unknown_def_type:
      default:
        gcc_unreachable ();
    }

  if (bb_vinfo)
    {
      gcc_assert (PURE_SLP_STMT (stmt_info));

      scalar_type = TREE_TYPE (gimple_get_lhs (stmt));
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
                           "get vectype for scalar type:  ");
          dump_generic_expr (MSG_NOTE, TDF_SLIM, scalar_type);
        }

      vectype = get_vectype_for_scalar_type (scalar_type);
      if (!vectype)
        {
          if (dump_enabled_p ())
            {
               dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                "not SLPed: unsupported data-type ");
               dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
                                  scalar_type);
            }
          return false;
        }

      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location, "vectype:  ");
          dump_generic_expr (MSG_NOTE, TDF_SLIM, vectype);
        }

      STMT_VINFO_VECTYPE (stmt_info) = vectype;
   }

  if (STMT_VINFO_RELEVANT_P (stmt_info))
    {
      gcc_assert (!VECTOR_MODE_P (TYPE_MODE (gimple_expr_type (stmt))));
      gcc_assert (STMT_VINFO_VECTYPE (stmt_info));
      *need_to_vectorize = true;
    }

   ok = true;
   if (!bb_vinfo
       && (STMT_VINFO_RELEVANT_P (stmt_info)
           || STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def))
      ok = (vectorizable_conversion (stmt, NULL, NULL, NULL)
            || vectorizable_shift (stmt, NULL, NULL, NULL)
            || vectorizable_operation (stmt, NULL, NULL, NULL)
            || vectorizable_assignment (stmt, NULL, NULL, NULL)
            || vectorizable_load (stmt, NULL, NULL, NULL, NULL)
	    || vectorizable_call (stmt, NULL, NULL, NULL)
            || vectorizable_store (stmt, NULL, NULL, NULL)
            || vectorizable_reduction (stmt, NULL, NULL, NULL)
            || vectorizable_condition (stmt, NULL, NULL, NULL, 0, NULL));
    else
      {
        if (bb_vinfo)
	  ok = (vectorizable_conversion (stmt, NULL, NULL, node)
		|| vectorizable_shift (stmt, NULL, NULL, node)
                || vectorizable_operation (stmt, NULL, NULL, node)
                || vectorizable_assignment (stmt, NULL, NULL, node)
                || vectorizable_load (stmt, NULL, NULL, node, NULL)
		|| vectorizable_call (stmt, NULL, NULL, node)
                || vectorizable_store (stmt, NULL, NULL, node)
                || vectorizable_condition (stmt, NULL, NULL, NULL, 0, node));
      }

  if (!ok)
    {
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "not vectorized: relevant stmt not ");
          dump_printf (MSG_MISSED_OPTIMIZATION, "supported: ");
          dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
        }

      return false;
    }

  if (bb_vinfo)
    return true;

  /* Stmts that are (also) "live" (i.e. - that are used out of the loop)
      need extra handling, except for vectorizable reductions.  */
  if (STMT_VINFO_LIVE_P (stmt_info)
      && STMT_VINFO_TYPE (stmt_info) != reduc_vec_info_type)
    ok = vectorizable_live_operation (stmt, NULL, NULL);

  if (!ok)
    {
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                           "not vectorized: live stmt not ");
          dump_printf (MSG_MISSED_OPTIMIZATION,  "supported: ");
          dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
        }

       return false;
    }

  return true;
}


/* Function vect_transform_stmt.

   Create a vectorized stmt to replace STMT, and insert it at BSI.  */

bool
vect_transform_stmt (gimple stmt, gimple_stmt_iterator *gsi,
		     bool *grouped_store, slp_tree slp_node,
                     slp_instance slp_node_instance)
{
  bool is_store = false;
  gimple vec_stmt = NULL;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  bool done;

  switch (STMT_VINFO_TYPE (stmt_info))
    {
    case type_demotion_vec_info_type:
    case type_promotion_vec_info_type:
    case type_conversion_vec_info_type:
      done = vectorizable_conversion (stmt, gsi, &vec_stmt, slp_node);
      gcc_assert (done);
      break;

    case induc_vec_info_type:
      gcc_assert (!slp_node);
      done = vectorizable_induction (stmt, gsi, &vec_stmt);
      gcc_assert (done);
      break;

    case shift_vec_info_type:
      done = vectorizable_shift (stmt, gsi, &vec_stmt, slp_node);
      gcc_assert (done);
      break;

    case op_vec_info_type:
      done = vectorizable_operation (stmt, gsi, &vec_stmt, slp_node);
      gcc_assert (done);
      break;

    case assignment_vec_info_type:
      done = vectorizable_assignment (stmt, gsi, &vec_stmt, slp_node);
      gcc_assert (done);
      break;

    case load_vec_info_type:
      done = vectorizable_load (stmt, gsi, &vec_stmt, slp_node,
                                slp_node_instance);
      gcc_assert (done);
      break;

    case store_vec_info_type:
      done = vectorizable_store (stmt, gsi, &vec_stmt, slp_node);
      gcc_assert (done);
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info) && !slp_node)
	{
	  /* In case of interleaving, the whole chain is vectorized when the
	     last store in the chain is reached.  Store stmts before the last
	     one are skipped, and there vec_stmt_info shouldn't be freed
	     meanwhile.  */
	  *grouped_store = true;
	  if (STMT_VINFO_VEC_STMT (stmt_info))
	    is_store = true;
	  }
      else
	is_store = true;
      break;

    case condition_vec_info_type:
      done = vectorizable_condition (stmt, gsi, &vec_stmt, NULL, 0, slp_node);
      gcc_assert (done);
      break;

    case call_vec_info_type:
      done = vectorizable_call (stmt, gsi, &vec_stmt, slp_node);
      stmt = gsi_stmt (*gsi);
      break;

    case reduc_vec_info_type:
      done = vectorizable_reduction (stmt, gsi, &vec_stmt, slp_node);
      gcc_assert (done);
      break;

    default:
      if (!STMT_VINFO_LIVE_P (stmt_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                             "stmt not supported.");
	  gcc_unreachable ();
	}
    }

  /* Handle inner-loop stmts whose DEF is used in the loop-nest that
     is being vectorized, but outside the immediately enclosing loop.  */
  if (vec_stmt
      && STMT_VINFO_LOOP_VINFO (stmt_info)
      && nested_in_vect_loop_p (LOOP_VINFO_LOOP (
                                STMT_VINFO_LOOP_VINFO (stmt_info)), stmt)
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
      gimple exit_phi;

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "Record the vdef for outer-loop vectorization.");

      /* Find the relevant loop-exit phi-node, and reord the vec_stmt there
        (to be used when vectorizing outer-loop stmts that use the DEF of
        STMT).  */
      if (gimple_code (stmt) == GIMPLE_PHI)
        scalar_dest = PHI_RESULT (stmt);
      else
        scalar_dest = gimple_assign_lhs (stmt);

      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, scalar_dest)
       {
         if (!flow_bb_inside_loop_p (innerloop, gimple_bb (USE_STMT (use_p))))
           {
             exit_phi = USE_STMT (use_p);
             STMT_VINFO_VEC_STMT (vinfo_for_stmt (exit_phi)) = vec_stmt;
           }
       }
    }

  /* Handle stmts whose DEF is used outside the loop-nest that is
     being vectorized.  */
  if (STMT_VINFO_LIVE_P (stmt_info)
      && STMT_VINFO_TYPE (stmt_info) != reduc_vec_info_type)
    {
      done = vectorizable_live_operation (stmt, gsi, &vec_stmt);
      gcc_assert (done);
    }

  if (vec_stmt)
    STMT_VINFO_VEC_STMT (stmt_info) = vec_stmt;

  return is_store;
}


/* Remove a group of stores (for SLP or interleaving), free their
   stmt_vec_info.  */

void
vect_remove_stores (gimple first_stmt)
{
  gimple next = first_stmt;
  gimple tmp;
  gimple_stmt_iterator next_si;

  while (next)
    {
      stmt_vec_info stmt_info = vinfo_for_stmt (next);

      tmp = GROUP_NEXT_ELEMENT (stmt_info);
      if (is_pattern_stmt_p (stmt_info))
	next = STMT_VINFO_RELATED_STMT (stmt_info);
      /* Free the attached stmt_vec_info and remove the stmt.  */
      next_si = gsi_for_stmt (next);
      unlink_stmt_vdef (next);
      gsi_remove (&next_si, true);
      release_defs (next);
      free_stmt_vec_info (next);
      next = tmp;
    }
}


/* Function new_stmt_vec_info.

   Create and initialize a new stmt_vec_info struct for STMT.  */

stmt_vec_info
new_stmt_vec_info (gimple stmt, loop_vec_info loop_vinfo,
                   bb_vec_info bb_vinfo)
{
  stmt_vec_info res;
  res = (stmt_vec_info) xcalloc (1, sizeof (struct _stmt_vec_info));

  STMT_VINFO_TYPE (res) = undef_vec_info_type;
  STMT_VINFO_STMT (res) = stmt;
  STMT_VINFO_LOOP_VINFO (res) = loop_vinfo;
  STMT_VINFO_BB_VINFO (res) = bb_vinfo;
  STMT_VINFO_RELEVANT (res) = vect_unused_in_scope;
  STMT_VINFO_LIVE_P (res) = false;
  STMT_VINFO_VECTYPE (res) = NULL;
  STMT_VINFO_VEC_STMT (res) = NULL;
  STMT_VINFO_VECTORIZABLE (res) = true;
  STMT_VINFO_IN_PATTERN_P (res) = false;
  STMT_VINFO_RELATED_STMT (res) = NULL;
  STMT_VINFO_PATTERN_DEF_SEQ (res) = NULL;
  STMT_VINFO_DATA_REF (res) = NULL;

  STMT_VINFO_DR_BASE_ADDRESS (res) = NULL;
  STMT_VINFO_DR_OFFSET (res) = NULL;
  STMT_VINFO_DR_INIT (res) = NULL;
  STMT_VINFO_DR_STEP (res) = NULL;
  STMT_VINFO_DR_ALIGNED_TO (res) = NULL;

  if (gimple_code (stmt) == GIMPLE_PHI
      && is_loop_header_bb_p (gimple_bb (stmt)))
    STMT_VINFO_DEF_TYPE (res) = vect_unknown_def_type;
  else
    STMT_VINFO_DEF_TYPE (res) = vect_internal_def;

  STMT_VINFO_SAME_ALIGN_REFS (res).create (0);
  STMT_SLP_TYPE (res) = loop_vect;
  GROUP_FIRST_ELEMENT (res) = NULL;
  GROUP_NEXT_ELEMENT (res) = NULL;
  GROUP_SIZE (res) = 0;
  GROUP_STORE_COUNT (res) = 0;
  GROUP_GAP (res) = 0;
  GROUP_SAME_DR_STMT (res) = NULL;

  return res;
}


/* Create a hash table for stmt_vec_info. */

void
init_stmt_vec_info_vec (void)
{
  gcc_assert (!stmt_vec_info_vec.exists ());
  stmt_vec_info_vec.create (50);
}


/* Free hash table for stmt_vec_info. */

void
free_stmt_vec_info_vec (void)
{
  unsigned int i;
  vec_void_p info;
  FOR_EACH_VEC_ELT (stmt_vec_info_vec, i, info)
    if (info != NULL)
      free_stmt_vec_info (STMT_VINFO_STMT ((stmt_vec_info) info));
  gcc_assert (stmt_vec_info_vec.exists ());
  stmt_vec_info_vec.release ();
}


/* Free stmt vectorization related info.  */

void
free_stmt_vec_info (gimple stmt)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

  if (!stmt_info)
    return;

  /* Check if this statement has a related "pattern stmt"
     (introduced by the vectorizer during the pattern recognition
     pass).  Free pattern's stmt_vec_info and def stmt's stmt_vec_info
     too.  */
  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
    {
      stmt_vec_info patt_info
	= vinfo_for_stmt (STMT_VINFO_RELATED_STMT (stmt_info));
      if (patt_info)
	{
	  gimple_seq seq = STMT_VINFO_PATTERN_DEF_SEQ (patt_info);
	  if (seq)
	    {
	      gimple_stmt_iterator si;
	      for (si = gsi_start (seq); !gsi_end_p (si); gsi_next (&si))
		free_stmt_vec_info (gsi_stmt (si));
	    }
	  free_stmt_vec_info (STMT_VINFO_RELATED_STMT (stmt_info));
	}
    }

  STMT_VINFO_SAME_ALIGN_REFS (stmt_info).release ();
  set_vinfo_for_stmt (stmt, NULL);
  free (stmt_info);
}


/* Function get_vectype_for_scalar_type_and_size.

   Returns the vector type corresponding to SCALAR_TYPE  and SIZE as supported
   by the target.  */

static tree
get_vectype_for_scalar_type_and_size (tree scalar_type, unsigned size)
{
  enum machine_mode inner_mode = TYPE_MODE (scalar_type);
  enum machine_mode simd_mode;
  unsigned int nbytes = GET_MODE_SIZE (inner_mode);
  int nunits;
  tree vectype;

  if (nbytes == 0)
    return NULL_TREE;

  if (GET_MODE_CLASS (inner_mode) != MODE_INT
      && GET_MODE_CLASS (inner_mode) != MODE_FLOAT)
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

  /* If no size was supplied use the mode the target prefers.   Otherwise
     lookup a vector mode of the specified size.  */
  if (size == 0)
    simd_mode = targetm.vectorize.preferred_simd_mode (inner_mode);
  else
    simd_mode = mode_for_vector (inner_mode, size / nbytes);
  nunits = GET_MODE_SIZE (simd_mode) / nbytes;
  if (nunits <= 1)
    return NULL_TREE;

  vectype = build_vector_type (scalar_type, nunits);

  if (!VECTOR_MODE_P (TYPE_MODE (vectype))
      && !INTEGRAL_MODE_P (TYPE_MODE (vectype)))
    return NULL_TREE;

  return vectype;
}

unsigned int current_vector_size;

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
      && current_vector_size == 0)
    current_vector_size = GET_MODE_SIZE (TYPE_MODE (vectype));
  return vectype;
}

/* Function get_same_sized_vectype

   Returns a vector type corresponding to SCALAR_TYPE of size
   VECTOR_TYPE if supported by the target.  */

tree
get_same_sized_vectype (tree scalar_type, tree vector_type)
{
  return get_vectype_for_scalar_type_and_size
	   (scalar_type, GET_MODE_SIZE (TYPE_MODE (vector_type)));
}

/* Function vect_is_simple_use.

   Input:
   LOOP_VINFO - the vect info of the loop that is being vectorized.
   BB_VINFO - the vect info of the basic block that is being vectorized.
   OPERAND - operand of STMT in the loop or bb.
   DEF - the defining stmt in case OPERAND is an SSA_NAME.

   Returns whether a stmt with OPERAND can be vectorized.
   For loops, supportable operands are constants, loop invariants, and operands
   that are defined by the current iteration of the loop.  Unsupportable
   operands are those that are defined by a previous iteration of the loop (as
   is the case in reduction/induction computations).
   For basic blocks, supportable operands are constants and bb invariants.
   For now, operands defined outside the basic block are not supported.  */

bool
vect_is_simple_use (tree operand, gimple stmt, loop_vec_info loop_vinfo,
                    bb_vec_info bb_vinfo, gimple *def_stmt,
		    tree *def, enum vect_def_type *dt)
{
  basic_block bb;
  stmt_vec_info stmt_vinfo;
  struct loop *loop = NULL;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  *def_stmt = NULL;
  *def = NULL_TREE;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "vect_is_simple_use: operand ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, operand);
    }

  if (CONSTANT_CLASS_P (operand))
    {
      *dt = vect_constant_def;
      return true;
    }

  if (is_gimple_min_invariant (operand))
    {
      *def = operand;
      *dt = vect_external_def;
      return true;
    }

  if (TREE_CODE (operand) == PAREN_EXPR)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location, "non-associatable copy.");
      operand = TREE_OPERAND (operand, 0);
    }

  if (TREE_CODE (operand) != SSA_NAME)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "not ssa-name.");
      return false;
    }

  *def_stmt = SSA_NAME_DEF_STMT (operand);
  if (*def_stmt == NULL)
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "no def_stmt.");
      return false;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "def_stmt: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, *def_stmt, 0);
    }

  /* Empty stmt is expected only in case of a function argument.
     (Otherwise - we expect a phi_node or a GIMPLE_ASSIGN).  */
  if (gimple_nop_p (*def_stmt))
    {
      *def = operand;
      *dt = vect_external_def;
      return true;
    }

  bb = gimple_bb (*def_stmt);

  if ((loop && !flow_bb_inside_loop_p (loop, bb))
      || (!loop && bb != BB_VINFO_BB (bb_vinfo))
      || (!loop && gimple_code (*def_stmt) == GIMPLE_PHI))
    *dt = vect_external_def;
  else
    {
      stmt_vinfo = vinfo_for_stmt (*def_stmt);
      *dt = STMT_VINFO_DEF_TYPE (stmt_vinfo);
    }

  if (*dt == vect_unknown_def_type
      || (stmt
	  && *dt == vect_double_reduction_def
	  && gimple_code (stmt) != GIMPLE_PHI))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "Unsupported pattern.");
      return false;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "type of def: %d.", *dt);

  switch (gimple_code (*def_stmt))
    {
    case GIMPLE_PHI:
      *def = gimple_phi_result (*def_stmt);
      break;

    case GIMPLE_ASSIGN:
      *def = gimple_assign_lhs (*def_stmt);
      break;

    case GIMPLE_CALL:
      *def = gimple_call_lhs (*def_stmt);
      if (*def != NULL)
	break;
      /* FALLTHRU */
    default:
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "unsupported defining stmt: ");
      return false;
    }

  return true;
}

/* Function vect_is_simple_use_1.

   Same as vect_is_simple_use_1 but also determines the vector operand
   type of OPERAND and stores it to *VECTYPE.  If the definition of
   OPERAND is vect_uninitialized_def, vect_constant_def or
   vect_external_def *VECTYPE will be set to NULL_TREE and the caller
   is responsible to compute the best suited vector type for the
   scalar operand.  */

bool
vect_is_simple_use_1 (tree operand, gimple stmt, loop_vec_info loop_vinfo,
		      bb_vec_info bb_vinfo, gimple *def_stmt,
		      tree *def, enum vect_def_type *dt, tree *vectype)
{
  if (!vect_is_simple_use (operand, stmt, loop_vinfo, bb_vinfo, def_stmt,
			   def, dt))
    return false;

  /* Now get a vector type if the def is internal, otherwise supply
     NULL_TREE and leave it up to the caller to figure out a proper
     type for the use stmt.  */
  if (*dt == vect_internal_def
      || *dt == vect_induction_def
      || *dt == vect_reduction_def
      || *dt == vect_double_reduction_def
      || *dt == vect_nested_cycle)
    {
      stmt_vec_info stmt_info = vinfo_for_stmt (*def_stmt);

      if (STMT_VINFO_IN_PATTERN_P (stmt_info)
          && !STMT_VINFO_RELEVANT (stmt_info)
          && !STMT_VINFO_LIVE_P (stmt_info))
	stmt_info = vinfo_for_stmt (STMT_VINFO_RELATED_STMT (stmt_info));

      *vectype = STMT_VINFO_VECTYPE (stmt_info);
      gcc_assert (*vectype != NULL_TREE);
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

   Widening operations we currently support are NOP (CONVERT), FLOAT
   and WIDEN_MULT.  This function checks if these operations are supported
   by the target platform either directly (via vector tree-codes), or via
   target builtins.

   Output:
   - CODE1 and CODE2 are codes of vector operations to be used when
   vectorizing the operation, if available.
   - MULTI_STEP_CVT determines the number of required intermediate steps in
   case of multi-step conversion (like char->short->int - in that case
   MULTI_STEP_CVT will be 1).
   - INTERM_TYPES contains the intermediate type required to perform the
   widening operation (short in the above example).  */

bool
supportable_widening_operation (enum tree_code code, gimple stmt,
				tree vectype_out, tree vectype_in,
                                enum tree_code *code1, enum tree_code *code2,
                                int *multi_step_cvt,
                                vec<tree> *interm_types)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *vect_loop = NULL;
  enum machine_mode vec_mode;
  enum insn_code icode1, icode2;
  optab optab1, optab2;
  tree vectype = vectype_in;
  tree wide_vectype = vectype_out;
  enum tree_code c1, c2;
  int i;
  tree prev_type, intermediate_type;
  enum machine_mode intermediate_mode, prev_mode;
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
	  && !nested_in_vect_loop_p (vect_loop, stmt)
	  && supportable_widening_operation (VEC_WIDEN_MULT_EVEN_EXPR,
					     stmt, vectype_out, vectype_in,
					     code1, code2, multi_step_cvt,
					     interm_types))
	return true;
      c1 = VEC_WIDEN_MULT_LO_EXPR;
      c2 = VEC_WIDEN_MULT_HI_EXPR;
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
      /* ??? Not yet implemented due to missing VEC_UNPACK_FIX_TRUNC_HI_EXPR/
	 VEC_UNPACK_FIX_TRUNC_LO_EXPR tree codes and optabs used for
	 computing the operation.  */
      return false;

    default:
      gcc_unreachable ();
    }

  if (BYTES_BIG_ENDIAN && c1 != VEC_WIDEN_MULT_EVEN_EXPR)
    {
      enum tree_code ctmp = c1;
      c1 = c2;
      c2 = ctmp;
    }

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
    return true;

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
	return true;

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

   Narrowing operations we currently support are NOP (CONVERT) and
   FIX_TRUNC.  This function checks if these operations are supported by
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
  enum machine_mode vec_mode;
  enum insn_code icode1;
  optab optab1, interm_optab;
  tree vectype = vectype_in;
  tree narrow_vectype = vectype_out;
  enum tree_code c1;
  tree intermediate_type;
  enum machine_mode intermediate_mode, prev_mode;
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
      /* ??? Not yet implemented due to missing VEC_PACK_FLOAT_EXPR
	 tree code and optabs used for computing the operation.  */
      return false;

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
    return true;

  /* Check if it's a multi-step conversion that can be done using intermediate
     types.  */
  prev_mode = vec_mode;
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
	return true;

      prev_mode = intermediate_mode;
      optab1 = interm_optab;
    }

  interm_types->release ();
  return false;
}
