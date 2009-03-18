/* Loop Vectorization
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

/* Loop Vectorization Pass.

   This pass tries to vectorize loops. This first implementation focuses on
   simple inner-most loops, with no conditional control flow, and a set of
   simple operations which vector form can be expressed using existing
   tree codes (PLUS, MULT etc).

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
   accesses are required to have a  simple (consecutive) access pattern.

   Analysis phase:
   ===============
	The driver for the analysis phase is vect_analyze_loop_nest().
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
   the loop that needs to be vectorized. It insert the vector code sequence
   just before the scalar stmt S, and records a pointer to the vector code
   in STMT_VINFO_VEC_STMT (stmt_info) (stmt_info is the stmt_vec_info struct 
   attached to S). This pointer will be used for the vectorization of following
   stmts which use the def of stmt S. Stmt S is removed if it writes to memory;
   otherwise, we rely on dead code elimination for removing it.

	For example, say stmt S1 was vectorized into stmt VS1:

   VS1: vb = px[i];
   S1:	b = x[i];    STMT_VINFO_VEC_STMT (stmt_info (S1)) = VS1
   S2:  a = b;

   To vectorize stmt S2, the vectorizer first finds the stmt that defines
   the operand 'b' (S1), and gets the relevant vector def 'vb' from the
   vector stmt VS1 pointed to by STMT_VINFO_VEC_STMT (stmt_info (S1)). The
   resulting sequence would be:

   VS1: vb = px[i];
   S1:	b = x[i];	STMT_VINFO_VEC_STMT (stmt_info (S1)) = VS1
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "target.h"
#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "expr.h"
#include "recog.h"
#include "optabs.h"
#include "params.h"
#include "toplev.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "input.h"
#include "hashtab.h"
#include "tree-vectorizer.h"
#include "tree-pass.h"
#include "langhooks.h"

/*************************************************************************
  General Vectorization Utilities
 *************************************************************************/

/* vect_dump will be set to stderr or dump_file if exist.  */
FILE *vect_dump;

/* vect_verbosity_level set to an invalid value 
   to mark that it's uninitialized.  */
enum verbosity_levels vect_verbosity_level = MAX_VERBOSITY_LEVEL;

/* Loop location.  */
static LOC vect_loop_location;

/* Bitmap of virtual variables to be renamed.  */
bitmap vect_memsyms_to_rename;

/* Vector mapping GIMPLE stmt to stmt_vec_info. */
VEC(vec_void_p,heap) *stmt_vec_info_vec;


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


/* Renames the variables in basic block BB.  */

void
rename_variables_in_bb (basic_block bb)
{
  gimple_stmt_iterator gsi;
  gimple stmt;
  use_operand_p use_p;
  ssa_op_iter iter;
  edge e;
  edge_iterator ei;
  struct loop *loop = bb->loop_father;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	rename_use_op (use_p);
    }

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (!flow_bb_inside_loop_p (loop, e->dest))
	continue;
      for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
        rename_use_op (PHI_ARG_DEF_PTR_FROM_EDGE (gsi_stmt (gsi), e));
    }
}


/* Renames variables in new generated LOOP.  */

void
rename_variables_in_loop (struct loop *loop)
{
  unsigned i;
  basic_block *bbs;

  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    rename_variables_in_bb (bbs[i]);

  free (bbs);
}


/* Update the PHI nodes of NEW_LOOP.

   NEW_LOOP is a duplicate of ORIG_LOOP.
   AFTER indicates whether NEW_LOOP executes before or after ORIG_LOOP:
   AFTER is true if NEW_LOOP executes after ORIG_LOOP, and false if it
   executes before it.  */

static void
slpeel_update_phis_for_duplicate_loop (struct loop *orig_loop,
				       struct loop *new_loop, bool after)
{
  tree new_ssa_name;
  gimple phi_new, phi_orig;
  tree def;
  edge orig_loop_latch = loop_latch_edge (orig_loop);
  edge orig_entry_e = loop_preheader_edge (orig_loop);
  edge new_loop_exit_e = single_exit (new_loop);
  edge new_loop_entry_e = loop_preheader_edge (new_loop);
  edge entry_arg_e = (after ? orig_loop_latch : orig_entry_e);
  gimple_stmt_iterator gsi_new, gsi_orig;

  /*
     step 1. For each loop-header-phi:
             Add the first phi argument for the phi in NEW_LOOP
            (the one associated with the entry of NEW_LOOP)

     step 2. For each loop-header-phi:
             Add the second phi argument for the phi in NEW_LOOP
            (the one associated with the latch of NEW_LOOP)

     step 3. Update the phis in the successor block of NEW_LOOP.

        case 1: NEW_LOOP was placed before ORIG_LOOP:
                The successor block of NEW_LOOP is the header of ORIG_LOOP.
                Updating the phis in the successor block can therefore be done
                along with the scanning of the loop header phis, because the
                header blocks of ORIG_LOOP and NEW_LOOP have exactly the same
                phi nodes, organized in the same order.

        case 2: NEW_LOOP was placed after ORIG_LOOP:
                The successor block of NEW_LOOP is the original exit block of 
                ORIG_LOOP - the phis to be updated are the loop-closed-ssa phis.
                We postpone updating these phis to a later stage (when
                loop guards are added).
   */


  /* Scan the phis in the headers of the old and new loops
     (they are organized in exactly the same order).  */

  for (gsi_new = gsi_start_phis (new_loop->header),
       gsi_orig = gsi_start_phis (orig_loop->header);
       !gsi_end_p (gsi_new) && !gsi_end_p (gsi_orig);
       gsi_next (&gsi_new), gsi_next (&gsi_orig))
    {
      phi_new = gsi_stmt (gsi_new);
      phi_orig = gsi_stmt (gsi_orig);

      /* step 1.  */
      def = PHI_ARG_DEF_FROM_EDGE (phi_orig, entry_arg_e);
      add_phi_arg (phi_new, def, new_loop_entry_e);

      /* step 2.  */
      def = PHI_ARG_DEF_FROM_EDGE (phi_orig, orig_loop_latch);
      if (TREE_CODE (def) != SSA_NAME)
        continue;

      new_ssa_name = get_current_def (def);
      if (!new_ssa_name)
	{
	  /* This only happens if there are no definitions
	     inside the loop. use the phi_result in this case.  */
	  new_ssa_name = PHI_RESULT (phi_new);
	}

      /* An ordinary ssa name defined in the loop.  */
      add_phi_arg (phi_new, new_ssa_name, loop_latch_edge (new_loop));

      /* step 3 (case 1).  */
      if (!after)
        {
          gcc_assert (new_loop_exit_e == orig_entry_e);
          SET_PHI_ARG_DEF (phi_orig,
                           new_loop_exit_e->dest_idx,
                           new_ssa_name);
        }
    }
}


/* Update PHI nodes for a guard of the LOOP.

   Input:
   - LOOP, GUARD_EDGE: LOOP is a loop for which we added guard code that
        controls whether LOOP is to be executed.  GUARD_EDGE is the edge that
        originates from the guard-bb, skips LOOP and reaches the (unique) exit
        bb of LOOP.  This loop-exit-bb is an empty bb with one successor.
        We denote this bb NEW_MERGE_BB because before the guard code was added
        it had a single predecessor (the LOOP header), and now it became a merge
        point of two paths - the path that ends with the LOOP exit-edge, and
        the path that ends with GUARD_EDGE.
   - NEW_EXIT_BB: New basic block that is added by this function between LOOP
        and NEW_MERGE_BB. It is used to place loop-closed-ssa-form exit-phis.

   ===> The CFG before the guard-code was added:
        LOOP_header_bb:
          loop_body
          if (exit_loop) goto update_bb
          else           goto LOOP_header_bb
        update_bb:

   ==> The CFG after the guard-code was added:
        guard_bb:
          if (LOOP_guard_condition) goto new_merge_bb
          else                      goto LOOP_header_bb
        LOOP_header_bb:
          loop_body
          if (exit_loop_condition) goto new_merge_bb
          else                     goto LOOP_header_bb
        new_merge_bb:
          goto update_bb
        update_bb:

   ==> The CFG after this function:
        guard_bb:
          if (LOOP_guard_condition) goto new_merge_bb
          else                      goto LOOP_header_bb
        LOOP_header_bb:
          loop_body
          if (exit_loop_condition) goto new_exit_bb
          else                     goto LOOP_header_bb
        new_exit_bb:
        new_merge_bb:
          goto update_bb
        update_bb:

   This function:
   1. creates and updates the relevant phi nodes to account for the new
      incoming edge (GUARD_EDGE) into NEW_MERGE_BB. This involves:
      1.1. Create phi nodes at NEW_MERGE_BB.
      1.2. Update the phi nodes at the successor of NEW_MERGE_BB (denoted
           UPDATE_BB).  UPDATE_BB was the exit-bb of LOOP before NEW_MERGE_BB
   2. preserves loop-closed-ssa-form by creating the required phi nodes
      at the exit of LOOP (i.e, in NEW_EXIT_BB).

   There are two flavors to this function:

   slpeel_update_phi_nodes_for_guard1:
     Here the guard controls whether we enter or skip LOOP, where LOOP is a
     prolog_loop (loop1 below), and the new phis created in NEW_MERGE_BB are
     for variables that have phis in the loop header.

   slpeel_update_phi_nodes_for_guard2:
     Here the guard controls whether we enter or skip LOOP, where LOOP is an
     epilog_loop (loop2 below), and the new phis created in NEW_MERGE_BB are
     for variables that have phis in the loop exit.

   I.E., the overall structure is:

        loop1_preheader_bb:
                guard1 (goto loop1/merge1_bb)
        loop1
        loop1_exit_bb:
                guard2 (goto merge1_bb/merge2_bb)
        merge1_bb
        loop2
        loop2_exit_bb
        merge2_bb
        next_bb

   slpeel_update_phi_nodes_for_guard1 takes care of creating phis in
   loop1_exit_bb and merge1_bb. These are entry phis (phis for the vars
   that have phis in loop1->header).

   slpeel_update_phi_nodes_for_guard2 takes care of creating phis in
   loop2_exit_bb and merge2_bb. These are exit phis (phis for the vars
   that have phis in next_bb). It also adds some of these phis to
   loop1_exit_bb.

   slpeel_update_phi_nodes_for_guard1 is always called before
   slpeel_update_phi_nodes_for_guard2. They are both needed in order
   to create correct data-flow and loop-closed-ssa-form.

   Generally slpeel_update_phi_nodes_for_guard1 creates phis for variables
   that change between iterations of a loop (and therefore have a phi-node
   at the loop entry), whereas slpeel_update_phi_nodes_for_guard2 creates
   phis for variables that are used out of the loop (and therefore have 
   loop-closed exit phis). Some variables may be both updated between 
   iterations and used after the loop. This is why in loop1_exit_bb we
   may need both entry_phis (created by slpeel_update_phi_nodes_for_guard1)
   and exit phis (created by slpeel_update_phi_nodes_for_guard2).

   - IS_NEW_LOOP: if IS_NEW_LOOP is true, then LOOP is a newly created copy of
     an original loop. i.e., we have:

           orig_loop
           guard_bb (goto LOOP/new_merge)
           new_loop <-- LOOP
           new_exit
           new_merge
           next_bb

     If IS_NEW_LOOP is false, then LOOP is an original loop, in which case we
     have:

           new_loop
           guard_bb (goto LOOP/new_merge)
           orig_loop <-- LOOP
           new_exit
           new_merge
           next_bb

     The SSA names defined in the original loop have a current
     reaching definition that that records the corresponding new
     ssa-name used in the new duplicated loop copy.
  */

/* Function slpeel_update_phi_nodes_for_guard1
   
   Input:
   - GUARD_EDGE, LOOP, IS_NEW_LOOP, NEW_EXIT_BB - as explained above.
   - DEFS - a bitmap of ssa names to mark new names for which we recorded
            information. 
   
   In the context of the overall structure, we have:

        loop1_preheader_bb: 
                guard1 (goto loop1/merge1_bb)
LOOP->  loop1
        loop1_exit_bb:
                guard2 (goto merge1_bb/merge2_bb)
        merge1_bb
        loop2
        loop2_exit_bb
        merge2_bb
        next_bb

   For each name updated between loop iterations (i.e - for each name that has
   an entry (loop-header) phi in LOOP) we create a new phi in:
   1. merge1_bb (to account for the edge from guard1)
   2. loop1_exit_bb (an exit-phi to keep LOOP in loop-closed form)
*/

static void
slpeel_update_phi_nodes_for_guard1 (edge guard_edge, struct loop *loop,
                                    bool is_new_loop, basic_block *new_exit_bb,
                                    bitmap *defs)
{
  gimple orig_phi, new_phi;
  gimple update_phi, update_phi2;
  tree guard_arg, loop_arg;
  basic_block new_merge_bb = guard_edge->dest;
  edge e = EDGE_SUCC (new_merge_bb, 0);
  basic_block update_bb = e->dest;
  basic_block orig_bb = loop->header;
  edge new_exit_e;
  tree current_new_name;
  tree name;
  gimple_stmt_iterator gsi_orig, gsi_update;

  /* Create new bb between loop and new_merge_bb.  */
  *new_exit_bb = split_edge (single_exit (loop));

  new_exit_e = EDGE_SUCC (*new_exit_bb, 0);

  for (gsi_orig = gsi_start_phis (orig_bb),
       gsi_update = gsi_start_phis (update_bb);
       !gsi_end_p (gsi_orig) && !gsi_end_p (gsi_update);
       gsi_next (&gsi_orig), gsi_next (&gsi_update))
    {
      orig_phi = gsi_stmt (gsi_orig);
      update_phi = gsi_stmt (gsi_update);

      /* Virtual phi; Mark it for renaming. We actually want to call
	 mar_sym_for_renaming, but since all ssa renaming datastructures
	 are going to be freed before we get to call ssa_update, we just
	 record this name for now in a bitmap, and will mark it for
	 renaming later.  */
      name = PHI_RESULT (orig_phi);
      if (!is_gimple_reg (SSA_NAME_VAR (name)))
        bitmap_set_bit (vect_memsyms_to_rename, DECL_UID (SSA_NAME_VAR (name)));

      /** 1. Handle new-merge-point phis  **/

      /* 1.1. Generate new phi node in NEW_MERGE_BB:  */
      new_phi = create_phi_node (SSA_NAME_VAR (PHI_RESULT (orig_phi)),
                                 new_merge_bb);

      /* 1.2. NEW_MERGE_BB has two incoming edges: GUARD_EDGE and the exit-edge
            of LOOP. Set the two phi args in NEW_PHI for these edges:  */
      loop_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, EDGE_SUCC (loop->latch, 0));
      guard_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, loop_preheader_edge (loop));

      add_phi_arg (new_phi, loop_arg, new_exit_e);
      add_phi_arg (new_phi, guard_arg, guard_edge);

      /* 1.3. Update phi in successor block.  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi, e) == loop_arg
                  || PHI_ARG_DEF_FROM_EDGE (update_phi, e) == guard_arg);
      SET_PHI_ARG_DEF (update_phi, e->dest_idx, PHI_RESULT (new_phi));
      update_phi2 = new_phi;


      /** 2. Handle loop-closed-ssa-form phis  **/

      if (!is_gimple_reg (PHI_RESULT (orig_phi)))
	continue;

      /* 2.1. Generate new phi node in NEW_EXIT_BB:  */
      new_phi = create_phi_node (SSA_NAME_VAR (PHI_RESULT (orig_phi)),
                                 *new_exit_bb);

      /* 2.2. NEW_EXIT_BB has one incoming edge: the exit-edge of the loop.  */
      add_phi_arg (new_phi, loop_arg, single_exit (loop));

      /* 2.3. Update phi in successor of NEW_EXIT_BB:  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi2, new_exit_e) == loop_arg);
      SET_PHI_ARG_DEF (update_phi2, new_exit_e->dest_idx, PHI_RESULT (new_phi));

      /* 2.4. Record the newly created name with set_current_def.
         We want to find a name such that
                name = get_current_def (orig_loop_name)
         and to set its current definition as follows:
                set_current_def (name, new_phi_name)

         If LOOP is a new loop then loop_arg is already the name we're
         looking for. If LOOP is the original loop, then loop_arg is
         the orig_loop_name and the relevant name is recorded in its
         current reaching definition.  */
      if (is_new_loop)
        current_new_name = loop_arg;
      else
        {
          current_new_name = get_current_def (loop_arg);
	  /* current_def is not available only if the variable does not
	     change inside the loop, in which case we also don't care
	     about recording a current_def for it because we won't be
	     trying to create loop-exit-phis for it.  */
	  if (!current_new_name)
	    continue;
        }
      gcc_assert (get_current_def (current_new_name) == NULL_TREE);

      set_current_def (current_new_name, PHI_RESULT (new_phi));
      bitmap_set_bit (*defs, SSA_NAME_VERSION (current_new_name));
    }
}


/* Function slpeel_update_phi_nodes_for_guard2

   Input:
   - GUARD_EDGE, LOOP, IS_NEW_LOOP, NEW_EXIT_BB - as explained above.

   In the context of the overall structure, we have:

        loop1_preheader_bb: 
                guard1 (goto loop1/merge1_bb)
        loop1
        loop1_exit_bb: 
                guard2 (goto merge1_bb/merge2_bb)
        merge1_bb
LOOP->  loop2
        loop2_exit_bb
        merge2_bb
        next_bb

   For each name used out side the loop (i.e - for each name that has an exit
   phi in next_bb) we create a new phi in:
   1. merge2_bb (to account for the edge from guard_bb) 
   2. loop2_exit_bb (an exit-phi to keep LOOP in loop-closed form)
   3. guard2 bb (an exit phi to keep the preceding loop in loop-closed form),
      if needed (if it wasn't handled by slpeel_update_phis_nodes_for_phi1).
*/

static void
slpeel_update_phi_nodes_for_guard2 (edge guard_edge, struct loop *loop,
                                    bool is_new_loop, basic_block *new_exit_bb)
{
  gimple orig_phi, new_phi;
  gimple update_phi, update_phi2;
  tree guard_arg, loop_arg;
  basic_block new_merge_bb = guard_edge->dest;
  edge e = EDGE_SUCC (new_merge_bb, 0);
  basic_block update_bb = e->dest;
  edge new_exit_e;
  tree orig_def, orig_def_new_name;
  tree new_name, new_name2;
  tree arg;
  gimple_stmt_iterator gsi;

  /* Create new bb between loop and new_merge_bb.  */
  *new_exit_bb = split_edge (single_exit (loop));

  new_exit_e = EDGE_SUCC (*new_exit_bb, 0);

  for (gsi = gsi_start_phis (update_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      update_phi = gsi_stmt (gsi);
      orig_phi = update_phi;
      orig_def = PHI_ARG_DEF_FROM_EDGE (orig_phi, e);
      /* This loop-closed-phi actually doesn't represent a use
         out of the loop - the phi arg is a constant.  */ 
      if (TREE_CODE (orig_def) != SSA_NAME)
        continue;
      orig_def_new_name = get_current_def (orig_def);
      arg = NULL_TREE;

      /** 1. Handle new-merge-point phis  **/

      /* 1.1. Generate new phi node in NEW_MERGE_BB:  */
      new_phi = create_phi_node (SSA_NAME_VAR (PHI_RESULT (orig_phi)),
                                 new_merge_bb);

      /* 1.2. NEW_MERGE_BB has two incoming edges: GUARD_EDGE and the exit-edge
            of LOOP. Set the two PHI args in NEW_PHI for these edges:  */
      new_name = orig_def;
      new_name2 = NULL_TREE;
      if (orig_def_new_name)
        {
          new_name = orig_def_new_name;
	  /* Some variables have both loop-entry-phis and loop-exit-phis.
	     Such variables were given yet newer names by phis placed in
	     guard_bb by slpeel_update_phi_nodes_for_guard1. I.e:
	     new_name2 = get_current_def (get_current_def (orig_name)).  */
          new_name2 = get_current_def (new_name);
        }
  
      if (is_new_loop)
        {
          guard_arg = orig_def;
          loop_arg = new_name;
        }
      else
        {
          guard_arg = new_name;
          loop_arg = orig_def;
        }
      if (new_name2)
        guard_arg = new_name2;
  
      add_phi_arg (new_phi, loop_arg, new_exit_e);
      add_phi_arg (new_phi, guard_arg, guard_edge);

      /* 1.3. Update phi in successor block.  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi, e) == orig_def);
      SET_PHI_ARG_DEF (update_phi, e->dest_idx, PHI_RESULT (new_phi));
      update_phi2 = new_phi;


      /** 2. Handle loop-closed-ssa-form phis  **/

      /* 2.1. Generate new phi node in NEW_EXIT_BB:  */
      new_phi = create_phi_node (SSA_NAME_VAR (PHI_RESULT (orig_phi)),
                                 *new_exit_bb);

      /* 2.2. NEW_EXIT_BB has one incoming edge: the exit-edge of the loop.  */
      add_phi_arg (new_phi, loop_arg, single_exit (loop));

      /* 2.3. Update phi in successor of NEW_EXIT_BB:  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi2, new_exit_e) == loop_arg);
      SET_PHI_ARG_DEF (update_phi2, new_exit_e->dest_idx, PHI_RESULT (new_phi));


      /** 3. Handle loop-closed-ssa-form phis for first loop  **/

      /* 3.1. Find the relevant names that need an exit-phi in
	 GUARD_BB, i.e. names for which
	 slpeel_update_phi_nodes_for_guard1 had not already created a
	 phi node. This is the case for names that are used outside
	 the loop (and therefore need an exit phi) but are not updated
	 across loop iterations (and therefore don't have a
	 loop-header-phi).

	 slpeel_update_phi_nodes_for_guard1 is responsible for
	 creating loop-exit phis in GUARD_BB for names that have a
	 loop-header-phi.  When such a phi is created we also record
	 the new name in its current definition.  If this new name
	 exists, then guard_arg was set to this new name (see 1.2
	 above).  Therefore, if guard_arg is not this new name, this
	 is an indication that an exit-phi in GUARD_BB was not yet
	 created, so we take care of it here.  */
      if (guard_arg == new_name2)
	continue;
      arg = guard_arg;

      /* 3.2. Generate new phi node in GUARD_BB:  */
      new_phi = create_phi_node (SSA_NAME_VAR (PHI_RESULT (orig_phi)),
                                 guard_edge->src);

      /* 3.3. GUARD_BB has one incoming edge:  */
      gcc_assert (EDGE_COUNT (guard_edge->src->preds) == 1);
      add_phi_arg (new_phi, arg, EDGE_PRED (guard_edge->src, 0));

      /* 3.4. Update phi in successor of GUARD_BB:  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi2, guard_edge)
                                                                == guard_arg);
      SET_PHI_ARG_DEF (update_phi2, guard_edge->dest_idx, PHI_RESULT (new_phi));
    }
}


/* Make the LOOP iterate NITERS times. This is done by adding a new IV
   that starts at zero, increases by one and its limit is NITERS.

   Assumption: the exit-condition of LOOP is the last stmt in the loop.  */

void
slpeel_make_loop_iterate_ntimes (struct loop *loop, tree niters)
{
  tree indx_before_incr, indx_after_incr;
  gimple cond_stmt;
  gimple orig_cond;
  edge exit_edge = single_exit (loop);
  gimple_stmt_iterator loop_cond_gsi;
  gimple_stmt_iterator incr_gsi;
  bool insert_after;
  tree init = build_int_cst (TREE_TYPE (niters), 0);
  tree step = build_int_cst (TREE_TYPE (niters), 1);
  LOC loop_loc;
  enum tree_code code;

  orig_cond = get_loop_exit_condition (loop);
  gcc_assert (orig_cond);
  loop_cond_gsi = gsi_for_stmt (orig_cond);

  standard_iv_increment_position (loop, &incr_gsi, &insert_after);
  create_iv (init, step, NULL_TREE, loop,
             &incr_gsi, insert_after, &indx_before_incr, &indx_after_incr);

  indx_after_incr = force_gimple_operand_gsi (&loop_cond_gsi, indx_after_incr,
					      true, NULL_TREE, true,
					      GSI_SAME_STMT);
  niters = force_gimple_operand_gsi (&loop_cond_gsi, niters, true, NULL_TREE,
				     true, GSI_SAME_STMT);

  code = (exit_edge->flags & EDGE_TRUE_VALUE) ? GE_EXPR : LT_EXPR;
  cond_stmt = gimple_build_cond (code, indx_after_incr, niters, NULL_TREE,
				 NULL_TREE);

  gsi_insert_before (&loop_cond_gsi, cond_stmt, GSI_SAME_STMT);

  /* Remove old loop exit test:  */
  gsi_remove (&loop_cond_gsi, true);

  loop_loc = find_loop_location (loop);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (loop_loc != UNKNOWN_LOC)
        fprintf (dump_file, "\nloop at %s:%d: ",
                 LOC_FILE (loop_loc), LOC_LINE (loop_loc));
      print_gimple_stmt (dump_file, cond_stmt, 0, TDF_SLIM);
    }

  loop->nb_iterations = niters;
}


/* Given LOOP this function generates a new copy of it and puts it 
   on E which is either the entry or exit of LOOP.  */

struct loop *
slpeel_tree_duplicate_loop_to_edge_cfg (struct loop *loop, edge e)
{
  struct loop *new_loop;
  basic_block *new_bbs, *bbs;
  bool at_exit;
  bool was_imm_dom;
  basic_block exit_dest; 
  gimple phi;
  tree phi_arg;
  edge exit, new_exit;
  gimple_stmt_iterator gsi;

  at_exit = (e == single_exit (loop)); 
  if (!at_exit && e != loop_preheader_edge (loop))
    return NULL;

  bbs = get_loop_body (loop);

  /* Check whether duplication is possible.  */
  if (!can_copy_bbs_p (bbs, loop->num_nodes))
    {
      free (bbs);
      return NULL;
    }

  /* Generate new loop structure.  */
  new_loop = duplicate_loop (loop, loop_outer (loop));
  if (!new_loop)
    {
      free (bbs);
      return NULL;
    }

  exit_dest = single_exit (loop)->dest;
  was_imm_dom = (get_immediate_dominator (CDI_DOMINATORS, 
					  exit_dest) == loop->header ? 
		 true : false);

  new_bbs = XNEWVEC (basic_block, loop->num_nodes);

  exit = single_exit (loop);
  copy_bbs (bbs, loop->num_nodes, new_bbs,
	    &exit, 1, &new_exit, NULL,
	    e->src);

  /* Duplicating phi args at exit bbs as coming 
     also from exit of duplicated loop.  */
  for (gsi = gsi_start_phis (exit_dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi_stmt (gsi);
      phi_arg = PHI_ARG_DEF_FROM_EDGE (phi, single_exit (loop));
      if (phi_arg)
	{
	  edge new_loop_exit_edge;

	  if (EDGE_SUCC (new_loop->header, 0)->dest == new_loop->latch)
	    new_loop_exit_edge = EDGE_SUCC (new_loop->header, 1);
	  else
	    new_loop_exit_edge = EDGE_SUCC (new_loop->header, 0);
  
	  add_phi_arg (phi, phi_arg, new_loop_exit_edge);	
	}
    }    
   
  if (at_exit) /* Add the loop copy at exit.  */
    {
      redirect_edge_and_branch_force (e, new_loop->header);
      PENDING_STMT (e) = NULL;
      set_immediate_dominator (CDI_DOMINATORS, new_loop->header, e->src);
      if (was_imm_dom)
	set_immediate_dominator (CDI_DOMINATORS, exit_dest, new_loop->header);
    }
  else /* Add the copy at entry.  */
    {
      edge new_exit_e;
      edge entry_e = loop_preheader_edge (loop);
      basic_block preheader = entry_e->src;
           
      if (!flow_bb_inside_loop_p (new_loop, 
				  EDGE_SUCC (new_loop->header, 0)->dest))
        new_exit_e = EDGE_SUCC (new_loop->header, 0);
      else
	new_exit_e = EDGE_SUCC (new_loop->header, 1); 

      redirect_edge_and_branch_force (new_exit_e, loop->header);
      PENDING_STMT (new_exit_e) = NULL;
      set_immediate_dominator (CDI_DOMINATORS, loop->header,
			       new_exit_e->src);

      /* We have to add phi args to the loop->header here as coming 
	 from new_exit_e edge.  */
      for (gsi = gsi_start_phis (loop->header);
           !gsi_end_p (gsi);
           gsi_next (&gsi))
	{
	  phi = gsi_stmt (gsi);
	  phi_arg = PHI_ARG_DEF_FROM_EDGE (phi, entry_e);
	  if (phi_arg)
	    add_phi_arg (phi, phi_arg, new_exit_e);	
	}    

      redirect_edge_and_branch_force (entry_e, new_loop->header);
      PENDING_STMT (entry_e) = NULL;
      set_immediate_dominator (CDI_DOMINATORS, new_loop->header, preheader);
    }

  free (new_bbs);
  free (bbs);

  return new_loop;
}


/* Given the condition statement COND, put it as the last statement
   of GUARD_BB; EXIT_BB is the basic block to skip the loop;
   Assumes that this is the single exit of the guarded loop.  
   Returns the skip edge.  */

static edge
slpeel_add_loop_guard (basic_block guard_bb, tree cond, basic_block exit_bb,
		       basic_block dom_bb)
{
  gimple_stmt_iterator gsi;
  edge new_e, enter_e;
  gimple cond_stmt;
  gimple_seq gimplify_stmt_list = NULL;

  enter_e = EDGE_SUCC (guard_bb, 0);
  enter_e->flags &= ~EDGE_FALLTHRU;
  enter_e->flags |= EDGE_FALSE_VALUE;
  gsi = gsi_last_bb (guard_bb);

  cond = force_gimple_operand (cond, &gimplify_stmt_list, true, NULL_TREE);
  cond_stmt = gimple_build_cond (NE_EXPR,
				 cond, build_int_cst (TREE_TYPE (cond), 0),
				 NULL_TREE, NULL_TREE);
  if (gimplify_stmt_list)
    gsi_insert_seq_after (&gsi, gimplify_stmt_list, GSI_NEW_STMT);

  gsi = gsi_last_bb (guard_bb);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);

  /* Add new edge to connect guard block to the merge/loop-exit block.  */
  new_e = make_edge (guard_bb, exit_bb, EDGE_TRUE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, exit_bb, dom_bb);
  return new_e;
}


/* This function verifies that the following restrictions apply to LOOP:
   (1) it is innermost
   (2) it consists of exactly 2 basic blocks - header, and an empty latch.
   (3) it is single entry, single exit
   (4) its exit condition is the last stmt in the header
   (5) E is the entry/exit edge of LOOP.
 */

bool
slpeel_can_duplicate_loop_p (const struct loop *loop, const_edge e)
{
  edge exit_e = single_exit (loop);
  edge entry_e = loop_preheader_edge (loop);
  gimple orig_cond = get_loop_exit_condition (loop);
  gimple_stmt_iterator loop_exit_gsi = gsi_last_bb (exit_e->src);

  if (need_ssa_update_p ())
    return false;

  if (loop->inner
      /* All loops have an outer scope; the only case loop->outer is NULL is for
         the function itself.  */
      || !loop_outer (loop)
      || loop->num_nodes != 2
      || !empty_block_p (loop->latch)
      || !single_exit (loop)
      /* Verify that new loop exit condition can be trivially modified.  */
      || (!orig_cond || orig_cond != gsi_stmt (loop_exit_gsi))
      || (e != exit_e && e != entry_e))
    return false;

  return true;
}

#ifdef ENABLE_CHECKING
void
slpeel_verify_cfg_after_peeling (struct loop *first_loop,
                                 struct loop *second_loop)
{
  basic_block loop1_exit_bb = single_exit (first_loop)->dest;
  basic_block loop2_entry_bb = loop_preheader_edge (second_loop)->src;
  basic_block loop1_entry_bb = loop_preheader_edge (first_loop)->src;

  /* A guard that controls whether the second_loop is to be executed or skipped
     is placed in first_loop->exit.  first_loop->exit therefore has two
     successors - one is the preheader of second_loop, and the other is a bb
     after second_loop.
   */
  gcc_assert (EDGE_COUNT (loop1_exit_bb->succs) == 2);
   
  /* 1. Verify that one of the successors of first_loop->exit is the preheader
        of second_loop.  */
   
  /* The preheader of new_loop is expected to have two predecessors:
     first_loop->exit and the block that precedes first_loop.  */

  gcc_assert (EDGE_COUNT (loop2_entry_bb->preds) == 2 
              && ((EDGE_PRED (loop2_entry_bb, 0)->src == loop1_exit_bb
                   && EDGE_PRED (loop2_entry_bb, 1)->src == loop1_entry_bb)
               || (EDGE_PRED (loop2_entry_bb, 1)->src ==  loop1_exit_bb
                   && EDGE_PRED (loop2_entry_bb, 0)->src == loop1_entry_bb)));
  
  /* Verify that the other successor of first_loop->exit is after the
     second_loop.  */
  /* TODO */
}
#endif

/* If the run time cost model check determines that vectorization is
   not profitable and hence scalar loop should be generated then set
   FIRST_NITERS to prologue peeled iterations. This will allow all the
   iterations to be executed in the prologue peeled scalar loop.  */

void
set_prologue_iterations (basic_block bb_before_first_loop,
			 tree first_niters,
			 struct loop *loop,
			 unsigned int th)
{
  edge e;
  basic_block cond_bb, then_bb;
  tree var, prologue_after_cost_adjust_name;
  gimple_stmt_iterator gsi;
  gimple newphi;
  edge e_true, e_false, e_fallthru;
  gimple cond_stmt;
  gimple_seq gimplify_stmt_list = NULL, stmts = NULL;
  tree cost_pre_condition = NULL_TREE;
  tree scalar_loop_iters = 
    unshare_expr (LOOP_VINFO_NITERS_UNCHANGED (loop_vec_info_for_loop (loop)));

  e = single_pred_edge (bb_before_first_loop);
  cond_bb = split_edge(e);

  e = single_pred_edge (bb_before_first_loop);
  then_bb = split_edge(e);
  set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);

  e_false = make_single_succ_edge (cond_bb, bb_before_first_loop,
				   EDGE_FALSE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, bb_before_first_loop, cond_bb);

  e_true = EDGE_PRED (then_bb, 0);
  e_true->flags &= ~EDGE_FALLTHRU;
  e_true->flags |= EDGE_TRUE_VALUE;

  e_fallthru = EDGE_SUCC (then_bb, 0);

  cost_pre_condition =
    fold_build2 (LE_EXPR, boolean_type_node, scalar_loop_iters, 
		 build_int_cst (TREE_TYPE (scalar_loop_iters), th));
  cost_pre_condition =
    force_gimple_operand (cost_pre_condition, &gimplify_stmt_list,
			  true, NULL_TREE);
  cond_stmt = gimple_build_cond (NE_EXPR, cost_pre_condition,
				 build_int_cst (TREE_TYPE (cost_pre_condition),
						0), NULL_TREE, NULL_TREE);

  gsi = gsi_last_bb (cond_bb);
  if (gimplify_stmt_list)
    gsi_insert_seq_after (&gsi, gimplify_stmt_list, GSI_NEW_STMT);

  gsi = gsi_last_bb (cond_bb);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);
					  
  var = create_tmp_var (TREE_TYPE (scalar_loop_iters),
			"prologue_after_cost_adjust");
  add_referenced_var (var);
  prologue_after_cost_adjust_name = 
    force_gimple_operand (scalar_loop_iters, &stmts, false, var);

  gsi = gsi_last_bb (then_bb);
  if (stmts)
    gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);

  newphi = create_phi_node (var, bb_before_first_loop);
  add_phi_arg (newphi, prologue_after_cost_adjust_name, e_fallthru);
  add_phi_arg (newphi, first_niters, e_false);

  first_niters = PHI_RESULT (newphi);
}


/* Function slpeel_tree_peel_loop_to_edge.

   Peel the first (last) iterations of LOOP into a new prolog (epilog) loop
   that is placed on the entry (exit) edge E of LOOP. After this transformation
   we have two loops one after the other - first-loop iterates FIRST_NITERS
   times, and second-loop iterates the remainder NITERS - FIRST_NITERS times.
   If the cost model indicates that it is profitable to emit a scalar 
   loop instead of the vector one, then the prolog (epilog) loop will iterate
   for the entire unchanged scalar iterations of the loop.

   Input:
   - LOOP: the loop to be peeled.
   - E: the exit or entry edge of LOOP.
        If it is the entry edge, we peel the first iterations of LOOP. In this
        case first-loop is LOOP, and second-loop is the newly created loop.
        If it is the exit edge, we peel the last iterations of LOOP. In this
        case, first-loop is the newly created loop, and second-loop is LOOP.
   - NITERS: the number of iterations that LOOP iterates.
   - FIRST_NITERS: the number of iterations that the first-loop should iterate.
   - UPDATE_FIRST_LOOP_COUNT:  specified whether this function is responsible
        for updating the loop bound of the first-loop to FIRST_NITERS.  If it
        is false, the caller of this function may want to take care of this
        (this can be useful if we don't want new stmts added to first-loop).
   - TH: cost model profitability threshold of iterations for vectorization.
   - CHECK_PROFITABILITY: specify whether cost model check has not occurred
                          during versioning and hence needs to occur during
			  prologue generation or whether cost model check 
			  has not occurred during prologue generation and hence
			  needs to occur during epilogue generation.
	    

   Output:
   The function returns a pointer to the new loop-copy, or NULL if it failed
   to perform the transformation.

   The function generates two if-then-else guards: one before the first loop,
   and the other before the second loop:
   The first guard is:
     if (FIRST_NITERS == 0) then skip the first loop,
     and go directly to the second loop.
   The second guard is:
     if (FIRST_NITERS == NITERS) then skip the second loop.

   FORNOW only simple loops are supported (see slpeel_can_duplicate_loop_p).
   FORNOW the resulting code will not be in loop-closed-ssa form.
*/

struct loop*
slpeel_tree_peel_loop_to_edge (struct loop *loop, 
			       edge e, tree first_niters, 
			       tree niters, bool update_first_loop_count,
			       unsigned int th, bool check_profitability)
{
  struct loop *new_loop = NULL, *first_loop, *second_loop;
  edge skip_e;
  tree pre_condition = NULL_TREE;
  bitmap definitions;
  basic_block bb_before_second_loop, bb_after_second_loop;
  basic_block bb_before_first_loop;
  basic_block bb_between_loops;
  basic_block new_exit_bb;
  edge exit_e = single_exit (loop);
  LOC loop_loc;
  tree cost_pre_condition = NULL_TREE;
  
  if (!slpeel_can_duplicate_loop_p (loop, e))
    return NULL;
  
  /* We have to initialize cfg_hooks. Then, when calling
   cfg_hooks->split_edge, the function tree_split_edge 
   is actually called and, when calling cfg_hooks->duplicate_block,
   the function tree_duplicate_bb is called.  */
  gimple_register_cfg_hooks ();


  /* 1. Generate a copy of LOOP and put it on E (E is the entry/exit of LOOP).
        Resulting CFG would be:

        first_loop:
        do {
        } while ...

        second_loop:
        do {
        } while ...

        orig_exit_bb:
   */
  
  if (!(new_loop = slpeel_tree_duplicate_loop_to_edge_cfg (loop, e)))
    {
      loop_loc = find_loop_location (loop);
      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          if (loop_loc != UNKNOWN_LOC)
            fprintf (dump_file, "\n%s:%d: note: ",
                     LOC_FILE (loop_loc), LOC_LINE (loop_loc));
          fprintf (dump_file, "tree_duplicate_loop_to_edge_cfg failed.\n");
        }
      return NULL;
    }
  
  if (e == exit_e)
    {
      /* NEW_LOOP was placed after LOOP.  */
      first_loop = loop;
      second_loop = new_loop;
    }
  else
    {
      /* NEW_LOOP was placed before LOOP.  */
      first_loop = new_loop;
      second_loop = loop;
    }

  definitions = ssa_names_to_replace ();
  slpeel_update_phis_for_duplicate_loop (loop, new_loop, e == exit_e);
  rename_variables_in_loop (new_loop);


  /* 2.  Add the guard code in one of the following ways:

     2.a Add the guard that controls whether the first loop is executed.
         This occurs when this function is invoked for prologue or epilogue
	 generation and when the cost model check can be done at compile time.

         Resulting CFG would be:

         bb_before_first_loop:
         if (FIRST_NITERS == 0) GOTO bb_before_second_loop
                                GOTO first-loop

         first_loop:
         do {
         } while ...

         bb_before_second_loop:

         second_loop:
         do {
         } while ...

         orig_exit_bb:

     2.b Add the cost model check that allows the prologue
         to iterate for the entire unchanged scalar
         iterations of the loop in the event that the cost
         model indicates that the scalar loop is more
         profitable than the vector one. This occurs when
	 this function is invoked for prologue generation
	 and the cost model check needs to be done at run
	 time.

         Resulting CFG after prologue peeling would be:

         if (scalar_loop_iterations <= th)
           FIRST_NITERS = scalar_loop_iterations

         bb_before_first_loop:
         if (FIRST_NITERS == 0) GOTO bb_before_second_loop
                                GOTO first-loop

         first_loop:
         do {
         } while ...

         bb_before_second_loop:

         second_loop:
         do {
         } while ...

         orig_exit_bb:

     2.c Add the cost model check that allows the epilogue
         to iterate for the entire unchanged scalar
         iterations of the loop in the event that the cost
         model indicates that the scalar loop is more
         profitable than the vector one. This occurs when
	 this function is invoked for epilogue generation
	 and the cost model check needs to be done at run
	 time.

         Resulting CFG after prologue peeling would be:

         bb_before_first_loop:
         if ((scalar_loop_iterations <= th)
             ||
             FIRST_NITERS == 0) GOTO bb_before_second_loop
                                GOTO first-loop

         first_loop:
         do {
         } while ...

         bb_before_second_loop:

         second_loop:
         do {
         } while ...

         orig_exit_bb:
  */

  bb_before_first_loop = split_edge (loop_preheader_edge (first_loop));
  bb_before_second_loop = split_edge (single_exit (first_loop));

  /* Epilogue peeling.  */
  if (!update_first_loop_count)
    {
      pre_condition =
	fold_build2 (LE_EXPR, boolean_type_node, first_niters, 
		     build_int_cst (TREE_TYPE (first_niters), 0));
      if (check_profitability)
	{
	  tree scalar_loop_iters
	    = unshare_expr (LOOP_VINFO_NITERS_UNCHANGED
					(loop_vec_info_for_loop (loop)));
	  cost_pre_condition = 
	    fold_build2 (LE_EXPR, boolean_type_node, scalar_loop_iters, 
			 build_int_cst (TREE_TYPE (scalar_loop_iters), th));

	  pre_condition = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
				       cost_pre_condition, pre_condition);
	}
    }

  /* Prologue peeling.  */  
  else
    {
      if (check_profitability)
	set_prologue_iterations (bb_before_first_loop, first_niters,
				 loop, th);

      pre_condition =
	fold_build2 (LE_EXPR, boolean_type_node, first_niters, 
		     build_int_cst (TREE_TYPE (first_niters), 0));
    }

  skip_e = slpeel_add_loop_guard (bb_before_first_loop, pre_condition,
                                  bb_before_second_loop, bb_before_first_loop);
  slpeel_update_phi_nodes_for_guard1 (skip_e, first_loop,
				      first_loop == new_loop,
				      &new_exit_bb, &definitions);


  /* 3. Add the guard that controls whether the second loop is executed.
        Resulting CFG would be:

        bb_before_first_loop:
        if (FIRST_NITERS == 0) GOTO bb_before_second_loop (skip first loop)
                               GOTO first-loop

        first_loop:
        do {
        } while ...

        bb_between_loops:
        if (FIRST_NITERS == NITERS) GOTO bb_after_second_loop (skip second loop)
                                    GOTO bb_before_second_loop

        bb_before_second_loop:

        second_loop:
        do {
        } while ...

        bb_after_second_loop:

        orig_exit_bb:
   */

  bb_between_loops = new_exit_bb;
  bb_after_second_loop = split_edge (single_exit (second_loop));

  pre_condition = 
	fold_build2 (EQ_EXPR, boolean_type_node, first_niters, niters);
  skip_e = slpeel_add_loop_guard (bb_between_loops, pre_condition,
                                  bb_after_second_loop, bb_before_first_loop);
  slpeel_update_phi_nodes_for_guard2 (skip_e, second_loop,
                                     second_loop == new_loop, &new_exit_bb);

  /* 4. Make first-loop iterate FIRST_NITERS times, if requested.
   */
  if (update_first_loop_count)
    slpeel_make_loop_iterate_ntimes (first_loop, first_niters);

  BITMAP_FREE (definitions);
  delete_update_ssa ();

  return new_loop;
}

/* Function vect_get_loop_location.

   Extract the location of the loop in the source code.
   If the loop is not well formed for vectorization, an estimated
   location is calculated.
   Return the loop location if succeed and NULL if not.  */

LOC
find_loop_location (struct loop *loop)
{
  gimple stmt = NULL;
  basic_block bb;
  gimple_stmt_iterator si;

  if (!loop)
    return UNKNOWN_LOC;

  stmt = get_loop_exit_condition (loop);

  if (stmt && gimple_location (stmt) != UNKNOWN_LOC)
    return gimple_location (stmt);

  /* If we got here the loop is probably not "well formed",
     try to estimate the loop location */

  if (!loop->header)
    return UNKNOWN_LOC;

  bb = loop->header;

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      stmt = gsi_stmt (si);
      if (gimple_location (stmt) != UNKNOWN_LOC)
        return gimple_location (stmt);
    }

  return UNKNOWN_LOC;
}


/*************************************************************************
  Vectorization Debug Information.
 *************************************************************************/

/* Function vect_set_verbosity_level.

   Called from toplev.c upon detection of the
   -ftree-vectorizer-verbose=N option.  */

void
vect_set_verbosity_level (const char *val)
{
   unsigned int vl;

   vl = atoi (val);
   if (vl < MAX_VERBOSITY_LEVEL)
     vect_verbosity_level = vl;
   else
     vect_verbosity_level = MAX_VERBOSITY_LEVEL - 1;
}


/* Function vect_set_dump_settings.

   Fix the verbosity level of the vectorizer if the
   requested level was not set explicitly using the flag
   -ftree-vectorizer-verbose=N.
   Decide where to print the debugging information (dump_file/stderr).
   If the user defined the verbosity level, but there is no dump file,
   print to stderr, otherwise print to the dump file.  */

static void
vect_set_dump_settings (void)
{
  vect_dump = dump_file;

  /* Check if the verbosity level was defined by the user:  */
  if (vect_verbosity_level != MAX_VERBOSITY_LEVEL)
    {
      /* If there is no dump file, print to stderr.  */
      if (!dump_file)
        vect_dump = stderr;
      return;
    }

  /* User didn't specify verbosity level:  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    vect_verbosity_level = REPORT_DETAILS;
  else if (dump_file && (dump_flags & TDF_STATS))
    vect_verbosity_level = REPORT_UNVECTORIZED_LOOPS;
  else
    vect_verbosity_level = REPORT_NONE;

  gcc_assert (dump_file || vect_verbosity_level == REPORT_NONE);
}


/* Function debug_loop_details.

   For vectorization debug dumps.  */

bool
vect_print_dump_info (enum verbosity_levels vl)
{
  if (vl > vect_verbosity_level)
    return false;

  if (!current_function_decl || !vect_dump)
    return false;

  if (vect_loop_location == UNKNOWN_LOC)
    fprintf (vect_dump, "\n%s:%d: note: ",
	     DECL_SOURCE_FILE (current_function_decl),
	     DECL_SOURCE_LINE (current_function_decl));
  else
    fprintf (vect_dump, "\n%s:%d: note: ", 
	     LOC_FILE (vect_loop_location), LOC_LINE (vect_loop_location));

  return true;
}


/*************************************************************************
  Vectorization Utilities.
 *************************************************************************/

/* Function new_stmt_vec_info.

   Create and initialize a new stmt_vec_info struct for STMT.  */

stmt_vec_info
new_stmt_vec_info (gimple stmt, loop_vec_info loop_vinfo)
{
  stmt_vec_info res;
  res = (stmt_vec_info) xcalloc (1, sizeof (struct _stmt_vec_info));

  STMT_VINFO_TYPE (res) = undef_vec_info_type;
  STMT_VINFO_STMT (res) = stmt;
  STMT_VINFO_LOOP_VINFO (res) = loop_vinfo;
  STMT_VINFO_RELEVANT (res) = 0;
  STMT_VINFO_LIVE_P (res) = false;
  STMT_VINFO_VECTYPE (res) = NULL;
  STMT_VINFO_VEC_STMT (res) = NULL;
  STMT_VINFO_IN_PATTERN_P (res) = false;
  STMT_VINFO_RELATED_STMT (res) = NULL;
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
    STMT_VINFO_DEF_TYPE (res) = vect_loop_def;
  STMT_VINFO_SAME_ALIGN_REFS (res) = VEC_alloc (dr_p, heap, 5);
  STMT_VINFO_INSIDE_OF_LOOP_COST (res) = 0;
  STMT_VINFO_OUTSIDE_OF_LOOP_COST (res) = 0;
  STMT_SLP_TYPE (res) = 0;
  DR_GROUP_FIRST_DR (res) = NULL;
  DR_GROUP_NEXT_DR (res) = NULL;
  DR_GROUP_SIZE (res) = 0;
  DR_GROUP_STORE_COUNT (res) = 0;
  DR_GROUP_GAP (res) = 0;
  DR_GROUP_SAME_DR_STMT (res) = NULL;
  DR_GROUP_READ_WRITE_DEPENDENCE (res) = false;

  return res;
}

/* Create a hash table for stmt_vec_info. */

void
init_stmt_vec_info_vec (void)
{
  gcc_assert (!stmt_vec_info_vec);
  stmt_vec_info_vec = VEC_alloc (vec_void_p, heap, 50);
}

/* Free hash table for stmt_vec_info. */

void
free_stmt_vec_info_vec (void)
{
  gcc_assert (stmt_vec_info_vec);
  VEC_free (vec_void_p, heap, stmt_vec_info_vec);
}

/* Free stmt vectorization related info.  */

void
free_stmt_vec_info (gimple stmt)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

  if (!stmt_info)
    return;

  VEC_free (dr_p, heap, STMT_VINFO_SAME_ALIGN_REFS (stmt_info));
  set_vinfo_for_stmt (stmt, NULL);
  free (stmt_info);
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

loop_vec_info
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
	      set_vinfo_for_stmt (phi, new_stmt_vec_info (phi, res));
	    }

	  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	    {
	      gimple stmt = gsi_stmt (si);
	      gimple_set_uid (stmt, 0);
	      set_vinfo_for_stmt (stmt, new_stmt_vec_info (stmt, res));
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


/* Function vect_force_dr_alignment_p.

   Returns whether the alignment of a DECL can be forced to be aligned
   on ALIGNMENT bit boundary.  */

bool 
vect_can_force_dr_alignment_p (const_tree decl, unsigned int alignment)
{
  if (TREE_CODE (decl) != VAR_DECL)
    return false;

  if (DECL_EXTERNAL (decl))
    return false;

  if (TREE_ASM_WRITTEN (decl))
    return false;

  if (TREE_STATIC (decl))
    return (alignment <= MAX_OFILE_ALIGNMENT);
  else
    return (alignment <= MAX_STACK_ALIGNMENT);
}


/* Function get_vectype_for_scalar_type.

   Returns the vector type corresponding to SCALAR_TYPE as supported
   by the target.  */

tree
get_vectype_for_scalar_type (tree scalar_type)
{
  enum machine_mode inner_mode = TYPE_MODE (scalar_type);
  int nbytes = GET_MODE_SIZE (inner_mode);
  int nunits;
  tree vectype;

  if (nbytes == 0 || nbytes >= UNITS_PER_SIMD_WORD (inner_mode))
    return NULL_TREE;

  /* FORNOW: Only a single vector size per mode (UNITS_PER_SIMD_WORD)
     is expected.  */
  nunits = UNITS_PER_SIMD_WORD (inner_mode) / nbytes;

  vectype = build_vector_type (scalar_type, nunits);
  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "get vectype with %d units of type ", nunits);
      print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
    }

  if (!vectype)
    return NULL_TREE;

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "vectype: ");
      print_generic_expr (vect_dump, vectype, TDF_SLIM);
    }

  if (!VECTOR_MODE_P (TYPE_MODE (vectype))
      && !INTEGRAL_MODE_P (TYPE_MODE (vectype)))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "mode not supported by target.");
      return NULL_TREE;
    }

  return vectype;
}


/* Function vect_supportable_dr_alignment

   Return whether the data reference DR is supported with respect to its
   alignment.  */

enum dr_alignment_support
vect_supportable_dr_alignment (struct data_reference *dr)
{
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  enum machine_mode mode = (int) TYPE_MODE (vectype);
  struct loop *vect_loop = LOOP_VINFO_LOOP (STMT_VINFO_LOOP_VINFO (stmt_info));
  bool nested_in_vect_loop = nested_in_vect_loop_p (vect_loop, stmt);
  bool invariant_in_outerloop = false;

  if (aligned_access_p (dr))
    return dr_aligned;

  if (nested_in_vect_loop)
    {
      tree outerloop_step = STMT_VINFO_DR_STEP (stmt_info);
      invariant_in_outerloop =
	(tree_int_cst_compare (outerloop_step, size_zero_node) == 0);
    }

  /* Possibly unaligned access.  */

  /* We can choose between using the implicit realignment scheme (generating
     a misaligned_move stmt) and the explicit realignment scheme (generating
     aligned loads with a REALIGN_LOAD). There are two variants to the explicit
     realignment scheme: optimized, and unoptimized.
     We can optimize the realignment only if the step between consecutive
     vector loads is equal to the vector size.  Since the vector memory
     accesses advance in steps of VS (Vector Size) in the vectorized loop, it
     is guaranteed that the misalignment amount remains the same throughout the
     execution of the vectorized loop.  Therefore, we can create the
     "realignment token" (the permutation mask that is passed to REALIGN_LOAD)
     at the loop preheader.

     However, in the case of outer-loop vectorization, when vectorizing a
     memory access in the inner-loop nested within the LOOP that is now being
     vectorized, while it is guaranteed that the misalignment of the
     vectorized memory access will remain the same in different outer-loop
     iterations, it is *not* guaranteed that is will remain the same throughout
     the execution of the inner-loop.  This is because the inner-loop advances
     with the original scalar step (and not in steps of VS).  If the inner-loop
     step happens to be a multiple of VS, then the misalignment remains fixed
     and we can use the optimized realignment scheme.  For example:

      for (i=0; i<N; i++)
        for (j=0; j<M; j++)
          s += a[i+j];

     When vectorizing the i-loop in the above example, the step between
     consecutive vector loads is 1, and so the misalignment does not remain
     fixed across the execution of the inner-loop, and the realignment cannot
     be optimized (as illustrated in the following pseudo vectorized loop):

      for (i=0; i<N; i+=4)
        for (j=0; j<M; j++){
          vs += vp[i+j]; // misalignment of &vp[i+j] is {0,1,2,3,0,1,2,3,...}
                         // when j is {0,1,2,3,4,5,6,7,...} respectively.
                         // (assuming that we start from an aligned address).
          }

     We therefore have to use the unoptimized realignment scheme:

      for (i=0; i<N; i+=4)
          for (j=k; j<M; j+=4)
          vs += vp[i+j]; // misalignment of &vp[i+j] is always k (assuming
                           // that the misalignment of the initial address is
                           // 0).

     The loop can then be vectorized as follows:

      for (k=0; k<4; k++){
        rt = get_realignment_token (&vp[k]);
        for (i=0; i<N; i+=4){
          v1 = vp[i+k];
          for (j=k; j<M; j+=4){
            v2 = vp[i+j+VS-1];
            va = REALIGN_LOAD <v1,v2,rt>;
            vs += va;
            v1 = v2;
          }
        }
    } */

  if (DR_IS_READ (dr))
    {
      if (optab_handler (vec_realign_load_optab, mode)->insn_code != 
						   	     CODE_FOR_nothing
	  && (!targetm.vectorize.builtin_mask_for_load
	      || targetm.vectorize.builtin_mask_for_load ()))
	{
	  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
	  if (nested_in_vect_loop
	      && (TREE_INT_CST_LOW (DR_STEP (dr))
		  != GET_MODE_SIZE (TYPE_MODE (vectype))))
	    return dr_explicit_realign;
	  else
	    return dr_explicit_realign_optimized;
	}

      if (optab_handler (movmisalign_optab, mode)->insn_code != 
							     CODE_FOR_nothing)
	/* Can't software pipeline the loads, but can at least do them.  */
	return dr_unaligned_supported;
    }

  /* Unsupported.  */
  return dr_unaligned_unsupported;
}


/* Function vect_is_simple_use.

   Input:
   LOOP - the loop that is being vectorized.
   OPERAND - operand of a stmt in LOOP.
   DEF - the defining stmt in case OPERAND is an SSA_NAME.

   Returns whether a stmt with OPERAND can be vectorized.
   Supportable operands are constants, loop invariants, and operands that are
   defined by the current iteration of the loop. Unsupportable operands are 
   those that are defined by a previous iteration of the loop (as is the case
   in reduction/induction computations).  */

bool
vect_is_simple_use (tree operand, loop_vec_info loop_vinfo, gimple *def_stmt,
		    tree *def, enum vect_def_type *dt)
{ 
  basic_block bb;
  stmt_vec_info stmt_vinfo;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  *def_stmt = NULL;
  *def = NULL_TREE;
  
  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "vect_is_simple_use: operand ");
      print_generic_expr (vect_dump, operand, TDF_SLIM);
    }
    
  if (TREE_CODE (operand) == INTEGER_CST || TREE_CODE (operand) == REAL_CST)
    {
      *dt = vect_constant_def;
      return true;
    }
  if (is_gimple_min_invariant (operand))
    {
      *def = operand;
      *dt = vect_invariant_def;
      return true;
    }

  if (TREE_CODE (operand) == PAREN_EXPR)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "non-associatable copy.");
      operand = TREE_OPERAND (operand, 0);
    }
  if (TREE_CODE (operand) != SSA_NAME)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "not ssa-name.");
      return false;
    }
    
  *def_stmt = SSA_NAME_DEF_STMT (operand);
  if (*def_stmt == NULL)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "no def_stmt.");
      return false;
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "def_stmt: ");
      print_gimple_stmt (vect_dump, *def_stmt, 0, TDF_SLIM);
    }

  /* empty stmt is expected only in case of a function argument.
     (Otherwise - we expect a phi_node or a GIMPLE_ASSIGN).  */
  if (gimple_nop_p (*def_stmt))
    {
      *def = operand;
      *dt = vect_invariant_def;
      return true;
    }

  bb = gimple_bb (*def_stmt);
  if (!flow_bb_inside_loop_p (loop, bb))
    *dt = vect_invariant_def;
  else
    {
      stmt_vinfo = vinfo_for_stmt (*def_stmt);
      *dt = STMT_VINFO_DEF_TYPE (stmt_vinfo);
    }

  if (*dt == vect_unknown_def_type)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "Unsupported pattern.");
      return false;
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "type of def: %d.",*dt);

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
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "unsupported defining stmt: ");
      return false;
    }

  return true;
}


/* Function supportable_widening_operation

   Check whether an operation represented by the code CODE is a 
   widening operation that is supported by the target platform in 
   vector form (i.e., when operating on arguments of type VECTYPE).
    
   Widening operations we currently support are NOP (CONVERT), FLOAT
   and WIDEN_MULT.  This function checks if these operations are supported
   by the target platform either directly (via vector tree-codes), or via
   target builtins.

   Output:
   - CODE1 and CODE2 are codes of vector operations to be used when 
   vectorizing the operation, if available. 
   - DECL1 and DECL2 are decls of target builtin functions to be used
   when vectorizing the operation, if available. In this case,
   CODE1 and CODE2 are CALL_EXPR.  
   - MULTI_STEP_CVT determines the number of required intermediate steps in
   case of multi-step conversion (like char->short->int - in that case
   MULTI_STEP_CVT will be 1).
   - INTERM_TYPES contains the intermediate type required to perform the 
   widening operation (short in the above example).  */   

bool
supportable_widening_operation (enum tree_code code, gimple stmt, tree vectype,
                                tree *decl1, tree *decl2,
                                enum tree_code *code1, enum tree_code *code2,
                                int *multi_step_cvt,
                                VEC (tree, heap) **interm_types)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *vect_loop = LOOP_VINFO_LOOP (loop_info);
  bool ordered_p;
  enum machine_mode vec_mode;
  enum insn_code icode1 = 0, icode2 = 0;
  optab optab1, optab2;
  tree type = gimple_expr_type (stmt);
  tree wide_vectype = get_vectype_for_scalar_type (type);
  enum tree_code c1, c2;

  /* The result of a vectorized widening operation usually requires two vectors
     (because the widened results do not fit int one vector). The generated 
     vector results would normally be expected to be generated in the same 
     order as in the original scalar computation, i.e. if 8 results are
     generated in each vector iteration, they are to be organized as follows:
        vect1: [res1,res2,res3,res4], vect2: [res5,res6,res7,res8]. 

     However, in the special case that the result of the widening operation is 
     used in a reduction computation only, the order doesn't matter (because
     when vectorizing a reduction we change the order of the computation). 
     Some targets can take advantage of this and generate more efficient code.
     For example, targets like Altivec, that support widen_mult using a sequence
     of {mult_even,mult_odd} generate the following vectors:
        vect1: [res1,res3,res5,res7], vect2: [res2,res4,res6,res8].

     When vectorizing outer-loops, we execute the inner-loop sequentially
     (each vectorized inner-loop iteration contributes to VF outer-loop 
     iterations in parallel). We therefore don't allow to change the order 
     of the computation in the inner-loop during outer-loop vectorization.  */

   if (STMT_VINFO_RELEVANT (stmt_info) == vect_used_by_reduction
       && !nested_in_vect_loop_p (vect_loop, stmt))
     ordered_p = false;
   else
     ordered_p = true;

  if (!ordered_p
      && code == WIDEN_MULT_EXPR
      && targetm.vectorize.builtin_mul_widen_even
      && targetm.vectorize.builtin_mul_widen_even (vectype)
      && targetm.vectorize.builtin_mul_widen_odd
      && targetm.vectorize.builtin_mul_widen_odd (vectype))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "Unordered widening operation detected.");

      *code1 = *code2 = CALL_EXPR;
      *decl1 = targetm.vectorize.builtin_mul_widen_even (vectype);
      *decl2 = targetm.vectorize.builtin_mul_widen_odd (vectype);
      return true;
    }

  switch (code)
    {
    case WIDEN_MULT_EXPR:
      if (BYTES_BIG_ENDIAN)
        {
          c1 = VEC_WIDEN_MULT_HI_EXPR;
          c2 = VEC_WIDEN_MULT_LO_EXPR;
        }
      else
        {
          c2 = VEC_WIDEN_MULT_HI_EXPR;
          c1 = VEC_WIDEN_MULT_LO_EXPR;
        }
      break;

    CASE_CONVERT:
      if (BYTES_BIG_ENDIAN)
        {
          c1 = VEC_UNPACK_HI_EXPR;
          c2 = VEC_UNPACK_LO_EXPR;
        }
      else
        {
          c2 = VEC_UNPACK_HI_EXPR;
          c1 = VEC_UNPACK_LO_EXPR;
        }
      break;

    case FLOAT_EXPR:
      if (BYTES_BIG_ENDIAN)
        {
          c1 = VEC_UNPACK_FLOAT_HI_EXPR;
          c2 = VEC_UNPACK_FLOAT_LO_EXPR;
        }
      else
        {
          c2 = VEC_UNPACK_FLOAT_HI_EXPR;
          c1 = VEC_UNPACK_FLOAT_LO_EXPR;
        }
      break;

    case FIX_TRUNC_EXPR:
      /* ??? Not yet implemented due to missing VEC_UNPACK_FIX_TRUNC_HI_EXPR/
	 VEC_UNPACK_FIX_TRUNC_LO_EXPR tree codes and optabs used for
	 computing the operation.  */
      return false;

    default:
      gcc_unreachable ();
    }

  if (code == FIX_TRUNC_EXPR)
    {
      /* The signedness is determined from output operand.  */
      optab1 = optab_for_tree_code (c1, type, optab_default);
      optab2 = optab_for_tree_code (c2, type, optab_default);
    }
  else
    {
      optab1 = optab_for_tree_code (c1, vectype, optab_default);
      optab2 = optab_for_tree_code (c2, vectype, optab_default);
    }

  if (!optab1 || !optab2)
    return false;

  vec_mode = TYPE_MODE (vectype);
  if ((icode1 = optab_handler (optab1, vec_mode)->insn_code) == CODE_FOR_nothing
       || (icode2 = optab_handler (optab2, vec_mode)->insn_code)
                                                       == CODE_FOR_nothing)
    return false;

  /* Check if it's a multi-step conversion that can be done using intermediate 
     types.  */
  if (insn_data[icode1].operand[0].mode != TYPE_MODE (wide_vectype)
       || insn_data[icode2].operand[0].mode != TYPE_MODE (wide_vectype))
    {
      int i;
      tree prev_type = vectype, intermediate_type;
      enum machine_mode intermediate_mode, prev_mode = vec_mode;
      optab optab3, optab4;

      if (!CONVERT_EXPR_CODE_P (code))
        return false;
      
      *code1 = c1;
      *code2 = c2;
    
      /* We assume here that there will not be more than MAX_INTERM_CVT_STEPS
         intermediate  steps in promotion sequence. We try MAX_INTERM_CVT_STEPS
         to get to NARROW_VECTYPE, and fail if we do not.  */
      *interm_types = VEC_alloc (tree, heap, MAX_INTERM_CVT_STEPS);
      for (i = 0; i < 3; i++)
        {
          intermediate_mode = insn_data[icode1].operand[0].mode;
          intermediate_type = lang_hooks.types.type_for_mode (intermediate_mode,
                                                     TYPE_UNSIGNED (prev_type));
          optab3 = optab_for_tree_code (c1, intermediate_type, optab_default);
          optab4 = optab_for_tree_code (c2, intermediate_type, optab_default);

          if (!optab3 || !optab4
              || (icode1 = optab1->handlers[(int) prev_mode].insn_code)
                                                        == CODE_FOR_nothing
              || insn_data[icode1].operand[0].mode != intermediate_mode
              || (icode2 = optab2->handlers[(int) prev_mode].insn_code)
                                                        == CODE_FOR_nothing
              || insn_data[icode2].operand[0].mode != intermediate_mode
              || (icode1 = optab3->handlers[(int) intermediate_mode].insn_code) 
                                                        == CODE_FOR_nothing
              || (icode2 = optab4->handlers[(int) intermediate_mode].insn_code)
                                                        == CODE_FOR_nothing)
            return false;

          VEC_quick_push (tree, *interm_types, intermediate_type);
          (*multi_step_cvt)++;

          if (insn_data[icode1].operand[0].mode == TYPE_MODE (wide_vectype)
              && insn_data[icode2].operand[0].mode == TYPE_MODE (wide_vectype))
            return true;

          prev_type = intermediate_type;
          prev_mode = intermediate_mode;
        }

       return false;
    }

  *code1 = c1;
  *code2 = c2;
  return true;
}


/* Function supportable_narrowing_operation

   Check whether an operation represented by the code CODE is a 
   narrowing operation that is supported by the target platform in 
   vector form (i.e., when operating on arguments of type VECTYPE).
    
   Narrowing operations we currently support are NOP (CONVERT) and
   FIX_TRUNC. This function checks if these operations are supported by
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
				 const_gimple stmt, tree vectype,
				 enum tree_code *code1, int *multi_step_cvt,
                                 VEC (tree, heap) **interm_types)
{
  enum machine_mode vec_mode;
  enum insn_code icode1;
  optab optab1, interm_optab;
  tree type = gimple_expr_type (stmt);
  tree narrow_vectype = get_vectype_for_scalar_type (type);
  enum tree_code c1;
  tree intermediate_type, prev_type;
  int i;

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
    optab1 = optab_for_tree_code (c1, type, optab_default);
  else
    optab1 = optab_for_tree_code (c1, vectype, optab_default);

  if (!optab1)
    return false;

  vec_mode = TYPE_MODE (vectype);
  if ((icode1 = optab_handler (optab1, vec_mode)->insn_code) 
       == CODE_FOR_nothing)
    return false;

  /* Check if it's a multi-step conversion that can be done using intermediate
     types.  */
  if (insn_data[icode1].operand[0].mode != TYPE_MODE (narrow_vectype))
    {
      enum machine_mode intermediate_mode, prev_mode = vec_mode;

      *code1 = c1;
      prev_type = vectype;
      /* We assume here that there will not be more than MAX_INTERM_CVT_STEPS
         intermediate  steps in promotion sequence. We try MAX_INTERM_CVT_STEPS
         to get to NARROW_VECTYPE, and fail if we do not.  */
      *interm_types = VEC_alloc (tree, heap, MAX_INTERM_CVT_STEPS);
      for (i = 0; i < 3; i++)
        {
          intermediate_mode = insn_data[icode1].operand[0].mode;
          intermediate_type = lang_hooks.types.type_for_mode (intermediate_mode,
                                                     TYPE_UNSIGNED (prev_type));
          interm_optab = optab_for_tree_code (c1, intermediate_type, 
                                              optab_default);
          if (!interm_optab  
              || (icode1 = optab1->handlers[(int) prev_mode].insn_code)
                                                        == CODE_FOR_nothing
              || insn_data[icode1].operand[0].mode != intermediate_mode
              || (icode1 
                  = interm_optab->handlers[(int) intermediate_mode].insn_code)
                 == CODE_FOR_nothing)
            return false;

          VEC_quick_push (tree, *interm_types, intermediate_type);
          (*multi_step_cvt)++;

          if (insn_data[icode1].operand[0].mode == TYPE_MODE (narrow_vectype))
            return true;

          prev_type = intermediate_type;
          prev_mode = intermediate_mode;
        }

      return false;
    }

  *code1 = c1;
  return true;
}


/* Function reduction_code_for_scalar_code

   Input:
   CODE - tree_code of a reduction operations.

   Output:
   REDUC_CODE - the corresponding tree-code to be used to reduce the
      vector of partial results into a single scalar result (which
      will also reside in a vector).

   Return TRUE if a corresponding REDUC_CODE was found, FALSE otherwise.  */

bool
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

   Detect a cross-iteration def-use cycle that represents a simple
   reduction computation. We look for the following pattern:

   loop_header:
     a1 = phi < a0, a2 >
     a3 = ...
     a2 = operation (a3, a1)
  
   such that:
   1. operation is commutative and associative and it is safe to 
      change the order of the computation.
   2. no uses for a2 in the loop (a2 is used out of the loop)
   3. no uses of a1 in the loop besides the reduction operation.

   Condition 1 is tested here.
   Conditions 2,3 are tested in vect_mark_stmts_to_be_vectorized.  */

gimple
vect_is_simple_reduction (loop_vec_info loop_info, gimple phi)
{
  struct loop *loop = (gimple_bb (phi))->loop_father;
  struct loop *vect_loop = LOOP_VINFO_LOOP (loop_info);
  edge latch_e = loop_latch_edge (loop);
  tree loop_arg = PHI_ARG_DEF_FROM_EDGE (phi, latch_e);
  gimple def_stmt, def1, def2;
  enum tree_code code;
  tree op1, op2;
  tree type;
  int nloop_uses;
  tree name;
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  gcc_assert (loop == vect_loop || flow_loop_nested_p (vect_loop, loop));

  name = PHI_RESULT (phi);
  nloop_uses = 0;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, name)
    {
      gimple use_stmt = USE_STMT (use_p);
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

  if (!is_gimple_assign (def_stmt))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        print_gimple_stmt (vect_dump, def_stmt, 0, TDF_SLIM);
      return NULL;
    }

  name = gimple_assign_lhs (def_stmt);
  nloop_uses = 0;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, name)
    {
      gimple use_stmt = USE_STMT (use_p);
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

  code = gimple_assign_rhs_code (def_stmt);

  if (!commutative_tree_code (code) || !associative_tree_code (code))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        report_vect_op (def_stmt, "reduction: not commutative/associative: ");
      return NULL;
    }

  if (get_gimple_rhs_class (code) != GIMPLE_BINARY_RHS)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: not binary operation: ");
      return NULL;
    }

  op1 = gimple_assign_rhs1 (def_stmt);
  op2 = gimple_assign_rhs2 (def_stmt);
  if (TREE_CODE (op1) != SSA_NAME || TREE_CODE (op2) != SSA_NAME)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: uses not ssa_names: ");
      return NULL;
    }

  /* Check that it's ok to change the order of the computation.  */
  type = TREE_TYPE (gimple_assign_lhs (def_stmt));
  if (TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (TREE_TYPE (op1))
      || TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (TREE_TYPE (op2)))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "reduction: multiple types: operation type: ");
          print_generic_expr (vect_dump, type, TDF_SLIM);
          fprintf (vect_dump, ", operands types: ");
          print_generic_expr (vect_dump, TREE_TYPE (op1), TDF_SLIM);
          fprintf (vect_dump, ",");
          print_generic_expr (vect_dump, TREE_TYPE (op2), TDF_SLIM);
        }
      return NULL;
    }

  /* Generally, when vectorizing a reduction we change the order of the
     computation.  This may change the behavior of the program in some
     cases, so we need to check that this is ok.  One exception is when 
     vectorizing an outer-loop: the inner-loop is executed sequentially,
     and therefore vectorizing reductions in the inner-loop during
     outer-loop vectorization is safe.  */

  /* CHECKME: check for !flag_finite_math_only too?  */
  if (SCALAR_FLOAT_TYPE_P (type) && !flag_associative_math
      && !nested_in_vect_loop_p (vect_loop, def_stmt)) 
    {
      /* Changing the order of operations changes the semantics.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: unsafe fp math optimization: ");
      return NULL;
    }
  else if (INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_TRAPS (type)
	   && !nested_in_vect_loop_p (vect_loop, def_stmt))
    {
      /* Changing the order of operations changes the semantics.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: unsafe int math optimization: ");
      return NULL;
    }
  else if (SAT_FIXED_POINT_TYPE_P (type))
    {
      /* Changing the order of operations changes the semantics.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, 
			"reduction: unsafe fixed-point math optimization: ");
      return NULL;
    }

  /* reduction is safe. we're dealing with one of the following:
     1) integer arithmetic and no trapv
     2) floating point arithmetic, and special flags permit this optimization.
   */
  def1 = SSA_NAME_DEF_STMT (op1);
  def2 = SSA_NAME_DEF_STMT (op2);
  if (!def1 || !def2 || gimple_nop_p (def1) || gimple_nop_p (def2))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: no defs for operands: ");
      return NULL;
    }


  /* Check that one def is the reduction def, defined by PHI,
     the other def is either defined in the loop ("vect_loop_def"),
     or it's an induction (defined by a loop-header phi-node).  */

  if (def2 == phi
      && flow_bb_inside_loop_p (loop, gimple_bb (def1))
      && (is_gimple_assign (def1)
	  || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def1)) == vect_induction_def
	  || (gimple_code (def1) == GIMPLE_PHI
	      && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def1)) == vect_loop_def
	      && !is_loop_header_bb_p (gimple_bb (def1)))))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "detected reduction:");
      return def_stmt;
    }
  else if (def1 == phi
	   && flow_bb_inside_loop_p (loop, gimple_bb (def2))
	   && (is_gimple_assign (def2)
	       || STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def2)) == vect_induction_def
	       || (gimple_code (def2) == GIMPLE_PHI
		   && STMT_VINFO_DEF_TYPE (vinfo_for_stmt (def2)) == vect_loop_def
		   && !is_loop_header_bb_p (gimple_bb (def2)))))
    {
      /* Swap operands (just for simplicity - so that the rest of the code
	 can assume that the reduction variable is always the last (second)
	 argument).  */
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt ,
		        "detected reduction: need to swap operands:");
      swap_tree_operands (def_stmt, gimple_assign_rhs1_ptr (def_stmt),
			  gimple_assign_rhs2_ptr (def_stmt));
      return def_stmt;
    }
  else
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	report_vect_op (def_stmt, "reduction: unknown pattern.");
      return NULL;
    }
}


/* Function vect_is_simple_iv_evolution.

   FORNOW: A simple evolution of an induction variables in the loop is
   considered a polynomial evolution with constant step.  */

bool
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


/* Function vectorize_loops.
   
   Entry Point to loop vectorization phase.  */

unsigned
vectorize_loops (void)
{
  unsigned int i;
  unsigned int num_vectorized_loops = 0;
  unsigned int vect_loops_num;
  loop_iterator li;
  struct loop *loop;

  vect_loops_num = number_of_loops ();

  /* Bail out if there are no loops.  */
  if (vect_loops_num <= 1)
    return 0;

  /* Fix the verbosity level if not defined explicitly by the user.  */
  vect_set_dump_settings ();

  /* Allocate the bitmap that records which virtual variables that 
     need to be renamed.  */
  vect_memsyms_to_rename = BITMAP_ALLOC (NULL);

  init_stmt_vec_info_vec ();

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number 
     than all previously defined loops. This fact allows us to run 
     only over initial loops skipping newly generated ones.  */
  FOR_EACH_LOOP (li, loop, 0)
    if (optimize_loop_nest_for_speed_p (loop))
      {
	loop_vec_info loop_vinfo;

	vect_loop_location = find_loop_location (loop);
	loop_vinfo = vect_analyze_loop (loop);
	loop->aux = loop_vinfo;

	if (!loop_vinfo || !LOOP_VINFO_VECTORIZABLE_P (loop_vinfo))
	  continue;

	vect_transform_loop (loop_vinfo);
	num_vectorized_loops++;
      }
  vect_loop_location = UNKNOWN_LOC;

  statistics_counter_event (cfun, "Vectorized loops", num_vectorized_loops);
  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS)
      || (vect_print_dump_info (REPORT_VECTORIZED_LOOPS)
	  && num_vectorized_loops > 0))
    fprintf (vect_dump, "vectorized %u loops in function.\n",
	     num_vectorized_loops);

  /*  ----------- Finalize. -----------  */

  BITMAP_FREE (vect_memsyms_to_rename);

  for (i = 1; i < vect_loops_num; i++)
    {
      loop_vec_info loop_vinfo;

      loop = get_loop (i);
      if (!loop)
	continue;
      loop_vinfo = (loop_vec_info) loop->aux;
      destroy_loop_vec_info (loop_vinfo, true);
      loop->aux = NULL;
    }

  free_stmt_vec_info_vec ();

  return num_vectorized_loops > 0 ? TODO_cleanup_cfg : 0;
}

/* Increase alignment of global arrays to improve vectorization potential.
   TODO:
   - Consider also structs that have an array field.
   - Use ipa analysis to prune arrays that can't be vectorized?
     This should involve global alignment analysis and in the future also
     array padding.  */

static unsigned int
increase_alignment (void)
{
  struct varpool_node *vnode;

  /* Increase the alignment of all global arrays for vectorization.  */
  for (vnode = varpool_nodes_queue;
       vnode;
       vnode = vnode->next_needed)
    {
      tree vectype, decl = vnode->decl;
      unsigned int alignment;

      if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
	continue;
      vectype = get_vectype_for_scalar_type (TREE_TYPE (TREE_TYPE (decl)));
      if (!vectype)
	continue;
      alignment = TYPE_ALIGN (vectype);
      if (DECL_ALIGN (decl) >= alignment)
	continue;

      if (vect_can_force_dr_alignment_p (decl, alignment))
	{ 
	  DECL_ALIGN (decl) = TYPE_ALIGN (vectype);
	  DECL_USER_ALIGN (decl) = 1;
	  if (dump_file)
	    { 
	      fprintf (dump_file, "Increasing alignment of decl: ");
	      print_generic_expr (dump_file, decl, TDF_SLIM);
	    }
	}
    }
  return 0;
}

static bool
gate_increase_alignment (void)
{
  return flag_section_anchors && flag_tree_vectorize;
}

struct simple_ipa_opt_pass pass_ipa_increase_alignment = 
{
 {
  SIMPLE_IPA_PASS,
  "increase_alignment",			/* name */
  gate_increase_alignment,		/* gate */
  increase_alignment,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0 					/* todo_flags_finish */
 }
};
