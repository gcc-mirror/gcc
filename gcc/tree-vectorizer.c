/* Loop Vectorization
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
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
   vector stmt VS1 pointed by STMT_VINFO_VEC_STMT (stmt_info (S1)). The
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
   machine_mode (e.g, add_optab->handlers[(int) V8HImode].insn_code). If
   the value found is CODE_FOR_nothing, then there's no target support, and
   we can't vectorize the stmt.

   For additional information on this project see:
   http://gcc.gnu.org/projects/tree-ssa/vectorization.html
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
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
#include "optabs.h"
#include "toplev.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "input.h"
#include "tree-vectorizer.h"
#include "tree-pass.h"

/*************************************************************************
  Simple Loop Peeling Utilities
 *************************************************************************/
static struct loop *slpeel_tree_duplicate_loop_to_edge_cfg 
  (struct loop *, struct loops *, edge);
static void slpeel_update_phis_for_duplicate_loop 
  (struct loop *, struct loop *, bool after);
static void slpeel_update_phi_nodes_for_guard (edge, struct loop *, bool, bool);
static edge slpeel_add_loop_guard (basic_block, tree, basic_block, basic_block);

static void allocate_new_names (bitmap);
static void rename_use_op (use_operand_p);
static void rename_def_op (def_operand_p, tree);
static void rename_variables_in_bb (basic_block);
static void free_new_names (bitmap);
static void rename_variables_in_loop (struct loop *);

/*************************************************************************
  General Vectorization Utilities
 *************************************************************************/
static void vect_set_dump_settings (void);
static bool need_imm_uses_for (tree);

/* vect_dump will be set to stderr or dump_file if exist.  */
FILE *vect_dump;

/* vect_verbosity_level set to an invalid value 
   to mark that it's uninitialized.  */
enum verbosity_levels vect_verbosity_level = MAX_VERBOSITY_LEVEL;



/*************************************************************************
  Simple Loop Peeling Utilities

  Utilities to support loop peeling for vectorization purposes.
 *************************************************************************/


/* For each definition in DEFINITIONS this function allocates 
   new ssa name.  */

static void
allocate_new_names (bitmap definitions)
{
  unsigned ver;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (definitions, 0, ver, bi)
    {
      tree def = ssa_name (ver);
      tree *new_name_ptr = xmalloc (sizeof (tree));

      bool abnormal = SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def);

      *new_name_ptr = duplicate_ssa_name (def, SSA_NAME_DEF_STMT (def));
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (*new_name_ptr) = abnormal;

      SSA_NAME_AUX (def) = new_name_ptr;
    }
}


/* Renames the use *OP_P.  */

static void
rename_use_op (use_operand_p op_p)
{
  tree *new_name_ptr;

  if (TREE_CODE (USE_FROM_PTR (op_p)) != SSA_NAME)
    return;

  new_name_ptr = SSA_NAME_AUX (USE_FROM_PTR (op_p));

  /* Something defined outside of the loop.  */
  if (!new_name_ptr)
    return;

  /* An ordinary ssa name defined in the loop.  */

  SET_USE (op_p, *new_name_ptr);
}


/* Renames the def *OP_P in statement STMT.  */

static void
rename_def_op (def_operand_p op_p, tree stmt)
{
  tree *new_name_ptr;

  if (TREE_CODE (DEF_FROM_PTR (op_p)) != SSA_NAME)
    return;

  new_name_ptr = SSA_NAME_AUX (DEF_FROM_PTR (op_p));

  /* Something defined outside of the loop.  */
  if (!new_name_ptr)
    return;

  /* An ordinary ssa name defined in the loop.  */

  SET_DEF (op_p, *new_name_ptr);
  SSA_NAME_DEF_STMT (DEF_FROM_PTR (op_p)) = stmt;
}


/* Renames the variables in basic block BB.  */

static void
rename_variables_in_bb (basic_block bb)
{
  tree phi;
  block_stmt_iterator bsi;
  tree stmt;
  stmt_ann_t ann;
  use_optype uses;
  vuse_optype vuses;
  def_optype defs;
  v_may_def_optype v_may_defs;
  v_must_def_optype v_must_defs;
  unsigned i;
  edge e;
  edge_iterator ei;
  struct loop *loop = bb->loop_father;

  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    rename_def_op (PHI_RESULT_PTR (phi), phi);

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      stmt = bsi_stmt (bsi);
      get_stmt_operands (stmt);
      ann = stmt_ann (stmt);

      uses = USE_OPS (ann);
      for (i = 0; i < NUM_USES (uses); i++)
	rename_use_op (USE_OP_PTR (uses, i));

      defs = DEF_OPS (ann);
      for (i = 0; i < NUM_DEFS (defs); i++)
	rename_def_op (DEF_OP_PTR (defs, i), stmt);

      vuses = VUSE_OPS (ann);
      for (i = 0; i < NUM_VUSES (vuses); i++)
	rename_use_op (VUSE_OP_PTR (vuses, i));

      v_may_defs = V_MAY_DEF_OPS (ann);
      for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
	{
	  rename_use_op (V_MAY_DEF_OP_PTR (v_may_defs, i));
	  rename_def_op (V_MAY_DEF_RESULT_PTR (v_may_defs, i), stmt);
	}

      v_must_defs = V_MUST_DEF_OPS (ann);
      for (i = 0; i < NUM_V_MUST_DEFS (v_must_defs); i++)
	{
	  rename_use_op (V_MUST_DEF_KILL_PTR (v_must_defs, i));
	  rename_def_op (V_MUST_DEF_RESULT_PTR (v_must_defs, i), stmt);
	}
    }

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (!flow_bb_inside_loop_p (loop, e->dest))
	continue;
      for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
        rename_use_op (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e));
    }
}


/* Releases the structures holding the new ssa names.  */

static void
free_new_names (bitmap definitions)
{
  unsigned ver;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (definitions, 0, ver, bi)
    {
      tree def = ssa_name (ver);

      if (SSA_NAME_AUX (def))
	{
	  free (SSA_NAME_AUX (def));
	  SSA_NAME_AUX (def) = NULL;
	}
    }
}


/* Renames variables in new generated LOOP.  */

static void
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
  tree *new_name_ptr, new_ssa_name;
  tree phi_new, phi_orig;
  tree def;
  edge orig_loop_latch = loop_latch_edge (orig_loop);
  edge orig_entry_e = loop_preheader_edge (orig_loop);
  edge new_loop_exit_e = new_loop->exit_edges[0];
  edge new_loop_entry_e = loop_preheader_edge (new_loop);
  edge entry_arg_e = (after ? orig_loop_latch : orig_entry_e);

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

  for (phi_new = phi_nodes (new_loop->header),
       phi_orig = phi_nodes (orig_loop->header);
       phi_new && phi_orig;
       phi_new = PHI_CHAIN (phi_new), phi_orig = PHI_CHAIN (phi_orig))
    {
      /* step 1.  */
      def = PHI_ARG_DEF_FROM_EDGE (phi_orig, entry_arg_e);
      add_phi_arg (phi_new, def, new_loop_entry_e);

      /* step 2.  */
      def = PHI_ARG_DEF_FROM_EDGE (phi_orig, orig_loop_latch);
      if (TREE_CODE (def) != SSA_NAME)
        continue;

      new_name_ptr = SSA_NAME_AUX (def);
      if (!new_name_ptr)
        /* Something defined outside of the loop.  */
        continue;

      /* An ordinary ssa name defined in the loop.  */
      new_ssa_name = *new_name_ptr;
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
        We denote this bb NEW_MERGE_BB because it had a single predecessor (the
        LOOP header) before the guard code was added, and now it became a merge
        point of two paths - the path that ends with the LOOP exit-edge, and
        the path that ends with GUARD_EDGE.

        This function creates and updates the relevant phi nodes to account for
        the new incoming edge (GUARD_EDGE) into NEW_MERGE_BB:
        1. Create phi nodes at NEW_MERGE_BB.
        2. Update the phi nodes at the successor of NEW_MERGE_BB (denoted
           UPDATE_BB).  UPDATE_BB was the exit-bb of LOOP before NEW_MERGE_BB
           was added:

        ===> The CFG before the guard-code was added:
        LOOP_header_bb:
          if (exit_loop) goto update_bb : LOOP_header_bb
        update_bb:

        ==> The CFG after the guard-code was added:
        guard_bb: 
          if (LOOP_guard_condition) goto new_merge_bb : LOOP_header_bb
        LOOP_header_bb:
          if (exit_loop_condition) goto new_merge_bb : LOOP_header_bb
        new_merge_bb:
          goto update_bb
        update_bb:

   - ENTRY_PHIS: If ENTRY_PHIS is TRUE, this indicates that the phis in 
        UPDATE_BB are loop entry phis, like the phis in the LOOP header,
        organized in the same order. 
        If ENTRY_PHIs is FALSE, this indicates that the phis in UPDATE_BB are
        loop exit phis.

   - IS_NEW_LOOP: TRUE if LOOP is a new loop (a duplicated copy of another
        "original" loop).  FALSE if LOOP is an original loop (not a newly 
        created copy).  The SSA_NAME_AUX fields of the defs in the original
        loop are the corresponding new ssa-names used in the new duplicated
        loop copy.  IS_NEW_LOOP indicates which of the two args of the phi 
        nodes in UPDATE_BB takes the original ssa-name, and which takes the 
        new name: If IS_NEW_LOOP is TRUE, the phi-arg that is associated with
        the LOOP-exit-edge takes the new-name, and the phi-arg that is 
        associated with GUARD_EDGE takes the original name.  If IS_NEW_LOOP is
        FALSE, it's the other way around.
  */

static void
slpeel_update_phi_nodes_for_guard (edge guard_edge, 
				   struct loop *loop,
				   bool entry_phis,
				   bool is_new_loop)
{
  tree orig_phi, new_phi, update_phi;
  tree guard_arg, loop_arg;
  basic_block new_merge_bb = guard_edge->dest;
  edge e = EDGE_SUCC (new_merge_bb, 0);
  basic_block update_bb = e->dest;
  basic_block orig_bb = (entry_phis ? loop->header : update_bb);

  for (orig_phi = phi_nodes (orig_bb), update_phi = phi_nodes (update_bb);
       orig_phi && update_phi;
       orig_phi = PHI_CHAIN (orig_phi), update_phi = PHI_CHAIN (update_phi))
    {
      /* 1. Generate new phi node in NEW_MERGE_BB:  */
      new_phi = create_phi_node (SSA_NAME_VAR (PHI_RESULT (orig_phi)),
                                 new_merge_bb);

      /* 2. NEW_MERGE_BB has two incoming edges: GUARD_EDGE and the exit-edge
            of LOOP. Set the two phi args in NEW_PHI for these edges:  */
      if (entry_phis)
        {
          loop_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi,
                                            EDGE_SUCC (loop->latch, 0));
          guard_arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, loop->entry_edges[0]);
        }
      else /* exit phis */
        {
          tree orig_def = PHI_ARG_DEF_FROM_EDGE (orig_phi, e);
          tree *new_name_ptr = SSA_NAME_AUX (orig_def);
          tree new_name;

          if (new_name_ptr)
            new_name = *new_name_ptr;
          else
            /* Something defined outside of the loop  */
            new_name = orig_def;

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
        }
      add_phi_arg (new_phi, loop_arg, loop->exit_edges[0]);
      add_phi_arg (new_phi, guard_arg, guard_edge);

      /* 3. Update phi in successor block.  */
      gcc_assert (PHI_ARG_DEF_FROM_EDGE (update_phi, e) == loop_arg
                  || PHI_ARG_DEF_FROM_EDGE (update_phi, e) == guard_arg);
      SET_PHI_ARG_DEF (update_phi, e->dest_idx, PHI_RESULT (new_phi));
    }

  set_phi_nodes (new_merge_bb, phi_reverse (phi_nodes (new_merge_bb)));
}


/* Make the LOOP iterate NITERS times. This is done by adding a new IV
   that starts at zero, increases by one and its limit is NITERS.

   Assumption: the exit-condition of LOOP is the last stmt in the loop.  */

void
slpeel_make_loop_iterate_ntimes (struct loop *loop, tree niters)
{
  tree indx_before_incr, indx_after_incr, cond_stmt, cond;
  tree orig_cond;
  edge exit_edge = loop->exit_edges[0];
  block_stmt_iterator loop_cond_bsi;
  block_stmt_iterator incr_bsi;
  bool insert_after;
  tree begin_label = tree_block_label (loop->latch);
  tree exit_label = tree_block_label (loop->single_exit->dest);
  tree init = build_int_cst (TREE_TYPE (niters), 0);
  tree step = build_int_cst (TREE_TYPE (niters), 1);
  tree then_label;
  tree else_label;
  LOC loop_loc;

  orig_cond = get_loop_exit_condition (loop);
#ifdef ENABLE_CHECKING
  gcc_assert (orig_cond);
#endif
  loop_cond_bsi = bsi_for_stmt (orig_cond);

  standard_iv_increment_position (loop, &incr_bsi, &insert_after);
  create_iv (init, step, NULL_TREE, loop,
             &incr_bsi, insert_after, &indx_before_incr, &indx_after_incr);

  if (exit_edge->flags & EDGE_TRUE_VALUE) /* 'then' edge exits the loop.  */
    {
      cond = build2 (GE_EXPR, boolean_type_node, indx_after_incr, niters);
      then_label = build1 (GOTO_EXPR, void_type_node, exit_label);
      else_label = build1 (GOTO_EXPR, void_type_node, begin_label);
    }
  else /* 'then' edge loops back.  */
    {
      cond = build2 (LT_EXPR, boolean_type_node, indx_after_incr, niters);
      then_label = build1 (GOTO_EXPR, void_type_node, begin_label);
      else_label = build1 (GOTO_EXPR, void_type_node, exit_label);
    }

  cond_stmt = build3 (COND_EXPR, TREE_TYPE (orig_cond), cond,
		     then_label, else_label);
  bsi_insert_before (&loop_cond_bsi, cond_stmt, BSI_SAME_STMT);

  /* Remove old loop exit test:  */
  bsi_remove (&loop_cond_bsi);

  loop_loc = find_loop_location (loop);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (loop_loc != UNKNOWN_LOC)
        fprintf (dump_file, "\nloop at %s:%d: ",
                 LOC_FILE (loop_loc), LOC_LINE (loop_loc));
      print_generic_expr (dump_file, cond_stmt, TDF_SLIM);
    }

  loop->nb_iterations = niters;
}


/* Given LOOP this function generates a new copy of it and puts it 
   on E which is either the entry or exit of LOOP.  */

static struct loop *
slpeel_tree_duplicate_loop_to_edge_cfg (struct loop *loop, struct loops *loops, 
					edge e)
{
  struct loop *new_loop;
  basic_block *new_bbs, *bbs;
  bool at_exit;
  bool was_imm_dom;
  basic_block exit_dest; 
  tree phi, phi_arg;

  at_exit = (e == loop->exit_edges[0]); 
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
  new_loop = duplicate_loop (loops, loop, loop->outer);
  if (!new_loop)
    {
      free (bbs);
      return NULL;
    }

  exit_dest = loop->exit_edges[0]->dest;
  was_imm_dom = (get_immediate_dominator (CDI_DOMINATORS, 
					  exit_dest) == loop->header ? 
		 true : false);

  new_bbs = xmalloc (sizeof (basic_block) * loop->num_nodes);

  copy_bbs (bbs, loop->num_nodes, new_bbs, NULL, 0, NULL, NULL);

  /* Duplicating phi args at exit bbs as coming 
     also from exit of duplicated loop.  */
  for (phi = phi_nodes (exit_dest); phi; phi = PHI_CHAIN (phi))
    {
      phi_arg = PHI_ARG_DEF_FROM_EDGE (phi, loop->exit_edges[0]);
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
      set_immediate_dominator (CDI_DOMINATORS, loop->header,
			       new_exit_e->src);

      /* We have to add phi args to the loop->header here as coming 
	 from new_exit_e edge.  */
      for (phi = phi_nodes (loop->header); phi; phi = PHI_CHAIN (phi))
	{
	  phi_arg = PHI_ARG_DEF_FROM_EDGE (phi, entry_e);
	  if (phi_arg)
	    add_phi_arg (phi, phi_arg, new_exit_e);	
	}    

      redirect_edge_and_branch_force (entry_e, new_loop->header);
      set_immediate_dominator (CDI_DOMINATORS, new_loop->header, preheader);
    }

  flow_loop_scan (new_loop, LOOP_ALL);
  flow_loop_scan (loop, LOOP_ALL);  
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
  block_stmt_iterator bsi;
  edge new_e, enter_e;
  tree cond_stmt, then_label, else_label;

  enter_e = EDGE_SUCC (guard_bb, 0);
  enter_e->flags &= ~EDGE_FALLTHRU;
  enter_e->flags |= EDGE_FALSE_VALUE;
  bsi = bsi_last (guard_bb);

  then_label = build1 (GOTO_EXPR, void_type_node,
                       tree_block_label (exit_bb));
  else_label = build1 (GOTO_EXPR, void_type_node,
                       tree_block_label (enter_e->dest));
  cond_stmt = build3 (COND_EXPR, void_type_node, cond,
   		     then_label, else_label);
  bsi_insert_after (&bsi, cond_stmt, BSI_NEW_STMT);
  /* Add new edge to connect entry block to the second loop.  */
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
slpeel_can_duplicate_loop_p (struct loop *loop, edge e)
{
  edge exit_e = loop->exit_edges [0];
  edge entry_e = loop_preheader_edge (loop);
  tree orig_cond = get_loop_exit_condition (loop);
  block_stmt_iterator loop_exit_bsi = bsi_last (exit_e->src);

  if (any_marked_for_rewrite_p ())
    return false;

  if (loop->inner
      /* All loops have an outer scope; the only case loop->outer is NULL is for
         the function itself.  */
      || !loop->outer
      || loop->num_nodes != 2
      || !empty_block_p (loop->latch)
      || loop->num_exits != 1
      || loop->num_entries != 1
      /* Verify that new loop exit condition can be trivially modified.  */
      || (!orig_cond || orig_cond != bsi_stmt (loop_exit_bsi))
      || (e != exit_e && e != entry_e))
    return false;

  return true;
}

#ifdef ENABLE_CHECKING
void
slpeel_verify_cfg_after_peeling (struct loop *first_loop,
                                 struct loop *second_loop)
{
  basic_block loop1_exit_bb = first_loop->exit_edges[0]->dest;
  basic_block loop2_entry_bb = second_loop->pre_header;
  basic_block loop1_entry_bb = loop_preheader_edge (first_loop)->src;

  /* A guard that controls whether the second_loop is to be executed or skipped
     is placed in first_loop->exit.  first_loopt->exit therefore has two
     successors - one is the preheader of second_loop, and the other is a bb
     after second_loop.
   */
  gcc_assert (EDGE_COUNT (loop1_exit_bb->succs) == 2);
   
   
  /* 1. Verify that one of the successors of first_loopt->exit is the preheader
        of second_loop.  */
   
  /* The preheader of new_loop is expected to have two predessors:
     first_loop->exit and the block that precedes first_loop.  */

  gcc_assert (EDGE_COUNT (loop2_entry_bb->preds) == 2 
              && ((EDGE_PRED (loop2_entry_bb, 0)->src == loop1_exit_bb
                   && EDGE_PRED (loop2_entry_bb, 1)->src == loop1_entry_bb)
               || (EDGE_PRED (loop2_entry_bb, 1)->src ==  loop1_exit_bb
                   && EDGE_PRED (loop2_entry_bb, 0)->src == loop1_entry_bb)));
  
  /* Verify that the other successor of first_loopt->exit is after the
     second_loop.  */
  /* TODO */
}
#endif

/* Function slpeel_tree_peel_loop_to_edge.

   Peel the first (last) iterations of LOOP into a new prolog (epilog) loop
   that is placed on the entry (exit) edge E of LOOP. After this transformation
   we have two loops one after the other - first-loop iterates FIRST_NITERS
   times, and second-loop iterates the remainder NITERS - FIRST_NITERS times.

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
slpeel_tree_peel_loop_to_edge (struct loop *loop, struct loops *loops, 
			       edge e, tree first_niters, 
			       tree niters, bool update_first_loop_count)
{
  struct loop *new_loop = NULL, *first_loop, *second_loop;
  edge skip_e;
  tree pre_condition;
  bitmap definitions;
  basic_block bb_before_second_loop, bb_after_second_loop;
  basic_block bb_before_first_loop;
  basic_block bb_between_loops;
  edge exit_e = loop->exit_edges [0];
  LOC loop_loc;
  
  if (!slpeel_can_duplicate_loop_p (loop, e))
    return NULL;
  
  /* We have to initialize cfg_hooks. Then, when calling
   cfg_hooks->split_edge, the function tree_split_edge 
   is actually called and, when calling cfg_hooks->duplicate_block,
   the function tree_duplicate_bb is called.  */
  tree_register_cfg_hooks ();


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
  
  if (!(new_loop = slpeel_tree_duplicate_loop_to_edge_cfg (loop, loops, e)))
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

  definitions = marked_ssa_names ();
  allocate_new_names (definitions);
  slpeel_update_phis_for_duplicate_loop (loop, new_loop, e == exit_e);
  rename_variables_in_loop (new_loop);


  /* 2. Add the guard that controls whether the first loop is executed.
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
   */

  bb_before_first_loop = split_edge (loop_preheader_edge (first_loop));
  add_bb_to_loop (bb_before_first_loop, first_loop->outer);
  bb_before_second_loop = split_edge (first_loop->exit_edges[0]);
  add_bb_to_loop (bb_before_second_loop, first_loop->outer);
  flow_loop_scan (first_loop, LOOP_ALL);
  flow_loop_scan (second_loop, LOOP_ALL);

  pre_condition =
        build2 (LE_EXPR, boolean_type_node, first_niters, integer_zero_node);
  skip_e = slpeel_add_loop_guard (bb_before_first_loop, pre_condition,
                                  bb_before_second_loop, bb_before_first_loop);
  slpeel_update_phi_nodes_for_guard (skip_e, first_loop, true /* entry-phis */,
                                     first_loop == new_loop);


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

  bb_between_loops = split_edge (first_loop->exit_edges[0]);
  add_bb_to_loop (bb_between_loops, first_loop->outer);
  bb_after_second_loop = split_edge (second_loop->exit_edges[0]);
  add_bb_to_loop (bb_after_second_loop, second_loop->outer);
  flow_loop_scan (first_loop, LOOP_ALL);
  flow_loop_scan (second_loop, LOOP_ALL);

  pre_condition = build2 (EQ_EXPR, boolean_type_node, first_niters, niters);
  skip_e = slpeel_add_loop_guard (bb_between_loops, pre_condition,
                                  bb_after_second_loop, bb_before_first_loop);
  slpeel_update_phi_nodes_for_guard (skip_e, second_loop, false /* exit-phis */,
                                     second_loop == new_loop);

  /* Flow loop scan does not update loop->single_exit field.  */
  first_loop->single_exit = first_loop->exit_edges[0];
  second_loop->single_exit = second_loop->exit_edges[0];

  /* 4. Make first-loop iterate FIRST_NITERS times, if requested.
   */
  if (update_first_loop_count)
    slpeel_make_loop_iterate_ntimes (first_loop, first_niters);

  free_new_names (definitions);
  BITMAP_FREE (definitions);
  unmark_all_for_rewrite ();

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
  tree node = NULL_TREE;
  basic_block bb;
  block_stmt_iterator si;

  if (!loop)
    return UNKNOWN_LOC;

  node = get_loop_exit_condition (loop);

  if (node && EXPR_P (node) && EXPR_HAS_LOCATION (node)
      && EXPR_FILENAME (node) && EXPR_LINENO (node))
    return EXPR_LOC (node);

  /* If we got here the loop is probably not "well formed",
     try to estimate the loop location */

  if (!loop->header)
    return UNKNOWN_LOC;

  bb = loop->header;

  for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
    {
      node = bsi_stmt (si);
      if (node && EXPR_P (node) && EXPR_HAS_LOCATION (node))
        return EXPR_LOC (node);
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
vect_print_dump_info (enum verbosity_levels vl, LOC loc)
{
  if (vl > vect_verbosity_level)
    return false;

  if (loc == UNKNOWN_LOC)
    fprintf (vect_dump, "\n%s:%d: note: ",
		 DECL_SOURCE_FILE (current_function_decl),
		 DECL_SOURCE_LINE (current_function_decl));
  else
    fprintf (vect_dump, "\n%s:%d: note: ", LOC_FILE (loc), LOC_LINE (loc));


  return true;
}


/*************************************************************************
  Vectorization Utilities.
 *************************************************************************/

/* Function new_stmt_vec_info.

   Create and initialize a new stmt_vec_info struct for STMT.  */

stmt_vec_info
new_stmt_vec_info (tree stmt, loop_vec_info loop_vinfo)
{
  stmt_vec_info res;
  res = (stmt_vec_info) xcalloc (1, sizeof (struct _stmt_vec_info));

  STMT_VINFO_TYPE (res) = undef_vec_info_type;
  STMT_VINFO_STMT (res) = stmt;
  STMT_VINFO_LOOP_VINFO (res) = loop_vinfo;
  STMT_VINFO_RELEVANT_P (res) = 0;
  STMT_VINFO_VECTYPE (res) = NULL;
  STMT_VINFO_VEC_STMT (res) = NULL;
  STMT_VINFO_DATA_REF (res) = NULL;
  STMT_VINFO_MEMTAG (res) = NULL;
  STMT_VINFO_VECT_DR_BASE_ADDRESS (res) = NULL;
  STMT_VINFO_VECT_INIT_OFFSET (res) = NULL_TREE;
  STMT_VINFO_VECT_STEP (res) = NULL_TREE;
  STMT_VINFO_VECT_BASE_ALIGNED_P (res) = false;
  STMT_VINFO_VECT_MISALIGNMENT (res) = NULL_TREE;

  return res;
}


/* Function new_loop_vec_info.

   Create and initialize a new loop_vec_info struct for LOOP, as well as
   stmt_vec_info structs for all the stmts in LOOP.  */

loop_vec_info
new_loop_vec_info (struct loop *loop)
{
  loop_vec_info res;
  basic_block *bbs;
  block_stmt_iterator si;
  unsigned int i;

  res = (loop_vec_info) xcalloc (1, sizeof (struct _loop_vec_info));

  bbs = get_loop_body (loop);

  /* Create stmt_info for all stmts in the loop.  */
  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  tree stmt = bsi_stmt (si);
	  stmt_ann_t ann;

	  get_stmt_operands (stmt);
	  ann = stmt_ann (stmt);
	  set_stmt_info (ann, new_stmt_vec_info (stmt, res));
	}
    }

  LOOP_VINFO_LOOP (res) = loop;
  LOOP_VINFO_BBS (res) = bbs;
  LOOP_VINFO_EXIT_COND (res) = NULL;
  LOOP_VINFO_NITERS (res) = NULL;
  LOOP_VINFO_VECTORIZABLE_P (res) = 0;
  LOOP_DO_PEELING_FOR_ALIGNMENT (res) = false;
  LOOP_VINFO_VECT_FACTOR (res) = 0;
  VARRAY_GENERIC_PTR_INIT (LOOP_VINFO_DATAREF_WRITES (res), 20,
			   "loop_write_datarefs");
  VARRAY_GENERIC_PTR_INIT (LOOP_VINFO_DATAREF_READS (res), 20,
			   "loop_read_datarefs");
  LOOP_VINFO_UNALIGNED_DR (res) = NULL;
  LOOP_VINFO_LOC (res) = UNKNOWN_LOC;

  return res;
}


/* Function destroy_loop_vec_info.
 
   Free LOOP_VINFO struct, as well as all the stmt_vec_info structs of all the 
   stmts in the loop.  */

void
destroy_loop_vec_info (loop_vec_info loop_vinfo)
{
  struct loop *loop;
  basic_block *bbs;
  int nbbs;
  block_stmt_iterator si;
  int j;

  if (!loop_vinfo)
    return;

  loop = LOOP_VINFO_LOOP (loop_vinfo);

  bbs = LOOP_VINFO_BBS (loop_vinfo);
  nbbs = loop->num_nodes;

  for (j = 0; j < nbbs; j++)
    {
      basic_block bb = bbs[j];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  tree stmt = bsi_stmt (si);
	  stmt_ann_t ann = stmt_ann (stmt);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  free (stmt_info);
	  set_stmt_info (ann, NULL);
	}
    }

  free (LOOP_VINFO_BBS (loop_vinfo));
  varray_clear (LOOP_VINFO_DATAREF_WRITES (loop_vinfo));
  varray_clear (LOOP_VINFO_DATAREF_READS (loop_vinfo));

  free (loop_vinfo);
}


/* Function vect_strip_conversions

   Strip conversions that don't narrow the mode.  */

tree 
vect_strip_conversion (tree expr)
{
  tree to, ti, oprnd0;
  
  while (TREE_CODE (expr) == NOP_EXPR || TREE_CODE (expr) == CONVERT_EXPR)
    {
      to = TREE_TYPE (expr);
      oprnd0 = TREE_OPERAND (expr, 0);
      ti = TREE_TYPE (oprnd0);
 
      if (!INTEGRAL_TYPE_P (to) || !INTEGRAL_TYPE_P (ti))
	return NULL_TREE;
      if (GET_MODE_SIZE (TYPE_MODE (to)) < GET_MODE_SIZE (TYPE_MODE (ti)))
	return NULL_TREE;
      
      expr = oprnd0;
    }
  return expr; 
}


/* Function vect_force_dr_alignment_p.

   Returns whether the alignment of a DECL can be forced to be aligned
   on ALIGNMENT bit boundary.  */

bool 
vect_can_force_dr_alignment_p (tree decl, unsigned int alignment)
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
    /* This is not 100% correct.  The absolute correct stack alignment
       is STACK_BOUNDARY.  We're supposed to hope, but not assume, that
       PREFERRED_STACK_BOUNDARY is honored by all translation units.
       However, until someone implements forced stack alignment, SSE
       isn't really usable without this.  */  
    return (alignment <= PREFERRED_STACK_BOUNDARY); 
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

  if (nbytes == 0)
    return NULL_TREE;

  /* FORNOW: Only a single vector size per target (UNITS_PER_SIMD_WORD)
     is expected.  */
  nunits = UNITS_PER_SIMD_WORD / nbytes;

  vectype = build_vector_type (scalar_type, nunits);
  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    {
      fprintf (vect_dump, "get vectype with %d units of type ", nunits);
      print_generic_expr (vect_dump, scalar_type, TDF_SLIM);
    }

  if (!vectype)
    return NULL_TREE;

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
    {
      fprintf (vect_dump, "vectype: ");
      print_generic_expr (vect_dump, vectype, TDF_SLIM);
    }

  if (!VECTOR_MODE_P (TYPE_MODE (vectype)))
    {
      /* TODO: tree-complex.c sometimes can parallelize operations
         on generic vectors.  We can vectorize the loop in that case,
         but then we should re-run the lowering pass.  */
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
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
  tree vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (DR_STMT (dr)));
  enum machine_mode mode = (int) TYPE_MODE (vectype);

  if (aligned_access_p (dr))
    return dr_aligned;

  /* Possibly unaligned access.  */
  
  if (DR_IS_READ (dr))
    {
      if (vec_realign_load_optab->handlers[mode].insn_code != CODE_FOR_nothing
	  && (!targetm.vectorize.builtin_mask_for_load
	      || targetm.vectorize.builtin_mask_for_load ()))
	return dr_unaligned_software_pipeline;

      if (movmisalign_optab->handlers[mode].insn_code != CODE_FOR_nothing)
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
vect_is_simple_use (tree operand, loop_vec_info loop_vinfo, tree *def)
{ 
  tree def_stmt;
  basic_block bb;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  if (def)
    *def = NULL_TREE;

  if (TREE_CODE (operand) == INTEGER_CST || TREE_CODE (operand) == REAL_CST)
    return true;

  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  def_stmt = SSA_NAME_DEF_STMT (operand);
  if (def_stmt == NULL_TREE )
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
        fprintf (vect_dump, "no def_stmt.");
      return false;
    }

  /* empty stmt is expected only in case of a function argument.
     (Otherwise - we expect a phi_node or a modify_expr).  */
  if (IS_EMPTY_STMT (def_stmt))
    {
      tree arg = TREE_OPERAND (def_stmt, 0);
      if (TREE_CODE (arg) == INTEGER_CST || TREE_CODE (arg) == REAL_CST)
	return true;
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	{
	  fprintf (vect_dump, "Unexpected empty stmt: ");
	  print_generic_expr (vect_dump, def_stmt, TDF_SLIM);
	}
      return false;  
    }

  /* phi_node inside the loop indicates an induction/reduction pattern.
     This is not supported yet.  */
  bb = bb_for_stmt (def_stmt);
  if (TREE_CODE (def_stmt) == PHI_NODE && flow_bb_inside_loop_p (loop, bb))
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	fprintf (vect_dump, "reduction/induction - unsupported.");
      return false; /* FORNOW: not supported yet.  */
    }

  /* Expecting a modify_expr or a phi_node.  */
  if (TREE_CODE (def_stmt) == MODIFY_EXPR
      || TREE_CODE (def_stmt) == PHI_NODE)
    {
      if (def)
        *def = def_stmt; 	
      return true;
    }

  return false;
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
  init_expr = unshare_expr (initial_condition_in_loop_num (access_fn,
                                                           loop_nb));

  if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
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
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
        fprintf (vect_dump, "step unknown.");
      return false;
    }

  return true;
}


/* Function need_imm_uses_for.

   Return whether we ought to include information for 'var'
   when calculating immediate uses.  For this pass we only want use
   information for non-virtual variables.  */

static bool
need_imm_uses_for (tree var)
{
  return is_gimple_reg (var);
}


/* Function vectorize_loops.
   
   Entry Point to loop vectorization phase.  */

void
vectorize_loops (struct loops *loops)
{
  unsigned int i, loops_num;
  unsigned int num_vectorized_loops = 0;

  /* Fix the verbosity level if not defined explicitly by the user.  */
  vect_set_dump_settings ();

  /* Does the target support SIMD?  */
  /* FORNOW: until more sophisticated machine modelling is in place.  */
  if (!UNITS_PER_SIMD_WORD)
    {
      if (vect_print_dump_info (REPORT_DETAILS, UNKNOWN_LOC))
	fprintf (vect_dump, "vectorizer: target vector size is not defined.");
      return;
    }

#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
#endif

  compute_immediate_uses (TDFA_USE_OPS, need_imm_uses_for);

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number 
     than all previously defined loops. This fact allows us to run 
     only over initial loops skipping newly generated ones.  */
  loops_num = loops->num;
  for (i = 1; i < loops_num; i++)
    {
      loop_vec_info loop_vinfo;
      struct loop *loop = loops->parray[i];

      if (!loop)
        continue;

      loop_vinfo = vect_analyze_loop (loop);
      loop->aux = loop_vinfo;

      if (!loop_vinfo || !LOOP_VINFO_VECTORIZABLE_P (loop_vinfo))
	continue;

      vect_transform_loop (loop_vinfo, loops); 
      num_vectorized_loops++;
    }

  if (vect_print_dump_info (REPORT_VECTORIZED_LOOPS, UNKNOWN_LOC))
    fprintf (vect_dump, "vectorized %u loops in function.\n",
	     num_vectorized_loops);

  /*  ----------- Finalize. -----------  */

  free_df ();
  for (i = 1; i < loops_num; i++)
    {
      struct loop *loop = loops->parray[i];
      loop_vec_info loop_vinfo;

      if (!loop)
	continue;
      loop_vinfo = loop->aux;
      destroy_loop_vec_info (loop_vinfo);
      loop->aux = NULL;
    }

  rewrite_into_ssa (false);
  rewrite_into_loop_closed_ssa (); /* FORNOW */
  bitmap_clear (vars_to_rename);
}
