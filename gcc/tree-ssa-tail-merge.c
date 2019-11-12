/* Tail merging for gimple.
   Copyright (C) 2011-2019 Free Software Foundation, Inc.
   Contributed by Tom de Vries (tom@codesourcery.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Pass overview.


   MOTIVATIONAL EXAMPLE

   gimple representation of gcc/testsuite/gcc.dg/pr43864.c at

   hprofStartupp (charD.1 * outputFileNameD.2600, charD.1 * ctxD.2601)
   {
     struct FILED.1638 * fpD.2605;
     charD.1 fileNameD.2604[1000];
     intD.0 D.3915;
     const charD.1 * restrict outputFileName.0D.3914;

     # BLOCK 2 freq:10000
     # PRED: ENTRY [100.0%]  (fallthru,exec)
     # PT = nonlocal { D.3926 } (restr)
     outputFileName.0D.3914_3
       = (const charD.1 * restrict) outputFileNameD.2600_2(D);
     # .MEMD.3923_13 = VDEF <.MEMD.3923_12(D)>
     # USE = nonlocal null { fileNameD.2604 D.3926 } (restr)
     # CLB = nonlocal null { fileNameD.2604 D.3926 } (restr)
     sprintfD.759 (&fileNameD.2604, outputFileName.0D.3914_3);
     # .MEMD.3923_14 = VDEF <.MEMD.3923_13>
     # USE = nonlocal null { fileNameD.2604 D.3926 } (restr)
     # CLB = nonlocal null { fileNameD.2604 D.3926 } (restr)
     D.3915_4 = accessD.2606 (&fileNameD.2604, 1);
     if (D.3915_4 == 0)
       goto <bb 3>;
     else
       goto <bb 4>;
     # SUCC: 3 [10.0%]  (true,exec) 4 [90.0%]  (false,exec)

     # BLOCK 3 freq:1000
     # PRED: 2 [10.0%]  (true,exec)
     # .MEMD.3923_15 = VDEF <.MEMD.3923_14>
     # USE = nonlocal null { fileNameD.2604 D.3926 } (restr)
     # CLB = nonlocal null { fileNameD.2604 D.3926 } (restr)
     freeD.898 (ctxD.2601_5(D));
     goto <bb 7>;
     # SUCC: 7 [100.0%]  (fallthru,exec)

     # BLOCK 4 freq:9000
     # PRED: 2 [90.0%]  (false,exec)
     # .MEMD.3923_16 = VDEF <.MEMD.3923_14>
     # PT = nonlocal escaped
     # USE = nonlocal null { fileNameD.2604 D.3926 } (restr)
     # CLB = nonlocal null { fileNameD.2604 D.3926 } (restr)
     fpD.2605_8 = fopenD.1805 (&fileNameD.2604[0], 0B);
     if (fpD.2605_8 == 0B)
       goto <bb 5>;
     else
       goto <bb 6>;
     # SUCC: 5 [1.9%]  (true,exec) 6 [98.1%]  (false,exec)

     # BLOCK 5 freq:173
     # PRED: 4 [1.9%]  (true,exec)
     # .MEMD.3923_17 = VDEF <.MEMD.3923_16>
     # USE = nonlocal null { fileNameD.2604 D.3926 } (restr)
     # CLB = nonlocal null { fileNameD.2604 D.3926 } (restr)
     freeD.898 (ctxD.2601_5(D));
     goto <bb 7>;
     # SUCC: 7 [100.0%]  (fallthru,exec)

     # BLOCK 6 freq:8827
     # PRED: 4 [98.1%]  (false,exec)
     # .MEMD.3923_18 = VDEF <.MEMD.3923_16>
     # USE = nonlocal null { fileNameD.2604 D.3926 } (restr)
     # CLB = nonlocal null { fileNameD.2604 D.3926 } (restr)
     fooD.2599 (outputFileNameD.2600_2(D), fpD.2605_8);
     # SUCC: 7 [100.0%]  (fallthru,exec)

     # BLOCK 7 freq:10000
     # PRED: 3 [100.0%]  (fallthru,exec) 5 [100.0%]  (fallthru,exec)
	     6 [100.0%]  (fallthru,exec)
     # PT = nonlocal null

     # ctxD.2601_1 = PHI <0B(3), 0B(5), ctxD.2601_5(D)(6)>
     # .MEMD.3923_11 = PHI <.MEMD.3923_15(3), .MEMD.3923_17(5),
			    .MEMD.3923_18(6)>
     # VUSE <.MEMD.3923_11>
     return ctxD.2601_1;
     # SUCC: EXIT [100.0%]
   }

   bb 3 and bb 5 can be merged.  The blocks have different predecessors, but the
   same successors, and the same operations.


   CONTEXT

   A technique called tail merging (or cross jumping) can fix the example
   above.  For a block, we look for common code at the end (the tail) of the
   predecessor blocks, and insert jumps from one block to the other.
   The example is a special case for tail merging, in that 2 whole blocks
   can be merged, rather than just the end parts of it.
   We currently only focus on whole block merging, so in that sense
   calling this pass tail merge is a bit of a misnomer.

   We distinguish 2 kinds of situations in which blocks can be merged:
   - same operations, same predecessors.  The successor edges coming from one
     block are redirected to come from the other block.
   - same operations, same successors.  The predecessor edges entering one block
     are redirected to enter the other block.  Note that this operation might
     involve introducing phi operations.

   For efficient implementation, we would like to value numbers the blocks, and
   have a comparison operator that tells us whether the blocks are equal.
   Besides being runtime efficient, block value numbering should also abstract
   from irrelevant differences in order of operations, much like normal value
   numbering abstracts from irrelevant order of operations.

   For the first situation (same_operations, same predecessors), normal value
   numbering fits well.  We can calculate a block value number based on the
   value numbers of the defs and vdefs.

   For the second situation (same operations, same successors), this approach
   doesn't work so well.  We can illustrate this using the example.  The calls
   to free use different vdefs: MEMD.3923_16 and MEMD.3923_14, and these will
   remain different in value numbering, since they represent different memory
   states.  So the resulting vdefs of the frees will be different in value
   numbering, so the block value numbers will be different.

   The reason why we call the blocks equal is not because they define the same
   values, but because uses in the blocks use (possibly different) defs in the
   same way.  To be able to detect this efficiently, we need to do some kind of
   reverse value numbering, meaning number the uses rather than the defs, and
   calculate a block value number based on the value number of the uses.
   Ideally, a block comparison operator will also indicate which phis are needed
   to merge the blocks.

   For the moment, we don't do block value numbering, but we do insn-by-insn
   matching, using scc value numbers to match operations with results, and
   structural comparison otherwise, while ignoring vop mismatches.


   IMPLEMENTATION

   1. The pass first determines all groups of blocks with the same successor
      blocks.
   2. Within each group, it tries to determine clusters of equal basic blocks.
   3. The clusters are applied.
   4. The same successor groups are updated.
   5. This process is repeated from 2 onwards, until no more changes.


   LIMITATIONS/TODO

   - block only
   - handles only 'same operations, same successors'.
     It handles same predecessors as a special subcase though.
   - does not implement the reverse value numbering and block value numbering.
   - improve memory allocation: use garbage collected memory, obstacks,
     allocpools where appropriate.
   - no insertion of gimple_reg phis,  We only introduce vop-phis.
   - handle blocks with gimple_reg phi_nodes.


   PASS PLACEMENT
   This 'pass' is not a stand-alone gimple pass, but runs as part of
   pass_pre, in order to share the value numbering.


   SWITCHES

   - ftree-tail-merge.  On at -O2.  We may have to enable it only at -Os.  */

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
#include "trans-mem.h"
#include "cfganal.h"
#include "cfgcleanup.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-ssa-sccvn.h"
#include "cfgloop.h"
#include "tree-eh.h"
#include "tree-cfgcleanup.h"

const int ignore_edge_flags = EDGE_DFS_BACK | EDGE_EXECUTABLE;

/* Describes a group of bbs with the same successors.  The successor bbs are
   cached in succs, and the successor edge flags are cached in succ_flags.
   If a bb has the EDGE_TRUE/FALSE_VALUE flags swapped compared to succ_flags,
   it's marked in inverse.
   Additionally, the hash value for the struct is cached in hashval, and
   in_worklist indicates whether it's currently part of worklist.  */

struct same_succ : pointer_hash <same_succ>
{
  /* The bbs that have the same successor bbs.  */
  bitmap bbs;
  /* The successor bbs.  */
  bitmap succs;
  /* Indicates whether the EDGE_TRUE/FALSE_VALUEs of succ_flags are swapped for
     bb.  */
  bitmap inverse;
  /* The edge flags for each of the successor bbs.  */
  vec<int> succ_flags;
  /* Indicates whether the struct is currently in the worklist.  */
  bool in_worklist;
  /* The hash value of the struct.  */
  hashval_t hashval;

  /* hash_table support.  */
  static inline hashval_t hash (const same_succ *);
  static int equal (const same_succ *, const same_succ *);
  static void remove (same_succ *);
};

/* hash routine for hash_table support, returns hashval of E.  */

inline hashval_t
same_succ::hash (const same_succ *e)
{
  return e->hashval;
}

/* A group of bbs where 1 bb from bbs can replace the other bbs.  */

struct bb_cluster
{
  /* The bbs in the cluster.  */
  bitmap bbs;
  /* The preds of the bbs in the cluster.  */
  bitmap preds;
  /* Index in all_clusters vector.  */
  int index;
  /* The bb to replace the cluster with.  */
  basic_block rep_bb;
};

/* Per bb-info.  */

struct aux_bb_info
{
  /* The number of non-debug statements in the bb.  */
  int size;
  /* The same_succ that this bb is a member of.  */
  same_succ *bb_same_succ;
  /* The cluster that this bb is a member of.  */
  bb_cluster *cluster;
  /* The vop state at the exit of a bb.  This is shortlived data, used to
     communicate data between update_block_by and update_vuses.  */
  tree vop_at_exit;
  /* The bb that either contains or is dominated by the dependencies of the
     bb.  */
  basic_block dep_bb;
};

/* Macros to access the fields of struct aux_bb_info.  */

#define BB_SIZE(bb) (((struct aux_bb_info *)bb->aux)->size)
#define BB_SAME_SUCC(bb) (((struct aux_bb_info *)bb->aux)->bb_same_succ)
#define BB_CLUSTER(bb) (((struct aux_bb_info *)bb->aux)->cluster)
#define BB_VOP_AT_EXIT(bb) (((struct aux_bb_info *)bb->aux)->vop_at_exit)
#define BB_DEP_BB(bb) (((struct aux_bb_info *)bb->aux)->dep_bb)

/* Valueization helper querying the VN lattice.  */

static tree
tail_merge_valueize (tree name)
{
  if (TREE_CODE (name) == SSA_NAME
      && has_VN_INFO (name))
    {
      tree tem = VN_INFO (name)->valnum;
      if (tem != VN_TOP)
	return tem;
    }
  return name;
}

/* Returns true if the only effect a statement STMT has, is to define locally
   used SSA_NAMEs.  */

static bool
stmt_local_def (gimple *stmt)
{
  basic_block bb, def_bb;
  imm_use_iterator iter;
  use_operand_p use_p;
  tree val;
  def_operand_p def_p;

  if (gimple_vdef (stmt) != NULL_TREE
      || gimple_has_side_effects (stmt)
      || gimple_could_trap_p_1 (stmt, false, false)
      || gimple_vuse (stmt) != NULL_TREE
      /* Copied from tree-ssa-ifcombine.c:bb_no_side_effects_p():
	 const calls don't match any of the above, yet they could
	 still have some side-effects - they could contain
	 gimple_could_trap_p statements, like floating point
	 exceptions or integer division by zero.  See PR70586.
	 FIXME: perhaps gimple_has_side_effects or gimple_could_trap_p
	 should handle this.  */
      || is_gimple_call (stmt))
    return false;

  def_p = SINGLE_SSA_DEF_OPERAND (stmt, SSA_OP_DEF);
  if (def_p == NULL)
    return false;

  val = DEF_FROM_PTR (def_p);
  if (val == NULL_TREE || TREE_CODE (val) != SSA_NAME)
    return false;

  def_bb = gimple_bb (stmt);

  FOR_EACH_IMM_USE_FAST (use_p, iter, val)
    {
      if (is_gimple_debug (USE_STMT (use_p)))
	continue;
      bb = gimple_bb (USE_STMT (use_p));
      if (bb == def_bb)
	continue;

      if (gimple_code (USE_STMT (use_p)) == GIMPLE_PHI
	  && EDGE_PRED (bb, PHI_ARG_INDEX_FROM_USE (use_p))->src == def_bb)
	continue;

      return false;
    }

  return true;
}

/* Let GSI skip forwards over local defs.  */

static void
gsi_advance_fw_nondebug_nonlocal (gimple_stmt_iterator *gsi)
{
  gimple *stmt;

  while (true)
    {
      if (gsi_end_p (*gsi))
	return;
      stmt = gsi_stmt (*gsi);
      if (!stmt_local_def (stmt))
	return;
      gsi_next_nondebug (gsi);
    }
}

/* VAL1 and VAL2 are either:
   - uses in BB1 and BB2, or
   - phi alternatives for BB1 and BB2.
   Return true if the uses have the same gvn value.  */

static bool
gvn_uses_equal (tree val1, tree val2)
{
  gcc_checking_assert (val1 != NULL_TREE && val2 != NULL_TREE);

  if (val1 == val2)
    return true;

  if (tail_merge_valueize (val1) != tail_merge_valueize (val2))
    return false;

  return ((TREE_CODE (val1) == SSA_NAME || CONSTANT_CLASS_P (val1))
	  && (TREE_CODE (val2) == SSA_NAME || CONSTANT_CLASS_P (val2)));
}

/* Prints E to FILE.  */

static void
same_succ_print (FILE *file, const same_succ *e)
{
  unsigned int i;
  bitmap_print (file, e->bbs, "bbs:", "\n");
  bitmap_print (file, e->succs, "succs:", "\n");
  bitmap_print (file, e->inverse, "inverse:", "\n");
  fprintf (file, "flags:");
  for (i = 0; i < e->succ_flags.length (); ++i)
    fprintf (file, " %x", e->succ_flags[i]);
  fprintf (file, "\n");
}

/* Prints same_succ VE to VFILE.  */

inline int
ssa_same_succ_print_traverse (same_succ **pe, FILE *file)
{
  const same_succ *e = *pe;
  same_succ_print (file, e);
  return 1;
}

/* Update BB_DEP_BB (USE_BB), given a use of VAL in USE_BB.  */

static void
update_dep_bb (basic_block use_bb, tree val)
{
  basic_block dep_bb;

  /* Not a dep.  */
  if (TREE_CODE (val) != SSA_NAME)
    return;

  /* Skip use of global def.  */
  if (SSA_NAME_IS_DEFAULT_DEF (val))
    return;

  /* Skip use of local def.  */
  dep_bb = gimple_bb (SSA_NAME_DEF_STMT (val));
  if (dep_bb == use_bb)
    return;

  if (BB_DEP_BB (use_bb) == NULL
      || dominated_by_p (CDI_DOMINATORS, dep_bb, BB_DEP_BB (use_bb)))
    BB_DEP_BB (use_bb) = dep_bb;
}

/* Update BB_DEP_BB, given the dependencies in STMT.  */

static void
stmt_update_dep_bb (gimple *stmt)
{
  ssa_op_iter iter;
  use_operand_p use;

  FOR_EACH_SSA_USE_OPERAND (use, stmt, iter, SSA_OP_USE)
    update_dep_bb (gimple_bb (stmt), USE_FROM_PTR (use));
}

/* Calculates hash value for same_succ VE.  */

static hashval_t
same_succ_hash (const same_succ *e)
{
  inchash::hash hstate (bitmap_hash (e->succs));
  int flags;
  unsigned int i;
  unsigned int first = bitmap_first_set_bit (e->bbs);
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, first);
  int size = 0;
  gimple *stmt;
  tree arg;
  unsigned int s;
  bitmap_iterator bs;

  for (gimple_stmt_iterator gsi = gsi_start_nondebug_bb (bb);
       !gsi_end_p (gsi); gsi_next_nondebug (&gsi))
    {
      stmt = gsi_stmt (gsi);
      stmt_update_dep_bb (stmt);
      if (stmt_local_def (stmt))
	continue;
      size++;

      hstate.add_int (gimple_code (stmt));
      if (is_gimple_assign (stmt))
	hstate.add_int (gimple_assign_rhs_code (stmt));
      if (!is_gimple_call (stmt))
	continue;
      if (gimple_call_internal_p (stmt))
	hstate.add_int (gimple_call_internal_fn (stmt));
      else
	{
	  inchash::add_expr (gimple_call_fn (stmt), hstate);
	  if (gimple_call_chain (stmt))
	    inchash::add_expr (gimple_call_chain (stmt), hstate);
	}
      for (i = 0; i < gimple_call_num_args (stmt); i++)
	{
	  arg = gimple_call_arg (stmt, i);
	  arg = tail_merge_valueize (arg);
	  inchash::add_expr (arg, hstate);
	}
    }

  hstate.add_int (size);
  BB_SIZE (bb) = size;

  hstate.add_int (bb->loop_father->num);

  for (i = 0; i < e->succ_flags.length (); ++i)
    {
      flags = e->succ_flags[i];
      flags = flags & ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      hstate.add_int (flags);
    }

  EXECUTE_IF_SET_IN_BITMAP (e->succs, 0, s, bs)
    {
      int n = find_edge (bb, BASIC_BLOCK_FOR_FN (cfun, s))->dest_idx;
      for (gphi_iterator gsi = gsi_start_phis (BASIC_BLOCK_FOR_FN (cfun, s));
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  tree lhs = gimple_phi_result (phi);
	  tree val = gimple_phi_arg_def (phi, n);

	  if (virtual_operand_p (lhs))
	    continue;
	  update_dep_bb (bb, val);
	}
    }

  return hstate.end ();
}

/* Returns true if E1 and E2 have 2 successors, and if the successor flags
   are inverse for the EDGE_TRUE_VALUE and EDGE_FALSE_VALUE flags, and equal for
   the other edge flags.  */

static bool
inverse_flags (const same_succ *e1, const same_succ *e2)
{
  int f1a, f1b, f2a, f2b;
  int mask = ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

  if (e1->succ_flags.length () != 2)
    return false;

  f1a = e1->succ_flags[0];
  f1b = e1->succ_flags[1];
  f2a = e2->succ_flags[0];
  f2b = e2->succ_flags[1];

  if (f1a == f2a && f1b == f2b)
    return false;

  return (f1a & mask) == (f2a & mask) && (f1b & mask) == (f2b & mask);
}

/* Compares SAME_SUCCs E1 and E2.  */

int
same_succ::equal (const same_succ *e1, const same_succ *e2)
{
  unsigned int i, first1, first2;
  gimple_stmt_iterator gsi1, gsi2;
  gimple *s1, *s2;
  basic_block bb1, bb2;

  if (e1 == e2)
    return 1;

  if (e1->hashval != e2->hashval)
    return 0;

  if (e1->succ_flags.length () != e2->succ_flags.length ())
    return 0;

  if (!bitmap_equal_p (e1->succs, e2->succs))
    return 0;

  if (!inverse_flags (e1, e2))
    {
      for (i = 0; i < e1->succ_flags.length (); ++i)
	if (e1->succ_flags[i] != e2->succ_flags[i])
	  return 0;
    }

  first1 = bitmap_first_set_bit (e1->bbs);
  first2 = bitmap_first_set_bit (e2->bbs);

  bb1 = BASIC_BLOCK_FOR_FN (cfun, first1);
  bb2 = BASIC_BLOCK_FOR_FN (cfun, first2);

  if (BB_SIZE (bb1) != BB_SIZE (bb2))
    return 0;

  if (bb1->loop_father != bb2->loop_father)
    return 0;

  gsi1 = gsi_start_nondebug_bb (bb1);
  gsi2 = gsi_start_nondebug_bb (bb2);
  gsi_advance_fw_nondebug_nonlocal (&gsi1);
  gsi_advance_fw_nondebug_nonlocal (&gsi2);
  while (!(gsi_end_p (gsi1) || gsi_end_p (gsi2)))
    {
      s1 = gsi_stmt (gsi1);
      s2 = gsi_stmt (gsi2);
      if (gimple_code (s1) != gimple_code (s2))
	return 0;
      if (is_gimple_call (s1) && !gimple_call_same_target_p (s1, s2))
	return 0;
      gsi_next_nondebug (&gsi1);
      gsi_next_nondebug (&gsi2);
      gsi_advance_fw_nondebug_nonlocal (&gsi1);
      gsi_advance_fw_nondebug_nonlocal (&gsi2);
    }

  return 1;
}

/* Alloc and init a new SAME_SUCC.  */

static same_succ *
same_succ_alloc (void)
{
  same_succ *same = XNEW (struct same_succ);

  same->bbs = BITMAP_ALLOC (NULL);
  same->succs = BITMAP_ALLOC (NULL);
  same->inverse = BITMAP_ALLOC (NULL);
  same->succ_flags.create (10);
  same->in_worklist = false;

  return same;
}

/* Delete same_succ E.  */

void
same_succ::remove (same_succ *e)
{
  BITMAP_FREE (e->bbs);
  BITMAP_FREE (e->succs);
  BITMAP_FREE (e->inverse);
  e->succ_flags.release ();

  XDELETE (e);
}

/* Reset same_succ SAME.  */

static void
same_succ_reset (same_succ *same)
{
  bitmap_clear (same->bbs);
  bitmap_clear (same->succs);
  bitmap_clear (same->inverse);
  same->succ_flags.truncate (0);
}

static hash_table<same_succ> *same_succ_htab;

/* Array that is used to store the edge flags for a successor.  */

static int *same_succ_edge_flags;

/* Bitmap that is used to mark bbs that are recently deleted.  */

static bitmap deleted_bbs;

/* Bitmap that is used to mark predecessors of bbs that are
   deleted.  */

static bitmap deleted_bb_preds;

/* Prints same_succ_htab to stderr.  */

extern void debug_same_succ (void);
DEBUG_FUNCTION void
debug_same_succ ( void)
{
  same_succ_htab->traverse <FILE *, ssa_same_succ_print_traverse> (stderr);
}


/* Vector of bbs to process.  */

static vec<same_succ *> worklist;

/* Prints worklist to FILE.  */

static void
print_worklist (FILE *file)
{
  unsigned int i;
  for (i = 0; i < worklist.length (); ++i)
    same_succ_print (file, worklist[i]);
}

/* Adds SAME to worklist.  */

static void
add_to_worklist (same_succ *same)
{
  if (same->in_worklist)
    return;

  if (bitmap_count_bits (same->bbs) < 2)
    return;

  same->in_worklist = true;
  worklist.safe_push (same);
}

/* Add BB to same_succ_htab.  */

static void
find_same_succ_bb (basic_block bb, same_succ **same_p)
{
  unsigned int j;
  bitmap_iterator bj;
  same_succ *same = *same_p;
  same_succ **slot;
  edge_iterator ei;
  edge e;

  if (bb == NULL)
    return;
  bitmap_set_bit (same->bbs, bb->index);
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      int index = e->dest->index;
      bitmap_set_bit (same->succs, index);
      same_succ_edge_flags[index] = (e->flags & ~ignore_edge_flags);
    }
  EXECUTE_IF_SET_IN_BITMAP (same->succs, 0, j, bj)
    same->succ_flags.safe_push (same_succ_edge_flags[j]);

  same->hashval = same_succ_hash (same);

  slot = same_succ_htab->find_slot_with_hash (same, same->hashval, INSERT);
  if (*slot == NULL)
    {
      *slot = same;
      BB_SAME_SUCC (bb) = same;
      add_to_worklist (same);
      *same_p = NULL;
    }
  else
    {
      bitmap_set_bit ((*slot)->bbs, bb->index);
      BB_SAME_SUCC (bb) = *slot;
      add_to_worklist (*slot);
      if (inverse_flags (same, *slot))
	bitmap_set_bit ((*slot)->inverse, bb->index);
      same_succ_reset (same);
    }
}

/* Find bbs with same successors.  */

static void
find_same_succ (void)
{
  same_succ *same = same_succ_alloc ();
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      find_same_succ_bb (bb, &same);
      if (same == NULL)
	same = same_succ_alloc ();
    }

  same_succ::remove (same);
}

/* Initializes worklist administration.  */

static void
init_worklist (void)
{
  alloc_aux_for_blocks (sizeof (struct aux_bb_info));
  same_succ_htab = new hash_table<same_succ> (n_basic_blocks_for_fn (cfun));
  same_succ_edge_flags = XCNEWVEC (int, last_basic_block_for_fn (cfun));
  deleted_bbs = BITMAP_ALLOC (NULL);
  deleted_bb_preds = BITMAP_ALLOC (NULL);
  worklist.create (n_basic_blocks_for_fn (cfun));
  find_same_succ ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "initial worklist:\n");
      print_worklist (dump_file);
    }
}

/* Deletes worklist administration.  */

static void
delete_worklist (void)
{
  free_aux_for_blocks ();
  delete same_succ_htab;
  same_succ_htab = NULL;
  XDELETEVEC (same_succ_edge_flags);
  same_succ_edge_flags = NULL;
  BITMAP_FREE (deleted_bbs);
  BITMAP_FREE (deleted_bb_preds);
  worklist.release ();
}

/* Mark BB as deleted, and mark its predecessors.  */

static void
mark_basic_block_deleted (basic_block bb)
{
  edge e;
  edge_iterator ei;

  bitmap_set_bit (deleted_bbs, bb->index);

  FOR_EACH_EDGE (e, ei, bb->preds)
    bitmap_set_bit (deleted_bb_preds, e->src->index);
}

/* Removes BB from its corresponding same_succ.  */

static void
same_succ_flush_bb (basic_block bb)
{
  same_succ *same = BB_SAME_SUCC (bb);
  if (! same)
    return;

  BB_SAME_SUCC (bb) = NULL;
  if (bitmap_single_bit_set_p (same->bbs))
    same_succ_htab->remove_elt_with_hash (same, same->hashval);
  else
    bitmap_clear_bit (same->bbs, bb->index);
}

/* Removes all bbs in BBS from their corresponding same_succ.  */

static void
same_succ_flush_bbs (bitmap bbs)
{
  unsigned int i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (bbs, 0, i, bi)
    same_succ_flush_bb (BASIC_BLOCK_FOR_FN (cfun, i));
}

/* Release the last vdef in BB, either normal or phi result.  */

static void
release_last_vdef (basic_block bb)
{
  for (gimple_stmt_iterator i = gsi_last_bb (bb); !gsi_end_p (i);
       gsi_prev_nondebug (&i))
    {
      gimple *stmt = gsi_stmt (i);
      if (gimple_vdef (stmt) == NULL_TREE)
	continue;

      mark_virtual_operand_for_renaming (gimple_vdef (stmt));
      return;
    }

  for (gphi_iterator i = gsi_start_phis (bb); !gsi_end_p (i);
       gsi_next (&i))
    {
      gphi *phi = i.phi ();
      tree res = gimple_phi_result (phi);

      if (!virtual_operand_p (res))
	continue;

      mark_virtual_phi_result_for_renaming (phi);
      return;
    }
}

/* For deleted_bb_preds, find bbs with same successors.  */

static void
update_worklist (void)
{
  unsigned int i;
  bitmap_iterator bi;
  basic_block bb;
  same_succ *same;

  bitmap_and_compl_into (deleted_bb_preds, deleted_bbs);
  bitmap_clear (deleted_bbs);

  bitmap_clear_bit (deleted_bb_preds, ENTRY_BLOCK);
  same_succ_flush_bbs (deleted_bb_preds);

  same = same_succ_alloc ();
  EXECUTE_IF_SET_IN_BITMAP (deleted_bb_preds, 0, i, bi)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, i);
      gcc_assert (bb != NULL);
      find_same_succ_bb (bb, &same);
      if (same == NULL)
	same = same_succ_alloc ();
    }
  same_succ::remove (same);
  bitmap_clear (deleted_bb_preds);
}

/* Prints cluster C to FILE.  */

static void
print_cluster (FILE *file, bb_cluster *c)
{
  if (c == NULL)
    return;
  bitmap_print (file, c->bbs, "bbs:", "\n");
  bitmap_print (file, c->preds, "preds:", "\n");
}

/* Prints cluster C to stderr.  */

extern void debug_cluster (bb_cluster *);
DEBUG_FUNCTION void
debug_cluster (bb_cluster *c)
{
  print_cluster (stderr, c);
}

/* Update C->rep_bb, given that BB is added to the cluster.  */

static void
update_rep_bb (bb_cluster *c, basic_block bb)
{
  /* Initial.  */
  if (c->rep_bb == NULL)
    {
      c->rep_bb = bb;
      return;
    }

  /* Current needs no deps, keep it.  */
  if (BB_DEP_BB (c->rep_bb) == NULL)
    return;

  /* Bb needs no deps, change rep_bb.  */
  if (BB_DEP_BB (bb) == NULL)
    {
      c->rep_bb = bb;
      return;
    }

  /* Bb needs last deps earlier than current, change rep_bb.  A potential
     problem with this, is that the first deps might also be earlier, which
     would mean we prefer longer lifetimes for the deps.  To be able to check
     for this, we would have to trace BB_FIRST_DEP_BB as well, besides
     BB_DEP_BB, which is really BB_LAST_DEP_BB.
     The benefit of choosing the bb with last deps earlier, is that it can
     potentially be used as replacement for more bbs.  */
  if (dominated_by_p (CDI_DOMINATORS, BB_DEP_BB (c->rep_bb), BB_DEP_BB (bb)))
    c->rep_bb = bb;
}

/* Add BB to cluster C.  Sets BB in C->bbs, and preds of BB in C->preds.  */

static void
add_bb_to_cluster (bb_cluster *c, basic_block bb)
{
  edge e;
  edge_iterator ei;

  bitmap_set_bit (c->bbs, bb->index);

  FOR_EACH_EDGE (e, ei, bb->preds)
    bitmap_set_bit (c->preds, e->src->index);

  update_rep_bb (c, bb);
}

/* Allocate and init new cluster.  */

static bb_cluster *
new_cluster (void)
{
  bb_cluster *c;
  c = XCNEW (bb_cluster);
  c->bbs = BITMAP_ALLOC (NULL);
  c->preds = BITMAP_ALLOC (NULL);
  c->rep_bb = NULL;
  return c;
}

/* Delete clusters.  */

static void
delete_cluster (bb_cluster *c)
{
  if (c == NULL)
    return;
  BITMAP_FREE (c->bbs);
  BITMAP_FREE (c->preds);
  XDELETE (c);
}


/* Array that contains all clusters.  */

static vec<bb_cluster *> all_clusters;

/* Allocate all cluster vectors.  */

static void
alloc_cluster_vectors (void)
{
  all_clusters.create (n_basic_blocks_for_fn (cfun));
}

/* Reset all cluster vectors.  */

static void
reset_cluster_vectors (void)
{
  unsigned int i;
  basic_block bb;
  for (i = 0; i < all_clusters.length (); ++i)
    delete_cluster (all_clusters[i]);
  all_clusters.truncate (0);
  FOR_EACH_BB_FN (bb, cfun)
    BB_CLUSTER (bb) = NULL;
}

/* Delete all cluster vectors.  */

static void
delete_cluster_vectors (void)
{
  unsigned int i;
  for (i = 0; i < all_clusters.length (); ++i)
    delete_cluster (all_clusters[i]);
  all_clusters.release ();
}

/* Merge cluster C2 into C1.  */

static void
merge_clusters (bb_cluster *c1, bb_cluster *c2)
{
  bitmap_ior_into (c1->bbs, c2->bbs);
  bitmap_ior_into (c1->preds, c2->preds);
}

/* Register equivalence of BB1 and BB2 (members of cluster C).  Store c in
   all_clusters, or merge c with existing cluster.  */

static void
set_cluster (basic_block bb1, basic_block bb2)
{
  basic_block merge_bb, other_bb;
  bb_cluster *merge, *old, *c;

  if (BB_CLUSTER (bb1) == NULL && BB_CLUSTER (bb2) == NULL)
    {
      c = new_cluster ();
      add_bb_to_cluster (c, bb1);
      add_bb_to_cluster (c, bb2);
      BB_CLUSTER (bb1) = c;
      BB_CLUSTER (bb2) = c;
      c->index = all_clusters.length ();
      all_clusters.safe_push (c);
    }
  else if (BB_CLUSTER (bb1) == NULL || BB_CLUSTER (bb2) == NULL)
    {
      merge_bb = BB_CLUSTER (bb1) == NULL ? bb2 : bb1;
      other_bb = BB_CLUSTER (bb1) == NULL ? bb1 : bb2;
      merge = BB_CLUSTER (merge_bb);
      add_bb_to_cluster (merge, other_bb);
      BB_CLUSTER (other_bb) = merge;
    }
  else if (BB_CLUSTER (bb1) != BB_CLUSTER (bb2))
    {
      unsigned int i;
      bitmap_iterator bi;

      old = BB_CLUSTER (bb2);
      merge = BB_CLUSTER (bb1);
      merge_clusters (merge, old);
      EXECUTE_IF_SET_IN_BITMAP (old->bbs, 0, i, bi)
	BB_CLUSTER (BASIC_BLOCK_FOR_FN (cfun, i)) = merge;
      all_clusters[old->index] = NULL;
      update_rep_bb (merge, old->rep_bb);
      delete_cluster (old);
    }
  else
    gcc_unreachable ();
}

/* Return true if gimple operands T1 and T2 have the same value.  */

static bool
gimple_operand_equal_value_p (tree t1, tree t2)
{
  if (t1 == t2)
    return true;

  if (t1 == NULL_TREE
      || t2 == NULL_TREE)
    return false;

  if (operand_equal_p (t1, t2, OEP_MATCH_SIDE_EFFECTS))
    return true;

  return gvn_uses_equal (t1, t2);
}

/* Return true if gimple statements S1 and S2 are equal.  Gimple_bb (s1) and
   gimple_bb (s2) are members of SAME_SUCC.  */

static bool
gimple_equal_p (same_succ *same_succ, gimple *s1, gimple *s2)
{
  unsigned int i;
  tree lhs1, lhs2;
  basic_block bb1 = gimple_bb (s1), bb2 = gimple_bb (s2);
  tree t1, t2;
  bool inv_cond;
  enum tree_code code1, code2;

  if (gimple_code (s1) != gimple_code (s2))
    return false;

  switch (gimple_code (s1))
    {
    case GIMPLE_CALL:
      if (!gimple_call_same_target_p (s1, s2))
	return false;

      t1 = gimple_call_chain (s1);
      t2 = gimple_call_chain (s2);
      if (!gimple_operand_equal_value_p (t1, t2))
	return false;

      if (gimple_call_num_args (s1) != gimple_call_num_args (s2))
	return false;

      for (i = 0; i < gimple_call_num_args (s1); ++i)
	{
	  t1 = gimple_call_arg (s1, i);
	  t2 = gimple_call_arg (s2, i);
	  if (!gimple_operand_equal_value_p (t1, t2))
	    return false;
	}

      lhs1 = gimple_get_lhs (s1);
      lhs2 = gimple_get_lhs (s2);
      if (lhs1 == NULL_TREE && lhs2 == NULL_TREE)
	return true;
      if (lhs1 == NULL_TREE || lhs2 == NULL_TREE)
	return false;
      if (TREE_CODE (lhs1) == SSA_NAME && TREE_CODE (lhs2) == SSA_NAME)
	return tail_merge_valueize (lhs1) == tail_merge_valueize (lhs2);
      return operand_equal_p (lhs1, lhs2, 0);

    case GIMPLE_ASSIGN:
      lhs1 = gimple_get_lhs (s1);
      lhs2 = gimple_get_lhs (s2);
      if (TREE_CODE (lhs1) != SSA_NAME
	  && TREE_CODE (lhs2) != SSA_NAME)
	return (operand_equal_p (lhs1, lhs2, 0)
		&& gimple_operand_equal_value_p (gimple_assign_rhs1 (s1),
						 gimple_assign_rhs1 (s2)));
      else if (TREE_CODE (lhs1) == SSA_NAME
	       && TREE_CODE (lhs2) == SSA_NAME)
	return operand_equal_p (gimple_assign_rhs1 (s1),
				gimple_assign_rhs1 (s2), 0);
      return false;

    case GIMPLE_COND:
      t1 = gimple_cond_lhs (s1);
      t2 = gimple_cond_lhs (s2);
      if (!gimple_operand_equal_value_p (t1, t2))
	return false;

      t1 = gimple_cond_rhs (s1);
      t2 = gimple_cond_rhs (s2);
      if (!gimple_operand_equal_value_p (t1, t2))
	return false;

      code1 = gimple_expr_code (s1);
      code2 = gimple_expr_code (s2);
      inv_cond = (bitmap_bit_p (same_succ->inverse, bb1->index)
		  != bitmap_bit_p (same_succ->inverse, bb2->index));
      if (inv_cond)
	{
	  bool honor_nans = HONOR_NANS (t1);
	  code2 = invert_tree_comparison (code2, honor_nans);
	}
      return code1 == code2;

    default:
      return false;
    }
}

/* Let GSI skip backwards over local defs.  Return the earliest vuse in VUSE.
   Return true in VUSE_ESCAPED if the vuse influenced a SSA_OP_DEF of one of the
   processed statements.  */

static void
gsi_advance_bw_nondebug_nonlocal (gimple_stmt_iterator *gsi, tree *vuse,
				  bool *vuse_escaped)
{
  gimple *stmt;
  tree lvuse;

  while (true)
    {
      if (gsi_end_p (*gsi))
	return;
      stmt = gsi_stmt (*gsi);

      lvuse = gimple_vuse (stmt);
      if (lvuse != NULL_TREE)
	{
	  *vuse = lvuse;
	  if (!ZERO_SSA_OPERANDS (stmt, SSA_OP_DEF))
	    *vuse_escaped = true;
	}

      if (!stmt_local_def (stmt))
	return;
      gsi_prev_nondebug (gsi);
    }
}

/* Return true if equal (in the sense of gimple_equal_p) statements STMT1 and
   STMT2 are allowed to be merged.  */

static bool
merge_stmts_p (gimple *stmt1, gimple *stmt2)
{
  /* What could be better than this here is to blacklist the bb
     containing the stmt, when encountering the stmt f.i. in
     same_succ_hash.  */
  if (is_tm_ending (stmt1))
    return false;

  /* Verify EH landing pads.  */
  if (lookup_stmt_eh_lp_fn (cfun, stmt1) != lookup_stmt_eh_lp_fn (cfun, stmt2))
    return false;

  if (is_gimple_call (stmt1)
      && gimple_call_internal_p (stmt1))
    switch (gimple_call_internal_fn (stmt1))
      {
      case IFN_UBSAN_NULL:
      case IFN_UBSAN_BOUNDS:
      case IFN_UBSAN_VPTR:
      case IFN_UBSAN_CHECK_ADD:
      case IFN_UBSAN_CHECK_SUB:
      case IFN_UBSAN_CHECK_MUL:
      case IFN_UBSAN_OBJECT_SIZE:
      case IFN_UBSAN_PTR:
      case IFN_ASAN_CHECK:
	/* For these internal functions, gimple_location is an implicit
	   parameter, which will be used explicitly after expansion.
	   Merging these statements may cause confusing line numbers in
	   sanitizer messages.  */
	return gimple_location (stmt1) == gimple_location (stmt2);
      default:
	break;
      }

  return true;
}

/* Determines whether BB1 and BB2 (members of same_succ) are duplicates.  If so,
   clusters them.  */

static void
find_duplicate (same_succ *same_succ, basic_block bb1, basic_block bb2)
{
  gimple_stmt_iterator gsi1 = gsi_last_nondebug_bb (bb1);
  gimple_stmt_iterator gsi2 = gsi_last_nondebug_bb (bb2);
  tree vuse1 = NULL_TREE, vuse2 = NULL_TREE;
  bool vuse_escaped = false;

  gsi_advance_bw_nondebug_nonlocal (&gsi1, &vuse1, &vuse_escaped);
  gsi_advance_bw_nondebug_nonlocal (&gsi2, &vuse2, &vuse_escaped);

  while (!gsi_end_p (gsi1) && !gsi_end_p (gsi2))
    {
      gimple *stmt1 = gsi_stmt (gsi1);
      gimple *stmt2 = gsi_stmt (gsi2);

      if (gimple_code (stmt1) == GIMPLE_LABEL
	  && gimple_code (stmt2) == GIMPLE_LABEL)
	break;

      if (!gimple_equal_p (same_succ, stmt1, stmt2))
	return;

      if (!merge_stmts_p (stmt1, stmt2))
	return;

      gsi_prev_nondebug (&gsi1);
      gsi_prev_nondebug (&gsi2);
      gsi_advance_bw_nondebug_nonlocal (&gsi1, &vuse1, &vuse_escaped);
      gsi_advance_bw_nondebug_nonlocal (&gsi2, &vuse2, &vuse_escaped);
    }

  while (!gsi_end_p (gsi1) && gimple_code (gsi_stmt (gsi1)) == GIMPLE_LABEL)
    {
      tree label = gimple_label_label (as_a <glabel *> (gsi_stmt (gsi1)));
      if (DECL_NONLOCAL (label) || FORCED_LABEL (label))
	return;
      gsi_prev (&gsi1);
    }
  while (!gsi_end_p (gsi2) && gimple_code (gsi_stmt (gsi2)) == GIMPLE_LABEL)
    {
      tree label = gimple_label_label (as_a <glabel *> (gsi_stmt (gsi2)));
      if (DECL_NONLOCAL (label) || FORCED_LABEL (label))
	return;
      gsi_prev (&gsi2);
    }
  if (!(gsi_end_p (gsi1) && gsi_end_p (gsi2)))
    return;

  /* If the incoming vuses are not the same, and the vuse escaped into an
     SSA_OP_DEF, then merging the 2 blocks will change the value of the def,
     which potentially means the semantics of one of the blocks will be changed.
     TODO: make this check more precise.  */
  if (vuse_escaped && vuse1 != vuse2)
    return;

  if (dump_file)
    fprintf (dump_file, "find_duplicates: <bb %d> duplicate of <bb %d>\n",
	     bb1->index, bb2->index);

  set_cluster (bb1, bb2);
}

/* Returns whether for all phis in DEST the phi alternatives for E1 and
   E2 are equal.  */

static bool
same_phi_alternatives_1 (basic_block dest, edge e1, edge e2)
{
  int n1 = e1->dest_idx, n2 = e2->dest_idx;
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);
      tree val1 = gimple_phi_arg_def (phi, n1);
      tree val2 = gimple_phi_arg_def (phi, n2);

      if (virtual_operand_p (lhs))
	continue;

      if (operand_equal_for_phi_arg_p (val1, val2))
	continue;
      if (gvn_uses_equal (val1, val2))
	continue;

      return false;
    }

  return true;
}

/* Returns whether for all successors of BB1 and BB2 (members of SAME_SUCC), the
   phi alternatives for BB1 and BB2 are equal.  */

static bool
same_phi_alternatives (same_succ *same_succ, basic_block bb1, basic_block bb2)
{
  unsigned int s;
  bitmap_iterator bs;
  edge e1, e2;
  basic_block succ;

  EXECUTE_IF_SET_IN_BITMAP (same_succ->succs, 0, s, bs)
    {
      succ = BASIC_BLOCK_FOR_FN (cfun, s);
      e1 = find_edge (bb1, succ);
      e2 = find_edge (bb2, succ);
      if (e1->flags & EDGE_COMPLEX
	  || e2->flags & EDGE_COMPLEX)
	return false;

      /* For all phis in bb, the phi alternatives for e1 and e2 need to have
	 the same value.  */
      if (!same_phi_alternatives_1 (succ, e1, e2))
	return false;
    }

  return true;
}

/* Return true if BB has non-vop phis.  */

static bool
bb_has_non_vop_phi (basic_block bb)
{
  gimple_seq phis = phi_nodes (bb);
  gimple *phi;

  if (phis == NULL)
    return false;

  if (!gimple_seq_singleton_p (phis))
    return true;

  phi = gimple_seq_first_stmt (phis);
  return !virtual_operand_p (gimple_phi_result (phi));
}

/* Returns true if redirecting the incoming edges of FROM to TO maintains the
   invariant that uses in FROM are dominates by their defs.  */

static bool
deps_ok_for_redirect_from_bb_to_bb (basic_block from, basic_block to)
{
  basic_block cd, dep_bb = BB_DEP_BB (to);
  edge_iterator ei;
  edge e;

  if (dep_bb == NULL)
    return true;

  bitmap from_preds = BITMAP_ALLOC (NULL);
  FOR_EACH_EDGE (e, ei, from->preds)
    bitmap_set_bit (from_preds, e->src->index);
  cd = nearest_common_dominator_for_set (CDI_DOMINATORS, from_preds);
  BITMAP_FREE (from_preds);

  return dominated_by_p (CDI_DOMINATORS, dep_bb, cd);
}

/* Returns true if replacing BB1 (or its replacement bb) by BB2 (or its
   replacement bb) and vice versa maintains the invariant that uses in the
   replacement are dominates by their defs.  */

static bool
deps_ok_for_redirect (basic_block bb1, basic_block bb2)
{
  if (BB_CLUSTER (bb1) != NULL)
    bb1 = BB_CLUSTER (bb1)->rep_bb;

  if (BB_CLUSTER (bb2) != NULL)
    bb2 = BB_CLUSTER (bb2)->rep_bb;

  return (deps_ok_for_redirect_from_bb_to_bb (bb1, bb2)
	  && deps_ok_for_redirect_from_bb_to_bb (bb2, bb1));
}

/* Within SAME_SUCC->bbs, find clusters of bbs which can be merged.  */

static void
find_clusters_1 (same_succ *same_succ)
{
  basic_block bb1, bb2;
  unsigned int i, j;
  bitmap_iterator bi, bj;
  int nr_comparisons;
  int max_comparisons = param_max_tail_merge_comparisons;

  EXECUTE_IF_SET_IN_BITMAP (same_succ->bbs, 0, i, bi)
    {
      bb1 = BASIC_BLOCK_FOR_FN (cfun, i);

      /* TODO: handle blocks with phi-nodes.  We'll have to find corresponding
	 phi-nodes in bb1 and bb2, with the same alternatives for the same
	 preds.  */
      if (bb_has_non_vop_phi (bb1) || bb_has_eh_pred (bb1)
	  || bb_has_abnormal_pred (bb1))
	continue;

      nr_comparisons = 0;
      EXECUTE_IF_SET_IN_BITMAP (same_succ->bbs, i + 1, j, bj)
	{
	  bb2 = BASIC_BLOCK_FOR_FN (cfun, j);

	  if (bb_has_non_vop_phi (bb2) || bb_has_eh_pred (bb2)
	      || bb_has_abnormal_pred (bb2))
	    continue;

	  if (BB_CLUSTER (bb1) != NULL && BB_CLUSTER (bb1) == BB_CLUSTER (bb2))
	    continue;

	  /* Limit quadratic behavior.  */
	  nr_comparisons++;
	  if (nr_comparisons > max_comparisons)
	    break;

	  /* This is a conservative dependency check.  We could test more
	     precise for allowed replacement direction.  */
	  if (!deps_ok_for_redirect (bb1, bb2))
	    continue;

	  if (!(same_phi_alternatives (same_succ, bb1, bb2)))
	    continue;

	  find_duplicate (same_succ, bb1, bb2);
	}
    }
}

/* Find clusters of bbs which can be merged.  */

static void
find_clusters (void)
{
  same_succ *same;

  while (!worklist.is_empty ())
    {
      same = worklist.pop ();
      same->in_worklist = false;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "processing worklist entry\n");
	  same_succ_print (dump_file, same);
	}
      find_clusters_1 (same);
    }
}

/* Returns the vop phi of BB, if any.  */

static gphi *
vop_phi (basic_block bb)
{
  gphi *stmt;
  gphi_iterator gsi;
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi.phi ();
      if (! virtual_operand_p (gimple_phi_result (stmt)))
	continue;
      return stmt;
    }
  return NULL;
}

/* Redirect all edges from BB1 to BB2, removes BB1 and marks it as removed.  */

static void
replace_block_by (basic_block bb1, basic_block bb2)
{
  edge pred_edge;
  unsigned int i;
  gphi *bb2_phi;

  bb2_phi = vop_phi (bb2);

  /* Mark the basic block as deleted.  */
  mark_basic_block_deleted (bb1);

  /* Redirect the incoming edges of bb1 to bb2.  */
  for (i = EDGE_COUNT (bb1->preds); i > 0 ; --i)
    {
      pred_edge = EDGE_PRED (bb1, i - 1);
      pred_edge = redirect_edge_and_branch (pred_edge, bb2);
      gcc_assert (pred_edge != NULL);

      if (bb2_phi == NULL)
	continue;

      /* The phi might have run out of capacity when the redirect added an
	 argument, which means it could have been replaced.  Refresh it.  */
      bb2_phi = vop_phi (bb2);

      add_phi_arg (bb2_phi, SSA_NAME_VAR (gimple_phi_result (bb2_phi)),
		   pred_edge, UNKNOWN_LOCATION);
    }


  /* Merge the outgoing edge counts from bb1 onto bb2.  */
  edge e1, e2;
  edge_iterator ei;

  if (bb2->count.initialized_p ())
    FOR_EACH_EDGE (e1, ei, bb1->succs)
      {
        e2 = find_edge (bb2, e1->dest);
        gcc_assert (e2);

	/* If probabilities are same, we are done.
	   If counts are nonzero we can distribute accordingly. In remaining
	   cases just avreage the values and hope for the best.  */
	e2->probability = e1->probability.combine_with_count
	                     (bb1->count, e2->probability, bb2->count);
      }
  bb2->count += bb1->count;

  /* Move over any user labels from bb1 after the bb2 labels.  */
  gimple_stmt_iterator gsi1 = gsi_start_bb (bb1);
  if (!gsi_end_p (gsi1) && gimple_code (gsi_stmt (gsi1)) == GIMPLE_LABEL)
    {
      gimple_stmt_iterator gsi2 = gsi_after_labels (bb2);
      while (!gsi_end_p (gsi1)
	     && gimple_code (gsi_stmt (gsi1)) == GIMPLE_LABEL)
	{
	  tree label = gimple_label_label (as_a <glabel *> (gsi_stmt (gsi1)));
	  gcc_assert (!DECL_NONLOCAL (label) && !FORCED_LABEL (label));
	  if (DECL_ARTIFICIAL (label))
	    gsi_next (&gsi1);
	  else
	    gsi_move_before (&gsi1, &gsi2);
	}
    }

  /* Clear range info from all stmts in BB2 -- this transformation
     could make them out of date.  */
  reset_flow_sensitive_info_in_bb (bb2);

  /* Do updates that use bb1, before deleting bb1.  */
  release_last_vdef (bb1);
  same_succ_flush_bb (bb1);

  delete_basic_block (bb1);
}

/* Bbs for which update_debug_stmt need to be called.  */

static bitmap update_bbs;

/* For each cluster in all_clusters, merge all cluster->bbs.  Returns
   number of bbs removed.  */

static int
apply_clusters (void)
{
  basic_block bb1, bb2;
  bb_cluster *c;
  unsigned int i, j;
  bitmap_iterator bj;
  int nr_bbs_removed = 0;

  for (i = 0; i < all_clusters.length (); ++i)
    {
      c = all_clusters[i];
      if (c == NULL)
	continue;

      bb2 = c->rep_bb;
      bitmap_set_bit (update_bbs, bb2->index);

      bitmap_clear_bit (c->bbs, bb2->index);
      EXECUTE_IF_SET_IN_BITMAP (c->bbs, 0, j, bj)
	{
	  bb1 = BASIC_BLOCK_FOR_FN (cfun, j);
	  bitmap_clear_bit (update_bbs, bb1->index);

	  replace_block_by (bb1, bb2);
	  nr_bbs_removed++;
	}
    }

  return nr_bbs_removed;
}

/* Resets debug statement STMT if it has uses that are not dominated by their
   defs.  */

static void
update_debug_stmt (gimple *stmt)
{
  use_operand_p use_p;
  ssa_op_iter oi;
  basic_block bbuse;

  if (!gimple_debug_bind_p (stmt))
    return;

  bbuse = gimple_bb (stmt);
  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, oi, SSA_OP_USE)
    {
      tree name = USE_FROM_PTR (use_p);
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      basic_block bbdef = gimple_bb (def_stmt);
      if (bbdef == NULL || bbuse == bbdef
	  || dominated_by_p (CDI_DOMINATORS, bbuse, bbdef))
	continue;

      gimple_debug_bind_reset_value (stmt);
      update_stmt (stmt);
      break;
    }
}

/* Resets all debug statements that have uses that are not
   dominated by their defs.  */

static void
update_debug_stmts (void)
{
  basic_block bb;
  bitmap_iterator bi;
  unsigned int i;

  EXECUTE_IF_SET_IN_BITMAP (update_bbs, 0, i, bi)
    {
      gimple *stmt;
      gimple_stmt_iterator gsi;

      bb = BASIC_BLOCK_FOR_FN (cfun, i);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (!is_gimple_debug (stmt))
	    continue;
	  update_debug_stmt (stmt);
	}
    }
}

/* Runs tail merge optimization.  */

unsigned int
tail_merge_optimize (unsigned int todo)
{
  int nr_bbs_removed_total = 0;
  int nr_bbs_removed;
  bool loop_entered = false;
  int iteration_nr = 0;
  int max_iterations = param_max_tail_merge_iterations;

  if (!flag_tree_tail_merge
      || max_iterations == 0)
    return 0;

  timevar_push (TV_TREE_TAIL_MERGE);

  /* We enter from PRE which has critical edges split.  Elimination
     does not process trivially dead code so cleanup the CFG if we
     are told so.  And re-split critical edges then.  */
  if (todo & TODO_cleanup_cfg)
    {
      cleanup_tree_cfg ();
      todo &= ~TODO_cleanup_cfg;
      split_edges_for_insertion ();
    }

  if (!dom_info_available_p (CDI_DOMINATORS))
    {
      /* PRE can leave us with unreachable blocks, remove them now.  */
      delete_unreachable_blocks ();
      calculate_dominance_info (CDI_DOMINATORS);
    }
  init_worklist ();

  while (!worklist.is_empty ())
    {
      if (!loop_entered)
	{
	  loop_entered = true;
	  alloc_cluster_vectors ();
	  update_bbs = BITMAP_ALLOC (NULL);
	}
      else
	reset_cluster_vectors ();

      iteration_nr++;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "worklist iteration #%d\n", iteration_nr);

      find_clusters ();
      gcc_assert (worklist.is_empty ());
      if (all_clusters.is_empty ())
	break;

      nr_bbs_removed = apply_clusters ();
      nr_bbs_removed_total += nr_bbs_removed;
      if (nr_bbs_removed == 0)
	break;

      free_dominance_info (CDI_DOMINATORS);

      if (iteration_nr == max_iterations)
	break;

      calculate_dominance_info (CDI_DOMINATORS);
      update_worklist ();
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "htab collision / search: %f\n",
	     same_succ_htab->collisions ());

  if (nr_bbs_removed_total > 0)
    {
      if (MAY_HAVE_DEBUG_BIND_STMTS)
	{
	  calculate_dominance_info (CDI_DOMINATORS);
	  update_debug_stmts ();
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Before TODOs.\n");
	  dump_function_to_file (current_function_decl, dump_file, dump_flags);
	}

      mark_virtual_operands_for_renaming (cfun);
    }

  delete_worklist ();
  if (loop_entered)
    {
      delete_cluster_vectors ();
      BITMAP_FREE (update_bbs);
    }

  timevar_pop (TV_TREE_TAIL_MERGE);

  return todo;
}
