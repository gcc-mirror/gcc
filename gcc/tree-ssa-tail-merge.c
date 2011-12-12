/* Tail merging for gimple.
   Copyright (C) 2011 Free Software Foundation, Inc.
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


   SWITCHES

   - ftree-tail-merge.  On at -O2.  We may have to enable it only at -Os.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "flags.h"
#include "function.h"
#include "tree-flow.h"
#include "timevar.h"
#include "bitmap.h"
#include "tree-ssa-alias.h"
#include "params.h"
#include "tree-pretty-print.h"
#include "hashtab.h"
#include "gimple-pretty-print.h"
#include "tree-ssa-sccvn.h"
#include "tree-dump.h"

/* Describes a group of bbs with the same successors.  The successor bbs are
   cached in succs, and the successor edge flags are cached in succ_flags.
   If a bb has the EDGE_TRUE/VALSE_VALUE flags swapped compared to succ_flags,
   it's marked in inverse.
   Additionally, the hash value for the struct is cached in hashval, and
   in_worklist indicates whether it's currently part of worklist.  */

struct same_succ_def
{
  /* The bbs that have the same successor bbs.  */
  bitmap bbs;
  /* The successor bbs.  */
  bitmap succs;
  /* Indicates whether the EDGE_TRUE/FALSE_VALUEs of succ_flags are swapped for
     bb.  */
  bitmap inverse;
  /* The edge flags for each of the successor bbs.  */
  VEC (int, heap) *succ_flags;
  /* Indicates whether the struct is currently in the worklist.  */
  bool in_worklist;
  /* The hash value of the struct.  */
  hashval_t hashval;
};
typedef struct same_succ_def *same_succ;
typedef const struct same_succ_def *const_same_succ;

/* A group of bbs where 1 bb from bbs can replace the other bbs.  */

struct bb_cluster_def
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
typedef struct bb_cluster_def *bb_cluster;
typedef const struct bb_cluster_def *const_bb_cluster;

/* Per bb-info.  */

struct aux_bb_info
{
  /* The number of non-debug statements in the bb.  */
  int size;
  /* The same_succ that this bb is a member of.  */
  same_succ bb_same_succ;
  /* The cluster that this bb is a member of.  */
  bb_cluster cluster;
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

  if (vn_valueize (val1) != vn_valueize (val2))
    return false;

  return ((TREE_CODE (val1) == SSA_NAME || CONSTANT_CLASS_P (val1))
	  && (TREE_CODE (val2) == SSA_NAME || CONSTANT_CLASS_P (val2)));
}

/* Prints E to FILE.  */

static void
same_succ_print (FILE *file, const same_succ e)
{
  unsigned int i;
  bitmap_print (file, e->bbs, "bbs:", "\n");
  bitmap_print (file, e->succs, "succs:", "\n");
  bitmap_print (file, e->inverse, "inverse:", "\n");
  fprintf (file, "flags:");
  for (i = 0; i < VEC_length (int, e->succ_flags); ++i)
    fprintf (file, " %x", VEC_index (int, e->succ_flags, i));
  fprintf (file, "\n");
}

/* Prints same_succ VE to VFILE.  */

static int
same_succ_print_traverse (void **ve, void *vfile)
{
  const same_succ e = *((const same_succ *)ve);
  FILE *file = ((FILE*)vfile);
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
stmt_update_dep_bb (gimple stmt)
{
  ssa_op_iter iter;
  use_operand_p use;

  FOR_EACH_SSA_USE_OPERAND (use, stmt, iter, SSA_OP_USE)
    update_dep_bb (gimple_bb (stmt), USE_FROM_PTR (use));
}

/* Returns whether VAL is used in the same bb as in which it is defined, or
   in the phi of a successor bb.  */

static bool
local_def (tree val)
{
  gimple stmt, def_stmt;
  basic_block bb, def_bb;
  imm_use_iterator iter;
  bool res;

  if (TREE_CODE (val) != SSA_NAME)
    return false;
  def_stmt = SSA_NAME_DEF_STMT (val);
  def_bb = gimple_bb (def_stmt);

  res = true;
  FOR_EACH_IMM_USE_STMT (stmt, iter, val)
    {
      bb = gimple_bb (stmt);
      if (bb == def_bb)
	continue;
      if (gimple_code (stmt) == GIMPLE_PHI
	  && find_edge (def_bb, bb))
	continue;
      res = false;
      BREAK_FROM_IMM_USE_STMT (iter);
    }
  return res;
}

/* Calculates hash value for same_succ VE.  */

static hashval_t
same_succ_hash (const void *ve)
{
  const_same_succ e = (const_same_succ)ve;
  hashval_t hashval = bitmap_hash (e->succs);
  int flags;
  unsigned int i;
  unsigned int first = bitmap_first_set_bit (e->bbs);
  basic_block bb = BASIC_BLOCK (first);
  int size = 0;
  gimple_stmt_iterator gsi;
  gimple stmt;
  tree arg;
  unsigned int s;
  bitmap_iterator bs;

  for (gsi = gsi_start_nondebug_bb (bb);
       !gsi_end_p (gsi); gsi_next_nondebug (&gsi))
    {
      stmt = gsi_stmt (gsi);
      stmt_update_dep_bb (stmt);
      if (is_gimple_assign (stmt) && local_def (gimple_get_lhs (stmt))
	  && !gimple_has_side_effects (stmt))
	continue;
      size++;

      hashval = iterative_hash_hashval_t (gimple_code (stmt), hashval);
      if (is_gimple_assign (stmt))
	hashval = iterative_hash_hashval_t (gimple_assign_rhs_code (stmt),
					    hashval);
      if (!is_gimple_call (stmt))
	continue;
      if (gimple_call_internal_p (stmt))
	hashval = iterative_hash_hashval_t
	  ((hashval_t) gimple_call_internal_fn (stmt), hashval);
      else
	hashval = iterative_hash_expr (gimple_call_fn (stmt), hashval);
      for (i = 0; i < gimple_call_num_args (stmt); i++)
	{
	  arg = gimple_call_arg (stmt, i);
	  arg = vn_valueize (arg);
	  hashval = iterative_hash_expr (arg, hashval);
	}
    }

  hashval = iterative_hash_hashval_t (size, hashval);
  BB_SIZE (bb) = size;

  for (i = 0; i < VEC_length (int, e->succ_flags); ++i)
    {
      flags = VEC_index (int, e->succ_flags, i);
      flags = flags & ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      hashval = iterative_hash_hashval_t (flags, hashval);
    }

  EXECUTE_IF_SET_IN_BITMAP (e->succs, 0, s, bs)
    {
      int n = find_edge (bb, BASIC_BLOCK (s))->dest_idx;
      for (gsi = gsi_start_phis (BASIC_BLOCK (s)); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  tree lhs = gimple_phi_result (phi);
	  tree val = gimple_phi_arg_def (phi, n);

	  if (!is_gimple_reg (lhs))
	    continue;
	  update_dep_bb (bb, val);
	}
    }

  return hashval;
}

/* Returns true if E1 and E2 have 2 successors, and if the successor flags
   are inverse for the EDGE_TRUE_VALUE and EDGE_FALSE_VALUE flags, and equal for
   the other edge flags.  */

static bool
inverse_flags (const_same_succ e1, const_same_succ e2)
{
  int f1a, f1b, f2a, f2b;
  int mask = ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

  if (VEC_length (int, e1->succ_flags) != 2)
    return false;

  f1a = VEC_index (int, e1->succ_flags, 0);
  f1b = VEC_index (int, e1->succ_flags, 1);
  f2a = VEC_index (int, e2->succ_flags, 0);
  f2b = VEC_index (int, e2->succ_flags, 1);

  if (f1a == f2a && f1b == f2b)
    return false;

  return (f1a & mask) == (f2a & mask) && (f1b & mask) == (f2b & mask);
}

/* Compares SAME_SUCCs VE1 and VE2.  */

static int
same_succ_equal (const void *ve1, const void *ve2)
{
  const_same_succ e1 = (const_same_succ)ve1;
  const_same_succ e2 = (const_same_succ)ve2;
  unsigned int i, first1, first2;
  gimple_stmt_iterator gsi1, gsi2;
  gimple s1, s2;
  basic_block bb1, bb2;

  if (e1->hashval != e2->hashval)
    return 0;

  if (VEC_length (int, e1->succ_flags) != VEC_length (int, e2->succ_flags))
    return 0;

  if (!bitmap_equal_p (e1->succs, e2->succs))
    return 0;

  if (!inverse_flags (e1, e2))
    {
      for (i = 0; i < VEC_length (int, e1->succ_flags); ++i)
	if (VEC_index (int, e1->succ_flags, i)
	    != VEC_index (int, e1->succ_flags, i))
	  return 0;
    }

  first1 = bitmap_first_set_bit (e1->bbs);
  first2 = bitmap_first_set_bit (e2->bbs);

  bb1 = BASIC_BLOCK (first1);
  bb2 = BASIC_BLOCK (first2);

  if (BB_SIZE (bb1) != BB_SIZE (bb2))
    return 0;

  gsi1 = gsi_start_nondebug_bb (bb1);
  gsi2 = gsi_start_nondebug_bb (bb2);
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
    }

  return 1;
}

/* Alloc and init a new SAME_SUCC.  */

static same_succ
same_succ_alloc (void)
{
  same_succ same = XNEW (struct same_succ_def);

  same->bbs = BITMAP_ALLOC (NULL);
  same->succs = BITMAP_ALLOC (NULL);
  same->inverse = BITMAP_ALLOC (NULL);
  same->succ_flags = VEC_alloc (int, heap, 10);
  same->in_worklist = false;

  return same;
}

/* Delete same_succ VE.  */

static void
same_succ_delete (void *ve)
{
  same_succ e = (same_succ)ve;

  BITMAP_FREE (e->bbs);
  BITMAP_FREE (e->succs);
  BITMAP_FREE (e->inverse);
  VEC_free (int, heap, e->succ_flags);

  XDELETE (ve);
}

/* Reset same_succ SAME.  */

static void
same_succ_reset (same_succ same)
{
  bitmap_clear (same->bbs);
  bitmap_clear (same->succs);
  bitmap_clear (same->inverse);
  VEC_truncate (int, same->succ_flags, 0);
}

/* Hash table with all same_succ entries.  */

static htab_t same_succ_htab;

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
  htab_traverse (same_succ_htab, same_succ_print_traverse, stderr);
}

DEF_VEC_P (same_succ);
DEF_VEC_ALLOC_P (same_succ, heap);

/* Vector of bbs to process.  */

static VEC (same_succ, heap) *worklist;

/* Prints worklist to FILE.  */

static void
print_worklist (FILE *file)
{
  unsigned int i;
  for (i = 0; i < VEC_length (same_succ, worklist); ++i)
    same_succ_print (file, VEC_index (same_succ, worklist, i));
}

/* Adds SAME to worklist.  */

static void
add_to_worklist (same_succ same)
{
  if (same->in_worklist)
    return;

  if (bitmap_count_bits (same->bbs) < 2)
    return;

  same->in_worklist = true;
  VEC_safe_push (same_succ, heap, worklist, same);
}

/* Add BB to same_succ_htab.  */

static void
find_same_succ_bb (basic_block bb, same_succ *same_p)
{
  unsigned int j;
  bitmap_iterator bj;
  same_succ same = *same_p;
  same_succ *slot;
  edge_iterator ei;
  edge e;

  if (bb == NULL)
    return;
  bitmap_set_bit (same->bbs, bb->index);
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      int index = e->dest->index;
      bitmap_set_bit (same->succs, index);
      same_succ_edge_flags[index] = e->flags;
    }
  EXECUTE_IF_SET_IN_BITMAP (same->succs, 0, j, bj)
    VEC_safe_push (int, heap, same->succ_flags, same_succ_edge_flags[j]);

  same->hashval = same_succ_hash (same);

  slot = (same_succ *) htab_find_slot_with_hash (same_succ_htab, same,
						   same->hashval, INSERT);
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
  same_succ same = same_succ_alloc ();
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      find_same_succ_bb (bb, &same);
      if (same == NULL)
	same = same_succ_alloc ();
    }

  same_succ_delete (same);
}

/* Initializes worklist administration.  */

static void
init_worklist (void)
{
  alloc_aux_for_blocks (sizeof (struct aux_bb_info));
  same_succ_htab
    = htab_create (n_basic_blocks, same_succ_hash, same_succ_equal,
		   same_succ_delete);
  same_succ_edge_flags = XCNEWVEC (int, last_basic_block);
  deleted_bbs = BITMAP_ALLOC (NULL);
  deleted_bb_preds = BITMAP_ALLOC (NULL);
  worklist = VEC_alloc (same_succ, heap, n_basic_blocks);
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
  htab_delete (same_succ_htab);
  same_succ_htab = NULL;
  XDELETEVEC (same_succ_edge_flags);
  same_succ_edge_flags = NULL;
  BITMAP_FREE (deleted_bbs);
  BITMAP_FREE (deleted_bb_preds);
  VEC_free (same_succ, heap, worklist);
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
  same_succ same = BB_SAME_SUCC (bb);
  BB_SAME_SUCC (bb) = NULL;
  if (bitmap_single_bit_set_p (same->bbs))
    htab_remove_elt_with_hash (same_succ_htab, same, same->hashval);
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
    same_succ_flush_bb (BASIC_BLOCK (i));
}

/* Release the last vdef in BB, either normal or phi result.  */

static void
release_last_vdef (basic_block bb)
{
  gimple_stmt_iterator i;

  for (i = gsi_last_bb (bb); !gsi_end_p (i); gsi_prev_nondebug (&i))
    {
      gimple stmt = gsi_stmt (i);
      if (gimple_vdef (stmt) == NULL_TREE)
	continue;

      mark_virtual_operand_for_renaming (gimple_vdef (stmt));
      return;
    }

  for (i = gsi_start_phis (bb); !gsi_end_p (i); gsi_next (&i))
    {
      gimple phi = gsi_stmt (i);
      tree res = gimple_phi_result (phi);

      if (is_gimple_reg (res))
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
  same_succ same;

  bitmap_and_compl_into (deleted_bb_preds, deleted_bbs);
  bitmap_clear (deleted_bbs);

  bitmap_clear_bit (deleted_bb_preds, ENTRY_BLOCK);
  same_succ_flush_bbs (deleted_bb_preds);

  same = same_succ_alloc ();
  EXECUTE_IF_SET_IN_BITMAP (deleted_bb_preds, 0, i, bi)
    {
      bb = BASIC_BLOCK (i);
      gcc_assert (bb != NULL);
      find_same_succ_bb (bb, &same);
      if (same == NULL)
	same = same_succ_alloc ();
    }
  same_succ_delete (same);
  bitmap_clear (deleted_bb_preds);
}

/* Prints cluster C to FILE.  */

static void
print_cluster (FILE *file, bb_cluster c)
{
  if (c == NULL)
    return;
  bitmap_print (file, c->bbs, "bbs:", "\n");
  bitmap_print (file, c->preds, "preds:", "\n");
}

/* Prints cluster C to stderr.  */

extern void debug_cluster (bb_cluster);
DEBUG_FUNCTION void
debug_cluster (bb_cluster c)
{
  print_cluster (stderr, c);
}

/* Update C->rep_bb, given that BB is added to the cluster.  */

static void
update_rep_bb (bb_cluster c, basic_block bb)
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
add_bb_to_cluster (bb_cluster c, basic_block bb)
{
  edge e;
  edge_iterator ei;

  bitmap_set_bit (c->bbs, bb->index);

  FOR_EACH_EDGE (e, ei, bb->preds)
    bitmap_set_bit (c->preds, e->src->index);

  update_rep_bb (c, bb);
}

/* Allocate and init new cluster.  */

static bb_cluster
new_cluster (void)
{
  bb_cluster c;
  c = XCNEW (struct bb_cluster_def);
  c->bbs = BITMAP_ALLOC (NULL);
  c->preds = BITMAP_ALLOC (NULL);
  c->rep_bb = NULL;
  return c;
}

/* Delete clusters.  */

static void
delete_cluster (bb_cluster c)
{
  if (c == NULL)
    return;
  BITMAP_FREE (c->bbs);
  BITMAP_FREE (c->preds);
  XDELETE (c);
}

DEF_VEC_P (bb_cluster);
DEF_VEC_ALLOC_P (bb_cluster, heap);

/* Array that contains all clusters.  */

static VEC (bb_cluster, heap) *all_clusters;

/* Allocate all cluster vectors.  */

static void
alloc_cluster_vectors (void)
{
  all_clusters = VEC_alloc (bb_cluster, heap, n_basic_blocks);
}

/* Reset all cluster vectors.  */

static void
reset_cluster_vectors (void)
{
  unsigned int i;
  basic_block bb;
  for (i = 0; i < VEC_length (bb_cluster, all_clusters); ++i)
    delete_cluster (VEC_index (bb_cluster, all_clusters, i));
  VEC_truncate (bb_cluster, all_clusters, 0);
  FOR_EACH_BB (bb)
    BB_CLUSTER (bb) = NULL;
}

/* Delete all cluster vectors.  */

static void
delete_cluster_vectors (void)
{
  unsigned int i;
  for (i = 0; i < VEC_length (bb_cluster, all_clusters); ++i)
    delete_cluster (VEC_index (bb_cluster, all_clusters, i));
  VEC_free (bb_cluster, heap, all_clusters);
}

/* Merge cluster C2 into C1.  */

static void
merge_clusters (bb_cluster c1, bb_cluster c2)
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
  bb_cluster merge, old, c;

  if (BB_CLUSTER (bb1) == NULL && BB_CLUSTER (bb2) == NULL)
    {
      c = new_cluster ();
      add_bb_to_cluster (c, bb1);
      add_bb_to_cluster (c, bb2);
      BB_CLUSTER (bb1) = c;
      BB_CLUSTER (bb2) = c;
      c->index = VEC_length (bb_cluster, all_clusters);
      VEC_safe_push (bb_cluster, heap, all_clusters, c);
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
	BB_CLUSTER (BASIC_BLOCK (i)) = merge;
      VEC_replace (bb_cluster, all_clusters, old->index, NULL);
      update_rep_bb (merge, old->rep_bb);
      delete_cluster (old);
    }
  else
    gcc_unreachable ();
}

/* Return true if gimple statements S1 and S2 are equal.  Gimple_bb (s1) and
   gimple_bb (s2) are members of SAME_SUCC.  */

static bool
gimple_equal_p (same_succ same_succ, gimple s1, gimple s2)
{
  unsigned int i;
  tree lhs1, lhs2;
  basic_block bb1 = gimple_bb (s1), bb2 = gimple_bb (s2);
  tree t1, t2;
  bool equal, inv_cond;
  enum tree_code code1, code2;

  if (gimple_code (s1) != gimple_code (s2))
    return false;

  switch (gimple_code (s1))
    {
    case GIMPLE_CALL:
      if (gimple_call_num_args (s1) != gimple_call_num_args (s2))
	return false;
      if (!gimple_call_same_target_p (s1, s2))
        return false;

      /* Eventually, we'll significantly complicate the CFG by adding
	 back edges to properly model the effects of transaction restart.
	 For the bulk of optimization this does not matter, but what we
	 cannot recover from is tail merging blocks between two separate
	 transactions.  Avoid that by making commit not match.  */
      if (gimple_call_builtin_p (s1, BUILT_IN_TM_COMMIT))
	return false;

      equal = true;
      for (i = 0; i < gimple_call_num_args (s1); ++i)
	{
	  t1 = gimple_call_arg (s1, i);
	  t2 = gimple_call_arg (s2, i);
	  if (operand_equal_p (t1, t2, 0))
	    continue;
	  if (gvn_uses_equal (t1, t2))
	    continue;
	  equal = false;
	  break;
	}
      if (equal)
	return true;

      lhs1 = gimple_get_lhs (s1);
      lhs2 = gimple_get_lhs (s2);
      return (lhs1 != NULL_TREE && lhs2 != NULL_TREE
	      && TREE_CODE (lhs1) == SSA_NAME && TREE_CODE (lhs2) == SSA_NAME
	      && vn_valueize (lhs1) == vn_valueize (lhs2));

    case GIMPLE_ASSIGN:
      lhs1 = gimple_get_lhs (s1);
      lhs2 = gimple_get_lhs (s2);
      return (TREE_CODE (lhs1) == SSA_NAME
	      && TREE_CODE (lhs2) == SSA_NAME
	      && vn_valueize (lhs1) == vn_valueize (lhs2));

    case GIMPLE_COND:
      t1 = gimple_cond_lhs (s1);
      t2 = gimple_cond_lhs (s2);
      if (!operand_equal_p (t1, t2, 0)
	  && !gvn_uses_equal (t1, t2))
	return false;

      t1 = gimple_cond_rhs (s1);
      t2 = gimple_cond_rhs (s2);
      if (!operand_equal_p (t1, t2, 0)
	  && !gvn_uses_equal (t1, t2))
	return false;

      code1 = gimple_expr_code (s1);
      code2 = gimple_expr_code (s2);
      inv_cond = (bitmap_bit_p (same_succ->inverse, bb1->index)
		  != bitmap_bit_p (same_succ->inverse, bb2->index));
      if (inv_cond)
	{
	  bool honor_nans
	    = HONOR_NANS (TYPE_MODE (TREE_TYPE (gimple_cond_lhs (s1))));
	  code2 = invert_tree_comparison (code2, honor_nans);
	}
      return code1 == code2;

    default:
      return false;
    }
}

/* Let GSI skip backwards over local defs.  */

static void
gsi_advance_bw_nondebug_nonlocal (gimple_stmt_iterator *gsi)
{
  gimple stmt;

  while (true)
    {
      if (gsi_end_p (*gsi))
	return;
      stmt = gsi_stmt (*gsi);
      if (!(is_gimple_assign (stmt) && local_def (gimple_get_lhs (stmt))
	    && !gimple_has_side_effects (stmt)))
	return;
      gsi_prev_nondebug (gsi);
    }
}

/* Determines whether BB1 and BB2 (members of same_succ) are duplicates.  If so,
   clusters them.  */

static void
find_duplicate (same_succ same_succ, basic_block bb1, basic_block bb2)
{
  gimple_stmt_iterator gsi1 = gsi_last_nondebug_bb (bb1);
  gimple_stmt_iterator gsi2 = gsi_last_nondebug_bb (bb2);

  gsi_advance_bw_nondebug_nonlocal (&gsi1);
  gsi_advance_bw_nondebug_nonlocal (&gsi2);

  while (!gsi_end_p (gsi1) && !gsi_end_p (gsi2))
    {
      if (!gimple_equal_p (same_succ, gsi_stmt (gsi1), gsi_stmt (gsi2)))
	return;

      gsi_prev_nondebug (&gsi1);
      gsi_prev_nondebug (&gsi2);
      gsi_advance_bw_nondebug_nonlocal (&gsi1);
      gsi_advance_bw_nondebug_nonlocal (&gsi2);
    }

  if (!(gsi_end_p (gsi1) && gsi_end_p (gsi2)))
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
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree lhs = gimple_phi_result (phi);
      tree val1 = gimple_phi_arg_def (phi, n1);
      tree val2 = gimple_phi_arg_def (phi, n2);

      if (!is_gimple_reg (lhs))
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
same_phi_alternatives (same_succ same_succ, basic_block bb1, basic_block bb2)
{
  unsigned int s;
  bitmap_iterator bs;
  edge e1, e2;
  basic_block succ;

  EXECUTE_IF_SET_IN_BITMAP (same_succ->succs, 0, s, bs)
    {
      succ = BASIC_BLOCK (s);
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
  gimple phi;

  if (phis == NULL)
    return false;

  if (!gimple_seq_singleton_p (phis))
    return true;

  phi = gimple_seq_first_stmt (phis);
  return is_gimple_reg (gimple_phi_result (phi));
}

/* Returns true if redirecting the incoming edges of FROM to TO maintains the
   invariant that uses in FROM are dominates by their defs.  */

static bool
deps_ok_for_redirect_from_bb_to_bb (basic_block from, basic_block to)
{
  basic_block cd, dep_bb = BB_DEP_BB (to);
  edge_iterator ei;
  edge e;
  bitmap from_preds = BITMAP_ALLOC (NULL);

  if (dep_bb == NULL)
    return true;

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
find_clusters_1 (same_succ same_succ)
{
  basic_block bb1, bb2;
  unsigned int i, j;
  bitmap_iterator bi, bj;
  int nr_comparisons;
  int max_comparisons = PARAM_VALUE (PARAM_MAX_TAIL_MERGE_COMPARISONS);

  EXECUTE_IF_SET_IN_BITMAP (same_succ->bbs, 0, i, bi)
    {
      bb1 = BASIC_BLOCK (i);

      /* TODO: handle blocks with phi-nodes.  We'll have to find corresponding
	 phi-nodes in bb1 and bb2, with the same alternatives for the same
	 preds.  */
      if (bb_has_non_vop_phi (bb1))
	continue;

      nr_comparisons = 0;
      EXECUTE_IF_SET_IN_BITMAP (same_succ->bbs, i + 1, j, bj)
	{
	  bb2 = BASIC_BLOCK (j);

	  if (bb_has_non_vop_phi (bb2))
	    continue;

	  if (BB_CLUSTER (bb1) != NULL && BB_CLUSTER (bb1) == BB_CLUSTER (bb2))
	    continue;

	  /* Limit quadratic behaviour.  */
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
  same_succ same;

  while (!VEC_empty (same_succ, worklist))
    {
      same = VEC_pop (same_succ, worklist);
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

static gimple
vop_phi (basic_block bb)
{
  gimple stmt;
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (is_gimple_reg (gimple_phi_result (stmt)))
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
  gimple bb2_phi;

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

  bb2->frequency += bb1->frequency;
  if (bb2->frequency > BB_FREQ_MAX)
    bb2->frequency = BB_FREQ_MAX;
  bb1->frequency = 0;

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
  bb_cluster c;
  unsigned int i, j;
  bitmap_iterator bj;
  int nr_bbs_removed = 0;

  for (i = 0; i < VEC_length (bb_cluster, all_clusters); ++i)
    {
      c = VEC_index (bb_cluster, all_clusters, i);
      if (c == NULL)
	continue;

      bb2 = c->rep_bb;
      bitmap_set_bit (update_bbs, bb2->index);

      bitmap_clear_bit (c->bbs, bb2->index);
      EXECUTE_IF_SET_IN_BITMAP (c->bbs, 0, j, bj)
	{
	  bb1 = BASIC_BLOCK (j);
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
update_debug_stmt (gimple stmt)
{
  use_operand_p use_p;
  ssa_op_iter oi;
  basic_block bbdef, bbuse;
  gimple def_stmt;
  tree name;

  if (!gimple_debug_bind_p (stmt))
    return;

  bbuse = gimple_bb (stmt);
  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, oi, SSA_OP_USE)
    {
      name = USE_FROM_PTR (use_p);
      gcc_assert (TREE_CODE (name) == SSA_NAME);

      def_stmt = SSA_NAME_DEF_STMT (name);
      gcc_assert (def_stmt != NULL);

      bbdef = gimple_bb (def_stmt);
      if (bbdef == NULL || bbuse == bbdef
	  || dominated_by_p (CDI_DOMINATORS, bbuse, bbdef))
	continue;

      gimple_debug_bind_reset_value (stmt);
      update_stmt (stmt);
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
      gimple stmt;
      gimple_stmt_iterator gsi;

      bb = BASIC_BLOCK (i);
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
  int max_iterations = PARAM_VALUE (PARAM_MAX_TAIL_MERGE_ITERATIONS);

  if (!flag_tree_tail_merge || max_iterations == 0)
    return 0;

  timevar_push (TV_TREE_TAIL_MERGE);

  calculate_dominance_info (CDI_DOMINATORS);
  init_worklist ();

  while (!VEC_empty (same_succ, worklist))
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
      gcc_assert (VEC_empty (same_succ, worklist));
      if (VEC_empty (bb_cluster, all_clusters))
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
	     htab_collisions (same_succ_htab));

  if (nr_bbs_removed_total > 0)
    {
      if (MAY_HAVE_DEBUG_STMTS)
	{
	  calculate_dominance_info (CDI_DOMINATORS);
	  update_debug_stmts ();
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Before TODOs.\n");
	  dump_function_to_file (current_function_decl, dump_file, dump_flags);
	}

      todo |= (TODO_verify_ssa | TODO_verify_stmts | TODO_verify_flow
	       | TODO_dump_func);
      mark_sym_for_renaming (gimple_vop (cfun));
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
