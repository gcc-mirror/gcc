/* Thread edges through blocks and update the control flow and SSA graphs.
   Copyright (C) 2004-2019 Free Software Foundation, Inc.

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
#include "gimple-iterator.h"
#include "tree-ssa.h"
#include "tree-ssa-threadupdate.h"
#include "cfgloop.h"
#include "dbgcnt.h"
#include "tree-cfg.h"
#include "tree-vectorizer.h"

/* Given a block B, update the CFG and SSA graph to reflect redirecting
   one or more in-edges to B to instead reach the destination of an
   out-edge from B while preserving any side effects in B.

   i.e., given A->B and B->C, change A->B to be A->C yet still preserve the
   side effects of executing B.

     1. Make a copy of B (including its outgoing edges and statements).  Call
	the copy B'.  Note B' has no incoming edges or PHIs at this time.

     2. Remove the control statement at the end of B' and all outgoing edges
	except B'->C.

     3. Add a new argument to each PHI in C with the same value as the existing
	argument associated with edge B->C.  Associate the new PHI arguments
	with the edge B'->C.

     4. For each PHI in B, find or create a PHI in B' with an identical
	PHI_RESULT.  Add an argument to the PHI in B' which has the same
	value as the PHI in B associated with the edge A->B.  Associate
	the new argument in the PHI in B' with the edge A->B.

     5. Change the edge A->B to A->B'.

	5a. This automatically deletes any PHI arguments associated with the
	    edge A->B in B.

	5b. This automatically associates each new argument added in step 4
	    with the edge A->B'.

     6. Repeat for other incoming edges into B.

     7. Put the duplicated resources in B and all the B' blocks into SSA form.

   Note that block duplication can be minimized by first collecting the
   set of unique destination blocks that the incoming edges should
   be threaded to.

   We reduce the number of edges and statements we create by not copying all
   the outgoing edges and the control statement in step #1.  We instead create
   a template block without the outgoing edges and duplicate the template.

   Another case this code handles is threading through a "joiner" block.  In
   this case, we do not know the destination of the joiner block, but one
   of the outgoing edges from the joiner block leads to a threadable path.  This
   case largely works as outlined above, except the duplicate of the joiner
   block still contains a full set of outgoing edges and its control statement.
   We just redirect one of its outgoing edges to our jump threading path.  */


/* Steps #5 and #6 of the above algorithm are best implemented by walking
   all the incoming edges which thread to the same destination edge at
   the same time.  That avoids lots of table lookups to get information
   for the destination edge.

   To realize that implementation we create a list of incoming edges
   which thread to the same outgoing edge.  Thus to implement steps
   #5 and #6 we traverse our hash table of outgoing edge information.
   For each entry we walk the list of incoming edges which thread to
   the current outgoing edge.  */

struct el
{
  edge e;
  struct el *next;
};

/* Main data structure recording information regarding B's duplicate
   blocks.  */

/* We need to efficiently record the unique thread destinations of this
   block and specific information associated with those destinations.  We
   may have many incoming edges threaded to the same outgoing edge.  This
   can be naturally implemented with a hash table.  */

struct redirection_data : free_ptr_hash<redirection_data>
{
  /* We support wiring up two block duplicates in a jump threading path.

     One is a normal block copy where we remove the control statement
     and wire up its single remaining outgoing edge to the thread path.

     The other is a joiner block where we leave the control statement
     in place, but wire one of the outgoing edges to a thread path.

     In theory we could have multiple block duplicates in a jump
     threading path, but I haven't tried that.

     The duplicate blocks appear in this array in the same order in
     which they appear in the jump thread path.  */
  basic_block dup_blocks[2];

  /* The jump threading path.  */
  vec<jump_thread_edge *> *path;

  /* A list of incoming edges which we want to thread to the
     same path.  */
  struct el *incoming_edges;

  /* hash_table support.  */
  static inline hashval_t hash (const redirection_data *);
  static inline int equal (const redirection_data *, const redirection_data *);
};

/* Dump a jump threading path, including annotations about each
   edge in the path.  */

static void
dump_jump_thread_path (FILE *dump_file, vec<jump_thread_edge *> path,
		       bool registering)
{
  fprintf (dump_file,
	   "  %s%s jump thread: (%d, %d) incoming edge; ",
	   (registering ? "Registering" : "Cancelling"),
	   (path[0]->type == EDGE_FSM_THREAD ? " FSM": ""),
	   path[0]->e->src->index, path[0]->e->dest->index);

  for (unsigned int i = 1; i < path.length (); i++)
    {
      /* We can get paths with a NULL edge when the final destination
	 of a jump thread turns out to be a constant address.  We dump
	 those paths when debugging, so we have to be prepared for that
	 possibility here.  */
      if (path[i]->e == NULL)
	continue;

      if (path[i]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	fprintf (dump_file, " (%d, %d) joiner; ",
		 path[i]->e->src->index, path[i]->e->dest->index);
      if (path[i]->type == EDGE_COPY_SRC_BLOCK)
       fprintf (dump_file, " (%d, %d) normal;",
		 path[i]->e->src->index, path[i]->e->dest->index);
      if (path[i]->type == EDGE_NO_COPY_SRC_BLOCK)
       fprintf (dump_file, " (%d, %d) nocopy;",
		 path[i]->e->src->index, path[i]->e->dest->index);
      if (path[0]->type == EDGE_FSM_THREAD)
	fprintf (dump_file, " (%d, %d) ",
		 path[i]->e->src->index, path[i]->e->dest->index);
    }
  fputc ('\n', dump_file);
}

/* Simple hashing function.  For any given incoming edge E, we're going
   to be most concerned with the final destination of its jump thread
   path.  So hash on the block index of the final edge in the path.  */

inline hashval_t
redirection_data::hash (const redirection_data *p)
{
  vec<jump_thread_edge *> *path = p->path;
  return path->last ()->e->dest->index;
}

/* Given two hash table entries, return true if they have the same
   jump threading path.  */
inline int
redirection_data::equal (const redirection_data *p1, const redirection_data *p2)
{
  vec<jump_thread_edge *> *path1 = p1->path;
  vec<jump_thread_edge *> *path2 = p2->path;

  if (path1->length () != path2->length ())
    return false;

  for (unsigned int i = 1; i < path1->length (); i++)
    {
      if ((*path1)[i]->type != (*path2)[i]->type
	  || (*path1)[i]->e != (*path2)[i]->e)
	return false;
    }

  return true;
}

/* Rather than search all the edges in jump thread paths each time
   DOM is able to simply if control statement, we build a hash table
   with the deleted edges.  We only care about the address of the edge,
   not its contents.  */
struct removed_edges : nofree_ptr_hash<edge_def>
{
  static hashval_t hash (edge e) { return htab_hash_pointer (e); }
  static bool equal (edge e1, edge e2) { return e1 == e2; }
};

static hash_table<removed_edges> *removed_edges;

/* Data structure of information to pass to hash table traversal routines.  */
struct ssa_local_info_t
{
  /* The current block we are working on.  */
  basic_block bb;

  /* We only create a template block for the first duplicated block in a
     jump threading path as we may need many duplicates of that block.

     The second duplicate block in a path is specific to that path.  Creating
     and sharing a template for that block is considerably more difficult.  */
  basic_block template_block;

  /* Blocks duplicated for the thread.  */
  bitmap duplicate_blocks;

  /* TRUE if we thread one or more jumps, FALSE otherwise.  */
  bool jumps_threaded;

  /* When we have multiple paths through a joiner which reach different
     final destinations, then we may need to correct for potential
     profile insanities.  */
  bool need_profile_correction;
};

/* Passes which use the jump threading code register jump threading
   opportunities as they are discovered.  We keep the registered
   jump threading opportunities in this vector as edge pairs
   (original_edge, target_edge).  */
static vec<vec<jump_thread_edge *> *> paths;

/* When we start updating the CFG for threading, data necessary for jump
   threading is attached to the AUX field for the incoming edge.  Use these
   macros to access the underlying structure attached to the AUX field.  */
#define THREAD_PATH(E) ((vec<jump_thread_edge *> *)(E)->aux)

/* Jump threading statistics.  */

struct thread_stats_d
{
  unsigned long num_threaded_edges;
};

struct thread_stats_d thread_stats;


/* Remove the last statement in block BB if it is a control statement
   Also remove all outgoing edges except the edge which reaches DEST_BB.
   If DEST_BB is NULL, then remove all outgoing edges.  */

void
remove_ctrl_stmt_and_useless_edges (basic_block bb, basic_block dest_bb)
{
  gimple_stmt_iterator gsi;
  edge e;
  edge_iterator ei;

  gsi = gsi_last_bb (bb);

  /* If the duplicate ends with a control statement, then remove it.

     Note that if we are duplicating the template block rather than the
     original basic block, then the duplicate might not have any real
     statements in it.  */
  if (!gsi_end_p (gsi)
      && gsi_stmt (gsi)
      && (gimple_code (gsi_stmt (gsi)) == GIMPLE_COND
	  || gimple_code (gsi_stmt (gsi)) == GIMPLE_GOTO
	  || gimple_code (gsi_stmt (gsi)) == GIMPLE_SWITCH))
    gsi_remove (&gsi, true);

  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
    {
      if (e->dest != dest_bb)
	{
	  free_dom_edge_info (e);
	  remove_edge (e);
	}
      else
	{
	  e->probability = profile_probability::always ();
	  ei_next (&ei);
	}
    }

  /* If the remaining edge is a loop exit, there must have
     a removed edge that was not a loop exit.

     In that case BB and possibly other blocks were previously
     in the loop, but are now outside the loop.  Thus, we need
     to update the loop structures.  */
  if (single_succ_p (bb)
      && loop_outer (bb->loop_father)
      && loop_exit_edge_p (bb->loop_father, single_succ_edge (bb)))
    loops_state_set (LOOPS_NEED_FIXUP);
}

/* Create a duplicate of BB.  Record the duplicate block in an array
   indexed by COUNT stored in RD.  */

static void
create_block_for_threading (basic_block bb,
			    struct redirection_data *rd,
			    unsigned int count,
			    bitmap *duplicate_blocks)
{
  edge_iterator ei;
  edge e;

  /* We can use the generic block duplication code and simply remove
     the stuff we do not need.  */
  rd->dup_blocks[count] = duplicate_block (bb, NULL, NULL);

  FOR_EACH_EDGE (e, ei, rd->dup_blocks[count]->succs)
    {
      e->aux = NULL;

      /* If we duplicate a block with an outgoing edge marked as
	 EDGE_IGNORE, we must clear EDGE_IGNORE so that it doesn't
	 leak out of the current pass.

	 It would be better to simplify switch statements and remove
	 the edges before we get here, but the sequencing is nontrivial.  */
      e->flags &= ~EDGE_IGNORE;
    }

  /* Zero out the profile, since the block is unreachable for now.  */
  rd->dup_blocks[count]->count = profile_count::uninitialized ();
  if (duplicate_blocks)
    bitmap_set_bit (*duplicate_blocks, rd->dup_blocks[count]->index);
}

/* Main data structure to hold information for duplicates of BB.  */

static hash_table<redirection_data> *redirection_data;

/* Given an outgoing edge E lookup and return its entry in our hash table.

   If INSERT is true, then we insert the entry into the hash table if
   it is not already present.  INCOMING_EDGE is added to the list of incoming
   edges associated with E in the hash table.  */

static struct redirection_data *
lookup_redirection_data (edge e, enum insert_option insert)
{
  struct redirection_data **slot;
  struct redirection_data *elt;
  vec<jump_thread_edge *> *path = THREAD_PATH (e);

  /* Build a hash table element so we can see if E is already
     in the table.  */
  elt = XNEW (struct redirection_data);
  elt->path = path;
  elt->dup_blocks[0] = NULL;
  elt->dup_blocks[1] = NULL;
  elt->incoming_edges = NULL;

  slot = redirection_data->find_slot (elt, insert);

  /* This will only happen if INSERT is false and the entry is not
     in the hash table.  */
  if (slot == NULL)
    {
      free (elt);
      return NULL;
    }

  /* This will only happen if E was not in the hash table and
     INSERT is true.  */
  if (*slot == NULL)
    {
      *slot = elt;
      elt->incoming_edges = XNEW (struct el);
      elt->incoming_edges->e = e;
      elt->incoming_edges->next = NULL;
      return elt;
    }
  /* E was in the hash table.  */
  else
    {
      /* Free ELT as we do not need it anymore, we will extract the
	 relevant entry from the hash table itself.  */
      free (elt);

      /* Get the entry stored in the hash table.  */
      elt = *slot;

      /* If insertion was requested, then we need to add INCOMING_EDGE
	 to the list of incoming edges associated with E.  */
      if (insert)
	{
	  struct el *el = XNEW (struct el);
	  el->next = elt->incoming_edges;
	  el->e = e;
	  elt->incoming_edges = el;
	}

      return elt;
    }
}

/* Similar to copy_phi_args, except that the PHI arg exists, it just
   does not have a value associated with it.  */

static void
copy_phi_arg_into_existing_phi (edge src_e, edge tgt_e)
{
  int src_idx = src_e->dest_idx;
  int tgt_idx = tgt_e->dest_idx;

  /* Iterate over each PHI in e->dest.  */
  for (gphi_iterator gsi = gsi_start_phis (src_e->dest),
			   gsi2 = gsi_start_phis (tgt_e->dest);
       !gsi_end_p (gsi);
       gsi_next (&gsi), gsi_next (&gsi2))
    {
      gphi *src_phi = gsi.phi ();
      gphi *dest_phi = gsi2.phi ();
      tree val = gimple_phi_arg_def (src_phi, src_idx);
      location_t locus = gimple_phi_arg_location (src_phi, src_idx);

      SET_PHI_ARG_DEF (dest_phi, tgt_idx, val);
      gimple_phi_arg_set_location (dest_phi, tgt_idx, locus);
    }
}

/* Given ssa_name DEF, backtrack jump threading PATH from node IDX
   to see if it has constant value in a flow sensitive manner.  Set
   LOCUS to location of the constant phi arg and return the value.
   Return DEF directly if either PATH or idx is ZERO.  */

static tree
get_value_locus_in_path (tree def, vec<jump_thread_edge *> *path,
			 basic_block bb, int idx, location_t *locus)
{
  tree arg;
  gphi *def_phi;
  basic_block def_bb;

  if (path == NULL || idx == 0)
    return def;

  def_phi = dyn_cast <gphi *> (SSA_NAME_DEF_STMT (def));
  if (!def_phi)
    return def;

  def_bb = gimple_bb (def_phi);
  /* Don't propagate loop invariants into deeper loops.  */
  if (!def_bb || bb_loop_depth (def_bb) < bb_loop_depth (bb))
    return def;

  /* Backtrack jump threading path from IDX to see if def has constant
     value.  */
  for (int j = idx - 1; j >= 0; j--)
    {
      edge e = (*path)[j]->e;
      if (e->dest == def_bb)
	{
	  arg = gimple_phi_arg_def (def_phi, e->dest_idx);
	  if (is_gimple_min_invariant (arg))
	    {
	      *locus = gimple_phi_arg_location (def_phi, e->dest_idx);
	      return arg;
	    }
	  break;
	}
    }

  return def;
}

/* For each PHI in BB, copy the argument associated with SRC_E to TGT_E.
   Try to backtrack jump threading PATH from node IDX to see if the arg
   has constant value, copy constant value instead of argument itself
   if yes.  */

static void
copy_phi_args (basic_block bb, edge src_e, edge tgt_e,
	       vec<jump_thread_edge *> *path, int idx)
{
  gphi_iterator gsi;
  int src_indx = src_e->dest_idx;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree def = gimple_phi_arg_def (phi, src_indx);
      location_t locus = gimple_phi_arg_location (phi, src_indx);

      if (TREE_CODE (def) == SSA_NAME
	  && !virtual_operand_p (gimple_phi_result (phi)))
	def = get_value_locus_in_path (def, path, bb, idx, &locus);

      add_phi_arg (phi, def, tgt_e, locus);
    }
}

/* We have recently made a copy of ORIG_BB, including its outgoing
   edges.  The copy is NEW_BB.  Every PHI node in every direct successor of
   ORIG_BB has a new argument associated with edge from NEW_BB to the
   successor.  Initialize the PHI argument so that it is equal to the PHI
   argument associated with the edge from ORIG_BB to the successor.
   PATH and IDX are used to check if the new PHI argument has constant
   value in a flow sensitive manner.  */

static void
update_destination_phis (basic_block orig_bb, basic_block new_bb,
			 vec<jump_thread_edge *> *path, int idx)
{
  edge_iterator ei;
  edge e;

  FOR_EACH_EDGE (e, ei, orig_bb->succs)
    {
      edge e2 = find_edge (new_bb, e->dest);
      copy_phi_args (e->dest, e, e2, path, idx);
    }
}

/* Given a duplicate block and its single destination (both stored
   in RD).  Create an edge between the duplicate and its single
   destination.

   Add an additional argument to any PHI nodes at the single
   destination.  IDX is the start node in jump threading path
   we start to check to see if the new PHI argument has constant
   value along the jump threading path.  */

static void
create_edge_and_update_destination_phis (struct redirection_data *rd,
					 basic_block bb, int idx)
{
  edge e = make_single_succ_edge (bb, rd->path->last ()->e->dest, EDGE_FALLTHRU);

  rescan_loop_exit (e, true, false);

  /* We used to copy the thread path here.  That was added in 2007
     and dutifully updated through the representation changes in 2013.

     In 2013 we added code to thread from an interior node through
     the backedge to another interior node.  That runs after the code
     to thread through loop headers from outside the loop.

     The latter may delete edges in the CFG, including those
     which appeared in the jump threading path we copied here.  Thus
     we'd end up using a dangling pointer.

     After reviewing the 2007/2011 code, I can't see how anything
     depended on copying the AUX field and clearly copying the jump
     threading path is problematical due to embedded edge pointers.
     It has been removed.  */
  e->aux = NULL;

  /* If there are any PHI nodes at the destination of the outgoing edge
     from the duplicate block, then we will need to add a new argument
     to them.  The argument should have the same value as the argument
     associated with the outgoing edge stored in RD.  */
  copy_phi_args (e->dest, rd->path->last ()->e, e, rd->path, idx);
}

/* Look through PATH beginning at START and return TRUE if there are
   any additional blocks that need to be duplicated.  Otherwise,
   return FALSE.  */
static bool
any_remaining_duplicated_blocks (vec<jump_thread_edge *> *path,
				 unsigned int start)
{
  for (unsigned int i = start + 1; i < path->length (); i++)
    {
      if ((*path)[i]->type == EDGE_COPY_SRC_JOINER_BLOCK
	  || (*path)[i]->type == EDGE_COPY_SRC_BLOCK)
	return true;
    }
  return false;
}


/* Compute the amount of profile count coming into the jump threading
   path stored in RD that we are duplicating, returned in PATH_IN_COUNT_PTR and
   PATH_IN_FREQ_PTR, as well as the amount of counts flowing out of the
   duplicated path, returned in PATH_OUT_COUNT_PTR.  LOCAL_INFO is used to
   identify blocks duplicated for jump threading, which have duplicated
   edges that need to be ignored in the analysis.  Return true if path contains
   a joiner, false otherwise.

   In the non-joiner case, this is straightforward - all the counts
   flowing into the jump threading path should flow through the duplicated
   block and out of the duplicated path.

   In the joiner case, it is very tricky.  Some of the counts flowing into
   the original path go offpath at the joiner.  The problem is that while
   we know how much total count goes off-path in the original control flow,
   we don't know how many of the counts corresponding to just the jump
   threading path go offpath at the joiner.

   For example, assume we have the following control flow and identified
   jump threading paths:

		A     B     C
		 \    |    /
	       Ea \   |Eb / Ec
		   \  |  /
		    v v v
		      J       <-- Joiner
		     / \
		Eoff/   \Eon
		   /     \
		  v       v
		Soff     Son  <--- Normal
			 /\
		      Ed/  \ Ee
		       /    \
		      v     v
		      D      E

	    Jump threading paths: A -> J -> Son -> D (path 1)
				  C -> J -> Son -> E (path 2)

   Note that the control flow could be more complicated:
   - Each jump threading path may have more than one incoming edge.  I.e. A and
   Ea could represent multiple incoming blocks/edges that are included in
   path 1.
   - There could be EDGE_NO_COPY_SRC_BLOCK edges after the joiner (either
   before or after the "normal" copy block).  These are not duplicated onto
   the jump threading path, as they are single-successor.
   - Any of the blocks along the path may have other incoming edges that
   are not part of any jump threading path, but add profile counts along
   the path.

   In the above example, after all jump threading is complete, we will
   end up with the following control flow:

		A	   B	       C
		|	   |	       |
	      Ea|	   |Eb	       |Ec
		|	   |	       |
		v	   v	       v
	       Ja	   J	      Jc
	       / \	  / \Eon'     / \
	  Eona/   \   ---/---\--------   \Eonc
	     /     \ /  /     \		  \
	    v       v  v       v	  v
	   Sona     Soff      Son	Sonc
	     \		       /\	  /
	      \___________    /  \  _____/
			  \  /    \/
			   vv      v
			    D      E

   The main issue to notice here is that when we are processing path 1
   (A->J->Son->D) we need to figure out the outgoing edge weights to
   the duplicated edges Ja->Sona and Ja->Soff, while ensuring that the
   sum of the incoming weights to D remain Ed.  The problem with simply
   assuming that Ja (and Jc when processing path 2) has the same outgoing
   probabilities to its successors as the original block J, is that after
   all paths are processed and other edges/counts removed (e.g. none
   of Ec will reach D after processing path 2), we may end up with not
   enough count flowing along duplicated edge Sona->D.

   Therefore, in the case of a joiner, we keep track of all counts
   coming in along the current path, as well as from predecessors not
   on any jump threading path (Eb in the above example).  While we
   first assume that the duplicated Eona for Ja->Sona has the same
   probability as the original, we later compensate for other jump
   threading paths that may eliminate edges.  We do that by keep track
   of all counts coming into the original path that are not in a jump
   thread (Eb in the above example, but as noted earlier, there could
   be other predecessors incoming to the path at various points, such
   as at Son).  Call this cumulative non-path count coming into the path
   before D as Enonpath.  We then ensure that the count from Sona->D is as at
   least as big as (Ed - Enonpath), but no bigger than the minimum
   weight along the jump threading path.  The probabilities of both the
   original and duplicated joiner block J and Ja will be adjusted
   accordingly after the updates.  */

static bool
compute_path_counts (struct redirection_data *rd,
		     ssa_local_info_t *local_info,
		     profile_count *path_in_count_ptr,
		     profile_count *path_out_count_ptr)
{
  edge e = rd->incoming_edges->e;
  vec<jump_thread_edge *> *path = THREAD_PATH (e);
  edge elast = path->last ()->e;
  profile_count nonpath_count = profile_count::zero ();
  bool has_joiner = false;
  profile_count path_in_count = profile_count::zero ();

  /* Start by accumulating incoming edge counts to the path's first bb
     into a couple buckets:
	path_in_count: total count of incoming edges that flow into the
		  current path.
	nonpath_count: total count of incoming edges that are not
		  flowing along *any* path.  These are the counts
		  that will still flow along the original path after
		  all path duplication is done by potentially multiple
		  calls to this routine.
     (any other incoming edge counts are for a different jump threading
     path that will be handled by a later call to this routine.)
     To make this easier, start by recording all incoming edges that flow into
     the current path in a bitmap.  We could add up the path's incoming edge
     counts here, but we still need to walk all the first bb's incoming edges
     below to add up the counts of the other edges not included in this jump
     threading path.  */
  struct el *next, *el;
  auto_bitmap in_edge_srcs;
  for (el = rd->incoming_edges; el; el = next)
    {
      next = el->next;
      bitmap_set_bit (in_edge_srcs, el->e->src->index);
    }
  edge ein;
  edge_iterator ei;
  FOR_EACH_EDGE (ein, ei, e->dest->preds)
    {
      vec<jump_thread_edge *> *ein_path = THREAD_PATH (ein);
      /* Simply check the incoming edge src against the set captured above.  */
      if (ein_path
	  && bitmap_bit_p (in_edge_srcs, (*ein_path)[0]->e->src->index))
	{
	  /* It is necessary but not sufficient that the last path edges
	     are identical.  There may be different paths that share the
	     same last path edge in the case where the last edge has a nocopy
	     source block.  */
	  gcc_assert (ein_path->last ()->e == elast);
	  path_in_count += ein->count ();
	}
      else if (!ein_path)
	{
	  /* Keep track of the incoming edges that are not on any jump-threading
	     path.  These counts will still flow out of original path after all
	     jump threading is complete.  */
	    nonpath_count += ein->count ();
	}
    }

  /* Now compute the fraction of the total count coming into the first
     path bb that is from the current threading path.  */
  profile_count total_count = e->dest->count;
  /* Handle incoming profile insanities.  */
  if (total_count < path_in_count)
    path_in_count = total_count;
  profile_probability onpath_scale = path_in_count.probability_in (total_count);

  /* Walk the entire path to do some more computation in order to estimate
     how much of the path_in_count will flow out of the duplicated threading
     path.  In the non-joiner case this is straightforward (it should be
     the same as path_in_count, although we will handle incoming profile
     insanities by setting it equal to the minimum count along the path).

     In the joiner case, we need to estimate how much of the path_in_count
     will stay on the threading path after the joiner's conditional branch.
     We don't really know for sure how much of the counts
     associated with this path go to each successor of the joiner, but we'll
     estimate based on the fraction of the total count coming into the path
     bb was from the threading paths (computed above in onpath_scale).
     Afterwards, we will need to do some fixup to account for other threading
     paths and possible profile insanities.

     In order to estimate the joiner case's counts we also need to update
     nonpath_count with any additional counts coming into the path.  Other
     blocks along the path may have additional predecessors from outside
     the path.  */
  profile_count path_out_count = path_in_count;
  profile_count min_path_count = path_in_count;
  for (unsigned int i = 1; i < path->length (); i++)
    {
      edge epath = (*path)[i]->e;
      profile_count cur_count = epath->count ();
      if ((*path)[i]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  has_joiner = true;
	  cur_count = cur_count.apply_probability (onpath_scale);
	}
      /* In the joiner case we need to update nonpath_count for any edges
	 coming into the path that will contribute to the count flowing
	 into the path successor.  */
      if (has_joiner && epath != elast)
	{
	  /* Look for other incoming edges after joiner.  */
	  FOR_EACH_EDGE (ein, ei, epath->dest->preds)
	    {
	      if (ein != epath
		  /* Ignore in edges from blocks we have duplicated for a
		     threading path, which have duplicated edge counts until
		     they are redirected by an invocation of this routine.  */
		  && !bitmap_bit_p (local_info->duplicate_blocks,
				    ein->src->index))
		nonpath_count += ein->count ();
	    }
	}
      if (cur_count < path_out_count)
	path_out_count = cur_count;
      if (epath->count () < min_path_count)
	min_path_count = epath->count ();
    }

  /* We computed path_out_count above assuming that this path targeted
     the joiner's on-path successor with the same likelihood as it
     reached the joiner.  However, other thread paths through the joiner
     may take a different path through the normal copy source block
     (i.e. they have a different elast), meaning that they do not
     contribute any counts to this path's elast.  As a result, it may
     turn out that this path must have more count flowing to the on-path
     successor of the joiner.  Essentially, all of this path's elast
     count must be contributed by this path and any nonpath counts
     (since any path through the joiner with a different elast will not
     include a copy of this elast in its duplicated path).
     So ensure that this path's path_out_count is at least the
     difference between elast->count () and nonpath_count.  Otherwise the edge
     counts after threading will not be sane.  */
  if (local_info->need_profile_correction
      && has_joiner && path_out_count < elast->count () - nonpath_count)
    {
      path_out_count = elast->count () - nonpath_count;
      /* But neither can we go above the minimum count along the path
	 we are duplicating.  This can be an issue due to profile
	 insanities coming in to this pass.  */
      if (path_out_count > min_path_count)
	path_out_count = min_path_count;
    }

  *path_in_count_ptr = path_in_count;
  *path_out_count_ptr = path_out_count;
  return has_joiner;
}


/* Update the counts and frequencies for both an original path
   edge EPATH and its duplicate EDUP.  The duplicate source block
   will get a count of PATH_IN_COUNT and PATH_IN_FREQ,
   and the duplicate edge EDUP will have a count of PATH_OUT_COUNT.  */
static void
update_profile (edge epath, edge edup, profile_count path_in_count,
		profile_count path_out_count)
{

  /* First update the duplicated block's count.  */
  if (edup)
    {
      basic_block dup_block = edup->src;

      /* Edup's count is reduced by path_out_count.  We need to redistribute
         probabilities to the remaining edges.  */

      edge esucc;
      edge_iterator ei;
      profile_probability edup_prob
	 = path_out_count.probability_in (path_in_count);

      /* Either scale up or down the remaining edges.
	 probabilities are always in range <0,1> and thus we can't do
	 both by same loop.  */
      if (edup->probability > edup_prob)
	{
	   profile_probability rev_scale
	     = (profile_probability::always () - edup->probability)
	       / (profile_probability::always () - edup_prob);
	   FOR_EACH_EDGE (esucc, ei, dup_block->succs)
	     if (esucc != edup)
	       esucc->probability /= rev_scale;
	}
      else if (edup->probability < edup_prob)
	{
	   profile_probability scale
	     = (profile_probability::always () - edup_prob)
	       / (profile_probability::always () - edup->probability);
	  FOR_EACH_EDGE (esucc, ei, dup_block->succs)
	    if (esucc != edup)
	      esucc->probability *= scale;
	}
      if (edup_prob.initialized_p ())
        edup->probability = edup_prob;

      gcc_assert (!dup_block->count.initialized_p ());
      dup_block->count = path_in_count;
    }

  if (path_in_count == profile_count::zero ())
    return;

  profile_count final_count = epath->count () - path_out_count;

  /* Now update the original block's count in the
     opposite manner - remove the counts/freq that will flow
     into the duplicated block.  Handle underflow due to precision/
     rounding issues.  */
  epath->src->count -= path_in_count;

  /* Next update this path edge's original and duplicated counts.  We know
     that the duplicated path will have path_out_count flowing
     out of it (in the joiner case this is the count along the duplicated path
     out of the duplicated joiner).  This count can then be removed from the
     original path edge.  */

  edge esucc;
  edge_iterator ei;
  profile_probability epath_prob = final_count.probability_in (epath->src->count);

  if (epath->probability > epath_prob)
    {
       profile_probability rev_scale
	 = (profile_probability::always () - epath->probability)
	   / (profile_probability::always () - epath_prob);
       FOR_EACH_EDGE (esucc, ei, epath->src->succs)
	 if (esucc != epath)
	   esucc->probability /= rev_scale;
    }
  else if (epath->probability < epath_prob)
    {
       profile_probability scale
	 = (profile_probability::always () - epath_prob)
	   / (profile_probability::always () - epath->probability);
      FOR_EACH_EDGE (esucc, ei, epath->src->succs)
	if (esucc != epath)
	  esucc->probability *= scale;
    }
  if (epath_prob.initialized_p ())
    epath->probability = epath_prob;
}

/* Wire up the outgoing edges from the duplicate blocks and
   update any PHIs as needed.  Also update the profile counts
   on the original and duplicate blocks and edges.  */
void
ssa_fix_duplicate_block_edges (struct redirection_data *rd,
			       ssa_local_info_t *local_info)
{
  bool multi_incomings = (rd->incoming_edges->next != NULL);
  edge e = rd->incoming_edges->e;
  vec<jump_thread_edge *> *path = THREAD_PATH (e);
  edge elast = path->last ()->e;
  profile_count path_in_count = profile_count::zero ();
  profile_count path_out_count = profile_count::zero ();

  /* First determine how much profile count to move from original
     path to the duplicate path.  This is tricky in the presence of
     a joiner (see comments for compute_path_counts), where some portion
     of the path's counts will flow off-path from the joiner.  In the
     non-joiner case the path_in_count and path_out_count should be the
     same.  */
  bool has_joiner = compute_path_counts (rd, local_info,
					 &path_in_count, &path_out_count);

  for (unsigned int count = 0, i = 1; i < path->length (); i++)
    {
      edge epath = (*path)[i]->e;

      /* If we were threading through an joiner block, then we want
	 to keep its control statement and redirect an outgoing edge.
	 Else we want to remove the control statement & edges, then create
	 a new outgoing edge.  In both cases we may need to update PHIs.  */
      if ((*path)[i]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  edge victim;
	  edge e2;

	  gcc_assert (has_joiner);

	  /* This updates the PHIs at the destination of the duplicate
	     block.  Pass 0 instead of i if we are threading a path which
	     has multiple incoming edges.  */
	  update_destination_phis (local_info->bb, rd->dup_blocks[count],
				   path, multi_incomings ? 0 : i);

	  /* Find the edge from the duplicate block to the block we're
	     threading through.  That's the edge we want to redirect.  */
	  victim = find_edge (rd->dup_blocks[count], (*path)[i]->e->dest);

	  /* If there are no remaining blocks on the path to duplicate,
	     then redirect VICTIM to the final destination of the jump
	     threading path.  */
	  if (!any_remaining_duplicated_blocks (path, i))
	    {
	      e2 = redirect_edge_and_branch (victim, elast->dest);
	      /* If we redirected the edge, then we need to copy PHI arguments
		 at the target.  If the edge already existed (e2 != victim
		 case), then the PHIs in the target already have the correct
		 arguments.  */
	      if (e2 == victim)
		copy_phi_args (e2->dest, elast, e2,
			       path, multi_incomings ? 0 : i);
	    }
	  else
	    {
	      /* Redirect VICTIM to the next duplicated block in the path.  */
	      e2 = redirect_edge_and_branch (victim, rd->dup_blocks[count + 1]);

	      /* We need to update the PHIs in the next duplicated block.  We
		 want the new PHI args to have the same value as they had
		 in the source of the next duplicate block.

		 Thus, we need to know which edge we traversed into the
		 source of the duplicate.  Furthermore, we may have
		 traversed many edges to reach the source of the duplicate.

		 Walk through the path starting at element I until we
		 hit an edge marked with EDGE_COPY_SRC_BLOCK.  We want
		 the edge from the prior element.  */
	      for (unsigned int j = i + 1; j < path->length (); j++)
		{
		  if ((*path)[j]->type == EDGE_COPY_SRC_BLOCK)
		    {
		      copy_phi_arg_into_existing_phi ((*path)[j - 1]->e, e2);
		      break;
		    }
		}
	    }

	  /* Update the counts of both the original block
	     and path edge, and the duplicates.  The path duplicate's
	     incoming count are the totals for all edges
	     incoming to this jump threading path computed earlier.
	     And we know that the duplicated path will have path_out_count
	     flowing out of it (i.e. along the duplicated path out of the
	     duplicated joiner).  */
	  update_profile (epath, e2, path_in_count, path_out_count);
	}
      else if ((*path)[i]->type == EDGE_COPY_SRC_BLOCK)
	{
	  remove_ctrl_stmt_and_useless_edges (rd->dup_blocks[count], NULL);
	  create_edge_and_update_destination_phis (rd, rd->dup_blocks[count],
						   multi_incomings ? 0 : i);
	  if (count == 1)
	    single_succ_edge (rd->dup_blocks[1])->aux = NULL;

	  /* Update the counts of both the original block
	     and path edge, and the duplicates.  Since we are now after
	     any joiner that may have existed on the path, the count
	     flowing along the duplicated threaded path is path_out_count.
	     If we didn't have a joiner, then cur_path_freq was the sum
	     of the total frequencies along all incoming edges to the
	     thread path (path_in_freq).  If we had a joiner, it would have
	     been updated at the end of that handling to the edge frequency
	     along the duplicated joiner path edge.  */
	  update_profile (epath, EDGE_SUCC (rd->dup_blocks[count], 0),
			  path_out_count, path_out_count);
	}
      else
	{
	  /* No copy case.  In this case we don't have an equivalent block
	     on the duplicated thread path to update, but we do need
	     to remove the portion of the counts/freqs that were moved
	     to the duplicated path from the counts/freqs flowing through
	     this block on the original path.  Since all the no-copy edges
	     are after any joiner, the removed count is the same as
	     path_out_count.

	     If we didn't have a joiner, then cur_path_freq was the sum
	     of the total frequencies along all incoming edges to the
	     thread path (path_in_freq).  If we had a joiner, it would have
	     been updated at the end of that handling to the edge frequency
	     along the duplicated joiner path edge.  */
	   update_profile (epath, NULL, path_out_count, path_out_count);
	}

      /* Increment the index into the duplicated path when we processed
	 a duplicated block.  */
      if ((*path)[i]->type == EDGE_COPY_SRC_JOINER_BLOCK
	  || (*path)[i]->type == EDGE_COPY_SRC_BLOCK)
	{
	  count++;
	}
    }
}

/* Hash table traversal callback routine to create duplicate blocks.  */

int
ssa_create_duplicates (struct redirection_data **slot,
		       ssa_local_info_t *local_info)
{
  struct redirection_data *rd = *slot;

  /* The second duplicated block in a jump threading path is specific
     to the path.  So it gets stored in RD rather than in LOCAL_DATA.

     Each time we're called, we have to look through the path and see
     if a second block needs to be duplicated.

     Note the search starts with the third edge on the path.  The first
     edge is the incoming edge, the second edge always has its source
     duplicated.  Thus we start our search with the third edge.  */
  vec<jump_thread_edge *> *path = rd->path;
  for (unsigned int i = 2; i < path->length (); i++)
    {
      if ((*path)[i]->type == EDGE_COPY_SRC_BLOCK
	  || (*path)[i]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  create_block_for_threading ((*path)[i]->e->src, rd, 1,
				      &local_info->duplicate_blocks);
	  break;
	}
    }

  /* Create a template block if we have not done so already.  Otherwise
     use the template to create a new block.  */
  if (local_info->template_block == NULL)
    {
      create_block_for_threading ((*path)[1]->e->src, rd, 0,
				  &local_info->duplicate_blocks);
      local_info->template_block = rd->dup_blocks[0];

      /* We do not create any outgoing edges for the template.  We will
	 take care of that in a later traversal.  That way we do not
	 create edges that are going to just be deleted.  */
    }
  else
    {
      create_block_for_threading (local_info->template_block, rd, 0,
				  &local_info->duplicate_blocks);

      /* Go ahead and wire up outgoing edges and update PHIs for the duplicate
	 block.   */
      ssa_fix_duplicate_block_edges (rd, local_info);
    }

  /* Keep walking the hash table.  */
  return 1;
}

/* We did not create any outgoing edges for the template block during
   block creation.  This hash table traversal callback creates the
   outgoing edge for the template block.  */

inline int
ssa_fixup_template_block (struct redirection_data **slot,
			  ssa_local_info_t *local_info)
{
  struct redirection_data *rd = *slot;

  /* If this is the template block halt the traversal after updating
     it appropriately.

     If we were threading through an joiner block, then we want
     to keep its control statement and redirect an outgoing edge.
     Else we want to remove the control statement & edges, then create
     a new outgoing edge.  In both cases we may need to update PHIs.  */
  if (rd->dup_blocks[0] && rd->dup_blocks[0] == local_info->template_block)
    {
      ssa_fix_duplicate_block_edges (rd, local_info);
      return 0;
    }

  return 1;
}

/* Hash table traversal callback to redirect each incoming edge
   associated with this hash table element to its new destination.  */

int
ssa_redirect_edges (struct redirection_data **slot,
		    ssa_local_info_t *local_info)
{
  struct redirection_data *rd = *slot;
  struct el *next, *el;

  /* Walk over all the incoming edges associated with this hash table
     entry.  */
  for (el = rd->incoming_edges; el; el = next)
    {
      edge e = el->e;
      vec<jump_thread_edge *> *path = THREAD_PATH (e);

      /* Go ahead and free this element from the list.  Doing this now
	 avoids the need for another list walk when we destroy the hash
	 table.  */
      next = el->next;
      free (el);

      thread_stats.num_threaded_edges++;

      if (rd->dup_blocks[0])
	{
	  edge e2;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
		     e->src->index, e->dest->index, rd->dup_blocks[0]->index);

	  /* Redirect the incoming edge (possibly to the joiner block) to the
	     appropriate duplicate block.  */
	  e2 = redirect_edge_and_branch (e, rd->dup_blocks[0]);
	  gcc_assert (e == e2);
	  flush_pending_stmts (e2);
	}

      /* Go ahead and clear E->aux.  It's not needed anymore and failure
	 to clear it will cause all kinds of unpleasant problems later.  */
      delete_jump_thread_path (path);
      e->aux = NULL;

    }

  /* Indicate that we actually threaded one or more jumps.  */
  if (rd->incoming_edges)
    local_info->jumps_threaded = true;

  return 1;
}

/* Return true if this block has no executable statements other than
   a simple ctrl flow instruction.  When the number of outgoing edges
   is one, this is equivalent to a "forwarder" block.  */

static bool
redirection_block_p (basic_block bb)
{
  gimple_stmt_iterator gsi;

  /* Advance to the first executable statement.  */
  gsi = gsi_start_bb (bb);
  while (!gsi_end_p (gsi)
	 && (gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL
	     || is_gimple_debug (gsi_stmt (gsi))
	     || gimple_nop_p (gsi_stmt (gsi))
	     || gimple_clobber_p (gsi_stmt (gsi))))
    gsi_next (&gsi);

  /* Check if this is an empty block.  */
  if (gsi_end_p (gsi))
    return true;

  /* Test that we've reached the terminating control statement.  */
  return gsi_stmt (gsi)
	 && (gimple_code (gsi_stmt (gsi)) == GIMPLE_COND
	     || gimple_code (gsi_stmt (gsi)) == GIMPLE_GOTO
	     || gimple_code (gsi_stmt (gsi)) == GIMPLE_SWITCH);
}

/* BB is a block which ends with a COND_EXPR or SWITCH_EXPR and when BB
   is reached via one or more specific incoming edges, we know which
   outgoing edge from BB will be traversed.

   We want to redirect those incoming edges to the target of the
   appropriate outgoing edge.  Doing so avoids a conditional branch
   and may expose new optimization opportunities.  Note that we have
   to update dominator tree and SSA graph after such changes.

   The key to keeping the SSA graph update manageable is to duplicate
   the side effects occurring in BB so that those side effects still
   occur on the paths which bypass BB after redirecting edges.

   We accomplish this by creating duplicates of BB and arranging for
   the duplicates to unconditionally pass control to one specific
   successor of BB.  We then revector the incoming edges into BB to
   the appropriate duplicate of BB.

   If NOLOOP_ONLY is true, we only perform the threading as long as it
   does not affect the structure of the loops in a nontrivial way.

   If JOINERS is true, then thread through joiner blocks as well.  */

static bool
thread_block_1 (basic_block bb, bool noloop_only, bool joiners)
{
  /* E is an incoming edge into BB that we may or may not want to
     redirect to a duplicate of BB.  */
  edge e, e2;
  edge_iterator ei;
  ssa_local_info_t local_info;

  local_info.duplicate_blocks = BITMAP_ALLOC (NULL);
  local_info.need_profile_correction = false;

  /* To avoid scanning a linear array for the element we need we instead
     use a hash table.  For normal code there should be no noticeable
     difference.  However, if we have a block with a large number of
     incoming and outgoing edges such linear searches can get expensive.  */
  redirection_data
    = new hash_table<struct redirection_data> (EDGE_COUNT (bb->succs));

  /* Record each unique threaded destination into a hash table for
     efficient lookups.  */
  edge last = NULL;
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (e->aux == NULL)
	continue;

      vec<jump_thread_edge *> *path = THREAD_PATH (e);

      if (((*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK && !joiners)
	  || ((*path)[1]->type == EDGE_COPY_SRC_BLOCK && joiners))
	continue;

      e2 = path->last ()->e;
      if (!e2 || noloop_only)
	{
	  /* If NOLOOP_ONLY is true, we only allow threading through the
	     header of a loop to exit edges.  */

	  /* One case occurs when there was loop header buried in a jump
	     threading path that crosses loop boundaries.  We do not try
	     and thread this elsewhere, so just cancel the jump threading
	     request by clearing the AUX field now.  */
	  if (bb->loop_father != e2->src->loop_father
	      && (!loop_exit_edge_p (e2->src->loop_father, e2)
		  || flow_loop_nested_p (bb->loop_father,
					 e2->dest->loop_father)))
	    {
	      /* Since this case is not handled by our special code
		 to thread through a loop header, we must explicitly
		 cancel the threading request here.  */
	      delete_jump_thread_path (path);
	      e->aux = NULL;
	      continue;
	    }

	  /* Another case occurs when trying to thread through our
	     own loop header, possibly from inside the loop.  We will
	     thread these later.  */
	  unsigned int i;
	  for (i = 1; i < path->length (); i++)
	    {
	      if ((*path)[i]->e->src == bb->loop_father->header
		  && (!loop_exit_edge_p (bb->loop_father, e2)
		      || (*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK))
		break;
	    }

	  if (i != path->length ())
	    continue;

	  /* Loop parallelization can be confused by the result of
	     threading through the loop exit test back into the loop.
	     However, theading those jumps seems to help other codes.

	     I have been unable to find anything related to the shape of
	     the CFG, the contents of the affected blocks, etc which would
	     allow a more sensible test than what we're using below which
	     merely avoids the optimization when parallelizing loops.  */
	  if (flag_tree_parallelize_loops > 1)
	    {
	      for (i = 1; i < path->length (); i++)
	        if (bb->loop_father == e2->src->loop_father
		    && loop_exits_from_bb_p (bb->loop_father,
					     (*path)[i]->e->src)
		    && !loop_exit_edge_p (bb->loop_father, e2))
		  break;

	      if (i != path->length ())
		{
		  delete_jump_thread_path (path);
		  e->aux = NULL;
		  continue;
		}
	    }
	}

      /* Insert the outgoing edge into the hash table if it is not
	 already in the hash table.  */
      lookup_redirection_data (e, INSERT);

      /* When we have thread paths through a common joiner with different
	 final destinations, then we may need corrections to deal with
	 profile insanities.  See the big comment before compute_path_counts.  */
      if ((*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  if (!last)
	    last = e2;
	  else if (e2 != last)
	    local_info.need_profile_correction = true;
	}
    }

  /* We do not update dominance info.  */
  free_dominance_info (CDI_DOMINATORS);

  /* We know we only thread through the loop header to loop exits.
     Let the basic block duplication hook know we are not creating
     a multiple entry loop.  */
  if (noloop_only
      && bb == bb->loop_father->header)
    set_loop_copy (bb->loop_father, loop_outer (bb->loop_father));

  /* Now create duplicates of BB.

     Note that for a block with a high outgoing degree we can waste
     a lot of time and memory creating and destroying useless edges.

     So we first duplicate BB and remove the control structure at the
     tail of the duplicate as well as all outgoing edges from the
     duplicate.  We then use that duplicate block as a template for
     the rest of the duplicates.  */
  local_info.template_block = NULL;
  local_info.bb = bb;
  local_info.jumps_threaded = false;
  redirection_data->traverse <ssa_local_info_t *, ssa_create_duplicates>
			    (&local_info);

  /* The template does not have an outgoing edge.  Create that outgoing
     edge and update PHI nodes as the edge's target as necessary.

     We do this after creating all the duplicates to avoid creating
     unnecessary edges.  */
  redirection_data->traverse <ssa_local_info_t *, ssa_fixup_template_block>
			    (&local_info);

  /* The hash table traversals above created the duplicate blocks (and the
     statements within the duplicate blocks).  This loop creates PHI nodes for
     the duplicated blocks and redirects the incoming edges into BB to reach
     the duplicates of BB.  */
  redirection_data->traverse <ssa_local_info_t *, ssa_redirect_edges>
			    (&local_info);

  /* Done with this block.  Clear REDIRECTION_DATA.  */
  delete redirection_data;
  redirection_data = NULL;

  if (noloop_only
      && bb == bb->loop_father->header)
    set_loop_copy (bb->loop_father, NULL);

  BITMAP_FREE (local_info.duplicate_blocks);
  local_info.duplicate_blocks = NULL;

  /* Indicate to our caller whether or not any jumps were threaded.  */
  return local_info.jumps_threaded;
}

/* Wrapper for thread_block_1 so that we can first handle jump
   thread paths which do not involve copying joiner blocks, then
   handle jump thread paths which have joiner blocks.

   By doing things this way we can be as aggressive as possible and
   not worry that copying a joiner block will create a jump threading
   opportunity.  */

static bool
thread_block (basic_block bb, bool noloop_only)
{
  bool retval;
  retval = thread_block_1 (bb, noloop_only, false);
  retval |= thread_block_1 (bb, noloop_only, true);
  return retval;
}

/* Callback for dfs_enumerate_from.  Returns true if BB is different
   from STOP and DBDS_CE_STOP.  */

static basic_block dbds_ce_stop;
static bool
dbds_continue_enumeration_p (const_basic_block bb, const void *stop)
{
  return (bb != (const_basic_block) stop
	  && bb != dbds_ce_stop);
}

/* Evaluates the dominance relationship of latch of the LOOP and BB, and
   returns the state.  */

enum bb_dom_status
determine_bb_domination_status (struct loop *loop, basic_block bb)
{
  basic_block *bblocks;
  unsigned nblocks, i;
  bool bb_reachable = false;
  edge_iterator ei;
  edge e;

  /* This function assumes BB is a successor of LOOP->header.
     If that is not the case return DOMST_NONDOMINATING which
     is always safe.  */
    {
      bool ok = false;

      FOR_EACH_EDGE (e, ei, bb->preds)
	{
     	  if (e->src == loop->header)
	    {
	      ok = true;
	      break;
	    }
	}

      if (!ok)
	return DOMST_NONDOMINATING;
    }

  if (bb == loop->latch)
    return DOMST_DOMINATING;

  /* Check that BB dominates LOOP->latch, and that it is back-reachable
     from it.  */

  bblocks = XCNEWVEC (basic_block, loop->num_nodes);
  dbds_ce_stop = loop->header;
  nblocks = dfs_enumerate_from (loop->latch, 1, dbds_continue_enumeration_p,
				bblocks, loop->num_nodes, bb);
  for (i = 0; i < nblocks; i++)
    FOR_EACH_EDGE (e, ei, bblocks[i]->preds)
      {
	if (e->src == loop->header)
	  {
	    free (bblocks);
	    return DOMST_NONDOMINATING;
	  }
	if (e->src == bb)
	  bb_reachable = true;
      }

  free (bblocks);
  return (bb_reachable ? DOMST_DOMINATING : DOMST_LOOP_BROKEN);
}

/* Thread jumps through the header of LOOP.  Returns true if cfg changes.
   If MAY_PEEL_LOOP_HEADERS is false, we avoid threading from entry edges
   to the inside of the loop.  */

static bool
thread_through_loop_header (struct loop *loop, bool may_peel_loop_headers)
{
  basic_block header = loop->header;
  edge e, tgt_edge, latch = loop_latch_edge (loop);
  edge_iterator ei;
  basic_block tgt_bb, atgt_bb;
  enum bb_dom_status domst;

  /* We have already threaded through headers to exits, so all the threading
     requests now are to the inside of the loop.  We need to avoid creating
     irreducible regions (i.e., loops with more than one entry block), and
     also loop with several latch edges, or new subloops of the loop (although
     there are cases where it might be appropriate, it is difficult to decide,
     and doing it wrongly may confuse other optimizers).

     We could handle more general cases here.  However, the intention is to
     preserve some information about the loop, which is impossible if its
     structure changes significantly, in a way that is not well understood.
     Thus we only handle few important special cases, in which also updating
     of the loop-carried information should be feasible:

     1) Propagation of latch edge to a block that dominates the latch block
	of a loop.  This aims to handle the following idiom:

	first = 1;
	while (1)
	  {
	    if (first)
	      initialize;
	    first = 0;
	    body;
	  }

	After threading the latch edge, this becomes

	first = 1;
	if (first)
	  initialize;
	while (1)
	  {
	    first = 0;
	    body;
	  }

	The original header of the loop is moved out of it, and we may thread
	the remaining edges through it without further constraints.

     2) All entry edges are propagated to a single basic block that dominates
	the latch block of the loop.  This aims to handle the following idiom
	(normally created for "for" loops):

	i = 0;
	while (1)
	  {
	    if (i >= 100)
	      break;
	    body;
	    i++;
	  }

	This becomes

	i = 0;
	while (1)
	  {
	    body;
	    i++;
	    if (i >= 100)
	      break;
	  }
     */

  /* Threading through the header won't improve the code if the header has just
     one successor.  */
  if (single_succ_p (header))
    goto fail;

  if (!may_peel_loop_headers && !redirection_block_p (loop->header))
    goto fail;
  else
    {
      tgt_bb = NULL;
      tgt_edge = NULL;
      FOR_EACH_EDGE (e, ei, header->preds)
	{
	  if (!e->aux)
	    {
	      if (e == latch)
		continue;

	      /* If latch is not threaded, and there is a header
		 edge that is not threaded, we would create loop
		 with multiple entries.  */
	      goto fail;
	    }

	  vec<jump_thread_edge *> *path = THREAD_PATH (e);

	  if ((*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	    goto fail;
	  tgt_edge = (*path)[1]->e;
	  atgt_bb = tgt_edge->dest;
	  if (!tgt_bb)
	    tgt_bb = atgt_bb;
	  /* Two targets of threading would make us create loop
	     with multiple entries.  */
	  else if (tgt_bb != atgt_bb)
	    goto fail;
	}

      if (!tgt_bb)
	{
	  /* There are no threading requests.  */
	  return false;
	}

      /* Redirecting to empty loop latch is useless.  */
      if (tgt_bb == loop->latch
	  && empty_block_p (loop->latch))
	goto fail;
    }

  /* The target block must dominate the loop latch, otherwise we would be
     creating a subloop.  */
  domst = determine_bb_domination_status (loop, tgt_bb);
  if (domst == DOMST_NONDOMINATING)
    goto fail;
  if (domst == DOMST_LOOP_BROKEN)
    {
      /* If the loop ceased to exist, mark it as such, and thread through its
	 original header.  */
      mark_loop_for_removal (loop);
      return thread_block (header, false);
    }

  if (tgt_bb->loop_father->header == tgt_bb)
    {
      /* If the target of the threading is a header of a subloop, we need
	 to create a preheader for it, so that the headers of the two loops
	 do not merge.  */
      if (EDGE_COUNT (tgt_bb->preds) > 2)
	{
	  tgt_bb = create_preheader (tgt_bb->loop_father, 0);
	  gcc_assert (tgt_bb != NULL);
	}
      else
	tgt_bb = split_edge (tgt_edge);
    }

  basic_block new_preheader;

  /* Now consider the case entry edges are redirected to the new entry
     block.  Remember one entry edge, so that we can find the new
     preheader (its destination after threading).  */
  FOR_EACH_EDGE (e, ei, header->preds)
    {
      if (e->aux)
	break;
    }

  /* The duplicate of the header is the new preheader of the loop.  Ensure
     that it is placed correctly in the loop hierarchy.  */
  set_loop_copy (loop, loop_outer (loop));

  thread_block (header, false);
  set_loop_copy (loop, NULL);
  new_preheader = e->dest;

  /* Create the new latch block.  This is always necessary, as the latch
     must have only a single successor, but the original header had at
     least two successors.  */
  loop->latch = NULL;
  mfb_kj_edge = single_succ_edge (new_preheader);
  loop->header = mfb_kj_edge->dest;
  latch = make_forwarder_block (tgt_bb, mfb_keep_just, NULL);
  loop->header = latch->dest;
  loop->latch = latch->src;
  return true;

fail:
  /* We failed to thread anything.  Cancel the requests.  */
  FOR_EACH_EDGE (e, ei, header->preds)
    {
      vec<jump_thread_edge *> *path = THREAD_PATH (e);

      if (path)
	{
	  delete_jump_thread_path (path);
	  e->aux = NULL;
	}
    }
  return false;
}

/* E1 and E2 are edges into the same basic block.  Return TRUE if the
   PHI arguments associated with those edges are equal or there are no
   PHI arguments, otherwise return FALSE.  */

static bool
phi_args_equal_on_edges (edge e1, edge e2)
{
  gphi_iterator gsi;
  int indx1 = e1->dest_idx;
  int indx2 = e2->dest_idx;

  for (gsi = gsi_start_phis (e1->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();

      if (!operand_equal_p (gimple_phi_arg_def (phi, indx1),
			    gimple_phi_arg_def (phi, indx2), 0))
	return false;
    }
  return true;
}

/* Return the number of non-debug statements and non-virtual PHIs in a
   block.  */

static unsigned int
count_stmts_and_phis_in_block (basic_block bb)
{
  unsigned int num_stmts = 0;

  gphi_iterator gpi;
  for (gpi = gsi_start_phis (bb); !gsi_end_p (gpi); gsi_next (&gpi))
    if (!virtual_operand_p (PHI_RESULT (gpi.phi ())))
      num_stmts++;

  gimple_stmt_iterator gsi;
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!is_gimple_debug (stmt))
        num_stmts++;
    }

  return num_stmts;
}


/* Walk through the registered jump threads and convert them into a
   form convenient for this pass.

   Any block which has incoming edges threaded to outgoing edges
   will have its entry in THREADED_BLOCK set.

   Any threaded edge will have its new outgoing edge stored in the
   original edge's AUX field.

   This form avoids the need to walk all the edges in the CFG to
   discover blocks which need processing and avoids unnecessary
   hash table lookups to map from threaded edge to new target.  */

static void
mark_threaded_blocks (bitmap threaded_blocks)
{
  unsigned int i;
  bitmap_iterator bi;
  auto_bitmap tmp;
  basic_block bb;
  edge e;
  edge_iterator ei;

  /* It is possible to have jump threads in which one is a subpath
     of the other.  ie, (A, B), (B, C), (C, D) where B is a joiner
     block and (B, C), (C, D) where no joiner block exists.

     When this occurs ignore the jump thread request with the joiner
     block.  It's totally subsumed by the simpler jump thread request.

     This results in less block copying, simpler CFGs.  More importantly,
     when we duplicate the joiner block, B, in this case we will create
     a new threading opportunity that we wouldn't be able to optimize
     until the next jump threading iteration.

     So first convert the jump thread requests which do not require a
     joiner block.  */
  for (i = 0; i < paths.length (); i++)
    {
      vec<jump_thread_edge *> *path = paths[i];

      if (path->length () > 1
	  && (*path)[1]->type != EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  edge e = (*path)[0]->e;
	  e->aux = (void *)path;
	  bitmap_set_bit (tmp, e->dest->index);
	}
    }

  /* Now iterate again, converting cases where we want to thread
     through a joiner block, but only if no other edge on the path
     already has a jump thread attached to it.  We do this in two passes,
     to avoid situations where the order in the paths vec can hide overlapping
     threads (the path is recorded on the incoming edge, so we would miss
     cases where the second path starts at a downstream edge on the same
     path).  First record all joiner paths, deleting any in the unexpected
     case where there is already a path for that incoming edge.  */
  for (i = 0; i < paths.length ();)
    {
      vec<jump_thread_edge *> *path = paths[i];

      if (path->length () > 1
	  && (*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  /* Attach the path to the starting edge if none is yet recorded.  */
	  if ((*path)[0]->e->aux == NULL)
	    {
	      (*path)[0]->e->aux = path;
	      i++;
	    }
	  else
	    {
	      paths.unordered_remove (i);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		dump_jump_thread_path (dump_file, *path, false);
	      delete_jump_thread_path (path);
	    }
	}
      else
	{
	  i++;
	}
    }

  /* Second, look for paths that have any other jump thread attached to
     them, and either finish converting them or cancel them.  */
  for (i = 0; i < paths.length ();)
    {
      vec<jump_thread_edge *> *path = paths[i];
      edge e = (*path)[0]->e;

      if (path->length () > 1
	  && (*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK && e->aux == path)
	{
	  unsigned int j;
	  for (j = 1; j < path->length (); j++)
	    if ((*path)[j]->e->aux != NULL)
	      break;

	  /* If we iterated through the entire path without exiting the loop,
	     then we are good to go, record it.  */
	  if (j == path->length ())
	    {
	      bitmap_set_bit (tmp, e->dest->index);
	      i++;
	    }
	  else
	    {
	      e->aux = NULL;
	      paths.unordered_remove (i);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		dump_jump_thread_path (dump_file, *path, false);
	      delete_jump_thread_path (path);
	    }
	}
      else
	{
	  i++;
	}
    }

  /* When optimizing for size, prune all thread paths where statement
     duplication is necessary.

     We walk the jump thread path looking for copied blocks.  There's
     two types of copied blocks.

       EDGE_COPY_SRC_JOINER_BLOCK is always copied and thus we will
       cancel the jump threading request when optimizing for size.

       EDGE_COPY_SRC_BLOCK which is copied, but some of its statements
       will be killed by threading.  If threading does not kill all of
       its statements, then we should cancel the jump threading request
       when optimizing for size.  */
  if (optimize_function_for_size_p (cfun))
    {
      EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
	{
	  FOR_EACH_EDGE (e, ei, BASIC_BLOCK_FOR_FN (cfun, i)->preds)
	    if (e->aux)
	      {
		vec<jump_thread_edge *> *path = THREAD_PATH (e);

		unsigned int j;
		for (j = 1; j < path->length (); j++)
		  {
		    bb = (*path)[j]->e->src;
		    if (redirection_block_p (bb))
		      ;
		    else if ((*path)[j]->type == EDGE_COPY_SRC_JOINER_BLOCK
			     || ((*path)[j]->type == EDGE_COPY_SRC_BLOCK
			         && (count_stmts_and_phis_in_block (bb)
				     != estimate_threading_killed_stmts (bb))))
		      break;
		  }

		if (j != path->length ())
		  {
		    if (dump_file && (dump_flags & TDF_DETAILS))
		      dump_jump_thread_path (dump_file, *path, 0);
		    delete_jump_thread_path (path);
		    e->aux = NULL;
		  }
		else
		  bitmap_set_bit (threaded_blocks, i);
	      }
	}
    }
  else
    bitmap_copy (threaded_blocks, tmp);

  /* If we have a joiner block (J) which has two successors S1 and S2 and
     we are threading though S1 and the final destination of the thread
     is S2, then we must verify that any PHI nodes in S2 have the same
     PHI arguments for the edge J->S2 and J->S1->...->S2.

     We used to detect this prior to registering the jump thread, but
     that prohibits propagation of edge equivalences into non-dominated
     PHI nodes as the equivalency test might occur before propagation.

     This must also occur after we truncate any jump threading paths
     as this scenario may only show up after truncation.

     This works for now, but will need improvement as part of the FSA
     optimization.

     Note since we've moved the thread request data to the edges,
     we have to iterate on those rather than the threaded_edges vector.  */
  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, i);
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->aux)
	    {
	      vec<jump_thread_edge *> *path = THREAD_PATH (e);
	      bool have_joiner = ((*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK);

	      if (have_joiner)
		{
		  basic_block joiner = e->dest;
		  edge final_edge = path->last ()->e;
		  basic_block final_dest = final_edge->dest;
		  edge e2 = find_edge (joiner, final_dest);

		  if (e2 && !phi_args_equal_on_edges (e2, final_edge))
		    {
		      delete_jump_thread_path (path);
		      e->aux = NULL;
		    }
		}
	    }
	}
    }

  /* Look for jump threading paths which cross multiple loop headers.

     The code to thread through loop headers will change the CFG in ways
     that invalidate the cached loop iteration information.  So we must
     detect that case and wipe the cached information.  */
  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->aux)
	    {
	      vec<jump_thread_edge *> *path = THREAD_PATH (e);

	      for (unsigned int i = 0, crossed_headers = 0;
		   i < path->length ();
		   i++)
		{
		  basic_block dest = (*path)[i]->e->dest;
		  basic_block src = (*path)[i]->e->src;
		  /* If we enter a loop.  */
		  if (flow_loop_nested_p (src->loop_father, dest->loop_father))
		    ++crossed_headers;
		  /* If we step from a block outside an irreducible region
		     to a block inside an irreducible region, then we have
		     crossed into a loop.  */
		  else if (! (src->flags & BB_IRREDUCIBLE_LOOP)
			   && (dest->flags & BB_IRREDUCIBLE_LOOP))
		      ++crossed_headers;
		  if (crossed_headers > 1)
		    {
		      vect_free_loop_info_assumptions
			((*path)[path->length () - 1]->e->dest->loop_father);
		      break;
		    }
		}
	    }
	}
    }
}


/* Verify that the REGION is a valid jump thread.  A jump thread is a special
   case of SEME Single Entry Multiple Exits region in which all nodes in the
   REGION have exactly one incoming edge.  The only exception is the first block
   that may not have been connected to the rest of the cfg yet.  */

DEBUG_FUNCTION void
verify_jump_thread (basic_block *region, unsigned n_region)
{
  for (unsigned i = 0; i < n_region; i++)
    gcc_assert (EDGE_COUNT (region[i]->preds) <= 1);
}

/* Return true when BB is one of the first N items in BBS.  */

static inline bool
bb_in_bbs (basic_block bb, basic_block *bbs, int n)
{
  for (int i = 0; i < n; i++)
    if (bb == bbs[i])
      return true;

  return false;
}

DEBUG_FUNCTION void
debug_path (FILE *dump_file, int pathno)
{
  vec<jump_thread_edge *> *p = paths[pathno];
  fprintf (dump_file, "path: ");
  for (unsigned i = 0; i < p->length (); ++i)
    fprintf (dump_file, "%d -> %d, ",
	     (*p)[i]->e->src->index, (*p)[i]->e->dest->index);
  fprintf (dump_file, "\n");
}

DEBUG_FUNCTION void
debug_all_paths ()
{
  for (unsigned i = 0; i < paths.length (); ++i)
    debug_path (stderr, i);
}

/* Rewire a jump_thread_edge so that the source block is now a
   threaded source block.

   PATH_NUM is an index into the global path table PATHS.
   EDGE_NUM is the jump thread edge number into said path.

   Returns TRUE if we were able to successfully rewire the edge.  */

static bool
rewire_first_differing_edge (unsigned path_num, unsigned edge_num)
{
  vec<jump_thread_edge *> *path = paths[path_num];
  edge &e = (*path)[edge_num]->e;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "rewiring edge candidate: %d -> %d\n",
	     e->src->index, e->dest->index);
  basic_block src_copy = get_bb_copy (e->src);
  if (src_copy == NULL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "ignoring candidate: there is no src COPY\n");
      return false;
    }
  edge new_edge = find_edge (src_copy, e->dest);
  /* If the previously threaded paths created a flow graph where we
     can no longer figure out where to go, give up.  */
  if (new_edge == NULL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "ignoring candidate: we lost our way\n");
      return false;
    }
  e = new_edge;
  return true;
}

/* After an FSM path has been jump threaded, adjust the remaining FSM
   paths that are subsets of this path, so these paths can be safely
   threaded within the context of the new threaded path.

   For example, suppose we have just threaded:

   5 -> 6 -> 7 -> 8 -> 12	=>	5 -> 6' -> 7' -> 8' -> 12'

   And we have an upcoming threading candidate:
   5 -> 6 -> 7 -> 8 -> 15 -> 20

   This function adjusts the upcoming path into:
   8' -> 15 -> 20

   CURR_PATH_NUM is an index into the global paths table.  It
   specifies the path that was just threaded.  */

static void
adjust_paths_after_duplication (unsigned curr_path_num)
{
  vec<jump_thread_edge *> *curr_path = paths[curr_path_num];
  gcc_assert ((*curr_path)[0]->type == EDGE_FSM_THREAD);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "just threaded: ");
      debug_path (dump_file, curr_path_num);
    }

  /* Iterate through all the other paths and adjust them.  */
  for (unsigned cand_path_num = 0; cand_path_num < paths.length (); )
    {
      if (cand_path_num == curr_path_num)
	{
	  ++cand_path_num;
	  continue;
	}
      /* Make sure the candidate to adjust starts with the same path
	 as the recently threaded path and is an FSM thread.  */
      vec<jump_thread_edge *> *cand_path = paths[cand_path_num];
      if ((*cand_path)[0]->type != EDGE_FSM_THREAD
	  || (*cand_path)[0]->e != (*curr_path)[0]->e)
	{
	  ++cand_path_num;
	  continue;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "adjusting candidate: ");
	  debug_path (dump_file, cand_path_num);
	}

      /* Chop off from the candidate path any prefix it shares with
	 the recently threaded path.  */
      unsigned minlength = MIN (curr_path->length (), cand_path->length ());
      unsigned j;
      for (j = 0; j < minlength; ++j)
	{
	  edge cand_edge = (*cand_path)[j]->e;
	  edge curr_edge = (*curr_path)[j]->e;

	  /* Once the prefix no longer matches, adjust the first
	     non-matching edge to point from an adjusted edge to
	     wherever it was going.  */
	  if (cand_edge != curr_edge)
	    {
	      gcc_assert (cand_edge->src == curr_edge->src);
	      if (!rewire_first_differing_edge (cand_path_num, j))
		goto remove_candidate_from_list;
	      break;
	    }
	}
      if (j == minlength)
	{
	  /* If we consumed the max subgraph we could look at, and
	     still didn't find any different edges, it's the
	     last edge after MINLENGTH.  */
	  if (cand_path->length () > minlength)
	    {
	      if (!rewire_first_differing_edge (cand_path_num, j))
		goto remove_candidate_from_list;
	    }
	  else if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "adjusting first edge after MINLENGTH.\n");
	}
      if (j > 0)
	{
	  /* If we are removing everything, delete the entire candidate.  */
	  if (j == cand_path->length ())
	    {
	    remove_candidate_from_list:
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "adjusted candidate: [EMPTY]\n");
	      delete_jump_thread_path (cand_path);
	      paths.unordered_remove (cand_path_num);
	      continue;
	    }
	  /* Otherwise, just remove the redundant sub-path.  */
	  cand_path->block_remove (0, j);
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "adjusted candidate: ");
	  debug_path (dump_file, cand_path_num);
	}
      ++cand_path_num;
    }
}

/* Duplicates a jump-thread path of N_REGION basic blocks.
   The ENTRY edge is redirected to the duplicate of the region.

   Remove the last conditional statement in the last basic block in the REGION,
   and create a single fallthru edge pointing to the same destination as the
   EXIT edge.

   CURRENT_PATH_NO is an index into the global paths[] table
   specifying the jump-thread path.

   Returns false if it is unable to copy the region, true otherwise.  */

static bool
duplicate_thread_path (edge entry, edge exit, basic_block *region,
		       unsigned n_region, unsigned current_path_no)
{
  unsigned i;
  struct loop *loop = entry->dest->loop_father;
  edge exit_copy;
  edge redirected;
  profile_count curr_count;

  if (!can_copy_bbs_p (region, n_region))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nabout to thread: ");
      debug_path (dump_file, current_path_no);
    }

  /* Some sanity checking.  Note that we do not check for all possible
     missuses of the functions.  I.e. if you ask to copy something weird,
     it will work, but the state of structures probably will not be
     correct.  */
  for (i = 0; i < n_region; i++)
    {
      /* We do not handle subloops, i.e. all the blocks must belong to the
	 same loop.  */
      if (region[i]->loop_father != loop)
	return false;
    }

  initialize_original_copy_tables ();

  set_loop_copy (loop, loop);

  basic_block *region_copy = XNEWVEC (basic_block, n_region);
  copy_bbs (region, n_region, region_copy, &exit, 1, &exit_copy, loop,
	    split_edge_bb_loc (entry), false);

  /* Fix up: copy_bbs redirects all edges pointing to copied blocks.  The
     following code ensures that all the edges exiting the jump-thread path are
     redirected back to the original code: these edges are exceptions
     invalidating the property that is propagated by executing all the blocks of
     the jump-thread path in order.  */

  curr_count = entry->count ();

  for (i = 0; i < n_region; i++)
    {
      edge e;
      edge_iterator ei;
      basic_block bb = region_copy[i];

      /* Watch inconsistent profile.  */
      if (curr_count > region[i]->count)
	curr_count = region[i]->count;
      /* Scale current BB.  */
      if (region[i]->count.nonzero_p () && curr_count.initialized_p ())
	{
	  /* In the middle of the path we only scale the frequencies.
	     In last BB we need to update probabilities of outgoing edges
	     because we know which one is taken at the threaded path.  */
	  if (i + 1 != n_region)
	    scale_bbs_frequencies_profile_count (region + i, 1,
					         region[i]->count - curr_count,
					         region[i]->count);
	  else
	    update_bb_profile_for_threading (region[i],
					     curr_count,
					     exit);
	  scale_bbs_frequencies_profile_count (region_copy + i, 1, curr_count,
					       region_copy[i]->count);
	}

      if (single_succ_p (bb))
	{
	  /* Make sure the successor is the next node in the path.  */
	  gcc_assert (i + 1 == n_region
		      || region_copy[i + 1] == single_succ_edge (bb)->dest);
	  if (i + 1 != n_region)
	    {
	      curr_count = single_succ_edge (bb)->count ();
	    }
	  continue;
	}

      /* Special case the last block on the path: make sure that it does not
	 jump back on the copied path, including back to itself.  */
      if (i + 1 == n_region)
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (bb_in_bbs (e->dest, region_copy, n_region))
	      {
		basic_block orig = get_bb_original (e->dest);
		if (orig)
		  redirect_edge_and_branch_force (e, orig);
	      }
	  continue;
	}

      /* Redirect all other edges jumping to non-adjacent blocks back to the
	 original code.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (region_copy[i + 1] != e->dest)
	  {
	    basic_block orig = get_bb_original (e->dest);
	    if (orig)
	      redirect_edge_and_branch_force (e, orig);
	  }
	else
	  {
	    curr_count = e->count ();
	  }
    }


  if (flag_checking)
    verify_jump_thread (region_copy, n_region);

  /* Remove the last branch in the jump thread path.  */
  remove_ctrl_stmt_and_useless_edges (region_copy[n_region - 1], exit->dest);

  /* And fixup the flags on the single remaining edge.  */
  edge fix_e = find_edge (region_copy[n_region - 1], exit->dest);
  fix_e->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE | EDGE_ABNORMAL);
  fix_e->flags |= EDGE_FALLTHRU;

  edge e = make_edge (region_copy[n_region - 1], exit->dest, EDGE_FALLTHRU);

  if (e)
    {
      rescan_loop_exit (e, true, false);
      e->probability = profile_probability::always ();
    }

  /* Redirect the entry and add the phi node arguments.  */
  if (entry->dest == loop->header)
    mark_loop_for_removal (loop);
  redirected = redirect_edge_and_branch (entry, get_bb_copy (entry->dest));
  gcc_assert (redirected != NULL);
  flush_pending_stmts (entry);

  /* Add the other PHI node arguments.  */
  add_phi_args_after_copy (region_copy, n_region, NULL);

  free (region_copy);

  adjust_paths_after_duplication (current_path_no);

  free_original_copy_tables ();
  return true;
}

/* Return true when PATH is a valid jump-thread path.  */

static bool
valid_jump_thread_path (vec<jump_thread_edge *> *path)
{
  unsigned len = path->length ();

  /* Check that the path is connected.  */
  for (unsigned int j = 0; j < len - 1; j++)
    {
      edge e = (*path)[j]->e;
      if (e->dest != (*path)[j+1]->e->src)
	return false;
    }
  return true;
}

/* Remove any queued jump threads that include edge E.

   We don't actually remove them here, just record the edges into ax
   hash table.  That way we can do the search once per iteration of
   DOM/VRP rather than for every case where DOM optimizes away a COND_EXPR.  */

void
remove_jump_threads_including (edge_def *e)
{
  if (!paths.exists ())
    return;

  if (!removed_edges)
    removed_edges = new hash_table<struct removed_edges> (17);

  edge *slot = removed_edges->find_slot (e, INSERT);
  *slot = e;
}

/* Walk through all blocks and thread incoming edges to the appropriate
   outgoing edge for each edge pair recorded in THREADED_EDGES.

   It is the caller's responsibility to fix the dominance information
   and rewrite duplicated SSA_NAMEs back into SSA form.

   If MAY_PEEL_LOOP_HEADERS is false, we avoid threading edges through
   loop headers if it does not simplify the loop.

   Returns true if one or more edges were threaded, false otherwise.  */

bool
thread_through_all_blocks (bool may_peel_loop_headers)
{
  bool retval = false;
  unsigned int i;
  struct loop *loop;
  auto_bitmap threaded_blocks;
  hash_set<edge> visited_starting_edges;

  if (!paths.exists ())
    {
      retval = false;
      goto out;
    }

  memset (&thread_stats, 0, sizeof (thread_stats));

  /* Remove any paths that referenced removed edges.  */
  if (removed_edges)
    for (i = 0; i < paths.length (); )
      {
	unsigned int j;
	vec<jump_thread_edge *> *path = paths[i];

	for (j = 0; j < path->length (); j++)
	  {
	    edge e = (*path)[j]->e;
	    if (removed_edges->find_slot (e, NO_INSERT))
	      break;
	  }

	if (j != path->length ())
	  {
	    delete_jump_thread_path (path);
	    paths.unordered_remove (i);
	    continue;
	  }
	i++;
      }

  /* Jump-thread all FSM threads before other jump-threads.  */
  for (i = 0; i < paths.length ();)
    {
      vec<jump_thread_edge *> *path = paths[i];
      edge entry = (*path)[0]->e;

      /* Only code-generate FSM jump-threads in this loop.  */
      if ((*path)[0]->type != EDGE_FSM_THREAD)
	{
	  i++;
	  continue;
	}

      /* Do not jump-thread twice from the same starting edge.

	 Previously we only checked that we weren't threading twice
	 from the same BB, but that was too restrictive.  Imagine a
	 path that starts from GIMPLE_COND(x_123 == 0,...), where both
	 edges out of this conditional yield paths that can be
	 threaded (for example, both lead to an x_123==0 or x_123!=0
	 conditional further down the line.  */
      if (visited_starting_edges.contains (entry)
	  /* We may not want to realize this jump thread path for
	     various reasons.  So check it first.  */
	  || !valid_jump_thread_path (path))
	{
	  /* Remove invalid FSM jump-thread paths.  */
	  delete_jump_thread_path (path);
	  paths.unordered_remove (i);
	  continue;
	}

      unsigned len = path->length ();
      edge exit = (*path)[len - 1]->e;
      basic_block *region = XNEWVEC (basic_block, len - 1);

      for (unsigned int j = 0; j < len - 1; j++)
	region[j] = (*path)[j]->e->dest;

      if (duplicate_thread_path (entry, exit, region, len - 1, i))
	{
	  /* We do not update dominance info.  */
	  free_dominance_info (CDI_DOMINATORS);
	  visited_starting_edges.add (entry);
	  retval = true;
	  thread_stats.num_threaded_edges++;
	}

      delete_jump_thread_path (path);
      paths.unordered_remove (i);
      free (region);
    }

  /* Remove from PATHS all the jump-threads starting with an edge already
     jump-threaded.  */
  for (i = 0; i < paths.length ();)
    {
      vec<jump_thread_edge *> *path = paths[i];
      edge entry = (*path)[0]->e;

      /* Do not jump-thread twice from the same block.  */
      if (visited_starting_edges.contains (entry))
	{
	  delete_jump_thread_path (path);
	  paths.unordered_remove (i);
	}
      else
	i++;
    }

  mark_threaded_blocks (threaded_blocks);

  initialize_original_copy_tables ();

  /* The order in which we process jump threads can be important.

     Consider if we have two jump threading paths A and B.  If the
     target edge of A is the starting edge of B and we thread path A
     first, then we create an additional incoming edge into B->dest that
     we cannot discover as a jump threading path on this iteration.

     If we instead thread B first, then the edge into B->dest will have
     already been redirected before we process path A and path A will
     natually, with no further work, target the redirected path for B.

     An post-order is sufficient here.  Compute the ordering first, then
     process the blocks.  */
  if (!bitmap_empty_p (threaded_blocks))
    {
      int *postorder = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
      unsigned int postorder_num = post_order_compute (postorder, false, false);
      for (unsigned int i = 0; i < postorder_num; i++)
	{
	  unsigned int indx = postorder[i];
	  if (bitmap_bit_p (threaded_blocks, indx))
	    {
	      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, indx);
	      retval |= thread_block (bb, true);
	    }
	}
      free (postorder);
    }

  /* Then perform the threading through loop headers.  We start with the
     innermost loop, so that the changes in cfg we perform won't affect
     further threading.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      if (!loop->header
	  || !bitmap_bit_p (threaded_blocks, loop->header->index))
	continue;

      retval |= thread_through_loop_header (loop, may_peel_loop_headers);
    }

  /* All jump threading paths should have been resolved at this
     point.  Verify that is the case.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, bb->preds)
	gcc_assert (e->aux == NULL);
    }

  statistics_counter_event (cfun, "Jumps threaded",
			    thread_stats.num_threaded_edges);

  free_original_copy_tables ();

  paths.release ();

  if (retval)
    loops_state_set (LOOPS_NEED_FIXUP);

 out:
  delete removed_edges;
  removed_edges = NULL;
  return retval;
}

/* Delete the jump threading path PATH.  We have to explicitly delete
   each entry in the vector, then the container.  */

void
delete_jump_thread_path (vec<jump_thread_edge *> *path)
{
  for (unsigned int i = 0; i < path->length (); i++)
    delete (*path)[i];
  path->release();
  delete path;
}

/* Register a jump threading opportunity.  We queue up all the jump
   threading opportunities discovered by a pass and update the CFG
   and SSA form all at once.

   E is the edge we can thread, E2 is the new target edge, i.e., we
   are effectively recording that E->dest can be changed to E2->dest
   after fixing the SSA graph.  */

void
register_jump_thread (vec<jump_thread_edge *> *path)
{
  if (!dbg_cnt (registered_jump_thread))
    {
      delete_jump_thread_path (path);
      return;
    }

  /* First make sure there are no NULL outgoing edges on the jump threading
     path.  That can happen for jumping to a constant address.  */
  for (unsigned int i = 0; i < path->length (); i++)
    {
      if ((*path)[i]->e == NULL)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "Found NULL edge in jump threading path.  Cancelling jump thread:\n");
	      dump_jump_thread_path (dump_file, *path, false);
	    }

	  delete_jump_thread_path (path);
	  return;
	}

      /* Only the FSM threader is allowed to thread across
	 backedges in the CFG.  */
      if (flag_checking
	  && (*path)[0]->type != EDGE_FSM_THREAD)
	gcc_assert (((*path)[i]->e->flags & EDGE_DFS_BACK) == 0);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_jump_thread_path (dump_file, *path, true);

  if (!paths.exists ())
    paths.create (5);

  paths.safe_push (path);
}

/* Return how many uses of T there are within BB, as long as there
   aren't any uses outside BB.  If there are any uses outside BB,
   return -1 if there's at most one use within BB, or -2 if there is
   more than one use within BB.  */

static int
uses_in_bb (tree t, basic_block bb)
{
  int uses = 0;
  bool outside_bb = false;

  imm_use_iterator iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, iter, t)
    {
      if (is_gimple_debug (USE_STMT (use_p)))
	continue;

      if (gimple_bb (USE_STMT (use_p)) != bb)
	outside_bb = true;
      else
	uses++;

      if (outside_bb && uses > 1)
	return -2;
    }

  if (outside_bb)
    return -1;

  return uses;
}

/* Starting from the final control flow stmt in BB, assuming it will
   be removed, follow uses in to-be-removed stmts back to their defs
   and count how many defs are to become dead and be removed as
   well.  */

unsigned int
estimate_threading_killed_stmts (basic_block bb)
{
  int killed_stmts = 0;
  hash_map<tree, int> ssa_remaining_uses;
  auto_vec<gimple *, 4> dead_worklist;

  /* If the block has only two predecessors, threading will turn phi
     dsts into either src, so count them as dead stmts.  */
  bool drop_all_phis = EDGE_COUNT (bb->preds) == 2;

  if (drop_all_phis)
    for (gphi_iterator gsi = gsi_start_phis (bb);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gphi *phi = gsi.phi ();
	tree dst = gimple_phi_result (phi);

	/* We don't count virtual PHIs as stmts in
	   record_temporary_equivalences_from_phis.  */
	if (virtual_operand_p (dst))
	  continue;

	killed_stmts++;
      }

  if (gsi_end_p (gsi_last_bb (bb)))
    return killed_stmts;

  gimple *stmt = gsi_stmt (gsi_last_bb (bb));
  if (gimple_code (stmt) != GIMPLE_COND
      && gimple_code (stmt) != GIMPLE_GOTO
      && gimple_code (stmt) != GIMPLE_SWITCH)
    return killed_stmts;

  /* The control statement is always dead.  */
  killed_stmts++;
  dead_worklist.quick_push (stmt);
  while (!dead_worklist.is_empty ())
    {
      stmt = dead_worklist.pop ();

      ssa_op_iter iter;
      use_operand_p use_p;
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	{
	  tree t = USE_FROM_PTR (use_p);
	  gimple *def = SSA_NAME_DEF_STMT (t);

	  if (gimple_bb (def) == bb
	      && (gimple_code (def) != GIMPLE_PHI
		  || !drop_all_phis)
	      && !gimple_has_side_effects (def))
	    {
	      int *usesp = ssa_remaining_uses.get (t);
	      int uses;

	      if (usesp)
		uses = *usesp;
	      else
		uses = uses_in_bb (t, bb);

	      gcc_assert (uses);

	      /* Don't bother recording the expected use count if we
		 won't find any further uses within BB.  */
	      if (!usesp && (uses < -1 || uses > 1))
		{
		  usesp = &ssa_remaining_uses.get_or_insert (t);
		  *usesp = uses;
		}

	      if (uses < 0)
		continue;

	      --uses;
	      if (usesp)
		*usesp = uses;

	      if (!uses)
		{
		  killed_stmts++;
		  if (usesp)
		    ssa_remaining_uses.remove (t);
		  if (gimple_code (def) != GIMPLE_PHI)
		    dead_worklist.safe_push (def);
		}
	    }
	}
    }

  if (dump_file)
    fprintf (dump_file, "threading bb %i kills %i stmts\n",
	     bb->index, killed_stmts);

  return killed_stmts;
}
