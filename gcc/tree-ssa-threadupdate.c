/* Thread edges through blocks and update the control flow and SSA graphs.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2010, 201
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "function.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "cfgloop.h"

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

   Block duplication can be further minimized by using B instead of 
   creating B' for one destination if all edges into B are going to be
   threaded to a successor of B.  We had code to do this at one time, but
   I'm not convinced it is correct with the changes to avoid mucking up
   the loop structure (which may cancel threading requests, thus a block
   which we thought was going to become unreachable may still be reachable).
   This code was also going to get ugly with the introduction of the ability
   for a single jump thread request to bypass multiple blocks. 

   We further reduce the number of edges and statements we create by
   not copying all the outgoing edges and the control statement in
   step #1.  We instead create a template block without the outgoing
   edges and duplicate the template.  */


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

struct redirection_data
{
  /* A duplicate of B with the trailing control statement removed and which
     targets a single successor of B.  */
  basic_block dup_block;

  /* An outgoing edge from B.  DUP_BLOCK will have OUTGOING_EDGE->dest as
     its single successor.  */
  edge outgoing_edge;

  edge intermediate_edge;

  /* A list of incoming edges which we want to thread to
     OUTGOING_EDGE->dest.  */
  struct el *incoming_edges;
};

/* Main data structure to hold information for duplicates of BB.  */
static htab_t redirection_data;

/* Data structure of information to pass to hash table traversal routines.  */
struct local_info
{
  /* The current block we are working on.  */
  basic_block bb;

  /* A template copy of BB with no outgoing edges or control statement that
     we use for creating copies.  */
  basic_block template_block;

  /* TRUE if we thread one or more jumps, FALSE otherwise.  */
  bool jumps_threaded;
};

/* Passes which use the jump threading code register jump threading
   opportunities as they are discovered.  We keep the registered
   jump threading opportunities in this vector as edge pairs
   (original_edge, target_edge).  */
static VEC(edge,heap) *threaded_edges;

/* When we start updating the CFG for threading, data necessary for jump
   threading is attached to the AUX field for the incoming edge.  Use these
   macros to access the underlying structure attached to the AUX field.  */
#define THREAD_TARGET(E) ((edge *)(E)->aux)[0]
#define THREAD_TARGET2(E) ((edge *)(E)->aux)[1]

/* Jump threading statistics.  */

struct thread_stats_d
{
  unsigned long num_threaded_edges;
};

struct thread_stats_d thread_stats;


/* Remove the last statement in block BB if it is a control statement
   Also remove all outgoing edges except the edge which reaches DEST_BB.
   If DEST_BB is NULL, then remove all outgoing edges.  */

static void
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
	remove_edge (e);
      else
	ei_next (&ei);
    }
}

/* Create a duplicate of BB.  Record the duplicate block in RD.  */

static void
create_block_for_threading (basic_block bb, struct redirection_data *rd)
{
  edge_iterator ei;
  edge e;

  /* We can use the generic block duplication code and simply remove
     the stuff we do not need.  */
  rd->dup_block = duplicate_block (bb, NULL, NULL);

  FOR_EACH_EDGE (e, ei, rd->dup_block->succs)
    e->aux = NULL;

  /* Zero out the profile, since the block is unreachable for now.  */
  rd->dup_block->frequency = 0;
  rd->dup_block->count = 0;
}

/* Hashing and equality routines for our hash table.  */
static hashval_t
redirection_data_hash (const void *p)
{
  edge e = ((const struct redirection_data *)p)->outgoing_edge;
  return e->dest->index;
}

static int
redirection_data_eq (const void *p1, const void *p2)
{
  edge e1 = ((const struct redirection_data *)p1)->outgoing_edge;
  edge e2 = ((const struct redirection_data *)p2)->outgoing_edge;
  edge e3 = ((const struct redirection_data *)p1)->intermediate_edge;
  edge e4 = ((const struct redirection_data *)p2)->intermediate_edge;

  return e1 == e2 && e3 == e4;
}

/* Given an outgoing edge E lookup and return its entry in our hash table.

   If INSERT is true, then we insert the entry into the hash table if
   it is not already present.  INCOMING_EDGE is added to the list of incoming
   edges associated with E in the hash table.  */

static struct redirection_data *
lookup_redirection_data (edge e, enum insert_option insert)
{
  void **slot;
  struct redirection_data *elt;

 /* Build a hash table element so we can see if E is already
     in the table.  */
  elt = XNEW (struct redirection_data);
  elt->intermediate_edge = THREAD_TARGET2 (e) ? THREAD_TARGET (e) : NULL;
  elt->outgoing_edge = THREAD_TARGET2 (e) ? THREAD_TARGET2 (e) 
					  : THREAD_TARGET (e);
  elt->dup_block = NULL;
  elt->incoming_edges = NULL;

  slot = htab_find_slot (redirection_data, elt, insert);

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
      *slot = (void *)elt;
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
      elt = (struct redirection_data *) *slot;

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

/* For each PHI in BB, copy the argument associated with SRC_E to TGT_E.  */

static void
copy_phi_args (basic_block bb, edge src_e, edge tgt_e)
{
  gimple_stmt_iterator gsi;
  int src_indx = src_e->dest_idx;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      source_location locus = gimple_phi_arg_location (phi, src_indx);
      add_phi_arg (phi, gimple_phi_arg_def (phi, src_indx), tgt_e, locus);
    }
}

/* We have recently made a copy of ORIG_BB, including its outgoing
   edges.  The copy is NEW_BB.  Every PHI node in every direct successor of
   ORIG_BB has a new argument associated with edge from NEW_BB to the
   successor.  Initialize the PHI argument so that it is equal to the PHI
   argument associated with the edge from ORIG_BB to the successor.  */

static void
update_destination_phis (basic_block orig_bb, basic_block new_bb)
{
  edge_iterator ei;
  edge e;

  FOR_EACH_EDGE (e, ei, orig_bb->succs)
    {
      edge e2 = find_edge (new_bb, e->dest);
      copy_phi_args (e->dest, e, e2);
    }
}

/* Given a duplicate block and its single destination (both stored
   in RD).  Create an edge between the duplicate and its single
   destination.

   Add an additional argument to any PHI nodes at the single
   destination.  */

static void
create_edge_and_update_destination_phis (struct redirection_data *rd,
					 basic_block bb)
{
  edge e = make_edge (bb, rd->outgoing_edge->dest, EDGE_FALLTHRU);

  rescan_loop_exit (e, true, false);
  e->probability = REG_BR_PROB_BASE;
  e->count = bb->count;

  if (rd->outgoing_edge->aux)
    {
      e->aux = (edge *) XNEWVEC (edge, 2);
      THREAD_TARGET(e) = THREAD_TARGET (rd->outgoing_edge);
      THREAD_TARGET2(e) = THREAD_TARGET2 (rd->outgoing_edge);
    }
  else
    {
      e->aux = NULL;
    }

  /* If there are any PHI nodes at the destination of the outgoing edge
     from the duplicate block, then we will need to add a new argument
     to them.  The argument should have the same value as the argument
     associated with the outgoing edge stored in RD.  */
  copy_phi_args (e->dest, rd->outgoing_edge, e);
}

/* Wire up the outgoing edges from the duplicate block and
   update any PHIs as needed.  */
static void
fix_duplicate_block_edges (struct redirection_data *rd,
			   struct local_info *local_info)
{
  /* If we were threading through an joiner block, then we want
     to keep its control statement and redirect an outgoing edge.
     Else we want to remove the control statement & edges, then create
     a new outgoing edge.  In both cases we may need to update PHIs.  */
  if (THREAD_TARGET2 (rd->incoming_edges->e))
    {
      edge victim;
      edge e2;
      edge e = rd->incoming_edges->e;

      /* This updates the PHIs at the destination of the duplicate
	 block.  */
      update_destination_phis (local_info->bb, rd->dup_block);

      /* Find the edge from the duplicate block to the block we're
	 threading through.  That's the edge we want to redirect.  */
      victim = find_edge (rd->dup_block, THREAD_TARGET (e)->dest);
      e2 = redirect_edge_and_branch (victim, THREAD_TARGET2 (e)->dest);

      /* If we redirected the edge, then we need to copy PHI arguments
	 at the target.  If the edge already existed (e2 != victim case),
	 then the PHIs in the target already have the correct arguments.  */
      if (e2 == victim)
	copy_phi_args (e2->dest, THREAD_TARGET2 (e), e2);
    }
  else
    {
      remove_ctrl_stmt_and_useless_edges (rd->dup_block, NULL);
      create_edge_and_update_destination_phis (rd, rd->dup_block);
    }
}
/* Hash table traversal callback routine to create duplicate blocks.  */

static int
create_duplicates (void **slot, void *data)
{
  struct redirection_data *rd = (struct redirection_data *) *slot;
  struct local_info *local_info = (struct local_info *)data;

  /* Create a template block if we have not done so already.  Otherwise
     use the template to create a new block.  */
  if (local_info->template_block == NULL)
    {
      create_block_for_threading (local_info->bb, rd);
      local_info->template_block = rd->dup_block;

      /* We do not create any outgoing edges for the template.  We will
	 take care of that in a later traversal.  That way we do not
	 create edges that are going to just be deleted.  */
    }
  else
    {
      create_block_for_threading (local_info->template_block, rd);

      /* Go ahead and wire up outgoing edges and update PHIs for the duplicate
	 block.   */
      fix_duplicate_block_edges (rd, local_info);
    }

  /* Keep walking the hash table.  */
  return 1;
}

/* We did not create any outgoing edges for the template block during
   block creation.  This hash table traversal callback creates the
   outgoing edge for the template block.  */

static int
fixup_template_block (void **slot, void *data)
{
  struct redirection_data *rd = (struct redirection_data *) *slot;
  struct local_info *local_info = (struct local_info *)data;

  /* If this is the template block halt the traversal after updating
     it appropriately.

     If we were threading through an joiner block, then we want
     to keep its control statement and redirect an outgoing edge.
     Else we want to remove the control statement & edges, then create
     a new outgoing edge.  In both cases we may need to update PHIs.  */
  if (rd->dup_block && rd->dup_block == local_info->template_block)
    {
      fix_duplicate_block_edges (rd, local_info);
      return 0;
    }

  return 1;
}

/* Hash table traversal callback to redirect each incoming edge
   associated with this hash table element to its new destination.  */

static int
redirect_edges (void **slot, void *data)
{
  struct redirection_data *rd = (struct redirection_data *) *slot;
  struct local_info *local_info = (struct local_info *)data;
  struct el *next, *el;

  /* Walk over all the incoming edges associated associated with this
     hash table entry.  */
  for (el = rd->incoming_edges; el; el = next)
    {
      edge e = el->e;

      /* Go ahead and free this element from the list.  Doing this now
	 avoids the need for another list walk when we destroy the hash
	 table.  */
      next = el->next;
      free (el);

      thread_stats.num_threaded_edges++;
      /* If we are threading through a joiner block, then we have to
	 find the edge we want to redirect and update some PHI nodes.  */
      if (THREAD_TARGET2 (e))
	{
	  edge e2;

	  /* We want to redirect the incoming edge to the joiner block (E)
	     to instead reach the duplicate of the joiner block.  */
	  e2 = redirect_edge_and_branch (e, rd->dup_block);
	  flush_pending_stmts (e2);
	}
      else if (rd->dup_block)
	{
	  edge e2;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
		     e->src->index, e->dest->index, rd->dup_block->index);

	  rd->dup_block->count += e->count;

	  /* Excessive jump threading may make frequencies large enough so
	     the computation overflows.  */
	  if (rd->dup_block->frequency < BB_FREQ_MAX * 2)
	    rd->dup_block->frequency += EDGE_FREQUENCY (e);
	  EDGE_SUCC (rd->dup_block, 0)->count += e->count;
	  /* Redirect the incoming edge to the appropriate duplicate
	     block.  */
	  e2 = redirect_edge_and_branch (e, rd->dup_block);
	  gcc_assert (e == e2);
	  flush_pending_stmts (e2);
	}

      /* Go ahead and clear E->aux.  It's not needed anymore and failure
         to clear it will cause all kinds of unpleasant problems later.  */
      free (e->aux);
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
             || gimple_nop_p (gsi_stmt (gsi))))
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
   does not affect the structure of the loops in a nontrivial way.  */

static bool
thread_block (basic_block bb, bool noloop_only)
{
  /* E is an incoming edge into BB that we may or may not want to
     redirect to a duplicate of BB.  */
  edge e, e2;
  edge_iterator ei;
  struct local_info local_info;
  struct loop *loop = bb->loop_father;

  /* To avoid scanning a linear array for the element we need we instead
     use a hash table.  For normal code there should be no noticeable
     difference.  However, if we have a block with a large number of
     incoming and outgoing edges such linear searches can get expensive.  */
  redirection_data = htab_create (EDGE_COUNT (bb->succs),
				  redirection_data_hash,
				  redirection_data_eq,
				  free);

  /* If we thread the latch of the loop to its exit, the loop ceases to
     exist.  Make sure we do not restrict ourselves in order to preserve
     this loop.  */
  if (loop->header == bb)
    {
      e = loop_latch_edge (loop);

      if (e->aux)
	e2 = THREAD_TARGET (e);
      else
	e2 = NULL;

      if (e2 && loop_exit_edge_p (loop, e2))
	{
	  loop->header = NULL;
	  loop->latch = NULL;
	}
    }

  /* Record each unique threaded destination into a hash table for
     efficient lookups.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (e->aux == NULL)
	continue;

      if (THREAD_TARGET2 (e))
	e2 = THREAD_TARGET2 (e);
      else
	e2 = THREAD_TARGET (e);

      if (!e2
	  /* If NOLOOP_ONLY is true, we only allow threading through the
	     header of a loop to exit edges.  */
	  || (noloop_only
	      && bb == bb->loop_father->header
	      && (!loop_exit_edge_p (bb->loop_father, e2)
		  || THREAD_TARGET2 (e))))
	continue;

      if (e->dest == e2->src)
	update_bb_profile_for_threading (e->dest, EDGE_FREQUENCY (e),
				         e->count, THREAD_TARGET (e));

      /* Insert the outgoing edge into the hash table if it is not
	 already in the hash table.  */
      lookup_redirection_data (e, INSERT);
    }

  /* We do not update dominance info.  */
  free_dominance_info (CDI_DOMINATORS);

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
  htab_traverse (redirection_data, create_duplicates, &local_info);

  /* The template does not have an outgoing edge.  Create that outgoing
     edge and update PHI nodes as the edge's target as necessary.

     We do this after creating all the duplicates to avoid creating
     unnecessary edges.  */
  htab_traverse (redirection_data, fixup_template_block, &local_info);

  /* The hash table traversals above created the duplicate blocks (and the
     statements within the duplicate blocks).  This loop creates PHI nodes for
     the duplicated blocks and redirects the incoming edges into BB to reach
     the duplicates of BB.  */
  htab_traverse (redirection_data, redirect_edges, &local_info);

  /* Done with this block.  Clear REDIRECTION_DATA.  */
  htab_delete (redirection_data);
  redirection_data = NULL;

  /* Indicate to our caller whether or not any jumps were threaded.  */
  return local_info.jumps_threaded;
}

/* Threads edge E through E->dest to the edge THREAD_TARGET (E).  Returns the
   copy of E->dest created during threading, or E->dest if it was not necessary
   to copy it (E is its single predecessor).  */

static basic_block
thread_single_edge (edge e)
{
  basic_block bb = e->dest;
  edge eto = THREAD_TARGET (e);
  struct redirection_data rd;

  free (e->aux);
  e->aux = NULL;

  thread_stats.num_threaded_edges++;

  if (single_pred_p (bb))
    {
      /* If BB has just a single predecessor, we should only remove the
	 control statements at its end, and successors except for ETO.  */
      remove_ctrl_stmt_and_useless_edges (bb, eto->dest);

      /* And fixup the flags on the single remaining edge.  */
      eto->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE | EDGE_ABNORMAL);
      eto->flags |= EDGE_FALLTHRU;

      return bb;
    }

  /* Otherwise, we need to create a copy.  */
  if (e->dest == eto->src)
    update_bb_profile_for_threading (bb, EDGE_FREQUENCY (e), e->count, eto);

  rd.outgoing_edge = eto;

  create_block_for_threading (bb, &rd);
  remove_ctrl_stmt_and_useless_edges (rd.dup_block, NULL);
  create_edge_and_update_destination_phis (&rd, rd.dup_block);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
	     e->src->index, e->dest->index, rd.dup_block->index);

  rd.dup_block->count = e->count;
  rd.dup_block->frequency = EDGE_FREQUENCY (e);
  single_succ_edge (rd.dup_block)->count = e->count;
  redirect_edge_and_branch (e, rd.dup_block);
  flush_pending_stmts (e);

  return rd.dup_block;
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
{
  /* BB does not dominate latch of the LOOP.  */
  DOMST_NONDOMINATING,
  /* The LOOP is broken (there is no path from the header to its latch.  */
  DOMST_LOOP_BROKEN,
  /* BB dominates the latch of the LOOP.  */
  DOMST_DOMINATING
};

static enum bb_dom_status
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

  if (latch->aux)
    {
      if (THREAD_TARGET2 (latch))
	goto fail;
      tgt_edge = THREAD_TARGET (latch);
      tgt_bb = tgt_edge->dest;
    }
  else if (!may_peel_loop_headers
	   && !redirection_block_p (loop->header))
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

	  if (THREAD_TARGET2 (e))
	    goto fail;
	  tgt_edge = THREAD_TARGET (e);
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
      loop->header = NULL;
      loop->latch = NULL;
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

  if (latch->aux)
    {
      /* First handle the case latch edge is redirected.  */
      loop->latch = thread_single_edge (latch);
      gcc_assert (single_succ (loop->latch) == tgt_bb);
      loop->header = tgt_bb;

      /* Thread the remaining edges through the former header.  */
      thread_block (header, false);
    }
  else
    {
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
    }

  return true;

fail:
  /* We failed to thread anything.  Cancel the requests.  */
  FOR_EACH_EDGE (e, ei, header->preds)
    {
      free (e->aux);
      e->aux = NULL;
    }
  return false;
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
  bitmap tmp = BITMAP_ALLOC (NULL);
  basic_block bb;
  edge e;
  edge_iterator ei;

  for (i = 0; i < VEC_length (edge, threaded_edges); i += 3)
    {
      edge e = VEC_index (edge, threaded_edges, i);
      edge *x = (edge *) XNEWVEC (edge, 2);

      e->aux = x;
      THREAD_TARGET (e) = VEC_index (edge, threaded_edges, i + 1);
      THREAD_TARGET2 (e) = VEC_index (edge, threaded_edges, i + 2);
      bitmap_set_bit (tmp, e->dest->index);
    }

  /* If optimizing for size, only thread through block if we don't have
     to duplicate it or it's an otherwise empty redirection block.  */
  if (optimize_function_for_size_p (cfun))
    {
      EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
	{
	  bb = BASIC_BLOCK (i);
	  if (EDGE_COUNT (bb->preds) > 1
	      && !redirection_block_p (bb))
	    {
	      FOR_EACH_EDGE (e, ei, bb->preds)
		{
		  free (e->aux);
		  e->aux = NULL;
		}
	    }
	  else
	    bitmap_set_bit (threaded_blocks, i);
	}
    }
  else
    bitmap_copy (threaded_blocks, tmp);

  BITMAP_FREE(tmp);
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
  bitmap_iterator bi;
  bitmap threaded_blocks;
  struct loop *loop;
  loop_iterator li;

  /* We must know about loops in order to preserve them.  */
  gcc_assert (current_loops != NULL);

  if (threaded_edges == NULL)
    return false;

  threaded_blocks = BITMAP_ALLOC (NULL);
  memset (&thread_stats, 0, sizeof (thread_stats));

  mark_threaded_blocks (threaded_blocks);

  initialize_original_copy_tables ();

  /* First perform the threading requests that do not affect
     loop structure.  */
  EXECUTE_IF_SET_IN_BITMAP (threaded_blocks, 0, i, bi)
    {
      basic_block bb = BASIC_BLOCK (i);

      if (EDGE_COUNT (bb->preds) > 0)
	retval |= thread_block (bb, true);
    }

  /* Then perform the threading through loop headers.  We start with the
     innermost loop, so that the changes in cfg we perform won't affect
     further threading.  */
  FOR_EACH_LOOP (li, loop, LI_FROM_INNERMOST)
    {
      if (!loop->header
	  || !bitmap_bit_p (threaded_blocks, loop->header->index))
	continue;

      retval |= thread_through_loop_header (loop, may_peel_loop_headers);
    }

  statistics_counter_event (cfun, "Jumps threaded",
			    thread_stats.num_threaded_edges);

  free_original_copy_tables ();

  BITMAP_FREE (threaded_blocks);
  threaded_blocks = NULL;
  VEC_free (edge, heap, threaded_edges);
  threaded_edges = NULL;

  if (retval)
    loops_state_set (LOOPS_NEED_FIXUP);

  return retval;
}

/* Register a jump threading opportunity.  We queue up all the jump
   threading opportunities discovered by a pass and update the CFG
   and SSA form all at once.

   E is the edge we can thread, E2 is the new target edge, i.e., we
   are effectively recording that E->dest can be changed to E2->dest
   after fixing the SSA graph.  */

void
register_jump_thread (edge e, edge e2, edge e3)
{
  /* This can occur if we're jumping to a constant address or
     or something similar.  Just get out now.  */
  if (e2 == NULL)
    return;

  if (threaded_edges == NULL)
    threaded_edges = VEC_alloc (edge, heap, 15);

  if (dump_file && (dump_flags & TDF_DETAILS)
      && e->dest != e2->src)
    fprintf (dump_file,
	     "  Registering jump thread around one or more intermediate blocks\n");

  VEC_safe_push (edge, heap, threaded_edges, e);
  VEC_safe_push (edge, heap, threaded_edges, e2);
  VEC_safe_push (edge, heap, threaded_edges, e3);
}
