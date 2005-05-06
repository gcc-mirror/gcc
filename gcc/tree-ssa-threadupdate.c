/* Thread edges through blocks and update the control flow and SSA graphs.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"

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
   the set of unique destination blocks that the incoming edges should
   be threaded to.  Block duplication can be further minimized by using
   B instead of creating B' for one destination if all edges into B are
   going to be threaded to a successor of B.

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

  /* A list of incoming edges which we want to thread to
     OUTGOING_EDGE->dest.  */
  struct el *incoming_edges;

  /* Flag indicating whether or not we should create a duplicate block
     for this thread destination.  This is only true if we are threading
     all incoming edges and thus are using BB itself as a duplicate block.  */
  bool do_not_duplicate;
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
};

/* Remove the last statement in block BB if it is a control statement
   Also remove all outgoing edges except the edge which reaches DEST_BB.
   If DEST_BB is NULL, then remove all outgoing edges.  */

static void
remove_ctrl_stmt_and_useless_edges (basic_block bb, basic_block dest_bb)
{
  block_stmt_iterator bsi;
  edge e;
  edge_iterator ei;

  bsi = bsi_last (bb);

  /* If the duplicate ends with a control statement, then remove it.

     Note that if we are duplicating the template block rather than the
     original basic block, then the duplicate might not have any real
     statements in it.  */
  if (!bsi_end_p (bsi)
      && bsi_stmt (bsi)
      && (TREE_CODE (bsi_stmt (bsi)) == COND_EXPR
	  || TREE_CODE (bsi_stmt (bsi)) == SWITCH_EXPR))
    bsi_remove (&bsi);

  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
    {
      if (e->dest != dest_bb)
	remove_edge (e);
      else
	ei_next (&ei);
    }
}

/* Create a duplicate of BB which only reaches the destination of the edge
   stored in RD.  Record the duplicate block in RD.  */

static void
create_block_for_threading (basic_block bb, struct redirection_data *rd)
{
  /* We can use the generic block duplication code and simply remove
     the stuff we do not need.  */
  rd->dup_block = duplicate_block (bb, NULL);

  /* Zero out the profile, since the block is unreachable for now.  */
  rd->dup_block->frequency = 0;
  rd->dup_block->count = 0;

  /* The call to duplicate_block will copy everything, including the
     useless COND_EXPR or SWITCH_EXPR at the end of BB.  We just remove
     the useless COND_EXPR or SWITCH_EXPR here rather than having a
     specialized block copier.  We also remove all outgoing edges
     from the duplicate block.  The appropriate edge will be created
     later.  */
  remove_ctrl_stmt_and_useless_edges (rd->dup_block, NULL);
}

/* Hashing and equality routines for our hash table.  */
static hashval_t
redirection_data_hash (const void *p)
{
  edge e = ((struct redirection_data *)p)->outgoing_edge;
  return e->dest->index;
}

static int
redirection_data_eq (const void *p1, const void *p2)
{
  edge e1 = ((struct redirection_data *)p1)->outgoing_edge;
  edge e2 = ((struct redirection_data *)p2)->outgoing_edge;

  return e1 == e2;
}

/* Given an outgoing edge E lookup and return its entry in our hash table.

   If INSERT is true, then we insert the entry into the hash table if
   it is not already present.  INCOMING_EDGE is added to the list of incoming
   edges associated with E in the hash table.  */

static struct redirection_data *
lookup_redirection_data (edge e, edge incoming_edge, bool insert)
{
  void **slot;
  struct redirection_data *elt;

 /* Build a hash table element so we can see if E is already
     in the table.  */
  elt = xmalloc (sizeof (struct redirection_data));
  elt->outgoing_edge = e;
  elt->dup_block = NULL;
  elt->do_not_duplicate = false;
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
      elt->incoming_edges = xmalloc (sizeof (struct el));
      elt->incoming_edges->e = incoming_edge;
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
          struct el *el = xmalloc (sizeof (struct el));
	  el->next = elt->incoming_edges;
	  el->e = incoming_edge;
	  elt->incoming_edges = el;
	}

      return elt;
    }
}

/* Given a duplicate block and its single destination (both stored
   in RD).  Create an edge between the duplicate and its single
   destination.

   Add an additional argument to any PHI nodes at the single
   destination.  */

static void
create_edge_and_update_destination_phis (struct redirection_data *rd)
{
  edge e = make_edge (rd->dup_block, rd->outgoing_edge->dest, EDGE_FALLTHRU);
  tree phi;

  /* If there are any PHI nodes at the destination of the outgoing edge
     from the duplicate block, then we will need to add a new argument
     to them.  The argument should have the same value as the argument
     associated with the outgoing edge stored in RD.  */
  for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
    {
      int indx = rd->outgoing_edge->dest_idx;
      add_phi_arg (phi, PHI_ARG_DEF_TREE (phi, indx), e);
    }
}

/* Hash table traversal callback routine to create duplicate blocks.  */

static int
create_duplicates (void **slot, void *data)
{
  struct redirection_data *rd = (struct redirection_data *) *slot;
  struct local_info *local_info = (struct local_info *)data;

  /* If this entry should not have a duplicate created, then there's
     nothing to do.  */
  if (rd->do_not_duplicate)
    return 1;

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
         block.  */
      create_edge_and_update_destination_phis (rd);
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

  /* If this is the template block, then create its outgoing edges
     and halt the hash table traversal.  */
  if (rd->dup_block && rd->dup_block == local_info->template_block)
    {
      create_edge_and_update_destination_phis (rd);
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

      /* Go ahead and clear E->aux.  It's not needed anymore and failure
         to clear it will cause all kinds of unpleasant problems later.  */
      e->aux = NULL;

      if (rd->dup_block)
	{
	  edge e2;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
		     e->src->index, e->dest->index, rd->dup_block->index);

	  /* Redirect the incoming edge to the appropriate duplicate
	     block.  */
	  e2 = redirect_edge_and_branch (e, rd->dup_block);
	  flush_pending_stmts (e2);

	  if ((dump_file && (dump_flags & TDF_DETAILS))
	      && e->src != e2->src)
	    fprintf (dump_file, "    basic block %d created\n", e2->src->index);
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
		     e->src->index, e->dest->index, local_info->bb->index);

	  /* We are using BB as the duplicate.  Remove the unnecessary
	     outgoing edges and statements from BB.  */
	  remove_ctrl_stmt_and_useless_edges (local_info->bb,
					      rd->outgoing_edge->dest);

	  /* And fixup the flags on the single remaining edge.  */
	  EDGE_SUCC (local_info->bb, 0)->flags
	    &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
	  EDGE_SUCC (local_info->bb, 0)->flags |= EDGE_FALLTHRU;
	}
    }
  return 1;
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

   BB and its duplicates will have assignments to the same set of
   SSA_NAMEs.  Right now, we just call into rewrite_ssa_into_ssa
   to update the SSA graph for those names.

   We are also going to experiment with a true incremental update
   scheme for the duplicated resources.  One of the interesting
   properties we can exploit here is that all the resources set
   in BB will have the same IDFS, so we have one IDFS computation
   per block with incoming threaded edges, which can lower the
   cost of the true incremental update algorithm.  */

static void
thread_block (basic_block bb)
{
  /* E is an incoming edge into BB that we may or may not want to
     redirect to a duplicate of BB.  */
  edge e;
  edge_iterator ei;
  struct local_info local_info;

  /* ALL indicates whether or not all incoming edges into BB should
     be threaded to a duplicate of BB.  */
  bool all = true;

  /* To avoid scanning a linear array for the element we need we instead
     use a hash table.  For normal code there should be no noticeable
     difference.  However, if we have a block with a large number of
     incoming and outgoing edges such linear searches can get expensive.  */
  redirection_data = htab_create (EDGE_COUNT (bb->succs),
				  redirection_data_hash,
				  redirection_data_eq,
				  free);

  /* Record each unique threaded destination into a hash table for
     efficient lookups.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (!e->aux)
	{
	  all = false;
	}
      else
	{
	  edge e2 = e->aux;

	  /* Insert the outgoing edge into the hash table if it is not
	     already in the hash table.  */
	  lookup_redirection_data (e2, e, true);
	}
    }

  /* If we are going to thread all incoming edges to an outgoing edge, then
     BB will become unreachable.  Rather than just throwing it away, use
     it for one of the duplicates.  Mark the first incoming edge with the
     DO_NOT_DUPLICATE attribute.  */
  if (all)
    {
      edge e = EDGE_PRED (bb, 0)->aux;
      lookup_redirection_data (e, NULL, false)->do_not_duplicate = true;
    }

  /* Now create duplicates of BB.

     Note that for a block with a high outgoing degree we can waste
     a lot of time and memory creating and destroying useless edges.

     So we first duplicate BB and remove the control structure at the
     tail of the duplicate as well as all outgoing edges from the
     duplicate.  We then use that duplicate block as a template for
     the rest of the duplicates.  */
  local_info.template_block = NULL;
  local_info.bb = bb;
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
}

/* Walk through all blocks and thread incoming edges to the block's
   destinations as requested.  This is the only entry point into this
   file.

   Blocks which have one or more incoming edges have INCOMING_EDGE_THREADED
   set in the block's annotation.

   Each edge that should be threaded has the new destination edge stored in
   the original edge's AUX field.

   This routine (or one of its callees) will clear INCOMING_EDGE_THREADED
   in the block annotations and the AUX field in the edges.

   It is the caller's responsibility to fix the dominance information
   and rewrite duplicated SSA_NAMEs back into SSA form.

   Returns true if one or more edges were threaded, false otherwise.  */

bool
thread_through_all_blocks (void)
{
  basic_block bb;
  bool retval = false;

  FOR_EACH_BB (bb)
    {
      if (bb_ann (bb)->incoming_edge_threaded
	  && EDGE_COUNT (bb->preds) > 0)
	{
	  thread_block (bb);
	  retval = true;
	  bb_ann (bb)->incoming_edge_threaded = false;
	}
    }
  return retval;
}
