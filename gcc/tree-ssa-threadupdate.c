/* Thread edges through blocks and update the control flow and SSA graphs.
   Copyright (C) 2004 Free Software Foundation, Inc.

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
   going to be threaded to a successor of B.  */


/* Main data structure recording information regarding B's duplicate
   blocks.  */

struct redirection_data
{
  /* A duplicate of B with the trailing control statement removed and which
     targets a single successor of B.  */
  basic_block dup_block;

  /* An outgoing edge from B.  DUP_BLOCK will have OUTGOING_EDGE->dest as
     its single successor.  */
  edge outgoing_edge;
};

/* Main data structure to hold information for duplicates of BB.  */
static varray_type redirection_data;

/* For each PHI node in BB, find or create a PHI node in NEW_BB for the
   same PHI_RESULT.  Add an argument to the PHI node in NEW_BB which
   corresponds to the same PHI argument associated with edge E in BB.  */

static void
copy_phis_to_block (basic_block new_bb, basic_block bb, edge e)
{
  tree phi, arg;

  /* Walk over every PHI in BB.  */
  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree new_phi;

      /* First try to find a PHI node in NEW_BB which has the same
         PHI_RESULT as the PHI from BB we are currently processing.  */
      for (new_phi = phi_nodes (new_bb); new_phi;
	   new_phi = PHI_CHAIN (new_phi))
	if (PHI_RESULT (new_phi) == PHI_RESULT (phi))
	  break;

      /* If we did not find a suitable PHI in NEW_BB, create one.  */
      if (!new_phi)
	new_phi = create_phi_node (PHI_RESULT (phi), new_bb);

      /* Extract the argument corresponding to E from the current PHI
         node in BB.  */
      arg = PHI_ARG_DEF_TREE (phi, phi_arg_from_edge (phi, e));

      /* Now add that same argument to the new PHI node in block NEW_BB.  */
      add_phi_arg (&new_phi, arg, e);
    }
}

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
	ssa_remove_edge (e);
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
  basic_block template_block;

  /* ALL indicates whether or not all incoming edges into BB should
     be threaded to a duplicate of BB.  */
  bool all = true;

  unsigned int i;

  VARRAY_GENERIC_PTR_INIT (redirection_data, 2, "redirection data");

  /* Look at each incoming edge into BB.  Record each unique outgoing
     edge that we want to thread an incoming edge to.  Also note if
     all incoming edges are threaded or not.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (!e->aux)
	{
	  all = false;
	}
      else
	{
	  unsigned int i;

	  /* See if we can find an entry for the destination of this
	     threaded edge that has already been recorded.  */
	  for (i = 0; i < VARRAY_ACTIVE_SIZE (redirection_data); i++)
	    {
	      struct redirection_data *rd;
	      edge e2;

	      rd = VARRAY_GENERIC_PTR (redirection_data, i);
	      e2 = e->aux;

	      if (e2->dest == rd->outgoing_edge->dest)
		break;
	    }

	  /* If the loop did not terminate early, then we have a new
	     destination for the incoming threaded edges.  Record it.  */
	  if (i == VARRAY_ACTIVE_SIZE (redirection_data))
	    {
	      struct redirection_data *rd;

	      rd = ggc_alloc_cleared (sizeof (struct redirection_data));
	      rd->outgoing_edge = e->aux;
	      VARRAY_PUSH_GENERIC_PTR (redirection_data, rd);
	    }
	}
    }

  /* Now create duplicates of BB.  Note that if all incoming edges are
     threaded, then BB is going to become unreachable.  In that case
     we use BB for one of the duplicates rather than wasting memory
     duplicating BB.  Thus the odd starting condition for the loop.

     Note that for a block with a high outgoing degree we can waste
     a lot of time and memory creating and destroying useless edges.

     So we first duplicate BB and remove the control structure at the
     tail of the duplicate as well as all outgoing edges from the
     duplicate.  We then use that duplicate block as a template for
     the rest of the duplicates.  */
  template_block = NULL;
  for (i = (all ? 1 : 0); i < VARRAY_ACTIVE_SIZE (redirection_data); i++)
    {
      struct redirection_data *rd = VARRAY_GENERIC_PTR (redirection_data, i);

      if (template_block == NULL)
	{
	  create_block_for_threading (bb, rd);
	  template_block = rd->dup_block;
	}
      else
	{
	  create_block_for_threading (template_block, rd);
	}
    }

  /* Now created up edges from the duplicate blocks to their new
     destinations.  Doing this as a separate loop after block creation
     allows us to avoid creating lots of useless edges.  */
  for (i = (all ? 1 : 0); i < VARRAY_ACTIVE_SIZE (redirection_data); i++)
    {
      struct redirection_data *rd = VARRAY_GENERIC_PTR (redirection_data, i);
      tree phi;
      edge e;

      e = make_edge (rd->dup_block, rd->outgoing_edge->dest, EDGE_FALLTHRU);

      /* If there are any PHI nodes at the destination of the outgoing edge
	 from the duplicate block, then we will need to add a new argument
	 to them.  The argument should have the same value as the argument
	 associated with the outgoing edge stored in RD.  */
      for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
	{
	  int indx = phi_arg_from_edge (phi, rd->outgoing_edge);
	  add_phi_arg (&phi, PHI_ARG_DEF_TREE (phi, indx), e);
	}
    }

  /* The loop above created the duplicate blocks (and the statements
     within the duplicate blocks).  This loop creates PHI nodes for the
     duplicated blocks and redirects the incoming edges into BB to reach
     the duplicates of BB.

     Note that redirecting the edge will change e->pred_next, so we have
     to hold e->pred_next in a temporary. 

     If this turns out to be a performance problem, then we could create
     a list of incoming edges associated with each entry in 
     REDIRECTION_DATA and walk over that list of edges instead.  */
  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
    {
      edge new_dest = e->aux;

      /* E was not threaded, then there is nothing to do.  */
      if (!new_dest)
	{
	  ei_next (&ei);
	  continue;
	}

      /* Go ahead and clear E->aux.  It's not needed anymore and failure
         to clear it will cause all kinds of unpleasant problems later.  */
      e->aux = NULL;

      /* We know E is an edge we want to thread.  Find the entry associated
         with E's new destination in the REDIRECTION_DATA array.  */
      for (i = 0; i < VARRAY_ACTIVE_SIZE (redirection_data); i++)
	{
	  struct redirection_data *rd;

	  rd = VARRAY_GENERIC_PTR (redirection_data, i);

	  /* We have found the right entry if the outgoing edge in this
	     entry matches E's new destination.  Note that if we have not
	     created a duplicate block (rd->dup_block is NULL), then we
	     are going to re-use BB as a duplicate and we do not need
	     to create PHI nodes or redirect the edge.  */
	  if (rd->outgoing_edge == new_dest && rd->dup_block)
	    {
	      edge e2;
	      copy_phis_to_block (rd->dup_block, bb, e);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
			 e->src->index, e->dest->index, rd->dup_block->index);

	      e2 = redirect_edge_and_branch (e, rd->dup_block);
	      PENDING_STMT (e2) = NULL;

	      if ((dump_file && (dump_flags & TDF_DETAILS))
		  && e->src != e2->src)
	      fprintf (dump_file, "    basic block %d created\n",
		       e2->src->index);
	      break;
	    }
	}
    }

  /* If all the incoming edges where threaded, then we used BB as one
     of the duplicate blocks.  We need to fixup BB in that case so that
     it no longer has a COND_EXPR or SWITCH_EXPR and reaches one destination
     unconditionally.  */
  if (all)
    {
      struct redirection_data *rd;

      rd = VARRAY_GENERIC_PTR (redirection_data, 0);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
		 EDGE_PRED (bb, 0)->src->index, bb->index,
		 EDGE_SUCC (bb, 0)->dest->index);

      remove_ctrl_stmt_and_useless_edges (bb, rd->outgoing_edge->dest);
      EDGE_SUCC (bb, 0)->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      EDGE_SUCC (bb, 0)->flags |= EDGE_FALLTHRU;
    }

  /* Done with this block.  Clear REDIRECTION_DATA.  */
  VARRAY_CLEAR (redirection_data);
}

/* Walk through all blocks and thread incoming edges to the block's 
   destinations as requested.  This is the only entry point into this
   file.

   Blocks which have one or more incoming edges have INCOMING_EDGE_THREADED
   set in the block's annotation.
   this routine.

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
      if (bb_ann (bb)->incoming_edge_threaded)
	{
	  thread_block (bb);
	  retval = true;
	  bb_ann (bb)->incoming_edge_threaded = false;
	}
    }
  return retval;
}
