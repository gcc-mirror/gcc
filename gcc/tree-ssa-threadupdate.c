/* Thread edges through blocks and update the control flow and SSA graphs.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

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
#include "tree.h"
#include "flags.h"
#include "basic-block.h"
#include "function.h"
#include "hash-table.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "tree-ssa.h"
#include "tree-ssa-threadupdate.h"
#include "ssa-iterators.h"
#include "dumpfile.h"
#include "cfgloop.h"
#include "dbgcnt.h"
#include "tree-cfg.h"
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

struct redirection_data : typed_free_remove<redirection_data>
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
  typedef redirection_data value_type;
  typedef redirection_data compare_type;
  static inline hashval_t hash (const value_type *);
  static inline int equal (const value_type *, const compare_type *);
};

/* Simple hashing function.  For any given incoming edge E, we're going
   to be most concerned with the final destination of its jump thread
   path.  So hash on the block index of the final edge in the path.  */

inline hashval_t
redirection_data::hash (const value_type *p)
{
  vec<jump_thread_edge *> *path = p->path;
  return path->last ()->e->dest->index;
}

/* Given two hash table entries, return true if they have the same
   jump threading path.  */
inline int
redirection_data::equal (const value_type *p1, const compare_type *p2)
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

  /* TRUE if we thread one or more jumps, FALSE otherwise.  */
  bool jumps_threaded;
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

/* Create a duplicate of BB.  Record the duplicate block in an array
   indexed by COUNT stored in RD.  */

static void
create_block_for_threading (basic_block bb,
			    struct redirection_data *rd,
			    unsigned int count)
{
  edge_iterator ei;
  edge e;

  /* We can use the generic block duplication code and simply remove
     the stuff we do not need.  */
  rd->dup_blocks[count] = duplicate_block (bb, NULL, NULL);

  FOR_EACH_EDGE (e, ei, rd->dup_blocks[count]->succs)
    e->aux = NULL;

  /* Zero out the profile, since the block is unreachable for now.  */
  rd->dup_blocks[count]->frequency = 0;
  rd->dup_blocks[count]->count = 0;
}

/* Main data structure to hold information for duplicates of BB.  */

static hash_table <redirection_data> redirection_data;

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

  slot = redirection_data.find_slot (elt, insert);

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
  for (gimple_stmt_iterator gsi = gsi_start_phis (src_e->dest),
			    gsi2 = gsi_start_phis (tgt_e->dest);
       !gsi_end_p (gsi);
       gsi_next (&gsi), gsi_next (&gsi2))
    {
      gimple src_phi = gsi_stmt (gsi);
      gimple dest_phi = gsi_stmt (gsi2);
      tree val = gimple_phi_arg_def (src_phi, src_idx);
      source_location locus = gimple_phi_arg_location (src_phi, src_idx);

      SET_PHI_ARG_DEF (dest_phi, tgt_idx, val);
      gimple_phi_arg_set_location (dest_phi, tgt_idx, locus);
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
  edge e = make_edge (bb, rd->path->last ()->e->dest, EDGE_FALLTHRU);

  rescan_loop_exit (e, true, false);
  e->probability = REG_BR_PROB_BASE;
  e->count = bb->count;

  /* We have to copy path -- which means creating a new vector as well
     as all the jump_thread_edge entries.  */
  if (rd->path->last ()->e->aux)
    {
      vec<jump_thread_edge *> *path = THREAD_PATH (rd->path->last ()->e);
      vec<jump_thread_edge *> *copy = new vec<jump_thread_edge *> ();

      /* Sadly, the elements of the vector are pointers and need to
	 be copied as well.  */
      for (unsigned int i = 0; i < path->length (); i++)
	{
	  jump_thread_edge *x
	    = new jump_thread_edge ((*path)[i]->e, (*path)[i]->type);
	  copy->safe_push (x);
	}
      e->aux = (void *)copy;
    }
  else
    {
      e->aux = NULL;
    }

  /* If there are any PHI nodes at the destination of the outgoing edge
     from the duplicate block, then we will need to add a new argument
     to them.  The argument should have the same value as the argument
     associated with the outgoing edge stored in RD.  */
  copy_phi_args (e->dest, rd->path->last ()->e, e);
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

/* Wire up the outgoing edges from the duplicate blocks and
   update any PHIs as needed.  */
void
ssa_fix_duplicate_block_edges (struct redirection_data *rd,
			       ssa_local_info_t *local_info)
{
  edge e = rd->incoming_edges->e;
  vec<jump_thread_edge *> *path = THREAD_PATH (e);

  for (unsigned int count = 0, i = 1; i < path->length (); i++)
    {
      /* If we were threading through an joiner block, then we want
	 to keep its control statement and redirect an outgoing edge.
	 Else we want to remove the control statement & edges, then create
	 a new outgoing edge.  In both cases we may need to update PHIs.  */
      if ((*path)[i]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  edge victim;
	  edge e2;

	  /* This updates the PHIs at the destination of the duplicate
	     block.  */
	  update_destination_phis (local_info->bb, rd->dup_blocks[count]);

	  /* Find the edge from the duplicate block to the block we're
	     threading through.  That's the edge we want to redirect.  */
	  victim = find_edge (rd->dup_blocks[count], (*path)[i]->e->dest);

	  /* If there are no remaining blocks on the path to duplicate,
	     then redirect VICTIM to the final destination of the jump
	     threading path.  */
	  if (!any_remaining_duplicated_blocks (path, i))
	    {
	      e2 = redirect_edge_and_branch (victim, path->last ()->e->dest);
	      e2->count = path->last ()->e->count;
	      /* If we redirected the edge, then we need to copy PHI arguments
		 at the target.  If the edge already existed (e2 != victim
		 case), then the PHIs in the target already have the correct
		 arguments.  */
	      if (e2 == victim)
		copy_phi_args (e2->dest, path->last ()->e, e2);
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
	  count++;
	}
      else if ((*path)[i]->type == EDGE_COPY_SRC_BLOCK)
	{
	  remove_ctrl_stmt_and_useless_edges (rd->dup_blocks[count], NULL);
	  create_edge_and_update_destination_phis (rd, rd->dup_blocks[count]);
	  if (count == 1)
	    single_succ_edge (rd->dup_blocks[1])->aux = NULL;
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
	  create_block_for_threading ((*path)[i]->e->src, rd, 1);
	  break;
	}
    }

  /* Create a template block if we have not done so already.  Otherwise
     use the template to create a new block.  */
  if (local_info->template_block == NULL)
    {
      create_block_for_threading ((*path)[1]->e->src, rd, 0);
      local_info->template_block = rd->dup_blocks[0];

      /* We do not create any outgoing edges for the template.  We will
	 take care of that in a later traversal.  That way we do not
	 create edges that are going to just be deleted.  */
    }
  else
    {
      create_block_for_threading (local_info->template_block, rd, 0);

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

  /* Walk over all the incoming edges associated associated with this
     hash table entry.  */
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

	  rd->dup_blocks[0]->count += e->count;

	  /* Excessive jump threading may make frequencies large enough so
	     the computation overflows.  */
	  if (rd->dup_blocks[0]->frequency < BB_FREQ_MAX * 2)
	    rd->dup_blocks[0]->frequency += EDGE_FREQUENCY (e);

	  /* In the case of threading through a joiner block, the outgoing
	     edges from the duplicate block were updated when they were
	     redirected during ssa_fix_duplicate_block_edges.  */
	  if ((*path)[1]->type != EDGE_COPY_SRC_JOINER_BLOCK)
	    EDGE_SUCC (rd->dup_blocks[0], 0)->count += e->count;

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
  struct loop *loop = bb->loop_father;

  /* To avoid scanning a linear array for the element we need we instead
     use a hash table.  For normal code there should be no noticeable
     difference.  However, if we have a block with a large number of
     incoming and outgoing edges such linear searches can get expensive.  */
  redirection_data.create (EDGE_COUNT (bb->succs));

  /* If we thread the latch of the loop to its exit, the loop ceases to
     exist.  Make sure we do not restrict ourselves in order to preserve
     this loop.  */
  if (loop->header == bb)
    {
      e = loop_latch_edge (loop);
      vec<jump_thread_edge *> *path = THREAD_PATH (e);

      if (path
	  && (((*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK && joiners)
	      || ((*path)[1]->type == EDGE_COPY_SRC_BLOCK && !joiners)))
	{
	  for (unsigned int i = 1; i < path->length (); i++)
	    {
	      edge e2 = (*path)[i]->e;

	      if (loop_exit_edge_p (loop, e2))
		{
		  loop->header = NULL;
		  loop->latch = NULL;
		  loops_state_set (LOOPS_NEED_FIXUP);
		}
	    }
	}
    }

  /* Record each unique threaded destination into a hash table for
     efficient lookups.  */
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
	  if ((bb->loop_father != e2->src->loop_father
	       && !loop_exit_edge_p (e2->src->loop_father, e2))
	      || (e2->src->loop_father != e2->dest->loop_father
		  && !loop_exit_edge_p (e2->src->loop_father, e2)))
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
	}

      if (e->dest == e2->src)
	update_bb_profile_for_threading (e->dest, EDGE_FREQUENCY (e),
					 e->count, (*THREAD_PATH (e))[1]->e);

      /* Insert the outgoing edge into the hash table if it is not
	 already in the hash table.  */
      lookup_redirection_data (e, INSERT);
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
  redirection_data.traverse <ssa_local_info_t *, ssa_create_duplicates>
			    (&local_info);

  /* The template does not have an outgoing edge.  Create that outgoing
     edge and update PHI nodes as the edge's target as necessary.

     We do this after creating all the duplicates to avoid creating
     unnecessary edges.  */
  redirection_data.traverse <ssa_local_info_t *, ssa_fixup_template_block>
			    (&local_info);

  /* The hash table traversals above created the duplicate blocks (and the
     statements within the duplicate blocks).  This loop creates PHI nodes for
     the duplicated blocks and redirects the incoming edges into BB to reach
     the duplicates of BB.  */
  redirection_data.traverse <ssa_local_info_t *, ssa_redirect_edges>
			    (&local_info);

  /* Done with this block.  Clear REDIRECTION_DATA.  */
  redirection_data.dispose ();

  if (noloop_only
      && bb == bb->loop_father->header)
    set_loop_copy (bb->loop_father, NULL);

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


/* Threads edge E through E->dest to the edge THREAD_TARGET (E).  Returns the
   copy of E->dest created during threading, or E->dest if it was not necessary
   to copy it (E is its single predecessor).  */

static basic_block
thread_single_edge (edge e)
{
  basic_block bb = e->dest;
  struct redirection_data rd;
  vec<jump_thread_edge *> *path = THREAD_PATH (e);
  edge eto = (*path)[1]->e;

  for (unsigned int i = 0; i < path->length (); i++)
    delete (*path)[i];
  delete path;
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

  vec<jump_thread_edge *> *npath = new vec<jump_thread_edge *> ();
  jump_thread_edge *x = new jump_thread_edge (e, EDGE_START_JUMP_THREAD);
  npath->safe_push (x);

  x = new jump_thread_edge (eto, EDGE_COPY_SRC_BLOCK);
  npath->safe_push (x);
  rd.path = npath;

  create_block_for_threading (bb, &rd, 0);
  remove_ctrl_stmt_and_useless_edges (rd.dup_blocks[0], NULL);
  create_edge_and_update_destination_phis (&rd, rd.dup_blocks[0]);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  Threaded jump %d --> %d to %d\n",
	     e->src->index, e->dest->index, rd.dup_blocks[0]->index);

  rd.dup_blocks[0]->count = e->count;
  rd.dup_blocks[0]->frequency = EDGE_FREQUENCY (e);
  single_succ_edge (rd.dup_blocks[0])->count = e->count;
  redirect_edge_and_branch (e, rd.dup_blocks[0]);
  flush_pending_stmts (e);

  return rd.dup_blocks[0];
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

/* Return true if BB is part of the new pre-header that is created
   when threading the latch to DATA.  */

static bool
def_split_header_continue_p (const_basic_block bb, const void *data)
{
  const_basic_block new_header = (const_basic_block) data;
  const struct loop *l;

  if (bb == new_header
      || loop_depth (bb->loop_father) < loop_depth (new_header->loop_father))
    return false;
  for (l = bb->loop_father; l; l = loop_outer (l))
    if (l == new_header->loop_father)
      return true;
  return false;
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

  /* If we threaded the latch using a joiner block, we cancel the
     threading opportunity out of an abundance of caution.  However,
     still allow threading from outside to inside the loop.  */
  if (latch->aux)
    {
      vec<jump_thread_edge *> *path = THREAD_PATH (latch);
      if ((*path)[1]->type == EDGE_COPY_SRC_JOINER_BLOCK)
	{
	  delete_jump_thread_path (path);
	  latch->aux = NULL;
	}
    }

  if (latch->aux)
    {
      vec<jump_thread_edge *> *path = THREAD_PATH (latch);
      tgt_edge = (*path)[1]->e;
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
      loop->header = NULL;
      loop->latch = NULL;
      loops_state_set (LOOPS_NEED_FIXUP);
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
      basic_block *bblocks;
      unsigned nblocks, i;

      /* First handle the case latch edge is redirected.  We are copying
	 the loop header but not creating a multiple entry loop.  Make the
	 cfg manipulation code aware of that fact.  */
      set_loop_copy (loop, loop);
      loop->latch = thread_single_edge (latch);
      set_loop_copy (loop, NULL);
      gcc_assert (single_succ (loop->latch) == tgt_bb);
      loop->header = tgt_bb;

      /* Remove the new pre-header blocks from our loop.  */
      bblocks = XCNEWVEC (basic_block, loop->num_nodes);
      nblocks = dfs_enumerate_from (header, 0, def_split_header_continue_p,
				    bblocks, loop->num_nodes, tgt_bb);
      for (i = 0; i < nblocks; i++)
	if (bblocks[i]->loop_father == loop)
	  {
	    remove_bb_from_loops (bblocks[i]);
	    add_bb_to_loop (bblocks[i], loop_outer (loop));
	  }
      free (bblocks);

      /* If the new header has multiple latches mark it so.  */
      FOR_EACH_EDGE (e, ei, loop->header->preds)
	if (e->src->loop_father == loop
	    && e->src != loop->latch)
	  {
	    loop->latch = NULL;
	    loops_state_set (LOOPS_MAY_HAVE_MULTIPLE_LATCHES);
	  }

      /* Cancel remaining threading requests that would make the
	 loop a multiple entry loop.  */
      FOR_EACH_EDGE (e, ei, header->preds)
	{
	  edge e2;

	  if (e->aux == NULL)
	    continue;

	  vec<jump_thread_edge *> *path = THREAD_PATH (e);
	  e2 = path->last ()->e;

	  if (e->src->loop_father != e2->dest->loop_father
	      && e2->dest != loop->header)
	    {
	      delete_jump_thread_path (path);
	      e->aux = NULL;
	    }
	}

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
  gimple_stmt_iterator gsi;
  int indx1 = e1->dest_idx;
  int indx2 = e2->dest_idx;

  for (gsi = gsi_start_phis (e1->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);

      if (!operand_equal_p (gimple_phi_arg_def (phi, indx1),
			    gimple_phi_arg_def (phi, indx2), 0))
	return false;
    }
  return true;
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

  /* Move the jump threading requests from PATHS to each edge
     which starts a jump thread path.  */
  for (i = 0; i < paths.length (); i++)
    {
      vec<jump_thread_edge *> *path = paths[i];
      edge e = (*path)[0]->e;
      e->aux = (void *)path;
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
		  if (e->aux)
		    {
		      vec<jump_thread_edge *> *path = THREAD_PATH (e);
		      delete_jump_thread_path (path);
		      e->aux = NULL;
		    }
		}
	    }
	  else
	    bitmap_set_bit (threaded_blocks, i);
	}
    }
  else
    bitmap_copy (threaded_blocks, tmp);

  /* Look for jump threading paths which cross multiple loop headers.

     The code to thread through loop headers will change the CFG in ways
     that break assumptions made by the loop optimization code.

     We don't want to blindly cancel the requests.  We can instead do better
     by trimming off the end of the jump thread path.  */
  EXECUTE_IF_SET_IN_BITMAP (tmp, 0, i, bi)
    {
      basic_block bb = BASIC_BLOCK (i);
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->aux)
	    {
	      vec<jump_thread_edge *> *path = THREAD_PATH (e);

	      /* Basically we're looking for a situation where we can see
	  	 3 or more loop structures on a jump threading path.  */

	      struct loop *first_father = (*path)[0]->e->src->loop_father;
	      struct loop *second_father = NULL;
	      for (unsigned int i = 0; i < path->length (); i++)
		{
		  /* See if this is a loop father we have not seen before.  */
		  if ((*path)[i]->e->dest->loop_father != first_father
		      && (*path)[i]->e->dest->loop_father != second_father)
		    {
		      /* We've already seen two loop fathers, so we
			 need to trim this jump threading path.  */
		      if (second_father != NULL)
			{
			  /* Trim from entry I onwards.  */
			  for (unsigned int j = i; j < path->length (); j++)
			    delete (*path)[j];
			  path->truncate (i);

			  /* Now that we've truncated the path, make sure
			     what's left is still valid.   We need at least
			     two edges on the path and the last edge can not
			     be a joiner.  This should never happen, but let's
			     be safe.  */
			  if (path->length () < 2
			      || (path->last ()->type
				  == EDGE_COPY_SRC_JOINER_BLOCK))
			    {
			      delete_jump_thread_path (path);
			      e->aux = NULL;
			    }
			  break;
			}
		      else
			{
			  second_father = (*path)[i]->e->dest->loop_father;
			}
		    }
		}
	    }
	}
    }

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
      bb = BASIC_BLOCK (i);
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

  BITMAP_FREE (tmp);
}


/* Return TRUE if BB ends with a switch statement or a computed goto.
   Otherwise return false.  */
static bool
bb_ends_with_multiway_branch (basic_block bb ATTRIBUTE_UNUSED)
{
  gimple stmt = last_stmt (bb);
  if (stmt && gimple_code (stmt) == GIMPLE_SWITCH)
    return true;
  if (stmt && gimple_code (stmt) == GIMPLE_GOTO
      && TREE_CODE (gimple_goto_dest (stmt)) == SSA_NAME)
    return true;
  return false;
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

  /* We must know about loops in order to preserve them.  */
  gcc_assert (current_loops != NULL);

  if (!paths.exists ())
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
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      if (!loop->header
	  || !bitmap_bit_p (threaded_blocks, loop->header->index))
	continue;

      retval |= thread_through_loop_header (loop, may_peel_loop_headers);
    }

  /* Any jump threading paths that are still attached to edges at this
     point must be one of two cases.

     First, we could have a jump threading path which went from outside
     a loop to inside a loop that was ignored because a prior jump thread
     across a backedge was realized (which indirectly causes the loop
     above to ignore the latter thread).  We can detect these because the
     loop structures will be different and we do not currently try to
     optimize this case.

     Second, we could be threading across a backedge to a point within the
     same loop.  This occurrs for the FSA/FSM optimization and we would
     like to optimize it.  However, we have to be very careful as this
     may completely scramble the loop structures, with the result being
     irreducible loops causing us to throw away our loop structure.

     As a compromise for the latter case, if the thread path ends in
     a block where the last statement is a multiway branch, then go
     ahead and thread it, else ignore it.  */
  basic_block bb;
  edge e;
  FOR_EACH_BB (bb)
    {
      /* If we do end up threading here, we can remove elements from
	 BB->preds.  Thus we can not use the FOR_EACH_EDGE iterator.  */
      for (edge_iterator ei = ei_start (bb->preds);
	   (e = ei_safe_edge (ei));)
	if (e->aux)
	  {
	    vec<jump_thread_edge *> *path = THREAD_PATH (e);

	    /* Case 1, threading from outside to inside the loop
	       after we'd already threaded through the header.  */
	    if ((*path)[0]->e->dest->loop_father
		!= path->last ()->e->src->loop_father)
	      {
		delete_jump_thread_path (path);
		e->aux = NULL;
		ei_next (&ei);
	      }
	   else if (bb_ends_with_multiway_branch (path->last ()->e->src))
	      {
		/* The code to thread through loop headers may have
		   split a block with jump threads attached to it.

		   We can identify this with a disjoint jump threading
		   path.  If found, just remove it.  */
		for (unsigned int i = 0; i < path->length () - 1; i++)
		  if ((*path)[i]->e->dest != (*path)[i + 1]->e->src)
		    {
		      delete_jump_thread_path (path);
		      e->aux = NULL;
		      ei_next (&ei);
		      break;
		    }

		/* Our path is still valid, thread it.  */
	        if (e->aux)
		  {
		    struct loop *loop = (*path)[0]->e->dest->loop_father;

		    retval |= thread_block ((*path)[0]->e->dest, false);
		    e->aux = NULL;

		    /* This jump thread likely totally scrambled this loop.
		       So arrange for it to be fixed up.  */
		    loop->header = NULL;
		    loop->latch = NULL;
		  }
	      }
	   else
	      {
		delete_jump_thread_path (path);
		e->aux = NULL;
		ei_next (&ei);
	      }
 	  }
	else
	  ei_next (&ei);
    }

  statistics_counter_event (cfun, "Jumps threaded",
			    thread_stats.num_threaded_edges);

  free_original_copy_tables ();

  BITMAP_FREE (threaded_blocks);
  threaded_blocks = NULL;
  paths.release ();

  if (retval)
    loops_state_set (LOOPS_NEED_FIXUP);

  return retval;
}

/* Delete the jump threading path PATH.  We have to explcitly delete
   each entry in the vector, then the container.  */

void
delete_jump_thread_path (vec<jump_thread_edge *> *path)
{
  for (unsigned int i = 0; i < path->length (); i++)
    delete (*path)[i];
  path->release();
}

/* Dump a jump threading path, including annotations about each
   edge in the path.  */

static void
dump_jump_thread_path (FILE *dump_file, vec<jump_thread_edge *> path)
{
  fprintf (dump_file,
	   "  Registering jump thread: (%d, %d) incoming edge; ",
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
    }
  fputc ('\n', dump_file);
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
    if ((*path)[i]->e == NULL)
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file,
		     "Found NULL edge in jump threading path.  Cancelling jump thread:\n");
	    dump_jump_thread_path (dump_file, *path);
	  }

	delete_jump_thread_path (path);
	return;
      }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_jump_thread_path (dump_file, *path);

  if (!paths.exists ())
    paths.create (5);

  paths.safe_push (path);
}
