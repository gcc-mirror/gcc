/* Optimization of PHI nodes by converting them into straightline code.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "basic-block.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "langhooks.h"

static void tree_ssa_phiopt (void);
static bool conditional_replacement (basic_block bb, tree phi, tree arg0,
                                     tree arg1);


/* This pass eliminates PHI nodes which can be trivially implemented as
   an assignment from a conditional expression.  ie if we have something
   like:

     bb0:
      if (cond) goto bb2; else goto bb1;
     bb1:
     bb2:
      x = PHI (0 (bb1), 1 (bb0)

    We can rewrite that as:
    
    bb0:
    bb1:
    bb2:
      x = cond;

    bb1 will become unreachable and bb0 and bb2 will almost always
    be merged into a single block.  This occurs often due to gimplification
    of conditionals.  */
   
static void
tree_ssa_phiopt (void)
{
  basic_block bb;
  bool removed_phis = false;

  /* Search every basic block for PHI nodes we may be able to optimize.  */
  FOR_EACH_BB (bb)
    {
      tree arg0, arg1, phi;


      /* We're searching for blocks with one PHI node which has two
	 arguments.  */
      phi = phi_nodes (bb);
      if (phi && TREE_CHAIN (phi) == NULL
	  && PHI_NUM_ARGS (phi) == 2)
        {

            arg0 = PHI_ARG_DEF (phi, 0);
            arg1 = PHI_ARG_DEF (phi, 1);
            
            /* Do the replacement of conditional if it can be done.  */
            if (conditional_replacement (bb, phi, arg0, arg1))
              {
                /* We have done the replacement so we need to rebuild the cfg.   */
                removed_phis = true;
                continue;
              }
        }
    }

  /* If we removed any PHIs, then we have unreachable blocks and blocks
     which need to be merged in the CFG.  */
  if (removed_phis)
    cleanup_tree_cfg ();
}

/*  The function conditional_replacement does the main work of doing the conditional
    replacement.  Return true if the replacement is done.  Otherwise return false.
    bb is the basic block where the replacement is going to be done on.  arg0
    is argument 0 from the phi.  Likewise for arg1.   */

static bool
conditional_replacement (basic_block bb, tree phi, tree arg0, tree arg1)
{
  tree result;
  basic_block other_block = NULL;
  basic_block cond_block = NULL;
  tree last0, last1, new, cond;
  block_stmt_iterator bsi;
  edge true_edge, false_edge;

  /* The PHI arguments have the constants 0 and 1, then convert
    it to the conditional.  */
  if ((integer_zerop (arg0) && integer_onep (arg1))
      || (integer_zerop (arg1) && integer_onep (arg0)))
    ;
  else
    return false;
  
  /* One of the alternatives must come from a block ending with
      a COND_EXPR.  The other block must be entirely empty, except
      for labels.  */
  last0 = last_stmt (bb->pred->src);
  last1 = last_stmt (bb->pred->pred_next->src);
  if (last0 && TREE_CODE (last0) == COND_EXPR)
    {
      cond_block = bb->pred->src;
      other_block = bb->pred->pred_next->src;
    }
  else if (last1 && TREE_CODE (last1) == COND_EXPR)
    {
      other_block = bb->pred->src;
      cond_block = bb->pred->pred_next->src;
    }
  else
    return false;
  
  /* COND_BLOCK must have precisely two successors.  We indirectly
      verify that those successors are BB and OTHER_BLOCK.  */
  if (!cond_block->succ
      || !cond_block->succ->succ_next
      || cond_block->succ->succ_next->succ_next
      || (cond_block->succ->flags & EDGE_ABNORMAL) != 0
      || (cond_block->succ->succ_next->flags & EDGE_ABNORMAL) != 0)
    return false;
  
  /* OTHER_BLOCK must have a single predecessor which is COND_BLOCK,
      OTHER_BLOCK must have a single successor which is BB and
      OTHER_BLOCK must have no PHI nodes.  */
  if (!other_block->pred
      || other_block->pred->src != cond_block
      || other_block->pred->pred_next
      || !other_block->succ
      || other_block->succ->dest != bb
      || other_block->succ->succ_next
      || phi_nodes (other_block))
    return false;
  
  /* OTHER_BLOCK must have no executable statements.  */
  bsi = bsi_start (other_block);
  while (!bsi_end_p (bsi)
          && (TREE_CODE (bsi_stmt (bsi)) == LABEL_EXPR
              || IS_EMPTY_STMT (bsi_stmt (bsi))))
    bsi_next (&bsi);
  
  if (!bsi_end_p (bsi))
    return false;
  
  /* If the condition is not a naked SSA_NAME and its type does not
      match the type of the result, then we can not optimize this case
      as it would likely create non-gimple code when the condition
      was converted to the result's type.  */
  cond = COND_EXPR_COND (last_stmt (cond_block));
  result = PHI_RESULT (phi);
  if (TREE_CODE (cond) != SSA_NAME
      && !lang_hooks.types_compatible_p (TREE_TYPE (cond), TREE_TYPE (result)))
    return false;
  
  /* If the condition was a naked SSA_NAME and the type is not the
      same as the type of the result, then convert the type of the
      condition.  */
  if (!lang_hooks.types_compatible_p (TREE_TYPE (cond), TREE_TYPE (result)))
    cond = fold_convert (TREE_TYPE (result), cond);
  
  /* We need to know which is the true edge and which is the false
      edge so that we know when to invert the condition below.  */
  extract_true_false_edges_from_block (cond_block, &true_edge, &false_edge);
  
  /* At this point we know we have a COND_EXPR with two successors.
      One successor is BB, the other successor is an empty block which
      falls through into BB.
  
      There is a single PHI node at the join point (BB) and its arguments
      are constants (0, 1).
  
      So, given the condition COND, and the two PHI arguments, we can
      rewrite this PHI into non-branching code: 
  
        dest = (COND) or dest = COND'
  
      We use the condition as-is if the argument associated with the
      true edge has the value one or the argument associated with the
      false edge as the value zero.  Note that those conditions are not
      the same since only one of the outgoing edges from the COND_EXPR
      will directly reach BB and thus be associated with an argument.  */
  if ((PHI_ARG_EDGE (phi, 0) == true_edge && integer_onep (arg0))
      || (PHI_ARG_EDGE (phi, 0) == false_edge && integer_zerop (arg0))
      || (PHI_ARG_EDGE (phi, 1) == true_edge && integer_onep (arg1))
      || (PHI_ARG_EDGE (phi, 1) == false_edge && integer_zerop (arg1)))
    {
      new = build (MODIFY_EXPR, TREE_TYPE (PHI_RESULT (phi)),
                    PHI_RESULT (phi), cond);
    }
  else
    {
      cond = invert_truthvalue (cond);
  
      if (is_gimple_cast (cond)
	  && !is_gimple_val (TREE_OPERAND (cond, 0)))
	return false;
      
      if (TREE_CODE (cond) == TRUTH_NOT_EXPR
          &&  !is_gimple_val (TREE_OPERAND (cond, 0)))
        return false;

      new = build (MODIFY_EXPR, TREE_TYPE (PHI_RESULT (phi)),
                    PHI_RESULT (phi), cond);
    }
  
  /* Insert our new statement at the head of our block.  */
  bsi = bsi_start (bb);
  bsi_insert_after (&bsi, new, BSI_SAME_STMT);
  
  /* Register our new statement as the defining statement for
      the result.  */
  SSA_NAME_DEF_STMT (PHI_RESULT (phi)) = new;
  
  /* Remove the now useless PHI node. 
  
      We do not want to use remove_phi_node since that releases the
      SSA_NAME as well and the SSA_NAME is still being used.  */
  release_phi_node (phi);
  bb_ann (bb)->phi_nodes = NULL;
  
  /* Disconnect the edge leading into the empty block.  That will
     make the empty block unreachable and it will be removed later.  */
  if (cond_block->succ->dest == bb)
    {
      cond_block->succ->flags |= EDGE_FALLTHRU;
      cond_block->succ->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      ssa_remove_edge (cond_block->succ->succ_next);
    }
  else
    {
      cond_block->succ->succ_next->flags |= EDGE_FALLTHRU;
      cond_block->succ->succ_next->flags
	&= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);
      ssa_remove_edge (cond_block->succ);
    }
  
  /* Eliminate the COND_EXPR at the end of COND_BLOCK.  */
  bsi = bsi_last (cond_block);
  bsi_remove (&bsi);
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
              "COND_EXPR in block %d and PHI in block %d converted to straightline code.\n",
              cond_block->index,
              bb->index);
            
  /* Note that we optimized this PHI.  */
  return true;
}


/* Always do these optimizations if we have SSA
   trees to work on.  */						
static bool
gate_phiopt (void)
{
  return 1;
}
												
struct tree_opt_pass pass_phiopt =
{
  "phiopt",				/* name */
  gate_phiopt,				/* gate */
  tree_ssa_phiopt,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PHIOPT,			/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect	/* todo_flags_finish */
    | TODO_verify_ssa
};
												

