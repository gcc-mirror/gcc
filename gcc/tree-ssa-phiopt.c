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
#include "flags.h"
#include "tm_p.h"
#include "basic-block.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "langhooks.h"

static void tree_ssa_phiopt (void);
static bool conditional_replacement (basic_block, tree, tree, tree);
static bool value_replacement (basic_block, tree, tree, tree);
static bool abs_replacement (basic_block, tree, tree, tree);
static void replace_phi_with_stmt (block_stmt_iterator, basic_block,
				   basic_block, tree, tree);
static bool candidate_bb_for_phi_optimization (basic_block,
					       basic_block *,
					       basic_block *);

/* This pass eliminates PHI nodes which can be trivially implemented as
   an assignment from a conditional expression.  i.e. if we have something
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
    of conditionals. 
   
   Also done is the following optimization:

     bb0:
      if (a != b) goto bb2; else goto bb1;
     bb1:
     bb2:
      x = PHI (a (bb1), b (bb0))

   We can rewrite that as:

     bb0:
     bb1:
     bb2:
      x = b;

   This can sometimes occur as a result of other optimizations.  A
   similar transformation is done by the ifcvt RTL optimizer. 

   This pass also eliminates PHI nodes which are really absolute 
   values.  i.e. if we have something like:

     bb0:
      if (a >= 0) goto bb2; else goto bb1;
     bb1:
      x = -a;
     bb2:
      x = PHI (x (bb1), a (bb0));

   We can rewrite that as:

     bb0:
     bb1:
     bb2:
      x = ABS_EXPR< a >;

   bb1 will become unreachable and bb0 and bb2 will almost always be merged
   into a single block.  Similar transformations are done by the ifcvt
   RTL optimizer.  */ 

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
      if (phi && PHI_CHAIN (phi) == NULL
	  && PHI_NUM_ARGS (phi) == 2)
	{
	  arg0 = PHI_ARG_DEF (phi, 0);
	  arg1 = PHI_ARG_DEF (phi, 1);
	    
	  /* Do the replacement of conditional if it can be done.  */
	    if (conditional_replacement (bb, phi, arg0, arg1)
		|| value_replacement (bb, phi, arg0, arg1)
		|| abs_replacement (bb, phi, arg0, arg1))
	      {
		/* We have done the replacement so we need to rebuild the
		   cfg when this pass is complete.  */
		removed_phis = true;
	      }
	}
    }
}

/* Return TRUE if block BB has no executable statements, otherwise return
   FALSE.  */
bool
empty_block_p (basic_block bb)
{
  block_stmt_iterator bsi;

  /* BB must have no executable statements.  */
  bsi = bsi_start (bb);
  while (!bsi_end_p (bsi)
	  && (TREE_CODE (bsi_stmt (bsi)) == LABEL_EXPR
	      || IS_EMPTY_STMT (bsi_stmt (bsi))))
    bsi_next (&bsi);
  
  if (!bsi_end_p (bsi))
    return false;

  return true;
}

/* BB is a basic block which has only one PHI node with precisely two
   arguments.

   Examine both of BB's predecessors to see if one ends with a 
   COND_EXPR and the other is a successor of the COND_EXPR.  If so, then
   we may be able to optimize PHI nodes at the start of BB. 

   If so, mark store the block with the COND_EXPR into COND_BLOCK_P
   and the other block into OTHER_BLOCK_P and return true, otherwise
   return false.  */

static bool
candidate_bb_for_phi_optimization (basic_block bb,
				   basic_block *cond_block_p,
				   basic_block *other_block_p)
{
  tree last0, last1;
  basic_block cond_block, other_block;

  /* One of the alternatives must come from a block ending with
     a COND_EXPR.  */
  last0 = last_stmt (EDGE_PRED (bb, 0)->src);
  last1 = last_stmt (EDGE_PRED (bb, 1)->src);
  if (last0 && TREE_CODE (last0) == COND_EXPR)
    {
      cond_block = EDGE_PRED (bb, 0)->src;
      other_block = EDGE_PRED (bb, 1)->src;
    }
  else if (last1 && TREE_CODE (last1) == COND_EXPR)
    {
      other_block = EDGE_PRED (bb, 0)->src;
      cond_block = EDGE_PRED (bb, 1)->src;
    }
  else
    return false;
  
  /* COND_BLOCK must have precisely two successors.  We indirectly
     verify that those successors are BB and OTHER_BLOCK.  */
  if (EDGE_COUNT (cond_block->succs) != 2
      || (EDGE_SUCC (cond_block, 0)->flags & EDGE_ABNORMAL) != 0
      || (EDGE_SUCC (cond_block, 1)->flags & EDGE_ABNORMAL) != 0)
    return false;
  
  /* OTHER_BLOCK must have a single predecessor which is COND_BLOCK,
     OTHER_BLOCK must have a single successor which is BB and
     OTHER_BLOCK must have no PHI nodes.  */
  if (EDGE_COUNT (other_block->preds) != 1
      || EDGE_PRED (other_block, 0)->src != cond_block
      || EDGE_COUNT (other_block->succs) != 1
      || EDGE_SUCC (other_block, 0)->dest != bb
      || phi_nodes (other_block))
    return false;
  
  *cond_block_p = cond_block;
  *other_block_p = other_block;
  /* Everything looks OK.  */
  return true;
}

/* Replace PHI in block BB with statement NEW.  NEW is inserted after
   BSI.  Remove the edge from COND_BLOCK which does not lead to BB (COND_BLOCK
   is known to have two edges, one of which must reach BB).  */

static void
replace_phi_with_stmt (block_stmt_iterator bsi, basic_block bb,
		       basic_block cond_block, tree phi, tree new)
{
  basic_block block_to_remove;

  /* Insert our new statement at the head of our block.  */
  bsi_insert_after (&bsi, new, BSI_NEW_STMT);
  
  /* Register our new statement as the defining statement for
     the result.  */
  SSA_NAME_DEF_STMT (PHI_RESULT (phi)) = new;
  
  /* Remove the now useless PHI node. 
  
     We do not want to use remove_phi_node since that releases the
     SSA_NAME as well and the SSA_NAME is still being used.  */
  release_phi_node (phi);
  bb_ann (bb)->phi_nodes = NULL;
  
  /* Remove the empty basic block.  */
  if (EDGE_SUCC (cond_block, 0)->dest == bb)
    {
      EDGE_SUCC (cond_block, 0)->flags |= EDGE_FALLTHRU;
      EDGE_SUCC (cond_block, 0)->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

      block_to_remove = EDGE_SUCC (cond_block, 1)->dest;
    }
  else
    {
      EDGE_SUCC (cond_block, 1)->flags |= EDGE_FALLTHRU;
      EDGE_SUCC (cond_block, 1)->flags
	&= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

      block_to_remove = EDGE_SUCC (cond_block, 0)->dest;
    }
  delete_basic_block (block_to_remove);
  
  /* Eliminate the COND_EXPR at the end of COND_BLOCK.  */
  bsi = bsi_last (cond_block);
  bsi_remove (&bsi);
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	      "COND_EXPR in block %d and PHI in block %d converted to straightline code.\n",
	      cond_block->index,
	      bb->index);
}

/*  The function conditional_replacement does the main work of doing the
    conditional replacement.  Return true if the replacement is done.
    Otherwise return false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from PHI.  Likewise for ARG1.  */

static bool
conditional_replacement (basic_block bb, tree phi, tree arg0, tree arg1)
{
  tree result;
  tree old_result = NULL;
  basic_block other_block = NULL;
  basic_block cond_block = NULL;
  tree new, cond;
  block_stmt_iterator bsi;
  edge true_edge, false_edge;
  tree new_var = NULL;

  /* The PHI arguments have the constants 0 and 1, then convert
     it to the conditional.  */
  if ((integer_zerop (arg0) && integer_onep (arg1))
      || (integer_zerop (arg1) && integer_onep (arg0)))
    ;
  else
    return false;
  
  if (!candidate_bb_for_phi_optimization (bb, &cond_block, &other_block)
      || !empty_block_p (other_block))
    return false;
										
  /* If the condition is not a naked SSA_NAME and its type does not
     match the type of the result, then we have to create a new
     variable to optimize this case as it would likely create
     non-gimple code when the condition was converted to the
     result's type.  */
  cond = COND_EXPR_COND (last_stmt (cond_block));
  result = PHI_RESULT (phi);
  if (TREE_CODE (cond) != SSA_NAME
      && !lang_hooks.types_compatible_p (TREE_TYPE (cond), TREE_TYPE (result)))
    {
      new_var = make_rename_temp (TREE_TYPE (cond), NULL);
      old_result = cond;
      cond = new_var;
    }
  
  /* If the condition was a naked SSA_NAME and the type is not the
     same as the type of the result, then convert the type of the
     condition.  */
  if (!lang_hooks.types_compatible_p (TREE_TYPE (cond), TREE_TYPE (result)))
    cond = fold_convert (TREE_TYPE (result), cond);
  
  /* We need to know which is the true edge and which is the false
     edge so that we know when to invert the condition below.  */
  extract_true_false_edges_from_block (cond_block, &true_edge, &false_edge);
      
  /* Insert our new statement at the head of our block.  */
  bsi = bsi_after_labels (bb);
  
  if (old_result)
    {
      tree new1;
      if (!COMPARISON_CLASS_P (old_result))
	return false;
      
      new1 = build (TREE_CODE (old_result), TREE_TYPE (old_result),
		    TREE_OPERAND (old_result, 0),
		    TREE_OPERAND (old_result, 1));
      
      new1 = build (MODIFY_EXPR, TREE_TYPE (old_result), new_var, new1);
      bsi_insert_after (&bsi, new1, BSI_NEW_STMT);
    }
  
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
      tree cond1 = invert_truthvalue (cond);
      
      cond = cond1;
      /* If what we get back is a conditional expression, there is no
	  way that it can be gimple.  */
      if (TREE_CODE (cond) == COND_EXPR)
	return false; 

      /* If what we get back is not gimple try to create it as gimple by
	 using a temporary variable.  */
      if (is_gimple_cast (cond)
	  && !is_gimple_val (TREE_OPERAND (cond, 0)))
	{
	  tree temp = TREE_OPERAND (cond, 0);
	  tree new_var_1 = make_rename_temp (TREE_TYPE (temp), NULL);
	  new = build (MODIFY_EXPR, TREE_TYPE (new_var_1), new_var_1, temp);
	  bsi_insert_after (&bsi, new, BSI_NEW_STMT);
	  cond = fold_convert (TREE_TYPE (result), new_var_1);
	}
      
      if (TREE_CODE (cond) == TRUTH_NOT_EXPR
	  &&  !is_gimple_val (TREE_OPERAND (cond, 0)))
	return false;

      new = build (MODIFY_EXPR, TREE_TYPE (PHI_RESULT (phi)),
		    PHI_RESULT (phi), cond);
    }
  
  replace_phi_with_stmt (bsi, bb, cond_block, phi, new);

  /* Note that we optimized this PHI.  */
  return true;
}

/*  The function value_replacement does the main work of doing the value
    replacement.  Return true if the replacement is done.  Otherwise return
    false.
    BB is the basic block where the replacement is going to be done on.  ARG0
    is argument 0 from the PHI.  Likewise for ARG1.  */

static bool
value_replacement (basic_block bb, tree phi, tree arg0, tree arg1)
{
  tree result;
  basic_block other_block = NULL;
  basic_block cond_block = NULL;
  tree new, cond;
  edge true_edge, false_edge;

  /* If the type says honor signed zeros we cannot do this
     optimization.  */
  if (HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg1))))
    return false;

  if (!candidate_bb_for_phi_optimization (bb, &cond_block, &other_block)
      || !empty_block_p (other_block))
    return false;

  cond = COND_EXPR_COND (last_stmt (cond_block));
  result = PHI_RESULT (phi);

  /* This transformation is only valid for equality comparisons.  */
  if (TREE_CODE (cond) != NE_EXPR && TREE_CODE (cond) != EQ_EXPR)
    return false;

  /* We need to know which is the true edge and which is the false
      edge so that we know if have abs or negative abs.  */
  extract_true_false_edges_from_block (cond_block, &true_edge, &false_edge);

  /* At this point we know we have a COND_EXPR with two successors.
     One successor is BB, the other successor is an empty block which
     falls through into BB.

     The condition for the COND_EXPR is known to be NE_EXPR or EQ_EXPR.

     There is a single PHI node at the join point (BB) with two arguments.

     We now need to verify that the two arguments in the PHI node match
     the two arguments to the equality comparison.  */
  
  if ((operand_equal_for_phi_arg_p (arg0, TREE_OPERAND (cond, 0))
       && operand_equal_for_phi_arg_p (arg1, TREE_OPERAND (cond, 1)))
      || (operand_equal_for_phi_arg_p (arg1, TREE_OPERAND (cond, 0))
	  && operand_equal_for_phi_arg_p (arg0, TREE_OPERAND (cond, 1))))
    {
      edge e;
      tree arg;

      /* For NE_EXPR, we want to build an assignment result = arg where
	 arg is the PHI argument associated with the true edge.  For
	 EQ_EXPR we want the PHI argument associated with the false edge.  */
      e = (TREE_CODE (cond) == NE_EXPR ? true_edge : false_edge);

      /* Unfortunately, E may not reach BB (it may instead have gone to
	 OTHER_BLOCK).  If that is the case, then we want the single outgoing
	 edge from OTHER_BLOCK which reaches BB and represents the desired
	 path from COND_BLOCK.  */
      if (e->dest == other_block)
	e = EDGE_SUCC (e->dest, 0);

      /* Now we know the incoming edge to BB that has the argument for the
	 RHS of our new assignment statement.  */
      if (PHI_ARG_EDGE (phi, 0) == e)
	arg = arg0;
      else
	arg = arg1;

      /* Build the new assignment.  */
      new = build (MODIFY_EXPR, TREE_TYPE (result), result, arg);

      replace_phi_with_stmt (bsi_after_labels (bb), bb, cond_block, phi, new);

      /* Note that we optimized this PHI.  */
      return true;
    }
  return false;
}

/*  The function absolute_replacement does the main work of doing the absolute
    replacement.  Return true if the replacement is done.  Otherwise return
    false.
    bb is the basic block where the replacement is going to be done on.  arg0
    is argument 0 from the phi.  Likewise for arg1.  */
static bool
abs_replacement (basic_block bb, tree phi, tree arg0, tree arg1)
{
  tree result;
  basic_block other_block = NULL;
  basic_block cond_block = NULL;
  tree new, cond;
  block_stmt_iterator bsi;
  edge true_edge, false_edge;
  tree assign = NULL;
  edge e;
  tree rhs = NULL, lhs = NULL;
  bool negate;
  enum tree_code cond_code;

  /* If the type says honor signed zeros we cannot do this
     optimization.  */
  if (HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg1))))
    return false;

  if (!candidate_bb_for_phi_optimization (bb, &cond_block, &other_block))
    return false;

  /* OTHER_BLOCK must have only one executable statement which must have the
     form arg0 = -arg1 or arg1 = -arg0.  */
  bsi = bsi_start (other_block);
  while (!bsi_end_p (bsi))
    {
      tree stmt = bsi_stmt (bsi);

      /* Empty statements and labels are uninteresting.  */
      if (TREE_CODE (stmt) == LABEL_EXPR
          || IS_EMPTY_STMT (stmt))
        {
          bsi_next (&bsi);
          continue;
        }

      /* If we found the assignment, but it was not the only executable
	 statement in OTHER_BLOCK, then we can not optimize.  */
      if (assign)
	return false;

      /* If we got here, then we have found the first executable statement
	 in OTHER_BLOCK.  If it is anything other than arg = -arg1 or
	 arg1 = -arg0, then we can not optimize.  */
      if (TREE_CODE (stmt) == MODIFY_EXPR)
        {
          lhs = TREE_OPERAND (stmt, 0);
          rhs = TREE_OPERAND (stmt, 1);

          if (TREE_CODE (rhs) == NEGATE_EXPR)
            {
              rhs = TREE_OPERAND (rhs, 0);

              /* The assignment has to be arg0 = -arg1 or arg1 = -arg0.  */
              if ((lhs == arg0 && rhs == arg1)
		  || (lhs == arg1 && rhs == arg0))
		{
		  assign = stmt;
		  bsi_next (&bsi);
		}
	      else
		return false;
            }
	  else
	    return false;
        }
      else
	return false;
    }

  /* If we did not find the proper negation assignment, then we can not
     optimize.  */
  if (assign == NULL)
    return false;

  cond = COND_EXPR_COND (last_stmt (cond_block));
  result = PHI_RESULT (phi);

  /* Only relationals comparing arg[01] against zero are interesting.  */
  cond_code = TREE_CODE (cond);
  if (cond_code != GT_EXPR && cond_code != GE_EXPR
      && cond_code != LT_EXPR && cond_code != LE_EXPR)
    return false;

  /* Make sure the conditional is arg[01] OP y.  */
  if (TREE_OPERAND (cond, 0) != rhs)
    return false;

  if (FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (cond, 1)))
	       ? real_zerop (TREE_OPERAND (cond, 1))
	       : integer_zerop (TREE_OPERAND (cond, 1)))
    ;
  else
    return false;

  /* We need to know which is the true edge and which is the false
     edge so that we know if have abs or negative abs.  */
  extract_true_false_edges_from_block (cond_block, &true_edge, &false_edge);

  /* For GT_EXPR/GE_EXPR, if the true edge goes to OTHER_BLOCK, then we
     will need to negate the result.  Similarly for LT_EXPR/LE_EXPR if
     the false edge goes to OTHER_BLOCK.  */
  if (cond_code == GT_EXPR || cond_code == GE_EXPR)
    e = true_edge;
  else
    e = false_edge;
  
  if (e->dest == other_block)
    negate = true;
  else
    negate = false;
  
  if (negate)
    lhs = make_rename_temp (TREE_TYPE (result), NULL);
  else
    lhs = result;

  /* Build the modify expression with abs expression.  */
  new = build (MODIFY_EXPR, TREE_TYPE (lhs),
               lhs, build1 (ABS_EXPR, TREE_TYPE (lhs), rhs));

  replace_phi_with_stmt (bsi_after_labels (bb), bb, cond_block, phi, new);

  if (negate)
    {

      /* Get the right BSI.  We want to insert after the recently 
	 added ABS_EXPR statement (which we know is the first statement
	 in the block.  */
      bsi = bsi_start (bb);
      bsi_next (&bsi);
      new = build (MODIFY_EXPR, TREE_TYPE (result),
                   result, build1 (NEGATE_EXPR, TREE_TYPE (lhs), lhs));

      bsi_insert_after (&bsi, new, BSI_NEW_STMT);

      /* Register the new statement as defining the temporary -- this is
	 normally done by replace_phi_with_stmt, but the link will be wrong
	 if we had to negate the resulting value.  */
      SSA_NAME_DEF_STMT (result) = new;
    }

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
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_cleanup_cfg | TODO_dump_func | TODO_ggc_collect	/* todo_flags_finish */
    | TODO_verify_ssa | TODO_rename_vars
    | TODO_verify_flow,
  0					/* letter */
};
												

