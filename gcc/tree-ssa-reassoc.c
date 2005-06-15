/* Reassociation for trees.
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org>

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
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-dump.h"
#include "timevar.h"
#include "hashtab.h"
#include "tree-iterator.h"
#include "tree-pass.h"

/*  This is a simple global reassociation pass that uses a combination
    of heuristics and a hashtable to try to expose more operations to
    CSE.  

    The basic idea behind the heuristic is to rank expressions by
    depth of the computation tree and loop depth, and try to produce
    expressions consisting of small rank operations, as they are more
    likely to reoccur.  In addition, we use a hashtable to try to see
    if we can transpose an operation into something we have seen
    before.

    Note that the way the hashtable is structured will sometimes find
    matches that will not expose additional redundancies, since it is
    not unwound as we traverse back up one branch of the dominator
    tree and down another.  However, the cost of improving this is
    probably not worth the additional benefits it will bring.  */

/* Statistics */
static struct
{
  int reassociated_by_rank;
  int reassociated_by_match;
} reassociate_stats;



/* Seen binary operator hashtable.  */
static htab_t seen_binops;

/* Binary operator struct. */

typedef struct seen_binop_d
{
  tree op1;
  tree op2;
} *seen_binop_t;

/* Return a SEEN_BINOP_T if we have seen an associative binary
   operator with OP1 and OP2 in it.  */

static seen_binop_t
find_seen_binop (tree op1, tree op2)
{
  void **slot;
  struct seen_binop_d sbd;
  sbd.op1 = op1;
  sbd.op2 = op2;
  slot = htab_find_slot (seen_binops, &sbd, NO_INSERT);
  if (!slot)
    return NULL;
  return ((seen_binop_t) *slot);
}

/* Insert a binary operator consisting of OP1 and OP2 into the
   SEEN_BINOP table.  */

static void
insert_seen_binop (tree op1, tree op2)
{
  void **slot;
  seen_binop_t new_pair = xmalloc (sizeof (*new_pair));
  new_pair->op1 = op1;
  new_pair->op2 = op2;
  slot = htab_find_slot (seen_binops, new_pair, INSERT);
  if (*slot != NULL)
    free (*slot);
  *slot = new_pair;
}

/* Return the hash value for a seen binop structure pointed to by P.
   Because all the binops we consider are associative, we just add the
   hash value for op1 and op2.  */

static hashval_t
seen_binop_hash (const void *p)
{
  const seen_binop_t sb = (seen_binop_t) p;
  return iterative_hash_expr (sb->op1, 0) + iterative_hash_expr (sb->op2, 0);
}

/* Return true if two seen binop structures pointed to by P1 and P2 are equal.
   We have to check the operators both ways because we don't know what
   order they appear in the table.  */

static int
seen_binop_eq (const void *p1, const void *p2)
{
  const seen_binop_t sb1 = (seen_binop_t) p1;
  const seen_binop_t sb2 = (seen_binop_t) p2;
  return (sb1->op1 == sb2->op1 && sb1->op2 == sb2->op2)
    || (sb1->op2 == sb2->op1 && sb1->op1 == sb2->op2);
}

/* Value rank structure.  */

typedef struct valrank_d
{
  tree e;   
  unsigned int rank;  
} *valrank_t;

/* Starting rank number for a given basic block, so that we can rank
   operations using unmovable instructions in that BB based on the bb
   depth.  */
static unsigned int *bb_rank;

/* Value rank hashtable.  */
static htab_t value_rank;


/* Look up the value rank structure for expression E.  */

static valrank_t
find_value_rank (tree e)
{
  void **slot;
  struct valrank_d vrd;
  vrd.e = e;
  slot = htab_find_slot (value_rank, &vrd, NO_INSERT);
  if (!slot)
    return NULL;
  return ((valrank_t) *slot);
}

/* Insert {E,RANK} into the value rank hashtable.  */

static void
insert_value_rank (tree e, unsigned int rank)
{
  void **slot;
  valrank_t new_pair = xmalloc (sizeof (*new_pair));
  new_pair->e = e;
  new_pair->rank = rank;
  slot = htab_find_slot (value_rank, new_pair, INSERT);
  gcc_assert (*slot == NULL);
  *slot = new_pair;

}


/* Return the hash value for a value rank structure  */

static hashval_t
valrank_hash (const void *p)
{
  const valrank_t vr = (valrank_t) p;
  return iterative_hash_expr (vr->e, 0);
}

/* Return true if two value rank structures are equal.  */

static int
valrank_eq (const void *p1, const void *p2)
{
  const valrank_t vr1 = (valrank_t) p1;
  const valrank_t vr2 = (valrank_t) p2;
  return vr1->e == vr2->e;
}


/* Initialize the reassociation pass.  */

static void
init_reassoc (void)
{
  int i;
  unsigned int rank = 2;
  
  tree param;
  int *bbs = xmalloc ((last_basic_block + 1) * sizeof (int));
  
  memset (&reassociate_stats, 0, sizeof (reassociate_stats));

  /* Reverse RPO (Reverse Post Order) will give us something where
     deeper loops come later.  */
  flow_reverse_top_sort_order_compute (bbs);
  bb_rank = xcalloc (last_basic_block + 1, sizeof (unsigned int));
  value_rank = htab_create (511, valrank_hash,
			    valrank_eq, free);
  seen_binops = htab_create (511, seen_binop_hash,
			     seen_binop_eq, free);

  /* Give each argument a distinct rank.   */
  for (param = DECL_ARGUMENTS (current_function_decl);
       param;
       param = TREE_CHAIN (param))
    {
      if (default_def (param) != NULL)
	{
	  tree def = default_def (param);
	  insert_value_rank (def, ++rank);
	}
    }
  /* Give the chain decl a distinct rank. */
  if (cfun->static_chain_decl != NULL)
    {
      tree def = default_def (cfun->static_chain_decl);
      if (def != NULL)
        insert_value_rank (def, ++rank);
    }
  
  /* Set up rank for each BB  */
  for (i = 0; i < n_basic_blocks; i++)
    bb_rank[bbs[i]] = ++rank  << 16;

  free (bbs);
  calculate_dominance_info (CDI_DOMINATORS);

}

/* Cleanup after the reassociation pass, and print stats if
   requested.  */

static void
fini_reassoc (void)
{

  if (dump_file && (dump_flags & TDF_STATS))
    {
      fprintf (dump_file, "Reassociation stats:\n");
      fprintf (dump_file, "Reassociated by rank: %d\n", reassociate_stats.reassociated_by_rank);
      fprintf (dump_file, "Reassociated by match: %d\n", reassociate_stats.reassociated_by_match);
    }
  htab_delete (value_rank);
  htab_delete (seen_binops);
  free (bb_rank);
}

/* Given an expression E, return the rank of the expression.  */

static unsigned int
get_rank (tree e)
{
  valrank_t vr;

  /* Constants have rank 0.  */  
  if (is_gimple_min_invariant (e))
    return 0;
  
  /* SSA_NAME's have the rank of the expression they are the result
     of.
     For globals and uninitialized values, the rank is 0.
     For function arguments, use the pre-setup rank.
     For PHI nodes, stores, asm statements, etc, we use the rank of
     the BB.
     For simple operations, the rank is the maximum rank of any of
     its operands, or the bb_rank, whichever is less.
     I make no claims that this is optimal, however, it gives good
     results.  */

  if (TREE_CODE (e) == SSA_NAME)
    {
      tree stmt;
      tree rhs;      
      unsigned int rank, maxrank;
      int i;
      
      if (TREE_CODE (SSA_NAME_VAR (e)) == PARM_DECL
	  && e == default_def (SSA_NAME_VAR (e)))
	return find_value_rank (e)->rank;
      
      stmt = SSA_NAME_DEF_STMT (e);
      if (bb_for_stmt (stmt) == NULL)
	return 0;
      
      if (TREE_CODE (stmt) != MODIFY_EXPR
	  || !ZERO_SSA_OPERANDS (stmt, SSA_OP_VIRTUAL_DEFS))
	return bb_rank[bb_for_stmt (stmt)->index];

      /* If we already have a rank for this expression, use that.  */
      vr = find_value_rank (e);
      if (vr)
	return vr->rank;

      /* Otherwise, find the maximum rank for the operands, or the bb
	 rank, whichever is less.   */
      rank = 0;
      maxrank = bb_rank[bb_for_stmt(stmt)->index];
      rhs = TREE_OPERAND (stmt, 1);
      if (TREE_CODE_LENGTH (TREE_CODE (rhs)) == 0)
	rank = MAX (rank, get_rank (rhs));
      else 
	{
	  for (i = 0; 
	       i < TREE_CODE_LENGTH (TREE_CODE (rhs)) 
		 && TREE_OPERAND (rhs, i)
		 && rank != maxrank; i++)
	    rank = MAX(rank, get_rank (TREE_OPERAND (rhs, i)));
	}
      
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Rank for ");
	  print_generic_expr (dump_file, e, 0);
	  fprintf (dump_file, " is %d\n", (rank + 1));
	}
      
      /* Note the rank in the hashtable so we don't recompute it.  */
      insert_value_rank (e, (rank + 1));
      return (rank + 1);
    }

  /* Globals, etc,  are rank 0 */
  return 0;
}


/* Decide whether we should transpose RHS and some operand of
   LHSDEFOP.
   If yes, then return true and set TAKEOP to the operand number of LHSDEFOP to
   switch RHS for.
   Otherwise, return false.  */

static bool
should_transpose (tree rhs ATTRIBUTE_UNUSED, 
		  unsigned int rhsrank,
		  tree lhsdefop, unsigned int *takeop)
{
  /* Attempt to expose the low ranked
     arguments to CSE if we have something like:
     a = <rank 2> + c (rank 1)
     b = a (rank 3) + d (rank 1)
     We want to transform this into:
     a = c + d
     b = <rank 2> + <rank 3>
     
     The op finding part wouldn't be necessary if
			 we could swap the operands above and not have
			 update_stmt change them back on us.
  */
  unsigned int lowrankop;
  unsigned int lowrank;
  unsigned int highrank;
  unsigned int highrankop;
  unsigned int temp;
  
  lowrankop = 0;
  *takeop = 1;
  lowrank = get_rank (TREE_OPERAND (lhsdefop, 0));
  temp = get_rank (TREE_OPERAND (lhsdefop, 1));
  highrank = temp;
  highrankop = 1;
  if (temp < lowrank)
    {
      lowrankop = 1;
      highrankop = 0;
      *takeop = 0;
      highrank = lowrank;
      lowrank = temp;
    }
  
  /* If highrank == lowrank, then we had something
     like:
     a = <rank 1> + <rank 1> 
     already, so there is no guarantee that
     swapping our argument in is going to be
     better.
     If we run reassoc twice, we could probably
     have a flag that switches this behavior on,
     so that we try once without it, and once with
     it, so that redundancy elimination sees it
     both ways.
  */		      
  
  if (lowrank == rhsrank && highrank != lowrank)
    return true;

  /* Also, see if the LHS's high ranked op should be switched with our
     RHS simply because it is greater in rank than our current RHS.  */
  if (TREE_CODE (TREE_OPERAND (lhsdefop, 0)) == SSA_NAME)
    {
      tree iop = SSA_NAME_DEF_STMT (TREE_OPERAND (lhsdefop, highrankop));
      if (TREE_CODE (iop) == MODIFY_EXPR)
	iop = TREE_OPERAND (iop, 1);
      if (TREE_CODE (iop) == TREE_CODE (lhsdefop))
	*takeop = 1;
      if (rhsrank < get_rank (TREE_OPERAND (lhsdefop, *takeop)))
	return true;
    }		  
  
  return false;
}

/* Attempt to reassociate the associative binary operator BEXPR, which
   is in the statement pointed to by CURRBSI.  Return true if we
   changed the statement.  */

static bool
reassociate_expr (tree bexpr, block_stmt_iterator *currbsi)
{
  tree lhs = TREE_OPERAND (bexpr, 0);
  tree rhs = TREE_OPERAND (bexpr, 1);
  tree lhsdef;
  tree lhsi;
  bool changed = false;
  unsigned int lhsrank = get_rank (lhs);
  unsigned int rhsrank = get_rank (rhs);

  /* I don't want to get into the business of floating point
     reassociation.  */
  if (!INTEGRAL_TYPE_P (TREE_TYPE (lhs))
      || !INTEGRAL_TYPE_P (TREE_TYPE (rhs)))
    return false;
    
  /* We want the greater ranked operand to be our "LHS" for simplicity
     sake.  There is no point in actually modifying the expression, as
     update_stmt will simply resort the operands anyway. */
  if (lhsrank < rhsrank)
    {
      tree temp;
      unsigned int temp1;
      temp = lhs;
      lhs = rhs;
      rhs = temp;
      temp1 = lhsrank;
      lhsrank = rhsrank;
      rhsrank = temp1;
    }

  /* If the high ranked operand is an SSA_NAME, and the binary
     operator is not something we've already seen somewhere else
     (i.e., it may be redundant), attempt to reassociate it.
     
     We can't reassociate expressions unless the expression we are
     going to reassociate with is only used in our current expression,
     or else we may screw up other computations, like so:

     a = b + c
     e = a + d
     
     g = a + f
     
     We cannot reassociate and rewrite the "a = ..." , 
     because that would change the value of the computation of 
     "g = a + f".  */
  if (TREE_CODE (lhs) == SSA_NAME && !find_seen_binop (lhs, rhs))
    {
      lhsdef = SSA_NAME_DEF_STMT (lhs);
      if (TREE_CODE (lhsdef) == MODIFY_EXPR)
	{
	  lhsi = TREE_OPERAND (lhsdef, 1);
	  if (TREE_CODE (lhsi) == TREE_CODE (bexpr))
	    {
	      use_operand_p use;
	      tree usestmt;
	      if (single_imm_use (lhs, &use, &usestmt))
		{
		  unsigned int takeop = 0;
		  unsigned int otherop = 1;
		  bool foundmatch = false;
		  bool foundrank = false;

		  /* If we can easily transpose this into an operation
		     we've already seen, let's do that.
		     otherwise, let's try to expose low ranked ops to
		     CSE.  */
		  if (find_seen_binop (TREE_OPERAND (lhsi, 1), rhs))
		    {
		      takeop = 0;
		      otherop = 1;
		      foundmatch = true;
		    }
		  else if (find_seen_binop (TREE_OPERAND (lhsi, 0),
					    rhs))
		    {
		      takeop = 1;
		      otherop = 0;
		      foundmatch = true;
		    }
		  else if (should_transpose (rhs, rhsrank, lhsi,
					     &takeop))
		    {
		      foundrank = true;
		    }		  
		  if (foundmatch || foundrank)
		    {
		      block_stmt_iterator lhsbsi = bsi_for_stmt (lhsdef);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "Reassociating by %s\n",
				   foundmatch ? "match" : "rank");
			  fprintf (dump_file, "Before LHS:");
			  print_generic_stmt (dump_file, lhsi, 0);
			  fprintf (dump_file, "Before curr expr:");
			  print_generic_stmt (dump_file, bexpr, 0);
			}
		      TREE_OPERAND (bexpr, 0) = TREE_OPERAND (lhsi, takeop);
		      TREE_OPERAND (lhsi, takeop) = rhs;
		      TREE_OPERAND (bexpr, 1) = TREE_OPERAND (lhsdef, 0);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "After LHS:");
			  print_generic_stmt (dump_file, lhsi, 0);
			  fprintf (dump_file, "After curr expr:");
			  print_generic_stmt (dump_file, bexpr, 0);
			}
		      bsi_move_before (&lhsbsi, currbsi);
		      update_stmt (lhsdef);
		      update_stmt (bsi_stmt (*currbsi));
		      lhsbsi = bsi_for_stmt (lhsdef);
		      update_stmt (bsi_stmt (lhsbsi));

		      /* If update_stmt didn't reorder our operands,
			 we'd like to recurse on the expression we
			 just reassociated and reassociate it
			 top-down, exposing further opportunities.
			 Unfortunately, update_stmt does reorder them,
			 so we can't do this cheaply.  */
		      if (!foundmatch)
			reassociate_stats.reassociated_by_rank++;
		      else
			reassociate_stats.reassociated_by_match++;
		      return true;
		    }
		}
	    }
	}
    }
  return changed;
}

/* Reassociate expressions in basic block BB and its dominator as
   children , return true if any
   expressions changed.  */

static bool
reassociate_bb (basic_block bb)
{
  bool changed = false;
  block_stmt_iterator bsi;
  basic_block son;

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree stmt = bsi_stmt (bsi);
      
      if (TREE_CODE (stmt) == MODIFY_EXPR)
	{
	  tree rhs = TREE_OPERAND (stmt, 1);
	  if (associative_tree_code (TREE_CODE (rhs)))
	    {
	      if (reassociate_expr (rhs, &bsi))
		{
		  changed = true;
		  update_stmt (stmt);		  
		}
	      insert_seen_binop (TREE_OPERAND (rhs, 0),
				 TREE_OPERAND (rhs, 1));
	    }
	}
    }
  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    {
      changed |= reassociate_bb (son);
    }
  return changed;  
}

	
static bool
do_reassoc (void)
{  
  bool changed = false;
  
  changed = reassociate_bb (ENTRY_BLOCK_PTR);

  return changed;  
}


/* Gate and execute functions for Reassociation.  */

static void
execute_reassoc (void)
{
  init_reassoc ();
  do_reassoc ();
  fini_reassoc ();
}

struct tree_opt_pass pass_reassoc =
{
  "reassoc",				/* name */
  NULL,				/* gate */
  execute_reassoc,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_REASSOC,				/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa | TODO_dump_func 
  | TODO_ggc_collect | TODO_verify_ssa, /* todo_flags_finish */
  0					/* letter */
};
