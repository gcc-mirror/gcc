/* Predicate aware uninitialized variable warning.
   Copyright (C) 2001-2015 Free Software Foundation, Inc.
   Contributed by Xinliang David Li <davidxl@google.com>

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
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "flags.h"
#include "tm_p.h"
#include "predict.h"
#include "hard-reg-set.h"
#include "input.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "bitmap.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssa.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "diagnostic-core.h"
#include "params.h"
#include "tree-cfg.h"

/* This implements the pass that does predicate aware warning on uses of
   possibly uninitialized variables. The pass first collects the set of
   possibly uninitialized SSA names. For each such name, it walks through
   all its immediate uses. For each immediate use, it rebuilds the condition
   expression (the predicate) that guards the use. The predicate is then
   examined to see if the variable is always defined under that same condition.
   This is done either by pruning the unrealizable paths that lead to the
   default definitions or by checking if the predicate set that guards the
   defining paths is a superset of the use predicate.  */


/* Pointer set of potentially undefined ssa names, i.e.,
   ssa names that are defined by phi with operands that
   are not defined or potentially undefined.  */
static hash_set<tree> *possibly_undefined_names = 0;

/* Bit mask handling macros.  */
#define MASK_SET_BIT(mask, pos) mask |= (1 << pos)
#define MASK_TEST_BIT(mask, pos) (mask & (1 << pos))
#define MASK_EMPTY(mask) (mask == 0)

/* Returns the first bit position (starting from LSB)
   in mask that is non zero. Returns -1 if the mask is empty.  */
static int
get_mask_first_set_bit (unsigned mask)
{
  int pos = 0;
  if (mask == 0)
    return -1;

  while ((mask & (1 << pos)) == 0)
    pos++;

  return pos;
}
#define MASK_FIRST_SET_BIT(mask) get_mask_first_set_bit (mask)

/* Return true if T, an SSA_NAME, has an undefined value.  */
static bool
has_undefined_value_p (tree t)
{
  return (ssa_undefined_value_p (t)
          || (possibly_undefined_names
              && possibly_undefined_names->contains (t)));
}



/* Like has_undefined_value_p, but don't return true if TREE_NO_WARNING
   is set on SSA_NAME_VAR.  */

static inline bool
uninit_undefined_value_p (tree t) {
  if (!has_undefined_value_p (t))
    return false;
  if (SSA_NAME_VAR (t) && TREE_NO_WARNING (SSA_NAME_VAR (t)))
    return false;
  return true;
}

/* Emit warnings for uninitialized variables.  This is done in two passes.

   The first pass notices real uses of SSA names with undefined values.
   Such uses are unconditionally uninitialized, and we can be certain that
   such a use is a mistake.  This pass is run before most optimizations,
   so that we catch as many as we can.

   The second pass follows PHI nodes to find uses that are potentially
   uninitialized.  In this case we can't necessarily prove that the use
   is really uninitialized.  This pass is run after most optimizations,
   so that we thread as many jumps and possible, and delete as much dead
   code as possible, in order to reduce false positives.  We also look
   again for plain uninitialized variables, since optimization may have
   changed conditionally uninitialized to unconditionally uninitialized.  */

/* Emit a warning for EXPR based on variable VAR at the point in the
   program T, an SSA_NAME, is used being uninitialized.  The exact
   warning text is in MSGID and DATA is the gimple stmt with info about
   the location in source code. When DATA is a GIMPLE_PHI, PHIARG_IDX
   gives which argument of the phi node to take the location from.  WC
   is the warning code.  */

static void
warn_uninit (enum opt_code wc, tree t, tree expr, tree var,
	     const char *gmsgid, void *data, location_t phiarg_loc)
{
  gimple context = (gimple) data;
  location_t location, cfun_loc;
  expanded_location xloc, floc;

  /* Ignore COMPLEX_EXPR as initializing only a part of a complex
     turns in a COMPLEX_EXPR with the not initialized part being
     set to its previous (undefined) value.  */
  if (is_gimple_assign (context)
      && gimple_assign_rhs_code (context) == COMPLEX_EXPR)
    return;
  if (!has_undefined_value_p (t))
    return;

  /* Anonymous SSA_NAMEs shouldn't be uninitialized, but ssa_undefined_value_p
     can return true if the def stmt of anonymous SSA_NAME is COMPLEX_EXPR
     created for conversion from scalar to complex.  Use the underlying var of
     the COMPLEX_EXPRs real part in that case.  See PR71581.  */
  if (expr == NULL_TREE
      && var == NULL_TREE
      && SSA_NAME_VAR (t) == NULL_TREE
      && is_gimple_assign (SSA_NAME_DEF_STMT (t))
      && gimple_assign_rhs_code (SSA_NAME_DEF_STMT (t)) == COMPLEX_EXPR)
    {
      tree v = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (t));
      if (TREE_CODE (v) == SSA_NAME
	  && has_undefined_value_p (v)
	  && (integer_zerop (gimple_assign_rhs2 (SSA_NAME_DEF_STMT (t)))
	      || real_zerop (gimple_assign_rhs2 (SSA_NAME_DEF_STMT (t)))
	      || fixed_zerop (gimple_assign_rhs2 (SSA_NAME_DEF_STMT (t)))))
	{
	  expr = SSA_NAME_VAR (v);
	  var = expr;
	}
    }

  if (expr == NULL_TREE)
    return;

  /* TREE_NO_WARNING either means we already warned, or the front end
     wishes to suppress the warning.  */
  if ((context
       && (gimple_no_warning_p (context)
	   || (gimple_assign_single_p (context)
	       && TREE_NO_WARNING (gimple_assign_rhs1 (context)))))
      || TREE_NO_WARNING (expr))
    return;

  if (context != NULL && gimple_has_location (context))
    location = gimple_location (context);
  else if (phiarg_loc != UNKNOWN_LOCATION)
    location = phiarg_loc;
  else
    location = DECL_SOURCE_LOCATION (var);
  location = linemap_resolve_location (line_table, location,
				       LRK_SPELLING_LOCATION,
				       NULL);
  cfun_loc = DECL_SOURCE_LOCATION (cfun->decl);
  xloc = expand_location (location);
  floc = expand_location (cfun_loc);
  if (warning_at (location, wc, gmsgid, expr))
    {
      TREE_NO_WARNING (expr) = 1;

      if (location == DECL_SOURCE_LOCATION (var))
	return;
      if (xloc.file != floc.file
	  || linemap_location_before_p (line_table,
					location, cfun_loc)
	  || linemap_location_before_p (line_table,
					cfun->function_end_locus,
					location))
	inform (DECL_SOURCE_LOCATION (var), "%qD was declared here", var);
    }
}

static unsigned int
warn_uninitialized_vars (bool warn_possibly_uninitialized)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      bool always_executed = dominated_by_p (CDI_POST_DOMINATORS,
					     single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)), bb);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  use_operand_p use_p;
	  ssa_op_iter op_iter;
	  tree use;

	  if (is_gimple_debug (stmt))
	    continue;

	  /* We only do data flow with SSA_NAMEs, so that's all we
	     can warn about.  */
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, op_iter, SSA_OP_USE)
	    {
	      use = USE_FROM_PTR (use_p);
	      if (always_executed)
		warn_uninit (OPT_Wuninitialized, use,
			     SSA_NAME_VAR (use), SSA_NAME_VAR (use),
			     "%qD is used uninitialized in this function",
			     stmt, UNKNOWN_LOCATION);
	      else if (warn_possibly_uninitialized)
		warn_uninit (OPT_Wmaybe_uninitialized, use,
			     SSA_NAME_VAR (use), SSA_NAME_VAR (use),
			     "%qD may be used uninitialized in this function",
			     stmt, UNKNOWN_LOCATION);
	    }

	  /* For memory the only cheap thing we can do is see if we
	     have a use of the default def of the virtual operand.
	     ???  Not so cheap would be to use the alias oracle via
	     walk_aliased_vdefs, if we don't find any aliasing vdef
	     warn as is-used-uninitialized, if we don't find an aliasing
	     vdef that kills our use (stmt_kills_ref_p), warn as
	     may-be-used-uninitialized.  But this walk is quadratic and
	     so must be limited which means we would miss warning
	     opportunities.  */
	  use = gimple_vuse (stmt);
	  if (use
	      && gimple_assign_single_p (stmt)
	      && !gimple_vdef (stmt)
	      && SSA_NAME_IS_DEFAULT_DEF (use))
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);
	      tree base = get_base_address (rhs);

	      /* Do not warn if it can be initialized outside this function.  */
	      if (TREE_CODE (base) != VAR_DECL
		  || DECL_HARD_REGISTER (base)
		  || is_global_var (base))
		continue;

	      if (always_executed)
		warn_uninit (OPT_Wuninitialized, use,
			     gimple_assign_rhs1 (stmt), base,
			     "%qE is used uninitialized in this function",
			     stmt, UNKNOWN_LOCATION);
	      else if (warn_possibly_uninitialized)
		warn_uninit (OPT_Wmaybe_uninitialized, use,
			     gimple_assign_rhs1 (stmt), base,
			     "%qE may be used uninitialized in this function",
			     stmt, UNKNOWN_LOCATION);
	    }
	}
    }

  return 0;
}

/* Checks if the operand OPND of PHI is defined by
   another phi with one operand defined by this PHI,
   but the rest operands are all defined. If yes,
   returns true to skip this this operand as being
   redundant. Can be enhanced to be more general.  */

static bool
can_skip_redundant_opnd (tree opnd, gimple phi)
{
  gimple op_def;
  tree phi_def;
  int i, n;

  phi_def = gimple_phi_result (phi);
  op_def = SSA_NAME_DEF_STMT (opnd);
  if (gimple_code (op_def) != GIMPLE_PHI)
    return false;
  n = gimple_phi_num_args (op_def);
  for (i = 0; i < n; ++i)
    {
      tree op = gimple_phi_arg_def (op_def, i);
      if (TREE_CODE (op) != SSA_NAME)
        continue;
      if (op != phi_def && uninit_undefined_value_p (op))
        return false;
    }

  return true;
}

/* Returns a bit mask holding the positions of arguments in PHI
   that have empty (or possibly empty) definitions.  */

static unsigned
compute_uninit_opnds_pos (gphi *phi)
{
  size_t i, n;
  unsigned uninit_opnds = 0;

  n = gimple_phi_num_args (phi);
  /* Bail out for phi with too many args.  */
  if (n > 32)
    return 0;

  for (i = 0; i < n; ++i)
    {
      tree op = gimple_phi_arg_def (phi, i);
      if (TREE_CODE (op) == SSA_NAME
          && uninit_undefined_value_p (op)
          && !can_skip_redundant_opnd (op, phi))
	{
          if (cfun->has_nonlocal_label || cfun->calls_setjmp)
	    {
	      /* Ignore SSA_NAMEs that appear on abnormal edges
		 somewhere.  */
	      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
		continue;
	    }
	  MASK_SET_BIT (uninit_opnds, i);
	}
    }
  return uninit_opnds;
}

/* Find the immediate postdominator PDOM of the specified
   basic block BLOCK.  */

static inline basic_block
find_pdom (basic_block block)
{
   if (block == EXIT_BLOCK_PTR_FOR_FN (cfun))
     return EXIT_BLOCK_PTR_FOR_FN (cfun);
   else
     {
       basic_block bb
           = get_immediate_dominator (CDI_POST_DOMINATORS, block);
       if (! bb)
	 return EXIT_BLOCK_PTR_FOR_FN (cfun);
       return bb;
     }
}

/* Find the immediate DOM of the specified
   basic block BLOCK.  */

static inline basic_block
find_dom (basic_block block)
{
   if (block == ENTRY_BLOCK_PTR_FOR_FN (cfun))
     return ENTRY_BLOCK_PTR_FOR_FN (cfun);
   else
     {
       basic_block bb = get_immediate_dominator (CDI_DOMINATORS, block);
       if (! bb)
	 return ENTRY_BLOCK_PTR_FOR_FN (cfun);
       return bb;
     }
}

/* Returns true if BB1 is postdominating BB2 and BB1 is
   not a loop exit bb. The loop exit bb check is simple and does
   not cover all cases.  */

static bool
is_non_loop_exit_postdominating (basic_block bb1, basic_block bb2)
{
  if (!dominated_by_p (CDI_POST_DOMINATORS, bb2, bb1))
    return false;

  if (single_pred_p (bb1) && !single_succ_p (bb2))
    return false;

  return true;
}

/* Find the closest postdominator of a specified BB, which is control
   equivalent to BB.  */

static inline  basic_block
find_control_equiv_block (basic_block bb)
{
  basic_block pdom;

  pdom = find_pdom (bb);

  /* Skip the postdominating bb that is also loop exit.  */
  if (!is_non_loop_exit_postdominating (pdom, bb))
    return NULL;

  if (dominated_by_p (CDI_DOMINATORS, pdom, bb))
    return pdom;

  return NULL;
}

#define MAX_NUM_CHAINS 8
#define MAX_CHAIN_LEN 5
#define MAX_POSTDOM_CHECK 8
#define MAX_SWITCH_CASES 40

/* Computes the control dependence chains (paths of edges)
   for DEP_BB up to the dominating basic block BB (the head node of a
   chain should be dominated by it).  CD_CHAINS is pointer to an
   array holding the result chains.  CUR_CD_CHAIN is the current
   chain being computed.  *NUM_CHAINS is total number of chains.  The
   function returns true if the information is successfully computed,
   return false if there is no control dependence or not computed.  */

static bool
compute_control_dep_chain (basic_block bb, basic_block dep_bb,
                           vec<edge> *cd_chains,
                           size_t *num_chains,
			   vec<edge> *cur_cd_chain,
			   int *num_calls)
{
  edge_iterator ei;
  edge e;
  size_t i;
  bool found_cd_chain = false;
  size_t cur_chain_len = 0;

  if (EDGE_COUNT (bb->succs) < 2)
    return false;

  if (*num_calls > PARAM_VALUE (PARAM_UNINIT_CONTROL_DEP_ATTEMPTS))
    return false;
  ++*num_calls;

  /* Could use a set instead.  */
  cur_chain_len = cur_cd_chain->length ();
  if (cur_chain_len > MAX_CHAIN_LEN)
    return false;

  for (i = 0; i < cur_chain_len; i++)
    {
      edge e = (*cur_cd_chain)[i];
      /* Cycle detected. */
      if (e->src == bb)
        return false;
    }

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      basic_block cd_bb;
      int post_dom_check = 0;
      if (e->flags & (EDGE_FAKE | EDGE_ABNORMAL))
        continue;

      cd_bb = e->dest;
      cur_cd_chain->safe_push (e);
      while (!is_non_loop_exit_postdominating (cd_bb, bb))
        {
          if (cd_bb == dep_bb)
            {
              /* Found a direct control dependence.  */
              if (*num_chains < MAX_NUM_CHAINS)
                {
                  cd_chains[*num_chains] = cur_cd_chain->copy ();
                  (*num_chains)++;
                }
              found_cd_chain = true;
              /* Check path from next edge.  */
              break;
            }

          /* Now check if DEP_BB is indirectly control dependent on BB.  */
          if (compute_control_dep_chain (cd_bb, dep_bb, cd_chains,
					 num_chains, cur_cd_chain, num_calls))
            {
              found_cd_chain = true;
              break;
            }

          cd_bb = find_pdom (cd_bb);
          post_dom_check++;
	  if (cd_bb == EXIT_BLOCK_PTR_FOR_FN (cfun) || post_dom_check >
	      MAX_POSTDOM_CHECK)
            break;
        }
      cur_cd_chain->pop ();
      gcc_assert (cur_cd_chain->length () == cur_chain_len);
    }
  gcc_assert (cur_cd_chain->length () == cur_chain_len);

  return found_cd_chain;
}

/* The type to represent a simple predicate  */

typedef struct use_def_pred_info
{
  tree pred_lhs;
  tree pred_rhs;
  enum tree_code cond_code;
  bool invert;
} pred_info;

/* The type to represent a sequence of predicates grouped
  with .AND. operation.  */

typedef vec<pred_info, va_heap, vl_ptr> pred_chain;

/* The type to represent a sequence of pred_chains grouped
  with .OR. operation.  */

typedef vec<pred_chain, va_heap, vl_ptr> pred_chain_union;

/* Converts the chains of control dependence edges into a set of
   predicates. A control dependence chain is represented by a vector
   edges. DEP_CHAINS points to an array of dependence chains.
   NUM_CHAINS is the size of the chain array. One edge in a dependence
   chain is mapped to predicate expression represented by pred_info
   type. One dependence chain is converted to a composite predicate that
   is the result of AND operation of pred_info mapped to each edge.
   A composite predicate is presented by a vector of pred_info. On
   return, *PREDS points to the resulting array of composite predicates.
   *NUM_PREDS is the number of composite predictes.  */

static bool
convert_control_dep_chain_into_preds (vec<edge> *dep_chains,
                                      size_t num_chains,
                                      pred_chain_union *preds)
{
  bool has_valid_pred = false;
  size_t i, j;
  if (num_chains == 0 || num_chains >= MAX_NUM_CHAINS)
    return false;

  /* Now convert the control dep chain into a set
     of predicates.  */
  preds->reserve (num_chains);

  for (i = 0; i < num_chains; i++)
    {
      vec<edge> one_cd_chain = dep_chains[i];

      has_valid_pred = false;
      pred_chain t_chain = vNULL;
      for (j = 0; j < one_cd_chain.length (); j++)
        {
          gimple cond_stmt;
          gimple_stmt_iterator gsi;
          basic_block guard_bb;
          pred_info one_pred;
          edge e;

          e = one_cd_chain[j];
          guard_bb = e->src;
          gsi = gsi_last_bb (guard_bb);
          if (gsi_end_p (gsi))
            {
              has_valid_pred = false;
              break;
            }
          cond_stmt = gsi_stmt (gsi);
          if (is_gimple_call (cond_stmt)
              && EDGE_COUNT (e->src->succs) >= 2)
            {
              /* Ignore EH edge. Can add assertion
                 on the other edge's flag.  */
              continue;
            }
          /* Skip if there is essentially one succesor.  */
          if (EDGE_COUNT (e->src->succs) == 2)
            {
              edge e1;
              edge_iterator ei1;
              bool skip = false;

              FOR_EACH_EDGE (e1, ei1, e->src->succs)
                {
                  if (EDGE_COUNT (e1->dest->succs) == 0)
                    {
                      skip = true;
                      break;
                    }
                }
              if (skip)
                continue;
            }
          if (gimple_code (cond_stmt) == GIMPLE_COND)
	    {
	      one_pred.pred_lhs = gimple_cond_lhs (cond_stmt);
	      one_pred.pred_rhs = gimple_cond_rhs (cond_stmt);
	      one_pred.cond_code = gimple_cond_code (cond_stmt);
	      one_pred.invert = !!(e->flags & EDGE_FALSE_VALUE);
	      t_chain.safe_push (one_pred);
	      has_valid_pred = true;
	    }
	  else if (gswitch *gs = dyn_cast <gswitch *> (cond_stmt))
	    {
	      /* Avoid quadratic behavior.  */
	      if (gimple_switch_num_labels (gs) > MAX_SWITCH_CASES)
		{
		  has_valid_pred = false;
		  break;
		}
	      /* Find the case label.  */
	      tree l = NULL_TREE;
	      unsigned idx;
	      for (idx = 0; idx < gimple_switch_num_labels (gs); ++idx)
		{
		  tree tl = gimple_switch_label (gs, idx);
		  if (e->dest == label_to_block (CASE_LABEL (tl)))
		    {
		      if (!l)
			l = tl;
		      else
			{
			  l = NULL_TREE;
			  break;
			}
		    }
		}
	      /* If more than one label reaches this block or the case
	         label doesn't have a single value (like the default one)
		 fail.  */
	      if (!l
		  || !CASE_LOW (l)
		  || (CASE_HIGH (l) && !operand_equal_p (CASE_LOW (l),
							 CASE_HIGH (l), 0)))
		{
		  has_valid_pred = false;
		  break;
		}
	      one_pred.pred_lhs = gimple_switch_index (gs);
	      one_pred.pred_rhs = CASE_LOW (l);
	      one_pred.cond_code = EQ_EXPR;
	      one_pred.invert = false;
	      t_chain.safe_push (one_pred);
	      has_valid_pred = true;
	    }
	  else
            {
              has_valid_pred = false;
              break;
            }
        }

      if (!has_valid_pred)
        break;
      else
        preds->safe_push (t_chain);
    }
  return has_valid_pred;
}

/* Computes all control dependence chains for USE_BB. The control
   dependence chains are then converted to an array of composite
   predicates pointed to by PREDS.  PHI_BB is the basic block of
   the phi whose result is used in USE_BB.  */

static bool
find_predicates (pred_chain_union *preds,
                 basic_block phi_bb,
                 basic_block use_bb)
{
  size_t num_chains = 0, i;
  int num_calls = 0;
  vec<edge> dep_chains[MAX_NUM_CHAINS];
  auto_vec<edge, MAX_CHAIN_LEN + 1> cur_chain;
  bool has_valid_pred = false;
  basic_block cd_root = 0;

  /* First find the closest bb that is control equivalent to PHI_BB
     that also dominates USE_BB.  */
  cd_root = phi_bb;
  while (dominated_by_p (CDI_DOMINATORS, use_bb, cd_root))
    {
      basic_block ctrl_eq_bb = find_control_equiv_block (cd_root);
      if (ctrl_eq_bb && dominated_by_p (CDI_DOMINATORS, use_bb, ctrl_eq_bb))
        cd_root = ctrl_eq_bb;
      else
        break;
    }

  compute_control_dep_chain (cd_root, use_bb, dep_chains, &num_chains,
			     &cur_chain, &num_calls);

  has_valid_pred
    = convert_control_dep_chain_into_preds (dep_chains, num_chains, preds);
  for (i = 0; i < num_chains; i++)
    dep_chains[i].release ();
  return has_valid_pred;
}

/* Computes the set of incoming edges of PHI that have non empty
   definitions of a phi chain.  The collection will be done
   recursively on operands that are defined by phis. CD_ROOT
   is the control dependence root. *EDGES holds the result, and
   VISITED_PHIS is a pointer set for detecting cycles.  */

static void
collect_phi_def_edges (gphi *phi, basic_block cd_root,
                       vec<edge> *edges,
                       hash_set<gimple> *visited_phis)
{
  size_t i, n;
  edge opnd_edge;
  tree opnd;

  if (visited_phis->add (phi))
    return;

  n = gimple_phi_num_args (phi);
  for (i = 0; i < n; i++)
    {
      opnd_edge = gimple_phi_arg_edge (phi, i);
      opnd = gimple_phi_arg_def (phi, i);

      if (TREE_CODE (opnd) != SSA_NAME)
        {
          if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "\n[CHECK] Found def edge %d in ", (int)i);
              print_gimple_stmt (dump_file, phi, 0, 0);
            }
          edges->safe_push (opnd_edge);
        }
      else
        {
          gimple def = SSA_NAME_DEF_STMT (opnd);

          if (gimple_code (def) == GIMPLE_PHI
              && dominated_by_p (CDI_DOMINATORS,
                                 gimple_bb (def), cd_root))
            collect_phi_def_edges (as_a <gphi *> (def), cd_root, edges,
                                   visited_phis);
          else if (!uninit_undefined_value_p (opnd))
            {
              if (dump_file && (dump_flags & TDF_DETAILS))
                {
                  fprintf (dump_file, "\n[CHECK] Found def edge %d in ", (int)i);
                  print_gimple_stmt (dump_file, phi, 0, 0);
                }
              edges->safe_push (opnd_edge);
            }
        }
    }
}

/* For each use edge of PHI, computes all control dependence chains.
   The control dependence chains are then converted to an array of
   composite predicates pointed to by PREDS.  */

static bool
find_def_preds (pred_chain_union *preds, gphi *phi)
{
  size_t num_chains = 0, i, n;
  vec<edge> dep_chains[MAX_NUM_CHAINS];
  auto_vec<edge, MAX_CHAIN_LEN + 1> cur_chain;
  vec<edge> def_edges = vNULL;
  bool has_valid_pred = false;
  basic_block phi_bb, cd_root = 0;

  phi_bb = gimple_bb (phi);
  /* First find the closest dominating bb to be
     the control dependence root  */
  cd_root = find_dom (phi_bb);
  if (!cd_root)
    return false;

  hash_set<gimple> visited_phis;
  collect_phi_def_edges (phi, cd_root, &def_edges, &visited_phis);

  n = def_edges.length ();
  if (n == 0)
    return false;

  for (i = 0; i < n; i++)
    {
      size_t prev_nc, j;
      int num_calls = 0;
      edge opnd_edge;

      opnd_edge = def_edges[i];
      prev_nc = num_chains;
      compute_control_dep_chain (cd_root, opnd_edge->src, dep_chains,
				 &num_chains, &cur_chain, &num_calls);

      /* Now update the newly added chains with
         the phi operand edge:  */
      if (EDGE_COUNT (opnd_edge->src->succs) > 1)
        {
	  if (prev_nc == num_chains && num_chains < MAX_NUM_CHAINS)
	    dep_chains[num_chains++] = vNULL;
          for (j = prev_nc; j < num_chains; j++)
	    dep_chains[j].safe_push (opnd_edge);
        }
    }

  has_valid_pred
    = convert_control_dep_chain_into_preds (dep_chains, num_chains, preds);
  for (i = 0; i < num_chains; i++)
    dep_chains[i].release ();
  return has_valid_pred;
}

/* Dumps the predicates (PREDS) for USESTMT.  */

static void
dump_predicates (gimple usestmt, pred_chain_union preds,
                 const char* msg)
{
  size_t i, j;
  pred_chain one_pred_chain = vNULL;
  fprintf (dump_file, "%s", msg);
  print_gimple_stmt (dump_file, usestmt, 0, 0);
  fprintf (dump_file, "is guarded by :\n\n");
  size_t num_preds = preds.length ();
  /* Do some dumping here:  */
  for (i = 0; i < num_preds; i++)
    {
      size_t np;

      one_pred_chain = preds[i];
      np = one_pred_chain.length ();

      for (j = 0; j < np; j++)
        {
          pred_info one_pred = one_pred_chain[j];
          if (one_pred.invert)
            fprintf (dump_file, " (.NOT.) ");
          print_generic_expr (dump_file, one_pred.pred_lhs, 0);
          fprintf (dump_file, " %s ", op_symbol_code (one_pred.cond_code));
          print_generic_expr (dump_file, one_pred.pred_rhs, 0);
          if (j < np - 1)
            fprintf (dump_file, " (.AND.) ");
          else
            fprintf (dump_file, "\n");
        }
      if (i < num_preds - 1)
        fprintf (dump_file, "(.OR.)\n");
      else
        fprintf (dump_file, "\n\n");
    }
}

/* Destroys the predicate set *PREDS.  */

static void
destroy_predicate_vecs (pred_chain_union preds)
{
  size_t i;

  size_t n = preds.length ();
  for (i = 0; i < n; i++)
    preds[i].release ();
  preds.release ();
}


/* Computes the 'normalized' conditional code with operand
   swapping and condition inversion.  */

static enum tree_code
get_cmp_code (enum tree_code orig_cmp_code,
              bool swap_cond, bool invert)
{
  enum tree_code tc = orig_cmp_code;

  if (swap_cond)
    tc = swap_tree_comparison (orig_cmp_code);
  if (invert)
    tc = invert_tree_comparison (tc, false);

  switch (tc)
    {
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      break;
    default:
      return ERROR_MARK;
    }
  return tc;
}

/* Returns true if VAL falls in the range defined by BOUNDARY and CMPC, i.e.
   all values in the range satisfies (x CMPC BOUNDARY) == true.  */

static bool
is_value_included_in (tree val, tree boundary, enum tree_code cmpc)
{
  bool inverted = false;
  bool is_unsigned;
  bool result;

  /* Only handle integer constant here.  */
  if (TREE_CODE (val) != INTEGER_CST
      || TREE_CODE (boundary) != INTEGER_CST)
    return true;

  is_unsigned = TYPE_UNSIGNED (TREE_TYPE (val));

  if (cmpc == GE_EXPR || cmpc == GT_EXPR
      || cmpc == NE_EXPR)
    {
      cmpc = invert_tree_comparison (cmpc, false);
      inverted = true;
    }

  if (is_unsigned)
    {
      if (cmpc == EQ_EXPR)
        result = tree_int_cst_equal (val, boundary);
      else if (cmpc == LT_EXPR)
        result = tree_int_cst_lt (val, boundary);
      else
        {
          gcc_assert (cmpc == LE_EXPR);
          result = tree_int_cst_le (val, boundary);
        }
    }
  else
    {
      if (cmpc == EQ_EXPR)
        result = tree_int_cst_equal (val, boundary);
      else if (cmpc == LT_EXPR)
        result = tree_int_cst_lt (val, boundary);
      else
        {
          gcc_assert (cmpc == LE_EXPR);
          result = (tree_int_cst_equal (val, boundary)
                    || tree_int_cst_lt (val, boundary));
        }
    }

  if (inverted)
    result ^= 1;

  return result;
}

/* Returns true if PRED is common among all the predicate
   chains (PREDS) (and therefore can be factored out).
   NUM_PRED_CHAIN is the size of array PREDS.  */

static bool
find_matching_predicate_in_rest_chains (pred_info pred,
                                        pred_chain_union preds,
                                        size_t num_pred_chains)
{
  size_t i, j, n;

  /* Trival case.  */
  if (num_pred_chains == 1)
    return true;

  for (i = 1; i < num_pred_chains; i++)
    {
      bool found = false;
      pred_chain one_chain = preds[i];
      n = one_chain.length ();
      for (j = 0; j < n; j++)
        {
          pred_info pred2 = one_chain[j];
          /* Can relax the condition comparison to not
             use address comparison. However, the most common
             case is that multiple control dependent paths share
             a common path prefix, so address comparison should
             be ok.  */

          if (operand_equal_p (pred2.pred_lhs, pred.pred_lhs, 0)
              && operand_equal_p (pred2.pred_rhs, pred.pred_rhs, 0)
              && pred2.invert == pred.invert)
            {
              found = true;
              break;
            }
        }
      if (!found)
        return false;
    }
  return true;
}

/* Forward declaration.  */
static bool
is_use_properly_guarded (gimple use_stmt,
                         basic_block use_bb,
                         gphi *phi,
                         unsigned uninit_opnds,
                         hash_set<gphi *> *visited_phis);

/* Returns true if all uninitialized opnds are pruned. Returns false
   otherwise. PHI is the phi node with uninitialized operands,
   UNINIT_OPNDS is the bitmap of the uninitialize operand positions,
   FLAG_DEF is the statement defining the flag guarding the use of the
   PHI output, BOUNDARY_CST is the const value used in the predicate
   associated with the flag, CMP_CODE is the comparison code used in
   the predicate, VISITED_PHIS is the pointer set of phis visited, and
   VISITED_FLAG_PHIS is the pointer to the pointer set of flag definitions
   that are also phis.

   Example scenario:

   BB1:
   flag_1 = phi <0, 1>                  // (1)
   var_1  = phi <undef, some_val>


   BB2:
   flag_2 = phi <0,   flag_1, flag_1>   // (2)
   var_2  = phi <undef, var_1, var_1>
   if (flag_2 == 1)
      goto BB3;

   BB3:
   use of var_2                         // (3)

   Because some flag arg in (1) is not constant, if we do not look into the
   flag phis recursively, it is conservatively treated as unknown and var_1
   is thought to be flowed into use at (3). Since var_1 is potentially uninitialized
   a false warning will be emitted. Checking recursively into (1), the compiler can
   find out that only some_val (which is defined) can flow into (3) which is OK.

*/

static bool
prune_uninit_phi_opnds_in_unrealizable_paths (gphi *phi,
					      unsigned uninit_opnds,
					      gphi *flag_def,
					      tree boundary_cst,
					      enum tree_code cmp_code,
					      hash_set<gphi *> *visited_phis,
					      bitmap *visited_flag_phis)
{
  unsigned i;

  for (i = 0; i < MIN (32, gimple_phi_num_args (flag_def)); i++)
    {
      tree flag_arg;

      if (!MASK_TEST_BIT (uninit_opnds, i))
        continue;

      flag_arg = gimple_phi_arg_def (flag_def, i);
      if (!is_gimple_constant (flag_arg))
        {
          gphi *flag_arg_def, *phi_arg_def;
          tree phi_arg;
          unsigned uninit_opnds_arg_phi;

          if (TREE_CODE (flag_arg) != SSA_NAME)
            return false;
          flag_arg_def = dyn_cast <gphi *> (SSA_NAME_DEF_STMT (flag_arg));
	  if (!flag_arg_def)
            return false;

          phi_arg = gimple_phi_arg_def (phi, i);
          if (TREE_CODE (phi_arg) != SSA_NAME)
            return false;

          phi_arg_def = dyn_cast <gphi *> (SSA_NAME_DEF_STMT (phi_arg));
	  if (!phi_arg_def)
            return false;

          if (gimple_bb (phi_arg_def) != gimple_bb (flag_arg_def))
            return false;

          if (!*visited_flag_phis)
            *visited_flag_phis = BITMAP_ALLOC (NULL);

          if (bitmap_bit_p (*visited_flag_phis,
                            SSA_NAME_VERSION (gimple_phi_result (flag_arg_def))))
            return false;

          bitmap_set_bit (*visited_flag_phis,
                          SSA_NAME_VERSION (gimple_phi_result (flag_arg_def)));

          /* Now recursively prune the uninitialized phi args.  */
          uninit_opnds_arg_phi = compute_uninit_opnds_pos (phi_arg_def);
          if (!prune_uninit_phi_opnds_in_unrealizable_paths
		 (phi_arg_def, uninit_opnds_arg_phi, flag_arg_def,
		  boundary_cst, cmp_code, visited_phis, visited_flag_phis))
            return false;

          bitmap_clear_bit (*visited_flag_phis,
                            SSA_NAME_VERSION (gimple_phi_result (flag_arg_def)));
          continue;
        }

      /* Now check if the constant is in the guarded range.  */
      if (is_value_included_in (flag_arg, boundary_cst, cmp_code))
        {
          tree opnd;
          gimple opnd_def;

          /* Now that we know that this undefined edge is not
             pruned. If the operand is defined by another phi,
             we can further prune the incoming edges of that
             phi by checking the predicates of this operands.  */

          opnd = gimple_phi_arg_def (phi, i);
          opnd_def = SSA_NAME_DEF_STMT (opnd);
          if (gphi *opnd_def_phi = dyn_cast <gphi *> (opnd_def))
            {
              edge opnd_edge;
              unsigned uninit_opnds2
                  = compute_uninit_opnds_pos (opnd_def_phi);
              if (!MASK_EMPTY (uninit_opnds2))
		{
		  opnd_edge = gimple_phi_arg_edge (phi, i);
		  if (!is_use_properly_guarded (phi,
						opnd_edge->src,
						opnd_def_phi,
						uninit_opnds2,
						visited_phis))
		    return false;
		}
            }
          else
            return false;
        }
    }

  return true;
}

/* A helper function that determines if the predicate set
   of the use is not overlapping with that of the uninit paths.
   The most common senario of guarded use is in Example 1:
     Example 1:
           if (some_cond)
           {
              x = ...;
              flag = true;
           }

            ... some code ...

           if (flag)
              use (x);

     The real world examples are usually more complicated, but similar
     and usually result from inlining:

         bool init_func (int * x)
         {
             if (some_cond)
                return false;
             *x  =  ..
             return true;
         }

         void foo(..)
         {
             int x;

             if (!init_func(&x))
                return;

             .. some_code ...
             use (x);
         }

     Another possible use scenario is in the following trivial example:

     Example 2:
          if (n > 0)
             x = 1;
          ...
          if (n > 0)
            {
              if (m < 2)
                 .. = x;
            }

     Predicate analysis needs to compute the composite predicate:

       1) 'x' use predicate: (n > 0) .AND. (m < 2)
       2) 'x' default value  (non-def) predicate: .NOT. (n > 0)
       (the predicate chain for phi operand defs can be computed
       starting from a bb that is control equivalent to the phi's
       bb and is dominating the operand def.)

       and check overlapping:
          (n > 0) .AND. (m < 2) .AND. (.NOT. (n > 0))
        <==> false

     This implementation provides framework that can handle
     scenarios. (Note that many simple cases are handled properly
     without the predicate analysis -- this is due to jump threading
     transformation which eliminates the merge point thus makes
     path sensitive analysis unnecessary.)

     NUM_PREDS is the number is the number predicate chains, PREDS is
     the array of chains, PHI is the phi node whose incoming (undefined)
     paths need to be pruned, and UNINIT_OPNDS is the bitmap holding
     uninit operand positions. VISITED_PHIS is the pointer set of phi
     stmts being checked.  */


static bool
use_pred_not_overlap_with_undef_path_pred (pred_chain_union preds,
				           gphi *phi, unsigned uninit_opnds,
					   hash_set<gphi *> *visited_phis)
{
  unsigned int i, n;
  gimple flag_def = 0;
  tree  boundary_cst = 0;
  enum tree_code cmp_code;
  bool swap_cond = false;
  bool invert = false;
  pred_chain the_pred_chain = vNULL;
  bitmap visited_flag_phis = NULL;
  bool all_pruned = false;
  size_t num_preds = preds.length ();

  gcc_assert (num_preds > 0);
  /* Find within the common prefix of multiple predicate chains
     a predicate that is a comparison of a flag variable against
     a constant.  */
  the_pred_chain = preds[0];
  n = the_pred_chain.length ();
  for (i = 0; i < n; i++)
    {
      tree cond_lhs, cond_rhs, flag = 0;

      pred_info the_pred = the_pred_chain[i];

      invert = the_pred.invert;
      cond_lhs = the_pred.pred_lhs;
      cond_rhs = the_pred.pred_rhs;
      cmp_code = the_pred.cond_code;

      if (cond_lhs != NULL_TREE && TREE_CODE (cond_lhs) == SSA_NAME
          && cond_rhs != NULL_TREE && is_gimple_constant (cond_rhs))
        {
          boundary_cst = cond_rhs;
          flag = cond_lhs;
        }
      else if (cond_rhs != NULL_TREE && TREE_CODE (cond_rhs) == SSA_NAME
               && cond_lhs != NULL_TREE && is_gimple_constant (cond_lhs))
        {
          boundary_cst = cond_lhs;
          flag = cond_rhs;
          swap_cond = true;
        }

      if (!flag)
        continue;

      flag_def = SSA_NAME_DEF_STMT (flag);

      if (!flag_def)
        continue;

      if ((gimple_code (flag_def) == GIMPLE_PHI)
          && (gimple_bb (flag_def) == gimple_bb (phi))
          && find_matching_predicate_in_rest_chains (the_pred, preds,
						     num_preds))
        break;

      flag_def = 0;
    }

  if (!flag_def)
    return false;

  /* Now check all the uninit incoming edge has a constant flag value
     that is in conflict with the use guard/predicate.  */
  cmp_code = get_cmp_code (cmp_code, swap_cond, invert);

  if (cmp_code == ERROR_MARK)
    return false;

  all_pruned = prune_uninit_phi_opnds_in_unrealizable_paths (phi,
                                                             uninit_opnds,
                                                             as_a <gphi *> (flag_def),
                                                             boundary_cst,
                                                             cmp_code,
                                                             visited_phis,
                                                             &visited_flag_phis);

  if (visited_flag_phis)
    BITMAP_FREE (visited_flag_phis);

  return all_pruned;
}

/* The helper function returns true if two predicates X1 and X2
   are equivalent. It assumes the expressions have already
   properly re-associated.  */

static inline bool
pred_equal_p (pred_info x1, pred_info x2)
{
  enum tree_code c1, c2;
  if (!operand_equal_p (x1.pred_lhs, x2.pred_lhs, 0)
      || !operand_equal_p (x1.pred_rhs, x2.pred_rhs, 0))
    return false;

  c1 = x1.cond_code;
  if (x1.invert != x2.invert
      && TREE_CODE_CLASS (x2.cond_code) == tcc_comparison)
    c2 = invert_tree_comparison (x2.cond_code, false);
  else
    c2 = x2.cond_code;

  return c1 == c2;
}

/* Returns true if the predication is testing !=.  */

static inline bool
is_neq_relop_p (pred_info pred)
{

  return (pred.cond_code == NE_EXPR && !pred.invert) 
          || (pred.cond_code == EQ_EXPR && pred.invert);
}

/* Returns true if pred is of the form X != 0.  */

static inline bool 
is_neq_zero_form_p (pred_info pred)
{
  if (!is_neq_relop_p (pred) || !integer_zerop (pred.pred_rhs)
      || TREE_CODE (pred.pred_lhs) != SSA_NAME)
    return false;
  return true;
}

/* The helper function returns true if two predicates X1
   is equivalent to X2 != 0.  */

static inline bool
pred_expr_equal_p (pred_info x1, tree x2)
{
  if (!is_neq_zero_form_p (x1))
    return false;

  return operand_equal_p (x1.pred_lhs, x2, 0);
}

/* Returns true of the domain of single predicate expression
   EXPR1 is a subset of that of EXPR2. Returns false if it
   can not be proved.  */

static bool
is_pred_expr_subset_of (pred_info expr1, pred_info expr2)
{
  enum tree_code code1, code2;

  if (pred_equal_p (expr1, expr2))
    return true;

  if ((TREE_CODE (expr1.pred_rhs) != INTEGER_CST)
      || (TREE_CODE (expr2.pred_rhs) != INTEGER_CST))
    return false;

  if (!operand_equal_p (expr1.pred_lhs, expr2.pred_lhs, 0))
    return false;

  code1 = expr1.cond_code;
  if (expr1.invert)
    code1 = invert_tree_comparison (code1, false);
  code2 = expr2.cond_code;
  if (expr2.invert)
    code2 = invert_tree_comparison (code2, false);

  if ((code1 == EQ_EXPR || code1 == BIT_AND_EXPR)
      && code2 == BIT_AND_EXPR)
    return wi::eq_p (expr1.pred_rhs,
		     wi::bit_and (expr1.pred_rhs, expr2.pred_rhs));

  if (code1 != code2 && code2 != NE_EXPR)
    return false;

  if (is_value_included_in (expr1.pred_rhs, expr2.pred_rhs, code2))
    return true;

  return false;
}

/* Returns true if the domain of PRED1 is a subset
   of that of PRED2. Returns false if it can not be proved so.  */

static bool
is_pred_chain_subset_of (pred_chain pred1,
                         pred_chain pred2)
{
  size_t np1, np2, i1, i2;

  np1 = pred1.length ();
  np2 = pred2.length ();

  for (i2 = 0; i2 < np2; i2++)
    {
      bool found = false;
      pred_info info2 = pred2[i2];
      for (i1 = 0; i1 < np1; i1++)
        {
          pred_info info1 = pred1[i1];
          if (is_pred_expr_subset_of (info1, info2))
            {
              found = true;
              break;
            }
        }
      if (!found)
        return false;
    }
  return true;
}

/* Returns true if the domain defined by
   one pred chain ONE_PRED is a subset of the domain
   of *PREDS. It returns false if ONE_PRED's domain is
   not a subset of any of the sub-domains of PREDS
   (corresponding to each individual chains in it), even
   though it may be still be a subset of whole domain
   of PREDS which is the union (ORed) of all its subdomains.
   In other words, the result is conservative.  */

static bool
is_included_in (pred_chain one_pred, pred_chain_union preds)
{
  size_t i;
  size_t n = preds.length ();

  for (i = 0; i < n; i++)
    {
      if (is_pred_chain_subset_of (one_pred, preds[i]))
        return true;
    }

  return false;
}

/* Compares two predicate sets PREDS1 and PREDS2 and returns
   true if the domain defined by PREDS1 is a superset
   of PREDS2's domain. N1 and N2 are array sizes of PREDS1 and
   PREDS2 respectively. The implementation chooses not to build
   generic trees (and relying on the folding capability of the
   compiler), but instead performs brute force comparison of
   individual predicate chains (won't be a compile time problem
   as the chains are pretty short). When the function returns
   false, it does not necessarily mean *PREDS1 is not a superset
   of *PREDS2, but mean it may not be so since the analysis can
   not prove it. In such cases, false warnings may still be
   emitted.  */

static bool
is_superset_of (pred_chain_union preds1, pred_chain_union preds2)
{
  size_t i, n2;
  pred_chain one_pred_chain = vNULL;

  n2 = preds2.length ();

  for (i = 0; i < n2; i++)
    {
      one_pred_chain = preds2[i];
      if (!is_included_in (one_pred_chain, preds1))
        return false;
    }

  return true;
}

/* Returns true if TC is AND or OR.  */

static inline bool
is_and_or_or_p (enum tree_code tc, tree type)
{
  return (tc == BIT_IOR_EXPR
          || (tc == BIT_AND_EXPR
              && (type == 0 || TREE_CODE (type) == BOOLEAN_TYPE)));
}

/* Returns true if X1 is the negate of X2.  */

static inline bool
pred_neg_p (pred_info x1, pred_info x2)
{
  enum tree_code c1, c2;
  if (!operand_equal_p (x1.pred_lhs, x2.pred_lhs, 0)
      || !operand_equal_p (x1.pred_rhs, x2.pred_rhs, 0))
    return false;
      
  c1 = x1.cond_code;
  if (x1.invert == x2.invert)
    c2 = invert_tree_comparison (x2.cond_code, false);
  else
    c2 = x2.cond_code;

  return c1 == c2;
}

/* 1) ((x IOR y) != 0) AND (x != 0) is equivalent to (x != 0);
   2) (X AND Y) OR (!X AND Y) is equivalent to Y;
   3) X OR (!X AND Y) is equivalent to (X OR Y);
   4) ((x IAND y) != 0) || (x != 0 AND y != 0)) is equivalent to
      (x != 0 AND y != 0)
   5) (X AND Y) OR (!X AND Z) OR (!Y AND Z) is equivalent to
      (X AND Y) OR Z 

   PREDS is the predicate chains, and N is the number of chains.  */

/* Helper function to implement rule 1 above.  ONE_CHAIN is
   the AND predication to be simplified.  */

static void
simplify_pred (pred_chain *one_chain)
{
  size_t i, j, n;
  bool simplified = false;
  pred_chain s_chain = vNULL;

  n = one_chain->length ();

  for (i = 0; i < n; i++)
    {
      pred_info *a_pred = &(*one_chain)[i];

      if (!a_pred->pred_lhs)
        continue;
      if (!is_neq_zero_form_p (*a_pred))
        continue;

      gimple def_stmt = SSA_NAME_DEF_STMT (a_pred->pred_lhs);
      if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
        continue;
      if (gimple_assign_rhs_code (def_stmt) == BIT_IOR_EXPR)
        {
          for (j = 0; j < n; j++)
            {
              pred_info *b_pred = &(*one_chain)[j];

              if (!b_pred->pred_lhs)
                continue;
              if (!is_neq_zero_form_p (*b_pred))
                continue;

              if (pred_expr_equal_p (*b_pred, gimple_assign_rhs1 (def_stmt))
                  || pred_expr_equal_p (*b_pred, gimple_assign_rhs2 (def_stmt)))
                 {
                   /* Mark a_pred for removal.  */
                   a_pred->pred_lhs = NULL;
                   a_pred->pred_rhs = NULL;
                   simplified = true;
                   break;
                 }
            }
        }
    }

  if (!simplified)
     return;

  for (i = 0; i < n; i++)
    {
      pred_info *a_pred = &(*one_chain)[i];
      if (!a_pred->pred_lhs)
        continue;
      s_chain.safe_push (*a_pred);
    }

   one_chain->release ();
   *one_chain = s_chain;
}

/* The helper function implements the rule 2 for the
   OR predicate PREDS.

   2) (X AND Y) OR (!X AND Y) is equivalent to Y.  */

static bool
simplify_preds_2 (pred_chain_union *preds)
{
  size_t i, j, n;
  bool simplified = false;
  pred_chain_union s_preds = vNULL;

  /* (X AND Y) OR (!X AND Y) is equivalent to Y.  
     (X AND Y) OR (X AND !Y) is equivalent to X.  */

  n = preds->length ();
  for (i = 0; i < n; i++)
    {
      pred_info x, y;
      pred_chain *a_chain = &(*preds)[i];

      if (a_chain->length () != 2)
        continue;

      x = (*a_chain)[0];
      y = (*a_chain)[1];

      for (j = 0; j < n; j++)
        {
          pred_chain *b_chain;
          pred_info x2, y2;

          if (j == i)
            continue;

          b_chain = &(*preds)[j];
          if (b_chain->length () != 2)
            continue;

          x2 = (*b_chain)[0];
          y2 = (*b_chain)[1];

          if (pred_equal_p (x, x2) && pred_neg_p (y, y2))
            {
              /* Kill a_chain.  */
              a_chain->release ();
              b_chain->release ();
              b_chain->safe_push (x);
              simplified = true;
              break;
            }
          if (pred_neg_p (x, x2) && pred_equal_p (y, y2))
            {
              /* Kill a_chain.  */
              a_chain->release ();
              b_chain->release ();
              b_chain->safe_push (y);
              simplified = true;
              break;
            }
        }
    }
  /* Now clean up the chain.  */
  if (simplified)
    {
      for (i = 0; i < n; i++)
        {
          if ((*preds)[i].is_empty ())
            continue;
          s_preds.safe_push ((*preds)[i]);
        }
      preds->release ();
      (*preds) = s_preds;
      s_preds = vNULL;
    }

  return simplified;
}

/* The helper function implements the rule 2 for the
   OR predicate PREDS.

   3) x OR (!x AND y) is equivalent to x OR y.  */

static bool
simplify_preds_3 (pred_chain_union *preds)
{
  size_t i, j, n;
  bool simplified = false;

  /* Now iteratively simplify X OR (!X AND Z ..)
       into X OR (Z ...).  */

  n = preds->length ();
  if (n < 2)
    return false;

  for (i = 0; i < n; i++)
    {
      pred_info x;
      pred_chain *a_chain = &(*preds)[i];

      if (a_chain->length () != 1)
        continue;

      x = (*a_chain)[0];

      for (j = 0; j < n; j++)
        {
          pred_chain *b_chain;
          pred_info x2;
          size_t k;

          if (j == i)
            continue;

          b_chain = &(*preds)[j];
          if (b_chain->length () < 2)
            continue;

          for (k = 0; k < b_chain->length (); k++)
            {
              x2 = (*b_chain)[k];
              if (pred_neg_p (x, x2))
                {
                  b_chain->unordered_remove (k);
                  simplified = true;
                  break;
                }
            }
        }
    }
  return simplified;
}

/* The helper function implements the rule 4 for the
   OR predicate PREDS.

   2) ((x AND y) != 0) OR (x != 0 AND y != 0) is equivalent to
       (x != 0 ANd y != 0).   */

static bool
simplify_preds_4 (pred_chain_union *preds)
{
  size_t i, j, n;
  bool simplified = false;
  pred_chain_union s_preds = vNULL;
  gimple def_stmt;

  n = preds->length ();
  for (i = 0; i < n; i++)
    {
      pred_info z;
      pred_chain *a_chain = &(*preds)[i];

      if (a_chain->length () != 1)
        continue;

      z = (*a_chain)[0];

      if (!is_neq_zero_form_p (z))
        continue;

      def_stmt = SSA_NAME_DEF_STMT (z.pred_lhs);
      if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
        continue;

      if (gimple_assign_rhs_code (def_stmt) != BIT_AND_EXPR)
        continue;

      for (j = 0; j < n; j++)
        {
          pred_chain *b_chain;
          pred_info x2, y2;

          if (j == i)
            continue;

          b_chain = &(*preds)[j];
          if (b_chain->length () != 2)
            continue;

          x2 = (*b_chain)[0];
          y2 = (*b_chain)[1];
          if (!is_neq_zero_form_p (x2)
              || !is_neq_zero_form_p (y2))
            continue;

          if ((pred_expr_equal_p (x2, gimple_assign_rhs1 (def_stmt))
               && pred_expr_equal_p (y2, gimple_assign_rhs2 (def_stmt)))
              || (pred_expr_equal_p (x2, gimple_assign_rhs2 (def_stmt))
                  && pred_expr_equal_p (y2, gimple_assign_rhs1 (def_stmt))))
            {
              /* Kill a_chain.  */
              a_chain->release ();
              simplified = true;
              break;
            }
        }
    }
  /* Now clean up the chain.  */
  if (simplified)
    {
      for (i = 0; i < n; i++)
        {
          if ((*preds)[i].is_empty ())
            continue;
          s_preds.safe_push ((*preds)[i]);
        }
      preds->release ();
      (*preds) = s_preds;
      s_preds = vNULL;
    }

  return simplified;
}


/* This function simplifies predicates in PREDS.  */

static void
simplify_preds (pred_chain_union *preds, gimple use_or_def, bool is_use)
{
  size_t i, n;
  bool changed = false;

  if (dump_file && dump_flags & TDF_DETAILS)
    {
      fprintf (dump_file, "[BEFORE SIMPLICATION -- ");
      dump_predicates (use_or_def, *preds, is_use ? "[USE]:\n" : "[DEF]:\n");
    }

  for (i = 0; i < preds->length (); i++)
    simplify_pred (&(*preds)[i]);

  n = preds->length ();
  if (n < 2)
    return;

  do
    {
      changed = false;
      if (simplify_preds_2 (preds))
        changed = true;

      /* Now iteratively simplify X OR (!X AND Z ..)
       into X OR (Z ...).  */
      if (simplify_preds_3 (preds))
        changed = true;

      if (simplify_preds_4 (preds))
        changed = true;

    } while (changed);

  return;
}

/* This is a helper function which attempts to normalize predicate chains
  by following UD chains. It basically builds up a big tree of either IOR
  operations or AND operations, and convert the IOR tree into a 
  pred_chain_union or BIT_AND tree into a pred_chain.
  Example:

  _3 = _2 RELOP1 _1;
  _6 = _5 RELOP2 _4;
  _9 = _8 RELOP3 _7;
  _10 = _3 | _6;
  _12 = _9 | _0;
  _t = _10 | _12;

 then _t != 0 will be normalized into a pred_chain_union

   (_2 RELOP1 _1) OR (_5 RELOP2 _4) OR (_8 RELOP3 _7) OR (_0 != 0)

 Similarly given,

  _3 = _2 RELOP1 _1;
  _6 = _5 RELOP2 _4;
  _9 = _8 RELOP3 _7;
  _10 = _3 & _6;
  _12 = _9 & _0;

 then _t != 0 will be normalized into a pred_chain:
   (_2 RELOP1 _1) AND (_5 RELOP2 _4) AND (_8 RELOP3 _7) AND (_0 != 0)
   
  */

/* This is a helper function that stores a PRED into NORM_PREDS.  */

inline static void
push_pred (pred_chain_union *norm_preds, pred_info pred)
{
  pred_chain pred_chain = vNULL;
  pred_chain.safe_push (pred);
  norm_preds->safe_push (pred_chain);
}

/* A helper function that creates a predicate of the form
   OP != 0 and push it WORK_LIST.  */

inline static void
push_to_worklist (tree op, vec<pred_info, va_heap, vl_ptr> *work_list,
                  hash_set<tree> *mark_set)
{
  if (mark_set->contains (op))
    return;
  mark_set->add (op);

  pred_info arg_pred;
  arg_pred.pred_lhs = op;
  arg_pred.pred_rhs = integer_zero_node;
  arg_pred.cond_code = NE_EXPR;
  arg_pred.invert = false;
  work_list->safe_push (arg_pred);
}

/* A helper that generates a pred_info from a gimple assignment
   CMP_ASSIGN with comparison rhs.  */

static pred_info
get_pred_info_from_cmp (gimple cmp_assign)
{
  pred_info n_pred;
  n_pred.pred_lhs = gimple_assign_rhs1 (cmp_assign);
  n_pred.pred_rhs = gimple_assign_rhs2 (cmp_assign);
  n_pred.cond_code = gimple_assign_rhs_code (cmp_assign);
  n_pred.invert = false;
  return n_pred;
}

/* Returns true if the PHI is a degenerated phi with
   all args with the same value (relop). In that case, *PRED
   will be updated to that value.  */

static bool
is_degenerated_phi (gimple phi, pred_info *pred_p)
{
  int i, n;
  tree op0;
  gimple def0;
  pred_info pred0;

  n = gimple_phi_num_args (phi);
  op0 = gimple_phi_arg_def (phi, 0);

  if (TREE_CODE (op0) != SSA_NAME)
    return false;

  def0 = SSA_NAME_DEF_STMT (op0);
  if (gimple_code (def0) != GIMPLE_ASSIGN)
    return false;
  if (TREE_CODE_CLASS (gimple_assign_rhs_code (def0))
      != tcc_comparison)
    return false;
  pred0 = get_pred_info_from_cmp (def0);

  for (i = 1; i < n; ++i)
    {
      gimple def;
      pred_info pred;
      tree op = gimple_phi_arg_def (phi, i);

      if (TREE_CODE (op) != SSA_NAME)
        return false;

      def = SSA_NAME_DEF_STMT (op);
      if (gimple_code (def) != GIMPLE_ASSIGN)
        return false;
      if (TREE_CODE_CLASS (gimple_assign_rhs_code (def))
          != tcc_comparison)
        return false;
      pred = get_pred_info_from_cmp (def);
      if (!pred_equal_p (pred, pred0))
        return false;
    }

  *pred_p = pred0;
  return true;
}

/* Normalize one predicate PRED  
   1) if PRED can no longer be normlized, put it into NORM_PREDS.
   2) otherwise if PRED is of the form x != 0, follow x's definition
      and put normalized predicates into WORK_LIST.  */
 
static void
normalize_one_pred_1 (pred_chain_union *norm_preds, 
                      pred_chain *norm_chain,
                      pred_info pred,
                      enum tree_code and_or_code,
                      vec<pred_info, va_heap, vl_ptr> *work_list,
		      hash_set<tree> *mark_set)
{
  if (!is_neq_zero_form_p (pred))
    {
      if (and_or_code == BIT_IOR_EXPR)
        push_pred (norm_preds, pred);
      else
        norm_chain->safe_push (pred);
      return;
    }

  gimple def_stmt = SSA_NAME_DEF_STMT (pred.pred_lhs);
 
  if (gimple_code (def_stmt) == GIMPLE_PHI
      && is_degenerated_phi (def_stmt, &pred))
    work_list->safe_push (pred);
  else if (gimple_code (def_stmt) == GIMPLE_PHI
           && and_or_code == BIT_IOR_EXPR)
    {
      int i, n;
      n = gimple_phi_num_args (def_stmt);

      /* If we see non zero constant, we should punt. The predicate
       * should be one guarding the phi edge.  */
      for (i = 0; i < n; ++i)
        {
          tree op = gimple_phi_arg_def (def_stmt, i);
          if (TREE_CODE (op) == INTEGER_CST && !integer_zerop (op))
            {
              push_pred (norm_preds, pred);
              return;
            }
        }

      for (i = 0; i < n; ++i)
        {
          tree op = gimple_phi_arg_def (def_stmt, i);
          if (integer_zerop (op))
            continue;

          push_to_worklist (op, work_list, mark_set);
        }
    }
  else if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
    {
      if (and_or_code == BIT_IOR_EXPR)
	push_pred (norm_preds, pred);
      else
	norm_chain->safe_push (pred);
    }
  else if (gimple_assign_rhs_code (def_stmt) == and_or_code)
    {
      /* Avoid splitting up bit manipulations like x & 3 or y | 1.  */
      if (is_gimple_min_invariant (gimple_assign_rhs2 (def_stmt)))
	{
	  /* But treat x & 3 as condition.  */
	  if (and_or_code == BIT_AND_EXPR)
	    {
	      pred_info n_pred;
	      n_pred.pred_lhs = gimple_assign_rhs1 (def_stmt);
	      n_pred.pred_rhs = gimple_assign_rhs2 (def_stmt);
	      n_pred.cond_code = and_or_code;
	      n_pred.invert = false;
	      norm_chain->safe_push (n_pred);
	    }
	}
      else
	{
	  push_to_worklist (gimple_assign_rhs1 (def_stmt), work_list, mark_set);
	  push_to_worklist (gimple_assign_rhs2 (def_stmt), work_list, mark_set);
	}
    }
  else if (TREE_CODE_CLASS (gimple_assign_rhs_code (def_stmt))
	   == tcc_comparison)
    {
      pred_info n_pred = get_pred_info_from_cmp (def_stmt);
      if (and_or_code == BIT_IOR_EXPR)
	push_pred (norm_preds, n_pred);
      else
	norm_chain->safe_push (n_pred);
    }
  else
    {
      if (and_or_code == BIT_IOR_EXPR)
	push_pred (norm_preds, pred);
      else
	norm_chain->safe_push (pred);
    }
}

/* Normalize PRED and store the normalized predicates into NORM_PREDS.  */

static void
normalize_one_pred (pred_chain_union *norm_preds,
                    pred_info pred)
{
  vec<pred_info, va_heap, vl_ptr> work_list = vNULL;
  enum tree_code and_or_code = ERROR_MARK;
  pred_chain norm_chain = vNULL;

  if (!is_neq_zero_form_p (pred))
    {
      push_pred (norm_preds, pred);
      return;
    }

  gimple def_stmt = SSA_NAME_DEF_STMT (pred.pred_lhs);
  if (gimple_code (def_stmt) == GIMPLE_ASSIGN)
    and_or_code = gimple_assign_rhs_code (def_stmt);
  if (and_or_code != BIT_IOR_EXPR
      && and_or_code != BIT_AND_EXPR)
    {
      if (TREE_CODE_CLASS (and_or_code)
          == tcc_comparison)
        {
          pred_info n_pred = get_pred_info_from_cmp (def_stmt);
          push_pred (norm_preds, n_pred);
        } 
       else
          push_pred (norm_preds, pred);
      return;
    }

  work_list.safe_push (pred);
  hash_set<tree> mark_set;

  while (!work_list.is_empty ())
    {
      pred_info a_pred = work_list.pop ();
      normalize_one_pred_1 (norm_preds, &norm_chain, a_pred,
                            and_or_code, &work_list, &mark_set);
    }
  if (and_or_code == BIT_AND_EXPR)
    norm_preds->safe_push (norm_chain);

  work_list.release ();
}

static void
normalize_one_pred_chain (pred_chain_union *norm_preds,
                          pred_chain one_chain)
{
  vec<pred_info, va_heap, vl_ptr> work_list = vNULL;
  hash_set<tree> mark_set;
  pred_chain norm_chain = vNULL;
  size_t i;

  for (i = 0; i < one_chain.length (); i++)
    {
      work_list.safe_push (one_chain[i]);
      mark_set.add (one_chain[i].pred_lhs);
    }

  while (!work_list.is_empty ())
    {
      pred_info a_pred = work_list.pop ();
      normalize_one_pred_1 (0, &norm_chain, a_pred,
                            BIT_AND_EXPR, &work_list, &mark_set);
    }

  norm_preds->safe_push (norm_chain);
  work_list.release ();
}

/* Normalize predicate chains PREDS and returns the normalized one.  */

static pred_chain_union
normalize_preds (pred_chain_union preds, gimple use_or_def, bool is_use)
{
  pred_chain_union norm_preds = vNULL;
  size_t n = preds.length ();
  size_t i;

  if (dump_file && dump_flags & TDF_DETAILS)
    {
      fprintf (dump_file, "[BEFORE NORMALIZATION --");
      dump_predicates (use_or_def, preds, is_use ? "[USE]:\n" : "[DEF]:\n");
    }

  for (i = 0; i < n; i++)
    {
      if (preds[i].length () != 1)
        normalize_one_pred_chain (&norm_preds, preds[i]);
      else
        {
          normalize_one_pred (&norm_preds, preds[i][0]);
          preds[i].release ();
        }
    }

  if (dump_file)
    {
      fprintf (dump_file, "[AFTER NORMALIZATION -- ");
      dump_predicates (use_or_def, norm_preds, is_use ? "[USE]:\n" : "[DEF]:\n");
    }

  preds.release ();
  return norm_preds;
}


/* Computes the predicates that guard the use and checks
   if the incoming paths that have empty (or possibly
   empty) definition can be pruned/filtered. The function returns
   true if it can be determined that the use of PHI's def in
   USE_STMT is guarded with a predicate set not overlapping with
   predicate sets of all runtime paths that do not have a definition.
   Returns false if it is not or it can not be determined. USE_BB is
   the bb of the use (for phi operand use, the bb is not the bb of
   the phi stmt, but the src bb of the operand edge). UNINIT_OPNDS
   is a bit vector. If an operand of PHI is uninitialized, the
   corresponding bit in the vector is 1.  VISIED_PHIS is a pointer
   set of phis being visted.  */

static bool
is_use_properly_guarded (gimple use_stmt,
                         basic_block use_bb,
                         gphi *phi,
                         unsigned uninit_opnds,
                         hash_set<gphi *> *visited_phis)
{
  basic_block phi_bb;
  pred_chain_union preds = vNULL;
  pred_chain_union def_preds = vNULL;
  bool has_valid_preds = false;
  bool is_properly_guarded = false;

  if (visited_phis->add (phi))
    return false;

  phi_bb = gimple_bb (phi);

  if (is_non_loop_exit_postdominating (use_bb, phi_bb))
    return false;

  has_valid_preds = find_predicates (&preds, phi_bb, use_bb);

  if (!has_valid_preds)
    {
      destroy_predicate_vecs (preds);
      return false;
    }

  /* Try to prune the dead incoming phi edges. */
  is_properly_guarded
    = use_pred_not_overlap_with_undef_path_pred (preds, phi, uninit_opnds,
						 visited_phis);

  if (is_properly_guarded)
    {
      destroy_predicate_vecs (preds);
      return true;
    }

  has_valid_preds = find_def_preds (&def_preds, phi);

  if (!has_valid_preds)
    {
      destroy_predicate_vecs (preds);
      destroy_predicate_vecs (def_preds);
      return false;
    }

  simplify_preds (&preds, use_stmt, true);
  preds = normalize_preds (preds, use_stmt, true);

  simplify_preds (&def_preds, phi, false);
  def_preds = normalize_preds (def_preds, phi, false);

  is_properly_guarded = is_superset_of (def_preds, preds);

  destroy_predicate_vecs (preds);
  destroy_predicate_vecs (def_preds);
  return is_properly_guarded;
}

/* Searches through all uses of a potentially
   uninitialized variable defined by PHI and returns a use
   statement if the use is not properly guarded. It returns
   NULL if all uses are guarded. UNINIT_OPNDS is a bitvector
   holding the position(s) of uninit PHI operands. WORKLIST
   is the vector of candidate phis that may be updated by this
   function. ADDED_TO_WORKLIST is the pointer set tracking
   if the new phi is already in the worklist.  */

static gimple
find_uninit_use (gphi *phi, unsigned uninit_opnds,
                 vec<gphi *> *worklist,
		 hash_set<gphi *> *added_to_worklist)
{
  tree phi_result;
  use_operand_p use_p;
  gimple use_stmt;
  imm_use_iterator iter;

  phi_result = gimple_phi_result (phi);

  FOR_EACH_IMM_USE_FAST (use_p, iter, phi_result)
    {
      basic_block use_bb;

      use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;

      if (gphi *use_phi = dyn_cast <gphi *> (use_stmt))
	use_bb = gimple_phi_arg_edge (use_phi,
				      PHI_ARG_INDEX_FROM_USE (use_p))->src;
      else
	use_bb = gimple_bb (use_stmt);

      hash_set<gphi *> visited_phis;
      if (is_use_properly_guarded (use_stmt, use_bb, phi, uninit_opnds,
                                   &visited_phis))
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "[CHECK]: Found unguarded use: ");
          print_gimple_stmt (dump_file, use_stmt, 0, 0);
        }
      /* Found one real use, return.  */
      if (gimple_code (use_stmt) != GIMPLE_PHI)
        return use_stmt;

      /* Found a phi use that is not guarded,
         add the phi to the worklist.  */
      if (!added_to_worklist->add (as_a <gphi *> (use_stmt)))
        {
          if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "[WORKLIST]: Update worklist with phi: ");
              print_gimple_stmt (dump_file, use_stmt, 0, 0);
            }

          worklist->safe_push (as_a <gphi *> (use_stmt));
          possibly_undefined_names->add (phi_result);
        }
    }

  return NULL;
}

/* Look for inputs to PHI that are SSA_NAMEs that have empty definitions
   and gives warning if there exists a runtime path from the entry to a
   use of the PHI def that does not contain a definition. In other words,
   the warning is on the real use. The more dead paths that can be pruned
   by the compiler, the fewer false positives the warning is. WORKLIST
   is a vector of candidate phis to be examined. ADDED_TO_WORKLIST is
   a pointer set tracking if the new phi is added to the worklist or not.  */

static void
warn_uninitialized_phi (gphi *phi, vec<gphi *> *worklist,
                        hash_set<gphi *> *added_to_worklist)
{
  unsigned uninit_opnds;
  gimple uninit_use_stmt = 0;
  tree uninit_op;
  int phiarg_index;
  location_t loc;

  /* Don't look at virtual operands.  */
  if (virtual_operand_p (gimple_phi_result (phi)))
    return;

  uninit_opnds = compute_uninit_opnds_pos (phi);

  if  (MASK_EMPTY (uninit_opnds))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "[CHECK]: examining phi: ");
      print_gimple_stmt (dump_file, phi, 0, 0);
    }

  /* Now check if we have any use of the value without proper guard.  */
  uninit_use_stmt = find_uninit_use (phi, uninit_opnds,
                                     worklist, added_to_worklist);

  /* All uses are properly guarded.  */
  if (!uninit_use_stmt)
    return;

  phiarg_index = MASK_FIRST_SET_BIT (uninit_opnds);
  uninit_op = gimple_phi_arg_def (phi, phiarg_index);
  if (SSA_NAME_VAR (uninit_op) == NULL_TREE)
    return;
  if (gimple_phi_arg_has_location (phi, phiarg_index))
    loc = gimple_phi_arg_location (phi, phiarg_index);
  else
    loc = UNKNOWN_LOCATION;
  warn_uninit (OPT_Wmaybe_uninitialized, uninit_op, SSA_NAME_VAR (uninit_op),
	       SSA_NAME_VAR (uninit_op),
               "%qD may be used uninitialized in this function",
               uninit_use_stmt, loc);

}

static bool
gate_warn_uninitialized (void)
{
  return warn_uninitialized || warn_maybe_uninitialized;
}

namespace {

const pass_data pass_data_late_warn_uninitialized =
{
  GIMPLE_PASS, /* type */
  "uninit", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_late_warn_uninitialized : public gimple_opt_pass
{
public:
  pass_late_warn_uninitialized (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_late_warn_uninitialized, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_late_warn_uninitialized (m_ctxt); }
  virtual bool gate (function *) { return gate_warn_uninitialized (); }
  virtual unsigned int execute (function *);

}; // class pass_late_warn_uninitialized

unsigned int
pass_late_warn_uninitialized::execute (function *fun)
{
  basic_block bb;
  gphi_iterator gsi;
  vec<gphi *> worklist = vNULL;

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  /* Re-do the plain uninitialized variable check, as optimization may have
     straightened control flow.  Do this first so that we don't accidentally
     get a "may be" warning when we'd have seen an "is" warning later.  */
  warn_uninitialized_vars (/*warn_possibly_uninitialized=*/1);

  timevar_push (TV_TREE_UNINIT);

  possibly_undefined_names = new hash_set<tree>;
  hash_set<gphi *> added_to_worklist;

  /* Initialize worklist  */
  FOR_EACH_BB_FN (bb, fun)
    for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gphi *phi = gsi.phi ();
	size_t n, i;

	n = gimple_phi_num_args (phi);

	/* Don't look at virtual operands.  */
	if (virtual_operand_p (gimple_phi_result (phi)))
	  continue;

	for (i = 0; i < n; ++i)
	  {
	    tree op = gimple_phi_arg_def (phi, i);
	    if (TREE_CODE (op) == SSA_NAME
		&& uninit_undefined_value_p (op))
	      {
		worklist.safe_push (phi);
		added_to_worklist.add (phi);
		if (dump_file && (dump_flags & TDF_DETAILS))
		  {
		    fprintf (dump_file, "[WORKLIST]: add to initial list: ");
		    print_gimple_stmt (dump_file, phi, 0, 0);
		  }
		break;
	      }
	  }
      }

  while (worklist.length () != 0)
    {
      gphi *cur_phi = 0;
      cur_phi = worklist.pop ();
      warn_uninitialized_phi (cur_phi, &worklist, &added_to_worklist);
    }

  worklist.release ();
  delete possibly_undefined_names;
  possibly_undefined_names = NULL;
  free_dominance_info (CDI_POST_DOMINATORS);
  timevar_pop (TV_TREE_UNINIT);
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_late_warn_uninitialized (gcc::context *ctxt)
{
  return new pass_late_warn_uninitialized (ctxt);
}


static unsigned int
execute_early_warn_uninitialized (void)
{
  /* Currently, this pass runs always but
     execute_late_warn_uninitialized only runs with optimization. With
     optimization we want to warn about possible uninitialized as late
     as possible, thus don't do it here.  However, without
     optimization we need to warn here about "may be uninitialized".  */
  calculate_dominance_info (CDI_POST_DOMINATORS);

  warn_uninitialized_vars (/*warn_possibly_uninitialized=*/!optimize);

  /* Post-dominator information can not be reliably updated. Free it
     after the use.  */

  free_dominance_info (CDI_POST_DOMINATORS);
  return 0;
}


namespace {

const pass_data pass_data_early_warn_uninitialized =
{
  GIMPLE_PASS, /* type */
  "*early_warn_uninitialized", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_UNINIT, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_early_warn_uninitialized : public gimple_opt_pass
{
public:
  pass_early_warn_uninitialized (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_warn_uninitialized, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return gate_warn_uninitialized (); }
  virtual unsigned int execute (function *)
    {
      return execute_early_warn_uninitialized ();
    }

}; // class pass_early_warn_uninitialized

} // anon namespace

gimple_opt_pass *
make_pass_early_warn_uninitialized (gcc::context *ctxt)
{
  return new pass_early_warn_uninitialized (ctxt);
}
