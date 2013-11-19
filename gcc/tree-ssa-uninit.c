/* Predicate aware uninitialized variable warning.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.
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
#include "tree.h"
#include "flags.h"
#include "tm_p.h"
#include "basic-block.h"
#include "function.h"
#include "gimple-pretty-print.h"
#include "bitmap.h"
#include "pointer-set.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssa.h"
#include "tree-inline.h"
#include "hashtab.h"
#include "tree-pass.h"
#include "diagnostic-core.h"

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
static struct pointer_set_t *possibly_undefined_names = 0;

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
              && pointer_set_contains (possibly_undefined_names, t)));
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
   warning text is in MSGID and LOCUS may contain a location or be null.
   WC is the warning code.  */

static void
warn_uninit (enum opt_code wc, tree t,
	     tree expr, tree var, const char *gmsgid, void *data)
{
  gimple context = (gimple) data;
  location_t location, cfun_loc;
  expanded_location xloc, floc;

  if (!has_undefined_value_p (t))
    return;

  /* TREE_NO_WARNING either means we already warned, or the front end
     wishes to suppress the warning.  */
  if ((context
       && (gimple_no_warning_p (context)
	   || (gimple_assign_single_p (context)
	       && TREE_NO_WARNING (gimple_assign_rhs1 (context)))))
      || TREE_NO_WARNING (expr))
    return;

  location = (context != NULL && gimple_has_location (context))
	     ? gimple_location (context)
	     : DECL_SOURCE_LOCATION (var);
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

  FOR_EACH_BB (bb)
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
			     stmt);
	      else if (warn_possibly_uninitialized)
		warn_uninit (OPT_Wmaybe_uninitialized, use,
			     SSA_NAME_VAR (use), SSA_NAME_VAR (use),
			     "%qD may be used uninitialized in this function",
			     stmt);
	    }

	  /* For memory the only cheap thing we can do is see if we
	     have a use of the default def of the virtual operand.
	     ???  Note that at -O0 we do not have virtual operands.
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
			     stmt);
	      else if (warn_possibly_uninitialized)
		warn_uninit (OPT_Wmaybe_uninitialized, use,
			     gimple_assign_rhs1 (stmt), base,
			     "%qE may be used uninitialized in this function",
			     stmt);
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
compute_uninit_opnds_pos (gimple phi)
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

/* Computes the control dependence chains (paths of edges)
   for DEP_BB up to the dominating basic block BB (the head node of a
   chain should be dominated by it).  CD_CHAINS is pointer to a
   dynamic array holding the result chains. CUR_CD_CHAIN is the current
   chain being computed.  *NUM_CHAINS is total number of chains.  The
   function returns true if the information is successfully computed,
   return false if there is no control dependence or not computed.  */

static bool
compute_control_dep_chain (basic_block bb, basic_block dep_bb,
                           vec<edge> *cd_chains,
                           size_t *num_chains,
                           vec<edge> *cur_cd_chain)
{
  edge_iterator ei;
  edge e;
  size_t i;
  bool found_cd_chain = false;
  size_t cur_chain_len = 0;

  if (EDGE_COUNT (bb->succs) < 2)
    return false;

  /* Could  use a set instead.  */
  cur_chain_len = cur_cd_chain->length ();
  if (cur_chain_len > MAX_CHAIN_LEN)
    return false;

  for (i = 0; i < cur_chain_len; i++)
    {
      edge e = (*cur_cd_chain)[i];
      /* cycle detected. */
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
              /* check path from next edge.  */
              break;
            }

          /* Now check if DEP_BB is indirectly control dependent on BB.  */
          if (compute_control_dep_chain (cd_bb, dep_bb, cd_chains,
                                         num_chains, cur_cd_chain))
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

typedef struct use_pred_info
{
  gimple cond;
  bool invert;
} *use_pred_info_t;



/* Converts the chains of control dependence edges into a set of
   predicates. A control dependence chain is represented by a vector
   edges. DEP_CHAINS points to an array of dependence chains.
   NUM_CHAINS is the size of the chain array. One edge in a dependence
   chain is mapped to predicate expression represented by use_pred_info_t
   type. One dependence chain is converted to a composite predicate that
   is the result of AND operation of use_pred_info_t mapped to each edge.
   A composite predicate is presented by a vector of use_pred_info_t. On
   return, *PREDS points to the resulting array of composite predicates.
   *NUM_PREDS is the number of composite predictes.  */

static bool
convert_control_dep_chain_into_preds (vec<edge> *dep_chains,
                                      size_t num_chains,
                                      vec<use_pred_info_t> **preds,
                                      size_t *num_preds)
{
  bool has_valid_pred = false;
  size_t i, j;
  if (num_chains == 0 || num_chains >= MAX_NUM_CHAINS)
    return false;

  /* Now convert the control dep chain into a set
     of predicates.  */
  typedef vec<use_pred_info_t> vec_use_pred_info_t_heap;
  *preds = XCNEWVEC (vec_use_pred_info_t_heap, num_chains);
  *num_preds = num_chains;

  for (i = 0; i < num_chains; i++)
    {
      vec<edge> one_cd_chain = dep_chains[i];

      has_valid_pred = false;
      for (j = 0; j < one_cd_chain.length (); j++)
        {
          gimple cond_stmt;
          gimple_stmt_iterator gsi;
          basic_block guard_bb;
          use_pred_info_t one_pred;
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
          if (gimple_code (cond_stmt) == GIMPLE_CALL
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
          if (gimple_code (cond_stmt) != GIMPLE_COND)
            {
              has_valid_pred = false;
              break;
            }
          one_pred = XNEW (struct use_pred_info);
          one_pred->cond = cond_stmt;
          one_pred->invert = !!(e->flags & EDGE_FALSE_VALUE);
          (*preds)[i].safe_push (one_pred);
	  has_valid_pred = true;
        }

      if (!has_valid_pred)
        break;
    }
  return has_valid_pred;
}

/* Computes all control dependence chains for USE_BB. The control
   dependence chains are then converted to an array of composite
   predicates pointed to by PREDS.  PHI_BB is the basic block of
   the phi whose result is used in USE_BB.  */

static bool
find_predicates (vec<use_pred_info_t> **preds,
                 size_t *num_preds,
                 basic_block phi_bb,
                 basic_block use_bb)
{
  size_t num_chains = 0, i;
  vec<edge> *dep_chains = 0;
  vec<edge> cur_chain = vNULL;
  bool has_valid_pred = false;
  basic_block cd_root = 0;

  typedef vec<edge> vec_edge_heap;
  dep_chains = XCNEWVEC (vec_edge_heap, MAX_NUM_CHAINS);

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

  compute_control_dep_chain (cd_root, use_bb,
                             dep_chains, &num_chains,
                             &cur_chain);

  has_valid_pred
      = convert_control_dep_chain_into_preds (dep_chains,
                                              num_chains,
                                              preds,
                                              num_preds);
  /* Free individual chain  */
  cur_chain.release ();
  for (i = 0; i < num_chains; i++)
    dep_chains[i].release ();
  free (dep_chains);
  return has_valid_pred;
}

/* Computes the set of incoming edges of PHI that have non empty
   definitions of a phi chain.  The collection will be done
   recursively on operands that are defined by phis. CD_ROOT
   is the control dependence root. *EDGES holds the result, and
   VISITED_PHIS is a pointer set for detecting cycles.  */

static void
collect_phi_def_edges (gimple phi, basic_block cd_root,
                       vec<edge> *edges,
                       struct pointer_set_t *visited_phis)
{
  size_t i, n;
  edge opnd_edge;
  tree opnd;

  if (pointer_set_insert (visited_phis, phi))
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
            collect_phi_def_edges (def, cd_root, edges,
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
find_def_preds (vec<use_pred_info_t> **preds,
                size_t *num_preds, gimple phi)
{
  size_t num_chains = 0, i, n;
  vec<edge> *dep_chains = 0;
  vec<edge> cur_chain = vNULL;
  vec<edge> def_edges = vNULL;
  bool has_valid_pred = false;
  basic_block phi_bb, cd_root = 0;
  struct pointer_set_t *visited_phis;

  typedef vec<edge> vec_edge_heap;
  dep_chains = XCNEWVEC (vec_edge_heap, MAX_NUM_CHAINS);

  phi_bb = gimple_bb (phi);
  /* First find the closest dominating bb to be
     the control dependence root  */
  cd_root = find_dom (phi_bb);
  if (!cd_root)
    return false;

  visited_phis = pointer_set_create ();
  collect_phi_def_edges (phi, cd_root, &def_edges, visited_phis);
  pointer_set_destroy (visited_phis);

  n = def_edges.length ();
  if (n == 0)
    return false;

  for (i = 0; i < n; i++)
    {
      size_t prev_nc, j;
      edge opnd_edge;

      opnd_edge = def_edges[i];
      prev_nc = num_chains;
      compute_control_dep_chain (cd_root, opnd_edge->src,
                                 dep_chains, &num_chains,
                                 &cur_chain);
      /* Free individual chain  */
      cur_chain.release ();

      /* Now update the newly added chains with
         the phi operand edge:  */
      if (EDGE_COUNT (opnd_edge->src->succs) > 1)
        {
          if (prev_nc == num_chains
              && num_chains < MAX_NUM_CHAINS)
            num_chains++;
          for (j = prev_nc; j < num_chains; j++)
            {
              dep_chains[j].safe_push (opnd_edge);
            }
        }
    }

  has_valid_pred
      = convert_control_dep_chain_into_preds (dep_chains,
                                              num_chains,
                                              preds,
                                              num_preds);
  for (i = 0; i < num_chains; i++)
    dep_chains[i].release ();
  free (dep_chains);
  return has_valid_pred;
}

/* Dumps the predicates (PREDS) for USESTMT.  */

static void
dump_predicates (gimple usestmt, size_t num_preds,
                 vec<use_pred_info_t> *preds,
                 const char* msg)
{
  size_t i, j;
  vec<use_pred_info_t> one_pred_chain;
  fprintf (dump_file, msg);
  print_gimple_stmt (dump_file, usestmt, 0, 0);
  fprintf (dump_file, "is guarded by :\n");
  /* do some dumping here:  */
  for (i = 0; i < num_preds; i++)
    {
      size_t np;

      one_pred_chain = preds[i];
      np = one_pred_chain.length ();

      for (j = 0; j < np; j++)
        {
          use_pred_info_t one_pred
              = one_pred_chain[j];
          if (one_pred->invert)
            fprintf (dump_file, " (.NOT.) ");
          print_gimple_stmt (dump_file, one_pred->cond, 0, 0);
          if (j < np - 1)
            fprintf (dump_file, "(.AND.)\n");
        }
      if (i < num_preds - 1)
        fprintf (dump_file, "(.OR.)\n");
    }
}

/* Destroys the predicate set *PREDS.  */

static void
destroy_predicate_vecs (size_t n,
                        vec<use_pred_info_t> * preds)
{
  size_t i, j;
  for (i = 0; i < n; i++)
    {
      for (j = 0; j < preds[i].length (); j++)
        free (preds[i][j]);
      preds[i].release ();
    }
  free (preds);
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
        result = INT_CST_LT_UNSIGNED (val, boundary);
      else
        {
          gcc_assert (cmpc == LE_EXPR);
          result = (tree_int_cst_equal (val, boundary)
                    || INT_CST_LT_UNSIGNED (val, boundary));
        }
    }
  else
    {
      if (cmpc == EQ_EXPR)
        result = tree_int_cst_equal (val, boundary);
      else if (cmpc == LT_EXPR)
        result = INT_CST_LT (val, boundary);
      else
        {
          gcc_assert (cmpc == LE_EXPR);
          result = (tree_int_cst_equal (val, boundary)
                    || INT_CST_LT (val, boundary));
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
find_matching_predicate_in_rest_chains (use_pred_info_t pred,
                                        vec<use_pred_info_t> *preds,
                                        size_t num_pred_chains)
{
  size_t i, j, n;

  /* trival case  */
  if (num_pred_chains == 1)
    return true;

  for (i = 1; i < num_pred_chains; i++)
    {
      bool found = false;
      vec<use_pred_info_t> one_chain = preds[i];
      n = one_chain.length ();
      for (j = 0; j < n; j++)
        {
          use_pred_info_t pred2
              = one_chain[j];
          /* can relax the condition comparison to not
             use address comparison. However, the most common
             case is that multiple control dependent paths share
             a common path prefix, so address comparison should
             be ok.  */

          if (pred2->cond == pred->cond
              && pred2->invert == pred->invert)
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
                         gimple phi,
                         unsigned uninit_opnds,
                         struct pointer_set_t *visited_phis);

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
prune_uninit_phi_opnds_in_unrealizable_paths (
    gimple phi, unsigned uninit_opnds,
    gimple flag_def, tree boundary_cst,
    enum tree_code cmp_code,
    struct pointer_set_t *visited_phis,
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
          gimple flag_arg_def, phi_arg_def;
          tree phi_arg;
          unsigned uninit_opnds_arg_phi;

          if (TREE_CODE (flag_arg) != SSA_NAME)
            return false;
          flag_arg_def = SSA_NAME_DEF_STMT (flag_arg);
          if (gimple_code (flag_arg_def) != GIMPLE_PHI)
            return false;

          phi_arg = gimple_phi_arg_def (phi, i);
          if (TREE_CODE (phi_arg) != SSA_NAME)
            return false;

          phi_arg_def = SSA_NAME_DEF_STMT (phi_arg);
          if (gimple_code (phi_arg_def) != GIMPLE_PHI)
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
          if (!prune_uninit_phi_opnds_in_unrealizable_paths (
                  phi_arg_def, uninit_opnds_arg_phi,
                  flag_arg_def, boundary_cst, cmp_code,
                  visited_phis, visited_flag_phis))
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
          if (gimple_code (opnd_def) == GIMPLE_PHI)
            {
              edge opnd_edge;
              unsigned uninit_opnds2
                  = compute_uninit_opnds_pos (opnd_def);
              gcc_assert (!MASK_EMPTY (uninit_opnds2));
              opnd_edge = gimple_phi_arg_edge (phi, i);
              if (!is_use_properly_guarded (phi,
                                            opnd_edge->src,
                                            opnd_def,
                                            uninit_opnds2,
                                            visited_phis))
                  return false;
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
use_pred_not_overlap_with_undef_path_pred (
    size_t num_preds,
    vec<use_pred_info_t> *preds,
    gimple phi, unsigned uninit_opnds,
    struct pointer_set_t *visited_phis)
{
  unsigned int i, n;
  gimple flag_def = 0;
  tree  boundary_cst = 0;
  enum tree_code cmp_code;
  bool swap_cond = false;
  bool invert = false;
  vec<use_pred_info_t> the_pred_chain;
  bitmap visited_flag_phis = NULL;
  bool all_pruned = false;

  gcc_assert (num_preds > 0);
  /* Find within the common prefix of multiple predicate chains
     a predicate that is a comparison of a flag variable against
     a constant.  */
  the_pred_chain = preds[0];
  n = the_pred_chain.length ();
  for (i = 0; i < n; i++)
    {
      gimple cond;
      tree cond_lhs, cond_rhs, flag = 0;

      use_pred_info_t the_pred
          = the_pred_chain[i];

      cond = the_pred->cond;
      invert = the_pred->invert;
      cond_lhs = gimple_cond_lhs (cond);
      cond_rhs = gimple_cond_rhs (cond);
      cmp_code = gimple_cond_code (cond);

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
          && find_matching_predicate_in_rest_chains (
              the_pred, preds, num_preds))
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
                                                             flag_def,
                                                             boundary_cst,
                                                             cmp_code,
                                                             visited_phis,
                                                             &visited_flag_phis);

  if (visited_flag_phis)
    BITMAP_FREE (visited_flag_phis);

  return all_pruned;
}

/* Returns true if TC is AND or OR */

static inline bool
is_and_or_or (enum tree_code tc, tree typ)
{
  return (tc == BIT_IOR_EXPR
          || (tc == BIT_AND_EXPR
              && (typ == 0 || TREE_CODE (typ) == BOOLEAN_TYPE)));
}

typedef struct norm_cond
{
  vec<gimple> conds;
  enum tree_code cond_code;
  bool invert;
} *norm_cond_t;


/* Normalizes gimple condition COND. The normalization follows
   UD chains to form larger condition expression trees. NORM_COND
   holds the normalized result. COND_CODE is the logical opcode
   (AND or OR) of the normalized tree.  */

static void
normalize_cond_1 (gimple cond,
                  norm_cond_t norm_cond,
                  enum tree_code cond_code)
{
  enum gimple_code gc;
  enum tree_code cur_cond_code;
  tree rhs1, rhs2;

  gc = gimple_code (cond);
  if (gc != GIMPLE_ASSIGN)
    {
      norm_cond->conds.safe_push (cond);
      return;
    }

  cur_cond_code = gimple_assign_rhs_code (cond);
  rhs1 = gimple_assign_rhs1 (cond);
  rhs2 = gimple_assign_rhs2 (cond);
  if (cur_cond_code == NE_EXPR)
    {
      if (integer_zerop (rhs2)
          && (TREE_CODE (rhs1) == SSA_NAME))
        normalize_cond_1 (
            SSA_NAME_DEF_STMT (rhs1),
            norm_cond, cond_code);
      else if (integer_zerop (rhs1)
               && (TREE_CODE (rhs2) == SSA_NAME))
        normalize_cond_1 (
            SSA_NAME_DEF_STMT (rhs2),
            norm_cond, cond_code);
      else
        norm_cond->conds.safe_push (cond);

      return;
    }

  if (is_and_or_or (cur_cond_code, TREE_TYPE (rhs1))
      && (cond_code == cur_cond_code || cond_code == ERROR_MARK)
      && (TREE_CODE (rhs1) == SSA_NAME && TREE_CODE (rhs2) == SSA_NAME))
    {
      normalize_cond_1 (SSA_NAME_DEF_STMT (rhs1),
                        norm_cond, cur_cond_code);
      normalize_cond_1 (SSA_NAME_DEF_STMT (rhs2),
                        norm_cond, cur_cond_code);
      norm_cond->cond_code = cur_cond_code;
    }
  else
    norm_cond->conds.safe_push (cond);
}

/* See normalize_cond_1 for details. INVERT is a flag to indicate
   if COND needs to be inverted or not.  */

static void
normalize_cond (gimple cond, norm_cond_t norm_cond, bool invert)
{
  enum tree_code cond_code;

  norm_cond->cond_code = ERROR_MARK;
  norm_cond->invert = false;
  norm_cond->conds.create (0);
  gcc_assert (gimple_code (cond) == GIMPLE_COND);
  cond_code = gimple_cond_code (cond);
  if (invert)
    cond_code = invert_tree_comparison (cond_code, false);

  if (cond_code == NE_EXPR)
    {
      if (integer_zerop (gimple_cond_rhs (cond))
          && (TREE_CODE (gimple_cond_lhs (cond)) == SSA_NAME))
        normalize_cond_1 (
            SSA_NAME_DEF_STMT (gimple_cond_lhs (cond)),
            norm_cond, ERROR_MARK);
      else if (integer_zerop (gimple_cond_lhs (cond))
               && (TREE_CODE (gimple_cond_rhs (cond)) == SSA_NAME))
        normalize_cond_1 (
            SSA_NAME_DEF_STMT (gimple_cond_rhs (cond)),
            norm_cond, ERROR_MARK);
      else
        {
          norm_cond->conds.safe_push (cond);
          norm_cond->invert = invert;
        }
    }
  else
    {
      norm_cond->conds.safe_push (cond);
      norm_cond->invert = invert;
    }

  gcc_assert (norm_cond->conds.length () == 1
              || is_and_or_or (norm_cond->cond_code, NULL));
}

/* Returns true if the domain for condition COND1 is a subset of
   COND2. REVERSE is a flag. when it is true the function checks
   if COND1 is a superset of COND2. INVERT1 and INVERT2 are flags
   to indicate if COND1 and COND2 need to be inverted or not.  */

static bool
is_gcond_subset_of (gimple cond1, bool invert1,
                    gimple cond2, bool invert2,
                    bool reverse)
{
  enum gimple_code gc1, gc2;
  enum tree_code cond1_code, cond2_code;
  gimple tmp;
  tree cond1_lhs, cond1_rhs, cond2_lhs, cond2_rhs;

  /* Take the short cut.  */
  if (cond1 == cond2)
    return true;

  if (reverse)
    {
      tmp = cond1;
      cond1 = cond2;
      cond2 = tmp;
    }

  gc1 = gimple_code (cond1);
  gc2 = gimple_code (cond2);

  if ((gc1 != GIMPLE_ASSIGN && gc1 != GIMPLE_COND)
      || (gc2 != GIMPLE_ASSIGN && gc2 != GIMPLE_COND))
    return cond1 == cond2;

  cond1_code = ((gc1 == GIMPLE_ASSIGN)
                ? gimple_assign_rhs_code (cond1)
                : gimple_cond_code (cond1));

  cond2_code = ((gc2 == GIMPLE_ASSIGN)
                ? gimple_assign_rhs_code (cond2)
                : gimple_cond_code (cond2));

  if (TREE_CODE_CLASS (cond1_code) != tcc_comparison
      || TREE_CODE_CLASS (cond2_code) != tcc_comparison)
    return false;

  if (invert1)
    cond1_code = invert_tree_comparison (cond1_code, false);
  if (invert2)
    cond2_code = invert_tree_comparison (cond2_code, false);

  cond1_lhs = ((gc1 == GIMPLE_ASSIGN)
               ? gimple_assign_rhs1 (cond1)
               : gimple_cond_lhs (cond1));
  cond1_rhs = ((gc1 == GIMPLE_ASSIGN)
               ? gimple_assign_rhs2 (cond1)
               : gimple_cond_rhs (cond1));
  cond2_lhs = ((gc2 == GIMPLE_ASSIGN)
               ? gimple_assign_rhs1 (cond2)
               : gimple_cond_lhs (cond2));
  cond2_rhs = ((gc2 == GIMPLE_ASSIGN)
               ? gimple_assign_rhs2 (cond2)
               : gimple_cond_rhs (cond2));

  /* Assuming const operands have been swapped to the
     rhs at this point of the analysis.  */

  if (cond1_lhs != cond2_lhs)
    return false;

  if (!is_gimple_constant (cond1_rhs)
      || TREE_CODE (cond1_rhs) != INTEGER_CST)
    return (cond1_rhs == cond2_rhs);

  if (!is_gimple_constant (cond2_rhs)
      || TREE_CODE (cond2_rhs) != INTEGER_CST)
    return (cond1_rhs == cond2_rhs);

  if (cond1_code == EQ_EXPR)
    return is_value_included_in (cond1_rhs,
                                 cond2_rhs, cond2_code);
  if (cond1_code == NE_EXPR || cond2_code == EQ_EXPR)
    return ((cond2_code == cond1_code)
            && tree_int_cst_equal (cond1_rhs, cond2_rhs));

  if (((cond1_code == GE_EXPR || cond1_code == GT_EXPR)
       && (cond2_code == LE_EXPR || cond2_code == LT_EXPR))
      || ((cond1_code == LE_EXPR || cond1_code == LT_EXPR)
          && (cond2_code == GE_EXPR || cond2_code == GT_EXPR)))
    return false;

  if (cond1_code != GE_EXPR && cond1_code != GT_EXPR
      && cond1_code != LE_EXPR && cond1_code != LT_EXPR)
    return false;

  if (cond1_code == GT_EXPR)
    {
      cond1_code = GE_EXPR;
      cond1_rhs = fold_binary (PLUS_EXPR, TREE_TYPE (cond1_rhs),
                               cond1_rhs,
                               fold_convert (TREE_TYPE (cond1_rhs),
                                             integer_one_node));
    }
  else if (cond1_code == LT_EXPR)
    {
      cond1_code = LE_EXPR;
      cond1_rhs = fold_binary (MINUS_EXPR, TREE_TYPE (cond1_rhs),
                               cond1_rhs,
                               fold_convert (TREE_TYPE (cond1_rhs),
                                             integer_one_node));
    }

  if (!cond1_rhs)
    return false;

  gcc_assert (cond1_code == GE_EXPR || cond1_code == LE_EXPR);

  if (cond2_code == GE_EXPR || cond2_code == GT_EXPR ||
      cond2_code == LE_EXPR || cond2_code == LT_EXPR)
    return is_value_included_in (cond1_rhs,
                                 cond2_rhs, cond2_code);
  else if (cond2_code == NE_EXPR)
    return
        (is_value_included_in (cond1_rhs,
                               cond2_rhs, cond2_code)
         && !is_value_included_in (cond2_rhs,
                                   cond1_rhs, cond1_code));
  return false;
}

/* Returns true if the domain of the condition expression 
   in COND is a subset of any of the sub-conditions
   of the normalized condtion NORM_COND.  INVERT is a flag
   to indicate of the COND needs to be inverted.
   REVERSE is a flag. When it is true, the check is reversed --
   it returns true if COND is a superset of any of the subconditions
   of NORM_COND.  */

static bool
is_subset_of_any (gimple cond, bool invert,
                  norm_cond_t norm_cond, bool reverse)
{
  size_t i;
  size_t len = norm_cond->conds.length ();

  for (i = 0; i < len; i++)
    {
      if (is_gcond_subset_of (cond, invert,
                              norm_cond->conds[i],
                              false, reverse))
        return true;
    }
  return false;
}

/* NORM_COND1 and NORM_COND2 are normalized logical/BIT OR
   expressions (formed by following UD chains not control
   dependence chains). The function returns true of domain
   of and expression NORM_COND1 is a subset of NORM_COND2's.
   The implementation is conservative, and it returns false if
   it the inclusion relationship may not hold.  */

static bool
is_or_set_subset_of (norm_cond_t norm_cond1,
                     norm_cond_t norm_cond2)
{
  size_t i;
  size_t len = norm_cond1->conds.length ();

  for (i = 0; i < len; i++)
    {
      if (!is_subset_of_any (norm_cond1->conds[i],
                             false, norm_cond2, false))
        return false;
    }
  return true;
}

/* NORM_COND1 and NORM_COND2 are normalized logical AND
   expressions (formed by following UD chains not control
   dependence chains). The function returns true of domain
   of and expression NORM_COND1 is a subset of NORM_COND2's.  */

static bool
is_and_set_subset_of (norm_cond_t norm_cond1,
                      norm_cond_t norm_cond2)
{
  size_t i;
  size_t len = norm_cond2->conds.length ();

  for (i = 0; i < len; i++)
    {
      if (!is_subset_of_any (norm_cond2->conds[i],
                             false, norm_cond1, true))
        return false;
    }
  return true;
}

/* Returns true of the domain if NORM_COND1 is a subset 
   of that of NORM_COND2. Returns false if it can not be 
   proved to be so.  */

static bool
is_norm_cond_subset_of (norm_cond_t norm_cond1,
                        norm_cond_t norm_cond2)
{
  size_t i;
  enum tree_code code1, code2;

  code1 = norm_cond1->cond_code;
  code2 = norm_cond2->cond_code;

  if (code1 == BIT_AND_EXPR)
    {
      /* Both conditions are AND expressions.  */
      if (code2 == BIT_AND_EXPR)
        return is_and_set_subset_of (norm_cond1, norm_cond2);
      /* NORM_COND1 is an AND expression, and NORM_COND2 is an OR
         expression. In this case, returns true if any subexpression
         of NORM_COND1 is a subset of any subexpression of NORM_COND2.  */
      else if (code2 == BIT_IOR_EXPR)
        {
          size_t len1;
          len1 = norm_cond1->conds.length ();
          for (i = 0; i < len1; i++)
            {
              gimple cond1 = norm_cond1->conds[i];
              if (is_subset_of_any (cond1, false, norm_cond2, false))
                return true;
            }
          return false;
        }
      else
        {
          gcc_assert (code2 == ERROR_MARK);
          gcc_assert (norm_cond2->conds.length () == 1);
          return is_subset_of_any (norm_cond2->conds[0],
                                   norm_cond2->invert, norm_cond1, true);
        }
    }
  /* NORM_COND1 is an OR expression  */
  else if (code1 == BIT_IOR_EXPR)
    {
      if (code2 != code1)
        return false;

      return is_or_set_subset_of (norm_cond1, norm_cond2);
    }
  else
    {
      gcc_assert (code1 == ERROR_MARK);
      gcc_assert (norm_cond1->conds.length () == 1);
      /* Conservatively returns false if NORM_COND1 is non-decomposible
         and NORM_COND2 is an AND expression.  */
      if (code2 == BIT_AND_EXPR)
        return false;

      if (code2 == BIT_IOR_EXPR)
        return is_subset_of_any (norm_cond1->conds[0],
                                 norm_cond1->invert, norm_cond2, false);

      gcc_assert (code2 == ERROR_MARK);
      gcc_assert (norm_cond2->conds.length () == 1);
      return is_gcond_subset_of (norm_cond1->conds[0],
                                 norm_cond1->invert,
                                 norm_cond2->conds[0],
                                 norm_cond2->invert, false);
    }
}

/* Returns true of the domain of single predicate expression
   EXPR1 is a subset of that of EXPR2. Returns false if it
   can not be proved.  */

static bool
is_pred_expr_subset_of (use_pred_info_t expr1,
                        use_pred_info_t expr2)
{
  gimple cond1, cond2;
  enum tree_code code1, code2;
  struct norm_cond norm_cond1, norm_cond2;
  bool is_subset = false;

  cond1 = expr1->cond;
  cond2 = expr2->cond;
  code1 = gimple_cond_code (cond1);
  code2 = gimple_cond_code (cond2);

  if (expr1->invert)
    code1 = invert_tree_comparison (code1, false);
  if (expr2->invert)
    code2 = invert_tree_comparison (code2, false);

  /* Fast path -- match exactly  */
  if ((gimple_cond_lhs (cond1) == gimple_cond_lhs (cond2))
      && (gimple_cond_rhs (cond1) == gimple_cond_rhs (cond2))
      && (code1 == code2))
    return true;

  /* Normalize conditions. To keep NE_EXPR, do not invert
     with both need inversion.  */
  normalize_cond (cond1, &norm_cond1, (expr1->invert));
  normalize_cond (cond2, &norm_cond2, (expr2->invert));

  is_subset = is_norm_cond_subset_of (&norm_cond1, &norm_cond2);

  /* Free memory  */
  norm_cond1.conds.release ();
  norm_cond2.conds.release ();
  return is_subset ;
}

/* Returns true if the domain of PRED1 is a subset
   of that of PRED2. Returns false if it can not be proved so.  */

static bool
is_pred_chain_subset_of (vec<use_pred_info_t> pred1,
                         vec<use_pred_info_t> pred2)
{
  size_t np1, np2, i1, i2;

  np1 = pred1.length ();
  np2 = pred2.length ();

  for (i2 = 0; i2 < np2; i2++)
    {
      bool found = false;
      use_pred_info_t info2
          = pred2[i2];
      for (i1 = 0; i1 < np1; i1++)
        {
          use_pred_info_t info1
              = pred1[i1];
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
   not a subset of any of the sub-domains of PREDS (
   corresponding to each individual chains in it), even
   though it may be still be a subset of whole domain
   of PREDS which is the union (ORed) of all its subdomains.
   In other words, the result is conservative.  */

static bool
is_included_in (vec<use_pred_info_t> one_pred,
                vec<use_pred_info_t> *preds,
                size_t n)
{
  size_t i;

  for (i = 0; i < n; i++)
    {
      if (is_pred_chain_subset_of (one_pred, preds[i]))
        return true;
    }

  return false;
}

/* compares two predicate sets PREDS1 and PREDS2 and returns
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
is_superset_of (vec<use_pred_info_t> *preds1,
                size_t n1,
                vec<use_pred_info_t> *preds2,
                size_t n2)
{
  size_t i;
  vec<use_pred_info_t> one_pred_chain;

  for (i = 0; i < n2; i++)
    {
      one_pred_chain = preds2[i];
      if (!is_included_in (one_pred_chain, preds1, n1))
        return false;
    }

  return true;
}

/* Comparison function used by qsort. It is used to
   sort predicate chains to allow predicate
   simplification.  */

static int
pred_chain_length_cmp (const void *p1, const void *p2)
{
  use_pred_info_t i1, i2;
  vec<use_pred_info_t>  const *chain1
      = (vec<use_pred_info_t>  const *)p1;
  vec<use_pred_info_t>  const *chain2
      = (vec<use_pred_info_t>  const *)p2;

  if (chain1->length () != chain2->length ())
    return (chain1->length () - chain2->length ());

  i1 = (*chain1)[0];
  i2 = (*chain2)[0];

  /* Allow predicates with similar prefix come together.  */
  if (!i1->invert && i2->invert)
    return -1;
  else if (i1->invert && !i2->invert)
    return 1;

  return gimple_uid (i1->cond) - gimple_uid (i2->cond);
}

/* x OR (!x AND y) is equivalent to x OR y.
   This function normalizes x1 OR (!x1 AND x2) OR (!x1 AND !x2 AND x3)
   into x1 OR x2 OR x3.  PREDS is the predicate chains, and N is
   the number of chains. Returns true if normalization happens.  */

static bool
normalize_preds (vec<use_pred_info_t> *preds, size_t *n)
{
  size_t i, j, ll;
  vec<use_pred_info_t> pred_chain;
  vec<use_pred_info_t> x = vNULL;
  use_pred_info_t xj = 0, nxj = 0;

  if (*n < 2)
    return false;

  /* First sort the chains in ascending order of lengths.  */
  qsort (preds, *n, sizeof (void *), pred_chain_length_cmp);
  pred_chain = preds[0];
  ll = pred_chain.length ();
  if (ll != 1)
   {
     if (ll == 2)
       {
         use_pred_info_t xx, yy, xx2, nyy;
         vec<use_pred_info_t> pred_chain2 = preds[1];
         if (pred_chain2.length () != 2)
           return false;

         /* See if simplification x AND y OR x AND !y is possible.  */
         xx = pred_chain[0];
         yy = pred_chain[1];
         xx2 = pred_chain2[0];
         nyy = pred_chain2[1];
         if (gimple_cond_lhs (xx->cond) != gimple_cond_lhs (xx2->cond)
             || gimple_cond_rhs (xx->cond) != gimple_cond_rhs (xx2->cond)
             || gimple_cond_code (xx->cond) != gimple_cond_code (xx2->cond)
             || (xx->invert != xx2->invert))
           return false;
         if (gimple_cond_lhs (yy->cond) != gimple_cond_lhs (nyy->cond)
             || gimple_cond_rhs (yy->cond) != gimple_cond_rhs (nyy->cond)
             || gimple_cond_code (yy->cond) != gimple_cond_code (nyy->cond)
             || (yy->invert == nyy->invert))
           return false;

         /* Now merge the first two chains.  */
         free (yy);
         free (nyy);
         free (xx2);
         pred_chain.release ();
         pred_chain2.release ();
         pred_chain.safe_push (xx);
         preds[0] = pred_chain;
         for (i = 1; i < *n - 1; i++)
           preds[i] = preds[i + 1];

         preds[*n - 1].create (0);
         *n = *n - 1;
       }
     else
       return false;
   }

  x.safe_push (pred_chain[0]);

  /* The loop extracts x1, x2, x3, etc from chains
     x1 OR (!x1 AND x2) OR (!x1 AND !x2 AND x3) OR ...  */
  for (i = 1; i < *n; i++)
    {
      pred_chain = preds[i];
      if (pred_chain.length () != i + 1)
        return false;

      for (j = 0; j < i; j++)
        {
          xj = x[j];
          nxj = pred_chain[j];

          /* Check if nxj is !xj  */
          if (gimple_cond_lhs (xj->cond) != gimple_cond_lhs (nxj->cond)
              || gimple_cond_rhs (xj->cond) != gimple_cond_rhs (nxj->cond)
              || gimple_cond_code (xj->cond) != gimple_cond_code (nxj->cond)
              || (xj->invert == nxj->invert))
            return false;
        }

      x.safe_push (pred_chain[i]);
    }

  /* Now normalize the pred chains using the extraced x1, x2, x3 etc.  */
  for (j = 0; j < *n; j++)
    {
      use_pred_info_t t;
      xj = x[j];

      t = XNEW (struct use_pred_info);
      *t = *xj;

      x[j] = t;
    }

  for (i = 0; i < *n; i++)
    {
      pred_chain = preds[i];
      for (j = 0; j < pred_chain.length (); j++)
        free (pred_chain[j]);
      pred_chain.release ();
      /* A new chain.  */
      pred_chain.safe_push (x[i]);
      preds[i] = pred_chain;
    }
  return true;
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
                         gimple phi,
                         unsigned uninit_opnds,
                         struct pointer_set_t *visited_phis)
{
  basic_block phi_bb;
  vec<use_pred_info_t> *preds = 0;
  vec<use_pred_info_t> *def_preds = 0;
  size_t num_preds = 0, num_def_preds = 0;
  bool has_valid_preds = false;
  bool is_properly_guarded = false;

  if (pointer_set_insert (visited_phis, phi))
    return false;

  phi_bb = gimple_bb (phi);

  if (is_non_loop_exit_postdominating (use_bb, phi_bb))
    return false;

  has_valid_preds = find_predicates (&preds, &num_preds,
                                     phi_bb, use_bb);

  if (!has_valid_preds)
    {
      destroy_predicate_vecs (num_preds, preds);
      return false;
    }

  if (dump_file)
    dump_predicates (use_stmt, num_preds, preds,
                     "\nUse in stmt ");

  has_valid_preds = find_def_preds (&def_preds,
                                    &num_def_preds, phi);

  if (has_valid_preds)
    {
      bool normed;
      if (dump_file)
        dump_predicates (phi, num_def_preds, def_preds,
                         "Operand defs of phi ");

      normed = normalize_preds (def_preds, &num_def_preds);
      if (normed && dump_file)
        {
          fprintf (dump_file, "\nNormalized to\n");
          dump_predicates (phi, num_def_preds, def_preds,
                           "Operand defs of phi ");
        }
      is_properly_guarded =
          is_superset_of (def_preds, num_def_preds,
                          preds, num_preds);
    }

  /* further prune the dead incoming phi edges. */
  if (!is_properly_guarded)
    is_properly_guarded
        = use_pred_not_overlap_with_undef_path_pred (
            num_preds, preds, phi, uninit_opnds, visited_phis);

  destroy_predicate_vecs (num_preds, preds);
  destroy_predicate_vecs (num_def_preds, def_preds);
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
find_uninit_use (gimple phi, unsigned uninit_opnds,
                 vec<gimple> *worklist,
		 struct pointer_set_t *added_to_worklist)
{
  tree phi_result;
  use_operand_p use_p;
  gimple use_stmt;
  imm_use_iterator iter;

  phi_result = gimple_phi_result (phi);

  FOR_EACH_IMM_USE_FAST (use_p, iter, phi_result)
    {
      struct pointer_set_t *visited_phis;
      basic_block use_bb;

      use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;

      visited_phis = pointer_set_create ();

      if (gimple_code (use_stmt) == GIMPLE_PHI)
	use_bb = gimple_phi_arg_edge (use_stmt,
				      PHI_ARG_INDEX_FROM_USE (use_p))->src;
      else
	use_bb = gimple_bb (use_stmt);

      if (is_use_properly_guarded (use_stmt,
                                   use_bb, 
                                   phi,
                                   uninit_opnds,
                                   visited_phis))
        {
          pointer_set_destroy (visited_phis);
          continue;
        }
      pointer_set_destroy (visited_phis);

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
      if (!pointer_set_insert (added_to_worklist,
                               use_stmt))
        {
          if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "[WORKLIST]: Update worklist with phi: ");
              print_gimple_stmt (dump_file, use_stmt, 0, 0);
            }

          worklist->safe_push (use_stmt);
          pointer_set_insert (possibly_undefined_names, phi_result);
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
warn_uninitialized_phi (gimple phi, vec<gimple> *worklist,
                        struct pointer_set_t *added_to_worklist)
{
  unsigned uninit_opnds;
  gimple uninit_use_stmt = 0;
  tree uninit_op;

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

  uninit_op = gimple_phi_arg_def (phi, MASK_FIRST_SET_BIT (uninit_opnds));
  if (SSA_NAME_VAR (uninit_op) == NULL_TREE)
    return;
  warn_uninit (OPT_Wmaybe_uninitialized, uninit_op, SSA_NAME_VAR (uninit_op),
	       SSA_NAME_VAR (uninit_op),
               "%qD may be used uninitialized in this function",
               uninit_use_stmt);

}


/* Entry point to the late uninitialized warning pass.  */

static unsigned int
execute_late_warn_uninitialized (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  vec<gimple> worklist = vNULL;
  struct pointer_set_t *added_to_worklist;

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  /* Re-do the plain uninitialized variable check, as optimization may have
     straightened control flow.  Do this first so that we don't accidentally
     get a "may be" warning when we'd have seen an "is" warning later.  */
  warn_uninitialized_vars (/*warn_possibly_uninitialized=*/1);

  timevar_push (TV_TREE_UNINIT);

  possibly_undefined_names = pointer_set_create ();
  added_to_worklist = pointer_set_create ();

  /* Initialize worklist  */
  FOR_EACH_BB (bb)
    for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
        gimple phi = gsi_stmt (gsi);
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
		pointer_set_insert (added_to_worklist, phi);
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
      gimple cur_phi = 0;
      cur_phi = worklist.pop ();
      warn_uninitialized_phi (cur_phi, &worklist, added_to_worklist);
    }

  worklist.release ();
  pointer_set_destroy (added_to_worklist);
  pointer_set_destroy (possibly_undefined_names);
  possibly_undefined_names = NULL;
  free_dominance_info (CDI_POST_DOMINATORS);
  timevar_pop (TV_TREE_UNINIT);
  return 0;
}

static bool
gate_warn_uninitialized (void)
{
  return warn_uninitialized != 0;
}

namespace {

const pass_data pass_data_late_warn_uninitialized =
{
  GIMPLE_PASS, /* type */
  "uninit", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
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
  bool gate () { return gate_warn_uninitialized (); }
  unsigned int execute () { return execute_late_warn_uninitialized (); }

}; // class pass_late_warn_uninitialized

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
     optimization we need to warn here about "may be uninitialized".
  */
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
  true, /* has_gate */
  true, /* has_execute */
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
  bool gate () { return gate_warn_uninitialized (); }
  unsigned int execute () { return execute_early_warn_uninitialized (); }

}; // class pass_early_warn_uninitialized

} // anon namespace

gimple_opt_pass *
make_pass_early_warn_uninitialized (gcc::context *ctxt)
{
  return new pass_early_warn_uninitialized (ctxt);
}


