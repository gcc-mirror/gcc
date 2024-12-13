/* Lower GIMPLE_SWITCH expressions to something more efficient than
   a jump table.
   Copyright (C) 2006-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* This file handles the lowering of GIMPLE_SWITCH to an indexed
   load, or a series of bit-test-and-branch expressions.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-fold.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "alloc-pool.h"
#include "target.h"
#include "tree-into-ssa.h"
#include "omp-general.h"
#include "gimple-range.h"
#include "tree-cfgcleanup.h"
#include "hwint.h"
#include "internal-fn.h"
#include "diagnostic-core.h"

/* ??? For lang_hooks.types.type_for_mode, but is there a word_mode
   type in the GIMPLE type system that is language-independent?  */
#include "langhooks.h"

#include "tree-switch-conversion.h"

using namespace tree_switch_conversion;

/* Does the target have optabs needed to efficiently compute exact base two
   logarithm of a variable with type TYPE?

   If yes, returns TYPE.  If no, returns NULL_TREE.  May also return another
   type.  This indicates that logarithm of the variable can be computed but
   only after it is converted to this type.

   Also see gen_log2.  */

static tree
can_log2 (tree type, optimization_type opt_type)
{
  /* Check if target supports FFS for given type.  */
  if (direct_internal_fn_supported_p (IFN_FFS, type, opt_type))
    return type;

  /* Check if target supports FFS for some type we could convert to.  */
  int prec = TYPE_PRECISION (type);
  int i_prec = TYPE_PRECISION (integer_type_node);
  int li_prec = TYPE_PRECISION (long_integer_type_node);
  int lli_prec = TYPE_PRECISION (long_long_integer_type_node);
  tree new_type;
  if (prec <= i_prec
      && direct_internal_fn_supported_p (IFN_FFS, integer_type_node, opt_type))
    new_type = integer_type_node;
  else if (prec <= li_prec
	   && direct_internal_fn_supported_p (IFN_FFS, long_integer_type_node,
					      opt_type))
    new_type = long_integer_type_node;
  else if (prec <= lli_prec
	   && direct_internal_fn_supported_p (IFN_FFS,
					      long_long_integer_type_node,
					      opt_type))
    new_type = long_long_integer_type_node;
  else
    return NULL_TREE;
  return new_type;
}

/* Assume that OP is a power of two.  Build a sequence of gimple statements
   efficiently computing the base two logarithm of OP using special optabs.
   Return the ssa name represeting the result of the logarithm through RESULT.

   Before computing the logarithm, OP may have to be converted to another type.
   This should be specified in TYPE.  Use can_log2 to decide what this type
   should be.

   Should only be used if can_log2 doesn't reject the type of OP.  */

static gimple_seq
gen_log2 (tree op, location_t loc, tree *result, tree type)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi = gsi_last (stmts);

  tree orig_type = TREE_TYPE (op);
  tree tmp1;
  if (type != orig_type)
    tmp1 = gimple_convert (&gsi, false, GSI_NEW_STMT, loc, type, op);
  else
    tmp1 = op;
  /* Build FFS (op) - 1.  */
  tree tmp2 = gimple_build (&gsi, false, GSI_NEW_STMT, loc, IFN_FFS, orig_type,
			    tmp1);
  tree tmp3 = gimple_build (&gsi, false, GSI_NEW_STMT, loc, MINUS_EXPR,
			    orig_type, tmp2, build_one_cst (orig_type));
  *result = tmp3;
  return stmts;
}

/* Build a sequence of gimple statements checking that OP is a power of 2.
   Return the result as a boolean_type_node ssa name through RESULT.  Assumes
   that OP's value will be non-negative.  The generated check may give
   arbitrary answer for negative values.  */

static gimple_seq
gen_pow2p (tree op, location_t loc, tree *result)
{
  gimple_seq stmts = NULL;
  gimple_stmt_iterator gsi = gsi_last (stmts);

  tree type = TREE_TYPE (op);
  tree utype = unsigned_type_for (type);

  /* Build (op ^ (op - 1)) > (op - 1).  */
  tree tmp1;
  if (types_compatible_p (type, utype))
    tmp1 = op;
  else
    tmp1 = gimple_convert (&gsi, false, GSI_NEW_STMT, loc, utype, op);
  tree tmp2 = gimple_build (&gsi, false, GSI_NEW_STMT, loc, MINUS_EXPR, utype,
			    tmp1, build_one_cst (utype));
  tree tmp3 = gimple_build (&gsi, false, GSI_NEW_STMT, loc, BIT_XOR_EXPR,
			    utype, tmp1, tmp2);
  *result = gimple_build (&gsi, false, GSI_NEW_STMT, loc, GT_EXPR,
			  boolean_type_node, tmp3, tmp2);

  return stmts;
}


/* Constructor.  */

switch_conversion::switch_conversion (): m_final_bb (NULL),
  m_constructors (NULL), m_default_values (NULL),
  m_arr_ref_first (NULL), m_arr_ref_last (NULL),
  m_reason (NULL), m_default_case_nonstandard (false), m_cfg_altered (false),
  m_exp_index_transform_applied (false)
{
}

/* Collection information about SWTCH statement.  */

void
switch_conversion::collect (gswitch *swtch)
{
  unsigned int branch_num = gimple_switch_num_labels (swtch);
  tree min_case, max_case;
  unsigned int i;
  edge e, e_default, e_first;
  edge_iterator ei;

  m_switch = swtch;

  /* The gimplifier has already sorted the cases by CASE_LOW and ensured there
     is a default label which is the first in the vector.
     Collect the bits we can deduce from the CFG.  */
  m_index_expr = gimple_switch_index (swtch);
  m_switch_bb = gimple_bb (swtch);
  e_default = gimple_switch_default_edge (cfun, swtch);
  m_default_bb = e_default->dest;
  m_default_prob = e_default->probability;

  /* Get upper and lower bounds of case values, and the covered range.  */
  min_case = gimple_switch_label (swtch, 1);
  max_case = gimple_switch_label (swtch, branch_num - 1);

  m_range_min = CASE_LOW (min_case);
  if (CASE_HIGH (max_case) != NULL_TREE)
    m_range_max = CASE_HIGH (max_case);
  else
    m_range_max = CASE_LOW (max_case);

  m_contiguous_range = true;
  tree last = CASE_HIGH (min_case) ? CASE_HIGH (min_case) : m_range_min;
  for (i = 2; i < branch_num; i++)
    {
      tree elt = gimple_switch_label (swtch, i);
      if (wi::to_wide (last) + 1 != wi::to_wide (CASE_LOW (elt)))
	{
	  m_contiguous_range = false;
	  break;
	}
      last = CASE_HIGH (elt) ? CASE_HIGH (elt) : CASE_LOW (elt);
    }

  if (m_contiguous_range)
    e_first = gimple_switch_edge (cfun, swtch, 1);
  else
    e_first = e_default;

  /* See if there is one common successor block for all branch
     targets.  If it exists, record it in FINAL_BB.
     Start with the destination of the first non-default case
     if the range is contiguous and default case otherwise as
     guess or its destination in case it is a forwarder block.  */
  if (! single_pred_p (e_first->dest))
    m_final_bb = e_first->dest;
  else if (single_succ_p (e_first->dest)
	   && ! single_pred_p (single_succ (e_first->dest)))
    m_final_bb = single_succ (e_first->dest);
  /* Require that all switch destinations are either that common
     FINAL_BB or a forwarder to it, except for the default
     case if contiguous range.  */
  auto_vec<edge, 10> fw_edges;
  m_uniq = 0;
  if (m_final_bb)
    FOR_EACH_EDGE (e, ei, m_switch_bb->succs)
      {
	edge phi_e = nullptr;
	if (e->dest == m_final_bb)
	  phi_e = e;
	else if (single_pred_p (e->dest)
		 && single_succ_p (e->dest)
		 && single_succ (e->dest) == m_final_bb)
	  phi_e = single_succ_edge (e->dest);
	if (phi_e)
	  {
	    if (e == e_default)
	      ;
	    else if (phi_e == e || empty_block_p (e->dest))
	      {
		/* For empty blocks consider forwarders with equal
		   PHI arguments in m_final_bb as unique.  */
		unsigned i;
		for (i = 0; i < fw_edges.length (); ++i)
		  if (phi_alternatives_equal (m_final_bb, fw_edges[i], phi_e))
		    break;
		if (i == fw_edges.length ())
		  {
		    /* But limit the above possibly quadratic search.  */
		    if (fw_edges.length () < 10)
		      fw_edges.quick_push (phi_e);
		    m_uniq++;
		  }
	      }
	    else
	      m_uniq++;
	    continue;
	  }

	if (e == e_default && m_contiguous_range)
	  {
	    m_default_case_nonstandard = true;
	    continue;
	  }

	m_final_bb = NULL;
	break;
      }

  /* When there's not a single common successor block conservatively
     approximate the number of unique non-default targets.  */
  if (!m_final_bb)
    m_uniq = EDGE_COUNT (gimple_bb (swtch)->succs) - 1;

  m_range_size
    = int_const_binop (MINUS_EXPR, m_range_max, m_range_min);

  /* Get a count of the number of case labels.  Single-valued case labels
     simply count as one, but a case range counts double, since it may
     require two compares if it gets lowered as a branching tree.  */
  m_count = 0;
  for (i = 1; i < branch_num; i++)
    {
      tree elt = gimple_switch_label (swtch, i);
      m_count++;
      if (CASE_HIGH (elt)
	  && ! tree_int_cst_equal (CASE_LOW (elt), CASE_HIGH (elt)))
	m_count++;
    }
}

/* Check that the "exponential index transform" can be applied to this switch.

   See comment of the exp_index_transform function for details about this
   transformation.

   We want:
   - This form of the switch is more efficient
   - Cases are powers of 2

   Expects that SWTCH has at least one case.  */

bool
switch_conversion::is_exp_index_transform_viable (gswitch *swtch)
{
  tree index = gimple_switch_index (swtch);
  tree index_type = TREE_TYPE (index);
  basic_block swtch_bb = gimple_bb (swtch);
  unsigned num_labels = gimple_switch_num_labels (swtch);

  optimization_type opt_type = bb_optimization_type (swtch_bb);
  m_exp_index_transform_log2_type = can_log2 (index_type, opt_type);
  if (!m_exp_index_transform_log2_type)
    return false;

  /* Check that each case label corresponds only to one value
     (no case 1..3).  */
  unsigned i;
  for (i = 1; i < num_labels; i++)
    {
      tree label = gimple_switch_label (swtch, i);
      if (CASE_HIGH (label))
	return false;
    }

  /* Check that each label is nonnegative and a power of 2.  */
  for (i = 1; i < num_labels; i++)
    {
      tree label = gimple_switch_label (swtch, i);
      wide_int label_wi = wi::to_wide (CASE_LOW (label));
      if (!wi::ge_p (label_wi, 0, TYPE_SIGN (index_type)))
	return false;
      if (wi::exact_log2 (label_wi) == -1)
	return false;
    }

  if (dump_file)
    fprintf (dump_file, "Exponential index transform viable\n");

  return true;
}

/* Perform the "exponential index transform".

   Assume that cases of SWTCH are powers of 2.  The transformation replaces the
   cases by their exponents (2^k -> k).  It also inserts a statement that
   computes the exponent of the original index variable (basically taking the
   logarithm) and then sets the result as the new index variable.

   The transformation also inserts a conditional statement checking that the
   incoming original index variable is a power of 2 with the false edge leading
   to the default case.

   The exponential index transform shrinks the range of case numbers which
   helps switch conversion convert switches it otherwise could not.

   Consider for example:

   switch (i)
     {
       case (1 << 0): return 0;
       case (1 << 1): return 1;
       case (1 << 2): return 2;
       ...
       case (1 << 30): return 30;
       default: return 31;
     }

   First, exponential index transform gets applied.  Since each case becomes
   case x: return x;, the rest of switch conversion is then able to get rid of
   the switch statement.

   if (i is power of 2)
     return log2 (i);
   else
     return 31;

   */

void
switch_conversion::exp_index_transform (gswitch *swtch)
{
  if (dump_file)
    fprintf (dump_file, "Applying exponential index transform\n");

  tree index = gimple_switch_index (swtch);
  tree index_type = TREE_TYPE (index);
  basic_block swtch_bb = gimple_bb (swtch);
  unsigned num_labels = gimple_switch_num_labels (swtch);

  /* Insert a cond stmt that checks if the index variable is a power of 2.  */
  gimple_stmt_iterator gsi = gsi_for_stmt (swtch);
  gsi_prev (&gsi);
  gimple *foo = gsi_stmt (gsi);
  edge new_edge1 = split_block (swtch_bb, foo);

  swtch_bb = new_edge1->dest;
  basic_block cond_bb = new_edge1->src;
  new_edge1->flags |= EDGE_TRUE_VALUE;
  new_edge1->flags &= ~EDGE_FALLTHRU;
  new_edge1->probability = profile_probability::even ();

  basic_block default_bb = gimple_switch_default_bb (cfun, swtch);
  edge new_edge2 = make_edge (cond_bb, default_bb, EDGE_FALSE_VALUE);
  new_edge2->probability = profile_probability::even ();

  tree tmp;
  gimple_seq stmts = gen_pow2p (index, UNKNOWN_LOCATION, &tmp);
  gsi = gsi_last_bb (cond_bb);
  gsi_insert_seq_after (&gsi, stmts, GSI_LAST_NEW_STMT);
  gcond *stmt_cond = gimple_build_cond (NE_EXPR, tmp, boolean_false_node,
					NULL, NULL);
  gsi_insert_after (&gsi, stmt_cond, GSI_NEW_STMT);

  /* We just added an edge going to default bb so fix PHI nodes in that bb:
     For each PHI add new PHI arg.  It will be the same arg as when comming to
     the default bb from the switch bb.  */
  edge default_edge = find_edge (swtch_bb, default_bb);
  for (gphi_iterator gsi = gsi_start_phis (default_bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree arg = PHI_ARG_DEF_FROM_EDGE (phi, default_edge);
      location_t loc = gimple_phi_arg_location_from_edge (phi, default_edge);
      add_phi_arg (phi, arg, new_edge2, loc);
    }

  /* Insert a sequence of stmts that takes the log of the index variable.  */
  stmts = gen_log2 (index, UNKNOWN_LOCATION, &tmp,
		    m_exp_index_transform_log2_type);
  gsi = gsi_after_labels (swtch_bb);
  gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);

  /* Use the result of the logarithm as the new index variable.  */
  gimple_switch_set_index (swtch, tmp);
  update_stmt (swtch);

  /* Replace each case number with its logarithm.  */
  unsigned i;
  for (i = 1; i < num_labels; i++)
    {
      tree label = gimple_switch_label (swtch, i);
      CASE_LOW (label) = build_int_cst (index_type,
					tree_log2 (CASE_LOW (label)));
    }

  /* Fix the dominator tree, if it is available.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      /* Analysis of how dominators should look after we add the edge E going
	 from the cond block to the default block.

	 1 For the blocks between the switch block and the final block
	 (excluding the final block itself):  They had the switch block as
	 their immediate dominator.  That shouldn't change.

	 2 The final block may now have the switch block or the cond block as
	 its immediate dominator.  There's no easy way of knowing (consider
	 two cases where in both m_default_case_nonstandard = true, in one a
	 path through default intersects the final block and in one all paths
	 through default avoid the final block but intersect a successor of the
	 final block).

	 3 Other blocks that had the switch block as their immediate dominator
	 should now have the cond block as their immediate dominator.

	 4 Immediate dominators of the rest of the blocks shouldn't change.

	 Reasoning for 3 and 4:

	 We'll only consider blocks that do not fall into 1 or 2.

	 Consider a block X whose original imm dom was the switch block.  All
	 paths to X must also intersect the cond block since it's the only
	 pred of the switch block.  The final block doesn't dominate X so at
	 least one path P must lead through the default block.  Let P' be P but
	 instead of going through the switch block, take E.  The switch block
	 doesn't dominate X so its imm dom must now be the cond block.

	 Consider a block X whose original imm dom was Y != the switch block.
	 We only added an edge so all original paths to X are still present.
	 So X gained no new dominators.  Observe that Y still dominates X.
	 There would have to be a path that avoids Y otherwise.  But any block
	 we can avoid now except for the switch block we were able to avoid
	 before adding E.  */

      redirect_immediate_dominators (CDI_DOMINATORS, swtch_bb, cond_bb);

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, swtch_bb->succs)
	{
	  basic_block bb = e->dest;
	  if (bb == m_final_bb || bb == default_bb)
	    continue;
	  set_immediate_dominator (CDI_DOMINATORS, bb, swtch_bb);
	}

      vec<basic_block> v;
      v.create (1);
      v.quick_push (m_final_bb);
      iterate_fix_dominators (CDI_DOMINATORS, v, true);
    }

  /* Update information about the switch statement.  */
  tree first_label = gimple_switch_label (swtch, 1);
  tree last_label = gimple_switch_label (swtch, num_labels - 1);

  m_range_min = CASE_LOW (first_label);
  m_range_max = CASE_LOW (last_label);
  m_index_expr = gimple_switch_index (swtch);
  m_switch_bb = swtch_bb;

  m_range_size = int_const_binop (MINUS_EXPR, m_range_max, m_range_min);

  m_cfg_altered = true;

  m_contiguous_range = true;
  wide_int last_wi = wi::to_wide (CASE_LOW (first_label));
  for (i = 2; i < num_labels; i++)
    {
      tree label = gimple_switch_label (swtch, i);
      wide_int label_wi = wi::to_wide (CASE_LOW (label));
      m_contiguous_range &= wi::eq_p (wi::add (last_wi, 1), label_wi);
      last_wi = label_wi;
    }

  m_exp_index_transform_applied = true;
}

/* Checks whether the range given by individual case statements of the switch
   switch statement isn't too big and whether the number of branches actually
   satisfies the size of the new array.  */

bool
switch_conversion::check_range ()
{
  gcc_assert (m_range_size);
  if (!tree_fits_uhwi_p (m_range_size))
    {
      m_reason = "index range way too large or otherwise unusable";
      return false;
    }

  if (tree_to_uhwi (m_range_size)
      > ((unsigned) m_count * param_switch_conversion_branch_ratio))
    {
      m_reason = "the maximum range-branch ratio exceeded";
      return false;
    }

  return true;
}

/* Checks whether all but the final BB basic blocks are empty.  */

bool
switch_conversion::check_all_empty_except_final ()
{
  edge e, e_default = find_edge (m_switch_bb, m_default_bb);
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, m_switch_bb->succs)
    {
      if (e->dest == m_final_bb)
	continue;

      if (!empty_block_p (e->dest))
	{
	  if (m_contiguous_range && e == e_default)
	    {
	      m_default_case_nonstandard = true;
	      continue;
	    }

	  m_reason = "bad case - a non-final BB not empty";
	  return false;
	}
    }

  return true;
}

/* This function checks whether all required values in phi nodes in final_bb
   are constants.  Required values are those that correspond to a basic block
   which is a part of the examined switch statement.  It returns true if the
   phi nodes are OK, otherwise false.  */

bool
switch_conversion::check_final_bb ()
{
  gphi_iterator gsi;

  m_phi_count = 0;
  for (gsi = gsi_start_phis (m_final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      unsigned int i;

      if (virtual_operand_p (gimple_phi_result (phi)))
	continue;

      m_phi_count++;

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  basic_block bb = gimple_phi_arg_edge (phi, i)->src;

	  if (bb == m_switch_bb
	      || (single_pred_p (bb)
		  && single_pred (bb) == m_switch_bb
		  && (!m_default_case_nonstandard
		      || empty_block_p (bb))))
	    {
	      tree reloc, val;
	      const char *reason = NULL;

	      val = gimple_phi_arg_def (phi, i);
	      if (!is_gimple_ip_invariant (val))
		reason = "non-invariant value from a case";
	      else
		{
		  reloc = initializer_constant_valid_p (val, TREE_TYPE (val));
		  if ((flag_pic && reloc != null_pointer_node)
		      || (!flag_pic && reloc == NULL_TREE))
		    {
		      if (reloc)
			reason
			  = "value from a case would need runtime relocations";
		      else
			reason
			  = "value from a case is not a valid initializer";
		    }
		}
	      if (reason)
		{
		  /* For contiguous range, we can allow non-constant
		     or one that needs relocation, as long as it is
		     only reachable from the default case.  */
		  if (bb == m_switch_bb)
		    bb = m_final_bb;
		  if (!m_contiguous_range || bb != m_default_bb)
		    {
		      m_reason = reason;
		      return false;
		    }

		  unsigned int branch_num = gimple_switch_num_labels (m_switch);
		  for (unsigned int i = 1; i < branch_num; i++)
		    {
		      if (gimple_switch_label_bb (cfun, m_switch, i) == bb)
			{
			  m_reason = reason;
			  return false;
			}
		    }
		  m_default_case_nonstandard = true;
		}
	    }
	}
    }

  return true;
}

/* The following function allocates default_values, target_{in,out}_names and
   constructors arrays.  The last one is also populated with pointers to
   vectors that will become constructors of new arrays.  */

void
switch_conversion::create_temp_arrays ()
{
  int i;

  m_default_values = XCNEWVEC (tree, m_phi_count * 3);
  /* ??? Macros do not support multi argument templates in their
     argument list.  We create a typedef to work around that problem.  */
  typedef vec<constructor_elt, va_gc> *vec_constructor_elt_gc;
  m_constructors = XCNEWVEC (vec_constructor_elt_gc, m_phi_count);
  m_target_inbound_names = m_default_values + m_phi_count;
  m_target_outbound_names = m_target_inbound_names + m_phi_count;
  for (i = 0; i < m_phi_count; i++)
    vec_alloc (m_constructors[i], tree_to_uhwi (m_range_size) + 1);
}

/* Populate the array of default values in the order of phi nodes.
   DEFAULT_CASE is the CASE_LABEL_EXPR for the default switch branch
   if the range is non-contiguous or the default case has standard
   structure, otherwise it is the first non-default case instead.  */

void
switch_conversion::gather_default_values (tree default_case)
{
  gphi_iterator gsi;
  basic_block bb = label_to_block (cfun, CASE_LABEL (default_case));
  edge e;
  int i = 0;

  gcc_assert (CASE_LOW (default_case) == NULL_TREE
	      || m_default_case_nonstandard);

  if (bb == m_final_bb)
    e = find_edge (m_switch_bb, bb);
  else
    e = single_succ_edge (bb);

  for (gsi = gsi_start_phis (m_final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (virtual_operand_p (gimple_phi_result (phi)))
	continue;
      tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
      gcc_assert (val);
      m_default_values[i++] = val;
    }
}

/* The following function populates the vectors in the constructors array with
   future contents of the static arrays.  The vectors are populated in the
   order of phi nodes.  */

void
switch_conversion::build_constructors ()
{
  unsigned i, branch_num = gimple_switch_num_labels (m_switch);
  tree pos = m_range_min;
  tree pos_one = build_int_cst (TREE_TYPE (pos), 1);

  for (i = 1; i < branch_num; i++)
    {
      tree cs = gimple_switch_label (m_switch, i);
      basic_block bb = label_to_block (cfun, CASE_LABEL (cs));
      edge e;
      tree high;
      gphi_iterator gsi;
      int j;

      if (bb == m_final_bb)
	e = find_edge (m_switch_bb, bb);
      else
	e = single_succ_edge (bb);
      gcc_assert (e);

      while (tree_int_cst_lt (pos, CASE_LOW (cs)))
	{
	  int k;
	  for (k = 0; k < m_phi_count; k++)
	    {
	      constructor_elt elt;

	      elt.index = int_const_binop (MINUS_EXPR, pos, m_range_min);
	      if (TYPE_PRECISION (TREE_TYPE (elt.index))
		  > TYPE_PRECISION (sizetype))
		elt.index = fold_convert (sizetype, elt.index);
	      elt.value
		= unshare_expr_without_location (m_default_values[k]);
	      m_constructors[k]->quick_push (elt);
	    }

	  pos = int_const_binop (PLUS_EXPR, pos, pos_one);
	}
      gcc_assert (tree_int_cst_equal (pos, CASE_LOW (cs)));

      j = 0;
      if (CASE_HIGH (cs))
	high = CASE_HIGH (cs);
      else
	high = CASE_LOW (cs);
      for (gsi = gsi_start_phis (m_final_bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    continue;
	  tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  tree low = CASE_LOW (cs);
	  pos = CASE_LOW (cs);

	  do
	    {
	      constructor_elt elt;

	      elt.index = int_const_binop (MINUS_EXPR, pos, m_range_min);
	      if (TYPE_PRECISION (TREE_TYPE (elt.index))
		  > TYPE_PRECISION (sizetype))
		elt.index = fold_convert (sizetype, elt.index);
	      elt.value = unshare_expr_without_location (val);
	      m_constructors[j]->quick_push (elt);

	      pos = int_const_binop (PLUS_EXPR, pos, pos_one);
	    } while (!tree_int_cst_lt (high, pos)
		     && tree_int_cst_lt (low, pos));
	  j++;
	}
    }
}

/* If all values in the constructor vector are products of a linear function
   a * x + b, then return true.  When true, COEFF_A and COEFF_B and
   coefficients of the linear function.  Note that equal values are special
   case of a linear function with a and b equal to zero.  */

bool
switch_conversion::contains_linear_function_p (vec<constructor_elt, va_gc> *vec,
					       wide_int *coeff_a,
					       wide_int *coeff_b)
{
  unsigned int i;
  constructor_elt *elt;

  gcc_assert (vec->length () >= 2);

  /* Let's try to find any linear function a * x + y that can apply to
     given values. 'a' can be calculated as follows:

     a = (y2 - y1) / (x2 - x1) where x2 - x1 = 1 (consecutive case indices)
     a = y2 - y1

     and

     b = y2 - a * x2

  */

  tree elt0 = (*vec)[0].value;
  tree elt1 = (*vec)[1].value;

  if (TREE_CODE (elt0) != INTEGER_CST || TREE_CODE (elt1) != INTEGER_CST)
    return false;

  wide_int range_min
    = wide_int::from (wi::to_wide (m_range_min),
		      TYPE_PRECISION (TREE_TYPE (elt0)),
		      TYPE_SIGN (TREE_TYPE (m_range_min)));
  wide_int y1 = wi::to_wide (elt0);
  wide_int y2 = wi::to_wide (elt1);
  wide_int a = y2 - y1;
  wide_int b = y2 - a * (range_min + 1);

  /* Verify that all values fulfill the linear function.  */
  FOR_EACH_VEC_SAFE_ELT (vec, i, elt)
    {
      if (TREE_CODE (elt->value) != INTEGER_CST)
	return false;

      wide_int value = wi::to_wide (elt->value);
      if (a * range_min + b != value)
	return false;

      ++range_min;
    }

  *coeff_a = a;
  *coeff_b = b;

  return true;
}

/* Return type which should be used for array elements, either TYPE's
   main variant or, for integral types, some smaller integral type
   that can still hold all the constants.  */

tree
switch_conversion::array_value_type (tree type, int num)
{
  unsigned int i, len = vec_safe_length (m_constructors[num]);
  constructor_elt *elt;
  int sign = 0;
  tree smaller_type;

  /* Types with alignments greater than their size can reach here, e.g. out of
     SRA.  We couldn't use these as an array component type so get back to the
     main variant first, which, for our purposes, is fine for other types as
     well.  */

  type = TYPE_MAIN_VARIANT (type);

  if (!INTEGRAL_TYPE_P (type)
      || (TREE_CODE (type) == BITINT_TYPE
	  && (TYPE_PRECISION (type) > MAX_FIXED_MODE_SIZE
	      || TYPE_MODE (type) == BLKmode)))
    return type;

  scalar_int_mode type_mode = SCALAR_INT_TYPE_MODE (type);
  scalar_int_mode mode = get_narrowest_mode (type_mode);
  if (GET_MODE_SIZE (type_mode) <= GET_MODE_SIZE (mode))
    return type;

  if (len < (optimize_bb_for_size_p (gimple_bb (m_switch)) ? 2 : 32))
    return type;

  FOR_EACH_VEC_SAFE_ELT (m_constructors[num], i, elt)
    {
      wide_int cst;

      if (TREE_CODE (elt->value) != INTEGER_CST)
	return type;

      cst = wi::to_wide (elt->value);
      while (1)
	{
	  unsigned int prec = GET_MODE_BITSIZE (mode);
	  if (prec > HOST_BITS_PER_WIDE_INT)
	    return type;

	  if (sign >= 0 && cst == wi::zext (cst, prec))
	    {
	      if (sign == 0 && cst == wi::sext (cst, prec))
		break;
	      sign = 1;
	      break;
	    }
	  if (sign <= 0 && cst == wi::sext (cst, prec))
	    {
	      sign = -1;
	      break;
	    }

	  if (sign == 1)
	    sign = 0;

	  if (!GET_MODE_WIDER_MODE (mode).exists (&mode)
	      || GET_MODE_SIZE (mode) >= GET_MODE_SIZE (type_mode))
	    return type;
	}
    }

  if (sign == 0)
    sign = TYPE_UNSIGNED (type) ? 1 : -1;
  smaller_type = lang_hooks.types.type_for_mode (mode, sign >= 0);
  if (GET_MODE_SIZE (type_mode)
      <= GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (smaller_type)))
    return type;

  return smaller_type;
}

/* Create an appropriate array type and declaration and assemble a static
   array variable.  Also create a load statement that initializes
   the variable in question with a value from the static array.  SWTCH is
   the switch statement being converted, NUM is the index to
   arrays of constructors, default values and target SSA names
   for this particular array.  ARR_INDEX_TYPE is the type of the index
   of the new array, PHI is the phi node of the final BB that corresponds
   to the value that will be loaded from the created array.  TIDX
   is an ssa name of a temporary variable holding the index for loads from the
   new array.  */

void
switch_conversion::build_one_array (int num, tree arr_index_type,
				    gphi *phi, tree tidx)
{
  tree name;
  gimple *load;
  gimple_stmt_iterator gsi = gsi_for_stmt (m_switch);
  location_t loc = gimple_location (m_switch);

  gcc_assert (m_default_values[num]);

  name = copy_ssa_name (PHI_RESULT (phi));
  m_target_inbound_names[num] = name;

  vec<constructor_elt, va_gc> *constructor = m_constructors[num];
  wide_int coeff_a, coeff_b;
  bool linear_p = contains_linear_function_p (constructor, &coeff_a, &coeff_b);
  tree type;
  if (linear_p
      && (type = range_check_type (TREE_TYPE ((*constructor)[0].value))))
    {
      if (dump_file && coeff_a.to_uhwi () > 0)
	fprintf (dump_file, "Linear transformation with A = %" PRId64
		 " and B = %" PRId64 "\n", coeff_a.to_shwi (),
		 coeff_b.to_shwi ());

      /* We must use type of constructor values.  */
      gimple_seq seq = NULL;
      tree tmp = gimple_convert (&seq, type, m_index_expr);
      tree tmp2 = gimple_build (&seq, MULT_EXPR, type,
				wide_int_to_tree (type, coeff_a), tmp);
      tree tmp3 = gimple_build (&seq, PLUS_EXPR, type, tmp2,
				wide_int_to_tree (type, coeff_b));
      tree tmp4 = gimple_convert (&seq, TREE_TYPE (name), tmp3);
      gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
      load = gimple_build_assign (name, tmp4);
    }
  else
    {
      tree array_type, ctor, decl, value_type, fetch, default_type;

      default_type = TREE_TYPE (m_default_values[num]);
      value_type = array_value_type (default_type, num);
      array_type = build_array_type (value_type, arr_index_type);
      if (default_type != value_type)
	{
	  unsigned int i;
	  constructor_elt *elt;

	  FOR_EACH_VEC_SAFE_ELT (constructor, i, elt)
	    elt->value = fold_convert (value_type, elt->value);
	}
      ctor = build_constructor (array_type, constructor);
      TREE_CONSTANT (ctor) = true;
      TREE_STATIC (ctor) = true;

      decl = build_decl (loc, VAR_DECL, NULL_TREE, array_type);
      TREE_STATIC (decl) = 1;
      DECL_INITIAL (decl) = ctor;

      DECL_NAME (decl) = create_tmp_var_name ("CSWTCH");
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      TREE_CONSTANT (decl) = 1;
      TREE_READONLY (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      if (offloading_function_p (cfun->decl))
	DECL_ATTRIBUTES (decl)
	  = tree_cons (get_identifier ("omp declare target"), NULL_TREE,
		       NULL_TREE);
      varpool_node::finalize_decl (decl);

      fetch = build4 (ARRAY_REF, value_type, decl, tidx, NULL_TREE,
		      NULL_TREE);
      if (default_type != value_type)
	{
	  fetch = fold_convert (default_type, fetch);
	  fetch = force_gimple_operand_gsi (&gsi, fetch, true, NULL_TREE,
					    true, GSI_SAME_STMT);
	}
      load = gimple_build_assign (name, fetch);
    }

  gsi_insert_before (&gsi, load, GSI_SAME_STMT);
  update_stmt (load);
  m_arr_ref_last = load;
}

/* Builds and initializes static arrays initialized with values gathered from
   the switch statement.  Also creates statements that load values from
   them.  */

void
switch_conversion::build_arrays ()
{
  tree arr_index_type;
  tree tidx, sub, utype, tidxtype;
  gimple *stmt;
  gimple_stmt_iterator gsi;
  gphi_iterator gpi;
  int i;
  location_t loc = gimple_location (m_switch);

  gsi = gsi_for_stmt (m_switch);

  /* Make sure we do not generate arithmetics in a subrange.  */
  utype = TREE_TYPE (m_index_expr);
  if (TREE_TYPE (utype))
    utype = lang_hooks.types.type_for_mode (TYPE_MODE (TREE_TYPE (utype)), 1);
  else if (TREE_CODE (utype) == BITINT_TYPE
	   && (TYPE_PRECISION (utype) > MAX_FIXED_MODE_SIZE
	       || TYPE_MODE (utype) == BLKmode))
    utype = unsigned_type_for (utype);
  else
    utype = lang_hooks.types.type_for_mode (TYPE_MODE (utype), 1);
  if (TYPE_PRECISION (utype) > TYPE_PRECISION (sizetype))
    tidxtype = sizetype;
  else
    tidxtype = utype;

  arr_index_type = build_index_type (m_range_size);
  tidx = make_ssa_name (tidxtype);
  sub = fold_build2_loc (loc, MINUS_EXPR, utype,
			 fold_convert_loc (loc, utype, m_index_expr),
			 fold_convert_loc (loc, utype, m_range_min));
  sub = fold_convert (tidxtype, sub);
  sub = force_gimple_operand_gsi (&gsi, sub,
				  false, NULL, true, GSI_SAME_STMT);
  stmt = gimple_build_assign (tidx, sub);

  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
  update_stmt (stmt);
  m_arr_ref_first = stmt;

  for (gpi = gsi_start_phis (m_final_bb), i = 0;
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi)))
	build_one_array (i++, arr_index_type, phi, tidx);
      else
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, m_switch_bb->succs)
	    {
	      if (e->dest == m_final_bb)
		break;
	      if (!m_default_case_nonstandard
		  || e->dest != m_default_bb)
		{
		  e = single_succ_edge (e->dest);
		  break;
		}
	    }
	  gcc_assert (e && e->dest == m_final_bb);
	  m_target_vop = PHI_ARG_DEF_FROM_EDGE (phi, e);
	}
    }
}

/* Generates and appropriately inserts loads of default values at the position
   given by GSI.  Returns the last inserted statement.  */

gassign *
switch_conversion::gen_def_assigns (gimple_stmt_iterator *gsi)
{
  int i;
  gassign *assign = NULL;

  for (i = 0; i < m_phi_count; i++)
    {
      tree name = copy_ssa_name (m_target_inbound_names[i]);
      m_target_outbound_names[i] = name;
      assign = gimple_build_assign (name, m_default_values[i]);
      gsi_insert_before (gsi, assign, GSI_SAME_STMT);
      update_stmt (assign);
    }
  return assign;
}

/* Deletes the unused bbs and edges that now contain the switch statement and
   its empty branch bbs.  BBD is the now dead BB containing
   the original switch statement, FINAL is the last BB of the converted
   switch statement (in terms of succession).  */

void
switch_conversion::prune_bbs (basic_block bbd, basic_block final,
			      basic_block default_bb)
{
  edge_iterator ei;
  edge e;

  for (ei = ei_start (bbd->succs); (e = ei_safe_edge (ei)); )
    {
      basic_block bb;
      bb = e->dest;
      remove_edge (e);
      if (bb != final && bb != default_bb)
	delete_basic_block (bb);
    }
  delete_basic_block (bbd);
}

/* Add values to phi nodes in final_bb for the two new edges.  E1F is the edge
   from the basic block loading values from an array and E2F from the basic
   block loading default values.  BBF is the last switch basic block (see the
   bbf description in the comment below).  */

void
switch_conversion::fix_phi_nodes (edge e1f, edge e2f, basic_block bbf)
{
  gphi_iterator gsi;
  int i;

  for (gsi = gsi_start_phis (bbf), i = 0;
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree inbound, outbound;
      if (virtual_operand_p (gimple_phi_result (phi)))
	inbound = outbound = m_target_vop;
      else
	{
	  inbound = m_target_inbound_names[i];
	  outbound = m_target_outbound_names[i++];
	}
      add_phi_arg (phi, inbound, e1f, UNKNOWN_LOCATION);
      if (!m_default_case_nonstandard)
	add_phi_arg (phi, outbound, e2f, UNKNOWN_LOCATION);
    }
}

/* Creates a check whether the switch expression value actually falls into the
   range given by all the cases.  If it does not, the temporaries are loaded
   with default values instead.  */

void
switch_conversion::gen_inbound_check ()
{
  tree label_decl1 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl2 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl3 = create_artificial_label (UNKNOWN_LOCATION);
  glabel *label1, *label2, *label3;
  tree utype, tidx;
  tree bound;

  gcond *cond_stmt;

  gassign *last_assign = NULL;
  gimple_stmt_iterator gsi;
  basic_block bb0, bb1, bb2, bbf, bbd;
  edge e01 = NULL, e02, e21, e1d, e1f, e2f;
  location_t loc = gimple_location (m_switch);

  gcc_assert (m_default_values);

  bb0 = gimple_bb (m_switch);

  tidx = gimple_assign_lhs (m_arr_ref_first);
  utype = TREE_TYPE (tidx);

  /* (end of) block 0 */
  gsi = gsi_for_stmt (m_arr_ref_first);
  gsi_next (&gsi);

  bound = fold_convert_loc (loc, utype, m_range_size);
  cond_stmt = gimple_build_cond (LE_EXPR, tidx, bound, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);
  update_stmt (cond_stmt);

  /* block 2 */
  if (!m_default_case_nonstandard)
    {
      label2 = gimple_build_label (label_decl2);
      gsi_insert_before (&gsi, label2, GSI_SAME_STMT);
      last_assign = gen_def_assigns (&gsi);
    }

  /* block 1 */
  label1 = gimple_build_label (label_decl1);
  gsi_insert_before (&gsi, label1, GSI_SAME_STMT);

  /* block F */
  gsi = gsi_start_bb (m_final_bb);
  label3 = gimple_build_label (label_decl3);
  gsi_insert_before (&gsi, label3, GSI_SAME_STMT);

  /* cfg fix */
  e02 = split_block (bb0, cond_stmt);
  bb2 = e02->dest;

  if (m_default_case_nonstandard)
    {
      bb1 = bb2;
      bb2 = m_default_bb;
      e01 = e02;
      e01->flags = EDGE_TRUE_VALUE;
      e02 = make_edge (bb0, bb2, EDGE_FALSE_VALUE);
      edge e_default = find_edge (bb1, bb2);
      for (gphi_iterator gsi = gsi_start_phis (bb2);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  tree arg = PHI_ARG_DEF_FROM_EDGE (phi, e_default);
	  add_phi_arg (phi, arg, e02,
		       gimple_phi_arg_location_from_edge (phi, e_default));
	}
      /* Partially fix the dominator tree, if it is available.  */
      if (dom_info_available_p (CDI_DOMINATORS))
	redirect_immediate_dominators (CDI_DOMINATORS, bb1, bb0);
    }
  else
    {
      e21 = split_block (bb2, last_assign);
      bb1 = e21->dest;
      remove_edge (e21);
    }

  e1d = split_block (bb1, m_arr_ref_last);
  bbd = e1d->dest;
  remove_edge (e1d);

  /* Flags and profiles of the edge for in-range values.  */
  if (!m_default_case_nonstandard)
    e01 = make_edge (bb0, bb1, EDGE_TRUE_VALUE);
  e01->probability = m_default_prob.invert ();

  /* Flags and profiles of the edge taking care of out-of-range values.  */
  e02->flags &= ~EDGE_FALLTHRU;
  e02->flags |= EDGE_FALSE_VALUE;
  e02->probability = m_default_prob;

  bbf = m_final_bb;

  e1f = make_edge (bb1, bbf, EDGE_FALLTHRU);
  e1f->probability = profile_probability::always ();

  if (m_default_case_nonstandard)
    e2f = NULL;
  else
    {
      e2f = make_edge (bb2, bbf, EDGE_FALLTHRU);
      e2f->probability = profile_probability::always ();
    }

  /* frequencies of the new BBs */
  bb1->count = e01->count ();
  bb2->count = e02->count ();
  if (!m_default_case_nonstandard)
    bbf->count = e1f->count () + e2f->count ();

  /* Tidy blocks that have become unreachable.  */
  bool prune_default_bb = !m_default_case_nonstandard
    && !m_exp_index_transform_applied;
  prune_bbs (bbd, m_final_bb, prune_default_bb ? NULL : m_default_bb);

  /* Fixup the PHI nodes in bbF.  */
  fix_phi_nodes (e1f, e2f, bbf);

  /* Fix the dominator tree, if it is available.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      vec<basic_block> bbs_to_fix_dom;

      set_immediate_dominator (CDI_DOMINATORS, bb1, bb0);
      if (!m_default_case_nonstandard)
	set_immediate_dominator (CDI_DOMINATORS, bb2, bb0);
      if (! get_immediate_dominator (CDI_DOMINATORS, bbf))
	/* If bbD was the immediate dominator ...  */
	set_immediate_dominator (CDI_DOMINATORS, bbf, bb0);

      bbs_to_fix_dom.create (3 + (bb2 != bbf));
      bbs_to_fix_dom.quick_push (bb0);
      bbs_to_fix_dom.quick_push (bb1);
      if (bb2 != bbf)
	bbs_to_fix_dom.quick_push (bb2);
      bbs_to_fix_dom.quick_push (bbf);

      iterate_fix_dominators (CDI_DOMINATORS, bbs_to_fix_dom, true);
      bbs_to_fix_dom.release ();
    }
}

/* The following function is invoked on every switch statement (the current
   one is given in SWTCH) and runs the individual phases of switch
   conversion on it one after another until one fails or the conversion
   is completed.  On success, NULL is in m_reason, otherwise points
   to a string with the reason why the conversion failed.  */

void
switch_conversion::expand (gswitch *swtch)
{
  /* Group case labels so that we get the right results from the heuristics
     that decide on the code generation approach for this switch.  */
  m_cfg_altered |= group_case_labels_stmt (swtch);

  /* If this switch is now a degenerate case with only a default label,
     there is nothing left for us to do.  */
  if (gimple_switch_num_labels (swtch) < 2)
    {
      m_reason = "switch is a degenerate case";
      return;
    }

  collect (swtch);

  /* No error markers should reach here (they should be filtered out
     during gimplification).  */
  gcc_checking_assert (TREE_TYPE (m_index_expr) != error_mark_node);

  /* Prefer bit test if possible.  */
  if (tree_fits_uhwi_p (m_range_size)
      && bit_test_cluster::can_be_handled (tree_to_uhwi (m_range_size), m_uniq)
      && bit_test_cluster::is_beneficial (m_count, m_uniq))
    {
      m_reason = "expanding as bit test is preferable";
      return;
    }

  if (m_uniq <= 2)
    {
      /* This will be expanded as a decision tree .  */
      m_reason = "expanding as jumps is preferable";
      return;
    }

  /* If there is no common successor, we cannot do the transformation.  */
  if (!m_final_bb)
    {
      m_reason = "no common successor to all case label target blocks found";
      return;
    }

  /* Sometimes it is possible to use the "exponential index transform" to help
     switch conversion convert switches which it otherwise could not convert.
     However, we want to do this transform only when we know that switch
     conversion will then really be able to convert the switch.  So we first
     check if the transformation is applicable and then maybe later do the
     transformation.  */
  bool exp_transform_viable = is_exp_index_transform_viable (swtch);

  /* Check the case label values are within reasonable range.

     If we will be doing exponential index transform, the range will be always
     reasonable.  */
  if (!exp_transform_viable && !check_range ())
    {
      gcc_assert (m_reason);
      return;
    }

  /* For all the cases, see whether they are empty, the assignments they
     represent constant and so on...  */
  if (!check_all_empty_except_final ())
    {
      gcc_assert (m_reason);
      return;
    }
  if (!check_final_bb ())
    {
      gcc_assert (m_reason);
      return;
    }

  /* At this point all checks have passed and we can proceed with the
     transformation.  */

  if (exp_transform_viable)
    exp_index_transform (swtch);

  create_temp_arrays ();
  gather_default_values (m_default_case_nonstandard
			 ? gimple_switch_label (swtch, 1)
			 : gimple_switch_default_label (swtch));
  build_constructors ();

  build_arrays (); /* Build the static arrays and assignments.  */
  gen_inbound_check ();	/* Build the bounds check.  */

  m_cfg_altered = true;
}

/* Destructor.  */

switch_conversion::~switch_conversion ()
{
  XDELETEVEC (m_constructors);
  XDELETEVEC (m_default_values);
}

/* Constructor.  */

group_cluster::group_cluster (vec<cluster *> &clusters,
			      unsigned start, unsigned end)
{
  gcc_checking_assert (end - start + 1 >= 1);
  m_prob = profile_probability::never ();
  m_cases.create (end - start + 1);
  for (unsigned i = start; i <= end; i++)
    {
      m_cases.quick_push (static_cast<simple_cluster *> (clusters[i]));
      m_prob += clusters[i]->m_prob;
    }
  m_subtree_prob = m_prob;
}

/* Destructor.  */

group_cluster::~group_cluster ()
{
  for (unsigned i = 0; i < m_cases.length (); i++)
    delete m_cases[i];

  m_cases.release ();
}

/* Dump content of a cluster.  */

void
group_cluster::dump (FILE *f, bool details)
{
  unsigned total_values = 0;
  for (unsigned i = 0; i < m_cases.length (); i++)
    total_values += m_cases[i]->get_range (m_cases[i]->get_low (),
					   m_cases[i]->get_high ());

  unsigned comparison_count = 0;
  for (unsigned i = 0; i < m_cases.length (); i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (m_cases[i]);
      comparison_count += sc->get_comparison_count ();
    }

  unsigned HOST_WIDE_INT range = get_range (get_low (), get_high ());
  fprintf (f, "%s", get_type () == JUMP_TABLE ? "JT" : "BT");

  if (details)
    fprintf (f, "(values:%d comparisons:%d range:" HOST_WIDE_INT_PRINT_DEC
	     " density: %.2f%%)", total_values, comparison_count, range,
	     100.0f * comparison_count / range);

  fprintf (f, ":");
  PRINT_CASE (f, get_low ());
  fprintf (f, "-");
  PRINT_CASE (f, get_high ());
  fprintf (f, " ");
}

/* Emit GIMPLE code to handle the cluster.  */

void
jump_table_cluster::emit (tree index_expr, tree,
			  tree default_label_expr, basic_block default_bb,
			  location_t loc)
{
  tree low = get_low ();
  unsigned HOST_WIDE_INT range = get_range (low, get_high ());
  unsigned HOST_WIDE_INT nondefault_range = 0;
  bool bitint = false;
  gimple_stmt_iterator gsi = gsi_start_bb (m_case_bb);

  /* For large/huge _BitInt, subtract low from index_expr, cast to unsigned
     DImode type (get_range doesn't support ranges larger than 64-bits)
     and subtract low from all case values as well.  */
  if (TREE_CODE (TREE_TYPE (index_expr)) == BITINT_TYPE
      && TYPE_PRECISION (TREE_TYPE (index_expr)) > GET_MODE_PRECISION (DImode))
    {
      bitint = true;
      tree this_low = low, type;
      gimple *g;
      gimple_seq seq = NULL;
      if (!TYPE_OVERFLOW_WRAPS (TREE_TYPE (index_expr)))
	{
	  type = unsigned_type_for (TREE_TYPE (index_expr));
	  index_expr = gimple_convert (&seq, type, index_expr);
	  this_low = fold_convert (type, this_low);
	}
      this_low = const_unop (NEGATE_EXPR, TREE_TYPE (this_low), this_low);
      index_expr = gimple_build (&seq, PLUS_EXPR, TREE_TYPE (index_expr),
				 index_expr, this_low);
      type = build_nonstandard_integer_type (GET_MODE_PRECISION (DImode), 1);
      g = gimple_build_cond (GT_EXPR, index_expr,
			     fold_convert (TREE_TYPE (index_expr),
					   TYPE_MAX_VALUE (type)),
			     NULL_TREE, NULL_TREE);
      gimple_seq_add_stmt (&seq, g);
      gimple_seq_set_location (seq, loc);
      gsi_insert_seq_after (&gsi, seq, GSI_NEW_STMT);
      edge e1 = split_block (m_case_bb, g);
      e1->flags = EDGE_FALSE_VALUE;
      e1->probability = profile_probability::likely ();
      edge e2 = make_edge (e1->src, default_bb, EDGE_TRUE_VALUE);
      e2->probability = e1->probability.invert ();
      gsi = gsi_start_bb (e1->dest);
      seq = NULL;
      index_expr = gimple_convert (&seq, type, index_expr);
      gimple_seq_set_location (seq, loc);
      gsi_insert_seq_after (&gsi, seq, GSI_NEW_STMT);
    }

  /* For jump table we just emit a new gswitch statement that will
     be latter lowered to jump table.  */
  auto_vec <tree> labels;
  labels.create (m_cases.length ());

  basic_block case_bb = gsi_bb (gsi);
  make_edge (case_bb, default_bb, 0);
  for (unsigned i = 0; i < m_cases.length (); i++)
    {
      tree lab = unshare_expr (m_cases[i]->m_case_label_expr);
      if (bitint)
	{
	  CASE_LOW (lab)
	    = fold_convert (TREE_TYPE (index_expr),
			    const_binop (MINUS_EXPR,
					 TREE_TYPE (CASE_LOW (lab)),
					 CASE_LOW (lab), low));
	  if (CASE_HIGH (lab))
	    CASE_HIGH (lab)
	      = fold_convert (TREE_TYPE (index_expr),
			      const_binop (MINUS_EXPR,
					   TREE_TYPE (CASE_HIGH (lab)),
					   CASE_HIGH (lab), low));
	}
      labels.quick_push (lab);
      make_edge (case_bb, m_cases[i]->m_case_bb, 0);
    }

  gswitch *s = gimple_build_switch (index_expr,
				    unshare_expr (default_label_expr), labels);
  gimple_set_location (s, loc);
  gsi_insert_after (&gsi, s, GSI_NEW_STMT);

  /* Set up even probabilities for all cases.  */
  for (unsigned i = 0; i < m_cases.length (); i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (m_cases[i]);
      edge case_edge = find_edge (case_bb, sc->m_case_bb);
      unsigned HOST_WIDE_INT case_range
	= sc->get_range (sc->get_low (), sc->get_high ());
      nondefault_range += case_range;

      /* case_edge->aux is number of values in a jump-table that are covered
	 by the case_edge.  */
      case_edge->aux = (void *) ((intptr_t) (case_edge->aux) + case_range);
    }

  edge default_edge = gimple_switch_default_edge (cfun, s);
  default_edge->probability = profile_probability::never ();

  for (unsigned i = 0; i < m_cases.length (); i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (m_cases[i]);
      edge case_edge = find_edge (case_bb, sc->m_case_bb);
      case_edge->probability
	= profile_probability::always ().apply_scale ((intptr_t)case_edge->aux,
						      range);
    }

  /* Number of non-default values is probability of default edge.  */
  default_edge->probability
    += profile_probability::always ().apply_scale (nondefault_range,
						   range).invert ();

  switch_decision_tree::reset_out_edges_aux (s);
}

/* Find jump tables of given CLUSTERS, where all members of the vector
   are of type simple_cluster.  New clusters are returned.  */

vec<cluster *>
jump_table_cluster::find_jump_tables (vec<cluster *> &clusters)
{
  if (!is_enabled ())
    return clusters.copy ();

  unsigned l = clusters.length ();

  /* Note: l + 1 is the number of cases of the switch.  */
  if (l + 1 > (unsigned) param_switch_lower_slow_alg_max_cases)
    return clusters.copy ();

  auto_vec<min_cluster_item> min;
  min.reserve (l + 1);

  min.quick_push (min_cluster_item (0, 0, 0));

  unsigned HOST_WIDE_INT max_ratio
    = (optimize_insn_for_size_p ()
       ? param_jump_table_max_growth_ratio_for_size
       : param_jump_table_max_growth_ratio_for_speed);

  for (unsigned i = 1; i <= l; i++)
    {
      /* Set minimal # of clusters with i-th item to infinite.  */
      min.quick_push (min_cluster_item (INT_MAX, INT_MAX, INT_MAX));

      /* Pre-calculate number of comparisons for the clusters.  */
      HOST_WIDE_INT comparison_count = 0;
      for (unsigned k = 0; k <= i - 1; k++)
	{
	  simple_cluster *sc = static_cast<simple_cluster *> (clusters[k]);
	  comparison_count += sc->get_comparison_count ();
	}

      for (unsigned j = 0; j < i; j++)
	{
	  unsigned HOST_WIDE_INT s = min[j].m_non_jt_cases;
	  if (i - j < case_values_threshold ())
	    s += i - j;

	  /* Prefer clusters with smaller number of numbers covered.  */
	  if ((min[j].m_count + 1 < min[i].m_count
	       || (min[j].m_count + 1 == min[i].m_count
		   && s < min[i].m_non_jt_cases))
	      && can_be_handled (clusters, j, i - 1, max_ratio,
				 comparison_count))
	    min[i] = min_cluster_item (min[j].m_count + 1, j, s);

	  simple_cluster *sc = static_cast<simple_cluster *> (clusters[j]);
	  comparison_count -= sc->get_comparison_count ();
	}

      gcc_checking_assert (comparison_count == 0);
      gcc_checking_assert (min[i].m_count != INT_MAX);
    }

  /* No result.  */
  if (min[l].m_count == l)
    return clusters.copy ();

  vec<cluster *> output;
  output.create (4);

  /* Find and build the clusters.  */
  for (unsigned int end = l;;)
    {
      int start = min[end].m_start;

      /* Do not allow clusters with small number of cases.  */
      if (is_beneficial (clusters, start, end - 1))
	output.safe_push (new jump_table_cluster (clusters, start, end - 1));
      else
	for (int i = end - 1; i >= start; i--)
	  output.safe_push (clusters[i]);

      end = start;

      if (start <= 0)
	break;
    }

  output.reverse ();
  return output;
}

/* Return true when cluster starting at START and ending at END (inclusive)
   can build a jump-table.  */

bool
jump_table_cluster::can_be_handled (const vec<cluster *> &clusters,
				    unsigned start, unsigned end,
				    unsigned HOST_WIDE_INT max_ratio,
				    unsigned HOST_WIDE_INT comparison_count)
{
  /* If the switch is relatively small such that the cost of one
     indirect jump on the target are higher than the cost of a
     decision tree, go with the decision tree.

     If range of values is much bigger than number of values,
     or if it is too large to represent in a HOST_WIDE_INT,
     make a sequence of conditional branches instead of a dispatch.

     The definition of "much bigger" depends on whether we are
     optimizing for size or for speed.

     For algorithm correctness, jump table for a single case must return
     true.  We bail out in is_beneficial if it's called just for
     a single case.  */
  if (start == end)
    return true;

  unsigned HOST_WIDE_INT range = get_range (clusters[start]->get_low (),
					    clusters[end]->get_high ());
  /* Check overflow.  */
  if (range == 0)
    return false;

  if (range > HOST_WIDE_INT_M1U / 100)
    return false;

  unsigned HOST_WIDE_INT lhs = 100 * range;
  if (lhs < range)
    return false;

  return lhs <= max_ratio * comparison_count;
}

/* Return true if cluster starting at START and ending at END (inclusive)
   is profitable transformation.  */

bool
jump_table_cluster::is_beneficial (const vec<cluster *> &,
				   unsigned start, unsigned end)
{
  /* Single case bail out.  */
  if (start == end)
    return false;

  return end - start + 1 >= case_values_threshold ();
}

/* Find bit tests of given CLUSTERS, where all members of the vector are of
   type simple_cluster.  Use a fast algorithm that might not find the optimal
   solution (minimal number of clusters on the output).  New clusters are
   returned.

   You should call find_bit_tests () instead of calling this function
   directly.  */

vec<cluster *>
bit_test_cluster::find_bit_tests_fast (vec<cluster *> &clusters)
{
  unsigned l = clusters.length ();
  vec<cluster *> output;

  output.create (l);

  /* Look at sliding BITS_PER_WORD sized windows in the switch value space
     and determine if they are suitable for a bit test cluster.  Worst case
     this can examine every value BITS_PER_WORD-1 times.  */
  unsigned k;
  for (unsigned i = 0; i < l; i += k)
    {
      hash_set<basic_block> targets;
      cluster *start_cluster = clusters[i];

      /* Find the biggest k such that clusters i to i+k-1 can be turned into a
	 one big bit test cluster.  */
      k = 0;
      while (i + k < l)
	{
	  cluster *end_cluster = clusters[i + k];

	  /* Does value range fit into the BITS_PER_WORD window?  */
	  HOST_WIDE_INT w = cluster::get_range (start_cluster->get_low (),
						end_cluster->get_high ());
	  if (w == 0 || w > BITS_PER_WORD)
	    break;

	  /* Check for max # of targets.  */
	  if (targets.elements () == m_max_case_bit_tests
	      && !targets.contains (end_cluster->m_case_bb))
	    break;

	  targets.add (end_cluster->m_case_bb);
	  k++;
	}

      if (is_beneficial (k, targets.elements ()))
	{
	  output.safe_push (new bit_test_cluster (clusters, i, i + k - 1,
						  i == 0 && k == l));
	}
      else
	{
	  output.safe_push (clusters[i]);
	  /* ??? Might be able to skip more.  */
	  k = 1;
	}
    }

  return output;
}

/* Find bit tests of given CLUSTERS, where all members of the vector
   are of type simple_cluster.  Use a slow (quadratic) algorithm that always
   finds the optimal solution (minimal number of clusters on the output).  New
   clusters are returned.

   You should call find_bit_tests () instead of calling this function
   directly.  */

vec<cluster *>
bit_test_cluster::find_bit_tests_slow (vec<cluster *> &clusters)
{
  unsigned l = clusters.length ();
  auto_vec<min_cluster_item> min;
  min.reserve (l + 1);

  min.quick_push (min_cluster_item (0, 0, 0));

  for (unsigned i = 1; i <= l; i++)
    {
      /* Set minimal # of clusters with i-th item to infinite.  */
      min.quick_push (min_cluster_item (INT_MAX, INT_MAX, INT_MAX));

      for (unsigned j = 0; j < i; j++)
	{
	  if (min[j].m_count + 1 < min[i].m_count
	      && can_be_handled (clusters, j, i - 1))
	    min[i] = min_cluster_item (min[j].m_count + 1, j, INT_MAX);
	}

      gcc_checking_assert (min[i].m_count != INT_MAX);
    }

  /* No result.  */
  if (min[l].m_count == l)
    return clusters.copy ();

  vec<cluster *> output;
  output.create (4);

  /* Find and build the clusters.  */
  for (unsigned end = l;;)
    {
      int start = min[end].m_start;

      if (is_beneficial (clusters, start, end - 1))
	{
	  bool entire = start == 0 && end == clusters.length ();
	  output.safe_push (new bit_test_cluster (clusters, start, end - 1,
						  entire));
	}
      else
	for (int i = end - 1; i >= start; i--)
	  output.safe_push (clusters[i]);

      end = start;

      if (start <= 0)
	break;
    }

  output.reverse ();
  return output;
}

/* Find bit tests of given CLUSTERS, where all members of the vector
   are of type simple_cluster.  MAX_C is the approx max number of cases per
   label.  New clusters are returned.  */

vec<cluster *>
bit_test_cluster::find_bit_tests (vec<cluster *> &clusters, int max_c)
{
  if (!is_enabled () || max_c == 1)
    return clusters.copy ();

  unsigned l = clusters.length ();

  /* Note: l + 1 is the number of cases of the switch.  */
  if (l + 1 > (unsigned) param_switch_lower_slow_alg_max_cases)
    return find_bit_tests_fast (clusters);
  else
    return find_bit_tests_slow (clusters);
}

/* Return true when RANGE of case values with UNIQ labels
   can build a bit test.  */

bool
bit_test_cluster::can_be_handled (unsigned HOST_WIDE_INT range,
				  unsigned int uniq)
{
  /* Check overflow.  */
  if (range == 0)
    return false;

  if (range >= GET_MODE_BITSIZE (word_mode))
    return false;

  return uniq <= m_max_case_bit_tests;
}

/* Return true when cluster starting at START and ending at END (inclusive)
   can build a bit test.  */

bool
bit_test_cluster::can_be_handled (const vec<cluster *> &clusters,
				  unsigned start, unsigned end)
{
  auto_vec<int, m_max_case_bit_tests> dest_bbs;
  /* For algorithm correctness, bit test for a single case must return
     true.  We bail out in is_beneficial if it's called just for
     a single case.  */
  if (start == end)
    return true;

  unsigned HOST_WIDE_INT range = get_range (clusters[start]->get_low (),
					    clusters[end]->get_high ());

  /* Make a guess first.  */
  if (!can_be_handled (range, m_max_case_bit_tests))
    return false;

  for (unsigned i = start; i <= end; i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (clusters[i]);
      /* m_max_case_bit_tests is very small integer, thus the operation
	 is constant. */
      if (!dest_bbs.contains (sc->m_case_bb->index))
	{
	  if (dest_bbs.length () >= m_max_case_bit_tests)
	    return false;
	  dest_bbs.quick_push (sc->m_case_bb->index);
	}
    }

  return true;
}

/* Return true when COUNT of cases of UNIQ labels is beneficial for bit test
   transformation.  */

bool
bit_test_cluster::is_beneficial (unsigned count, unsigned uniq)
{
  return (((uniq == 1 && count >= 3)
	   || (uniq == 2 && count >= 5)
	   || (uniq == 3 && count >= 6)));
}

/* Return true if cluster starting at START and ending at END (inclusive)
   is profitable transformation.  */

bool
bit_test_cluster::is_beneficial (const vec<cluster *> &clusters,
				 unsigned start, unsigned end)
{
  /* Single case bail out.  */
  if (start == end)
    return false;

  auto_bitmap dest_bbs;

  for (unsigned i = start; i <= end; i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (clusters[i]);
      bitmap_set_bit (dest_bbs, sc->m_case_bb->index);
    }

  unsigned uniq = bitmap_count_bits (dest_bbs);
  unsigned count = end - start + 1;
  return is_beneficial (count, uniq);
}

/* Comparison function for qsort to order bit tests by decreasing
   probability of execution.  */

int
case_bit_test::cmp (const void *p1, const void *p2)
{
  const case_bit_test *const d1 = (const case_bit_test *) p1;
  const case_bit_test *const d2 = (const case_bit_test *) p2;

  if (d2->bits != d1->bits)
    return d2->bits - d1->bits;

  /* Stabilize the sort.  */
  return (LABEL_DECL_UID (CASE_LABEL (d2->label))
	  - LABEL_DECL_UID (CASE_LABEL (d1->label)));
}

/*  Expand a switch statement by a short sequence of bit-wise
    comparisons.  "switch(x)" is effectively converted into
    "if ((1 << (x-MINVAL)) & CST)" where CST and MINVAL are
    integer constants.

    INDEX_EXPR is the value being switched on.

    MINVAL is the lowest case value of in the case nodes,
    and RANGE is highest value minus MINVAL.  MINVAL and RANGE
    are not guaranteed to be of the same type as INDEX_EXPR
    (the gimplifier doesn't change the type of case label values,
    and MINVAL and RANGE are derived from those values).
    MAXVAL is MINVAL + RANGE.

    There *MUST* be max_case_bit_tests or less unique case
    node targets.  */

void
bit_test_cluster::emit (tree index_expr, tree index_type,
			tree, basic_block default_bb, location_t loc)
{
  case_bit_test test[m_max_case_bit_tests] = { {} };
  unsigned int i, j, k;
  unsigned int count;

  tree unsigned_index_type = range_check_type (index_type);

  gimple_stmt_iterator gsi;
  gassign *shift_stmt;

  tree idx, tmp, csui;
  tree word_type_node = lang_hooks.types.type_for_mode (word_mode, 1);
  tree word_mode_zero = fold_convert (word_type_node, integer_zero_node);
  tree word_mode_one = fold_convert (word_type_node, integer_one_node);
  int prec = TYPE_PRECISION (word_type_node);
  wide_int wone = wi::one (prec);

  tree minval = get_low ();
  tree maxval = get_high ();

  /* Go through all case labels, and collect the case labels, profile
     counts, and other information we need to build the branch tests.  */
  count = 0;
  for (i = 0; i < m_cases.length (); i++)
    {
      unsigned int lo, hi;
      simple_cluster *n = static_cast<simple_cluster *> (m_cases[i]);
      for (k = 0; k < count; k++)
	if (n->m_case_bb == test[k].target_bb)
	  break;

      if (k == count)
	{
	  gcc_checking_assert (count < m_max_case_bit_tests);
	  test[k].mask = wi::zero (prec);
	  test[k].target_bb = n->m_case_bb;
	  test[k].label = n->m_case_label_expr;
	  test[k].bits = 0;
	  test[k].prob = profile_probability::never ();
	  count++;
	}

      test[k].bits += n->get_range (n->get_low (), n->get_high ());
      test[k].prob += n->m_prob;

      lo = tree_to_uhwi (int_const_binop (MINUS_EXPR, n->get_low (), minval));
      if (n->get_high () == NULL_TREE)
	hi = lo;
      else
	hi = tree_to_uhwi (int_const_binop (MINUS_EXPR, n->get_high (),
					    minval));

      for (j = lo; j <= hi; j++)
	test[k].mask |= wi::lshift (wone, j);
    }

  qsort (test, count, sizeof (*test), case_bit_test::cmp);

  /* If every possible relative value of the index expression is a valid shift
     amount, then we can merge the entry test in the bit test.  */
  bool entry_test_needed;
  int_range_max r;
  if (TREE_CODE (index_expr) == SSA_NAME
      && get_range_query (cfun)->range_of_expr (r, index_expr)
      && !r.undefined_p ()
      && !r.varying_p ()
      && wi::leu_p (r.upper_bound () - r.lower_bound (), prec - 1))
    {
      wide_int min = r.lower_bound ();
      wide_int max = r.upper_bound ();
      tree index_type = TREE_TYPE (index_expr);
      minval = fold_convert (index_type, minval);
      wide_int iminval = wi::to_wide (minval);
      if (wi::lt_p (min, iminval, TYPE_SIGN (index_type)))
	{
	  minval = wide_int_to_tree (index_type, min);
	  for (i = 0; i < count; i++)
	    test[i].mask = wi::lshift (test[i].mask, iminval - min);
	}
      else if (wi::gt_p (min, iminval, TYPE_SIGN (index_type)))
	{
	  minval = wide_int_to_tree (index_type, min);
	  for (i = 0; i < count; i++)
	    test[i].mask = wi::lrshift (test[i].mask, min - iminval);
	}
      maxval = wide_int_to_tree (index_type, max);
      entry_test_needed = false;
    }
  else
    entry_test_needed = true;

  /* If all values are in the 0 .. BITS_PER_WORD-1 range, we can get rid of
     the minval subtractions, but it might make the mask constants more
     expensive.  So, compare the costs.  */
  if (compare_tree_int (minval, 0) > 0 && compare_tree_int (maxval, prec) < 0)
    {
      int cost_diff;
      HOST_WIDE_INT m = tree_to_uhwi (minval);
      rtx reg = gen_raw_REG (word_mode, 10000);
      bool speed_p = optimize_insn_for_speed_p ();
      cost_diff = set_src_cost (gen_rtx_PLUS (word_mode, reg,
					      GEN_INT (-m)),
				word_mode, speed_p);
      for (i = 0; i < count; i++)
	{
	  rtx r = immed_wide_int_const (test[i].mask, word_mode);
	  cost_diff += set_src_cost (gen_rtx_AND (word_mode, reg, r),
				     word_mode, speed_p);
	  r = immed_wide_int_const (wi::lshift (test[i].mask, m), word_mode);
	  cost_diff -= set_src_cost (gen_rtx_AND (word_mode, reg, r),
				     word_mode, speed_p);
	}
      if (cost_diff > 0)
	{
	  for (i = 0; i < count; i++)
	    test[i].mask = wi::lshift (test[i].mask, m);
	  minval = build_zero_cst (TREE_TYPE (minval));
	}
    }

  /* Now build the test-and-branch code.  */

  gsi = gsi_last_bb (m_case_bb);

  /* idx = (unsigned)x - minval.  */
  idx = fold_convert_loc (loc, unsigned_index_type, index_expr);
  idx = fold_build2_loc (loc, MINUS_EXPR, unsigned_index_type, idx,
			 fold_convert_loc (loc, unsigned_index_type, minval));
  idx = force_gimple_operand_gsi (&gsi, idx,
				  /*simple=*/true, NULL_TREE,
				  /*before=*/true, GSI_SAME_STMT);

  profile_probability subtree_prob = m_subtree_prob;
  profile_probability default_prob = m_default_prob;
  if (!default_prob.initialized_p ())
    default_prob = m_subtree_prob.invert ();

  if (m_handles_entire_switch && entry_test_needed)
    {
      tree range = int_const_binop (MINUS_EXPR, maxval, minval);
      /* if (idx > range) goto default */
      range
	= force_gimple_operand_gsi (&gsi,
				    fold_convert (unsigned_index_type, range),
				    /*simple=*/true, NULL_TREE,
				    /*before=*/true, GSI_SAME_STMT);
      tmp = fold_build2 (GT_EXPR, boolean_type_node, idx, range);
      default_prob = default_prob / 2;
      basic_block new_bb
	= hoist_edge_and_branch_if_true (&gsi, tmp, default_bb,
					 default_prob, loc);
      gsi = gsi_last_bb (new_bb);
    }

  tmp = fold_build2_loc (loc, LSHIFT_EXPR, word_type_node, word_mode_one,
			 fold_convert_loc (loc, word_type_node, idx));

  /* csui = (1 << (word_mode) idx) */
  if (count > 1)
    {
      csui = make_ssa_name (word_type_node);
      tmp = force_gimple_operand_gsi (&gsi, tmp,
				     /*simple=*/false, NULL_TREE,
				     /*before=*/true, GSI_SAME_STMT);
      shift_stmt = gimple_build_assign (csui, tmp);
      gsi_insert_before (&gsi, shift_stmt, GSI_SAME_STMT);
      update_stmt (shift_stmt);
    }
  else
    csui = tmp;

  /* for each unique set of cases:
       if (const & csui) goto target  */
  for (k = 0; k < count; k++)
    {
      profile_probability prob = test[k].prob / (subtree_prob + default_prob);
      subtree_prob -= test[k].prob;
      tmp = wide_int_to_tree (word_type_node, test[k].mask);
      tmp = fold_build2_loc (loc, BIT_AND_EXPR, word_type_node, csui, tmp);
      tmp = fold_build2_loc (loc, NE_EXPR, boolean_type_node,
			     tmp, word_mode_zero);
      tmp = force_gimple_operand_gsi (&gsi, tmp,
				      /*simple=*/true, NULL_TREE,
				      /*before=*/true, GSI_SAME_STMT);
      basic_block new_bb
	= hoist_edge_and_branch_if_true (&gsi, tmp, test[k].target_bb,
					 prob, loc);
      gsi = gsi_last_bb (new_bb);
    }

  /* We should have removed all edges now.  */
  gcc_assert (EDGE_COUNT (gsi_bb (gsi)->succs) == 0);

  /* If nothing matched, go to the default label.  */
  edge e = make_edge (gsi_bb (gsi), default_bb, EDGE_FALLTHRU);
  e->probability = profile_probability::always ();
}

/* Split the basic block at the statement pointed to by GSIP, and insert
   a branch to the target basic block of E_TRUE conditional on tree
   expression COND.

   It is assumed that there is already an edge from the to-be-split
   basic block to E_TRUE->dest block.  This edge is removed, and the
   profile information on the edge is re-used for the new conditional
   jump.

   The CFG is updated.  The dominator tree will not be valid after
   this transformation, but the immediate dominators are updated if
   UPDATE_DOMINATORS is true.

   Returns the newly created basic block.  */

basic_block
bit_test_cluster::hoist_edge_and_branch_if_true (gimple_stmt_iterator *gsip,
						 tree cond, basic_block case_bb,
						 profile_probability prob,
						 location_t loc)
{
  tree tmp;
  gcond *cond_stmt;
  edge e_false;
  basic_block new_bb, split_bb = gsi_bb (*gsip);

  edge e_true = make_edge (split_bb, case_bb, EDGE_TRUE_VALUE);
  e_true->probability = prob;
  gcc_assert (e_true->src == split_bb);

  tmp = force_gimple_operand_gsi (gsip, cond, /*simple=*/true, NULL,
				  /*before=*/true, GSI_SAME_STMT);
  cond_stmt = gimple_build_cond_from_tree (tmp, NULL_TREE, NULL_TREE);
  gimple_set_location (cond_stmt, loc);
  gsi_insert_before (gsip, cond_stmt, GSI_SAME_STMT);

  e_false = split_block (split_bb, cond_stmt);
  new_bb = e_false->dest;
  redirect_edge_pred (e_true, split_bb);

  e_false->flags &= ~EDGE_FALLTHRU;
  e_false->flags |= EDGE_FALSE_VALUE;
  e_false->probability = e_true->probability.invert ();
  new_bb->count = e_false->count ();

  return new_bb;
}

/* Compute the number of case labels that correspond to each outgoing edge of
   switch statement.  Record this information in the aux field of the edge.
   Return the approx max number of cases per edge.  */

int
switch_decision_tree::compute_cases_per_edge ()
{
  int max_c = 0;
  reset_out_edges_aux (m_switch);
  int ncases = gimple_switch_num_labels (m_switch);
  for (int i = ncases - 1; i >= 1; --i)
    {
      edge case_edge = gimple_switch_edge (cfun, m_switch, i);
      case_edge->aux = (void *) ((intptr_t) (case_edge->aux) + 1);
      /* For a range case add one extra. That's enough for the bit
	 cluster heuristic.  */
      if ((intptr_t)case_edge->aux > max_c)
	max_c = (intptr_t)case_edge->aux +
		!!CASE_HIGH (gimple_switch_label (m_switch, i));
    }
  return max_c;
}

/* Analyze switch statement and return true when the statement is expanded
   as decision tree.  */

bool
switch_decision_tree::analyze_switch_statement ()
{
  unsigned l = gimple_switch_num_labels (m_switch);
  basic_block bb = gimple_bb (m_switch);
  auto_vec<cluster *> clusters;
  clusters.create (l - 1);

  basic_block default_bb = gimple_switch_default_bb (cfun, m_switch);
  m_case_bbs.reserve (l);
  m_case_bbs.quick_push (default_bb);

  int max_c = compute_cases_per_edge ();

  for (unsigned i = 1; i < l; i++)
    {
      tree elt = gimple_switch_label (m_switch, i);
      tree lab = CASE_LABEL (elt);
      basic_block case_bb = label_to_block (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      tree low = CASE_LOW (elt);
      tree high = CASE_HIGH (elt);

      profile_probability p
	= case_edge->probability / ((intptr_t) (case_edge->aux));
      clusters.quick_push (new simple_cluster (low, high, elt, case_edge->dest,
					       p));
      m_case_bbs.quick_push (case_edge->dest);
    }

  reset_out_edges_aux (m_switch);

  if (l > (unsigned) param_switch_lower_slow_alg_max_cases)
    warning_at (gimple_location (m_switch), OPT_Wdisabled_optimization,
	       "Using faster switch lowering algorithms. "
	       "Number of switch cases (%d) exceeds "
	       "%<--param=switch-lower-slow-alg-max-cases=%d%> limit.",
	       l, param_switch_lower_slow_alg_max_cases);

  /* Find bit-test clusters.  */
  vec<cluster *> output = bit_test_cluster::find_bit_tests (clusters, max_c);

  /* Find jump table clusters.  We are looking for these in the sequences of
     simple clusters which we didn't manage to convert into bit-test
     clusters.  */
  vec<cluster *> output2;
  auto_vec<cluster *> tmp;
  output2.create (1);
  tmp.create (1);

  for (unsigned i = 0; i < output.length (); i++)
    {
      cluster *c = output[i];
      if (c->get_type () != SIMPLE_CASE)
	{
	  if (!tmp.is_empty ())
	    {
	      vec<cluster *> n = jump_table_cluster::find_jump_tables (tmp);
	      output2.safe_splice (n);
	      n.release ();
	      tmp.truncate (0);
	    }
	  output2.safe_push (c);
	}
      else
	tmp.safe_push (c);
    }

  /* We still can have a temporary vector to test.  */
  if (!tmp.is_empty ())
    {
      vec<cluster *> n = jump_table_cluster::find_jump_tables (tmp);
      output2.safe_splice (n);
      n.release ();
    }

  if (dump_file)
    {
      fprintf (dump_file, ";; GIMPLE switch case clusters: ");
      for (unsigned i = 0; i < output2.length (); i++)
	output2[i]->dump (dump_file, dump_flags & TDF_DETAILS);
      fprintf (dump_file, "\n");
    }

  output.release ();

  bool expanded = try_switch_expansion (output2);
  release_clusters (output2);
  return expanded;
}

/* Attempt to expand CLUSTERS as a decision tree.  Return true when
   expanded.  */

bool
switch_decision_tree::try_switch_expansion (vec<cluster *> &clusters)
{
  tree index_expr = gimple_switch_index (m_switch);
  tree index_type = TREE_TYPE (index_expr);
  basic_block bb = gimple_bb (m_switch);

  if (gimple_switch_num_labels (m_switch) == 1
      || range_check_type (index_type) == NULL_TREE)
    return false;

  /* Find the default case target label.  */
  edge default_edge = gimple_switch_default_edge (cfun, m_switch);
  m_default_bb = default_edge->dest;

  /* Do the insertion of a case label into m_case_list.  The labels are
     fed to us in descending order from the sorted vector of case labels used
     in the tree part of the middle end.  So the list we construct is
     sorted in ascending order.  */

  for (int i = clusters.length () - 1; i >= 0; i--)
    {
      case_tree_node *r = m_case_list;
      m_case_list = m_case_node_pool.allocate ();
      m_case_list->m_right = r;
      m_case_list->m_c = clusters[i];
    }

  record_phi_operand_mapping ();

  /* Split basic block that contains the gswitch statement.  */
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  edge e;
  if (gsi_end_p (gsi))
    e = split_block_after_labels (bb);
  else
    {
      gsi_prev (&gsi);
      e = split_block (bb, gsi_stmt (gsi));
    }
  bb = split_edge (e);

  /* Create new basic blocks for non-case clusters where specific expansion
     needs to happen.  */
  for (unsigned i = 0; i < clusters.length (); i++)
    if (clusters[i]->get_type () != SIMPLE_CASE)
      {
	clusters[i]->m_case_bb = create_empty_bb (bb);
	clusters[i]->m_case_bb->count = bb->count;
	clusters[i]->m_case_bb->loop_father = bb->loop_father;
      }

  /* Do not do an extra work for a single cluster.  */
  if (clusters.length () == 1
      && clusters[0]->get_type () != SIMPLE_CASE)
    {
      cluster *c = clusters[0];
      c->emit (index_expr, index_type,
	       gimple_switch_default_label (m_switch), m_default_bb,
	       gimple_location (m_switch));
      redirect_edge_succ (single_succ_edge (bb), c->m_case_bb);
    }
  else
    {
      emit (bb, index_expr, default_edge->probability, index_type);

      /* Emit cluster-specific switch handling.  */
      for (unsigned i = 0; i < clusters.length (); i++)
	if (clusters[i]->get_type () != SIMPLE_CASE)
	  {
	    edge e = single_pred_edge (clusters[i]->m_case_bb);
	    e->dest->count = e->src->count.apply_probability (e->probability);
	    clusters[i]->emit (index_expr, index_type,
			       gimple_switch_default_label (m_switch),
			       m_default_bb, gimple_location (m_switch));
	  }
    }

  fix_phi_operands_for_edges ();

  return true;
}

/* Before switch transformation, record all SSA_NAMEs defined in switch BB
   and used in a label basic block.  */

void
switch_decision_tree::record_phi_operand_mapping ()
{
  basic_block switch_bb = gimple_bb (m_switch);
  /* Record all PHI nodes that have to be fixed after conversion.  */
  for (unsigned i = 0; i < m_case_bbs.length (); i++)
    {
      gphi_iterator gsi;
      basic_block bb = m_case_bbs[i];
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();

	  for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      basic_block phi_src_bb = gimple_phi_arg_edge (phi, i)->src;
	      if (phi_src_bb == switch_bb)
		{
		  tree def = gimple_phi_arg_def (phi, i);
		  tree result = gimple_phi_result (phi);
		  m_phi_mapping.put (result, def);
		  break;
		}
	    }
	}
    }
}

/* Append new operands to PHI statements that were introduced due to
   addition of new edges to case labels.  */

void
switch_decision_tree::fix_phi_operands_for_edges ()
{
  gphi_iterator gsi;

  for (unsigned i = 0; i < m_case_bbs.length (); i++)
    {
      basic_block bb = m_case_bbs[i];
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  for (unsigned j = 0; j < gimple_phi_num_args (phi); j++)
	    {
	      tree def = gimple_phi_arg_def (phi, j);
	      if (def == NULL_TREE)
		{
		  edge e = gimple_phi_arg_edge (phi, j);
		  tree *definition
		    = m_phi_mapping.get (gimple_phi_result (phi));
		  gcc_assert (definition);
		  add_phi_arg (phi, *definition, e, UNKNOWN_LOCATION);
		}
	    }
	}
    }
}

/* Generate a decision tree, switching on INDEX_EXPR and jumping to
   one of the labels in CASE_LIST or to the DEFAULT_LABEL.

   We generate a binary decision tree to select the appropriate target
   code.  */

void
switch_decision_tree::emit (basic_block bb, tree index_expr,
			    profile_probability default_prob, tree index_type)
{
  balance_case_nodes (&m_case_list, NULL);

  if (dump_file)
    dump_function_to_file (current_function_decl, dump_file, dump_flags);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      int indent_step = ceil_log2 (TYPE_PRECISION (index_type)) + 2;
      fprintf (dump_file, ";; Expanding GIMPLE switch as decision tree:\n");
      gcc_assert (m_case_list != NULL);
      dump_case_nodes (dump_file, m_case_list, indent_step, 0);
    }

  bb = emit_case_nodes (bb, index_expr, m_case_list, default_prob, index_type,
			gimple_location (m_switch));

  if (bb)
    emit_jump (bb, m_default_bb);

  /* Remove all edges and do just an edge that will reach default_bb.  */
  bb = gimple_bb (m_switch);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_remove (&gsi, true);

  delete_basic_block (bb);
}

/* Take an ordered list of case nodes
   and transform them into a near optimal binary tree,
   on the assumption that any target code selection value is as
   likely as any other.

   The transformation is performed by splitting the ordered
   list into two equal sections plus a pivot.  The parts are
   then attached to the pivot as left and right branches.  Each
   branch is then transformed recursively.  */

void
switch_decision_tree::balance_case_nodes (case_tree_node **head,
					  case_tree_node *parent)
{
  case_tree_node *np;

  np = *head;
  if (np)
    {
      int i = 0;
      case_tree_node **npp;
      case_tree_node *left;
      profile_probability prob = profile_probability::never ();

      /* Count the number of entries on branch.  */

      while (np)
	{
	  i++;
	  prob += np->m_c->m_prob;
	  np = np->m_right;
	}

      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;
	  profile_probability pivot_prob = prob / 2;

	  /* Find the place in the list that bisects the list's total cost
	     by probability.  */
	  while (1)
	    {
	      /* Skip nodes while their probability does not reach
		 that amount.  */
	      prob -= (*npp)->m_c->m_prob;
	      if ((prob.initialized_p () && prob < pivot_prob)
		  || ! (*npp)->m_right)
		break;
	      npp = &(*npp)->m_right;
	    }

	  np = *npp;
 	  *npp = 0;
	  *head = np;
	  np->m_parent = parent;
	  np->m_left = left == np ? NULL : left;

	  /* Optimize each of the two split parts.  */
	  balance_case_nodes (&np->m_left, np);
	  balance_case_nodes (&np->m_right, np);
	  np->m_c->m_subtree_prob = np->m_c->m_prob;
	  if (np->m_left)
	    np->m_c->m_subtree_prob += np->m_left->m_c->m_subtree_prob;
	  if (np->m_right)
	    np->m_c->m_subtree_prob += np->m_right->m_c->m_subtree_prob;
	}
      else
	{
	  /* Else leave this branch as one level,
	     but fill in `parent' fields.  */
	  np = *head;
	  np->m_parent = parent;
	  np->m_c->m_subtree_prob = np->m_c->m_prob;
	  for (; np->m_right; np = np->m_right)
	    {
	      np->m_right->m_parent = np;
	      (*head)->m_c->m_subtree_prob += np->m_right->m_c->m_subtree_prob;
	    }
	}
    }
}

/* Dump ROOT, a list or tree of case nodes, to file.  */

void
switch_decision_tree::dump_case_nodes (FILE *f, case_tree_node *root,
				       int indent_step, int indent_level)
{
  if (root == 0)
    return;
  indent_level++;

  dump_case_nodes (f, root->m_left, indent_step, indent_level);

  fputs (";; ", f);
  fprintf (f, "%*s", indent_step * indent_level, "");
  root->m_c->dump (f);
  root->m_c->m_prob.dump (f);
  fputs (" subtree: ", f);
  root->m_c->m_subtree_prob.dump (f);
  fputs (")\n", f);

  dump_case_nodes (f, root->m_right, indent_step, indent_level);
}


/* Add an unconditional jump to CASE_BB that happens in basic block BB.  */

void
switch_decision_tree::emit_jump (basic_block bb, basic_block case_bb)
{
  edge e = single_succ_edge (bb);
  redirect_edge_succ (e, case_bb);
}

/* Generate code to compare OP0 with OP1 so that the condition codes are
   set and to jump to LABEL_BB if the condition is true.
   COMPARISON is the GIMPLE comparison (EQ, NE, GT, etc.).
   PROB is the probability of jumping to LABEL_BB.  */

basic_block
switch_decision_tree::emit_cmp_and_jump_insns (basic_block bb, tree op0,
					       tree op1, tree_code comparison,
					       basic_block label_bb,
					       profile_probability prob,
					       location_t loc)
{
  // TODO: it's once called with lhs != index.
  op1 = fold_convert (TREE_TYPE (op0), op1);

  gcond *cond = gimple_build_cond (comparison, op0, op1, NULL_TREE, NULL_TREE);
  gimple_set_location (cond, loc);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, cond, GSI_NEW_STMT);

  gcc_assert (single_succ_p (bb));

  /* Make a new basic block where false branch will take place.  */
  edge false_edge = split_block (bb, cond);
  false_edge->flags = EDGE_FALSE_VALUE;
  false_edge->probability = prob.invert ();
  false_edge->dest->count = bb->count.apply_probability (prob.invert ());

  edge true_edge = make_edge (bb, label_bb, EDGE_TRUE_VALUE);
  true_edge->probability = prob;

  return false_edge->dest;
}

/* Generate code to jump to LABEL if OP0 and OP1 are equal.
   PROB is the probability of jumping to LABEL_BB.
   BB is a basic block where the new condition will be placed.  */

basic_block
switch_decision_tree::do_jump_if_equal (basic_block bb, tree op0, tree op1,
					basic_block label_bb,
					profile_probability prob,
					location_t loc)
{
  op1 = fold_convert (TREE_TYPE (op0), op1);

  gcond *cond = gimple_build_cond (EQ_EXPR, op0, op1, NULL_TREE, NULL_TREE);
  gimple_set_location (cond, loc);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_before (&gsi, cond, GSI_SAME_STMT);

  gcc_assert (single_succ_p (bb));

  /* Make a new basic block where false branch will take place.  */
  edge false_edge = split_block (bb, cond);
  false_edge->flags = EDGE_FALSE_VALUE;
  false_edge->probability = prob.invert ();
  false_edge->dest->count = bb->count.apply_probability (prob.invert ());

  edge true_edge = make_edge (bb, label_bb, EDGE_TRUE_VALUE);
  true_edge->probability = prob;

  return false_edge->dest;
}

/* Emit step-by-step code to select a case for the value of INDEX.
   The thus generated decision tree follows the form of the
   case-node binary tree NODE, whose nodes represent test conditions.
   DEFAULT_PROB is probability of cases leading to default BB.
   INDEX_TYPE is the type of the index of the switch.  */

basic_block
switch_decision_tree::emit_case_nodes (basic_block bb, tree index,
				       case_tree_node *node,
				       profile_probability default_prob,
				       tree index_type, location_t loc)
{
  profile_probability p;

  /* If node is null, we are done.  */
  if (node == NULL)
    return bb;

  /* Single value case.  */
  if (node->m_c->is_single_value_p ())
    {
      /* Node is single valued.  First see if the index expression matches
	 this node and then check our children, if any.  */
      p = node->m_c->m_prob / (node->m_c->m_subtree_prob + default_prob);
      bb = do_jump_if_equal (bb, index, node->m_c->get_low (),
			     node->m_c->m_case_bb, p, loc);
      /* Since this case is taken at this point, reduce its weight from
	 subtree_weight.  */
      node->m_c->m_subtree_prob -= node->m_c->m_prob;

      if (node->m_left != NULL && node->m_right != NULL)
	{
	  /* 1) the node has both children

	     If both children are single-valued cases with no
	     children, finish up all the work.  This way, we can save
	     one ordered comparison.  */

	  if (!node->m_left->has_child ()
	      && node->m_left->m_c->is_single_value_p ()
	      && !node->m_right->has_child ()
	      && node->m_right->m_c->is_single_value_p ())
	    {
	      p = (node->m_right->m_c->m_prob
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = do_jump_if_equal (bb, index, node->m_right->m_c->get_low (),
				     node->m_right->m_c->m_case_bb, p, loc);
	      node->m_c->m_subtree_prob -= node->m_right->m_c->m_prob;

	      p = (node->m_left->m_c->m_prob
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = do_jump_if_equal (bb, index, node->m_left->m_c->get_low (),
				     node->m_left->m_c->m_case_bb, p, loc);
	    }
	  else
	    {
	      /* Branch to a label where we will handle it later.  */
	      basic_block test_bb = split_edge (single_succ_edge (bb));
	      redirect_edge_succ (single_pred_edge (test_bb),
				  single_succ_edge (bb)->dest);

	      p = ((node->m_right->m_c->m_subtree_prob + default_prob / 2)
		   / (node->m_c->m_subtree_prob + default_prob));
	      test_bb->count = bb->count.apply_probability (p);
	      bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_high (),
					    GT_EXPR, test_bb, p, loc);
	      default_prob /= 2;

	      /* Handle the left-hand subtree.  */
	      bb = emit_case_nodes (bb, index, node->m_left,
				    default_prob, index_type, loc);

	      /* If the left-hand subtree fell through,
		 don't let it fall into the right-hand subtree.  */
	      if (bb && m_default_bb)
		emit_jump (bb, m_default_bb);

	      bb = emit_case_nodes (test_bb, index, node->m_right,
				    default_prob, index_type, loc);
	    }
	}
      else if (node->m_left == NULL && node->m_right != NULL)
	{
	  /* 2) the node has only right child.  */

	  /* Here we have a right child but no left so we issue a conditional
	     branch to default and process the right child.

	     Omit the conditional branch to default if the right child
	     does not have any children and is single valued; it would
	     cost too much space to save so little time.  */

	  if (node->m_right->has_child ()
	      || !node->m_right->m_c->is_single_value_p ())
	    {
	      p = ((default_prob / 2)
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_low (),
					    LT_EXPR, m_default_bb, p, loc);
	      default_prob /= 2;

	      bb = emit_case_nodes (bb, index, node->m_right, default_prob,
				    index_type, loc);
	    }
	  else
	    {
	      /* We cannot process node->right normally
		 since we haven't ruled out the numbers less than
		 this node's value.  So handle node->right explicitly.  */
	      p = (node->m_right->m_c->m_subtree_prob
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = do_jump_if_equal (bb, index, node->m_right->m_c->get_low (),
				     node->m_right->m_c->m_case_bb, p, loc);
	    }
	}
      else if (node->m_left != NULL && node->m_right == NULL)
	{
	  /* 3) just one subtree, on the left.  Similar case as previous.  */

	  if (node->m_left->has_child ()
	      || !node->m_left->m_c->is_single_value_p ())
	    {
	      p = ((default_prob / 2)
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_high (),
					    GT_EXPR, m_default_bb, p, loc);
	      default_prob /= 2;

	      bb = emit_case_nodes (bb, index, node->m_left, default_prob,
				    index_type, loc);
	    }
	  else
	    {
	      /* We cannot process node->left normally
		 since we haven't ruled out the numbers less than
		 this node's value.  So handle node->left explicitly.  */
	      p = (node->m_left->m_c->m_subtree_prob
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = do_jump_if_equal (bb, index, node->m_left->m_c->get_low (),
				     node->m_left->m_c->m_case_bb, p, loc);
	    }
	}
    }
  else
    {
      /* Node is a range.  These cases are very similar to those for a single
	 value, except that we do not start by testing whether this node
	 is the one to branch to.  */
      if (node->has_child () || node->m_c->get_type () != SIMPLE_CASE)
	{
	  bool is_bt = node->m_c->get_type () == BIT_TEST;
	  int parts = is_bt ? 3 : 2;

	  /* Branch to a label where we will handle it later.  */
	  basic_block test_bb = split_edge (single_succ_edge (bb));
	  redirect_edge_succ (single_pred_edge (test_bb),
			      single_succ_edge (bb)->dest);

	  profile_probability right_prob = profile_probability::never ();
	  if (node->m_right)
	    right_prob = node->m_right->m_c->m_subtree_prob;
	  p = ((right_prob + default_prob / parts)
	       / (node->m_c->m_subtree_prob + default_prob));
	  test_bb->count = bb->count.apply_probability (p);

	  bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_high (),
					GT_EXPR, test_bb, p, loc);

	  default_prob /= parts;
	  node->m_c->m_subtree_prob -= right_prob;
	  if (is_bt)
	    node->m_c->m_default_prob = default_prob;

	   /* Value belongs to this node or to the left-hand subtree.  */
	   p = node->m_c->m_prob / (node->m_c->m_subtree_prob + default_prob);
	   bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_low (),
					 GE_EXPR, node->m_c->m_case_bb, p, loc);

	   /* Handle the left-hand subtree.  */
	   bb = emit_case_nodes (bb, index, node->m_left, default_prob,
				 index_type, loc);

	   /* If the left-hand subtree fell through,
	      don't let it fall into the right-hand subtree.  */
	   if (bb && m_default_bb)
	     emit_jump (bb, m_default_bb);

	   bb = emit_case_nodes (test_bb, index, node->m_right, default_prob,
				 index_type, loc);
	}
      else
	{
	  /* Node has no children so we check low and high bounds to remove
	     redundant tests.  Only one of the bounds can exist,
	     since otherwise this node is bounded--a case tested already.  */
	  tree lhs, rhs;
	  generate_range_test (bb, index, node->m_c->get_low (),
			       node->m_c->get_high (), &lhs, &rhs);
	  p = default_prob / (node->m_c->m_subtree_prob + default_prob);

	  bb = emit_cmp_and_jump_insns (bb, lhs, rhs, GT_EXPR,
					m_default_bb, p, loc);

	  emit_jump (bb, node->m_c->m_case_bb);
	  return NULL;
	}
    }

  return bb;
}

/* The main function of the pass scans statements for switches and invokes
   process_switch on them.  */

namespace {

const pass_data pass_data_convert_switch =
{
  GIMPLE_PASS, /* type */
  "switchconv", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SWITCH_CONVERSION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_convert_switch : public gimple_opt_pass
{
public:
  pass_convert_switch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_convert_switch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
  {
    return flag_tree_switch_conversion != 0;
  }
  unsigned int execute (function *) final override;

}; // class pass_convert_switch

unsigned int
pass_convert_switch::execute (function *fun)
{
  basic_block bb;
  bool cfg_altered = false;

  FOR_EACH_BB_FN (bb, fun)
  {
    if (gswitch *stmt = safe_dyn_cast <gswitch *> (*gsi_last_bb (bb)))
      {
	if (dump_file)
	  {
	    expanded_location loc = expand_location (gimple_location (stmt));

	    fprintf (dump_file, "beginning to process the following "
		     "SWITCH statement (%s:%d) : ------- \n",
		     loc.file, loc.line);
	    print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    putc ('\n', dump_file);
	  }

	switch_conversion sconv;
	sconv.expand (stmt);
	cfg_altered |= sconv.m_cfg_altered;
	if (!sconv.m_reason)
	  {
	    if (dump_file)
	      {
		fputs ("Switch converted\n", dump_file);
		fputs ("--------------------------------\n", dump_file);
	      }

	    /* Make no effort to update the post-dominator tree.
	       It is actually not that hard for the transformations
	       we have performed, but it is not supported
	       by iterate_fix_dominators.  */
	    free_dominance_info (CDI_POST_DOMINATORS);
	  }
	else
	  {
	    if (dump_file)
	      {
		fputs ("Bailing out - ", dump_file);
		fputs (sconv.m_reason, dump_file);
		fputs ("\n--------------------------------\n", dump_file);
	      }
	  }
      }
  }

  return cfg_altered ? TODO_cleanup_cfg : 0;;
}

} // anon namespace

gimple_opt_pass *
make_pass_convert_switch (gcc::context *ctxt)
{
  return new pass_convert_switch (ctxt);
}

/* The main function of the pass scans statements for switches and invokes
   process_switch on them.  */

namespace {

template <bool O0> class pass_lower_switch: public gimple_opt_pass
{
public:
  pass_lower_switch (gcc::context *ctxt) : gimple_opt_pass (data, ctxt) {}

  static const pass_data data;
  opt_pass *
  clone () final override
  {
    return new pass_lower_switch<O0> (m_ctxt);
  }

  bool
  gate (function *) final override
  {
    return !O0 || !optimize;
  }

  unsigned int execute (function *fun) final override;
}; // class pass_lower_switch

template <bool O0>
const pass_data pass_lower_switch<O0>::data = {
  GIMPLE_PASS,		       /* type */
  O0 ? "switchlower_O0" : "switchlower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SWITCH_LOWERING, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

template <bool O0>
unsigned int
pass_lower_switch<O0>::execute (function *fun)
{
  basic_block bb;
  bool expanded = false;

  auto_vec<gimple *> switch_statements;
  switch_statements.create (1);

  FOR_EACH_BB_FN (bb, fun)
    {
      if (gswitch *swtch = safe_dyn_cast <gswitch *> (*gsi_last_bb (bb)))
	{
	  if (!O0)
	    group_case_labels_stmt (swtch);
	  switch_statements.safe_push (swtch);
	}
    }

  for (unsigned i = 0; i < switch_statements.length (); i++)
    {
      gimple *stmt = switch_statements[i];
      if (dump_file)
	{
	  expanded_location loc = expand_location (gimple_location (stmt));

	  fprintf (dump_file, "beginning to process the following "
		   "SWITCH statement (%s:%d) : ------- \n",
		   loc.file, loc.line);
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	  putc ('\n', dump_file);
	}

      gswitch *swtch = dyn_cast<gswitch *> (stmt);
      if (swtch)
	{
	  switch_decision_tree dt (swtch);
	  expanded |= dt.analyze_switch_statement ();
	}
    }

  if (expanded)
    {
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      mark_virtual_operands_for_renaming (cfun);
    }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_lower_switch_O0 (gcc::context *ctxt)
{
  return new pass_lower_switch<true> (ctxt);
}
gimple_opt_pass *
make_pass_lower_switch (gcc::context *ctxt)
{
  return new pass_lower_switch<false> (ctxt);
}
