/* Lower GIMPLE_SWITCH expressions to something more efficient than
   a jump table.
   Copyright (C) 2006-2018 Free Software Foundation, Inc.

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
#include "params.h"
#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "alloc-pool.h"
#include "target.h"
#include "tree-into-ssa.h"
#include "omp-general.h"

/* ??? For lang_hooks.types.type_for_mode, but is there a word_mode
   type in the GIMPLE type system that is language-independent?  */
#include "langhooks.h"

#include "tree-switch-conversion.h"

using namespace tree_switch_conversion;

/* Constructor.  */

switch_conversion::switch_conversion (): m_final_bb (NULL), m_other_count (),
  m_constructors (NULL), m_default_values (NULL),
  m_arr_ref_first (NULL), m_arr_ref_last (NULL),
  m_reason (NULL), m_default_case_nonstandard (false), m_cfg_altered (false)
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
  m_default_count = e_default->count ();
  FOR_EACH_EDGE (e, ei, m_switch_bb->succs)
    if (e != e_default)
      m_other_count += e->count ();

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
  if (m_final_bb)
    FOR_EACH_EDGE (e, ei, m_switch_bb->succs)
      {
	if (e->dest == m_final_bb)
	  continue;

	if (single_pred_p (e->dest)
	    && single_succ_p (e->dest)
	    && single_succ (e->dest) == m_final_bb)
	  continue;

	if (e == e_default && m_contiguous_range)
	  {
	    m_default_case_nonstandard = true;
	    continue;
	  }

	m_final_bb = NULL;
	break;
      }

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

  /* Get the number of unique non-default targets out of the GIMPLE_SWITCH
     block.  Assume a CFG cleanup would have already removed degenerate
     switch statements, this allows us to just use EDGE_COUNT.  */
  m_uniq = EDGE_COUNT (gimple_bb (swtch)->succs) - 1;
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
      > ((unsigned) m_count * SWITCH_CONVERSION_BRANCH_RATIO))
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
	      elt.value = unshare_expr_without_location (val);
	      m_constructors[j]->quick_push (elt);

	      pos = int_const_binop (PLUS_EXPR, pos, pos_one);
	    } while (!tree_int_cst_lt (high, pos)
		     && tree_int_cst_lt (low, pos));
	  j++;
	}
    }
}

/* If all values in the constructor vector are the same, return the value.
   Otherwise return NULL_TREE.  Not supposed to be called for empty
   vectors.  */

tree
switch_conversion::contains_same_values_p (vec<constructor_elt, va_gc> *vec)
{
  unsigned int i;
  tree prev = NULL_TREE;
  constructor_elt *elt;

  FOR_EACH_VEC_SAFE_ELT (vec, i, elt)
    {
      if (!prev)
	prev = elt->value;
      else if (!operand_equal_p (elt->value, prev, OEP_ONLY_CONST))
	return NULL_TREE;
    }
  return prev;
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

  if (!INTEGRAL_TYPE_P (type))
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
  tree name, cst;
  gimple *load;
  gimple_stmt_iterator gsi = gsi_for_stmt (m_switch);
  location_t loc = gimple_location (m_switch);

  gcc_assert (m_default_values[num]);

  name = copy_ssa_name (PHI_RESULT (phi));
  m_target_inbound_names[num] = name;

  cst = contains_same_values_p (m_constructors[num]);
  if (cst)
    load = gimple_build_assign (name, cst);
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

	  FOR_EACH_VEC_SAFE_ELT (m_constructors[num], i, elt)
	    elt->value = fold_convert (value_type, elt->value);
	}
      ctor = build_constructor (array_type, m_constructors[num]);
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
  tree tidx, sub, utype;
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
  else
    utype = lang_hooks.types.type_for_mode (TYPE_MODE (utype), 1);

  arr_index_type = build_index_type (m_range_size);
  tidx = make_ssa_name (utype);
  sub = fold_build2_loc (loc, MINUS_EXPR, utype,
			 fold_convert_loc (loc, utype, m_index_expr),
			 fold_convert_loc (loc, utype, m_range_min));
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
  prune_bbs (bbd, m_final_bb,
	     m_default_case_nonstandard ? m_default_bb : NULL);

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
  gcc_assert (gimple_switch_num_labels (swtch) >= 2);

  collect (swtch);

  /* No error markers should reach here (they should be filtered out
     during gimplification).  */
  gcc_checking_assert (TREE_TYPE (m_index_expr) != error_mark_node);

  /* A switch on a constant should have been optimized in tree-cfg-cleanup.  */
  gcc_checking_assert (!TREE_CONSTANT (m_index_expr));

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

  /* Check the case label values are within reasonable range:  */
  if (!check_range ())
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
      comparison_count += sc->m_range_p ? 2 : 1;
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
			  tree default_label_expr, basic_block default_bb)
{
  unsigned HOST_WIDE_INT range = get_range (get_low (), get_high ());
  unsigned HOST_WIDE_INT nondefault_range = 0;

  /* For jump table we just emit a new gswitch statement that will
     be latter lowered to jump table.  */
  auto_vec <tree> labels;
  labels.create (m_cases.length ());

  make_edge (m_case_bb, default_bb, 0);
  for (unsigned i = 0; i < m_cases.length (); i++)
    {
      labels.quick_push (unshare_expr (m_cases[i]->m_case_label_expr));
      make_edge (m_case_bb, m_cases[i]->m_case_bb, 0);
    }

  gswitch *s = gimple_build_switch (index_expr,
				    unshare_expr (default_label_expr), labels);
  gimple_stmt_iterator gsi = gsi_start_bb (m_case_bb);
  gsi_insert_after (&gsi, s, GSI_NEW_STMT);

  /* Set up even probabilities for all cases.  */
  for (unsigned i = 0; i < m_cases.length (); i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (m_cases[i]);
      edge case_edge = find_edge (m_case_bb, sc->m_case_bb);
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
      edge case_edge = find_edge (m_case_bb, sc->m_case_bb);
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
  auto_vec<min_cluster_item> min;
  min.reserve (l + 1);

  min.quick_push (min_cluster_item (0, 0, 0));

  for (unsigned i = 1; i <= l; i++)
    {
      /* Set minimal # of clusters with i-th item to infinite.  */
      min.quick_push (min_cluster_item (INT_MAX, INT_MAX, INT_MAX));

      for (unsigned j = 0; j < i; j++)
	{
	  unsigned HOST_WIDE_INT s = min[j].m_non_jt_cases;
	  if (i - j < case_values_threshold ())
	    s += i - j;

	  /* Prefer clusters with smaller number of numbers covered.  */
	  if ((min[j].m_count + 1 < min[i].m_count
	       || (min[j].m_count + 1 == min[i].m_count
		   && s < min[i].m_non_jt_cases))
	      && can_be_handled (clusters, j, i - 1))
	    min[i] = min_cluster_item (min[j].m_count + 1, j, s);
	}

      gcc_checking_assert (min[i].m_count != INT_MAX);
    }

  /* No result.  */
  if (min[l].m_count == INT_MAX)
    return clusters.copy ();

  vec<cluster *> output;
  output.create (4);

  /* Find and build the clusters.  */
  for (int end = l;;)
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
				    unsigned start, unsigned end)
{
  /* If the switch is relatively small such that the cost of one
     indirect jump on the target are higher than the cost of a
     decision tree, go with the decision tree.

     If range of values is much bigger than number of values,
     or if it is too large to represent in a HOST_WIDE_INT,
     make a sequence of conditional branches instead of a dispatch.

     The definition of "much bigger" depends on whether we are
     optimizing for size or for speed.  */
  if (!flag_jump_tables)
    return false;

  /* For algorithm correctness, jump table for a single case must return
     true.  We bail out in is_beneficial if it's called just for
     a single case.  */
  if (start == end)
    return true;

  unsigned HOST_WIDE_INT max_ratio
    = optimize_insn_for_size_p () ? max_ratio_for_size : max_ratio_for_speed;
  unsigned HOST_WIDE_INT range = get_range (clusters[start]->get_low (),
					    clusters[end]->get_high ());
  /* Check overflow.  */
  if (range == 0)
    return false;

  unsigned HOST_WIDE_INT comparison_count = 0;
  for (unsigned i = start; i <= end; i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (clusters[i]);
      comparison_count += sc->m_range_p ? 2 : 1;
    }

  return range <= max_ratio * comparison_count;
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

/* Definition of jump_table_cluster constants.  */

const unsigned HOST_WIDE_INT jump_table_cluster::max_ratio_for_size;
const unsigned HOST_WIDE_INT jump_table_cluster::max_ratio_for_speed;

/* Find bit tests of given CLUSTERS, where all members of the vector
   are of type simple_cluster.  New clusters are returned.  */

vec<cluster *>
bit_test_cluster::find_bit_tests (vec<cluster *> &clusters)
{
  vec<cluster *> output;
  output.create (4);

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
  if (min[l].m_count == INT_MAX)
    return clusters.copy ();

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
	for (int i = end - 1; i >=  start; i--)
	  output.safe_push (clusters[i]);

      end = start;

      if (start <= 0)
	break;
    }

  output.reverse ();
  return output;
}

/* Return true when RANGE of case values with UNIQ labels
   can build a bit test.  */

bool
bit_test_cluster::can_be_handled (unsigned HOST_WIDE_INT range,
				  unsigned int uniq)
{
  /* Check overflow.  */
  if (range == 0)
    return 0;

  if (range >= GET_MODE_BITSIZE (word_mode))
    return false;

  return uniq <= 3;
}

/* Return true when cluster starting at START and ending at END (inclusive)
   can build a bit test.  */

bool
bit_test_cluster::can_be_handled (const vec<cluster *> &clusters,
				  unsigned start, unsigned end)
{
  /* For algorithm correctness, bit test for a single case must return
     true.  We bail out in is_beneficial if it's called just for
     a single case.  */
  if (start == end)
    return true;

  unsigned HOST_WIDE_INT range = get_range (clusters[start]->get_low (),
					    clusters[end]->get_high ());
  auto_bitmap dest_bbs;

  for (unsigned i = start; i <= end; i++)
    {
      simple_cluster *sc = static_cast<simple_cluster *> (clusters[i]);
      bitmap_set_bit (dest_bbs, sc->m_case_bb->index);
    }

  return can_be_handled (range, bitmap_count_bits (dest_bbs));
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
  const struct case_bit_test *const d1 = (const struct case_bit_test *) p1;
  const struct case_bit_test *const d2 = (const struct case_bit_test *) p2;

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
			tree, basic_block default_bb)
{
  struct case_bit_test test[m_max_case_bit_tests] = { {} };
  unsigned int i, j, k;
  unsigned int count;

  tree unsigned_index_type = unsigned_type_for (index_type);

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
  tree range = int_const_binop (MINUS_EXPR, maxval, minval);
  unsigned HOST_WIDE_INT bt_range = get_range (minval, maxval);

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
	  count++;
	}

      test[k].bits += n->get_range (n->get_low (), n->get_high ());

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

  /* If all values are in the 0 .. BITS_PER_WORD-1 range, we can get rid of
     the minval subtractions, but it might make the mask constants more
     expensive.  So, compare the costs.  */
  if (compare_tree_int (minval, 0) > 0
      && compare_tree_int (maxval, GET_MODE_BITSIZE (word_mode)) < 0)
    {
      int cost_diff;
      HOST_WIDE_INT m = tree_to_uhwi (minval);
      rtx reg = gen_raw_REG (word_mode, 10000);
      bool speed_p = optimize_insn_for_speed_p ();
      cost_diff = set_rtx_cost (gen_rtx_PLUS (word_mode, reg,
					      GEN_INT (-m)), speed_p);
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
	  range = maxval;
	}
    }

  /* Now build the test-and-branch code.  */

  gsi = gsi_last_bb (m_case_bb);

  /* idx = (unsigned)x - minval.  */
  idx = fold_convert (unsigned_index_type, index_expr);
  idx = fold_build2 (MINUS_EXPR, unsigned_index_type, idx,
		     fold_convert (unsigned_index_type, minval));
  idx = force_gimple_operand_gsi (&gsi, idx,
				  /*simple=*/true, NULL_TREE,
				  /*before=*/true, GSI_SAME_STMT);

  if (m_handles_entire_switch)
    {
      /* if (idx > range) goto default */
      range
	= force_gimple_operand_gsi (&gsi,
				    fold_convert (unsigned_index_type, range),
				    /*simple=*/true, NULL_TREE,
				    /*before=*/true, GSI_SAME_STMT);
      tmp = fold_build2 (GT_EXPR, boolean_type_node, idx, range);
      basic_block new_bb
	= hoist_edge_and_branch_if_true (&gsi, tmp, default_bb,
					 profile_probability::unlikely ());
      gsi = gsi_last_bb (new_bb);
    }

  /* csui = (1 << (word_mode) idx) */
  csui = make_ssa_name (word_type_node);
  tmp = fold_build2 (LSHIFT_EXPR, word_type_node, word_mode_one,
		     fold_convert (word_type_node, idx));
  tmp = force_gimple_operand_gsi (&gsi, tmp,
				  /*simple=*/false, NULL_TREE,
				  /*before=*/true, GSI_SAME_STMT);
  shift_stmt = gimple_build_assign (csui, tmp);
  gsi_insert_before (&gsi, shift_stmt, GSI_SAME_STMT);
  update_stmt (shift_stmt);

  profile_probability prob = profile_probability::always ();

  /* for each unique set of cases:
       if (const & csui) goto target  */
  for (k = 0; k < count; k++)
    {
      prob = profile_probability::always ().apply_scale (test[k].bits,
							 bt_range);
      bt_range -= test[k].bits;
      tmp = wide_int_to_tree (word_type_node, test[k].mask);
      tmp = fold_build2 (BIT_AND_EXPR, word_type_node, csui, tmp);
      tmp = force_gimple_operand_gsi (&gsi, tmp,
				      /*simple=*/true, NULL_TREE,
				      /*before=*/true, GSI_SAME_STMT);
      tmp = fold_build2 (NE_EXPR, boolean_type_node, tmp, word_mode_zero);
      basic_block new_bb
	= hoist_edge_and_branch_if_true (&gsi, tmp, test[k].target_bb, prob);
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
						 profile_probability prob)
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
   switch statement.  Record this information in the aux field of the edge.  */

void
switch_decision_tree::compute_cases_per_edge ()
{
  reset_out_edges_aux (m_switch);
  int ncases = gimple_switch_num_labels (m_switch);
  for (int i = ncases - 1; i >= 1; --i)
    {
      edge case_edge = gimple_switch_edge (cfun, m_switch, i);
      case_edge->aux = (void *) ((intptr_t) (case_edge->aux) + 1);
    }
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

  compute_cases_per_edge ();

  for (unsigned i = 1; i < l; i++)
    {
      tree elt = gimple_switch_label (m_switch, i);
      tree lab = CASE_LABEL (elt);
      basic_block case_bb = label_to_block (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      tree low = CASE_LOW (elt);
      tree high = CASE_HIGH (elt);

      profile_probability p
	= case_edge->probability.apply_scale (1, (intptr_t) (case_edge->aux));
      clusters.quick_push (new simple_cluster (low, high, elt, case_edge->dest,
					       p));
      m_case_bbs.quick_push (case_edge->dest);
    }

  reset_out_edges_aux (m_switch);

  /* Find jump table clusters.  */
  vec<cluster *> output = jump_table_cluster::find_jump_tables (clusters);

  /* Find bit test clusters.  */
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
	      vec<cluster *> n = bit_test_cluster::find_bit_tests (tmp);
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
      vec<cluster *> n = bit_test_cluster::find_bit_tests (tmp);
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

  for (unsigned i = 0; i < output2.length (); i++)
    delete output2[i];

  output2.release ();

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

  if (gimple_switch_num_labels (m_switch) == 1)
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
	clusters[i]->m_case_bb->loop_father = bb->loop_father;
      }

  /* Do not do an extra work for a single cluster.  */
  if (clusters.length () == 1
      && clusters[0]->get_type () != SIMPLE_CASE)
    {
      cluster *c = clusters[0];
      c->emit (index_expr, index_type,
	       gimple_switch_default_label (m_switch), m_default_bb);
      redirect_edge_succ (single_succ_edge (bb), c->m_case_bb);
    }
  else
    {
      emit (bb, index_expr, default_edge->probability, index_type);

      /* Emit cluster-specific switch handling.  */
      for (unsigned i = 0; i < clusters.length (); i++)
	if (clusters[i]->get_type () != SIMPLE_CASE)
	  clusters[i]->emit (index_expr, index_type,
			     gimple_switch_default_label (m_switch),
			     m_default_bb);
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

  bb = emit_case_nodes (bb, index_expr, m_case_list, default_prob, index_type);

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
      int ranges = 0;
      case_tree_node **npp;
      case_tree_node *left;
      profile_probability prob = profile_probability::never ();

      /* Count the number of entries on branch.  Also count the ranges.  */

      while (np)
	{
	  if (!tree_int_cst_equal (np->m_c->get_low (), np->m_c->get_high ()))
	    ranges++;

	  i++;
	  prob += np->m_c->m_prob;
	  np = np->m_right;
	}

      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;
	  profile_probability pivot_prob = prob.apply_scale (1, 2);

	  /* Find the place in the list that bisects the list's total cost,
	     where ranges count as 2.  */
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
					       profile_probability prob)
{
  // TODO: it's once called with lhs != index.
  op1 = fold_convert (TREE_TYPE (op0), op1);

  gcond *cond = gimple_build_cond (comparison, op0, op1, NULL_TREE, NULL_TREE);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, cond, GSI_NEW_STMT);

  gcc_assert (single_succ_p (bb));

  /* Make a new basic block where false branch will take place.  */
  edge false_edge = split_block (bb, cond);
  false_edge->flags = EDGE_FALSE_VALUE;
  false_edge->probability = prob.invert ();

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
					profile_probability prob)
{
  op1 = fold_convert (TREE_TYPE (op0), op1);

  gcond *cond = gimple_build_cond (EQ_EXPR, op0, op1, NULL_TREE, NULL_TREE);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_before (&gsi, cond, GSI_SAME_STMT);

  gcc_assert (single_succ_p (bb));

  /* Make a new basic block where false branch will take place.  */
  edge false_edge = split_block (bb, cond);
  false_edge->flags = EDGE_FALSE_VALUE;
  false_edge->probability = prob.invert ();

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
				       tree index_type)
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
			     node->m_c->m_case_bb, p);
      /* Since this case is taken at this point, reduce its weight from
	 subtree_weight.  */
      node->m_c->m_subtree_prob -= p;

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
				     node->m_right->m_c->m_case_bb, p);

	      p = (node->m_left->m_c->m_prob
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = do_jump_if_equal (bb, index, node->m_left->m_c->get_low (),
				     node->m_left->m_c->m_case_bb, p);
	    }
	  else
	    {
	      /* Branch to a label where we will handle it later.  */
	      basic_block test_bb = split_edge (single_succ_edge (bb));
	      redirect_edge_succ (single_pred_edge (test_bb),
				  single_succ_edge (bb)->dest);

	      p = ((node->m_right->m_c->m_subtree_prob
		    + default_prob.apply_scale (1, 2))
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_high (),
					    GT_EXPR, test_bb, p);
	      default_prob = default_prob.apply_scale (1, 2);

	      /* Handle the left-hand subtree.  */
	      bb = emit_case_nodes (bb, index, node->m_left,
				    default_prob, index_type);

	      /* If the left-hand subtree fell through,
		 don't let it fall into the right-hand subtree.  */
	      if (bb && m_default_bb)
		emit_jump (bb, m_default_bb);

	      bb = emit_case_nodes (test_bb, index, node->m_right,
				    default_prob, index_type);
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
	      p = (default_prob.apply_scale (1, 2)
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_low (),
					    LT_EXPR, m_default_bb, p);
	      default_prob = default_prob.apply_scale (1, 2);

	      bb = emit_case_nodes (bb, index, node->m_right, default_prob,
				    index_type);
	    }
	  else
	    {
	      /* We cannot process node->right normally
		 since we haven't ruled out the numbers less than
		 this node's value.  So handle node->right explicitly.  */
	      p = (node->m_right->m_c->m_subtree_prob
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = do_jump_if_equal (bb, index, node->m_right->m_c->get_low (),
				     node->m_right->m_c->m_case_bb, p);
	    }
	}
      else if (node->m_left != NULL && node->m_right == NULL)
	{
	  /* 3) just one subtree, on the left.  Similar case as previous.  */

	  if (node->m_left->has_child ()
	      || !node->m_left->m_c->is_single_value_p ())
	    {
	      p = (default_prob.apply_scale (1, 2)
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_high (),
					    GT_EXPR, m_default_bb, p);
		  default_prob = default_prob.apply_scale (1, 2);

	      bb = emit_case_nodes (bb, index, node->m_left, default_prob,
				    index_type);
	    }
	  else
	    {
	      /* We cannot process node->left normally
		 since we haven't ruled out the numbers less than
		 this node's value.  So handle node->left explicitly.  */
	      p = (node->m_left->m_c->m_subtree_prob
		   / (node->m_c->m_subtree_prob + default_prob));
	      bb = do_jump_if_equal (bb, index, node->m_left->m_c->get_low (),
				     node->m_left->m_c->m_case_bb, p);
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
	  /* Branch to a label where we will handle it later.  */
	  basic_block test_bb = split_edge (single_succ_edge (bb));
	  redirect_edge_succ (single_pred_edge (test_bb),
			      single_succ_edge (bb)->dest);


	   profile_probability right_prob = profile_probability::never ();
	   if (node->m_right)
	     right_prob = node->m_right->m_c->m_subtree_prob;
	  p = ((right_prob + default_prob.apply_scale (1, 2))
	       / (node->m_c->m_subtree_prob + default_prob));

	  bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_high (),
					GT_EXPR, test_bb, p);
	  default_prob = default_prob.apply_scale (1, 2);

	  /* Value belongs to this node or to the left-hand subtree.  */
	  p = node->m_c->m_prob / (node->m_c->m_subtree_prob + default_prob);
	  bb = emit_cmp_and_jump_insns (bb, index, node->m_c->get_low (),
					GE_EXPR, node->m_c->m_case_bb, p);

	  /* Handle the left-hand subtree.  */
	  bb = emit_case_nodes (bb, index, node->m_left,
				default_prob, index_type);

	  /* If the left-hand subtree fell through,
	     don't let it fall into the right-hand subtree.  */
	  if (bb && m_default_bb)
	    emit_jump (bb, m_default_bb);

	  bb = emit_case_nodes (test_bb, index, node->m_right,
				default_prob, index_type);
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
					m_default_bb, p);

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
  virtual bool gate (function *) { return flag_tree_switch_conversion != 0; }
  virtual unsigned int execute (function *);

}; // class pass_convert_switch

unsigned int
pass_convert_switch::execute (function *fun)
{
  basic_block bb;
  bool cfg_altered = false;

  FOR_EACH_BB_FN (bb, fun)
  {
    gimple *stmt = last_stmt (bb);
    if (stmt && gimple_code (stmt) == GIMPLE_SWITCH)
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
	sconv.expand (as_a <gswitch *> (stmt));
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
  clone ()
  {
    return new pass_lower_switch<O0> (m_ctxt);
  }

  virtual bool
  gate (function *)
  {
    return !O0 || !optimize;
  }

  virtual unsigned int execute (function *fun);
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
      gimple *stmt = last_stmt (bb);
      if (stmt && gimple_code (stmt) == GIMPLE_SWITCH)
	switch_statements.safe_push (stmt);
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


