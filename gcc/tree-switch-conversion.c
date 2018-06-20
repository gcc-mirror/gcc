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
  basic_block first;

  m_switch = swtch;

  /* The gimplifier has already sorted the cases by CASE_LOW and ensured there
     is a default label which is the first in the vector.
     Collect the bits we can deduce from the CFG.  */
  m_index_expr = gimple_switch_index (swtch);
  m_switch_bb = gimple_bb (swtch);
  m_default_bb
    = label_to_block (CASE_LABEL (gimple_switch_default_label (swtch)));
  e_default = find_edge (m_switch_bb, m_default_bb);
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
    {
      first = label_to_block (CASE_LABEL (gimple_switch_label (swtch, 1)));
      e_first = find_edge (m_switch_bb, first);
    }
  else
    {
      first = m_default_bb;
      e_first = e_default;
    }

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
		      tree lab = CASE_LABEL (gimple_switch_label (m_switch, i));
		      if (label_to_block (lab) == bb)
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
  basic_block bb = label_to_block (CASE_LABEL (default_case));
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
      basic_block bb = label_to_block (CASE_LABEL (cs));
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

  /* A switch on a constant should have been optimized in tree-cfg-cleanup.  */
  gcc_checking_assert (!TREE_CONSTANT (m_index_expr));

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

struct case_node
{
  case_node		*left;	/* Left son in binary tree.  */
  case_node		*right;	/* Right son in binary tree;
				   also node chain.  */
  case_node		*parent; /* Parent of node in binary tree.  */
  tree			low;	/* Lowest index value for this label.  */
  tree			high;	/* Highest index value for this label.  */
  basic_block		case_bb; /* Label to jump to when node matches.  */
  tree			case_label; /* Label to jump to when node matches.  */
  profile_probability   prob; /* Probability of taking this case.  */
  profile_probability   subtree_prob;  /* Probability of reaching subtree
					  rooted at this node.  */
};

typedef case_node *case_node_ptr;

static basic_block emit_case_nodes (basic_block, tree, case_node_ptr,
				    basic_block, tree, profile_probability,
				    tree, hash_map<tree, tree> *);

/* Return the smallest number of different values for which it is best to use a
   jump-table instead of a tree of conditional branches.  */

static unsigned int
case_values_threshold (void)
{
  unsigned int threshold = PARAM_VALUE (PARAM_CASE_VALUES_THRESHOLD);

  if (threshold == 0)
    threshold = targetm.case_values_threshold ();

  return threshold;
}

/* Reset the aux field of all outgoing edges of basic block BB.  */

static inline void
reset_out_edges_aux (basic_block bb)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    e->aux = (void *) 0;
}

/* Compute the number of case labels that correspond to each outgoing edge of
   STMT.  Record this information in the aux field of the edge.  */

static inline void
compute_cases_per_edge (gswitch *stmt)
{
  basic_block bb = gimple_bb (stmt);
  reset_out_edges_aux (bb);
  int ncases = gimple_switch_num_labels (stmt);
  for (int i = ncases - 1; i >= 1; --i)
    {
      tree elt = gimple_switch_label (stmt, i);
      tree lab = CASE_LABEL (elt);
      basic_block case_bb = label_to_block_fn (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      case_edge->aux = (void *) ((intptr_t) (case_edge->aux) + 1);
    }
}

/* Do the insertion of a case label into case_list.  The labels are
   fed to us in descending order from the sorted vector of case labels used
   in the tree part of the middle end.  So the list we construct is
   sorted in ascending order.

   LABEL is the case label to be inserted.  LOW and HIGH are the bounds
   against which the index is compared to jump to LABEL and PROB is the
   estimated probability LABEL is reached from the switch statement.  */

static case_node *
add_case_node (case_node *head, tree low, tree high, basic_block case_bb,
	       tree case_label, profile_probability prob,
	       object_allocator<case_node> &case_node_pool)
{
  case_node *r;

  gcc_checking_assert (low);
  gcc_checking_assert (high && (TREE_TYPE (low) == TREE_TYPE (high)));

  /* Add this label to the chain.  */
  r = case_node_pool.allocate ();
  r->low = low;
  r->high = high;
  r->case_bb = case_bb;
  r->case_label = case_label;
  r->parent = r->left = NULL;
  r->prob = prob;
  r->subtree_prob = prob;
  r->right = head;
  return r;
}

/* Dump ROOT, a list or tree of case nodes, to file.  */

static void
dump_case_nodes (FILE *f, case_node *root, int indent_step, int indent_level)
{
  if (root == 0)
    return;
  indent_level++;

  dump_case_nodes (f, root->left, indent_step, indent_level);

  fputs (";; ", f);
  fprintf (f, "%*s", indent_step * indent_level, "");
  print_dec (wi::to_wide (root->low), f, TYPE_SIGN (TREE_TYPE (root->low)));
  if (!tree_int_cst_equal (root->low, root->high))
    {
      fprintf (f, " ... ");
      print_dec (wi::to_wide (root->high), f,
		 TYPE_SIGN (TREE_TYPE (root->high)));
    }
  fputs ("\n", f);

  dump_case_nodes (f, root->right, indent_step, indent_level);
}

/* Take an ordered list of case nodes
   and transform them into a near optimal binary tree,
   on the assumption that any target code selection value is as
   likely as any other.

   The transformation is performed by splitting the ordered
   list into two equal sections plus a pivot.  The parts are
   then attached to the pivot as left and right branches.  Each
   branch is then transformed recursively.  */

static void
balance_case_nodes (case_node_ptr *head, case_node_ptr parent)
{
  case_node_ptr np;

  np = *head;
  if (np)
    {
      int i = 0;
      int ranges = 0;
      case_node_ptr *npp;
      case_node_ptr left;

      /* Count the number of entries on branch.  Also count the ranges.  */

      while (np)
	{
	  if (!tree_int_cst_equal (np->low, np->high))
	    ranges++;

	  i++;
	  np = np->right;
	}

      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;

	  /* If there are just three nodes, split at the middle one.  */
	  if (i == 3)
	    npp = &(*npp)->right;
	  else
	    {
	      /* Find the place in the list that bisects the list's total cost,
		 where ranges count as 2.
		 Here I gets half the total cost.  */
	      i = (i + ranges + 1) / 2;
	      while (1)
		{
		  /* Skip nodes while their cost does not reach that amount.  */
		  if (!tree_int_cst_equal ((*npp)->low, (*npp)->high))
		    i--;
		  i--;
		  if (i <= 0)
		    break;
		  npp = &(*npp)->right;
		}
	    }
	  *head = np = *npp;
	  *npp = 0;
	  np->parent = parent;
	  np->left = left;

	  /* Optimize each of the two split parts.  */
	  balance_case_nodes (&np->left, np);
	  balance_case_nodes (&np->right, np);
	  np->subtree_prob = np->prob;
	  np->subtree_prob += np->left->subtree_prob;
	  np->subtree_prob += np->right->subtree_prob;
	}
      else
	{
	  /* Else leave this branch as one level,
	     but fill in `parent' fields.  */
	  np = *head;
	  np->parent = parent;
	  np->subtree_prob = np->prob;
	  for (; np->right; np = np->right)
	    {
	      np->right->parent = np;
	      (*head)->subtree_prob += np->right->subtree_prob;
	    }
	}
    }
}

/* Return true if a switch should be expanded as a decision tree.
   RANGE is the difference between highest and lowest case.
   UNIQ is number of unique case node targets, not counting the default case.
   COUNT is the number of comparisons needed, not counting the default case.  */

static bool
expand_switch_as_decision_tree_p (tree range,
				  unsigned int uniq ATTRIBUTE_UNUSED,
				  unsigned int count)
{
  int max_ratio;

  /* If neither casesi or tablejump is available, or flag_jump_tables
     over-ruled us, we really have no choice.  */
  if (!targetm.have_casesi () && !targetm.have_tablejump ())
    return true;
  if (!flag_jump_tables)
    return true;
#ifndef ASM_OUTPUT_ADDR_DIFF_ELT
  if (flag_pic)
    return true;
#endif

  /* If the switch is relatively small such that the cost of one
     indirect jump on the target are higher than the cost of a
     decision tree, go with the decision tree.

     If range of values is much bigger than number of values,
     or if it is too large to represent in a HOST_WIDE_INT,
     make a sequence of conditional branches instead of a dispatch.

     The definition of "much bigger" depends on whether we are
     optimizing for size or for speed.  If the former, the maximum
     ratio range/count = 3, because this was found to be the optimal
     ratio for size on i686-pc-linux-gnu, see PR11823.  The ratio
     10 is much older, and was probably selected after an extensive
     benchmarking investigation on numerous platforms.  Or maybe it
     just made sense to someone at some point in the history of GCC,
     who knows...  */
  max_ratio = optimize_insn_for_size_p () ? 3 : 10;
  if (count < case_values_threshold () || !tree_fits_uhwi_p (range)
      || compare_tree_int (range, max_ratio * count) > 0)
    return true;

  return false;
}

static void
fix_phi_operands_for_edge (edge e, hash_map<tree, tree> *phi_mapping)
{
  basic_block bb = e->dest;
  gphi_iterator gsi;
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();

      tree *definition = phi_mapping->get (gimple_phi_result (phi));
      if (definition)
	add_phi_arg (phi, *definition, e, UNKNOWN_LOCATION);
    }
}


/* Add an unconditional jump to CASE_BB that happens in basic block BB.  */

static void
emit_jump (basic_block bb, basic_block case_bb,
	   hash_map<tree, tree> *phi_mapping)
{
  edge e = single_succ_edge (bb);
  redirect_edge_succ (e, case_bb);
  fix_phi_operands_for_edge (e, phi_mapping);
}

/* Generate a decision tree, switching on INDEX_EXPR and jumping to
   one of the labels in CASE_LIST or to the DEFAULT_LABEL.
   DEFAULT_PROB is the estimated probability that it jumps to
   DEFAULT_LABEL.

   We generate a binary decision tree to select the appropriate target
   code.  */

static void
emit_case_decision_tree (gswitch *s, tree index_expr, tree index_type,
			 case_node_ptr case_list, basic_block default_bb,
			 tree default_label, profile_probability default_prob,
			 hash_map<tree, tree> *phi_mapping)
{
  balance_case_nodes (&case_list, NULL);

  if (dump_file)
    dump_function_to_file (current_function_decl, dump_file, dump_flags);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      int indent_step = ceil_log2 (TYPE_PRECISION (index_type)) + 2;
      fprintf (dump_file, ";; Expanding GIMPLE switch as decision tree:\n");
      dump_case_nodes (dump_file, case_list, indent_step, 0);
    }

  basic_block bb = gimple_bb (s);
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

  bb = emit_case_nodes (bb, index_expr, case_list, default_bb, default_label,
			default_prob, index_type, phi_mapping);

  if (bb)
    emit_jump (bb, default_bb, phi_mapping);

  /* Remove all edges and do just an edge that will reach default_bb.  */
  gsi = gsi_last_bb (gimple_bb (s));
  gsi_remove (&gsi, true);
}

static void
record_phi_operand_mapping (const vec<basic_block> bbs, basic_block switch_bb,
			    hash_map <tree, tree> *map)
{
  /* Record all PHI nodes that have to be fixed after conversion.  */
  for (unsigned i = 0; i < bbs.length (); i++)
    {
      basic_block bb = bbs[i];

      gphi_iterator gsi;
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
		  map->put (result, def);
		  break;
		}
	    }
	}
    }
}

/* Attempt to expand gimple switch STMT to a decision tree.  */

static bool
try_switch_expansion (gswitch *stmt)
{
  tree minval = NULL_TREE, maxval = NULL_TREE, range = NULL_TREE;
  basic_block default_bb;
  unsigned int count, uniq;
  int i;
  int ncases = gimple_switch_num_labels (stmt);
  tree index_expr = gimple_switch_index (stmt);
  tree index_type = TREE_TYPE (index_expr);
  tree elt;
  basic_block bb = gimple_bb (stmt);

  hash_map<tree, tree> phi_mapping;
  auto_vec<basic_block> case_bbs;

  /* A list of case labels; it is first built as a list and it may then
     be rearranged into a nearly balanced binary tree.  */
  case_node *case_list = 0;

  /* A pool for case nodes.  */
  object_allocator<case_node> case_node_pool ("struct case_node pool");

  /* cleanup_tree_cfg removes all SWITCH_EXPR with their index
     expressions being INTEGER_CST.  */
  gcc_assert (TREE_CODE (index_expr) != INTEGER_CST);

  if (ncases == 1)
    return false;

  /* Find the default case target label.  */
  tree default_label = CASE_LABEL (gimple_switch_default_label (stmt));
  default_bb = label_to_block_fn (cfun, default_label);
  edge default_edge = find_edge (bb, default_bb);
  profile_probability default_prob = default_edge->probability;
  case_bbs.safe_push (default_bb);

  /* Get upper and lower bounds of case values.  */
  elt = gimple_switch_label (stmt, 1);
  minval = fold_convert (index_type, CASE_LOW (elt));
  elt = gimple_switch_label (stmt, ncases - 1);
  if (CASE_HIGH (elt))
    maxval = fold_convert (index_type, CASE_HIGH (elt));
  else
    maxval = fold_convert (index_type, CASE_LOW (elt));

  /* Compute span of values.  */
  range = fold_build2 (MINUS_EXPR, index_type, maxval, minval);

  /* Listify the labels queue and gather some numbers to decide
     how to expand this switch.  */
  uniq = 0;
  count = 0;
  hash_set<tree> seen_labels;
  compute_cases_per_edge (stmt);

  for (i = ncases - 1; i >= 1; --i)
    {
      elt = gimple_switch_label (stmt, i);
      tree low = CASE_LOW (elt);
      gcc_assert (low);
      tree high = CASE_HIGH (elt);
      gcc_assert (!high || tree_int_cst_lt (low, high));
      tree lab = CASE_LABEL (elt);

      /* Count the elements.
	 A range counts double, since it requires two compares.  */
      count++;
      if (high)
	count++;

      /* If we have not seen this label yet, then increase the
	 number of unique case node targets seen.  */
      if (!seen_labels.add (lab))
	uniq++;

      /* The bounds on the case range, LOW and HIGH, have to be converted
	 to case's index type TYPE.  Note that the original type of the
	 case index in the source code is usually "lost" during
	 gimplification due to type promotion, but the case labels retain the
	 original type.  Make sure to drop overflow flags.  */
      low = fold_convert (index_type, low);
      if (TREE_OVERFLOW (low))
	low = wide_int_to_tree (index_type, wi::to_wide (low));

      /* The canonical from of a case label in GIMPLE is that a simple case
	 has an empty CASE_HIGH.  For the casesi and tablejump expanders,
	 the back ends want simple cases to have high == low.  */
      if (!high)
	high = low;
      high = fold_convert (index_type, high);
      if (TREE_OVERFLOW (high))
	high = wide_int_to_tree (index_type, wi::to_wide (high));

      basic_block case_bb = label_to_block_fn (cfun, lab);
      edge case_edge = find_edge (bb, case_bb);
      case_list = add_case_node (
	case_list, low, high, case_bb, lab,
	case_edge->probability.apply_scale (1, (intptr_t) (case_edge->aux)),
	case_node_pool);

      case_bbs.safe_push (case_bb);
    }
  reset_out_edges_aux (bb);
  record_phi_operand_mapping (case_bbs, bb, &phi_mapping);

  /* cleanup_tree_cfg removes all SWITCH_EXPR with a single
     destination, such as one with a default case only.
     It also removes cases that are out of range for the switch
     type, so we should never get a zero here.  */
  gcc_assert (count > 0);

  /* Decide how to expand this switch.
     The two options at this point are a dispatch table (casesi or
     tablejump) or a decision tree.  */

  if (expand_switch_as_decision_tree_p (range, uniq, count))
    {
      emit_case_decision_tree (stmt, index_expr, index_type, case_list,
			       default_bb, default_label, default_prob,
			       &phi_mapping);
      return true;
    }

  return false;
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

	  expanded |= try_switch_expansion (as_a<gswitch *> (stmt));
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

/* Generate code to compare X with Y so that the condition codes are
   set and to jump to LABEL if the condition is true.  If X is a
   constant and Y is not a constant, then the comparison is swapped to
   ensure that the comparison RTL has the canonical form.

   UNSIGNEDP nonzero says that X and Y are unsigned; this matters if they
   need to be widened.  UNSIGNEDP is also used to select the proper
   branch condition code.

   If X and Y have mode BLKmode, then SIZE specifies the size of both X and Y.

   MODE is the mode of the inputs (in case they are const_int).

   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).
   It will be potentially converted into an unsigned variant based on
   UNSIGNEDP to select a proper jump instruction.

   PROB is the probability of jumping to LABEL.  */

static basic_block
emit_cmp_and_jump_insns (basic_block bb, tree op0, tree op1,
			 tree_code comparison, basic_block label_bb,
			 profile_probability prob,
			 hash_map<tree, tree> *phi_mapping)
{
  gcond *cond = gimple_build_cond (comparison, op0, op1, NULL_TREE, NULL_TREE);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, cond, GSI_NEW_STMT);

  gcc_assert (single_succ_p (bb));

  /* Make a new basic block where false branch will take place.  */
  edge false_edge = split_block (bb, cond);
  false_edge->flags = EDGE_FALSE_VALUE;
  false_edge->probability = prob.invert ();

  edge true_edge = make_edge (bb, label_bb, EDGE_TRUE_VALUE);
  fix_phi_operands_for_edge (true_edge, phi_mapping);
  true_edge->probability = prob;

  return false_edge->dest;
}

/* Computes the conditional probability of jumping to a target if the branch
   instruction is executed.
   TARGET_PROB is the estimated probability of jumping to a target relative
   to some basic block BB.
   BASE_PROB is the probability of reaching the branch instruction relative
   to the same basic block BB.  */

static inline profile_probability
conditional_probability (profile_probability target_prob,
			 profile_probability base_prob)
{
  return target_prob / base_prob;
}

/* Emit step-by-step code to select a case for the value of INDEX.
   The thus generated decision tree follows the form of the
   case-node binary tree NODE, whose nodes represent test conditions.
   INDEX_TYPE is the type of the index of the switch.  */

static basic_block
emit_case_nodes (basic_block bb, tree index, case_node_ptr node,
		 basic_block default_bb, tree default_label,
		 profile_probability default_prob, tree index_type,
		 hash_map<tree, tree> *phi_mapping)
{
  /* If node is null, we are done.  */
  if (node == NULL)
    return bb;

  /* Branch to a label where we will handle it later.  */
  basic_block test_bb = split_edge (single_succ_edge (bb));
  redirect_edge_succ (single_pred_edge (test_bb),
		      single_succ_edge (bb)->dest);

  profile_probability probability
    = node->right ? node->right->subtree_prob : profile_probability::never ();
  probability
    = conditional_probability (probability + default_prob.apply_scale (1, 2),
			       node->subtree_prob + default_prob);
  bb = emit_cmp_and_jump_insns (bb, index, node->high, GT_EXPR,
				test_bb, probability, phi_mapping);
  default_prob = default_prob.apply_scale (1, 2);

  /* Value belongs to this node or to the left-hand subtree.  */
  probability
    = conditional_probability (node->prob, node->subtree_prob + default_prob);
  bb = emit_cmp_and_jump_insns (bb, index, node->low, GE_EXPR,
				node->case_bb, probability,
				phi_mapping);

  /* Handle the left-hand subtree.  */
  bb = emit_case_nodes (bb, index, node->left, default_bb,
			default_label, default_prob, index_type,
			phi_mapping);

  /* If the left-hand subtree fell through,
     don't let it fall into the right-hand subtree.  */
  if (default_bb)
    emit_jump (bb, default_bb, phi_mapping);

  bb = emit_case_nodes (test_bb, index, node->right, default_bb,
			default_label, default_prob, index_type,
			phi_mapping);

  return bb;
}
