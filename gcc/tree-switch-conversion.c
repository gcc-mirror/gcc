/* Switch Conversion converts variable initializations based on switch
   statements to initializations from a static array.
   Copyright (C) 2006, 2008 Free Software Foundation, Inc.
   Contributed by Martin Jambor <jamborm@suse.cz>

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

/*
     Switch initialization conversion

The following pass changes simple initializations of scalars in a switch
statement into initializations from a static array.  Obviously, the values must
be constant and known at compile time and a default branch must be
provided.  For example, the following code:

        int a,b;

        switch (argc)
	{
         case 1:
         case 2:
                a_1 = 8;
                b_1 = 6;
                break;
         case 3:
                a_2 = 9;
                b_2 = 5;
                break;
         case 12:
                a_3 = 10;
                b_3 = 4;
                break;
         default:
                a_4 = 16;
                b_4 = 1;
        }
	a_5 = PHI <a_1, a_2, a_3, a_4>
	b_5 = PHI <b_1, b_2, b_3, b_4>


is changed into:

        static const int = CSWTCH01[] = {6, 6, 5, 1, 1, 1, 1, 1, 1, 1, 1, 4};
        static const int = CSWTCH02[] = {8, 8, 9, 16, 16, 16, 16, 16, 16, 16,
                                 16, 16, 10};

        if (((unsigned) argc) - 1 < 11)
          {
	    a_6 = CSWTCH02[argc - 1];
            b_6 = CSWTCH01[argc - 1];
	  }
	else
	  {
	    a_7 = 16;
	    b_7 = 1;
          }
	  a_5 = PHI <a_6, a_7>
	  b_b = PHI <b_6, b_7>

There are further constraints.  Specifically, the range of values across all
case labels must not be bigger than SWITCH_CONVERSION_BRANCH_RATIO (default
eight) times the number of the actual switch branches. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include <signal.h>

#include "line-map.h"
#include "params.h"
#include "flags.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-flow-inline.h"
#include "tree-ssa-operands.h"
#include "output.h"
#include "input.h"
#include "tree-pass.h"
#include "diagnostic.h"
#include "tree-dump.h"
#include "timevar.h"

/* The main structure of the pass.  */
struct switch_conv_info
{
  /* The expression used to decide the switch branch.  (It is subsequently used
     as the index to the created array.) */
  tree index_expr;

  /* The following integer constants store the minimum value covered by the
     cases.  */
  tree range_min;

  /* The difference between the above two numbers, i.e. The size of the array
     that would have to be created by the transformation.  */
  tree range_size;

  /* Basic block that contains the actual SWITCH_EXPR.  */
  basic_block switch_bb;

  /* All branches of the switch statement must have a single successor stored in
     the following variable.  */
  basic_block final_bb;

  /* Number of phi nodes in the final bb (that we'll be replacing).  */
  int phi_count;

  /* Array of default values, in the same order as phi nodes.  */
  tree *default_values;

  /* Constructors of new static arrays.  */
  VEC (constructor_elt, gc) **constructors;

  /* Array of ssa names that are initialized with a value from a new static
     array.  */
  tree *target_inbound_names;

  /* Array of ssa names that are initialized with the default value if the
     switch expression is out of range.  */
  tree *target_outbound_names;

  /* The probability of the default edge in the replaced switch.  */
  int default_prob;

  /* The count of the default edge in the replaced switch.  */
  gcov_type default_count;

  /* Combined count of all other (non-default) edges in the replaced switch.  */
  gcov_type other_count;

  /* The first load statement that loads a temporary from a new static array.
   */
  gimple arr_ref_first;

  /* The last load statement that loads a temporary from a new static array.  */
  gimple arr_ref_last;

  /* String reason why the case wasn't a good candidate that is written to the
     dump file, if there is one.  */
  const char *reason;
};

/* Global pass info.  */
static struct switch_conv_info info;


/* Checks whether the range given by individual case statements of the SWTCH
   switch statement isn't too big and whether the number of branches actually
   satisfies the size of the new array.  */

static bool
check_range (gimple swtch)
{
  tree min_case, max_case;
  unsigned int branch_num = gimple_switch_num_labels (swtch);
  tree range_max;

  /* The gimplifier has already sorted the cases by CASE_LOW and ensured there
     is a default label which is the last in the vector.  */

  min_case = gimple_switch_label (swtch, 1);
  info.range_min = CASE_LOW (min_case);

  gcc_assert (branch_num > 1);
  gcc_assert (CASE_LOW (gimple_switch_label (swtch, 0)) == NULL_TREE);
  max_case = gimple_switch_label (swtch, branch_num - 1);
  if (CASE_HIGH (max_case) != NULL_TREE)
    range_max = CASE_HIGH (max_case);
  else
    range_max = CASE_LOW (max_case);

  gcc_assert (info.range_min);
  gcc_assert (range_max);

  info.range_size = int_const_binop (MINUS_EXPR, range_max, info.range_min, 0);

  gcc_assert (info.range_size);
  if (!host_integerp (info.range_size, 1))
    {
      info.reason = "index range way too large or otherwise unusable.\n";
      return false;
    }

  if ((unsigned HOST_WIDE_INT) tree_low_cst (info.range_size, 1)
      > ((unsigned) branch_num * SWITCH_CONVERSION_BRANCH_RATIO))
    {
      info.reason = "the maximum range-branch ratio exceeded.\n";
      return false;
    }

  return true;
}

/* Checks the given CS switch case whether it is suitable for conversion
   (whether all but the default basic blocks are empty and so on).  If it is,
   adds the case to the branch list along with values for the defined variables
   and returns true.  Otherwise returns false.  */

static bool
check_process_case (tree cs)
{
  tree ldecl;
  basic_block label_bb, following_bb;
  edge e;

  ldecl = CASE_LABEL (cs);
  label_bb = label_to_block (ldecl);

  e = find_edge (info.switch_bb, label_bb);
  gcc_assert (e);

  if (CASE_LOW (cs) == NULL_TREE)
    {
      /* Default branch.  */
      info.default_prob = e->probability;
      info.default_count = e->count;
    }
  else
    info.other_count += e->count;

  if (!label_bb)
    {
      info.reason = "  Bad case - cs BB  label is NULL\n";
      return false;
    }

  if (!single_pred_p (label_bb))
    {
      if (info.final_bb && info.final_bb != label_bb)
	{
	  info.reason = "  Bad case - a non-final BB has two predecessors\n";
	  return false; /* sth complex going on in this branch  */
	}

      following_bb = label_bb;
    }
  else
    {
      if (!empty_block_p (label_bb))
	{
	  info.reason = "  Bad case - a non-final BB not empty\n";
	  return false;
	}

      e = single_succ_edge (label_bb);
      following_bb = single_succ (label_bb);
    }

  if (!info.final_bb)
    info.final_bb = following_bb;
  else if (info.final_bb != following_bb)
    {
      info.reason = "  Bad case - different final BB\n";
      return false; /* the only successor is not common for all the branches */
    }

  return true;
}

/* This function checks whether all required values in phi nodes in final_bb
   are constants.  Required values are those that correspond to a basic block
   which is a part of the examined switch statement.  It returns true if the
   phi nodes are OK, otherwise false.  */

static bool
check_final_bb (void)
{
  gimple_stmt_iterator gsi;

  info.phi_count = 0;
  for (gsi = gsi_start_phis (info.final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      unsigned int i;

      info.phi_count++;

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  basic_block bb = gimple_phi_arg_edge (phi, i)->src;

	  if (bb == info.switch_bb
	      || (single_pred_p (bb) && single_pred (bb) == info.switch_bb))
	    {
	      tree reloc, val;

	      val = gimple_phi_arg_def (phi, i);
	      if (!is_gimple_ip_invariant (val))
		{
		  info.reason = "   Non-invariant value from a case\n";
		  return false; /* Non-invariant argument.  */
		}
	      reloc = initializer_constant_valid_p (val, TREE_TYPE (val));
	      if ((flag_pic && reloc != null_pointer_node)
		  || (!flag_pic && reloc == NULL_TREE))
		{
		  if (reloc)
		    info.reason
		      = "   Value from a case would need runtime relocations\n";
		  else
		    info.reason
		      = "   Value from a case is not a valid initializer\n";
		  return false;
		}
	    }
	}
    }

  return true;
}

/* The following function allocates default_values, target_{in,out}_names and
   constructors arrays.  The last one is also populated with pointers to
   vectors that will become constructors of new arrays.  */

static void
create_temp_arrays (void)
{
  int i;

  info.default_values = (tree *) xcalloc (info.phi_count, sizeof (tree));
  info.constructors = (VEC (constructor_elt, gc) **) xcalloc (info.phi_count,
							      sizeof (tree));
  info.target_inbound_names = (tree *) xcalloc (info.phi_count, sizeof (tree));
  info.target_outbound_names = (tree *) xcalloc (info.phi_count,
						 sizeof (tree));

  for (i = 0; i < info.phi_count; i++)
    info.constructors[i]
      = VEC_alloc (constructor_elt, gc, tree_low_cst (info.range_size, 1) + 1);
}

/* Free the arrays created by create_temp_arrays().  The vectors that are
   created by that function are not freed here, however, because they have
   already become constructors and must be preserved.  */

static void
free_temp_arrays (void)
{
  free (info.constructors);
  free (info.default_values);
  free (info.target_inbound_names);
  free (info.target_outbound_names);
}

/* Populate the array of default values in the order of phi nodes.
   DEFAULT_CASE is the CASE_LABEL_EXPR for the default switch branch.  */

static void
gather_default_values (tree default_case)
{
  gimple_stmt_iterator gsi;
  basic_block bb = label_to_block (CASE_LABEL (default_case));
  edge e;
  int i = 0;

  gcc_assert (CASE_LOW (default_case) == NULL_TREE);

  if (bb == info.final_bb)
    e = find_edge (info.switch_bb, bb);
  else
    e = single_succ_edge (bb);

  for (gsi = gsi_start_phis (info.final_bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);
      tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
      gcc_assert (val);
      info.default_values[i++] = val;
    }
}

/* The following function populates the vectors in the constructors array with
   future contents of the static arrays.  The vectors are populated in the
   order of phi nodes.  SWTCH is the switch statement being converted.  */

static void
build_constructors (gimple swtch)
{
  unsigned i, branch_num = gimple_switch_num_labels (swtch);
  tree pos = info.range_min;

  for (i = 1; i < branch_num; i++)
    {
      tree cs = gimple_switch_label (swtch, i);
      basic_block bb = label_to_block (CASE_LABEL (cs));
      edge e;
      tree high;
      gimple_stmt_iterator gsi;
      int j;

      if (bb == info.final_bb)
	e = find_edge (info.switch_bb, bb);
      else
	e = single_succ_edge (bb);
      gcc_assert (e);

      while (tree_int_cst_lt (pos, CASE_LOW (cs)))
	{
	  int k;
	  for (k = 0; k < info.phi_count; k++)
	    {
	      constructor_elt *elt;

	      elt = VEC_quick_push (constructor_elt,
				    info.constructors[k], NULL);
	      elt->index = int_const_binop (MINUS_EXPR, pos,
					    info.range_min, 0);
	      elt->value = info.default_values[k];
	    }

	  pos = int_const_binop (PLUS_EXPR, pos, integer_one_node, 0);
	}
      gcc_assert (tree_int_cst_equal (pos, CASE_LOW (cs)));

      j = 0;
      if (CASE_HIGH (cs))
	high = CASE_HIGH (cs);
      else
	high = CASE_LOW (cs);
      for (gsi = gsi_start_phis (info.final_bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  tree val = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  tree low = CASE_LOW (cs);
	  pos = CASE_LOW (cs);

	  do
	    {
	      constructor_elt *elt;

	      elt = VEC_quick_push (constructor_elt,
				    info.constructors[j], NULL);
	      elt->index = int_const_binop (MINUS_EXPR, pos, info.range_min, 0);
	      elt->value = val;

	      pos = int_const_binop (PLUS_EXPR, pos, integer_one_node, 0);
	    } while (!tree_int_cst_lt (high, pos)
		     && tree_int_cst_lt (low, pos));
	  j++;
	}
    }
}

/* If all values in the constructor vector are the same, return the value.
   Otherwise return NULL_TREE.  Not supposed to be called for empty
   vectors.  */

static tree
constructor_contains_same_values_p (VEC (constructor_elt, gc) *vec)
{
  int i, len = VEC_length (constructor_elt, vec);
  tree prev = NULL_TREE;

  for (i = 0; i < len; i++)
    {
      constructor_elt *elt = VEC_index (constructor_elt, vec, i);

      if (!prev)
	prev = elt->value;
      else if (!operand_equal_p (elt->value, prev, OEP_ONLY_CONST))
	return NULL_TREE;
    }
  return prev;
}

/* Create an appropriate array type and declaration and assemble a static array
   variable.  Also create a load statement that initializes the variable in
   question with a value from the static array.  SWTCH is the switch statement
   being converted, NUM is the index to arrays of constructors, default values
   and target SSA names for this particular array.  ARR_INDEX_TYPE is the type
   of the index of the new array, PHI is the phi node of the final BB that
   corresponds to the value that will be loaded from the created array.  TIDX
   is an ssa name of a temporary variable holding the index for loads from the
   new array.  */

static void
build_one_array (gimple swtch, int num, tree arr_index_type, gimple phi,
		 tree tidx)
{
  tree name, cst;
  gimple load;
  gimple_stmt_iterator gsi = gsi_for_stmt (swtch);
  location_t loc = gimple_location (swtch);

  gcc_assert (info.default_values[num]);

  name = make_ssa_name (SSA_NAME_VAR (PHI_RESULT (phi)), NULL);
  info.target_inbound_names[num] = name;

  cst = constructor_contains_same_values_p (info.constructors[num]);
  if (cst)
    load = gimple_build_assign (name, cst);
  else
    {
      tree array_type, ctor, decl, value_type, fetch;

      value_type = TREE_TYPE (info.default_values[num]);
      array_type = build_array_type (value_type, arr_index_type);
      ctor = build_constructor (array_type, info.constructors[num]);
      TREE_CONSTANT (ctor) = true;

      decl = build_decl (loc, VAR_DECL, NULL_TREE, array_type);
      TREE_STATIC (decl) = 1;
      DECL_INITIAL (decl) = ctor;

      DECL_NAME (decl) = create_tmp_var_name ("CSWTCH");
      DECL_ARTIFICIAL (decl) = 1;
      TREE_CONSTANT (decl) = 1;
      add_referenced_var (decl);
      varpool_mark_needed_node (varpool_node (decl));
      varpool_finalize_decl (decl);

      fetch = build4 (ARRAY_REF, value_type, decl, tidx, NULL_TREE,
		      NULL_TREE);
      load = gimple_build_assign (name, fetch);
    }

  SSA_NAME_DEF_STMT (name) = load;
  gsi_insert_before (&gsi, load, GSI_SAME_STMT);
  update_stmt (load);
  info.arr_ref_last = load;
}

/* Builds and initializes static arrays initialized with values gathered from
   the SWTCH switch statement.  Also creates statements that load values from
   them.  */

static void
build_arrays (gimple swtch)
{
  tree arr_index_type;
  tree tidx, sub, tmp;
  gimple stmt;
  gimple_stmt_iterator gsi;
  int i;
  location_t loc = gimple_location (swtch);

  gsi = gsi_for_stmt (swtch);

  arr_index_type = build_index_type (info.range_size);
  tmp = create_tmp_var (TREE_TYPE (info.index_expr), "csti");
  add_referenced_var (tmp);
  tidx = make_ssa_name (tmp, NULL);
  sub = fold_build2_loc (loc, MINUS_EXPR,
		     TREE_TYPE (info.index_expr), info.index_expr,
		     fold_convert_loc (loc, TREE_TYPE (info.index_expr),
				       info.range_min));
  sub = force_gimple_operand_gsi (&gsi, sub,
				  false, NULL, true, GSI_SAME_STMT);
  stmt = gimple_build_assign (tidx, sub);
  SSA_NAME_DEF_STMT (tidx) = stmt;

  gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
  update_stmt (stmt);
  info.arr_ref_first = stmt;

  for (gsi = gsi_start_phis (info.final_bb), i = 0;
       !gsi_end_p (gsi); gsi_next (&gsi), i++)
    build_one_array (swtch, i, arr_index_type, gsi_stmt (gsi), tidx);
}

/* Generates and appropriately inserts loads of default values at the position
   given by BSI.  Returns the last inserted statement.  */

static gimple
gen_def_assigns (gimple_stmt_iterator *gsi)
{
  int i;
  gimple assign = NULL;

  for (i = 0; i < info.phi_count; i++)
    {
      tree name
	= make_ssa_name (SSA_NAME_VAR (info.target_inbound_names[i]), NULL);

      info.target_outbound_names[i] = name;
      assign = gimple_build_assign (name, info.default_values[i]);
      SSA_NAME_DEF_STMT (name) = assign;
      gsi_insert_before (gsi, assign, GSI_SAME_STMT);
      update_stmt (assign);
    }
  return assign;
}

/* Deletes the unused bbs and edges that now contain the switch statement and
   its empty branch bbs.  BBD is the now dead BB containing the original switch
   statement, FINAL is the last BB of the converted switch statement (in terms
   of succession).  */

static void
prune_bbs (basic_block bbd, basic_block final)
{
  edge_iterator ei;
  edge e;

  for (ei = ei_start (bbd->succs); (e = ei_safe_edge (ei)); )
    {
      basic_block bb;
      bb = e->dest;
      remove_edge (e);
      if (bb != final)
	delete_basic_block (bb);
    }
  delete_basic_block (bbd);
}

/* Add values to phi nodes in final_bb for the two new edges.  E1F is the edge
   from the basic block loading values from an array and E2F from the basic
   block loading default values.  BBF is the last switch basic block (see the
   bbf description in the comment below).  */

static void
fix_phi_nodes (edge e1f, edge e2f, basic_block bbf)
{
  gimple_stmt_iterator gsi;
  int i;

  for (gsi = gsi_start_phis (bbf), i = 0;
       !gsi_end_p (gsi); gsi_next (&gsi), i++)
    {
      gimple phi = gsi_stmt (gsi);
      add_phi_arg (phi, info.target_inbound_names[i], e1f, UNKNOWN_LOCATION);
      add_phi_arg (phi, info.target_outbound_names[i], e2f, UNKNOWN_LOCATION);
    }

}

/* Creates a check whether the switch expression value actually falls into the
   range given by all the cases.  If it does not, the temporaries are loaded
   with default values instead.  SWTCH is the switch statement being converted.

   bb0 is the bb with the switch statement, however, we'll end it with a
       condition instead.

   bb1 is the bb to be used when the range check went ok.  It is derived from
       the switch BB

   bb2 is the bb taken when the expression evaluated outside of the range
       covered by the created arrays.  It is populated by loads of default
       values.

   bbF is a fall through for both bb1 and bb2 and contains exactly what
       originally followed the switch statement.

   bbD contains the switch statement (in the end).  It is unreachable but we
       still need to strip off its edges.
*/

static void
gen_inbound_check (gimple swtch)
{
  tree label_decl1 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl2 = create_artificial_label (UNKNOWN_LOCATION);
  tree label_decl3 = create_artificial_label (UNKNOWN_LOCATION);
  gimple label1, label2, label3;

  tree utype;
  tree tmp_u_1, tmp_u_2, tmp_u_var;
  tree cast;
  gimple cast_assign, minus_assign;
  tree ulb, minus;
  tree bound;

  gimple cond_stmt;

  gimple last_assign;
  gimple_stmt_iterator gsi;
  basic_block bb0, bb1, bb2, bbf, bbd;
  edge e01, e02, e21, e1d, e1f, e2f;
  location_t loc = gimple_location (swtch);

  gcc_assert (info.default_values);
  bb0 = gimple_bb (swtch);

  /* Make sure we do not generate arithmetics in a subrange.  */
  if (TREE_TYPE (TREE_TYPE (info.index_expr)))
    utype = unsigned_type_for (TREE_TYPE (TREE_TYPE (info.index_expr)));
  else
    utype = unsigned_type_for (TREE_TYPE (info.index_expr));

  /* (end of) block 0 */
  gsi = gsi_for_stmt (info.arr_ref_first);
  tmp_u_var = create_tmp_var (utype, "csui");
  add_referenced_var (tmp_u_var);
  tmp_u_1 = make_ssa_name (tmp_u_var, NULL);

  cast = fold_convert_loc (loc, utype, info.index_expr);
  cast_assign = gimple_build_assign (tmp_u_1, cast);
  SSA_NAME_DEF_STMT (tmp_u_1) = cast_assign;
  gsi_insert_before (&gsi, cast_assign, GSI_SAME_STMT);
  update_stmt (cast_assign);

  ulb = fold_convert_loc (loc, utype, info.range_min);
  minus = fold_build2_loc (loc, MINUS_EXPR, utype, tmp_u_1, ulb);
  minus = force_gimple_operand_gsi (&gsi, minus, false, NULL, true,
				    GSI_SAME_STMT);
  tmp_u_2 = make_ssa_name (tmp_u_var, NULL);
  minus_assign = gimple_build_assign (tmp_u_2, minus);
  SSA_NAME_DEF_STMT (tmp_u_2) = minus_assign;
  gsi_insert_before (&gsi, minus_assign, GSI_SAME_STMT);
  update_stmt (minus_assign);

  bound = fold_convert_loc (loc, utype, info.range_size);
  cond_stmt = gimple_build_cond (LE_EXPR, tmp_u_2, bound, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);
  update_stmt (cond_stmt);

  /* block 2 */
  gsi = gsi_for_stmt (info.arr_ref_first);
  label2 = gimple_build_label (label_decl2);
  gsi_insert_before (&gsi, label2, GSI_SAME_STMT);
  last_assign = gen_def_assigns (&gsi);

  /* block 1 */
  gsi = gsi_for_stmt (info.arr_ref_first);
  label1 = gimple_build_label (label_decl1);
  gsi_insert_before (&gsi, label1, GSI_SAME_STMT);

  /* block F */
  gsi = gsi_start_bb (info.final_bb);
  label3 = gimple_build_label (label_decl3);
  gsi_insert_before (&gsi, label3, GSI_SAME_STMT);

  /* cfg fix */
  e02 = split_block (bb0, cond_stmt);
  bb2 = e02->dest;

  e21 = split_block (bb2, last_assign);
  bb1 = e21->dest;
  remove_edge (e21);

  e1d = split_block (bb1, info.arr_ref_last);
  bbd = e1d->dest;
  remove_edge (e1d);

  /* flags and profiles of the edge for in-range values */
  e01 = make_edge (bb0, bb1, EDGE_TRUE_VALUE);
  e01->probability = REG_BR_PROB_BASE - info.default_prob;
  e01->count = info.other_count;

  /* flags and profiles of the edge taking care of out-of-range values */
  e02->flags &= ~EDGE_FALLTHRU;
  e02->flags |= EDGE_FALSE_VALUE;
  e02->probability = info.default_prob;
  e02->count = info.default_count;

  bbf = info.final_bb;

  e1f = make_edge (bb1, bbf, EDGE_FALLTHRU);
  e1f->probability = REG_BR_PROB_BASE;
  e1f->count = info.other_count;

  e2f = make_edge (bb2, bbf, EDGE_FALLTHRU);
  e2f->probability = REG_BR_PROB_BASE;
  e2f->count = info.default_count;

  /* frequencies of the new BBs */
  bb1->frequency = EDGE_FREQUENCY (e01);
  bb2->frequency = EDGE_FREQUENCY (e02);
  bbf->frequency = EDGE_FREQUENCY (e1f) + EDGE_FREQUENCY (e2f);

  prune_bbs (bbd, info.final_bb); /* To keep calc_dfs_tree() in dominance.c
				     happy.  */

  fix_phi_nodes (e1f, e2f, bbf);

  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
}

/* The following function is invoked on every switch statement (the current one
   is given in SWTCH) and runs the individual phases of switch conversion on it
   one after another until one fails or the conversion is completed.  */

static bool
process_switch (gimple swtch)
{
  unsigned int i, branch_num = gimple_switch_num_labels (swtch);
  tree index_type;

  /* Operand 2 is either NULL_TREE or a vector of cases (stmt.c).  */
  if (branch_num < 2)
    {
      info.reason = "switch has no labels\n";
      return false;
    }

  info.final_bb = NULL;
  info.switch_bb = gimple_bb (swtch);
  info.index_expr = gimple_switch_index (swtch);
  index_type = TREE_TYPE (info.index_expr);
  info.arr_ref_first = NULL;
  info.arr_ref_last = NULL;
  info.default_prob = 0;
  info.default_count = 0;
  info.other_count = 0;

  /* An ERROR_MARK occurs for various reasons including invalid data type.
     (comment from stmt.c) */
  if (index_type == error_mark_node)
    {
      info.reason = "index error.\n";
      return false;
    }

  /* Check the case label values are within reasonable range:  */
  if (!check_range (swtch))
    return false;

  /* For all the cases, see whether they are empty, the assignments they
     represent constant and so on...  */
  for (i = 0; i < branch_num; i++)
    if (!check_process_case (gimple_switch_label (swtch, i)))
      {
	if (dump_file)
	  fprintf (dump_file, "Processing of case %i failed\n", i);
	return false;
      }

  if (!check_final_bb ())
    return false;

  /* At this point all checks have passed and we can proceed with the
     transformation.  */

  create_temp_arrays ();
  gather_default_values (gimple_switch_label (swtch, 0));
  build_constructors (swtch);

  build_arrays (swtch); /* Build the static arrays and assignments.   */
  gen_inbound_check (swtch);	/* Build the bounds check.  */

  /* Cleanup:  */
  free_temp_arrays ();
  return true;
}

/* The main function of the pass scans statements for switches and invokes
   process_switch on them.  */

static unsigned int
do_switchconv (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
  {
    gimple stmt = last_stmt (bb);
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

	info.reason = NULL;
	if (process_switch (stmt))
	  {
	    if (dump_file)
	      {
		fputs ("Switch converted\n", dump_file);
		fputs ("--------------------------------\n", dump_file);
	      }
	  }
	else
	  {
	    if (dump_file)
	      {
		gcc_assert (info.reason);
		fputs ("Bailing out - ", dump_file);
		fputs (info.reason, dump_file);
		fputs ("--------------------------------\n", dump_file);
	      }
	  }
      }
  }

  return 0;
}

/* The pass gate. */

static bool
switchconv_gate (void)
{
  return flag_tree_switch_conversion != 0;
}

struct gimple_opt_pass pass_convert_switch =
{
 {
  GIMPLE_PASS,
  "switchconv",				/* name */
  switchconv_gate,			/* gate */
  do_switchconv,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SWITCH_CONVERSION,		/* tv_id */
  PROP_cfg | PROP_ssa,	                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa | TODO_dump_func
  | TODO_ggc_collect | TODO_verify_ssa  /* todo_flags_finish */
 }
};
