/* Backward propagation of indirect loads through PHIs.
   Copyright (C) 2007-2013 Free Software Foundation, Inc.
   Contributed by Richard Guenther <rguenther@suse.de>

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
#include "tm_p.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "flags.h"

/* This pass propagates indirect loads through the PHI node for its
   address to make the load source possibly non-addressable and to
   allow for PHI optimization to trigger.

   For example the pass changes

     # addr_1 = PHI <&a, &b>
     tmp_1 = *addr_1;

   to

     # tmp_1 = PHI <a, b>

   but also handles more complex scenarios like

     D.2077_2 = &this_1(D)->a1;
     ...

     # b_12 = PHI <&c(2), D.2077_2(3)>
     D.2114_13 = *b_12;
     ...

     # b_15 = PHI <b_12(4), &b(5)>
     D.2080_5 = &this_1(D)->a0;
     ...

     # b_18 = PHI <D.2080_5(6), &c(7)>
     ...

     # b_21 = PHI <b_15(8), b_18(9)>
     D.2076_8 = *b_21;

   where the addresses loaded are defined by PHIs itself.
   The above happens for

     std::max(std::min(a0, c), std::min(std::max(a1, c), b))

   where this pass transforms it to a form later PHI optimization
   recognizes and transforms it to the simple

     D.2109_10 = this_1(D)->a1;
     D.2110_11 = c;
     D.2114_31 = MAX_EXPR <D.2109_10, D.2110_11>;
     D.2115_14 = b;
     D.2125_17 = MIN_EXPR <D.2115_14, D.2114_31>;
     D.2119_16 = this_1(D)->a0;
     D.2124_32 = MIN_EXPR <D.2110_11, D.2119_16>;
     D.2076_33 = MAX_EXPR <D.2125_17, D.2124_32>;

   The pass does a dominator walk processing loads using a basic-block
   local analysis and stores the result for use by transformations on
   dominated basic-blocks.  */


/* Structure to keep track of the value of a dereferenced PHI result
   and the virtual operand used for that dereference.  */

struct phiprop_d
{
  tree value;
  tree vuse;
};

/* Verify if the value recorded for NAME in PHIVN is still valid at
   the start of basic block BB.  */

static bool
phivn_valid_p (struct phiprop_d *phivn, tree name, basic_block bb)
{
  tree vuse = phivn[SSA_NAME_VERSION (name)].vuse;
  gimple use_stmt;
  imm_use_iterator ui2;
  bool ok = true;

  /* The def stmts of the virtual uses need to be dominated by bb.  */
  gcc_assert (vuse != NULL_TREE);

  FOR_EACH_IMM_USE_STMT (use_stmt, ui2, vuse)
    {
      /* If BB does not dominate a VDEF, the value is invalid.  */
      if ((gimple_vdef (use_stmt) != NULL_TREE
	   || gimple_code (use_stmt) == GIMPLE_PHI)
	  && !dominated_by_p (CDI_DOMINATORS, gimple_bb (use_stmt), bb))
	{
	  ok = false;
	  BREAK_FROM_IMM_USE_STMT (ui2);
	}
    }

  return ok;
}

/* Insert a new phi node for the dereference of PHI at basic_block
   BB with the virtual operands from USE_STMT.  */

static tree
phiprop_insert_phi (basic_block bb, gimple phi, gimple use_stmt,
		    struct phiprop_d *phivn, size_t n)
{
  tree res;
  gimple new_phi;
  edge_iterator ei;
  edge e;

  gcc_assert (is_gimple_assign (use_stmt)
	      && gimple_assign_rhs_code (use_stmt) == MEM_REF);

  /* Build a new PHI node to replace the definition of
     the indirect reference lhs.  */
  res = gimple_assign_lhs (use_stmt);
  new_phi = create_phi_node (res, bb);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Inserting PHI for result of load ");
      print_gimple_stmt (dump_file, use_stmt, 0, 0);
    }

  /* Add PHI arguments for each edge inserting loads of the
     addressable operands.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      tree old_arg, new_var;
      gimple tmp;
      source_location locus;

      old_arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
      locus = gimple_phi_arg_location_from_edge (phi, e);
      while (TREE_CODE (old_arg) == SSA_NAME
	     && (SSA_NAME_VERSION (old_arg) >= n
	         || phivn[SSA_NAME_VERSION (old_arg)].value == NULL_TREE))
	{
	  gimple def_stmt = SSA_NAME_DEF_STMT (old_arg);
	  old_arg = gimple_assign_rhs1 (def_stmt);
	  locus = gimple_location (def_stmt);
	}

      if (TREE_CODE (old_arg) == SSA_NAME)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  for edge defining ");
	      print_generic_expr (dump_file, PHI_ARG_DEF_FROM_EDGE (phi, e), 0);
	      fprintf (dump_file, " reusing PHI result ");
	      print_generic_expr (dump_file,
				  phivn[SSA_NAME_VERSION (old_arg)].value, 0);
	      fprintf (dump_file, "\n");
	    }
	  /* Reuse a formerly created dereference.  */
	  new_var = phivn[SSA_NAME_VERSION (old_arg)].value;
	}
      else
	{
	  tree rhs = gimple_assign_rhs1 (use_stmt);
	  gcc_assert (TREE_CODE (old_arg) == ADDR_EXPR);
	  new_var = make_ssa_name (TREE_TYPE (rhs), NULL);
	  if (!is_gimple_min_invariant (old_arg))
	    old_arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  else
	    old_arg = unshare_expr (old_arg);
	  tmp = gimple_build_assign (new_var,
				     fold_build2 (MEM_REF, TREE_TYPE (rhs),
						  old_arg,
						  TREE_OPERAND (rhs, 1)));
	  gimple_set_location (tmp, locus);

	  gsi_insert_on_edge (e, tmp);
	  update_stmt (tmp);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  for edge defining ");
	      print_generic_expr (dump_file, PHI_ARG_DEF_FROM_EDGE (phi, e), 0);
	      fprintf (dump_file, " inserting load ");
	      print_gimple_stmt (dump_file, tmp, 0, 0);
	    }
	}

      add_phi_arg (new_phi, new_var, e, locus);
    }

  update_stmt (new_phi);

  if (dump_file && (dump_flags & TDF_DETAILS))
    print_gimple_stmt (dump_file, new_phi, 0, 0);

  return res;
}

/* Propagate between the phi node arguments of PHI in BB and phi result
   users.  For now this matches
        # p_2 = PHI <&x, &y>
      <Lx>:;
	p_3 = p_2;
	z_2 = *p_3;
   and converts it to
	# z_2 = PHI <x, y>
      <Lx>:;
   Returns true if a transformation was done and edge insertions
   need to be committed.  Global data PHIVN and N is used to track
   past transformation results.  We need to be especially careful here
   with aliasing issues as we are moving memory reads.  */

static bool
propagate_with_phi (basic_block bb, gimple phi, struct phiprop_d *phivn,
		    size_t n)
{
  tree ptr = PHI_RESULT (phi);
  gimple use_stmt;
  tree res = NULL_TREE;
  gimple_stmt_iterator gsi;
  imm_use_iterator ui;
  use_operand_p arg_p, use;
  ssa_op_iter i;
  bool phi_inserted;
  tree type = NULL_TREE;

  if (!POINTER_TYPE_P (TREE_TYPE (ptr))
      || !is_gimple_reg_type (TREE_TYPE (TREE_TYPE (ptr))))
    return false;

  /* Check if we can "cheaply" dereference all phi arguments.  */
  FOR_EACH_PHI_ARG (arg_p, phi, i, SSA_OP_USE)
    {
      tree arg = USE_FROM_PTR (arg_p);
      /* Walk the ssa chain until we reach a ssa name we already
	 created a value for or we reach a definition of the form
	 ssa_name_n = &var;  */
      while (TREE_CODE (arg) == SSA_NAME
	     && !SSA_NAME_IS_DEFAULT_DEF (arg)
	     && (SSA_NAME_VERSION (arg) >= n
	         || phivn[SSA_NAME_VERSION (arg)].value == NULL_TREE))
	{
	  gimple def_stmt = SSA_NAME_DEF_STMT (arg);
	  if (!gimple_assign_single_p (def_stmt))
	    return false;
	  arg = gimple_assign_rhs1 (def_stmt);
	}
      if (TREE_CODE (arg) != ADDR_EXPR
	  && !(TREE_CODE (arg) == SSA_NAME
	       && SSA_NAME_VERSION (arg) < n
	       && phivn[SSA_NAME_VERSION (arg)].value != NULL_TREE
	       && (!type
		   || types_compatible_p
		       (type, TREE_TYPE (phivn[SSA_NAME_VERSION (arg)].value)))
	       && phivn_valid_p (phivn, arg, bb)))
	return false;
      if (!type
	  && TREE_CODE (arg) == SSA_NAME)
	type = TREE_TYPE (phivn[SSA_NAME_VERSION (arg)].value);
    }

  /* Find a dereferencing use.  First follow (single use) ssa
     copy chains for ptr.  */
  while (single_imm_use (ptr, &use, &use_stmt)
	 && gimple_assign_ssa_name_copy_p (use_stmt))
    ptr = gimple_assign_lhs (use_stmt);

  /* Replace the first dereference of *ptr if there is one and if we
     can move the loads to the place of the ptr phi node.  */
  phi_inserted = false;
  FOR_EACH_IMM_USE_STMT (use_stmt, ui, ptr)
    {
      gimple def_stmt;
      tree vuse;

      /* Check whether this is a load of *ptr.  */
      if (!(is_gimple_assign (use_stmt)
	    && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
	    && gimple_assign_rhs_code (use_stmt) == MEM_REF
	    && TREE_OPERAND (gimple_assign_rhs1 (use_stmt), 0) == ptr
	    && integer_zerop (TREE_OPERAND (gimple_assign_rhs1 (use_stmt), 1))
	    && (!type
		|| types_compatible_p
		     (TREE_TYPE (gimple_assign_lhs (use_stmt)), type))
	    /* We cannot replace a load that may throw or is volatile.  */
	    && !stmt_can_throw_internal (use_stmt)))
	continue;

      /* Check if we can move the loads.  The def stmt of the virtual use
	 needs to be in a different basic block dominating bb.  */
      vuse = gimple_vuse (use_stmt);
      def_stmt = SSA_NAME_DEF_STMT (vuse);
      if (!SSA_NAME_IS_DEFAULT_DEF (vuse)
	  && (gimple_bb (def_stmt) == bb
	      || !dominated_by_p (CDI_DOMINATORS,
				  bb, gimple_bb (def_stmt))))
	goto next;

      /* Found a proper dereference.  Insert a phi node if this
	 is the first load transformation.  */
      if (!phi_inserted)
	{
	  res = phiprop_insert_phi (bb, phi, use_stmt, phivn, n);
	  type = TREE_TYPE (res);

	  /* Remember the value we created for *ptr.  */
	  phivn[SSA_NAME_VERSION (ptr)].value = res;
	  phivn[SSA_NAME_VERSION (ptr)].vuse = vuse;

	  /* Remove old stmt.  The phi is taken care of by DCE, if we
	     want to delete it here we also have to delete all intermediate
	     copies.  */
	  gsi = gsi_for_stmt (use_stmt);
	  gsi_remove (&gsi, true);

	  phi_inserted = true;
	}
      else
	{
	  /* Further replacements are easy, just make a copy out of the
	     load.  */
	  gimple_assign_set_rhs1 (use_stmt, res);
	  update_stmt (use_stmt);
	}

next:;
      /* Continue searching for a proper dereference.  */
    }

  return phi_inserted;
}

/* Main entry for phiprop pass.  */

static unsigned int
tree_ssa_phiprop (void)
{
  vec<basic_block> bbs;
  struct phiprop_d *phivn;
  bool did_something = false;
  basic_block bb;
  gimple_stmt_iterator gsi;
  unsigned i;
  size_t n;

  calculate_dominance_info (CDI_DOMINATORS);

  n = num_ssa_names;
  phivn = XCNEWVEC (struct phiprop_d, n);

  /* Walk the dominator tree in preorder.  */
  bbs = get_all_dominated_blocks (CDI_DOMINATORS,
				  single_succ (ENTRY_BLOCK_PTR));
  FOR_EACH_VEC_ELT (bbs, i, bb)
    for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      did_something |= propagate_with_phi (bb, gsi_stmt (gsi), phivn, n);

  if (did_something)
    gsi_commit_edge_inserts ();

  bbs.release ();
  free (phivn);

  return 0;
}

static bool
gate_phiprop (void)
{
  return flag_tree_phiprop;
}

namespace {

const pass_data pass_data_phiprop =
{
  GIMPLE_PASS, /* type */
  "phiprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_PHIPROP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_update_ssa | TODO_verify_ssa ), /* todo_flags_finish */
};

class pass_phiprop : public gimple_opt_pass
{
public:
  pass_phiprop(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_phiprop, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_phiprop (); }
  unsigned int execute () { return tree_ssa_phiprop (); }

}; // class pass_phiprop

} // anon namespace

gimple_opt_pass *
make_pass_phiprop (gcc::context *ctxt)
{
  return new pass_phiprop (ctxt);
}
