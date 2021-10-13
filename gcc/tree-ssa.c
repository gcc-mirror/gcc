/* Miscellaneous SSA utility functions.
   Copyright (C) 2001-2021 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-ssa-loop-manip.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "cfgexpand.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"

/* Pointer map of variable mappings, keyed by edge.  */
static hash_map<edge, auto_vec<edge_var_map> > *edge_var_maps;


/* Add a mapping with PHI RESULT and PHI DEF associated with edge E.  */

void
redirect_edge_var_map_add (edge e, tree result, tree def, location_t locus)
{
  edge_var_map new_node;

  if (edge_var_maps == NULL)
    edge_var_maps = new hash_map<edge, auto_vec<edge_var_map> >;

  auto_vec<edge_var_map> &slot = edge_var_maps->get_or_insert (e);
  new_node.def = def;
  new_node.result = result;
  new_node.locus = locus;

  slot.safe_push (new_node);
}


/* Clear the var mappings in edge E.  */

void
redirect_edge_var_map_clear (edge e)
{
  if (!edge_var_maps)
    return;

  auto_vec<edge_var_map> *head = edge_var_maps->get (e);

  if (head)
    head->release ();
}


/* Duplicate the redirected var mappings in OLDE in NEWE.

   This assumes a hash_map can have multiple edges mapping to the same
   var_map (many to one mapping), since we don't remove the previous mappings.
   */

void
redirect_edge_var_map_dup (edge newe, edge olde)
{
  if (!edge_var_maps)
    return;

  auto_vec<edge_var_map> *new_head = &edge_var_maps->get_or_insert (newe);
  auto_vec<edge_var_map> *old_head = edge_var_maps->get (olde);
  if (!old_head)
    return;

  new_head->safe_splice (*old_head);
}


/* Return the variable mappings for a given edge.  If there is none, return
   NULL.  */

vec<edge_var_map> *
redirect_edge_var_map_vector (edge e)
{
  /* Hey, what kind of idiot would... you'd be surprised.  */
  if (!edge_var_maps)
    return NULL;

  auto_vec<edge_var_map> *slot = edge_var_maps->get (e);
  if (!slot)
    return NULL;

  return slot;
}

/* Clear the edge variable mappings.  */

void
redirect_edge_var_map_empty (void)
{
  if (edge_var_maps)
    edge_var_maps->empty ();
}


/* Remove the corresponding arguments from the PHI nodes in E's
   destination block and redirect it to DEST.  Return redirected edge.
   The list of removed arguments is stored in a vector accessed
   through edge_var_maps.  */

edge
ssa_redirect_edge (edge e, basic_block dest)
{
  gphi_iterator gsi;
  gphi *phi;

  redirect_edge_var_map_clear (e);

  /* Remove the appropriate PHI arguments in E's destination block.
     If we are redirecting a copied edge the destination has not
     got PHI argument space reserved nor an interesting argument.  */
  if (! (e->dest->flags & BB_DUPLICATED))
    for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	tree def;
	location_t locus;

	phi = gsi.phi ();
	def = gimple_phi_arg_def (phi, e->dest_idx);
	locus = gimple_phi_arg_location (phi, e->dest_idx);

	if (def == NULL_TREE)
	  continue;

	redirect_edge_var_map_add (e, gimple_phi_result (phi), def, locus);
      }

  e = redirect_edge_succ_nodup (e, dest);

  return e;
}


/* Add PHI arguments queued in PENDING_STMT list on edge E to edge
   E->dest.  */

void
flush_pending_stmts (edge e)
{
  gphi *phi;
  edge_var_map *vm;
  int i;
  gphi_iterator gsi;

  vec<edge_var_map> *v = redirect_edge_var_map_vector (e);
  if (!v)
    return;

  for (gsi = gsi_start_phis (e->dest), i = 0;
       !gsi_end_p (gsi) && v->iterate (i, &vm);
       gsi_next (&gsi), i++)
    {
      tree def;

      phi = gsi.phi ();
      def = redirect_edge_var_map_def (vm);
      add_phi_arg (phi, def, e, redirect_edge_var_map_location (vm));
    }

  redirect_edge_var_map_clear (e);
}

/* Replace the LHS of STMT, an assignment, either a GIMPLE_ASSIGN or a
   GIMPLE_CALL, with NLHS, in preparation for modifying the RHS to an
   expression with a different value.

   This will update any annotations (say debug bind stmts) referring
   to the original LHS, so that they use the RHS instead.  This is
   done even if NLHS and LHS are the same, for it is understood that
   the RHS will be modified afterwards, and NLHS will not be assigned
   an equivalent value.

   Adjusting any non-annotation uses of the LHS, if needed, is a
   responsibility of the caller.

   The effect of this call should be pretty much the same as that of
   inserting a copy of STMT before STMT, and then removing the
   original stmt, at which time gsi_remove() would have update
   annotations, but using this function saves all the inserting,
   copying and removing.  */

void
gimple_replace_ssa_lhs (gimple *stmt, tree nlhs)
{
  if (MAY_HAVE_DEBUG_BIND_STMTS)
    {
      tree lhs = gimple_get_lhs (stmt);

      gcc_assert (SSA_NAME_DEF_STMT (lhs) == stmt);

      insert_debug_temp_for_var_def (NULL, lhs);
    }

  gimple_set_lhs (stmt, nlhs);
}


/* Given a tree for an expression for which we might want to emit
   locations or values in debug information (generally a variable, but
   we might deal with other kinds of trees in the future), return the
   tree that should be used as the variable of a DEBUG_BIND STMT or
   VAR_LOCATION INSN or NOTE.  Return NULL if VAR is not to be tracked.  */

tree
target_for_debug_bind (tree var)
{
  if (!MAY_HAVE_DEBUG_BIND_STMTS)
    return NULL_TREE;

  if (TREE_CODE (var) == SSA_NAME)
    {
      var = SSA_NAME_VAR (var);
      if (var == NULL_TREE)
	return NULL_TREE;
    }

  if ((!VAR_P (var) || VAR_DECL_IS_VIRTUAL_OPERAND (var))
      && TREE_CODE (var) != PARM_DECL)
    return NULL_TREE;

  if (DECL_HAS_VALUE_EXPR_P (var))
    return target_for_debug_bind (DECL_VALUE_EXPR (var));

  if (DECL_IGNORED_P (var))
    return NULL_TREE;

  /* var-tracking only tracks registers.  */
  if (!is_gimple_reg_type (TREE_TYPE (var)))
    return NULL_TREE;

  return var;
}

/* Called via walk_tree, look for SSA_NAMEs that have already been
   released.  */

static tree
find_released_ssa_name (tree *tp, int *walk_subtrees, void *data_)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data_;

  if (wi && wi->is_lhs)
    return NULL_TREE;

  if (TREE_CODE (*tp) == SSA_NAME)
    {
      if (SSA_NAME_IN_FREE_LIST (*tp))
	return *tp;

      *walk_subtrees = 0;
    }
  else if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Insert a DEBUG BIND stmt before the DEF of VAR if VAR is referenced
   by other DEBUG stmts, and replace uses of the DEF with the
   newly-created debug temp.  */

void
insert_debug_temp_for_var_def (gimple_stmt_iterator *gsi, tree var)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  gimple *stmt;
  gimple *def_stmt = NULL;
  int usecount = 0;
  tree value = NULL;

  if (!MAY_HAVE_DEBUG_BIND_STMTS)
    return;

  /* If this name has already been registered for replacement, do nothing
     as anything that uses this name isn't in SSA form.  */
  if (name_registered_for_update_p (var))
    return;

  /* Check whether there are debug stmts that reference this variable and,
     if there are, decide whether we should use a debug temp.  */
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, var)
    {
      stmt = USE_STMT (use_p);

      if (!gimple_debug_bind_p (stmt))
	continue;

      if (usecount++)
	break;

      if (gimple_debug_bind_get_value (stmt) != var)
	{
	  /* Count this as an additional use, so as to make sure we
	     use a temp unless VAR's definition has a SINGLE_RHS that
	     can be shared.  */
	  usecount++;
	  break;
	}
    }

  if (!usecount)
    return;

  if (gsi)
    def_stmt = gsi_stmt (*gsi);
  else
    def_stmt = SSA_NAME_DEF_STMT (var);

  /* If we didn't get an insertion point, and the stmt has already
     been removed, we won't be able to insert the debug bind stmt, so
     we'll have to drop debug information.  */
  if (gimple_code (def_stmt) == GIMPLE_PHI)
    {
      value = degenerate_phi_result (as_a <gphi *> (def_stmt));
      if (value && walk_tree (&value, find_released_ssa_name, NULL, NULL))
	value = NULL;
      /* error_mark_node is what fixup_noreturn_call changes PHI arguments
	 to.  */
      else if (value == error_mark_node)
	value = NULL;
    }
  else if (gimple_clobber_p (def_stmt))
    /* We can end up here when rewriting a decl into SSA and coming
       along a clobber for the original decl.  Turn that into
       # DEBUG decl => NULL  */
    value = NULL;
  else if (is_gimple_assign (def_stmt))
    {
      bool no_value = false;

      if (!dom_info_available_p (CDI_DOMINATORS))
	{
	  struct walk_stmt_info wi;

	  memset (&wi, 0, sizeof (wi));

	  /* When removing blocks without following reverse dominance
	     order, we may sometimes encounter SSA_NAMEs that have
	     already been released, referenced in other SSA_DEFs that
	     we're about to release.  Consider:

	     <bb X>:
	     v_1 = foo;

	     <bb Y>:
	     w_2 = v_1 + bar;
	     # DEBUG w => w_2

	     If we deleted BB X first, propagating the value of w_2
	     won't do us any good.  It's too late to recover their
	     original definition of v_1: when it was deleted, it was
	     only referenced in other DEFs, it couldn't possibly know
	     it should have been retained, and propagating every
	     single DEF just in case it might have to be propagated
	     into a DEBUG STMT would probably be too wasteful.

	     When dominator information is not readily available, we
	     check for and accept some loss of debug information.  But
	     if it is available, there's no excuse for us to remove
	     blocks in the wrong order, so we don't even check for
	     dead SSA NAMEs.  SSA verification shall catch any
	     errors.  */
	  if ((!gsi && !gimple_bb (def_stmt))
	      || walk_gimple_op (def_stmt, find_released_ssa_name, &wi))
	    no_value = true;
	}

      if (!no_value)
	value = gimple_assign_rhs_to_tree (def_stmt);
    }

  if (value)
    {
      /* If there's a single use of VAR, and VAR is the entire debug
	 expression (usecount would have been incremented again
	 otherwise), and the definition involves only constants and
	 SSA names, then we can propagate VALUE into this single use,
	 avoiding the temp.

	 We can also avoid using a temp if VALUE can be shared and
	 propagated into all uses, without generating expressions that
	 wouldn't be valid gimple RHSs.

	 Other cases that would require unsharing or non-gimple RHSs
	 are deferred to a debug temp, although we could avoid temps
	 at the expense of duplication of expressions.  */

      if (CONSTANT_CLASS_P (value)
	  || gimple_code (def_stmt) == GIMPLE_PHI
	  || (usecount == 1
	      && (!gimple_assign_single_p (def_stmt)
		  || is_gimple_min_invariant (value)))
	  || is_gimple_reg (value))
	;
      else
	{
	  gdebug *def_temp;
	  tree vexpr = make_node (DEBUG_EXPR_DECL);

	  def_temp = gimple_build_debug_bind (vexpr,
					      unshare_expr (value),
					      def_stmt);

	  DECL_ARTIFICIAL (vexpr) = 1;
	  TREE_TYPE (vexpr) = TREE_TYPE (value);
	  if (DECL_P (value))
	    SET_DECL_MODE (vexpr, DECL_MODE (value));
	  else
	    SET_DECL_MODE (vexpr, TYPE_MODE (TREE_TYPE (value)));

	  if (gsi)
	    gsi_insert_before (gsi, def_temp, GSI_SAME_STMT);
	  else
	    {
	      gimple_stmt_iterator ngsi = gsi_for_stmt (def_stmt);
	      gsi_insert_before (&ngsi, def_temp, GSI_SAME_STMT);
	    }

	  value = vexpr;
	}
    }

  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, var)
    {
      if (!gimple_debug_bind_p (stmt))
	continue;

      if (value)
	{
	  FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	    /* unshare_expr is not needed here.  vexpr is either a
	       SINGLE_RHS, that can be safely shared, some other RHS
	       that was unshared when we found it had a single debug
	       use, or a DEBUG_EXPR_DECL, that can be safely
	       shared.  */
	    SET_USE (use_p, unshare_expr (value));
	  /* If we didn't replace uses with a debug decl fold the
	     resulting expression.  Otherwise we end up with invalid IL.  */
	  if (TREE_CODE (value) != DEBUG_EXPR_DECL)
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	      fold_stmt_inplace (&gsi);
	    }
	}
      else
	gimple_debug_bind_reset_value (stmt);

      update_stmt (stmt);
    }
}


/* Insert a DEBUG BIND stmt before STMT for each DEF referenced by
   other DEBUG stmts, and replace uses of the DEF with the
   newly-created debug temp.  */

void
insert_debug_temps_for_defs (gimple_stmt_iterator *gsi)
{
  gimple *stmt;
  ssa_op_iter op_iter;
  def_operand_p def_p;

  if (!MAY_HAVE_DEBUG_BIND_STMTS)
    return;

  stmt = gsi_stmt (*gsi);

  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, op_iter, SSA_OP_DEF)
    {
      tree var = DEF_FROM_PTR (def_p);

      if (TREE_CODE (var) != SSA_NAME)
	continue;

      insert_debug_temp_for_var_def (gsi, var);
    }
}

/* Reset all debug stmts that use SSA_NAME(s) defined in STMT.  */

void
reset_debug_uses (gimple *stmt)
{
  ssa_op_iter op_iter;
  def_operand_p def_p;
  imm_use_iterator imm_iter;
  gimple *use_stmt;

  if (!MAY_HAVE_DEBUG_BIND_STMTS)
    return;

  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, op_iter, SSA_OP_DEF)
    {
      tree var = DEF_FROM_PTR (def_p);

      if (TREE_CODE (var) != SSA_NAME)
	continue;

      FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, var)
	{
	  if (!gimple_debug_bind_p (use_stmt))
	    continue;

	  gimple_debug_bind_reset_value (use_stmt);
	  update_stmt (use_stmt);
	}
    }
}

/* Delete SSA DEFs for SSA versions in the TOREMOVE bitmap, removing
   dominated stmts before their dominators, so that release_ssa_defs
   stands a chance of propagating DEFs into debug bind stmts.  */

void
release_defs_bitset (bitmap toremove)
{
  unsigned j;
  bitmap_iterator bi;

  /* Performing a topological sort is probably overkill, this will
     most likely run in slightly superlinear time, rather than the
     pathological quadratic worst case.
     But iterate from max SSA name version to min one because
     that mimics allocation order during code generation behavior best.
     Use an array for this which we compact on-the-fly with a NULL
     marker moving towards the end of the vector.  */
  auto_vec<tree, 16> names;
  names.reserve (bitmap_count_bits (toremove) + 1);
  names.quick_push (NULL_TREE);
  EXECUTE_IF_SET_IN_BITMAP (toremove, 0, j, bi)
    names.quick_push (ssa_name (j));

  bitmap_tree_view (toremove);
  while (!bitmap_empty_p (toremove))
    {
      j = names.length () - 1;
      for (unsigned i = names.length () - 1; names[i];)
	{
	  bool remove_now = true;
	  tree var = names[i];
	  gimple *stmt;
	  imm_use_iterator uit;

	  FOR_EACH_IMM_USE_STMT (stmt, uit, var)
	    {
	      ssa_op_iter dit;
	      def_operand_p def_p;

	      /* We can't propagate PHI nodes into debug stmts.  */
	      if (gimple_code (stmt) == GIMPLE_PHI
		  || is_gimple_debug (stmt))
		continue;

	      /* If we find another definition to remove that uses
		 the one we're looking at, defer the removal of this
		 one, so that it can be propagated into debug stmts
		 after the other is.  */
	      FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, dit, SSA_OP_DEF)
		{
		  tree odef = DEF_FROM_PTR (def_p);

		  if (bitmap_bit_p (toremove, SSA_NAME_VERSION (odef)))
		    {
		      remove_now = false;
		      break;
		    }
		}

	      if (!remove_now)
		break;
	    }

	  if (remove_now)
	    {
	      gimple *def = SSA_NAME_DEF_STMT (var);
	      gimple_stmt_iterator gsi = gsi_for_stmt (def);

	      if (gimple_code (def) == GIMPLE_PHI)
		remove_phi_node (&gsi, true);
	      else
		{
		  gsi_remove (&gsi, true);
		  release_defs (def);
		}
	      bitmap_clear_bit (toremove, SSA_NAME_VERSION (var));
	    }
	  else
	    --i;
	  if (--j != i)
	    names[i] = names[j];
	}
    }
  bitmap_list_view (toremove);
}

/* Disable warnings about missing quoting in GCC diagnostics for
   the verification errors.  Their format strings don't follow GCC
   diagnostic conventions and the calls are ultimately followed by
   one to internal_error.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

/* Verify virtual SSA form.  */

bool
verify_vssa (basic_block bb, tree current_vdef, sbitmap visited)
{
  bool err = false;

  if (bitmap_bit_p (visited, bb->index))
    return false;

  bitmap_set_bit (visited, bb->index);

  /* Pick up the single virtual PHI def.  */
  gphi *phi = NULL;
  for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
       gsi_next (&si))
    {
      tree res = gimple_phi_result (si.phi ());
      if (virtual_operand_p (res))
	{
	  if (phi)
	    {
	      error ("multiple virtual PHI nodes in BB %d", bb->index);
	      print_gimple_stmt (stderr, phi, 0);
	      print_gimple_stmt (stderr, si.phi (), 0);
	      err = true;
	    }
	  else
	    phi = si.phi ();
	}
    }
  if (phi)
    {
      current_vdef = gimple_phi_result (phi);
      if (TREE_CODE (current_vdef) != SSA_NAME)
	{
	  error ("virtual definition is not an SSA name");
	  print_gimple_stmt (stderr, phi, 0);
	  err = true;
	}
    }

  /* Verify stmts.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      tree vuse = gimple_vuse (stmt);
      if (vuse)
	{
	  if (vuse != current_vdef)
	    {
	      error ("stmt with wrong VUSE");
	      print_gimple_stmt (stderr, stmt, 0, TDF_VOPS);
	      fprintf (stderr, "expected ");
	      print_generic_expr (stderr, current_vdef);
	      fprintf (stderr, "\n");
	      err = true;
	    }
	  tree vdef = gimple_vdef (stmt);
	  if (vdef)
	    {
	      current_vdef = vdef;
	      if (TREE_CODE (current_vdef) != SSA_NAME)
		{
		  error ("virtual definition is not an SSA name");
		  print_gimple_stmt (stderr, phi, 0);
		  err = true;
		}
	    }
	}
    }

  /* Verify destination PHI uses and recurse.  */
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      gphi *phi = get_virtual_phi (e->dest);
      if (phi
	  && PHI_ARG_DEF_FROM_EDGE (phi, e) != current_vdef)
	{
	  error ("PHI node with wrong VUSE on edge from BB %d",
		 e->src->index);
	  print_gimple_stmt (stderr, phi, 0, TDF_VOPS);
	  fprintf (stderr, "expected ");
	  print_generic_expr (stderr, current_vdef);
	  fprintf (stderr, "\n");
	  err = true;
	}

      /* Recurse.  */
      err |= verify_vssa (e->dest, current_vdef, visited);
    }

  return err;
}

/* Return true if SSA_NAME is malformed and mark it visited.

   IS_VIRTUAL is true if this SSA_NAME was found inside a virtual
      operand.  */

static bool
verify_ssa_name (tree ssa_name, bool is_virtual)
{
  if (TREE_CODE (ssa_name) != SSA_NAME)
    {
      error ("expected an SSA_NAME object");
      return true;
    }

  if (SSA_NAME_IN_FREE_LIST (ssa_name))
    {
      error ("found an SSA_NAME that had been released into the free pool");
      return true;
    }

  if (SSA_NAME_VAR (ssa_name) != NULL_TREE
      && TREE_TYPE (ssa_name) != TREE_TYPE (SSA_NAME_VAR (ssa_name)))
    {
      error ("type mismatch between an SSA_NAME and its symbol");
      return true;
    }

  if (is_virtual && !virtual_operand_p (ssa_name))
    {
      error ("found a virtual definition for a GIMPLE register");
      return true;
    }

  if (is_virtual && SSA_NAME_VAR (ssa_name) != gimple_vop (cfun))
    {
      error ("virtual SSA name for non-VOP decl");
      return true;
    }

  if (!is_virtual && virtual_operand_p (ssa_name))
    {
      error ("found a real definition for a non-register");
      return true;
    }

  if (SSA_NAME_IS_DEFAULT_DEF (ssa_name)
      && !gimple_nop_p (SSA_NAME_DEF_STMT (ssa_name)))
    {
      error ("found a default name with a non-empty defining statement");
      return true;
    }

  return false;
}


/* Return true if the definition of SSA_NAME at block BB is malformed.

   STMT is the statement where SSA_NAME is created.

   DEFINITION_BLOCK is an array of basic blocks indexed by SSA_NAME
      version numbers.  If DEFINITION_BLOCK[SSA_NAME_VERSION] is set,
      it means that the block in that array slot contains the
      definition of SSA_NAME.

   IS_VIRTUAL is true if SSA_NAME is created by a VDEF.  */

static bool
verify_def (basic_block bb, basic_block *definition_block, tree ssa_name,
	    gimple *stmt, bool is_virtual)
{
  if (verify_ssa_name (ssa_name, is_virtual))
    goto err;

  if (SSA_NAME_VAR (ssa_name)
      && TREE_CODE (SSA_NAME_VAR (ssa_name)) == RESULT_DECL
      && DECL_BY_REFERENCE (SSA_NAME_VAR (ssa_name)))
    {
      error ("RESULT_DECL should be read only when DECL_BY_REFERENCE is set");
      goto err;
    }

  if (definition_block[SSA_NAME_VERSION (ssa_name)])
    {
      error ("SSA_NAME created in two different blocks %i and %i",
	     definition_block[SSA_NAME_VERSION (ssa_name)]->index, bb->index);
      goto err;
    }

  definition_block[SSA_NAME_VERSION (ssa_name)] = bb;

  if (SSA_NAME_DEF_STMT (ssa_name) != stmt)
    {
      error ("SSA_NAME_DEF_STMT is wrong");
      fprintf (stderr, "Expected definition statement:\n");
      print_gimple_stmt (stderr, SSA_NAME_DEF_STMT (ssa_name), 4, TDF_VOPS);
      fprintf (stderr, "\nActual definition statement:\n");
      print_gimple_stmt (stderr, stmt, 4, TDF_VOPS);
      goto err;
    }

  return false;

err:
  fprintf (stderr, "while verifying SSA_NAME ");
  print_generic_expr (stderr, ssa_name);
  fprintf (stderr, " in statement\n");
  print_gimple_stmt (stderr, stmt, 4, TDF_VOPS);

  return true;
}


/* Return true if the use of SSA_NAME at statement STMT in block BB is
   malformed.

   DEF_BB is the block where SSA_NAME was found to be created.

   IDOM contains immediate dominator information for the flowgraph.

   CHECK_ABNORMAL is true if the caller wants to check whether this use
      is flowing through an abnormal edge (only used when checking PHI
      arguments).

   If NAMES_DEFINED_IN_BB is not NULL, it contains a bitmap of ssa names
     that are defined before STMT in basic block BB.  */

static bool
verify_use (basic_block bb, basic_block def_bb, use_operand_p use_p,
	    gimple *stmt, bool check_abnormal, bitmap names_defined_in_bb)
{
  bool err = false;
  tree ssa_name = USE_FROM_PTR (use_p);

  if (!TREE_VISITED (ssa_name))
    if (verify_imm_links (stderr, ssa_name))
      err = true;

  TREE_VISITED (ssa_name) = 1;

  if (gimple_nop_p (SSA_NAME_DEF_STMT (ssa_name))
      && SSA_NAME_IS_DEFAULT_DEF (ssa_name))
    ; /* Default definitions have empty statements.  Nothing to do.  */
  else if (!def_bb)
    {
      error ("missing definition");
      err = true;
    }
  else if (bb != def_bb
	   && !dominated_by_p (CDI_DOMINATORS, bb, def_bb))
    {
      error ("definition in block %i does not dominate use in block %i",
	     def_bb->index, bb->index);
      err = true;
    }
  else if (bb == def_bb
	   && names_defined_in_bb != NULL
	   && !bitmap_bit_p (names_defined_in_bb, SSA_NAME_VERSION (ssa_name)))
    {
      error ("definition in block %i follows the use", def_bb->index);
      err = true;
    }

  if (check_abnormal
      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name))
    {
      error ("SSA_NAME_OCCURS_IN_ABNORMAL_PHI should be set");
      err = true;
    }

  /* Make sure the use is in an appropriate list by checking the previous
     element to make sure it's the same.  */
  if (use_p->prev == NULL)
    {
      error ("no immediate_use list");
      err = true;
    }
  else
    {
      tree listvar;
      if (use_p->prev->use == NULL)
	listvar = use_p->prev->loc.ssa_name;
      else
	listvar = USE_FROM_PTR (use_p->prev);
      if (listvar != ssa_name)
        {
	  error ("wrong immediate use list");
	  err = true;
	}
    }

  if (err)
    {
      fprintf (stderr, "for SSA_NAME: ");
      print_generic_expr (stderr, ssa_name, TDF_VOPS);
      fprintf (stderr, " in statement:\n");
      print_gimple_stmt (stderr, stmt, 0, TDF_VOPS);
    }

  return err;
}


/* Return true if any of the arguments for PHI node PHI at block BB is
   malformed.

   DEFINITION_BLOCK is an array of basic blocks indexed by SSA_NAME
      version numbers.  If DEFINITION_BLOCK[SSA_NAME_VERSION] is set,
      it means that the block in that array slot contains the
      definition of SSA_NAME.  */

static bool
verify_phi_args (gphi *phi, basic_block bb, basic_block *definition_block)
{
  edge e;
  bool err = false;
  size_t i, phi_num_args = gimple_phi_num_args (phi);

  if (EDGE_COUNT (bb->preds) != phi_num_args)
    {
      error ("incoming edge count does not match number of PHI arguments");
      err = true;
      goto error;
    }

  for (i = 0; i < phi_num_args; i++)
    {
      use_operand_p op_p = gimple_phi_arg_imm_use_ptr (phi, i);
      tree op = USE_FROM_PTR (op_p);

      e = EDGE_PRED (bb, i);

      if (op == NULL_TREE)
	{
	  error ("PHI argument is missing for edge %d->%d",
	         e->src->index,
		 e->dest->index);
	  err = true;
	  goto error;
	}

      if (TREE_CODE (op) != SSA_NAME && !is_gimple_min_invariant (op))
	{
	  error ("PHI argument is not SSA_NAME, or invariant");
	  err = true;
	}

      if ((e->flags & EDGE_ABNORMAL) && TREE_CODE (op) != SSA_NAME)
	{
	  error ("PHI argument on abnormal edge is not SSA_NAME");
	  err = true;
	}

      if (TREE_CODE (op) == SSA_NAME)
	{
	  err = verify_ssa_name (op, virtual_operand_p (gimple_phi_result (phi)));
	  err |= verify_use (e->src, definition_block[SSA_NAME_VERSION (op)],
			     op_p, phi, e->flags & EDGE_ABNORMAL, NULL);
	}

      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  tree base = TREE_OPERAND (op, 0);
	  while (handled_component_p (base))
	    base = TREE_OPERAND (base, 0);
	  if ((VAR_P (base)
	       || TREE_CODE (base) == PARM_DECL
	       || TREE_CODE (base) == RESULT_DECL)
	      && !TREE_ADDRESSABLE (base))
	    {
	      error ("address taken, but ADDRESSABLE bit not set");
	      err = true;
	    }
	}

      if (e->dest != bb)
	{
	  error ("wrong edge %d->%d for PHI argument",
	         e->src->index, e->dest->index);
	  err = true;
	}

      if (err)
	{
	  fprintf (stderr, "PHI argument\n");
	  print_generic_stmt (stderr, op, TDF_VOPS);
	  goto error;
	}
    }

error:
  if (err)
    {
      fprintf (stderr, "for PHI node\n");
      print_gimple_stmt (stderr, phi, 0, TDF_VOPS|TDF_MEMSYMS);
    }


  return err;
}


/* Verify common invariants in the SSA web.
   TODO: verify the variable annotations.  */

DEBUG_FUNCTION void
verify_ssa (bool check_modified_stmt, bool check_ssa_operands)
{
  basic_block bb;
  basic_block *definition_block = XCNEWVEC (basic_block, num_ssa_names);
  ssa_op_iter iter;
  tree op;
  enum dom_state orig_dom_state = dom_info_state (CDI_DOMINATORS);
  auto_bitmap names_defined_in_bb;

  gcc_assert (!need_ssa_update_p (cfun));

  timevar_push (TV_TREE_SSA_VERIFY);

    {
      /* Keep track of SSA names present in the IL.  */
      size_t i;
      tree name;
      hash_map <void *, tree> ssa_info;

      FOR_EACH_SSA_NAME (i, name, cfun)
	{
	  gimple *stmt;
	  TREE_VISITED (name) = 0;

	  verify_ssa_name (name, virtual_operand_p (name));

	  stmt = SSA_NAME_DEF_STMT (name);
	  if (!gimple_nop_p (stmt))
	    {
	      basic_block bb = gimple_bb (stmt);
	      if (verify_def (bb, definition_block,
			      name, stmt, virtual_operand_p (name)))
		goto err;
	    }

	  void *info = NULL;
	  if (POINTER_TYPE_P (TREE_TYPE (name)))
	    info = SSA_NAME_PTR_INFO (name);
	  else if (INTEGRAL_TYPE_P (TREE_TYPE (name)))
	    info = SSA_NAME_RANGE_INFO (name);
	  if (info)
	    {
	      bool existed;
	      tree &val = ssa_info.get_or_insert (info, &existed);
	      if (existed)
		{
		  error ("shared SSA name info");
		  print_generic_expr (stderr, val);
		  fprintf (stderr, " and ");
		  print_generic_expr (stderr, name);
		  fprintf (stderr, "\n");
		  goto err;
		}
	      else
		val = name;
	    }
	}
    }

  calculate_dominance_info (CDI_DOMINATORS);

  /* Now verify all the uses and make sure they agree with the definitions
     found in the previous pass.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      edge e;
      edge_iterator ei;

      /* Make sure that all edges have a clear 'aux' field.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->aux)
	    {
	      error ("AUX pointer initialized for edge %d->%d", e->src->index,
		      e->dest->index);
	      goto err;
	    }
	}

      /* Verify the arguments for every PHI node in the block.  */
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  if (verify_phi_args (phi, bb, definition_block))
	    goto err;

	  bitmap_set_bit (names_defined_in_bb,
			  SSA_NAME_VERSION (gimple_phi_result (phi)));
	}

      /* Now verify all the uses and vuses in every statement of the block.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  use_operand_p use_p;

	  if (check_modified_stmt && gimple_modified_p (stmt))
	    {
	      error ("stmt (%p) marked modified after optimization pass: ",
		     (void *)stmt);
	      print_gimple_stmt (stderr, stmt, 0, TDF_VOPS);
	      goto err;
	    }

	  if (check_ssa_operands && verify_ssa_operands (cfun, stmt))
	    {
	      print_gimple_stmt (stderr, stmt, 0, TDF_VOPS);
	      goto err;
	    }

	  if (gimple_debug_bind_p (stmt)
	      && !gimple_debug_bind_has_value_p (stmt))
	    continue;

	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE|SSA_OP_VUSE)
	    {
	      op = USE_FROM_PTR (use_p);
	      if (verify_use (bb, definition_block[SSA_NAME_VERSION (op)],
			      use_p, stmt, false, names_defined_in_bb))
		goto err;
	    }

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_ALL_DEFS)
	    {
	      if (SSA_NAME_DEF_STMT (op) != stmt)
		{
		  error ("SSA_NAME_DEF_STMT is wrong");
		  fprintf (stderr, "Expected definition statement:\n");
		  print_gimple_stmt (stderr, stmt, 4, TDF_VOPS);
		  fprintf (stderr, "\nActual definition statement:\n");
		  print_gimple_stmt (stderr, SSA_NAME_DEF_STMT (op),
				     4, TDF_VOPS);
		  goto err;
		}
	      bitmap_set_bit (names_defined_in_bb, SSA_NAME_VERSION (op));
	    }
	}

      bitmap_clear (names_defined_in_bb);
    }

  free (definition_block);

  if (gimple_vop (cfun)
      && ssa_default_def (cfun, gimple_vop (cfun)))
    {
      auto_sbitmap visited (last_basic_block_for_fn (cfun) + 1);
      bitmap_clear (visited);
      if (verify_vssa (ENTRY_BLOCK_PTR_FOR_FN (cfun),
		       ssa_default_def (cfun, gimple_vop (cfun)), visited))
	goto err;
    }

  /* Restore the dominance information to its prior known state, so
     that we do not perturb the compiler's subsequent behavior.  */
  if (orig_dom_state == DOM_NONE)
    free_dominance_info (CDI_DOMINATORS);
  else
    set_dom_info_availability (CDI_DOMINATORS, orig_dom_state);

  timevar_pop (TV_TREE_SSA_VERIFY);
  return;

err:
  internal_error ("verify_ssa failed");
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

/* Initialize global DFA and SSA structures.
   If SIZE is non-zero allocated ssa names array of a given size.  */

void
init_tree_ssa (struct function *fn, int size)
{
  fn->gimple_df = ggc_cleared_alloc<gimple_df> ();
  fn->gimple_df->default_defs = hash_table<ssa_name_hasher>::create_ggc (20);
  pt_solution_reset (&fn->gimple_df->escaped);
  init_ssanames (fn, size);
}

/* Deallocate memory associated with SSA data structures for FNDECL.  */

void
delete_tree_ssa (struct function *fn)
{
  fini_ssanames (fn);

  /* We no longer maintain the SSA operand cache at this point.  */
  if (ssa_operands_active (fn))
    fini_ssa_operands (fn);

  fn->gimple_df->default_defs->empty ();
  fn->gimple_df->default_defs = NULL;
  pt_solution_reset (&fn->gimple_df->escaped);
  if (fn->gimple_df->decls_to_pointers != NULL)
    delete fn->gimple_df->decls_to_pointers;
  fn->gimple_df->decls_to_pointers = NULL;
  fn->gimple_df = NULL;

  /* We no longer need the edge variable maps.  */
  redirect_edge_var_map_empty ();
}

/* Return true if EXPR is a useless type conversion, otherwise return
   false.  */

bool
tree_ssa_useless_type_conversion (tree expr)
{
  /* If we have an assignment that merely uses a NOP_EXPR to change
     the top of the RHS to the type of the LHS and the type conversion
     is "safe", then strip away the type conversion so that we can
     enter LHS = RHS into the const_and_copies table.  */
  if (CONVERT_EXPR_P (expr)
      || TREE_CODE (expr) == VIEW_CONVERT_EXPR
      || TREE_CODE (expr) == NON_LVALUE_EXPR)
    return useless_type_conversion_p
      (TREE_TYPE (expr),
       TREE_TYPE (TREE_OPERAND (expr, 0)));

  return false;
}

/* Strip conversions from EXP according to
   tree_ssa_useless_type_conversion and return the resulting
   expression.  */

tree
tree_ssa_strip_useless_type_conversions (tree exp)
{
  while (tree_ssa_useless_type_conversion (exp))
    exp = TREE_OPERAND (exp, 0);
  return exp;
}

/* Return true if T, as SSA_NAME, has an implicit default defined value.  */

bool
ssa_defined_default_def_p (tree t)
{
  tree var = SSA_NAME_VAR (t);

  if (!var)
    ;
  /* Parameters get their initial value from the function entry.  */
  else if (TREE_CODE (var) == PARM_DECL)
    return true;
  /* When returning by reference the return address is actually a hidden
     parameter.  */
  else if (TREE_CODE (var) == RESULT_DECL && DECL_BY_REFERENCE (var))
    return true;
  /* Hard register variables get their initial value from the ether.  */
  else if (VAR_P (var) && DECL_HARD_REGISTER (var))
    return true;

  return false;
}


/* Return true if T, an SSA_NAME, has an undefined value.  PARTIAL is what
   should be returned if the value is only partially undefined.  */

bool
ssa_undefined_value_p (tree t, bool partial)
{
  gimple *def_stmt;

  if (ssa_defined_default_def_p (t))
    return false;

  /* The value is undefined iff its definition statement is empty.  */
  def_stmt = SSA_NAME_DEF_STMT (t);
  if (gimple_nop_p (def_stmt))
    return true;

  /* The value is undefined if the definition statement is a call
     to .DEFERRED_INIT function.  */
  if (gimple_call_internal_p (def_stmt, IFN_DEFERRED_INIT))
    return true;

  /* The value is partially undefined if the definition statement is
     a REALPART_EXPR or IMAGPART_EXPR and its operand is defined by
     the call to .DEFERRED_INIT function.  This is for handling the
     following case:

  1 typedef _Complex float C;
  2 C foo (int cond)
  3 {
  4   C f;
  5   __imag__ f = 0;
  6   if (cond)
  7     {
  8       __real__ f = 1;
  9       return f;
 10     }
 11   return f;
 12 }

    with -ftrivial-auto-var-init, compiler will insert the following
    artificial initialization:
  f = .DEFERRED_INIT (f, 2);
  _1 = REALPART_EXPR <f>;

    we should treat the definition _1 = REALPART_EXPR <f> as undefined.  */
  if (partial && is_gimple_assign (def_stmt)
      && (gimple_assign_rhs_code (def_stmt) == REALPART_EXPR
	  || gimple_assign_rhs_code (def_stmt) == IMAGPART_EXPR))
    {
      tree real_imag_part = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 0);
      if (TREE_CODE (real_imag_part) == SSA_NAME
	 && gimple_call_internal_p (SSA_NAME_DEF_STMT (real_imag_part),
				    IFN_DEFERRED_INIT))
	return true;
    }

  /* Check if the complex was not only partially defined.  */
  if (partial && is_gimple_assign (def_stmt)
      && gimple_assign_rhs_code (def_stmt) == COMPLEX_EXPR)
    {
      tree rhs1, rhs2;

      rhs1 = gimple_assign_rhs1 (def_stmt);
      rhs2 = gimple_assign_rhs2 (def_stmt);
      return (TREE_CODE (rhs1) == SSA_NAME && ssa_undefined_value_p (rhs1))
	     || (TREE_CODE (rhs2) == SSA_NAME && ssa_undefined_value_p (rhs2));
    }
  return false;
}


/* Return TRUE iff STMT, a gimple statement, references an undefined
   SSA name.  */

bool
gimple_uses_undefined_value_p (gimple *stmt)
{
  ssa_op_iter iter;
  tree op;

  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
    if (ssa_undefined_value_p (op))
      return true;

  return false;
}



/* If necessary, rewrite the base of the reference tree *TP from
   a MEM_REF to a plain or converted symbol.  */

static void
maybe_rewrite_mem_ref_base (tree *tp, bitmap suitable_for_renaming)
{
  tree sym;

  while (handled_component_p (*tp))
    tp = &TREE_OPERAND (*tp, 0);
  if (TREE_CODE (*tp) == MEM_REF
      && TREE_CODE (TREE_OPERAND (*tp, 0)) == ADDR_EXPR
      && (sym = TREE_OPERAND (TREE_OPERAND (*tp, 0), 0))
      && DECL_P (sym)
      && !TREE_ADDRESSABLE (sym)
      && bitmap_bit_p (suitable_for_renaming, DECL_UID (sym))
      && is_gimple_reg_type (TREE_TYPE (*tp))
      && ! VOID_TYPE_P (TREE_TYPE (*tp)))
    {
      if (TREE_CODE (TREE_TYPE (sym)) == VECTOR_TYPE
	  && useless_type_conversion_p (TREE_TYPE (*tp),
					TREE_TYPE (TREE_TYPE (sym)))
	  && multiple_of_p (sizetype, TREE_OPERAND (*tp, 1),
			    TYPE_SIZE_UNIT (TREE_TYPE (*tp))))
	{
	  *tp = build3 (BIT_FIELD_REF, TREE_TYPE (*tp), sym, 
			TYPE_SIZE (TREE_TYPE (*tp)),
			int_const_binop (MULT_EXPR,
					 bitsize_int (BITS_PER_UNIT),
					 TREE_OPERAND (*tp, 1)));
	}
      else if (TREE_CODE (TREE_TYPE (sym)) == COMPLEX_TYPE
	       && useless_type_conversion_p (TREE_TYPE (*tp),
					     TREE_TYPE (TREE_TYPE (sym))))
	{
	  *tp = build1 (integer_zerop (TREE_OPERAND (*tp, 1))
			? REALPART_EXPR : IMAGPART_EXPR,
			TREE_TYPE (*tp), sym);
	}
      else if (integer_zerop (TREE_OPERAND (*tp, 1))
	       && DECL_SIZE (sym) == TYPE_SIZE (TREE_TYPE (*tp)))
	{
	  if (!useless_type_conversion_p (TREE_TYPE (*tp),
					  TREE_TYPE (sym)))
	    *tp = build1 (VIEW_CONVERT_EXPR,
			  TREE_TYPE (*tp), sym);
	  else
	    *tp = sym;
	}
      else if (DECL_SIZE (sym)
	       && TREE_CODE (DECL_SIZE (sym)) == INTEGER_CST
	       && (known_subrange_p
		   (mem_ref_offset (*tp),
		    wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (*tp))),
		    0, wi::to_offset (DECL_SIZE_UNIT (sym))))
	       && (! INTEGRAL_TYPE_P (TREE_TYPE (*tp)) 
		   || (wi::to_offset (TYPE_SIZE (TREE_TYPE (*tp)))
		       == TYPE_PRECISION (TREE_TYPE (*tp))))
	       && wi::umod_trunc (wi::to_offset (TYPE_SIZE (TREE_TYPE (*tp))),
				  BITS_PER_UNIT) == 0)
	{
	  *tp = build3 (BIT_FIELD_REF, TREE_TYPE (*tp), sym,
			TYPE_SIZE (TREE_TYPE (*tp)),
			wide_int_to_tree (bitsizetype,
					  mem_ref_offset (*tp)
					  << LOG2_BITS_PER_UNIT));
	}
    }
}

/* For a tree REF return its base if it is the base of a MEM_REF
   that cannot be rewritten into SSA form.  Otherwise return NULL_TREE.  */

static tree
non_rewritable_mem_ref_base (tree ref)
{
  tree base;

  /* A plain decl does not need it set.  */
  if (DECL_P (ref))
    return NULL_TREE;

  if (! (base = CONST_CAST_TREE (strip_invariant_refs (ref))))
    {
      base = get_base_address (ref);
      if (DECL_P (base))
	return base;
      return NULL_TREE;
    }

  /* But watch out for MEM_REFs we cannot lower to a
     VIEW_CONVERT_EXPR or a BIT_FIELD_REF.  */
  if (TREE_CODE (base) == MEM_REF
      && TREE_CODE (TREE_OPERAND (base, 0)) == ADDR_EXPR)
    {
      tree decl = TREE_OPERAND (TREE_OPERAND (base, 0), 0);
      if (! DECL_P (decl))
	return NULL_TREE;
      if (! is_gimple_reg_type (TREE_TYPE (base))
	  || VOID_TYPE_P (TREE_TYPE (base))
	  || TREE_THIS_VOLATILE (decl) != TREE_THIS_VOLATILE (base))
	return decl;
      if ((TREE_CODE (TREE_TYPE (decl)) == VECTOR_TYPE
	   || TREE_CODE (TREE_TYPE (decl)) == COMPLEX_TYPE)
	  && useless_type_conversion_p (TREE_TYPE (base),
					TREE_TYPE (TREE_TYPE (decl)))
	  && known_ge (mem_ref_offset (base), 0)
	  && known_gt (wi::to_poly_offset (TYPE_SIZE_UNIT (TREE_TYPE (decl))),
		       mem_ref_offset (base))
	  && multiple_of_p (sizetype, TREE_OPERAND (base, 1),
			    TYPE_SIZE_UNIT (TREE_TYPE (base))))
	return NULL_TREE;
      /* For same sizes and zero offset we can use a VIEW_CONVERT_EXPR.  */
      if (integer_zerop (TREE_OPERAND (base, 1))
	  && DECL_SIZE (decl) == TYPE_SIZE (TREE_TYPE (base)))
	return NULL_TREE;
      /* For integral typed extracts we can use a BIT_FIELD_REF.  */
      if (DECL_SIZE (decl)
	  && TREE_CODE (DECL_SIZE_UNIT (decl)) == INTEGER_CST
	  && (known_subrange_p
	      (mem_ref_offset (base),
	       wi::to_poly_offset (TYPE_SIZE_UNIT (TREE_TYPE (base))),
	       0, wi::to_poly_offset (DECL_SIZE_UNIT (decl))))
	  /* ???  We can't handle bitfield precision extracts without
	     either using an alternate type for the BIT_FIELD_REF and
	     then doing a conversion or possibly adjusting the offset
	     according to endianness.  */
	  && (! INTEGRAL_TYPE_P (TREE_TYPE (base))
	      || (wi::to_offset (TYPE_SIZE (TREE_TYPE (base)))
		  == TYPE_PRECISION (TREE_TYPE (base))))
	  && wi::umod_trunc (wi::to_offset (TYPE_SIZE (TREE_TYPE (base))),
			     BITS_PER_UNIT) == 0)
	return NULL_TREE;
      return decl;
    }

  /* We cannot rewrite TARGET_MEM_REFs.  */
  if (TREE_CODE (base) == TARGET_MEM_REF
      && TREE_CODE (TREE_OPERAND (base, 0)) == ADDR_EXPR)
    {
      tree decl = TREE_OPERAND (TREE_OPERAND (base, 0), 0);
      if (! DECL_P (decl))
	return NULL_TREE;
      return decl;
    }

  return NULL_TREE;
}

/* For an lvalue tree LHS return true if it cannot be rewritten into SSA form.
   Otherwise return true.  */

static bool 
non_rewritable_lvalue_p (tree lhs)
{
  /* A plain decl is always rewritable.  */
  if (DECL_P (lhs))
    return false;

  /* We can re-write REALPART_EXPR and IMAGPART_EXPR sets in
     a reasonably efficient manner... */
  if ((TREE_CODE (lhs) == REALPART_EXPR
       || TREE_CODE (lhs) == IMAGPART_EXPR)
      && DECL_P (TREE_OPERAND (lhs, 0)))
    return false;

  /* ???  The following could be relaxed allowing component
     references that do not change the access size.  */
  if (TREE_CODE (lhs) == MEM_REF
      && TREE_CODE (TREE_OPERAND (lhs, 0)) == ADDR_EXPR)
    {
      tree decl = TREE_OPERAND (TREE_OPERAND (lhs, 0), 0);

      /* A decl that is wrapped inside a MEM-REF that covers
	 it full is also rewritable.  */
      if (integer_zerop (TREE_OPERAND (lhs, 1))
	  && DECL_P (decl)
	  && DECL_SIZE (decl) == TYPE_SIZE (TREE_TYPE (lhs))
	  /* If the dynamic type of the decl has larger precision than
	     the decl itself we can't use the decls type for SSA rewriting.  */
	  && ((! INTEGRAL_TYPE_P (TREE_TYPE (decl))
	       || compare_tree_int (DECL_SIZE (decl),
				    TYPE_PRECISION (TREE_TYPE (decl))) == 0)
	      || (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		  && (TYPE_PRECISION (TREE_TYPE (decl))
		      >= TYPE_PRECISION (TREE_TYPE (lhs)))))
	  /* Make sure we are not re-writing non-float copying into float
	     copying as that can incur normalization.  */
	  && (! FLOAT_TYPE_P (TREE_TYPE (decl))
	      || types_compatible_p (TREE_TYPE (lhs), TREE_TYPE (decl)))
	  && (TREE_THIS_VOLATILE (decl) == TREE_THIS_VOLATILE (lhs)))
	return false;

      /* A vector-insert using a MEM_REF or ARRAY_REF is rewritable
	 using a BIT_INSERT_EXPR.  */
      if (DECL_P (decl)
	  && VECTOR_TYPE_P (TREE_TYPE (decl))
	  && TYPE_MODE (TREE_TYPE (decl)) != BLKmode
	  && known_ge (mem_ref_offset (lhs), 0)
	  && known_gt (wi::to_poly_offset (TYPE_SIZE_UNIT (TREE_TYPE (decl))),
		       mem_ref_offset (lhs))
	  && multiple_of_p (sizetype, TREE_OPERAND (lhs, 1),
			    TYPE_SIZE_UNIT (TREE_TYPE (lhs)))
	  && known_ge (wi::to_poly_offset (TYPE_SIZE (TREE_TYPE (decl))),
		       wi::to_poly_offset (TYPE_SIZE (TREE_TYPE (lhs)))))
	{
	  poly_uint64 lhs_bits, nelts;
	  if (poly_int_tree_p (TYPE_SIZE (TREE_TYPE (lhs)), &lhs_bits)
	      && multiple_p (lhs_bits,
			     tree_to_uhwi
			       (TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl)))),
			     &nelts)
	      && valid_vector_subparts_p (nelts))
	    {
	      if (known_eq (nelts, 1u))
		return false;
	      /* For sub-vector inserts the insert vector mode has to be
		 supported.  */
	      tree vtype = build_vector_type (TREE_TYPE (TREE_TYPE (decl)),
					      nelts);
	      if (TYPE_MODE (vtype) != BLKmode)
		return false;
	    }
	}
    }

  /* A vector-insert using a BIT_FIELD_REF is rewritable using
     BIT_INSERT_EXPR.  */
  if (TREE_CODE (lhs) == BIT_FIELD_REF
      && DECL_P (TREE_OPERAND (lhs, 0))
      && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (lhs, 0)))
      && TYPE_MODE (TREE_TYPE (TREE_OPERAND (lhs, 0))) != BLKmode
      && operand_equal_p (TYPE_SIZE_UNIT (TREE_TYPE (lhs)),
			  TYPE_SIZE_UNIT
			    (TREE_TYPE (TREE_TYPE (TREE_OPERAND (lhs, 0)))), 0)
      && (tree_to_uhwi (TREE_OPERAND (lhs, 2))
	  % tree_to_uhwi (TYPE_SIZE (TREE_TYPE (lhs)))) == 0)
    return false;

  return true;
}

/* When possible, clear TREE_ADDRESSABLE bit, set or clear DECL_NOT_GIMPLE_REG_P
   and mark the variable VAR for conversion into SSA.  Return true when updating
   stmts is required.  */

static void
maybe_optimize_var (tree var, bitmap addresses_taken, bitmap not_reg_needs,
		    bitmap suitable_for_renaming)
{
  /* Global Variables, result decls cannot be changed.  */
  if (is_global_var (var)
      || TREE_CODE (var) == RESULT_DECL
      || bitmap_bit_p (addresses_taken, DECL_UID (var)))
    return;

  bool maybe_reg = false;
  if (TREE_ADDRESSABLE (var))
    {
      TREE_ADDRESSABLE (var) = 0;
      maybe_reg = true;
      if (dump_file)
	{
	  fprintf (dump_file, "No longer having address taken: ");
	  print_generic_expr (dump_file, var);
	  fprintf (dump_file, "\n");
	}
    }

  /* For register type decls if we do not have any partial defs
     we cannot express in SSA form mark them as DECL_NOT_GIMPLE_REG_P
     as to avoid SSA rewrite.  For the others go ahead and mark
     them for renaming.  */
  if (is_gimple_reg_type (TREE_TYPE (var)))
    {
      if (bitmap_bit_p (not_reg_needs, DECL_UID (var)))
	{
	  DECL_NOT_GIMPLE_REG_P (var) = 1;
	  if (dump_file)
	    {
	      fprintf (dump_file, "Has partial defs: ");
	      print_generic_expr (dump_file, var);
	      fprintf (dump_file, "\n");
	    }
	}
      else if (DECL_NOT_GIMPLE_REG_P (var))
	{
	  maybe_reg = true;
	  DECL_NOT_GIMPLE_REG_P (var) = 0;
	}
      if (maybe_reg && is_gimple_reg (var))
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Now a gimple register: ");
	      print_generic_expr (dump_file, var);
	      fprintf (dump_file, "\n");
	    }
	  bitmap_set_bit (suitable_for_renaming, DECL_UID (var));
	}
    }
}

/* Return true when STMT is ASAN mark where second argument is an address
   of a local variable.  */

static bool
is_asan_mark_p (gimple *stmt)
{
  if (!gimple_call_internal_p (stmt, IFN_ASAN_MARK))
    return false;

  tree addr = get_base_address (gimple_call_arg (stmt, 1));
  if (TREE_CODE (addr) == ADDR_EXPR
      && VAR_P (TREE_OPERAND (addr, 0)))
    {
      tree var = TREE_OPERAND (addr, 0);
      if (lookup_attribute (ASAN_USE_AFTER_SCOPE_ATTRIBUTE,
			    DECL_ATTRIBUTES (var)))
	return false;

      unsigned addressable = TREE_ADDRESSABLE (var);
      TREE_ADDRESSABLE (var) = 0;
      bool r = is_gimple_reg (var);
      TREE_ADDRESSABLE (var) = addressable;
      return r;
    }

  return false;
}

/* Compute TREE_ADDRESSABLE and whether we have unhandled partial defs
   for local variables.  */

void
execute_update_addresses_taken (void)
{
  basic_block bb;
  auto_bitmap addresses_taken;
  auto_bitmap not_reg_needs;
  auto_bitmap suitable_for_renaming;
  tree var;
  unsigned i;

  timevar_push (TV_ADDRESS_TAKEN);

  /* Collect into ADDRESSES_TAKEN all variables whose address is taken within
     the function body.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  enum gimple_code code = gimple_code (stmt);
	  tree decl;

	  if (code == GIMPLE_CALL)
	    {
	      if (optimize_atomic_compare_exchange_p (stmt))
		{
		  /* For __atomic_compare_exchange_N if the second argument
		     is &var, don't mark var addressable;
		     if it becomes non-addressable, we'll rewrite it into
		     ATOMIC_COMPARE_EXCHANGE call.  */
		  tree arg = gimple_call_arg (stmt, 1);
		  gimple_call_set_arg (stmt, 1, null_pointer_node);
		  gimple_ior_addresses_taken (addresses_taken, stmt);
		  gimple_call_set_arg (stmt, 1, arg);
		}
	      else if (is_asan_mark_p (stmt)
		       || gimple_call_internal_p (stmt, IFN_GOMP_SIMT_ENTER))
		;
	      else
		gimple_ior_addresses_taken (addresses_taken, stmt);
	    }
	  else
	    /* Note all addresses taken by the stmt.  */
	    gimple_ior_addresses_taken (addresses_taken, stmt);

	  /* If we have a call or an assignment, see if the lhs contains
	     a local decl that requires not to be a gimple register.  */
	  if (code == GIMPLE_ASSIGN || code == GIMPLE_CALL)
	    {
              tree lhs = gimple_get_lhs (stmt);
              if (lhs
		  && TREE_CODE (lhs) != SSA_NAME
		  && ((code == GIMPLE_CALL && ! DECL_P (lhs))
		      || non_rewritable_lvalue_p (lhs)))
		{
		  decl = get_base_address (lhs);
		  if (DECL_P (decl))
		    bitmap_set_bit (not_reg_needs, DECL_UID (decl));
                }
	    }

	  if (gimple_assign_single_p (stmt))
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);
	      if ((decl = non_rewritable_mem_ref_base (rhs)))
		bitmap_set_bit (not_reg_needs, DECL_UID (decl));
	    }

	  else if (code == GIMPLE_CALL)
	    {
	      for (i = 0; i < gimple_call_num_args (stmt); ++i)
		{
		  tree arg = gimple_call_arg (stmt, i);
		  if ((decl = non_rewritable_mem_ref_base (arg)))
		    bitmap_set_bit (not_reg_needs, DECL_UID (decl));
		}
	    }

	  else if (code == GIMPLE_ASM)
	    {
	      gasm *asm_stmt = as_a <gasm *> (stmt);
	      for (i = 0; i < gimple_asm_noutputs (asm_stmt); ++i)
		{
		  tree link = gimple_asm_output_op (asm_stmt, i);
		  tree lhs = TREE_VALUE (link);
		  if (TREE_CODE (lhs) != SSA_NAME)
		    {
		      decl = get_base_address (lhs);
		      if (DECL_P (decl)
			  && (non_rewritable_lvalue_p (lhs)
			      /* We cannot move required conversions from
				 the lhs to the rhs in asm statements, so
				 require we do not need any.  */
			      || !useless_type_conversion_p
			            (TREE_TYPE (lhs), TREE_TYPE (decl))))
			bitmap_set_bit (not_reg_needs, DECL_UID (decl));
		    }
		}
	      for (i = 0; i < gimple_asm_ninputs (asm_stmt); ++i)
		{
		  tree link = gimple_asm_input_op (asm_stmt, i);
		  if ((decl = non_rewritable_mem_ref_base (TREE_VALUE (link))))
		    bitmap_set_bit (not_reg_needs, DECL_UID (decl));
		}
	    }
	}

      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  size_t i;
	  gphi *phi = gsi.phi ();

	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree op = PHI_ARG_DEF (phi, i), var;
	      if (TREE_CODE (op) == ADDR_EXPR
		  && (var = get_base_address (TREE_OPERAND (op, 0))) != NULL
		  && DECL_P (var))
		bitmap_set_bit (addresses_taken, DECL_UID (var));
	    }
	}
    }

  /* We cannot iterate over all referenced vars because that can contain
     unused vars from BLOCK trees, which causes code generation differences
     for -g vs. -g0.  */
  for (var = DECL_ARGUMENTS (cfun->decl); var; var = DECL_CHAIN (var))
    maybe_optimize_var (var, addresses_taken, not_reg_needs,
			suitable_for_renaming);

  FOR_EACH_VEC_SAFE_ELT (cfun->local_decls, i, var)
    maybe_optimize_var (var, addresses_taken, not_reg_needs,
			suitable_for_renaming);

  /* Operand caches need to be recomputed for operands referencing the updated
     variables and operands need to be rewritten to expose bare symbols.  */
  if (!bitmap_empty_p (suitable_for_renaming))
    {
      FOR_EACH_BB_FN (bb, cfun)
	for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	  {
	    gimple *stmt = gsi_stmt (gsi);

	    /* Re-write TARGET_MEM_REFs of symbols we want to
	       rewrite into SSA form.  */
	    if (gimple_assign_single_p (stmt))
	      {
		tree lhs = gimple_assign_lhs (stmt);
		tree rhs, *rhsp = gimple_assign_rhs1_ptr (stmt);
		tree sym;

		/* Rewrite LHS IMAG/REALPART_EXPR similar to
		   gimplify_modify_expr_complex_part.  */
		if ((TREE_CODE (lhs) == IMAGPART_EXPR
		     || TREE_CODE (lhs) == REALPART_EXPR)
		    && DECL_P (TREE_OPERAND (lhs, 0))
		    && bitmap_bit_p (suitable_for_renaming,
				     DECL_UID (TREE_OPERAND (lhs, 0))))
		  {
		    tree other = make_ssa_name (TREE_TYPE (lhs));
		    tree lrhs = build1 (TREE_CODE (lhs) == IMAGPART_EXPR
					? REALPART_EXPR : IMAGPART_EXPR,
					TREE_TYPE (other),
					TREE_OPERAND (lhs, 0));
		    gimple *load = gimple_build_assign (other, lrhs);
		    location_t loc = gimple_location (stmt);
		    gimple_set_location (load, loc);
		    gimple_set_vuse (load, gimple_vuse (stmt));
		    gsi_insert_before (&gsi, load, GSI_SAME_STMT);
		    gimple_assign_set_lhs (stmt, TREE_OPERAND (lhs, 0));
		    gimple_assign_set_rhs_with_ops
		      (&gsi, COMPLEX_EXPR,
		       TREE_CODE (lhs) == IMAGPART_EXPR
		       ? other : gimple_assign_rhs1 (stmt),
		       TREE_CODE (lhs) == IMAGPART_EXPR
		       ? gimple_assign_rhs1 (stmt) : other, NULL_TREE);
		    stmt = gsi_stmt (gsi);
		    unlink_stmt_vdef (stmt);
		    update_stmt (stmt);
		    continue;
		  }

		/* Rewrite a vector insert via a BIT_FIELD_REF on the LHS
		   into a BIT_INSERT_EXPR.  */
		if (TREE_CODE (lhs) == BIT_FIELD_REF
		    && DECL_P (TREE_OPERAND (lhs, 0))
		    && bitmap_bit_p (suitable_for_renaming,
				     DECL_UID (TREE_OPERAND (lhs, 0)))
		    && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (lhs, 0)))
		    && TYPE_MODE (TREE_TYPE (TREE_OPERAND (lhs, 0))) != BLKmode
		    && operand_equal_p (TYPE_SIZE_UNIT (TREE_TYPE (lhs)),
					TYPE_SIZE_UNIT (TREE_TYPE
					  (TREE_TYPE (TREE_OPERAND (lhs, 0)))),
					0)
		    && (tree_to_uhwi (TREE_OPERAND (lhs, 2))
			% tree_to_uhwi (TYPE_SIZE (TREE_TYPE (lhs))) == 0))
		  {
		    tree var = TREE_OPERAND (lhs, 0);
		    tree val = gimple_assign_rhs1 (stmt);
		    if (! types_compatible_p (TREE_TYPE (TREE_TYPE (var)),
					      TREE_TYPE (val)))
		      {
			tree tem = make_ssa_name (TREE_TYPE (TREE_TYPE (var)));
			gimple *pun
			  = gimple_build_assign (tem,
						 build1 (VIEW_CONVERT_EXPR,
							 TREE_TYPE (tem), val));
			gsi_insert_before (&gsi, pun, GSI_SAME_STMT);
			val = tem;
		      }
		    tree bitpos = TREE_OPERAND (lhs, 2);
		    gimple_assign_set_lhs (stmt, var);
		    gimple_assign_set_rhs_with_ops
		      (&gsi, BIT_INSERT_EXPR, var, val, bitpos);
		    stmt = gsi_stmt (gsi);
		    unlink_stmt_vdef (stmt);
		    update_stmt (stmt);
		    continue;
		  }

		/* Rewrite a vector insert using a MEM_REF on the LHS
		   into a BIT_INSERT_EXPR.  */
		if (TREE_CODE (lhs) == MEM_REF
		    && TREE_CODE (TREE_OPERAND (lhs, 0)) == ADDR_EXPR
		    && (sym = TREE_OPERAND (TREE_OPERAND (lhs, 0), 0))
		    && DECL_P (sym)
		    && bitmap_bit_p (suitable_for_renaming, DECL_UID (sym))
		    && VECTOR_TYPE_P (TREE_TYPE (sym))
		    && TYPE_MODE (TREE_TYPE (sym)) != BLKmode
		    /* If it is a full replacement we can do better below.  */
		    && maybe_ne (wi::to_poly_offset
				   (TYPE_SIZE_UNIT (TREE_TYPE (lhs))),
				 wi::to_poly_offset
                                   (TYPE_SIZE_UNIT (TREE_TYPE (sym))))
		    && known_ge (mem_ref_offset (lhs), 0)
		    && known_gt (wi::to_poly_offset
				   (TYPE_SIZE_UNIT (TREE_TYPE (sym))),
				 mem_ref_offset (lhs))
		    && multiple_of_p (sizetype,
				      TREE_OPERAND (lhs, 1),
				      TYPE_SIZE_UNIT (TREE_TYPE (lhs))))
		  {
		    tree val = gimple_assign_rhs1 (stmt);
		    if (! types_compatible_p (TREE_TYPE (val),
					      TREE_TYPE (TREE_TYPE (sym))))
		      {
			poly_uint64 lhs_bits, nelts;
			tree temtype = TREE_TYPE (TREE_TYPE (sym));
			if (poly_int_tree_p (TYPE_SIZE (TREE_TYPE (lhs)),
					     &lhs_bits)
			    && multiple_p (lhs_bits,
					   tree_to_uhwi
					     (TYPE_SIZE (TREE_TYPE
							   (TREE_TYPE (sym)))),
					   &nelts)
			    && maybe_ne (nelts, 1u)
			    && valid_vector_subparts_p (nelts))
			  temtype = build_vector_type (temtype, nelts);
			tree tem = make_ssa_name (temtype);
			gimple *pun
			  = gimple_build_assign (tem,
						 build1 (VIEW_CONVERT_EXPR,
							 TREE_TYPE (tem), val));
			gsi_insert_before (&gsi, pun, GSI_SAME_STMT);
			val = tem;
		      }
		    tree bitpos
		      = wide_int_to_tree (bitsizetype,
					  mem_ref_offset (lhs) * BITS_PER_UNIT);
		    gimple_assign_set_lhs (stmt, sym);
		    gimple_assign_set_rhs_with_ops
		      (&gsi, BIT_INSERT_EXPR, sym, val, bitpos);
		    stmt = gsi_stmt (gsi);
		    unlink_stmt_vdef (stmt);
		    update_stmt (stmt);
		    continue;
		  }

		/* We shouldn't have any fancy wrapping of
		   component-refs on the LHS, but look through
		   VIEW_CONVERT_EXPRs as that is easy.  */
		while (TREE_CODE (lhs) == VIEW_CONVERT_EXPR)
		  lhs = TREE_OPERAND (lhs, 0);
		if (TREE_CODE (lhs) == MEM_REF
		    && TREE_CODE (TREE_OPERAND (lhs, 0)) == ADDR_EXPR
		    && integer_zerop (TREE_OPERAND (lhs, 1))
		    && (sym = TREE_OPERAND (TREE_OPERAND (lhs, 0), 0))
		    && DECL_P (sym)
		    && !TREE_ADDRESSABLE (sym)
		    && bitmap_bit_p (suitable_for_renaming, DECL_UID (sym)))
		  lhs = sym;
		else
		  lhs = gimple_assign_lhs (stmt);

		/* Rewrite the RHS and make sure the resulting assignment
		   is validly typed.  */
		maybe_rewrite_mem_ref_base (rhsp, suitable_for_renaming);
		rhs = gimple_assign_rhs1 (stmt);
		if (gimple_assign_lhs (stmt) != lhs
		    && !useless_type_conversion_p (TREE_TYPE (lhs),
						   TREE_TYPE (rhs)))
		  {
		    if (gimple_clobber_p (stmt))
		      {
			rhs = build_constructor (TREE_TYPE (lhs), NULL);
			TREE_THIS_VOLATILE (rhs) = 1;
		      }
		    else
		      rhs = fold_build1 (VIEW_CONVERT_EXPR,
					 TREE_TYPE (lhs), rhs);
		  }
		if (gimple_assign_lhs (stmt) != lhs)
		  gimple_assign_set_lhs (stmt, lhs);

		if (gimple_assign_rhs1 (stmt) != rhs)
		  {
		    gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
		    gimple_assign_set_rhs_from_tree (&gsi, rhs);
		  }
	      }

	    else if (gimple_code (stmt) == GIMPLE_CALL)
	      {
		unsigned i;
		if (optimize_atomic_compare_exchange_p (stmt))
		  {
		    tree expected = gimple_call_arg (stmt, 1);
		    if (bitmap_bit_p (suitable_for_renaming,
				      DECL_UID (TREE_OPERAND (expected, 0))))
		      {
			fold_builtin_atomic_compare_exchange (&gsi);
			continue;
		      }
		  }
		else if (is_asan_mark_p (stmt))
		  {
		    tree var = TREE_OPERAND (gimple_call_arg (stmt, 1), 0);
		    if (bitmap_bit_p (suitable_for_renaming, DECL_UID (var)))
		      {
			unlink_stmt_vdef (stmt);
			if (asan_mark_p (stmt, ASAN_MARK_POISON))
			  {
			    gcall *call
			      = gimple_build_call_internal (IFN_ASAN_POISON, 0);
			    gimple_call_set_lhs (call, var);
			    gsi_replace (&gsi, call, true);
			  }
			else
			  {
			    /* In ASAN_MARK (UNPOISON, &b, ...) the variable
			       is uninitialized.  Avoid dependencies on
			       previous out of scope value.  */
			    tree clobber = build_clobber (TREE_TYPE (var));
			    gimple *g = gimple_build_assign (var, clobber);
			    gsi_replace (&gsi, g, true);
			  }
			continue;
		      }
		  }
		else if (gimple_call_internal_p (stmt, IFN_GOMP_SIMT_ENTER))
		  for (i = 1; i < gimple_call_num_args (stmt); i++)
		    {
		      tree *argp = gimple_call_arg_ptr (stmt, i);
		      if (*argp == null_pointer_node)
			continue;
		      gcc_assert (TREE_CODE (*argp) == ADDR_EXPR
				  && VAR_P (TREE_OPERAND (*argp, 0)));
		      tree var = TREE_OPERAND (*argp, 0);
		      if (bitmap_bit_p (suitable_for_renaming, DECL_UID (var)))
			*argp = null_pointer_node;
		    }
		for (i = 0; i < gimple_call_num_args (stmt); ++i)
		  {
		    tree *argp = gimple_call_arg_ptr (stmt, i);
		    maybe_rewrite_mem_ref_base (argp, suitable_for_renaming);
		  }
	      }

	    else if (gimple_code (stmt) == GIMPLE_ASM)
	      {
		gasm *asm_stmt = as_a <gasm *> (stmt);
		unsigned i;
		for (i = 0; i < gimple_asm_noutputs (asm_stmt); ++i)
		  {
		    tree link = gimple_asm_output_op (asm_stmt, i);
		    maybe_rewrite_mem_ref_base (&TREE_VALUE (link),
						suitable_for_renaming);
		  }
		for (i = 0; i < gimple_asm_ninputs (asm_stmt); ++i)
		  {
		    tree link = gimple_asm_input_op (asm_stmt, i);
		    maybe_rewrite_mem_ref_base (&TREE_VALUE (link),
						suitable_for_renaming);
		  }
	      }

	    else if (gimple_debug_bind_p (stmt)
		     && gimple_debug_bind_has_value_p (stmt))
	      {
		tree *valuep = gimple_debug_bind_get_value_ptr (stmt);
		tree decl;
		maybe_rewrite_mem_ref_base (valuep, suitable_for_renaming);
		decl = non_rewritable_mem_ref_base (*valuep);
		if (decl
		    && bitmap_bit_p (suitable_for_renaming, DECL_UID (decl)))
		  gimple_debug_bind_reset_value (stmt);
	      }

	    if (gimple_references_memory_p (stmt)
		|| is_gimple_debug (stmt))
	      update_stmt (stmt);

	    gsi_next (&gsi);
	  }

      /* Update SSA form here, we are called as non-pass as well.  */
      if (number_of_loops (cfun) > 1
	  && loops_state_satisfies_p (LOOP_CLOSED_SSA))
	rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
      else
	update_ssa (TODO_update_ssa);
    }

  timevar_pop (TV_ADDRESS_TAKEN);
}

namespace {

const pass_data pass_data_update_address_taken =
{
  GIMPLE_PASS, /* type */
  "addressables", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_ADDRESS_TAKEN, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_address_taken, /* todo_flags_finish */
};

class pass_update_address_taken : public gimple_opt_pass
{
public:
  pass_update_address_taken (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_update_address_taken, ctxt)
  {}

  /* opt_pass methods: */

}; // class pass_update_address_taken

} // anon namespace

gimple_opt_pass *
make_pass_update_address_taken (gcc::context *ctxt)
{
  return new pass_update_address_taken (ctxt);
}
