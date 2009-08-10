/* Miscellaneous SSA utility functions.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tm_p.h"
#include "target.h"
#include "ggc.h"
#include "langhooks.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "bitmap.h"
#include "pointer-set.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-inline.h"
#include "varray.h"
#include "timevar.h"
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "toplev.h"

/* Pointer map of variable mappings, keyed by edge.  */
static struct pointer_map_t *edge_var_maps;


/* Add a mapping with PHI RESULT and PHI DEF associated with edge E.  */

void
redirect_edge_var_map_add (edge e, tree result, tree def, source_location locus)
{
  void **slot;
  edge_var_map_vector old_head, head;
  edge_var_map new_node;

  if (edge_var_maps == NULL)
    edge_var_maps = pointer_map_create ();

  slot = pointer_map_insert (edge_var_maps, e);
  old_head = head = (edge_var_map_vector) *slot;
  if (!head)
    {
      head = VEC_alloc (edge_var_map, heap, 5);
      *slot = head;
    }
  new_node.def = def;
  new_node.result = result;
  new_node.locus = locus;

  VEC_safe_push (edge_var_map, heap, head, &new_node);
  if (old_head != head)
    {
      /* The push did some reallocation.  Update the pointer map.  */
      *slot = head;
    }
}


/* Clear the var mappings in edge E.  */

void
redirect_edge_var_map_clear (edge e)
{
  void **slot;
  edge_var_map_vector head;

  if (!edge_var_maps)
    return;

  slot = pointer_map_contains (edge_var_maps, e);

  if (slot)
    {
      head = (edge_var_map_vector) *slot;
      VEC_free (edge_var_map, heap, head);
      *slot = NULL;
    }
}


/* Duplicate the redirected var mappings in OLDE in NEWE.

   Since we can't remove a mapping, let's just duplicate it.  This assumes a
   pointer_map can have multiple edges mapping to the same var_map (many to
   one mapping), since we don't remove the previous mappings.  */

void
redirect_edge_var_map_dup (edge newe, edge olde)
{
  void **new_slot, **old_slot;
  edge_var_map_vector head;

  if (!edge_var_maps)
    return;

  new_slot = pointer_map_insert (edge_var_maps, newe);
  old_slot = pointer_map_contains (edge_var_maps, olde);
  if (!old_slot)
    return;
  head = (edge_var_map_vector) *old_slot;

  if (head)
    *new_slot = VEC_copy (edge_var_map, heap, head);
  else
    *new_slot = VEC_alloc (edge_var_map, heap, 5);
}


/* Return the variable mappings for a given edge.  If there is none, return
   NULL.  */

edge_var_map_vector
redirect_edge_var_map_vector (edge e)
{
  void **slot;

  /* Hey, what kind of idiot would... you'd be surprised.  */
  if (!edge_var_maps)
    return NULL;

  slot = pointer_map_contains (edge_var_maps, e);
  if (!slot)
    return NULL;

  return (edge_var_map_vector) *slot;
}

/* Used by redirect_edge_var_map_destroy to free all memory.  */

static bool
free_var_map_entry (const void *key ATTRIBUTE_UNUSED,
		    void **value,
		    void *data ATTRIBUTE_UNUSED)
{
  edge_var_map_vector head = (edge_var_map_vector) *value;
  VEC_free (edge_var_map, heap, head);
  return true;
}

/* Clear the edge variable mappings.  */

void
redirect_edge_var_map_destroy (void)
{
  if (edge_var_maps)
    {
      pointer_map_traverse (edge_var_maps, free_var_map_entry, NULL);
      pointer_map_destroy (edge_var_maps);
      edge_var_maps = NULL;
    }
}


/* Remove the corresponding arguments from the PHI nodes in E's
   destination block and redirect it to DEST.  Return redirected edge.
   The list of removed arguments is stored in a vector accessed
   through edge_var_maps.  */

edge
ssa_redirect_edge (edge e, basic_block dest)
{
  gimple_stmt_iterator gsi;
  gimple phi;

  redirect_edge_var_map_clear (e);

  /* Remove the appropriate PHI arguments in E's destination block.  */
  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      tree def;
      source_location locus ;

      phi = gsi_stmt (gsi);
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
  gimple phi;
  edge_var_map_vector v;
  edge_var_map *vm;
  int i;
  gimple_stmt_iterator gsi;

  v = redirect_edge_var_map_vector (e);
  if (!v)
    return;

  for (gsi = gsi_start_phis (e->dest), i = 0;
       !gsi_end_p (gsi) && VEC_iterate (edge_var_map, v, i, vm);
       gsi_next (&gsi), i++)
    {
      tree def;

      phi = gsi_stmt (gsi);
      def = redirect_edge_var_map_def (vm);
      add_phi_arg (phi, def, e, redirect_edge_var_map_location (vm));
    }

  redirect_edge_var_map_clear (e);
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

  if (TREE_TYPE (ssa_name) != TREE_TYPE (SSA_NAME_VAR (ssa_name)))
    {
      error ("type mismatch between an SSA_NAME and its symbol");
      return true;
    }

  if (SSA_NAME_IN_FREE_LIST (ssa_name))
    {
      error ("found an SSA_NAME that had been released into the free pool");
      return true;
    }

  if (is_virtual && is_gimple_reg (ssa_name))
    {
      error ("found a virtual definition for a GIMPLE register");
      return true;
    }

  if (is_virtual && SSA_NAME_VAR (ssa_name) != gimple_vop (cfun))
    {
      error ("virtual SSA name for non-VOP decl");
      return true;
    }

  if (!is_virtual && !is_gimple_reg (ssa_name))
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
	    gimple stmt, bool is_virtual)
{
  if (verify_ssa_name (ssa_name, is_virtual))
    goto err;

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
  print_generic_expr (stderr, ssa_name, 0);
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
	    gimple stmt, bool check_abnormal, bitmap names_defined_in_bb)
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
verify_phi_args (gimple phi, basic_block bb, basic_block *definition_block)
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

      if (TREE_CODE (op) == SSA_NAME)
	{
	  err = verify_ssa_name (op, !is_gimple_reg (gimple_phi_result (phi)));
	  err |= verify_use (e->src, definition_block[SSA_NAME_VERSION (op)],
			     op_p, phi, e->flags & EDGE_ABNORMAL, NULL);
	}

      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  tree base = TREE_OPERAND (op, 0);
	  while (handled_component_p (base))
	    base = TREE_OPERAND (base, 0);
	  if ((TREE_CODE (base) == VAR_DECL
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

void
verify_ssa (bool check_modified_stmt)
{
  size_t i;
  basic_block bb;
  basic_block *definition_block = XCNEWVEC (basic_block, num_ssa_names);
  ssa_op_iter iter;
  tree op;
  enum dom_state orig_dom_state = dom_info_state (CDI_DOMINATORS);
  bitmap names_defined_in_bb = BITMAP_ALLOC (NULL);

  gcc_assert (!need_ssa_update_p (cfun));

  verify_stmts ();

  timevar_push (TV_TREE_SSA_VERIFY);

  /* Keep track of SSA names present in the IL.  */
  for (i = 1; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name)
	{
	  gimple stmt;
	  TREE_VISITED (name) = 0;

	  stmt = SSA_NAME_DEF_STMT (name);
	  if (!gimple_nop_p (stmt))
	    {
	      basic_block bb = gimple_bb (stmt);
	      verify_def (bb, definition_block,
			  name, stmt, !is_gimple_reg (name));

	    }
	}
    }

  calculate_dominance_info (CDI_DOMINATORS);

  /* Now verify all the uses and make sure they agree with the definitions
     found in the previous pass.  */
  FOR_EACH_BB (bb)
    {
      edge e;
      gimple phi;
      edge_iterator ei;
      gimple_stmt_iterator gsi;

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
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  phi = gsi_stmt (gsi);
	  if (verify_phi_args (phi, bb, definition_block))
	    goto err;

	  bitmap_set_bit (names_defined_in_bb,
			  SSA_NAME_VERSION (gimple_phi_result (phi)));
	}

      /* Now verify all the uses and vuses in every statement of the block.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  use_operand_p use_p;
	  bool has_err;

	  if (check_modified_stmt && gimple_modified_p (stmt))
	    {
	      error ("stmt (%p) marked modified after optimization pass: ",
		     (void *)stmt);
	      print_gimple_stmt (stderr, stmt, 0, TDF_VOPS);
	      goto err;
	    }

	  if (is_gimple_assign (stmt)
	      && TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
	    {
	      tree lhs, base_address;

	      lhs = gimple_assign_lhs (stmt);
	      base_address = get_base_address (lhs);

	      if (base_address
		  && SSA_VAR_P (base_address)
		  && !gimple_vdef (stmt)
		  && optimize > 0)
		{
		  error ("statement makes a memory store, but has no VDEFS");
		  print_gimple_stmt (stderr, stmt, 0, TDF_VOPS);
		  goto err;
		}
	    }

	  /* Verify the single virtual operand and its constraints.  */
	  has_err = false;
	  if (gimple_vdef (stmt))
	    {
	      if (gimple_vdef_op (stmt) == NULL_DEF_OPERAND_P)
		{
		  error ("statement has VDEF operand not in defs list");
		  has_err = true;
		}
	      if (!gimple_vuse (stmt))
		{
		  error ("statement has VDEF but no VUSE operand");
		  has_err = true;
		}
	      else if (SSA_NAME_VAR (gimple_vdef (stmt))
		       != SSA_NAME_VAR (gimple_vuse (stmt)))
		{
		  error ("VDEF and VUSE do not use the same symbol");
		  has_err = true;
		}
	      has_err |= verify_ssa_name (gimple_vdef (stmt), true);
	    }
	  if (gimple_vuse (stmt))
	    {
	      if  (gimple_vuse_op (stmt) == NULL_USE_OPERAND_P)
		{
		  error ("statement has VUSE operand not in uses list");
		  has_err = true;
		}
	      has_err |= verify_ssa_name (gimple_vuse (stmt), true);
	    }
	  if (has_err)
	    {
	      error ("in statement");
	      print_gimple_stmt (stderr, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
	      goto err;
	    }

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE|SSA_OP_DEF)
	    {
	      if (verify_ssa_name (op, false))
		{
		  error ("in statement");
		  print_gimple_stmt (stderr, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
		  goto err;
		}
	    }

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

  /* Restore the dominance information to its prior known state, so
     that we do not perturb the compiler's subsequent behavior.  */
  if (orig_dom_state == DOM_NONE)
    free_dominance_info (CDI_DOMINATORS);
  else
    set_dom_info_availability (CDI_DOMINATORS, orig_dom_state);
  
  BITMAP_FREE (names_defined_in_bb);
  timevar_pop (TV_TREE_SSA_VERIFY);
  return;

err:
  internal_error ("verify_ssa failed");
}

/* Return true if the uid in both int tree maps are equal.  */

int
int_tree_map_eq (const void *va, const void *vb)
{
  const struct int_tree_map *a = (const struct int_tree_map *) va;
  const struct int_tree_map *b = (const struct int_tree_map *) vb;
  return (a->uid == b->uid);
}

/* Hash a UID in a int_tree_map.  */

unsigned int
int_tree_map_hash (const void *item)
{
  return ((const struct int_tree_map *)item)->uid;
}

/* Return true if the DECL_UID in both trees are equal.  */

int
uid_decl_map_eq (const void *va, const void *vb)
{
  const_tree a = (const_tree) va;
  const_tree b = (const_tree) vb;
  return (a->decl_minimal.uid == b->decl_minimal.uid);
}

/* Hash a tree in a uid_decl_map.  */

unsigned int
uid_decl_map_hash (const void *item)
{
  return ((const_tree)item)->decl_minimal.uid;
}

/* Return true if the DECL_UID in both trees are equal.  */

static int
uid_ssaname_map_eq (const void *va, const void *vb)
{
  const_tree a = (const_tree) va;
  const_tree b = (const_tree) vb;
  return (a->ssa_name.var->decl_minimal.uid == b->ssa_name.var->decl_minimal.uid);
}

/* Hash a tree in a uid_decl_map.  */

static unsigned int
uid_ssaname_map_hash (const void *item)
{
  return ((const_tree)item)->ssa_name.var->decl_minimal.uid;
}


/* Initialize global DFA and SSA structures.  */

void
init_tree_ssa (struct function *fn)
{
  fn->gimple_df = GGC_CNEW (struct gimple_df);
  fn->gimple_df->referenced_vars = htab_create_ggc (20, uid_decl_map_hash, 
				     		    uid_decl_map_eq, NULL);
  fn->gimple_df->default_defs = htab_create_ggc (20, uid_ssaname_map_hash, 
				                 uid_ssaname_map_eq, NULL);
  pt_solution_reset (&fn->gimple_df->escaped);
  pt_solution_reset (&fn->gimple_df->callused);
  init_ssanames (fn, 0);
  init_phinodes ();
}


/* Deallocate memory associated with SSA data structures for FNDECL.  */

void
delete_tree_ssa (void)
{
  referenced_var_iterator rvi;
  tree var;

  /* Remove annotations from every referenced local variable.  */
  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      if (is_global_var (var))
	continue;
      if (var->base.ann)
        ggc_free (var->base.ann);
      var->base.ann = NULL;
    }
  htab_delete (gimple_referenced_vars (cfun));
  cfun->gimple_df->referenced_vars = NULL;

  fini_ssanames ();
  fini_phinodes ();

  /* We no longer maintain the SSA operand cache at this point.  */
  if (ssa_operands_active ())
    fini_ssa_operands ();

  delete_alias_heapvars ();

  htab_delete (cfun->gimple_df->default_defs);
  cfun->gimple_df->default_defs = NULL;
  pt_solution_reset (&cfun->gimple_df->escaped);
  pt_solution_reset (&cfun->gimple_df->callused);
  if (cfun->gimple_df->decls_to_pointers != NULL)
    pointer_map_destroy (cfun->gimple_df->decls_to_pointers);
  cfun->gimple_df->decls_to_pointers = NULL;
  cfun->gimple_df->modified_noreturn_calls = NULL;
  cfun->gimple_df = NULL;

  /* We no longer need the edge variable maps.  */
  redirect_edge_var_map_destroy ();
}

/* Return true if the conversion from INNER_TYPE to OUTER_TYPE is a
   useless type conversion, otherwise return false.

   This function implicitly defines the middle-end type system.  With
   the notion of 'a < b' meaning that useless_type_conversion_p (a, b)
   holds and 'a > b' meaning that useless_type_conversion_p (b, a) holds,
   the following invariants shall be fulfilled:

     1) useless_type_conversion_p is transitive.
	If a < b and b < c then a < c.

     2) useless_type_conversion_p is not symmetric.
	From a < b does not follow a > b.

     3) Types define the available set of operations applicable to values.
	A type conversion is useless if the operations for the target type
	is a subset of the operations for the source type.  For example
	casts to void* are useless, casts from void* are not (void* can't
	be dereferenced or offsetted, but copied, hence its set of operations
	is a strict subset of that of all other data pointer types).  Casts
	to const T* are useless (can't be written to), casts from const T*
	to T* are not.  */

bool
useless_type_conversion_p (tree outer_type, tree inner_type)
{
  /* Do the following before stripping toplevel qualifiers.  */
  if (POINTER_TYPE_P (inner_type)
      && POINTER_TYPE_P (outer_type))
    {
      /* Do not lose casts to restrict qualified pointers.  */
      if ((TYPE_RESTRICT (outer_type)
	   != TYPE_RESTRICT (inner_type))
	  && TYPE_RESTRICT (outer_type))
	return false;
    }

  /* From now on qualifiers on value types do not matter.  */
  inner_type = TYPE_MAIN_VARIANT (inner_type);
  outer_type = TYPE_MAIN_VARIANT (outer_type);

  if (inner_type == outer_type)
    return true;

  /* If we know the canonical types, compare them.  */
  if (TYPE_CANONICAL (inner_type)
      && TYPE_CANONICAL (inner_type) == TYPE_CANONICAL (outer_type))
    return true;

  /* Changes in machine mode are never useless conversions unless we
     deal with aggregate types in which case we defer to later checks.  */
  if (TYPE_MODE (inner_type) != TYPE_MODE (outer_type)
      && !AGGREGATE_TYPE_P (inner_type))
    return false;

  /* If both the inner and outer types are integral types, then the
     conversion is not necessary if they have the same mode and
     signedness and precision, and both or neither are boolean.  */
  if (INTEGRAL_TYPE_P (inner_type)
      && INTEGRAL_TYPE_P (outer_type))
    {
      /* Preserve changes in signedness or precision.  */
      if (TYPE_UNSIGNED (inner_type) != TYPE_UNSIGNED (outer_type)
	  || TYPE_PRECISION (inner_type) != TYPE_PRECISION (outer_type))
	return false;

      /* We don't need to preserve changes in the types minimum or
	 maximum value in general as these do not generate code
	 unless the types precisions are different.  */
      return true;
    }

  /* Scalar floating point types with the same mode are compatible.  */
  else if (SCALAR_FLOAT_TYPE_P (inner_type)
	   && SCALAR_FLOAT_TYPE_P (outer_type))
    return true;

  /* Fixed point types with the same mode are compatible.  */
  else if (FIXED_POINT_TYPE_P (inner_type)
	   && FIXED_POINT_TYPE_P (outer_type))
    return true;

  /* We need to take special care recursing to pointed-to types.  */
  else if (POINTER_TYPE_P (inner_type)
	   && POINTER_TYPE_P (outer_type))
    {
      /* If the outer type is (void *) or a pointer to an incomplete
	 record type, then the conversion is not necessary.  */
      if (VOID_TYPE_P (TREE_TYPE (outer_type))
	  || (AGGREGATE_TYPE_P (TREE_TYPE (outer_type))
	      && TREE_CODE (TREE_TYPE (outer_type)) != ARRAY_TYPE
	      && (TREE_CODE (TREE_TYPE (outer_type))
		  == TREE_CODE (TREE_TYPE (inner_type)))
	      && !COMPLETE_TYPE_P (TREE_TYPE (outer_type))))
	return true;

      /* Don't lose casts between pointers to volatile and non-volatile
	 qualified types.  Doing so would result in changing the semantics
	 of later accesses.  For function types the volatile qualifier
	 is used to indicate noreturn functions.  */
      if (TREE_CODE (TREE_TYPE (outer_type)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (outer_type)) != METHOD_TYPE
	  && TREE_CODE (TREE_TYPE (inner_type)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (inner_type)) != METHOD_TYPE
	  && (TYPE_VOLATILE (TREE_TYPE (outer_type))
	      != TYPE_VOLATILE (TREE_TYPE (inner_type)))
	  && TYPE_VOLATILE (TREE_TYPE (outer_type)))
	return false;

      /* We require explicit conversions from incomplete target types.  */
      if (!COMPLETE_TYPE_P (TREE_TYPE (inner_type))
	  && COMPLETE_TYPE_P (TREE_TYPE (outer_type)))
	return false;

      /* Do not lose casts between pointers that when dereferenced access
	 memory with different alias sets.  */
      if (get_deref_alias_set (inner_type) != get_deref_alias_set (outer_type))
	return false;

      /* We do not care for const qualification of the pointed-to types
	 as const qualification has no semantic value to the middle-end.  */

      /* Otherwise pointers/references are equivalent if their pointed
	 to types are effectively the same.  We can strip qualifiers
	 on pointed-to types for further comparison, which is done in
	 the callee.  Note we have to use true compatibility here
	 because addresses are subject to propagation into dereferences
	 and thus might get the original type exposed which is equivalent
	 to a reverse conversion.  */
      return types_compatible_p (TREE_TYPE (outer_type),
				 TREE_TYPE (inner_type));
    }

  /* Recurse for complex types.  */
  else if (TREE_CODE (inner_type) == COMPLEX_TYPE
	   && TREE_CODE (outer_type) == COMPLEX_TYPE)
    return useless_type_conversion_p (TREE_TYPE (outer_type),
				      TREE_TYPE (inner_type));

  /* Recurse for vector types with the same number of subparts.  */
  else if (TREE_CODE (inner_type) == VECTOR_TYPE
	   && TREE_CODE (outer_type) == VECTOR_TYPE
	   && TYPE_PRECISION (inner_type) == TYPE_PRECISION (outer_type))
    return useless_type_conversion_p (TREE_TYPE (outer_type),
				      TREE_TYPE (inner_type));

  else if (TREE_CODE (inner_type) == ARRAY_TYPE
	   && TREE_CODE (outer_type) == ARRAY_TYPE)
    {
      /* Preserve string attributes.  */
      if (TYPE_STRING_FLAG (inner_type) != TYPE_STRING_FLAG (outer_type))
	return false;

      /* Conversions from array types with unknown extent to
	 array types with known extent are not useless.  */
      if (!TYPE_DOMAIN (inner_type)
	  && TYPE_DOMAIN (outer_type))
	return false;

      /* Nor are conversions from array types with non-constant size to
         array types with constant size or to different size.  */
      if (TYPE_SIZE (outer_type)
	  && TREE_CODE (TYPE_SIZE (outer_type)) == INTEGER_CST
	  && (!TYPE_SIZE (inner_type)
	      || TREE_CODE (TYPE_SIZE (inner_type)) != INTEGER_CST
	      || !tree_int_cst_equal (TYPE_SIZE (outer_type),
				      TYPE_SIZE (inner_type))))
	return false;

      /* Check conversions between arrays with partially known extents.
	 If the array min/max values are constant they have to match.
	 Otherwise allow conversions to unknown and variable extents.
	 In particular this declares conversions that may change the
	 mode to BLKmode as useless.  */
      if (TYPE_DOMAIN (inner_type)
	  && TYPE_DOMAIN (outer_type)
	  && TYPE_DOMAIN (inner_type) != TYPE_DOMAIN (outer_type))
	{
	  tree inner_min = TYPE_MIN_VALUE (TYPE_DOMAIN (inner_type));
	  tree outer_min = TYPE_MIN_VALUE (TYPE_DOMAIN (outer_type));
	  tree inner_max = TYPE_MAX_VALUE (TYPE_DOMAIN (inner_type));
	  tree outer_max = TYPE_MAX_VALUE (TYPE_DOMAIN (outer_type));

	  /* After gimplification a variable min/max value carries no
	     additional information compared to a NULL value.  All that
	     matters has been lowered to be part of the IL.  */
	  if (inner_min && TREE_CODE (inner_min) != INTEGER_CST)
	    inner_min = NULL_TREE;
	  if (outer_min && TREE_CODE (outer_min) != INTEGER_CST)
	    outer_min = NULL_TREE;
	  if (inner_max && TREE_CODE (inner_max) != INTEGER_CST)
	    inner_max = NULL_TREE;
	  if (outer_max && TREE_CODE (outer_max) != INTEGER_CST)
	    outer_max = NULL_TREE;

	  /* Conversions NULL / variable <- cst are useless, but not
	     the other way around.  */
	  if (outer_min
	      && (!inner_min
		  || !tree_int_cst_equal (inner_min, outer_min)))
	    return false;
	  if (outer_max
	      && (!inner_max
		  || !tree_int_cst_equal (inner_max, outer_max)))
	    return false;
	}

      /* Recurse on the element check.  */
      return useless_type_conversion_p (TREE_TYPE (outer_type),
					TREE_TYPE (inner_type));
    }

  else if ((TREE_CODE (inner_type) == FUNCTION_TYPE
	    || TREE_CODE (inner_type) == METHOD_TYPE)
	   && TREE_CODE (inner_type) == TREE_CODE (outer_type))
    {
      tree outer_parm, inner_parm;

      /* If the return types are not compatible bail out.  */
      if (!useless_type_conversion_p (TREE_TYPE (outer_type),
				      TREE_TYPE (inner_type)))
	return false;

      /* Method types should belong to a compatible base class.  */
      if (TREE_CODE (inner_type) == METHOD_TYPE
	  && !useless_type_conversion_p (TYPE_METHOD_BASETYPE (outer_type),
					 TYPE_METHOD_BASETYPE (inner_type)))
	return false;

      /* A conversion to an unprototyped argument list is ok.  */
      if (!TYPE_ARG_TYPES (outer_type))
	return true;

      /* If the argument types are compatible the conversion is useless.  */
      if (TYPE_ARG_TYPES (outer_type) == TYPE_ARG_TYPES (inner_type))
	return true;

      for (outer_parm = TYPE_ARG_TYPES (outer_type),
	   inner_parm = TYPE_ARG_TYPES (inner_type);
	   outer_parm && inner_parm;
	   outer_parm = TREE_CHAIN (outer_parm),
	   inner_parm = TREE_CHAIN (inner_parm))
	if (!useless_type_conversion_p (TREE_VALUE (outer_parm),
					TREE_VALUE (inner_parm)))
	  return false;

      /* If there is a mismatch in the number of arguments the functions
	 are not compatible.  */
      if (outer_parm || inner_parm)
	return false;

      /* Defer to the target if necessary.  */
      if (TYPE_ATTRIBUTES (inner_type) || TYPE_ATTRIBUTES (outer_type))
	return targetm.comp_type_attributes (outer_type, inner_type) != 0;

      return true;
    }

  /* For aggregates we may need to fall back to structural equality
     checks.  */
  else if (AGGREGATE_TYPE_P (inner_type)
	   && TREE_CODE (inner_type) == TREE_CODE (outer_type))
    {
      if (TYPE_STRUCTURAL_EQUALITY_P (outer_type)
	  || TYPE_STRUCTURAL_EQUALITY_P (inner_type))
	return lang_hooks.types_compatible_p (inner_type, outer_type);

      return false;
    }
  
  return false;
}

/* Return true if a conversion from either type of TYPE1 and TYPE2
   to the other is not required.  Otherwise return false.  */

bool
types_compatible_p (tree type1, tree type2)
{
  return (type1 == type2
	  || (useless_type_conversion_p (type1, type2)
	      && useless_type_conversion_p (type2, type1)));
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


/* Internal helper for walk_use_def_chains.  VAR, FN and DATA are as
   described in walk_use_def_chains.
   
   VISITED is a pointer set used to mark visited SSA_NAMEs to avoid
      infinite loops.  We used to have a bitmap for this to just mark
      SSA versions we had visited.  But non-sparse bitmaps are way too
      expensive, while sparse bitmaps may cause quadratic behavior.

   IS_DFS is true if the caller wants to perform a depth-first search
      when visiting PHI nodes.  A DFS will visit each PHI argument and
      call FN after each one.  Otherwise, all the arguments are
      visited first and then FN is called with each of the visited
      arguments in a separate pass.  */

static bool
walk_use_def_chains_1 (tree var, walk_use_def_chains_fn fn, void *data,
		       struct pointer_set_t *visited, bool is_dfs)
{
  gimple def_stmt;

  if (pointer_set_insert (visited, var))
    return false;

  def_stmt = SSA_NAME_DEF_STMT (var);

  if (gimple_code (def_stmt) != GIMPLE_PHI)
    {
      /* If we reached the end of the use-def chain, call FN.  */
      return fn (var, def_stmt, data);
    }
  else
    {
      size_t i;

      /* When doing a breadth-first search, call FN before following the
	 use-def links for each argument.  */
      if (!is_dfs)
	for (i = 0; i < gimple_phi_num_args (def_stmt); i++)
	  if (fn (gimple_phi_arg_def (def_stmt, i), def_stmt, data))
	    return true;

      /* Follow use-def links out of each PHI argument.  */
      for (i = 0; i < gimple_phi_num_args (def_stmt); i++)
	{
	  tree arg = gimple_phi_arg_def (def_stmt, i);

	  /* ARG may be NULL for newly introduced PHI nodes.  */
	  if (arg
	      && TREE_CODE (arg) == SSA_NAME
	      && walk_use_def_chains_1 (arg, fn, data, visited, is_dfs))
	    return true;
	}

      /* When doing a depth-first search, call FN after following the
	 use-def links for each argument.  */
      if (is_dfs)
	for (i = 0; i < gimple_phi_num_args (def_stmt); i++)
	  if (fn (gimple_phi_arg_def (def_stmt, i), def_stmt, data))
	    return true;
    }
  
  return false;
}
  


/* Walk use-def chains starting at the SSA variable VAR.  Call
   function FN at each reaching definition found.  FN takes three
   arguments: VAR, its defining statement (DEF_STMT) and a generic
   pointer to whatever state information that FN may want to maintain
   (DATA).  FN is able to stop the walk by returning true, otherwise
   in order to continue the walk, FN should return false.  

   Note, that if DEF_STMT is a PHI node, the semantics are slightly
   different.  The first argument to FN is no longer the original
   variable VAR, but the PHI argument currently being examined.  If FN
   wants to get at VAR, it should call PHI_RESULT (PHI).

   If IS_DFS is true, this function will:

	1- walk the use-def chains for all the PHI arguments, and,
	2- call (*FN) (ARG, PHI, DATA) on all the PHI arguments.

   If IS_DFS is false, the two steps above are done in reverse order
   (i.e., a breadth-first search).  */

void
walk_use_def_chains (tree var, walk_use_def_chains_fn fn, void *data,
                     bool is_dfs)
{
  gimple def_stmt;

  gcc_assert (TREE_CODE (var) == SSA_NAME);

  def_stmt = SSA_NAME_DEF_STMT (var);

  /* We only need to recurse if the reaching definition comes from a PHI
     node.  */
  if (gimple_code (def_stmt) != GIMPLE_PHI)
    (*fn) (var, def_stmt, data);
  else
    {
      struct pointer_set_t *visited = pointer_set_create ();
      walk_use_def_chains_1 (var, fn, data, visited, is_dfs);
      pointer_set_destroy (visited);
    }
}


/* Return true if T, an SSA_NAME, has an undefined value.  */

bool
ssa_undefined_value_p (tree t)
{
  tree var = SSA_NAME_VAR (t);

  /* Parameters get their initial value from the function entry.  */
  if (TREE_CODE (var) == PARM_DECL)
    return false;

  /* Hard register variables get their initial value from the ether.  */
  if (TREE_CODE (var) == VAR_DECL && DECL_HARD_REGISTER (var))
    return false;

  /* The value is undefined iff its definition statement is empty.  */
  return gimple_nop_p (SSA_NAME_DEF_STMT (t));
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

/* Emit a warning for T, an SSA_NAME, being uninitialized.  The exact
   warning text is in MSGID and LOCUS may contain a location or be null.  */

static void
warn_uninit (tree t, const char *gmsgid, void *data)
{
  tree var = SSA_NAME_VAR (t);
  gimple context = (gimple) data;
  location_t location;
  expanded_location xloc, floc;

  if (!ssa_undefined_value_p (t))
    return;

  /* TREE_NO_WARNING either means we already warned, or the front end
     wishes to suppress the warning.  */
  if (TREE_NO_WARNING (var))
    return;

  /* Do not warn if it can be initialized outside this module.  */
  if (is_global_var (var))
    return;
  
  location = (context != NULL && gimple_has_location (context))
	     ? gimple_location (context)
	     : DECL_SOURCE_LOCATION (var);
  xloc = expand_location (location);
  floc = expand_location (DECL_SOURCE_LOCATION (cfun->decl));
  if (warning_at (location, OPT_Wuninitialized, gmsgid, var))
    {
      TREE_NO_WARNING (var) = 1;

      if (xloc.file != floc.file
	  || xloc.line < floc.line
	  || xloc.line > LOCATION_LINE (cfun->function_end_locus))
	inform (DECL_SOURCE_LOCATION (var), "%qD was declared here", var);
    }
}

struct walk_data {
  gimple stmt;
  bool always_executed;
  bool warn_possibly_uninitialized;
};

/* Called via walk_tree, look for SSA_NAMEs that have empty definitions
   and warn about them.  */

static tree
warn_uninitialized_var (tree *tp, int *walk_subtrees, void *data_)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data_;
  struct walk_data *data = (struct walk_data *) wi->info;
  tree t = *tp;

  /* We do not care about LHS.  */
  if (wi->is_lhs)
    {
      /* Except for operands of INDIRECT_REF.  */
      if (!INDIRECT_REF_P (t))
	return NULL_TREE;
      t = TREE_OPERAND (t, 0);
    }

  switch (TREE_CODE (t))
    {
    case ADDR_EXPR:
      /* Taking the address of an uninitialized variable does not
	 count as using it.  */
      *walk_subtrees = 0;
      break;

    case VAR_DECL:
      {
	/* A VAR_DECL in the RHS of a gimple statement may mean that
	   this variable is loaded from memory.  */
	use_operand_p vuse;
	tree op;

	/* If there is not gimple stmt, 
	   or alias information has not been computed,
	   then we cannot check VUSE ops.  */
	if (data->stmt == NULL)
	  return NULL_TREE;

	/* If the load happens as part of a call do not warn about it.  */
	if (is_gimple_call (data->stmt))
	  return NULL_TREE;

	vuse = gimple_vuse_op (data->stmt);
	if (vuse == NULL_USE_OPERAND_P)
	  return NULL_TREE;

	op = USE_FROM_PTR (vuse);
	if (t != SSA_NAME_VAR (op) 
	    || !SSA_NAME_IS_DEFAULT_DEF (op))
	  return NULL_TREE;
	/* If this is a VUSE of t and it is the default definition,
	   then warn about op.  */
	t = op;
	/* Fall through into SSA_NAME.  */
      }

    case SSA_NAME:
      /* We only do data flow with SSA_NAMEs, so that's all we
	 can warn about.  */
      if (data->always_executed)
        warn_uninit (t, "%qD is used uninitialized in this function",
		     data->stmt);
      else if (data->warn_possibly_uninitialized)
        warn_uninit (t, "%qD may be used uninitialized in this function",
		     data->stmt);
      *walk_subtrees = 0;
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      /* The total store transformation performed during gimplification
	 creates uninitialized variable uses.  If all is well, these will
	 be optimized away, so don't warn now.  */
      if (TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME)
	*walk_subtrees = 0;
      break;

    default:
      if (IS_TYPE_OR_DECL_P (t))
	*walk_subtrees = 0;
      break;
    }

  return NULL_TREE;
}

/* Look for inputs to PHI that are SSA_NAMEs that have empty definitions
   and warn about them.  */

static void
warn_uninitialized_phi (gimple phi)
{
  size_t i, n = gimple_phi_num_args (phi);

  /* Don't look at memory tags.  */
  if (!is_gimple_reg (gimple_phi_result (phi)))
    return;

  for (i = 0; i < n; ++i)
    {
      tree op = gimple_phi_arg_def (phi, i);
      if (TREE_CODE (op) == SSA_NAME)
	warn_uninit (op, "%qD may be used uninitialized in this function",
		     NULL);
    }
}

static unsigned int
warn_uninitialized_vars (bool warn_possibly_uninitialized)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  struct walk_data data;

  data.warn_possibly_uninitialized = warn_possibly_uninitialized;

  calculate_dominance_info (CDI_POST_DOMINATORS);

  FOR_EACH_BB (bb)
    {
      data.always_executed = dominated_by_p (CDI_POST_DOMINATORS,
					     single_succ (ENTRY_BLOCK_PTR), bb);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  struct walk_stmt_info wi;
	  data.stmt = gsi_stmt (gsi);
	  memset (&wi, 0, sizeof (wi));
	  wi.info = &data;
	  walk_gimple_op (gsi_stmt (gsi), warn_uninitialized_var, &wi);
	}
    }

  /* Post-dominator information can not be reliably updated. Free it
     after the use.  */

  free_dominance_info (CDI_POST_DOMINATORS);
  return 0;
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
  warn_uninitialized_vars (/*warn_possibly_uninitialized=*/!optimize);
  return 0;
}

static unsigned int
execute_late_warn_uninitialized (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  /* Re-do the plain uninitialized variable check, as optimization may have
     straightened control flow.  Do this first so that we don't accidentally
     get a "may be" warning when we'd have seen an "is" warning later.  */
  warn_uninitialized_vars (/*warn_possibly_uninitialized=*/1);

  FOR_EACH_BB (bb)
    for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      warn_uninitialized_phi (gsi_stmt (gsi));

  return 0;
}

static bool
gate_warn_uninitialized (void)
{
  return warn_uninitialized != 0;
}

struct gimple_opt_pass pass_early_warn_uninitialized =
{
 {
  GIMPLE_PASS,
  NULL,					/* name */
  gate_warn_uninitialized,		/* gate */
  execute_early_warn_uninitialized,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_late_warn_uninitialized =
{
 {
  GIMPLE_PASS,
  NULL,					/* name */
  gate_warn_uninitialized,		/* gate */
  execute_late_warn_uninitialized,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};

/* Compute TREE_ADDRESSABLE and DECL_GIMPLE_REG_P for local variables.  */

void
execute_update_addresses_taken (bool do_optimize)
{
  tree var;
  referenced_var_iterator rvi;
  gimple_stmt_iterator gsi;
  basic_block bb;
  bitmap addresses_taken = BITMAP_ALLOC (NULL);
  bitmap not_reg_needs = BITMAP_ALLOC (NULL);
  bool update_vops = false;

  /* Collect into ADDRESSES_TAKEN all variables whose address is taken within
     the function body.  */
  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  enum gimple_code code = gimple_code (stmt);

	  /* Note all addresses taken by the stmt.  */
	  gimple_ior_addresses_taken (addresses_taken, stmt);

	  /* If we have a call or an assignment, see if the lhs contains
	     a local decl that requires not to be a gimple register.  */
	  if (code == GIMPLE_ASSIGN || code == GIMPLE_CALL)
	    {
              tree lhs = gimple_get_lhs (stmt);
              
              /* We may not rewrite TMR_SYMBOL to SSA.  */
              if (lhs && TREE_CODE (lhs) == TARGET_MEM_REF
                  && TMR_SYMBOL (lhs))
                bitmap_set_bit (not_reg_needs, DECL_UID (TMR_SYMBOL (lhs)));

              /* A plain decl does not need it set.  */
              else if (lhs && handled_component_p (lhs))
                {
                  var = get_base_address (lhs);
                  if (DECL_P (var))
                    bitmap_set_bit (not_reg_needs, DECL_UID (var));
                }
	    }
	}

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  size_t i;
	  gimple phi = gsi_stmt (gsi);

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

  /* When possible, clear ADDRESSABLE bit or set the REGISTER bit
     and mark variable for conversion into SSA.  */
  if (optimize && do_optimize)
    FOR_EACH_REFERENCED_VAR (var, rvi)
      {
	/* Global Variables, result decls cannot be changed.  */
	if (is_global_var (var)
	    || TREE_CODE (var) == RESULT_DECL
	    || bitmap_bit_p (addresses_taken, DECL_UID (var)))
	  continue;

	if (TREE_ADDRESSABLE (var)
	    /* Do not change TREE_ADDRESSABLE if we need to preserve var as
	       a non-register.  Otherwise we are confused and forget to
	       add virtual operands for it.  */
	    && (!is_gimple_reg_type (TREE_TYPE (var))
		|| !bitmap_bit_p (not_reg_needs, DECL_UID (var))))
	  {
	    TREE_ADDRESSABLE (var) = 0;
	    if (is_gimple_reg (var))
	      mark_sym_for_renaming (var);
	    update_vops = true;
	    if (dump_file)
	      {
		fprintf (dump_file, "No longer having address taken ");
		print_generic_expr (dump_file, var, 0);
		fprintf (dump_file, "\n");
	      }
	  }
	if (!DECL_GIMPLE_REG_P (var)
	    && !bitmap_bit_p (not_reg_needs, DECL_UID (var))
	    && (TREE_CODE (TREE_TYPE (var)) == COMPLEX_TYPE
		|| TREE_CODE (TREE_TYPE (var)) == VECTOR_TYPE)
	    && !TREE_THIS_VOLATILE (var)
	    && (TREE_CODE (var) != VAR_DECL || !DECL_HARD_REGISTER (var)))
	  {
	    DECL_GIMPLE_REG_P (var) = 1;
	    mark_sym_for_renaming (var);
	    update_vops = true;
	    if (dump_file)
	      {
		fprintf (dump_file, "Decl is now a gimple register ");
		print_generic_expr (dump_file, var, 0);
		fprintf (dump_file, "\n");
	      }
	  }
      }

  /* Operand caches needs to be recomputed for operands referencing the updated
     variables.  */
  if (update_vops)
    {
      FOR_EACH_BB (bb)
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);

	      if (gimple_references_memory_p (stmt))
		update_stmt (stmt);
	    }

      /* Update SSA form here, we are called as non-pass as well.  */
      update_ssa (TODO_update_ssa);
    }

  BITMAP_FREE (not_reg_needs);
  BITMAP_FREE (addresses_taken);
}

struct gimple_opt_pass pass_update_address_taken =
{
 {
  GIMPLE_PASS,
  "addressables",			/* name */
  NULL,					/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_address_taken
  | TODO_dump_func			/* todo_flags_finish */
 }
};
