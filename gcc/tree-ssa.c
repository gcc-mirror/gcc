/* Miscellaneous SSA utility functions.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.

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
#include "tree-gimple.h"
#include "tree-inline.h"
#include "varray.h"
#include "timevar.h"
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "toplev.h"

/* Remove the corresponding arguments from the PHI nodes in E's
   destination block and redirect it to DEST.  Return redirected edge.
   The list of removed arguments is stored in PENDING_STMT (e).  */

edge
ssa_redirect_edge (edge e, basic_block dest)
{
  tree phi;
  tree list = NULL, *last = &list;
  tree src, dst, node;

  /* Remove the appropriate PHI arguments in E's destination block.  */
  for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
    {
      if (PHI_ARG_DEF (phi, e->dest_idx) == NULL_TREE)
	continue;

      src = PHI_ARG_DEF (phi, e->dest_idx);
      dst = PHI_RESULT (phi);
      node = build_tree_list (dst, src);
      *last = node;
      last = &TREE_CHAIN (node);
    }

  e = redirect_edge_succ_nodup (e, dest);
  PENDING_STMT (e) = list;

  return e;
}

/* Add PHI arguments queued in PENDING_STMT list on edge E to edge
   E->dest.  */

void
flush_pending_stmts (edge e)
{
  tree phi, arg;

  if (!PENDING_STMT (e))
    return;

  for (phi = phi_nodes (e->dest), arg = PENDING_STMT (e);
       phi;
       phi = PHI_CHAIN (phi), arg = TREE_CHAIN (arg))
    {
      tree def = TREE_VALUE (arg);
      add_phi_arg (phi, def, e);
    }

  PENDING_STMT (e) = NULL;
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

  if (!is_virtual && !is_gimple_reg (ssa_name))
    {
      error ("found a real definition for a non-register");
      return true;
    }

  if (is_virtual && var_ann (SSA_NAME_VAR (ssa_name)) 
      && get_subvars_for_var (SSA_NAME_VAR (ssa_name)) != NULL)
    {
      error ("found real variable when subvariables should have appeared");
      return true;
    }

  if (SSA_NAME_IS_DEFAULT_DEF (ssa_name)
      && !IS_EMPTY_STMT (SSA_NAME_DEF_STMT (ssa_name)))
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
	    tree stmt, bool is_virtual)
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
      print_generic_stmt (stderr, SSA_NAME_DEF_STMT (ssa_name), TDF_VOPS);
      fprintf (stderr, "\nActual definition statement:\n");
      print_generic_stmt (stderr, stmt, TDF_VOPS);
      goto err;
    }

  return false;

err:
  fprintf (stderr, "while verifying SSA_NAME ");
  print_generic_expr (stderr, ssa_name, 0);
  fprintf (stderr, " in statement\n");
  print_generic_stmt (stderr, stmt, TDF_VOPS);

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
	    tree stmt, bool check_abnormal, bitmap names_defined_in_bb)
{
  bool err = false;
  tree ssa_name = USE_FROM_PTR (use_p);

  if (!TREE_VISITED (ssa_name))
    if (verify_imm_links (stderr, ssa_name))
      err = true;

  TREE_VISITED (ssa_name) = 1;

  if (IS_EMPTY_STMT (SSA_NAME_DEF_STMT (ssa_name))
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
      tree listvar ;
      if (use_p->prev->use == NULL)
	listvar = use_p->prev->stmt;
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
      print_generic_stmt (stderr, stmt, TDF_VOPS);
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
verify_phi_args (tree phi, basic_block bb, basic_block *definition_block)
{
  edge e;
  bool err = false;
  unsigned i, phi_num_args = PHI_NUM_ARGS (phi);

  if (EDGE_COUNT (bb->preds) != phi_num_args)
    {
      error ("incoming edge count does not match number of PHI arguments");
      err = true;
      goto error;
    }

  for (i = 0; i < phi_num_args; i++)
    {
      use_operand_p op_p = PHI_ARG_DEF_PTR (phi, i);
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
	  err = verify_ssa_name (op, !is_gimple_reg (PHI_RESULT (phi)));
	  err |= verify_use (e->src, definition_block[SSA_NAME_VERSION (op)],
			     op_p, phi, e->flags & EDGE_ABNORMAL, NULL);
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
      print_generic_stmt (stderr, phi, TDF_VOPS|TDF_MEMSYMS);
    }


  return err;
}


static void
verify_flow_insensitive_alias_info (void)
{
  tree var;
  referenced_var_iterator rvi;

  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      unsigned int j;
      bitmap aliases;
      tree alias;
      bitmap_iterator bi;

      if (!MTAG_P (var) || !MTAG_ALIASES (var))
	continue;
      
      aliases = MTAG_ALIASES (var);

      EXECUTE_IF_SET_IN_BITMAP (aliases, 0, j, bi)
	{
	  alias = referenced_var (j);

	  if (TREE_CODE (alias) != MEMORY_PARTITION_TAG
	      && !may_be_aliased (alias))
	    {
	      error ("non-addressable variable inside an alias set");
	      debug_variable (alias);
	      goto err;
	    }
	}
    }

  return;

err:
  debug_variable (var);
  internal_error ("verify_flow_insensitive_alias_info failed");
}


static void
verify_flow_sensitive_alias_info (void)
{
  size_t i;
  tree ptr;

  for (i = 1; i < num_ssa_names; i++)
    {
      tree var;
      var_ann_t ann;
      struct ptr_info_def *pi;
 

      ptr = ssa_name (i);
      if (!ptr)
	continue;

      /* We only care for pointers that are actually referenced in the
	 program.  */
      if (!POINTER_TYPE_P (TREE_TYPE (ptr)) || !TREE_VISITED (ptr))
	continue;

      /* RESULT_DECL is special.  If it's a GIMPLE register, then it
	 is only written-to only once in the return statement.
	 Otherwise, aggregate RESULT_DECLs may be written-to more than
	 once in virtual operands.  */
      var = SSA_NAME_VAR (ptr);
      if (TREE_CODE (var) == RESULT_DECL
	  && is_gimple_reg (ptr))
	continue;

      pi = SSA_NAME_PTR_INFO (ptr);
      if (pi == NULL)
	continue;

      ann = var_ann (var);
      if (pi->is_dereferenced && !pi->name_mem_tag && !ann->symbol_mem_tag)
	{
	  error ("dereferenced pointers should have a name or a symbol tag");
	  goto err;
	}

      if (pi->name_mem_tag
	  && (pi->pt_vars == NULL || bitmap_empty_p (pi->pt_vars)))
	{
	  error ("pointers with a memory tag, should have points-to sets");
	  goto err;
	}

      if (pi->value_escapes_p && pi->name_mem_tag)
	{
	  tree t = memory_partition (pi->name_mem_tag);
	  if (t == NULL_TREE)
	    t = pi->name_mem_tag;
	  
	  if (!is_call_clobbered (t))
	    {
	      error ("pointer escapes but its name tag is not call-clobbered");
	      goto err;
	    }
	}
    }

  return;

err:
  debug_variable (ptr);
  internal_error ("verify_flow_sensitive_alias_info failed");
}


/* Verify the consistency of call clobbering information.  */

static void
verify_call_clobbering (void)
{
  unsigned int i;
  bitmap_iterator bi;
  tree var;
  referenced_var_iterator rvi;

  /* At all times, the result of the call_clobbered flag should
     match the result of the call_clobbered_vars bitmap.  Verify both
     that everything in call_clobbered_vars is marked
     call_clobbered, and that everything marked
     call_clobbered is in call_clobbered_vars.  */
  EXECUTE_IF_SET_IN_BITMAP (gimple_call_clobbered_vars (cfun), 0, i, bi)
    {
      var = referenced_var (i);

      if (memory_partition (var))
	var = memory_partition (var);

      if (!MTAG_P (var) && !var_ann (var)->call_clobbered)
	{
	  error ("variable in call_clobbered_vars but not marked "
	         "call_clobbered");
	  debug_variable (var);
	  goto err;
	}
    }

  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      if (is_gimple_reg (var))
	continue;

      if (memory_partition (var))
	var = memory_partition (var);

      if (!MTAG_P (var)
	  && var_ann (var)->call_clobbered
	  && !bitmap_bit_p (gimple_call_clobbered_vars (cfun), DECL_UID (var)))
	{
	  error ("variable marked call_clobbered but not in "
	         "call_clobbered_vars bitmap.");
	  debug_variable (var);
	  goto err;
	}
    }

  return;

 err:
    internal_error ("verify_call_clobbering failed");
}


/* Verify invariants in memory partitions.  */

static void
verify_memory_partitions (void)
{
  unsigned i;
  tree mpt;
  VEC(tree,heap) *mpt_table = gimple_ssa_operands (cfun)->mpt_table;
  struct pointer_set_t *partitioned_syms = pointer_set_create ();

  for (i = 0; VEC_iterate (tree, mpt_table, i, mpt); i++)
    {
      unsigned j;
      bitmap_iterator bj;

      if (MPT_SYMBOLS (mpt) == NULL)
	{
	  error ("Memory partitions should have at least one symbol");
	  debug_variable (mpt);
	  goto err;
	}

      EXECUTE_IF_SET_IN_BITMAP (MPT_SYMBOLS (mpt), 0, j, bj)
	{
	  tree var = referenced_var (j);
	  if (pointer_set_insert (partitioned_syms, var))
	    {
	      error ("Partitioned symbols should belong to exactly one "
		     "partition");
	      debug_variable (var);
	      goto err;
	    }
	}
    }

  pointer_set_destroy (partitioned_syms);

  return;

err:
  internal_error ("verify_memory_partitions failed");
}


/* Verify the consistency of aliasing information.  */

static void
verify_alias_info (void)
{
  verify_flow_sensitive_alias_info ();
  verify_call_clobbering ();
  verify_flow_insensitive_alias_info ();
  verify_memory_partitions ();
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

  gcc_assert (!need_ssa_update_p ());

  verify_stmts ();

  timevar_push (TV_TREE_SSA_VERIFY);

  /* Keep track of SSA names present in the IL.  */
  for (i = 1; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name)
	{
	  tree stmt;
	  TREE_VISITED (name) = 0;

	  stmt = SSA_NAME_DEF_STMT (name);
	  if (!IS_EMPTY_STMT (stmt))
	    {
	      basic_block bb = bb_for_stmt (stmt);
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
      tree phi;
      edge_iterator ei;
      block_stmt_iterator bsi;

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
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  if (verify_phi_args (phi, bb, definition_block))
	    goto err;

	  bitmap_set_bit (names_defined_in_bb,
			  SSA_NAME_VERSION (PHI_RESULT (phi)));
	}

      /* Now verify all the uses and vuses in every statement of the block.  */
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);
	  use_operand_p use_p;

	  if (check_modified_stmt && stmt_modified_p (stmt))
	    {
	      error ("stmt (%p) marked modified after optimization pass: ",
		     (void *)stmt);
	      print_generic_stmt (stderr, stmt, TDF_VOPS);
	      goto err;
	    }

	  if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT
	      && TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) != SSA_NAME)
	    {
	      tree lhs, base_address;

	      lhs = GIMPLE_STMT_OPERAND (stmt, 0);
	      base_address = get_base_address (lhs);

	      if (base_address
		  && gimple_aliases_computed_p (cfun)
		  && SSA_VAR_P (base_address)
		  && !stmt_ann (stmt)->has_volatile_ops
		  && ZERO_SSA_OPERANDS (stmt, SSA_OP_VDEF))
		{
		  error ("statement makes a memory store, but has no VDEFS");
		  print_generic_stmt (stderr, stmt, TDF_VOPS);
		  goto err;
		}
	    }

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_ALL_VIRTUALS)
	    {
	      if (verify_ssa_name (op, true))
		{
		  error ("in statement");
		  print_generic_stmt (stderr, stmt, TDF_VOPS|TDF_MEMSYMS);
		  goto err;
		}
	    }

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE|SSA_OP_DEF)
	    {
	      if (verify_ssa_name (op, false))
		{
		  error ("in statement");
		  print_generic_stmt (stderr, stmt, TDF_VOPS|TDF_MEMSYMS);
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
	    bitmap_set_bit (names_defined_in_bb, SSA_NAME_VERSION (op));
	}

      bitmap_clear (names_defined_in_bb);
    }

  /* Finally, verify alias information.  */
  if (gimple_aliases_computed_p (cfun))
    verify_alias_info ();

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
init_tree_ssa (void)
{
  cfun->gimple_df = GGC_CNEW (struct gimple_df);
  cfun->gimple_df->referenced_vars = htab_create_ggc (20, uid_decl_map_hash, 
				     		      uid_decl_map_eq, NULL);
  cfun->gimple_df->default_defs = htab_create_ggc (20, uid_ssaname_map_hash, 
				                   uid_ssaname_map_eq, NULL);
  cfun->gimple_df->call_clobbered_vars = BITMAP_GGC_ALLOC ();
  cfun->gimple_df->addressable_vars = BITMAP_GGC_ALLOC ();
  init_ssanames ();
  init_phinodes ();
}


/* Deallocate memory associated with SSA data structures for FNDECL.  */

void
delete_tree_ssa (void)
{
  size_t i;
  basic_block bb;
  block_stmt_iterator bsi;
  referenced_var_iterator rvi;
  tree var;

  /* Release any ssa_names still in use.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      tree var = ssa_name (i);
      if (var && TREE_CODE (var) == SSA_NAME)
        {
	  SSA_NAME_IMM_USE_NODE (var).prev = &(SSA_NAME_IMM_USE_NODE (var));
	  SSA_NAME_IMM_USE_NODE (var).next = &(SSA_NAME_IMM_USE_NODE (var));
	}
      release_ssa_name (var);
    }

  /* Remove annotations from every tree in the function.  */
  FOR_EACH_BB (bb)
    {
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);
	  stmt_ann_t ann = get_stmt_ann (stmt);

	  free_ssa_operands (&ann->operands);
	  ann->addresses_taken = 0;
	  mark_stmt_modified (stmt);
	}
      set_phi_nodes (bb, NULL);
    }

  /* Remove annotations from every referenced local variable.  */
  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      if (!MTAG_P (var)
	  && (TREE_STATIC (var) || DECL_EXTERNAL (var)))
	{
	  var_ann (var)->mpt = NULL_TREE;
	  var_ann (var)->symbol_mem_tag = NULL_TREE;
	  var_ann (var)->subvars = NULL;
	  continue;
	}
      if (var->base.ann)
        ggc_free (var->base.ann);
      var->base.ann = NULL;
    }
  htab_delete (gimple_referenced_vars (cfun));
  cfun->gimple_df->referenced_vars = NULL;

  fini_ssanames ();
  fini_phinodes ();
  /* we no longer maintain the SSA operand cache at this point.  */
  if (ssa_operands_active ())
    fini_ssa_operands ();

  cfun->gimple_df->global_var = NULL_TREE;
  
  htab_delete (cfun->gimple_df->default_defs);
  cfun->gimple_df->default_defs = NULL;
  cfun->gimple_df->call_clobbered_vars = NULL;
  cfun->gimple_df->addressable_vars = NULL;
  cfun->gimple_df->modified_noreturn_calls = NULL;
  if (gimple_aliases_computed_p (cfun))
    {
      delete_alias_heapvars ();
      gcc_assert (!need_ssa_update_p ());
    }
  cfun->gimple_df->aliases_computed_p = false;
  delete_mem_ref_stats (cfun);

  cfun->gimple_df = NULL;
}

/* Helper function for useless_type_conversion_p.  */

static bool
useless_type_conversion_p_1 (tree outer_type, tree inner_type)
{
  /* Qualifiers on value types do not matter.  */
  inner_type = TYPE_MAIN_VARIANT (inner_type);
  outer_type = TYPE_MAIN_VARIANT (outer_type);

  if (inner_type == outer_type)
    return true;

  /* If we know the canonical types, compare them.  */
  if (TYPE_CANONICAL (inner_type)
      && TYPE_CANONICAL (inner_type) == TYPE_CANONICAL (outer_type))
    return true;

  /* Changes in machine mode are never useless conversions.  */
  if (TYPE_MODE (inner_type) != TYPE_MODE (outer_type))
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

      /* Conversions from a non-base to a base type are not useless.
	 This way we preserve the invariant to do arithmetic in
	 base types only.  */
      if (TREE_TYPE (inner_type)
	  && TREE_TYPE (inner_type) != inner_type
	  && (TREE_TYPE (outer_type) == outer_type
	      || TREE_TYPE (outer_type) == NULL_TREE))
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

  /* We need to take special care recursing to pointed-to types.  */
  else if (POINTER_TYPE_P (inner_type)
	   && POINTER_TYPE_P (outer_type))
    {
      /* Don't lose casts between pointers to volatile and non-volatile
	 qualified types.  Doing so would result in changing the semantics
	 of later accesses.  */
      if ((TYPE_VOLATILE (TREE_TYPE (outer_type))
	   != TYPE_VOLATILE (TREE_TYPE (inner_type)))
	  && TYPE_VOLATILE (TREE_TYPE (outer_type)))
	return false;

      /* Do not lose casts between pointers with different
	 TYPE_REF_CAN_ALIAS_ALL setting or alias sets.  */
      if ((TYPE_REF_CAN_ALIAS_ALL (inner_type)
	   != TYPE_REF_CAN_ALIAS_ALL (outer_type))
	  || (get_alias_set (TREE_TYPE (inner_type))
	      != get_alias_set (TREE_TYPE (outer_type))))
	return false;

      /* Do not lose casts from const qualified to non-const
	 qualified.  */
      if ((TYPE_READONLY (TREE_TYPE (outer_type))
	   != TYPE_READONLY (TREE_TYPE (inner_type)))
	  && TYPE_READONLY (TREE_TYPE (inner_type)))
	return false;

      /* Do not lose casts to restrict qualified pointers.  */
      if ((TYPE_RESTRICT (outer_type)
	   != TYPE_RESTRICT (inner_type))
	  && TYPE_RESTRICT (outer_type))
	return false;

      /* Otherwise pointers/references are equivalent if their pointed
	 to types are effectively the same.  We can strip qualifiers
	 on pointed-to types for further comparison, which is done in
	 the callee.  */
      return useless_type_conversion_p_1 (TREE_TYPE (outer_type),
				          TREE_TYPE (inner_type));
    }

  /* Recurse for complex types.  */
  else if (TREE_CODE (inner_type) == COMPLEX_TYPE
	   && TREE_CODE (outer_type) == COMPLEX_TYPE)
    return useless_type_conversion_p_1 (TREE_TYPE (outer_type),
				        TREE_TYPE (inner_type));

  /* Recurse for vector types with the same number of subparts.  */
  else if (TREE_CODE (inner_type) == VECTOR_TYPE
	   && TREE_CODE (outer_type) == VECTOR_TYPE
	   && TYPE_PRECISION (inner_type) == TYPE_PRECISION (outer_type))
    return useless_type_conversion_p_1 (TREE_TYPE (outer_type),
				        TREE_TYPE (inner_type));

  /* For aggregates we may need to fall back to structural equality
     checks.  */
  else if (AGGREGATE_TYPE_P (inner_type)
	   && AGGREGATE_TYPE_P (outer_type))
    {
      /* Different types of aggregates are incompatible.  */
      if (TREE_CODE (inner_type) != TREE_CODE (outer_type))
	return false;

      /* ???  This seems to be necessary even for aggregates that don't
	 have TYPE_STRUCTURAL_EQUALITY_P set.  */

      /* ???  This should eventually just return false.  */
      return lang_hooks.types_compatible_p (inner_type, outer_type);
    }
  /* Also for functions and possibly other types with
     TYPE_STRUCTURAL_EQUALITY_P set.  */
  else if (TYPE_STRUCTURAL_EQUALITY_P (inner_type)
	   && TYPE_STRUCTURAL_EQUALITY_P (outer_type))
    return lang_hooks.types_compatible_p (inner_type, outer_type);
  
  return false;
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
  /* If the outer type is (void *), then the conversion is not
     necessary.  We have to make sure to not apply this while
     recursing though.  */
  if (POINTER_TYPE_P (inner_type)
      && POINTER_TYPE_P (outer_type)
      && TREE_CODE (TREE_TYPE (outer_type)) == VOID_TYPE)
    return true;

  return useless_type_conversion_p_1 (outer_type, inner_type);
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
  if (TREE_CODE (expr) == NOP_EXPR || TREE_CODE (expr) == CONVERT_EXPR
      || TREE_CODE (expr) == VIEW_CONVERT_EXPR
      || TREE_CODE (expr) == NON_LVALUE_EXPR)
    /* FIXME: Use of GENERIC_TREE_TYPE here is a temporary measure to work
       around known bugs with GIMPLE_MODIFY_STMTs appearing in places
       they shouldn't.  See PR 30391.  */
    return useless_type_conversion_p
      (TREE_TYPE (expr),
       GENERIC_TREE_TYPE (TREE_OPERAND (expr, 0)));

  return false;
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
  tree def_stmt;

  if (pointer_set_insert (visited, var))
    return false;

  def_stmt = SSA_NAME_DEF_STMT (var);

  if (TREE_CODE (def_stmt) != PHI_NODE)
    {
      /* If we reached the end of the use-def chain, call FN.  */
      return fn (var, def_stmt, data);
    }
  else
    {
      int i;

      /* When doing a breadth-first search, call FN before following the
	 use-def links for each argument.  */
      if (!is_dfs)
	for (i = 0; i < PHI_NUM_ARGS (def_stmt); i++)
	  if (fn (PHI_ARG_DEF (def_stmt, i), def_stmt, data))
	    return true;

      /* Follow use-def links out of each PHI argument.  */
      for (i = 0; i < PHI_NUM_ARGS (def_stmt); i++)
	{
	  tree arg = PHI_ARG_DEF (def_stmt, i);

	  /* ARG may be NULL for newly introduced PHI nodes.  */
	  if (arg
	      && TREE_CODE (arg) == SSA_NAME
	      && walk_use_def_chains_1 (arg, fn, data, visited, is_dfs))
	    return true;
	}

      /* When doing a depth-first search, call FN after following the
	 use-def links for each argument.  */
      if (is_dfs)
	for (i = 0; i < PHI_NUM_ARGS (def_stmt); i++)
	  if (fn (PHI_ARG_DEF (def_stmt, i), def_stmt, data))
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
  tree def_stmt;

  gcc_assert (TREE_CODE (var) == SSA_NAME);

  def_stmt = SSA_NAME_DEF_STMT (var);

  /* We only need to recurse if the reaching definition comes from a PHI
     node.  */
  if (TREE_CODE (def_stmt) != PHI_NODE)
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
  return IS_EMPTY_STMT (SSA_NAME_DEF_STMT (t));
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
  tree context = (tree) data;
  location_t *locus;
  expanded_location xloc, floc;

  if (!ssa_undefined_value_p (t))
    return;

  /* TREE_NO_WARNING either means we already warned, or the front end
     wishes to suppress the warning.  */
  if (TREE_NO_WARNING (var))
    return;

  locus = (context != NULL && EXPR_HAS_LOCATION (context)
	   ? EXPR_LOCUS (context)
	   : &DECL_SOURCE_LOCATION (var));
  warning (OPT_Wuninitialized, gmsgid, locus, var);
  xloc = expand_location (*locus);
  floc = expand_location (DECL_SOURCE_LOCATION (cfun->decl));
  if (xloc.file != floc.file
      || xloc.line < floc.line
      || xloc.line > LOCATION_LINE (cfun->function_end_locus))
    inform ("%J%qD was declared here", var, var);

  TREE_NO_WARNING (var) = 1;
}

struct walk_data {
  tree stmt;
  bool always_executed;
};

/* Called via walk_tree, look for SSA_NAMEs that have empty definitions
   and warn about them.  */

static tree
warn_uninitialized_var (tree *tp, int *walk_subtrees, void *data_)
{
  struct walk_data *data = (struct walk_data *)data_;
  tree t = *tp;

  switch (TREE_CODE (t))
    {
    case SSA_NAME:
      /* We only do data flow with SSA_NAMEs, so that's all we
	 can warn about.  */
      if (data->always_executed)
        warn_uninit (t, "%H%qD is used uninitialized in this function",
		     data->stmt);
      else
        warn_uninit (t, "%H%qD may be used uninitialized in this function",
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
warn_uninitialized_phi (tree phi)
{
  int i, n = PHI_NUM_ARGS (phi);

  /* Don't look at memory tags.  */
  if (!is_gimple_reg (PHI_RESULT (phi)))
    return;

  for (i = 0; i < n; ++i)
    {
      tree op = PHI_ARG_DEF (phi, i);
      if (TREE_CODE (op) == SSA_NAME)
	warn_uninit (op, "%H%qD may be used uninitialized in this function",
		     NULL);
    }
}

static unsigned int
execute_early_warn_uninitialized (void)
{
  block_stmt_iterator bsi;
  basic_block bb;
  struct walk_data data;

  calculate_dominance_info (CDI_POST_DOMINATORS);

  FOR_EACH_BB (bb)
    {
      data.always_executed = dominated_by_p (CDI_POST_DOMINATORS,
					     single_succ (ENTRY_BLOCK_PTR), bb);
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  data.stmt = bsi_stmt (bsi);
	  walk_tree (bsi_stmt_ptr (bsi), warn_uninitialized_var,
		     &data, NULL);
        }
    }
  return 0;
}

static unsigned int
execute_late_warn_uninitialized (void)
{
  basic_block bb;
  tree phi;

  /* Re-do the plain uninitialized variable check, as optimization may have
     straightened control flow.  Do this first so that we don't accidentally
     get a "may be" warning when we'd have seen an "is" warning later.  */
  execute_early_warn_uninitialized ();

  FOR_EACH_BB (bb)
    for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
      warn_uninitialized_phi (phi);
  return 0;
}

static bool
gate_warn_uninitialized (void)
{
  return warn_uninitialized != 0;
}

struct tree_opt_pass pass_early_warn_uninitialized =
{
  NULL,					/* name */
  gate_warn_uninitialized,		/* gate */
  execute_early_warn_uninitialized,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,                                    /* todo_flags_finish */
  0				        /* letter */
};

struct tree_opt_pass pass_late_warn_uninitialized =
{
  NULL,					/* name */
  gate_warn_uninitialized,		/* gate */
  execute_late_warn_uninitialized,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,                                    /* todo_flags_finish */
  0				        /* letter */
};

/* Compute TREE_ADDRESSABLE for local variables.  */

static unsigned int
execute_update_addresses_taken (void)
{
  tree var;
  referenced_var_iterator rvi;
  block_stmt_iterator bsi;
  basic_block bb;
  bitmap addresses_taken = BITMAP_ALLOC (NULL);
  bitmap vars_updated = BITMAP_ALLOC (NULL);
  bool update_vops = false;
  tree phi;

  /* Collect into ADDRESSES_TAKEN all variables whose address is taken within
     the function body.  */
  FOR_EACH_BB (bb)
    {
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  stmt_ann_t s_ann = stmt_ann (bsi_stmt (bsi));

	  if (s_ann->addresses_taken)
	    bitmap_ior_into (addresses_taken, s_ann->addresses_taken);
	}
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  unsigned i, phi_num_args = PHI_NUM_ARGS (phi);
	  for (i = 0; i < phi_num_args; i++)
	    {
	      tree op = PHI_ARG_DEF (phi, i), var;
	      if (TREE_CODE (op) == ADDR_EXPR
		  && (var = get_base_address (TREE_OPERAND (op, 0))) != NULL_TREE
		  && DECL_P (var))
		bitmap_set_bit (addresses_taken, DECL_UID (var));
	    }
	}
    }

  /* When possible, clear ADDRESSABLE bit and mark variable for conversion into
     SSA.  */
  FOR_EACH_REFERENCED_VAR (var, rvi)
    if (!is_global_var (var)
	&& TREE_CODE (var) != RESULT_DECL
	&& TREE_ADDRESSABLE (var)
	&& !bitmap_bit_p (addresses_taken, DECL_UID (var)))
      {
        TREE_ADDRESSABLE (var) = 0;
	if (is_gimple_reg (var))
	  mark_sym_for_renaming (var);
	update_vops = true;
	bitmap_set_bit (vars_updated, DECL_UID (var));
	if (dump_file)
	  {
	    fprintf (dump_file, "No longer having address taken ");
	    print_generic_expr (dump_file, var, 0);
	    fprintf (dump_file, "\n");
	  }
      }

  /* Operand caches needs to be recomputed for operands referencing the updated
     variables.  */
  if (update_vops)
    FOR_EACH_BB (bb)
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);

	  if ((LOADED_SYMS (stmt)
	       && bitmap_intersect_p (LOADED_SYMS (stmt), vars_updated))
	      || (STORED_SYMS (stmt)
		  && bitmap_intersect_p (STORED_SYMS (stmt), vars_updated)))
	    update_stmt (stmt);
	}
  BITMAP_FREE (addresses_taken);
  BITMAP_FREE (vars_updated);
  return 0;
}

struct tree_opt_pass pass_update_address_taken =
{
  "addressables",			/* name */
  NULL,					/* gate */
  execute_update_addresses_taken,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_ssa,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa,                      /* todo_flags_finish */
  0				        /* letter */
};
