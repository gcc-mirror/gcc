/* Miscellaneous SSA utility functions.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
#include "errors.h"
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

/* Remove the corresponding arguments from the PHI nodes in E's
   destination block and redirect it to DEST.  Return redirected edge.
   The list of removed arguments is stored in PENDING_STMT (e).  */

edge
ssa_redirect_edge (edge e, basic_block dest)
{
  tree phi, next;
  tree list = NULL, *last = &list;
  tree src, dst, node;

  /* Remove the appropriate PHI arguments in E's destination block.  */
  for (phi = phi_nodes (e->dest); phi; phi = next)
    {
      next = PHI_CHAIN (phi);

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

/* Add PHI arguments queued in PENDINT_STMT list on edge E to edge
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
      error ("Expected an SSA_NAME object");
      return true;
    }

  if (TREE_TYPE (ssa_name) != TREE_TYPE (SSA_NAME_VAR (ssa_name)))
    {
      error ("Type mismatch between an SSA_NAME and its symbol.");
      return true;
    }

  if (SSA_NAME_IN_FREE_LIST (ssa_name))
    {
      error ("Found an SSA_NAME that had been released into the free pool");
      return true;
    }

  if (is_virtual && is_gimple_reg (ssa_name))
    {
      error ("Found a virtual definition for a GIMPLE register");
      return true;
    }

  if (!is_virtual && !is_gimple_reg (ssa_name))
    {
      error ("Found a real definition for a non-register");
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

   IS_VIRTUAL is true if SSA_NAME is created by a V_MAY_DEF or a
      V_MUST_DEF.  */

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

   IS_VIRTUAL is true if SSA_NAME is created by a V_MAY_DEF or a
      V_MUST_DEF.
   
   If NAMES_DEFINED_IN_BB is not NULL, it contains a bitmap of ssa names
     that are defined before STMT in basic block BB.  */

static bool
verify_use (basic_block bb, basic_block def_bb, tree ssa_name,
	    tree stmt, bool check_abnormal, bool is_virtual,
	    bitmap names_defined_in_bb)
{
  bool err = false;

  err = verify_ssa_name (ssa_name, is_virtual);
  TREE_VISITED (ssa_name) = 1;

  if (IS_EMPTY_STMT (SSA_NAME_DEF_STMT (ssa_name))
      && var_ann (SSA_NAME_VAR (ssa_name))->default_def == ssa_name)
    ; /* Default definitions have empty statements.  Nothing to do.  */
  else if (!def_bb)
    {
      error ("Missing definition");
      err = true;
    }
  else if (bb != def_bb
	   && !dominated_by_p (CDI_DOMINATORS, bb, def_bb))
    {
      error ("Definition in block %i does not dominate use in block %i",
	     def_bb->index, bb->index);
      err = true;
    }
  else if (bb == def_bb
	   && names_defined_in_bb != NULL
	   && !bitmap_bit_p (names_defined_in_bb, SSA_NAME_VERSION (ssa_name)))
    {
      error ("Definition in block %i follows the use", def_bb->index);
      err = true;
    }

  if (check_abnormal
      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name))
    {
      error ("SSA_NAME_OCCURS_IN_ABNORMAL_PHI should be set");
      err = true;
    }

  if (err)
    {
      fprintf (stderr, "for SSA_NAME: ");
      print_generic_expr (stderr, ssa_name, TDF_VOPS);
      fprintf (stderr, "in statement:\n");
      print_generic_stmt (stderr, stmt, TDF_VOPS);
    }

  return err;
}


/* Return true if any of the arguments for PHI node PHI at block BB is
   malformed.

   DEFINITION_BLOCK is an array of basic blocks indexed by SSA_NAME version
      numbers.  If DEFINITION_BLOCK[SSA_NAME_VERSION] is set, it means that the
      block in that array slot contains the definition of SSA_NAME.  */

static bool
verify_phi_args (tree phi, basic_block bb, basic_block *definition_block)
{
  edge e;
  bool err = false;
  unsigned i, phi_num_args = PHI_NUM_ARGS (phi);

  if (EDGE_COUNT (bb->preds) != phi_num_args)
    {
      error ("Incoming edge count does not match number of PHI arguments\n");
      err = true;
      goto error;
    }

  for (i = 0; i < phi_num_args; i++)
    {
      tree op = PHI_ARG_DEF (phi, i);

      e = EDGE_PRED (bb, i);

      if (op == NULL_TREE)
	{
	  error ("PHI argument is missing for edge %d->%d\n",
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
	err = verify_use (e->src, definition_block[SSA_NAME_VERSION (op)], op,
			  phi, e->flags & EDGE_ABNORMAL,
			  !is_gimple_reg (PHI_RESULT (phi)),
			  NULL);

      if (e->dest != bb)
	{
	  error ("Wrong edge %d->%d for PHI argument\n",
	         e->src->index, e->dest->index, bb->index);
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
      print_generic_stmt (stderr, phi, TDF_VOPS);
    }


  return err;
}


static void
verify_flow_insensitive_alias_info (void)
{
  size_t i;
  tree var;
  bitmap visited = BITMAP_ALLOC (NULL);

  for (i = 0; i < num_referenced_vars; i++)
    {
      size_t j;
      var_ann_t ann;
      varray_type may_aliases;

      var = referenced_var (i);
      ann = var_ann (var);
      may_aliases = ann->may_aliases;

      for (j = 0; may_aliases && j < VARRAY_ACTIVE_SIZE (may_aliases); j++)
	{
	  tree alias = VARRAY_TREE (may_aliases, j);

	  bitmap_set_bit (visited, var_ann (alias)->uid);

	  if (!may_be_aliased (alias))
	    {
	      error ("Non-addressable variable inside an alias set.");
	      debug_variable (alias);
	      goto err;
	    }
	}
    }

  for (i = 0; i < num_referenced_vars; i++)
    {
      var_ann_t ann;

      var = referenced_var (i);
      ann = var_ann (var);

      if (ann->mem_tag_kind == NOT_A_TAG
	  && ann->is_alias_tag
	  && !bitmap_bit_p (visited, ann->uid))
	{
	  error ("Addressable variable that is an alias tag but is not in any alias set.");
	  goto err;
	}
    }

  BITMAP_FREE (visited);
  return;

err:
  debug_variable (var);
  internal_error ("verify_flow_insensitive_alias_info failed.");
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
      if (pi->is_dereferenced && !pi->name_mem_tag && !ann->type_mem_tag)
	{
	  error ("Dereferenced pointers should have a name or a type tag");
	  goto err;
	}

      if (pi->name_mem_tag
	  && !pi->pt_malloc
	  && (pi->pt_vars == NULL || bitmap_empty_p (pi->pt_vars)))
	{
	  error ("Pointers with a memory tag, should have points-to sets or point to malloc");
	  goto err;
	}

      if (pi->value_escapes_p
	  && pi->name_mem_tag
	  && !is_call_clobbered (pi->name_mem_tag))
	{
	  error ("Pointer escapes but its name tag is not call-clobbered.");
	  goto err;
	}
    }

  return;

err:
  debug_variable (ptr);
  internal_error ("verify_flow_sensitive_alias_info failed.");
}

DEF_VEC_MALLOC_P (bitmap);

/* Verify that all name tags have different points to sets.
   This algorithm takes advantage of the fact that every variable with the
   same name tag must have the same points-to set. 
   So we check a single variable for each name tag, and verify that its
   points-to set is different from every other points-to set for other name
   tags.

   Additionally, given a pointer P_i with name tag NMT and type tag
   TMT, this function verified the alias set of TMT is a superset of
   the alias set of NMT.  */

static void
verify_name_tags (void)
{
  size_t i;  
  size_t j;
  bitmap first, second;  
  VEC (tree) *name_tag_reps = NULL;
  VEC (bitmap) *pt_vars_for_reps = NULL;
  bitmap type_aliases = BITMAP_ALLOC (NULL);

  /* First we compute the name tag representatives and their points-to sets.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      struct ptr_info_def *pi;
      tree tmt, ptr = ssa_name (i);

      if (ptr == NULL_TREE)
	continue;
      
      pi = SSA_NAME_PTR_INFO (ptr);

      if (!TREE_VISITED (ptr) 
	  || !POINTER_TYPE_P (TREE_TYPE (ptr)) 
	  || !pi
	  || !pi->name_mem_tag 
	  || TREE_VISITED (pi->name_mem_tag))
	continue;

      TREE_VISITED (pi->name_mem_tag) = 1;

      if (pi->pt_vars == NULL)
	continue;

      VEC_safe_push (tree, name_tag_reps, ptr);
      VEC_safe_push (bitmap, pt_vars_for_reps, pi->pt_vars);

      /* Verify that alias set of PTR's type tag is a superset of the
	 alias set of PTR's name tag.  */
      tmt = var_ann (SSA_NAME_VAR (ptr))->type_mem_tag;
      if (tmt)
	{
	  size_t i;
	  varray_type aliases = var_ann (tmt)->may_aliases;
	  bitmap_clear (type_aliases);
	  for (i = 0; aliases && i < VARRAY_ACTIVE_SIZE (aliases); i++)
	    {
	      tree alias = VARRAY_TREE (aliases, i);
	      bitmap_set_bit (type_aliases, var_ann (alias)->uid);
	    }

	  /* When grouping, we may have added PTR's type tag into the
	     alias set of PTR's name tag.  To prevent a false
	     positive, pretend that TMT is in its own alias set.  */
	  bitmap_set_bit (type_aliases, var_ann (tmt)->uid);

	  if (bitmap_equal_p (type_aliases, pi->pt_vars))
	    continue;

	  if (!bitmap_intersect_compl_p (type_aliases, pi->pt_vars))
	    {
	      error ("Alias set of a pointer's type tag should be a superset of the corresponding name tag");
	      debug_variable (tmt);
	      debug_variable (pi->name_mem_tag);
	      goto err;
	    }
	}
    }
  
  /* Now compare all the representative bitmaps with all other representative
     bitmaps, to verify that they are all different.  */
  for (i = 0; VEC_iterate (bitmap, pt_vars_for_reps, i, first); i++)
    {
       for (j = i + 1; VEC_iterate (bitmap, pt_vars_for_reps, j, second); j++)
	 { 
	   if (bitmap_equal_p (first, second))
	     {
	       error ("Two different pointers with identical points-to sets but different name tags");
	       debug_variable (VEC_index (tree, name_tag_reps, j));
	       goto err;
	     }
	 }
    }

  /* Lastly, clear out the visited flags.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      if (ssa_name (i))
	{
	  tree ptr = ssa_name (i);
	  struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);
	  if (!TREE_VISITED (ptr) 
	      || !POINTER_TYPE_P (TREE_TYPE (ptr)) 
	      || !pi
	      || !pi->name_mem_tag)
	    continue;
	  TREE_VISITED (pi->name_mem_tag) = 0;
	}
    } 

  VEC_free (bitmap, pt_vars_for_reps);
  BITMAP_FREE (type_aliases);
  return;
  
err:
  debug_variable (VEC_index (tree, name_tag_reps, i));
  internal_error ("verify_name_tags failed");
}


/* Verify the consistency of aliasing information.  */

static void
verify_alias_info (void)
{
  verify_flow_sensitive_alias_info ();
  verify_name_tags ();
  verify_flow_insensitive_alias_info ();
}


/* Verify common invariants in the SSA web.
   TODO: verify the variable annotations.  */

void
verify_ssa (void)
{
  size_t i;
  basic_block bb;
  basic_block *definition_block = xcalloc (num_ssa_names, sizeof (basic_block));
  ssa_op_iter iter;
  tree op;
  enum dom_state orig_dom_state = dom_computed[CDI_DOMINATORS];
  bitmap names_defined_in_bb = BITMAP_ALLOC (NULL);

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
	      error ("AUX pointer initialized for edge %d->%d\n", e->src->index,
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

	      get_stmt_operands (stmt);

	      if (stmt_ann (stmt)->makes_aliased_stores 
		  && NUM_V_MAY_DEFS (STMT_V_MAY_DEF_OPS (stmt)) == 0)
		{
		  error ("Statement makes aliased stores, but has no V_MAY_DEFS");
		  print_generic_stmt (stderr, stmt, TDF_VOPS);
		  goto err;
		}

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_ALL_USES | SSA_OP_ALL_KILLS)
	    {
	      if (verify_use (bb, definition_block[SSA_NAME_VERSION (op)],
			      op, stmt, false, !is_gimple_reg (op),
			      names_defined_in_bb))
		goto err;
	    }

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_ALL_DEFS)
	    {
	      bitmap_set_bit (names_defined_in_bb, SSA_NAME_VERSION (op));
	    }
	}

      bitmap_clear (names_defined_in_bb);
    }

  /* Finally, verify alias information.  */
  verify_alias_info ();

  free (definition_block);
  /* Restore the dominance information to its prior known state, so
     that we do not perturb the compiler's subsequent behavior.  */
  if (orig_dom_state == DOM_NONE)
    free_dominance_info (CDI_DOMINATORS);
  else
    dom_computed[CDI_DOMINATORS] = orig_dom_state;
  
  BITMAP_FREE (names_defined_in_bb);
  timevar_pop (TV_TREE_SSA_VERIFY);
  return;

err:
  internal_error ("verify_ssa failed.");
}


/* Initialize global DFA and SSA structures.  */

void
init_tree_ssa (void)
{
  VARRAY_TREE_INIT (referenced_vars, 20, "referenced_vars");
  call_clobbered_vars = BITMAP_ALLOC (NULL);
  addressable_vars = BITMAP_ALLOC (NULL);
  init_ssa_operands ();
  init_ssanames ();
  init_phinodes ();
  global_var = NULL_TREE;
  aliases_computed_p = false;
}


/* Deallocate memory associated with SSA data structures for FNDECL.  */

void
delete_tree_ssa (void)
{
  size_t i;
  basic_block bb;
  block_stmt_iterator bsi;

  /* Remove annotations from every tree in the function.  */
  FOR_EACH_BB (bb)
    for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
      {
	tree stmt = bsi_stmt (bsi);
        release_defs (stmt);
	ggc_free (stmt->common.ann);
	stmt->common.ann = NULL;
      }

  /* Remove annotations from every referenced variable.  */
  if (referenced_vars)
    {
      for (i = 0; i < num_referenced_vars; i++)
	{
	  tree var = referenced_var (i);
	  ggc_free (var->common.ann);
	  var->common.ann = NULL;
	}
      referenced_vars = NULL;
    }

  fini_ssanames ();
  fini_phinodes ();
  fini_ssa_operands ();

  global_var = NULL_TREE;
  BITMAP_FREE (call_clobbered_vars);
  call_clobbered_vars = NULL;
  BITMAP_FREE (addressable_vars);
  addressable_vars = NULL;
  modified_noreturn_calls = NULL;
  aliases_computed_p = false;
}


/* Return true if EXPR is a useless type conversion, otherwise return
   false.  */

bool
tree_ssa_useless_type_conversion_1 (tree outer_type, tree inner_type)
{
  if (inner_type == outer_type)
    return true;

  /* Changes in machine mode are never useless conversions.  */
  if (TYPE_MODE (inner_type) != TYPE_MODE (outer_type))
    return false;

  /* If the inner and outer types are effectively the same, then
     strip the type conversion and enter the equivalence into
     the table.  */
  if (lang_hooks.types_compatible_p (inner_type, outer_type))
    return true;

  /* If both types are pointers and the outer type is a (void *), then
     the conversion is not necessary.  The opposite is not true since
     that conversion would result in a loss of information if the
     equivalence was used.  Consider an indirect function call where
     we need to know the exact type of the function to correctly
     implement the ABI.  */
  else if (POINTER_TYPE_P (inner_type)
           && POINTER_TYPE_P (outer_type)
	   && TYPE_REF_CAN_ALIAS_ALL (inner_type)
	      == TYPE_REF_CAN_ALIAS_ALL (outer_type)
	   && TREE_CODE (TREE_TYPE (outer_type)) == VOID_TYPE)
    return true;

  /* Pointers and references are equivalent once we get to GENERIC,
     so strip conversions that just switch between them.  */
  else if (POINTER_TYPE_P (inner_type)
           && POINTER_TYPE_P (outer_type)
	   && TYPE_REF_CAN_ALIAS_ALL (inner_type)
	      == TYPE_REF_CAN_ALIAS_ALL (outer_type)
           && lang_hooks.types_compatible_p (TREE_TYPE (inner_type),
					     TREE_TYPE (outer_type)))
    return true;

  /* If both the inner and outer types are integral types, then the
     conversion is not necessary if they have the same mode and
     signedness and precision, and both or neither are boolean.  Some
     code assumes an invariant that boolean types stay boolean and do
     not become 1-bit bit-field types.  Note that types with precision
     not using all bits of the mode (such as bit-field types in C)
     mean that testing of precision is necessary.  */
  else if (INTEGRAL_TYPE_P (inner_type)
           && INTEGRAL_TYPE_P (outer_type)
	   && TYPE_UNSIGNED (inner_type) == TYPE_UNSIGNED (outer_type)
	   && TYPE_PRECISION (inner_type) == TYPE_PRECISION (outer_type))
    {
      bool first_boolean = (TREE_CODE (inner_type) == BOOLEAN_TYPE);
      bool second_boolean = (TREE_CODE (outer_type) == BOOLEAN_TYPE);
      if (first_boolean == second_boolean)
	return true;
    }

  /* Recurse for complex types.  */
  else if (TREE_CODE (inner_type) == COMPLEX_TYPE
	   && TREE_CODE (outer_type) == COMPLEX_TYPE
	   && tree_ssa_useless_type_conversion_1 (TREE_TYPE (outer_type),
						  TREE_TYPE (inner_type)))
    return true;

  return false;
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
    return tree_ssa_useless_type_conversion_1 (TREE_TYPE (expr),
					       TREE_TYPE (TREE_OPERAND (expr,
									0)));


  return false;
}

/* Returns true if statement STMT may read memory.  */

bool
stmt_references_memory_p (tree stmt)
{
  stmt_ann_t ann;

  get_stmt_operands (stmt);
  ann = stmt_ann (stmt);

  if (ann->has_volatile_ops)
    return true;

  return (NUM_VUSES (VUSE_OPS (ann)) > 0
	  || NUM_V_MAY_DEFS (V_MAY_DEF_OPS (ann)) > 0
	  || NUM_V_MUST_DEFS (V_MUST_DEF_OPS (ann)) > 0);
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
	  if (TREE_CODE (arg) == SSA_NAME
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


/* Replaces VAR with REPL in memory reference expression *X in
   statement STMT.  */

static void
propagate_into_addr (tree stmt, tree var, tree *x, tree repl)
{
  tree new_var, ass_stmt, addr_var;
  basic_block bb;
  block_stmt_iterator bsi;

  /* There is nothing special to handle in the other cases.  */
  if (TREE_CODE (repl) != ADDR_EXPR)
    return;
  addr_var = TREE_OPERAND (repl, 0);

  while (handled_component_p (*x)
	 || TREE_CODE (*x) == REALPART_EXPR
	 || TREE_CODE (*x) == IMAGPART_EXPR)
    x = &TREE_OPERAND (*x, 0);

  if (TREE_CODE (*x) != INDIRECT_REF
      || TREE_OPERAND (*x, 0) != var)
    return;

  if (TREE_TYPE (*x) == TREE_TYPE (addr_var))
    {
      *x = addr_var;
      mark_new_vars_to_rename (stmt, vars_to_rename);
      return;
    }


  /* Frontends sometimes produce expressions like *&a instead of a[0].
     Create a temporary variable to handle this case.  */
  ass_stmt = build2 (MODIFY_EXPR, void_type_node, NULL_TREE, repl);
  new_var = duplicate_ssa_name (var, ass_stmt);
  TREE_OPERAND (*x, 0) = new_var;
  TREE_OPERAND (ass_stmt, 0) = new_var;

  bb = bb_for_stmt (stmt);
  tree_block_label (bb);
  bsi = bsi_after_labels (bb);
  bsi_insert_after (&bsi, ass_stmt, BSI_NEW_STMT);

  mark_new_vars_to_rename (stmt, vars_to_rename);
}

/* Replaces immediate uses of VAR by REPL.  */

static void
replace_immediate_uses (tree var, tree repl)
{
  int i, j, n;
  dataflow_t df;
  tree stmt;
  bool mark_new_vars;
  ssa_op_iter iter;
  use_operand_p use_p;

  df = get_immediate_uses (SSA_NAME_DEF_STMT (var));
  n = num_immediate_uses (df);

  for (i = 0; i < n; i++)
    {
      stmt = immediate_use (df, i);

      if (TREE_CODE (stmt) == PHI_NODE)
	{
	  for (j = 0; j < PHI_NUM_ARGS (stmt); j++)
	    if (PHI_ARG_DEF (stmt, j) == var)
	      {
		SET_PHI_ARG_DEF (stmt, j, repl);
		if (TREE_CODE (repl) == SSA_NAME
		    && PHI_ARG_EDGE (stmt, j)->flags & EDGE_ABNORMAL)
		  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (repl) = 1;
	      }

	  continue;
	}

      get_stmt_operands (stmt);
      mark_new_vars = false;
      if (is_gimple_reg (SSA_NAME_VAR (var)))
	{
	  if (TREE_CODE (stmt) == MODIFY_EXPR)
	    {
	      propagate_into_addr (stmt, var, &TREE_OPERAND (stmt, 0), repl);
	      propagate_into_addr (stmt, var, &TREE_OPERAND (stmt, 1), repl);
	    }

	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	    if (USE_FROM_PTR (use_p) == var)
	      {
		propagate_value (use_p, repl);
		mark_new_vars = POINTER_TYPE_P (TREE_TYPE (repl));
	      }
	}
      else
	{
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, 
				    SSA_OP_VIRTUAL_USES | SSA_OP_VIRTUAL_KILLS)
	    if (USE_FROM_PTR (use_p) == var)
	      propagate_value (use_p, repl);
	}

      /* FIXME.  If REPL is a constant, we need to fold STMT.
	 However, fold_stmt wants a pointer to the statement, because
	 it may happen that it needs to replace the whole statement
	 with a new expression.  Since the current def-use machinery
	 does not return pointers to statements, we call fold_stmt
	 with the address of a local temporary, if that call changes
	 the temporary then we fallback on looking for a proper
	 pointer to STMT by scanning STMT's basic block.

	 Note that all this will become unnecessary soon.  This
	 pass is being replaced with a proper copy propagation pass
	 for 4.1 (dnovillo, 2004-09-17).  */
      if (TREE_CODE (repl) != SSA_NAME)
	{
	  tree tmp = stmt;
	  fold_stmt (&tmp);
          mark_new_vars = true;
	  if (tmp != stmt)
	    {
	      block_stmt_iterator si = bsi_for_stmt (stmt);
	      mark_new_vars_to_rename (tmp, vars_to_rename);
	      redirect_immediate_uses (stmt, tmp);
	      bsi_replace (&si, tmp, true);
	      stmt = bsi_stmt (si);
	    }
	}

      /* If REPL is a pointer, it may have different memory tags associated
	 with it.  For instance, VAR may have had a name tag while REPL
	 only had a type tag.  In these cases, the virtual operands (if
	 any) in the statement will refer to different symbols which need
	 to be renamed.  */
      if (mark_new_vars)
	mark_new_vars_to_rename (stmt, vars_to_rename);
      else
	modify_stmt (stmt);
    }
}

/* Gets the value VAR is equivalent to according to EQ_TO.  */

static tree
get_eq_name (tree *eq_to, tree var)
{
  unsigned ver;
  tree val = var;

  while (TREE_CODE (val) == SSA_NAME)
    {
      ver = SSA_NAME_VERSION (val);
      if (!eq_to[ver])
	break;

      val = eq_to[ver];
    }

  while (TREE_CODE (var) == SSA_NAME)
    {
      ver = SSA_NAME_VERSION (var);
      if (!eq_to[ver])
	break;

      var = eq_to[ver];
      eq_to[ver] = val;
    }

  return val;
}

/* Checks whether phi node PHI is redundant and if it is, records the ssa name
   its result is redundant to to EQ_TO array.  */

static void
check_phi_redundancy (tree phi, tree *eq_to)
{
  tree val = NULL_TREE, def, res = PHI_RESULT (phi), stmt;
  unsigned i, ver = SSA_NAME_VERSION (res), n;
  dataflow_t df;

  /* It is unlikely that such large phi node would be redundant.  */
  if (PHI_NUM_ARGS (phi) > 16)
    return;

  for (i = 0; i < (unsigned) PHI_NUM_ARGS (phi); i++)
    {
      def = PHI_ARG_DEF (phi, i);

      if (TREE_CODE (def) == SSA_NAME)
	{
	  def = get_eq_name (eq_to, def);
	  if (def == res)
	    continue;
	}

      if (val
	  && !operand_equal_for_phi_arg_p (val, def))
	return;

      val = def;
    }

  /* At least one of the arguments should not be equal to the result, or
     something strange is happening.  */
  gcc_assert (val);

  if (get_eq_name (eq_to, res) == val)
    return;

  if (!may_propagate_copy (res, val))
    return;

  eq_to[ver] = val;

  df = get_immediate_uses (SSA_NAME_DEF_STMT (res));
  n = num_immediate_uses (df);

  for (i = 0; i < n; i++)
    {
      stmt = immediate_use (df, i);

      if (TREE_CODE (stmt) == PHI_NODE)
	check_phi_redundancy (stmt, eq_to);
    }
}

/* Removes redundant phi nodes.

   A redundant PHI node is a PHI node where all of its PHI arguments
   are the same value, excluding any PHI arguments which are the same
   as the PHI result.

   A redundant PHI node is effectively a copy, so we forward copy propagate
   which removes all uses of the destination of the PHI node then
   finally we delete the redundant PHI node.

   Note that if we can not copy propagate the PHI node, then the PHI
   will not be removed.  Thus we do not have to worry about dependencies
   between PHIs and the problems serializing PHIs into copies creates. 
   
   The most important effect of this pass is to remove degenerate PHI
   nodes created by removing unreachable code.  */

void
kill_redundant_phi_nodes (void)
{
  tree *eq_to;
  unsigned i, old_num_ssa_names;
  basic_block bb;
  tree phi, var, repl, stmt;

  /* The EQ_TO[VER] holds the value by that the ssa name VER should be
     replaced.  If EQ_TO[VER] is ssa name and it is decided to replace it by
     other value, it may be necessary to follow the chain till the final value.
     We perform path shortening (replacing the entries of the EQ_TO array with
     heads of these chains) whenever we access the field to prevent quadratic
     complexity (probably would not occur in practice anyway, but let us play
     it safe).  */
  eq_to = xcalloc (num_ssa_names, sizeof (tree));

  /* We have had cases where computing immediate uses takes a
     significant amount of compile time.  If we run into such
     problems here, we may want to only compute immediate uses for
     a subset of all the SSA_NAMEs instead of computing it for
     all of the SSA_NAMEs.  */
  compute_immediate_uses (TDFA_USE_OPS | TDFA_USE_VOPS, NULL);
  old_num_ssa_names = num_ssa_names;

  FOR_EACH_BB (bb)
    {
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  var = PHI_RESULT (phi);
	  check_phi_redundancy (phi, eq_to);
	}
    }

  /* Now propagate the values.  */
  for (i = 0; i < old_num_ssa_names; i++)
    {
      if (!ssa_name (i))
	continue;

      repl = get_eq_name (eq_to, ssa_name (i));
      if (repl != ssa_name (i))
	replace_immediate_uses (ssa_name (i), repl);
    }

  /* And remove the dead phis.  */
  for (i = 0; i < old_num_ssa_names; i++)
    {
      if (!ssa_name (i))
	continue;

      repl = get_eq_name (eq_to, ssa_name (i));
      if (repl != ssa_name (i))
	{
	  stmt = SSA_NAME_DEF_STMT (ssa_name (i));
	  remove_phi_node (stmt, NULL_TREE, bb_for_stmt (stmt));
	}
    }

  free_df ();
  free (eq_to);
}

struct tree_opt_pass pass_redundant_phi =
{
  "redphi",				/* name */
  NULL,					/* gate */
  kill_redundant_phi_nodes,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_REDPHI,			/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars 
    | TODO_ggc_collect | TODO_verify_ssa, /* todo_flags_finish */
  0					/* letter */
};

/* Emit warnings for uninitialized variables.  This is done in two passes.

   The first pass notices real uses of SSA names with default definitions.
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
warn_uninit (tree t, const char *msgid, location_t *locus)
{
  tree var = SSA_NAME_VAR (t);
  tree def = SSA_NAME_DEF_STMT (t);

  /* Default uses (indicated by an empty definition statement),
     are uninitialized.  */
  if (!IS_EMPTY_STMT (def))
    return;

  /* Except for PARMs of course, which are always initialized.  */
  if (TREE_CODE (var) == PARM_DECL)
    return;

  /* Hard register variables get their initial value from the ether.  */
  if (TREE_CODE (var) == VAR_DECL && DECL_HARD_REGISTER (var))
    return;

  /* TREE_NO_WARNING either means we already warned, or the front end
     wishes to suppress the warning.  */
  if (TREE_NO_WARNING (var))
    return;

  if (!locus)
    locus = &DECL_SOURCE_LOCATION (var);
  warning (msgid, locus, var);
  TREE_NO_WARNING (var) = 1;
}
   
/* Called via walk_tree, look for SSA_NAMEs that have empty definitions
   and warn about them.  */

static tree
warn_uninitialized_var (tree *tp, int *walk_subtrees, void *data)
{
  location_t *locus = data;
  tree t = *tp;

  /* We only do data flow with SSA_NAMEs, so that's all we can warn about.  */
  if (TREE_CODE (t) == SSA_NAME)
    {
      warn_uninit (t, "%H%qD is used uninitialized in this function", locus);
      *walk_subtrees = 0;
    }
  else if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;

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

static void
execute_early_warn_uninitialized (void)
{
  block_stmt_iterator bsi;
  basic_block bb;

  FOR_EACH_BB (bb)
    for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
      walk_tree (bsi_stmt_ptr (bsi), warn_uninitialized_var,
		 EXPR_LOCUS (bsi_stmt (bsi)), NULL);
}

static void
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
	  
