/* Pass computing data for optimizing stdarg functions.
   Copyright (C) 2004 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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
#include "function.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-pass.h"

/* A simple pass that attempts to optimize stdarg functions on architectures
   that need to save register arguments to stack on entry to stdarg functions.
   If the function doesn't use any va_start macros, no registers need to
   be saved.  If va_start macros are used, the va_list variables don't escape
   the function, it is only necessary to save registers that will be used
   in va_arg macros.  E.g. if va_arg is only used with integral types
   in the function, floating point registers don't need to be saved, etc.  */

struct stdarg_info
{
  bitmap va_list_vars;
  basic_block va_start_bb, bb;
  int compute_sizes, va_start_count;
};

/* Return true if basic block VA_ARG_BB is dominated by VA_START_BB and
   is executed at most as many times as VA_START_BB.  */

static bool
reachable_at_most_once (basic_block va_arg_bb, basic_block va_start_bb)
{
  edge *stack, e;
  edge_iterator ei;
  int sp;
  sbitmap visited;
  bool ret;

  if (va_arg_bb == va_start_bb)
    return true;

  if (! dominated_by_p (CDI_DOMINATORS, va_arg_bb, va_start_bb))
    return false;

  stack = xmalloc ((n_basic_blocks + 1) * sizeof (edge));
  sp = 0;

  visited = sbitmap_alloc (last_basic_block);
  sbitmap_zero (visited);
  ret = true;

  FOR_EACH_EDGE (e, ei, va_arg_bb->preds)
    stack[sp++] = e;

  while (sp)
    {
      basic_block src;

      --sp;
      e = stack[sp];
      src = e->src;

      if (e->flags & EDGE_COMPLEX)
	{
	  ret = false;
	  break;
	}

      if (src == va_start_bb)
	continue;

      /* va_arg_bb can be executed more times than va_start_bb.  */
      if (src == va_arg_bb)
	{
	  ret = false;
	  break;
	}

      gcc_assert (src != ENTRY_BLOCK_PTR);

      if (! TEST_BIT (visited, src->index))
	{
	  SET_BIT (visited, src->index);
	  FOR_EACH_EDGE (e, ei, src->preds)
	    stack[sp++] = e;
	}
    }

  free (stack);
  sbitmap_free (visited);
  return ret;
}


/* For statement COUNTER = RHS, if RHS is COUNTER + constant,
   return constant, otherwise return 0.  */

static unsigned HOST_WIDE_INT
va_list_counter_bump (tree counter, tree rhs)
{
  tree plus_stmt = SSA_NAME_DEF_STMT (rhs);
  tree rhs1, addend, load_stmt, counter1;

  if (TREE_CODE (plus_stmt) != MODIFY_EXPR
      || TREE_OPERAND (plus_stmt, 0) != rhs)
    return 0;

  rhs1 = TREE_OPERAND (plus_stmt, 1);

  if (TREE_CODE (rhs1) != PLUS_EXPR
      || TREE_CODE (TREE_OPERAND (rhs1, 0)) != SSA_NAME
      || TREE_CODE (TREE_OPERAND (rhs1, 1)) != INTEGER_CST
      || !host_integerp (TREE_OPERAND (rhs1, 1), 1))
    return 0;

  addend = TREE_OPERAND (rhs1, 0);
  load_stmt = SSA_NAME_DEF_STMT (addend);

  if (TREE_CODE (load_stmt) != MODIFY_EXPR
      || TREE_OPERAND (load_stmt, 0) != addend)
    return 0;

  counter1 = TREE_OPERAND (load_stmt, 1);
  if (TREE_CODE (counter) != TREE_CODE (counter1))
    return 0;

  if (TREE_CODE (counter) == COMPONENT_REF)
    {
      if (get_base_address (counter) != get_base_address (counter1)
	  || TREE_CODE (TREE_OPERAND (counter1, 1)) != FIELD_DECL
	  || TREE_OPERAND (counter, 1) != TREE_OPERAND (counter1, 1))
	return 0;
    }
  else
    return 0;

  return tree_low_cst (TREE_OPERAND (rhs1, 1), 1);
}


/* Called by walk_tree to look for references to va_list variables.  */

static tree
find_va_list_reference (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
			void *data)
{
  bitmap va_list_vars = (bitmap) data;
  tree var = *tp;

  if (TREE_CODE (var) == SSA_NAME)
    var = SSA_NAME_VAR (var);

  if (TREE_CODE (var) == VAR_DECL
      && bitmap_bit_p (va_list_vars, var_ann (var)->uid))
    return var;

  return NULL_TREE;
}


/* Helper function of va_list_counter_struct_op.  Compute
  cfun->va_list_{g,f}pr_size.  AP is a va_list GPR/FPR counter,
  if WRITE_P is true, seen in AP = VAR, otherwise seen in VAR = AP
  statement.  GPR_P is true if AP is a GPR counter, false if it is
  a FPR counter.  */

static void
va_list_counter_op (struct stdarg_info *si, tree ap, tree var, bool gpr_p,
		    bool write_p)
{
  unsigned HOST_WIDE_INT increment;

  if (si->compute_sizes < 0)
    {
      si->compute_sizes = 0;
      if (si->va_start_count == 1
	  && reachable_at_most_once (si->bb, si->va_start_bb))
	si->compute_sizes = 1;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "bb%d will %sbe executed at most once for each va_start "
		 "in bb%d\n", si->bb->index, si->compute_sizes ? "" : "not ",
		 si->va_start_bb->index);
    }

  if (write_p
      && si->compute_sizes
      && (increment = va_list_counter_bump (ap, var)) != 0)
    {
      if (gpr_p && cfun->va_list_gpr_size + increment < VA_LIST_MAX_GPR_SIZE)
	{
	  cfun->va_list_gpr_size += increment;
	  return;
	}

      if (!gpr_p && cfun->va_list_fpr_size + increment < VA_LIST_MAX_FPR_SIZE)
	{
	  cfun->va_list_fpr_size += increment;
	  return;
	}
    }

  if (write_p || !si->compute_sizes)
    {
      if (gpr_p)
	cfun->va_list_gpr_size = VA_LIST_MAX_GPR_SIZE;
      else
	cfun->va_list_fpr_size = VA_LIST_MAX_FPR_SIZE;
    }
}


/* If AP is a va_list GPR/FPR counter, compute cfun->va_list_{g,f}pr_size.
   If WRITE_P is true, AP has been seen in AP = VAR assignment, if WRITE_P
   is false, AP has been seen in VAR = AP assignment.
   Return true if the AP = VAR (resp. VAR = AP) statement is a recognized
   va_arg operation that doesn't cause the va_list variable to escape
   current function.  */

static bool
va_list_counter_struct_op (struct stdarg_info *si, tree ap, tree var,
			   bool write_p)
{
  tree base;

  if (TREE_CODE (ap) != COMPONENT_REF
      || TREE_CODE (TREE_OPERAND (ap, 1)) != FIELD_DECL)
    return false;

  if (TREE_CODE (var) != SSA_NAME
      || bitmap_bit_p (si->va_list_vars, var_ann (SSA_NAME_VAR (var))->uid))
    return false;

  base = get_base_address (ap);
  if (TREE_CODE (base) != VAR_DECL
      || !bitmap_bit_p (si->va_list_vars, var_ann (base)->uid))
    return false;

  if (TREE_OPERAND (ap, 1) == va_list_gpr_counter_field)
    va_list_counter_op (si, ap, var, true, write_p);
  else if (TREE_OPERAND (ap, 1) == va_list_fpr_counter_field)
    va_list_counter_op (si, ap, var, false, write_p);

  return true;
}


/* Return true if this optimization pass should be done.
   It makes only sense for stdarg functions.  */

static bool
gate_optimize_stdarg (void)
{
  /* This optimization is only for stdarg functions.  */
  return current_function_stdarg != 0;
}  


/* Entry point to the stdarg optimization pass.  */

static void
execute_optimize_stdarg (void)
{
  basic_block bb;
  bool va_list_escapes = false;
  struct stdarg_info si;
  const char *funcname = NULL;

  cfun->va_list_gpr_size = 0;
  cfun->va_list_fpr_size = 0;
  memset (&si, 0, sizeof (si));
  si.va_list_vars = BITMAP_XMALLOC ();

  if (dump_file)
    funcname = lang_hooks.decl_printable_name (current_function_decl, 2);

  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;

      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
	{
	  tree stmt = bsi_stmt (i);
	  tree call = get_call_expr_in (stmt), callee;
	  tree ap;

	  if (!call)
	    continue;

	  callee = get_callee_fndecl (call);
	  if (!callee
	      || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL
	      || DECL_FUNCTION_CODE (callee) != BUILT_IN_VA_START)
	    continue;

	  si.va_start_count++;
	  ap = TREE_VALUE (TREE_OPERAND (call, 1));
	  if (TREE_CODE (ap) != ADDR_EXPR
	      || TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (ap, 0)))
		 != TYPE_MAIN_VARIANT (va_list_type_node)
	      || TREE_CODE (TREE_OPERAND (ap, 0)) != VAR_DECL)
	    {
	      va_list_escapes = true;
	      break;
	    }

	  ap = TREE_OPERAND (ap, 0);	    
	  if (is_global_var (ap))
	    {
	      va_list_escapes = true;
	      break;
	    }

	  bitmap_set_bit (si.va_list_vars, var_ann (ap)->uid);

	  /* VA_START_BB will be only used if there is just one
	     va_start in the function.  */
	  si.va_start_bb = bb;
	}

      if (va_list_escapes)
	break;
    }

  /* If there were no va_start uses in the function, there is no need to
     save anything.  */
  if (si.va_start_count == 0)
    goto finish;

  /* If some va_list arguments weren't local, we can't optimize.  */
  if (va_list_escapes)
    goto finish;

  /* If the backend didn't tell us what the counter fields are, there is
     nothing more we can do.  */
  if (va_list_gpr_counter_field == NULL_TREE
      && va_list_fpr_counter_field == NULL_TREE)
    {
      va_list_escapes = true;
      goto finish;
    }

  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;

      si.compute_sizes = -1;
      si.bb = bb;
      for (i = bsi_start (bb);
	   !bsi_end_p (i) && !va_list_escapes;
	   bsi_next (&i))
	{
	  tree stmt = bsi_stmt (i);
	  tree call;

	  /* Don't look at __builtin_va_{start,end}, they are ok.  */
	  call = get_call_expr_in (stmt);
	  if (call)
	    {
	      tree callee = get_callee_fndecl (call);

	      if (callee
		  && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL
		  && (DECL_FUNCTION_CODE (callee) == BUILT_IN_VA_START
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_VA_END))
		continue;
	    }

	  if (TREE_CODE (stmt) == MODIFY_EXPR)
	    {
	      tree lhs = TREE_OPERAND (stmt, 0);
	      tree rhs = TREE_OPERAND (stmt, 1);

	      if (TREE_CODE (rhs) == WITH_SIZE_EXPR)
		rhs = TREE_OPERAND (rhs, 0);

	      /* Check for ap[0].field = temp.  */
	      if (va_list_counter_struct_op (&si, lhs, rhs, true))
		continue;

	      /* Check for temp = ap[0].field.  */
	      else if (va_list_counter_struct_op (&si, rhs, lhs, false))
		continue;
	    }

	  /* All other uses of va_list are either va_copy (that is not handled
	     in this optimization), taking address of va_list variable or
	     passing va_list to other functions (in that case va_list might
	     escape the function and therefore va_start needs to set it up
	     fully), or some unexpected use of va_list.  None of these should
	     happen in a gimplified VA_ARG_EXPR.  */
	  if (walk_tree (&stmt, find_va_list_reference, si.va_list_vars, NULL))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fputs ("va_list escapes in ", dump_file);
		  print_generic_expr (dump_file, stmt, dump_flags);
		  fputc ('\n', dump_file);
		}
	      va_list_escapes = true;
	    }
	}

      if (va_list_escapes)
	break;
    }

finish:
  if (va_list_escapes)
    {
      cfun->va_list_gpr_size = VA_LIST_MAX_GPR_SIZE;
      cfun->va_list_fpr_size = VA_LIST_MAX_FPR_SIZE;
    }
  BITMAP_XFREE (si.va_list_vars);
  if (dump_file)
    {
      fprintf (dump_file, "%s: va_list escapes %d, needs to save ",
	       funcname, (int) va_list_escapes);
      if (cfun->va_list_gpr_size >= VA_LIST_MAX_GPR_SIZE)
	fputs ("all", dump_file);
      else
	fprintf (dump_file, "%d", cfun->va_list_gpr_size);
      fputs (" GPR units and ", dump_file);
      if (cfun->va_list_fpr_size >= VA_LIST_MAX_FPR_SIZE)
	fputs ("all", dump_file);
      else
	fprintf (dump_file, "%d", cfun->va_list_fpr_size);
      fputs (" FPR units.\n", dump_file);
    }
}


struct tree_opt_pass pass_stdarg =
{
  "stdarg",				/* name */
  gate_optimize_stdarg,			/* gate */
  execute_optimize_stdarg,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func,			/* todo_flags_finish */
  0					/* letter */
};
