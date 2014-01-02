/* SSA operands management for trees.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.

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
#include "stmt.h"
#include "print-tree.h"
#include "flags.h"
#include "function.h"
#include "gimple-pretty-print.h"
#include "bitmap.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-inline.h"
#include "timevar.h"
#include "dumpfile.h"
#include "timevar.h"
#include "langhooks.h"
#include "diagnostic-core.h"


/* This file contains the code required to manage the operands cache of the
   SSA optimizer.  For every stmt, we maintain an operand cache in the stmt
   annotation.  This cache contains operands that will be of interest to
   optimizers and other passes wishing to manipulate the IL.

   The operand type are broken up into REAL and VIRTUAL operands.  The real
   operands are represented as pointers into the stmt's operand tree.  Thus
   any manipulation of the real operands will be reflected in the actual tree.
   Virtual operands are represented solely in the cache, although the base
   variable for the SSA_NAME may, or may not occur in the stmt's tree.
   Manipulation of the virtual operands will not be reflected in the stmt tree.

   The routines in this file are concerned with creating this operand cache
   from a stmt tree.

   The operand tree is the parsed by the various get_* routines which look
   through the stmt tree for the occurrence of operands which may be of
   interest, and calls are made to the append_* routines whenever one is
   found.  There are 4 of these routines, each representing one of the
   4 types of operands. Defs, Uses, Virtual Uses, and Virtual May Defs.

   The append_* routines check for duplication, and simply keep a list of
   unique objects for each operand type in the build_* extendable vectors.

   Once the stmt tree is completely parsed, the finalize_ssa_operands()
   routine is called, which proceeds to perform the finalization routine
   on each of the 4 operand vectors which have been built up.

   If the stmt had a previous operand cache, the finalization routines
   attempt to match up the new operands with the old ones.  If it's a perfect
   match, the old vector is simply reused.  If it isn't a perfect match, then
   a new vector is created and the new operands are placed there.  For
   virtual operands, if the previous cache had SSA_NAME version of a
   variable, and that same variable occurs in the same operands cache, then
   the new cache vector will also get the same SSA_NAME.

   i.e., if a stmt had a VUSE of 'a_5', and 'a' occurs in the new
   operand vector for VUSE, then the new vector will also be modified
   such that it contains 'a_5' rather than 'a'.  */


/* Flags to describe operand properties in helpers.  */

/* By default, operands are loaded.  */
#define opf_use		0

/* Operand is the target of an assignment expression or a
   call-clobbered variable.  */
#define opf_def 	(1 << 0)

/* No virtual operands should be created in the expression.  This is used
   when traversing ADDR_EXPR nodes which have different semantics than
   other expressions.  Inside an ADDR_EXPR node, the only operands that we
   need to consider are indices into arrays.  For instance, &a.b[i] should
   generate a USE of 'i' but it should not generate a VUSE for 'a' nor a
   VUSE for 'b'.  */
#define opf_no_vops 	(1 << 1)

/* Operand is in a place where address-taken does not imply addressable.  */
#define opf_non_addressable (1 << 3)

/* Operand is in a place where opf_non_addressable does not apply.  */
#define opf_not_non_addressable (1 << 4)

/* Operand is having its address taken.  */
#define opf_address_taken (1 << 5)

/* Array for building all the use operands.  */
static vec<tree> build_uses;

/* The built VDEF operand.  */
static tree build_vdef;

/* The built VUSE operand.  */
static tree build_vuse;

/* Bitmap obstack for our datastructures that needs to survive across
   compilations of multiple functions.  */
static bitmap_obstack operands_bitmap_obstack;

static void get_expr_operands (struct function *, gimple, tree *, int);

/* Number of functions with initialized ssa_operands.  */
static int n_initialized = 0;

/* Accessor to tree-ssa-operands.c caches.  */
static inline struct ssa_operands *
gimple_ssa_operands (const struct function *fun)
{
  return &fun->gimple_df->ssa_operands;
}


/*  Return true if the SSA operands cache is active.  */

bool
ssa_operands_active (struct function *fun)
{
  if (fun == NULL)
    return false;

  return fun->gimple_df && gimple_ssa_operands (fun)->ops_active;
}


/* Create the VOP variable, an artificial global variable to act as a
   representative of all of the virtual operands FUD chain.  */

static void
create_vop_var (struct function *fn)
{
  tree global_var;

  gcc_assert (fn->gimple_df->vop == NULL_TREE);

  global_var = build_decl (BUILTINS_LOCATION, VAR_DECL,
			   get_identifier (".MEM"),
			   void_type_node);
  DECL_ARTIFICIAL (global_var) = 1;
  TREE_READONLY (global_var) = 0;
  DECL_EXTERNAL (global_var) = 1;
  TREE_STATIC (global_var) = 1;
  TREE_USED (global_var) = 1;
  DECL_CONTEXT (global_var) = NULL_TREE;
  TREE_THIS_VOLATILE (global_var) = 0;
  TREE_ADDRESSABLE (global_var) = 0;
  VAR_DECL_IS_VIRTUAL_OPERAND (global_var) = 1;

  fn->gimple_df->vop = global_var;
}

/* These are the sizes of the operand memory buffer in bytes which gets
   allocated each time more operands space is required.  The final value is
   the amount that is allocated every time after that.
   In 1k we can fit 25 use operands (or 63 def operands) on a host with
   8 byte pointers, that would be 10 statements each with 1 def and 2
   uses.  */

#define OP_SIZE_INIT	0
#define OP_SIZE_1	(1024 - sizeof (void *))
#define OP_SIZE_2	(1024 * 4 - sizeof (void *))
#define OP_SIZE_3	(1024 * 16 - sizeof (void *))

/* Initialize the operand cache routines.  */

void
init_ssa_operands (struct function *fn)
{
  if (!n_initialized++)
    {
      build_uses.create (10);
      build_vuse = NULL_TREE;
      build_vdef = NULL_TREE;
      bitmap_obstack_initialize (&operands_bitmap_obstack);
    }

  gcc_assert (gimple_ssa_operands (fn)->operand_memory == NULL);
  gimple_ssa_operands (fn)->operand_memory_index
     = gimple_ssa_operands (fn)->ssa_operand_mem_size;
  gimple_ssa_operands (fn)->ops_active = true;
  gimple_ssa_operands (fn)->ssa_operand_mem_size = OP_SIZE_INIT;
  create_vop_var (fn);
}


/* Dispose of anything required by the operand routines.  */

void
fini_ssa_operands (struct function *fn)
{
  struct ssa_operand_memory_d *ptr;

  if (!--n_initialized)
    {
      build_uses.release ();
      build_vdef = NULL_TREE;
      build_vuse = NULL_TREE;
    }

  gimple_ssa_operands (fn)->free_uses = NULL;

  while ((ptr = gimple_ssa_operands (fn)->operand_memory) != NULL)
    {
      gimple_ssa_operands (fn)->operand_memory
	= gimple_ssa_operands (fn)->operand_memory->next;
      ggc_free (ptr);
    }

  gimple_ssa_operands (fn)->ops_active = false;

  if (!n_initialized)
    bitmap_obstack_release (&operands_bitmap_obstack);

  fn->gimple_df->vop = NULL_TREE;
}


/* Return memory for an operand of size SIZE.  */

static inline void *
ssa_operand_alloc (struct function *fn, unsigned size)
{
  char *ptr;

  gcc_assert (size == sizeof (struct use_optype_d));

  if (gimple_ssa_operands (fn)->operand_memory_index + size
      >= gimple_ssa_operands (fn)->ssa_operand_mem_size)
    {
      struct ssa_operand_memory_d *ptr;

      switch (gimple_ssa_operands (fn)->ssa_operand_mem_size)
	{
	case OP_SIZE_INIT:
	  gimple_ssa_operands (fn)->ssa_operand_mem_size = OP_SIZE_1;
	  break;
	case OP_SIZE_1:
	  gimple_ssa_operands (fn)->ssa_operand_mem_size = OP_SIZE_2;
	  break;
	case OP_SIZE_2:
	case OP_SIZE_3:
	  gimple_ssa_operands (fn)->ssa_operand_mem_size = OP_SIZE_3;
	  break;
	default:
	  gcc_unreachable ();
	}


      ptr = ggc_alloc_ssa_operand_memory_d (sizeof (void *)
                        + gimple_ssa_operands (fn)->ssa_operand_mem_size);

      ptr->next = gimple_ssa_operands (fn)->operand_memory;
      gimple_ssa_operands (fn)->operand_memory = ptr;
      gimple_ssa_operands (fn)->operand_memory_index = 0;
    }

  ptr = &(gimple_ssa_operands (fn)->operand_memory
	  ->mem[gimple_ssa_operands (fn)->operand_memory_index]);
  gimple_ssa_operands (fn)->operand_memory_index += size;
  return ptr;
}


/* Allocate a USE operand.  */

static inline struct use_optype_d *
alloc_use (struct function *fn)
{
  struct use_optype_d *ret;
  if (gimple_ssa_operands (fn)->free_uses)
    {
      ret = gimple_ssa_operands (fn)->free_uses;
      gimple_ssa_operands (fn)->free_uses
	= gimple_ssa_operands (fn)->free_uses->next;
    }
  else
    ret = (struct use_optype_d *)
          ssa_operand_alloc (fn, sizeof (struct use_optype_d));
  return ret;
}


/* Adds OP to the list of uses of statement STMT after LAST.  */

static inline use_optype_p
add_use_op (struct function *fn, gimple stmt, tree *op, use_optype_p last)
{
  use_optype_p new_use;

  new_use = alloc_use (fn);
  USE_OP_PTR (new_use)->use = op;
  link_imm_use_stmt (USE_OP_PTR (new_use), *op, stmt);
  last->next = new_use;
  new_use->next = NULL;
  return new_use;
}



/* Takes elements from build_defs and turns them into def operands of STMT.
   TODO -- Make build_defs vec of tree *.  */

static inline void
finalize_ssa_defs (struct function *fn, gimple stmt)
{
  /* Pre-pend the vdef we may have built.  */
  if (build_vdef != NULL_TREE)
    {
      tree oldvdef = gimple_vdef (stmt);
      if (oldvdef
	  && TREE_CODE (oldvdef) == SSA_NAME)
	oldvdef = SSA_NAME_VAR (oldvdef);
      if (oldvdef != build_vdef)
	gimple_set_vdef (stmt, build_vdef);
    }

  /* Clear and unlink a no longer necessary VDEF.  */
  if (build_vdef == NULL_TREE
      && gimple_vdef (stmt) != NULL_TREE)
    {
      if (TREE_CODE (gimple_vdef (stmt)) == SSA_NAME)
	{
	  unlink_stmt_vdef (stmt);
	  release_ssa_name_fn (fn, gimple_vdef (stmt));
	}
      gimple_set_vdef (stmt, NULL_TREE);
    }

  /* If we have a non-SSA_NAME VDEF, mark it for renaming.  */
  if (gimple_vdef (stmt)
      && TREE_CODE (gimple_vdef (stmt)) != SSA_NAME)
    {
      fn->gimple_df->rename_vops = 1;
      fn->gimple_df->ssa_renaming_needed = 1;
    }
}


/* Takes elements from build_uses and turns them into use operands of STMT.
   TODO -- Make build_uses vec of tree *.  */

static inline void
finalize_ssa_uses (struct function *fn, gimple stmt)
{
  unsigned new_i;
  struct use_optype_d new_list;
  use_optype_p old_ops, ptr, last;

  /* Pre-pend the VUSE we may have built.  */
  if (build_vuse != NULL_TREE)
    {
      tree oldvuse = gimple_vuse (stmt);
      if (oldvuse
	  && TREE_CODE (oldvuse) == SSA_NAME)
	oldvuse = SSA_NAME_VAR (oldvuse);
      if (oldvuse != (build_vuse != NULL_TREE
		      ? build_vuse : build_vdef))
	gimple_set_vuse (stmt, NULL_TREE);
      build_uses.safe_insert (0, (tree)gimple_vuse_ptr (stmt));
    }

  new_list.next = NULL;
  last = &new_list;

  old_ops = gimple_use_ops (stmt);

  /* Clear a no longer necessary VUSE.  */
  if (build_vuse == NULL_TREE
      && gimple_vuse (stmt) != NULL_TREE)
    gimple_set_vuse (stmt, NULL_TREE);

  /* If there is anything in the old list, free it.  */
  if (old_ops)
    {
      for (ptr = old_ops; ptr; ptr = ptr->next)
	delink_imm_use (USE_OP_PTR (ptr));
      old_ops->next = gimple_ssa_operands (fn)->free_uses;
      gimple_ssa_operands (fn)->free_uses = old_ops;
    }

  /* If we added a VUSE, make sure to set the operand if it is not already
     present and mark it for renaming.  */
  if (build_vuse != NULL_TREE
      && gimple_vuse (stmt) == NULL_TREE)
    {
      gimple_set_vuse (stmt, gimple_vop (fn));
      fn->gimple_df->rename_vops = 1;
      fn->gimple_df->ssa_renaming_needed = 1;
    }

  /* Now create nodes for all the new nodes.  */
  for (new_i = 0; new_i < build_uses.length (); new_i++)
    {
      tree *op = (tree *) build_uses[new_i];
      last = add_use_op (fn, stmt, op, last);
    }

  /* Now set the stmt's operands.  */
  gimple_set_use_ops (stmt, new_list.next);
}


/* Clear the in_list bits and empty the build array for VDEFs and
   VUSEs.  */

static inline void
cleanup_build_arrays (void)
{
  build_vdef = NULL_TREE;
  build_vuse = NULL_TREE;
  build_uses.truncate (0);
}


/* Finalize all the build vectors, fill the new ones into INFO.  */

static inline void
finalize_ssa_stmt_operands (struct function *fn, gimple stmt)
{
  finalize_ssa_defs (fn, stmt);
  finalize_ssa_uses (fn, stmt);
  cleanup_build_arrays ();
}


/* Start the process of building up operands vectors in INFO.  */

static inline void
start_ssa_stmt_operands (void)
{
  gcc_assert (build_uses.length () == 0);
  gcc_assert (build_vuse == NULL_TREE);
  gcc_assert (build_vdef == NULL_TREE);
}


/* Add USE_P to the list of pointers to operands.  */

static inline void
append_use (tree *use_p)
{
  build_uses.safe_push ((tree) use_p);
}


/* Add VAR to the set of variables that require a VDEF operator.  */

static inline void
append_vdef (tree var)
{
  if (!optimize)
    return;

  gcc_assert ((build_vdef == NULL_TREE
	       || build_vdef == var)
	      && (build_vuse == NULL_TREE
		  || build_vuse == var));

  build_vdef = var;
  build_vuse = var;
}


/* Add VAR to the set of variables that require a VUSE operator.  */

static inline void
append_vuse (tree var)
{
  if (!optimize)
    return;

  gcc_assert (build_vuse == NULL_TREE
	      || build_vuse == var);

  build_vuse = var;
}

/* Add virtual operands for STMT.  FLAGS is as in get_expr_operands.  */

static void
add_virtual_operand (struct function *fn,
		     gimple stmt ATTRIBUTE_UNUSED, int flags)
{
  /* Add virtual operands to the stmt, unless the caller has specifically
     requested not to do that (used when adding operands inside an
     ADDR_EXPR expression).  */
  if (flags & opf_no_vops)
    return;

  gcc_assert (!is_gimple_debug (stmt));

  if (flags & opf_def)
    append_vdef (gimple_vop (fn));
  else
    append_vuse (gimple_vop (fn));
}


/* Add *VAR_P to the appropriate operand array for statement STMT.
   FLAGS is as in get_expr_operands.  If *VAR_P is a GIMPLE register,
   it will be added to the statement's real operands, otherwise it is
   added to virtual operands.  */

static void
add_stmt_operand (struct function *fn, tree *var_p, gimple stmt, int flags)
{
  tree var = *var_p;

  gcc_assert (SSA_VAR_P (*var_p));

  if (is_gimple_reg (var))
    {
      /* The variable is a GIMPLE register.  Add it to real operands.  */
      if (flags & opf_def)
	;
      else
	append_use (var_p);
      if (DECL_P (*var_p))
	fn->gimple_df->ssa_renaming_needed = 1;
    }
  else
    {
      /* Mark statements with volatile operands.  */
      if (!(flags & opf_no_vops)
	  && TREE_THIS_VOLATILE (var))
	gimple_set_has_volatile_ops (stmt, true);

      /* The variable is a memory access.  Add virtual operands.  */
      add_virtual_operand (fn, stmt, flags);
    }
}

/* Mark the base address of REF as having its address taken.
   REF may be a single variable whose address has been taken or any
   other valid GIMPLE memory reference (structure reference, array,
   etc).  */

static void
mark_address_taken (tree ref)
{
  tree var;

  /* Note that it is *NOT OKAY* to use the target of a COMPONENT_REF
     as the only thing we take the address of.  If VAR is a structure,
     taking the address of a field means that the whole structure may
     be referenced using pointer arithmetic.  See PR 21407 and the
     ensuing mailing list discussion.  */
  var = get_base_address (ref);
  if (var)
    {
      if (DECL_P (var))
	TREE_ADDRESSABLE (var) = 1;
      else if (TREE_CODE (var) == MEM_REF
	       && TREE_CODE (TREE_OPERAND (var, 0)) == ADDR_EXPR
	       && DECL_P (TREE_OPERAND (TREE_OPERAND (var, 0), 0)))
	TREE_ADDRESSABLE (TREE_OPERAND (TREE_OPERAND (var, 0), 0)) = 1;
    }
}


/* A subroutine of get_expr_operands to handle MEM_REF.

   STMT is the statement being processed, EXPR is the MEM_REF
      that got us here.

   FLAGS is as in get_expr_operands.  */

static void
get_mem_ref_operands (struct function *fn,
		      gimple stmt, tree expr, int flags)
{
  tree *pptr = &TREE_OPERAND (expr, 0);

  if (!(flags & opf_no_vops)
      && TREE_THIS_VOLATILE (expr))
    gimple_set_has_volatile_ops (stmt, true);

  /* Add the VOP.  */
  add_virtual_operand (fn, stmt, flags);

  /* If requested, add a USE operand for the base pointer.  */
  get_expr_operands (fn, stmt, pptr,
		     opf_non_addressable | opf_use
		     | (flags & (opf_no_vops|opf_not_non_addressable)));
}


/* A subroutine of get_expr_operands to handle TARGET_MEM_REF.  */

static void
get_tmr_operands (struct function *fn, gimple stmt, tree expr, int flags)
{
  if (!(flags & opf_no_vops)
      && TREE_THIS_VOLATILE (expr))
    gimple_set_has_volatile_ops (stmt, true);

  /* First record the real operands.  */
  get_expr_operands (fn, stmt,
		     &TMR_BASE (expr), opf_use | (flags & opf_no_vops));
  get_expr_operands (fn, stmt,
		     &TMR_INDEX (expr), opf_use | (flags & opf_no_vops));
  get_expr_operands (fn, stmt,
		     &TMR_INDEX2 (expr), opf_use | (flags & opf_no_vops));

  add_virtual_operand (fn, stmt, flags);
}


/* If STMT is a call that may clobber globals and other symbols that
   escape, add them to the VDEF/VUSE lists for it.  */

static void
maybe_add_call_vops (struct function *fn, gimple stmt)
{
  int call_flags = gimple_call_flags (stmt);

  /* If aliases have been computed already, add VDEF or VUSE
     operands for all the symbols that have been found to be
     call-clobbered.  */
  if (!(call_flags & ECF_NOVOPS))
    {
      /* A 'pure' or a 'const' function never call-clobbers anything.
	 A 'noreturn' function might, but since we don't return anyway
	 there is no point in recording that.  */
      if (!(call_flags & (ECF_PURE | ECF_CONST | ECF_NORETURN)))
	add_virtual_operand (fn, stmt, opf_def);
      else if (!(call_flags & ECF_CONST))
	add_virtual_operand (fn, stmt, opf_use);
    }
}


/* Scan operands in the ASM_EXPR stmt referred to in INFO.  */

static void
get_asm_stmt_operands (struct function *fn, gimple stmt)
{
  size_t i, noutputs;
  const char **oconstraints;
  const char *constraint;
  bool allows_mem, allows_reg, is_inout;

  noutputs = gimple_asm_noutputs (stmt);
  oconstraints = (const char **) alloca ((noutputs) * sizeof (const char *));

  /* Gather all output operands.  */
  for (i = 0; i < gimple_asm_noutputs (stmt); i++)
    {
      tree link = gimple_asm_output_op (stmt, i);
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      oconstraints[i] = constraint;
      parse_output_constraint (&constraint, i, 0, 0, &allows_mem,
	                       &allows_reg, &is_inout);

      /* This should have been split in gimplify_asm_expr.  */
      gcc_assert (!allows_reg || !is_inout);

      /* Memory operands are addressable.  Note that STMT needs the
	 address of this operand.  */
      if (!allows_reg && allows_mem)
	mark_address_taken (TREE_VALUE (link));

      get_expr_operands (fn, stmt,
			 &TREE_VALUE (link), opf_def | opf_not_non_addressable);
    }

  /* Gather all input operands.  */
  for (i = 0; i < gimple_asm_ninputs (stmt); i++)
    {
      tree link = gimple_asm_input_op (stmt, i);
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      parse_input_constraint (&constraint, 0, 0, noutputs, 0, oconstraints,
	                      &allows_mem, &allows_reg);

      /* Memory operands are addressable.  Note that STMT needs the
	 address of this operand.  */
      if (!allows_reg && allows_mem)
	mark_address_taken (TREE_VALUE (link));

      get_expr_operands (fn, stmt, &TREE_VALUE (link), opf_not_non_addressable);
    }

  /* Clobber all memory and addressable symbols for asm ("" : : : "memory");  */
  if (gimple_asm_clobbers_memory_p (stmt))
    add_virtual_operand (fn, stmt, opf_def);
}


/* Recursively scan the expression pointed to by EXPR_P in statement
   STMT.  FLAGS is one of the OPF_* constants modifying how to
   interpret the operands found.  */

static void
get_expr_operands (struct function *fn, gimple stmt, tree *expr_p, int flags)
{
  enum tree_code code;
  enum tree_code_class codeclass;
  tree expr = *expr_p;
  int uflags = opf_use;

  if (expr == NULL)
    return;

  if (is_gimple_debug (stmt))
    uflags |= (flags & opf_no_vops);

  code = TREE_CODE (expr);
  codeclass = TREE_CODE_CLASS (code);

  switch (code)
    {
    case ADDR_EXPR:
      /* Taking the address of a variable does not represent a
	 reference to it, but the fact that the statement takes its
	 address will be of interest to some passes (e.g. alias
	 resolution).  */
      if ((!(flags & opf_non_addressable)
	   || (flags & opf_not_non_addressable))
	  && !is_gimple_debug (stmt))
	mark_address_taken (TREE_OPERAND (expr, 0));

      /* Otherwise, there may be variables referenced inside but there
	 should be no VUSEs created, since the referenced objects are
	 not really accessed.  The only operands that we should find
	 here are ARRAY_REF indices which will always be real operands
	 (GIMPLE does not allow non-registers as array indices).  */
      flags |= opf_no_vops;
      get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 0),
			 flags | opf_not_non_addressable | opf_address_taken);
      return;

    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      if (!(flags & opf_address_taken))
	add_stmt_operand (fn, expr_p, stmt, flags);
      return;

    case DEBUG_EXPR_DECL:
      gcc_assert (gimple_debug_bind_p (stmt));
      return;

    case MEM_REF:
      get_mem_ref_operands (fn, stmt, expr, flags);
      return;

    case TARGET_MEM_REF:
      get_tmr_operands (fn, stmt, expr, flags);
      return;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case COMPONENT_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      {
	if (!(flags & opf_no_vops)
	    && TREE_THIS_VOLATILE (expr))
	  gimple_set_has_volatile_ops (stmt, true);

	get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 0), flags);

	if (code == COMPONENT_REF)
	  {
	    if (!(flags & opf_no_vops)
		&& TREE_THIS_VOLATILE (TREE_OPERAND (expr, 1)))
	      gimple_set_has_volatile_ops (stmt, true);
	    get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 2), uflags);
	  }
	else if (code == ARRAY_REF || code == ARRAY_RANGE_REF)
	  {
            get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 1), uflags);
            get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 2), uflags);
            get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 3), uflags);
	  }

	return;
      }

    case WITH_SIZE_EXPR:
      /* WITH_SIZE_EXPR is a pass-through reference to its first argument,
	 and an rvalue reference to its second argument.  */
      get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 1), uflags);
      get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 0), flags);
      return;

    case COND_EXPR:
    case VEC_COND_EXPR:
    case VEC_PERM_EXPR:
      get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 0), uflags);
      get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 1), uflags);
      get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 2), uflags);
      return;

    case CONSTRUCTOR:
      {
	/* General aggregate CONSTRUCTORs have been decomposed, but they
	   are still in use as the COMPLEX_EXPR equivalent for vectors.  */
	constructor_elt *ce;
	unsigned HOST_WIDE_INT idx;

	/* A volatile constructor is actually TREE_CLOBBER_P, transfer
	   the volatility to the statement, don't use TREE_CLOBBER_P for
	   mirroring the other uses of THIS_VOLATILE in this file.  */
	if (!(flags & opf_no_vops)
	    && TREE_THIS_VOLATILE (expr))
	  gimple_set_has_volatile_ops (stmt, true);

	for (idx = 0;
	     vec_safe_iterate (CONSTRUCTOR_ELTS (expr), idx, &ce);
	     idx++)
	  get_expr_operands (fn, stmt, &ce->value, uflags);

	return;
      }

    case BIT_FIELD_REF:
      if (!(flags & opf_no_vops)
	  && TREE_THIS_VOLATILE (expr))
	gimple_set_has_volatile_ops (stmt, true);
      /* FALLTHRU */

    case VIEW_CONVERT_EXPR:
    do_unary:
      get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 0), flags);
      return;

    case COMPOUND_EXPR:
    case OBJ_TYPE_REF:
    case ASSERT_EXPR:
    do_binary:
      {
	get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 0), flags);
	get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 1), flags);
	return;
      }

    case DOT_PROD_EXPR:
    case REALIGN_LOAD_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
    case FMA_EXPR:
      {
	get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 0), flags);
	get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 1), flags);
	get_expr_operands (fn, stmt, &TREE_OPERAND (expr, 2), flags);
	return;
      }

    case FUNCTION_DECL:
    case LABEL_DECL:
    case CONST_DECL:
    case CASE_LABEL_EXPR:
      /* Expressions that make no memory references.  */
      return;

    default:
      if (codeclass == tcc_unary)
	goto do_unary;
      if (codeclass == tcc_binary || codeclass == tcc_comparison)
	goto do_binary;
      if (codeclass == tcc_constant || codeclass == tcc_type)
	return;
    }

  /* If we get here, something has gone wrong.  */
#ifdef ENABLE_CHECKING
  fprintf (stderr, "unhandled expression in get_expr_operands():\n");
  debug_tree (expr);
  fputs ("\n", stderr);
#endif
  gcc_unreachable ();
}


/* Parse STMT looking for operands.  When finished, the various
   build_* operand vectors will have potential operands in them.  */

static void
parse_ssa_operands (struct function *fn, gimple stmt)
{
  enum gimple_code code = gimple_code (stmt);
  size_t i, n, start = 0;

  switch (code)
    {
    case GIMPLE_ASM:
      get_asm_stmt_operands (fn, stmt);
      break;

    case GIMPLE_TRANSACTION:
      /* The start of a transaction is a memory barrier.  */
      add_virtual_operand (fn, stmt, opf_def | opf_use);
      break;

    case GIMPLE_DEBUG:
      if (gimple_debug_bind_p (stmt)
	  && gimple_debug_bind_has_value_p (stmt))
	get_expr_operands (fn, stmt, gimple_debug_bind_get_value_ptr (stmt),
			   opf_use | opf_no_vops);
      break;

    case GIMPLE_RETURN:
      append_vuse (gimple_vop (fn));
      goto do_default;

    case GIMPLE_CALL:
      /* Add call-clobbered operands, if needed.  */
      maybe_add_call_vops (fn, stmt);
      /* FALLTHRU */

    case GIMPLE_ASSIGN:
      get_expr_operands (fn, stmt, gimple_op_ptr (stmt, 0), opf_def);
      start = 1;
      /* FALLTHRU */

    default:
    do_default:
      n = gimple_num_ops (stmt);
      for (i = start; i < n; i++)
	get_expr_operands (fn, stmt, gimple_op_ptr (stmt, i), opf_use);
      break;
    }
}


/* Create an operands cache for STMT.  */

static void
build_ssa_operands (struct function *fn, gimple stmt)
{
  /* Initially assume that the statement has no volatile operands.  */
  gimple_set_has_volatile_ops (stmt, false);

  start_ssa_stmt_operands ();
  parse_ssa_operands (fn, stmt);
  finalize_ssa_stmt_operands (fn, stmt);
}

/* Verifies SSA statement operands.  */

DEBUG_FUNCTION bool
verify_ssa_operands (struct function *fn, gimple stmt)
{
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;
  unsigned i;
  tree use, def;
  bool volatile_p = gimple_has_volatile_ops (stmt);

  /* build_ssa_operands w/o finalizing them.  */
  gimple_set_has_volatile_ops (stmt, false);
  start_ssa_stmt_operands ();
  parse_ssa_operands (fn, stmt);

  /* Now verify the built operands are the same as present in STMT.  */
  def = gimple_vdef (stmt);
  if (def
      && TREE_CODE (def) == SSA_NAME)
    def = SSA_NAME_VAR (def);
  if (build_vdef != def)
    {
      error ("virtual definition of statement not up-to-date");
      return true;
    }
  if (gimple_vdef (stmt)
      && ((def_p = gimple_vdef_op (stmt)) == NULL_DEF_OPERAND_P
	  || DEF_FROM_PTR (def_p) != gimple_vdef (stmt)))
    {
      error ("virtual def operand missing for stmt");
      return true;
    }

  use = gimple_vuse (stmt);
  if (use
      && TREE_CODE (use) == SSA_NAME)
    use = SSA_NAME_VAR (use);
  if (build_vuse != use)
    {
      error ("virtual use of statement not up-to-date");
      return true;
    }
  if (gimple_vuse (stmt)
      && ((use_p = gimple_vuse_op (stmt)) == NULL_USE_OPERAND_P
	  || USE_FROM_PTR (use_p) != gimple_vuse (stmt)))
    {
      error ("virtual use operand missing for stmt");
      return true;
    }

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      FOR_EACH_VEC_ELT (build_uses, i, use)
	{
	  if (use_p->use == (tree *)use)
	    {
	      build_uses[i] = NULL_TREE;
	      break;
	    }
	}
      if (i == build_uses.length ())
	{
	  error ("excess use operand for stmt");
	  debug_generic_expr (USE_FROM_PTR (use_p));
	  return true;
	}
    }
  FOR_EACH_VEC_ELT (build_uses, i, use)
    if (use != NULL_TREE)
      {
	error ("use operand missing for stmt");
	debug_generic_expr (*(tree *)use);
	return true;
      }

  if (gimple_has_volatile_ops (stmt) != volatile_p)
    {
      error ("stmt volatile flag not up-to-date");
      return true;
    }

  cleanup_build_arrays ();
  return false;
}


/* Releases the operands of STMT back to their freelists, and clears
   the stmt operand lists.  */

void
free_stmt_operands (struct function *fn, gimple stmt)
{
  use_optype_p uses = gimple_use_ops (stmt), last_use;

  if (uses)
    {
      for (last_use = uses; last_use->next; last_use = last_use->next)
	delink_imm_use (USE_OP_PTR (last_use));
      delink_imm_use (USE_OP_PTR (last_use));
      last_use->next = gimple_ssa_operands (fn)->free_uses;
      gimple_ssa_operands (fn)->free_uses = uses;
      gimple_set_use_ops (stmt, NULL);
    }

  if (gimple_has_mem_ops (stmt))
    {
      gimple_set_vuse (stmt, NULL_TREE);
      gimple_set_vdef (stmt, NULL_TREE);
    }
}


/* Get the operands of statement STMT.  */

void
update_stmt_operands (struct function *fn, gimple stmt)
{
  /* If update_stmt_operands is called before SSA is initialized, do
     nothing.  */
  if (!ssa_operands_active (fn))
    return;

  timevar_push (TV_TREE_OPS);

  /* If the stmt is a noreturn call queue it to be processed by
     split_bbs_on_noreturn_calls during cfg cleanup.  */
  if (is_gimple_call (stmt)
      && gimple_call_noreturn_p (stmt))
    vec_safe_push (MODIFIED_NORETURN_CALLS (fn), stmt);

  gcc_assert (gimple_modified_p (stmt));
  build_ssa_operands (fn, stmt);
  gimple_set_modified (stmt, false);

  timevar_pop (TV_TREE_OPS);
}


/* Swap operands EXP0 and EXP1 in statement STMT.  No attempt is done
   to test the validity of the swap operation.  */

void
swap_ssa_operands (gimple stmt, tree *exp0, tree *exp1)
{
  tree op0, op1;
  op0 = *exp0;
  op1 = *exp1;

  if (op0 != op1)
    {
      /* Attempt to preserve the relative positions of these two operands in
	 their * respective immediate use lists by adjusting their use pointer
	 to point to the new operand position.  */
      use_optype_p use0, use1, ptr;
      use0 = use1 = NULL;

      /* Find the 2 operands in the cache, if they are there.  */
      for (ptr = gimple_use_ops (stmt); ptr; ptr = ptr->next)
	if (USE_OP_PTR (ptr)->use == exp0)
	  {
	    use0 = ptr;
	    break;
	  }

      for (ptr = gimple_use_ops (stmt); ptr; ptr = ptr->next)
	if (USE_OP_PTR (ptr)->use == exp1)
	  {
	    use1 = ptr;
	    break;
	  }

      /* And adjust their location to point to the new position of the
         operand.  */
      if (use0)
	USE_OP_PTR (use0)->use = exp1;
      if (use1)
	USE_OP_PTR (use1)->use = exp0;

      /* Now swap the data.  */
      *exp0 = op1;
      *exp1 = op0;
    }
}


/* Scan the immediate_use list for VAR making sure its linked properly.
   Return TRUE if there is a problem and emit an error message to F.  */

DEBUG_FUNCTION bool
verify_imm_links (FILE *f, tree var)
{
  use_operand_p ptr, prev, list;
  int count;

  gcc_assert (TREE_CODE (var) == SSA_NAME);

  list = &(SSA_NAME_IMM_USE_NODE (var));
  gcc_assert (list->use == NULL);

  if (list->prev == NULL)
    {
      gcc_assert (list->next == NULL);
      return false;
    }

  prev = list;
  count = 0;
  for (ptr = list->next; ptr != list; )
    {
      if (prev != ptr->prev)
	goto error;

      if (ptr->use == NULL)
	goto error; /* 2 roots, or SAFE guard node.  */
      else if (*(ptr->use) != var)
	goto error;

      prev = ptr;
      ptr = ptr->next;

      /* Avoid infinite loops.  50,000,000 uses probably indicates a
	 problem.  */
      if (count++ > 50000000)
	goto error;
    }

  /* Verify list in the other direction.  */
  prev = list;
  for (ptr = list->prev; ptr != list; )
    {
      if (prev != ptr->next)
	goto error;
      prev = ptr;
      ptr = ptr->prev;
      if (count-- < 0)
	goto error;
    }

  if (count != 0)
    goto error;

  return false;

 error:
  if (ptr->loc.stmt && gimple_modified_p (ptr->loc.stmt))
    {
      fprintf (f, " STMT MODIFIED. - <%p> ", (void *)ptr->loc.stmt);
      print_gimple_stmt (f, ptr->loc.stmt, 0, TDF_SLIM);
    }
  fprintf (f, " IMM ERROR : (use_p : tree - %p:%p)", (void *)ptr,
	   (void *)ptr->use);
  print_generic_expr (f, USE_FROM_PTR (ptr), TDF_SLIM);
  fprintf (f, "\n");
  return true;
}


/* Dump all the immediate uses to FILE.  */

void
dump_immediate_uses_for (FILE *file, tree var)
{
  imm_use_iterator iter;
  use_operand_p use_p;

  gcc_assert (var && TREE_CODE (var) == SSA_NAME);

  print_generic_expr (file, var, TDF_SLIM);
  fprintf (file, " : -->");
  if (has_zero_uses (var))
    fprintf (file, " no uses.\n");
  else
    if (has_single_use (var))
      fprintf (file, " single use.\n");
    else
      fprintf (file, "%d uses.\n", num_imm_uses (var));

  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
    {
      if (use_p->loc.stmt == NULL && use_p->use == NULL)
        fprintf (file, "***end of stmt iterator marker***\n");
      else
	if (!is_gimple_reg (USE_FROM_PTR (use_p)))
	  print_gimple_stmt (file, USE_STMT (use_p), 0, TDF_VOPS|TDF_MEMSYMS);
	else
	  print_gimple_stmt (file, USE_STMT (use_p), 0, TDF_SLIM);
    }
  fprintf (file, "\n");
}


/* Dump all the immediate uses to FILE.  */

void
dump_immediate_uses (FILE *file)
{
  tree var;
  unsigned int x;

  fprintf (file, "Immediate_uses: \n\n");
  for (x = 1; x < num_ssa_names; x++)
    {
      var = ssa_name (x);
      if (!var)
        continue;
      dump_immediate_uses_for (file, var);
    }
}


/* Dump def-use edges on stderr.  */

DEBUG_FUNCTION void
debug_immediate_uses (void)
{
  dump_immediate_uses (stderr);
}


/* Dump def-use edges on stderr.  */

DEBUG_FUNCTION void
debug_immediate_uses_for (tree var)
{
  dump_immediate_uses_for (stderr, var);
}


/* Unlink STMTs virtual definition from the IL by propagating its use.  */

void
unlink_stmt_vdef (gimple stmt)
{
  use_operand_p use_p;
  imm_use_iterator iter;
  gimple use_stmt;
  tree vdef = gimple_vdef (stmt);
  tree vuse = gimple_vuse (stmt);

  if (!vdef
      || TREE_CODE (vdef) != SSA_NAME)
    return;

  FOR_EACH_IMM_USE_STMT (use_stmt, iter, vdef)
    {
      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	SET_USE (use_p, vuse);
    }

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (vdef))
    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (vuse) = 1;
}


/* Return true if the var whose chain of uses starts at PTR has no
   nondebug uses.  */
bool
has_zero_uses_1 (const ssa_use_operand_t *head)
{
  const ssa_use_operand_t *ptr;

  for (ptr = head->next; ptr != head; ptr = ptr->next)
    if (!is_gimple_debug (USE_STMT (ptr)))
      return false;

  return true;
}


/* Return true if the var whose chain of uses starts at PTR has a
   single nondebug use.  Set USE_P and STMT to that single nondebug
   use, if so, or to NULL otherwise.  */
bool
single_imm_use_1 (const ssa_use_operand_t *head,
		  use_operand_p *use_p, gimple *stmt)
{
  ssa_use_operand_t *ptr, *single_use = 0;

  for (ptr = head->next; ptr != head; ptr = ptr->next)
    if (!is_gimple_debug (USE_STMT (ptr)))
      {
	if (single_use)
	  {
	    single_use = NULL;
	    break;
	  }
	single_use = ptr;
      }

  if (use_p)
    *use_p = single_use;

  if (stmt)
    *stmt = single_use ? single_use->loc.stmt : NULL;

  return single_use;
}
