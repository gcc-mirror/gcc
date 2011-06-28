/* upc-gimplify.c: UPC language specific tree lowering pass
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

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
#include "coretypes.h"
#include "system.h"
#include "tree.h"
#include "vec.h"
#include "input.h"
#include "c-tree.h"
#include "flags.h"
#include "opts.h"
#include "options.h"
#include "toplev.h"
#include "output.h"
#include "tm.h"
#include "rtl.h"
#include "insn-flags.h"
#include "expr.h"
#include "optabs.h"
#include "c-family/c-common.h"
#include "c-family/c-pragma.h"
#include "c-family/c-upc.h"
#include "function.h"
#include "bitmap.h"
#include "gimple.h"
#include "tree-check.h"
#include "ggc.h"
#include "target.h"
#include "upc-tree.h"
#include "upc-act.h"
#include "upc-pts.h"
#include "upc-rts-names.h"
#include "upc-gasp.h"
#include "upc-gimplify.h"
#include "langhooks.h"


static int upc_call_has_shared_args (tree);
static tree upc_expand_get (location_t, tree, gimple_seq *, gimple_seq *);
static tree upc_expand_put (location_t, tree, tree, gimple_seq *);
static tree upc_make_bit_field_ref (tree, tree, int, int);
static tree upc_shared_addr (location_t, tree);
static tree upc_simplify_shared_ref (location_t, tree);
static void upc_strip_useless_generic_pts_cvt (tree *);

static int upc_gimplify_addr_expr (location_t, tree *,
				   gimple_seq *, gimple_seq *);
static int upc_gimplify_array_ref (location_t, tree *,
				   gimple_seq *, gimple_seq *);
static int upc_gimplify_call_expr (tree *, gimple_seq *);
static int upc_gimplify_compound_lval (tree, gimple_seq *, gimple_seq *);
static int upc_gimplify_field_ref (location_t, tree *,
				   gimple_seq *, gimple_seq *);
static int upc_gimplify_forall_stmt (tree *, gimple_seq *, gimple_seq *);
static int upc_gimplify_indirect_ref (location_t, tree *,
				      gimple_seq *, gimple_seq *);
static int upc_gimplify_intermed_ref (tree *, gimple_seq *, gimple_seq *);
static int upc_gimplify_lval (location_t, tree * expr_p,
			      gimple_seq * pre_p, gimple_seq * post_p,
			      fallback_t fallback);
static int upc_gimplify_modify_expr (location_t, tree *,
				     gimple_seq *, gimple_seq *, bool);
static int upc_gimplify_pts_arith_expr (location_t, tree *,
					gimple_seq *, gimple_seq *);
static int upc_gimplify_pts_cond_expr (location_t, tree *,
				       gimple_seq *, gimple_seq *);
static int upc_gimplify_pts_cvt (location_t, tree *,
				 gimple_seq *, gimple_seq *);
static int upc_gimplify_real_image_ref (location_t, tree *,
					gimple_seq *, gimple_seq *);
static int upc_gimplify_shared_inc_dec_expr (location_t, tree *,
					     gimple_seq *, gimple_seq *);
static int upc_gimplify_shared_var_ref (location_t, tree *,
					gimple_seq *, gimple_seq *);
static int upc_gimplify_sync_stmt (location_t, tree *,
				   gimple_seq *, gimple_seq *);

/* Generate a call to the runtime to implement a 'get' of a shared
 * object.  SRC_ADDR is a pointer to UPC shared value that references
 * the shared object. */

static tree
upc_expand_get (location_t loc, tree src_addr,
		gimple_seq * pre_p, gimple_seq * post_p)
{
  tree type = TREE_TYPE (TREE_TYPE (src_addr));
  /* Drop the shared qualifier.  */
  tree result_type = TYPE_MAIN_VARIANT (type);
  int strict_mode = TYPE_STRICT (type)
    || (!TYPE_RELAXED (type) && get_upc_consistency_mode ());
  int doprofcall = flag_upc_instrument && get_upc_pupc_mode ();
  optab get_op = (POINTER_SIZE == 64)
    ? (doprofcall ? (strict_mode ? xgetsg_optab : xgetg_optab)
       : (strict_mode ? xgets_optab : xget_optab))
    : (doprofcall ? (strict_mode ? getsg_optab : getg_optab)
       : (strict_mode ? gets_optab : get_optab));
  enum machine_mode mode = TYPE_MODE (type);
  enum machine_mode op_mode = (mode == TImode) ? BLKmode : mode;
  rtx lib_op = optab_libfunc (get_op, op_mode);
  const char *libfunc_name;
  tree result, libfunc, lib_args, lib_call;
  src_addr = get_initialized_tmp_var (src_addr, pre_p, post_p);
  /* The runtime API expects the internal rep. of a UPC pointer-to-shared.  */
  src_addr = build1 (NOP_EXPR, upc_pts_rep_type_node, src_addr);
  if (!lib_op)
    abort ();
  libfunc_name = XSTR (lib_op, 0);
  libfunc = identifier_global_value (get_identifier (libfunc_name));
  if (!libfunc)
    internal_error ("UPC runtime function %s not found", libfunc_name);
  if (op_mode == BLKmode)
    {
      tree size = size_in_bytes (result_type);
      tree result_addr;
      result = create_tmp_var (result_type, 0);
      mark_addressable (result);
      result_addr = build_fold_addr_expr_loc (loc, result);
      lib_args = tree_cons (NULL_TREE, result_addr,
			    tree_cons (NULL_TREE, src_addr,
				       tree_cons (NULL_TREE, size,
						  NULL_TREE)));
      if (doprofcall)
	lib_args = upc_gasp_add_src_args (lib_args,
					  input_filename, input_line);
      lib_call = build_function_call (loc, libfunc, lib_args);
      gimplify_and_add (lib_call, pre_p);
    }
  else
    {
      lib_args = tree_cons (NULL_TREE, src_addr, NULL_TREE);
      if (doprofcall)
	lib_args = upc_gasp_add_src_args (lib_args,
					  input_filename, input_line);
      lib_call = build_function_call (loc, libfunc, lib_args);
      if (!lang_hooks.types_compatible_p (result_type, TREE_TYPE (lib_call)))
	lib_call = build1 (NOP_EXPR, result_type, lib_call);
      result = get_initialized_tmp_var (lib_call, pre_p, post_p);
    }
  return result;
}

/* Generate a call to the runtime to implement a 'put' into a shared
   object.  DEST_ADDR is a pointer-to-shared used to locate
   the destination in UPC shared memory.  SRC is the value
   to be stored into the destination.  */

static tree
upc_expand_put (location_t loc, tree dest_addr, tree src, gimple_seq * pre_p)
{
  tree type = TREE_TYPE (src);
  int strict_mode = TYPE_STRICT (type)
    || (!TYPE_RELAXED (type) && get_upc_consistency_mode ());
  int doprofcall = flag_upc_instrument && get_upc_pupc_mode ();
  optab put_op = (POINTER_SIZE == 64)
    ? (doprofcall ? (strict_mode ? xputsg_optab : xputg_optab)
       : (strict_mode ? xputs_optab : xput_optab))
    : (doprofcall ? (strict_mode ? putsg_optab : putg_optab)
       : (strict_mode ? puts_optab : put_optab));
  enum machine_mode mode = TYPE_MODE (type);
  enum machine_mode op_mode = (mode == TImode) ? BLKmode : mode;
  int is_src_shared = (TREE_SHARED (src)
		       || upc_shared_type_p (TREE_TYPE (src)));
  int is_shared_copy = (op_mode == BLKmode) && is_src_shared;
  const char *libfunc_name;
  tree src_addr, libfunc, lib_args, lib_call;
  dest_addr = get_initialized_tmp_var (dest_addr, pre_p, /* post_p */ NULL);
  /* runtime library expects internal rep. of UPC pointer-to-shared. */
  dest_addr = build1 (NOP_EXPR, upc_pts_rep_type_node, dest_addr);
  lib_args = tree_cons (NULL_TREE, dest_addr, NULL_TREE);
  if (op_mode == BLKmode)
    {
      if (!(is_src_shared && INDIRECT_REF_P (src))
	  && (!is_gimple_addressable (src)
	      || is_gimple_non_addressable (src)))
	{
	  /* We can't address the object - we have to copy
	     to a local (non-shared) temporary.  */
	  prepare_gimple_addressable (&src, pre_p);
	  mark_addressable (src);
	  is_shared_copy = 0;
	  is_src_shared = 0;
	}
    }
  if (is_shared_copy)
    libfunc_name = doprofcall
      ? (strict_mode ? "__copysgblk5" : "__copygblk5")
      : (strict_mode ? "__copysblk3" : "__copyblk3");
  else
    {
      rtx lib_op = optab_libfunc (put_op, op_mode);
      if (!lib_op)
	abort ();
      libfunc_name = XSTR (lib_op, 0);
    }
  libfunc = identifier_global_value (get_identifier (libfunc_name));
  if (!libfunc)
    internal_error ("UPC runtime function %s not found", libfunc_name);
  if (op_mode == BLKmode)
    {
      tree size = tree_expr_size (src);
      src_addr = build_fold_addr_expr_loc (loc, src);
      if (is_shared_copy)
	src_addr = fold (build1 (VIEW_CONVERT_EXPR,
				 upc_pts_rep_type_node, src_addr));
      lib_args = chainon (lib_args,
			  tree_cons (NULL_TREE, src_addr,
				     tree_cons (NULL_TREE, size, NULL_TREE)));
      if (doprofcall)
	lib_args = upc_gasp_add_src_args (lib_args, input_filename,
					  input_line);
    }
  else
    {
      tree src_type = lang_hooks.types.type_for_mode (mode, 0);
      tree libfunc_arg_types = TYPE_ARG_TYPES (TREE_TYPE (libfunc));
      tree put_arg_type = TREE_VALUE (TREE_CHAIN (libfunc_arg_types));
      if (TYPE_PRECISION (put_arg_type) != TYPE_PRECISION (src_type))
	internal_error ("%s: UPC put operation argument precision mismatch",
			libfunc_name);
      /* Avoid warnings about implicit conversion between
         actual parameter value's type, and the type of the
         runtime routine's parameter. */
      if (!lang_hooks.types_compatible_p (src_type, TREE_TYPE (src)))
	src = build1 (AGGREGATE_TYPE_P (TREE_TYPE (src))
		      ? VIEW_CONVERT_EXPR : NOP_EXPR, src_type, src);
      lib_args = chainon (lib_args, tree_cons (NULL_TREE, src, NULL_TREE));
      if (doprofcall)
	lib_args = upc_gasp_add_src_args (lib_args,
					  input_filename, input_line);
    }
  lib_call = build_function_call (loc, libfunc, lib_args);
  return lib_call;
}

/* If the tree pointed to by *EXPR_P is a compound lvalue,
   which yields a UPC shared value, then fetch the value
   into a regular (non UPC shared) temporary and
   use the temporary instead.  */

static int
upc_gimplify_lval (location_t loc, tree * expr_p,
		   gimple_seq * pre_p, gimple_seq * post_p,
		   fallback_t fallback ATTRIBUTE_UNUSED)
{
  const tree expr = *expr_p;
  const enum tree_code code = TREE_CODE (expr);
  const tree type = TREE_TYPE (expr);
  const tree op0 = TREE_OPERAND (expr, 0);
  const tree type0 = TREE_TYPE (op0);
  switch (code)
    {
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (op0 && TREE_SHARED (op0))
	return upc_gimplify_array_ref (loc, expr_p, pre_p, post_p);
      break;
    case BIT_FIELD_REF:
    case COMPONENT_REF:
      if (op0 && TREE_SHARED (op0))
	return upc_gimplify_field_ref (loc, expr_p, pre_p, post_p);
      break;
    case INDIRECT_REF:
      if (type0 && (TREE_CODE (type0) == POINTER_TYPE)
	  && upc_shared_type_p (TREE_TYPE (type0)))
	return upc_gimplify_indirect_ref (loc, expr_p, pre_p, post_p);
      break;
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (op0 && TREE_SHARED (op0))
	return upc_gimplify_real_image_ref (loc, expr_p, pre_p, post_p);
      break;
    case VAR_DECL:
      if (type && upc_shared_type_p (type))
	return upc_gimplify_shared_var_ref (loc, expr_p, pre_p, post_p);
      break;
    case VIEW_CONVERT_EXPR:
      if (type && upc_shared_type_p (type))
	TREE_TYPE (expr) = build_upc_unshared_type (type);
      gcc_assert (!TREE_SHARED (expr));
      break;
    default:
      gcc_unreachable ();
    }
  return GS_UNHANDLED;
}

/* Recurse through the chain of references in a compound lvalue
   and working from the innermost reference back up the chain,
   for each pointer value stored in UPC shared memory,
   fetch its value into a temporary.  This is done
   for both UPC pointers-to-shared and regular "C" pointers,
   so that the resulting chain of pointer references
   does not involve references to UPC shared memory.  */

static int
upc_gimplify_intermed_ref (tree * expr_p, gimple_seq * pre_p,
			   gimple_seq * post_p)
{
  const tree expr = *expr_p;
  const location_t loc = EXPR_LOCATION (expr);
  const enum tree_code code = TREE_CODE (expr);
  const tree type = TREE_TYPE (expr);
  int ret;
  if (code == INDIRECT_REF || handled_component_p (expr))
    {
      tree *op0_p = &TREE_OPERAND (expr, 0);
      ret = upc_gimplify_intermed_ref (op0_p, pre_p, post_p);
      if (ret != GS_OK)
	return ret;
      if (POINTER_TYPE_P (type)
	  && (TREE_SHARED (expr) || upc_shared_type_p (type)))
	{
	  const tree shared_addr = upc_shared_addr (loc, expr);
	  *expr_p = upc_expand_get (loc, shared_addr, pre_p, post_p);
	}
    }
  return GS_OK;
}

/* For each intermediate pointer value in the chain of references
   that is located in UPC shared memory, fetch its value into a regular
   (non- UPC shared) temporary and use the temporary instead.  */

static int
upc_gimplify_compound_lval (tree expr,
			    gimple_seq * pre_p, gimple_seq * post_p)
{
  const enum tree_code code = TREE_CODE (expr);
  if (code == INDIRECT_REF || handled_component_p (expr))
    {
      tree *op0_p = &TREE_OPERAND (expr, 0);
      return upc_gimplify_intermed_ref (op0_p, pre_p, post_p);
    }
  return GS_OK;
}

/* Return a BIT_FIELD_REF of type TYPE to refer to BITSIZE bits of INNER
   starting at BITPOS.  The field is unsigned if UNSIGNEDP is nonzero.  */

static tree
upc_make_bit_field_ref (tree inner, tree type, int bitsize, int bitpos)
{
  tree result;

  if (bitpos == 0)
    {
      tree size = TYPE_SIZE (TREE_TYPE (inner));
      if ((INTEGRAL_TYPE_P (TREE_TYPE (inner))
	   || POINTER_TYPE_P (TREE_TYPE (inner)))
	  && host_integerp (size, 0) && tree_low_cst (size, 0) == bitsize)
	return fold_convert (type, inner);
    }

  result = build3 (BIT_FIELD_REF, type, inner,
		   size_int (bitsize), bitsize_int (bitpos));

  return result;
}

/* On entry, EXP points to a shared field reference
   or array reference.  Simplify it so that is in
   one of two forms:
   1) (INDIRECT_REF <pointer to shared object>)
   2) (BIT_FIELD_REF (INDIRECT_REF <pointer to shared object>) ...)
   The first form is returned for all shared field references
   except those that require access to bit fields within
   a storage unit.  */

static tree
upc_simplify_shared_ref (location_t loc, tree exp)
{
  tree ref = 0;
  enum machine_mode mode;
  HOST_WIDE_INT bitsize;
  HOST_WIDE_INT bitpos;
  int unsignedp = 0;
  int volatilep = 0;
  tree type, inner, ptr_type, offset, base, t_offset;
  /* We need to construct a pointer-to-shared type that
     will be used to point to the component's value.  However,
     for a COMPONENT_REF, the original type will likely not have
     upc_shared_type_p() asserted, but rather, the expression node itself
     will have TREE_SHARED asserted.  We need to first propagate
     this information into a new shared type, which will in turn be
     used to build the required pointer-to-shared type.  Further,
     any pointer to a shared component must be constrained to have
     a blocking factor of zero.  */
  type = TREE_TYPE (exp);
  if (!upc_shared_type_p (type))
    {
      int quals = TYPE_QUALS (type) | TREE_QUALS (exp);
      gcc_assert (quals & TYPE_QUAL_SHARED);
      type = c_build_qualified_type (type, quals);
    }
  /* A pointer to a component of a shared structure
     must have a blocking factor of zero.  */
  inner = strip_array_types (type);
  if (!TYPE_BLOCK_FACTOR (inner)
      || !integer_zerop (TYPE_BLOCK_FACTOR (inner)))
    {
      type = build_variant_type_copy (type);
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree last = type;
	  while (TREE_CODE (TREE_TYPE (last)) == ARRAY_TYPE)
	    last = TREE_TYPE (last);
	  inner = build_variant_type_copy (TREE_TYPE (last));
	  /* Push the blocking factor down to the array
	     element type.  */
	  TYPE_BLOCK_FACTOR (inner) = integer_zero_node;
	  TREE_TYPE (last) = inner;
	}
    }
  ptr_type = build_pointer_type (type);
  base = get_inner_reference (exp, &bitsize, &bitpos, &offset,
			      &mode, &unsignedp, &volatilep, false);
  /* Calculate the base address, using the pointer type information
     derived above.  */
  if (TREE_CODE (base) == INDIRECT_REF)
    {
      /* remove indirection */
      base = TREE_OPERAND (base, 0);
      /* The conversion below may set the phase field to zero.
         This is the behavior we want, because subsequent
         address calculations will ignore the phase.  */
      if (TREE_TYPE (base) != ptr_type)
	base = convert (ptr_type, base);
    }
  else if (TREE_CODE (base) == VAR_DECL)
    base = upc_build_shared_var_addr (loc, ptr_type, base);
  else
    gcc_unreachable ();
  t_offset = size_int (bitpos / BITS_PER_UNIT);
  if (offset != 0)
    t_offset = build_binary_op (loc, PLUS_EXPR, t_offset, offset, 0);
  /* Make base point to the specified field. */
  if (!integer_zerop (t_offset))
    base = (*upc_pts.add_offset) (loc, base, t_offset);
  /* Convert base back to an indirect ref. */
  ref = build_fold_indirect_ref_loc (loc, base);
  if (((bitpos % BITS_PER_UNIT) != 0) || ((bitsize % BITS_PER_UNIT) != 0))
    {
      tree field_type = lang_hooks.types.type_for_mode (mode, unsignedp);
      /* We need to return a bit field reference. */
      ref = upc_make_bit_field_ref (ref, field_type, bitsize, bitpos);
    }
  return ref;
}

/* Return the UPC shared address of the lvalue
   identified by EXP.  */

static tree
upc_shared_addr (location_t loc, tree exp)
{
  const enum tree_code code = TREE_CODE (exp);
  const tree type = TREE_TYPE (exp);
  tree addr, ref;
  switch (code)
    {
    case VAR_DECL:
      addr = upc_build_shared_var_addr (loc, build_pointer_type (type), exp);
      break;
    case INDIRECT_REF:
      addr = TREE_OPERAND (exp, 0);
      break;
    case ARRAY_REF:
    case COMPONENT_REF:
    case BIT_FIELD_REF:
      ref = upc_simplify_shared_ref (loc, exp);
      if (TREE_CODE (ref) == ERROR_MARK)
	return ref;
      if (TREE_CODE (ref) == BIT_FIELD_REF)
	{
	  error ("accesses to UPC shared bit fields "
		 "are not yet implemented");
	  return error_mark_node;
	}
      /* Remove the indirection by taking the address and simplifying.  */
      gcc_assert (INDIRECT_REF_P (ref));
      addr = build_fold_addr_expr_loc (loc, ref);
      break;
    default:
      gcc_unreachable ();
    }
  return addr;
}

/* Rewrite a 'upc_forall' statement into a regular 'for' statement
   and them gimplify the result. */

static int
upc_gimplify_forall_stmt (tree * expr_p ATTRIBUTE_UNUSED,
			  gimple_seq * pre_p ATTRIBUTE_UNUSED,
			  gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  /* The operands are UPC_FORALL_INIT_STMT, UPC_FORALL_COND,
     UPC_FORALL_EXPR, UPC_FORALL_BODY, and UPC_FORALL_AFFINITY 
     respectively. */
  /* handled in c-parser.c */
  internal_error ("upc_gimplify_forall_stmt not yet implemented");
  return GS_UNHANDLED;
}

/* Rewrite a UPC synchronization statement (upc_wait, upc_notify,
   and upc_barrier) into a call to the runtime. */

static int
upc_gimplify_sync_stmt (location_t loc,
			tree * stmt_p,
			gimple_seq * pre_p ATTRIBUTE_UNUSED,
			gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  /* The first operand is the synchronization operation, UPC_SYNC_OP:
     UPC_SYNC_NOTIFY_OP 1       Notify operation
     UPC_SYNC_WAIT_OP           2       Wait operation
     UPC_SYNC_BARRIER_OP        3       Barrier operation
     The second operand, UPC_SYNC_ID is the (optional) expression
     whose value specifies the barrier identifier which is checked
     by the various synchronization operations. */
  tree stmt = *stmt_p;
  tree sync_op = UPC_SYNC_OP (stmt);
  tree sync_id = UPC_SYNC_ID (stmt);
  const int op = (int) tree_low_cst (sync_op, 1);
  const char *libfunc_name = (char *) 0;
  int doprofcall = flag_upc_instrument && get_upc_pupc_mode ();
  tree libfunc, lib_args;
  switch (op)
    {
    case UPC_SYNC_NOTIFY_OP:
      libfunc_name = doprofcall ? UPC_NOTIFYG_LIBCALL : UPC_NOTIFY_LIBCALL;
      break;
    case UPC_SYNC_WAIT_OP:
      libfunc_name = doprofcall ? UPC_WAITG_LIBCALL : UPC_WAIT_LIBCALL;
      break;
    case UPC_SYNC_BARRIER_OP:
      libfunc_name = doprofcall ? UPC_BARRIERG_LIBCALL : UPC_BARRIER_LIBCALL;
      break;
    default:
      gcc_unreachable ();
    }
  libfunc = identifier_global_value (get_identifier (libfunc_name));
  if (!libfunc)
    internal_error ("UPC runtime function %s not found", libfunc_name);
  if (!sync_id)
    sync_id = build_int_cst (NULL_TREE, INT_MIN);
  lib_args = tree_cons (NULL_TREE, sync_id, NULL_TREE);
  if (doprofcall)
    lib_args = upc_gasp_add_src_args (lib_args, input_filename, input_line);
  *stmt_p = build_function_call (loc, libfunc, lib_args);
  return GS_UNHANDLED;
}

/* Rewrite a reference to a UPC shared variable into a call
   to the runtime to perform a 'get' operation.  */

static int
upc_gimplify_shared_var_ref (location_t loc, tree * expr_p,
			     gimple_seq * pre_p, gimple_seq * post_p)
{
  tree src_addr = build_unary_op (loc, ADDR_EXPR, *expr_p, 1);
  *expr_p = upc_expand_get (loc, src_addr, pre_p, post_p);
  return GS_OK;
}

/* Expand & of a UPC shared object into equivalent code. */

static int
upc_gimplify_addr_expr (location_t loc,
			tree * expr_p,
			gimple_seq * pre_p ATTRIBUTE_UNUSED,
			gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  const tree exp = *expr_p;
  const tree op0 = TREE_OPERAND (exp, 0);
  *expr_p = upc_shared_addr (loc, op0);
  return GS_OK;
}

/* TODO: start commenting here.  */
static int
upc_gimplify_indirect_ref (location_t loc, tree * expr_p,
			   gimple_seq * pre_p, gimple_seq * post_p)
{
  tree src_addr;
  /* drop the indirect ref. to obtain the address of the
     shared object. */
  src_addr = TREE_OPERAND (*expr_p, 0);
  *expr_p = upc_expand_get (loc, src_addr, pre_p, post_p);
  return GS_OK;
}

static int
upc_gimplify_real_image_ref (location_t loc ATTRIBUTE_UNUSED,
			     tree * expr_p ATTRIBUTE_UNUSED,
			     gimple_seq * pre_p ATTRIBUTE_UNUSED,
			     gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
  return GS_OK;
}

/* Rewrite a[i] into *(a + i).  This code is taken from
   build_array_ref.  We could call build_array_ref
   directly, and depend upon it to do this rewrite if
   the array is shared, but it is clearer to handle
   it explicitly here.  */

static int
upc_gimplify_array_ref (location_t loc,
			tree * expr_p,
			gimple_seq * pre_p ATTRIBUTE_UNUSED,
			gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  tree exp = *expr_p;
  tree array = TREE_OPERAND (exp, 0);
  tree index = TREE_OPERAND (exp, 1);
  tree ar = default_conversion (array);
  gcc_assert (TREE_CODE (exp) == ARRAY_REF);
  if (ar == error_mark_node)
    return GS_ERROR;
  gcc_assert (TREE_CODE (TREE_TYPE (ar)) == POINTER_TYPE);
  gcc_assert (TREE_CODE (TREE_TYPE (TREE_TYPE (ar))) != FUNCTION_TYPE);
  *expr_p = build_indirect_ref (loc,
				build_binary_op (loc, PLUS_EXPR, ar, index,
						 0), RO_ARRAY_INDEXING);
  return GS_OK;
}

/* Handle conversions between UPC pointers-to-shared and
   local pointers, or between UPC pointers-to-shared which
   have differing block factors.  */

static int
upc_gimplify_pts_cvt (location_t loc,
		      tree * expr_p,
		      gimple_seq * pre_p ATTRIBUTE_UNUSED,
		      gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  *expr_p = (*upc_pts.cvt) (loc, *expr_p);
  return GS_OK;
}

/* Rewrite op0 CMP op1 into either a bitwise
   comparison of the UPC pointer-to-shared operands
   or by taking the difference, and comparing it
   to zero. */

static int
upc_gimplify_pts_cond_expr (location_t loc,
			    tree * expr_p,
			    gimple_seq * pre_p ATTRIBUTE_UNUSED,
			    gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  *expr_p = (*upc_pts.cond_expr) (loc, *expr_p);
  return GS_OK;
}

/* Rewrite a reference to a bit field within a UPC shared struct/union.
   When implemented, the translated tree will need to fetch
   the container for this bit-field from UPC shared memory,
   and then isolate the bit field within the container.  */

static int
upc_gimplify_field_ref (location_t loc,
			tree * expr_p,
			gimple_seq * pre_p ATTRIBUTE_UNUSED,
			gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  tree ref = *expr_p;
  ref = upc_simplify_shared_ref (loc, ref);
  if (TREE_CODE (ref) == BIT_FIELD_REF)
    {
      error ("accesses to UPC shared bit fields "
             "are not yet implemented");
      ref = error_mark_node;
    }
  *expr_p = ref;
  return GS_OK;
}

/* Expand the addition of UPC pointer-to-shared value and an integer.
   When the operation is subtraction, rewrite the expression (p - i)
   into (p + (-i)) and expand the sum.  The special handling of
   subtraction is required because addition/subtraction of UPC
   pointers-to-shared is a non-trivial operation, and it simpler
   to only implement addition.  */

static int
upc_gimplify_pts_arith_expr (location_t loc,
			     tree * expr_p,
			     gimple_seq * pre_p ATTRIBUTE_UNUSED,
			     gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  tree exp = *expr_p;
  if (TREE_CODE (exp) == PLUS_EXPR || TREE_CODE (exp) == POINTER_PLUS_EXPR)
    {
      *expr_p = (*upc_pts.sum) (loc, exp);
    }
  else if (TREE_CODE (exp) == MINUS_EXPR)
    {
      const tree type0 = TREE_TYPE (TREE_OPERAND (exp, 0));
      const tree type1 = TREE_TYPE (TREE_OPERAND (exp, 1));
      if ((TREE_CODE (type0) == POINTER_TYPE)
	  && (TREE_CODE (type1) == INTEGER_TYPE))
	{
	  /* Rewrite the expression p - i into p + (-i),
	     and expand the sum. */
	  tree int_op = TREE_OPERAND (exp, 1);
	  if (TREE_CODE (int_op) == INTEGER_CST
	      && TREE_CODE (TREE_TYPE (int_op)) == POINTER_TYPE)
	    {
	      /* Earlier passes have altered the type of the integer
	         constant to be a UPC pointer-to-shared type.  This won't
	         play well when we try to negate it. For now, convert
	         it back to a size type. */
	      int_op = ssize_int (tree_low_cst (int_op, 0));
	    }
	  TREE_SET_CODE (exp, PLUS_EXPR);
	  /* Make sure that int_op is a signed type to
	     ensure negation works properly.  */
	  if (TYPE_UNSIGNED (TREE_TYPE (int_op)))
	    int_op = convert (ssizetype, int_op);
	  TREE_OPERAND (exp, 1) =
	    build_unary_op (loc, NEGATE_EXPR, int_op, 0);
	  *expr_p = (*upc_pts.sum) (loc, exp);
	}
      else
	*expr_p = (*upc_pts.diff) (loc, exp);
    }
  else
    gcc_unreachable ();
  return GS_OK;
}

/* Rewrite the increment/decrement of a UPC shared value into
   an equivalent assignment statement.  (Although some future
   implementations of the UPC runtime API might be able to
   implement these operations atomically, that is not currently
   defined in the runtime API.)  */

static int
upc_gimplify_shared_inc_dec_expr (location_t loc,
				  tree * expr_p,
				  gimple_seq * pre_p ATTRIBUTE_UNUSED,
				  gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  const tree exp = *expr_p;
  const enum tree_code code = TREE_CODE (exp);
  const tree op0 = TREE_OPERAND (exp, 0);
  const tree type = TYPE_MAIN_VARIANT (TREE_TYPE (op0));
  enum tree_code inc_dec_code;
  tree val = get_initialized_tmp_var (op0, pre_p, post_p);
  tree result, mod_expr;
  switch (code)
    {
    case POSTDECREMENT_EXPR:
      result = val;
      inc_dec_code = MINUS_EXPR;
      break;
    case POSTINCREMENT_EXPR:
      result = val;
      inc_dec_code = PLUS_EXPR;
      break;
    case PREDECREMENT_EXPR:
      result = op0;
      inc_dec_code = MINUS_EXPR;
      break;
    case PREINCREMENT_EXPR:
      result = op0;
      inc_dec_code = PLUS_EXPR;
      break;
    default:
      gcc_unreachable ();
    }
  mod_expr = build2 (MODIFY_EXPR, type, op0,
		     build_binary_op (loc, inc_dec_code,
				      val, integer_one_node, 0));
  gimplify_and_add (mod_expr, pre_p);
  *expr_p = result;
  return GS_OK;
}

/* Return TRUE if any of the arguments to the call expression
   given by EPXR are UPC shared values (ie, they have
   their TREE_SHARED bit set).  */

static int
upc_call_has_shared_args (tree expr)
{
  const unsigned int nargs = call_expr_nargs (expr);
  unsigned int i;
  /* Finally, gimplify the function arguments.  */
  for (i = 0; i < nargs; i++)
    {
      tree arg = CALL_EXPR_ARG (expr, i);
      if (TREE_SHARED (arg))
	return 1;
    }
  return 0;
}

/* Copy any UPC shared actual argument values to a local
   temporary, so that they can be passed to the
   called function.  */

static int
upc_gimplify_call_expr (tree * expr_p, gimple_seq * pre_p)
{
  const tree expr = *expr_p;
  const unsigned int nargs = call_expr_nargs (expr);
  int i;
  for (i = (PUSH_ARGS_REVERSED ? nargs - 1 : 0);
       PUSH_ARGS_REVERSED ? i >= 0 : i < nargs;
       PUSH_ARGS_REVERSED ? i-- : i++)
    {
      tree *arg = &CALL_EXPR_ARG (expr, i);
      if (TREE_SHARED (*arg))
	{
	  /* post_p is NULL to ensure that side-effects occur
	     before the CALL (see gimplify_arg).  */
	  *arg = get_initialized_tmp_var (*arg, pre_p, NULL);
	}
    }
  return GS_OK;
}

/* Simplify assignments to generic pointer-to-shared objects,
   where an intermediate conversion appears on the right hand
   side of the assignment.  Conversions from non-generic
   pointer-to-shared to generic pointer-to-shared are preserved
   up to the point of the final assignment, because a conversion
   from generic pointer-to-shared to non-generic pointer-to-shared
   may reset the phase in some cases, and we can only determine
   that the conversion is unnecessary when we know the target
   of the assignment expression.  */

static void
upc_strip_useless_generic_pts_cvt (tree * expr_p)
{
  while (TREE_CODE (*expr_p) == CONVERT_EXPR
	 && POINTER_TYPE_P (TREE_TYPE (*expr_p))
	 && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (*expr_p))))
    {
      *expr_p = TREE_OPERAND (*expr_p, 0);
    }
}

/* Gimplify the MODIFY_EXPR node of the 'shared' value
   pointed to by EXPR_P.  Called via a language hook,
   early in the processing of gimplify_modify_expr().
   If the target of the assignment is a UPC 'shared'
   reference, or an indirection via a UPC pointer-to-shared,
   the assignment statement is rewritten into a call
   to a runtime routine that does a remote 'put'.
   If WANT_VALUE is asserted, we must first
   copy the right-hand-side to a temporary variable,
   and return the temporary.  PRE_P and POST_P have the conventional
   definitions of pre- and post- actions.  */

static int
upc_gimplify_modify_expr (location_t loc, tree * expr_p,
			  gimple_seq * pre_p, gimple_seq * post_p,
			  bool want_value)
{
  const tree dest = TREE_OPERAND (*expr_p, 0);
  tree src = TREE_OPERAND (*expr_p, 1);
  int ret;
  ret = upc_gimplify_compound_lval (dest, pre_p, post_p);
  if (ret != GS_OK)
    return ret;
  if (TREE_SHARED (dest)
      || (TREE_TYPE (dest) && upc_shared_type_p (TREE_TYPE (dest))))
    {
      /* <shared dest> = <(shared|unshared) src> */
      const tree dest_addr = build_fold_addr_expr_loc (loc, dest);
      tree put_op, result;
      if (want_value)
	src = get_initialized_tmp_var (src, pre_p, post_p);
      put_op = upc_expand_put (loc, dest_addr, src, pre_p);
      if (want_value)
	{
	  gimplify_and_add (put_op, pre_p);
	  result = src;
	}
      else
	result = put_op;
      *expr_p = result;
    }
  else if (TREE_SHARED (src)
	   || (TREE_TYPE (src) && upc_shared_type_p (TREE_TYPE (src))))
    {
      /* <unshared dest> = <shared src> */
      const tree src_addr = upc_shared_addr (loc, src);
      src = upc_expand_get (loc, src_addr, pre_p, post_p);
      TREE_OPERAND (*expr_p, 1) = src;
    }
  return GS_OK;
}

/* Language hook to gimplify UPC specific constructs.
   This routine looks for tree nodes that will likely
   require a UPC-specific re-write, and then calls
   the appropriate translation function.  */

int
upc_gimplify_expr (tree * expr_p,
		   gimple_seq * pre_p, gimple_seq * post_p,
		   bool (*gimple_test_f) (tree) ATTRIBUTE_UNUSED,
		   int fallback)
{
  const tree expr = *expr_p;
  const location_t loc = EXPR_LOCATION (expr);
  const enum tree_code code = TREE_CODE (expr);
  const tree type = TREE_TYPE (expr);
  const tree op0 = (TREE_CODE_LENGTH (code) >= 1)
    ? TREE_OPERAND (expr, 0) : NULL_TREE;
  const tree type0 = (op0 != NULL_TREE) ? TREE_TYPE (op0) : NULL_TREE;
  tree op1 = (TREE_CODE_LENGTH (code) >= 2)
    ? TREE_OPERAND (expr, 1) : NULL_TREE;
  tree type1 = (op1 != NULL_TREE) ? TREE_TYPE (op1) : NULL_TREE;
  int ret;
  switch (code)
    {
    case UPC_FORALL_STMT:
      return upc_gimplify_forall_stmt (expr_p, pre_p, post_p);

    case UPC_SYNC_STMT:
      return upc_gimplify_sync_stmt (loc, expr_p, pre_p, post_p);

    case VAR_DECL:
      if (type && upc_shared_type_p (type))
	return upc_gimplify_shared_var_ref (loc, expr_p, pre_p, post_p);
      break;

    case ADDR_EXPR:
      if (POINTER_TYPE_P (type) && TREE_TYPE (type)
	  && upc_shared_type_p (TREE_TYPE (type)))
	return upc_gimplify_addr_expr (loc, expr_p, pre_p, post_p);
      break;

    case INDIRECT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
    case VIEW_CONVERT_EXPR:
      ret = upc_gimplify_compound_lval (expr, pre_p, post_p);
      if (ret != GS_OK)
	return ret;
      return upc_gimplify_lval (loc, expr_p, pre_p, post_p, fallback);
      break;

    case BIT_FIELD_REF:
      if (op0 && TREE_SHARED (op0))
	return upc_gimplify_field_ref (loc, expr_p, pre_p, post_p);
      break;

    case NON_LVALUE_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
      /* Conversions to a UPC shared type aren't valid.
         The front-end will sometimes convert
         an expression operand to the type of another
         operand.  If that operand has UPC shared type,
         then the conversion target type is 'shared' qualified.
         We unshare the type in order to produce a
         valid tree.  */
      if (type && upc_shared_type_p (type))
	TREE_TYPE (expr) = build_upc_unshared_type (type);
      if (upc_pts_cvt_op_p (expr))
	return upc_gimplify_pts_cvt (loc, expr_p, pre_p, post_p);
      break;

    case EQ_EXPR:
    case GE_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case LT_EXPR:
    case NE_EXPR:
      if ((type0 && (TREE_CODE (type0) == POINTER_TYPE)
	   && upc_shared_type_p (TREE_TYPE (type0)))
	  || (type1 && (TREE_CODE (type1) == POINTER_TYPE)
	      && upc_shared_type_p (TREE_TYPE (type1))))
	return upc_gimplify_pts_cond_expr (loc, expr_p, pre_p, post_p);
      break;

    case MINUS_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      if ((type0 && (TREE_CODE (type0) == POINTER_TYPE)
	   && upc_shared_type_p (TREE_TYPE (type0)))
	  || (type1 && (TREE_CODE (type1) == POINTER_TYPE)
	      && upc_shared_type_p (TREE_TYPE (type1))))
	return upc_gimplify_pts_arith_expr (loc, expr_p, pre_p, post_p);
      break;

    case CALL_EXPR:
      if (upc_call_has_shared_args (expr))
	return upc_gimplify_call_expr (expr_p, pre_p);
      break;

    case MODIFY_EXPR:
    case INIT_EXPR:
      if (POINTER_TYPE_P (type0) && upc_shared_type_p (TREE_TYPE (type0))
	  && VOID_TYPE_P (TREE_TYPE (type0))
	  && TREE_CODE (op1) == CONVERT_EXPR
	  && POINTER_TYPE_P (type1) && VOID_TYPE_P (TREE_TYPE (type1)))
	{
	  upc_strip_useless_generic_pts_cvt (&TREE_OPERAND (expr, 1));
	  /* Recalculate op1 and type1 because TREE_OPERAND (expr, 1)
	     was rewritten, above.  */
	  op1 = TREE_OPERAND (expr, 1);
	  type1 = TREE_TYPE (op1);
	}
      if ((op0 && (TREE_SHARED (op0)
		   || (type0 && upc_shared_type_p (type0))))
	  || (op1 && (TREE_SHARED (op1)
		      || (type1 && upc_shared_type_p (type1)))))
	return upc_gimplify_modify_expr (loc, expr_p, pre_p, post_p,
					 fallback != fb_none);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if ((op0 && TREE_SHARED (op0))
	  || (type0 && (upc_shared_type_p (type0)
			|| (POINTER_TYPE_P (type0)
			    && upc_shared_type_p (TREE_TYPE (type0))))))
	return upc_gimplify_shared_inc_dec_expr (loc, expr_p, pre_p, post_p);
      break;

    case INTEGER_CST:
      /* Integer constants can't be UPC 'shared' qualified.
         The front-end can create integer constants with shared
         type when changing the type to agree with that of another
         expression operand.

         Unsharing an integer constant requires special handling
         because an internal hash table is kept on a type by type
         basis.  Thus, we can't rewrite the type directly.
         We re-create the constant with its unshared type to
         ensure that the hash table is updated.  */
      if (type && upc_shared_type_p (type))
	{
	  const tree u_type = build_upc_unshared_type (type);
	  *expr_p = build_int_cst_wide (u_type,
					TREE_INT_CST_LOW (expr),
					TREE_INT_CST_HIGH (expr));
	}
      gcc_assert (!TREE_SHARED (expr));
      break;

    case REAL_CST:
    case COMPLEX_CST:
    case STRING_CST:
    case VECTOR_CST:
    case CONSTRUCTOR:
      /* A constant's type can't be UPC 'shared' qualified.
         The front-end will sometimes convert
         an expression operand to the type of another
         operand.  If that other operand has UPC shared type,
         then the converted constant's type will be shared.
         We unshare the type in order to produce a
         valid constant.  */
      if (type && upc_shared_type_p (type))
	TREE_TYPE (expr) = build_upc_unshared_type (type);
      gcc_assert (!TREE_SHARED (expr));
      break;

    default:
      break;
    }

  /* If it isn't a UPC construct, let the regular C language hook
     have a try.  */
  return c_gimplify_expr (expr_p, pre_p, post_p, gimple_test_f, fallback);
}

/* Convert the tree representation of FNDECL built by the UPC front-end
   into GENERIC trees.  */

void
upc_genericize (tree fndecl)
{
  /* Take care of C-specific actions first.
     Normally, we'd do this after the language-specific
     actions, but c_genericize is only a dumping pass
     now, and should be renamed.  */
  c_genericize (fndecl);
  /* Perform a full gimplify pass, because the UPC lowering rewrites
     are implemented using the gimplify framework.  (The default
     bitmap is used by the gimplify pass and must be initialized.) */
  bitmap_obstack_initialize (NULL);
  gimplify_function_tree (fndecl);
  bitmap_obstack_release (NULL);
}
