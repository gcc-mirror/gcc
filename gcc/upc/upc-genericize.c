/* upc-genericize.c: UPC language specific tree lowering pass
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
#include "timevar.h"
#include "tree-check.h"
#include "tree-flow.h"
#include "tree-iterator.h"
#include "ggc.h"
#include "target.h"
#include "upc-tree.h"
#include "upc-act.h"
#include "upc-pts.h"
#include "upc-rts-names.h"
#include "upc-gasp.h"
#include "upc-genericize.h"
#include "langhooks.h"

static tree upc_expand_get (location_t, tree);
static tree upc_expand_put (location_t, tree, tree, int);
static tree upc_create_tmp_var (tree);
static tree upc_copy_value_to_tmp_var (tree *, tree);
static tree upc_make_bit_field_ref (tree, tree, int, int);
static tree upc_shared_addr (location_t, tree);
static tree upc_simplify_shared_ref (location_t, tree);
static void upc_strip_useless_generic_pts_cvt (tree *);

static void upc_genericize_addr_expr (location_t, tree *);
static void upc_genericize_array_ref (location_t, tree *);
static void upc_genericize_compound_expr (tree *, int);
static void upc_genericize_cond_expr (tree *, int);
static void upc_genericize_decl_expr (tree *);
static tree upc_genericize_expr (tree *, int *, void *);
static void upc_genericize_field_ref (location_t, tree *);
static void upc_genericize_forall_stmt (tree *);
static void upc_genericize_indirect_ref (location_t, tree *);
static void upc_genericize_modify_expr (location_t, tree *, int);
static void upc_genericize_pts_arith_expr (location_t, tree *);
static void upc_genericize_pts_cond_expr (location_t, tree *);
static void upc_genericize_pts_cvt (location_t, tree *);
static void upc_genericize_real_image_ref (location_t, tree *);
static void upc_genericize_shared_inc_dec_expr (location_t, tree *, int);
static void upc_genericize_shared_var_ref (location_t, tree *);
static void upc_genericize_stmt (tree *, int);
static void upc_genericize_stmt_list (tree *);
static void upc_genericize_sync_stmt (location_t, tree *);

/* Create a new temporary variable declaration of type TYPE.
   Push the variable into the current function binding.  */

static tree
upc_create_tmp_var (tree type)
{
  tree tmp_var;
  /* We don't allow types that are addressable or incomplete.  */
  gcc_assert (!TREE_ADDRESSABLE (type) && COMPLETE_TYPE_P (type));
  /* The temp. cannot be shared qualified.  */
  if (upc_shared_type_p (type))
    type = build_upc_unshared_type (type);
  tmp_var = create_tmp_var_raw (type, "UPC");
  DECL_CONTEXT (tmp_var) = current_function_decl;
  DECL_SEEN_IN_BIND_EXPR_P (tmp_var) = 1;
  /* record_vars() will do what we want, but not if GIMPLE_DF is set.
     (since we haven't gimplified yet, it should not be set).  */
  gcc_assert (!cfun->gimple_df);
  record_vars (tmp_var);
  return tmp_var;
}

/* Create a temporary variable and initialize it to VAL.
   The expression used initialized the temporary value
   is returned in *VAL_EXPR.  */

static tree
upc_copy_value_to_tmp_var (tree *val_expr, tree val)
{
  const tree type = TREE_TYPE (val);
  const tree tmp_var = upc_create_tmp_var (type);
  *val_expr = build2 (INIT_EXPR, type, tmp_var, val);
  SET_EXPR_LOCATION (*val_expr, EXPR_LOC_OR_HERE (val));
  return tmp_var;
}

/* Generate a call to the runtime to implement a 'get' of a shared
   object.  SRC_ADDR is a pointer to UPC shared value that references
   the shared object.  */

static tree
upc_expand_get (location_t loc, tree src_addr)
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
      tree result_tmp, result_addr;
      result_tmp = upc_create_tmp_var (result_type);
      TREE_ADDRESSABLE (result_tmp) = 1;
      result_addr = build_fold_addr_expr_loc (loc, result_tmp);
      lib_args = tree_cons (NULL_TREE, result_addr,
			    tree_cons (NULL_TREE, src_addr,
				       tree_cons (NULL_TREE, size,
						  NULL_TREE)));
      if (doprofcall)
	lib_args = upc_gasp_add_src_args (lib_args,
					  input_filename, input_line);
      lib_call = build_function_call (loc, libfunc, lib_args);
      result = build2 (COMPOUND_EXPR, result_type, lib_call, result_tmp);
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
      result = lib_call;
    }
  return result;
}

/* Generate a call to the runtime to implement a 'put' into a shared
   object.  DEST_ADDR is a pointer-to-shared used to locate
   the destination in UPC shared memory.  SRC is the value
   to be stored into the destination.  If WANT_VALUE is set,
   then return a compound expression that evaluates to
   the value or SRC.  */

static tree
upc_expand_put (location_t loc, tree dest_addr, tree src, int want_value)
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
  int local_copy = want_value
    || (op_mode == BLKmode
	&& !(is_src_shared && INDIRECT_REF_P (src))
	&& (!is_gimple_addressable (src) || is_gimple_non_addressable (src)));
  int is_shared_copy = !local_copy && (op_mode == BLKmode) && is_src_shared;
  const char *libfunc_name;
  tree libfunc, lib_args, src_tmp_init_expr, result;
  if (local_copy)
    src = upc_copy_value_to_tmp_var (&src_tmp_init_expr, src);
  /* The runtime library expects the internal rep. of
     of a UPC pointer-to-shared. */
  dest_addr = build1 (NOP_EXPR, upc_pts_rep_type_node, dest_addr);
  lib_args = tree_cons (NULL_TREE, dest_addr, NULL_TREE);
  if (is_shared_copy)
    libfunc_name = doprofcall
      ? (strict_mode ? "__copysgblk5" : "__copygblk5")
      : (strict_mode ? "__copysblk3" : "__copyblk3");
  else
    {
      rtx lib_op = optab_libfunc (put_op, op_mode);
      gcc_assert (lib_op);
      libfunc_name = XSTR (lib_op, 0);
    }
  libfunc = identifier_global_value (get_identifier (libfunc_name));
  if (!libfunc)
    internal_error ("UPC runtime function %s not found", libfunc_name);
  if (op_mode == BLKmode)
    {
      const tree size = tree_expr_size (src);
      tree src_addr = build_fold_addr_expr_loc (loc, src);
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
  result = build_function_call (loc, libfunc, lib_args);
  if (want_value)
    result = build2 (COMPOUND_EXPR, TREE_TYPE (src), result, src);
  if (local_copy)
    result = build2 (COMPOUND_EXPR, TREE_TYPE (result),
		     src_tmp_init_expr, result);
  return result;
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
  if (INDIRECT_REF_P (base))
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
      ref = upc_simplify_shared_ref (loc, exp);
      if (TREE_CODE (ref) == ERROR_MARK)
	return ref;
      /* Remove the indirection by taking the address and simplifying.  */
      gcc_assert (INDIRECT_REF_P (ref));
      addr = build_fold_addr_expr_loc (loc, ref);
      break;
    case BIT_FIELD_REF:
      error ("invalid & operation applied to a UPC shared bit field");
      return error_mark_node;
    default:
      gcc_unreachable ();
    }
  return addr;
}

/* Rewrite a 'upc_forall' statement into a
   regular 'for' statement.  */

static void
upc_genericize_forall_stmt (tree *expr_p ATTRIBUTE_UNUSED)
{
  /* Handled in c-parser.c.  */
  gcc_unreachable ();
}

/* Rewrite a UPC synchronization statement (upc_wait, upc_notify,
   and upc_barrier) into a call to the runtime. */

static void
upc_genericize_sync_stmt (location_t loc, tree *stmt_p)
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
}

/* Rewrite a reference to a UPC shared variable into a call
   to the runtime to perform a 'get' operation.  */

static void
upc_genericize_shared_var_ref (location_t loc, tree *expr_p)
{
  tree src_addr = build_unary_op (loc, ADDR_EXPR, *expr_p, 1);
  *expr_p = upc_expand_get (loc, src_addr);
}

/* Expand & of a UPC shared object into equivalent code. */

static void
upc_genericize_addr_expr (location_t loc, tree *expr_p)
{
  const tree exp = *expr_p;
  const tree op0 = TREE_OPERAND (exp, 0);
  *expr_p = upc_shared_addr (loc, op0);
}

static void
upc_genericize_indirect_ref (location_t loc, tree *expr_p)
{
  tree src_addr;
  /* drop the indirect ref. to obtain the address of the
     shared object. */
  src_addr = TREE_OPERAND (*expr_p, 0);
  *expr_p = upc_expand_get (loc, src_addr);
}

static void
upc_genericize_real_image_ref (location_t loc ATTRIBUTE_UNUSED,
			       tree *expr_p ATTRIBUTE_UNUSED)
{
  /* not yet implemented */
  gcc_unreachable ();
}

/* Rewrite a[i] into *(a + i).  This code is taken from
   build_array_ref.  We could call build_array_ref
   directly, and depend upon it to do this rewrite if
   the array is shared, but it is clearer to handle
   it explicitly here.  */

static void
upc_genericize_array_ref (location_t loc, tree *expr_p)
{
  tree exp = *expr_p;
  tree array = TREE_OPERAND (exp, 0);
  tree index = TREE_OPERAND (exp, 1);
  tree ar = default_conversion (array);
  gcc_assert (TREE_CODE (exp) == ARRAY_REF);
  if (ar == error_mark_node)
    return;
  gcc_assert (TREE_CODE (TREE_TYPE (ar)) == POINTER_TYPE);
  gcc_assert (TREE_CODE (TREE_TYPE (TREE_TYPE (ar))) != FUNCTION_TYPE);
  *expr_p = build_indirect_ref (loc,
				build_binary_op (loc, PLUS_EXPR, ar, index,
						 0), RO_ARRAY_INDEXING);
}

/* Handle conversions between UPC pointers-to-shared and
   local pointers, or between UPC pointers-to-shared which
   have differing block factors.  */

static void
upc_genericize_pts_cvt (location_t loc, tree *expr_p)
{
  *expr_p = (*upc_pts.cvt) (loc, *expr_p);
}

/* Rewrite op0 CMP op1 into either a bitwise
   comparison of the UPC pointer-to-shared operands
   or by taking the difference, and comparing it
   to zero. */

static void
upc_genericize_pts_cond_expr (location_t loc, tree *expr_p)
{
  *expr_p = (*upc_pts.cond_expr) (loc, *expr_p);
}

/* Rewrite a reference to a bit field within a UPC shared struct/union.
   When implemented, the translated tree will need to fetch
   the container for this bit-field from UPC shared memory,
   and then isolate the bit field within the container.  */

static void
upc_genericize_field_ref (location_t loc, tree *expr_p)
{
  tree ref = *expr_p;
  ref = upc_simplify_shared_ref (loc, ref);
  if (TREE_CODE (ref) == BIT_FIELD_REF)
    {
      error ("accesses to UPC shared bit fields " "are not yet implemented");
      ref = error_mark_node;
    }
  gcc_assert (INDIRECT_REF_P (ref));
  *expr_p = upc_expand_get (loc, TREE_OPERAND (ref, 0));
}

/* Expand the addition of UPC pointer-to-shared value and an integer.
   When the operation is subtraction, rewrite the expression (p - i)
   into (p + (-i)) and expand the sum.  The special handling of
   subtraction is required because addition/subtraction of UPC
   pointers-to-shared is a non-trivial operation, and it simpler
   to only implement addition.  */

static void
upc_genericize_pts_arith_expr (location_t loc, tree *expr_p)
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
}

/* Rewrite the increment/decrement of a UPC shared value into
   an equivalent assignment statement.  (Although some future
   implementations of the UPC runtime API might be able to
   implement these operations atomically, that is not currently
   defined in the runtime API.)  If WANT_VALUE is set, then
   generate a compound expression that yields the appropriate value.  */

static void
upc_genericize_shared_inc_dec_expr (location_t loc, tree *expr_p,
				    int want_value)
{
  const tree exp = *expr_p;
  const enum tree_code code = TREE_CODE (exp);
  const tree op0 = TREE_OPERAND (exp, 0);
  const enum tree_code inc_dec_code = (code == POSTINCREMENT_EXPR
				       || code == PREINCREMENT_EXPR)
    ? PLUS_EXPR : MINUS_EXPR;
  tree val, val_init_expr, inc_dec_expr, mod_expr;
  val = upc_copy_value_to_tmp_var (&val_init_expr, op0);
  inc_dec_expr = build_binary_op (loc, inc_dec_code,
				  val, integer_one_node, 0);
  mod_expr = build2 (MODIFY_EXPR, TREE_TYPE (val), op0, inc_dec_expr);
  if (want_value)
    {
      if (code == PREDECREMENT_EXPR || code == PREINCREMENT_EXPR)
	upc_genericize_modify_expr (loc, &mod_expr, 1);
      else
	{
	  upc_genericize_modify_expr (loc, &mod_expr, 0);
	  mod_expr = build2 (COMPOUND_EXPR, TREE_TYPE (val), mod_expr, val);
	}
    }
  else
    upc_genericize_modify_expr (loc, &mod_expr, 0);
  *expr_p = build2 (COMPOUND_EXPR, TREE_TYPE (mod_expr),
		    val_init_expr, mod_expr);
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
upc_strip_useless_generic_pts_cvt (tree *expr_p)
{
  while (TREE_CODE (*expr_p) == CONVERT_EXPR
	 && POINTER_TYPE_P (TREE_TYPE (*expr_p))
	 && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (*expr_p))))
    {
      *expr_p = TREE_OPERAND (*expr_p, 0);
    }
}

/* Generify the MODIFY_EXPR node of the 'shared' value
   pointed to by EXPR_P.  If the target of the assignment
   is a UPC 'shared' reference, or an indirection via a UPC
   pointer-to-shared, the assignment statement is rewritten into
   a call to a runtime routine that does a remote 'put'. 
   If WANT_VALUE is set, then return a compound expression
   that evaluates to the value of the source operand.  */

static void
upc_genericize_modify_expr (location_t loc, tree *expr_p, int want_value)
{
  const tree dest = TREE_OPERAND (*expr_p, 0);
  tree src = TREE_OPERAND (*expr_p, 1);
  if (TREE_SHARED (dest)
      || (TREE_TYPE (dest) && upc_shared_type_p (TREE_TYPE (dest))))
    {
      /* <shared dest> = <(shared|unshared) src> */
      const tree dest_addr = build_fold_addr_expr_loc (loc, dest);
      *expr_p = upc_expand_put (loc, dest_addr, src, want_value);
    }
  else if (TREE_SHARED (src)
	   || (TREE_TYPE (src) && upc_shared_type_p (TREE_TYPE (src))))
    {
      /* <unshared dest> = <shared src> */
      const tree src_addr = upc_shared_addr (loc, src);
      /* We could check for BLKmode and in
         that case perform a upc_memget().  */
      src = upc_expand_get (loc, src_addr);
      if (want_value && TREE_CODE (src) != COMPOUND_EXPR)
	{
	  tree src_tmp, src_tmp_init_expr;
	  src_tmp = upc_copy_value_to_tmp_var (&src_tmp_init_expr, src);
	  src = build2 (COMPOUND_EXPR, TREE_TYPE (src_tmp),
			src_tmp_init_expr, src_tmp);
	}
      TREE_OPERAND (*expr_p, 1) = src;
    }
}

typedef struct walk_data_struct
{
  int want_value;
} walk_data_t;
typedef walk_data_t *walk_data_p;

/* This routine is called to convert UPC specific constructs
   into GENERIC.  It is called from 'walk_tree' as it traverses
   the function body.

   This routine looks for tree nodes that will likely
   require a UPC-specific re-write, and then calls
   the appropriate translation function.
   
   The DATA parameter will point to a 'walk_data_t'
   structure, which presently has a single filed,
   'want_value'.  If 'want_value' is non-zero, it
   indicates that the value of the expression should
   be returned.  */

static tree
upc_genericize_expr (tree *expr_p, int *walk_subtrees, void *data)
{
  const walk_data_p wdata = (walk_data_p) data;
  const tree expr = *expr_p;
  const location_t loc = EXPR_LOCATION (expr);
  const enum tree_code code = TREE_CODE (expr);
  const tree type = CODE_CONTAINS_STRUCT (code, TS_TYPED)
    ? TREE_TYPE (expr) : NULL;
  const int want_value = wdata->want_value
                         && !(type && VOID_TYPE_P (type));
  const tree op0 = (TREE_CODE_LENGTH (code) >= 1)
    ? TREE_OPERAND (expr, 0) : NULL_TREE;
  const tree type0 = (op0 != NULL_TREE) ? TREE_TYPE (op0) : NULL_TREE;
  tree op1 = (TREE_CODE_LENGTH (code) >= 2)
    ? TREE_OPERAND (expr, 1) : NULL_TREE;
  tree type1 = (op1 != NULL_TREE) ? TREE_TYPE (op1) : NULL_TREE;
  switch (code)
    {
    case UPC_FORALL_STMT:
      upc_genericize_forall_stmt (expr_p);
      break;

    case UPC_SYNC_STMT:
      upc_genericize_sync_stmt (loc, expr_p);
      break;

    case ADDR_EXPR:
      if (POINTER_TYPE_P (type) && TREE_TYPE (type)
	  && upc_shared_type_p (TREE_TYPE (type)))
	upc_genericize_addr_expr (loc, expr_p);
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (op0 && TREE_SHARED (op0))
	upc_genericize_array_ref (loc, expr_p);
      break;

    case BIT_FIELD_REF:
    case COMPONENT_REF:
      if (op0 && TREE_SHARED (op0))
	upc_genericize_field_ref (loc, expr_p);
      break;

    case INDIRECT_REF:
      if (type0 && (TREE_CODE (type0) == POINTER_TYPE)
	  && upc_shared_type_p (TREE_TYPE (type0)))
	upc_genericize_indirect_ref (loc, expr_p);
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (op0 && TREE_SHARED (op0))
	upc_genericize_real_image_ref (loc, expr_p);
      break;

    case VAR_DECL:
      if (type && upc_shared_type_p (type))
	upc_genericize_shared_var_ref (loc, expr_p);
      break;

    case VIEW_CONVERT_EXPR:
      if (type && upc_shared_type_p (type))
	TREE_TYPE (expr) = build_upc_unshared_type (type);
      gcc_assert (!TREE_SHARED (expr));
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
	upc_genericize_pts_cvt (loc, expr_p);
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
	upc_genericize_pts_cond_expr (loc, expr_p);
      break;

    case MINUS_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      if ((type0 && (TREE_CODE (type0) == POINTER_TYPE)
	   && upc_shared_type_p (TREE_TYPE (type0)))
	  || (type1 && (TREE_CODE (type1) == POINTER_TYPE)
	      && upc_shared_type_p (TREE_TYPE (type1))))
	upc_genericize_pts_arith_expr (loc, expr_p);
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
	upc_genericize_modify_expr (loc, expr_p, want_value);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if ((op0 && TREE_SHARED (op0))
	  || (type0 && (upc_shared_type_p (type0)
			|| (POINTER_TYPE_P (type0)
			    && upc_shared_type_p (TREE_TYPE (type0))))))
	upc_genericize_shared_inc_dec_expr (loc, expr_p, want_value);
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

    case STATEMENT_LIST:
      upc_genericize_stmt_list (expr_p);
      *walk_subtrees = 0;
      break;

    case COMPOUND_EXPR:
      upc_genericize_compound_expr (expr_p, want_value);
      *walk_subtrees = 0;
      break;

    case COND_EXPR:
      upc_genericize_cond_expr (expr_p, want_value);
      *walk_subtrees = 0;
      break;

    case DECL_EXPR:
      upc_genericize_decl_expr (expr_p);
      *walk_subtrees = 0;
      break;

    default:
      gcc_assert (!TREE_SHARED (expr));
      break;
    }

  /* After evaluating the current node, we assert the
     want_value flag so that subtrees will be fully
     evaluated.  */
  wdata->want_value = 1;

  /* Continue tree traversal.  */
  return NULL_TREE;
}

/* Convert a compound expression into GENERIC form.
   A compound expression contains two expressions to compute,
   one (the LHS) followed by the other (the RHS).
   The LHS value is ignored.  The RHS value is used.  */

static void
upc_genericize_compound_expr (tree *expr_p, int want_value)
{
  tree *lhs_p = &TREE_OPERAND (*expr_p, 0);
  tree *rhs_p = &TREE_OPERAND (*expr_p, 1);
  upc_genericize_stmt (lhs_p, 0);
  upc_genericize_stmt (rhs_p, want_value);
}

/* Convert a comditional expression into GENERIC form.
   A conditional expression contains three expressions
   and is of the form expression ( ... ? ... : ...  in C).
   The first operand is the condition, the second is
   the then-value and the third is the else-value.  */

static void
upc_genericize_cond_expr (tree *expr_p, int want_value)
{
  tree *cond_p = &TREE_OPERAND (*expr_p, 0);
  tree *then_p = &TREE_OPERAND (*expr_p, 1);
  tree *else_p = &TREE_OPERAND (*expr_p, 2);
  upc_genericize_stmt (cond_p, 1);
  if (*then_p)
    upc_genericize_stmt (then_p, want_value);
  if (*else_p)
    upc_genericize_stmt (else_p, want_value);
}

/* Convert a declaration expression into GENERIC form.
   A declaration expression is used to represent a local declaration.
   The operand refers to the DECL associated with
   the given declaration statement.  */

static void
upc_genericize_decl_expr (tree *expr_p)
{
  tree decl = DECL_EXPR_DECL (*expr_p);
  tree *decl_init_p = &DECL_INITIAL (decl);
  if (*decl_init_p)
    upc_genericize_stmt (decl_init_p, 0);
}

/* Convert the tree rooted at STMT_P into GENERIC.
   WANT_VALUE is initial value the flag that
   upc_genericize_expr() queries to determine
   whether the expression node should return a value.
   NOTE: STMT_P can point to any kind of expression node.  */

static void
upc_genericize_stmt (tree *stmt_p, int want_value)
{
  walk_data_t wdata;
  wdata.want_value = want_value;
  (void) walk_tree (stmt_p, upc_genericize_expr, &wdata, NULL);
}

/* Convert a statement list to GENERIC.  */

static void
upc_genericize_stmt_list (tree *stmt_list_p)
{
  tree_stmt_iterator s = tsi_start (*stmt_list_p);
  while (!tsi_end_p (s))
    {
      tree *stmt_p = tsi_stmt_ptr (s);
      upc_genericize_stmt (stmt_p, 0);
      tsi_next (&s);
    }
}

/* Convert the function body identified by BODY_P into GENERIC.
   Initially assert WANT_VALUE as TRUE.  Traverse the function
   body by calling walk_tree() and applying upc_genericize_expr().  */

static void
upc_genericize_body (tree *body_p, tree fndecl)
{
  location_t saved_location = input_location;

  timevar_push (TV_TREE_UPC_GENERICIZE);

  input_location = DECL_SOURCE_LOCATION (fndecl);

  upc_genericize_stmt (body_p, 0);

  timevar_pop (TV_TREE_UPC_GENERICIZE);

  input_location = saved_location;
}

/* Convert the function tree rooted at FNDECL into GENERIC.
   After some initial set up, call upc_genericize_body()
   on the function body.  */

static void
upc_genericize_function_tree (tree fndecl)
{
  tree oldfn;

  gcc_assert (DECL_SAVED_TREE (fndecl));

  oldfn = current_function_decl;
  current_function_decl = fndecl;
  if (DECL_STRUCT_FUNCTION (fndecl))
    push_cfun (DECL_STRUCT_FUNCTION (fndecl));
  else
    push_struct_function (fndecl);

  upc_genericize_body (&DECL_SAVED_TREE (fndecl), fndecl);

  if (flag_upc_instrument_functions
      && !DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (fndecl)
      && !flag_instrument_functions_exclude_p (fndecl))
    {
      upc_instrument_func (fndecl);
    }

  current_function_decl = oldfn;
  pop_cfun ();
}

/* Convert the tree representation of FNDECL built by the UPC front-end
   into the GENERIC form.  */

void
upc_genericize (tree fndecl)
{
  struct cgraph_node *cgn;
  /* Lower this function and any nested functions.  */
  upc_genericize_function_tree (fndecl);
  cgn = cgraph_get_create_node (fndecl);
  for (cgn = cgn->nested; cgn; cgn = cgn->next_nested)
    upc_genericize (cgn->decl);
  /* Perform "C" lowering.  */
  c_genericize (fndecl);
}
