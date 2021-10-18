/* Predicate aware uninitialized variable warning.
   Copyright (C) 2001-2021 Free Software Foundation, Inc.
   Contributed by Xinliang David Li <davidxl@google.com>

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

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "cfghooks.h"
#include "attribs.h"
#include "builtins.h"
#include "calls.h"
#include "gimple-range.h"

#include "gimple-predicate-analysis.h"

/* This implements the pass that does predicate aware warning on uses of
   possibly uninitialized variables.  The pass first collects the set of
   possibly uninitialized SSA names.  For each such name, it walks through
   all its immediate uses.  For each immediate use, it rebuilds the condition
   expression (the predicate) that guards the use.  The predicate is then
   examined to see if the variable is always defined under that same condition.
   This is done either by pruning the unrealizable paths that lead to the
   default definitions or by checking if the predicate set that guards the
   defining paths is a superset of the use predicate.  */

/* Pointer set of potentially undefined ssa names, i.e.,
   ssa names that are defined by phi with operands that
   are not defined or potentially undefined.  */
static hash_set<tree> *possibly_undefined_names = 0;

/* Returns the first bit position (starting from LSB)
   in mask that is non zero.  Returns -1 if the mask is empty.  */
static int
get_mask_first_set_bit (unsigned mask)
{
  int pos = 0;
  if (mask == 0)
    return -1;

  while ((mask & (1 << pos)) == 0)
    pos++;

  return pos;
}
#define MASK_FIRST_SET_BIT(mask) get_mask_first_set_bit (mask)

/* Return true if T, an SSA_NAME, has an undefined value.  */
static bool
has_undefined_value_p (tree t)
{
  return (ssa_undefined_value_p (t)
	  || (possibly_undefined_names
	      && possibly_undefined_names->contains (t)));
}

/* Return true if EXPR should suppress either uninitialized warning.  */

static inline bool
get_no_uninit_warning (tree expr)
{
  return warning_suppressed_p (expr, OPT_Wuninitialized);
}

/* Suppress both uninitialized warnings for EXPR.  */

static inline void
set_no_uninit_warning (tree expr)
{
  suppress_warning (expr, OPT_Wuninitialized);
}

/* Like has_undefined_value_p, but don't return true if the no-warning
   bit is set on SSA_NAME_VAR for either uninit warning.  */

static inline bool
uninit_undefined_value_p (tree t)
{
  if (!has_undefined_value_p (t))
    return false;
  if (!SSA_NAME_VAR (t))
    return true;
  return !get_no_uninit_warning (SSA_NAME_VAR (t));
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

/* Emit warning OPT for variable VAR at the point in the program where
   the SSA_NAME T is being used uninitialized.  The warning text is in
   MSGID and STMT is the statement that does the uninitialized read.
   PHI_ARG_LOC is the location of the PHI argument if T and VAR are one,
   or UNKNOWN_LOCATION otherwise.  */

static void
warn_uninit (opt_code opt, tree t, tree var, const char *gmsgid,
	     gimple *context, location_t phi_arg_loc = UNKNOWN_LOCATION)
{
  /* Bail if the value isn't provably uninitialized.  */
  if (!has_undefined_value_p (t))
    return;

  /* Ignore COMPLEX_EXPR as initializing only a part of a complex
     turns in a COMPLEX_EXPR with the not initialized part being
     set to its previous (undefined) value.  */
  if (is_gimple_assign (context)
      && gimple_assign_rhs_code (context) == COMPLEX_EXPR)
    return;

  /* Ignore REALPART_EXPR or IMAGPART_EXPR if its operand is a call to
     .DEFERRED_INIT.  This is for handling the following case correctly:

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
    artificial initialization at line 4:
  f = .DEFERRED_INIT (f, 2);
  _1 = REALPART_EXPR <f>;

    without the following special handling, _1 = REALPART_EXPR <f> will
    be treated as the uninitialized use point, which is incorrect. (the
    real uninitialized use point is at line 11).  */
  if (is_gimple_assign (context)
      && (gimple_assign_rhs_code (context) == REALPART_EXPR
	  || gimple_assign_rhs_code (context) == IMAGPART_EXPR))
    {
      tree v = gimple_assign_rhs1 (context);
      if (TREE_CODE (TREE_OPERAND (v, 0)) == SSA_NAME
	  && gimple_call_internal_p (SSA_NAME_DEF_STMT (TREE_OPERAND (v, 0)),
				     IFN_DEFERRED_INIT))
	return;
    }

  /* Anonymous SSA_NAMEs shouldn't be uninitialized, but ssa_undefined_value_p
     can return true if the def stmt of an anonymous SSA_NAME is COMPLEX_EXPR
     created for conversion from scalar to complex.  Use the underlying var of
     the COMPLEX_EXPRs real part in that case.  See PR71581.  */
  if (!var && !SSA_NAME_VAR (t))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (t);
      if (is_gimple_assign (def_stmt)
	  && gimple_assign_rhs_code (def_stmt) == COMPLEX_EXPR)
	{
	  tree v = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (v) == SSA_NAME
	      && has_undefined_value_p (v)
	      && zerop (gimple_assign_rhs2 (def_stmt)))
	    var = SSA_NAME_VAR (v);
	}
    }

  if (var == NULL_TREE)
    return;

  /* Avoid warning if we've already done so or if the warning has been
     suppressed.  */
  if (((warning_suppressed_p (context, OPT_Wuninitialized)
	|| (gimple_assign_single_p (context)
	    && get_no_uninit_warning (gimple_assign_rhs1 (context)))))
      || get_no_uninit_warning (var))
    return;

  /* Use either the location of the read statement or that of the PHI
     argument, or that of the uninitialized variable, in that order,
     whichever is valid.  */
  location_t location;
  if (gimple_has_location (context))
    location = gimple_location (context);
  else if (phi_arg_loc != UNKNOWN_LOCATION)
    location = phi_arg_loc;
  else
    location = DECL_SOURCE_LOCATION (var);
  location = linemap_resolve_location (line_table, location,
				       LRK_SPELLING_LOCATION, NULL);

  auto_diagnostic_group d;
  if (!warning_at (location, opt, gmsgid, var))
    return;

  /* Avoid subsequent warnings for reads of the same variable again.  */
  suppress_warning (var, opt);

  /* Issue a note pointing to the read variable unless the warning
     is at the same location.  */
  location_t var_loc = DECL_SOURCE_LOCATION (var);
  if (location == var_loc)
    return;

  inform (var_loc, "%qD was declared here", var);
}

struct check_defs_data
{
  /* If we found any may-defs besides must-def clobbers.  */
  bool found_may_defs;
};

/* Return true if STMT is a call to built-in function all of whose
   by-reference arguments are const-qualified (i.e., the function can
   be assumed not to modify them).  */

static bool
builtin_call_nomodifying_p (gimple *stmt)
{
  if (!gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    return false;

  tree fndecl = gimple_call_fndecl (stmt);
  if (!fndecl)
    return false;

  tree fntype = TREE_TYPE (fndecl);
  if (!fntype)
    return false;

  /* Check the called function's signature for non-constc pointers.
     If one is found, return false.  */
  unsigned argno = 0;
  tree argtype;
  function_args_iterator it;
  FOREACH_FUNCTION_ARGS (fntype, argtype, it)
    {
      if (VOID_TYPE_P (argtype))
	return true;

      ++argno;

      if (!POINTER_TYPE_P (argtype))
	continue;

      if (TYPE_READONLY (TREE_TYPE (argtype)))
	continue;

      return false;
    }

  /* If the number of actual arguments to the call is less than or
     equal to the number of parameters, return false.  */
  unsigned nargs = gimple_call_num_args (stmt);
  if (nargs <= argno)
    return false;

  /* Check arguments passed through the ellipsis in calls to variadic
     functions for pointers.  If one is found that's a non-constant
     pointer, return false.  */
  for (; argno < nargs; ++argno)
    {
      tree arg = gimple_call_arg (stmt, argno);
      argtype = TREE_TYPE (arg);
      if (!POINTER_TYPE_P (argtype))
	continue;

      if (TYPE_READONLY (TREE_TYPE (argtype)))
	continue;

      return false;
    }

  return true;
}

/* If ARG is a FNDECL parameter declared with attribute access none or
   write_only issue a warning for its read access via PTR.  */

static void
maybe_warn_read_write_only (tree fndecl, gimple *stmt, tree arg, tree ptr)
{
  if (!fndecl)
    return;

  if (get_no_uninit_warning (arg))
    return;

  tree fntype = TREE_TYPE (fndecl);
  if (!fntype)
    return;

  /* Initialize a map of attribute access specifications for arguments
     to the function function call.  */
  rdwr_map rdwr_idx;
  init_attr_rdwr_indices (&rdwr_idx, TYPE_ATTRIBUTES (fntype));

  unsigned argno = 0;
  tree parms = DECL_ARGUMENTS (fndecl);
  for (tree parm = parms; parm; parm = TREE_CHAIN (parm), ++argno)
    {
      if (parm != arg)
	continue;

      const attr_access* access = rdwr_idx.get (argno);
      if (!access)
	break;

      if (access->mode != access_none
	  && access->mode != access_write_only)
	continue;

      location_t stmtloc
	= linemap_resolve_location (line_table, gimple_location (stmt),
				    LRK_SPELLING_LOCATION, NULL);

      if (!warning_at (stmtloc, OPT_Wmaybe_uninitialized,
		       "%qE may be used uninitialized", ptr))
	break;

      suppress_warning (arg, OPT_Wmaybe_uninitialized);

      const char* const access_str =
	TREE_STRING_POINTER (access->to_external_string ());

      location_t parmloc = DECL_SOURCE_LOCATION (parm);
      inform (parmloc, "accessing argument %u of a function declared with "
	      "attribute %<%s%>",
	      argno + 1, access_str);

      break;
    }
}

/* Callback for walk_aliased_vdefs.  */

static bool
check_defs (ao_ref *ref, tree vdef, void *data_)
{
  check_defs_data *data = (check_defs_data *)data_;
  gimple *def_stmt = SSA_NAME_DEF_STMT (vdef);

  /* Ignore the vdef if the definition statement is a call
     to .DEFERRED_INIT function.  */
  if (gimple_call_internal_p (def_stmt, IFN_DEFERRED_INIT))
    return false;

  /* The ASAN_MARK intrinsic doesn't modify the variable.  */
  if (is_gimple_call (def_stmt))
    {
      /* The ASAN_MARK intrinsic doesn't modify the variable.  */
      if (gimple_call_internal_p (def_stmt)
	  && gimple_call_internal_fn (def_stmt) == IFN_ASAN_MARK)
	return false;

      if (tree fndecl = gimple_call_fndecl (def_stmt))
	{
	  /* Some sanitizer calls pass integer arguments to built-ins
	     that expect pointets. Avoid using gimple_call_builtin_p()
	     which fails for such calls.  */
	  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	    {
	      built_in_function fncode = DECL_FUNCTION_CODE (fndecl);
	      if (fncode > BEGIN_SANITIZER_BUILTINS
		  && fncode < END_SANITIZER_BUILTINS)
		return false;
	    }
	}
    }

  /* End of VLA scope is not a kill.  */
  if (gimple_call_builtin_p (def_stmt, BUILT_IN_STACK_RESTORE))
    return false;

  /* If this is a clobber then if it is not a kill walk past it.  */
  if (gimple_clobber_p (def_stmt))
    {
      if (stmt_kills_ref_p (def_stmt, ref))
	return true;
      return false;
    }

  if (builtin_call_nomodifying_p (def_stmt))
    return false;

  /* Found a may-def on this path.  */
  data->found_may_defs = true;
  return true;
}

/* Counters and limits controlling the the depth of analysis and
   strictness of the warning.  */
struct wlimits
{
  /* Number of VDEFs encountered.  */
  unsigned int vdef_cnt;
  /* Number of statements examined by walk_aliased_vdefs.  */
  unsigned int oracle_cnt;
  /* Limit on the number of statements visited by walk_aliased_vdefs.  */
  unsigned limit;
  /* Set when basic block with statement is executed unconditionally.  */
  bool always_executed;
  /* Set to issue -Wmaybe-uninitialized.  */
  bool wmaybe_uninit;
};

/* Determine if REF references an uninitialized operand and diagnose
   it if so.  STMS is the referencing statement.  LHS is the result
   of the access and may be null.  RHS is the variable referenced by
   the access; it may not be null.  */

static tree
maybe_warn_operand (ao_ref &ref, gimple *stmt, tree lhs, tree rhs,
		    wlimits &wlims)
{
  bool has_bit_insert = false;
  use_operand_p luse_p;
  imm_use_iterator liter;

  if (get_no_uninit_warning (rhs))
    return NULL_TREE;

  /* Do not warn if the base was marked so or this is a
     hard register var.  */
  tree base = ao_ref_base (&ref);
  if ((VAR_P (base)
       && DECL_HARD_REGISTER (base))
      || get_no_uninit_warning (base))
    return NULL_TREE;

  /* Do not warn if the access is zero size or if it's fully outside
     the object.  */
  poly_int64 decl_size;
  if (known_size_p (ref.size)
      && known_eq (ref.max_size, ref.size)
      && (known_eq (ref.size, 0)
	  || known_le (ref.offset + ref.size, 0)))
    return NULL_TREE;

  if (DECL_P (base)
      && known_ge (ref.offset, 0)
      && DECL_SIZE (base)
      && poly_int_tree_p (DECL_SIZE (base), &decl_size)
      && known_le (decl_size, ref.offset))
    return NULL_TREE;

  /* Do not warn if the result of the access is then used for
     a BIT_INSERT_EXPR. */
  if (lhs && TREE_CODE (lhs) == SSA_NAME)
    FOR_EACH_IMM_USE_FAST (luse_p, liter, lhs)
      {
	gimple *use_stmt = USE_STMT (luse_p);
	/* BIT_INSERT_EXPR first operand should not be considered
	   a use for the purpose of uninit warnings.  */
	if (gassign *ass = dyn_cast <gassign *> (use_stmt))
	  {
	    if (gimple_assign_rhs_code (ass) == BIT_INSERT_EXPR
		&& luse_p->use == gimple_assign_rhs1_ptr (ass))
	      {
		has_bit_insert = true;
		break;
	      }
	  }
      }

  if (has_bit_insert)
    return NULL_TREE;

  /* Limit the walking to a constant number of stmts after
     we overcommit quadratic behavior for small functions
     and O(n) behavior.  */
  if (wlims.oracle_cnt > 128 * 128
      && wlims.oracle_cnt > wlims.vdef_cnt * 2)
    wlims.limit = 32;

  check_defs_data data;
  bool fentry_reached = false;
  data.found_may_defs = false;
  tree use = gimple_vuse (stmt);
  if (!use)
    return NULL_TREE;
  int res = walk_aliased_vdefs (&ref, use,
				check_defs, &data, NULL,
				&fentry_reached, wlims.limit);
  if (res == -1)
    {
      wlims.oracle_cnt += wlims.limit;
      return NULL_TREE;
    }

  wlims.oracle_cnt += res;
  if (data.found_may_defs)
    return NULL_TREE;

  bool found_alloc = false;

  if (fentry_reached)
    {
      if (TREE_CODE (base) == MEM_REF)
	base = TREE_OPERAND (base, 0);

      /* Follow the chain of SSA_NAME assignments looking for an alloca
	 call (or VLA) or malloc/realloc, or for decls.  If any is found
	 (and in the latter case, the operand is a local variable) issue
	 a warning.  */
      while (TREE_CODE (base) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (base);

	  if (is_gimple_call (def_stmt)
	      && gimple_call_builtin_p (def_stmt))
	    {
	      /* Detect uses of uninitialized alloca/VLAs.  */
	      tree fndecl = gimple_call_fndecl (def_stmt);
	      const built_in_function fncode = DECL_FUNCTION_CODE (fndecl);
	      if (fncode == BUILT_IN_ALLOCA
		  || fncode  == BUILT_IN_ALLOCA_WITH_ALIGN
		  || fncode  == BUILT_IN_MALLOC)
		found_alloc = true;
	      break;
	    }

	  if (!is_gimple_assign (def_stmt))
	    break;

	  tree_code code = gimple_assign_rhs_code (def_stmt);
	  if (code != ADDR_EXPR && code != POINTER_PLUS_EXPR)
	    break;

	  base = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (base) == ADDR_EXPR)
	    base = TREE_OPERAND (base, 0);

	  if (DECL_P (base)
	      || TREE_CODE (base) == COMPONENT_REF)
	    rhs = base;

	  if (TREE_CODE (base) == MEM_REF)
	    base = TREE_OPERAND (base, 0);

	  if (tree ba = get_base_address (base))
	    base = ba;
	}

      /* Replace the RHS expression with BASE so that it
	 refers to it in the diagnostic (instead of to
	 '<unknown>').  */
      if (DECL_P (base)
	  && EXPR_P (rhs)
	  && TREE_CODE (rhs) != COMPONENT_REF)
	rhs = base;
    }

  /* Do not warn if it can be initialized outside this function.
     If we did not reach function entry then we found killing
     clobbers on all paths to entry.  */
  if (!found_alloc && fentry_reached)
    {
      if (TREE_CODE (base) == SSA_NAME)
	{
	  tree var = SSA_NAME_VAR (base);
	  if (var && TREE_CODE (var) == PARM_DECL)
	    {
	      maybe_warn_read_write_only (cfun->decl, stmt, var, rhs);
	      return NULL_TREE;
	    }
	}

      if (!VAR_P (base)
	  || is_global_var (base))
	/* ???  We'd like to use ref_may_alias_global_p but that
	   excludes global readonly memory and thus we get bogus
	   warnings from p = cond ? "a" : "b" for example.  */
	return NULL_TREE;
    }

  /* Strip the address-of expression from arrays passed to functions. */
  if (TREE_CODE (rhs) == ADDR_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  /* Check again since RHS may have changed above.  */
  if (get_no_uninit_warning (rhs))
    return NULL_TREE;

  /* Avoid warning about empty types such as structs with no members.
     The first_field() test is important for C++ where the predicate
     alone isn't always sufficient.  */
  tree rhstype = TREE_TYPE (rhs);
  if (POINTER_TYPE_P (rhstype))
    rhstype = TREE_TYPE (rhstype);
  if (is_empty_type (rhstype))
    return NULL_TREE;

  bool warned = false;
  /* We didn't find any may-defs so on all paths either
     reached function entry or a killing clobber.  */
  location_t location
    = linemap_resolve_location (line_table, gimple_location (stmt),
				LRK_SPELLING_LOCATION, NULL);
  if (wlims.always_executed)
    {
      if (warning_at (location, OPT_Wuninitialized,
		      "%qE is used uninitialized", rhs))
	{
	  /* ???  This is only effective for decls as in
	     gcc.dg/uninit-B-O0.c.  Avoid doing this for maybe-uninit
	     uses or accesses by functions as it may hide important
	     locations.  */
	  if (lhs)
	    set_no_uninit_warning (rhs);
	  warned = true;
	}
    }
  else if (wlims.wmaybe_uninit)
    warned = warning_at (location, OPT_Wmaybe_uninitialized,
			 "%qE may be used uninitialized", rhs);

  return warned ? base : NULL_TREE;
}


/* Diagnose passing addresses of uninitialized objects to either const
   pointer arguments to functions, or to functions declared with attribute
   access implying read access to those objects.  */

static void
maybe_warn_pass_by_reference (gcall *stmt, wlimits &wlims)
{
  if (!wlims.wmaybe_uninit)
    return;

  unsigned nargs = gimple_call_num_args (stmt);
  if (!nargs)
    return;

  tree fndecl = gimple_call_fndecl (stmt);
  tree fntype = gimple_call_fntype (stmt);
  if (!fntype)
    return;

  /* Const function do not read their arguments.  */
  if (gimple_call_flags (stmt) & ECF_CONST)
    return;

  const built_in_function fncode
    = (fndecl && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL)
       ? DECL_FUNCTION_CODE (fndecl) : (built_in_function)BUILT_IN_LAST);

  if (fncode == BUILT_IN_MEMCPY || fncode == BUILT_IN_MEMMOVE)
    /* Avoid diagnosing calls to raw memory functions (this is overly
       permissive; consider tightening it up).  */
    return;

  /* Save the current warning setting and replace it either a "maybe"
     when passing addresses of uninitialized variables to const-qualified
     pointers or arguments declared with attribute read_write, or with
     a "certain" when passing them to arguments declared with attribute
     read_only.  */
  const bool save_always_executed = wlims.always_executed;

  /* Initialize a map of attribute access specifications for arguments
     to the function function call.  */
  rdwr_map rdwr_idx;
  init_attr_rdwr_indices (&rdwr_idx, TYPE_ATTRIBUTES (fntype));

  tree argtype;
  unsigned argno = 0;
  function_args_iterator it;

  FOREACH_FUNCTION_ARGS (fntype, argtype, it)
    {
      ++argno;

      if (!POINTER_TYPE_P (argtype))
	continue;

      tree access_size = NULL_TREE;
      const attr_access* access = rdwr_idx.get (argno - 1);
      if (access)
	{
	  if (access->mode == access_none
	      || access->mode == access_write_only)
	    continue;

	  if (access->mode == access_deferred
	      && !TYPE_READONLY (TREE_TYPE (argtype)))
	    continue;

	  if (save_always_executed && access->mode == access_read_only)
	    /* Attribute read_only arguments imply read access.  */
	    wlims.always_executed = true;
	  else
	    /* Attribute read_write arguments are documented as requiring
	       initialized objects but it's expected that aggregates may
	       be only partially initialized regardless.  */
	    wlims.always_executed = false;

	  if (access->sizarg < nargs)
	    access_size = gimple_call_arg (stmt, access->sizarg);
	}
      else if (!TYPE_READONLY (TREE_TYPE (argtype)))
	continue;
      else if (save_always_executed && fncode != BUILT_IN_LAST)
	/* Const-qualified arguments to built-ins imply read access.  */
	wlims.always_executed = true;
      else
	/* Const-qualified arguments to ordinary functions imply a likely
	   (but not definitive) read access.  */
	wlims.always_executed = false;

      /* Ignore args we are not going to read from.  */
      if (gimple_call_arg_flags (stmt, argno - 1) & (EAF_UNUSED | EAF_NOREAD))
	continue;

      tree arg = gimple_call_arg (stmt, argno - 1);
      if (!POINTER_TYPE_P (TREE_TYPE (arg)))
	/* Avoid actual arguments with invalid types.  */
	continue;

      ao_ref ref;
      ao_ref_init_from_ptr_and_size (&ref, arg, access_size);
      tree argbase = maybe_warn_operand (ref, stmt, NULL_TREE, arg, wlims);
      if (!argbase)
	continue;

      if (access && access->mode != access_deferred)
	{
	  const char* const access_str =
	    TREE_STRING_POINTER (access->to_external_string ());

	  if (fndecl)
	    {
	      location_t loc = DECL_SOURCE_LOCATION (fndecl);
	      inform (loc, "in a call to %qD declared with "
		      "attribute %<%s%> here", fndecl, access_str);
	    }
	  else
	    {
	      /* Handle calls through function pointers.  */
	      location_t loc = gimple_location (stmt);
	      inform (loc, "in a call to %qT declared with "
		      "attribute %<%s%>", fntype, access_str);
	    }
	}
      else
	{
	  /* For a declaration with no relevant attribute access create
	     a dummy object and use the formatting function to avoid
	     having to complicate things here.  */
	  attr_access ptr_access = { };
	  if (!access)
	    access = &ptr_access;
	  const std::string argtypestr = access->array_as_string (argtype);
	  if (fndecl)
	    {
	      location_t loc (DECL_SOURCE_LOCATION (fndecl));
	      inform (loc, "by argument %u of type %s to %qD "
		      "declared here",
		      argno, argtypestr.c_str (), fndecl);
	    }
	  else
	    {
	      /* Handle calls through function pointers.  */
	      location_t loc (gimple_location (stmt));
	      inform (loc, "by argument %u of type %s to %qT",
		      argno, argtypestr.c_str (), fntype);
	    }
	}

      if (DECL_P (argbase))
	{
	  location_t loc = DECL_SOURCE_LOCATION (argbase);
	  inform (loc, "%qD declared here", argbase);
	}
    }

  wlims.always_executed = save_always_executed;
}

/* Warn about an uninitialized PHI argument on the fallthru path to
   an always executed block BB.  */

static void
warn_uninit_phi_uses (basic_block bb)
{
  edge_iterator ei;
  edge e, found = NULL, found_back = NULL;
  /* Look for a fallthru and possibly a single backedge.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      /* Ignore backedges.  */
      if (dominated_by_p (CDI_DOMINATORS, e->src, bb))
	{
	  if (found_back)
	    {
	      found = NULL;
	      break;
	    }
	  found_back = e;
	  continue;
	}
      if (found)
	{
	  found = NULL;
	  break;
	}
      found = e;
    }
  if (!found)
    return;

  basic_block succ = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
       gsi_next (&si))
    {
      gphi *phi = si.phi ();
      tree def = PHI_ARG_DEF_FROM_EDGE (phi, found);
      if (TREE_CODE (def) != SSA_NAME
	  || !SSA_NAME_IS_DEFAULT_DEF (def)
	  || virtual_operand_p (def))
	continue;
      /* If there's a default def on the fallthru edge PHI
	 value and there's a use that post-dominates entry
	 then that use is uninitialized and we can warn.  */
      imm_use_iterator iter;
      use_operand_p use_p;
      gimple *use_stmt = NULL;
      FOR_EACH_IMM_USE_FAST (use_p, iter, gimple_phi_result (phi))
	{
	  use_stmt = USE_STMT (use_p);
	  if (gimple_location (use_stmt) != UNKNOWN_LOCATION
	      && dominated_by_p (CDI_POST_DOMINATORS, succ,
				 gimple_bb (use_stmt))
	      /* If we found a non-fallthru edge make sure the
		 use is inside the loop, otherwise the backedge
		 can serve as initialization.  */
	      && (!found_back
		  || dominated_by_p (CDI_DOMINATORS, found_back->src,
				     gimple_bb (use_stmt))))
	    break;
	  use_stmt = NULL;
	}
      if (use_stmt)
	warn_uninit (OPT_Wuninitialized, def, SSA_NAME_VAR (def),
		     "%qD is used uninitialized", use_stmt);
    }
}

/* Issue warnings about reads of uninitialized variables.  WMAYBE_UNINIT
   is true to issue -Wmaybe-uninitialized, otherwise -Wuninitialized.  */

static void
warn_uninitialized_vars (bool wmaybe_uninit)
{
  /* Counters and limits controlling the the depth of the warning.  */
  wlimits wlims = { };
  wlims.wmaybe_uninit = wmaybe_uninit;

  gimple_stmt_iterator gsi;
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      basic_block succ = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
      wlims.always_executed = dominated_by_p (CDI_POST_DOMINATORS, succ, bb);

      if (wlims.always_executed)
	warn_uninit_phi_uses (bb);

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  /* The call is an artificial use, will not provide meaningful
	     error message.  If the result of the call is used somewhere
	     else, we warn there instead.  */
	  if (gimple_call_internal_p (stmt, IFN_DEFERRED_INIT))
	    continue;

	  if (is_gimple_debug (stmt))
	    continue;

	  /* We only do data flow with SSA_NAMEs, so that's all we
	     can warn about.  */
	  use_operand_p use_p;
	  ssa_op_iter op_iter;
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, op_iter, SSA_OP_USE)
	    {
	      /* BIT_INSERT_EXPR first operand should not be considered
	         a use for the purpose of uninit warnings.  */
	      if (gassign *ass = dyn_cast <gassign *> (stmt))
		{
		  if (gimple_assign_rhs_code (ass) == BIT_INSERT_EXPR
		      && use_p->use == gimple_assign_rhs1_ptr (ass))
		    continue;
		}
	      tree use = USE_FROM_PTR (use_p);
	      if (wlims.always_executed)
		warn_uninit (OPT_Wuninitialized, use, SSA_NAME_VAR (use),
			     "%qD is used uninitialized", stmt);
	      else if (wmaybe_uninit)
		warn_uninit (OPT_Wmaybe_uninitialized, use, SSA_NAME_VAR (use),
			     "%qD may be used uninitialized", stmt);
	    }

	  /* For limiting the alias walk below we count all
	     vdefs in the function.  */
	  if (gimple_vdef (stmt))
	    wlims.vdef_cnt++;

	  if (gcall *call = dyn_cast <gcall *> (stmt))
	    maybe_warn_pass_by_reference (call, wlims);
	  else if (gimple_assign_load_p (stmt)
		   && gimple_has_location (stmt))
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);
	      tree lhs = gimple_assign_lhs (stmt);

	      ao_ref ref;
	      ao_ref_init (&ref, rhs);
	      tree var = maybe_warn_operand (ref, stmt, lhs, rhs, wlims);
	      if (!var)
		continue;

	      if (DECL_P (var))
		{
		  location_t loc = DECL_SOURCE_LOCATION (var);
		  inform (loc, "%qD declared here", var);
		}
	    }
	}
    }
}

/* Checks if the operand OPND of PHI is defined by
   another phi with one operand defined by this PHI,
   but the rest operands are all defined.  If yes,
   returns true to skip this operand as being
   redundant.  Can be enhanced to be more general.  */

static bool
can_skip_redundant_opnd (tree opnd, gimple *phi)
{
  tree phi_def = gimple_phi_result (phi);
  gimple *op_def = SSA_NAME_DEF_STMT (opnd);
  if (gimple_code (op_def) != GIMPLE_PHI)
    return false;

  unsigned n = gimple_phi_num_args (op_def);
  for (unsigned i = 0; i < n; ++i)
    {
      tree op = gimple_phi_arg_def (op_def, i);
      if (TREE_CODE (op) != SSA_NAME)
	continue;
      if (op != phi_def && uninit_undefined_value_p (op))
	return false;
    }

  return true;
}

/* Return a bitset holding the positions of arguments in PHI with empty
   (or possibly empty) definitions.  */

static unsigned
compute_uninit_opnds_pos (gphi *phi)
{
  unsigned uninit_opnds = 0;

  unsigned n = gimple_phi_num_args (phi);
  /* Bail out for phi with too many args.  */
  if (n > predicate::func_t::max_phi_args)
    return 0;

  for (unsigned i = 0; i < n; ++i)
    {
      tree op = gimple_phi_arg_def (phi, i);
      if (TREE_CODE (op) == SSA_NAME
	  && uninit_undefined_value_p (op)
	  && !can_skip_redundant_opnd (op, phi))
	{
	  if (cfun->has_nonlocal_label || cfun->calls_setjmp)
	    {
	      /* Ignore SSA_NAMEs that appear on abnormal edges
		 somewhere.  */
	      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
		continue;
	    }
	  MASK_SET_BIT (uninit_opnds, i);
	}
    }
  return uninit_opnds;
}

/* Function object type used to determine whether an expression
   is of interest to the predicate analyzer.  */

struct uninit_undef_val_t: public predicate::func_t
{
  virtual bool operator()(tree) override;
  virtual unsigned phi_arg_set (gphi *) override;
};

/* Return true if the argument is an expression of interest.  */

bool
uninit_undef_val_t::operator()(tree val)
{
  if (TREE_CODE (val) == SSA_NAME)
    return uninit_undefined_value_p (val);

  return false;
}

/* Return a bitset of PHI arguments of interest.  */

unsigned
uninit_undef_val_t::phi_arg_set (gphi *phi)
{
  return compute_uninit_opnds_pos (phi);
}

/* Searches through all uses of a potentially
   uninitialized variable defined by PHI and returns a use
   statement if the use is not properly guarded.  It returns
   NULL if all uses are guarded.  UNINIT_OPNDS is a bitvector
   holding the position(s) of uninit PHI operands.  WORKLIST
   is the vector of candidate phis that may be updated by this
   function.  ADDED_TO_WORKLIST is the pointer set tracking
   if the new phi is already in the worklist.  */

static gimple *
find_uninit_use (gphi *phi, unsigned uninit_opnds,
		 vec<gphi *> *worklist, hash_set<gphi *> *added_to_worklist)
{
  /* The Boolean predicate guarding the PHI definition.  Initialized
     lazily from PHI in the first call to is_use_guarded() and cached
     for subsequent iterations.  */
  uninit_undef_val_t eval;
  predicate def_preds (eval);

  use_operand_p use_p;
  imm_use_iterator iter;
  tree phi_result = gimple_phi_result (phi);
  FOR_EACH_IMM_USE_FAST (use_p, iter, phi_result)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;

      basic_block use_bb;
      if (gphi *use_phi = dyn_cast<gphi *> (use_stmt))
	use_bb = gimple_phi_arg_edge (use_phi,
				      PHI_ARG_INDEX_FROM_USE (use_p))->src;
      else
	use_bb = gimple_bb (use_stmt);

      if (def_preds.is_use_guarded (use_stmt, use_bb, phi, uninit_opnds))
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Found unguarded use in bb %u: ",
		   use_bb->index);
	  print_gimple_stmt (dump_file, use_stmt, 0);
	}
      /* Found one real use, return.  */
      if (gimple_code (use_stmt) != GIMPLE_PHI)
	return use_stmt;

      /* Found a phi use that is not guarded,
	 add the phi to the worklist.  */
      if (!added_to_worklist->add (as_a<gphi *> (use_stmt)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "[WORKLIST]: Update worklist with phi: ");
	      print_gimple_stmt (dump_file, use_stmt, 0);
	    }

	  worklist->safe_push (as_a<gphi *> (use_stmt));
	  possibly_undefined_names->add (phi_result);
	}
    }

  return NULL;
}

/* Look for inputs to PHI that are SSA_NAMEs that have empty definitions
   and gives warning if there exists a runtime path from the entry to a
   use of the PHI def that does not contain a definition.  In other words,
   the warning is on the real use.  The more dead paths that can be pruned
   by the compiler, the fewer false positives the warning is.  WORKLIST
   is a vector of candidate phis to be examined.  ADDED_TO_WORKLIST is
   a pointer set tracking if the new phi is added to the worklist or not.  */

static void
warn_uninitialized_phi (gphi *phi, vec<gphi *> *worklist,
			hash_set<gphi *> *added_to_worklist)
{
  /* Don't look at virtual operands.  */
  if (virtual_operand_p (gimple_phi_result (phi)))
    return;

  unsigned uninit_opnds = compute_uninit_opnds_pos (phi);
  if (MASK_EMPTY (uninit_opnds))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Examining phi: ");
      print_gimple_stmt (dump_file, phi, 0);
    }

  gimple *uninit_use_stmt = find_uninit_use (phi, uninit_opnds,
					     worklist, added_to_worklist);

  /* All uses are properly guarded but a new PHI may have been added
     to WORKLIST.  */
  if (!uninit_use_stmt)
    return;

  unsigned phiarg_index = MASK_FIRST_SET_BIT (uninit_opnds);
  tree uninit_op = gimple_phi_arg_def (phi, phiarg_index);
  if (SSA_NAME_VAR (uninit_op) == NULL_TREE)
    return;

  location_t loc = UNKNOWN_LOCATION;
  if (gimple_phi_arg_has_location (phi, phiarg_index))
    loc = gimple_phi_arg_location (phi, phiarg_index);
  else
    {
      tree arg_def = gimple_phi_arg_def (phi, phiarg_index);
      if (TREE_CODE (arg_def) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (arg_def);
	  if (gphi *arg_phi = dyn_cast<gphi *> (def_stmt))
	    {
	      unsigned uop = compute_uninit_opnds_pos (arg_phi);
	      unsigned idx = MASK_FIRST_SET_BIT (uop);
	      if (idx < gimple_phi_num_args (arg_phi)
		  && gimple_phi_arg_has_location (arg_phi, idx))
		loc = gimple_phi_arg_location (arg_phi, idx);
	    }
	}
    }

  warn_uninit (OPT_Wmaybe_uninitialized, uninit_op,
	       SSA_NAME_VAR (uninit_op),
	       "%qD may be used uninitialized in this function",
	       uninit_use_stmt, loc);
}

static bool
gate_warn_uninitialized (void)
{
  return warn_uninitialized || warn_maybe_uninitialized;
}

namespace {

const pass_data pass_data_late_warn_uninitialized =
{
  GIMPLE_PASS, /* type */
  "uninit", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_late_warn_uninitialized : public gimple_opt_pass
{
public:
  pass_late_warn_uninitialized (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_late_warn_uninitialized, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass *clone () { return new pass_late_warn_uninitialized (m_ctxt); }
  virtual bool gate (function *) { return gate_warn_uninitialized (); }
  virtual unsigned int execute (function *);

}; // class pass_late_warn_uninitialized

static void
execute_late_warn_uninitialized (function *fun)
{
  basic_block bb;
  gphi_iterator gsi;
  vec<gphi *> worklist = vNULL;

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);
  /* Re-do the plain uninitialized variable check, as optimization may have
     straightened control flow.  Do this first so that we don't accidentally
     get a "may be" warning when we'd have seen an "is" warning later.  */
  warn_uninitialized_vars (/*warn_maybe_uninitialized=*/1);

  timevar_push (TV_TREE_UNINIT);

  possibly_undefined_names = new hash_set<tree>;
  hash_set<gphi *> added_to_worklist;

  /* Initialize worklist  */
  FOR_EACH_BB_FN (bb, fun)
    for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gphi *phi = gsi.phi ();

	/* Don't look at virtual operands.  */
	if (virtual_operand_p (gimple_phi_result (phi)))
	  continue;

	unsigned n = gimple_phi_num_args (phi);
	for (unsigned i = 0; i < n; ++i)
	  {
	    tree op = gimple_phi_arg_def (phi, i);
	    if (TREE_CODE (op) == SSA_NAME && uninit_undefined_value_p (op))
	      {
		worklist.safe_push (phi);
		added_to_worklist.add (phi);
		if (dump_file && (dump_flags & TDF_DETAILS))
		  {
		    fprintf (dump_file, "[WORKLIST]: add to initial list "
			     "for operand %u of: ", i);
		    print_gimple_stmt (dump_file, phi, 0);
		  }
		break;
	      }
	  }
      }

  while (worklist.length () != 0)
    {
      gphi *cur_phi = 0;
      cur_phi = worklist.pop ();
      warn_uninitialized_phi (cur_phi, &worklist, &added_to_worklist);
    }

  worklist.release ();
  delete possibly_undefined_names;
  possibly_undefined_names = NULL;
  free_dominance_info (CDI_POST_DOMINATORS);
  timevar_pop (TV_TREE_UNINIT);
}

unsigned int
pass_late_warn_uninitialized::execute (function *fun)
{
  execute_late_warn_uninitialized (fun);
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_late_warn_uninitialized (gcc::context *ctxt)
{
  return new pass_late_warn_uninitialized (ctxt);
}

static unsigned int
execute_early_warn_uninitialized (void)
{
  /* Currently, this pass runs always but
     execute_late_warn_uninitialized only runs with optimization.  With
     optimization we want to warn about possible uninitialized as late
     as possible, thus don't do it here.  However, without
     optimization we need to warn here about "may be uninitialized".  */
  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  warn_uninitialized_vars (/*warn_maybe_uninitialized=*/!optimize);

  /* Post-dominator information cannot be reliably updated.  Free it
     after the use.  */

  free_dominance_info (CDI_POST_DOMINATORS);
  return 0;
}

namespace {

const pass_data pass_data_early_warn_uninitialized =
{
  GIMPLE_PASS, /* type */
  "*early_warn_uninitialized", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_UNINIT, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_early_warn_uninitialized : public gimple_opt_pass
{
public:
  pass_early_warn_uninitialized (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_warn_uninitialized, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return gate_warn_uninitialized (); }
  virtual unsigned int execute (function *)
  {
    return execute_early_warn_uninitialized ();
  }

}; // class pass_early_warn_uninitialized

} // anon namespace

gimple_opt_pass *
make_pass_early_warn_uninitialized (gcc::context *ctxt)
{
  return new pass_early_warn_uninitialized (ctxt);
}
