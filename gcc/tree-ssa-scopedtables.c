/* Header file for SSA dominator optimizations.
   Copyright (C) 2013-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "tree-pretty-print.h"
#include "tree-ssa-scopedtables.h"
#include "tree-ssa-threadedge.h"
#include "stor-layout.h"
#include "fold-const.h"
#include "tree-eh.h"
#include "internal-fn.h"
#include "tree-dfa.h"

static bool hashable_expr_equal_p (const struct hashable_expr *,
				   const struct hashable_expr *);

/* Initialize local stacks for this optimizer and record equivalences
   upon entry to BB.  Equivalences can come from the edge traversed to
   reach BB or they may come from PHI nodes at the start of BB.  */

/* Pop items off the unwinding stack, removing each from the hash table
   until a marker is encountered.  */

void
avail_exprs_stack::pop_to_marker ()
{
  /* Remove all the expressions made available in this block.  */
  while (m_stack.length () > 0)
    {
      std::pair<expr_hash_elt_t, expr_hash_elt_t> victim = m_stack.pop ();
      expr_hash_elt **slot;

      if (victim.first == NULL)
	break;

      /* This must precede the actual removal from the hash table,
         as ELEMENT and the table entry may share a call argument
         vector which will be freed during removal.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "<<<< ");
	  victim.first->print (dump_file);
        }

      slot = m_avail_exprs->find_slot (victim.first, NO_INSERT);
      gcc_assert (slot && *slot == victim.first);
      if (victim.second != NULL)
	{
	  delete *slot;
	  *slot = victim.second;
	}
      else
	m_avail_exprs->clear_slot (slot);
    }
}

/* Add <ELT1,ELT2> to the unwinding stack so they can be later removed
   from the hash table.  */

void
avail_exprs_stack::record_expr (class expr_hash_elt *elt1,
				class expr_hash_elt *elt2,
				char type)
{
  if (elt1 && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "%c>>> ", type);
      elt1->print (dump_file);
    }

  m_stack.safe_push (std::pair<expr_hash_elt_t, expr_hash_elt_t> (elt1, elt2));
}

/* Generate a hash value for a pair of expressions.  This can be used
   iteratively by passing a previous result in HSTATE.

   The same hash value is always returned for a given pair of expressions,
   regardless of the order in which they are presented.  This is useful in
   hashing the operands of commutative functions.  */

namespace inchash
{

static void
add_expr_commutative (const_tree t1, const_tree t2, hash &hstate)
{
  hash one, two;

  inchash::add_expr (t1, one);
  inchash::add_expr (t2, two);
  hstate.add_commutative (one, two);
}

/* Compute a hash value for a hashable_expr value EXPR and a
   previously accumulated hash value VAL.  If two hashable_expr
   values compare equal with hashable_expr_equal_p, they must
   hash to the same value, given an identical value of VAL.
   The logic is intended to follow inchash::add_expr in tree.c.  */

static void
add_hashable_expr (const struct hashable_expr *expr, hash &hstate)
{
  switch (expr->kind)
    {
    case EXPR_SINGLE:
      inchash::add_expr (expr->ops.single.rhs, hstate);
      break;

    case EXPR_UNARY:
      hstate.add_object (expr->ops.unary.op);

      /* Make sure to include signedness in the hash computation.
         Don't hash the type, that can lead to having nodes which
         compare equal according to operand_equal_p, but which
         have different hash codes.  */
      if (CONVERT_EXPR_CODE_P (expr->ops.unary.op)
          || expr->ops.unary.op == NON_LVALUE_EXPR)
        hstate.add_int (TYPE_UNSIGNED (expr->type));

      inchash::add_expr (expr->ops.unary.opnd, hstate);
      break;

    case EXPR_BINARY:
      hstate.add_object (expr->ops.binary.op);
      if (commutative_tree_code (expr->ops.binary.op))
	inchash::add_expr_commutative (expr->ops.binary.opnd0,
					  expr->ops.binary.opnd1, hstate);
      else
        {
          inchash::add_expr (expr->ops.binary.opnd0, hstate);
          inchash::add_expr (expr->ops.binary.opnd1, hstate);
        }
      break;

    case EXPR_TERNARY:
      hstate.add_object (expr->ops.ternary.op);
      if (commutative_ternary_tree_code (expr->ops.ternary.op))
	inchash::add_expr_commutative (expr->ops.ternary.opnd0,
					  expr->ops.ternary.opnd1, hstate);
      else
        {
          inchash::add_expr (expr->ops.ternary.opnd0, hstate);
          inchash::add_expr (expr->ops.ternary.opnd1, hstate);
        }
      inchash::add_expr (expr->ops.ternary.opnd2, hstate);
      break;

    case EXPR_CALL:
      {
        size_t i;
        enum tree_code code = CALL_EXPR;
        gcall *fn_from;

        hstate.add_object (code);
        fn_from = expr->ops.call.fn_from;
        if (gimple_call_internal_p (fn_from))
          hstate.merge_hash ((hashval_t) gimple_call_internal_fn (fn_from));
        else
          inchash::add_expr (gimple_call_fn (fn_from), hstate);
        for (i = 0; i < expr->ops.call.nargs; i++)
          inchash::add_expr (expr->ops.call.args[i], hstate);
      }
      break;

    case EXPR_PHI:
      {
        size_t i;

        for (i = 0; i < expr->ops.phi.nargs; i++)
          inchash::add_expr (expr->ops.phi.args[i], hstate);
      }
      break;

    default:
      gcc_unreachable ();
    }
}

}

/* Hashing and equality functions.  We compute a value number for expressions
   using the code of the expression and the SSA numbers of its operands.  */

static hashval_t
avail_expr_hash (class expr_hash_elt *p)
{
  const struct hashable_expr *expr = p->expr ();
  inchash::hash hstate;

  if (expr->kind == EXPR_SINGLE)
    {
      /* T could potentially be a switch index or a goto dest.  */
      tree t = expr->ops.single.rhs;
      if (TREE_CODE (t) == MEM_REF || handled_component_p (t))
	{
	  /* Make equivalent statements of both these kinds hash together.
	     Dealing with both MEM_REF and ARRAY_REF allows us not to care
	     about equivalence with other statements not considered here.  */
	  bool reverse;
	  HOST_WIDE_INT offset, size, max_size;
	  tree base = get_ref_base_and_extent (t, &offset, &size, &max_size,
					       &reverse);
	  /* Strictly, we could try to normalize variable-sized accesses too,
	    but here we just deal with the common case.  */
	  if (size != -1
	      && size == max_size)
	    {
	      enum tree_code code = MEM_REF;
	      hstate.add_object (code);
	      inchash::add_expr (base, hstate);
	      hstate.add_object (offset);
	      hstate.add_object (size);
	      return hstate.end ();
	    }
	}
    }

  inchash::add_hashable_expr (expr, hstate);

  return hstate.end ();
}

/* Compares trees T0 and T1 to see if they are MEM_REF or ARRAY_REFs equivalent
   to each other.  (That is, they return the value of the same bit of memory.)

   Return TRUE if the two are so equivalent; FALSE if not (which could still
   mean the two are equivalent by other means).  */

static bool
equal_mem_array_ref_p (tree t0, tree t1)
{
  if (TREE_CODE (t0) != MEM_REF && ! handled_component_p (t0))
    return false;
  if (TREE_CODE (t1) != MEM_REF && ! handled_component_p (t1))
    return false;

  if (!types_compatible_p (TREE_TYPE (t0), TREE_TYPE (t1)))
    return false;
  bool rev0;
  HOST_WIDE_INT off0, sz0, max0;
  tree base0 = get_ref_base_and_extent (t0, &off0, &sz0, &max0, &rev0);
  if (sz0 == -1
      || sz0 != max0)
    return false;

  bool rev1;
  HOST_WIDE_INT off1, sz1, max1;
  tree base1 = get_ref_base_and_extent (t1, &off1, &sz1, &max1, &rev1);
  if (sz1 == -1
      || sz1 != max1)
    return false;

  if (rev0 != rev1)
    return false;

  /* Types were compatible, so this is a sanity check.  */
  gcc_assert (sz0 == sz1);

  return (off0 == off1) && operand_equal_p (base0, base1, 0);
}

/* Compare two hashable_expr structures for equivalence.  They are
   considered equivalent when the expressions they denote must
   necessarily be equal.  The logic is intended to follow that of
   operand_equal_p in fold-const.c */

static bool
hashable_expr_equal_p (const struct hashable_expr *expr0,
		       const struct hashable_expr *expr1)
{
  tree type0 = expr0->type;
  tree type1 = expr1->type;

  /* If either type is NULL, there is nothing to check.  */
  if ((type0 == NULL_TREE) ^ (type1 == NULL_TREE))
    return false;

  /* If both types don't have the same signedness, precision, and mode,
     then we can't consider  them equal.  */
  if (type0 != type1
      && (TREE_CODE (type0) == ERROR_MARK
	  || TREE_CODE (type1) == ERROR_MARK
	  || TYPE_UNSIGNED (type0) != TYPE_UNSIGNED (type1)
	  || TYPE_PRECISION (type0) != TYPE_PRECISION (type1)
	  || TYPE_MODE (type0) != TYPE_MODE (type1)))
    return false;

  if (expr0->kind != expr1->kind)
    return false;

  switch (expr0->kind)
    {
    case EXPR_SINGLE:
      return equal_mem_array_ref_p (expr0->ops.single.rhs,
				    expr1->ops.single.rhs)
	     || operand_equal_p (expr0->ops.single.rhs,
				 expr1->ops.single.rhs, 0);
    case EXPR_UNARY:
      if (expr0->ops.unary.op != expr1->ops.unary.op)
        return false;

      if ((CONVERT_EXPR_CODE_P (expr0->ops.unary.op)
           || expr0->ops.unary.op == NON_LVALUE_EXPR)
          && TYPE_UNSIGNED (expr0->type) != TYPE_UNSIGNED (expr1->type))
        return false;

      return operand_equal_p (expr0->ops.unary.opnd,
                              expr1->ops.unary.opnd, 0);

    case EXPR_BINARY:
      if (expr0->ops.binary.op != expr1->ops.binary.op)
	return false;

      if (operand_equal_p (expr0->ops.binary.opnd0,
			   expr1->ops.binary.opnd0, 0)
	  && operand_equal_p (expr0->ops.binary.opnd1,
			      expr1->ops.binary.opnd1, 0))
	return true;

      /* For commutative ops, allow the other order.  */
      return (commutative_tree_code (expr0->ops.binary.op)
	      && operand_equal_p (expr0->ops.binary.opnd0,
				  expr1->ops.binary.opnd1, 0)
	      && operand_equal_p (expr0->ops.binary.opnd1,
				  expr1->ops.binary.opnd0, 0));

    case EXPR_TERNARY:
      if (expr0->ops.ternary.op != expr1->ops.ternary.op
	  || !operand_equal_p (expr0->ops.ternary.opnd2,
			       expr1->ops.ternary.opnd2, 0))
	return false;

      if (operand_equal_p (expr0->ops.ternary.opnd0,
			   expr1->ops.ternary.opnd0, 0)
	  && operand_equal_p (expr0->ops.ternary.opnd1,
			      expr1->ops.ternary.opnd1, 0))
	return true;

      /* For commutative ops, allow the other order.  */
      return (commutative_ternary_tree_code (expr0->ops.ternary.op)
	      && operand_equal_p (expr0->ops.ternary.opnd0,
				  expr1->ops.ternary.opnd1, 0)
	      && operand_equal_p (expr0->ops.ternary.opnd1,
				  expr1->ops.ternary.opnd0, 0));

    case EXPR_CALL:
      {
        size_t i;

        /* If the calls are to different functions, then they
           clearly cannot be equal.  */
        if (!gimple_call_same_target_p (expr0->ops.call.fn_from,
                                        expr1->ops.call.fn_from))
          return false;

        if (! expr0->ops.call.pure)
          return false;

        if (expr0->ops.call.nargs !=  expr1->ops.call.nargs)
          return false;

        for (i = 0; i < expr0->ops.call.nargs; i++)
          if (! operand_equal_p (expr0->ops.call.args[i],
                                 expr1->ops.call.args[i], 0))
            return false;

	if (stmt_could_throw_p (expr0->ops.call.fn_from))
	  {
	    int lp0 = lookup_stmt_eh_lp (expr0->ops.call.fn_from);
	    int lp1 = lookup_stmt_eh_lp (expr1->ops.call.fn_from);
	    if ((lp0 > 0 || lp1 > 0) && lp0 != lp1)
	      return false;
	  }

        return true;
      }

    case EXPR_PHI:
      {
        size_t i;

        if (expr0->ops.phi.nargs !=  expr1->ops.phi.nargs)
          return false;

        for (i = 0; i < expr0->ops.phi.nargs; i++)
          if (! operand_equal_p (expr0->ops.phi.args[i],
                                 expr1->ops.phi.args[i], 0))
            return false;

        return true;
      }

    default:
      gcc_unreachable ();
    }
}

/* Given a statement STMT, construct a hash table element.  */

expr_hash_elt::expr_hash_elt (gimple *stmt, tree orig_lhs)
{
  enum gimple_code code = gimple_code (stmt);
  struct hashable_expr *expr = this->expr ();

  if (code == GIMPLE_ASSIGN)
    {
      enum tree_code subcode = gimple_assign_rhs_code (stmt);

      switch (get_gimple_rhs_class (subcode))
        {
        case GIMPLE_SINGLE_RHS:
	  expr->kind = EXPR_SINGLE;
	  expr->type = TREE_TYPE (gimple_assign_rhs1 (stmt));
	  expr->ops.single.rhs = gimple_assign_rhs1 (stmt);
	  break;
        case GIMPLE_UNARY_RHS:
	  expr->kind = EXPR_UNARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  if (CONVERT_EXPR_CODE_P (subcode))
	    subcode = NOP_EXPR;
	  expr->ops.unary.op = subcode;
	  expr->ops.unary.opnd = gimple_assign_rhs1 (stmt);
	  break;
        case GIMPLE_BINARY_RHS:
	  expr->kind = EXPR_BINARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.binary.op = subcode;
	  expr->ops.binary.opnd0 = gimple_assign_rhs1 (stmt);
	  expr->ops.binary.opnd1 = gimple_assign_rhs2 (stmt);
	  break;
        case GIMPLE_TERNARY_RHS:
	  expr->kind = EXPR_TERNARY;
	  expr->type = TREE_TYPE (gimple_assign_lhs (stmt));
	  expr->ops.ternary.op = subcode;
	  expr->ops.ternary.opnd0 = gimple_assign_rhs1 (stmt);
	  expr->ops.ternary.opnd1 = gimple_assign_rhs2 (stmt);
	  expr->ops.ternary.opnd2 = gimple_assign_rhs3 (stmt);
	  break;
        default:
          gcc_unreachable ();
        }
    }
  else if (code == GIMPLE_COND)
    {
      expr->type = boolean_type_node;
      expr->kind = EXPR_BINARY;
      expr->ops.binary.op = gimple_cond_code (stmt);
      expr->ops.binary.opnd0 = gimple_cond_lhs (stmt);
      expr->ops.binary.opnd1 = gimple_cond_rhs (stmt);
    }
  else if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
    {
      size_t nargs = gimple_call_num_args (call_stmt);
      size_t i;

      gcc_assert (gimple_call_lhs (call_stmt));

      expr->type = TREE_TYPE (gimple_call_lhs (call_stmt));
      expr->kind = EXPR_CALL;
      expr->ops.call.fn_from = call_stmt;

      if (gimple_call_flags (call_stmt) & (ECF_CONST | ECF_PURE))
        expr->ops.call.pure = true;
      else
        expr->ops.call.pure = false;

      expr->ops.call.nargs = nargs;
      expr->ops.call.args = XCNEWVEC (tree, nargs);
      for (i = 0; i < nargs; i++)
        expr->ops.call.args[i] = gimple_call_arg (call_stmt, i);
    }
  else if (gswitch *swtch_stmt = dyn_cast <gswitch *> (stmt))
    {
      expr->type = TREE_TYPE (gimple_switch_index (swtch_stmt));
      expr->kind = EXPR_SINGLE;
      expr->ops.single.rhs = gimple_switch_index (swtch_stmt);
    }
  else if (code == GIMPLE_GOTO)
    {
      expr->type = TREE_TYPE (gimple_goto_dest (stmt));
      expr->kind = EXPR_SINGLE;
      expr->ops.single.rhs = gimple_goto_dest (stmt);
    }
  else if (code == GIMPLE_PHI)
    {
      size_t nargs = gimple_phi_num_args (stmt);
      size_t i;

      expr->type = TREE_TYPE (gimple_phi_result (stmt));
      expr->kind = EXPR_PHI;
      expr->ops.phi.nargs = nargs;
      expr->ops.phi.args = XCNEWVEC (tree, nargs);
      for (i = 0; i < nargs; i++)
        expr->ops.phi.args[i] = gimple_phi_arg_def (stmt, i);
    }
  else
    gcc_unreachable ();

  m_lhs = orig_lhs;
  m_vop = gimple_vuse (stmt);
  m_hash = avail_expr_hash (this);
  m_stamp = this;
}

/* Given a hashable_expr expression ORIG and an ORIG_LHS,
   construct a hash table element.  */

expr_hash_elt::expr_hash_elt (struct hashable_expr *orig, tree orig_lhs)
{
  m_expr = *orig;
  m_lhs = orig_lhs;
  m_vop = NULL_TREE;
  m_hash = avail_expr_hash (this);
  m_stamp = this;
}

/* Copy constructor for a hash table element.  */

expr_hash_elt::expr_hash_elt (class expr_hash_elt &old_elt)
{
  m_expr = old_elt.m_expr;
  m_lhs = old_elt.m_lhs;
  m_vop = old_elt.m_vop;
  m_hash = old_elt.m_hash;
  m_stamp = this;

  /* Now deep copy the malloc'd space for CALL and PHI args.  */
  if (old_elt.m_expr.kind == EXPR_CALL)
    {
      size_t nargs = old_elt.m_expr.ops.call.nargs;
      size_t i;

      m_expr.ops.call.args = XCNEWVEC (tree, nargs);
      for (i = 0; i < nargs; i++)
        m_expr.ops.call.args[i] = old_elt.m_expr.ops.call.args[i];
    }
  else if (old_elt.m_expr.kind == EXPR_PHI)
    {
      size_t nargs = old_elt.m_expr.ops.phi.nargs;
      size_t i;

      m_expr.ops.phi.args = XCNEWVEC (tree, nargs);
      for (i = 0; i < nargs; i++)
        m_expr.ops.phi.args[i] = old_elt.m_expr.ops.phi.args[i];
    }
}

/* Calls and PHIs have a variable number of arguments that are allocated
   on the heap.  Thus we have to have a special dtor to release them.  */

expr_hash_elt::~expr_hash_elt ()
{
  if (m_expr.kind == EXPR_CALL)
    free (m_expr.ops.call.args);
  else if (m_expr.kind == EXPR_PHI)
    free (m_expr.ops.phi.args);
}

/* Print a diagnostic dump of an expression hash table entry.  */

void
expr_hash_elt::print (FILE *stream)
{
  fprintf (stream, "STMT ");

  if (m_lhs)
    {
      print_generic_expr (stream, m_lhs, 0);
      fprintf (stream, " = ");
    }

  switch (m_expr.kind)
    {
      case EXPR_SINGLE:
        print_generic_expr (stream, m_expr.ops.single.rhs, 0);
        break;

      case EXPR_UNARY:
	fprintf (stream, "%s ", get_tree_code_name (m_expr.ops.unary.op));
        print_generic_expr (stream, m_expr.ops.unary.opnd, 0);
        break;

      case EXPR_BINARY:
        print_generic_expr (stream, m_expr.ops.binary.opnd0, 0);
	fprintf (stream, " %s ", get_tree_code_name (m_expr.ops.binary.op));
        print_generic_expr (stream, m_expr.ops.binary.opnd1, 0);
        break;

      case EXPR_TERNARY:
	fprintf (stream, " %s <", get_tree_code_name (m_expr.ops.ternary.op));
        print_generic_expr (stream, m_expr.ops.ternary.opnd0, 0);
	fputs (", ", stream);
        print_generic_expr (stream, m_expr.ops.ternary.opnd1, 0);
	fputs (", ", stream);
        print_generic_expr (stream, m_expr.ops.ternary.opnd2, 0);
	fputs (">", stream);
        break;

      case EXPR_CALL:
        {
          size_t i;
          size_t nargs = m_expr.ops.call.nargs;
          gcall *fn_from;

          fn_from = m_expr.ops.call.fn_from;
          if (gimple_call_internal_p (fn_from))
            fputs (internal_fn_name (gimple_call_internal_fn (fn_from)),
                   stream);
          else
            print_generic_expr (stream, gimple_call_fn (fn_from), 0);
          fprintf (stream, " (");
          for (i = 0; i < nargs; i++)
            {
              print_generic_expr (stream, m_expr.ops.call.args[i], 0);
              if (i + 1 < nargs)
                fprintf (stream, ", ");
            }
          fprintf (stream, ")");
        }
        break;

      case EXPR_PHI:
        {
          size_t i;
          size_t nargs = m_expr.ops.phi.nargs;

          fprintf (stream, "PHI <");
          for (i = 0; i < nargs; i++)
            {
              print_generic_expr (stream, m_expr.ops.phi.args[i], 0);
              if (i + 1 < nargs)
                fprintf (stream, ", ");
            }
          fprintf (stream, ">");
        }
        break;
    }

  if (m_vop)
    {
      fprintf (stream, " with ");
      print_generic_expr (stream, m_vop, 0);
    }

  fprintf (stream, "\n");
}

/* Pop entries off the stack until we hit the NULL marker.
   For each entry popped, use the SRC/DEST pair to restore
   SRC to its prior value.  */

void
const_and_copies::pop_to_marker (void)
{
  while (m_stack.length () > 0)
    {
      tree prev_value, dest;

      dest = m_stack.pop ();

      /* A NULL value indicates we should stop unwinding, otherwise
	 pop off the next entry as they're recorded in pairs.  */
      if (dest == NULL)
	break;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "<<<< COPY ");
	  print_generic_expr (dump_file, dest, 0);
	  fprintf (dump_file, " = ");
	  print_generic_expr (dump_file, SSA_NAME_VALUE (dest), 0);
	  fprintf (dump_file, "\n");
	}

      prev_value = m_stack.pop ();
      set_ssa_name_value (dest, prev_value);
    }
}

/* Record that X has the value Y and that X's previous value is PREV_X. 

   This variant does not follow the value chain for Y.  */

void
const_and_copies::record_const_or_copy_raw (tree x, tree y, tree prev_x)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "0>>> COPY ");
      print_generic_expr (dump_file, x, 0);
      fprintf (dump_file, " = ");
      print_generic_expr (dump_file, y, 0);
      fprintf (dump_file, "\n");
    }

  set_ssa_name_value (x, y);
  m_stack.reserve (2);
  m_stack.quick_push (prev_x);
  m_stack.quick_push (x);
}

/* Record that X has the value Y.  */

void
const_and_copies::record_const_or_copy (tree x, tree y)
{
  record_const_or_copy (x, y, SSA_NAME_VALUE (x));
}

/* Record that X has the value Y and that X's previous value is PREV_X. 

   This variant follow's Y value chain.  */

void
const_and_copies::record_const_or_copy (tree x, tree y, tree prev_x)
{
  /* Y may be NULL if we are invalidating entries in the table.  */
  if (y && TREE_CODE (y) == SSA_NAME)
    {
      tree tmp = SSA_NAME_VALUE (y);
      y = tmp ? tmp : y;
    }

  record_const_or_copy_raw (x, y, prev_x);
}

bool
expr_elt_hasher::equal (const value_type &p1, const compare_type &p2)
{
  const struct hashable_expr *expr1 = p1->expr ();
  const struct expr_hash_elt *stamp1 = p1->stamp ();
  const struct hashable_expr *expr2 = p2->expr ();
  const struct expr_hash_elt *stamp2 = p2->stamp ();

  /* This case should apply only when removing entries from the table.  */
  if (stamp1 == stamp2)
    return true;

  if (p1->hash () != p2->hash ())
    return false;

  /* In case of a collision, both RHS have to be identical and have the
     same VUSE operands.  */
  if (hashable_expr_equal_p (expr1, expr2)
      && types_compatible_p (expr1->type, expr2->type))
    return true;

  return false;
}

/* Given a conditional expression COND as a tree, initialize
   a hashable_expr expression EXPR.  The conditional must be a
   comparison or logical negation.  A constant or a variable is
   not permitted.  */

void
initialize_expr_from_cond (tree cond, struct hashable_expr *expr)
{
  expr->type = boolean_type_node;

  if (COMPARISON_CLASS_P (cond))
    {
      expr->kind = EXPR_BINARY;
      expr->ops.binary.op = TREE_CODE (cond);
      expr->ops.binary.opnd0 = TREE_OPERAND (cond, 0);
      expr->ops.binary.opnd1 = TREE_OPERAND (cond, 1);
    }
  else if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
    {
      expr->kind = EXPR_UNARY;
      expr->ops.unary.op = TRUTH_NOT_EXPR;
      expr->ops.unary.opnd = TREE_OPERAND (cond, 0);
    }
  else
    gcc_unreachable ();
}

