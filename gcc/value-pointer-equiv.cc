/* Context-aware pointer equivalence tracker.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "alloc-pool.h"
#include "domwalk.h"
#include "tree-cfgcleanup.h"
#include "vr-values.h"
#include "gimple-range.h"
#include "fold-const.h"
#include "value-pointer-equiv.h"

// Unwindable SSA equivalence table for pointers.
//
// The main query point is get_replacement() which returns what a
// given SSA can be replaced with in the current scope.

class ssa_equiv_stack
{
public:
  ssa_equiv_stack ();
  void enter (basic_block);
  void leave (basic_block);
  void push_replacement (tree name, tree replacement);
  tree get_replacement (tree name);

private:
  auto_vec<std::pair <tree, tree>> m_stack;
  auto_vec<tree> m_replacements;
  const std::pair <tree, tree> m_marker = std::make_pair (NULL_TREE, NULL_TREE);
};

ssa_equiv_stack::ssa_equiv_stack ()
{
  m_replacements.safe_grow_cleared (num_ssa_names + 1);
}

// Pushes a marker at the given point.

void
ssa_equiv_stack::enter (basic_block)
{
  m_stack.safe_push (m_marker);
}

// Pops the stack to the last marker, while performing replacements
// along the way.

void
ssa_equiv_stack::leave (basic_block)
{
  gcc_checking_assert (!m_stack.is_empty ());
  while (m_stack.last () != m_marker)
    {
      std::pair<tree, tree> e = m_stack.pop ();
      m_replacements[SSA_NAME_VERSION (e.first)] = e.second;
    }
  m_stack.pop ();
}

// Set the equivalence of NAME to REPLACEMENT.

void
ssa_equiv_stack::push_replacement (tree name, tree replacement)
{
  unsigned v = SSA_NAME_VERSION (name);

  if (v >= m_replacements.length ())
    m_replacements.safe_grow_cleared (num_ssa_names + 1);

  tree old = m_replacements[v];
  m_replacements[v] = replacement;
  m_stack.safe_push (std::make_pair (name, old));
}

// Return the equivalence of NAME.

tree
ssa_equiv_stack::get_replacement (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);

  if (v >= m_replacements.length ())
    m_replacements.safe_grow_cleared (num_ssa_names + 1);

  return m_replacements[v];
}

pointer_equiv_analyzer::pointer_equiv_analyzer (gimple_ranger *r)
{
  m_ranger = r;
  m_global_points.safe_grow_cleared (num_ssa_names + 1);
  m_cond_points = new ssa_equiv_stack;
}

pointer_equiv_analyzer::~pointer_equiv_analyzer ()
{
  delete m_cond_points;
}

// Set the global pointer equivalency for SSA to POINTEE.

void
pointer_equiv_analyzer::set_global_equiv (tree ssa, tree pointee)
{
  unsigned v = SSA_NAME_VERSION (ssa);

  if (v >= m_global_points.length ())
    m_global_points.safe_grow_cleared (num_ssa_names + 1);

  m_global_points[v] = pointee;
}

// Set the conditional pointer equivalency for SSA to POINTEE.

void
pointer_equiv_analyzer::set_cond_equiv (tree ssa, tree pointee)
{
  m_cond_points->push_replacement (ssa, pointee);
}

// Return the current pointer equivalency info for SSA, or NULL if
// none is available.  Note that global info takes priority over
// conditional info.

tree
pointer_equiv_analyzer::get_equiv (tree ssa)
{
  unsigned v = SSA_NAME_VERSION (ssa);

  if (v >= m_global_points.length ())
    m_global_points.safe_grow_cleared (num_ssa_names + 1);

  tree ret = m_global_points[v];
  if (ret)
    return ret;
  return m_cond_points->get_replacement (ssa);
}

// Method to be called on entry to a BB.

void
pointer_equiv_analyzer::enter (basic_block bb)
{
  m_cond_points->enter (bb);

  for (gphi_iterator iter = gsi_start_phis (bb);
       !gsi_end_p (iter);
       gsi_next (&iter))
    {
      gphi *phi = iter.phi ();
      tree lhs = gimple_phi_result (phi);
      if (!POINTER_TYPE_P (TREE_TYPE (lhs)))
	continue;
      tree arg0 = gimple_phi_arg_def (phi, 0);
      if (TREE_CODE (arg0) == SSA_NAME && !is_gimple_min_invariant (arg0))
	arg0 = get_equiv (arg0);
      if (arg0 && is_gimple_min_invariant (arg0))
	{
	  // If all the PHI args point to the same place, set the
	  // pointer equivalency info for the PHI result.  This can
	  // happen for passes that create redundant PHIs like
	  // PHI<&foo, &foo> or PHI<&foo>.
	  for (size_t i = 1; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree argi = gimple_phi_arg_def (phi, i);
	      if (TREE_CODE (argi) == SSA_NAME
		  && !is_gimple_min_invariant (argi))
		argi = get_equiv (argi);
	      if (!argi || !operand_equal_p (arg0, argi))
		return;
	    }
	  set_global_equiv (lhs, arg0);
	}
    }

  edge pred = single_pred_edge_ignoring_loop_edges (bb, false);
  if (pred)
    visit_edge (pred);
}

// Method to be called on exit from a BB.

void
pointer_equiv_analyzer::leave (basic_block bb)
{
  m_cond_points->leave (bb);
}

// Helper function to return the pointer equivalency information for
// EXPR from a gimple statement with CODE.  This returns either the
// cached pointer equivalency info for an SSA, or an invariant in case
// EXPR is one (i.e. &foo).  Returns NULL if EXPR is neither an SSA
// nor an invariant.

tree
pointer_equiv_analyzer::get_equiv_expr (tree_code code, tree expr)
{
  if (code == SSA_NAME)
    return get_equiv (expr);

  if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS
      && is_gimple_min_invariant (expr))
    return expr;

  return NULL;
}

// Hack to provide context to the gimple fold callback.
static struct
{
  gimple *m_stmt;
  gimple_ranger *m_ranger;
  pointer_equiv_analyzer *m_pta;
} x_fold_context;

// Gimple fold callback.
static tree
pta_valueize (tree name)
{
  tree ret
    = x_fold_context.m_ranger->value_of_expr (name, x_fold_context.m_stmt);

  if (!ret && supported_pointer_equiv_p (name))
    ret = x_fold_context.m_pta->get_equiv (name);

  return ret ? ret : name;
}

// Method to be called on gimple statements during traversal of the IL.

void
pointer_equiv_analyzer::visit_stmt (gimple *stmt)
{
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return;

  tree lhs = gimple_assign_lhs (stmt);
  if (!supported_pointer_equiv_p (lhs))
    return;

  tree rhs = gimple_assign_rhs1 (stmt);
  rhs = get_equiv_expr (gimple_assign_rhs_code (stmt), rhs);
  if (rhs)
    {
      set_global_equiv (lhs, rhs);
      return;
    }

  // If we couldn't find anything, try fold.
  x_fold_context = { stmt, m_ranger, this};
  rhs = gimple_fold_stmt_to_constant_1 (stmt, pta_valueize, pta_valueize);
  if (rhs)
    {
      rhs = get_equiv_expr (TREE_CODE (rhs), rhs);
      if (rhs)
	{
	  set_global_equiv (lhs, rhs);
	  return;
	}
    }
}

// If the edge in E is a conditional that sets a pointer equality, set the
// conditional pointer equivalency information.

void
pointer_equiv_analyzer::visit_edge (edge e)
{
  gcond *stmt = safe_dyn_cast <gcond *> (*gsi_last_bb (e->src));
  tree lhs;
  // Recognize: x_13 [==,!=] &foo.
  if (stmt
      && ((lhs = gimple_cond_lhs (stmt)), true)
      && TREE_CODE (lhs) == SSA_NAME
      && POINTER_TYPE_P (TREE_TYPE (lhs))
      && TREE_CODE (gimple_cond_rhs (stmt)) == ADDR_EXPR)
    {
      tree_code code = gimple_cond_code (stmt);
      if ((code == EQ_EXPR && e->flags & EDGE_TRUE_VALUE)
	  || ((code == NE_EXPR && e->flags & EDGE_FALSE_VALUE)))
	set_cond_equiv (lhs, gimple_cond_rhs (stmt));
    }
}
