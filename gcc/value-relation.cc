/* Header file for the value range relational processing.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"

#include "gimple-range.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "alloc-pool.h"
#include "dominance.h"

static const char *const kind_string[VREL_LAST] =
{ "varying", "undefined", "<", "<=", ">", ">=", "==", "!=", "pe8", "pe16",
  "pe32", "pe64" };

// Print a relation_kind REL to file F.

void
print_relation (FILE *f, relation_kind rel)
{
  fprintf (f, " %s ", kind_string[rel]);
}

// This table is used to negate the operands.  op1 REL op2 -> !(op1 REL op2).
// Partial equivalence can't be negated, so VARYING is correct.
static const unsigned char rr_negate_table[VREL_LAST] = {
  VREL_VARYING, VREL_UNDEFINED, VREL_GE, VREL_GT, VREL_LE, VREL_LT, VREL_NE,
  VREL_EQ, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING };

// Negate the relation, as in logical negation.

relation_kind
relation_negate (relation_kind r)
{
  return relation_kind (rr_negate_table [r]);
}

// This table is used to swap the operands.  op1 REL op2 -> op2 REL op1.
// Partial equivalences swap to themselves as the low N bits are equal.
static const unsigned char rr_swap_table[VREL_LAST] = {
  VREL_VARYING, VREL_UNDEFINED, VREL_GT, VREL_GE, VREL_LT, VREL_LE, VREL_EQ,
  VREL_NE, VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 };

// Return the relation as if the operands were swapped.

relation_kind
relation_swap (relation_kind r)
{
  return relation_kind (rr_swap_table [r]);
}

// This table is used to perform an intersection between 2 relations.
// EQ is a full equivalency, and thus replaces any partial equivalence.
// Likewise, the higher bit PE is more "equivalent" than the lower bit version
// and thus more restrictive.

static const unsigned char rr_intersect_table[VREL_LAST][VREL_LAST] = {
// VREL_VARYING
  { VREL_VARYING, VREL_UNDEFINED, VREL_LT, VREL_LE, VREL_GT, VREL_GE, VREL_EQ,
    VREL_NE, VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 },
// VREL_UNDEFINED
  { VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED,
    VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED,
    VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED },
// VREL_LT
  { VREL_LT, VREL_UNDEFINED, VREL_LT, VREL_LT, VREL_UNDEFINED, VREL_UNDEFINED,
    VREL_UNDEFINED, VREL_LT,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_LE
  { VREL_LE, VREL_UNDEFINED, VREL_LT, VREL_LE, VREL_UNDEFINED, VREL_EQ,
    VREL_EQ, VREL_LT,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_GT
  { VREL_GT, VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED, VREL_GT, VREL_GT,
    VREL_UNDEFINED, VREL_GT,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_GE
  { VREL_GE, VREL_UNDEFINED, VREL_UNDEFINED, VREL_EQ, VREL_GT, VREL_GE,
    VREL_EQ, VREL_GT,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_EQ
  { VREL_EQ, VREL_UNDEFINED, VREL_UNDEFINED, VREL_EQ, VREL_UNDEFINED, VREL_EQ,
    VREL_EQ, VREL_UNDEFINED,
    VREL_EQ, VREL_EQ, VREL_EQ, VREL_EQ },
// VREL_NE
  { VREL_NE, VREL_UNDEFINED, VREL_LT, VREL_LT, VREL_GT, VREL_GT,
    VREL_UNDEFINED, VREL_NE,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_PE8
  { VREL_PE8, VREL_UNDEFINED, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_EQ, VREL_VARYING,
    VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 },
// VREL_PE16
  { VREL_PE16, VREL_UNDEFINED, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_EQ, VREL_VARYING,
    VREL_PE16, VREL_PE16, VREL_PE32, VREL_PE64 },
// VREL_PE32
  { VREL_PE32, VREL_UNDEFINED, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_EQ, VREL_VARYING,
    VREL_PE32, VREL_PE32, VREL_PE32, VREL_PE64 },
// VREL_PE64
  { VREL_PE64, VREL_UNDEFINED, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_EQ, VREL_VARYING,
    VREL_PE64, VREL_PE64, VREL_PE64, VREL_PE64 } };


// Intersect relation R1 with relation R2 and return the resulting relation.

relation_kind
relation_intersect (relation_kind r1, relation_kind r2)
{
  return relation_kind (rr_intersect_table[r1][r2]);
}


// This table is used to perform a union between 2 relations.
// EQ unions with a PE to produce the same PE, and likewise whichever PE
// has the least common bits forms the union.

static const unsigned char rr_union_table[VREL_LAST][VREL_LAST] = {
// VREL_VARYING
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_UNDEFINED
  { VREL_VARYING, VREL_UNDEFINED, VREL_LT, VREL_LE, VREL_GT, VREL_GE,
    VREL_EQ, VREL_NE, VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 },
// VREL_LT
  { VREL_VARYING, VREL_LT, VREL_LT, VREL_LE, VREL_NE, VREL_VARYING, VREL_LE,
    VREL_NE, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_LE
  { VREL_VARYING, VREL_LE, VREL_LE, VREL_LE, VREL_VARYING, VREL_VARYING,
    VREL_LE, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_GT
  { VREL_VARYING, VREL_GT, VREL_NE, VREL_VARYING, VREL_GT, VREL_GE, VREL_GE,
    VREL_NE, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_GE
  { VREL_VARYING, VREL_GE, VREL_VARYING, VREL_VARYING, VREL_GE, VREL_GE,
    VREL_GE, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_EQ
  { VREL_VARYING, VREL_EQ, VREL_LE, VREL_LE, VREL_GE, VREL_GE, VREL_EQ,
    VREL_VARYING, VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 },
// VREL_NE
  { VREL_VARYING, VREL_NE, VREL_NE, VREL_VARYING, VREL_NE, VREL_VARYING,
    VREL_VARYING, VREL_NE,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_PE8
  { VREL_VARYING, VREL_PE8, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE8, VREL_VARYING,
    VREL_PE8, VREL_PE8, VREL_PE8, VREL_PE8 },
// VREL_PE16
  { VREL_VARYING, VREL_PE16, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE16, VREL_VARYING,
    VREL_PE8, VREL_PE16, VREL_PE16, VREL_PE16 },
// VREL_PE32
  { VREL_VARYING, VREL_PE32, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE32, VREL_VARYING,
    VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE32 },
// VREL_PE64
  { VREL_VARYING, VREL_PE64, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE64, VREL_VARYING,
    VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 } };

// Union relation R1 with relation R2 and return the result.

relation_kind
relation_union (relation_kind r1, relation_kind r2)
{
  return relation_kind (rr_union_table[r1][r2]);
}


// This table is used to determine transitivity between 2 relations.
// (A relation0 B) and (B relation1 C) implies  (A result C)
// Chaining two partial equivalences leaves only the bits both agree on, ie
// the narrower of the two.

static const unsigned char rr_transitive_table[VREL_LAST][VREL_LAST] = {
// VREL_VARYING
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_UNDEFINED
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_LT
  { VREL_VARYING, VREL_VARYING, VREL_LT, VREL_LT, VREL_VARYING, VREL_VARYING,
    VREL_LT, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_LE
  { VREL_VARYING, VREL_VARYING, VREL_LT, VREL_LE, VREL_VARYING, VREL_VARYING,
    VREL_LE, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_GT
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_GT, VREL_GT,
    VREL_GT, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_GE
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_GT, VREL_GE,
    VREL_GE, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_EQ
  { VREL_VARYING, VREL_VARYING, VREL_LT, VREL_LE, VREL_GT, VREL_GE, VREL_EQ,
    VREL_NE, VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 },
// VREL_NE
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_NE, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_PE8
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE8, VREL_VARYING,
    VREL_PE8, VREL_PE8, VREL_PE8, VREL_PE8 },
// VREL_PE16
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE16, VREL_VARYING,
    VREL_PE8, VREL_PE16, VREL_PE16, VREL_PE16 },
// VREL_PE32
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE32, VREL_VARYING,
    VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE32 },
// VREL_PE64
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_PE64, VREL_VARYING,
    VREL_PE8, VREL_PE16, VREL_PE32, VREL_PE64 } };

// Apply transitive operation between relation R1 and relation R2, and
// return the resulting relation, if any.

relation_kind
relation_transitive (relation_kind r1, relation_kind r2)
{
  return relation_kind (rr_transitive_table[r1][r2]);
}

// When one name is an equivalence of another, ensure the equivalence
// range is correct.  Specifically for floating point, a +0 is also
// equivalent to a -0 which may not be reflected.  See PR 111694.

void
adjust_equivalence_range (vrange &range)
{
  if (range.undefined_p () || !is_a<frange> (range))
    return;

  frange fr = as_a<frange> (range);
  // If range includes 0 make sure both signs of zero are included.
  if (fr.contains_p (dconst0) || fr.contains_p (dconstm0))
    {
      frange zeros (range.type (), dconstm0, dconst0);
      range.union_ (zeros);
    }
 }

// Given an equivalence set EQUIV, set all the bits in B that are still valid
// members of EQUIV in basic block BB.

void
relation_oracle::valid_equivs (bitmap b, const_bitmap equivs, basic_block bb)
{
  unsigned i;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (equivs, 0, i, bi)
    {
      tree ssa = ssa_name (i);
      if (ssa && !SSA_NAME_IN_FREE_LIST (ssa))
	{
	  const_bitmap ssa_equiv = equiv_set (ssa, bb);
	  if (ssa_equiv == equivs)
	    bitmap_set_bit (b, i);
	}
    }
}

// Return any known relation between SSA1 and SSA2 before stmt S is executed.
// If GET_RANGE is true, query the range of both operands first to ensure
// the definitions have been processed and any relations have be created.

relation_kind
relation_oracle::query (gimple *s, tree ssa1, tree ssa2)
{
  if (TREE_CODE (ssa1) != SSA_NAME || TREE_CODE (ssa2) != SSA_NAME)
    return VREL_VARYING;
  return query (gimple_bb (s), ssa1, ssa2);
}

// Return any known relation between SSA1 and SSA2 on edge E.
// If GET_RANGE is true, query the range of both operands first to ensure
// the definitions have been processed and any relations have be created.

relation_kind
relation_oracle::query (edge e, tree ssa1, tree ssa2)
{
  basic_block bb;
  if (TREE_CODE (ssa1) != SSA_NAME || TREE_CODE (ssa2) != SSA_NAME)
    return VREL_VARYING;

  // Use destination block if it has a single predecessor, and this picks
  // up any relation on the edge.
  // Otherwise choose the src edge and the result is the same as on-exit.
  if (!single_pred_p (e->dest))
    bb = e->src;
  else
    bb = e->dest;

  return query (bb, ssa1, ssa2);
}
// -------------------------------------------------------------------------

// The very first element in the m_equiv chain is actually just a summary
// element in which the m_names bitmap is used to indicate that an ssa_name
// has an equivalence set in this block.
// This allows for much faster traversal of the DOM chain, as a search for
// SSA_NAME simply requires walking the DOM chain until a block is found
// which has the bit for SSA_NAME set. Then scan for the equivalency set in
// that block.   No previous lists need be searched.

// If SSA has an equivalence in this list, find and return it.
// Otherwise return NULL.

equiv_chain *
equiv_chain::find (unsigned ssa)
{
  equiv_chain *ptr = NULL;
  // If there are equiv sets and SSA is in one in this list, find it.
  // Otherwise return NULL.
  if (bitmap_bit_p (m_names, ssa))
    {
      for (ptr = m_next; ptr; ptr = ptr->m_next)
	if (bitmap_bit_p (ptr->m_names, ssa))
	  break;
    }
  return ptr;
}

// Dump the names in this equivalence set.

void
equiv_chain::dump (FILE *f) const
{
  bitmap_iterator bi;
  unsigned i;

  if (!m_names || bitmap_empty_p (m_names))
    return;
  fprintf (f, "Equivalence set : [");
  unsigned c = 0;
  EXECUTE_IF_SET_IN_BITMAP (m_names, 0, i, bi)
    {
      if (ssa_name (i))
	{
	  if (c++)
	    fprintf (f, ", ");
	  print_generic_expr (f, ssa_name (i), TDF_SLIM);
	}
    }
  fprintf (f, "]\n");
}

// Instantiate an equivalency oracle.

equiv_oracle::equiv_oracle ()
{
  bitmap_obstack_initialize (&m_bitmaps);
  m_equiv.create (0);
  m_equiv.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
  m_equiv_set = BITMAP_ALLOC (&m_bitmaps);
  bitmap_tree_view (m_equiv_set);
  obstack_init (&m_chain_obstack);
  m_name_info.create (0);
  m_name_info.safe_grow_cleared (num_ssa_names + 1);
  m_partial.create (0);
  m_partial.safe_grow_cleared (num_ssa_names + 1);
  // Create a bitmap to avoid registering multiple equivalences from a LHS.
  // See PR 124809.
  m_lhs_equiv_set_p = BITMAP_ALLOC (&m_bitmaps);
  bitmap_tree_view (m_lhs_equiv_set_p);
}

// Destruct an equivalency oracle.

equiv_oracle::~equiv_oracle ()
{
  m_partial.release ();
  m_name_info.release ();
  obstack_free (&m_chain_obstack, NULL);
  m_equiv.release ();
  bitmap_obstack_release (&m_bitmaps);
}

// Add a partial equivalence R between OP1 and OP2.  Return false if no
// new relation is added.

bool
equiv_oracle::add_partial_equiv (relation_kind r, tree op1, tree op2)
{
  int v1 = SSA_NAME_VERSION (op1);
  int v2 = SSA_NAME_VERSION (op2);
  int prec2 = TYPE_PRECISION (TREE_TYPE (op2));
  int bits = pe_to_bits (r);
  gcc_checking_assert (bits && prec2 >= bits);

  if (v1 >= (int)m_partial.length () || v2 >= (int)m_partial.length ())
    m_partial.safe_grow_cleared (num_ssa_names + 1);
  gcc_checking_assert (v1 < (int)m_partial.length ()
		       && v2 < (int)m_partial.length ());

  pe_slice &pe1 = m_partial[v1];
  pe_slice &pe2 = m_partial[v2];

  if (pe1.members)
    {
      // If the definition pe1 already has an entry, either the stmt is
      // being re-evaluated, or the def was used before being registered.
      // In either case, if PE2 has an entry, we simply do nothing.
      if (pe2.members)
	return false;
      // If there are no uses of op2, do not register.
      if (has_zero_uses (op2))
	return false;
      // PE1 is the LHS and already has members, so everything in the set
      // should be a slice of PE2 rather than PE1.
      pe2.code = pe_min (r, pe1.code);
      pe2.ssa_base = op2;
      pe2.members = pe1.members;
      bitmap_iterator bi;
      unsigned x;
      EXECUTE_IF_SET_IN_BITMAP (pe1.members, 0, x, bi)
	{
	  m_partial[x].ssa_base = op2;
	  m_partial[x].code = pe_min (m_partial[x].code, pe2.code);
	}
      bitmap_set_bit (pe1.members, v2);
      return true;
    }
  if (pe2.members)
    {
      // If there are no uses of op1, do not register.
      if (has_zero_uses (op1))
	return false;
      pe1.ssa_base = pe2.ssa_base;
      // If pe2 is a 16 bit value, but only an 8 bit copy, we can't be any
      // more than an 8 bit equivalence here, so choose MIN value.
      pe1.code = pe_min (r, pe2.code);
      pe1.members = pe2.members;
      bitmap_set_bit (pe1.members, v1);
    }
  else
    {
      // If there are no uses of either operand, do not register.
      if (has_zero_uses (op1) || has_zero_uses (op2))
	return false;
      // Neither name has an entry, simply create op1 as slice of op2.
      pe2.code = bits_to_pe (TYPE_PRECISION (TREE_TYPE (op2)));
      if (pe2.code == VREL_VARYING)
	return false;
      pe2.ssa_base = op2;
      pe2.members = BITMAP_ALLOC (&m_bitmaps);
      bitmap_set_bit (pe2.members, v2);
      pe1.ssa_base = op2;
      pe1.code = r;
      pe1.members = pe2.members;
      bitmap_set_bit (pe1.members, v1);
    }
  return true;
}

// Return the set of partial equivalences associated with NAME.  The bitmap
// will be NULL if there are none.

const pe_slice *
equiv_oracle::partial_equiv_set (tree name)
{
  int v = SSA_NAME_VERSION (name);
  if (v >= (int)m_partial.length ())
    return NULL;
  return &m_partial[v];
}

// Query if there is a partial equivalence between SSA1 and SSA2.  Return
// VREL_VARYING if there is not one.  If BASE is non-null, return the base
// ssa-name this is a slice of.

relation_kind
equiv_oracle::partial_equiv (tree ssa1, tree ssa2, tree *base) const
{
  int v1 = SSA_NAME_VERSION (ssa1);
  int v2 = SSA_NAME_VERSION (ssa2);

  if (v1 >= (int)m_partial.length () || v2 >= (int)m_partial.length ())
    return VREL_VARYING;

  const pe_slice &pe1 = m_partial[v1];
  const pe_slice &pe2 = m_partial[v2];
  if (pe1.members && pe2.members == pe1.members)
    {
      if (base)
	*base = pe1.ssa_base;
      return pe_min (pe1.code, pe2.code);
    }
  return VREL_VARYING;
}

void
equiv_oracle::register_equiv_block (unsigned v, unsigned bbi)
{
  if (v >= m_name_info.length ())
    m_name_info.safe_grow_cleared (num_ssa_names + 1);

  if (!m_name_info[v].m_block_list)
    m_name_info[v].m_block_list = BITMAP_ALLOC (&m_bitmaps);

  bitmap_set_bit (m_name_info[v].m_block_list, bbi);
}

void
equiv_oracle::register_equiv_block (const_bitmap names, basic_block bb)
{
  bitmap_iterator bi;
  unsigned v;

  EXECUTE_IF_SET_IN_BITMAP (names, 0, v, bi)
    register_equiv_block (v, bb->index);
}

// Find and return the equivalency set for SSA along the dominators of BB.
// This is the external API.

const_bitmap
equiv_oracle::equiv_set (tree ssa, basic_block bb)
{
  // Search the dominator tree for an equivalency.
  equiv_chain *equiv = find_equiv_dom (ssa, bb);
  if (equiv)
    return equiv->m_names;

  // Otherwise return a cached equiv set containing just this SSA.
  unsigned v = SSA_NAME_VERSION (ssa);
  if (v >= m_name_info.length ())
    m_name_info.safe_grow_cleared (num_ssa_names + 1);

  if (!m_name_info[v].m_self_equiv)
    {
      m_name_info[v].m_self_equiv = BITMAP_ALLOC (&m_bitmaps);
      bitmap_set_bit (m_name_info[v].m_self_equiv, v);
    }
  return m_name_info[v].m_self_equiv;
}

// Query if there is a relation (equivalence) between 2 SSA_NAMEs.

relation_kind
equiv_oracle::query (basic_block bb, tree ssa1, tree ssa2)
{
  // If the 2 ssa names share the same equiv set, they are equal.
  if (equiv_set (ssa1, bb) == equiv_set (ssa2, bb))
    return VREL_EQ;

  // Check if there is a partial equivalence.
  return partial_equiv (ssa1, ssa2);
}

// Query if there is a relation (equivalence) between 2 SSA_NAMEs.

relation_kind
equiv_oracle::query (basic_block bb ATTRIBUTE_UNUSED, const_bitmap e1,
		     const_bitmap e2)
{
  // If the 2 ssa names share the same equiv set, they are equal.
  if (bitmap_equal_p (e1, e2))
    return VREL_EQ;
  return VREL_VARYING;
}

// If SSA has an equivalence in block BB, find and return it.
// Otherwise return NULL.

equiv_chain *
equiv_oracle::find_equiv_block (unsigned ssa, int bb) const
{
  if (bb >= (int)m_equiv.length () || !m_equiv[bb])
    return NULL;

  return m_equiv[bb]->find (ssa);
}

// Starting at block BB, walk the dominator chain looking for the nearest
// equivalence set containing NAME.

equiv_chain *
equiv_oracle::find_equiv_dom (tree name, basic_block bb) const
{
  unsigned v = SSA_NAME_VERSION (name);
  // Short circuit looking for names which have no equivalences.
  // Saves time looking for something which does not exist.
  if (!bitmap_bit_p (m_equiv_set, v))
    return NULL;

  // NAME has at least once equivalence set, check to see if it has one along
  // the dominator tree.
  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      equiv_chain *ptr = find_equiv_block (v, bb->index);
      if (ptr)
	return ptr;
    }
  return NULL;
}

// Register equivalence between ssa_name V and set EQUIV in block BB,

bitmap
equiv_oracle::register_equiv (basic_block bb, unsigned v, equiv_chain *equiv)
{
  // V will have an equivalency now.
  bitmap_set_bit (m_equiv_set, v);

  // If that equiv chain is in this block, simply use it.
  if (equiv->m_bb == bb)
    {
      bitmap_set_bit (equiv->m_names, v);
      bitmap_set_bit (m_equiv[bb->index]->m_names, v);
      // Add BB to V.
      register_equiv_block (v, bb->index);
      return NULL;
    }

  // Otherwise create an equivalence for this block which is a copy
  // of equiv, the add V to the set.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  valid_equivs (b, equiv->m_names, bb);
  bitmap_set_bit (b, v);
  // Add BB to the all the equiv names.
  register_equiv_block (b, bb);
  return b;
}

// Register equivalence between set equiv_1 and equiv_2 in block BB.
// Return NULL if either name can be merged with the other.  Otherwise
// return a pointer to the combined bitmap of names.  This allows the
// caller to do any setup required for a new element.

bitmap
equiv_oracle::register_equiv (basic_block bb, equiv_chain *equiv_1,
			      equiv_chain *equiv_2)
{
  // If equiv_1 is already in BB, use it as the combined set.
  if (equiv_1->m_bb == bb)
    {
      valid_equivs (equiv_1->m_names, equiv_2->m_names, bb);
      // Its hard to delete from a single linked list, so
      // just clear the second one.
      if (equiv_2->m_bb == bb)
	bitmap_clear (equiv_2->m_names);
      else
	{
	  // Ensure the new names are in the summary for BB.
	  bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_1->m_names);
	  // Add BB to the names in equiv2.
	  register_equiv_block (equiv_2->m_names, bb);
	}
      return NULL;
    }
  // If equiv_2 is in BB, use it for the combined set.
  if (equiv_2->m_bb == bb)
    {
      valid_equivs (equiv_2->m_names, equiv_1->m_names, bb);
      // Ensure the new names are in the summary.
      bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_2->m_names);
      // Add BB to the names in equiv1.
      register_equiv_block (equiv_1->m_names, bb);
      return NULL;
    }

  // At this point, neither equivalence is from this block.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  valid_equivs (b, equiv_1->m_names, bb);
  valid_equivs (b, equiv_2->m_names, bb);
  // Add BB to the all the equiv names.
  register_equiv_block (b, bb);
  return b;
}

// Create an equivalency set containing only SSA in its definition block.
// This is done the first time SSA is registered in an equivalency and blocks
// any DOM searches past the definition.

void
equiv_oracle::register_initial_def (tree ssa)
{
  if (SSA_NAME_IS_DEFAULT_DEF (ssa))
    return;
  basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (ssa));

  // If defining stmt is not in the IL, simply return.
  if (!bb)
    return;
  gcc_checking_assert (!find_equiv_dom (ssa, bb));

  unsigned v = SSA_NAME_VERSION (ssa);
  bitmap_set_bit (m_equiv_set, v);
  bitmap equiv_set = BITMAP_ALLOC (&m_bitmaps);
  bitmap_set_bit (equiv_set, v);
  add_equiv_to_block (bb, equiv_set);
}

// Clear the equivalence lists and partial equivalencs for NAME.

void
equiv_oracle::clear (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  // Remove NAME from any blocks it is an equivalence in.
  if (bitmap_bit_p (m_equiv_set, v))
    {
      gcc_checking_assert (m_name_info[v].m_block_list);
      bitmap_iterator bi;
      unsigned bbi;

      EXECUTE_IF_SET_IN_BITMAP (m_name_info[v].m_block_list, 0, bbi, bi)
	{
	  if (bbi >= m_equiv.length ())
	    break;
	  if (!m_equiv[bbi])
	    continue;
	  equiv_chain *ptr = m_equiv[bbi]->find (v);
	  if (ptr)
	    {
	      bitmap_clear_bit (ptr->m_names, v);
	      bitmap_clear_bit (m_equiv[bbi]->m_names, v);
	    }
	}
      bitmap_clear_bit (m_equiv_set, v);
      bitmap_clear (m_name_info[v].m_block_list);
    }
  // Eliminate any partial equivs.
  if (v < m_partial.length ())
    m_partial[v].members = NULL;
}


// Register an equivalence between SSA1 and SSA2 in block BB.
// The equivalence oracle maintains a vector of equivalencies indexed by basic
// block. When an equivalence between SSA1 and SSA2 is registered in block BB,
// a query is made as to what equivalences both names have already, and
// any preexisting equivalences are merged to create a single equivalence
// containing all the ssa_names in this basic block.
// Return false if no new relation is added.

bool
equiv_oracle::record (basic_block bb, relation_kind k, tree ssa1, tree ssa2)
{
  // Process partial equivalencies.
  if (relation_partial_equiv_p (k))
    return add_partial_equiv (k, ssa1, ssa2);

  // Only handle equality relations.
  if (k != VREL_EQ)
    return false;

  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);

  // If this is the first time an ssa_name has an equivalency registered
  // create a self-equivalency record in the def block.
  if (!bitmap_bit_p (m_equiv_set, v1))
    register_initial_def (ssa1);
  if (!bitmap_bit_p (m_equiv_set, v2))
    register_initial_def (ssa2);

  equiv_chain *equiv_1 = find_equiv_dom (ssa1, bb);
  equiv_chain *equiv_2 = find_equiv_dom (ssa2, bb);

  // Check if they are the same set
  if (equiv_1 && equiv_1 == equiv_2)
    return false;

  bitmap equiv_set;

  // Case where we have 2 SSA_NAMEs that are not in any set.
  if (!equiv_1 && !equiv_2)
    {
      bitmap_set_bit (m_equiv_set, v1);
      bitmap_set_bit (m_equiv_set, v2);

      equiv_set = BITMAP_ALLOC (&m_bitmaps);
      bitmap_set_bit (equiv_set, v1);
      bitmap_set_bit (equiv_set, v2);
    }
  else if (!equiv_1 && equiv_2)
    equiv_set = register_equiv (bb, v1, equiv_2);
  else if (equiv_1 && !equiv_2)
    equiv_set = register_equiv (bb, v2, equiv_1);
  else
    equiv_set = register_equiv (bb, equiv_1, equiv_2);

  // A non-null return is a bitmap that is to be added to the current
  // block as a new equivalence.
  if (!equiv_set)
    return false;

  add_equiv_to_block (bb, equiv_set);
  return true;
}

// Add an equivalency record in block BB containing bitmap EQUIV_SET.
// Note the internal caller is responsible for allocating EQUIV_SET properly.

void
equiv_oracle::add_equiv_to_block (basic_block bb, bitmap equiv_set)
{
  equiv_chain *ptr;

  // Check if this is the first time a block has an equivalence added.
  // and create a header block. And set the summary for this block.
  limit_check (bb);
  if (!m_equiv[bb->index])
    {
      ptr = (equiv_chain *) obstack_alloc (&m_chain_obstack,
					   sizeof (equiv_chain));
      ptr->m_names = BITMAP_ALLOC (&m_bitmaps);
      bitmap_copy (ptr->m_names, equiv_set);
      ptr->m_bb = bb;
      ptr->m_next = NULL;
      m_equiv[bb->index] = ptr;
    }

  // Now create the element for this equiv set and initialize it.
  ptr = (equiv_chain *) obstack_alloc (&m_chain_obstack, sizeof (equiv_chain));
  ptr->m_names = equiv_set;
  ptr->m_bb = bb;
  gcc_checking_assert (bb->index < (int)m_equiv.length ());
  ptr->m_next = m_equiv[bb->index]->m_next;
  m_equiv[bb->index]->m_next = ptr;
  bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_set);
  // Add BB to the equiv set.
  register_equiv_block (equiv_set, bb);
}

// Make sure the BB vector is big enough and grow it if needed.

void
equiv_oracle::limit_check (basic_block bb)
{
  int i = (bb) ? bb->index : last_basic_block_for_fn (cfun);
  if (i >= (int)m_equiv.length ())
    m_equiv.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
}

// Dump the equivalence sets in BB to file F.

void
equiv_oracle::dump (FILE *f, basic_block bb) const
{
  if (bb->index >= (int)m_equiv.length ())
    return;
  // Process equivalences.
  if (m_equiv[bb->index])
    {
      equiv_chain *ptr = m_equiv[bb->index]->m_next;
      for (; ptr; ptr = ptr->m_next)
	ptr->dump (f);
    }
  // Look for partial equivalences defined in this block..
  for (unsigned i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (!gimple_range_ssa_p (name) || !SSA_NAME_DEF_STMT (name))
	continue;
      if (i >= m_partial.length ())
	break;
     tree base = m_partial[i].ssa_base;
      if (base && name != base && gimple_bb (SSA_NAME_DEF_STMT (name)) == bb)
	{
	  relation_kind k = partial_equiv (name, base);
	  if (k != VREL_VARYING)
	    {
	      value_relation vr (k, name, base);
	      fprintf (f, "Partial equiv ");
	      vr.dump (f);
	      fputc ('\n',f);
	    }
	}
    }
}

// Dump all equivalence sets known to the oracle.

void
equiv_oracle::dump (FILE *f) const
{
  fprintf (f, "Equivalency dump\n");
  for (unsigned i = 0; i < m_equiv.length (); i++)
    if (m_equiv[i] && BASIC_BLOCK_FOR_FN (cfun, i))
      {
	fprintf (f, "BB%d\n", i);
	dump (f, BASIC_BLOCK_FOR_FN (cfun, i));
      }
}


// --------------------------------------------------------------------------

// Adjust the relation by Swapping the operands and relation.

void
value_relation::swap ()
{
  related = relation_swap (related);
  tree tmp = name1;
  name1 = name2;
  name2 = tmp;
}

// Perform an intersection between 2 relations. *this &&= p.
// Return false if the relations cannot be intersected.

bool
value_relation::intersect (value_relation &p)
{
  // Save previous value
  relation_kind old = related;

  if (p.op1 () == op1 () && p.op2 () == op2 ())
    related = relation_intersect (kind (), p.kind ());
  else if (p.op2 () == op1 () && p.op1 () == op2 ())
    related = relation_intersect (kind (), relation_swap (p.kind ()));
  else
    return false;

  return old != related;
}

// Perform a union between 2 relations. *this ||= p.

bool
value_relation::union_ (value_relation &p)
{
  // Save previous value
  relation_kind old = related;

  if (p.op1 () == op1 () && p.op2 () == op2 ())
    related = relation_union (kind(), p.kind());
  else if (p.op2 () == op1 () && p.op1 () == op2 ())
    related = relation_union (kind(), relation_swap (p.kind ()));
  else
    return false;

  return old != related;
}

// Identify and apply any transitive relations between REL
// and THIS.  Return true if there was a transformation.

bool
value_relation::apply_transitive (const value_relation &rel)
{
  relation_kind k = VREL_VARYING;

  // Identify any common operand, and normalize the relations to
  // the form : A < B  B < C produces A < C
  if (rel.op1 () == name2)
    {
      // A < B   B < C
      if (rel.op2 () == name1)
	return false;
      k = relation_transitive (kind (), rel.kind ());
      if (k != VREL_VARYING)
	{
	  related = k;
	  name2 = rel.op2 ();
	  return true;
	}
    }
  else if (rel.op1 () == name1)
    {
      // B > A   B < C
      if (rel.op2 () == name2)
	return false;
      k = relation_transitive (relation_swap (kind ()), rel.kind ());
      if (k != VREL_VARYING)
	{
	  related = k;
	  name1 = name2;
	  name2 = rel.op2 ();
	  return true;
	}
    }
  else if (rel.op2 () == name2)
    {
       // A < B   C > B
       if (rel.op1 () == name1)
	 return false;
      k = relation_transitive (kind (), relation_swap (rel.kind ()));
      if (k != VREL_VARYING)
	{
	  related = k;
	  name2 = rel.op1 ();
	  return true;
	}
    }
  else if (rel.op2 () == name1)
    {
      // B > A  C > B
      if (rel.op1 () == name2)
	return false;
      k = relation_transitive (relation_swap (kind ()),
			       relation_swap (rel.kind ()));
      if (k != VREL_VARYING)
	{
	  related = k;
	  name1 = name2;
	  name2 = rel.op1 ();
	  return true;
	}
    }
  return false;
}

// Create a trio from this value relation given LHS, OP1 and OP2.

relation_trio
value_relation::create_trio (tree lhs, tree op1, tree op2)
{
  relation_kind lhs_1;
  if (lhs == name1 && op1 == name2)
    lhs_1 = related;
  else if (lhs == name2 && op1 == name1)
    lhs_1 = relation_swap (related);
  else
    lhs_1 = VREL_VARYING;

  relation_kind lhs_2;
  if (lhs == name1 && op2 == name2)
    lhs_2 = related;
  else if (lhs == name2 && op2 == name1)
    lhs_2 = relation_swap (related);
  else
    lhs_2 = VREL_VARYING;

  relation_kind op_op;
  if (op1 == name1 && op2 == name2)
    op_op = related;
  else if (op1 == name2 && op2 == name1)
    op_op = relation_swap (related);
  else if  (op1 == op2)
    op_op = VREL_EQ;
  else
    op_op = VREL_VARYING;

  return relation_trio (lhs_1, lhs_2, op_op);
}

// Dump the relation to file F.

void
value_relation::dump (FILE *f) const
{
  if (!name1 || !name2)
    {
      fprintf (f, "no relation registered");
      return;
    }
  fputc ('(', f);
  print_generic_expr (f, op1 (), TDF_SLIM);
  print_relation (f, kind ());
  print_generic_expr (f, op2 (), TDF_SLIM);
  fputc(')', f);
}

// This container is used to link relations in a chain.

class relation_chain : public value_relation
{
public:
  relation_chain *m_next;
};

// Given relation record PTR in block BB, return the next relation in the
// list.  If PTR is NULL, retrieve the first relation in BB.
// If NAME is sprecified, return only relations which include NAME.
// Return NULL when there are no relations left.

relation_chain *
dom_oracle::next_relation (basic_block bb, relation_chain *ptr,
			   tree name) const
{
  relation_chain *p;
  // No value_relation pointer is used to initialize the iterator.
  if (!ptr)
    {
      int bbi = bb->index;
      if (bbi >= (int)m_relations.length())
	return NULL;
      else
	p = m_relations[bbi].m_head;
    }
  else
    p = ptr->m_next;

  if (name)
    for ( ; p; p = p->m_next)
      if (p->op1 () == name || p->op2 () == name)
	break;
  return p;
}

// Instantiate a block relation iterator to iterate over the relations
// on exit from block BB in ORACLE.  Limit this to relations involving NAME
// if specified.  Return the first such relation in VR if there is one.

block_relation_iterator::block_relation_iterator (const relation_oracle *oracle,
						  basic_block bb,
						  value_relation &vr,
						  tree name)
{
  m_oracle = oracle;
  m_bb = bb;
  m_name = name;
  m_ptr = oracle->next_relation (bb, NULL, m_name);
  if (m_ptr)
    {
      m_done = false;
      vr = *m_ptr;
    }
  else
    m_done = true;
}

// Retrieve the next relation from the iterator and return it in VR.

void
block_relation_iterator::get_next_relation (value_relation &vr)
{
  m_ptr = m_oracle->next_relation (m_bb, m_ptr, m_name);
  if (m_ptr)
    {
      vr = *m_ptr;
      if (m_name)
	{
	  if (vr.op1 () != m_name)
	    {
	      gcc_checking_assert (vr.op2 () == m_name);
	      vr.swap ();
	    }
	}
    }
  else
    m_done = true;
}

// ------------------------------------------------------------------------

// Find the relation between any ssa_name in B1 and any name in B2 in LIST.
// This will allow equivalencies to be applied to any SSA_NAME in a relation.

relation_kind
relation_chain_head::find_relation (const_bitmap b1, const_bitmap b2) const
{
  if (!m_names)
    return VREL_VARYING;

  // If both b1 and b2 aren't referenced in this block, cant be a relation
  if (!bitmap_intersect_p (m_names, b1) || !bitmap_intersect_p (m_names, b2))
    return VREL_VARYING;

  // Search for the first relation that contains BOTH an element from B1
  // and B2, and return that relation.
  for (relation_chain *ptr = m_head; ptr ; ptr = ptr->m_next)
    {
      unsigned op1 = SSA_NAME_VERSION (ptr->op1 ());
      unsigned op2 = SSA_NAME_VERSION (ptr->op2 ());
      if (bitmap_bit_p (b1, op1) && bitmap_bit_p (b2, op2))
	return ptr->kind ();
      if (bitmap_bit_p (b1, op2) && bitmap_bit_p (b2, op1))
	return relation_swap (ptr->kind ());
    }

  return VREL_VARYING;
}

// ------------------------------------------------------------------------
// frontier_data methods - what one side of a bidirectional search knows.

// Construct frontier data allocating the bitmap from OBSTACK and side flag RHS.

frontier_data::frontier_data (bitmap_obstack *obstack, bool rhs)
{
  m_known.create (0);
  m_visited = BITMAP_ALLOC (obstack);
  m_root = NULL_TREE;
  m_rhs = rhs;
}

// Record that the root of this search is related to ssa version V by REL.

void
frontier_data::set_known (unsigned v, relation_kind rel)
{
  if (m_known.length () <= v)
    m_known.safe_grow_cleared (num_ssa_names + 1);
  m_known[v] = rel;
}

// Return what the root of this search is known to be relative to ssa version V.

relation_kind
frontier_data::known_relation (unsigned v) const
{
  if (v < m_known.length ())
    return m_known[v];
  return VREL_VARYING;
}

// Reset for the next search.  Only the entries which were actually set need
// to be put back, and m_visited lists exactly those.

void
frontier_data::clear_search ()
{
  unsigned i;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (m_visited, 0, i, bi)
    {
      if (i < m_known.length ())
	m_known[i] = VREL_VARYING;
    }
  bitmap_clear (m_visited);
  m_root = NULL_TREE;
}

// ------------------------------------------------------------------------
// Instantiate a relation oracle.

dom_oracle::dom_oracle () : m_lhs_search (&m_bitmaps, false),
			    m_rhs_search (&m_bitmaps, true)
{
  m_relations.create (0);
  m_relations.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
  m_relation_set = BITMAP_ALLOC (&m_bitmaps);
  m_block_list.create (0);
  m_block_list.safe_grow_cleared (num_ssa_names + 1);
  m_tmp = BITMAP_ALLOC (&m_bitmaps);
  m_tmp2 = BITMAP_ALLOC (&m_bitmaps);
  m_near.create (0);
  m_worklist.create (0);
  m_wl_ix = 0;
}

// Destruct a relation oracle.

dom_oracle::~dom_oracle ()
{
  m_worklist.release ();
  m_near.release ();
  m_block_list.release ();
  m_relations.release ();
}

// Remove any relations with NAME from this list.

void
relation_chain_head::clear (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!m_names || !bitmap_bit_p (m_names, v))
    return;

  relation_chain *ptr, *last = NULL;;

  for (ptr = m_head; ptr; ptr = ptr->m_next)
    {
      tree op1 = ptr->op1 ();
      tree op2 = ptr->op2 ();
      // Delink any elements with NAME.
      if (op1 == name || op2 == name)
	{
	  if (!last)
	    m_head = ptr->m_next;
	  else
	    last->m_next = ptr->m_next;
	  m_num_relations--;
	}
      else
	last = ptr;
    }
  // And remove name from the possible relations in this block bitfield.
  bitmap_clear_bit (m_names, v);
}

// Remove any relations involving NAME from the DOM oracle

void
dom_oracle::clear (tree name)
{
  equiv_oracle::clear (name);
  unsigned v = SSA_NAME_VERSION (name);
  if (bitmap_bit_p (m_relation_set, v))
    {
      gcc_checking_assert (m_block_list[v]);
      bitmap_iterator bi;
      unsigned bbi;

      EXECUTE_IF_SET_IN_BITMAP (m_block_list[v], 0, bbi, bi)
	{
	  if (bbi >= m_relations.length())
	    break;
	  m_relations[bbi].clear (name);
	}
      bitmap_clear_bit (m_relation_set, v);
      bitmap_clear (m_block_list[v]);
    }
}

// Register relation K between ssa_name OP1 and OP2 on STMT.
// Return false if no new relation is added.

bool
relation_oracle::record (gimple *stmt, relation_kind k, tree op1, tree op2)
{
  gcc_checking_assert (TREE_CODE (op1) == SSA_NAME);
  gcc_checking_assert (TREE_CODE (op2) == SSA_NAME);
  gcc_checking_assert (stmt && gimple_bb (stmt));

  // Don't register lack of a relation.
  if (k == VREL_VARYING)
    return false;

  // If an equivalence is being added between a PHI and one of its arguments
  // make sure that that argument is not defined in the same block.
  // This can happen along back edges and the equivalence will not be
  // applicable as it would require a use before def.
  if (k == VREL_EQ && is_a<gphi *> (stmt))
    {
      tree phi_def = gimple_phi_result (stmt);
      gcc_checking_assert (phi_def == op1 || phi_def == op2);
      tree arg = op2;
      if (phi_def == op2)
	arg = op1;
      if (gimple_bb (stmt) == gimple_bb (SSA_NAME_DEF_STMT (arg)))
	return false;
    }

  // If the LHS of a statement has already been processed and an equivalence
  // registered, do not register another one.  See PR 124809.
  if (m_lhs_equiv_set_p && relation_equiv_p (k)
      && gimple_get_lhs (stmt) == op1)
    {
      if (!bitmap_set_bit (m_lhs_equiv_set_p, SSA_NAME_VERSION (op1)))
	return false;
    }
  bool ret = record (gimple_bb (stmt), k, op1, op2);

  if (ret && dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, op1, op2);
      fprintf (dump_file, " Registering value_relation ");
      vr.dump (dump_file);
      fprintf (dump_file, " (bb%d) at ", gimple_bb (stmt)->index);
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
  return ret;
}

// Register relation K between ssa_name OP1 and OP2 on edge E.
// Return false if no new relation is added.

bool
relation_oracle::record (edge e, relation_kind k, tree op1, tree op2)
{
  gcc_checking_assert (TREE_CODE (op1) == SSA_NAME);
  gcc_checking_assert (TREE_CODE (op2) == SSA_NAME);

  // Do not register lack of relation, or blocks which have more than
  // edge E for a predecessor.
  if (k == VREL_VARYING || !single_pred_p (e->dest))
    return false;

  bool ret = record (e->dest, k, op1, op2);

  if (ret && dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, op1, op2);
      fprintf (dump_file, " Registering value_relation ");
      vr.dump (dump_file);
      fprintf (dump_file, " on (%d->%d)\n", e->src->index, e->dest->index);
    }
  return ret;
}

// Register relation K between OP! and OP2 in block BB.
// This creates the record and searches for existing records in the dominator
// tree to merge with.  Return false if no new relation is added.

bool
dom_oracle::record (basic_block bb, relation_kind k, tree op1, tree op2)
{
  // If the 2 ssa_names are the same, do nothing.  An equivalence is implied,
  // and no other relation makes sense.
  if (op1 == op2)
    return false;

  // Do not register an impossible relation.
  if (k == VREL_UNDEFINED)
    return false;

  // Equivalencies are handled by the equivalence oracle.
  if (relation_equiv_p (k))
    return equiv_oracle::record (bb, k, op1, op2);
  else
    {
      relation_chain *ptr = search_and_merge_relation (bb, k, op1, op2);
      return ptr != NULL;
    }
}

void
dom_oracle::record_relation_block (unsigned v, unsigned bbi)
{
  if (v>= m_block_list.length ())
    m_block_list.safe_grow_cleared (num_ssa_names + 1);

  if (!m_block_list[v])
    m_block_list[v] = BITMAP_ALLOC (&m_bitmaps);

  bitmap_set_bit (m_block_list[v], bbi);
}

// Register relation K between OP1 and OP2 in block BB by creating a new
// record.  It is an error for there to be an existing record.
// Return the record, or NULL if no record was created.

relation_chain *
dom_oracle::create_relation_in_bb (basic_block bb, relation_kind k, tree op1,
				   tree op2)
{
  int bbi = bb->index;

  if (bbi >= (int)m_relations.length())
    m_relations.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);

  if (m_relations[bbi].m_num_relations >= param_relation_block_limit)
    return NULL;
  m_relations[bbi].m_num_relations++;
  // Check for an existing relation further up the DOM chain.
  // By including dominating relations, The first one found in any search
  // will be the aggregate of all the previous ones.

  relation_chain *ptr;

  // Summary bitmap indicating what ssa_names have relations in this BB.
  bitmap bm = m_relations[bbi].m_names;
  if (!bm)
    bm = m_relations[bbi].m_names = BITMAP_ALLOC (&m_bitmaps);
  unsigned v1 = SSA_NAME_VERSION (op1);
  unsigned v2 = SSA_NAME_VERSION (op2);

  // Assert there is no existing relation.
  gcc_checking_assert (find_relation_block (bbi, op1, op2, NULL)
		       == VREL_VARYING);

  bitmap_set_bit (bm, v1);
  bitmap_set_bit (bm, v2);
  bitmap_set_bit (m_relation_set, v1);
  bitmap_set_bit (m_relation_set, v2);
  record_relation_block (v1, bbi);
  record_relation_block (v2, bbi);

  ptr = (relation_chain *) obstack_alloc (&m_chain_obstack,
					  sizeof (relation_chain));
  ptr->set_relation (k, op1, op2);
  ptr->m_next = m_relations[bbi].m_head;
  m_relations[bbi].m_head = ptr;
  return ptr;
}

// Register relation K between OP1 and OP2 in block BB by searching the
// dominator tree for any existing record to merge with.  If there were
// none, create a new record.
// Return the record, or NULL if no record was found or created.

relation_chain *
dom_oracle::search_and_merge_relation (basic_block bb, relation_kind k,
				       tree op1, tree op2)
{
  // Check for invalid relations to register.
  gcc_checking_assert (k != VREL_VARYING && k != VREL_UNDEFINED
		       && k != VREL_EQ);

  relation_chain *ptr;
  relation_kind curr = find_relation_block (bb->index, op1, op2, &ptr);

  // If there is an existing relation in this block, just intersect with it.
  if (curr != VREL_VARYING)
    {
      // If K contradicts what is already recorded, the block is unreachable.
      // Leave the existing relation alone rather than replacing it with
      // UNDEFINED, matching what the dominator merge below does.
      if (relation_intersect (curr, k) == VREL_UNDEFINED)
	return NULL;
      // If there was no change, return no record.
      value_relation vr (k, op1, op2);
      if (!ptr->intersect (vr))
	return NULL;
      return ptr;
    }

  // Create the relation in this block.
  ptr = create_relation_in_bb (bb, k, op1, op2);
  if (ptr)
    {
      // Check for an existing relation further up the DOM chain.
      // By including dominating relations, The first one found in any search
      // will be the aggregate of all the previous ones.
      curr = find_relation_dom (get_immediate_dominator (CDI_DOMINATORS, bb),
				op1, op2);
      if (curr != VREL_VARYING)
	{
	  curr = relation_intersect (curr, k);
	  // Intersect the new relation with the existing one, unless the
	  // result is UNDEFINED.  Then just leave it.
	  if (curr != k && curr != VREL_UNDEFINED)
	    ptr->set_relation (curr, op1, op2);
	}
    }
  return ptr;
}

// Find the relation between any ssa_name in B1 and any name in B2 in block BB.
// This will allow equivalencies to be applied to any SSA_NAME in a relation.

relation_kind
dom_oracle::find_relation_block (unsigned bb, const_bitmap b1,
				      const_bitmap b2) const
{
  if (bb >= m_relations.length())
    return VREL_VARYING;

  return m_relations[bb].find_relation (b1, b2);
}

// Search the DOM tree for a relation between an element of equivalency set B1
// and B2, starting with block BB.

relation_kind
dom_oracle::query (basic_block bb, const_bitmap b1, const_bitmap b2)
{
  relation_kind r;
  if (bitmap_equal_p (b1, b2))
    return VREL_EQ;

  // If either name does not occur in a relation anywhere, there isn't one.
  if (!bitmap_intersect_p (m_relation_set, b1)
      || !bitmap_intersect_p (m_relation_set, b2))
    return VREL_VARYING;

  // Search each block in the DOM tree checking.
  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      r = find_relation_block (bb->index, b1, b2);
      if (r != VREL_VARYING)
	return r;
    }
  return VREL_VARYING;

}

// Find a relation in block BB between ssa version V1 and V2.  If a relation
// is found, return a pointer to the chain object in OBJ.

relation_kind
dom_oracle::find_relation_block (int bb, tree ssa1, tree ssa2,
				     relation_chain **obj) const
{
  if (bb >= (int)m_relations.length())
    return VREL_VARYING;

  const_bitmap bm = m_relations[bb].m_names;
  if (!bm)
    return VREL_VARYING;

  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);

  // If both b1 and b2 aren't referenced in this block, cant be a relation
  if (!bitmap_bit_p (bm, v1) || !bitmap_bit_p (bm, v2))
    return VREL_VARYING;

  relation_chain *ptr;
  for (ptr = m_relations[bb].m_head; ptr ; ptr = ptr->m_next)
    {
      tree op1 = ptr->op1 ();
      tree op2 = ptr->op2 ();
      if (ssa1 == op1 && ssa2 == op2)
	{
	  if (obj)
	    *obj = ptr;
	  return ptr->kind ();
	}
      if (ssa1 == op2 && ssa2 == op1)
	{
	  if (obj)
	    *obj = ptr;
	  return relation_swap (ptr->kind ());
	}
    }

  return VREL_VARYING;
}

// See if a relation can be found between SSA1 and SSA2 in basic block BB based
// on values as they exist in basic block ORIG.   This will only occur
// if SSA1 and SSA2 occur in the same statement together.

relation_kind
dom_oracle::recomputed_relation (basic_block orig_bb, tree ssa1,
				 tree ssa2) const
{
  if (ssa1 == ssa2)
    return VREL_EQ;
  if (!orig_bb)
    return VREL_VARYING;
  gori_map *gori_ssa = get_range_query (cfun)->gori_ssa ();
  if (!gori_ssa)
    return VREL_VARYING;
  gimple_outgoing_range &gori = get_range_query (cfun)->gori ();

  // Both ssa1 and ssa2 must be exported somewhere.
  if (!gori_ssa->is_export_p (ssa1) || !gori_ssa->is_export_p (ssa2))
    return VREL_VARYING;

  relation_kind result = VREL_VARYING;
  use_operand_p use_p;
  imm_use_iterator iter;

  // Any statement with both SSA1 and SSA2 as operands may imply a relation
  // between them, once the range of its LHS at ORIG_BB is known.
  FOR_EACH_IMM_USE_FAST (use_p, iter, ssa1)
    {
      bool reversed = false;
      gimple *s = USE_STMT (use_p);

      // S must be a binary assignment LHS = op1 op2
      if (!is_gimple_assign (s) || gimple_num_ops (s) != 3)
	continue;

      // S must strictly dominate ORIG_BB.  DOMINATED_BY_P is block granular,
      // so the same-block case must be excluded.
      basic_block sbb = gimple_bb (s);
      if (!sbb || sbb == orig_bb
	  || !dominated_by_p (CDI_DOMINATORS, orig_bb, sbb))
	continue;

      gimple_range_op_handler s_handler (s);
      if (!s_handler)
	continue;

      if (s_handler.operand1 () == ssa1)
	{
	  // op1 == ssa1, move on if op2 != ssa2.
	  if (s_handler.operand2 () != ssa2)
	    continue;
	}
      // Otherwise only move on if op1 == ssa2 && op2 == ssa1.
      else if (s_handler.operand1 () != ssa2 || s_handler.operand2 () != ssa1)
	continue;
      else
	reversed = true;


      // Find the outgoing edge of SBB which must have been taken to reach
      // ORIG_BB.  SINGLE_PRED_P is required as well as dominance: if E->dest
      // has another predecessor, it can dominate ORIG_BB via a path which
      // never traversed E, and E's range would not apply.

      edge e = NULL, se;
      edge_iterator ei;
      FOR_EACH_EDGE (se, ei, sbb->succs)
      if (single_pred_p (se->dest)
	  && dominated_by_p (CDI_DOMINATORS, orig_bb, se->dest))
	{
	  e = se;
	  break;
	}
      if (!e)
	continue;

      int_range_max edge_range;
      gimple *stmt = gori.edge_range_p (edge_range, e);
      if (!stmt)
	continue;

      // Walk back from the terminator of SBB towards S, recalculating each
      // value as it would be in ORIG_BB rather than as it is on E.  This is
      // the whole point: RANGE_ON_ENTRY cannot supply S's LHS at ORIG_BB
      // because it evaluates each dominating edge using the values available
      // at that edge's source.
      tree lhs_s = s_handler.lhs ();
      value_range lhs_range (edge_range);

      while (stmt && stmt != s)
	{
	  bool ret;
	  gimple_range_op_handler handler (stmt);
	  if (!handler)
	    break;

	  tree op1 = handler.operand1 ();
	  tree op2 = handler.operand2 ();
	  value_range op1_range (TREE_TYPE (op1));
	  value_range op2_range;

	  // Follow the operand whose dependency chain reaches S's LHS.
	  bool op1_dep = gimple_range_ssa_p (op1)
			 && (op1 == lhs_s || gori_ssa->in_chain_p (lhs_s, op1));
	  bool op2_dep = gimple_range_ssa_p (op2)
			 && (op2 == lhs_s || gori_ssa->in_chain_p (lhs_s, op2));
	  if (op1_dep == op2_dep)
	    break;

	  if (op1_dep)
	    {
	      if (!op2)
		ret = handler.calc_op1 (op1_range, lhs_range);
	      else
		{
		  op2_range.set_range_class (TREE_TYPE (op2));
		  get_range_query (cfun)->range_on_entry (op2_range, orig_bb,
							  op2);
		  ret = handler.calc_op1 (op1_range, lhs_range, op2_range);
		}
	      if (!ret)
		break;
	      lhs_range = op1_range;
	      stmt = SSA_NAME_DEF_STMT (op1);
	    }
	  else
	    {
	      op2_range.set_range_class (TREE_TYPE (op2));
	      get_range_query (cfun)->range_on_entry (op1_range, orig_bb, op1);
	      ret = handler.calc_op2 (op2_range, lhs_range, op1_range);
	      if (!ret)
		break;
	      lhs_range = op2_range;
	      stmt = SSA_NAME_DEF_STMT (op2);
	    }

	  // Bail if the chain leaves SBB.
	  if (!stmt || gimple_bb (stmt) != sbb)
	    break;
	}

      if (stmt != s)
	continue;

      // LHS_RANGE is now S's LHS as it is known in ORIG_BB.
      value_range op1_range (TREE_TYPE (s_handler.operand1 ()));
      value_range op2_range (TREE_TYPE (s_handler.operand2 ()));
      get_range_query (cfun)->range_on_entry (op1_range, orig_bb,
					    s_handler.operand1 ());
      get_range_query (cfun)->range_on_entry (op2_range, orig_bb,
					    s_handler.operand2 ());
      relation_kind r = s_handler.op1_op2_relation (lhs_range, op1_range,
						  op2_range);
      if (reversed)
	r = relation_swap (r);
      relation_kind k = relation_intersect (result, r);
      if (k != VREL_UNDEFINED)
	result = k;
    }
  return result;
}

// Find a relation between SSA1 and SSA2 in the dominator tree starting with
// block BB

relation_kind
dom_oracle::find_relation_dom (basic_block start_bb, tree ssa1, tree ssa2) const
{
  relation_kind r;
  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);
  // IF either name does not occur in a relation anywhere, there isn't one.
  if (!bitmap_bit_p (m_relation_set, v1) || !bitmap_bit_p (m_relation_set, v2))
    return VREL_VARYING;
  for (basic_block bb = start_bb;
       bb;
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      r = find_relation_block (bb->index, ssa1, ssa2);
      if (r != VREL_VARYING)
	return r;
    }
  return VREL_VARYING;
}

// Starting with basic block BB, look for the next block in the dominator
// tree which contains a relation involving NAME.  There can be more than one,
// and they are returned in the class local m_near vector.  The block in which
// the relations are found are returned.  If no relations are found, NULL is
// returned and the m_near vector is empty.

basic_block
dom_oracle::nearest_relations (tree name, basic_block bb)
{
  if (TREE_CODE (name) != SSA_NAME)
    return NULL;

  unsigned v = SSA_NAME_VERSION (name);
  if (!bitmap_bit_p (m_relation_set, v))
    return NULL;

  // A relation involving NAME can only be registered in a block
  // dominated by NAME's definition.  Relations occurring *in* the def block
  // are attached to the def block itself, so once that block has been
  // examined there is nothing left to find and the walk can terminate.
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (name));
  bool at_def = false;

  m_near.truncate (0);
  for (; bb && !at_def; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      at_def = (bb == def_bb);

      if (bb->index >= (int) m_relations.length ())
	continue;

      const_bitmap bm = m_relations[bb->index].m_names;
      if (!bm || !bitmap_bit_p (bm, v))
	continue;

      for (relation_chain *ptr = m_relations[bb->index].m_head;
	   ptr; ptr = ptr->m_next)
	{
	  if (v == SSA_NAME_VERSION (ptr->op1 ())
	      && !SSA_NAME_IN_FREE_LIST (ptr->op2 ()))
	    m_near.safe_push ({ ptr->kind (), ptr->op2 (), bb });
	  else if (v == SSA_NAME_VERSION (ptr->op2 ())
		   && !SSA_NAME_IN_FREE_LIST (ptr->op1 ()))
	    m_near.safe_push ({ relation_swap (ptr->kind ()),
				ptr->op1 (), bb });
	}

      if (!m_near.is_empty ())
	return bb;
    }

  return NULL;
}

// Record relation K between OP1 and OP2, discovered by a search in block BB,
// so subsequent queries in BB or anything it dominates find it directly.
// Use SEARCH_AND_MERGE_RELATION rather than CREATE_RELATION_IN_BB.  The pair
// may already have a record in BB from an earlier query, in which case the
// two must be intersected rather than a second record created.

void
dom_oracle::cache_relation (basic_block bb, relation_kind k, tree op1,
			    tree op2)
{
  if (!bb || op1 == op2)
    return;
  // There is nothing useful to record for VARYING or UNDEFINED and
  // Equivalences belong to the equivalence oracle.
  if (k == VREL_VARYING || k == VREL_UNDEFINED || relation_equiv_p (k))
    return;
  search_and_merge_relation (bb, k, op1, op2);
}

// Begin the search for SIDE at its root ROOT, whose equivalence set is
// EQUIV, in block BB.  Every name equivalent to ROOT is an equally good
// starting point, so seed them all.

void
dom_oracle::start_search (frontier_data &side, tree root, const_bitmap equiv,
			  basic_block bb)
{
  side.m_root = root;

  unsigned root_v = SSA_NAME_VERSION (root);
  m_worklist.safe_push ({ VREL_EQ, root, bb, side.m_rhs });
  bitmap_set_bit (side.m_visited, root_v);
  side.set_known (root_v, VREL_EQ);

  unsigned i;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (equiv, 0, i, bi)
    {
      // Equivalences are maintained lazily, so ensure they are still
      // valid before adding them to the frontier.
      tree name = ssa_name (i);
      if (i == root_v || !name || SSA_NAME_IN_FREE_LIST (name))
	continue;
      m_worklist.safe_push ({ VREL_EQ, name, bb, side.m_rhs });
      bitmap_set_bit (side.m_visited, i);
      side.set_known (i, VREL_EQ);
    }
}

// SIDE's root has been shown to be related to NAME by REL.  Queue NAME for
// expansion starting at block BB.

void
dom_oracle::add_to_frontier (frontier_data &side, tree name, relation_kind rel,
			     basic_block bb)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!side.visited_p (v))
    {
      bitmap_set_bit (side.m_visited, v);
      side.set_known (v, rel);
      m_worklist.safe_push ({ rel, name, bb, side.m_rhs });
      return;
    }

  // NAME already had a relation, so intersect the two and requeue NAME if
  // there is an improvement.
  relation_kind curr = side.known_relation (v);
  relation_kind k = relation_intersect (curr, rel);
  if (k != curr && k != VREL_UNDEFINED)
    {
      side.set_known (v, k);
      m_worklist.safe_push ({ k, name, bb, side.m_rhs });
    }
}

// Expand worklist entry W, looking for a name the other side of the search
// has already reached.  BB is the block the query was made in.  Return true
// and set RESULT if a match was found.  W is passed by value as
// add_to_frontier may realloc the vector.

bool
dom_oracle::expand_frontier (frontier_element w, basic_block bb,
			     relation_kind &result)
{
  frontier_data &self = w.rhs ? m_rhs_search : m_lhs_search;
  frontier_data &other = w.rhs ? m_lhs_search : m_rhs_search;

  if (dump_file && (param_ranger_debug & RANGER_DEBUG_RELATION))
    {
      fprintf (dump_file, "  %s frontier expanding: ", w.rhs ? "RHS" : "LHS");
      print_generic_expr (dump_file, w.name, TDF_SLIM);
      fprintf (dump_file, " (rel: %d)\n", w.rel);
    }

  basic_block found = nearest_relations (w.name, w.bb);
  if (!found)
    return false;

  for (unsigned i = 0; i < m_near.length (); ++i)
    {
      // nearest_relations sets m_near[i].rel as "w.name <rel> m_near.name",
      // so composing it with "root <w.rel> w.name" gives "root <kind> next".
      value_relation path (w.rel, self.m_root, w.name);
      value_relation near_rel (m_near[i].rel, w.name, m_near[i].name);

      if (!path.apply_transitive (near_rel))
	continue;

      relation_kind kind = path.kind ();
      tree next = path.op2 ();
      unsigned nv = SSA_NAME_VERSION (next);

      // If the other side has reached NEXT it knows "other_root <rel> next",
      // which composes with what is known here to relate the two roots.
      if (other.visited_p (nv))
	{
	  relation_kind k
	    = relation_transitive (kind,
				   relation_swap (other.known_relation (nv)));
	  // If the result is not VARYING, we have a match.
	  if (k != VREL_VARYING)
	    {
	      result = w.rhs ? relation_swap (k) : k;
	      if (dump_file && (param_ranger_debug & RANGER_DEBUG_RELATION))
		{
		  fprintf (dump_file, "  %s frontier found connection at: ",
			   w.rhs ? "RHS" : "LHS");
		  print_generic_expr (dump_file, next, TDF_SLIM);
		  fprintf (dump_file, ", combined relation: ");
		  print_relation (dump_file, result);
		  fputc ('\n', dump_file);
		}
	      return true;
	    }
	}

      // Add to this side's frontier if it has not visited NEXT yet.
      add_to_frontier (self, next, kind, bb);
    }

  // Keep searching for NAME in the dominator tree.  Do not search past
  // the def block as that is pointless.  Do search the def block however
  // as relations within the body of the block are stored there.
  // Use whatever the latest known relation value is.
  basic_block idom = get_immediate_dominator (CDI_DOMINATORS, found);
  if (idom && gimple_bb (SSA_NAME_DEF_STMT (w.name)) != found)
    m_worklist.safe_push ({ self.known_relation (SSA_NAME_VERSION (w.name)),
			    w.name, idom, w.rhs });

  return false;
}

// Search for a relation between LHS and RHS in block BB or one of its
// dominators, expanding a frontier from both names until the two meet.
// LHS_EQUIV and RHS_EQUIV are the equivalence sets of LHS and RHS.

relation_kind
dom_oracle::relation_search (basic_block bb, tree lhs, const_bitmap lhs_equiv,
			     tree rhs, const_bitmap rhs_equiv)
{
  // Initialize both search frontiers.
  start_search (m_lhs_search, lhs, lhs_equiv, bb);
  start_search (m_rhs_search, rhs, rhs_equiv, bb);

  relation_kind result = VREL_VARYING;

  // Each expansion walks the dominator tree looking for the next block with
  // a relation.
  unsigned budget = param_transitive_relations_work_bound;

  if (dump_file && (param_ranger_debug & RANGER_DEBUG_RELATION))
    {
      fprintf (dump_file, "Bidirectional relation search: ");
      print_generic_expr (dump_file, lhs, TDF_SLIM);
      fprintf (dump_file, " vs ");
      print_generic_expr (dump_file, rhs, TDF_SLIM);
      fprintf (dump_file, " in bb%d\n", bb ? bb->index : -1);
    }

  // Expand until exhausted or until a connection is found.
  while (m_wl_ix < m_worklist.length ())
    {
      if (!budget--)
	{
	  if (dump_file && (param_ranger_debug & RANGER_DEBUG_RELATION))
	    fprintf (dump_file, "  search budget exhausted\n");
	  break;
	}

      if (expand_frontier (m_worklist[m_wl_ix++], bb, result))
	break;
    }

  // Clear search state for reuse.
  m_worklist.truncate (0);
  m_wl_ix = 0;
  m_lhs_search.clear_search ();
  m_rhs_search.clear_search ();

  // Check to see if any new relations can be formed by recomputing an
  // expression using LHS and RHS.
  // This call must come after the worklists are cleared above.  The call
  // may invoke range_on_entry which may trigger another relation search,
  // and thus the current search must be cleared before making this call.
  relation_kind rel = recomputed_relation (bb, lhs, rhs);

  if (rel != VREL_VARYING)
    {
      rel = relation_intersect (result, rel);
      if (rel != VREL_UNDEFINED)
	result = rel;
    }

  // Record whatever was found so the next query for this pair is a direct
  // lookup rather than another search.
  cache_relation (bb, result, lhs, rhs);

  if (dump_file && (param_ranger_debug & RANGER_DEBUG_RELATION)
      && result != VREL_VARYING)
    {
      fprintf (dump_file, "  relation_search returning: ");
      print_generic_expr (dump_file, lhs, TDF_SLIM);
      print_relation (dump_file, result);
      print_generic_expr (dump_file, rhs, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  return result;
}

// Query if there is a relation between SSA1 and SS2 in block BB or a
// dominator of BB

relation_kind
dom_oracle::query (basic_block bb, tree ssa1, tree ssa2)
{
  relation_kind kind;
  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);
  if (v1 == v2)
    return VREL_EQ;

  // If v1 or v2 do not have any relations or equivalences, a partial
  // equivalence is the only possibility.
  if ((!bitmap_bit_p (m_relation_set, v1) && !has_equiv_p (v1))
      || (!bitmap_bit_p (m_relation_set, v2) && !has_equiv_p (v2)))
    return partial_equiv (ssa1, ssa2);

  // Check for equivalence first.  They must be in each equivalency set.
  const_bitmap equiv1 = equiv_set (ssa1, bb);
  const_bitmap equiv2 = equiv_set (ssa2, bb);
  if (bitmap_bit_p (equiv1, v2) && bitmap_bit_p (equiv2, v1))
    return VREL_EQ;

  // A statement such as c = a & 0xff makes a partial equivalence between
  // c and a, and an ordinary comparison can then relate the same pair.
  // If both exist, prefer the relation, so look for that first.
  kind = relation_search (bb, ssa1, equiv1, ssa2, equiv2);

  // Finally look for partial equivalences.
  if (kind == VREL_VARYING)
    kind = partial_equiv (ssa1, ssa2);
  return kind;
}

// Dump all the relations in block BB to file F.

void
dom_oracle::dump (FILE *f, basic_block bb) const
{
  equiv_oracle::dump (f,bb);

  if (bb->index >= (int)m_relations.length ())
    return;
  if (!m_relations[bb->index].m_names)
    return;

  value_relation vr;
  FOR_EACH_RELATION_BB (this, bb, vr)
    {
      fprintf (f, "Relational : ");
      vr.dump (f);
      fprintf (f, "\n");
    }
}

// Dump all the relations known to file F.

void
dom_oracle::dump (FILE *f) const
{
  fprintf (f, "Relation dump\n");
  for (unsigned i = 0; i < m_relations.length (); i++)
    if (BASIC_BLOCK_FOR_FN (cfun, i))
      {
	fprintf (f, "BB%d\n", i);
	dump (f, BASIC_BLOCK_FOR_FN (cfun, i));
      }
}

void
relation_oracle::debug () const
{
  dump (stderr);
}

path_oracle::path_oracle (relation_oracle *oracle)
{
  set_root_oracle (oracle);
  bitmap_obstack_initialize (&m_bitmaps);
  obstack_init (&m_chain_obstack);

  // Initialize header records.
  m_equiv.m_names = BITMAP_ALLOC (&m_bitmaps);
  m_equiv.m_bb = NULL;
  m_equiv.m_next = NULL;
  m_relations.m_names = BITMAP_ALLOC (&m_bitmaps);
  m_relations.m_head = NULL;
  m_killed_defs = BITMAP_ALLOC (&m_bitmaps);
}

path_oracle::~path_oracle ()
{
  obstack_free (&m_chain_obstack, NULL);
  bitmap_obstack_release (&m_bitmaps);
}

// Clear any range info and relations associated with NAME.

void
path_oracle::clear (tree name)
{
  if (m_root)
    m_root->clear (name);

  m_relations.clear (name);

  unsigned v = SSA_NAME_VERSION (name);
  equiv_chain *ptr = m_equiv.find (v);
  if (ptr)
    bitmap_clear_bit (ptr->m_names, v);
}

// Return the equiv set for SSA, and if there isn't one, check for equivs
// starting in block BB.

const_bitmap
path_oracle::equiv_set (tree ssa, basic_block bb)
{
  // Check the list first.
  equiv_chain *ptr = m_equiv.find (SSA_NAME_VERSION (ssa));
  if (ptr)
    return ptr->m_names;

  // Otherwise defer to the root oracle.
  if (m_root)
    return m_root->equiv_set (ssa, bb);

  // Allocate a throw away bitmap if there isn't a root oracle.
  bitmap tmp = BITMAP_ALLOC (&m_bitmaps);
  bitmap_set_bit (tmp, SSA_NAME_VERSION (ssa));
  return tmp;
}

// Register an equivalence between SSA1 and SSA2 resolving unknowns from
// block BB.  Return false if no new equivalence was added.

bool
path_oracle::register_equiv (basic_block bb, tree ssa1, tree ssa2)
{
  const_bitmap equiv_1 = equiv_set (ssa1, bb);
  const_bitmap equiv_2 = equiv_set (ssa2, bb);

  // Check if they are the same set, if so, we're done.
  if (bitmap_equal_p (equiv_1, equiv_2))
    return false;

  // Don't mess around, simply create a new record and insert it first.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  valid_equivs (b, equiv_1, bb);
  valid_equivs (b, equiv_2, bb);

  equiv_chain *ptr = (equiv_chain *) obstack_alloc (&m_chain_obstack,
						    sizeof (equiv_chain));
  ptr->m_names = b;
  ptr->m_bb = NULL;
  ptr->m_next = m_equiv.m_next;
  m_equiv.m_next = ptr;
  bitmap_ior_into (m_equiv.m_names, b);
  return true;
}

// Register killing definition of an SSA_NAME.

void
path_oracle::killing_def (tree ssa)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, " Registering killing_def (path_oracle) ");
      print_generic_expr (dump_file, ssa, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  unsigned v = SSA_NAME_VERSION (ssa);

  bitmap_set_bit (m_killed_defs, v);
  bitmap_set_bit (m_equiv.m_names, v);

  // Now add an equivalency with itself so we don't look to the root oracle.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  bitmap_set_bit (b, v);
  equiv_chain *ptr = (equiv_chain *) obstack_alloc (&m_chain_obstack,
						    sizeof (equiv_chain));
  ptr->m_names = b;
  ptr->m_bb = NULL;
  ptr->m_next = m_equiv.m_next;
  m_equiv.m_next = ptr;

  // Walk the relation list and remove SSA from any relations.
  if (!bitmap_bit_p (m_relations.m_names, v))
    return;

  bitmap_clear_bit (m_relations.m_names, v);
  relation_chain **prev = &(m_relations.m_head);
  relation_chain *next = NULL;
  for (relation_chain *ptr = m_relations.m_head; ptr; ptr = next)
    {
      gcc_checking_assert (*prev == ptr);
      next = ptr->m_next;
      if (SSA_NAME_VERSION (ptr->op1 ()) == v
	  || SSA_NAME_VERSION (ptr->op2 ()) == v)
	*prev = ptr->m_next;
      else
	prev = &(ptr->m_next);
    }
}

// Register relation K between SSA1 and SSA2, resolving unknowns by
// querying from BB.  Return false if no new relation is registered.

bool
path_oracle::record (basic_block bb, relation_kind k, tree ssa1, tree ssa2)
{
  // If the 2 ssa_names are the same, do nothing.  An equivalence is implied,
  // and no other relation makes sense.
  if (ssa1 == ssa2)
    return false;

  // Partial equivalences are tracked in the root equivalence oracle rather
  // than on the path.  Registering a partial equivalence in the path would
  // cause normal relations to collapse to VARYING.
  if (relation_partial_equiv_p (k))
    return false;

  relation_kind curr = query (bb, ssa1, ssa2);
  // Likewise, a partial equivalency result should not be combined with K
  // or the result drops to VARYING.
  if (curr != VREL_VARYING && !relation_partial_equiv_p (curr))
    k = relation_intersect (curr, k);

  // Do not register an impossible relation.
  if (k == VREL_UNDEFINED)
    return false;

  bool ret;
  if (k == VREL_EQ)
    ret = register_equiv (bb, ssa1, ssa2);
  else
    {
      bitmap_set_bit (m_relations.m_names, SSA_NAME_VERSION (ssa1));
      bitmap_set_bit (m_relations.m_names, SSA_NAME_VERSION (ssa2));
      relation_chain *ptr = (relation_chain *) obstack_alloc (&m_chain_obstack,
							  sizeof (relation_chain));
      ptr->set_relation (k, ssa1, ssa2);
      ptr->m_next = m_relations.m_head;
      m_relations.m_head = ptr;
      ret = true;
    }

  if (ret && dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, ssa1, ssa2);
      fprintf (dump_file, " Registering value_relation (path_oracle) ");
      vr.dump (dump_file);
      fprintf (dump_file, " (root: bb%d)\n", bb->index);
    }
  return ret;
}

// Query for a relationship between equiv set B1 and B2, resolving unknowns
// starting at block BB.

relation_kind
path_oracle::query (basic_block bb, const_bitmap b1, const_bitmap b2)
{
  if (bitmap_equal_p (b1, b2))
    return VREL_EQ;

  relation_kind k = m_relations.find_relation (b1, b2);

  // Do not look at the root oracle for names that have been killed
  // along the path.
  if (bitmap_intersect_p (m_killed_defs, b1)
      || bitmap_intersect_p (m_killed_defs, b2))
    return k;

  // Query the root oracle for relations with path local equivalencies.
  if (k == VREL_VARYING && m_root)
    k = m_root->query (bb, b1, b2);

  return k;
}

// Query for a relationship between SSA1 and SSA2, resolving unknowns
// starting at block BB.

relation_kind
path_oracle::query (basic_block bb, tree ssa1, tree ssa2)
{
  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);

  if (v1 == v2)
    return VREL_EQ;

  const_bitmap equiv_1 = equiv_set (ssa1, bb);
  const_bitmap equiv_2 = equiv_set (ssa2, bb);
  if (bitmap_bit_p (equiv_1, v2) && bitmap_bit_p (equiv_2, v1))
    return VREL_EQ;

  relation_kind rel = query (bb, equiv_1, equiv_2);

  // If the path relation query fails, check for relations in the root oracle.
  if (rel == VREL_VARYING && m_root)
      rel = m_root->query (bb, ssa1, ssa2);
  return rel;
}

// Reset any relations registered on this path.  ORACLE is the root
// oracle to use.

void
path_oracle::reset_path (relation_oracle *oracle)
{
  set_root_oracle (oracle);
  m_equiv.m_next = NULL;
  bitmap_clear (m_equiv.m_names);
  m_relations.m_head = NULL;
  bitmap_clear (m_relations.m_names);
  bitmap_clear (m_killed_defs);
}

// Dump relation in basic block... Do nothing here.

void
path_oracle::dump (FILE *, basic_block) const
{
}

// Dump the relations and equivalencies found in the path.

void
path_oracle::dump (FILE *f) const
{
  equiv_chain *ptr = m_equiv.m_next;
  relation_chain *ptr2 = m_relations.m_head;

  if (ptr || ptr2)
    fprintf (f, "\npath_oracle:\n");

  for (; ptr; ptr = ptr->m_next)
    ptr->dump (f);

  for (; ptr2; ptr2 = ptr2->m_next)
    {
      fprintf (f, "Relational : ");
      ptr2->dump (f);
      fprintf (f, "\n");
    }
}

// ------------------------------------------------------------------------
//  EQUIV iterator.  Although we have bitmap iterators, don't expose that it
//  is currently a bitmap.  Use an export iterator to hide future changes.

// Construct a basic iterator over an equivalence bitmap.

equiv_relation_iterator::equiv_relation_iterator (relation_oracle *oracle,
						  basic_block bb, tree name,
						  bool full, bool partial)
{
  m_name = name;
  m_oracle = oracle;
  m_pe = partial ? oracle->partial_equiv_set (name) : NULL;
  m_bm = NULL;
  if (full)
    m_bm = oracle->equiv_set (name, bb);
  if (!m_bm && m_pe)
    m_bm = m_pe->members;
  if (m_bm)
    bmp_iter_set_init (&m_bi, m_bm, 1, &m_y);
}

// Move to the next export bitmap spot.

void
equiv_relation_iterator::next ()
{
  bmp_iter_next (&m_bi, &m_y);
}

// Fetch the name of the next export in the export list.  Return NULL if
// iteration is done.

tree
equiv_relation_iterator::get_name (relation_kind *rel)
{
  if (!m_bm)
    return NULL_TREE;

  while (bmp_iter_set (&m_bi, &m_y))
    {
      // Do not return self.
      tree t = ssa_name (m_y);
      if (t && t != m_name)
	{
	  relation_kind k = VREL_EQ;
	  if (m_pe && m_bm == m_pe->members)
	    {
	      const pe_slice *equiv_pe = m_oracle->partial_equiv_set (t);
	      if (equiv_pe && equiv_pe->members == m_pe->members)
		k = pe_min (m_pe->code, equiv_pe->code);
	      else
		k = VREL_VARYING;
	    }
	  if (relation_equiv_p (k))
	    {
	      if (rel)
		*rel = k;
	      return t;
	    }
	}
      next ();
    }

  // Process partial equivs after full equivs if both were requested.
  if (m_pe && m_bm != m_pe->members)
    {
      m_bm = m_pe->members;
      if (m_bm)
	{
	  // Recursively call back to process First PE.
	  bmp_iter_set_init (&m_bi, m_bm, 1, &m_y);
	  return get_name (rel);
	}
    }
  return NULL_TREE;
}

#if CHECKING_P
#include "selftest.h"

namespace selftest
{
void
relation_tests ()
{
  // rr_*_table tables use unsigned char rather than relation_kind.
  ASSERT_LT (VREL_LAST, UCHAR_MAX);
  for (relation_kind r1 = VREL_VARYING; r1 < VREL_LAST;
       r1 = relation_kind (r1 + 1))
    {
      // Swapping the operands twice is a no-op.
      ASSERT_EQ (relation_swap (relation_swap (r1)), r1);

      // VARYING intersect X is X.
      // UNDEFINED intersect X is UNDEFINED.
      ASSERT_EQ (relation_intersect (VREL_VARYING, r1), r1);
      ASSERT_EQ (relation_intersect (VREL_UNDEFINED, r1), VREL_UNDEFINED);

      // UNDEFINED union X is X.
      // VARYING union X is VARYING.
      ASSERT_EQ (relation_union (VREL_UNDEFINED, r1), r1);
      ASSERT_EQ (relation_union (VREL_VARYING, r1), VREL_VARYING);

      // Verify commutativity of relation_intersect and relation_union.
      for (relation_kind r2 = VREL_VARYING; r2 < VREL_LAST;
	   r2 = relation_kind (r2 + 1))
	{
	  ASSERT_EQ (relation_intersect (r1, r2), relation_intersect (r2, r1));
	  ASSERT_EQ (relation_union (r1, r2), relation_union (r2, r1));
	}
    }

  // Verify partial equivalence properties.
  for (relation_kind r1 = VREL_PE8; r1 <= VREL_PE64;
       r1 = relation_kind (r1 + 1))
    {
      ASSERT_EQ (relation_swap (r1), r1);
      ASSERT_EQ (relation_intersect (VREL_EQ, r1), VREL_EQ);
      ASSERT_EQ (relation_union (VREL_EQ, r1), r1);
      ASSERT_EQ (relation_transitive (VREL_EQ, r1), r1);
      ASSERT_EQ (relation_transitive (r1, VREL_EQ), r1);
      for (relation_kind r2 = VREL_PE8; r2 <= VREL_PE64;
	   r2 = relation_kind (r2 + 1))
	{
	  ASSERT_EQ (relation_intersect (r1, r2), MAX (r1, r2));
	  ASSERT_EQ (relation_union (r1, r2), pe_min (r1, r2));
	  ASSERT_EQ (relation_transitive (r1, r2), pe_min (r1, r2));
	}
    }
}

} // namespace selftest

#endif // CHECKING_P
