/* Header file for the value range relational processing.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
static const unsigned char rr_negate_table[VREL_LAST] = {
  VREL_VARYING, VREL_UNDEFINED, VREL_GE, VREL_GT, VREL_LE, VREL_LT, VREL_NE,
  VREL_EQ };

// Negate the relation, as in logical negation.

relation_kind
relation_negate (relation_kind r)
{
  return relation_kind (rr_negate_table [r]);
}

// This table is used to swap the operands.  op1 REL op2 -> op2 REL op1.
static const unsigned char rr_swap_table[VREL_LAST] = {
  VREL_VARYING, VREL_UNDEFINED, VREL_GT, VREL_GE, VREL_LT, VREL_LE, VREL_EQ,
  VREL_NE };

// Return the relation as if the operands were swapped.

relation_kind
relation_swap (relation_kind r)
{
  return relation_kind (rr_swap_table [r]);
}

// This table is used to perform an intersection between 2 relations.

static const unsigned char rr_intersect_table[VREL_LAST][VREL_LAST] = {
// VREL_VARYING
  { VREL_VARYING, VREL_UNDEFINED, VREL_LT, VREL_LE, VREL_GT, VREL_GE, VREL_EQ,
    VREL_NE },
// VREL_UNDEFINED
  { VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED,
    VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED },
// VREL_LT
  { VREL_LT, VREL_UNDEFINED, VREL_LT, VREL_LT, VREL_UNDEFINED, VREL_UNDEFINED,
    VREL_UNDEFINED, VREL_LT },
// VREL_LE
  { VREL_LE, VREL_UNDEFINED, VREL_LT, VREL_LE, VREL_UNDEFINED, VREL_EQ,
    VREL_EQ, VREL_LT },
// VREL_GT
  { VREL_GT, VREL_UNDEFINED, VREL_UNDEFINED, VREL_UNDEFINED, VREL_GT, VREL_GT,
    VREL_UNDEFINED, VREL_GT },
// VREL_GE
  { VREL_GE, VREL_UNDEFINED, VREL_UNDEFINED, VREL_EQ, VREL_GT, VREL_GE,
    VREL_EQ, VREL_GT },
// VREL_EQ
  { VREL_EQ, VREL_UNDEFINED, VREL_UNDEFINED, VREL_EQ, VREL_UNDEFINED, VREL_EQ,
    VREL_EQ, VREL_UNDEFINED },
// VREL_NE
  { VREL_NE, VREL_UNDEFINED, VREL_LT, VREL_LT, VREL_GT, VREL_GT,
    VREL_UNDEFINED, VREL_NE } };


// Intersect relation R1 with relation R2 and return the resulting relation.

relation_kind
relation_intersect (relation_kind r1, relation_kind r2)
{
  return relation_kind (rr_intersect_table[r1][r2]);
}


// This table is used to perform a union between 2 relations.

static const unsigned char rr_union_table[VREL_LAST][VREL_LAST] = {
// VREL_VARYING
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_UNDEFINED
  { VREL_VARYING, VREL_UNDEFINED, VREL_LT, VREL_LE, VREL_GT, VREL_GE,
    VREL_EQ, VREL_NE },
// VREL_LT
  { VREL_VARYING, VREL_LT, VREL_LT, VREL_LE, VREL_NE, VREL_VARYING, VREL_LE,
    VREL_NE },
// VREL_LE
  { VREL_VARYING, VREL_LE, VREL_LE, VREL_LE, VREL_VARYING, VREL_VARYING,
    VREL_LE, VREL_VARYING },
// VREL_GT
  { VREL_VARYING, VREL_GT, VREL_NE, VREL_VARYING, VREL_GT, VREL_GE, VREL_GE,
    VREL_NE },
// VREL_GE
  { VREL_VARYING, VREL_GE, VREL_VARYING, VREL_VARYING, VREL_GE, VREL_GE,
    VREL_GE, VREL_VARYING },
// VREL_EQ
  { VREL_VARYING, VREL_EQ, VREL_LE, VREL_LE, VREL_GE, VREL_GE, VREL_EQ,
    VREL_VARYING },
// VREL_NE
  { VREL_VARYING, VREL_NE, VREL_NE, VREL_VARYING, VREL_NE, VREL_VARYING,
    VREL_VARYING, VREL_NE } };

// Union relation R1 with relation R2 and return the result.

relation_kind
relation_union (relation_kind r1, relation_kind r2)
{
  return relation_kind (rr_union_table[r1][r2]);
}


// This table is used to determine transitivity between 2 relations.
// (A relation0 B) and (B relation1 C) implies  (A result C)

static const unsigned char rr_transitive_table[VREL_LAST][VREL_LAST] = {
// VREL_VARYING
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_UNDEFINED
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING },
// VREL_LT
  { VREL_VARYING, VREL_VARYING, VREL_LT, VREL_LT, VREL_VARYING, VREL_VARYING,
    VREL_LT, VREL_VARYING },
// VREL_LE
  { VREL_VARYING, VREL_VARYING, VREL_LT, VREL_LE, VREL_VARYING, VREL_VARYING,
    VREL_LE, VREL_VARYING },
// VREL_GT
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_GT, VREL_GT,
    VREL_GT, VREL_VARYING },
// VREL_GE
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_GT, VREL_GE,
    VREL_GE, VREL_VARYING },
// VREL_EQ
  { VREL_VARYING, VREL_VARYING, VREL_LT, VREL_LE, VREL_GT, VREL_GE, VREL_EQ,
    VREL_VARYING },
// VREL_NE
  { VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING, VREL_VARYING,
    VREL_VARYING, VREL_VARYING, VREL_VARYING } };

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

// This vector maps a relation to the equivalent tree code.

static const tree_code relation_to_code [VREL_LAST] = {
  ERROR_MARK, ERROR_MARK, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EQ_EXPR,
  NE_EXPR };

// This routine validates that a relation can be applied to a specific set of
// ranges.  In particular, floating point x == x may not be true if the NaN bit
// is set in the range.  Symbolically the oracle will determine x == x,
// but specific range instances may override this.
// To verify, attempt to fold the relation using the supplied ranges.
// One would expect [1,1] to be returned, anything else means there is something
// in the range preventing the relation from applying.
// If there is no mechanism to verify, assume the relation is acceptable.

relation_kind
relation_oracle::validate_relation (relation_kind rel, vrange &op1, vrange &op2)
{
  // If there is no mapping to a tree code, leave the relation as is.
  tree_code code = relation_to_code [rel];
  if (code == ERROR_MARK)
    return rel;

  // Undefined ranges cannot be checked either.
  if (op1.undefined_p () || op2.undefined_p ())
    return rel;

  tree t1 = op1.type ();
  tree t2 = op2.type ();

  // If the range types are not compatible, no relation can exist.
  if (!range_compatible_p (t1, t2))
    return VREL_VARYING;

  // If there is no handler, leave the relation as is.
  range_op_handler handler (code);
  if (!handler)
    return rel;

  // If the relation cannot be folded for any reason, leave as is.
  Value_Range result (boolean_type_node);
  if (!handler.fold_range (result, boolean_type_node, op1, op2,
			   relation_trio::op1_op2 (rel)))
    return rel;

  // The expression op1 REL op2 using REL should fold to [1,1].
  // Any other result means the relation is not verified to be true.
  if (result.varying_p () || result.zero_p ())
    return VREL_VARYING;

  return rel;
}

// If no range is available, create a varying range for each SSA name and
// verify.

relation_kind
relation_oracle::validate_relation (relation_kind rel, tree ssa1, tree ssa2)
{
  Value_Range op1, op2;
  op1.set_varying (TREE_TYPE (ssa1));
  op2.set_varying (TREE_TYPE (ssa2));

  return validate_relation (rel, op1, op2);
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
  obstack_init (&m_chain_obstack);
  m_self_equiv.create (0);
  m_self_equiv.safe_grow_cleared (num_ssa_names + 1);
  m_partial.create (0);
  m_partial.safe_grow_cleared (num_ssa_names + 1);
}

// Destruct an equivalency oracle.

equiv_oracle::~equiv_oracle ()
{
  m_partial.release ();
  m_self_equiv.release ();
  obstack_free (&m_chain_obstack, NULL);
  m_equiv.release ();
  bitmap_obstack_release (&m_bitmaps);
}

// Add a partial equivalence R between OP1 and OP2.

void
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
	return;
      // If there are no uses of op2, do not register.
      if (has_zero_uses (op2))
	return;
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
      return;
    }
  if (pe2.members)
    {
      // If there are no uses of op1, do not register.
      if (has_zero_uses (op1))
	return;
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
	return;
      // Neither name has an entry, simply create op1 as slice of op2.
      pe2.code = bits_to_pe (TYPE_PRECISION (TREE_TYPE (op2)));
      if (pe2.code == VREL_VARYING)
	return;
      pe2.ssa_base = op2;
      pe2.members = BITMAP_ALLOC (&m_bitmaps);
      bitmap_set_bit (pe2.members, v2);
      pe1.ssa_base = op2;
      pe1.code = r;
      pe1.members = pe2.members;
      bitmap_set_bit (pe1.members, v1);
    }
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
  if (v >= m_self_equiv.length ())
    m_self_equiv.safe_grow_cleared (num_ssa_names + 1);

  if (!m_self_equiv[v])
    {
      m_self_equiv[v] = BITMAP_ALLOC (&m_bitmaps);
      bitmap_set_bit (m_self_equiv[v], v);
    }
  return m_self_equiv[v];
}

// Query if there is a relation (equivalence) between 2 SSA_NAMEs.

relation_kind
equiv_oracle::query_relation (basic_block bb, tree ssa1, tree ssa2)
{
  // If the 2 ssa names share the same equiv set, they are equal.
  if (equiv_set (ssa1, bb) == equiv_set (ssa2, bb))
    return VREL_EQ;

  // Check if there is a partial equivalence.
  return partial_equiv (ssa1, ssa2);
}

// Query if there is a relation (equivalence) between 2 SSA_NAMEs.

relation_kind
equiv_oracle::query_relation (basic_block bb ATTRIBUTE_UNUSED, const_bitmap e1,
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
      return NULL;
    }

  // Otherwise create an equivalence for this block which is a copy
  // of equiv, the add V to the set.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  valid_equivs (b, equiv->m_names, bb);
  bitmap_set_bit (b, v);
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
	// Ensure the new names are in the summary for BB.
	bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_1->m_names);
      return NULL;
    }
  // If equiv_2 is in BB, use it for the combined set.
  if (equiv_2->m_bb == bb)
    {
      valid_equivs (equiv_2->m_names, equiv_1->m_names, bb);
      // Ensure the new names are in the summary.
      bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_2->m_names);
      return NULL;
    }

  // At this point, neither equivalence is from this block.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  valid_equivs (b, equiv_1->m_names, bb);
  valid_equivs (b, equiv_2->m_names, bb);
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
  gcc_checking_assert (bb && !find_equiv_dom (ssa, bb));

  unsigned v = SSA_NAME_VERSION (ssa);
  bitmap_set_bit (m_equiv_set, v);
  bitmap equiv_set = BITMAP_ALLOC (&m_bitmaps);
  bitmap_set_bit (equiv_set, v);
  add_equiv_to_block (bb, equiv_set);
}

// Register an equivalence between SSA1 and SSA2 in block BB.
// The equivalence oracle maintains a vector of equivalencies indexed by basic
// block. When an equivalence between SSA1 and SSA2 is registered in block BB,
// a query is made as to what equivalences both names have already, and
// any preexisting equivalences are merged to create a single equivalence
// containing all the ssa_names in this basic block.

void
equiv_oracle::register_relation (basic_block bb, relation_kind k, tree ssa1,
				 tree ssa2)
{
  // Process partial equivalencies.
  if (relation_partial_equiv_p (k))
    {
      add_partial_equiv (k, ssa1, ssa2);
      return;
    }
  // Only handle equality relations.
  if (k != VREL_EQ)
    return;

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
    return;

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
    return;

  add_equiv_to_block (bb, equiv_set);
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
// Negate the current relation.

void
value_relation::negate ()
{
  related = relation_negate (related);
}

// Perform an intersection between 2 relations. *this &&= p.

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

// Instantiate a relation oracle.

dom_oracle::dom_oracle ()
{
  m_relations.create (0);
  m_relations.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
  m_relation_set = BITMAP_ALLOC (&m_bitmaps);
  m_tmp = BITMAP_ALLOC (&m_bitmaps);
  m_tmp2 = BITMAP_ALLOC (&m_bitmaps);
}

// Destruct a relation oracle.

dom_oracle::~dom_oracle ()
{
  m_relations.release ();
}

// Register relation K between ssa_name OP1 and OP2 on STMT.

void
relation_oracle::register_stmt (gimple *stmt, relation_kind k, tree op1,
				tree op2)
{
  gcc_checking_assert (TREE_CODE (op1) == SSA_NAME);
  gcc_checking_assert (TREE_CODE (op2) == SSA_NAME);
  gcc_checking_assert (stmt && gimple_bb (stmt));

  // Don't register lack of a relation.
  if (k == VREL_VARYING)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, op1, op2);
      fprintf (dump_file, " Registering value_relation ");
      vr.dump (dump_file);
      fprintf (dump_file, " (bb%d) at ", gimple_bb (stmt)->index);
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

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
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  Not registered due to ");
	      print_generic_expr (dump_file, arg, TDF_SLIM);
	      fprintf (dump_file, " being defined in the same block.\n");
	    }
	  return;
	}
    }
  register_relation (gimple_bb (stmt), k, op1, op2);
}

// Register relation K between ssa_name OP1 and OP2 on edge E.

void
relation_oracle::register_edge (edge e, relation_kind k, tree op1, tree op2)
{
  gcc_checking_assert (TREE_CODE (op1) == SSA_NAME);
  gcc_checking_assert (TREE_CODE (op2) == SSA_NAME);

  // Do not register lack of relation, or blocks which have more than
  // edge E for a predecessor.
  if (k == VREL_VARYING || !single_pred_p (e->dest))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, op1, op2);
      fprintf (dump_file, " Registering value_relation ");
      vr.dump (dump_file);
      fprintf (dump_file, " on (%d->%d)\n", e->src->index, e->dest->index);
    }

  register_relation (e->dest, k, op1, op2);
}

// Register relation K between OP! and OP2 in block BB.
// This creates the record and searches for existing records in the dominator
// tree to merge with.

void
dom_oracle::register_relation (basic_block bb, relation_kind k, tree op1,
			       tree op2)
{
  // If the 2 ssa_names are the same, do nothing.  An equivalence is implied,
  // and no other relation makes sense.
  if (op1 == op2)
    return;

  // Equivalencies are handled by the equivalence oracle.
  if (relation_equiv_p (k))
    equiv_oracle::register_relation (bb, k, op1, op2);
  else
    {
      // if neither op1 nor op2 are in a relation before this is registered,
      // there will be no transitive.
      bool check = bitmap_bit_p (m_relation_set, SSA_NAME_VERSION (op1))
		   || bitmap_bit_p (m_relation_set, SSA_NAME_VERSION (op2));
      relation_chain *ptr = set_one_relation (bb, k, op1, op2);
      if (ptr && check)
	register_transitives (bb, *ptr);
    }
}

// Register relation K between OP! and OP2 in block BB.
// This creates the record and searches for existing records in the dominator
// tree to merge with.  Return the record, or NULL if no record was created.

relation_chain *
dom_oracle::set_one_relation (basic_block bb, relation_kind k, tree op1,
			      tree op2)
{
  gcc_checking_assert (k != VREL_VARYING && k != VREL_EQ);

  value_relation vr(k, op1, op2);
  int bbi = bb->index;

  if (bbi >= (int)m_relations.length())
    m_relations.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);

  // Summary bitmap indicating what ssa_names have relations in this BB.
  bitmap bm = m_relations[bbi].m_names;
  if (!bm)
    bm = m_relations[bbi].m_names = BITMAP_ALLOC (&m_bitmaps);
  unsigned v1 = SSA_NAME_VERSION (op1);
  unsigned v2 = SSA_NAME_VERSION (op2);

  relation_kind curr;
  relation_chain *ptr;
  curr = find_relation_block (bbi, v1, v2, &ptr);
  // There is an existing relation in this block, just intersect with it.
  if (curr != VREL_VARYING)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "    Intersecting with existing ");
	  ptr->dump (dump_file);
	}
      // Check into whether we can simply replace the relation rather than
      // intersecting it.  This may help with some optimistic iterative
      // updating algorithms.
      bool new_rel = ptr->intersect (vr);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " to produce ");
	  ptr->dump (dump_file);
	  fprintf (dump_file, " %s.\n", new_rel ? "Updated" : "No Change");
	}
      // If there was no change, return no record..
      if (!new_rel)
	return NULL;
    }
  else
    {
      if (m_relations[bbi].m_num_relations >= param_relation_block_limit)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Not registered due to bb being full\n");
	  return NULL;
	}
      m_relations[bbi].m_num_relations++;
      // Check for an existing relation further up the DOM chain.
      // By including dominating relations, The first one found in any search
      // will be the aggregate of all the previous ones.
      curr = find_relation_dom (bb, v1, v2);
      if (curr != VREL_VARYING)
	k = relation_intersect (curr, k);

      bitmap_set_bit (bm, v1);
      bitmap_set_bit (bm, v2);
      bitmap_set_bit (m_relation_set, v1);
      bitmap_set_bit (m_relation_set, v2);

      ptr = (relation_chain *) obstack_alloc (&m_chain_obstack,
					      sizeof (relation_chain));
      ptr->set_relation (k, op1, op2);
      ptr->m_next = m_relations[bbi].m_head;
      m_relations[bbi].m_head = ptr;
    }
  return ptr;
}

// Starting at ROOT_BB search the DOM tree  looking for relations which
// may produce transitive relations to RELATION.  EQUIV1 and EQUIV2 are
// bitmaps for op1/op2 and any of their equivalences that should also be
// considered.

void
dom_oracle::register_transitives (basic_block root_bb,
				  const value_relation &relation)
{
  basic_block bb;
  // Only apply transitives to certain kinds of operations.
  switch (relation.kind ())
    {
      case VREL_LE:
      case VREL_LT:
      case VREL_GT:
      case VREL_GE:
	break;
      default:
	return;
    }

  const_bitmap equiv1 = equiv_set (relation.op1 (), root_bb);
  const_bitmap equiv2 = equiv_set (relation.op2 (), root_bb);

  for (bb = root_bb; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      int bbi = bb->index;
      if (bbi >= (int)m_relations.length())
	continue;
      const_bitmap bm = m_relations[bbi].m_names;
      if (!bm)
	continue;
      if (!bitmap_intersect_p (bm, equiv1) && !bitmap_intersect_p (bm, equiv2))
	continue;
      // At least one of the 2 ops has a relation in this block.
      relation_chain *ptr;
      for (ptr = m_relations[bbi].m_head; ptr ; ptr = ptr->m_next)
	{
	  // In the presence of an equivalence, 2 operands may do not
	  // naturally match. ie  with equivalence a_2 == b_3
	  // given c_1 < a_2 && b_3 < d_4
	  // convert the second relation (b_3 < d_4) to match any
	  // equivalences to found in the first relation.
	  // ie convert b_3 < d_4 to a_2 < d_4, which then exposes the
	  // transitive operation:  c_1 < a_2 && a_2 < d_4 -> c_1 < d_4

	  tree r1, r2;
	  tree p1 = ptr->op1 ();
	  tree p2 = ptr->op2 ();
	  // Find which equivalence is in the first operand.
	  if (bitmap_bit_p (equiv1, SSA_NAME_VERSION (p1)))
	    r1 = p1;
	  else if (bitmap_bit_p (equiv1, SSA_NAME_VERSION (p2)))
	    r1 = p2;
	  else
	    r1 = NULL_TREE;

	  // Find which equivalence is in the second operand.
	  if (bitmap_bit_p (equiv2, SSA_NAME_VERSION (p1)))
	    r2 = p1;
	  else if (bitmap_bit_p (equiv2, SSA_NAME_VERSION (p2)))
	    r2 = p2;
	  else
	    r2 = NULL_TREE;

	  // Ignore if both NULL (not relevant relation) or the same,
	  if (r1 == r2)
	    continue;

	  // Any operand not an equivalence, just take the real operand.
	  if (!r1)
	    r1 = relation.op1 ();
	  if (!r2)
	    r2 = relation.op2 ();

	  value_relation nr (relation.kind (), r1, r2);
	  if (nr.apply_transitive (*ptr))
	    {
	      if (!set_one_relation (root_bb, nr.kind (), nr.op1 (), nr.op2 ()))
		return;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "   Registering transitive relation ");
		  nr.dump (dump_file);
		  fputc ('\n', dump_file);
		}
	    }

	}
    }
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
dom_oracle::query_relation (basic_block bb, const_bitmap b1,
			    const_bitmap b2)
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
dom_oracle::find_relation_block (int bb, unsigned v1, unsigned v2,
				     relation_chain **obj) const
{
  if (bb >= (int)m_relations.length())
    return VREL_VARYING;

  const_bitmap bm = m_relations[bb].m_names;
  if (!bm)
    return VREL_VARYING;

  // If both b1 and b2 aren't referenced in this block, cant be a relation
  if (!bitmap_bit_p (bm, v1) || !bitmap_bit_p (bm, v2))
    return VREL_VARYING;

  relation_chain *ptr;
  for (ptr = m_relations[bb].m_head; ptr ; ptr = ptr->m_next)
    {
      unsigned op1 = SSA_NAME_VERSION (ptr->op1 ());
      unsigned op2 = SSA_NAME_VERSION (ptr->op2 ());
      if (v1 == op1 && v2 == op2)
	{
	  if (obj)
	    *obj = ptr;
	  return ptr->kind ();
	}
      if (v1 == op2 && v2 == op1)
	{
	  if (obj)
	    *obj = ptr;
	  return relation_swap (ptr->kind ());
	}
    }

  return VREL_VARYING;
}

// Find a relation between SSA version V1 and V2 in the dominator tree
// starting with block BB

relation_kind
dom_oracle::find_relation_dom (basic_block bb, unsigned v1, unsigned v2) const
{
  relation_kind r;
  // IF either name does not occur in a relation anywhere, there isn't one.
  if (!bitmap_bit_p (m_relation_set, v1) || !bitmap_bit_p (m_relation_set, v2))
    return VREL_VARYING;

  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      r = find_relation_block (bb->index, v1, v2);
      if (r != VREL_VARYING)
	return r;
    }
  return VREL_VARYING;

}

// Query if there is a relation between SSA1 and SS2 in block BB or a
// dominator of BB

relation_kind
dom_oracle::query_relation (basic_block bb, tree ssa1, tree ssa2)
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

  kind = partial_equiv (ssa1, ssa2);
  if (kind != VREL_VARYING)
    return kind;

  // Initially look for a direct relationship and just return that.
  kind = find_relation_dom (bb, v1, v2);
  if (kind != VREL_VARYING)
    return kind;

  // Query using the equivalence sets.
  kind = query_relation (bb, equiv1, equiv2);
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

  relation_chain *ptr = m_relations[bb->index].m_head;
  for (; ptr; ptr = ptr->m_next)
    {
      fprintf (f, "Relational : ");
      ptr->dump (f);
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
// block BB.

void
path_oracle::register_equiv (basic_block bb, tree ssa1, tree ssa2)
{
  const_bitmap equiv_1 = equiv_set (ssa1, bb);
  const_bitmap equiv_2 = equiv_set (ssa2, bb);

  // Check if they are the same set, if so, we're done.
  if (bitmap_equal_p (equiv_1, equiv_2))
    return;

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
// querying from BB.

void
path_oracle::register_relation (basic_block bb, relation_kind k, tree ssa1,
				tree ssa2)
{
  // If the 2 ssa_names are the same, do nothing.  An equivalence is implied,
  // and no other relation makes sense.
  if (ssa1 == ssa2)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, ssa1, ssa2);
      fprintf (dump_file, " Registering value_relation (path_oracle) ");
      vr.dump (dump_file);
      fprintf (dump_file, " (root: bb%d)\n", bb->index);
    }

  relation_kind curr = query_relation (bb, ssa1, ssa2);
  if (curr != VREL_VARYING)
    k = relation_intersect (curr, k);

  if (k == VREL_EQ)
    {
      register_equiv (bb, ssa1, ssa2);
      return;
    }

  bitmap_set_bit (m_relations.m_names, SSA_NAME_VERSION (ssa1));
  bitmap_set_bit (m_relations.m_names, SSA_NAME_VERSION (ssa2));
  relation_chain *ptr = (relation_chain *) obstack_alloc (&m_chain_obstack,
						      sizeof (relation_chain));
  ptr->set_relation (k, ssa1, ssa2);
  ptr->m_next = m_relations.m_head;
  m_relations.m_head = ptr;
}

// Query for a relationship between equiv set B1 and B2, resolving unknowns
// starting at block BB.

relation_kind
path_oracle::query_relation (basic_block bb, const_bitmap b1, const_bitmap b2)
{
  if (bitmap_equal_p (b1, b2))
    return VREL_EQ;

  relation_kind k = m_relations.find_relation (b1, b2);

  // Do not look at the root oracle for names that have been killed
  // along the path.
  if (bitmap_intersect_p (m_killed_defs, b1)
      || bitmap_intersect_p (m_killed_defs, b2))
    return k;

  if (k == VREL_VARYING && m_root)
    k = m_root->query_relation (bb, b1, b2);

  return k;
}

// Query for a relationship between SSA1 and SSA2, resolving unknowns
// starting at block BB.

relation_kind
path_oracle::query_relation (basic_block bb, tree ssa1, tree ssa2)
{
  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);

  if (v1 == v2)
    return VREL_EQ;

  const_bitmap equiv_1 = equiv_set (ssa1, bb);
  const_bitmap equiv_2 = equiv_set (ssa2, bb);
  if (bitmap_bit_p (equiv_1, v2) && bitmap_bit_p (equiv_2, v1))
    return VREL_EQ;

  return query_relation (bb, equiv_1, equiv_2);
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
  // Verify commutativity of relation_intersect and relation_union.
  for (relation_kind r1 = VREL_VARYING; r1 < VREL_PE8;
       r1 = relation_kind (r1 + 1))
    for (relation_kind r2 = VREL_VARYING; r2 < VREL_PE8;
	 r2 = relation_kind (r2 + 1))
      {
	ASSERT_EQ (relation_intersect (r1, r2), relation_intersect (r2, r1));
	ASSERT_EQ (relation_union (r1, r2), relation_union (r2, r1));
      }
}

} // namespace selftest

#endif // CHECKING_P
