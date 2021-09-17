/* Header file for the value range relational processing.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// These VREL codes are arranged such that VREL_NONE is the first
// code, and all the rest are contiguous up to and including VREL_LAST.

#define VREL_FIRST              VREL_NONE
#define VREL_LAST               NE_EXPR
#define VREL_COUNT              (VREL_LAST - VREL_FIRST + 1)

// vrel_range_assert will either assert that the tree code passed is valid,
// or mark invalid codes as unreachable to help with table optimation.
#if CHECKING_P
  #define vrel_range_assert(c) 			\
    gcc_checking_assert ((c) >= VREL_FIRST && (c) <= VREL_LAST)
#else
  #define vrel_range_assert(c)			\
    if ((c) < VREL_FIRST || (c) > VREL_LAST)	\
      gcc_unreachable ();
#endif

static const char *kind_string[VREL_COUNT] =
{ "none", "<", "<=", ">", ">=", "empty", "==", "!=" };

// Print a relation_kind REL to file F.

void
print_relation (FILE *f, relation_kind rel)
{
  vrel_range_assert (rel);
  fprintf (f, " %s ", kind_string[rel - VREL_FIRST]);
}

// This table is used to negate the operands.  op1 REL op2 -> !(op1 REL op2).
relation_kind rr_negate_table[VREL_COUNT] = {
//     NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY,      EQ_EXPR, NE_EXPR
  VREL_NONE, GE_EXPR, GT_EXPR, LE_EXPR, LT_EXPR, VREL_EMPTY, NE_EXPR, EQ_EXPR };

// Negate the relation, as in logical negation.

relation_kind
relation_negate (relation_kind r)
{
  vrel_range_assert (r);
  return rr_negate_table [r - VREL_FIRST];
}

// This table is used to swap the operands.  op1 REL op2 -> op2 REL op1.
relation_kind rr_swap_table[VREL_COUNT] = {
//     NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY,      EQ_EXPR, NE_EXPR
  VREL_NONE, GT_EXPR, GE_EXPR, LT_EXPR, LE_EXPR, VREL_EMPTY, EQ_EXPR, NE_EXPR };

// Return the relation as if the operands were swapped.

relation_kind
relation_swap (relation_kind r)
{
  vrel_range_assert (r);
  return rr_swap_table [r - VREL_FIRST];
}

// This table is used to perform an intersection between 2 relations.

relation_kind rr_intersect_table[VREL_COUNT][VREL_COUNT] = {
//   NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY, EQ_EXPR, NE_EXPR
// VREL_NONE
  { VREL_NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, VREL_EMPTY, EQ_EXPR, NE_EXPR },
// LT_EXPR
  { LT_EXPR, LT_EXPR, LT_EXPR, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, LT_EXPR },
// LE_EXPR
  { LE_EXPR, LT_EXPR, LE_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY, EQ_EXPR, LT_EXPR },
// GT_EXPR
  { GT_EXPR, VREL_EMPTY, VREL_EMPTY, GT_EXPR, GT_EXPR, VREL_EMPTY, VREL_EMPTY, GT_EXPR },
// GE_EXPR
  { GE_EXPR, VREL_EMPTY, EQ_EXPR, GT_EXPR, GE_EXPR, VREL_EMPTY, EQ_EXPR, GT_EXPR },
// VREL_EMPTY
  { VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY },
// EQ_EXPR
  { EQ_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY },
// NE_EXPR
  { NE_EXPR, LT_EXPR, LT_EXPR, GT_EXPR, GT_EXPR, VREL_EMPTY, VREL_EMPTY, NE_EXPR } };


// Intersect relation R1 with relation R2 and return the resulting relation.

relation_kind
relation_intersect (relation_kind r1, relation_kind r2)
{
  vrel_range_assert (r1);
  vrel_range_assert (r2);
  return rr_intersect_table[r1 - VREL_FIRST][r2 - VREL_FIRST];
}


// This table is used to perform a union between 2 relations.

relation_kind rr_union_table[VREL_COUNT][VREL_COUNT] = {
//    	 NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY, EQ_EXPR, NE_EXPR
// VREL_NONE
  { VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE },
// LT_EXPR
  { VREL_NONE, LT_EXPR, LE_EXPR, NE_EXPR, VREL_NONE, LT_EXPR, LE_EXPR, NE_EXPR },
// LE_EXPR
  { VREL_NONE, LE_EXPR, LE_EXPR, VREL_NONE, VREL_NONE, LE_EXPR, LE_EXPR, VREL_NONE },
// GT_EXPR
  { VREL_NONE, NE_EXPR, VREL_NONE, GT_EXPR, GE_EXPR, GT_EXPR, GE_EXPR, NE_EXPR },
// GE_EXPR
  { VREL_NONE, VREL_NONE, VREL_NONE, GE_EXPR, GE_EXPR, GE_EXPR, GE_EXPR, VREL_NONE },
// VREL_EMPTY
  { VREL_NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, VREL_EMPTY, EQ_EXPR, NE_EXPR },
// EQ_EXPR
  { VREL_NONE, LE_EXPR, LE_EXPR, GE_EXPR, GE_EXPR, EQ_EXPR, EQ_EXPR, VREL_NONE },
// NE_EXPR
  { VREL_NONE, NE_EXPR, VREL_NONE, NE_EXPR, VREL_NONE, NE_EXPR, VREL_NONE, NE_EXPR } };

// Union relation R1 with relation R2 and return the result.

relation_kind
relation_union (relation_kind r1, relation_kind r2)
{
  vrel_range_assert (r1);
  vrel_range_assert (r2);
  return rr_union_table[r1 - VREL_FIRST][r2 - VREL_FIRST];
}


// This table is used to determine transitivity between 2 relations.
// (A relation0 B) and (B relation1 C) implies  (A result C)

relation_kind rr_transitive_table[VREL_COUNT][VREL_COUNT] = {
//   NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY, EQ_EXPR, NE_EXPR
// VREL_NONE
  { VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE },
// LT_EXPR
  { VREL_NONE, LT_EXPR, LT_EXPR, VREL_NONE, VREL_NONE, VREL_NONE, LT_EXPR, VREL_NONE },
// LE_EXPR
  { VREL_NONE, LT_EXPR, LE_EXPR, VREL_NONE, VREL_NONE, VREL_NONE, LE_EXPR, VREL_NONE },
// GT_EXPR
  { VREL_NONE, VREL_NONE, VREL_NONE, GT_EXPR, GT_EXPR, VREL_NONE, GT_EXPR, VREL_NONE },
// GE_EXPR
  { VREL_NONE, VREL_NONE, VREL_NONE, GT_EXPR, GE_EXPR, VREL_NONE, GE_EXPR, VREL_NONE },
// VREL_EMPTY
  { VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE },
// EQ_EXPR
  { VREL_NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, VREL_NONE, EQ_EXPR, VREL_NONE },
// NE_EXPR
  { VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE } };

// Apply transitive operation between relation R1 and relation R2, and
// return the resulting relation, if any.

relation_kind
relation_transitive (relation_kind r1, relation_kind r2)
{
  vrel_range_assert (r1);
  vrel_range_assert (r2);
  return rr_transitive_table[r1 - VREL_FIRST][r2 - VREL_FIRST];
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

  if (!m_names)
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
}

// Destruct an equivalency oracle.

equiv_oracle::~equiv_oracle ()
{
  m_self_equiv.release ();
  obstack_free (&m_chain_obstack, NULL);
  m_equiv.release ();
  bitmap_obstack_release (&m_bitmaps);
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

// Query if thre is a relation (equivalence) between 2 SSA_NAMEs.

relation_kind
equiv_oracle::query_relation (basic_block bb, tree ssa1, tree ssa2)
{
  // If the 2 ssa names share the same equiv set, they are equal.
  if (equiv_set (ssa1, bb) == equiv_set (ssa2, bb))
    return EQ_EXPR;
  return VREL_NONE;
}

// Query if thre is a relation (equivalence) between 2 SSA_NAMEs.

relation_kind
equiv_oracle::query_relation (basic_block bb ATTRIBUTE_UNUSED, const_bitmap e1,
			      const_bitmap e2)
{
  // If the 2 ssa names share the same equiv set, they are equal.
  if (bitmap_equal_p (e1, e2))
    return EQ_EXPR;
  return VREL_NONE;
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

// Register equivalance between ssa_name V and set EQUIV in block BB,

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
  bitmap_copy (b, equiv->m_names);
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
  // If equiv_1 is alreayd in BB, use it as the combined set.
  if (equiv_1->m_bb == bb)
    {
      bitmap_ior_into  (equiv_1->m_names, equiv_2->m_names);
      // Its hard to delete from a single linked list, so
      // just clear the second one.
      if (equiv_2->m_bb == bb)
	bitmap_clear (equiv_2->m_names);
      else
	// Ensure equiv_2s names are in the summary for BB.
	bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_2->m_names);
      return NULL;
    }
  // If equiv_2 is in BB, use it for the combined set.
  if (equiv_2->m_bb == bb)
    {
      bitmap_ior_into (equiv_2->m_names, equiv_1->m_names);
      // Add equiv_1 names into the summary.
      bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_1->m_names);
      return NULL;
    }

  // At this point, neither equivalence is from this block.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  bitmap_copy (b, equiv_1->m_names);
  bitmap_ior_into (b, equiv_2->m_names);
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
// block. When an equivalence bteween SSA1 and SSA2 is registered in block BB,
// a query is made as to what equivalences both names have already, and
// any preexisting equivalences are merged to create a single equivalence
// containing all the ssa_names in this basic block.

void
equiv_oracle::register_relation (basic_block bb, relation_kind k, tree ssa1,
				 tree ssa2)
{
  // Only handle equality relations.
  if (k != EQ_EXPR)
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
// Note the internal caller is responible for allocating EQUIV_SET properly.

void
equiv_oracle::add_equiv_to_block (basic_block bb, bitmap equiv_set)
{
  equiv_chain *ptr;

  // Check if this is the first time a block has an equivalence added.
  // and create a header block. And set the summary for this block.
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
  if (!m_equiv[bb->index])
    return;

  equiv_chain *ptr = m_equiv[bb->index]->m_next;
  for (; ptr; ptr = ptr->m_next)
    ptr->dump (f);
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

// The value-relation class is used to encapsulate the represention of an
// individual relation between 2 ssa-names, and to facilitate operating on
// the relation.

class value_relation
{
public:
  value_relation ();
  value_relation (relation_kind kind, tree n1, tree n2);
  void set_relation (relation_kind kind, tree n1, tree n2);

  inline relation_kind kind () const { return related; }
  inline tree op1 () const { return name1; }
  inline tree op2 () const { return name2; }

  bool union_ (value_relation &p);
  bool intersect (value_relation &p);
  void negate ();
  bool apply_transitive (const value_relation &rel);

  void dump (FILE *f) const;
private:
  relation_kind related;
  tree name1, name2;
};

// Set relation R between ssa_name N1 and N2.

inline void
value_relation::set_relation (relation_kind r, tree n1, tree n2)
{
  gcc_checking_assert (SSA_NAME_VERSION (n1) != SSA_NAME_VERSION (n2));
  related = r;
  name1 = n1;
  name2 = n2;
}

// Default constructor.

inline
value_relation::value_relation ()
{
  related = VREL_NONE;
  name1 = NULL_TREE;
  name2 = NULL_TREE;
}

// Constructor for relation R between SSA version N1 nd N2.

inline
value_relation::value_relation (relation_kind kind, tree n1, tree n2)
{
  set_relation (kind, n1, n2);
}

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
  relation_kind k = VREL_NONE;

  // Idenity any common operand, and notrmalize the relations to
  // the form : A < B  B < C produces A < C
  if (rel.op1 () == name2)
    {
      // A < B   B < C
      if (rel.op2 () == name1)
	return false;
      k = relation_transitive (kind (), rel.kind ());
      if (k != VREL_NONE)
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
      if (k != VREL_NONE)
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
      if (k != VREL_NONE)
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
      if (k != VREL_NONE)
	{
	  related = k;
	  name1 = name2;
	  name2 = rel.op1 ();
	  return true;
	}
    }
  return false;
}

// Dump the relation to file F.

void
value_relation::dump (FILE *f) const
{
  if (!name1 || !name2)
    {
      fprintf (f, "uninitialized");
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
    return VREL_NONE;

  // If both b1 and b2 aren't referenced in thie block, cant be a relation
  if (!bitmap_intersect_p (m_names, b1) || !bitmap_intersect_p (m_names, b2))
    return VREL_NONE;

  // Search for the fiorst relation that contains BOTH an element from B1
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

  return VREL_NONE;
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
  if (k == VREL_NONE)
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
  if (k == EQ_EXPR && is_a<gphi *> (stmt))
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
  if (k == VREL_NONE || !single_pred_p (e->dest))
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
{  // Equivalencies are handled by the equivalence oracle.
  if (k == EQ_EXPR)
    equiv_oracle::register_relation (bb, k, op1, op2);
  else
    {
      relation_chain *ptr = set_one_relation (bb, k, op1, op2);
      register_transitives (bb, *ptr);
    }
}

// Register relation K between OP! and OP2 in block BB.
// This creates the record and searches for existing records in the dominator
// tree to merge with.

relation_chain *
dom_oracle::set_one_relation (basic_block bb, relation_kind k, tree op1,
			      tree op2)
{
  gcc_checking_assert (k != VREL_NONE && k != EQ_EXPR);

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
  if (curr != VREL_NONE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "    Intersecting with existing ");
	  ptr->dump (dump_file);
	}
      // Check into whether we can simply replace the relation rather than
      // intersecting it.  THis may help with some optimistic iterative
      // updating algorithms.
      ptr->intersect (vr);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " to produce ");
	  ptr->dump (dump_file);
	  fprintf (dump_file, "\n");
	}
    }
  else
    {
      // Check for an existing relation further up the DOM chain.
      // By including dominating relations, The first one found in any search
      // will be the aggregate of all the previous ones.
      curr = find_relation_dom (bb, v1, v2);
      if (curr != VREL_NONE)
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
      case LE_EXPR:
      case LT_EXPR:
      case GT_EXPR:
      case GE_EXPR:
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
	      set_one_relation (root_bb, nr.kind (), nr.op1 (), nr.op2 ());
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
    return VREL_NONE;

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
    return EQ_EXPR;

  // If either name does not occur in a relation anywhere, there isnt one.
  if (!bitmap_intersect_p (m_relation_set, b1)
      || !bitmap_intersect_p (m_relation_set, b2))
    return VREL_NONE;

  // Search each block in the DOM tree checking.
  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      r = find_relation_block (bb->index, b1, b2);
      if (r != VREL_NONE)
	return r;
    }
  return VREL_NONE;

}

// Find a relation in block BB between ssa version V1 and V2.  If a relation
// is found, return a pointer to the chain object in OBJ.

relation_kind
dom_oracle::find_relation_block (int bb, unsigned v1, unsigned v2,
				     relation_chain **obj) const
{
  if (bb >= (int)m_relations.length())
    return VREL_NONE;

  const_bitmap bm = m_relations[bb].m_names;
  if (!bm)
    return VREL_NONE;

  // If both b1 and b2 aren't referenced in thie block, cant be a relation
  if (!bitmap_bit_p (bm, v1) || !bitmap_bit_p (bm, v2))
    return VREL_NONE;

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

  return VREL_NONE;
}

// Find a relation between SSA version V1 and V2 in the dominator tree
// starting with block BB

relation_kind
dom_oracle::find_relation_dom (basic_block bb, unsigned v1, unsigned v2) const
{
  relation_kind r;
  // IF either name does not occur in a relation anywhere, there isnt one.
  if (!bitmap_bit_p (m_relation_set, v1) || !bitmap_bit_p (m_relation_set, v2))
    return VREL_NONE;

  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      r = find_relation_block (bb->index, v1, v2);
      if (r != VREL_NONE)
	return r;
    }
  return VREL_NONE;

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
    return EQ_EXPR;

  // Check for equivalence first.  They must be in each equivalency set.
  const_bitmap equiv1 = equiv_set (ssa1, bb);
  const_bitmap equiv2 = equiv_set (ssa2, bb);
  if (bitmap_bit_p (equiv1, v2) && bitmap_bit_p (equiv2, v1))
    return EQ_EXPR;

  // Initially look for a direct relationship and just return that.
  kind = find_relation_dom (bb, v1, v2);
  if (kind != VREL_NONE)
    return kind;

  // Query using the equiovalence sets.
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
  m_root = oracle;
  bitmap_obstack_initialize (&m_bitmaps);
  obstack_init (&m_chain_obstack);

  // Initialize header records.
  m_equiv.m_names = BITMAP_ALLOC (&m_bitmaps);
  m_equiv.m_bb = NULL;
  m_equiv.m_next = NULL;
  m_relations.m_names = BITMAP_ALLOC (&m_bitmaps);
  m_relations.m_head = NULL;
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

// Register an equivalence between SSA1 and SSA2 resolving unkowns from
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
  bitmap_copy (b, equiv_1);
  bitmap_ior_into (b, equiv_2);

  equiv_chain *ptr = (equiv_chain *) obstack_alloc (&m_chain_obstack,
						    sizeof (equiv_chain));
  ptr->m_names = b;
  ptr->m_bb = NULL;
  ptr->m_next = m_equiv.m_next;
  m_equiv.m_next = ptr;
  bitmap_ior_into (m_equiv.m_names, b);
}

// Register relation K between SSA1 and SSA2, resolving unknowns by
// querying from BB.

void
path_oracle::register_relation (basic_block bb, relation_kind k, tree ssa1,
				tree ssa2)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, ssa1, ssa2);
      fprintf (dump_file, " Registering value_relation (path_oracle) ");
      vr.dump (dump_file);
      fprintf (dump_file, " (bb%d)\n", bb->index);
    }

  if (k == EQ_EXPR)
    {
      register_equiv (bb, ssa1, ssa2);
      return;
    }

  relation_kind curr = query_relation (bb, ssa1, ssa2);
  if (curr != VREL_NONE)
    k = relation_intersect (curr, k);

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
    return EQ_EXPR;

  relation_kind k = m_relations.find_relation (b1, b2);

  if (k == VREL_NONE && m_root)
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
    return EQ_EXPR;

  const_bitmap equiv_1 = equiv_set (ssa1, bb);
  const_bitmap equiv_2 = equiv_set (ssa2, bb);
  if (bitmap_bit_p (equiv_1, v2) && bitmap_bit_p (equiv_2, v1))
    return EQ_EXPR;

  return query_relation (bb, equiv_1, equiv_2);
}

// Reset any relations registered on this path.

void
path_oracle::reset_path ()
{
  m_equiv.m_next = NULL;
  bitmap_clear (m_equiv.m_names);
  m_relations.m_head = NULL;
  bitmap_clear (m_relations.m_names);
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
  for (; ptr; ptr = ptr->m_next)
    ptr->dump (f);

  relation_chain *ptr2 = m_relations.m_head;
  for (; ptr2; ptr2 = ptr2->m_next)
    {
      fprintf (f, "Relational : ");
      ptr2->dump (f);
      fprintf (f, "\n");
    }
}
