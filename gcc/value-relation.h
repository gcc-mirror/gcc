/* Header file for the value range relational processing.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#ifndef GCC_VALUE_RELATION_H
#define GCC_VALUE_RELATION_H


// This file provides access to a relation oracle which can be used to
// maintain and query relations and equivalences between SSA_NAMES.
//
// The general range_query object provided in value-query.h provides
// access to an oracle, if one is available, via the oracle() method.
// There are also a couple of access routines provided, which even if there is
// no oracle, will return the default VREL_VARYING no relation.
//
// Typically, when a ranger object is active, there will be an oracle, and
// any information available can be directly queried.  Ranger also sets and
// utilizes the relation information to enhance it's range calculations, this
// is totally transparent to the client, and they are free to make queries.
//
// relation_kind is a new enum which represents the different relations,
// often with a direct mapping to tree codes. ie VREL_EQ is equivalent to
// EQ_EXPR.
//
// A query is made requesting the relation between SSA1 and SSA@ in a basic
// block, or on an edge, the possible return values are:
//
//  VREL_EQ, VREL_NE, VREL_LT, VREL_LE, VREL_GT, and VREL_GE mean the same.
//  VREL_VARYING : No relation between the 2 names.
//  VREL_UNDEFINED : Impossible relation (ie, A < B && A > B)
//
// The oracle maintains VREL_EQ relations with equivalency sets, so if a
// relation comes back VREL_EQ, it is also possible to query the set of
// equivalencies.  These are basically bitmaps over ssa_names.  An iterator is
// provided later for this activity.
//
// Relations are maintained via the dominance trees and are optimized assuming
// they are registered in dominance order.   When a new relation is added, it
// is intersected with whatever existing relation exists in the dominance tree
// and registered at the specified block.


// These codes are arranged such that VREL_VARYING is the first code, and all
// the rest are contiguous.

typedef enum relation_kind_t
{
  VREL_VARYING = 0,	// No known relation,  AKA varying.
  VREL_UNDEFINED,	// Impossible relation, ie (r1 < r2) && (r2 > r1)
  VREL_LT,		// r1 < r2
  VREL_LE,		// r1 <= r2
  VREL_GT,		// r1 > r2
  VREL_GE,		// r1 >= r2
  VREL_EQ,		// r1 == r2
  VREL_NE,		// r1 != r2
  VREL_PE8,		// 8 bit partial equivalency
  VREL_PE16,		// 16 bit partial equivalency
  VREL_PE32,		// 32 bit partial equivalency
  VREL_PE64,		// 64 bit partial equivalency
  VREL_LAST		// terminate, not a real relation.
} relation_kind;

// General relation kind transformations.
relation_kind relation_union (relation_kind r1, relation_kind r2);
relation_kind relation_intersect (relation_kind r1, relation_kind r2);
relation_kind relation_negate (relation_kind r);
relation_kind relation_swap (relation_kind r);
inline bool relation_lt_le_gt_ge_p (relation_kind r)
		      { return (r >= VREL_LT && r <= VREL_GE); }
inline bool relation_partial_equiv_p (relation_kind r)
		      { return (r >= VREL_PE8 && r <= VREL_PE64); }
inline bool relation_equiv_p (relation_kind r)
		      { return r == VREL_EQ || relation_partial_equiv_p (r); }

void print_relation (FILE *f, relation_kind rel);

class relation_oracle
{
public:
  virtual ~relation_oracle () { }
  // register a relation between 2 ssa names at a stmt.
  void register_stmt (gimple *, relation_kind, tree, tree);
  // register a relation between 2 ssa names on an edge.
  void register_edge (edge, relation_kind, tree, tree);

  // register a relation between 2 ssa names in a basic block.
  virtual void register_relation (basic_block, relation_kind, tree, tree) = 0;
  // Query for a relation between two ssa names in a basic block.
  virtual relation_kind query_relation (basic_block, tree, tree) = 0;

  relation_kind validate_relation (relation_kind, tree, tree);
  relation_kind validate_relation (relation_kind, vrange &, vrange &);

  virtual void dump (FILE *, basic_block) const = 0;
  virtual void dump (FILE *) const = 0;
  void debug () const;
protected:
  friend class equiv_relation_iterator;
  // Return equivalency set for an SSA name in a basic block.
  virtual const_bitmap equiv_set (tree, basic_block) = 0;
  // Return partial equivalency record for an SSA name.
  virtual const class pe_slice *partial_equiv_set (tree) { return NULL; }
  void valid_equivs (bitmap b, const_bitmap equivs, basic_block bb);
  // Query for a relation between two equivalency sets in a basic block.
  virtual relation_kind query_relation (basic_block, const_bitmap,
					const_bitmap) = 0;
  friend class path_oracle;
};

// This class represents an equivalency set, and contains a link to the next
// one in the list to be searched.

class equiv_chain
{
public:
  bitmap m_names;		// ssa-names in equiv set.
  basic_block m_bb;		// Block this belongs to
  equiv_chain *m_next;		// Next in block list.
  void dump (FILE *f) const;	// Show names in this list.
  equiv_chain *find (unsigned ssa);
};

class pe_slice
{
public:
  tree ssa_base;  	// Slice of this name.
  relation_kind code;	// bits that are equivalent.
  bitmap members;	// Other members in the partial equivalency.
};

// The equivalency oracle maintains equivalencies using the dominator tree.
// Equivalencies apply to an entire basic block.  Equivalencies on edges
// can be represented only on edges whose destination is a single-pred block,
// and the equivalence is simply applied to that successor block.

class equiv_oracle : public relation_oracle
{
public:
  equiv_oracle ();
  ~equiv_oracle ();

  const_bitmap equiv_set (tree ssa, basic_block bb) final override;
  const pe_slice *partial_equiv_set (tree name) final override;
  void register_relation (basic_block bb, relation_kind k, tree ssa1,
			  tree ssa2) override;

  void add_partial_equiv (relation_kind, tree, tree);
  relation_kind partial_equiv (tree ssa1, tree ssa2, tree *base = NULL) const;
  relation_kind query_relation (basic_block, tree, tree) override;
  relation_kind query_relation (basic_block, const_bitmap, const_bitmap)
    override;
  void dump (FILE *f, basic_block bb) const override;
  void dump (FILE *f) const override;

protected:
  bitmap_obstack m_bitmaps;
  struct obstack m_chain_obstack;
private:
  bitmap m_equiv_set;	// Index by ssa-name. true if an equivalence exists.
  vec <equiv_chain *> m_equiv;	// Index by BB.  list of equivalences.
  vec <bitmap> m_self_equiv;  // Index by ssa-name, self equivalency set.
  vec <pe_slice> m_partial;  // Partial equivalencies.

  void limit_check (basic_block bb = NULL);
  equiv_chain *find_equiv_block (unsigned ssa, int bb) const;
  equiv_chain *find_equiv_dom (tree name, basic_block bb) const;

  bitmap register_equiv (basic_block bb, unsigned v, equiv_chain *equiv_1);
  bitmap register_equiv (basic_block bb, equiv_chain *equiv_1,
			 equiv_chain *equiv_2);
  void register_initial_def (tree ssa);
  void add_equiv_to_block (basic_block bb, bitmap equiv);
};

// Summary block header for relations.

class relation_chain_head
{
public:
  bitmap m_names;		// ssa_names with relations in this block.
  class relation_chain *m_head; // List of relations in block.
  int m_num_relations;		// Number of relations in block.
  relation_kind find_relation (const_bitmap b1, const_bitmap b2) const;
};

// A relation oracle maintains a set of relations between ssa_names using the
// dominator tree structures.  Equivalencies are considered a subset of
// a general relation and maintained by an equivalence oracle by transparently
// passing any EQ_EXPR relations to it.
// Relations are handled at the basic block level.  All relations apply to
// an entire block, and are thus kept in a summary index by block.
// Similar to the equivalence oracle, edges are handled by applying the
// relation to the destination block of the edge, but ONLY if that block
// has a single successor.  For now.

class dom_oracle : public equiv_oracle
{
public:
  dom_oracle ();
  ~dom_oracle ();

  void register_relation (basic_block bb, relation_kind k, tree op1, tree op2)
    final override;

  relation_kind query_relation (basic_block bb, tree ssa1, tree ssa2)
    final override;
  relation_kind query_relation (basic_block bb, const_bitmap b1,
				const_bitmap b2) final override;

  void dump (FILE *f, basic_block bb) const final override;
  void dump (FILE *f) const final override;
private:
  bitmap m_tmp, m_tmp2;
  bitmap m_relation_set;  // Index by ssa-name. True if a relation exists
  vec <relation_chain_head> m_relations;  // Index by BB, list of relations.
  relation_kind find_relation_block (unsigned bb, const_bitmap b1,
				     const_bitmap b2) const;
  relation_kind find_relation_block (int bb, unsigned v1, unsigned v2,
				     relation_chain **obj = NULL) const;
  relation_kind find_relation_dom (basic_block bb, unsigned v1, unsigned v2) const;
  relation_chain *set_one_relation (basic_block bb, relation_kind k, tree op1,
				    tree op2);
  void register_transitives (basic_block, const class value_relation &);

};

// A path_oracle implements relations in a list.  The only sense of ordering
// is the latest registered relation is the first found during a search.
// It can be constructed with an optional "root" oracle which will be used
// to look up any relations not found in the list.
// This allows the client to walk paths starting at some block and register
// and query relations along that path, ignoring other edges.
//
// For registering a relation, a query if made of the root oracle if there is
// any known relationship at block BB, and it is combined with this new
// relation and entered in the list.
//
// Queries are resolved by looking first in the list, and only if nothing is
// found is the root oracle queried at block BB.
//
// reset_path is used to clear all locally registered paths to initial state.

class path_oracle : public relation_oracle
{
public:
  path_oracle (relation_oracle *oracle = NULL);
  ~path_oracle ();
  const_bitmap equiv_set (tree, basic_block) final override;
  void register_relation (basic_block, relation_kind, tree, tree) final override;
  void killing_def (tree);
  relation_kind query_relation (basic_block, tree, tree) final override;
  relation_kind query_relation (basic_block, const_bitmap, const_bitmap)
    final override;
  void reset_path (relation_oracle *oracle = NULL);
  void set_root_oracle (relation_oracle *oracle) { m_root = oracle; }
  void dump (FILE *, basic_block) const final override;
  void dump (FILE *) const final override;
private:
  void register_equiv (basic_block bb, tree ssa1, tree ssa2);
  equiv_chain m_equiv;
  relation_chain_head m_relations;
  relation_oracle *m_root;
  bitmap m_killed_defs;

  bitmap_obstack m_bitmaps;
  struct obstack m_chain_obstack;
};

// Used to assist with iterating over the equivalence list.
class equiv_relation_iterator {
public:
  equiv_relation_iterator (relation_oracle *oracle, basic_block bb, tree name,
			   bool full = true, bool partial = false);
  void next ();
  tree get_name (relation_kind *rel = NULL);
protected:
  relation_oracle *m_oracle;
  const_bitmap m_bm;
  const pe_slice *m_pe;
  bitmap_iterator m_bi;
  unsigned m_y;
  tree m_name;
};

#define FOR_EACH_EQUIVALENCE(oracle, bb, name, equiv_name)		\
  for (equiv_relation_iterator iter (oracle, bb, name, true, false);	\
       ((equiv_name) = iter.get_name ());				\
       iter.next ())

#define FOR_EACH_PARTIAL_EQUIV(oracle, bb, name, equiv_name, equiv_rel)	\
  for (equiv_relation_iterator iter (oracle, bb, name, false, true);	\
       ((equiv_name) = iter.get_name (&equiv_rel));			\
       iter.next ())

#define FOR_EACH_PARTIAL_AND_FULL_EQUIV(oracle, bb, name, equiv_name, 	\
						      equiv_rel)	\
  for (equiv_relation_iterator iter (oracle, bb, name, true, true);	\
       ((equiv_name) = iter.get_name (&equiv_rel));			\
       iter.next ())

// -----------------------------------------------------------------------

// Range-ops deals with a LHS and 2 operands. A relation trio is a set of
// 3 potential relations packed into a single unsigned value.
//  1 - LHS relation OP1
//  2 - LHS relation OP2
//  3 - OP1 relation OP2
//  VREL_VARYING is a value of 0, and is the default for each position.
class relation_trio
{
public:
  relation_trio ();
  relation_trio (relation_kind lhs_op1, relation_kind lhs_op2,
		 relation_kind op1_op2);
  relation_kind lhs_op1 ();
  relation_kind lhs_op2 ();
  relation_kind op1_op2 ();
  relation_trio swap_op1_op2 ();

  static relation_trio lhs_op1 (relation_kind k);
  static relation_trio lhs_op2 (relation_kind k);
  static relation_trio op1_op2 (relation_kind k);

protected:
  unsigned m_val;
};

//  Default VREL_VARYING for all 3 relations.
#define TRIO_VARYING	relation_trio ()

#define TRIO_SHIFT	4
#define TRIO_MASK	0x000F

// These 3 classes are shortcuts for when a caller has a single relation to
// pass as a trio, it can simply construct the appropriate one.  The other
// unspecified relations will be VREL_VARYING.

inline relation_trio::relation_trio ()
{
  STATIC_ASSERT (VREL_LAST <= (1 << TRIO_SHIFT));
  m_val = 0;
}

inline relation_trio::relation_trio (relation_kind lhs_op1,
				     relation_kind lhs_op2,
				     relation_kind op1_op2)
{
  STATIC_ASSERT (VREL_LAST <= (1 << TRIO_SHIFT));
  unsigned i1 = (unsigned) lhs_op1;
  unsigned i2 = ((unsigned) lhs_op2) << TRIO_SHIFT;
  unsigned i3 = ((unsigned) op1_op2) << (TRIO_SHIFT * 2);
  m_val = i1 | i2 | i3;
}

inline relation_trio
relation_trio::lhs_op1 (relation_kind k)
{
  return relation_trio (k, VREL_VARYING, VREL_VARYING);
}
inline relation_trio
relation_trio::lhs_op2 (relation_kind k)
{
  return relation_trio (VREL_VARYING, k, VREL_VARYING);
}
inline relation_trio
relation_trio::op1_op2 (relation_kind k)
{
  return relation_trio (VREL_VARYING, VREL_VARYING, k);
}

inline relation_kind
relation_trio::lhs_op1 ()
{
  return (relation_kind) (m_val & TRIO_MASK);
}

inline relation_kind
relation_trio::lhs_op2 ()
{
  return (relation_kind) ((m_val >> TRIO_SHIFT) & TRIO_MASK);
}

inline relation_kind
relation_trio::op1_op2 ()
{
  return (relation_kind) ((m_val >> (TRIO_SHIFT * 2)) & TRIO_MASK);
}

inline relation_trio
relation_trio::swap_op1_op2 ()
{
  return relation_trio (lhs_op2 (), lhs_op1 (), relation_swap (op1_op2 ()));
}

// -----------------------------------------------------------------------

// The value-relation class is used to encapsulate the representation of an
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

  relation_trio create_trio (tree lhs, tree op1, tree op2);
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
  gcc_checking_assert (TREE_CODE (n1) == SSA_NAME
		       && TREE_CODE (n2) == SSA_NAME);
  related = r;
  name1 = n1;
  name2 = n2;
}

// Default constructor.

inline
value_relation::value_relation ()
{
  related = VREL_VARYING;
  name1 = NULL_TREE;
  name2 = NULL_TREE;
}

// Constructor for relation R between SSA version N1 and N2.

inline
value_relation::value_relation (relation_kind kind, tree n1, tree n2)
{
  set_relation (kind, n1, n2);
}

// Return the number of bits associated with partial equivalency T.
// Return 0 if this is not a supported partial equivalency relation.

inline int
pe_to_bits (relation_kind t)
{
  switch (t)
  {
    case VREL_PE8:
      return 8;
    case VREL_PE16:
      return 16;
    case VREL_PE32:
      return 32;
    case VREL_PE64:
      return 64;
    default:
      return 0;
  }
}

// Return the partial equivalency code associated with the number of BITS.
// return VREL_VARYING if there is no exact match.

inline relation_kind
bits_to_pe (int bits)
{
  switch (bits)
  {
    case 8:
      return VREL_PE8;
    case 16:
      return VREL_PE16;
    case 32:
      return VREL_PE32;
    case 64:
      return VREL_PE64;
    default:
      return VREL_VARYING;
  }
}

// Given partial equivalencies T1 and T2, return the smallest kind.

inline relation_kind
pe_min (relation_kind t1, relation_kind t2)
{
  gcc_checking_assert (relation_partial_equiv_p (t1));
  gcc_checking_assert (relation_partial_equiv_p (t2));
  // VREL_PE are declared small to large, so simple min will suffice.
  return MIN (t1, t2);
}
#endif  /* GCC_VALUE_RELATION_H */
