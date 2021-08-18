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

#ifndef GCC_VALUE_RELATION_H
#define GCC_VALUE_RELATION_H


// This file provides access to a relation oracle which can be used to
// maintain and query relations and equivalences between SSA_NAMES.
//
// The general range_query object provided in value-query.h provides
// access to an oracle, if one is available, via the oracle() method.
// Thre are also a couple of access routines provided, which even if there is
// no oracle, will return the default VREL_NONE no relation.
//
// Typically, when a ranger object is active, there will be an oracle, and
// any information available can be directly queried.  Ranger also sets and
// utilizes the relation information to enhance it's range calculations, this
// is totally transparent to the client, and they are free to make queries.
//
//
// relation_kind is a typedef of enum tree_code, but has restricted range
// and a couple of extra values.
//
// A query is made requesting the relation between SSA1 and SSA@ in a basic
// block, or on an edge, the possible return values are:
//
//  EQ_EXPR, NE_EXPR, LT_EXPR, LE_EXPR, GT_EXPR, and GE_EXPR mean the same.
//  VREL_NONE : No relation between the 2 names.
//  VREL_EMPTY : Impossible relation (ie, A < B && A > B produces VREL_EMPTY.
//
// The oracle maintains EQ_EXPR relations with equivalency sets, so if a
// relation comes back EQ_EXPR, it is also possible to query the set of
// equivlaencies.  These are basically bitmaps over ssa_names.
//
// relations are maintained via the dominace trees, are are optimized assuming
// they are registered in dominance order.   When a new relation is added, it
// is intersected with whatever existing relation exists in the dominance tree
// and registered at the specified block.


// Rather than introduce a new enumerated type for relations, we can use the
// existing tree_codes for relations, plus add a couple of #defines for
// the other cases.  These codes are arranged such that VREL_NONE is the first
// code, and all the rest are contiguous.

typedef enum tree_code relation_kind;

#define VREL_NONE		TRUTH_NOT_EXPR
#define VREL_EMPTY		LTGT_EXPR

// General relation kind transformations.
relation_kind relation_union (relation_kind r1, relation_kind r2);
relation_kind relation_intersect (relation_kind r1, relation_kind r2);
relation_kind relation_negate (relation_kind r);
relation_kind relation_swap (relation_kind r);
void print_relation (FILE *f, relation_kind rel);

// Declared internally in value-relation.cc
class equiv_chain;

// The equivalency oracle maintains equivalencies using the dominator tree.
// Equivalencies apply to an entire basic block.  Equivalencies on edges
// can be represented only on edges whose destination is a single-pred block,
// and the equivalence is simply applied to that succesor block.

class equiv_oracle
{
public:
  equiv_oracle ();
  ~equiv_oracle ();

  const_bitmap equiv_set (tree ssa, basic_block bb) const;
  void register_equiv (basic_block bb, tree ssa1, tree ssa2);

  void dump (FILE *f, basic_block bb) const;
  void dump (FILE *f) const;

protected:
  bitmap_obstack m_bitmaps;
  struct obstack m_chain_obstack;
private:
  bitmap m_equiv_set;	// Index by ssa-name. true if an equivalence exists.
  vec <equiv_chain *> m_equiv;	// Index by BB.  list of equivalences.

  void limit_check (basic_block bb = NULL);
  equiv_chain *find_equiv_block (unsigned ssa, int bb) const;
  equiv_chain *find_equiv_dom (tree name, basic_block bb) const;

  bitmap register_equiv (basic_block bb, unsigned v, equiv_chain *equiv_1);
  bitmap register_equiv (basic_block bb, equiv_chain *equiv_1,
			 equiv_chain *equiv_2);

};

// Summary block header for relations.

class relation_chain_head
{
public:
  bitmap m_names;		// ssa_names with relations in this block.
  class relation_chain *m_head; // List of relations in block.
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

class relation_oracle : public equiv_oracle
{
public:
  relation_oracle ();
  ~relation_oracle ();

  void register_relation (gimple *stmt, relation_kind k, tree op1, tree op2);
  void register_relation (edge e, relation_kind k, tree op1, tree op2);

  relation_kind query_relation (basic_block bb, tree ssa1, tree ssa2);

  void dump (FILE *f, basic_block bb) const;
  void dump (FILE *f) const;
private:
  bitmap m_tmp, m_tmp2;
  bitmap m_relation_set;  // Index by ssa-name. True if a relation exists
  vec <relation_chain_head> m_relations;  // Index by BB, list of relations.
  relation_kind find_relation_block (unsigned bb, const_bitmap b1,
				     const_bitmap b2);
  relation_kind find_relation_dom (basic_block bb, const_bitmap b1,
				   const_bitmap b2);
  relation_kind find_relation_block (int bb, unsigned v1, unsigned v2,
				     relation_chain **obj = NULL);
  relation_kind find_relation_dom (basic_block bb, unsigned v1, unsigned v2);
  void register_relation (basic_block bb, relation_kind k, tree op1, tree op2,
			  bool transitive_p = false);
  void register_transitives (basic_block, const class value_relation &);
  void register_transitives (basic_block, const value_relation &, const_bitmap,
			     const_bitmap);

};

#endif  /* GCC_VALUE_RELATION_H */
