// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file splay_tree_.hpp
 * Contains an implementation class for splay_tree_.
 */
/*
 * This implementation uses an idea from the SGI STL (using a "header" node
 *    which is needed for efficient iteration). Following is the SGI STL
 *    copyright.
 *
 * Copyright (c) 1996,1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.    Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.    It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.    Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.    It is provided "as is" without express or implied warranty.
 *
 *
 */

#ifdef PB_DS_DATA_TRUE_INDICATOR
#ifndef PB_DS_BIN_SEARCH_TREE_HPP__DATA_TRUE_INDICATOR
#define PB_DS_BIN_SEARCH_TREE_HPP__DATA_TRUE_INDICATOR
#include <ext/pb_ds/detail/bin_search_tree_/bin_search_tree_.hpp>
#endif // #ifndef BIN_SEARCH_TREE_HPP__DATA_TRUE_INDICATOR
#endif // #ifdef PB_DS_DATA_TRUE_INDICATOR

#ifdef PB_DS_DATA_FALSE_INDICATOR
#ifndef PB_DS_BIN_SEARCH_TREE_HPP__DATA_FALSE_INDICATOR
#define PB_DS_BIN_SEARCH_TREE_HPP__DATA_FALSE_INDICATOR
#include <ext/pb_ds/detail/bin_search_tree_/bin_search_tree_.hpp>
#endif // #ifndef BIN_SEARCH_TREE_HPP__DATA_FALSE_INDICATOR
#endif // #ifdef PB_DS_DATA_FALSE_INDICATOR

#include <utility>
#include <vector>
#include <assert.h>

namespace pb_ds
{
  namespace detail
  {

#ifdef PB_DS_SPLAY_TREE_DEBUG_
#define PB_DS_DBG_ASSERT(X) assert(X)
#define PB_DS_DBG_VERIFY(X) assert(X)
#define PB_DS_DBG_ONLY(X) X
#else // #ifdef PB_DS_SPLAY_TREE_DEBUG_
#define PB_DS_DBG_ASSERT(X)
#define PB_DS_DBG_VERIFY(X) {if((X)==0);}
#define PB_DS_DBG_ONLY(X) ;
#endif // #ifdef PB_DS_SPLAY_TREE_DEBUG_

#define PB_DS_CLASS_T_DEC						\
    template<								\
						typename Key,		\
						typename Mapped,	\
						class Cmp_Fn,		\
						class Node_And_It_Traits, \
						class Allocator>

#ifdef PB_DS_DATA_TRUE_INDICATOR
#define PB_DS_CLASS_NAME			\
    splay_tree_data_
#endif // #ifdef PB_DS_DATA_TRUE_INDICATOR

#ifdef PB_DS_DATA_FALSE_INDICATOR
#define PB_DS_CLASS_NAME			\
    splay_tree_no_data_
#endif // #ifdef PB_DS_DATA_FALSE_INDICATOR

#ifdef PB_DS_DATA_TRUE_INDICATOR
#define PB_DS_BASE_CLASS_NAME			\
    bin_search_tree_data_
#endif // #ifdef PB_DS_DATA_TRUE_INDICATOR

#ifdef PB_DS_DATA_FALSE_INDICATOR
#define PB_DS_BASE_CLASS_NAME			\
    bin_search_tree_no_data_
#endif // #ifdef PB_DS_DATA_FALSE_INDICATOR

#define PB_DS_CLASS_C_DEC						\
    PB_DS_CLASS_NAME<							\
						Key,			\
						Mapped,			\
						Cmp_Fn,			\
						Node_And_It_Traits,	\
						Allocator>

#define PB_DS_BASE_C_DEC						\
    PB_DS_BASE_CLASS_NAME<						\
							Key,		\
							Mapped,		\
							Cmp_Fn,		\
							Node_And_It_Traits, \
							Allocator>

#ifdef PB_DS_DATA_TRUE_INDICATOR
#define PB_DS_V2F(X) (X).first
#define PB_DS_V2S(X) (X).second
#define PB_DS_EP2VP(X)& ((X)->m_value)
#endif // #ifdef PB_DS_DATA_TRUE_INDICATOR

#ifdef PB_DS_DATA_FALSE_INDICATOR
#define PB_DS_V2F(X) (X)
#define PB_DS_V2S(X) Mapped_Data()
#define PB_DS_EP2VP(X)& ((X)->m_value.first)
#endif // #ifdef PB_DS_DATA_FALSE_INDICATOR

    // $p14y 7r33 7481.
    template<typename Key,
	     typename Mapped,
	     class Cmp_Fn,
	     class Node_And_It_Traits,
	     class Allocator>
    class PB_DS_CLASS_NAME : public PB_DS_BASE_C_DEC
    {

    private:
      typedef typename PB_DS_BASE_C_DEC::node_pointer node_pointer;

    public:

      typedef typename Allocator::size_type size_type;

      typedef typename Allocator::difference_type difference_type;

      typedef typename PB_DS_BASE_C_DEC::key_type key_type;

      typedef typename PB_DS_BASE_C_DEC::key_pointer key_pointer;

      typedef typename PB_DS_BASE_C_DEC::const_key_pointer const_key_pointer;

      typedef typename PB_DS_BASE_C_DEC::key_reference key_reference;

      typedef
      typename PB_DS_BASE_C_DEC::const_key_reference
      const_key_reference;

      typedef typename PB_DS_BASE_C_DEC::mapped_type mapped_type;

      typedef typename PB_DS_BASE_C_DEC::mapped_pointer mapped_pointer;

      typedef
      typename PB_DS_BASE_C_DEC::const_mapped_pointer
      const_mapped_pointer;

      typedef typename PB_DS_BASE_C_DEC::mapped_reference mapped_reference;

      typedef
      typename PB_DS_BASE_C_DEC::const_mapped_reference
      const_mapped_reference;

      typedef typename PB_DS_BASE_C_DEC::value_type value_type;

      typedef typename PB_DS_BASE_C_DEC::pointer pointer;

      typedef typename PB_DS_BASE_C_DEC::const_pointer const_pointer;

      typedef typename PB_DS_BASE_C_DEC::reference reference;

      typedef typename PB_DS_BASE_C_DEC::const_reference const_reference;

      typedef typename PB_DS_BASE_C_DEC::point_iterator point_iterator;

      typedef typename PB_DS_BASE_C_DEC::const_iterator const_point_iterator;

      typedef typename PB_DS_BASE_C_DEC::iterator iterator;

      typedef typename PB_DS_BASE_C_DEC::const_iterator const_iterator;

      typedef typename PB_DS_BASE_C_DEC::reverse_iterator reverse_iterator;

      typedef
      typename PB_DS_BASE_C_DEC::const_reverse_iterator
      const_reverse_iterator;

      typedef Cmp_Fn cmp_fn;

      typedef Allocator allocator;

      typedef typename PB_DS_BASE_C_DEC::node_update node_update;

    public:

      PB_DS_CLASS_NAME();

      PB_DS_CLASS_NAME(const Cmp_Fn& r_cmp_fn);

      PB_DS_CLASS_NAME(const Cmp_Fn& r_cmp_fn, const node_update& r_node_update);

      PB_DS_CLASS_NAME(const PB_DS_CLASS_C_DEC& other);

      void
      swap(PB_DS_CLASS_C_DEC& other);

      template<typename It>
      void
      copy_from_range(It first_it, It last_it);

      void
      initialize();

      inline std::pair<
	point_iterator,
	bool>
      insert(const_reference r_value);

      inline mapped_reference
      operator[](const_key_reference r_key)
      {
#ifdef PB_DS_DATA_TRUE_INDICATOR
	PB_DS_DBG_ONLY(PB_DS_CLASS_C_DEC::assert_valid();)

	  std::pair<point_iterator, bool> ins_pair =
	  insert_leaf_imp(
			  value_type(
				     r_key,
				     mapped_type()));

	ins_pair.first.m_p_nd->m_special = false;

	PB_DS_DBG_ONLY(PB_DS_BASE_C_DEC::assert_valid());

	splay(ins_pair.first.m_p_nd);

	PB_DS_DBG_ONLY(PB_DS_CLASS_C_DEC::assert_valid();)

	  return (ins_pair.first.m_p_nd->m_value.second);
#else // #ifdef PB_DS_DATA_TRUE_INDICATOR
	insert(r_key);

	return (PB_DS_BASE_C_DEC::s_null_mapped);
#endif // #ifdef PB_DS_DATA_TRUE
      }

      inline point_iterator
      find(const_key_reference r_key);

      inline const_point_iterator
      find(const_key_reference r_key) const;

      inline bool
      erase(const_key_reference r_key);

      inline iterator
      erase(iterator it);

      inline reverse_iterator
      erase(reverse_iterator it);

      template<typename Pred>
      inline size_type
      erase_if(Pred pred);

      void
      join(PB_DS_CLASS_C_DEC& other);

      void
      split(const_key_reference r_key, PB_DS_CLASS_C_DEC& other);

    private:

      inline std::pair<point_iterator, bool>
      insert_leaf_imp(const_reference r_mapped_value);

      inline node_pointer
      find_imp(const_key_reference r_key);

      inline const node_pointer
      find_imp(const_key_reference r_key) const;

#ifdef PB_DS_SPLAY_TREE_DEBUG_

      void
      assert_valid() const;

      void
      assert_special_imp(const node_pointer p_nd) const;

#endif // #ifdef PB_DS_SPLAY_TREE_DEBUG_

      void
      splay(node_pointer p_nd);

      inline void
      splay_zig_zag_left(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent);

      inline void
      splay_zig_zag_right(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent);

      inline void
      splay_zig_zig_left(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent);

      inline void
      splay_zig_zig_right(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent);

      inline void
      splay_zz_start(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent);

      inline void
      splay_zz_end(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent);

      inline node_pointer
      leftmost(node_pointer p_nd);

      void
      erase_node(node_pointer p_nd);

    };

#include <ext/pb_ds/detail/splay_tree_/constructors_destructor_fn_imps.hpp>
#include <ext/pb_ds/detail/splay_tree_/insert_fn_imps.hpp>
#include <ext/pb_ds/detail/splay_tree_/splay_fn_imps.hpp>
#include <ext/pb_ds/detail/splay_tree_/erase_fn_imps.hpp>
#include <ext/pb_ds/detail/splay_tree_/find_fn_imps.hpp>
#include <ext/pb_ds/detail/splay_tree_/debug_fn_imps.hpp>
#include <ext/pb_ds/detail/splay_tree_/split_join_fn_imps.hpp>

#undef PB_DS_CLASS_T_DEC

#undef PB_DS_CLASS_C_DEC

#undef PB_DS_CLASS_NAME

#undef PB_DS_BASE_CLASS_NAME

#undef PB_DS_BASE_C_DEC

#undef PB_DS_V2F
#undef PB_DS_EP2VP
#undef PB_DS_V2S

#undef PB_DS_DBG_ASSERT
#undef PB_DS_DBG_VERIFY
#undef PB_DS_DBG_ONLY

  } // namespace detail
} // namespace pb_ds

