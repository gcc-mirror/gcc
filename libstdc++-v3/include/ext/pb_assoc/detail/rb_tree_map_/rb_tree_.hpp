// -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice and
// this permission notice appear in supporting documentation. None of
// the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied warranty.

/**
 * @file rb_tree_.hpp
 * Contains an implementation for rb_tree_.
 */

/*
 * This implementation uses an idea from the SGI STL (using a "header" node
 *	which is needed for efficient iteration).
 */

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#ifndef BIN_SEARCH_TREE_HPP__DATA_TRUE_INDICATOR
#define BIN_SEARCH_TREE_HPP__DATA_TRUE_INDICATOR
#include <ext/pb_assoc/detail/bin_search_tree_/bin_search_tree_.hpp>
#endif // #ifndef BIN_SEARCH_TREE_HPP__DATA_TRUE_INDICATOR
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#ifndef BIN_SEARCH_TREE_HPP__DATA_FALSE_INDICATOR
#define BIN_SEARCH_TREE_HPP__DATA_FALSE_INDICATOR
#include <ext/pb_assoc/detail/bin_search_tree_/bin_search_tree_.hpp>
#endif // #ifndef BIN_SEARCH_TREE_HPP__DATA_FALSE_INDICATOR
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#include <ext/pb_assoc/tree_policy.hpp>
#include <ext/pb_assoc/detail/standard_policies.hpp>
#include <ext/pb_assoc/detail/rb_tree_map_/node.hpp>
#include <utility>
#include <vector>
#include <assert.h>

namespace pb_assoc
{

  namespace detail
  {

#ifdef PB_ASSOC_RB_TREE_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_RB_TREE_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_RB_TREE_DEBUG_

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Cmp_Fn, \
		class Allocator, \
		class Node_Updator>

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	rb_tree_data_
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	rb_tree_no_data_
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_BASE_CLASS_NAME \
	bin_search_tree_data_
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_BASE_CLASS_NAME \
	bin_search_tree_no_data_
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_CLASS_C_DEC \
	PB_ASSOC_CLASS_NAME< \
		Key, \
		Data, \
		Cmp_Fn, \
		Allocator, \
		Node_Updator>

#define PB_ASSOC_TYPES_TRAITS_C_DEC \
	types_traits< \
		Key, \
		Data, \
		Allocator>

#define PB_ASSOC_NODE_C_DEC \
	rb_tree_node_< \
		typename PB_ASSOC_TYPES_TRAITS_C_DEC::value_type, \
		Allocator>

#define PB_ASSOC_BASE_C_DEC \
	PB_ASSOC_BASE_CLASS_NAME< \
		Key, \
		Data, \
		PB_ASSOC_NODE_C_DEC, \
		Cmp_Fn, \
		Allocator, \
		Node_Updator>

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_V2F(X) (X).first
#define PB_ASSOC_V2S(X) (X).second
#define PB_ASSOC_EP2VP(X)& ((X)->m_value)
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_V2F(X) (X)
#define PB_ASSOC_V2S(X) Mapped_Data()
#define PB_ASSOC_EP2VP(X)& ((X)->m_value.first)
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

    template<typename Key,
	     typename Data,
	     class Cmp_Fn,
	     class Allocator,
	     class Node_Updator>
    struct PB_ASSOC_CLASS_NAME : public PB_ASSOC_BASE_C_DEC
    {

    public:

      typedef typename Allocator::size_type size_type;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_key_reference
      const_key_reference;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::data_type data_type;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::data_reference
      data_reference;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_data_reference
      const_data_reference;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::value_type value_type;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::pointer pointer;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_pointer
      const_pointer;

      typedef typename PB_ASSOC_TYPES_TRAITS_C_DEC::reference reference;

      typedef
      typename PB_ASSOC_TYPES_TRAITS_C_DEC::const_reference
      const_reference;

      typedef typename PB_ASSOC_BASE_C_DEC::find_iterator find_iterator;

      typedef
      typename PB_ASSOC_BASE_C_DEC::const_iterator
      const_find_iterator;

      typedef typename PB_ASSOC_BASE_C_DEC::iterator iterator;

      typedef typename PB_ASSOC_BASE_C_DEC::const_iterator const_iterator;

      typedef
      typename PB_ASSOC_BASE_C_DEC::reverse_iterator
      reverse_iterator;

      typedef
      typename PB_ASSOC_BASE_C_DEC::const_reverse_iterator
      const_reverse_iterator;

    private:

      typedef typename PB_ASSOC_BASE_C_DEC::node_pointer node_pointer;

    protected:

      PB_ASSOC_CLASS_NAME();

      PB_ASSOC_CLASS_NAME(const Cmp_Fn& r_cmp_fn);

      PB_ASSOC_CLASS_NAME(const Cmp_Fn& r_cmp_fn, const Node_Updator& r_node_updator);

      PB_ASSOC_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other);

      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      template<class It>
      void
      copy_from_range(It first_it, It last_it);

      inline std::pair<find_iterator, bool>
      insert(const_reference r_value);

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      inline data_reference
      subscript_imp(const_key_reference r_key);
#endif // #ifdef PB_ASSOC_DATA_TRUE

      inline static bool
      is_effectively_black(const node_pointer p_nd);

      inline size_type
      erase(const_key_reference r_key);

      inline const_iterator
      erase(const_iterator it);

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      inline iterator
      erase(iterator it);
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      inline const_reverse_iterator
      erase(const_reverse_iterator it);

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
      inline reverse_iterator
      erase(reverse_iterator it);
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

      template<class Pred>
      inline size_type
      erase_if(Pred pred);

      void
      join(PB_ASSOC_CLASS_C_DEC& r_other);

      void
      split(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other);

#ifdef PB_ASSOC_RB_TREE_DEBUG_

      virtual void
      assert_valid() const;

      size_type
      assert_node_consistent(const node_pointer p_nd) const;

#endif // #ifdef PB_ASSOC_RB_TREE_DEBUG_

    private:

      void
      initialize();

      void
      insert_fixup(node_pointer p_nd);

      void
      erase_node(node_pointer p_nd);

      void
      remove_node(node_pointer p_nd);

      void
      remove_fixup(node_pointer p_x, node_pointer p_new_x_parent);

      void
      split_imp(node_pointer p_nd, PB_ASSOC_CLASS_C_DEC& r_other);

      inline node_pointer
      split_min();

      std::pair<node_pointer, node_pointer>
      split_min_imp();

      void
      join_imp(node_pointer p_x, node_pointer p_r);

      std::pair<node_pointer, node_pointer>
      find_join_pos_right(node_pointer p_l, size_type h_l, size_type h_r);

      std::pair<node_pointer, node_pointer>
      find_join_pos_left(node_pointer p_r, size_type h_l, size_type h_r);

      inline size_type
      black_height(node_pointer p_nd);

      void
      split_at_node(node_pointer p_nd, PB_ASSOC_CLASS_C_DEC& r_other);

    };

#include <ext/pb_assoc/detail/rb_tree_map_/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/rb_tree_map_/insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/rb_tree_map_/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/rb_tree_map_/find_fn_imps.hpp>
#include <ext/pb_assoc/detail/rb_tree_map_/debug_fn_imps.hpp>
#include <ext/pb_assoc/detail/rb_tree_map_/split_join_fn_imps.hpp>
#include <ext/pb_assoc/detail/rb_tree_map_/info_fn_imps.hpp>

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_TYPES_TRAITS_C_DEC

#undef PB_ASSOC_BASE_CLASS_NAME

#undef PB_ASSOC_NODE_C_DEC

#undef PB_ASSOC_BASE_C_DEC

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

#undef PB_ASSOC_V2F
#undef PB_ASSOC_EP2VP
#undef PB_ASSOC_V2S

  } // namespace detail

} // namespace pb_assoc

