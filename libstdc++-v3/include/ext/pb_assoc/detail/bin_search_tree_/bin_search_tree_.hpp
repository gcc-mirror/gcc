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

/*
 * @file bin_search_tree_.hpp
 * Contains an implementation class for bin_search_tree_.
 */
/*
 * This implementation uses an idea from the SGI STL (using a "header" node
 *	which is needed for efficient iteration).
 */

#include <ext/pb_assoc/exception.hpp>
#include <ext/pb_assoc/detail/eq_fn/eq_by_less.hpp>
#include <ext/pb_assoc/detail/types_traits.hpp>
#include <ext/pb_assoc/detail/map_debug_base.hpp>
#include <ext/pb_assoc/tree_policy.hpp>
#include <ext/pb_assoc/detail/cond_dealtor.hpp>
#include <ext/pb_assoc/detail/type_utils.hpp>
#include <utility>
#include <functional>
#include <assert.h>

namespace pb_assoc
{

  namespace detail
  {

#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

#define PB_ASSOC_CLASS_T_DEC \
	template< \
		typename Key, \
		typename Data, \
		class Node, \
		class Cmp_Fn, \
		class Allocator, \
		class Node_Updator>

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	bin_search_tree_data_
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
#define PB_ASSOC_CLASS_NAME \
	bin_search_tree_no_data_
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATOR

#define PB_ASSOC_CLASS_C_DEC \
	PB_ASSOC_CLASS_NAME< \
		Key, \
		Data, \
		Node, \
		Cmp_Fn, \
		Allocator, \
		Node_Updator>

#define PB_ASSOC_TYPES_TRAITS_C_DEC \
	pb_assoc::detail::types_traits< \
		Key, \
		Data, \
		Allocator>

#ifdef PB_ASSOC_USE_MAP_DEBUG_BASE
#define PB_ASSOC_MAP_DEBUG_BASE_C_DEC \
	pb_assoc::detail::map_debug_base< \
		Key, \
		eq_by_less<Key, Cmp_Fn> >
#endif // #ifdef PB_ASSOC_USE_MAP_DEBUG_BASE

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
	     class Node,
	     class Cmp_Fn,
	     class Allocator,
	     class Node_Updator>
    class PB_ASSOC_CLASS_NAME :
#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
      protected PB_ASSOC_MAP_DEBUG_BASE_C_DEC,
#endif // #ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
      protected Cmp_Fn,
      public PB_ASSOC_TYPES_TRAITS_C_DEC,
      public Node_Updator
    {

    protected:

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

      typedef PB_ASSOC_TYPES_TRAITS_C_DEC my_traits_base;

      typedef Node node;

      typedef
      pb_assoc::detail::cond_dealtor<
	node,
	Allocator>
      cond_dealtor_t;

      typedef
      typename Allocator::template rebind<node>::other
      node_allocator;

      typedef typename node_allocator::value_type node_type;

      typedef typename node_allocator::pointer node_pointer;

      typedef value_type mapped_value_type;

      typedef reference mapped_reference;

      typedef const_reference const_mapped_reference;

      typedef pointer mapped_pointer;

      typedef const_pointer const_mapped_pointer;

#include <ext/pb_assoc/detail/bin_search_tree_/find_iterators.hpp>

      typedef const_it_<true> const_find_iterator;

      typedef it_<true> find_iterator;

      typedef const_find_iterator const_iterator;

      typedef find_iterator iterator;

      typedef const_it_<false> const_reverse_iterator;

      typedef it_<false> reverse_iterator;

#include <ext/pb_assoc/detail/bin_search_tree_/node_iterators.hpp>

      typedef const_node_it_ const_node_iterator;

      typedef node_it_ node_iterator;

      typedef Cmp_Fn cmp_fn;

      typedef Allocator allocator;

    private:

#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
      typedef PB_ASSOC_MAP_DEBUG_BASE_C_DEC my_map_debug_base;
#endif // #ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

    protected:

      PB_ASSOC_CLASS_NAME();

      PB_ASSOC_CLASS_NAME(const Cmp_Fn& r_cmp_fn);

      PB_ASSOC_CLASS_NAME(const Cmp_Fn& r_cmp_fn, const Node_Updator& r_updator);

      PB_ASSOC_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other);

      void
      swap(PB_ASSOC_CLASS_C_DEC& r_other);

      ~PB_ASSOC_CLASS_NAME();

      void
      initialize_min_max();

      template<class Other_Map_Type>
      bool
      cmp_with_other(const Other_Map_Type& r_other) const;

      inline bool
      empty() const;

      inline size_type
      size() const;

      inline size_type
      max_size() const;

      Cmp_Fn& 
      get_cmp_fn();

      const Cmp_Fn& 
      get_cmp_fn() const;

      inline std::pair<find_iterator, bool>
      insert_leaf(const_reference r_value);

      inline find_iterator
      lower_bound(const_key_reference r_key);

      inline const_find_iterator
      lower_bound(const_key_reference r_key) const;

      inline find_iterator
      upper_bound(const_key_reference r_key);

      inline const_find_iterator
      upper_bound(const_key_reference r_key) const;

      inline find_iterator
      find(const_key_reference r_key);

      inline const_find_iterator
      find(const_key_reference r_key) const;

      inline void
      update_min_max_for_erased_node(node_pointer p_nd);

      inline void
      actual_erase_node(node_pointer p_nd);

      void
      clear();

      inline void
      rotate_left(node_pointer p_x);

      inline void
      rotate_right(node_pointer p_y);

      inline void
      rotate_parent(node_pointer p_nd);

      inline void
      apply_update(node_pointer p_nd, pb_assoc::null_node_updator* );

      template<class Node_Updator_>
      inline void
      apply_update(node_pointer p_nd, Node_Updator_* p_updator);

      template<class Node_Updator_>
      inline void
      update_to_top(node_pointer p_nd, Node_Updator_* p_updator);

      inline void
      update_to_top(node_pointer p_nd, pb_assoc::null_node_updator* );

      inline iterator
      begin();

      inline const_iterator
      begin() const;

      inline iterator
      find_end();

      inline const_iterator
      find_end() const;

      inline iterator
      end();

      inline const_iterator
      end() const;

      inline reverse_iterator
      rbegin()
      {
	return (reverse_iterator(m_p_head->m_p_right));
      }

      inline const_reverse_iterator
      rbegin() const;

      inline reverse_iterator
      find_rend();

      inline const_reverse_iterator
      find_rend() const;

      inline reverse_iterator
      rend();

      inline const_reverse_iterator
      rend() const;

      bool
      join_prep(PB_ASSOC_CLASS_C_DEC& r_other);

      void
      join_finish(PB_ASSOC_CLASS_C_DEC& r_other);

      bool
      split_prep(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other);

      void
      split_finish(PB_ASSOC_CLASS_C_DEC& r_other);

      size_type
      recursive_count(node_pointer p_nd) const;

      inline const_node_iterator
      node_begin() const;

      inline node_iterator
      node_begin();

      inline const_node_iterator
      node_end() const;

      inline node_iterator
      node_end();

    private:

      inline std::pair<node_pointer, bool>
      erase(node_pointer p_nd);

#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

      void
      assert_valid(bool check_iterators, bool check_metadata) const;

      std::pair<const_pointer, const_pointer>
      assert_node_consistent(const node_pointer p_nd) const
      {
	if (p_nd == NULL)
	  return (std::make_pair((const_pointer)NULL,(const_pointer)NULL));

	assert_node_consistent_with_left(p_nd);
	assert_node_consistent_with_right(p_nd);

	const std::pair<const_pointer, const_pointer>
	  l_range =
	  assert_node_consistent(p_nd->m_p_left);

	if (l_range.second != NULL)
	  PB_ASSOC_DBG_ASSERT(Cmp_Fn::operator()(
						 PB_ASSOC_V2F(*l_range.second),
						 PB_ASSOC_V2F(p_nd->m_value)));

	const std::pair<const_pointer, const_pointer>
	  r_range =
	  assert_node_consistent(p_nd->m_p_right);

	if (r_range.first != NULL)
	  PB_ASSOC_DBG_ASSERT(Cmp_Fn::operator()(
						 PB_ASSOC_V2F(p_nd->m_value),
						 PB_ASSOC_V2F(*r_range.first)));

	return (std::make_pair((l_range.first != NULL)? l_range.first :& p_nd->m_value,(r_range.second != NULL)? r_range.second :& p_nd->m_value));
      }

      void
      assert_consistent_with_debug_base() const;

      void
      assert_node_consistent_with_left(const node_pointer p_nd) const;

      void
      assert_node_consistent_with_right(const node_pointer p_nd) const;

      void
      assert_consistent_with_debug_base(const node_pointer p_nd) const;

      void
      assert_min() const;

      void
      assert_min_imp(const node_pointer p_nd) const;

      void
      assert_max() const;

      void
      assert_max_imp(const node_pointer p_nd) const;

      void
      assert_iterators() const;

      void
      assert_size() const;

#endif // #ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

      void
      initialize();

      node_pointer
      recursive_copy_node(const node_pointer p_nd);

      inline node_pointer
      get_new_node_for_leaf_insert(const_reference r_val, pb_assoc::detail::int_to_type<false>);

      inline node_pointer
      get_new_node_for_leaf_insert(const_reference r_val, pb_assoc::detail::int_to_type<true>);

      inline iterator
      insert_imp_empty(const_reference r_value);

      inline iterator
      insert_leaf_new(const_reference r_value, node_pointer p_nd, bool left_nd);

      static void
      clear_imp(node_pointer p_nd);

    protected:
      node_pointer m_p_head;

      iterator m_end_it;

      reverse_iterator m_rend_it;

      size_type m_size;

      static node_allocator s_node_allocator;
    };

#include <ext/pb_assoc/detail/bin_search_tree_/constructors_destructor_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/iterators_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/debug_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/insert_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/erase_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/find_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/info_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/split_join_fn_imps.hpp>
#include <ext/pb_assoc/detail/bin_search_tree_/rotate_fn_imps.hpp>

#undef PB_ASSOC_CLASS_C_DEC

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_NAME

#undef PB_ASSOC_TYPES_TRAITS_C_DEC

#undef PB_ASSOC_MAP_DEBUG_BASE_C_DEC

#undef PB_ASSOC_V2F
#undef PB_ASSOC_EP2VP
#undef PB_ASSOC_V2S

#undef PB_ASSOC_DBG_ASSERT
#undef PB_ASSOC_DBG_VERIFY
#undef PB_ASSOC_DBG_ONLY

  } // namespace detail

} // namespace pb_assoc
