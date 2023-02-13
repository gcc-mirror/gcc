// -*- C++ -*-

// Copyright (C) 2005-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


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
 * @file common_type.hpp
 * Contains types used for regression tests.
 */

#ifndef PB_DS_RAND_REGRESSION_TEST_COMMON_TYPE_HPP
#define PB_DS_RAND_REGRESSION_TEST_COMMON_TYPE_HPP

#include <vector>
#include <regression/basic_type.hpp>
#include <common_type/assoc/common_type.hpp>
#include <common_type/priority_queue/common_type.hpp>

namespace __gnu_pbds
{
namespace test
{
  typedef __gnu_cxx::throw_allocator_random<basic_type> alloc_type;

  struct hash
  {
    typedef alloc_type::rebind<basic_type>::other basic_type_rebind;
    typedef basic_type_rebind::const_reference const_reference;
    typedef basic_type::const_iterator const_iterator;

    size_t
    operator()(const_reference r_key) const
    {
      size_t ret = 0;
      for (const_iterator it = r_key.begin(); it != r_key.end(); ++it)
	ret = ret * 5 + static_cast<size_t>(*it);
      return ret;
    }
  };

  typedef __gnu_pbds::trie_string_access_traits<basic_type, 'a', 'a' + basic_type::distinct_chars - 1, false, alloc_type> access_traits_t;

  template<typename Data_Type>
  struct tree_types
  {
  private:
    typedef typename tree_common_types<basic_type, Data_Type, std::less<basic_type>, __gnu_pbds::null_node_update, alloc_type>::regression_tl no_order_statistics_tl_t;

    typedef typename tree_common_types<basic_type, Data_Type, std::less<basic_type>, __gnu_pbds::tree_order_statistics_node_update, alloc_type>::regression_tl order_statistics_tl_t;

  public:
    typedef typename __gnu_cxx::typelist::append<no_order_statistics_tl_t, order_statistics_tl_t>::type tl_t;

    typedef no_order_statistics_tl_t min_tl_t;
  };

  template<typename Data_Type>
  struct trie_types
  {
  private:
    typedef typename trie_common_types<basic_type, Data_Type, access_traits_t, __gnu_pbds::pat_trie_tag, __gnu_pbds::null_node_update, alloc_type>::regression_tl no_updates_tl_t;

    typedef typename trie_common_types<basic_type, Data_Type, access_traits_t, __gnu_pbds::pat_trie_tag, __gnu_pbds::trie_order_statistics_node_update, alloc_type>::regression_tl order_statistics_tl_t;

    typedef typename trie_common_types<basic_type, Data_Type, access_traits_t, __gnu_pbds::pat_trie_tag, __gnu_pbds::trie_prefix_search_node_update, alloc_type>::regression_tl prefix_search_tl_t;

  public:
    typedef typename __gnu_cxx::typelist::append<no_updates_tl_t, typename __gnu_cxx::typelist::append<prefix_search_tl_t, order_statistics_tl_t>::type>::type tl_t;

    typedef no_updates_tl_t min_tl_t;
  };

  template<typename Data_Type>
  struct hash_types
  {
    typedef typename hash_common_types<basic_type, Data_Type, hash, std::equal_to<basic_type>, alloc_type>::regression_tl tl_t;

    typedef tl_t min_tl_t;
  };

  template<typename Data_Type>
  struct lu_types
  {
    typedef typename lu_common_types<basic_type, Data_Type, std::equal_to<basic_type>, alloc_type>::regression_tl tl_t;

  typedef tl_t min_tl_t;
  };

  // Sequence types.
  typedef pq_common_types<basic_type, std::less<basic_type>, alloc_type> pq_types;

  typedef pq_types::regression_tl pq_tl_t;
  typedef pq_tl_t min_pq_tl_t;

  template<typename _Tp, typename _Alloc>
  struct vector_adaptor : public std::vector<_Tp, _Alloc>
  {
  private:
    typedef std::vector<_Tp, _Alloc> base_type;

  public:
    typedef typename base_type::value_type		value_type;
    typedef typename base_type::pointer 		pointer;
    typedef typename base_type::const_pointer 		const_pointer;
    typedef typename base_type::reference 		reference;
    typedef typename base_type::const_reference 	const_reference;
    typedef typename base_type::iterator 		iterator;
    typedef typename base_type::const_iterator 		const_iterator;
    typedef typename base_type::reverse_iterator	reverse_iterator;
    typedef typename base_type::const_reverse_iterator 	const_reverse_iterator;
    typedef typename base_type::size_type 		size_type;
    typedef typename base_type::difference_type 	difference_type;
    typedef typename base_type::allocator_type 		allocator_type;

    typedef __gnu_pbds::sequence_tag 			container_category;
    typedef std::less<_Tp> 		   		cmp_fn;

    const cmp_fn&
    get_cmp_fn() const
    { return _M_cmp; }

    vector_adaptor() { }
    vector_adaptor(iterator) { }
    vector_adaptor(iterator, iterator) { }
    vector_adaptor(iterator, iterator, const cmp_fn&) { }
    vector_adaptor(const cmp_fn& other) { }

    using base_type::push_back;
    using base_type::pop_back;

    // erase_if

    cmp_fn _M_cmp;

  };

  typedef vector_adaptor<basic_type, alloc_type>	vector_type;
  typedef __gnu_cxx::typelist::create1<vector_type>::type vector_tl_t;

  // Associative types.
  typedef tree_types<null_type>::tl_t 		tree_set_tl_t;
  typedef tree_types<null_type>::min_tl_t	min_tree_set_tl_t;
  typedef tree_types<basic_type>::tl_t 			tree_map_tl_t;
  typedef tree_types<basic_type>::min_tl_t 		min_tree_map_tl_t;

  typedef hash_types<null_type>::tl_t 		hash_set_tl_t;
  typedef hash_types<null_type>::min_tl_t 	min_hash_set_tl_t;
  typedef hash_types<basic_type>::tl_t 			hash_map_tl_t;
  typedef hash_types<basic_type>::min_tl_t 		min_hash_map_tl_t;

  typedef lu_types<null_type>::tl_t 		lu_set_tl_t;
  typedef lu_types<null_type>::min_tl_t 		min_lu_set_tl_t;
  typedef lu_types<basic_type>::tl_t 			lu_map_tl_t;
  typedef lu_types<basic_type>::min_tl_t 		min_lu_map_tl_t;

  typedef trie_types<null_type>::tl_t 		trie_set_tl_t;
  typedef trie_types<null_type>::min_tl_t 	min_trie_set_tl_t;
  typedef trie_types<basic_type>::tl_t 			trie_map_tl_t;
  typedef trie_types<basic_type>::min_tl_t 		min_trie_map_tl_t;
} // namespace test
} // namespace __gnu_pbds

#endif
