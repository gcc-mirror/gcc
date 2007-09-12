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
 * @file common_type.hpp
 * Contains types used for regression tests.
 */

#ifndef PB_DS_RAND_REGRESSION_TEST_COMMON_TYPE_HPP
#define PB_DS_RAND_REGRESSION_TEST_COMMON_TYPE_HPP

#include <regression/basic_type.hpp>
#include <common_type/assoc/common_type.hpp>

namespace __gnu_pbds
{
namespace test
{
  typedef __gnu_cxx::throw_allocator<basic_type> alloc_type;

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

  typedef __gnu_pbds::string_trie_e_access_traits<basic_type, 'a', 'a' + basic_type::distinct_chars - 1, false, alloc_type> e_access_traits_t;

  template<typename Data_Type>
  struct tree_types
  {
  private:
    typedef typename tree_common_types<basic_type, Data_Type, std::less<basic_type>, __gnu_pbds::null_tree_node_update, alloc_type>::regression_tl no_order_statistics_tl_t;

    typedef typename tree_common_types<basic_type, Data_Type, std::less<basic_type>, __gnu_pbds::tree_order_statistics_node_update, alloc_type>::regression_tl order_statistics_tl_t;

  public:
    typedef typename __gnu_cxx::typelist::append<no_order_statistics_tl_t, order_statistics_tl_t>::type tl_t;

    typedef no_order_statistics_tl_t min_tl_t;
  };

  template<typename Data_Type>
  struct trie_types
  {
  private:
    typedef typename trie_common_types<basic_type, Data_Type, e_access_traits_t, __gnu_pbds::pat_trie_tag, __gnu_pbds::null_trie_node_update, alloc_type>::regression_tl no_updates_tl_t;

    typedef typename trie_common_types<basic_type, Data_Type, e_access_traits_t, __gnu_pbds::pat_trie_tag, __gnu_pbds::trie_order_statistics_node_update, alloc_type>::regression_tl order_statistics_tl_t;

    typedef typename trie_common_types<basic_type, Data_Type, e_access_traits_t, __gnu_pbds::pat_trie_tag, __gnu_pbds::trie_prefix_search_node_update, alloc_type>::regression_tl prefix_search_tl_t;

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

  typedef tree_types<null_mapped_type>::tl_t 		tree_set_tl_t;
  typedef tree_types<null_mapped_type>::min_tl_t	min_tree_set_tl_t;
  typedef tree_types<basic_type>::tl_t 			tree_map_tl_t;
  typedef tree_types<basic_type>::min_tl_t 		min_tree_map_tl_t;

  typedef hash_types<null_mapped_type>::tl_t 		hash_set_tl_t;
  typedef hash_types<null_mapped_type>::min_tl_t 	min_hash_set_tl_t;
  typedef hash_types<basic_type>::tl_t 			hash_map_tl_t;
  typedef hash_types<basic_type>::min_tl_t 		min_hash_map_tl_t;

  typedef lu_types<null_mapped_type>::tl_t 		lu_set_tl_t;
  typedef lu_types<null_mapped_type>::min_tl_t 		min_lu_set_tl_t;
  typedef lu_types<basic_type>::tl_t 			lu_map_tl_t;
  typedef lu_types<basic_type>::min_tl_t 		min_lu_map_tl_t;

  typedef trie_types<null_mapped_type>::tl_t 		trie_set_tl_t;
  typedef trie_types<null_mapped_type>::min_tl_t 	min_trie_set_tl_t;
  typedef trie_types<basic_type>::tl_t 			trie_map_tl_t;
  typedef trie_types<basic_type>::min_tl_t 		min_trie_map_tl_t;
} // namespace test
} // namespace __gnu_pbds

#endif 
