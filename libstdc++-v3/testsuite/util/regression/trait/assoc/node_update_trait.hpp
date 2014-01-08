// -*- C++ -*-

// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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
 * @file node_update_trait.hpp
 * Contains traits for a random regression test
 *    for a specific container type.
 */

#ifndef PB_DS_REGRESSION_TEST_NODE_UPDATOR_TRAIT_HPP
#define PB_DS_REGRESSION_TEST_NODE_UPDATOR_TRAIT_HPP

#include <ext/pb_ds/assoc_container.hpp>
#include <common_type/assoc/detail/tree_supports_order_statistics.hpp>
#include <common_type/assoc/detail/trie_supports_order_statistics.hpp>
#include <common_type/assoc/detail/trie_supports_prefix_search.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    namespace detail
    {

      template<typename Cntnr, class Tag>
      struct regression_test_node_update_traits
      {
	enum
	  {
	    order_statistics = false,
	    prefix_search = false
	  };
      };

      template<typename Cntnr>
      struct regression_test_node_update_traits<
	Cntnr,
	__gnu_pbds::pat_trie_tag>
      {
	enum
	  {
	    order_statistics =
            trie_supports_order_statistics<Cntnr>::value,
	    prefix_search =
            trie_supports_prefix_search<Cntnr>::value
	  };
      };

      template<typename Cntnr>
      struct regression_test_node_update_traits<
	Cntnr,
	__gnu_pbds::rb_tree_tag>
      {
	enum
	  {
	    order_statistics =
            tree_supports_order_statistics<Cntnr>::value,
	    prefix_search = false
	  };
      };

      template<typename Cntnr>
      struct regression_test_node_update_traits<
	Cntnr,
	__gnu_pbds::splay_tree_tag>
      {
	enum
	  {
	    order_statistics =
            tree_supports_order_statistics<Cntnr>::value,
	    prefix_search = false
	  };
      };

      template<typename Cntnr>
      struct regression_test_node_update_traits<
	Cntnr,
	__gnu_pbds::ov_tree_tag>
      {
	enum
	  {
	    order_statistics =
            tree_supports_order_statistics<Cntnr>::value,
	    prefix_search = false
	  };
      };

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_REGRESSION_TEST_NODE_UPDATOR_TRAIT_HPP
