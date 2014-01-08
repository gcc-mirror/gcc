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
 * @file common_type.hpp
 * Contains types for a generic multimap_insert_test test.
 */

#ifndef PB_DS_MULTIMAP_RANDOM_INT_INSERT_TEST_COMMON_TYPE_HPP
#define PB_DS_MULTIMAP_RANDOM_INT_INSERT_TEST_COMMON_TYPE_HPP

#include <common_type/assoc/common_type.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    typedef
    __gnu_pbds::test::hash_common_types<
      int,
      __gnu_pbds::null_type>::tl
    hash_set_tl_t;

    template<typename Cntnr_T>
    struct hash_mmap_transform
    {
      typedef
      typename __gnu_pbds::test::hash_common_types<
	int,
	__gnu_pbds::compound_data_type<
	Cntnr_T> >::tl
      type;
    };

    typedef
    __gnu_cxx::typelist::flatten<
      __gnu_cxx::typelist::transform<
      hash_set_tl_t,
      hash_mmap_transform>::type>::type
    hash_mmap_tl_t;

    typedef
    __gnu_pbds::test::tree_common_types<
      int,
      __gnu_pbds::null_type>::tl
    tree_set_tl_t;

    template<typename Cntnr_T>
    struct tree_mmap_transform
    {
      typedef
      typename __gnu_pbds::test::tree_common_types<
	int,
	__gnu_pbds::compound_data_type<
	Cntnr_T> >::tl
      type;
    };

    typedef
    __gnu_cxx::typelist::flatten<
      __gnu_cxx::typelist::transform<
      tree_set_tl_t,
      tree_mmap_transform>::type>::type
    tree_mmap_tl_t;

    typedef hash_mmap_tl_t mmap_tl_t;

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_MULTIMAP_RANDOM_INT_INSERT_TEST_COMMON_TYPE_HPP

