// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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
 * @file get_set_loads_trait.hpp
 * Contains traits for a random regression test
 *    for a specific container type.
 */

#ifndef PB_DS_REGRESSION_TEST_SET_LOADS_TRAIT_HPP
#define PB_DS_REGRESSION_TEST_SET_LOADS_TRAIT_HPP

#include <ext/pb_ds/assoc_container.hpp>

namespace __gnu_pbds
{

  namespace test
  {

    namespace detail
    {

      template<typename Cntnr, class Tag>
      struct regression_test_get_set_loacontainer_traits
      {
	enum
	  {
	    value = false
	  };
      };

      template<typename Cntnr>
      struct regression_test_hash_get_set_loacontainer_traits
      {
	enum
	  {
	    value = Cntnr::resize_policy::trigger_policy::get_set_loads
	  };
      };

      template<typename Cntnr>
      struct regression_test_get_set_loacontainer_traits<
        Cntnr,
        __gnu_pbds::cc_hash_tag> : public regression_test_hash_get_set_loacontainer_traits<
        Cntnr>
      { };

      template<typename Cntnr>
      struct regression_test_get_set_loacontainer_traits<
        Cntnr,
        __gnu_pbds::gp_hash_tag> : public regression_test_hash_get_set_loacontainer_traits<
        Cntnr>
      { };

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_REGRESSION_TEST_SET_LOADS_TRAIT_HPP
