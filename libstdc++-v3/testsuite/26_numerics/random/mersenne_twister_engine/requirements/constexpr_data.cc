// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <random>
#include <testsuite_common_types.h>

namespace __gnu_test
{
  struct constexpr_member_data
  {
    template<typename _Ttesttype>
      void
      operator()()
      {
	struct _Concept
	{
	  void __constraint()
	  {
	    constexpr auto v1(_Ttesttype::word_size);
	    constexpr auto v2(_Ttesttype::state_size);
	    constexpr auto v3(_Ttesttype::shift_size);
	    constexpr auto v4(_Ttesttype::mask_bits);
	    constexpr auto v5(_Ttesttype::xor_mask);
	    constexpr auto v6(_Ttesttype::tempering_u);
	    constexpr auto v7(_Ttesttype::tempering_d);
	    constexpr auto v8(_Ttesttype::tempering_s);
	    constexpr auto v9(_Ttesttype::tempering_b);
	    constexpr auto v10(_Ttesttype::tempering_t);
	    constexpr auto v11(_Ttesttype::tempering_c);
	    constexpr auto v12(_Ttesttype::tempering_l);
	    constexpr auto v13(_Ttesttype::initialization_multiplier);
	    constexpr auto v14(_Ttesttype::default_seed);
	  }
	};

	_Concept c;
	c.__constraint();
      }
  };
}

int main()
{
  __gnu_test::constexpr_member_data test;
  typedef std::mt19937 type;
  test.operator()<type>();
  return 0;
}
