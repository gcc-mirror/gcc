// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

#include <limits>
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
	    using std::float_denorm_style;
	    using std::float_round_style;
	    constexpr bool v1 __attribute__((unused))
	      = _Ttesttype::is_specialized;
	    constexpr int v2 __attribute__((unused))
	      = _Ttesttype::digits;
	    constexpr int v3 __attribute__((unused))
	      = _Ttesttype::digits10;
	    constexpr int v4 __attribute__((unused))
	      = _Ttesttype::max_digits10;
	    constexpr bool v5 __attribute__((unused))
	      = _Ttesttype::is_signed;
	    constexpr bool v6 __attribute__((unused))
	      = _Ttesttype::is_integer;
	    constexpr bool v7 __attribute__((unused))
	      = _Ttesttype::is_exact;
	    constexpr int v8 __attribute__((unused))
	      = _Ttesttype::radix;
	    constexpr int v9 __attribute__((unused))
	      = _Ttesttype::min_exponent;
	    constexpr int v10 __attribute__((unused))
	      = _Ttesttype::min_exponent10;
	    constexpr int v11 __attribute__((unused))
	      = _Ttesttype::max_exponent;
	    constexpr int v12 __attribute__((unused))
	      = _Ttesttype::max_exponent10;
	    constexpr bool v13 __attribute__((unused))
	      = _Ttesttype::has_infinity;
	    constexpr bool v14 __attribute__((unused))
	      = _Ttesttype::has_quiet_NaN;
	    constexpr bool v15 __attribute__((unused))
	      = _Ttesttype::has_signaling_NaN;
	    constexpr float_denorm_style v16 __attribute__((unused))
	      = _Ttesttype::has_denorm;
	    constexpr bool v17 __attribute__((unused))
	      = _Ttesttype::has_denorm_loss;
	    constexpr bool v18 __attribute__((unused))
	      = _Ttesttype::is_iec559;
	    constexpr bool v19 __attribute__((unused))
	      = _Ttesttype::is_bounded;
	    constexpr bool v20 __attribute__((unused))
	      = _Ttesttype::is_modulo;
	    constexpr bool v21 __attribute__((unused))
	      = _Ttesttype::traps;
	    constexpr bool v22 __attribute__((unused))
	      = _Ttesttype::tinyness_before;
	    constexpr float_round_style v23 __attribute__((unused))
	      = _Ttesttype::round_style;
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
  __gnu_cxx::typelist::apply_generator(test, __gnu_test::limits_tl());
  return 0;
}
