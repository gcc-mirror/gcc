// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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
  struct constexpr_member_functions
  {
    template<typename _Ttesttype, typename _Tbasetype>
      void
      operator()()
      {
	struct _Concept
	{
	  void __constraint()
	  { 
	    constexpr _Tbasetype v1 __attribute__((unused))
	      = _Ttesttype::min();
	    constexpr _Tbasetype v2 __attribute__((unused))
	      = _Ttesttype::max();
	    constexpr _Tbasetype v3 __attribute__((unused))
	      = _Ttesttype::lowest();
	    constexpr _Tbasetype v4 __attribute__((unused))
	      = _Ttesttype::epsilon();
	    constexpr _Tbasetype v5 __attribute__((unused))
	      = _Ttesttype::round_error();
	    constexpr _Tbasetype v6 __attribute__((unused))
	      = _Ttesttype::infinity();
	    constexpr _Tbasetype v7 __attribute__((unused))
	      = _Ttesttype::quiet_NaN();
	    constexpr _Tbasetype v8 __attribute__((unused))
	      = _Ttesttype::signaling_NaN();
	    constexpr _Tbasetype v9 __attribute__((unused))
	      = _Ttesttype::denorm_min();
	  }
	};

	_Concept c;
	c.__constraint();
      }
  };
}

int main()
{
  __gnu_test::constexpr_member_functions test;
  __gnu_cxx::typelist::apply_generator(test,
				       __gnu_test::limits_tl(),
				       __gnu_test::integral_types_gnu::type());
  return 0;
}
