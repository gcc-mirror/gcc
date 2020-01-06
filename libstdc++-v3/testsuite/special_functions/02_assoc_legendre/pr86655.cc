// { dg-do run { target c++11 } }
// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__ -ffp-contract=off" }

// Copyright (C) 2019-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <initializer_list>
#include <cmath>
#if defined(__TEST_DEBUG)
#  include <iostream>
#  define VERIFY(A) \
  if (!(A)) \
    { \
      std::cout << "line " << __LINE__ \
	<< "  std::assoc_legendre(l, m, x) == 0: " << (A) \
	<< '\n'; \
    }
#else
#  include <testsuite_hooks.h>
#endif

template<typename _Tp>
  void
  test_m_gt_l()
  {
    bool test __attribute__((unused)) = true;
    for (auto l : {0u, 1u, 2u, 5u})
      for (auto m : {l + 1u, l + 2u})
	for (auto i : {-2, -1, 0, 1, 2})
	  {
	    auto x = _Tp(i * 0.5L);
	    VERIFY(std::assoc_legendre(l, m, x) == _Tp(0));
	  }
  }

int
main()
{
  test_m_gt_l<float>();
  test_m_gt_l<double>();
  test_m_gt_l<long double>();
}
