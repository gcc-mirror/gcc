// { dg-do run }
// { dg-options "-std=c++98 -ffp-contract=off" }

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

#include <tr1/cmath>
#if defined(__TEST_DEBUG)
#  include <iostream>
#  define VERIFY(A) \
  if (!(A)) \
    { \
      std::cout << "line " << __LINE__ \
	<< "  std::sph_legendre(l, m, x) == 0: " << (A) \
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
    unsigned int larr[4] = {0u, 1u, 2u, 5u};
    for (unsigned int l = 0; l < 4; ++l)
      for (unsigned int m = larr[l] + 1u; m <= larr[l] + 2u; ++m)
	for (int i = -2; i <= +2; ++i)
	  {
	    _Tp theta = std::acos(_Tp(i * 0.5L));
	    VERIFY(std::tr1::sph_legendre(larr[l], m, theta) == _Tp(0));
	  }
  }

int
main()
{
  test_m_gt_l<float>();
  test_m_gt_l<double>();
  test_m_gt_l<long double>();
}
