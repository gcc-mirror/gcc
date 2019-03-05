// { dg-do run { target c++11 } }
// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__" }
// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// PR libstdc++/68397 -  std::tr1::expint fails in __expint_En_cont_frac
// for some long double arguments due to low __max_iter value

#include <cmath>
#include <testsuite_hooks.h>

void
test01()
{
  // Answers from Wolfram Alpha.
  long double ans_ok = -0.10001943365331651406888645149537315243646135979573L;
  long double ans_bomb = -0.10777727809650077516264612749163100483995270163783L;

  auto Ei_ok = std::expint(-1.500001L);
  auto diff_ok = Ei_ok - ans_ok;
  VERIFY(std::abs(diff_ok) < 1.0e-15);

  auto Ei_bomb = std::expint(-1.450001L);
  auto diff_bomb = Ei_bomb - ans_bomb;
  VERIFY(std::abs(diff_bomb) < 1.0e-15);
}

int
main()
{
  test01();
  return 0;
}
