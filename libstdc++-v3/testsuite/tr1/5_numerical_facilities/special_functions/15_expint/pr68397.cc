// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

#include <tr1/cmath>
#include <testsuite_hooks.h>

void
test01()
{
  // Answers from Wolfram Alpha.
  long double ans_ok = -0.10001943365331651406888645149537315243646135979573L;
  long double ans_bomb = -0.10777727809650077516264612749163100483995270163783L;

  long double Ei_ok = std::tr1::expint(-1.500001L);
  long double diff_ok = std::abs(Ei_ok - ans_ok);
  VERIFY(diff_ok < 1.0e-15L);

  long double Ei_bomb = std::tr1::expint(-1.450001L);
  long double diff_bomb = std::abs(Ei_bomb - ans_bomb);
  VERIFY(diff_bomb < 1.0e-15L);
}

int
main()
{
  test01();
  return 0;
}

