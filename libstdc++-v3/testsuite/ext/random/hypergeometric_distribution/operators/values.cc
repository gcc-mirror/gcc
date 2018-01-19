// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
// { dg-require-cmath "" }
//
// 2013-11-18  Edward M. Smith-Rowland  <3dw4rd@verizon.net>
//
// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

// Class template hypergeometric_distribution
// 26.5.1.6 Random number distribution requirements [rand.req.dist]

#include <ext/random>
#include <functional>
#include <testsuite_random.h>

void
test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  __gnu_cxx::hypergeometric_distribution<> hd1{15, 3, 2};
  auto bhd1 = std::bind(hd1, eng);
  testDiscreteDist(bhd1, [](int k)
		   { return hypergeometric_pdf(k, 15, 3, 2); });

  __gnu_cxx::hypergeometric_distribution<> hd2{500, 50, 30};
  auto bhd2 = std::bind(hd2, eng);
  testDiscreteDist(bhd2, [](int k)
		   { return hypergeometric_pdf(k, 500, 50, 30); });

  __gnu_cxx::hypergeometric_distribution<> hd3{100, 20, 5};
  auto bhd3 = std::bind(hd3, eng);
  testDiscreteDist(bhd3, [](int k)
		   { return hypergeometric_pdf(k, 100, 20, 5); });
}

int
main()
{
  test01();
  return 0;
}
