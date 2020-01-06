// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
// 2013-03-02  Ulrich Drepper <drepper@gmail.com>
//
// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  __gnu_cxx::von_mises_distribution<> u(1.5, 3.0);
  VERIFY( u.mu() == 1.5 );
  VERIFY( u.kappa() == 3.0 );
  VERIFY( u.min() == -__gnu_cxx::__math_constants<double>::__pi );
  VERIFY( u.max() == __gnu_cxx::__math_constants<double>::__pi );
}

void
test02()
{
  using param_type = __gnu_cxx::von_mises_distribution<>::param_type;
  const param_type p(1.5, 3.0);
  __gnu_cxx::von_mises_distribution<> u(p);
  VERIFY( u.param() == p );
  VERIFY( u.param() != param_type{} );
  VERIFY( u.min() == -__gnu_cxx::__math_constants<double>::__pi );
  VERIFY( u.max() == __gnu_cxx::__math_constants<double>::__pi );
}

int main()
{
  test01();
  test02();
}
