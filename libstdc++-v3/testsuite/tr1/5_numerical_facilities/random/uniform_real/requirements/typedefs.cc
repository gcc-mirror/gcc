// { dg-do compile }
//
// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006-2014 Free Software Foundation, Inc.
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

// 5.1.7.6 Class template uniform_real [tr.rand.dist.runif]
// 5.1.1 [7] Table 17

#include <tr1/random>

void
test01() 
{ 
  using namespace std::tr1;

  typedef uniform_real<double> test_type;

  typedef test_type::input_type  input_type;
  typedef test_type::result_type result_type;
}
