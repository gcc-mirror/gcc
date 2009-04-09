// { dg-do compile }
//
// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006, 2009 Free Software Foundation, Inc.
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

// 5.1.3 class template variate_generator

#include <tr1/random>

void
test01() 
{ 
  using namespace std::tr1;

  typedef variate_generator
    <
    linear_congruential<unsigned long, 16807 , 0 , 2147483647>,
    uniform_int<int>
    > test_type;

  typedef test_type::engine_type       engine_type;
  typedef test_type::engine_value_type engine_value_type;
  typedef test_type::distribution_type distribution_type;
  typedef test_type::result_type       result_type;
}
