// { dg-do compile }
//
// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006-2023 Free Software Foundation, Inc.
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

template<typename T, typename U> struct require_same; // not defined
template<typename T> struct require_same<T, T> { };

typedef std::tr1::linear_congruential<unsigned long, 16807, 0, 2147483647> E;
typedef std::tr1::uniform_int<int> D;

void
test01()
{
  typedef std::tr1::variate_generator<E, D> test_type;

  typedef test_type::engine_type       engine_type;
  typedef test_type::engine_value_type engine_value_type;
  typedef test_type::distribution_type distribution_type;
  typedef test_type::result_type       result_type;

  require_same<engine_type, E> check_e;
  require_same<engine_value_type, E> check_ev;
  require_same<distribution_type, D> check_d;
  require_same<result_type, typename D::result_type> check_r;
}

void
test02()
{
  typedef std::tr1::variate_generator<E&, D> test_type;

  typedef test_type::engine_type       engine_type;
  typedef test_type::engine_value_type engine_value_type;
  typedef test_type::distribution_type distribution_type;
  typedef test_type::result_type       result_type;

  require_same<engine_type, E&> check_e;
  require_same<engine_value_type, E> check_ev;
  require_same<distribution_type, D> check_d;
  require_same<result_type, typename D::result_type> check_r;
}

void
test03()
{
  typedef std::tr1::variate_generator<E*, D> test_type;

  typedef test_type::engine_type       engine_type;
  typedef test_type::engine_value_type engine_value_type;
  typedef test_type::distribution_type distribution_type;
  typedef test_type::result_type       result_type;

  require_same<engine_type, E*> check_e;
  require_same<engine_value_type, E> check_ev;
  require_same<distribution_type, D> check_d;
  require_same<result_type, typename D::result_type> check_r;
}
