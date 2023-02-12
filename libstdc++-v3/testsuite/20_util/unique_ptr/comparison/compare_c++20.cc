// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <memory>
#include <testsuite_hooks.h>

void
test01()
{
  std::unique_ptr<int> p0, p00;
  VERIFY( p0 == p00 );
  VERIFY( !(p0 < p00) );
  VERIFY( !(p0 > p00) );
  VERIFY( p0 <= p00 );
  VERIFY( p0 >= p00 );
  VERIFY( std::is_eq(p0 <=> p00) );

  std::unique_ptr<int> p1(new int(1));
  VERIFY( p1 == p1 );
  VERIFY( !(p1 < p1) );
  VERIFY( !(p1 > p1) );
  VERIFY( p1 <= p1 );
  VERIFY( p1 >= p1 );
  VERIFY( std::is_eq(p1 <=> p1) );

  std::unique_ptr<const int> p2(new int(1));
  VERIFY( p1 >= p1 );
  VERIFY( p1 != p2 );
  VERIFY( (p1 < p2) || (p1 > p2) );
  VERIFY( (p1 <= p2) || (p1 >= p2) );
  VERIFY( std::is_neq(p1 <=> p2) );

  VERIFY( p1 != p0 );
  VERIFY( !(p1 < p0) );
  VERIFY( p1 > p0 );
  VERIFY( !(p1 <= p0) );
  VERIFY( p1 >= p0 );
  VERIFY( std::is_gt(p1 <=> p0) );
  VERIFY( std::is_lt(p0 <=> p1) );
}

void
test02()
{
  std::unique_ptr<int> p0;
  VERIFY( p0 == nullptr );
  VERIFY( !(p0 < nullptr) );
  VERIFY( !(p0 > nullptr) );
  VERIFY( p0 <= nullptr );
  VERIFY( p0 >= nullptr );
  VERIFY( std::is_eq(p0 <=> nullptr) );

  VERIFY( nullptr == p0 );
  VERIFY( !(nullptr < p0) );
  VERIFY( !(nullptr > p0) );
  VERIFY( nullptr <= p0 );
  VERIFY( nullptr >= p0 );
  VERIFY( std::is_eq(nullptr <=> p0) );

  std::unique_ptr<int> p1(new int(1));
  VERIFY( p1 != nullptr );
  VERIFY( !(p1 < nullptr) );
  VERIFY( p1 > nullptr );
  VERIFY( !(p1 <= nullptr) );
  VERIFY( p1 >= nullptr );
  VERIFY( std::is_gt(p1 <=> nullptr) );

  VERIFY( nullptr != p1 );
  VERIFY( nullptr < p1 );
  VERIFY( !(nullptr > p1) );
  VERIFY( nullptr <= p1 );
  VERIFY( !(nullptr >= p1) );
  VERIFY( std::is_lt(nullptr <=> p1) );
}

int
main()
{
  test01();
  test02();
}
