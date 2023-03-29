// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <stack>
#include <deque>
#include <list>
#include <testsuite_allocator.h>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::stack<unsigned> s0;

  std::stack s1 = s0;
  check_type<std::stack<unsigned>>(s1);

  std::stack s2 = std::move(s0);
  check_type<std::stack<unsigned>>(s2);

  const std::stack s3 = s0;
  check_type<const std::stack<unsigned>>(s3);

  const std::stack s4 = s3;
  check_type<const std::stack<unsigned>>(s4);

  std::allocator<unsigned> a;
  std::stack s5(s0, a);
  check_type<std::stack<unsigned>>(s5);

  std::stack s6(std::move(s0), a);
  check_type<std::stack<unsigned>>(s6);

  const std::stack s7(s3, a);
  check_type<const std::stack<unsigned>>(s7);
}

void
test02()
{
  std::deque<unsigned> d;
  std::list<long> l;

  std::stack s1(d);
  check_type<std::stack<unsigned>>(s1);

  std::stack s2(d, d.get_allocator());
  check_type<std::stack<unsigned>>(s2);

  std::stack s3(std::move(d));
  check_type<std::stack<unsigned>>(s3);

  std::stack s4(std::move(d), d.get_allocator());
  check_type<std::stack<unsigned>>(s4);

  std::stack s5(l);
  check_type<std::stack<long, std::list<long>>>(s5);

  std::stack s6(l, l.get_allocator());
  check_type<std::stack<long, std::list<long>>>(s6);

  std::stack s7(std::move(l));
  check_type<std::stack<long, std::list<long>>>(s7);

  std::stack s8(std::move(l), l.get_allocator());
  check_type<std::stack<long, std::list<long>>>(s8);
}

#if __cpp_lib_adaptor_iterator_pair_constructor
void
test03()
{
  std::list<long> l;

  std::stack s1(l.begin(), l.end());
  check_type<std::stack<long>>(s1);

  std::stack s2(l.begin(), l.end(), std::allocator<long>());
  check_type<std::stack<long>>(s2);
}
#endif
