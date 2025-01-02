// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#include <queue>
#include <deque>
#include <vector>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::priority_queue<unsigned> s0;

  std::priority_queue s1 = s0;
  check_type<std::priority_queue<unsigned>>(s1);

  std::priority_queue s2 = std::move(s0);
  check_type<std::priority_queue<unsigned>>(s2);

  const std::priority_queue s3 = s0;
  check_type<const std::priority_queue<unsigned>>(s3);

  const std::priority_queue s4 = s3;
  check_type<const std::priority_queue<unsigned>>(s4);

  std::allocator<unsigned> a;
  std::priority_queue s5(s0, a);
  check_type<std::priority_queue<unsigned>>(s5);

  std::priority_queue s6(std::move(s0), a);
  check_type<std::priority_queue<unsigned>>(s6);

  const std::priority_queue s7(s3, a);
  check_type<const std::priority_queue<unsigned>>(s7);
}

template<typename T>
  using input_iterator_seq
    = __gnu_test::test_container<T, __gnu_test::input_iterator_wrapper>;

void
test02()
{
  using Deque = std::deque<int>;
  Deque d;
  using Vector = std::vector<short>;
  Vector v;
  using Cmp = std::greater<long>;
  Cmp cmp;

  std::priority_queue s1(cmp, d);
  check_type<std::priority_queue<int, Deque, Cmp>>(s1);

  std::priority_queue s2(cmp, d, d.get_allocator());
  check_type<std::priority_queue<int, Deque, Cmp>>(s2);

  std::priority_queue s3(cmp, std::move(d));
  check_type<std::priority_queue<int, Deque, Cmp>>(s3);

  std::priority_queue s4(cmp, std::move(d), d.get_allocator());
  check_type<std::priority_queue<int, Deque, Cmp>>(s4);

  std::priority_queue s5(cmp, v);
  check_type<std::priority_queue<short, Vector, Cmp>>(s5);

  std::priority_queue s6(cmp, v, v.get_allocator());
  check_type<std::priority_queue<short, Vector, Cmp>>(s6);

  std::priority_queue s7(cmp, std::move(v));
  check_type<std::priority_queue<short, Vector, Cmp>>(s7);

  std::priority_queue s8(cmp, std::move(v), v.get_allocator());
  check_type<std::priority_queue<short, Vector, Cmp>>(s8);

  short a[1] = {};
  input_iterator_seq<short> seq(a);

  std::priority_queue s9(seq.begin(), seq.end());
  check_type<std::priority_queue<short>>(s9);

  std::priority_queue s10(seq.begin(), seq.end(), {});
  check_type<std::priority_queue<short>>(s10);

  std::priority_queue s11(seq.begin(), seq.end(), {}, {});
  check_type<std::priority_queue<short>>(s11);

  std::priority_queue s12(seq.begin(), seq.end(), cmp);
  check_type<std::priority_queue<short, Vector, Cmp>>(s12);

  std::priority_queue s13(seq.begin(), seq.end(), cmp, {});
  check_type<std::priority_queue<short, Vector, Cmp>>(s13);

  std::priority_queue s14(seq.begin(), seq.end(), cmp, std::deque<short>{});
  check_type<std::priority_queue<short, std::deque<short>, Cmp>>(s14);
}

struct Pool;

template<typename T>
struct Alloc : __gnu_test::SimpleAllocator<T>
{
  Alloc(Pool*) { }

  template<typename U>
    Alloc(const Alloc<U>&) { }
};

void
test_p1518r2()
{
  // P1518R2 - Stop overconstraining allocators in container deduction guides.
  // This is a C++23 feature but we support it for C++17 too.

  using Vector = std::vector<short, Alloc<short>>;
  using Cmp = std::greater<long>;
  Pool* p = nullptr;
  Vector v(p);
  Cmp cmp;

  std::priority_queue q1(cmp, v, p);
  check_type<std::priority_queue<short, Vector, Cmp>>(q1);

  std::priority_queue q2(cmp, std::move(v), p);
  check_type<std::priority_queue<short, Vector, Cmp>>(q2);

  std::priority_queue q3(q1, p);
  check_type<std::priority_queue<short, Vector, Cmp>>(q3);

  std::priority_queue q4(std::move(q1), p);
  check_type<std::priority_queue<short, Vector, Cmp>>(q4);
}
