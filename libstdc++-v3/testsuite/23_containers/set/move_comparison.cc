// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <set>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

struct Cmp
{
  Cmp() = default;
  Cmp(const Cmp&) = default;
  Cmp(Cmp&&) = default;
  Cmp& operator=(const Cmp&) = default;
  Cmp& operator=(Cmp&& c) { c.moved_from = true; return *this; }

  bool moved_from = false;

  bool operator()(int l, int r) const
  {
    VERIFY(!moved_from);
    return l < r;
  }
};

void
test01()
{
  using allocator = __gnu_test::uneq_allocator<int>;
  using test_type = std::set<int, Cmp, allocator>;
  test_type s1(allocator(1)), s2(allocator(2));
  s1.insert(1);
  s1.insert(2);
  s1.insert(3);
  s2 = std::move(s1);
  VERIFY(s1.key_comp().moved_from);
}

int
main()
{
  test01();
}
