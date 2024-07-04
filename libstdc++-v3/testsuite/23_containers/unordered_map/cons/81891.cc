// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct fails_on_copy {
  fails_on_copy() = default;
  fails_on_copy(const fails_on_copy&) { throw 0; };
};

using value_type = std::pair<const int, fails_on_copy>;

void
test01()
{
  value_type p;
  try
  {
    std::unordered_map<int, fails_on_copy> umap(&p, &p + 1);
  }
  catch(...)
  { }
}

void
test02()
{
  using Alloc = __gnu_test::tracker_allocator<value_type>;
  using std::hash;
  using std::equal_to;

  value_type p;
  try
  {
    std::unordered_map<int, fails_on_copy, hash<int>, equal_to<int>, Alloc>
	umap(&p, &p + 1);
  }
  catch(...)
  { }

  using counter = __gnu_test::tracker_allocator_counter;
  VERIFY(counter::get_allocation_count() == counter::get_deallocation_count());
}

int
main()
{
  test01();
  test02();
}
