// { dg-do run { target c++17 } }
// { dg-require-effective-target std_allocator_new }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }

// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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

// libstdc++/96088

#include <string_view>
#include <string>
#include <unordered_set>

#include <testsuite_hooks.h>
#include <replacement_memory_operators.h>

static constexpr std::initializer_list<const char*> lst =
  { "long_str_for_dynamic_allocation" };

void
test01()
{
  __gnu_test::counter::reset();
  std::unordered_multiset<std::string,
			  std::hash<std::string_view>,
			  std::equal_to<std::string_view>> foo;
  foo.insert(lst.begin(), lst.end());
  VERIFY( foo.size() == 1 );

  VERIFY( __gnu_test::counter::count() == 3 );
  VERIFY( __gnu_test::counter::get()._M_increments == 3 );
}

void
test02()
{
  __gnu_test::counter::reset();
  std::unordered_multiset<std::string> foo;
  foo.insert(lst.begin(), lst.end());
  VERIFY( foo.size() == 1 );

  VERIFY( __gnu_test::counter::count() == 3 );
  VERIFY( __gnu_test::counter::get()._M_increments == 3 );
}

int
main()
{
  __gnu_test::counter::scope s;
  test01();
  test02();
  return 0;
}
