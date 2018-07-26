// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// { dg-skip-if "" { *-*-* } { "-fno-exceptions" } }

#include <vector>
#include <ext/throw_allocator.h>
#include <testsuite_hooks.h>

// PR libstdc++/72847
void
test01()
{
  typedef bool value_type;
  typedef __gnu_cxx::throw_allocator_limit<value_type> allocator_type;
  typedef std::vector<value_type, allocator_type> test_type;
  test_type v1(1, false);
  test_type v2(v1.capacity()+1, false);
  allocator_type::set_limit(0);
  try {
    v1 = v2;
  } catch (const __gnu_cxx::forced_error&) {
  }
  // throw_allocator will throw if double-free happens
}

// Container requirement testing, exceptional behavior
int main()
{
  test01();
  return 0;
}
