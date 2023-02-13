// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// 2009-11-10  Benjamin Kosnik  <benjamin@redhat.com>

// Copyright (C) 2009-2023 Free Software Foundation, Inc.
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

#include <string>
#include <debug/string>
#include <exception/safety.h>

void
value()
{
  // NB: Instantiating with __gnu_cxx::throw_value_limit would be illegal,
  // isn't a POD type.
  typedef char value_type;
  typedef __gnu_cxx::throw_allocator_limit<value_type> allocator_type;
  typedef std::char_traits<value_type> traits_type;

  {
    typedef std::basic_string<value_type, traits_type, allocator_type> test_type;
    __gnu_test::basic_safety<test_type> test;
  }

  {
    typedef __gnu_debug::basic_string<value_type, traits_type, allocator_type> test_type;
    __gnu_test::basic_safety<test_type> test;
  }
}

// Container requirement testing, exceptional behavior
int main()
{
  value();
  return 0;
}
