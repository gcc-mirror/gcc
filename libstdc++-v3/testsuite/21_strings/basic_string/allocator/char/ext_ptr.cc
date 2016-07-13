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

// { dg-options "-std=gnu++11" }

#include <string>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using C = char;
const C c = 'a';
using traits = std::char_traits<C>;

// basic_string is not required to support fancy pointers:
// http://cplusplus.github.io/LWG/lwg-closed.html#2084

using __gnu_test::CustomPointerAlloc;

void test01()
{
#if _GLIBCXX_USE_CXX11_ABI
  bool test __attribute__((unused)) = true;
  typedef CustomPointerAlloc<C> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;
  test_type v;
  v.assign(1, c);
  VERIFY( ++v.begin() == v.end() );

  v.assign(100, c);
  VERIFY( (v.begin() + 100) == v.end() );
#endif
}

int main()
{
  test01();
}
