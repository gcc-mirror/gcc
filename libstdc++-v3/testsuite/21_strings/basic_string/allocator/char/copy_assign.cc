// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
// COW strings don't support C++11 allocator propagation:
// { dg-require-effective-target cxx11_abi }

#include <string>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>
#include <ext/throw_allocator.h>

using C = char;
const C c = 'a';
using traits = std::char_traits<C>;

using __gnu_test::propagating_allocator;

void test01()
{
  typedef propagating_allocator<C, false> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;
  test_type v1(alloc_type(1));

  v1.assign(1, c);
  test_type v2(alloc_type(2));
  v2.assign(1, c);
  v2 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(2 == v2.get_allocator().get_personality());

  v1.assign(1, c);
  test_type v3(alloc_type(3));
  v3.assign(100, c);
  v3 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(3 == v3.get_allocator().get_personality());

  v1.assign(100, c);
  test_type v4(alloc_type(4));
  v4.assign(1, c);
  v4 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(4 == v4.get_allocator().get_personality());

  v1.assign(100, c);
  test_type v5(alloc_type(5));
  v5.assign(100, c);
  v5 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(5 == v5.get_allocator().get_personality());
}

void test02()
{
  typedef propagating_allocator<C, true> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;
  test_type v1(alloc_type(1));

  v1.assign(1, c);
  test_type v2(alloc_type(2));
  v2.assign(1, c);
  v2 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(1 == v2.get_allocator().get_personality());

  v1.assign(1, c);
  test_type v3(alloc_type(3));
  v3.assign(100, c);
  v3 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(1 == v3.get_allocator().get_personality());

  v1.assign(100, c);
  test_type v4(alloc_type(4));
  v4.assign(1, c);
  v4 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(1 == v4.get_allocator().get_personality());

  v1.assign(100, c);
  test_type v5(alloc_type(5));
  v5.assign(100, c);
  v5 = v1;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(1 == v5.get_allocator().get_personality());
}

void test03()
{
  // PR libstdc++/79254
  using throw_alloc = __gnu_cxx::throw_allocator_limit<C>;
  typedef propagating_allocator<C, true, throw_alloc> alloc_type;
  typedef std::basic_string<C, traits, alloc_type> test_type;
  alloc_type a1(1), a2(2);
  throw_alloc::set_limit(2); // Throw on third allocation (during assignment).
  const C* s1 = "a string that is longer than a small string";
  const C* s2 = "another string that is longer than a small string";
  test_type v1(s1, a1);
  test_type v2(s2, a2);
  bool caught = false;
  try {
    v1 = v2;
  } catch (__gnu_cxx::forced_error&) {
    caught = true;
  }
  VERIFY( caught );
  VERIFY( v1 == s1 );
  VERIFY( v1.get_allocator() == a1 );

  throw_alloc::set_limit(1); // Allow one more allocation (and no more).
  test_type v3(s1, a1);
  // No allocation when allocators are equal and capacity is sufficient:
  VERIFY( v1.capacity() >= v3.size() );
  v1 = v3;
  // No allocation when the contents fit in the small-string buffer:
  v2 = "sso";
  v1 = v2;
  VERIFY( v1.get_allocator() == a2 );
}

void test04()
{
  // LWG2579
  typedef propagating_allocator<C, true> alloc_type;

  typedef std::basic_string<C, traits, alloc_type> test_type;

  test_type v1("tralalala",alloc_type(1));
  test_type v2("content", alloc_type(2));
  test_type v3("content2", alloc_type(3));

  v1.assign(v2);
  v3 = v2;
  VERIFY(2 == v1.get_allocator().get_personality());
  VERIFY(2 == v3.get_allocator().get_personality());

}

void test05()
{
  // LWG2579
  typedef propagating_allocator<C, false> alloc_type;

  typedef std::basic_string<C, traits, alloc_type> test_type;

  test_type v1("tralalala",alloc_type(1));
  test_type v2("content", alloc_type(2));
  test_type v3("content2", alloc_type(3));

  v1.assign(v2);
  v3 = v2;
  VERIFY(1 == v1.get_allocator().get_personality());
  VERIFY(3 == v3.get_allocator().get_personality());
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return 0;
}
