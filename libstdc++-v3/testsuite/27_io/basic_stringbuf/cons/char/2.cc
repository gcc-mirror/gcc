// Copyright (C) 2020 Free Software Foundation, Inc.
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

// 27.7.1.1  basic_stringbuf constructors  [lib.stringbuf.cons]

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-effective-target cxx11-abi }

#include <sstream>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01()
{
  {
    using alloc_type = __gnu_test::uneq_allocator<char>;
    using sbuf_t = std::basic_stringbuf<char, std::char_traits<char>,
						alloc_type>;

    alloc_type aa;
    sbuf_t sbuf1(aa);
    VERIFY( aa == sbuf1.get_allocator() );

    alloc_type aaa(42);
    sbuf_t sbuf2(aaa);
    VERIFY( aaa == sbuf2.get_allocator() );

    VERIFY( sbuf1.get_allocator() != sbuf2.get_allocator() );
  }

  std::stringbuf::allocator_type a;
  {
    std::stringbuf sbuf(std::ios_base::in, a);
  }

  {
    std::stringbuf sbuf(a);
  }
}

auto const cstr = "This is a test";

void
test02()
{
  std::string s1(cstr);
  std::stringbuf sbuf(std::move(s1));
  VERIFY( s1.empty() );

  std::string s2(cstr);
  VERIFY( sbuf.str() == s2 );
}

void
test03()
{
  using alloc_type = __gnu_test::tracker_allocator<char>;
  using str_type = std::basic_string<char, std::char_traits<char>, alloc_type>;

  auto const mode = std::ios_base::in | std::ios_base::out;
  str_type s1(cstr);

  {
    std::stringbuf::allocator_type a;
    std::stringbuf sbuf(s1, mode, a);
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }

  {
    std::stringbuf sbuf(s1, mode);
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }

  {
    std::stringbuf sbuf(s1);
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }
}

void
test04()
{
  std::stringbuf sbuf1(cstr);

  std::stringbuf::allocator_type a;
  std::stringbuf sbuf2(std::move(sbuf1), a);
  VERIFY( sbuf1.str().empty() );

  std::string s(cstr);
  VERIFY( sbuf2.str() == s );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
