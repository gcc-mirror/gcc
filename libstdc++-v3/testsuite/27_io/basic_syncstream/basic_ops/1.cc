// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-do run { target c++2a } }
// { dg-require-effective-target cxx11-abi }

#include <sstream>
#include <string_view>
#include <syncstream>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01() // construction
{
  {
    std::stringbuf b;
    std::osyncstream s(&b);
    VERIFY( s.rdbuf() != nullptr );
    VERIFY( s.get_wrapped() == &b );
  }

  {
    std::ostringstream stm;
    std::osyncstream s(stm);
    VERIFY( s.get_wrapped() == stm.rdbuf() );
  }

  {
    using alloc_type = __gnu_test::uneq_allocator<char>;
    using sbuf_t = std::basic_syncbuf<char, std::char_traits<char>,
				      alloc_type>;
    using stream_t = std::basic_osyncstream<char, std::char_traits<char>,
					    alloc_type>;
    using str_t = std::basic_ostringstream<char, std::char_traits<char>,
					   alloc_type>;
    sbuf_t b;

    alloc_type aa;
    stream_t s1(&b, aa);
    VERIFY( aa == s1.rdbuf()->get_allocator() );

    alloc_type aaa(42);
    stream_t s2(&b, aaa);
    VERIFY( aaa == s2.rdbuf()->get_allocator() );

    VERIFY( s1.rdbuf()->get_allocator() != s2.rdbuf()->get_allocator() );

    str_t stm;
    stream_t s3(stm, aa);
    VERIFY( s3.get_wrapped() == stm.rdbuf() );
    VERIFY( aa == s1.rdbuf()->get_allocator() );
  }
}

void
test02() // moving
{
  {
    std::stringbuf b;
    std::osyncstream s1(&b);

    std::osyncstream s2(std::move(s1));

    VERIFY( s1.get_wrapped() == nullptr );
    VERIFY( s2.get_wrapped() == &b );
  }

  {
    std::stringbuf b1;
    std::osyncstream s1(&b1);

    std::stringbuf b2;
    std::osyncstream s2(&b2);
    s2 = std::move(s1);

    VERIFY( s1.get_wrapped() == nullptr );
    VERIFY( s2.get_wrapped() == &b1 );
  }
}

void
test03() // swaping
{
  std::stringbuf b1;
  std::osyncstream s1(&b1);

  std::stringbuf b2;
  std::osyncstream s2(&b2);

  std::swap(s1, s2);

  VERIFY( s1.get_wrapped() == &b2 );
  VERIFY( s2.get_wrapped() == &b1 );
}

void
test04() // emitting
{
  {
    std::stringbuf b;
    std::osyncstream s(&b);

    const std::string_view txt("This is a test");
    s << txt;

    s.emit();
    VERIFY( b.str() == txt );
  }

  {
    std::stringbuf b;
    std::osyncstream s(&b);

    s.put('a');
    s.put('b');
    s.put('c');

    s.emit();
    VERIFY( b.str() == "abc" );
  }

  {
    std::stringbuf b;
    std::osyncstream s(&b);

    s << "abc";
    s.put(' ');
    s << "def";
    s.emit();
    VERIFY( b.str() == "abc def" );

    s << "ghi";
    s.put(' ');
    s << "jkl";
    s.emit();
    VERIFY( b.str() == "abc defghi jkl" );
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
