// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-additional-options "-pthread" { target pthread } }
// { dg-do run { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <sstream>
#include <string_view>
#include <syncstream>

#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01() // construction
{
  {
    std::syncbuf s1;
    VERIFY( s1.get_wrapped() == nullptr );

    std::stringbuf b;
    std::syncbuf s2(&b);
    VERIFY( s2.get_wrapped() == &b );
  }

  {
    using alloc_type = __gnu_test::uneq_allocator<char>;
    using sbuf_t = std::basic_syncbuf<char, std::char_traits<char>,
				      alloc_type>;

    sbuf_t b;

    alloc_type aa;
    sbuf_t s1(&b, aa);
    VERIFY( aa == s1.get_allocator() );

    alloc_type aaa(42);
    sbuf_t s2(&b, aaa);
    VERIFY( aaa == s2.get_allocator() );

    VERIFY( s1.get_allocator() != s2.get_allocator() );
  }
}

void
test02() // moving
{
  {
    std::stringbuf b;
    std::syncbuf s1(&b);

    std::syncbuf s2(std::move(s1));

    VERIFY( s1.get_wrapped() == nullptr );
    VERIFY( s2.get_wrapped() == &b );
  }

  {
    std::stringbuf b;
    std::syncbuf s1(&b);

    std::syncbuf s2;
    s2 = std::move(s1);

    VERIFY( s1.get_wrapped() == nullptr );
    VERIFY( s2.get_wrapped() == &b );
  }
}

void
test03() // swaping
{
  std::stringbuf b;
  std::syncbuf s1(&b);

  std::syncbuf s2;
  std::swap(s1, s2);

  VERIFY( s1.get_wrapped() == nullptr );
  VERIFY( s2.get_wrapped() == &b );
}

void
test04() // emitting
{
  {
    std::stringbuf b;
    std::syncbuf s(&b);

    const std::string_view txt("This is a test");
    s.sputn(txt.data(), txt.size());

    VERIFY( b.str() != txt );
    VERIFY( s.pubsync() == 0 );
    VERIFY( b.str() != txt );

    VERIFY( s.emit() );
    VERIFY( b.str() == txt );
  }

  {
    std::stringbuf b;
    std::syncbuf s(&b);
    s.set_emit_on_sync(true);

    const std::string_view txt("This is a test");
    s.sputn(txt.data(), txt.size());

    VERIFY( s.pubsync() == 0 );
    VERIFY( b.str() == txt );
  }
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
