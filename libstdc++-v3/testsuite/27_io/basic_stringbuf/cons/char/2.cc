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

// C++20 29.8.2.2  basic_stringbuf constructors  [stringbuf.cons]

// { dg-do run { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <sstream>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

template<typename Alloc, typename C = typename Alloc::value_type>
  using stringbuf_with_alloc
    = std::basic_stringbuf<C, std::char_traits<C>, Alloc>;

void
test01()
{
  // Test C++20 constructors taking an allocator but no string.

  static_assert(!std::is_convertible_v<std::allocator<char>, std::stringbuf>,
      "stringbuf(const allocator<char>&) is explicit");

  {
    using alloc_type = __gnu_test::uneq_allocator<char>;
    using sbuf_t = stringbuf_with_alloc<alloc_type>;

    static_assert(!std::is_convertible_v<const alloc_type&, sbuf_t>,
	"basic_stringbuf(const basic_stringbuf::allocator_type&) is explicit");

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
    VERIFY( sbuf.str().empty() );

    std::stringbuf sbuf2 = {std::ios_base::in, a}; // non-explicit ctor
  }

  {
    std::stringbuf sbuf(a);
    VERIFY( sbuf.str().empty() );
  }
}

auto const cstr = "This is a test string";

void
test02()
{
  // Test C++20 constructor taking an rvalue string

  static_assert(!std::is_convertible_v<std::string, std::stringbuf>,
      "stringbuf(string&&, ios::openmode) is explicit");

  std::string s1(cstr);
  std::stringbuf sbuf1(std::move(s1));
  VERIFY( s1.empty() );
  VERIFY( sbuf1.str() == cstr );
  VERIFY( sbuf1.sgetc() == cstr[0] );

  std::string s2(cstr);
  std::stringbuf sbuf2(std::move(s2), std::ios_base::in);
  VERIFY( s2.empty() );
  VERIFY( sbuf2.str() == cstr );
  VERIFY( sbuf2.sgetc() == cstr[0] );
  VERIFY( sbuf2.sputc('X') == std::stringbuf::traits_type::eof() );

  std::string s3(cstr);
  std::stringbuf sbuf3(std::move(s3), std::ios_base::out);
  VERIFY( s3.empty() );
  VERIFY( sbuf3.str() == cstr );
  VERIFY( sbuf3.sputc('Y') == 'Y' );
  VERIFY( sbuf3.sgetc() == std::stringbuf::traits_type::eof() );
}

// A minimal allocator with no default constructor
template<typename T>
  struct NoDefaultCons : __gnu_test::SimpleAllocator<T>
  {
    using __gnu_test::SimpleAllocator<T>::SimpleAllocator;

    NoDefaultCons() = delete;

    NoDefaultCons(int) { }
  };

void
test03()
{
  // Test C++20 constructors taking strings using different allocators

  using alloc_type = __gnu_test::tracker_allocator<char>;
  using str_type = std::basic_string<char, std::char_traits<char>, alloc_type>;

  auto const mode = std::ios_base::in | std::ios_base::out;
  str_type s1(cstr);

  {
    // basic_stringbuf(const basic_string<char, traits_type, SAlloc>&,
    //                 ios_base::openmode,
    //                 const allocator_type&)

    std::stringbuf::allocator_type a;
    std::stringbuf sbuf = {s1, mode, a}; // ={} checks for non-explicit ctor
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );

    std::stringbuf sbuf2 = {std::move(s1), std::ios::in, a};
    VERIFY( sbuf2.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf2.sgetc() == s1[0] );
    VERIFY( sbuf2.sputc('X') == std::stringbuf::traits_type::eof() );

    std::stringbuf sbuf3 = {std::move(s1), std::ios::out, a};
    VERIFY( sbuf3.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf3.sputc('X') == 'X' );
    VERIFY( sbuf3.sgetc() == std::stringbuf::traits_type::eof() );
  }

  {
    // explicit
    // basic_stringbuf(const basic_string<char, traits_type, SAlloc>&,
    //                 ios_base::openmode)

    std::stringbuf sbuf(s1, mode);
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );

    std::stringbuf sbuf2(std::move(s1), std::ios::in);
    VERIFY( sbuf2.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf2.sgetc() == s1[0] );
    VERIFY( sbuf2.sputc('X') == std::stringbuf::traits_type::eof() );

    std::stringbuf sbuf3(std::move(s1), std::ios::out);
    VERIFY( sbuf3.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf3.sputc('X') == 'X' );
    VERIFY( sbuf3.sgetc() == std::stringbuf::traits_type::eof() );
  }

  {
    // explicit
    // basic_stringbuf(const basic_string<char, traits_type, SAlloc>&,
    //                 ios_base::openmode = ios_base::in|ios_base::out)

    static_assert( ! std::is_convertible_v<str_type, std::stringbuf>,
	"stringbuf(const basic_string<char, traits_type, SAlloc>&, openmode)"
	" is explicit");

    std::stringbuf sbuf(s1);
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );

    std::stringbuf sbuf2(std::move(s1));
    VERIFY( sbuf2.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf2.sgetc() == s1[0] );
  }

  {
    NoDefaultCons<char> a(1);
    stringbuf_with_alloc<NoDefaultCons<char>> sbuf1(s1, a);
    VERIFY( sbuf1.str() == cstr );
    VERIFY( sbuf1.sgetc() == s1[0] );

    stringbuf_with_alloc<NoDefaultCons<char>> sbuf2(s1, std::ios::in, a);
    VERIFY( sbuf2.str() == cstr );
    VERIFY( sbuf2.sgetc() == s1[0] );
    VERIFY( sbuf2.sputc('X') == std::stringbuf::traits_type::eof() );

    stringbuf_with_alloc<NoDefaultCons<char>> sbuf3(s1, std::ios::out, a);
    VERIFY( sbuf3.str() == cstr );
    VERIFY( sbuf3.sputc('X') == 'X' );
    VERIFY( sbuf3.sgetc() == std::stringbuf::traits_type::eof() );
  }
}

void
test04()
{
  // Test C++20 allocator-extended move constructor

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
}
