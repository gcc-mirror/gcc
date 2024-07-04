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

  static_assert(!std::is_convertible_v<std::allocator<wchar_t>, std::wstringbuf>,
      "wstringbuf(const allocator<wchar_t>&) is explicit");

  {
    using alloc_type = __gnu_test::uneq_allocator<wchar_t>;
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

  std::wstringbuf::allocator_type a;
  {
    std::wstringbuf sbuf(std::ios_base::in, a);
    VERIFY( sbuf.str().empty() );

    std::wstringbuf sbuf2 = {std::ios_base::in, a}; // non-explicit ctor
  }

  {
    std::wstringbuf sbuf(a);
    VERIFY( sbuf.str().empty() );
  }
}

auto const cstr = L"This is a test string";

void
test02()
{
  // Test C++20 constructor taking an rvalue string

  static_assert(!std::is_convertible_v<std::wstring, std::wstringbuf>,
      "wstringbuf(wstring&&, ios::openmode) is explicit");

  std::wstring s1(cstr);
  std::wstringbuf sbuf1(std::move(s1));
  VERIFY( s1.empty() );
  VERIFY( sbuf1.str() == cstr );
  VERIFY( sbuf1.sgetc() == cstr[0] );

  std::wstring s2(cstr);
  std::wstringbuf sbuf2(std::move(s2), std::ios_base::in);
  VERIFY( s2.empty() );
  VERIFY( sbuf2.str() == cstr );
  VERIFY( sbuf2.sgetc() == cstr[0] );
  VERIFY( sbuf2.sputc(L'X') == std::wstringbuf::traits_type::eof() );

  std::wstring s3(cstr);
  std::wstringbuf sbuf3(std::move(s3), std::ios_base::out);
  VERIFY( s3.empty() );
  VERIFY( sbuf3.str() == cstr );
  VERIFY( sbuf3.sputc(L'Y') == L'Y' );
  VERIFY( sbuf3.sgetc() == std::wstringbuf::traits_type::eof() );
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

  using alloc_type = __gnu_test::tracker_allocator<wchar_t>;
  using str_type = std::basic_string<wchar_t, std::char_traits<wchar_t>, alloc_type>;

  auto const mode = std::ios_base::in | std::ios_base::out;
  str_type s1(cstr);

  {
    // basic_stringbuf(const basic_string<wchar_t, traits_type, SAlloc>&,
    //                 ios_base::openmode,
    //                 const allocator_type&)

    std::wstringbuf::allocator_type a;
    std::wstringbuf sbuf = {s1, mode, a}; // ={} checks for non-explicit ctor
    std::wstring s2(cstr);
    VERIFY( sbuf.str() == s2 );

    std::wstringbuf sbuf2 = {std::move(s1), std::ios::in, a};
    VERIFY( sbuf2.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf2.sgetc() == s1[0] );
    VERIFY( sbuf2.sputc(L'X') == std::wstringbuf::traits_type::eof() );

    std::wstringbuf sbuf3 = {std::move(s1), std::ios::out, a};
    VERIFY( sbuf3.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf3.sputc(L'X') == L'X' );
    VERIFY( sbuf3.sgetc() == std::wstringbuf::traits_type::eof() );
  }

  {
    // explicit
    // basic_stringbuf(const basic_string<wchar_t, traits_type, SAlloc>&,
    //                 ios_base::openmode)

    std::wstringbuf sbuf(s1, mode);
    std::wstring s2(cstr);
    VERIFY( sbuf.str() == s2 );

    std::wstringbuf sbuf2(std::move(s1), std::ios::in);
    VERIFY( sbuf2.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf2.sgetc() == s1[0] );
    VERIFY( sbuf2.sputc(L'X') == std::wstringbuf::traits_type::eof() );

    std::wstringbuf sbuf3(std::move(s1), std::ios::out);
    VERIFY( sbuf3.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf3.sputc(L'X') == L'X' );
    VERIFY( sbuf3.sgetc() == std::wstringbuf::traits_type::eof() );
  }

  {
    // explicit
    // basic_stringbuf(const basic_string<wchar_t, traits_type, SAlloc>&,
    //                 ios_base::openmode = ios_base::in|ios_base::out)

    static_assert( ! std::is_convertible_v<str_type, std::wstringbuf>,
	"wstringbuf(const basic_string<wchar_t, traits_type, SAlloc>&,"
		  " openmode) is explicit");

    std::wstringbuf sbuf(s1);
    std::wstring s2(cstr);
    VERIFY( sbuf.str() == s2 );

    std::wstringbuf sbuf2(std::move(s1));
    VERIFY( sbuf2.str() == s2 );
    VERIFY( s1 == cstr ); // did not move from std::move(s1)
    VERIFY( sbuf2.sgetc() == s1[0] );
  }

  {
    NoDefaultCons<wchar_t> a(1);
    stringbuf_with_alloc<NoDefaultCons<wchar_t>> sbuf1(s1, a);
    VERIFY( sbuf1.str() == cstr );
    VERIFY( sbuf1.sgetc() == s1[0] );

    stringbuf_with_alloc<NoDefaultCons<wchar_t>> sbuf2(s1, std::ios::in, a);
    VERIFY( sbuf2.str() == cstr );
    VERIFY( sbuf2.sgetc() == s1[0] );
    VERIFY( sbuf2.sputc(L'X') == std::wstringbuf::traits_type::eof() );

    stringbuf_with_alloc<NoDefaultCons<wchar_t>> sbuf3(s1, std::ios::out, a);
    VERIFY( sbuf3.str() == cstr );
    VERIFY( sbuf3.sputc(L'X') == L'X' );
    VERIFY( sbuf3.sgetc() == std::wstringbuf::traits_type::eof() );
  }
}

void
test04()
{
  // Test C++20 allocator-extended move constructor

  std::wstringbuf sbuf1(cstr);

  std::wstringbuf::allocator_type a;
  std::wstringbuf sbuf2(std::move(sbuf1), a);
  VERIFY( sbuf1.str().empty() );

  std::wstring s(cstr);
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
