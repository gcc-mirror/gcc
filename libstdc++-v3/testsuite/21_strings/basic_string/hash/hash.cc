// Copyright (C) 2019-2022 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <string>
#include <memory_resource>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// C++17 24.3.5 [basic.string.hash]
// If S is one of these string types, SV is the corresponding string view type,
// and s is an object of type S, then hash<S>()(s) == hash<SV>()(SV(s)).

template<typename S>
  bool
  test(const S& s)
  {
    using std::hash;
    using SV = std::basic_string_view<typename S::value_type>;
    return hash<S>()(s) == hash<SV>()(SV(s));
  }

void
test01()
{
  VERIFY( test(std::string("a narrow string")) );
  VERIFY( test(std::u16string(u"a utf-16 string")) );
  VERIFY( test(std::u32string(U"a utf-32 string")) );
  VERIFY( test(std::wstring(L"a wide string")) );
}

void
test02()
{
#if _GLIBCXX_USE_CXX11_ABI
  VERIFY( test(std::pmr::string("a narrow string, but with PMR!")) );
  VERIFY( test(std::pmr::u16string(u"a utf-16 string, but with PMR!")) );
  VERIFY( test(std::pmr::u32string(U"a utf-32 string, but with PMR!")) );
  VERIFY( test(std::pmr::wstring(L"a wide string, but with PMR!")) );
#endif
}

template<typename C>
using String
  = std::basic_string<C, std::char_traits<C>, __gnu_test::SimpleAllocator<C>>;

void
test03()
{
  // LWG 3705. Hashability shouldn't depend on basic_string's allocator
  VERIFY( test(String<char>("a narrow string")) );
  VERIFY( test(String<char16_t>(u"a utf-16 string")) );
  VERIFY( test(String<char32_t>(U"a utf-32 string")) );
  VERIFY( test(String<wchar_t>(L"a wide string")) );
}

int
main()
{
  test01();
  test02();
  test03();
}
