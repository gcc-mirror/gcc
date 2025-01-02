// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

#include <debug/string>
#include <memory_resource>
#include <testsuite_hooks.h>

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
  VERIFY( test(__gnu_debug::string("a narrow string")) );
#if _GLIBCXX_USE_CHAR8_T
  VERIFY( test(__gnu_debug::u8string(u8"a narrow string")) );
#endif
  VERIFY( test(__gnu_debug::u16string(u"a utf-16 string")) );
  VERIFY( test(__gnu_debug::u32string(U"a utf-32 string")) );
  VERIFY( test(__gnu_debug::wstring(L"a wide string")) );
}

#if _GLIBCXX_USE_CHAR8_T
void
test02()
{
  using std::hash;
  __gnu_debug::string native("a string, a string, my stringdom for a string");
  __gnu_debug::u8string utf8(u8"a string, a string, my stringdom for a string");
  VERIFY( hash<__gnu_debug::string>()(native) == hash<__gnu_debug::u8string>()(utf8) );
}
#endif

int
main()
{
  test01();
#if _GLIBCXX_USE_CHAR8_T
  test02();
#endif
}
