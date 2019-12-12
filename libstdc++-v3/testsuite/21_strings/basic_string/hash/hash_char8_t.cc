// Copyright (C) 2019 Free Software Foundation, Inc.
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
// { dg-do run { target c++2a } }

#include <string>
#include <memory_resource>
#include <testsuite_hooks.h>

// C++2a N4810 21.3.5 [basic.string.hash]
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
  VERIFY( test(std::u8string(u8"a utf-8 string")) );
#if _GLIBCXX_USE_CXX11_ABI
  VERIFY( test(std::pmr::string("a narrow string, but with PMR!")) );
  VERIFY( test(std::pmr::u8string(u8"a utf-8 string, but with PMR!")) );
#endif
}

void
test02()
{
  using std::hash;
  std::string native("a string, a string, my stringdom for a string");
  std::u8string utf8(u8"a string, a string, my stringdom for a string");
  VERIFY( hash<std::string>()(native) == hash<std::u8string>()(utf8) );
}

int
main()
{
  test01();
  test02();
}
