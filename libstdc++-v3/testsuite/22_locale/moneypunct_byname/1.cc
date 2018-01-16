// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

// { dg-do link { target c++11 } }

#include <locale>
#include <string>

std::string s = "C";

template<typename C, bool I>
struct facet : std::moneypunct_byname<C, I>
{
  facet() : std::moneypunct_byname<C, I>(s) { }
};

void
test01()
{
  facet<char, false> c0;
  facet<char, true> c1;

#ifdef _GLIBCXX_USE_WCHAR_T
  facet<wchar_t, false> w0;
  facet<wchar_t, true> w1;
#endif
}

int
main()
{
  test01();
}
