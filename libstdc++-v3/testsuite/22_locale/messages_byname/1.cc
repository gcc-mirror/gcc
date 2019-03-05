// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

template<typename C>
struct facet : std::messages_byname<C>
{
  facet() : std::messages_byname<C>(s) { }
};

void
test01()
{
  facet<char> c;

#ifdef _GLIBCXX_USE_WCHAR_T
  facet<wchar_t> w;
#endif
}

int
main()
{
  test01();
}
