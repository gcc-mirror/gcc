// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <iterator>
#include <istream>

namespace adl
{
  template<typename T>
    void operator&(const T&) = delete;

  struct traits : std::char_traits<char> { };

  struct X { void f() const { } };

  std::basic_istream<char, adl::traits>&
  operator>>(std::basic_istream<char, adl::traits>& is, X&)
  { return is; }
}

void
test01()
{
  std::basic_istream<char, adl::traits> is(nullptr);
  std::istream_iterator<adl::X, char, adl::traits> ii(is);
  ii->f();
}
