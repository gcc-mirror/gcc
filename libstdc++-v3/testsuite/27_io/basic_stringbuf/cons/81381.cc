// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

// PR libstdc++/81381

#include <memory>
#include <sstream>
#include <testsuite_allocator.h>

using Alloc = __gnu_test::uneq_allocator<char>;
using String = std::basic_string<char, std::char_traits<char>, Alloc>;

struct SB : std::basic_stringbuf<char, std::char_traits<char>, Alloc>
{
  SB(const String& s) : basic_stringbuf(s) { }

  using basic_stringbuf::overflow;
};

int main()
{
  String s(Alloc(23));
  SB b(s);
  b.overflow('a');
  VERIFY( b.str().get_allocator() == s.get_allocator() );
}
