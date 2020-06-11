// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-do run }

// PR libstdc++/94749
// basic_istream::ignore(n, c) discards n+1 if next character is equal to c.

#include <sstream>
#include <testsuite_hooks.h>

typedef wchar_t C;

void
test01()
{
  std::basic_istringstream<C> s(L" +   -");
  s.ignore(1, L'+');
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.get() == L'-' );
}

void
test02()
{
  std::basic_istringstream<C> s(L".+...-");
  s.ignore(1, L'+');
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.get() == L'-' );
}

void
test03()
{
  std::basic_istringstream<C, __gnu_cxx::char_traits<C> > s(L" +   -");
  s.ignore(1, L'+');
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.get() == L'-' );
}

void
test04()
{
  std::basic_istringstream<C, __gnu_cxx::char_traits<C> > s(L".+...-");
  s.ignore(1, L'+');
  VERIFY( s.get() == L'+' );
  s.ignore(3, L'-');
  VERIFY( s.get() == L'-' );
}


int
main()
{
  // test01();
  // test02();
  test03();
  test04();
}
