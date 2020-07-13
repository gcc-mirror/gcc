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

// PR libstdc++/96161
// basic_istream::ignore sets eofbit too soon

#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

typedef char C;

void
test01()
{
  std::basic_istringstream<C> s("  ");
  s.ignore(2, '+');
  VERIFY( s.gcount() == 2 );
  VERIFY( s.good() );
  VERIFY( s.get() == std::char_traits<C>::eof() );
  VERIFY( s.eof() );
}

void
test02()
{
  std::basic_istringstream<C> s("  ");
  s.ignore(2);
  VERIFY( s.gcount() == 2 );
  VERIFY( s.good() );
  VERIFY( s.get() == std::char_traits<C>::eof() );
  VERIFY( s.eof() );
}

void
test03()
{
  std::basic_istringstream<C, __gnu_cxx::char_traits<C> > s("  ");
  s.ignore(2, '+');
  VERIFY( s.gcount() == 2 );
  VERIFY( s.good() );
  VERIFY( s.get() == __gnu_cxx::char_traits<C>::eof() );
  VERIFY( s.eof() );
}

void
test04()
{
  std::basic_istringstream<C, __gnu_cxx::char_traits<C> > s("  ");
  s.ignore(2);
  VERIFY( s.gcount() == 2 );
  VERIFY( s.good() );
  VERIFY( s.get() == __gnu_cxx::char_traits<C>::eof() );
  VERIFY( s.eof() );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
