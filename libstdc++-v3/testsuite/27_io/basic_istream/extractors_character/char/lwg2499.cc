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

// { dg-do run { target c++20 } }

// LWG 2499
// operator>>(basic_istream&, CharT*) makes it hard to avoid buffer overflows

#include <sstream>
#include <testsuite_hooks.h>

template<typename T>
void
test(std::basic_istream<char, T>& in)
{
  char pc[3];
  in >> pc;
  VERIFY( in.good() );
  VERIFY( pc[0] == 'a' && pc[1] == 'b' && pc[2] == '\0' );

  signed char sc[4];
  in >> sc;
  VERIFY( in.good() );
  VERIFY( sc[0] == 'c' && sc[1] == 'd' && sc[2] == 'e' && sc[3] ==  '\0' );

  unsigned char uc[4];
  in >> uc;
  VERIFY( in.good() );
  VERIFY( uc[0] == 'f' && uc[1] == 'g' && uc[2] == 'h' && uc[3] ==  '\0' );

  pc[2] = '#';
  in >> pc;
  VERIFY( in.good() );
  VERIFY( pc[0] == 'i' && pc[1] == '\0' && pc[2] == '#' );

  in >> pc;
  VERIFY( in.good() );
  VERIFY( pc[0] == 'j' && pc[1] == 'k' && pc[2] == '\0' );

  pc[2] = '#';
  in >> pc;
  VERIFY( in.eof() );
  VERIFY( pc[0] == 'l' && pc[1] == '\0' && pc[2] == '#' );
}

void
test01()
{
  std::istringstream in("abcdefghi jk l");
  test(in);
}

void
test02()
{
  struct CT : std::char_traits<char> { };
  std::basic_istringstream<char, CT> in("abcdefghi jk l");
  test(in);
}

int main()
{
  test01();
  test02();
}
