// Copyright (C) 2009 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.6.1.3 unformatted input functions

#include <istream>
#include <streambuf>
#include <testsuite_hooks.h>

// libstdc++/38678
void test01()
{
  bool test __attribute__((unused)) = true;

  static char x = '0';

  struct : std::streambuf
  {
    char c;

    int_type
    underflow()
    {
      c = x++;
      setg(&c, &c, &c + 1);
      return traits_type::to_int_type(c);
    }

    std::streamsize
    xsgetn(char*, std::streamsize)
    {
      VERIFY( !"xsgetn should not be called" );
      return 0;
    }
  } sb;

  std::istream in(&sb);

  char s[4] = "";

  in.read(s, 4);

  VERIFY( in.good() );
  VERIFY( 4 == in.gcount() );
  VERIFY( '0' == s[0] && '1' == s[1] && '2' == s[2] && '3' == s[3] );
}

int main()
{
  test01();
  return 0;
}
