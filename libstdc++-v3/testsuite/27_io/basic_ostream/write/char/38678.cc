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

// 27.6.2.6 unformatted output functions

#include <ostream>
#include <streambuf>
#include <testsuite_hooks.h>

// libstdc++/38678
void test01()
{
  bool test __attribute__((unused)) = true;

  static char s[4] = "";
  static unsigned i = 0;

  struct : std::streambuf
  {
    int_type
    overflow(int_type c)
    {
      s[i++] = traits_type::to_char_type(c);
      return traits_type::not_eof(c);
    }

    std::streamsize
    xsputn(const char*, std::streamsize)
    {
      VERIFY( !"xsputn should not be called" );
      return 0;
    }
  } sb;

  std::ostream out(&sb);

  out.write("0123", 4);

  VERIFY( out.good() );
  VERIFY( 4 == i );
  VERIFY( '0' == s[0] && '1' == s[1] && '2' == s[2] && '3' == s[3] );
}

int main()
{
  test01();
  return 0;
}
