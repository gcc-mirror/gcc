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

  static wchar_t s[4] = L"";
  static unsigned i = 0;

  struct : std::wstreambuf
  {
    int_type
    overflow(int_type c)
    {
      s[i++] = traits_type::to_char_type(c);
      return traits_type::not_eof(c);
    }

    std::streamsize
    xsputn(const wchar_t*, std::streamsize)
    {
      VERIFY( !"xsputn should not be called" );
      return 0;
    }
  } sb;

  std::wostream out(&sb);

  out.write(L"0123", 4);

  VERIFY( out.good() );
  VERIFY( 4 == i );
  VERIFY( L'0' == s[0] && L'1' == s[1] && L'2' == s[2] && L'3' == s[3] );
}

int main()
{
  test01();
  return 0;
}
