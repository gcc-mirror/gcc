// Copyright (C) 2004 Free Software Foundation
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.6.1.3 unformatted input functions

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  typedef std::ios::traits_type traits_type;

  bool test __attribute__((unused)) = true;
  const std::wstring str_01;
  const std::wstring str_02(L"soul eyes: john coltrane quartet");
  std::wstring strtmp;

  std::wstringbuf isbuf_03(str_02, std::ios_base::in);
  std::wstringbuf isbuf_04(str_02, std::ios_base::in);

  std::wistream is_00(NULL);
  std::wistream is_03(&isbuf_03);
  std::wistream is_04(&isbuf_04);
  std::ios_base::iostate state1, state2, statefail, stateeof;
  statefail = std::ios_base::failbit;
  stateeof = std::ios_base::eofbit;

  // istream& read(char_type* s, streamsize n)
  wchar_t carray[60] = L"";
  is_04.read(carray, 9);
  VERIFY( is_04.peek() == L':' );

  // istream& ignore(streamsize n = 1, int_type delim = traits::eof())
  state1 = is_04.rdstate();
  is_04.ignore();
  VERIFY( is_04.gcount() == 1 );
  state2 = is_04.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( is_04.peek() == L' ' );

  state1 = is_04.rdstate();
  is_04.ignore(0);
  VERIFY( is_04.gcount() == 0 );
  state2 = is_04.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( is_04.peek() == L' ' );

  state1 = is_04.rdstate();
  is_04.ignore(5, traits_type::to_int_type(' '));
  VERIFY( is_04.gcount() == 1 );
  state2 = is_04.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( is_04.peek() == L'j' );
}

int 
main()
{
  test01();
  return 0;
}
