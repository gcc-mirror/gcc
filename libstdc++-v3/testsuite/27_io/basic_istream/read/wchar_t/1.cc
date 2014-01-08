// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

// 27.6.1.3 unformatted input functions

#include <cwchar> // for wcsncmp,...
#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  const std::wstring str_02(L"soul eyes: john coltrane quartet");

  std::wstringbuf isbuf_03(str_02, std::ios_base::in);
  std::wstringbuf isbuf_04(str_02, std::ios_base::in);

  std::wistream is_00(0);
  std::wistream is_03(&isbuf_03);
  std::wistream is_04(&isbuf_04);
  std::ios_base::iostate state1, state2, statefail, stateeof;
  statefail = std::ios_base::failbit;
  stateeof = std::ios_base::eofbit;

  // istream& read(char_type* s, streamsize n)
  wchar_t carray[60] = L"";
  state1 = is_04.rdstate();
  is_04.read(carray, 0);
  state2 = is_04.rdstate();
  VERIFY( state1 == state2 );

  state1 = is_04.rdstate();
  is_04.read(carray, 9);
  state2 = is_04.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( !std::wcsncmp(carray, L"soul eyes", 9) );
  VERIFY( is_04.peek() == L':' );

  state1 = is_03.rdstate();
  is_03.read(carray, 60);
  state2 = is_03.rdstate();
  VERIFY( state1 != state2 );
  VERIFY( static_cast<bool>(state2 & stateeof) ); 
  VERIFY( static_cast<bool>(state2 & statefail) ); 
  VERIFY( !std::wcsncmp(carray, L"soul eyes: john coltrane quartet", 35) );
}

int 
main()
{
  test01();
  return 0;
}
