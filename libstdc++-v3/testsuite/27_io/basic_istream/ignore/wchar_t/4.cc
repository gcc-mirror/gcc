// 2005-01-05  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005 Free Software Foundation
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
  using namespace std;
  bool test __attribute__((unused)) = true;

  const wstring str_01(L"Gesang der junglinge");
  ios_base::iostate state1, state2;

  wstringbuf isbuf_01(str_01, ios_base::in);
  wistream is_01(&isbuf_01);

  state1 = is_01.rdstate();
  VERIFY( state1 == ios_base::goodbit );

  is_01.ignore(11, L'j');
  VERIFY( is_01.gcount() == 11 );
  state2 = is_01.rdstate();
  VERIFY( state2 == state1 );
  VERIFY( is_01.peek() == L'j' );

  is_01.ignore(9);
  VERIFY( is_01.gcount() == 9 );
  state2 = is_01.rdstate();
  VERIFY( state2 == state1 );
}

int 
main()
{
  test01();
  return 0;
}
