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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.6.1.3 unformatted input functions

#include <sstream>
#include <testsuite_hooks.h>

// DR 243. get and getline when sentry reports failure.
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wstringbuf strbuf01;
  wstringbuf strbuf02;
  wistream istr01(&strbuf01);
  wistream istr02(&strbuf02);
  wchar_t buf02[2] = L"*" ;
 
  istr01.peek();
  VERIFY( istr01.eof() );

  istr01.get(0, 0);
  VERIFY( istr01.gcount() == 0 );
  VERIFY( istr01.fail() );

  istr02.peek();
  VERIFY( istr02.eof() );

  istr02.get(buf02, 1);
  VERIFY( istr02.gcount() == 0 );
  VERIFY( istr02.fail() );
  VERIFY( buf02[0] == wchar_t() );  
}

int main() 
{
  test01();
  return 0;
}
