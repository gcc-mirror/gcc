// 2004-08-12  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2020 Free Software Foundation, Inc.
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

// 27.7.1.3 Overridden virtual functions

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/16956
void test01()
{
  using namespace std;

  typedef wstringbuf::int_type int_type;
  typedef wstringbuf::traits_type traits_type;
  typedef wstringbuf::pos_type pos_type;
  typedef wstringbuf::off_type off_type;

  wstringbuf strb_01(L"lara's place", ios_base::in);
  pos_type pt_1 = strb_01.pubseekoff(5, ios_base::cur, ios_base::in);
  int_type c1 = strb_01.sgetc();
  VERIFY( c1 != traits_type::eof() );
  pos_type pt_2 = strb_01.pubseekoff(2, ios_base::cur, ios_base::in);
  pos_type pt_3 = strb_01.pubseekpos(pt_1, ios_base::in);
  int_type c2 = strb_01.sbumpc();
  VERIFY( off_type(pt_3) == off_type(pt_2) - 2 );
  VERIFY( c2 == c1 );

  wstringbuf strb_02(L"-", ios_base::out);
  pos_type pt_4 = strb_02.pubseekoff(0, ios_base::cur, ios_base::out);
  strb_02.sputn(L"red", 3);
  pos_type pt_5 = strb_02.pubseekoff(-3, ios_base::cur, ios_base::out);
  strb_02.pubseekpos(pt_5, ios_base::out);
  VERIFY( off_type(pt_5) == off_type(pt_4) );
  strb_02.sputn(L"blu", 3);
  VERIFY( strb_02.str() == L"blu" );
}

int main()
{
  test01();
  return 0;
}
