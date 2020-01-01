// Copyright (C) 2003-2020 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <locale>
#include <fstream>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// libstdc++/12790
void test01()
{
  using namespace std;
  using __gnu_test::pod_uchar;
  typedef basic_filebuf<pod_uchar>::traits_type traits_type;

  const char* name = "tmp_seekoff_12790";

  locale loc(locale::classic(),
	     new codecvt<traits_type::char_type, char,
	     traits_type::state_type>);

  basic_filebuf<pod_uchar> fb;
  fb.pubimbue(loc);

  fb.open(name, ios_base::out | ios_base::trunc);
  fb.sputc(pod_uchar::from<char>('b'));
  fb.sputc(pod_uchar::from<char>(0xff));
  fb.sputc(pod_uchar::from<char>('a'));
  fb.sputc(pod_uchar::from<char>(0xfc));
  fb.sputc(pod_uchar::from<char>(0));
  fb.sputc(pod_uchar::from<char>(0));

  fb.close();
  fb.open(name, ios_base::in);

  fb.sbumpc();
  fb.sbumpc();

  // Check that seekoff returns the correct state
  traits_type::pos_type pos = fb.pubseekoff(0, ios_base::cur);

  traits_type::int_type c = fb.sbumpc();
  VERIFY( c != traits_type::eof() );
  VERIFY( traits_type::eq(traits_type::to_char_type(c),
			  pod_uchar::from<char>('a')) );
  fb.sbumpc();

  fb.pubseekpos(pos);

  c = fb.sbumpc();
  VERIFY( c != traits_type::eof() );
  VERIFY( traits_type::eq(traits_type::to_char_type(c),
			  pod_uchar::from<char>('a')) );

  fb.close();
}

int main()
{
  test01();
  return 0;
}
