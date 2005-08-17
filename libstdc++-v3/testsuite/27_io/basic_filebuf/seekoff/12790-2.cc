// Copyright (C) 2003, 2005 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

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

  bool test __attribute__((unused)) = true;
  const char* name = "tmp_seekoff_12790";

  locale loc(locale::classic(),
	     new codecvt<traits_type::char_type, char,
	     traits_type::state_type>);

  basic_filebuf<pod_uchar> fb;
  fb.pubimbue(loc);

  fb.open(name, ios_base::out);
  fb.sputc(pod_uchar::from<char>(0xff));
  fb.sputc(pod_uchar::from<char>(0));
  fb.sputc(pod_uchar::from<char>(0));
  fb.sputc(pod_uchar::from<char>('a'));
  fb.sputc(pod_uchar::from<char>('a'));
  fb.sputc(pod_uchar::from<char>('a'));
  fb.sputc(pod_uchar::from<char>('a'));
  fb.close();

  fb.open(name, ios_base::in);
  fb.sbumpc();
  fb.sbumpc();
  fb.sbumpc();

  // Check that seekoff resets the state when seeking to beginning.
  traits_type::pos_type pos = fb.pubseekoff(0, ios_base::beg);
  VERIFY( pos != traits_type::pos_type(traits_type::off_type(-1)) );

  traits_type::int_type c = fb.sbumpc();
  VERIFY( c != traits_type::eof() );
  VERIFY( traits_type::eq(traits_type::to_char_type(c),
			  pod_uchar::from<char>(0xff)) );
  fb.sbumpc();
  fb.sbumpc();
  fb.sbumpc();
  fb.sbumpc();
  fb.sbumpc();
  c = fb.sbumpc();
  VERIFY( c != traits_type::eof() );
  VERIFY( traits_type::eq(traits_type::to_char_type(c),
			  pod_uchar::from<char>('a')) );
  c = fb.sbumpc();
  VERIFY( c == traits_type::eof() );

  fb.close();
}

int main()
{
  test01();
  return 0;
}
