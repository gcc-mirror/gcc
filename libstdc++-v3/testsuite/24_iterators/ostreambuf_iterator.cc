// 2001-04-30  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001 Free Software Foundation, Inc.
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

// 24.5.4 template class ostreambuf_iterator

#include <sstream>
#include <iterator>
#include <debug_assert.h>

bool test01(void)
{
  typedef std::ostreambuf_iterator<char> costreambuf_iter;
  typedef costreambuf_iter::streambuf_type cstreambuf_type;
  bool test = true;
  const char slit01[] = "playa hermosa, liberia, guanacaste";
  std::string str01(slit01);
  std::string tmp;
  std::stringbuf     strbuf01;
  std::stringbuf     strbuf02(str01);
  std::ostringstream ostrs00(str01);
  std::ostringstream ostrs01(str01);

  // ctor sanity checks
  costreambuf_iter ostrb_it01(ostrs00);
  VERIFY( !ostrb_it01.failed() );
  ostrb_it01++;
  ++ostrb_it01;
  VERIFY( !ostrb_it01.failed() );
  ostrb_it01 = 'a';
  VERIFY( !ostrb_it01.failed() );
  *ostrb_it01;
  VERIFY( !ostrb_it01.failed() );

  costreambuf_iter ostrb_it02(NULL);
  VERIFY( ostrb_it02.failed() );
  ostrb_it02++;
  ++ostrb_it02;
  VERIFY( ostrb_it02.failed() );
  *ostrb_it02;
  VERIFY( ostrb_it02.failed() );
  ostrb_it02 = 'a';
  VERIFY( ostrb_it02.failed() );
  
  // charT operator*() const
  // ostreambuf_iterator& operator++();
  // ostreambuf_iterator& operator++(int);
  costreambuf_iter ostrb_it27(ostrs00);
  VERIFY( !ostrb_it27.failed() );
  for (int i = 0; i < strlen(slit01) - 2; ++i)
    ostrb_it27 = 'a';
  VERIFY( !ostrb_it27.failed() );
  tmp = ostrs00.str();
  VERIFY ( tmp == str01 );

  costreambuf_iter ostrb_it28(ostrs01);
  VERIFY( !ostrb_it28.failed() );
  for (int i = 0; i < strlen(slit01) + 1; ++i)
    ostrb_it28 = 'b';
  VERIFY( !ostrb_it28.failed() );
  tmp = ostrs01.str();
  VERIFY ( tmp != str01 );

#ifdef DEBUG_ASSERT
  assert(test);
#endif

  return test;
}

int main()
{
  test01();

  return 0;
}





