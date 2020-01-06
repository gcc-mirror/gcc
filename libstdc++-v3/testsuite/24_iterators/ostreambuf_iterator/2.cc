// 2001-04-30  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

// 24.5.4 template class ostreambuf_iterator

#include <sstream>
#include <iterator>
#include <testsuite_hooks.h>

void test02(void)
{
  typedef std::ostreambuf_iterator<char> costreambuf_iter;
  typedef costreambuf_iter::streambuf_type cstreambuf_type;
  const char slit01[] = "playa hermosa, liberia, guanacaste";
  const char slit02[] = "bodega bay, lost coast, california";
  std::string str01(slit01);
  std::string str02(slit02);
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

  costreambuf_iter ostrb_it02(0);
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
  costreambuf_iter ostrb_it27(ostrs01);
  VERIFY( !ostrb_it27.failed() );
  int j = str02.size();
  for (int i = 0; i < j; ++i)
    ostrb_it27 = str02[i];
  VERIFY( !ostrb_it27.failed() );
  tmp = ostrs01.str();
  VERIFY ( tmp != str01 );
  VERIFY ( tmp == str02 );

  costreambuf_iter ostrb_it28(ostrs00);
  VERIFY( !ostrb_it28.failed() );
  j = ostrs00.str().size();
  for (int i = 0; i < j + 2; ++i)
    ostrb_it28 = 'b';
  VERIFY( !ostrb_it28.failed() );
  tmp = ostrs00.str();
  VERIFY ( tmp != str01 );
  VERIFY ( tmp != str02 );
}

int main()
{
  test02();
  return 0;
}
