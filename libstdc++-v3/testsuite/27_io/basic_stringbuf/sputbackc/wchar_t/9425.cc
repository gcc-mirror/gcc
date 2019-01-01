// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/9439, libstdc++/9425
// basic_filebuf<>::pbackfail calls basic_filebuf<>::seekoff, but
// fails to check the return value
void test09()
{
  using namespace std;

  wstringbuf sbuf;
  wstringbuf::int_type r = sbuf.sputbackc(L'a');
  VERIFY( r == wstringbuf::traits_type::eof() );
}

int main() 
{
  test09();
  return 0;
}
