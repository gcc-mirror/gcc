// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
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

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/9825
// filebuf::sputbackc breaks sbumpc
void test12()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  stringbuf sbuf;
  sbuf.sputn("crazy bees!", 11);
  sbuf.pubseekoff(0, ios_base::beg);
  sbuf.sbumpc();
  sbuf.sputbackc('x');
  stringbuf::int_type c = sbuf.sbumpc();
  VERIFY( c == 'x' );
  c = sbuf.sbumpc();
  VERIFY( c == 'r' );
  c = sbuf.sbumpc();
  VERIFY( c == 'a' );
}

int main() 
{
  test12();
  return 0;
}
