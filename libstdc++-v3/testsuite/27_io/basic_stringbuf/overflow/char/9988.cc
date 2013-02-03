// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2013 Free Software Foundation, Inc.
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

class OverBuf : public std::stringbuf
{
public:
  int_type pub_overflow(int_type c = traits_type::eof())
  { return std::stringbuf::overflow(c); }
};

// libstdc++/9988
// filebuf::overflow writes EOF to file
void test15()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  OverBuf sb;

  sb.sputc('a');
  sb.pub_overflow('b');
  sb.pub_overflow();
  sb.sputc('c');

  stringbuf sbin(sb.str(), ios_base::in);
  stringbuf::int_type c;
  c = sbin.sbumpc();
  VERIFY( c == 'a' );
  c = sbin.sbumpc();
  VERIFY( c == 'b' );
  c = sbin.sbumpc();
  VERIFY( c == 'c' );
  c = sbin.sbumpc();
  VERIFY( c == stringbuf::traits_type::eof() );
}

int main() 
{
  test15();
  return 0;
}
