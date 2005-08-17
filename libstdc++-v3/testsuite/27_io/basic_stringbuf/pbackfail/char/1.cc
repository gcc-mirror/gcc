// 2004-10-01  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation, Inc.
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

// 27.7.1.3  Overridden virtual functions  [lib.stringbuf.virtuals]

#include <sstream>
#include <testsuite_hooks.h>

class my_stringbuf : public std::stringbuf
{
public:
  my_stringbuf(const std::string& str, std::ios_base::openmode mode)
  : std::stringbuf(str, mode) { }

  int_type 
  pub_pbackfail(int_type c) 
  { return this->pbackfail(c); }
};

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef my_stringbuf::int_type    int_type;
  typedef my_stringbuf::traits_type traits_type;

  my_stringbuf sbuf("any", ios_base::in | ios_base::out);

  int_type c = sbuf.sgetc();
  VERIFY( c == 'a' );

  c = sbuf.pub_pbackfail('z');
  VERIFY( c == traits_type::eof() );
  c = sbuf.sbumpc();
  VERIFY( c == 'a' );
 
  c = sbuf.pub_pbackfail('a');
  VERIFY( c == 'a' );
  c = sbuf.sbumpc();
  VERIFY( c == 'a' );
  
  c = sbuf.pub_pbackfail('x');
  VERIFY( c == 'x' );
  c = sbuf.sbumpc();
  VERIFY( c == 'x' );

  const int_type eof = traits_type::eof();
  c = sbuf.pub_pbackfail(eof);
  VERIFY( c == traits_type::not_eof(eof) );
  c = sbuf.sgetc();
  VERIFY( c == 'x' );
}

int main()
{
  test01();
  return 0;
}
