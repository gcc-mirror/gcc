// 2004-10-01  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

// 27.7.1.3  Overridden virtual functions  [lib.stringbuf.virtuals]

#include <sstream>
#include <testsuite_hooks.h>

class my_stringbuf : public std::wstringbuf
{
public:
  my_stringbuf(const std::wstring& str, std::ios_base::openmode mode)
  : std::wstringbuf(str, mode) { }

  int_type 
  pub_pbackfail(int_type __c) 
  { return this->pbackfail(__c); }
};

// We weren't enforcing 27.7.1.3/2, bullet 2: "... and if
// mode & ios_base::out is nonzero, ..."
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef my_stringbuf::int_type    int_type;
  typedef my_stringbuf::traits_type traits_type;

  my_stringbuf sbuf(L"any", ios_base::in);
  
  int_type c = sbuf.sbumpc();
  VERIFY( c == L'a' );

  c = sbuf.pub_pbackfail(L'x');
  VERIFY( c == traits_type::eof() );
  VERIFY( sbuf.str() == L"any" );
  c = sbuf.sgetc();
  VERIFY( c == L'n' );
}


int main()
{
  test01();
  return 0;
}
