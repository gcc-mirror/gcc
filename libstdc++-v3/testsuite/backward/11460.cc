// { dg-options "-Wno-deprecated" }
// Copyright (C) 2003-2016 Free Software Foundation, Inc.
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

#include <strstream>
#include <testsuite_hooks.h>

class Buf : public std::strstreambuf
{
public:
  Buf(const char* p, std::streamsize n)
  : std::strstreambuf(p, n)
  { }

  int_type pub_pbackfail(int_type c = traits_type::eof())
  {
    return pbackfail(c);
  }
};

// libstdc++/11460
void test01()
{
  typedef std::strstreambuf::traits_type Traits;
  const char str[] = "a\xff";
  
  Buf buf(str, 2);
  
  Traits::int_type a = Traits::to_int_type('a');
  VERIFY( buf.sbumpc() == a );
  VERIFY( buf.sungetc() == a );
  VERIFY( buf.sbumpc() == a );
  VERIFY( buf.sputbackc('a') == a );
  VERIFY( buf.sbumpc() == a );
  VERIFY( buf.pub_pbackfail() != Traits::eof() );
  VERIFY( buf.sbumpc() == a );
  VERIFY( buf.pub_pbackfail(a) == a );
  buf.sbumpc();
  
  Traits::int_type xff = Traits::to_int_type('\xff');
  VERIFY( buf.sbumpc() == xff );
  VERIFY( buf.sungetc() == xff );
  VERIFY( buf.sbumpc() == xff );
  VERIFY( buf.sputbackc('\xff') == xff );
  VERIFY( buf.sbumpc() == xff );
  VERIFY( buf.pub_pbackfail() != Traits::eof() );
  VERIFY( buf.sbumpc() == xff );
  VERIFY( buf.pub_pbackfail(xff) == xff );
}

int main()
{
  test01();
  return 0;
}
