// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 27.6.3 template class basic_streambuf

#include <streambuf>
#include <testsuite_hooks.h>

struct streambuf : std::streambuf
{
  streambuf()
  {
    setp(pbuf, std::end(pbuf));
    setg(gbuf, gbuf, gbuf);
  }

  streambuf(const std::locale& loc) : streambuf()
  {
    imbue(loc);
  }

  // implement tests as member functions to be able to call protected members
  void test_copy() const;
  void test_assign() const;
  void test_swap() const;

  char gbuf[32];
  char pbuf[32];
};

void streambuf::test_copy() const
{
  streambuf a(*this);

  VERIFY( eback()  == a.eback() );
  VERIFY( gptr()   == a.gptr() );
  VERIFY( egptr()  == a.egptr() );
  VERIFY( pbase()  == a.pbase() );
  VERIFY( pptr()   == a.pptr() );
  VERIFY( epptr()  == a.epptr() );
  VERIFY( getloc() == a.getloc() );
}

void streambuf::test_assign() const
{
  streambuf a;
  a = *this;

  VERIFY( eback()  == a.eback() );
  VERIFY( gptr()   == a.gptr() );
  VERIFY( egptr()  == a.egptr() );
  VERIFY( pbase()  == a.pbase() );
  VERIFY( pptr()   == a.pptr() );
  VERIFY( epptr()  == a.epptr() );
  VERIFY( getloc() == a.getloc() );
}

void streambuf::test_swap() const
{
  streambuf a(*this);
  streambuf b;
  const streambuf c(b);

  a.swap(b);

  VERIFY( eback()  == b.eback() );
  VERIFY( gptr()   == b.gptr() );
  VERIFY( egptr()  == b.egptr() );
  VERIFY( pbase()  == b.pbase() );
  VERIFY( pptr()   == b.pptr() );
  VERIFY( epptr()  == b.epptr() );
  VERIFY( getloc() == b.getloc() );

  VERIFY( c.eback()  == a.eback() );
  VERIFY( c.gptr()   == a.gptr() );
  VERIFY( c.egptr()  == a.egptr() );
  VERIFY( c.pbase()  == a.pbase() );
  VERIFY( c.pptr()   == a.pptr() );
  VERIFY( c.epptr()  == a.epptr() );
  VERIFY( c.getloc() == a.getloc() );
}

int main()
{
  std::locale loc(ISO_8859(15,de_DE));
  streambuf s(loc);
  s.test_copy();
  s.test_assign();
  s.test_swap();
}
