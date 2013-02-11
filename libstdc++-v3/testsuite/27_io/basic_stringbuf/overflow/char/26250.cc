// Copyright (C) 2006-2013 Free Software Foundation, Inc.
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

struct pubbuf
: std::stringbuf
{
  using std::stringbuf::eback;
  using std::stringbuf::egptr;
  using std::stringbuf::pbase;
  using std::stringbuf::pptr;
  using std::stringbuf::epptr;
  using std::stringbuf::overflow;
};

// libstdc++/26250
void test01()
{
  bool test __attribute__((unused)) = true;
  
  pubbuf buf;

  VERIFY( buf.overflow('x') == 'x' );
  VERIFY( buf.pptr() - buf.pbase() == 1 );
 
  // not required but good for efficiency
  // NB: we are implementing DR 169 and DR 432
  const int write_positions = buf.epptr() - buf.pbase();
  VERIFY( write_positions > 1 );

  // 27.7.1.3, p8:
  VERIFY( buf.egptr() - buf.eback() == 1 );
}

int main() 
{
  test01();
  return 0;
}
