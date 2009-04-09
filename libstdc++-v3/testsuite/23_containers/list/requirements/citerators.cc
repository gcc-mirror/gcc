// { dg-options "-std=gnu++0x" }

// 2007-10-15  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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


#include <list>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::list<int> l(7);
  VERIFY( l.cbegin() == l.begin() );
  VERIFY( l.cend() == l.end() );
  VERIFY( l.crbegin() == l.rbegin() );
  VERIFY( l.crend() == l.rend() );
  VERIFY( l.cbegin() != l.cend() );
  VERIFY( l.crbegin() != l.crend() );  
}

int main()
{
  test01();
  return 0;
}
