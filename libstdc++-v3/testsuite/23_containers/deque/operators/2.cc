// 2002-05-18  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2002, 2004, 2005, 2009 Free Software Foundation, Inc.
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

// 23.2.1 deque operators

#include <deque>
#include <testsuite_hooks.h>

// libstdc++/7186
void test02()
{
  bool test __attribute__((unused)) = true;

  std::deque<int> d(2);       
  typedef std::deque<int>::iterator iter;         
  typedef std::deque<int>::const_iterator constiter;

  iter beg = d.begin();
  iter end = d.end();
  constiter constbeg = d.begin();
  constiter constend = d.end();

  VERIFY( beg - constbeg == 0 );
  VERIFY( constend - end == 0 );

  VERIFY( end - constbeg > 0 );
  VERIFY( constend - beg > 0 );
}

int main()
{
  test02();
  return 0;
}
