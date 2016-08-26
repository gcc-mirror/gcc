// { dg-do run { target c++11 } }

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// 23.2.3.n forward_list xxx [lib.forward_list.xxx]

#include <forward_list>
#include <testsuite_hooks.h>

// This test verifies the following:
//   cbegin
//   erase_after one iterator
//   pos is useable and points to current element
void
test01()
{
  bool test __attribute__((unused)) = true;

  std::forward_list<int> fl({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

  std::forward_list<int>::const_iterator pos = fl.cbegin();
  ++pos;
  VERIFY( *pos == 1 );

  std::forward_list<int>::iterator pos2 = fl.erase_after(pos);

  VERIFY( *pos == 1 );
  ++pos;
  VERIFY( *pos == 3 );
  VERIFY( pos == pos2 );
}

// This test verifies the following:
//   cbegin
//   erase_after iterator range
//   pos is useable and points to current element
void
test02()
{
  bool test __attribute__((unused)) = true;

  std::forward_list<int> fl({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

  std::forward_list<int>::const_iterator pos = fl.cbegin();
  ++pos;
  VERIFY( *pos == 1 );

  std::forward_list<int>::iterator stop = fl.begin();
  ++stop;
  ++stop;
  ++stop;
  ++stop;
  VERIFY( *stop == 4 );

  std::forward_list<int>::iterator pos2 = fl.erase_after(pos, stop);

  VERIFY( pos2 == stop );
  VERIFY( *pos == 1 );
  ++pos;
  VERIFY( *pos == 4 );
  VERIFY( std::distance(fl.begin(), fl.end()) == 8 );

  std::forward_list<int>::iterator pos3
    = fl.erase_after(pos, fl.end());
  VERIFY( pos3 == fl.end() );
  VERIFY( ++pos == fl.end() );
  VERIFY( std::distance(fl.begin(), fl.end()) == 3 );

  std::forward_list<int>::iterator pos4
    = fl.erase_after(fl.before_begin(), pos);
  VERIFY( pos4 == pos );
  VERIFY( std::distance(fl.begin(), fl.end()) == 0 );
  VERIFY( fl.empty() );
}

int
main()
{
  test01();
  test02();
  return 0;
}
