// { dg-do run { target c++11 } }

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

#include <string>

// This test verifies the following:
//   insert_after single item
//   before_begin iterator
void
test01()
{
  std::forward_list<int> fl({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

  std::forward_list<int>::iterator ret = fl.insert_after(fl.before_begin(),
							 42);
  VERIFY( ret == fl.begin() );
  VERIFY( fl.front() == 42 );
}

// This test verifies the following:
void
test02()
{
  std::forward_list<int> fl({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

  std::forward_list<int>::const_iterator pos = fl.cbegin();
  ++pos;
  VERIFY( *pos == 1 );

  std::forward_list<int>::iterator ret = fl.insert_after(pos, 0, 42);
  VERIFY( ret == pos );

  ret = fl.insert_after(pos, 5, 42);
  VERIFY( *pos == 1 );

  ++pos;
  VERIFY( *pos == 42 );
  ++pos;
  ++pos;
  ++pos;
  ++pos;
  VERIFY( *pos == 42 );
  VERIFY( ret == pos );
  ++pos;
  VERIFY( *pos == 2 );
}

// This test verifies the following:
void
test03()
{
  std::forward_list<int> fl({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

  std::forward_list<int>::const_iterator pos = fl.cbegin();
  ++pos;
  VERIFY( *pos == 1 );

  int i[3] = {666, 777, 888};
  std::forward_list<int>::iterator ret = fl.insert_after(pos, i, i);
  VERIFY( ret == pos );

  ret = fl.insert_after(pos, i, i + 3);
  VERIFY( *pos == 1 );

  ++pos;
  ++pos;
  ++pos;
  VERIFY( *pos == 888 );
  VERIFY( ret == pos );
  ++pos;
  VERIFY( *pos == 2 );
}

// This test verifies the following:
void
test04()
{
  std::forward_list<int> fl({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});

  std::forward_list<int>::const_iterator pos = fl.cbegin();
  ++pos;
  VERIFY( *pos == 1 );

  std::forward_list<int>::iterator ret = fl.insert_after(pos, { });
  VERIFY( ret == pos);

  ret = fl.insert_after(pos, {-1, -2, -3, -4, -5});
  VERIFY( *pos == 1);

  ++pos;
  ++pos;
  ++pos;
  VERIFY( *pos == -3 );
  ++pos;
  ++pos;
  VERIFY( ret == pos );
  ++pos;
  VERIFY( *pos == 2 );
}

// This test verifies the following:
void
test05()
{
  std::forward_list<std::string> fl({"AAA", "BBB", "CCC"});

  std::forward_list<std::string>::const_iterator pos = fl.cbegin();
  ++pos;
  VERIFY( *pos == "BBB" );

  std::string x( "XXX" );
  std::forward_list<std::string>::iterator ret
    = fl.insert_after(pos, std::move(x));
  VERIFY( *pos == "BBB" );
  ++pos;
  VERIFY( ret == pos );
  VERIFY( *pos == "XXX" );
  ++pos;
  VERIFY( *pos == "CCC" );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return 0;
}
