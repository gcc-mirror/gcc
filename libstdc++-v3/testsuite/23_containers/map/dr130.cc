// { dg-options "-std=gnu++0x" }
// 2008-07-22  Edward Smith-Rowland  <3dw4rd@verizon.net>
//
// Copyright (C) 2009-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <map>
#include <testsuite_hooks.h>

//  DR 130. Associative erase should return an iterator.
void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  map<int, int> m0;
  typedef map<int, int>::iterator iterator;
  typedef map<int, int>::const_iterator const_iterator;
  typedef map<int, int>::value_type value_type;
  typedef pair<iterator, bool> insert_return_type;

  m0.insert(value_type(1, 1));
  insert_return_type irt1 = m0.insert(value_type(2, 2));
  insert_return_type irt2 = m0.insert(value_type(3, 3));

  iterator pos1 = m0.erase(irt1.first);
  VERIFY( pos1 == irt2.first );

  iterator pos2 = m0.erase(irt2.first);
  VERIFY( pos2 == m0.end() );
}

void
test02()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  map<int, int> m0;
  typedef map<int, int>::iterator iterator;
  typedef map<int, int>::const_iterator const_iterator;
  typedef map<int, int>::value_type value_type;
  typedef pair<iterator, bool> insert_return_type;

  insert_return_type irt0 = m0.insert(value_type(1, 1));
  m0.insert(value_type(2, 2));
  insert_return_type irt2 = m0.insert(value_type(3, 3));
  insert_return_type irt3 = m0.insert(value_type(4, 4));

  iterator pos1 = m0.erase(irt0.first, irt2.first);
  VERIFY( pos1 == irt2.first );

  iterator pos2 = m0.erase(irt2.first, ++irt3.first);
  VERIFY( pos2 == m0.end() );
}

int
main()
{
  test01();
  test02();
}
