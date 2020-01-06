// { dg-do run { target c++11 } }

// 2012-01-19  Jakub Jelinek  <jakub@redhat.com>
//
// Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <testsuite_hooks.h>

// libstdc++/51845
void test01()
{
  typedef std::unordered_multimap<int, int> Mmap;
  typedef Mmap::iterator       iterator;
  typedef Mmap::const_iterator const_iterator;
  typedef Mmap::value_type     value_type;

  Mmap mm1;

  mm1.insert(value_type(11135, 1));
  mm1.insert(value_type(11135, 17082));
  mm1.insert(value_type(9644, 24135));
  mm1.insert(value_type(9644, 9644));
  mm1.insert(value_type(13984, 19841));
  mm1.insert(value_type(9644, 1982));
  mm1.insert(value_type(13984, 1945));
  mm1.insert(value_type(7, 1982));
  mm1.insert(value_type(7, 1945));
  VERIFY( mm1.size() == 9 );

  iterator it1 = mm1.begin();
  ++it1;
  iterator it2 = it1;
  ++it2;
  ++it2;
  iterator it3 = mm1.erase(it1, it2);
  VERIFY( mm1.size() == 7 );
  VERIFY( it3 == it2 );
  VERIFY( *it3 == *it2 );

  const_iterator it4 = mm1.begin();
  ++it4;
  const_iterator it5 = it4;
  ++it5;
  const_iterator it6 = mm1.erase(it4);
  VERIFY( mm1.size() == 6 );
  VERIFY( it6 == it5 );
  VERIFY( *it6 == *it5 );
}

int main()
{
  test01();
  return 0;
}
