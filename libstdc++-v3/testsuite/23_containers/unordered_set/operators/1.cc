// { dg-do run { target c++11 } }

// 2010-03-25  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <testsuite_hooks.h>

void test01()
{
  std::unordered_set<int> us1, us2;
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(1);
  us2.insert(1);
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(2);
  us2.insert(2);
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(1);
  us2.insert(1);
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(3);
  VERIFY( us1 != us2 );
  VERIFY( !(us1 == us2) );

  us2.insert(3);
  VERIFY( (us1 == us2) );
  VERIFY( !(us1 != us2) );

  us2.clear();
  VERIFY( us1 != us2 );
  VERIFY( !(us1 == us2) );

  us1.clear();
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(1);
  us2.insert(2);
  VERIFY( us1 != us2 );
  VERIFY( !(us1 == us2) );

  us1.insert(2);
  us2.insert(1);
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(3);
  us2.insert(4);
  VERIFY( us1 != us2 );
  VERIFY( !(us1 == us2) );

  us1.insert(4);
  VERIFY( us1 != us2 );
  VERIFY( !(us1 == us2) );

  us2.insert(3);
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(1);
  us2.insert(1);
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  us1.insert(4);
  us2.insert(4);
  VERIFY( us1 == us2 );
  VERIFY( !(us1 != us2) );

  const std::unordered_set<int> cus1(us1), cus2(us2);
  VERIFY( cus1 == cus2 );
  VERIFY( !(cus1 != cus2) );
  VERIFY( cus1 == us2 );
  VERIFY( !(us1 != cus2) );
}

void test02()
{
  std::unordered_set<int> us1 { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  std::unordered_set<int> us2 { 0, 2, 4, 6, 8, 1, 3, 5, 7, 9 };

  VERIFY( us1 == us2 );
}

struct Hash
{
  std::size_t
  operator()(const std::pair<int, int>& p) const
  { return p.first; }
};

struct Equal
{
  bool
  operator()(const std::pair<int, int>& lhs, const std::pair<int, int>& rhs) const
  { return lhs.first == rhs.first; }
};

void test03()
{
  std::unordered_set<std::pair<int, int>, Hash, Equal> us1
  {
    { 0, 0 }, { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 },
    { 5, 5 }, { 6, 6 }, { 7, 7 }, { 8, 8 }, { 9, 9 }
  };
  std::unordered_set<std::pair<int, int>, Hash, Equal> us2
  {
    { 5, 5 }, { 6, 6 }, { 7, 7 }, { 8, 8 }, { 9, 9 },
    { 0, 0 }, { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 }
  };

  VERIFY( us1 == us2 );

  std::unordered_set<std::pair<int, int>, Hash, Equal> us3
  {
    { 5, -5 }, { 6, 6 }, { 7, 7 }, { 8, 8 }, { 9, 9 },
    { 0, 0  }, { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 }
  };

  VERIFY( us1 != us3 );
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
