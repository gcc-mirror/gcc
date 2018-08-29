// Copyright (C) 2012-2018 Free Software Foundation, Inc.
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
//

// { dg-do run { target c++11 } }

#include <set>
#include <unordered_map>
#include <testsuite_hooks.h>

struct A
{
  int x;
  static std::set<const A*> destroyed;

  A()
  { destroyed.erase(this); }

  A(const A& a)
    : x(a.x)
  { destroyed.erase(this); }

  ~A()
  { destroyed.insert(this); }

  bool
  operator==(const A& other) const
  {
    VERIFY( destroyed.find(this) == destroyed.end() );
    VERIFY( destroyed.find(&other) == destroyed.end() );
    return x == other.x;
  }
};

std::set<const A*> A::destroyed;

struct hasher
{
  std::size_t operator()(const A& a) const
  {
    VERIFY( A::destroyed.find(&a) == A::destroyed.end() );
    return a.x / 10;
  }
};

void test01()
{
  typedef std::unordered_multimap<A, A, hasher> UMMap;
  UMMap map;

  A::destroyed.clear();
  A a;
  a.x = 0;
  map.insert({a, a});
  map.insert({a, a});
  VERIFY( map.size() == 2 );
  std::size_t bkt = map.bucket(a);
  VERIFY( map.bucket_size(bkt) == 2 );

  VERIFY( map.erase( map.begin(bkt)->first ) == 2 );
}

void test02()
{
  typedef std::unordered_multimap<A, A, hasher> UMMap;
  UMMap map;

  A::destroyed.clear();
  A a;
  a.x = 0;
  map.insert({a, a});
  map.insert({a, a});
  VERIFY( map.size() == 2 );
  std::size_t bkt = map.bucket(a);
  VERIFY( map.bucket_size(bkt) == 2 );

  VERIFY( map.erase( map.begin(bkt)->second ) == 2 );
}

int main()
{
  test01();
  test02();
  return 0;
}
