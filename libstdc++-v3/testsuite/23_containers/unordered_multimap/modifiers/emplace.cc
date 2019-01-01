// { dg-do run { target c++11 } }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

// range insert

#include <tuple>
#include <vector>
#include <unordered_map>
#include <testsuite_hooks.h>

class PathPoint
{
public:
  PathPoint(char t, const std::vector<double>& c)
  : type(t), coords(c) { }
  PathPoint(char t, std::vector<double>&& c)
  : type(t), coords(std::move(c)) { }
  char getType() const { return type; }
  const std::vector<double>& getCoords() const { return coords; }
private:
  char type;
  std::vector<double> coords;
};

void test01()
{
  typedef std::unordered_multimap<char, std::vector<double>> MMap;
  MMap mm;

  std::vector<double> coord1 = { 0.0, 1.0, 2.0 };

  auto it = mm.emplace('a', coord1);
  VERIFY( mm.size() == 1 );
  VERIFY( it->first == 'a' );

  coord1[0] = 3.0;
  it = mm.emplace('a', coord1);
  VERIFY( mm.size() == 2 );
  VERIFY( it->first == 'a' );
  VERIFY( it->second[0] == 3.0 );

  it = mm.emplace_hint(mm.begin(), 'b', coord1);
  VERIFY( it != mm.end() );
  VERIFY( it->first == 'b' );
  VERIFY( it->second[0] == 3.0 );

  double *px = &coord1[0];
  it = mm.emplace('c', std::move(coord1));
  VERIFY( it->first == 'c' );
  VERIFY( &(it->second[0]) == px );
}

void test02()
{
  using namespace std;
  typedef unordered_multimap<char, PathPoint> Map;
  Map m;

  std::vector<double> coord1 = { 0.0, 1.0, 2.0 };

  auto it = m.emplace(piecewise_construct,
		       make_tuple('a'), make_tuple('a', coord1));
  VERIFY( m.size() == 1 );
  VERIFY( it->first == 'a' );

  coord1[0] = 3.0;
  it = m.emplace(piecewise_construct,
		  make_tuple('a'), make_tuple( 'b', coord1));
  VERIFY( m.size() == 2 );
  VERIFY( it->first == 'a' );
  VERIFY( it->second.getCoords()[0] == 3.0 );

  it = m.emplace_hint(m.begin(), piecewise_construct,
		      make_tuple('b'), make_tuple('c', coord1));
  VERIFY( it != m.end() );
  VERIFY( it->first == 'b' );
  VERIFY( it->second.getCoords()[0] == 3.0 );

  double *px = &coord1[0];
  it = m.emplace(piecewise_construct,
		  make_tuple('c'), make_tuple('d', move(coord1)));
  VERIFY( it->first == 'c' );
  VERIFY( &(it->second.getCoords()[0]) == px );
}


int main()
{
  test01();
  test02();
  return 0;
}
