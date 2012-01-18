// { dg-options "-std=gnu++0x" }
//
// Copyright (C) 2012 Free Software Foundation, Inc.
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

struct num
{
  int value;
  num(int n)                 : value(n) {}
  num(num const&)            = default;
  num& operator=(num const&) = default;
  num(num&& o)               : value(o.value)
  { o.value = -1; }
  num& operator=(num&& o)
  {
    if (this != &o)
      {
	value = o.value;
	o.value = -1;
      }
    return *this;
  }
};

struct num_hash
{
  size_t operator()(num const& a) const
  { return a.value; }
};

struct num_equal
{
  static bool _S_called_on_moved_instance;
  bool operator()(num const& a, num const& b) const
  {
    if (a.value == -1 || b.value == -1)
      _S_called_on_moved_instance = true;
    return a.value == b.value;
  }
};

bool num_equal::_S_called_on_moved_instance = false;

// libstdc++/51866
void test01()
{
  bool test __attribute__((unused)) = true;
  
  std::unordered_multiset<num, num_hash, num_equal> mset;
  mset.insert(num(1));
  mset.insert(num(2));
  mset.insert(num(1));
  mset.insert(num(2));
  VERIFY( mset.size() == 4 );
  auto iter = mset.cbegin();
  int x0 = (iter++)->value;
  int x1 = (iter++)->value;
  int x2 = (iter++)->value;
  int x3 = (iter++)->value;
  VERIFY( iter == mset.cend() );
  VERIFY( (x0 == 1 && x1 == 1 && x2 == 2 && x3 == 2)
	  || (x0 == 2 && x1 == 2 && x2 == 1 && x3 == 1) );
  VERIFY( !num_equal::_S_called_on_moved_instance );
}
  
int main()
{
  test01();
  return 0;
}
