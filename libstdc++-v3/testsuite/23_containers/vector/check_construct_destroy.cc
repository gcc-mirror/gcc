// 2004-07-26  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

#include <vector>
#include <testsuite_allocator.h>

using namespace __gnu_test;

int main()
{
  typedef std::vector<int, tracker_allocator<int> > Container;
  const int arr10[10] = { 2, 4, 1, 7, 3, 8, 10, 5, 9, 6 };
  bool ok = true;

  tracker_allocator_counter::reset();
  {
    Container c;
    ok = check_construct_destroy("empty container", 0, 0) && ok;
  }
  ok = check_construct_destroy("empty container", 0, 0) && ok;


  tracker_allocator_counter::reset();
  {
    Container c(arr10, arr10 + 10);
    ok = check_construct_destroy("Construct from range", 10, 0) && ok;
  }
  ok = check_construct_destroy("Construct from range", 10, 10) && ok;

  // For C++11 and later the allocator is used to construct/destroy an
  // additional temporary object during insertion.
  const int extra = __cplusplus >= 201102L ? 1 : 0;
  {
    Container c(arr10, arr10 + 10);
    c.reserve(100);
    tracker_allocator_counter::reset();
    c.insert(c.begin(), arr10[0]);
    ok = check_construct_destroy("Insert element", 1+extra, 0+extra) && ok;
  }
  ok = check_construct_destroy("Insert element", 1+extra, 11+extra) && ok;

  {
    Container c(arr10, arr10 + 10);
    c.reserve(100);
    tracker_allocator_counter::reset();
    c.insert(c.begin() + 5, arr10, arr10+3);
    ok = check_construct_destroy("Insert short range", 3, 0) && ok;
  }
  ok = check_construct_destroy("Insert short range", 3, 13) && ok;

  {
    Container c(arr10, arr10 + 10);
    c.reserve(100);
    tracker_allocator_counter::reset();
    c.insert(c.begin() + 7, arr10, arr10+10);
    ok = check_construct_destroy("Insert long range", 10, 0) && ok;
  }
  ok = check_construct_destroy("Insert long range", 10, 20) && ok;

  return ok ? 0 : 1;
}

