// { dg-options "-Wno-deprecated" }

// 2004-07-26  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

#include <hash_set>
#include <functional>
#include <iterator>
#include <testsuite_allocator.h>

using namespace __gnu_test;

int main()
{
  typedef __gnu_cxx::hash_set<int, __gnu_cxx::hash<int>, std::equal_to<int>,
                              tracker_allocator<int> >
    Container;

  const int arr10[10]  = { 2, 4, 1, 7, 3, 8, 10, 5, 9, 6 };
  const int arr10a[10] = { 31, 23, 82, 46, 13, 17, 30, 71, 22, 51 };
  bool ok = true;

  int buckets;

  tracker_allocator_counter::reset();
  {
    Container c;
    buckets = c.bucket_count();
    ok = check_construct_destroy("empty container", buckets, 0) && ok;
  }
  ok = check_construct_destroy("empty container", buckets, buckets) && ok;


  tracker_allocator_counter::reset();
  {
    Container c(arr10, arr10 + 10);
    ok = check_construct_destroy("Construct from range", buckets+10, 0) && ok;
  }
  ok = check_construct_destroy("Construct from range", buckets+10, buckets+10) && ok;

  tracker_allocator_counter::reset();
  {
    Container c(arr10, arr10 + 10);
    c.insert(arr10a[0]);
    ok = check_construct_destroy("Insert element", buckets+11, 0) && ok;
  }
  ok = check_construct_destroy("Insert element", buckets+11, buckets+11) && ok;

  tracker_allocator_counter::reset();
  {
    Container c(arr10, arr10 + 10);
    c.insert(arr10a, arr10a+3);
    ok = check_construct_destroy("Insert short range", buckets+13, 0) && ok;
  }
  ok = check_construct_destroy("Insert short range", buckets+13, buckets+13) && ok;

  tracker_allocator_counter::reset();
  {
    Container c(arr10, arr10 + 10);
    c.insert(arr10a, arr10a+10);
    ok = check_construct_destroy("Insert long range", buckets+20, 0) && ok;
  }
  ok = check_construct_destroy("Insert long range", buckets+20, buckets+20) && ok;

  return ok ? 0 : 1;
}

