// 2004-07-26  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <ext/hash_set>
#include <functional>
#include <iterator>
#include <testsuite_allocator.h>

using namespace __gnu_test;

int main()
{
  typedef __gnu_cxx::hash_set<int, __gnu_cxx::hash<int>, std::equal_to<int>,
                              tracker_alloc<int> >
    Container;

  const int arr10[10]  = { 2, 4, 1, 7, 3, 8, 10, 5, 9, 6 };
  const int arr10a[10] = { 31, 23, 82, 46, 13, 17, 30, 71, 22, 51 };
  bool ok = true;

  int buckets;

  allocation_tracker::resetCounts();
  {
    Container c;
    buckets = c.bucket_count();
    ok = check_construct_destroy("empty container", buckets, 0) && ok;
  }
  ok = check_construct_destroy("empty container", buckets, buckets) && ok;


  allocation_tracker::resetCounts();
  {
    Container c(arr10, arr10 + 10);
    ok = check_construct_destroy("Construct from range", buckets+10, 0) && ok;
  }
  ok = check_construct_destroy("Construct from range", buckets+10, buckets+10) && ok;

  allocation_tracker::resetCounts();
  {
    Container c(arr10, arr10 + 10);
    c.insert(arr10a[0]);
    ok = check_construct_destroy("Insert element", buckets+11, 0) && ok;
  }
  ok = check_construct_destroy("Insert element", buckets+11, buckets+11) && ok;

  allocation_tracker::resetCounts();
  {
    Container c(arr10, arr10 + 10);
    c.insert(arr10a, arr10a+3);
    ok = check_construct_destroy("Insert short range", buckets+13, 0) && ok;
  }
  ok = check_construct_destroy("Insert short range", buckets+13, buckets+13) && ok;

  allocation_tracker::resetCounts();
  {
    Container c(arr10, arr10 + 10);
    c.insert(arr10a, arr10a+10);
    ok = check_construct_destroy("Insert long range", buckets+20, 0) && ok;
  }
  ok = check_construct_destroy("Insert long range", buckets+20, buckets+20) && ok;

  return ok ? 0 : 1;
}

