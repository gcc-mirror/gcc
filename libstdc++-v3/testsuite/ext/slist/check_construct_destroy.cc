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

#include <ext/slist>
#include <iterator>
#include <testsuite_allocator.h>

using namespace __gnu_test;

int main()
{
  typedef __gnu_cxx::slist<int, tracker_alloc<int> > Container;
  const int arr10[10] = { 2, 4, 1, 7, 3, 8, 10, 5, 9, 6 };
  bool ok = true;

  allocation_tracker::resetCounts();
  {
    Container c;
    ok = check_construct_destroy("empty container", 0, 0) && ok;
  }
  ok = check_construct_destroy("empty container", 0, 0) && ok;


  allocation_tracker::resetCounts();
  {
    Container c(arr10, arr10 + 10);
    ok = check_construct_destroy("Construct from range", 10, 0) && ok;
  }
  ok = check_construct_destroy("Construct from range", 10, 10) && ok;

  {
    Container c(arr10, arr10 + 10);
    allocation_tracker::resetCounts();
    c.insert(c.begin(), arr10[0]);
    ok = check_construct_destroy("Insert element", 1, 0) && ok;
  }
  ok = check_construct_destroy("Insert element", 1, 11) && ok;

  {
    Container c(arr10, arr10 + 10);
    allocation_tracker::resetCounts();
    Container::iterator i5 = c.begin();
    std::advance(i5, 5);
    c.insert(i5, arr10, arr10+3);
    ok = check_construct_destroy("Insert short range", 3, 0) && ok;
  }
  ok = check_construct_destroy("Insert short range", 3, 13) && ok;

  {
    Container c(arr10, arr10 + 10);
    allocation_tracker::resetCounts();
    Container::iterator i7 = c.begin();
    std::advance(i7, 5);
    c.insert(i7, arr10, arr10+10);
    ok = check_construct_destroy("Insert long range", 10, 0) && ok;
  }
  ok = check_construct_destroy("Insert long range", 10, 20) && ok;

  return ok ? 0 : 1;
}

