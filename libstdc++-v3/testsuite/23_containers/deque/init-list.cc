// Copyright (C) 2008 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
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

// { dg-options "-std=gnu++0x" }

#include <deque>
#include <testsuite_allocator.h>

using namespace __gnu_test;

int main()
{
  typedef std::deque<int, tracker_allocator<int> > Container;
  const int arr10[10] = { 2, 4, 1, 7, 3, 8, 10, 5, 9, 6 };
  bool ok = true;

  tracker_allocator_counter::reset();
  {
    Container c({ 2, 4, 1 });
    ok = check_construct_destroy("Construct from init-list", 3, 0) && ok;
    ok &= (c[0] == 2);
    ok &= (c[1] == 4);
  }
  ok = check_construct_destroy("Construct from init-list", 3, 3) && ok;

  {
    Container c(arr10, arr10 + 10);
    tracker_allocator_counter::reset();
    c.insert(c.begin() + 7, { 234, 42, 1 });
    ok = check_construct_destroy("Insert init-list", 3, 0) && ok;
    ok &= (c[7] == 234);
  }
  ok = check_construct_destroy("Insert init-list", 3, 13) && ok;

  {
    Container c;
    tracker_allocator_counter::reset();
    c = { 13, 0, 42 };
    ok = check_construct_destroy("Assign init-list", 3, 0) && ok;
    ok &= (c[0] == 13);
  }
  ok = check_construct_destroy("Assign init-list", 3, 3) && ok;

  return ok ? 0 : 1;;
}
