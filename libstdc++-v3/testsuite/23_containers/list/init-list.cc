// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++0x" }

#include <list>
#include <testsuite_allocator.h>

using namespace __gnu_test;

int main()
{
  typedef std::list<int, tracker_allocator<int> > Container;
  const int arr10[10] = { 2, 4, 1, 7, 3, 8, 10, 5, 9, 6 };
  bool ok = true;

  tracker_allocator_counter::reset();
  {
    Container c({ 2, 4, 1 });
    ok = check_construct_destroy("Construct from init-list", 3, 0) && ok;
    Container::iterator i = c.begin();
    ok &= (*i++ == 2);
    ok &= (*i++ == 4);
  }
  ok = check_construct_destroy("Construct from init-list", 3, 3) && ok;

  {
    Container c(arr10, arr10 + 10);
    tracker_allocator_counter::reset();
    Container::iterator i = c.begin();
    ++i; ++i; ++i; ++i; ++i; ++i; ++i;
    c.insert(i, { 234, 42, 1 });
    ok = check_construct_destroy("Insert init-list", 3, 0) && ok;
    ok &= (*--i == 1);
    ok &= (*--i == 42);
  }
  ok = check_construct_destroy("Insert init-list", 3, 13) && ok;

  {
    Container c;
    tracker_allocator_counter::reset();
    c = { 13, 0, 42 };
    ok = check_construct_destroy("Assign init-list", 3, 0) && ok;
    Container::iterator i = c.begin();
    ok &= (*i++ == 13);
  }
  ok = check_construct_destroy("Assign init-list", 3, 3) && ok;

  return ok ? 0 : 1;;
}
