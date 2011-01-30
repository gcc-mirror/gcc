// { dg-require-profile-mode "" }

// -*- C++ -*-

// Unit tests for profile/impl/profile_algos.h.

// Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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

#include <vector>
#include <profile/impl/profiler.h>

using std::_GLIBCXX_STD_C::vector;

enum Failure
{
  NO_FAILURES = 0x0,
  INSERT_AFTER_N = 0x1,
  INSERT_AT_HEAD = 0x2,
  INSERT_AT_TAIL = 0x4,
  INSERT_IN_THE_MIDDLE = 0x8,
  TOP_N = 0x10,
  FOR_EACH = 0x20,
  REMOVE = 0x40
};
  

static int
test_insert_top_n()
{
  vector<int> v;

  for (int i = 0; i < 10; i++)
    v.push_back(10 - i);

  // Inserting -5 should have no effect if size is limited to 10.
  __gnu_profile::__insert_top_n(v, -5, 10);
  for (int i = 0; i < 10; i++)
    if (v[i] != 10 - i)
      return INSERT_AFTER_N;

  // Insert at head.
  __gnu_profile::__insert_top_n(v, 11, 10);
  for (int i = 0; i < 11; i++)
    if (v[i] != 11 - i)
      return INSERT_AT_HEAD;

  // Insert at end.
  __gnu_profile::__insert_top_n(v, 0, 100);
  for (int i = 0; i < 12; i++)
    if (v[i] != 11 - i)
      return INSERT_AT_TAIL;

  // Insert in the middle.
  __gnu_profile::__insert_top_n(v, 6, 11);
  for (int i = 0; i < 6; i++)
    if (v[i] != 11 - i)
      return INSERT_IN_THE_MIDDLE;
  for (int i = 6; i < 13; i++)
    if (v[i] != 12 - i)
      return INSERT_IN_THE_MIDDLE;

  return NO_FAILURES;
}

static int
test_top_n()
{
  vector<int> v, out;

  for (int i = 0; i < 100; i++)
    {
      v.push_back(100 + i);
      v.push_back(100 - i);
    }

  __gnu_profile::__top_n(v, out, 10);

  for (int i = 0; i < 10; i++)
    if (out[i] != 199 - i)
      return TOP_N;

  return NO_FAILURES;
}

struct test_for_each_helper
{
  static int sum;
  void operator ()(int i) { 
    sum += i;
  }
};

int test_for_each_helper::sum = 0;

static int
test_for_each()
{
  vector<int> v;
  test_for_each_helper helper;
  int checksum = 0;

  for (int i = 0; i < 10; i++)
    {
      v.push_back(i);
      checksum += i;
    }

  __gnu_profile::__for_each(v.begin(), v.end(), helper);

  return helper.sum == checksum ? NO_FAILURES : FOR_EACH;
}

static int
test_remove()
{
  vector<char> v;

  for (int i = 0; i < 10; i++)
    v.push_back(' ');
  v.push_back('x');
  for (int i = 0; i < 10; i++)
    v.push_back(' ');
  v.push_back('x');

  return __gnu_profile::__remove(v.begin(), v.end(), ' ') == v.begin() + 2
      ? NO_FAILURES : REMOVE;
}

int main()
{
  return test_insert_top_n() | test_top_n() | test_for_each() | test_remove();
}
