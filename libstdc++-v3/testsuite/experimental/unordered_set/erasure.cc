// { dg-do run { target c++14 } }

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <experimental/unordered_set>
#include <testsuite_hooks.h>

void
test01()
{
  auto is_odd = [](const int i) { return i % 2 != 0; };

  std::unordered_set<int> us{ 10, 11, 12, 14, 15, 17, 18, 19 };
  std::experimental::erase_if(us, is_odd);
  std::unordered_set<int> t{ 10, 12, 14, 18 };
  VERIFY( us == t );
}

void
test02()
{
  auto is_odd = [](const int i) { return i % 2 != 0; };

  std::unordered_multiset<int> ums{ 20, 21, 22, 22, 23, 23, 24, 25 };
  std::experimental::erase_if(ums, is_odd);
  std::unordered_multiset<int> t{ 20, 22, 22, 24 };
  VERIFY( ums == t );
}

void
test_pr107850()
{
  // Predicate only callable as non-const.
  struct Pred { bool operator()(const int&) { return false; } };
  const Pred pred; // erase_if parameter is passed by value, so non-const.
  std::unordered_set<int> s;
  std::experimental::erase_if(s, pred);
  std::unordered_multiset<int> ms;
  std::experimental::erase_if(ms, pred);
}

int
main()
{
  test01();
  test02();
  test_pr107850();

  return 0;
}
