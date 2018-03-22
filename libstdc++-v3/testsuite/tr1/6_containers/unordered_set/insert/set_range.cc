// { dg-do run }

// 2005-2-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 6.3.4.3 unordered_set
// range insert

#include <string>
#include <iterator>
#include <algorithm>
#include <tr1/unordered_set>
#include <testsuite_hooks.h>

void test01()
{
  typedef std::tr1::unordered_set<std::string> Set;
  Set s;
  VERIFY(s.empty());

  const int N = 10;
  const std::string A[N] = { "red", "green", "blue", "violet", "cyan",
			     "magenta", "yellow", "orange", "pink", "gray" };

  s.insert(A+0, A+N);
  VERIFY(s.size() == static_cast<unsigned int>(N));
  VERIFY(std::distance(s.begin(), s.end()) == N);

  for (int i = 0; i < N; ++i) {
    std::string str = A[i];
    Set::iterator it = std::find(s.begin(), s.end(), str);
    VERIFY(it != s.end());
  }
}

void test02()
{
  typedef std::tr1::unordered_set<int> Set;
  Set s;
  VERIFY(s.empty());

  const int N = 8;
  const int A[N] = { 3, 7, 4, 8, 2, 4, 6, 7 };

  s.insert(A+0, A+N);
  VERIFY(s.size() == 6);
  VERIFY(std::distance(s.begin(), s.end()) == 6);

  VERIFY(std::count(s.begin(), s.end(), 2) == 1);
  VERIFY(std::count(s.begin(), s.end(), 3) == 1);
  VERIFY(std::count(s.begin(), s.end(), 4) == 1);
  VERIFY(std::count(s.begin(), s.end(), 6) == 1);
  VERIFY(std::count(s.begin(), s.end(), 7) == 1);
  VERIFY(std::count(s.begin(), s.end(), 8) == 1);
}

int main()
{
  test01();
  test02();
  return 0;
}
