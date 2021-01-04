// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace std;
using namespace __gnu_test;

// libstdc++/64441
void
test01()
{
  const char s[] = "abc";
  const std::regex re("(\\d+)|(\\w+)");

  std::cmatch m;
  VERIFY(regex_search_debug(s, m, re));

  std::tuple<bool, string, int, int> expected[] = {
      make_tuple(true, "abc", 0, 3),
      make_tuple(false, "", 3, 3),
      make_tuple(true, "abc", 0, 3),
      make_tuple(false, "", 3, 3),
  };
  for (size_t i = 0, n = m.size(); i <= n; ++i) {
      auto&& sub = m[i];
      VERIFY(sub.matched == std::get<0>(expected[i]));
      VERIFY(sub.str() == std::get<1>(expected[i]));
      VERIFY((sub.first - s) == std::get<2>(expected[i]));
      VERIFY((sub.second - s) == std::get<3>(expected[i]));
  }
}

// libstdc++/64781
void
test02()
{
  std::match_results<const char*> m;
  const char s[] = "a";
  VERIFY(regex_search_debug(s, m, std::regex("a")));

  VERIFY(m.size() == 1);

  VERIFY(m[0].first == s+0);
  VERIFY(m[0].second == s+1);
  VERIFY(m[0].matched == true);

  VERIFY(m[42].first == s+1);
  VERIFY(m[42].second == s+1);
  VERIFY(m[42].matched == false);
}

int
main()
{
  test01();
  test02();
  return 0;
}
