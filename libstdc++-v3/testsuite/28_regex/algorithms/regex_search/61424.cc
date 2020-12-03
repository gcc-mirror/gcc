// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// PR libstdc++/61424

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace std;
using namespace __gnu_test;

int main()
{
  regex_constants::syntax_option_type grammar[] = {
    regex_constants::ECMAScript, regex_constants::extended,
    regex_constants::awk, regex_constants::egrep
  };

  string sol[] = {
      "tour",
      "tournament",
      "tournament",
      "tournament",
  };
  int i = 0;
  for (auto g : grammar)
  {
    regex re("tour|tournament|tourn", g);
    const char str[] = "tournament";
    cmatch m;
    VERIFY(regex_search(str, m, re));
    // TODO: Fix ECMAScript BFS matcher.
    //VERIFY(regex_search_debug(str, m, re));
    VERIFY(sol[i] == m[0]);
    i++;
  }
}
