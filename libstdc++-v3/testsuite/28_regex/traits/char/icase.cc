// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// 28.7 Class template regex_traits [re.traits]

#include <regex>
#include <testsuite_hooks.h>

using namespace std;

void
test01()
{
  {
    regex re("[T-f]", regex::icase);

    VERIFY(regex_match("A", re));
    VERIFY(regex_match("F", re));
    VERIFY(regex_match("a", re));
    VERIFY(regex_match("f", re));

    VERIFY(!regex_match("G", re));
    VERIFY(!regex_match("S", re));
    VERIFY(!regex_match("g", re));
    VERIFY(!regex_match("s", re));

    VERIFY(regex_match("T", re));
    VERIFY(regex_match("Z", re));
    VERIFY(regex_match("t", re));
    VERIFY(regex_match("z", re));
  }
  // icase works with std::regex_traits<>, because we know how it's implemented.
  {
    regex re("[T-f]", regex::icase | regex::collate);

    VERIFY(regex_match("A", re));
    VERIFY(regex_match("F", re));
    VERIFY(regex_match("a", re));
    VERIFY(regex_match("f", re));

    VERIFY(!regex_match("G", re));
    VERIFY(!regex_match("S", re));
    VERIFY(!regex_match("g", re));
    VERIFY(!regex_match("s", re));

    VERIFY(regex_match("T", re));
    VERIFY(regex_match("Z", re));
    VERIFY(regex_match("t", re));
    VERIFY(regex_match("z", re));
  }
}

int main()
{
  test01();
  return 0;
}
