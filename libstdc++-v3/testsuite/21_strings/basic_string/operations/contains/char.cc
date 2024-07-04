// { dg-do run { target c++23 } }

// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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

// basic_string contains

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
    const std::string haystack("no place for needles");

    VERIFY(haystack.contains(std::string("")));
    VERIFY(haystack.contains(std::string("no")));
    VERIFY(haystack.contains(std::string("needles")));
    VERIFY(haystack.contains(std::string(" for ")));
    VERIFY(!haystack.contains(std::string("places")));

    VERIFY(haystack.contains(std::string_view("")));
    VERIFY(haystack.contains(std::string_view("no")));
    VERIFY(haystack.contains(std::string_view("needles")));
    VERIFY(haystack.contains(std::string_view(" for ")));
    VERIFY(!haystack.contains(std::string_view("places")));

    VERIFY(!haystack.contains('\0'));
    VERIFY(haystack.contains('n'));
    VERIFY(haystack.contains('e'));
    VERIFY(haystack.contains('s'));
    VERIFY(!haystack.contains('x'));

    VERIFY(haystack.contains(""));
    VERIFY(haystack.contains("no"));
    VERIFY(haystack.contains("needles"));
    VERIFY(haystack.contains(" for "));
    VERIFY(!haystack.contains("places"));

    const std::string nothing;
    VERIFY(nothing.contains(""));
    VERIFY(!nothing.contains('\0'));
}

int
main()
{
  test01();
  return 0;
}
