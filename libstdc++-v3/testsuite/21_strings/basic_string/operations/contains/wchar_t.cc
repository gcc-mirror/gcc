// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

// Copyright (C) 2021-2023 Free Software Foundation, Inc.
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
    const std::wstring haystack(L"no place for needles");

    VERIFY(haystack.contains(std::wstring(L"")));
    VERIFY(haystack.contains(std::wstring(L"no")));
    VERIFY(haystack.contains(std::wstring(L"needles")));
    VERIFY(haystack.contains(std::wstring(L" for ")));
    VERIFY(!haystack.contains(std::wstring(L"places")));

    VERIFY(haystack.contains(std::wstring_view(L"")));
    VERIFY(haystack.contains(std::wstring_view(L"no")));
    VERIFY(haystack.contains(std::wstring_view(L"needles")));
    VERIFY(haystack.contains(std::wstring_view(L" for ")));
    VERIFY(!haystack.contains(std::wstring_view(L"places")));

    VERIFY(!haystack.contains('\0'));
    VERIFY(haystack.contains('n'));
    VERIFY(haystack.contains('e'));
    VERIFY(haystack.contains('s'));
    VERIFY(!haystack.contains('x'));

    VERIFY(haystack.contains(L""));
    VERIFY(haystack.contains(L"no"));
    VERIFY(haystack.contains(L"needles"));
    VERIFY(haystack.contains(L" for "));
    VERIFY(!haystack.contains(L"places"));

    const std::wstring nothing;
    VERIFY(nothing.contains(L""));
    VERIFY(!nothing.contains('\0'));
}

int
main()
{
  test01();
  return 0;
}
