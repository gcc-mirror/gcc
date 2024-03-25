// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

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

// basic_string_view contains

#include <string_view>

#if __STDC_HOSTED__
// This FTM is omitted since <string> is not freestanding.
# ifndef __cpp_lib_string_contains
#  error "Feature-test macro for contains missing in <string_view>"
# elif __cpp_lib_string_contains != 202011L
#  error "Feature-test macro for contains has wrong value in <string_view>"
# endif
#endif // HOSTED

void
test01()
{
    constexpr std::string_view haystack("no place for needles");

    static_assert(haystack.contains(std::string_view("")));
    static_assert(haystack.contains(std::string_view("no")));
    static_assert(haystack.contains(std::string_view("needles")));
    static_assert(haystack.contains(std::string_view(" for ")));
    static_assert(!haystack.contains(std::string_view("places")));

    static_assert(!haystack.contains('\0'));
    static_assert(haystack.contains('n'));
    static_assert(haystack.contains('e'));
    static_assert(haystack.contains('s'));
    static_assert(!haystack.contains('x'));

    static_assert(haystack.contains(""));
    static_assert(haystack.contains("no"));
    static_assert(haystack.contains("needles"));
    static_assert(haystack.contains(" for "));
    static_assert(!haystack.contains("places"));

    constexpr std::string_view nothing;
    static_assert(nothing.contains(""));
    static_assert(!nothing.contains('\0'));
}
