// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

#include <regex>
#include <testsuite_hooks.h>

// libstdc++/83601
int main() {
    auto format = std::regex_constants::format_sed;
    auto out = regex_replace("ab", std::regex("(a)(b)"), R"(\\1\&\\2)", format);
    VERIFY(out == R"(\1&\2)");

    return 0;
}
