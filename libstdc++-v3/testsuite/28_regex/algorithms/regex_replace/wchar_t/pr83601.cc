// { dg-do run { target c++11 } }

// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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
void frep(const wchar_t *istr, const wchar_t *rstr, const wchar_t *ostr) {
    std::basic_regex<wchar_t> wrgx(L"(a*)(b+)");
    std::basic_string<wchar_t> wstr = istr, wret = ostr, test;
    std::regex_replace(std::back_inserter(test), wstr.begin(), wstr.end(),
                       wrgx, std::basic_string<wchar_t>(rstr),
                       std::regex_constants::format_sed);
    VERIFY(test == wret);
}

int main() {
    frep(L"xbbyabz", L"!\\\\2!", L"x!\\2!y!\\2!z");
    frep(L"xbbyabz", L"!\\\\0!", L"x!\\0!y!\\0!z");
    frep(L"xbbyabz", L"!\\&!", L"x!&!y!&!z");
    return 0;
}
