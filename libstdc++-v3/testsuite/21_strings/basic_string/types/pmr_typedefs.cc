// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }
// { dg-require-effective-target cxx11_abi }

#include <string>

std::pmr::string s = "polly";

struct T : std::char_traits<char> { };

static_assert(std::is_same_v<std::pmr::basic_string<char>,
    std::basic_string<char, std::char_traits<char>,
		      std::pmr::polymorphic_allocator<char>>>);
static_assert(std::is_same_v<std::pmr::basic_string<char, T>,
    std::basic_string<char, T, std::pmr::polymorphic_allocator<char>>>);

static_assert(std::is_same_v<std::pmr::string,
    std::basic_string<char, std::char_traits<char>,
		      std::pmr::polymorphic_allocator<char>>>);
#ifdef _GLIBCXX_USE_CHAR8_T
static_assert(std::is_same_v<std::pmr::u8string,
    std::basic_string<char8_t, std::char_traits<char8_t>,
		      std::pmr::polymorphic_allocator<char8_t>>>);
#endif
static_assert(std::is_same_v<std::pmr::u16string,
    std::basic_string<char16_t, std::char_traits<char16_t>,
		      std::pmr::polymorphic_allocator<char16_t>>>);
static_assert(std::is_same_v<std::pmr::u32string,
    std::basic_string<char32_t, std::char_traits<char32_t>,
		      std::pmr::polymorphic_allocator<char32_t>>>);

static_assert(std::is_same_v<std::pmr::basic_string<wchar_t>,
    std::basic_string<wchar_t, std::char_traits<wchar_t>,
		      std::pmr::polymorphic_allocator<wchar_t>>>);
static_assert(std::is_same_v<std::pmr::basic_string<wchar_t, T>,
    std::basic_string<wchar_t, T, std::pmr::polymorphic_allocator<wchar_t>>>);

static_assert(std::is_same_v<std::pmr::wstring,
    std::basic_string<wchar_t, std::char_traits<wchar_t>,
		      std::pmr::polymorphic_allocator<wchar_t>>>);
