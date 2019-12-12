// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }
// { dg-require-effective-target cxx11-abi }

#include <regex>
#include <memory_resource>

struct X;
static_assert(std::is_same_v<std::pmr::match_results<X*>,
    std::match_results<X*,
      std::pmr::polymorphic_allocator<std::sub_match<X*>>>>);

static_assert(std::is_same_v<std::pmr::cmatch,
    std::match_results<const char*,
      std::pmr::polymorphic_allocator<std::sub_match<const char*>>>>);
static_assert(std::is_same_v<std::pmr::smatch,
    std::match_results<std::pmr::string::const_iterator,
      std::pmr::polymorphic_allocator<
	std::sub_match<std::pmr::string::const_iterator>>>>);
#ifdef _GLIBCXX_USE_WCHAR_T
static_assert(std::is_same_v<std::pmr::wcmatch,
    std::match_results<const wchar_t*,
      std::pmr::polymorphic_allocator<std::sub_match<const wchar_t*>>>>);
static_assert(std::is_same_v<std::pmr::wsmatch,
    std::match_results<std::pmr::wstring::const_iterator,
      std::pmr::polymorphic_allocator<
	std::sub_match<std::pmr::wstring::const_iterator>>>>);
#endif
