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

// { dg-do compile { target c++14 } }
// { dg-require-effective-target cxx11_abi }

#include <experimental/regex>

namespace xpmr = std::experimental::pmr;

struct X;
static_assert(std::is_same<xpmr::match_results<X*>,
    std::match_results<X*,
      xpmr::polymorphic_allocator<std::sub_match<X*>>>>::value,
    "pmr::match_results");

static_assert(std::is_same<xpmr::cmatch,
    std::match_results<const char*,
      xpmr::polymorphic_allocator<std::sub_match<const char*>>>>::value,
    "pmr::cmatch");
static_assert(std::is_same<xpmr::smatch,
    std::match_results<xpmr::string::const_iterator,
      xpmr::polymorphic_allocator<
	std::sub_match<xpmr::string::const_iterator>>>>::value,
    "pmr::smatch");
#ifdef _GLIBCXX_USE_WCHAR_T
static_assert(std::is_same<xpmr::wcmatch,
    std::match_results<const wchar_t*,
      xpmr::polymorphic_allocator<std::sub_match<const wchar_t*>>>>::value,
    "pmr::wcmatch");
static_assert(std::is_same<xpmr::wsmatch,
    std::match_results<xpmr::wstring::const_iterator,
      xpmr::polymorphic_allocator<
	std::sub_match<xpmr::wstring::const_iterator>>>>::value,
    "pmr::wsmatch");
#endif
