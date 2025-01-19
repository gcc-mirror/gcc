// { dg-do compile { target c++14 } }

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

#include <experimental/string_view>
#include <testsuite_containers.h>

namespace __gnu_test
{
  template<typename _Tp1, typename _Tp2>
    struct traits<std::experimental::basic_string_view<_Tp1, _Tp2>> : public traits_base
    {
      typedef std::true_type    is_container;
      typedef std::true_type    is_reversible;
    };
}

#include <testsuite_containers.h>

// Check container for required typedefs.

__gnu_test::basic_types<std::experimental::string_view> t1b;
__gnu_test::reversible_types<std::experimental::string_view> t1r;
typedef typename std::experimental::string_view::traits_type traits_type1;

__gnu_test::basic_types<std::experimental::wstring_view> t2b;
__gnu_test::reversible_types<std::experimental::wstring_view> t2r;
typedef typename std::experimental::wstring_view::traits_type traits_type2;

static_assert(
    std::is_same<std::experimental::string_view::pointer, char*>(),
    "pointer should be value_type*");
static_assert(
    std::is_same<std::experimental::string_view::const_pointer, const char*>(),
    "const_pointer should be const value_type*");
static_assert(
    std::is_same<std::experimental::string_view::reference, char&>(),
    "reference should be value_type&");
static_assert(
    std::is_same<std::experimental::string_view::const_reference, const char&>(),
    "const_reference should be const value_type&");
