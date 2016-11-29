// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

#include <type_traits>

// libstdc++/60326

using namespace std;
#ifdef _GLIBCXX_USE_WCHAR_T
using wchar_signed = make_signed<wchar_t>::type;
using wchar_unsigned = make_unsigned<wchar_t>::type;
static_assert( !is_same<wchar_signed, wchar_unsigned>::value, "wchar_t" );
#endif
static_assert( is_signed<make_signed<char16_t>::type>::value, "char16_t");
static_assert( is_signed<make_signed<char32_t>::type>::value, "char32_t");
