// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

#include <atomic>

using std::atomic;

template<typename T>
constexpr bool check(int macro)
{
  return std::atomic<T>::is_always_lock_free == (macro == 2);
}

static_assert( check<bool>(ATOMIC_BOOL_LOCK_FREE) );
static_assert( check<char>(ATOMIC_CHAR_LOCK_FREE) );
static_assert( check<signed char>(ATOMIC_CHAR_LOCK_FREE) );
static_assert( check<unsigned char>(ATOMIC_CHAR_LOCK_FREE) );
static_assert( check<int>(ATOMIC_INT_LOCK_FREE) );
static_assert( check<unsigned int>(ATOMIC_INT_LOCK_FREE) );
static_assert( check<long>(ATOMIC_LONG_LOCK_FREE) );
static_assert( check<unsigned long>(ATOMIC_LONG_LOCK_FREE) );
static_assert( check<long long>(ATOMIC_LLONG_LOCK_FREE) );
static_assert( check<unsigned long long>(ATOMIC_LLONG_LOCK_FREE) );
static_assert( check<wchar_t>(ATOMIC_WCHAR_T_LOCK_FREE) );
#ifdef _GLIBCXX_USE_CHAR8_T
static_assert( check<char8_t>(ATOMIC_CHAR8_T_LOCK_FREE) );
#endif
static_assert( check<char16_t>(ATOMIC_CHAR16_T_LOCK_FREE) );
static_assert( check<char32_t>(ATOMIC_CHAR32_T_LOCK_FREE) );
