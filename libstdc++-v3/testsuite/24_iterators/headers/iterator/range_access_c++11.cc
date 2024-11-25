// { dg-options "-std=gnu++11" }
// { dg-do compile { target c++11_only } }

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

#include <iterator>

#ifdef _GLIBCXX_RELEASE
// Conditional noexcept on these functions is a libstdc++ extension
# define NOTHROW(F) noexcept(noexcept(c.F()))
#else
# define NOTHROW(F)
#endif

namespace std
{
  template<class C> auto begin(C& c) NOTHROW(begin) -> decltype(c.begin());
  template<class C> auto begin(const C& c) NOTHROW(begin) -> decltype(c.begin());

  template<class C> auto end(C& c) NOTHROW(end) -> decltype(c.end());
  template<class C> auto end(const C& c) NOTHROW(end) -> decltype(c.end());

  template<class T, size_t N> T* begin(T (&array)[N]) noexcept;
  template<class T, size_t N> T* end(T (&array)[N]) noexcept;
}
