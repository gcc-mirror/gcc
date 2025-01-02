// { dg-do compile { target c++17 } }

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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
  template<class C> constexpr auto begin(C& c) NOTHROW(begin) -> decltype(c.begin());
  template<class C> constexpr auto begin(const C& c) NOTHROW(begin) -> decltype(c.begin());

  template<class C> constexpr auto end(C& c) NOTHROW(end) -> decltype(c.end());
  template<class C> constexpr auto end(const C& c) NOTHROW(end) -> decltype(c.end());

  template<class T, size_t N> constexpr T* begin(T (&array)[N]) noexcept;
  template<class T, size_t N> constexpr T* end(T (&array)[N]) noexcept;

  template<class C> constexpr auto cbegin(const C& c) -> decltype(c.begin());
  template<class C> constexpr auto cend(const C& c) -> decltype(c.end());

  template<class C> constexpr auto rbegin(C& c) -> decltype(c.rbegin());
  template<class C> constexpr auto rbegin(const C& c) -> decltype(c.rbegin());

  template<class C> constexpr auto rend(C& c) -> decltype(c.rend());
  template<class C> constexpr auto rend(const C& c) -> decltype(c.rend());

  template<class T, size_t N>
    constexpr reverse_iterator<T*> rbegin(T (&array)[N]) noexcept;
  template<class T, size_t N>
    constexpr reverse_iterator<T*> rend(T (&array)[N]) noexcept;

  template<class E>
    constexpr reverse_iterator<const E*> rbegin(initializer_list<E>) noexcept;
  template<class E>
    constexpr reverse_iterator<const E*> rend(initializer_list<E>) noexcept;

  template<class C>
    constexpr auto crbegin(const C& c) -> decltype(std::rbegin(c));
  template<class C>
    constexpr auto cend(const C& c) -> decltype(std::rend(c));
}
