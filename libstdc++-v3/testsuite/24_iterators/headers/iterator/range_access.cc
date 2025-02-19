// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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
# define NOTHROW(F) noexcept(noexcept(F))
#else
# define NOTHROW(F)
#endif

#if __cplusplus >= 201703L
# define CONSTEXPR_17 constexpr
#else
# define CONSTEXPR_17
#endif

#if __cplusplus >= 201402L
# define CONSTEXPR_14 constexpr
#else
# define CONSTEXPR_14
#endif

namespace std
{
  template<class C>
    CONSTEXPR_17 auto
    begin(C& c) NOTHROW(c.begin()) -> decltype(c.begin());
  template<class C>
    CONSTEXPR_17 auto
    begin(const C& c) NOTHROW(c.begin()) -> decltype(c.begin());

  template<class C>
    CONSTEXPR_17 auto
    end(C& c) NOTHROW(c.end()) -> decltype(c.end());
  template<class C>
    CONSTEXPR_17 auto
    end(const C& c) NOTHROW(c.end()) -> decltype(c.end());

  template<class T, size_t N>
    CONSTEXPR_14 T*
    begin(T (&array)[N]) noexcept;
  template<class T, size_t N>
    CONSTEXPR_14 T*
    end(T (&array)[N]) noexcept;

#if __cplusplus >= 201402L
  template<class C>
    constexpr auto
    cbegin(const C& c) noexcept(noexcept(std::begin(c))) -> decltype(c.begin());
  template<class C>
    constexpr auto
    cend(const C& c) noexcept(noexcept(std::end(c)))-> decltype(c.end());

  template<class C>
    CONSTEXPR_17 auto
    rbegin(C& c) NOTHROW(c.rbegin()) -> decltype(c.rbegin());
  template<class C>
    CONSTEXPR_17 auto
    rbegin(const C& c) NOTHROW(c.rbegin()) -> decltype(c.rbegin());

  template<class C>
    CONSTEXPR_17 auto
    rend(C& c) NOTHROW(c.rend()) -> decltype(c.rend());
  template<class C>
    CONSTEXPR_17 auto
    rend(const C& c) NOTHROW(c.rend()) -> decltype(c.rend());

  template<class T, size_t N>
    CONSTEXPR_17 reverse_iterator<T*>
    rbegin(T (&array)[N]) noexcept;
  template<class T, size_t N>
    CONSTEXPR_17 reverse_iterator<T*>
    rend(T (&array)[N]) noexcept;

  template<class E>
    CONSTEXPR_17 reverse_iterator<const E*>
    rbegin(initializer_list<E>) noexcept;
  template<class E>
    CONSTEXPR_17 reverse_iterator<const E*>
    rend(initializer_list<E>) noexcept;

  template<class C>
    CONSTEXPR_17 auto
    crbegin(const C& c) NOTHROW(std::rbegin(c)) -> decltype(std::rbegin(c));
  template<class C>
    CONSTEXPR_17 auto
    cend(const C& c) NOTHROW(std::rend(c)) -> decltype(std::rend(c));
#endif // C++14
}
