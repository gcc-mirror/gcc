// { dg-options "-std=gnu++14" }
// { dg-do compile }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

namespace std
{
  template<class C> auto begin(C& c) -> decltype(c.begin());
  template<class C> auto begin(const C& c) -> decltype(c.begin());

  template<class C> auto end(C& c) -> decltype(c.end());
  template<class C> auto end(const C& c) -> decltype(c.end());

  template<class T, size_t N> constexpr T* begin(T (&array)[N]);
  template<class T, size_t N> constexpr T* end(T (&array)[N]);

  template<class C> auto cbegin(const C& c) -> decltype(c.begin());
  template<class C> auto cend(const C& c) -> decltype(c.end());

  template<class C> auto rbegin(C& c) -> decltype(c.rbegin());
  template<class C> auto rbegin(const C& c) -> decltype(c.rbegin());

  template<class C> auto rend(C& c) -> decltype(c.rend());
  template<class C> auto rend(const C& c) -> decltype(c.rend());

  template<class T, size_t N>
    reverse_iterator<T*> rbegin(T (&array)[N]);
  template<class T, size_t N>
    reverse_iterator<T*> rend(T (&array)[N]);

  template<class E>
    reverse_iterator<const E*> rbegin(initializer_list<E>);
  template<class E>
    reverse_iterator<const E*> rend(initializer_list<E>);

  template<class C>
    auto crbegin(const C& c) -> decltype(std::rbegin(c));
  template<class C>
    auto cend(const C& c) -> decltype(std::rend(c));
}
