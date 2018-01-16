// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

#include <utility>

namespace std {
  //  lib.operators, operators:
  namespace rel_ops {
    template<class T> bool operator!=(const T&, const T&);
    template<class T> bool operator> (const T&, const T&);
    template<class T> bool operator<=(const T&, const T&);
    template<class T> bool operator>=(const T&, const T&);
  }

  //  lib.pairs, pairs:
  template <class T1, class T2> struct pair;
  template <class T1, class T2>
  _GLIBCXX_CONSTEXPR bool operator==(const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  _GLIBCXX_CONSTEXPR bool operator< (const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  _GLIBCXX_CONSTEXPR bool operator!=(const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  _GLIBCXX_CONSTEXPR bool operator> (const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  _GLIBCXX_CONSTEXPR bool operator>=(const pair<T1,T2>&, const pair<T1,T2>&);
  template <class T1, class T2>
  _GLIBCXX_CONSTEXPR bool operator<=(const pair<T1,T2>&, const pair<T1,T2>&);
}
