// { dg-do compile { target c++11 } }

// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

template <class T> struct B;

template <class T> struct A
{
  A(A&&) = default;
  A(const B<T> &);
};

template <class T> struct B
{
  std::pair<A<T>,int> a;
  B(B&&) = default;
};

bool b = std::is_move_constructible<A<int> >::value;
