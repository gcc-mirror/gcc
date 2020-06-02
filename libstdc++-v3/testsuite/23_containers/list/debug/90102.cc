// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <debug/list>

// PR libstdc++/90102

struct AnyCont
{
  template<class Cont, class Check = decltype(std::declval<Cont>().clear())>
  operator Cont () const;
} a;

// This should use copy constructor, not be ambiguous
__gnu_debug::list<int> c(a);

// Ensure construction from base container still works
__gnu_debug::list<int> d(static_cast<std::list<int>>(a));
