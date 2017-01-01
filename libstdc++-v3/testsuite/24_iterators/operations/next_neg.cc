// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

// { dg-options "-D_GLIBCXX_CONCEPT_CHECKS" }
// { dg-do compile { target c++11 } }

#include <iterator>

struct X {};

namespace std
{
  template<>
    struct iterator_traits<const X*> : iterator_traits<X*>
    {
      using iterator_category = input_iterator_tag;
      using reference = const X&;
      using pointer = const X*;
    };
}

void
test01()
{
  const X array[1] = { };
  std::next(array);
  // { dg-error "input_iterator" "" { target *-*-* } 220 }
}
