// { dg-options "-std=gnu++14" }
// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

#include "./synopsis_c++11.cc"

namespace std {

  // C++14 24.5, iterator adaptors:
  template <class Iterator>
  reverse_iterator<Iterator> make_reverse_iterator(const Iterator&);
}
