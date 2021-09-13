// { dg-do compile }

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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


#include <string>
#include <algorithm>

template<class T>
  class W
  {
  public:
    T data;
  };

template<class T>
  void
  swap(W<T>& x, W<T>& y)
  {
    using std::swap;
    swap(x.data, y.data);
  }

// DR 809. std::swap should be overloaded for array types.
void test01()
{
  W<std::string[8]> w1, w2;  // Two objects of a Swappable type.
    
  using std::swap;
  swap(w1, w2);
}
