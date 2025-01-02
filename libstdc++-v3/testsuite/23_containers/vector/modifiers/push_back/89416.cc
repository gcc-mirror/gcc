// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// PR libstdc++/89416

#include <vector>

template<typename T>
  struct Alloc : std::allocator<T>
  {
    using std::allocator<T>::allocator;

    template<typename U>
      struct rebind { using other = Alloc<U>;  };
  };

struct X
{
  X(int);
  X(X&&);
};

void test01()
{
  std::vector<X, Alloc<X>> V;
  V.push_back(X(1));
  V.emplace_back(1);
}
