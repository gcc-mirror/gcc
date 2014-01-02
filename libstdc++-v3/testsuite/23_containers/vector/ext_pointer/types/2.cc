// Test for Container using non-standard pointer types.

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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


// { dg-options "-std=gnu++0x" }

// This is a copy of vector/types/1.cc with altered allocator.
// The operator+()s in this test initially failed the test -
// they stress the accurate recognition, by the compiler,
// of _Pointer_adapter's own pointer arithmetic functions,
// which have to match perfectly on the int type to get
// chosen by the compiler when it sees: _Pointer_adapter<T> + int, etc.

#include <vector>
#include <ext/extptr_allocator.h>

namespace N
{
  struct X { };

  template<typename T>
    X operator+(T, std::size_t)
    { return X(); }

  template<typename T>
    X operator-(T, T)
    { return X(); }
}

int main()
{
  std::vector<N::X, __gnu_cxx::_ExtPtr_allocator<N::X> > v(5);
  const std::vector<N::X, __gnu_cxx::_ExtPtr_allocator<N::X> > w(1);

  v[0];
  w[0];
  v.size();
  v.capacity();
  v.resize(1);
  v.insert(v.begin(), N::X());
  v.insert(v.begin(), 1, N::X());
  v.insert(v.begin(), w.begin(), w.end());
  v = w;

  return 0;
}
