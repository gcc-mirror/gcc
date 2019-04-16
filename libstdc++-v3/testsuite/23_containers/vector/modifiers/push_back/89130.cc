// Copyright (C) 2019 Free Software Foundation, Inc.
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

#include <vector>

struct T
{
  T() { }
  T(const T&) { }
  T(T&&) = delete;  // this means T is not MoveInsertable into std::vector<T>
};

void f()
{
  const T val;
  std::vector<T> x;
  // push_back(const T&) only requires T is CopyInsertable into std::vector<T>:
  x.push_back(val);
}

template<typename U>
struct Alloc
{
  using value_type = U;
  Alloc() = default;
  Alloc(const Alloc&) = default;
  template<typename U2>
    Alloc(const Alloc<U2>&) { }

  U* allocate(unsigned n) { return std::allocator<U>().allocate(n); }
  void deallocate(U* p, unsigned n) { std::allocator<U>().deallocate(p, n); }

  void construct(Alloc*, U* p, U&& u)
  {
    // construct from const lvalue instead of rvalue:
    ::new(p) U(const_cast<const U&>(u));
  }
};

void g()
{
  const T val;
  std::vector<T, Alloc<T>> x;
  // push_back(const T&) only requires T is CopyInsertable into std::vector<T>:
  x.push_back(val);
}
