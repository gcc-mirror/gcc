// Copyright (C) 2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <functional>

// This type is not CopyConstructible because copying a non-const lvalue
// will call the throwing constructor.
struct A
{
  A() = default;
  A(const A&) { } // not trivial, so allocated on the heap by std::function
  A(A&) { throw 1; }
  void operator()() const { }
};

int main()
{
  const A a{};
  // Undefined, because std::function requires CopyConstructible:
  std::function<void()> f(a);
  // This will throw if the object is copied as non-const:
  std::function<void()> g(f);
}
