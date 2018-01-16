// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

#include <memory>

// libstdc++/52924

struct A { };

struct D {
  ~D() noexcept(false) { }
  void operator()(A*) { }
};

template<typename T>
struct Alloc : std::allocator<T>
{
  Alloc() = default;
  ~Alloc() noexcept(false) { }
  template<typename U> Alloc(const Alloc<U>&) { }

  template<typename U>
    struct rebind
    { typedef Alloc<U> other; };
};

A a;
D d;
Alloc<A> al;

auto sd = std::shared_ptr<A>(&a, d);
auto sa = std::shared_ptr<A>(&a, d, al);
auto as = std::allocate_shared<A>(al);
