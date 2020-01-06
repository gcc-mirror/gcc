// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

// { dg-options "-fno-rtti" }
// { dg-do run { target c++11 } }

#include <memory>
#include <testsuite_hooks.h>

int counter = 0;

template<typename T>
struct Alloc : std::allocator<T>
{
  template<typename U>
    struct rebind { using other = Alloc<U>; };

  Alloc() = default;

  template<typename U>
    Alloc(const Alloc<U>&) { }

  T* allocate(std::size_t n)
  {
    ++counter;
    return std::allocator<T>::allocate(n);
  }
};


void
test01()
{
  std::allocate_shared<int>(Alloc<int>());
  VERIFY( counter == 1 );
}

int
main()
{
  test01();
}
