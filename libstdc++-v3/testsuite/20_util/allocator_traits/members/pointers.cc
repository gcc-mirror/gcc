// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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
// { dg-require-effective-target hosted }

#include <memory>

// Non-type template param means pointer_traits::rebind can't be instantiated.
template<typename T, int = 0>
  struct Pointer
  {
    using element_type = T;
    Pointer(T* p = nullptr) : ptr(p) { }
    T* ptr;
  };

template<typename T>
  struct Alloc
  {
    using value_type = T;
    using pointer = Pointer<T>;
    using const_pointer = Pointer<const T>;
    using void_pointer = Pointer<void>;
    using const_void_pointer = Pointer<const void>;

    pointer allocate(std::size_t n)
    { return std::allocator<T>().allocate(n); }

    void allocate(pointer p, std::size_t n)
    { return std::allocator<T>().deallocate(p, n); }
  };

// The nested pointer types in Alloc should be found without attempting to
// instantiate pointer_traits::rebind (which would fail):
std::allocator_traits<Alloc<int>>::pointer p;
std::allocator_traits<Alloc<int>>::const_pointer cp;
std::allocator_traits<Alloc<int>>::void_pointer vp;
std::allocator_traits<Alloc<int>>::const_void_pointer cvp;
