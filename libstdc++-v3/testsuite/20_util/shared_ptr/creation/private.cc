// { dg-do run { target c++11 } }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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
#include <new>

// The behaviour tested here relies on the resolution of LWG issue 2070

template<typename T> struct MyAlloc;

class Private
{
  Private() = default;
  Private(const Private&) = default;
  ~Private() = default;

  friend class MyAlloc<Private>;

public:
  int get() const { return 0; }
};

template<typename T>
struct MyAlloc : std::allocator<T>
{
  template<typename U>
    struct rebind { typedef MyAlloc<U> other; };

  MyAlloc() = default;
  MyAlloc(const MyAlloc&) = default;

  template<typename U>
    MyAlloc(const MyAlloc<U>&) { }

  void construct(T* p) { ::new((void*)p) T(); }
  void destroy(T* p) { p->~T(); }
};

int main()
{
  MyAlloc<Private> a;
  auto p = std::allocate_shared<Private>(a);
  return p->get();
}
