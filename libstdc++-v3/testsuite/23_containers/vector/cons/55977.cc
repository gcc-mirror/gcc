// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <memory>
#include <utility>
#include <vector>
#include <iterator>

template <class T>
struct MyAllocator
{
  std::allocator<T> base;
  typedef T value_type;
  MyAllocator() = default;
  template <class U>
  MyAllocator(const MyAllocator<U>& other) : base(other.base) {}
  T* allocate(std::size_t n) { return base.allocate(n); }
  void deallocate(T* p, std::size_t n) { return base.deallocate(p, n); }
  template <class U, class... Args>
  void construct(U* p, Args&&... args)
  {
    ::new (static_cast<void*>(p)) U(std::forward<Args>(args)...);
  }
};

struct A
{
private:
  friend class MyAllocator<A>;
  A(int value) : value(value) {}
  int value;
public:
  A() : value() {}
  int get() const { return value; }
};

void foo()
{
  std::vector<A, MyAllocator<A>> v1;
  const int i = 1;
  v1.emplace_back(i); // OK
  std::vector<A, MyAllocator<A>> v2(std::istream_iterator<int>(), {}); // ERROR
}
