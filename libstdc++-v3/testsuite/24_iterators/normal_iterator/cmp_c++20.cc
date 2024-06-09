// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <iterator>
#include <vector>
#include <testsuite_allocator.h>

void
test01()
{
  using V = std::vector<int>;
  static_assert( std::totally_ordered<V::iterator> );
  static_assert( std::three_way_comparable<V::iterator> );
  using C = std::compare_three_way_result_t<V::iterator>;
  static_assert( std::same_as<C, std::strong_ordering> );

  static_assert( std::random_access_iterator<V::iterator> );
  static_assert( std::random_access_iterator<V::const_iterator> );
}

// User-defined pointer type that supports operator< but not operator<=>
template<typename T>
struct Pointer : __gnu_test::PointerBase<Pointer<T>, T>
{
  using __gnu_test::PointerBase<Pointer<T>, T>::PointerBase;

  friend bool operator<(const Pointer& lhs, const Pointer& rhs) noexcept
  { return lhs.value < rhs.value; }

  std::partial_ordering operator<=>(const Pointer&) const = delete;
};

// Minimal allocator using Pointer<T>
template<typename T>
struct Alloc
{
  typedef T value_type;
  typedef Pointer<T> pointer;

  Alloc() = default;
  template<typename U>
    Alloc(const Alloc<U>&) { }

  pointer allocate(std::size_t n)
  { return pointer(std::allocator<T>().allocate(n)); }

  void deallocate(pointer p, std::size_t n)
  { std::allocator<T>().deallocate(p.operator->(), n); }
};

void
test02()
{
  using V = std::vector<int, Alloc<int>>;
  static_assert( std::totally_ordered<V::iterator> );
  static_assert( std::three_way_comparable<V::iterator> );
  using C = std::compare_three_way_result_t<V::iterator>;
  static_assert( std::same_as<C, std::weak_ordering> );

  static_assert( std::random_access_iterator<V::iterator> );
  static_assert( std::random_access_iterator<V::const_iterator> );
}

void
test03()
{
  struct P : Pointer<int> {
    bool operator<(const P&) const = delete;
  };

  struct C {
    using pointer = P;
  };

  using I = __gnu_cxx::__normal_iterator<P, C>;
  static_assert( ! std::totally_ordered<I> );
  static_assert( ! std::three_way_comparable<I> );
}
