// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

#include <scoped_allocator>

template<typename T>
struct alloc
{
  using value_type = T;
  alloc() = default;
  template<typename U>
    alloc(alloc<U>) { }
  T* allocate(std::size_t);
  void deallocate(T*, std::size_t);
};

template<typename T, typename U>
  bool operator==(alloc<T>, alloc<U>) { return true; }

template<typename T, typename U>
  bool operator!=(alloc<T>, alloc<U>) { return false; }

struct X
{
  using allocator_type = alloc<int>;
  X(const allocator_type&);
};

template<typename A>
struct nested_alloc : A
{
  nested_alloc() = default;
  template<typename U>
    nested_alloc(nested_alloc<U>) { }

  // Need to customize rebind, otherwise nested_alloc<alloc<T>> gets rebound
  // to nested_alloc<U>.
  template<typename U>
    struct rebind
    {
      using other = typename std::allocator_traits<A>::template rebind_alloc<U>;
    };

  A& outer_allocator() { return *this; }

  template<typename U, typename... Args>
    void construct(U*, Args&&...)
    {
      static_assert(!std::is_same<U, X>::value,
          "OUTERMOST should recurse and use alloc<int> to construct X");
    }
};

template<typename T, typename U>
  bool operator==(nested_alloc<T> l, nested_alloc<U> r)
  { return l.outer_allocator() == r.outer_allocator(); }

template<typename T, typename U>
  bool operator!=(nested_alloc<T> l, nested_alloc<U> r)
  { return !(l == r); }

template<typename A>
  using scoped_alloc = std::scoped_allocator_adaptor<A>;

void
test01()
{
  scoped_alloc<nested_alloc<alloc<int>>> a;
  alignas(X) char buf[sizeof(X)];
  X* p = (X*)buf;
  // Test that OUTERMOST is recursive and doesn't just unwrap one level:
  a.construct(p);
}

void
test02()
{
  scoped_alloc<scoped_alloc<nested_alloc<alloc<int>>>> a;
  alignas(X) char buf[sizeof(X)];
  X* p = (X*)buf;
  // Test that OUTERMOST is recursive and doesn't just unwrap one level:
  a.construct(p);
}
