// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2013 Free Software Foundation, Inc.
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

// libstdc++/55043

#include <unordered_map>
#include <vector>

struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&) = default;
};

using hash = std::hash<int>;
using equal = std::equal_to<int>;

template<typename Alloc>
  using test_type = std::unordered_map<int, MoveOnly, hash, equal, Alloc>;

void test01()
{
  typedef test_type<std::allocator<MoveOnly>> uim;
  std::vector<uim> v;
  v.emplace_back(uim());
}

// Unordered containers don't use allocator_traits yet so need full
// Allocator interface, derive from std::allocator to get it.
template<typename T, bool R>
struct Alloc : std::allocator<T>
{
  template<typename U>
    struct rebind { typedef Alloc<U, R> other; };

  Alloc() = default;

  template<typename U>
    Alloc(const Alloc<U, R>&) { }

  typedef typename std::conditional<R, T&&, const T&>::type arg_type;

  void construct(T* p, arg_type) const
  { new((void*)p) T(); }
};

// verify is_copy_constructible depends on allocator
typedef test_type<Alloc<MoveOnly, true>> uim_rval;
static_assert(!std::is_copy_constructible<uim_rval>::value, "is not copyable");

typedef test_type<Alloc<MoveOnly, false>> uim_lval;
static_assert(std::is_copy_constructible<uim_lval>::value, "is copyable");
