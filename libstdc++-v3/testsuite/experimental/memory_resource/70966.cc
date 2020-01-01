// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/memory_resource>

namespace pmr = std::experimental::pmr;

struct X
{
  pmr::memory_resource* res = nullptr;
  void* ptr = nullptr;
  static constexpr std::size_t n = 64;

  constexpr X() { }

  explicit
  X(pmr::memory_resource* r) : res(r), ptr(r->allocate(n)) { }

  ~X() { if (ptr) res->deallocate(ptr, n); }
};

void
swap(X& lhs, X& rhs) {
    std::swap(lhs.res, rhs.res);
    std::swap(lhs.ptr, rhs.ptr);
}

void
test01()
{
  static X x1;
  X x2(pmr::new_delete_resource());
  swap(x1, x2);
  // Now x1 will deallocate the memory during destruction of static objects.
}

int main()
{
  test01();
}
