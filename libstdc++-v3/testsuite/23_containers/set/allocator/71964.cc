// Copyright (C) 2016 Free Software Foundation, Inc.
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

#include <set>
#include <testsuite_hooks.h>

template<typename T>
  struct mv_allocator
  {
    using value_type = T;
    using size_type = unsigned;

    mv_allocator()
    : moved_to(false), moved_from(false) { }

    template<typename U>
      mv_allocator(const mv_allocator<U> & a)
      : moved_to(a.moved_to), moved_from(a.moved_from) { }

    mv_allocator(const mv_allocator &) = default;

    mv_allocator(mv_allocator && a) noexcept : moved_to(true)
    {
      a.moved_from = true;
    }

    T* allocate(unsigned n) { return std::allocator<T>{}.allcoate(n); }
    void deallocate(T* p, unsigned n) { std::allocator<T>{}.deallocate(p, n); }

    bool moved_to;
    bool moved_from;
  };

template<typename T, typename U>
bool
operator==(const mv_allocator<T>&, const mv_allocator<U>&) { return true; }

template<typename T, typename U>
bool
operator!=(const mv_allocator<T>&, const mv_allocator<U>&) { return false; }

void
test01()
{
  std::set<int, std::less<int>, mv_allocator<int>> s;
  auto t = std::move(s);
  VERIFY( s.get_allocator().moved_from );
  VERIFY( t.get_allocator().moved_to );
}

int
main()
{
  test01();
}
