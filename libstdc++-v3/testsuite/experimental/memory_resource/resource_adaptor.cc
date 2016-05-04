// { dg-options "-std=gnu++14" }

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

#include <experimental/memory_resource>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using std::experimental::pmr::memory_resource;
using std::experimental::pmr::resource_adaptor;

template<typename T>
  struct Allocator : __gnu_test::SimpleAllocator<T>
  {
    Allocator(int) { } // not default constructible

    template<typename U>
      Allocator(const Allocator<U>&) { }
  };

template<typename T>
  bool aligned(void* p)
  {
    return (reinterpret_cast<std::uintptr_t>(p) % alignof(T)) == 0;
  }

// resource_adaptor
void
test05()
{
  bool test __attribute((unused)) = false;
  using std::max_align_t;
  using std::uintptr_t;
  void* p = nullptr;

  Allocator<int> a1(1), a2(2); // minimal interface allocators
  resource_adaptor<decltype(a1)> r1(a1), r2(a2);
  VERIFY( r1 == r1 );
  VERIFY( r1 == r2 );
  p = r1.allocate(1);
  VERIFY( aligned<max_align_t>(p) );
  r1.deallocate(p, 1);
  p = r1.allocate(1, alignof(short));
  VERIFY( aligned<short>(p) );
  r1.deallocate(p, 1, alignof(short));
  p = r1.allocate(1, alignof(long));
  VERIFY( aligned<long>(p) );
  r1.deallocate(p, 1, alignof(long));

  __gnu_test::uneq_allocator<double> a3(3), a4(4); // non-equal allocators
  resource_adaptor<decltype(a3)> r3(a3), r4(a4);
  VERIFY( r3 == r3 );
  VERIFY( r4 == r4 );
  VERIFY( r3 != r4 );
  p = r3.allocate(1);
  VERIFY( aligned<max_align_t>(p) );
  r3.deallocate(p, 1);
  p = r3.allocate(1, alignof(short));
  VERIFY( aligned<short>(p) );
  r3.deallocate(p, 1, alignof(short));
  p = r3.allocate(1, alignof(long));
  VERIFY( aligned<long>(p) );
  r3.deallocate(p, 1, alignof(long));

  // TODO test with an allocator that doesn't use new or malloc, so
  // returns pointers that are not suitably aligned for any type.
}

int main()
{
  test05();
}
