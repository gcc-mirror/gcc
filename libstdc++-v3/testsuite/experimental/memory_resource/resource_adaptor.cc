// { dg-do run { target c++14 } }
// { dg-require-cstdint "" }

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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
#include <cstdint>
#include <ext/debug_allocator.h>
#include <ext/new_allocator.h>
#include <ext/malloc_allocator.h>
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

template<std::size_t A>
  bool aligned(void* p)
  {
    return (reinterpret_cast<std::uintptr_t>(p) % A) == 0;
  }

template<typename T>
  bool aligned(void* p)
  { return aligned<alignof(T)>(p); }

// resource_adaptor
void
test05()
{
  using std::max_align_t;
  using std::size_t;
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
  constexpr size_t big_al = alignof(max_align_t) * 8;
  p = r1.allocate(1, big_al);
  VERIFY( aligned<big_al>(p) );
  r1.deallocate(p, 1, big_al);

  __gnu_test::uneq_allocator<double> a3(3), a4(4); // non-equal allocators
  resource_adaptor<decltype(a3)> r3(a3), r4(a4);
  VERIFY( r3 == r3 );
  VERIFY( r4 == r4 );
  VERIFY( r3 != r4 );
  VERIFY( r3 != r1 );
  VERIFY( r3 != r2 );
  p = r3.allocate(1);
  VERIFY( aligned<max_align_t>(p) );
  r3.deallocate(p, 1);
  p = r3.allocate(1, alignof(short));
  VERIFY( aligned<short>(p) );
  r3.deallocate(p, 1, alignof(short));
  p = r3.allocate(1, alignof(long));
  VERIFY( aligned<long>(p) );
  r3.deallocate(p, 1, alignof(long));
  p = r3.allocate(1, big_al);
  VERIFY( aligned<big_al>(p) );
  r3.deallocate(p, 1, big_al);

  __gnu_cxx::debug_allocator<std::allocator<short>> a5, a6;
  resource_adaptor<decltype(a5)> r5(a5), r6(a6);
  VERIFY( r5 == r5 );
  VERIFY( r5 == r6 );
  VERIFY( r5 != r1 );
  VERIFY( r5 != r3 );
  p = r5.allocate(1);
  VERIFY( aligned<max_align_t>(p) );
  r5.deallocate(p, 1);
  p = r5.allocate(1, alignof(short));
  VERIFY( aligned<short>(p) );
  r5.deallocate(p, 1, alignof(short));
  p = r5.allocate(1, alignof(long));
  VERIFY( aligned<long>(p) );
  r5.deallocate(p, 1, alignof(long));
  p = r5.allocate(1, big_al);
  VERIFY( aligned<big_al>(p) );
  r5.deallocate(p, 1, big_al);

  // Test extended alignments
  constexpr size_t al6 = (1ul << 6), al12 = (1ul << 12), al18 = (1ul << 18);
  p = r5.allocate(1024, al6);
  VERIFY( aligned<al6>(p) );
  r5.deallocate(p, 1024, al6);
  p = r5.allocate(1024, al12);
  VERIFY( aligned<al12>(p) );
  r5.deallocate(p, 1024, al12);
  p = r5.allocate(1024, al18);
  VERIFY( aligned<al18>(p) );
  r5.deallocate(p, 1024, al18);

  __gnu_cxx::new_allocator<short> a7, a8;
  resource_adaptor<decltype(a7)> r7(a7), r8(a8);
  VERIFY( r7 == r7 );
  VERIFY( r7 == r8 );
  VERIFY( r7 != r1 );
  VERIFY( r7 != r3 );
  VERIFY( r7 != r5 );
  p = r7.allocate(1);
  VERIFY( aligned<max_align_t>(p) );
  r7.deallocate(p, 1);
  p = r7.allocate(1, alignof(short));
  VERIFY( aligned<short>(p) );
  r7.deallocate(p, 1, alignof(short));
  p = r7.allocate(1, alignof(long));
  VERIFY( aligned<long>(p) );
  r7.deallocate(p, 1, alignof(long));
  p = r7.allocate(1, big_al);
  VERIFY( aligned<big_al>(p) );
  r7.deallocate(p, 1, big_al);
  // Test extended alignments
  p = r7.allocate(1024, al6);
  VERIFY( aligned<al6>(p) );
  r7.deallocate(p, 1024, al6);
  p = r7.allocate(1024, al12);
  VERIFY( aligned<al12>(p) );
  r7.deallocate(p, 1024, al12);
  p = r7.allocate(1024, al18);
  VERIFY( aligned<al18>(p) );
  r7.deallocate(p, 1024, al18);

  __gnu_cxx::malloc_allocator<short> a9, a10;
  resource_adaptor<decltype(a9)> r9(a9), r10(a10);
  VERIFY( r9 == r9 );
  VERIFY( r9 == r10 );
  VERIFY( r9 != r1 );
  VERIFY( r9 != r3 );
  VERIFY( r9 != r5 );
  VERIFY( r9 != r7 );
  p = r9.allocate(1);
  VERIFY( aligned<max_align_t>(p) );
  r9.deallocate(p, 1);
  p = r9.allocate(1, alignof(short));
  VERIFY( aligned<short>(p) );
  r9.deallocate(p, 1, alignof(short));
  p = r9.allocate(1, alignof(long));
  VERIFY( aligned<long>(p) );
  r9.deallocate(p, 1, alignof(long));
  p = r9.allocate(1, big_al);
  VERIFY( aligned<big_al>(p) );
  r9.deallocate(p, 1, big_al);
  // Test extended alignments
  p = r9.allocate(1024, al6);
  VERIFY( aligned<al6>(p) );
  r9.deallocate(p, 1024, al6);
  p = r9.allocate(1024, al12);
  VERIFY( aligned<al12>(p) );
  r9.deallocate(p, 1024, al12);
  p = r9.allocate(1024, al18);
  VERIFY( aligned<al18>(p) );
  r9.deallocate(p, 1024, al18);

  std::allocator<short> a11, a12;
  resource_adaptor<decltype(a11)> r11(a11), r12(a12);
  VERIFY( r11 == r11 );
  VERIFY( r11 == r12 );
  VERIFY( r11 != r1 );
  VERIFY( r11 != r3 );
  VERIFY( r11 != r5 );
  VERIFY( r11 != r7 );
  VERIFY( r11 != r9 );
  p = r11.allocate(1);
  VERIFY( aligned<max_align_t>(p) );
  r11.deallocate(p, 1);
  p = r11.allocate(1, alignof(short));
  VERIFY( aligned<short>(p) );
  r11.deallocate(p, 1, alignof(short));
  p = r11.allocate(1, alignof(long));
  VERIFY( aligned<long>(p) );
  r11.deallocate(p, 1, alignof(long));
  p = r11.allocate(1, big_al);
  VERIFY( aligned<big_al>(p) );
  r11.deallocate(p, 1, big_al);
  // Test extended alignments
  p = r11.allocate(1024, al6);
  VERIFY( aligned<al6>(p) );
  r11.deallocate(p, 1024, al6);
  p = r11.allocate(1024, al12);
  VERIFY( aligned<al12>(p) );
  r11.deallocate(p, 1024, al12);
  p = r11.allocate(1024, al18);
  VERIFY( aligned<al18>(p) );
  r11.deallocate(p, 1024, al18);
}

int main()
{
  test05();
}
