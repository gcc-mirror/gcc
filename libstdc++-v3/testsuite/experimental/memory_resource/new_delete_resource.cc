// Copyright (C) 2018 Free Software Foundation, Inc.
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
// { dg-require-cstdint "" }
// { dg-xfail-run-if "PR libstdc++/77691" { { i?86-*-solaris2.* x86_64-*-solaris2.* } && ilp32 } }

#include <experimental/memory_resource>
#include <cstdlib>
#include <testsuite_hooks.h>

bool new_called = false;
bool delete_called = false;
std::size_t bytes_allocated = 0;

void* operator new(std::size_t n)
{
  new_called = true;
  if (void* p = malloc(n))
  {
    bytes_allocated += n;
    return p;
  }
  throw std::bad_alloc();
}

void operator delete(void* p)
{
  delete_called = true;
  std::free(p);
  bytes_allocated = 0; // assume everything getting deleted
}

void operator delete(void* p, std::size_t n)
{
  delete_called = true;
  std::free(p);
  bytes_allocated -= n;
}


template<std::size_t A>
  bool aligned(void* p)
  {
    return (reinterpret_cast<std::uintptr_t>(p) % A) == 0;
  }

template<typename T>
  bool aligned(void* p)
  { return aligned<alignof(T)>(p); }

// Extended alignments:
constexpr std::size_t al6 = (1ul << 6), al12 = (1ul << 12), al18 = (1ul << 18);

using std::experimental::pmr::memory_resource;
using std::experimental::pmr::new_delete_resource;

memory_resource* global = nullptr;

void
test01()
{
  memory_resource* r1 = new_delete_resource();
  VERIFY( *r1 == *r1 );
  memory_resource* r2 = new_delete_resource();
  VERIFY( r1 == r2 );
  VERIFY( *r1 == *r2 );
  global = r1;
}

void
test02()
{
  memory_resource* r1 = new_delete_resource();
  VERIFY( r1 == global );
  VERIFY( *r1 == *global );

  new_called = false;
  delete_called = false;
  void* p = r1->allocate(1);
  VERIFY( new_called );
  VERIFY( ! delete_called );

  new_called = false;
  r1->deallocate(p, 1);
  VERIFY( ! new_called );
  VERIFY( delete_called );
}

void
test03()

{
  using std::max_align_t;
  using std::size_t;
  void* p = nullptr;

  bytes_allocated = 0;

  memory_resource* r1 = new_delete_resource();
  p = r1->allocate(1);
  VERIFY( bytes_allocated == 1 );
  VERIFY( aligned<max_align_t>(p) );
  r1->deallocate(p, 1);
  VERIFY( bytes_allocated == 0 );

  p = r1->allocate(2, alignof(char));
  VERIFY( bytes_allocated == 2 );
  VERIFY( aligned<max_align_t>(p) );
  r1->deallocate(p, 2);
  VERIFY( bytes_allocated == 0 );

  p = r1->allocate(3, alignof(short));
  VERIFY( bytes_allocated == 3 );
  VERIFY( aligned<short>(p) );
  r1->deallocate(p, 3, alignof(short));
  VERIFY( bytes_allocated == 0 );

  p = r1->allocate(4, alignof(long));
  VERIFY( bytes_allocated == 4 );
  VERIFY( aligned<long>(p) );
  r1->deallocate(p, 4, alignof(long));
  VERIFY( bytes_allocated == 0 );

  // Test extended aligments:
  p = r1->allocate(777, al6);
  VERIFY( bytes_allocated >= 777 );
  VERIFY( bytes_allocated < (777 + al6 + 8) );  // reasonable upper bound
  VERIFY( aligned<al6>(p) );
  r1->deallocate(p, 777, al6);
  VERIFY( bytes_allocated == 0 );

  p = r1->allocate(888, al12);
  VERIFY( bytes_allocated >= 888 );
  VERIFY( bytes_allocated < (888 + al12 + 8) );  // reasonable upper bound
  VERIFY( aligned<al12>(p) );
  r1->deallocate(p, 888, al12);
  VERIFY( bytes_allocated == 0 );

  p = r1->allocate(999, al18);
  VERIFY( bytes_allocated >= 999 );
  VERIFY( bytes_allocated < (999 + al18 + 8) );  // reasonable upper bound
  VERIFY( aligned<al18>(p) );
  r1->deallocate(p, 999, al18);
  VERIFY( bytes_allocated == 0 );
}

int main()
{
  test01();
  test02();
  test03();
}
