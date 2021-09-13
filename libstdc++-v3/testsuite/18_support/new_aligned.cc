// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <new>
#include <memory>
#include <testsuite_hooks.h>

struct Test
{
  Test(std::size_t size, std::size_t a)
  : size(size), alignment(std::align_val_t{a}),
    p(::operator new(size, alignment))
  { }

  ~Test() { ::operator delete(p, size, alignment); }

  std::size_t size;
  std::align_val_t alignment;
  void* p;

  bool valid() const { return p != nullptr; }

  bool aligned() const
  {
    auto ptr = p;
    auto space = size;
    return std::align((std::size_t)alignment, size, ptr, space) == p;
  }
};

// operator new(size_t size, align_val_t alignment) has
// undefined behaviour if the alignment argument is not
// a valid alignment value (i.e. not a power of two).
//
// Unlike posix_memalign there is no requirement that
// alignment >= sizeof(void*).
//
// Unlike aligned_alloc there is no requirement that
// size is an integer multiple of alignment.

void
test01()
{
  // Test small values that would not be valid for
  // posix_memalign or aligned_alloc.

  Test t11{1, 1};
  VERIFY( t11.valid() );
  VERIFY( t11.aligned() );

  Test t21{2, 1};
  VERIFY( t21.valid() );
  VERIFY( t21.aligned() );

  Test t12{1, 2};
  VERIFY( t12.valid() );
  VERIFY( t12.aligned() );

  Test t22{2, 2};
  VERIFY( t22.valid() );
  VERIFY( t22.aligned() );

  Test t32{3, 2};
  VERIFY( t32.valid() );
  VERIFY( t32.aligned() );

  Test t42{4, 2};
  VERIFY( t42.valid() );
  VERIFY( t42.aligned() );

  Test t24{2, 4};
  VERIFY( t24.valid() );
  VERIFY( t24.aligned() );

  Test t34{3, 4};
  VERIFY( t34.valid() );
  VERIFY( t34.aligned() );

  Test t44{4, 4};
  VERIFY( t44.valid() );
  VERIFY( t44.aligned() );

  // Test some larger values.

  Test t128_16{128, 16};
  VERIFY( t128_16.valid() );
  VERIFY( t128_16.aligned() );

  Test t128_64{128, 64};
  VERIFY( t128_64.valid() );
  VERIFY( t128_64.aligned() );

  Test t64_128{64, 128};
  VERIFY( t64_128.valid() );
  VERIFY( t64_128.aligned() );
}

int
main()
{
  test01();
}
