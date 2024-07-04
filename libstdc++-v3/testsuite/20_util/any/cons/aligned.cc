// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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
// { dg-require-cstdint "" }

#include <any>
#include <cstdint>
#include <testsuite_hooks.h>

// Alignment requiremnts of this type prevent it being stored in 'any'
struct alignas(2 * alignof(void*)) X { };

bool
stored_internally(void* obj, const std::any& a)
{
  std::uintptr_t a_addr = reinterpret_cast<std::uintptr_t>(&a);
  std::uintptr_t a_end = a_addr + sizeof(a);
  std::uintptr_t obj_addr = reinterpret_cast<std::uintptr_t>(obj);
  return (a_addr <= obj_addr) && (obj_addr < a_end);
}

void
test01()
{
  std::any a = X{};
  X& x = std::any_cast<X&>(a);
  VERIFY( !stored_internally(&x, a) );

  a = 'X';
  char& c = std::any_cast<char&>(a);
  VERIFY( stored_internally(&c, a) );
}

int
main()
{
  test01();
}
