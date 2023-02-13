// { dg-do run { target c++11 } }

// 2014-04-16 Rüdiger Sonderfeld  <ruediger@c-plusplus.de>

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// C++11 [ptr.align] (20.6.5): std::align

// { dg-require-cstdint "" }

#include <memory>
#include <cstdint>
#include <testsuite_hooks.h>

void
test01()
{
  using std::size_t;

  size_t space = 100;
  void* ptr = new char[space];
  char* const orig_ptr = static_cast<char*>(ptr);
  char* old_ptr = orig_ptr;
  const size_t orig_space = space;
  size_t old_space = space;
  const size_t alignment = 16;
  const size_t size = 10;
  while( void* const r = std::align(alignment, size, ptr, space) )
    {
      VERIFY( r == ptr );
      uintptr_t p = reinterpret_cast<uintptr_t>(ptr);
      VERIFY( p % alignment == 0 );
      char* const x = static_cast<char*>(ptr);
      VERIFY( size_t(x - old_ptr) == old_space - space );
      VERIFY( (void*)x < (void*)(orig_ptr + orig_space) );
      VERIFY( (void*)(x + size) < (void*)(orig_ptr + orig_space) );
      ptr = x + size;
      old_ptr = x;
      old_space = space;
      space -= size;
    }
  delete [] orig_ptr;
}

int main()
{
  test01();
}
