// 2004-10-10  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 20.4.1.1 allocator members

#include <ext/pool_allocator.h>

struct small
{
  char c[16];
};

struct big
{
  char c[64];
};

void*
operator new(size_t n) throw(std::bad_alloc)
{
  static bool first = true;
  if (!first)
    throw std::bad_alloc();
  first = false;
  return std::malloc(n);
}

// http://gcc.gnu.org/ml/libstdc++/2004-10/msg00098.html
void test01()
{
  using __gnu_cxx::__pool_alloc;

  __pool_alloc<big> alloc_big;
  alloc_big.allocate(1);

  // The constant 20 comes from __pool_alloc_base::_M_refill. See
  // also __pool_alloc_base::_M_allocate_chunk.
  __pool_alloc<small> alloc_small;
  for (int i = 0; i < 20 * sizeof(big) / sizeof(small) + 1; ++i)
    alloc_small.allocate(1);
}

int main()
{
  test01();
}
