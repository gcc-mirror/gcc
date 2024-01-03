// 2001-06-14  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

// { dg-require-effective-target std_allocator_new }

// 20.4.1.1 allocator members

#include <memory>
#include <stdexcept>
#include <cstdlib>
#include <testsuite_hooks.h>

#if __cplusplus >= 201103L
# define NOTHROW noexcept
#else
# define NOTHROW throw()
#endif

struct gnu { };

bool check_new = false;
bool check_delete = false;

void*
operator new(std::size_t n) THROW(std::bad_alloc)
{
  check_new = true;
  return std::malloc(n);
}

void operator delete(void *v) NOTHROW
{
  check_delete = true;
  return std::free(v);
}

#if __cpp_sized_deallocation
void operator delete(void *v, std::size_t) NOTHROW
{
  ::operator delete(v);
}
#endif

void test01()
{
  std::allocator<gnu> obj;

  gnu* pobj = obj.allocate(256);
  VERIFY( check_new );

  obj.deallocate(pobj, 256);
  VERIFY( check_delete );
}

int main()
{
  test01();
  return 0;
}

