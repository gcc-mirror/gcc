// 2001-06-14  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// 20.4.1.1 allocator members

#include <memory>
#include <stdexcept>
#include <cstdlib>
#include <testsuite_hooks.h>

struct gnu { };

bool check_new = false;
bool check_delete = false;

void* 
operator new(std::size_t n) throw(std::bad_alloc)
{
  check_new = true;
  return std::malloc(n);
}

void operator delete(void *v) throw()
{
  check_delete = true;
  return std::free(v);
}

void test01()
{
  bool test __attribute__((unused)) = true;
  std::allocator<gnu> obj;

  // NB: These should work for various size allocation and
  // deallocations.  Currently, they only work as expected for sizes >
  // _MAX_BYTES as defined in stl_alloc.h, which happes to be 128. 
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

