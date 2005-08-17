// 2001-11-25  Phil Edwards  <pme@gcc.gnu.org>
//
// Copyright (C) 2001, 2003, 2004 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 20.4.1.1 allocator members

#include <cstdlib>
#include <ext/debug_allocator.h>
#include <ext/malloc_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using __gnu_cxx::malloc_allocator;
using __gnu_cxx::debug_allocator;

void* 
operator new(std::size_t n) throw(std::bad_alloc)
{
  new_called = true;
  return std::malloc(n);
}

void
operator delete(void *v) throw()
{
  delete_called = true;
  return std::free(v);
}

// These just help tracking down error messages.
void test01() 
{ 
  bool test __attribute__((unused)) = true;
  typedef debug_allocator<malloc_allocator<unsigned int> > allocator_type;
  VERIFY( bool(__gnu_test::check_new<allocator_type, false>()) ); 
}

int main()
{
  test01();
  return 0;
}

