// 2001-11-25  Phil Edwards  <pme@gcc.gnu.org>
//
// Copyright (C) 2001, 2003 Free Software Foundation, Inc.
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

#include <cstdlib>
#include <memory>
#include <ext/pool_allocator.h>
#include <ext/debug_allocator.h>
#include <ext/malloc_allocator.h>
#include <testsuite_hooks.h>

using __gnu_cxx::__malloc_alloc;
using __gnu_cxx::__debug_alloc;
using __gnu_cxx::__pool_alloc;

template class __malloc_alloc<3>;
template class __debug_alloc<__malloc_alloc<3> >;
template class __pool_alloc<true, 3>;
template class __pool_alloc<false, 3>;

struct big
{
  long f[15];
};


bool         new_called;
bool         delete_called;
std::size_t  requested;

void* 
operator new(std::size_t n) throw(std::bad_alloc)
{
  new_called = true;
  requested = n;
  return std::malloc(n);
}

void
operator delete(void *v) throw()
{
  delete_called = true;
  return std::free(v);
}

template<typename Alloc, bool uses_global_new_and_delete>
void check_allocator()
{
  bool test __attribute__((unused)) = true;
  new_called = false;
  delete_called = false;
  requested = 0;

  std::__allocator<big, Alloc>   a;
  big *p = a.allocate(10);
  if (uses_global_new_and_delete)  
    VERIFY( requested >= (10 * 15 * sizeof(long)) );

  // Touch the far end of supposedly-allocated memory to check that we got
  // all of it.  Why "3"?  Because it's my favorite integer between e and pi.
  p[9].f[14] = 3;
  VERIFY( new_called == uses_global_new_and_delete );
  a.deallocate(p,10);
  VERIFY( delete_called == uses_global_new_and_delete );
}

// These just help tracking down error messages.
void test01() { check_allocator<__malloc_alloc<3>, false>(); }
void test02() { check_allocator<__debug_alloc<__malloc_alloc<3> >, false>(); }
void test03() { check_allocator<__pool_alloc<true, 3>, true>(); }
void test04() { check_allocator<__pool_alloc<false, 3>, true>(); }

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}

