// 2004-08-25 Benjamin Kosnik <bkoz@redhat.com>
//
// Copyright (C) 2004, 2005, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <memory>
#include <ext/mt_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

#ifdef __GTHREADS
#define __cxxthread true
#else
#define __cxxthread false
#endif

// Tune characteristics. 
// __common_pool_policy
void test01()
{
  bool test __attribute__((unused)) = true;

  using __gnu_cxx::__pool;
  using __gnu_cxx::__common_pool_policy;

  typedef __gnu_test::pod_int value_type;
  typedef __common_pool_policy<__pool, __cxxthread> policy_type;
  typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;
  typedef __gnu_cxx::__pool_base::_Tune tune_type;

  allocator_type a;
  tune_type t_default = a._M_get_options();
  tune_type t_opt(32, 5120, 32, 5120, 20, 10, false);
  tune_type t_small(16, 1024, 32, 2048, 1, 10, false);

  tune_type t1 = t_default;
  a._M_set_options(t_opt);
  tune_type t2 = a._M_get_options();
  VERIFY( t1._M_align != t2._M_align );

  allocator_type::pointer p1 = a.allocate(128);
  allocator_type::pointer p2 = a.allocate(5128);
  a._M_set_options(t_small);
  tune_type t3 = a._M_get_options();  
  VERIFY( t3._M_chunk_size != t_small._M_chunk_size );
  VERIFY( t3._M_chunk_size == t_opt._M_chunk_size );

  a.deallocate(p1, 128);
  a.deallocate(p2, 5128);
}

int main()
{
  test01();
  return 0;
}
