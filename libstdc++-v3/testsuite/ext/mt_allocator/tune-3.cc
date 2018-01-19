// 2004-08-25 Benjamin Kosnik <bkoz@redhat.com>
//
// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <memory>
#include <ext/mt_allocator.h>

// Tune characteristics, two of same type
template<typename _Tp>
struct test_policy
{ static bool per_type() { return true; } };

using __gnu_cxx::__pool;
using __gnu_cxx::__common_pool_policy;

template<>
struct test_policy<__common_pool_policy<__pool, true> >
{ static bool per_type() { return false; } };

template<>
struct test_policy<__common_pool_policy<__pool, false> >
{ static bool per_type() { return false; } };

// Tune characteristics, two of different types
template<typename _Tp, typename _Cp>
void test03()
{
  typedef __gnu_cxx::__pool_base::_Tune tune_type;
  typedef _Tp value_type;
  typedef _Cp policy_type;
  typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;

  allocator_type a;
  tune_type t_default = a._M_get_options();
  tune_type t_opt(32, 5120, 32, 5120, 20, 10, false);
  tune_type t_small(16, 1024, 32, 2048, 1, 10, false);

  // First instances assured.
  tune_type t1 = t_default;
  if (test_policy<policy_type>::per_type())
    {
      a._M_set_options(t_opt);
      t1 = a._M_get_options();
      VERIFY( t1._M_align != t_default._M_align );
    }

  // Lock tune settings.
  typename allocator_type::pointer p1 = a.allocate(128);

  allocator_type a2;
  tune_type t2 = a2._M_get_options();  
  VERIFY( t2._M_chunk_size == t1._M_chunk_size );

  typename allocator_type::pointer p2 = a2.allocate(5128);

  a2._M_set_options(t_small);
  tune_type t3 = a2._M_get_options();
  VERIFY( t3._M_chunk_size != t_small._M_chunk_size );
  VERIFY( t3._M_chunk_size == t2._M_chunk_size );

  a.deallocate(p1, 128);
  a2.deallocate(p2, 5128);
}

int main()
{
#ifdef __GTHREADS
  test03<int, __gnu_cxx::__per_type_pool_policy<int, __pool, true> >();
  test03<int, __gnu_cxx::__common_pool_policy<__pool, true> >();
#endif

  test03<int, __gnu_cxx::__per_type_pool_policy<int, __pool, false> >();
  test03<int, __gnu_cxx::__common_pool_policy<__pool, false> >();

  return 0;
}
