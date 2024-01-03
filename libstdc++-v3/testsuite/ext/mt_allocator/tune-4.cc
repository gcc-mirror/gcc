// 2004-08-25 Benjamin Kosnik <bkoz@redhat.com>
//
// Copyright (C) 2004-2024 Free Software Foundation, Inc.
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

struct pod2
{
  int i;
  int j;
  int k;
};

// Tune characteristics, two of different instantiations
template<typename _Tp, typename _Cp>
void test04()
{
  typedef __gnu_cxx::__pool_base::_Tune tune_type;
  typedef _Tp value_type;
  typedef _Cp policy_type;
  typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;

  allocator_type a;
  tune_type t_default = a._M_get_options();
  tune_type t_opt(32, 5120, 32, 5120, 20, 10, false);
  tune_type t_small(16, 1024, 32, 2048, 1, 10, false);

  // First instance of local type assured.
  tune_type t1 = t_default;
  if (test_policy<policy_type>::per_type())
    {
      a._M_set_options(t_opt);
      t1 = a._M_get_options();  
      VERIFY( t1._M_align != t_default._M_align );
    }

  // Lock tune settings.
  typename allocator_type::pointer p1 = a.allocate(128);

  typedef pod2 value2_type;
  typedef typename allocator_type::template rebind<value2_type>::other rebind_type;

  rebind_type a2;
  tune_type t2 = a2._M_get_options();  

  // Both policy_type and rebind_type::policy_type have same characteristics.
  if (test_policy<policy_type>::per_type())
    {
      a2._M_set_options(t_opt);
      tune_type t = a2._M_get_options();
      VERIFY( t2._M_align != t._M_align );
      t2 = t;
    }

  typename rebind_type::pointer p2 = a2.allocate(5128);

  a2._M_set_options(t_small);
  tune_type t4 = a2._M_get_options();
  VERIFY( t4._M_chunk_size != t_small._M_chunk_size );
  VERIFY( t4._M_chunk_size == t2._M_chunk_size );

  a.deallocate(p1, 128);
  a2.deallocate(p2, 5128);
}

int main()
{
#ifdef __GTHREADS
  test04<float, __gnu_cxx::__common_pool_policy<__pool, true> >();
  test04<double, __gnu_cxx::__per_type_pool_policy<double, __pool, true> >();
#endif
  test04<float, __gnu_cxx::__common_pool_policy<__pool, false> >();
  test04<double, __gnu_cxx::__per_type_pool_policy<double, __pool, false> >();

  return 0;
}
