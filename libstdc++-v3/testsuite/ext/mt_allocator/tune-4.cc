// 2004-08-25 Benjamin Kosnik <bkoz@redhat.com>
//
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

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
  bool test __attribute__((unused)) = true;
  
  typedef __gnu_cxx::__pool_base::_Tune tune_type;
  typedef _Tp value_type;
  typedef _Cp policy_type;

  typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;
  tune_type t_opt(16, 5120, 32, 5120, 20, 10, false);
  tune_type t_single(16, 5120, 32, 5120, 1, 10, false);

  allocator_type a;
  tune_type t_default = a._M_get_options();
  tune_type t1 = t_default;
  tune_type t2;
  if (test_policy<policy_type>::per_type())
    {
      a._M_set_options(t_opt);
      t2 = a._M_get_options();  
      VERIFY( t1._M_align != t2._M_align );
    }
  else
    t2 = t1;

  // Lock tune settings.
  typename allocator_type::pointer p1 = a.allocate(128);

  // First instance of local type assured.
  typedef pod2 value2_type;
  typedef typename allocator_type::template rebind<value2_type>::other rebind_type;

  rebind_type a2;
  tune_type t3 = a2._M_get_options();  
  tune_type t4;

  // Both policy_type and rebind_type::policy_type have same characteristics.
  if (test_policy<policy_type>::per_type())
    {
      a2._M_set_options(t_opt);
      t4 = a2._M_get_options();
      VERIFY( t3._M_align != t4._M_align );
      t3 = t4;
    }
  else
    VERIFY( t3._M_max_threads == t2._M_max_threads );

  typename rebind_type::pointer p2 = a2.allocate(5128);

  a2._M_set_options(t_single);
  t4 = a2._M_get_options();
  VERIFY( t4._M_max_threads != t_single._M_max_threads );
  VERIFY( t4._M_max_threads == t3._M_max_threads );

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
