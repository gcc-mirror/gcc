// 2004-08-25 Benjamin Kosnik <bkoz@redhat.com>
//
// Copyright (C) 2004, 2005 Free Software Foundation, Inc.
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

#include <memory>
#include <ext/mt_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// Tune characteristics. 
// __common_pool_policy
void test01()
{
  bool test __attribute__((unused)) = true;

  typedef __gnu_test::pod_int value_type;

  using __gnu_cxx::__pool;
  using __gnu_cxx::__common_pool_policy;

#ifdef __GTHREADS
  typedef __common_pool_policy<__pool, true> policy_type;
#else
  typedef __common_pool_policy<__pool, false> policy_type;
#endif
  typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;
  typedef __gnu_cxx::__pool_base::_Tune tune_type;

  tune_type t_default;
  tune_type t_opt(16, 5120, 32, 5120, 20, 10, false);
  tune_type t_single(16, 5120, 32, 5120, 1, 10, false);

  allocator_type a;
  tune_type t1 = a._M_get_options();  
  VERIFY( t1._M_align == t_default._M_align );
  a._M_set_options(t_opt);
  tune_type t2 = a._M_get_options();
  VERIFY( t1._M_align != t2._M_align );

  allocator_type::pointer p1 = a.allocate(128);
  allocator_type::pointer p2 = a.allocate(5128);
  a._M_set_options(t_single);
  t1 = a._M_get_options();  
  VERIFY( t1._M_max_threads != t_single._M_max_threads );
  VERIFY( t1._M_max_threads == t_opt._M_max_threads );

  a.deallocate(p1, 128);
  a.deallocate(p2, 5128);
}

int main()
{
  test01();
  return 0;
}
