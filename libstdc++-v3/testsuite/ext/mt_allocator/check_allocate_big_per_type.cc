// 2005-01-26  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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
#include <cstring>
#include <testsuite_hooks.h>

struct big { char array[256]; };

// __per_type_pool_policy
void test01()
{
  typedef big value_type;

  using __gnu_cxx::__pool;
  using __gnu_cxx::__per_type_pool_policy;

#ifdef __GTHREADS
  typedef __per_type_pool_policy<value_type, __pool, true> policy_type;
#else
  typedef __per_type_pool_policy<value_type, __pool, false> policy_type;
#endif
  typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;

  allocator_type a;
  allocator_type::pointer p1 = a.allocate(64);
  std::memset((void*)p1, 0, sizeof(big) * 64);
  a.deallocate(p1, 64);
}

int main()
{
  test01();
  return 0;
}
