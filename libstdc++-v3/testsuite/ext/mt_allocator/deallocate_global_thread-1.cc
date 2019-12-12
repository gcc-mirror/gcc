//
// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

// 20.4.1.1 allocator members

#include <list>
#include <string>
#include <stdexcept>
#include <cstdio>
#include <ext/mt_allocator.h>
#include <replacement_memory_operators.h>

typedef std::string value_type;
using __gnu_cxx::__pool;
using __gnu_cxx::__common_pool_policy;
typedef __common_pool_policy<__pool, true> policy_type;
typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;
typedef std::char_traits<value_type> traits_type;
typedef std::list<value_type, allocator_type> list_type;

list_type l;

int main()
{
  // NB: __mt_allocator doesn't clean itself up. Thus, the count will
  // not be zero.
  __gnu_test::counter::exceptions(false);
  l.push_back("bayou bend");
  return 0;
}
