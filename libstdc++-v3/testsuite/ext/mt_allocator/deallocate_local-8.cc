// { dg-require-cxa-atexit "" }

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

// 20.4.1.1 allocator members

#include <string>
#include <stdexcept>
#include <cstdio>
#include <ext/mt_allocator.h>
#include <replacement_memory_operators.h>

template<bool _Thread>
  struct cleanup_pool : public __gnu_cxx::__pool<false>
  {
    cleanup_pool() : __gnu_cxx::__pool<false>() { }

    cleanup_pool(const __gnu_cxx::__pool_base::_Tune& t) 
    : __gnu_cxx::__pool<false>(t) { }

    ~cleanup_pool() throw() { this->_M_destroy(); }
  };

typedef char value_type;
typedef std::char_traits<value_type> traits_type;
using __gnu_cxx::__pool;
using __gnu_cxx::__per_type_pool_policy;
typedef __per_type_pool_policy<value_type, cleanup_pool, false> policy_type;
typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;
typedef std::basic_string<value_type, traits_type, allocator_type> string_type;

int main()
{
  // NB: __mt_allocator doesn't clean itself up. Thus, the count will
  // not be zero.
  __gnu_test::counter::exceptions(false);
  string_type s;
  s += "bayou bend";
  return 0;
}
