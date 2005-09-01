// { dg-require-cxa-atexit "" }

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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 20.4.1.1 allocator members

#include <string>
#include <stdexcept>
#include <ext/mt_allocator.h>

static size_t count;

struct count_check
{
  count_check() { }
  ~count_check()
  {
    // NB: Using a pool that attempts to clean up resource use.
    if (count != 0)
      {
	printf("allocation/deallocation count is %zu \n", count);
	throw std::runtime_error("allocation/deallocation count isn't zero");
      }
  }
};
 
static count_check check;

void* operator new(size_t size) throw(std::bad_alloc)
{
  printf("operator new is called \n");
  void* p = malloc(size);
  if (p == NULL)
    throw std::bad_alloc();
  count++;
  return p;
}
 
void operator delete(void* p) throw()
{
  printf("operator delete is called \n");
  if (p == NULL)
    return;
  count--;
}

template<bool _Thread>
  struct cleanup_pool : public __gnu_cxx::__pool<true>
  {
    cleanup_pool() : __gnu_cxx::__pool<true>() { }

    cleanup_pool(const __gnu_cxx::__pool_base::_Tune& t) 
    : __gnu_cxx::__pool<true>(t) { }

    ~cleanup_pool() throw() { this->_M_destroy(); }
  };

typedef char value_type;
typedef std::char_traits<value_type> traits_type;
using __gnu_cxx::__pool;
using __gnu_cxx::__per_type_pool_policy;
typedef __per_type_pool_policy<value_type, cleanup_pool, true> policy_type;
typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;
typedef std::basic_string<value_type, traits_type, allocator_type> string_type;

int main()
{
  string_type s;
  s += "bayou bend";
  return 0;
}
