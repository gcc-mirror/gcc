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

// 20.4.1.1 allocator members

#include <string>
#include <stdexcept>
#include <ext/mt_allocator.h>
#include <testsuite_hooks.h>

static size_t count;

struct count_check
{
  count_check() {}
  ~count_check()
  {
    if (count != 0)
      {
	// NB: __mt_allocator doesn't clean itself up. Thus, this will
	// not be zero.
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
  if (count == 0)
    printf("All memory released \n");
  else
    printf("%u allocations to be released \n", count);
  free(p);
}

typedef char value_type;
typedef std::char_traits<value_type> traits_type;
using __gnu_cxx::__pool;
typedef __gnu_cxx::__common_pool_policy<__pool, false> policy_type;
typedef __gnu_cxx::__mt_alloc<value_type, policy_type> allocator_type;
typedef std::basic_string<value_type, traits_type, allocator_type> string_type;

int main()
{
  bool test __attribute__((unused)) = true;
  {
    string_type s;
    s += "bayou bend";
  }
  return 0;
}
