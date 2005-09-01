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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 20.4.1.1 allocator members

#include <string>
#include <stdexcept>
#include <ext/new_allocator.h>

static size_t count;

struct count_check
{
  count_check() { }
  ~count_check()
  {
    if (count != 0)
      throw std::runtime_error("allocation/deallocation count isn't zero");
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

typedef char char_t;
typedef std::char_traits<char_t> traits_t;
typedef __gnu_cxx::new_allocator<char_t> allocator_t;
typedef std::basic_string<char_t, traits_t, allocator_t> string_t;

int main()
{
  string_t s;
  s += "bayou bend";
  return 0;
}
