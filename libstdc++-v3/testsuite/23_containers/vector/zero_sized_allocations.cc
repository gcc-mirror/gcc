// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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
//

#include <vector>
#include <cstdlib>
#include <testsuite_hooks.h>

unsigned int zero_sized_news = 0;

void *operator new(std::size_t size) THROW (std::bad_alloc)
{
  /* malloc(0) is unpredictable; avoid it.  */
  if (size == 0)
    {
      size = 1;
      ++zero_sized_news;
    }

  void *p = std::malloc(size);

  if (p == 0)
    throw std::bad_alloc();

  return p;
}

void operator delete(void *ptr) throw()
{
  if (ptr != 0)
    std::free(ptr);
}

#if __cpp_sized_deallocation
void operator delete(void *ptr, std::size_t) throw()
{
  if (ptr != 0)
    std::free(ptr);
}
#endif

// http://gcc.gnu.org/ml/libstdc++/2007-09/msg00006.html
void test01()
{
  std::vector<std::vector<int> > *v;
  VERIFY( zero_sized_news == 0 );

  v = new std::vector<std::vector<int> >;
  VERIFY( zero_sized_news == 0 );

  v->resize(10);
  delete v;
  VERIFY( zero_sized_news == 0 );
}

int main()
{
  test01();
  return 0;
}
