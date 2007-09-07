// Copyright (C) 2007 Free Software Foundation, Inc.
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
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <vector>
#include <cstdlib>
#include <testsuite_hooks.h>

unsigned int zero_sized_news = 0;

void *operator new(size_t size) throw (std::bad_alloc)
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

// http://gcc.gnu.org/ml/libstdc++/2007-09/msg00006.html
void test01()
{
  bool test __attribute__((unused)) = true;

  std::vector<std::vector<int> > *v;
  VERIFY( zero_sized_news == 0 );

  v = new std::vector<std::vector<int> >;
  VERIFY( zero_sized_news == 0 );

  v->resize(10);
  delete(v);
  VERIFY( zero_sized_news == 0 );
}

int main()
{
  test01();
  return 0;
}
