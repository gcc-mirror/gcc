// { dg-require-namedlocale "" }
// { dg-require-namedlocale "en_US.ISO8859-1" }

// Copyright (C) 2003-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <new>
#include <locale>
#include <cstdlib>
#include <cstring>
#include <testsuite_hooks.h>

int times_to_fail = 0;

void* allocate(std::size_t n)
{
  if (!times_to_fail--)
    return 0;

  void* ret = std::malloc(n ? n : 1);
  if (ret)
    std::memset(ret, 0xbc, n);
  return ret;
}

void deallocate(void* p)
{
  if (p)
    std::free(p);
}

void* operator new(std::size_t n) THROW (std::bad_alloc)
{
  void* ret = allocate(n);
  if (!ret)
    throw std::bad_alloc();
  return ret;
}

void* operator new[](std::size_t n) THROW (std::bad_alloc)
{
  void* ret = allocate(n);
  if (!ret)
    throw std::bad_alloc();
  return ret;
}

void operator delete(void* p) throw()
{
  deallocate(p);
}

void operator delete[](void* p) throw()
{
  deallocate(p);
}

void* operator new(std::size_t n, const std::nothrow_t&) throw()
{
  return allocate(n);
}

void* operator new[](std::size_t n, const std::nothrow_t&) throw()
{
  return allocate(n);
}

void operator delete(void* p, const std::nothrow_t&) throw()
{
  deallocate(p);
}

void operator delete[](void* p, const std::nothrow_t&) throw()
{
  deallocate(p);
}

// libstdc++/12352
void test01(int iters)
{
  for (int j = 0; j < iters; ++j)
    {
      for (int i = 0; i < 100; ++i)
	{
	  times_to_fail = i;
	  try
	    {
	      std::locale loc1 = std::locale("");
	      std::locale loc2(loc1, std::locale::classic(),
			       std::locale::numeric);
	      std::locale loc3 = std::locale(ISO_8859(1,en_US));
	      std::locale loc4(loc3, std::locale::classic(),
			       std::locale::numeric);
	    }
	  catch (std::exception&)
	    {
	    }
	}
    }
}

int main(int argc, char* argv[])
{
  int iters = 1;
  if (argc > 1)
    iters = std::atoi(argv[1]);
  if (iters < 1)
    iters = 1;
  test01(iters);

  return 0;
}
