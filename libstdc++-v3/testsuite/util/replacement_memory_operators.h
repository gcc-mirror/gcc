//
// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

#include <exception>
#include <stdexcept>
#include <cstdlib>
#include <cstdio>
#include <testsuite_hooks.h>

namespace __gnu_test
{
  struct counter_error : public std::exception { };

  struct counter
  {
    std::size_t _M_count;
    bool	_M_throw;

    counter() : _M_count(0), _M_throw(true) { }

    ~counter() THROW (counter_error)
    {
      if (_M_throw && _M_count != 0)
	throw counter_error();
    }

    static void
    increment() { get()._M_count++; }

    static void
    decrement() { get()._M_count--; }

    static counter&
    get()
    {
      static counter g;
      return g;
    }

    static std::size_t
    count() { return get()._M_count; }

    static void
    exceptions(bool __b) { get()._M_throw = __b; }
  };

  template<typename Alloc, bool uses_global_new>
    bool
    check_new(Alloc a = Alloc())
    {
      __gnu_test::counter::exceptions(false);
      (void) a.allocate(10);
      const bool __b((__gnu_test::counter::count() > 0) == uses_global_new);
      if (!__b)
	throw std::logic_error("counter not incremented");
      return __b;
    }

  template<typename Alloc, bool uses_global_delete>
    bool
    check_delete(Alloc a = Alloc())
    {
      __gnu_test::counter::exceptions(false);
#if __cplusplus >= 201103L
      auto p = a.allocate(10);
#else
      typename Alloc::pointer p = a.allocate(10);
#endif
      const std::size_t count1 = __gnu_test::counter::count();
      a.deallocate(p, 10);
      const std::size_t count2 = __gnu_test::counter::count();
      const bool __b((count2 < count1) == uses_global_delete);
      if (!__b)
	throw std::logic_error("counter not decremented");
      return __b;
    }
} // namespace __gnu_test

void* operator new(std::size_t size) THROW(std::bad_alloc)
{
  std::printf("operator new is called \n");
  void* p = std::malloc(size);
  if (!p)
    throw std::bad_alloc();
  __gnu_test::counter::increment();
  return p;
}

void operator delete(void* p) throw()
{
  std::printf("operator delete is called \n");
  if (p)
    {
      std::free(p);
      __gnu_test::counter::decrement();

      std::size_t count = __gnu_test::counter::count();
      if (count == 0)
	std::printf("All memory released \n");
      else
	std::printf("%lu allocations to be released \n", (unsigned long)count);
    }
}
