// -*- C++ -*-
// Utility subroutines for the C++ library testsuite.
//
// Copyright (C) 2000-2021 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_TESTSUITE_NEW_OPERATORS_H
#define _GLIBCXX_TESTSUITE_NEW_OPERATORS_H

#include <new>
#include <testsuite_hooks.h>

namespace __gnu_test
{
  std::size_t&
  get_new_limit()
  {
    static std::size_t limit = 1024 * 1024;
    return limit;
  }

  void
  set_new_limit(std::size_t l)
  { get_new_limit() = l; }
}

void* operator new(std::size_t size) THROW(std::bad_alloc)
{
  if (size > __gnu_test::get_new_limit())
    throw std::bad_alloc();

  void* p = std::malloc(size);
  if (!p)
    throw std::bad_alloc();

  return p;
}

void* operator new (std::size_t size, const std::nothrow_t&) throw()
{
  if (size > __gnu_test::get_new_limit())
    return 0;

  return std::malloc(size);
}

void operator delete(void* p) throw()
{
  if (p)
    std::free(p);
}

#if __cpp_sized_deallocation
void operator delete(void* p, std::size_t) throw()
{ ::operator delete(p); }
#endif

void operator delete(void* p, const std::nothrow_t&) throw()
{
  if (p)
    std::free(p);
}


#endif // _GLIBCXX_TESTSUITE_NEW_OPERATORS_H


