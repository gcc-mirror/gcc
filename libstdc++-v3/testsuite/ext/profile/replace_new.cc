// -*- C++ -*-

// Copyright (C) 2006-2017 Free Software Foundation, Inc.
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

// { dg-require-profile-mode "" }

#include <vector>
#include <testsuite_hooks.h>

using std::vector;

void* operator new(std::size_t size) THROW(std::bad_alloc)
{
  void* p = std::malloc(size);
  if (!p)
    throw std::bad_alloc();
  return p;
}

void* operator new (std::size_t size, const std::nothrow_t&) throw()
{
  // With _GLIBCXX_PROFILE, the instrumentation of the vector constructor
  // will call back into this new operator.
  vector<int> v;
  return std::malloc(size);
}

void operator delete(void* p) throw()
{
  if (p)
    std::free(p);
}

int
main() 
{
  vector<int> v;
  return 0;
}
