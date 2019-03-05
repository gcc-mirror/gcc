// 20020717 gdr

// Copyright (C) 2002-2019 Free Software Foundation, Inc.
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

// Test slice class invariants

#include <valarray>
#include <cstdlib>
#include <testsuite_hooks.h>

bool
construction(std::size_t start, std::size_t size, std::size_t stride)
{
  std::slice s(start, size, stride);
  return s.start() == start && s.size() == size && s.stride() == stride;
}

bool
copy(std::size_t start, std::size_t size, std::size_t stride)
{
  std::slice s(start, size, stride);
  std::slice t = s;
  return t.start() == start && t.size() == size && t.stride() == stride;
}

bool
assignment(std::size_t start, std::size_t size, std::size_t stride)
{
  std::slice s(start, size, stride);
  std::slice t;
  t = s;
  return t.start() == start && t.size() == size && t.stride() == stride;
}


int main()
{
  std::srand(20020717);         
  using std::rand;
  VERIFY(construction(rand(), rand(), rand()));

  VERIFY(copy(rand(), rand(), rand()));

  VERIFY(assignment(rand(), rand(), rand()));

  return 0;
}
