//
// Copyright (C) 2006-2018 Free Software Foundation, Inc.
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

// { dg-options "-Wno-deprecated" }

#include <ext/array_allocator.h>

// libstdc++/26875
int main()
{
  typedef std::tr1::array<int, 1> array_type;
  array_type Array1;
  array_type Array2;

  typedef __gnu_cxx::array_allocator<int> allocator_type;
  allocator_type Allocator1(&Array1);
  allocator_type Allocator2(&Array2);

  try
    {
      Allocator1.allocate(1);
      Allocator2.allocate(1);
    }
  catch (std::bad_alloc& ex)
    {
      // fail, rethrow
      throw;
    }
    
  return 0;
}

