// 2001-11-25  Phil Edwards  <pme@gcc.gnu.org>
//
// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

// 20.4.1.1 allocator members

#include <cstdlib>
#include <ext/new_allocator.h>
#include <replacement_memory_operators.h>

int main()
{ 
  typedef __gnu_cxx::new_allocator<unsigned int> allocator_type;
  __gnu_test::check_new<allocator_type, true>(); 
  return 0;
}
