//
// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

// { dg-require-time "" }

#include <ext/throw_allocator.h>
#include <testsuite_allocator.h>

int main()
{ 
  typedef int value_type;
  typedef __gnu_cxx::throw_allocator_random<value_type> allocator_type;
  
  try { __gnu_test::check_deallocate_null<allocator_type>(); }
  catch (std::logic_error&)
    {
      // Should throw logic_error to catch null erase.
    }

  return 0;
}

