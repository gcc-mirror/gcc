//
// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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

#include <memory>
#include <ext/debug_allocator.h>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

int main()
{ 
  typedef int value_type;
  typedef std::allocator<value_type> debug_type;
  typedef __gnu_cxx::debug_allocator<debug_type> allocator_type;

  try
    {
      __gnu_test::check_deallocate_null<allocator_type>(); 
    }
  catch (std::runtime_error& obj)
    {
      // Ok.
    }
  catch (...)
    { 
      // Shouldn't get here.
      throw;
    }
  return 0;
}

