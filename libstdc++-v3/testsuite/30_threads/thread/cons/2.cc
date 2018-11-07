// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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


#include <functional> // std::ref
#include <thread>
#include <system_error>
#include <testsuite_hooks.h>

void
free_function(std::thread::id& id)
{
  id = std::this_thread::get_id();
}

// thread::id default cons
// thread::id copy ctor
// thread variadic cons, c++ function
// thread variadic cons joinable
// thread join
// thread join postcondition not joinable
// thread join postcondition function called correctly
// this_thread::get_id
void test02()
{
  try
    {
      std::thread::id id1;
      std::thread t(free_function, std::ref(id1));
      std::thread::id id2 = t.get_id();
      VERIFY( t.joinable() );
      t.join();      
      VERIFY( !t.joinable() );
      VERIFY( id1 == id2 );
    }
  catch (const std::system_error&)
    {
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }
}

int main()
{
  test02();
  return 0;
}
