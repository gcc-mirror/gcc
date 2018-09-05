// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }

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


#include <chrono>
#include <mutex>
#include <system_error>
#include <testsuite_hooks.h>

int main()
{
  typedef std::timed_mutex mutex_type;
  typedef std::unique_lock<mutex_type> lock_type;
  typedef std::chrono::system_clock clock_type;

  try 
    {
      clock_type::duration d = std::chrono::seconds(5);

      mutex_type m;
      lock_type lock(m, d);

      VERIFY( lock.owns_lock() );
      VERIFY( (bool)lock );
    }
  catch (const std::system_error& e)
    {
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }

  return 0;
}
