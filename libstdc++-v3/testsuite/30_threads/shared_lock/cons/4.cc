// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++14 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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


#include <shared_mutex>
#include <system_error>
#include <testsuite_hooks.h>

int main()
{
  typedef std::shared_timed_mutex mutex_type;
  typedef std::shared_lock<mutex_type> lock_type;

  try
    {
      mutex_type m;
      lock_type lock(m, std::try_to_lock);

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
