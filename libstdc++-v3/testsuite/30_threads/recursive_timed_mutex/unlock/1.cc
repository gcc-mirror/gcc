// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }

// Copyright (C) 2008-2019 Free Software Foundation, Inc.
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


#include <mutex>
#include <system_error>
#include <testsuite_hooks.h>

int main()
{
  typedef std::recursive_timed_mutex mutex_type;

  try 
    {
      // Unlock mutex that hasn't been locked.
      mutex_type m;
      m.unlock();
    }
  catch (const std::system_error& e)
    {
      // POSIX == EPERM
      VERIFY( true );
    }
  catch (...)
    {
      VERIFY( false );
    }

  return 0;
}
