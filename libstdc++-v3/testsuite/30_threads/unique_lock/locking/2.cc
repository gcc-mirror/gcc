// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

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

void test01()
{
  typedef std::mutex mutex_type;
  typedef std::unique_lock<mutex_type> lock_type;

  try
    {
      lock_type l;

      // Lock unique_lock w/o mutex
      try
        {
          l.lock();
        }
      catch (const std::system_error& ex)
        {
	  VERIFY( ex.code() == std::make_error_code
		  (std::errc::operation_not_permitted) );
        }
      catch (...)
        {
          VERIFY( false );
        }
    }
  catch (const std::system_error& e)
    {
      VERIFY( false );
    }
  catch (...)
    {
      VERIFY( false );
    }
}


void test02()
{
  typedef std::mutex mutex_type;
  typedef std::unique_lock<mutex_type> lock_type;

  try 
    {
      mutex_type m;
      lock_type l(m);

      // Lock already locked unique_lock.
      try
	{
	  l.lock();
	}
      catch (const std::system_error& ex)
	{
	  VERIFY( ex.code() == std::make_error_code
		  (std::errc::resource_deadlock_would_occur) );
	}
      catch (...)
	{
	  VERIFY( false );
	}
    }
  catch (const std::system_error& e)
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
  test01();
  test02();
}
