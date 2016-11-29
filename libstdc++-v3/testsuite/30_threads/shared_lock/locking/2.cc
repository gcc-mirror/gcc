// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* powerpc-ibm-aix* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* powerpc-ibm-aix* } }
// { dg-require-effective-target c++14 }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

void test01()
{
  typedef std::shared_timed_mutex mutex_type;
  typedef std::shared_lock<mutex_type> lock_type;

  try
    {
      lock_type l;

      // Lock shared_lock w/o mutex
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
  typedef std::shared_timed_mutex mutex_type;
  typedef std::shared_lock<mutex_type> lock_type;

  try
    {
      mutex_type m;
      lock_type l(m);

      // Lock already locked shared_lock.
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
