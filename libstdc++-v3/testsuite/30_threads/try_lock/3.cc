// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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

struct user_lock
{
  user_lock() : is_locked(false) { }
  ~user_lock() = default;
  user_lock(const user_lock&) = default;

  void lock()
  {
    bool test __attribute__((unused)) = true;
    VERIFY( !is_locked );
    is_locked = true;
  }

  bool try_lock() 
  { return is_locked ? false : (is_locked = true); }

  void unlock()
  {
    bool test __attribute__((unused)) = true;
    VERIFY( is_locked );
    is_locked = false;
  }

private:
  bool is_locked;
};

int main()
{
  bool test __attribute__((unused)) = true;

  try
    {
      std::mutex m1;
      std::recursive_mutex m2;
      user_lock m3;

      try
	{
	  //heterogeneous types
	  int result = std::try_lock(m1, m2, m3);
	  VERIFY( result == -1 );
	  m1.unlock();
	  m2.unlock();
	  m3.unlock();
	}
      catch (const std::system_error& e)
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

  return 0;
}
