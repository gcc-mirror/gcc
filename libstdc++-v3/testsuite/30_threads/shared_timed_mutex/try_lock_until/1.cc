// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++14 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include <thread>
#include <system_error>
#include <testsuite_hooks.h>
#include <slow_clock.h>

template <typename clock_type>
void test()
{
  typedef std::shared_timed_mutex mutex_type;

  try
    {
      mutex_type m;
      m.lock();
      bool b;

      std::thread t([&] {
	try
	  {
	    using namespace std::chrono;
	    const auto timeout = 100ms;

	    {
	      const auto start = clock_type::now();
	      const auto b = m.try_lock_until(start + timeout);
	      const auto t = clock_type::now() - start;
	      VERIFY( !b );
	      VERIFY( t >= timeout );
	    }

	    {
	      const auto start = clock_type::now();
	      const auto b = m.try_lock_shared_until(start + timeout);
	      const auto t = clock_type::now() - start;
	      VERIFY( !b );
	      VERIFY( t >= timeout );
	    }
	  }
	catch (const std::system_error& e)
	  {
	    VERIFY( false );
	  }
	});
      t.join();
      m.unlock();
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
  test<std::chrono::system_clock>();
  test<std::chrono::steady_clock>();
  test<__gnu_test::slow_clock>();
}
