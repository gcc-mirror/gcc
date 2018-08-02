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

#include <chrono>
#include <condition_variable>
#include <system_error>
#include <testsuite_hooks.h>

void test01()
{
  try 
    {
      std::chrono::microseconds ms(500);
      std::condition_variable c1;
      std::mutex m;
      std::unique_lock<std::mutex> l(m);

      auto then = std::chrono::steady_clock::now();
      std::cv_status result = c1.wait_until(l, then + ms);
      VERIFY( result == std::cv_status::timeout );
      VERIFY( (std::chrono::steady_clock::now() - then) >= ms );
      VERIFY( l.owns_lock() );
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

struct slow_clock
{
  using rep = std::chrono::system_clock::rep;
  using period = std::chrono::system_clock::period;
  using duration = std::chrono::system_clock::duration;
  using time_point = std::chrono::time_point<slow_clock, duration>;
  static constexpr bool is_steady = false;

  static time_point now()
  {
    auto real = std::chrono::system_clock::now();
    return time_point{real.time_since_epoch() / 3};
  }
};


void test01_alternate_clock()
{
  try
    {
      std::condition_variable c1;
      std::mutex m;
      std::unique_lock<std::mutex> l(m);
      auto const expire = slow_clock::now() + std::chrono::seconds(1);

      while (slow_clock::now() < expire)
       {
         auto const result = c1.wait_until(l, expire);

         // If wait_until returns before the timeout has expired when
         // measured against the supplied clock, then wait_until must
         // return no_timeout.
         if (slow_clock::now() < expire)
           VERIFY(result == std::cv_status::no_timeout);

         // If wait_until returns timeout then the timeout must have
         // expired.
         if (result == std::cv_status::timeout)
           VERIFY(slow_clock::now() >= expire);
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
  test01_alternate_clock();
  return 0;
}
