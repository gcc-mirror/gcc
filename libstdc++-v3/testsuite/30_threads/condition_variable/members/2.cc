// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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
#include <slow_clock.h>

template <typename ClockType>
void test01()
{
  try 
    {
      std::chrono::microseconds ms(500);
      std::condition_variable c1;
      std::mutex m;
      std::unique_lock<std::mutex> l(m);

      auto then = ClockType::now();
      std::cv_status result = c1.wait_until(l, then + ms);
      VERIFY( result == std::cv_status::timeout );
      VERIFY( (ClockType::now() - then) >= ms );
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

void test01_alternate_clock()
{
  using __gnu_test::slow_clock;

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

/* User defined clock that ticks in two-thousandths of a second
   forty-two minutes ahead of steady_clock. */
struct user_defined_clock
{
  typedef std::chrono::steady_clock::rep rep;
  typedef std::ratio<1, 2000> period;
  typedef std::chrono::duration<rep, period> duration;
  typedef std::chrono::time_point<user_defined_clock> time_point;

  static constexpr bool is_steady = true;

  static time_point now() noexcept
  {
    using namespace std::chrono;
    const auto steady_since_epoch = steady_clock::now().time_since_epoch();
    const auto user_since_epoch = duration_cast<duration>(steady_since_epoch);
    return time_point(user_since_epoch + minutes(42));
  }
};

/*
It's not possible for this test to automatically ensure that the
system_clock test cases result in a wait on CLOCK_REALTIME and steady_clock
test cases result in a wait on CLOCK_MONOTONIC. It's recommended to run the
test under strace(1) and check whether the expected futex calls are made by
glibc. See https://gcc.gnu.org/ml/libstdc++/2019-09/msg00022.html for
instructions.
*/

int main()
{
  test01<std::chrono::steady_clock>();
  test01<std::chrono::system_clock>();
  test01<user_defined_clock>();
  test01_alternate_clock();
}
