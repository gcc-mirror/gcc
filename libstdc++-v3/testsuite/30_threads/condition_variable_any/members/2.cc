// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

struct Mutex
{
  Mutex() : locked(false) { }

  void lock()
  {
    if (locked)
      throw locked;
    mtx.lock();
    locked = true;
  }

  void unlock()
  {
    if (!locked)
      throw locked;
    mtx.unlock();
    locked = false;
  }

  std::mutex mtx;
  bool locked;
};


template <typename ClockType>
void test01()
{
  try 
    {
      std::chrono::microseconds ms(500);
      std::condition_variable_any c1;
      Mutex m;
      m.lock();

      auto then = ClockType::now();
      std::cv_status result = c1.wait_until(m, then + ms);
      VERIFY( result == std::cv_status::timeout );
      VERIFY( (ClockType::now() - then) >= ms );
      VERIFY( m.locked );
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

int main()
{
  test01<std::chrono::steady_clock>();
  test01<std::chrono::system_clock>();
  test01<user_defined_clock>();
}
