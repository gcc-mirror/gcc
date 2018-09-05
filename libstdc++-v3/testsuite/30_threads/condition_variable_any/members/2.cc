// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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


void test01()
{
  try 
    {
      std::chrono::microseconds ms(500);
      std::condition_variable_any c1;
      Mutex m;
      m.lock();

      auto then = std::chrono::steady_clock::now();
      std::cv_status result = c1.wait_until(m, then + ms);
      VERIFY( result == std::cv_status::timeout );
      VERIFY( (std::chrono::steady_clock::now() - then) >= ms );
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

int main()
{
  test01();
  return 0;
}
