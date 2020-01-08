// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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


#include <future>
#include <testsuite_hooks.h>

struct sum {
  typedef int result_type;
  int operator()(int i, int& j, const int& k) { return i + j + k; }
};

void test01()
{
  using namespace std;

  int a = 1;
  int b = 1;
  int c = 1;
  future<int> f1 = async(launch::deferred, sum(), a, ref(b), cref(c));
  a = 0;
  b = 10;
  c = 100;

  const std::chrono::seconds delay(10);
  const auto then = std::chrono::system_clock::now() + delay;

  VERIFY( f1.valid() );
  // timed waiting functions should return 'deferred' immediately
  VERIFY( f1.wait_until(then) == std::future_status::deferred );
  VERIFY( f1.wait_for(delay) == std::future_status::deferred );
  VERIFY( std::chrono::system_clock::now() < then );

  f1.wait();

  VERIFY( f1.valid() );
  // timed waiting functions should return 'ready' immediately
  VERIFY( f1.wait_until(then) == std::future_status::ready );
  VERIFY( f1.wait_for(delay) == std::future_status::ready );
  VERIFY( std::chrono::system_clock::now() < then );

  VERIFY( f1.get() == 111 );
  VERIFY( !f1.valid() );
}

int main()
{
  test01();
  return 0;
}
