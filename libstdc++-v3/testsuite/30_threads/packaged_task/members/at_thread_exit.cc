// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

bool executed = false;

int execute(int i) { executed = true; return i + 1; }

std::future<int> f1;

bool ready(std::future<int>& f)
{
  return f.wait_for(std::chrono::milliseconds(1)) == std::future_status::ready;
}

void test01()
{
  std::packaged_task<int(int)> p1(execute);
  f1 = p1.get_future();

  p1.make_ready_at_thread_exit(1);

  VERIFY( executed );
  VERIFY( p1.valid() );
  VERIFY( !ready(f1) );
}

int main()
{
  std::thread t{test01};
  t.join();
  VERIFY( ready(f1) );
  VERIFY( f1.get() == 2 );
}
