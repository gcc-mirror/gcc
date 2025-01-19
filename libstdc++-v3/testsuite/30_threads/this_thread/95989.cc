// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-gthreads {} }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-additional-options "-static" { target static } }

#include <thread>
#include <testsuite_hooks.h>

__attribute__((noinline,noipa))
void
join(std::thread& t)
{
  if (!t.joinable())
    return;

  // Using thread::join() creates a dependency on libpthread symbols
  // so that __gthread_active_p is true, and we use pthread_self.
  t.join();
}

void
test01()
{
  std::thread t;
  // PR libstdc++/95989
  auto id = std::this_thread::get_id();
  VERIFY (t.get_id() != id );
}

int
main()
{
  test01();
}
