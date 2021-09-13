// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-add-options libatomic }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <atomic>
#include <thread>

#include <testsuite_hooks.h>

int
main ()
{
  struct S{ int i; };
  S aa{ 0 };
  S bb{ 42 };

  std::atomic<S> a{ aa };
  VERIFY( a.load().i == aa.i );
  a.wait(bb);
  std::thread t([&]
    {
      a.store(bb);
      a.notify_one();
    });
  a.wait(aa);
  t.join();
  return 0;
}
