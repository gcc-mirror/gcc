// { dg-do run { target *-*-linux* *-*-gnu* } }
// { dg-options "-pthread" }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }
// { dg-xfail-run-if "cannot catch forced_unwind" { *-*-* } { "-fno-rtti" } }

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

// Test (non-standard) handling of __forced_unwind exception.

#include <future>
#include <stdexcept>
#include <pthread.h>
#include <testsuite_hooks.h>

void f() { pthread_exit(nullptr); }

int main()
{
  auto fut = std::async(std::launch::async, f);
  try
  {
    fut.get();
    throw std::logic_error("Unreachable");
  }
  catch (const std::future_error& e)
  {
    VERIFY( e.code() == std::future_errc::broken_promise );
  }
}
