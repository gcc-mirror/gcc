// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-do link { target c++11 } }
// { dg-options "-static" { target *-*-*gnu* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-effective-target static }

#include <thread>

extern "C" {
  // Should not get multiple definition errors from libstdc++.a(thread.o)
  void execute_native_thread_routine(void) { }
  void execute_native_thread_routine_compat(void) { }
}

int main()
{
  execute_native_thread_routine();
  execute_native_thread_routine_compat();

  std::thread{}.detach();  // ensure libstdc++.a(thread.o) is linked in
}
