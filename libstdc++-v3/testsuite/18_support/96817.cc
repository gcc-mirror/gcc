// Copyright (C) 2020-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target hosted }

// Static init cannot detect recursion for gthreads targets without futexes
// (and the futex case can only detect it if __libc_single_threaded==true).
// { dg-skip-if "unsupported" { gthreads && { ! futex } } }

// PR libstdc++/96817

#include <exception>
#include <stdlib.h>

int init()
{
#if __has_include(<sys/single_threaded.h>)
  // This deadlocks unless __libc_single_threaded is available in Glibc,
  // because __cxa_guard_acquire uses __gthread_active_p and the
  // multithreaded init can't detect recursion (see PR 97211).
  static int i = init();
#endif
  return 0;
}

void clean_terminate() { _Exit(0); }

int
main ()
{
  std::set_terminate(clean_terminate);
  init();
}
