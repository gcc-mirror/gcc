// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

#include <thread>
#include <exception>
#include <cstdlib>
#if !_GLIBCXX_USE_C99_STDLIB && defined _GLIBCXX_HAVE_UNISTD_H
# include <unistd.h>
#endif

void handle_terminate()
{
#if _GLIBCXX_USE_C99_STDLIB
  std::_Exit(0);
#elif defined _GLIBCXX_HAVE_UNISTD_H
  _exit(0);
#else
  std::exit(0);
#endif
}

void f() { throw 1; }

void
test01()
{
  std::set_terminate(handle_terminate);
  std::thread t(f);
  // This should call the terminate handler and exit with zero status:
  t.join();
  // Should not reach here:
  std::abort();
}

int
main()
{
  test01();
}
