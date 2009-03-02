//
// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-darwin* alpha*-*-osf* } }

#include <cstdlib>
#include <pthread.h>

// This used to deadlock with the old libstdc++ because there is only one
// global mutex guarding initialization of statics and it is held during by
// the initializer thread of a static until the variable is completely
// initialized. If the initializer thread creates and waits for another thread
// which also initializes a static variable, there will be a deadlock because
// the first thread is holding the mutex and waiting for the second thread,
// which is blocked when it is acquiring the mutex.

int
get_bar (void)
{
  return 1;
}

void*
do_something (void *arg)
{
  static int bar __attribute__((unused)) = get_bar ();
  return NULL;
}

int
get_foo (void)
{
  pthread_t new_thread;

  if (pthread_create (&new_thread, NULL, do_something, NULL) != 0)
    std::abort ();

  if (pthread_join (new_thread, NULL) != 0)
    std::abort ();

  return 1;
}

int
main (int argc, char **argv)
{
  static int foo __attribute__((unused)) = get_foo ();
  return 0;  
}
