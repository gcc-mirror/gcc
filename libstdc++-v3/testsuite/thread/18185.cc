//
// Copyright (C) 2004 Free Software Foundation, Inc.
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
// { dg-options "-pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* } }
// { dg-options "-pthreads" { target *-*-solaris* } }

#include <ext/new_allocator.h>
#include <string>
#include <pthread.h>

static void *
foo (void *p)
{
  typedef std::char_traits<char> traits_type;
  typedef __gnu_cxx::new_allocator<char> allocator_type;
  typedef std::basic_string<char, traits_type, allocator_type> string_type;
  try
    {
      throw string_type("leak");
    }
  catch (const string_type&)
    {
      pthread_exit (0);
    }
}

// c++/18185
// This used to leak memory.
int
main ()
{
  pthread_t t;
  int j = pthread_create (&t, 0, foo, 0);
  int i = pthread_join (t, 0);
  return 0;
}
