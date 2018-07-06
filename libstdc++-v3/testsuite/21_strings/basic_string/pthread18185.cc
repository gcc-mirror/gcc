//
// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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
// { dg-options "-pthread"  }
// { dg-require-effective-target pthread }

#include <ext/new_allocator.h>
#include <string>
#include <pthread.h>

static void*
foo (void*)
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
  return 0; // See libstdc++/83450
}

// c++/18185
// This used to leak memory.
int
main ()
{
  pthread_t t;
  pthread_create (&t, 0, foo, 0);
  pthread_join (t, 0);
  return 0;
}
