// 2002-01-23  Loren J. Rittle <rittle@labs.mot.com> <ljrittle@acm.org>
// Adapted from http://gcc.gnu.org/ml/gcc-bugs/2002-01/msg00679.html
// which was adapted from pthread1.cc by Mike Lu <MLu@dynamicsoft.com>
//
// Copyright (C) 2002-2024 Free Software Foundation, Inc.
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

#include <string>
#include <list>
#include <pthread.h>

using namespace std;

static list<string> foo;
static pthread_mutex_t fooLock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t fooCondOverflow = PTHREAD_COND_INITIALIZER;
static pthread_cond_t fooCondUnderflow = PTHREAD_COND_INITIALIZER;
static unsigned max_size = 10;
#if defined(__CYGWIN__)
static int iters = 10000;
#else
static int iters = 300000;
#endif

void*
produce (void*)
{
  for (int num = 0; num < iters; )
    {
      string str ("test string");

      pthread_mutex_lock (&fooLock);
      while (foo.size () >= max_size)
	pthread_cond_wait (&fooCondOverflow, &fooLock);
      foo.push_back (str);
      num++;
      if (foo.size () >= (max_size / 2))
	pthread_cond_signal (&fooCondUnderflow);
      pthread_mutex_unlock (&fooLock);
    }

  // No more data will ever be written, ensure no fini race
  pthread_mutex_lock (&fooLock);
  pthread_cond_signal (&fooCondUnderflow);
  pthread_mutex_unlock (&fooLock);

  return 0;
}

void*
consume (void*)
{
  for (int num = 0; num < iters; )
    {
      pthread_mutex_lock (&fooLock);
      while (foo.size () == 0)
	pthread_cond_wait (&fooCondUnderflow, &fooLock);
      while (foo.size () > 0)
	{
	  string str = foo.back ();
	  foo.pop_back ();
	  num++;
	}
      pthread_cond_signal (&fooCondOverflow);
      pthread_mutex_unlock (&fooLock);
    }

  return 0;
}

int
main (void)
{
#if defined(__sun) && defined(__svr4__) && _XOPEN_VERSION >= 500
  pthread_setconcurrency (2);
#endif

  pthread_t prod;
  pthread_create (&prod, 0, produce, 0);
  pthread_t cons;
  pthread_create (&cons, 0, consume, 0);

  pthread_join (prod, 0);
  pthread_join (cons, 0);

  return 0;
}
