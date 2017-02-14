// 2002-01-23  Loren J. Rittle <rittle@labs.mot.com> <ljrittle@acm.org>
//
// Copyright (C) 2002-2017 Free Software Foundation, Inc.
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

// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* } }

// This multi-threading C++/STL/POSIX code adheres to rules outlined here:
// http://www.sgi.com/tech/stl/thread_safety.html
//
// It is believed to exercise the allocation code in a manner that
// should reveal memory leaks (and, under rare cases, race conditions,
// if the STL threading support is fubar'd).

#include <list>
#include <cstdlib>
#include <pthread.h>

const int thread_cycles = 10;
const int thread_pairs = 10;
const unsigned max_size = 100;
const int iters = 10000;

class task_queue
{
  typedef std::list<int> list_type;

public:
  task_queue ()
  {
    pthread_mutex_init (&fooLock, 0);
    pthread_cond_init (&fooCond1, 0);
    pthread_cond_init (&fooCond2, 0);
  }
  ~task_queue ()
  {
    pthread_mutex_destroy (&fooLock);
    pthread_cond_destroy (&fooCond1);
    pthread_cond_destroy (&fooCond2);
  }

  list_type		foo;
  pthread_mutex_t 	fooLock;
  pthread_cond_t 	fooCond1;
  pthread_cond_t 	fooCond2;
};

void*
produce(void* t)
{
  task_queue& tq = *(static_cast<task_queue*> (t));
  int num = 0;
  while (num < iters)
    {
      pthread_mutex_lock (&tq.fooLock);
      while (tq.foo.size () >= max_size)
	pthread_cond_wait (&tq.fooCond1, &tq.fooLock);
      tq.foo.push_back (num++);
      pthread_cond_signal (&tq.fooCond2);
      pthread_mutex_unlock (&tq.fooLock);
    }
  return 0;
}

void*
consume(void* t)
{
  task_queue& tq = *(static_cast<task_queue*> (t));
  int num = 0;
  while (num < iters)
    {
      pthread_mutex_lock (&tq.fooLock);
      while (tq.foo.size () == 0)
	pthread_cond_wait (&tq.fooCond2, &tq.fooLock);
      if (tq.foo.front () != num++)
	abort ();
      tq.foo.pop_front ();
      pthread_cond_signal (&tq.fooCond1);
      pthread_mutex_unlock (&tq.fooLock);
    }
  return 0;
}

int
main()
{
  pthread_t prod[thread_pairs];
  pthread_t cons[thread_pairs];

  task_queue* tq[thread_pairs];

#if defined(__sun) && defined(__svr4__) && _XOPEN_VERSION >= 500
  pthread_setconcurrency (thread_pairs * 2);
#endif

  for (int j = 0; j < thread_cycles; j++)
    {
      for (int i = 0; i < thread_pairs; i++)
	{
	  tq[i] = new task_queue;
	  pthread_create (&prod[i], 0, produce, static_cast<void*> (tq[i]));
	  pthread_create (&cons[i], 0, consume, static_cast<void*> (tq[i]));
	}

      for (int i = 0; i < thread_pairs; i++)
	{
	  pthread_join (prod[i], 0);
	  pthread_join (cons[i], 0);
	  delete tq[i];
	}
    }

  return 0;
}
