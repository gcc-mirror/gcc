// 2002-01-23  Loren J. Rittle <rittle@labs.mot.com> <ljrittle@acm.org>
// Adpated from libstdc++/5464 submitted by jjessel@amadeus.net
// Jean-Francois JESSEL (Amadeus SAS Development) 
//
// Copyright (C) 2002-2018 Free Software Foundation, Inc.
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

#include <vector>
#include <list>
#include <string>
#include <cstdlib>
#include <pthread.h>

#ifdef _GLIBCXX_HAVE_UNISTD_H
#include <unistd.h>	// To test for _POSIX_THREAD_PRIORITY_SCHEDULING
#endif

#define NTHREADS 8
#define LOOPS 20

struct tt_t
{
  char buf[100];
  int  i;
};

void*
thread_function (void* arg)
{
  typedef std::vector<tt_t>		vector_type;
  typedef std::list<std::string*>	list_type;

  int myid __attribute__((unused)) = *(int*) arg;
  for (int i = 0; i < LOOPS; i++)
    {
      vector_type myvect1;

      for (int j = 0; j < 2000; j++)
	{
	  vector_type myvect2;
	  tt_t v;
	  v.i = j;
	  myvect1.push_back (v);
	  myvect2.push_back (v);
	  list_type mylist;
	  std::string string_array[4];
	  string_array[0] = "toto";
	  string_array[1] = "titi";
	  string_array[2] = "tata";
	  string_array[3] = "tutu";
	  for (int k = 0; k < 4; k++)
	    {
	      if (mylist.size ())
		{
		  list_type::iterator aIt;
		  for (aIt = mylist.begin (); aIt != mylist.end (); ++aIt)
		    {
		      if ((*aIt) == &(string_array[k]))
			abort ();
		    }
		}
	      mylist.push_back (&(string_array[k]));
	    }
	}
    }

  return arg;
}

int
main ()
{
  int worker;
  pthread_t threads[NTHREADS];
  int ids[NTHREADS];
  void* status;

#if defined(__sun) && defined(__svr4__) && _XOPEN_VERSION >= 500
  pthread_setconcurrency (NTHREADS);
#endif

  pthread_attr_t tattr;
  int ret __attribute__((unused)) = pthread_attr_init (&tattr);
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
  ret = pthread_attr_setscope(&tattr, PTHREAD_SCOPE_SYSTEM);
#endif

  for (worker = 0; worker < NTHREADS; worker++)
    {
      ids[worker] = worker;
      if (pthread_create(&threads[worker], &tattr,
			 thread_function, &ids[worker]))
	abort ();
    }

  for (worker = 0; worker < NTHREADS; worker++)
    {
      if (pthread_join(threads[worker], static_cast<void **>(&status)))
	abort ();

      if (*((int *)status) != worker)
	abort ();
    }

  return (0);
}
