// 2003-05-03  Loren J. Rittle <rittle@labs.mot.com> <ljrittle@acm.org>
//
// Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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

// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options "-pthreads" { target *-*-solaris* } }

#include <ext/rope>
#include <cstring>
#include <pthread.h>
#include <testsuite_hooks.h>

const int max_thread_count = 4;
const int max_loop_count = 10000;

typedef __gnu_cxx::rope<char, std::allocator<char> > rope_type;
rope_type foo2;
rope_type foo4;

void* thread_main(void *) 
{
  // To see a problem with gcc 3.3 and before, set a break point here.
  // Single step through c_str implementation, call sched_yield after
  // capture of NULL __old_c_string in any thread.  Single step
  // through another thread past that same point.  Now, one thread
  // will receive a bad pointer return.  Adding dummy sched_yield
  // should never change program semantics under POSIX threads.
  const char* data4 = foo4.c_str();

  // Please note that the memory leak in the rope implementation with
  // this test case, existed before and after fixing this bug...
  bool test __attribute__((unused)) = true;
  VERIFY( !std::strcmp (data4, "barbazbonglehellohellohello") );
  return 0;
}

int
main()
{
  bool test __attribute__((unused)) = true;

  pthread_t tid[max_thread_count];

#if defined(__sun) && defined(__svr4__) && _XOPEN_VERSION >= 500
  pthread_setconcurrency (max_thread_count);
#endif

  rope_type foo;
  foo += "bar";
  foo += "baz";
  foo += "bongle";
  const char* data = foo.c_str();
  VERIFY( !std::strcmp (data, "barbazbongle") );

  const char* data2;
  {
    foo2 += "bar2";
    foo2 += "baz2";
    foo2 += "bongle2";
    data2 = foo2.c_str();
    VERIFY( !std::strcmp (data2, "bar2baz2bongle2") );
  }

  rope_type foo3 ("hello");
  const char* data3 = foo3.c_str();
  VERIFY( !std::strcmp (data3, "hello") );

  for (int j = 0; j < max_loop_count; j++)
    {
      foo4 = foo;
      foo4 += foo3;
      foo4 += foo3;
      foo4 += foo3;

      for (int i = 0; i < max_thread_count; i++)
	pthread_create (&tid[i], NULL, thread_main, 0);

      for (int i = 0; i < max_thread_count; i++)
	pthread_join (tid[i], NULL);
    }

  VERIFY( !std::strcmp (data, "barbazbongle") );
  VERIFY( !std::strcmp (data2, "bar2baz2bongle2") );

  return 0;
}
