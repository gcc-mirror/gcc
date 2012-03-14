// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-sched-yield "" }
 
// Copyright (C) 2011, 2012 Free Software Foundation, Inc.
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

#include <condition_variable>
#include <thread>
#include <mutex>
#include <array>
#include <sstream>

struct scoped_thread
{
  ~scoped_thread() { if (t.joinable()) t.join(); }
  std::thread t;
};

int main()
{
  typedef std::unique_lock<std::mutex> Lock;

  std::mutex                  m;
  std::condition_variable_any cond;
  unsigned int                product = 0;
  const unsigned int          count = 10;

  // writing to stream causes timing changes which makes deadlock easier
  // to reproduce - do not remove
  std::ostringstream out;

  // create consumers
  std::array<scoped_thread, 2> threads;
  for (std::size_t i = 0; i < threads.size(); ++i)
    threads[i].t
      = std::thread( [&]
		     {
		       for (unsigned int i = 0; i < count; ++i)
			 {
			   std::this_thread::yield();
			   Lock lock(m);
			   while(product == 0)
			     cond.wait(lock);
			   out << "got product "
			       << std::this_thread::get_id()
			       << ' ' << product << std::endl;
			   --product;
			 }
		     } );

  // single producer
  for (std::size_t i = 0; i < threads.size() * count; ++i)
    {
      std::this_thread::yield();
      Lock lock(m);
      ++product;
      out << "setting product " << std::this_thread::get_id()
	  << ' ' << product << std::endl;
      cond.notify_one();
    }
}
