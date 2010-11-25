// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-debug-mode "" }
// Copyright (C) 2010 Free Software Foundation, Inc.
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
//

// This test check for potential deadlock when swaping sequences in debug
// mode as it requires acquiring 2 locks at the same time.

#include <vector>
#include <thread>
#include <functional>
#include <testsuite_hooks.h>

// The following function mimic the one in src/debug.cc to associate a mutex
// to a given safe sequence instance.
size_t
get_index(std::vector<int>& v)
{
  const size_t mask = 0xf;
  // We have to check the address of the internal safe sequence that starts
  // after the normal vector memory footprint that is to say a 3 pointers
  // offset:
  void* __address = reinterpret_cast<char*>(&v) + 3 * sizeof(void*);
  return std::_Hash_impl::hash(__address) & mask;
}

void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  vector<int> v1, v2;
  vector<shared_ptr<vector<int> > > vs;
  vector<int> *pv3 = 0, *pv4 = 0;
  const int nb_attempts = 100;
  for (int i = 0; i != nb_attempts; ++i)
    {
      vs.push_back(shared_ptr<vector<int> >(new vector<int>()));
      if (!pv3)
	{
	  if (get_index(*vs.back()) == get_index(v1))
	    pv3 = vs.back().get();
	}
      else if (!pv4)
	{
	  if (get_index(*vs.back()) == get_index(v2))
	    {
	      pv4 = vs.back().get();
	      break;
	    }
	}
    }

  if (!pv3 || !pv4)
    // Maybe an other time...
    return;

  vector<int> &v3 = *pv3, &v4 = *pv4;

  // v1 and v3 shares the same mutex instance, like v2 and v4
  // thread t1 lock v1 and v2
  thread t1([&v1, &v2]()
    {
      for (int i = 0; i != 1000; ++i)
	v1.swap(v2);
    });
  // thread t2 lock v4 and v3
  thread t2([&v3, &v4]()
    {
      for (int i = 0; i != 1000; ++i)
	v4.swap(v3);
    });
  t2.join();
  t1.join();
}

int main()
{
  test01();
  return 0;
}
