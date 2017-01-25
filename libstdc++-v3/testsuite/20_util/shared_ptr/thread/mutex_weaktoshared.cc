// Copyright (C) 2006-2017 Free Software Foundation, Inc.
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

// 20.6.6.2 Template class shared_ptr [util.smartptr.shared]

// { dg-do run { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-rtems* *-*-darwin* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-dragonfly* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* } }
// { dg-require-effective-target c++11 }

#include <memory>
#include <random>
#include <vector>
#include <iostream>
#include <cstdlib>
#include <thread>
#include <atomic>
#include <functional>
#include <testsuite_hooks.h>

#ifdef _GLIBCXX_HAVE_UNISTD_H
#include <unistd.h>	// To test for _POSIX_THREAD_PRIORITY_SCHEDULING
#endif

/* This (brute-force) tests the atomicity and thus thread safety of the
 * shared_ptr <- weak_ptr
 * assignment operation by allocating a test object, retrieving a weak
 * reference to it, and letting a number of threads repeatedly create strong
 * references from the weak reference.
 * Specifically, this tests the function _Sp_counted_base<true>::add_ref_lock()
 */


const unsigned int HAMMER_MAX_THREADS = 10;
const unsigned int POOL_SIZE = 1000;
const unsigned long HAMMER_REPEAT = 100000;
const unsigned long KILL_ONE_IN = 1000;

struct A
{
  static std::atomic<int> counter;
  A() { counter.fetch_add(1, std::memory_order_relaxed); }
 ~A() { counter.fetch_sub(1, std::memory_order_relaxed); }
};

std::atomic<int> A::counter{0};

using std::_S_mutex;

typedef std::__shared_ptr<A, _S_mutex> sp_A_t;
typedef std::__weak_ptr<A, _S_mutex> wp_A_t;

typedef std::vector<sp_A_t> sp_vector_t;
typedef std::vector<wp_A_t> wp_vector_t;

struct shared_and_weak_pools
{
  sp_vector_t& shared_pool;
  wp_vector_t& weak_pool;
  
  shared_and_weak_pools(sp_vector_t& _shared_pool, wp_vector_t& _weak_pool)
    : shared_pool(_shared_pool), weak_pool(_weak_pool)
    { }
};

void thread_hammer_and_kill(shared_and_weak_pools& pools)
{
  std::mt19937 urbg;
  std::uniform_int_distribution<> dist(0, KILL_ONE_IN - 1);

  sp_vector_t::iterator cur_shared = pools.shared_pool.begin();
  wp_vector_t::iterator cur_weak = pools.weak_pool.begin();
  
  for (unsigned int i = 0; i < HAMMER_REPEAT; ++i)
    {
      try
      {
        sp_A_t strong(*cur_weak);
      }
      catch (std::bad_weak_ptr& exception)
      {
        ++cur_weak;
        if (cur_weak == pools.weak_pool.end())
          break;
      }
      
      if (dist(urbg) == 0)
      {
        cur_shared->reset();
        ++cur_shared;
      }
    }
}

void thread_hammer(wp_vector_t& weak_pool)
{
  wp_vector_t::iterator cur_weak = weak_pool.begin();

  for (unsigned int i = 0; i < HAMMER_REPEAT; ++i)
    {
      try
      {
        sp_A_t strong(*cur_weak);
      }
      catch (std::bad_weak_ptr& exception)
      {
        ++cur_weak;
        if (cur_weak == weak_pool.end())
          break;
      }
    }
}

void
test01()
{
  sp_vector_t obj_pool(POOL_SIZE);
  
  for(auto& obj : obj_pool)
    obj.reset(new A);

  // Obtain weak references.
  std::vector<wp_vector_t> weak_pool(HAMMER_MAX_THREADS, wp_vector_t(obj_pool.begin(), obj_pool.end()));
  
  // Launch threads with pointer to weak reference.
  std::thread threads[HAMMER_MAX_THREADS];
#if defined(__sun) && defined(__svr4__) && _XOPEN_VERSION >= 500
  pthread_setconcurrency (HAMMER_MAX_THREADS);
#endif
  
  shared_and_weak_pools pools(obj_pool, weak_pool[0]);
  threads[0] = std::thread(thread_hammer_and_kill, std::ref(pools));
  for (unsigned int worker = 1; worker < HAMMER_MAX_THREADS; worker++)
    threads[worker] = std::thread(thread_hammer, std::ref(weak_pool[worker]));

  // Wait for threads to complete, then check integrity of reference.
  for (auto& thread : threads)
    thread.join();

  obj_pool.clear();
  
  VERIFY( A::counter == 0 );
}

int 
main()
{
  test01();
  return 0;
}
