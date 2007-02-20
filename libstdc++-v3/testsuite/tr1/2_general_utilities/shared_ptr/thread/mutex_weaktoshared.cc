// Copyright (C) 2006 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// TR1 2.2.2 Template class shared_ptr [tr.util.smartptr.shared]

// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* } }
// { dg-options "-pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* } }
// { dg-options "-pthreads" { target *-*-solaris* } }

#include <tr1/memory>
#include <tr1/random>
#include <vector>
#include <testsuite_hooks.h>
#include <iostream>

#include <pthread.h>

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
    static _Atomic_word counter;
    A()
      {
	__gnu_cxx::__atomic_add(&counter, 1);
      }
    ~A()
      {
	__gnu_cxx::__atomic_add(&counter, -1);
      }
  };

_Atomic_word A::counter = 0;

using std::tr1::_S_mutex;

typedef std::tr1::__shared_ptr<A, _S_mutex> sp_A_t;
typedef std::tr1::__weak_ptr<A, _S_mutex> wp_A_t;

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

void* thread_hammer_and_kill(void* opaque_pools)
{
  shared_and_weak_pools& pools = *reinterpret_cast<shared_and_weak_pools*>(opaque_pools);
  // Using the same parameters as in the RNG test cases.
  std::tr1::mersenne_twister<
    unsigned long, 32, 624, 397, 31,
    0x9908b0dful, 11, 7,
    0x9d2c5680ul, 15,
    0xefc60000ul, 18> rng;
  
  sp_vector_t::iterator cur_shared = pools.shared_pool.begin();
  wp_vector_t::iterator cur_weak = pools.weak_pool.begin();
  
  for (unsigned int i = 0; i < HAMMER_REPEAT; ++i)
    {
      try
      {
        sp_A_t strong(*cur_weak);
      }
      catch (std::tr1::bad_weak_ptr& exception)
      {
        ++cur_weak;
        if (cur_weak == pools.weak_pool.end())
          break;
      }
      
      if (rng() % KILL_ONE_IN == 0)
      {
        cur_shared->reset();
        ++cur_shared;
      }
    }
  return 0;
}

void* thread_hammer(void* opaque_weak)
{
  wp_vector_t& weak_pool = *reinterpret_cast<wp_vector_t*>(opaque_weak);
  // Using the same parameters as in the RNG test cases.
  std::tr1::mersenne_twister<
    unsigned long, 32, 624, 397, 31,
    0x9908b0dful, 11, 7,
    0x9d2c5680ul, 15,
    0xefc60000ul, 18> rng;
  wp_vector_t::iterator cur_weak = weak_pool.begin();

  for (unsigned int i = 0; i < HAMMER_REPEAT; ++i)
    {
      try
      {
        sp_A_t strong(*cur_weak);
      }
      catch (std::tr1::bad_weak_ptr& exception)
      {
        ++cur_weak;
        if (cur_weak == weak_pool.end())
          break;
      }
    }
  return 0;
}

int
test01()
{
  bool test __attribute__((unused)) = true;
  sp_vector_t obj_pool(POOL_SIZE);
  
  for(sp_vector_t::iterator cur = obj_pool.begin(); cur != obj_pool.end(); ++cur)
  {
    cur->reset(new A);
  }
  // Obtain weak references.
  std::vector<wp_vector_t> weak_pool(HAMMER_MAX_THREADS, wp_vector_t(obj_pool.begin(), obj_pool.end()));
  
  // Launch threads with pointer to weak reference.
  pthread_t threads[HAMMER_MAX_THREADS];
#if defined(__sun) && defined(__svr4__) && _XOPEN_VERSION >= 500
  pthread_setconcurrency (HAMMER_MAX_THREADS);
#endif
  
  pthread_attr_t tattr;
  int ret = pthread_attr_init(&tattr);

  shared_and_weak_pools pools(obj_pool, weak_pool[0]);
  pthread_create(threads, &tattr, thread_hammer_and_kill, reinterpret_cast<void*>(&pools));
  for (unsigned int worker = 1; worker < HAMMER_MAX_THREADS; worker++)
    {
      if (pthread_create(&threads[worker], &tattr,
			 thread_hammer, reinterpret_cast<void*>(&weak_pool[worker])))
	abort();
    }
  // Wait for threads to complete, then check integrity of reference.
  void* status;
  for (unsigned int worker = 0; worker < HAMMER_MAX_THREADS; worker++)
    {
      if (pthread_join(threads[worker], &status))
	abort();
    }
  obj_pool.clear();
  
  VERIFY( A::counter == 0 );
  
  return 0;
}

int 
main()
{
  test01();
  return 0;
}
