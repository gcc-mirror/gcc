// Copyright (C) 2004 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/*
 * The goal with this application is to compare the performance of
 * different allocators in a simple producer-consumer scenario.
 */

// 2004-02-04 Felix Yen <fwy@alumni.brown.edu>

#include <vector>
#include <list>
#include <map>
#include <typeinfo>
#include <sstream>
#include <pthread.h>
#include <ext/mt_allocator.h>
#include <ext/new_allocator.h>
#include <ext/malloc_allocator.h>
#include <ext/bitmap_allocator.h>
#include <ext/pool_allocator.h>
#include <cxxabi.h>
#include <testsuite_performance.h>


using namespace std;
using __gnu_cxx::__mt_alloc;
using __gnu_cxx::new_allocator;
using __gnu_cxx::malloc_allocator;
using __gnu_cxx::bitmap_allocator;
using __gnu_cxx::__pool_alloc;
using abi::__cxa_demangle;

typedef int test_type;
typedef less<test_type> compare_type;
typedef malloc_allocator<test_type> malloc_alloc_type;
typedef new_allocator<test_type> new_alloc_type;
typedef __mt_alloc<test_type> so_alloc_type;
typedef bitmap_allocator<test_type> bit_alloc_type;
typedef __pool_alloc<test_type> po_alloc_type;

typedef pair<const test_type, test_type> pair_type;
typedef malloc_allocator<pair_type> malloc_pair_alloc_type;
typedef new_allocator<pair_type> new_pair_alloc_type;
typedef __mt_alloc<pair_type> so_pair_alloc_type;
typedef bitmap_allocator<pair_type> bit_pair_alloc_type;
typedef __pool_alloc<pair_type> po_pair_alloc_type;

// The number of iterations to be performed.
int iterations = 10000;

// TODO - restore Stefan's comment?  i don't understand it.  -- fwy
int insert_values = 128;

class Lock
{
public:
  Lock() {pthread_mutex_init(&mutex, 0);}
  ~Lock() {pthread_mutex_destroy(&mutex);}

public:
  inline pthread_mutex_t* operator&() {return &mutex;}

public:
  inline void lock() {pthread_mutex_lock(&mutex);}
  inline void unlock() {pthread_mutex_unlock(&mutex);}

private:
  Lock(const Lock&);
  Lock& operator=(Lock&);

private:
  pthread_mutex_t mutex;
};

class AutoLock
{
public:
  AutoLock(Lock& _lock)
  : lock(_lock)
  {lock.lock();}

  ~AutoLock() {lock.unlock();}

private:
  AutoLock(AutoLock&);
  AutoLock& operator=(AutoLock&);

private:
  Lock& lock;
};

template<typename Container>
  class Queue
  {
  public:
    Queue() {pthread_cond_init(&condition, 0);}
    ~Queue() {pthread_cond_destroy(&condition);}

  public:
    void push_back(const typename Container::value_type& x);
    void swap(Container& container);

  private:
    pthread_cond_t condition;
    Lock lock;
    Container queue;
  };

template<typename Container>
  void
  Queue<Container>::push_back(const typename Container::value_type& value)
  {
    AutoLock auto_lock(lock);
    const bool signal = queue.empty();
    queue.insert(queue.end(), value);
    if (signal) pthread_cond_signal(&condition);
  }

template<typename Container>
  void
  Queue<Container>::swap(Container& container)
  {
    AutoLock auto_lock(lock);
    while (queue.empty()) pthread_cond_wait(&condition, &lock);
    queue.swap(container);
  }

class Thread
{ 
  // NB: Make this the last data member of an object defining operator()().
public:
  class Attributes
  {
  public:
    Attributes(int state = PTHREAD_CREATE_JOINABLE);
    ~Attributes() {pthread_attr_destroy(&attributes);}

  public:
    inline pthread_attr_t* operator&() {return &attributes;}

  private:
    pthread_attr_t attributes;
  };

public:
  Thread() {thread = pthread_self();}
  ~Thread();

public:
  template <typename ThreadOwner>
    void create(ThreadOwner* owner);

private:
  pthread_t thread;
};

Thread::Attributes::Attributes(int state)
{
  pthread_attr_init(&attributes);
  pthread_attr_setdetachstate(&attributes, state);
}

Thread::~Thread()
{
  if (!pthread_equal(thread, pthread_self()))
    pthread_join(thread, 0);
}

template<typename ThreadOwner>
  void*
  create_thread(void* _this)
  {
    ThreadOwner* owner = static_cast<ThreadOwner*>(_this);
    (*owner)();
    return 0;
  }

template<typename ThreadOwner>
  void
  Thread::create(ThreadOwner* owner)
  {
    Thread::Attributes attributes;
    pthread_create(&thread, &attributes, create_thread<ThreadOwner>, owner);
  }

template<typename Container>
  class Consumer
  {
  public:
    Consumer(Queue<Container>& _queue)
    : queue(_queue)
    {thread.create(this);}

  public:
    void operator()();

  private:
    Queue<Container>& queue;
    Thread thread;
  };

template<typename Container>
  void
  Consumer<Container>::operator()()
  {
    for (int j = insert_values * iterations; j > 0;)
    {
      Container container;
      queue.swap(container);
      j -= container.size();
    }
  }

template<typename TestType>
  struct Value : public pair<TestType, TestType>
  {
    Value()
    : pair<TestType, TestType>(0, 0)
    { }

    inline Value operator++() {return ++this->first, *this;}
    inline operator TestType() const {return this->first;}
  };

template<typename Container>
  class ProducerConsumer : private Queue<Container>
  {
  public:
    ProducerConsumer() {thread.create(this);}
 
  public:
    void operator()();

  private:
    Thread thread;
  };

template<typename Container>
  void
  ProducerConsumer<Container>::operator()()
  {
    Consumer<Container> consumer(*this);
    Value<test_type> test_value;
    for (int j = insert_values * iterations; j-- > 0;)
      this->push_back(++test_value);
  }

template<typename Container>
  void
  test_container(Container obj)
  {
    using namespace __gnu_test;
    int status;

    time_counter time;
    resource_counter resource;

    clear_counters(time, resource);
    start_counters(time, resource);
    {
      ProducerConsumer<Container> pc1;
      ProducerConsumer<Container> pc2;
    }
    stop_counters(time, resource);

    std::ostringstream comment;
    comment << "iterations: " << iterations << '\t';
    comment << "type: " << __cxa_demangle(typeid(obj).name(), 0, 0, &status);
    report_header(__FILE__, comment.str());
    report_performance(__FILE__, string(), time, resource);
  }

int main(void)
{
#ifdef TEST_T1
  test_container(vector<test_type, malloc_alloc_type>());
#endif
#ifdef TEST_T2
  test_container(vector<test_type, new_alloc_type>());
#endif
#ifdef TEST_T3
  test_container(vector<test_type, so_alloc_type>());
#endif
#ifdef TEST_T4
  test_container(vector<test_type, bit_alloc_type>());
#endif
#ifdef TEST_T5
  test_container(vector<test_type, po_alloc_type>());
#endif

#ifdef TEST_T6
  test_container(list<test_type, malloc_alloc_type>());
#endif
#ifdef TEST_T7
  test_container(list<test_type, new_alloc_type>());
#endif
#ifdef TEST_T8
  test_container(list<test_type, so_alloc_type>());
#endif
#ifdef TEST_T9
  test_container(list<test_type, bit_alloc_type>());
#endif
#ifdef TEST_T10
  test_container(list<test_type, po_alloc_type>());
#endif

#ifdef TEST_T11
  test_container(map<test_type, test_type, compare_type,
		 malloc_pair_alloc_type>());
#endif
#ifdef TEST_T12
  test_container(map<test_type, test_type, compare_type,
		 new_pair_alloc_type>());
#endif
#ifdef TEST_T13
  test_container(map<test_type, test_type, compare_type,
		 so_pair_alloc_type>());
#endif
#ifdef TEST_T14
  test_container(map<test_type, test_type, compare_type,
		 bit_pair_alloc_type>());
#endif
#ifdef TEST_T15
  test_container(map<test_type, test_type, compare_type,
		 po_pair_alloc_type>());
#endif

  return 0;
}

