// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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


#include <testsuite_performance.h>

typedef int test_type;

// The number of iterations to be performed.
int iterations = 1000;

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
  struct Value : public std::pair<TestType, TestType>
  {
    Value()
    : std::pair<TestType, TestType>(0, 0)
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

template<typename Container, int Iter>
  void
  do_loop()
  {
    ProducerConsumer<Container> pc1;
    ProducerConsumer<Container> pc2;
  } 

int
main()
{ 
#ifdef TEST_T1
#define thread_type true
#endif    

  typedef __gnu_test::maps<test_type, thread_type>::type map_typelist;
  typedef __gnu_test::sets<test_type, thread_type>::type set_typelist;
  typedef __gnu_cxx::typelist::append<map_typelist, set_typelist>::type container_types;

  typedef test_sequence<thread_type> test_type;
  test_type test("producer_consumer_associative");
  __gnu_cxx::typelist::apply(test, container_types());

  return 0;
}

