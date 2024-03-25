// { dg-do compile { target c++11 } }

#include <mutex>
#include <chrono>

using namespace std;

struct Mutex
{
  void lock();
  void unlock();
  bool try_lock();

  bool try_lock_for(chrono::seconds);
  bool try_lock_until(chrono::system_clock::time_point);
};


void
test_nodiscard(Mutex& m)
{
  unique_lock<Mutex>(); // no warning
  unique_lock<Mutex>{}; // no warning
  unique_lock<Mutex>(m, defer_lock); // no warning
  unique_lock<Mutex>{m, defer_lock}; // no warning

  unique_lock<Mutex>{m}; // { dg-warning "ignoring return value" }
  unique_lock<Mutex>{m, try_to_lock}; // { dg-warning "ignoring return value" }
  unique_lock<Mutex>(m, try_to_lock); // { dg-warning "ignoring return value" }
  unique_lock<Mutex>{m, adopt_lock}; // { dg-warning "ignoring return value" }
  unique_lock<Mutex>(m, adopt_lock); // { dg-warning "ignoring return value" }

  chrono::seconds reltime(1);
  unique_lock<Mutex>{m, reltime}; // { dg-warning "ignoring return value" }
  unique_lock<Mutex>(m, reltime); // { dg-warning "ignoring return value" }
  chrono::system_clock::time_point abstime(reltime);
  unique_lock<Mutex>{m, abstime}; // { dg-warning "ignoring return value" }
  unique_lock<Mutex>(m, abstime); // { dg-warning "ignoring return value" }
}

