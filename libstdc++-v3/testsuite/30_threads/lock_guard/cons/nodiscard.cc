// { dg-do compile { target c++11 } }

#include <mutex>

struct Mutex
{
  void lock();
  void unlock();
  bool try_lock();
};

using namespace std;

void
test_nodiscard(Mutex& m)
{
  lock_guard<Mutex>{m}; // { dg-warning "ignoring return value" }
  lock_guard<Mutex>{m, adopt_lock}; // { dg-warning "ignoring return value" }
  lock_guard<Mutex>(m, adopt_lock); // { dg-warning "ignoring return value" }
}
