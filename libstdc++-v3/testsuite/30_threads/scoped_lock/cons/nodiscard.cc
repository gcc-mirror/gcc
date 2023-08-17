// { dg-do compile { target c++17 } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }

#include <mutex>

struct Mutex
{
  void lock();
  void unlock();
  bool try_lock();
};

using namespace std;

void
test_nodiscard(Mutex& m, mutex& m2)
{
  scoped_lock<>{}; // no warning
  scoped_lock<>(); // no warning

  scoped_lock{m}; // { dg-warning "ignoring return value" }
  scoped_lock{adopt_lock, m}; // { dg-warning "ignoring return value" }
  scoped_lock(adopt_lock, m); // { dg-warning "ignoring return value" }
  scoped_lock(m, m2); // { dg-warning "ignoring return value" }
  scoped_lock{m, m2}; // { dg-warning "ignoring return value" }
  scoped_lock{adopt_lock, m, m2}; // { dg-warning "ignoring return value" }
  scoped_lock(adopt_lock, m, m2); // { dg-warning "ignoring return value" }
}
