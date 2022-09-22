// { dg-do compile { target c++11 } }
// { dg-require-gthreads "" }

// PR libstdc++/106695
// Explicit copy constructor does not work for a parameter passed via std::async

#include <future>

struct A {
  A() = default;
  explicit A(const A&) = default;
};

void func(const A&) { }

void
test_async()
{
  (void) std::async(std::launch::async, func, A{});
  (void) std::async(std::launch::deferred, func, A{});
  (void) std::async(func, A{});
}

void
test_task()
{
  std::packaged_task<void(const A&)> task(func);
  task(A{});
}
