// { dg-do compile { target c++11 } }
// { dg-require-gthreads "" }

// PR libstdc++/106695
// Explicit copy constructor does not work for a parameter passed via std::async

#include <thread>

struct A {
  A() = default;
  explicit A(const A&) = default;
};

void func(const A&) { }

void
test_thread()
{
  std::thread t(func, A{});
  t.join();
}
