// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-gthreads "" }

#include <atomic>

void
test1(const std::atomic<char*>& a, char* p)
{
  a.wait(p);
}

void
test2(const std::atomic<int>* a, int v)
{
  std::atomic_wait(a, v);
  std::atomic_notify_one(a);
  std::atomic_notify_all(a);
}
