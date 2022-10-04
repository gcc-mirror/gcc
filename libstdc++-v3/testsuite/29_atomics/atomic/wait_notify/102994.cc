// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-gthreads "" }
// { dg-require-effective-target hosted }

#include <atomic>

void
test1(std::atomic<char*>& a, char* p)
{
  a.wait(p);
}

void
test2(std::atomic<int>* a, int v)
{
  std::atomic_wait(a, v);
  std::atomic_notify_one(a);
  std::atomic_notify_all(a);
}
