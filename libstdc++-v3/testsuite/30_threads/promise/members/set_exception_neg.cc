// { dg-options "-D_GLIBCXX_ASSERTIONS" }
// { dg-do run { xfail *-*-* } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// LWG 2276. Missing requirement on std::promise::set_exception

#include <future>

int main()
{
  std::promise<void> prom;
  auto f = prom.get_future();
  std::exception_ptr p;
  prom.set_exception(p); // Preconditions: p is not null
  f.get();
}
