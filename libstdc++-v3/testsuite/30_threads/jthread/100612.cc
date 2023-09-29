// { dg-do compile { target c++20 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

#include <thread>

void
test_pfm()
{
  // PR libstdc++/100612
  struct X
  {
    void run(std::stop_token) { }
    void run_arg(int) { }
    void run_args(std::stop_token, int, int) { }
  };

  X x;

  std::jthread{&X::run, &x};
  std::jthread{&X::run_arg, &x, 1};
  std::jthread{&X::run_args, &x, 1, 1};
}
