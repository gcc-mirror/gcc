// { dg-do run { target c++11 } }

// Bug 114359 - std::binomial_distribution hangs in infinite loop

// { dg-require-cmath "" }

// The requirement above is not strictly true.  The test should work
// without cmath, and it probably does, but without cmath,
// binomial_distribution::operator() skips the optimized algorithm and
// calls _M_waiting to loop a gazillion times.  On aarch64-rtems6
// qemu, that loop takes over 5 minutes to go through a small fraction
// of the iteration space (__x at 22k, limited at 1G; __sum at 2e-5,
// limited at 0.69).  The bug we're regression-testing here was in the
// cmath-requiring bit, so even if this could conceivably not time out
// on a really fast machine, there's hardly any reason to exercise
// this extreme case.

#include <random>

int main()
{
  std::default_random_engine g{};
  std::binomial_distribution<std::uint32_t> b(1U << 30);
  b(g);  // hangs forever
}
