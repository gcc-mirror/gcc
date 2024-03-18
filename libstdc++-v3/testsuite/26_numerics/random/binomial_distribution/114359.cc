// { dg-do run { target c++11 } }

// Bug 114359 - std::binomial_distribution hangs in infinite loop

#include <random>

int main()
{
  std::default_random_engine g{};
  std::binomial_distribution<std::uint32_t> b(1U << 30);
  b(g);  // hangs forever
}
