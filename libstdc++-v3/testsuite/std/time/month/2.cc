// { dg-do run { target c++20 } }

// Class month [time.cal.month]

#include <chrono>
#include <limits>
#include <testsuite_hooks.h>

using namespace std::chrono;

void test_extreme_values(months extreme)
{
  auto const count = extreme.count();
  auto const safe  = count < 0 ? count + 12 : count;
  auto const mod   = safe - 12 * ((safe < 0 ? safe - 11 : safe) / 12);

  for (unsigned m = 0; m < 256; ++m)
  {
    auto const month_plus_extreme = month{m} + extreme;
    VERIFY(unsigned{month_plus_extreme } == (m + 11 + mod) % 12 + 1);

    auto const month_minus_extreme = month{m} - extreme;
    VERIFY(unsigned{month_minus_extreme} == (m + 11 - mod) % 12 + 1);
  }
}

int main()
{
  test_extreme_values(months{std::numeric_limits<months::rep>::max()});
  test_extreme_values(months{std::numeric_limits<months::rep>::min()});
  return 0;
}
