// { dg-do run { target c++20 } }

// Class weekday [time.cal.wd]

#include <chrono>
#include <limits>
#include <testsuite_hooks.h>

using namespace std::chrono;

void test_extreme_values(days extreme)
{
  auto const count = extreme.count();
  auto const safe  = count < 0 ? count + 7 : count;
  auto const mod   = safe - 7 * ((safe < 0 ? safe - 6 : safe) / 7);

  for (unsigned d = 0; d < 254; ++d)
  {
    auto const weekday_plus_extreme = weekday{d} + extreme;
    VERIFY(weekday_plus_extreme.c_encoding()  == (d + mod) % 7);

    auto const weekday_minus_extreme = weekday{d} - extreme;
    VERIFY(weekday_minus_extreme.c_encoding() == (d + 7 - mod) % 7);
  }
}

int main()
{
  test_extreme_values(days{std::numeric_limits<days::rep>::max()});
  test_extreme_values(days{std::numeric_limits<days::rep>::min()});
  return 0;
}
