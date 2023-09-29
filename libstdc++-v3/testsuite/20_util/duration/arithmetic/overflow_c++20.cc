// { dg-do run { target c++20 } }

#include <chrono>
#include <cstdint>
#include <testsuite_hooks.h>

void
test_overflow()
{
  using namespace std::chrono;

  using seconds32_t = duration<std::int_least32_t>;
  seconds32_t t = 14h + 25min + 55s;
  auto snow = sys_days(1854y/December/11);
  auto snow_t = snow + t;
  // Fails if days::rep is 32-bit:
  VERIFY( snow_t.time_since_epoch() < seconds::zero() );
  auto y = floor<years>(snow);
  auto y_t = y + t;
  // Fails if years::rep is 32-bit:
  VERIFY( y_t.time_since_epoch() < seconds::zero() );
  VERIFY( y_t < snow_t );
}

int main()
{
  test_overflow();
}
