// { dg-do run { target c++11 } }

#include <random>
#include <testsuite_hooks.h>
#include <testsuite_random.h>

void
test01()
{
  for (auto token : { "mt19937", "prng", "rand_s" })
    if (__gnu_test::random_device_available(token))
      VERIFY( std::random_device(token).entropy() == 0.0 );

  using result_type = std::random_device::result_type;
  const double max = std::log2(std::numeric_limits<result_type>::max() + 1.0);

  for (auto token : { "/dev/random", "/dev/urandom" })
    if (__gnu_test::random_device_available(token))
    {
      const double entropy = std::random_device(token).entropy();
      VERIFY( entropy >= 0.0 );
      VERIFY( entropy <= max );
    }

  for (auto token : { "rdrand", "rdseed" })
    if (__gnu_test::random_device_available(token))
    {
      const double entropy = std::random_device(token).entropy();
      VERIFY( entropy == max );
    }
}

int
main()
{
  test01();
}
