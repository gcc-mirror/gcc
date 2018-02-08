
#include <tr1/cmath>
#include <testsuite_hooks.h>

void
test01()
{
  double Pi1 = std::tr1::comp_ellint_3(0.75, 0.0);
  VERIFY(std::abs(Pi1 - 1.91099) < 0.00001);

  double Pi2 = std::tr1::comp_ellint_3(0.75, 0.5);
  VERIFY(std::abs(Pi2 - 2.80011) < 0.00001);
}

int
main()
{
  test01();
  return 0;
}
