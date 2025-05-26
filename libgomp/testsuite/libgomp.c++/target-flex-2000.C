/* Tiny tuple test.  */

#include <tuple>

#include "target-flex-common.h"

bool test(int arg)
{
  bool ok;
  int out;
  std::tuple tup = {'a', arg, 3.14f};
  #pragma omp target map(from: ok, out) map(to: tup)
    {
      bool inner_ok = true;
      {
	VERIFY (std::get<0>(tup) == 'a');
	out = std::get<1>(tup);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (out == arg);
  return true;
}

int main()
{
  volatile int arg = 42u;
  return test(arg) ? 0 : 1;
}
