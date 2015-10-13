// { dg-do run }
// { dg-require-effective-target offload_device_nonshared_as }

#include <stdlib.h>

struct typeX
{
  int a;
};

class typeY
{
public:
  int foo () { return a^0x01; }
  int a;
};

#pragma omp declare target
struct typeX varX;
class typeY varY;
#pragma omp end declare target

int main ()
{
  varX.a = 0;
  varY.a = 0;

  #pragma omp target
    {
      varX.a = 100;
      varY.a = 100;
    }

  if (varX.a != 0 || varY.a != 0)
    abort ();

  #pragma omp target update from(varX, varY)

  if (varX.a != 100 || varY.a != 100)
    abort ();

  return 0;
}
