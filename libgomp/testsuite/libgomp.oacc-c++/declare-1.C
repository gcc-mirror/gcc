#include <stdlib.h>

template<class T>
T foo()
{
  T a, b;
  #pragma acc declare create (a)

  #pragma acc parallel copyout (b)
  {
    a = 5;
    b = a;
  }

  return b;
}

int
main (void)
{
  int rc;

  rc = foo<int>();

  if (rc != 5)
    abort ();

  return 0;
}
