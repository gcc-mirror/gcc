#include <complex.h>
#include <stdio.h>
#include <stdlib.h>

typedef float _Complex Type;

#define N 32

int
main (void)
{
  Type ary[N];

  for (int ix = 0; ix < N;  ix++)
    ary[ix] = 1.0 + 1.0j;

  Type tprod = 1.0;

#pragma acc parallel vector_length(32)
  {
#pragma acc loop vector reduction (*:tprod)
    for (int ix = 0; ix < N; ix++)
      tprod *= ary[ix];
  }

  Type expected = 65536.0;

  if (tprod != expected)
    abort ();

  return 0;
}
