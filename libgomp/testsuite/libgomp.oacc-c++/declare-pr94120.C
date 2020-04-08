#include <openacc.h>
#include <stdlib.h>

#define N 8

namespace one {
  int A[N] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  #pragma acc declare copyin (A)
};

namespace outer {
  namespace inner {
    int B[N];
    #pragma acc declare create (B)
  };
};

static void
f (void)
{
  int i;
  int C[N];
  #pragma acc declare copyout (C)

  if (!acc_is_present (&one::A, sizeof (one::A)))
    abort ();

  if (!acc_is_present (&outer::inner::B, sizeof (outer::inner::B)))
    abort ();

#pragma acc parallel
  for (i = 0; i < N; i++)
    {
      outer::inner::B[i] = one::A[i];
      C[i] = outer::inner::B[i];
    }

  for (i = 0; i < N; i++)
    {
      if (C[i] != i + 1)
	abort ();
    }

#pragma acc parallel
  for (i = 0; i < N; i++)
    if (outer::inner::B[i] != i + 1)
      abort ();
}


int
main (int argc, char **argv)
{
  f ();

  return 0;
}
