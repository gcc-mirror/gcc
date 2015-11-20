/* { dg-do run } */

int
main(void)
{
#define I 5
#define N 32
#define A 8

  int a = A;
  int s = I;

#pragma acc parallel vector_length(N) copy(s)
  {
    int i;
#pragma acc loop reduction(+:s)
    for (i = 0; i < N; ++i)
      s += a;
  }

  if (s != I + N * A)
    __builtin_abort();

  return 0;
}
