#pragma acc routine nohost
void
delay ()
{
  int i, sum;
  const int N = 500000;

  for (i = 0; i < N; i++)
    sum = sum + 1;
}

#pragma acc routine nohost
void
delay2 (unsigned long *d_o, unsigned long tid)
{
  int i, sum;
  const int N = 500000;

  for (i = 0; i < N; i++)
    sum = sum + 1;

  d_o[0] = tid;
}
