extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

#define N 10

int
main (void)
{
  int a[N];

  for (short i = 0; i < N; ++i)
    a[i] = -1;

#pragma acc parallel loop tile (2)
  for (short i = 0; i < N; ++i)
    a[i] = i;

  for (short i = 0; i < N; ++i)
    if (a[i] != i)
      abort ();

  return 0;
}
