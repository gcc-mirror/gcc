/* { dg-do run } */

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

#define N 32ULL
int a[N];

const unsigned long long c = 0x7fffffffffffffffULL;

void
f2_tpf_static32 (void)
{
  unsigned long long i;
  #pragma omp for
  for (i = c + N; i > c; i -= 1ULL)
    a[i - 1ULL - c] -= 4;
}

__attribute__((noinline, noclone)) int
test_tpf_static32 (void)
{
  int i, j, k;
  for (i = 0; i < N; i++)
    a[i] = i - 25;

  f2_tpf_static32 ();

  for (i = 0; i < N; i++)
    if (a[i] != i - 29)
      return 1;

  return 0;
}

int
main ()
{
  if (test_tpf_static32 ())
    abort ();

  return 0;
}
