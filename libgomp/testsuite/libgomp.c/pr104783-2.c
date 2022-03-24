#define N (32 * 32)

#define TYPE float
#define VAR v
#define INIT 0.0
#define UPDATE + 1.0
#define EXPECTED N

int
main (void)
{
  TYPE VAR = INIT;
  #pragma omp target map(tofrom: VAR)
  #pragma omp parallel for simd
  for (int i = 0 ; i < N; i++)
    {
      #pragma omp atomic update
      VAR = VAR UPDATE;
    }

  if (VAR != EXPECTED)
    __builtin_abort ();

  return 0;
}
