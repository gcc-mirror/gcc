#ifdef __cplusplus
extern "C"
#else
extern
#endif
void abort (void);

int
main (void)
{
  #define N 5
  int array[N][N];

  for (int i = 0; i < N; i++)
    {
      #pragma omp target enter data map(alloc: array[i:1][0:N])

      #pragma omp target
      for (int j = 0; j < N; j++)
	array[i][j] = i + j;

      #pragma omp target exit data map(from: array[i:1][0:N])
    }

  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      if (array[i][j] != i + j)
	abort ();

  return 0;
}
