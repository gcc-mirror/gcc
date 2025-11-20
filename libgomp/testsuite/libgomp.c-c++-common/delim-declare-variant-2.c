/* Check that the correct function is used;
   assumes that vendor(gnu) is always true.  */

int inner() { return 1; }

int outer(int is_novar) {
  int k;
  if (!is_novar) __builtin_abort();

  k = inner();
  if (k != 22) __builtin_abort();

  #pragma omp dispatch novariants(1)
    k = inner();
  if (k != 1) __builtin_abort();
  return 3;
}

#pragma omp begin declare variant match(implementation={vendor(gnu)})
int outer(int is_novar) {
  int k;
  if (is_novar) __builtin_abort();

  k = inner();
  if (k != 22) __builtin_abort();

  #pragma omp dispatch novariants(1)
    k = inner();
  if (k != 1) __builtin_abort();
  return 44;
}

int inner() { return 22; }
#pragma omp end declare variant

int
main()
{
  int j;
  j = outer(0);
  if (j != 44) __builtin_abort();

  #pragma omp dispatch novariants(1)
    j = outer(1);
  if (j != 3) __builtin_abort();
  return 0;
}
