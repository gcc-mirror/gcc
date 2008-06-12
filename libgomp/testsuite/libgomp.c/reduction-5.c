/* PR middle-end/36506 */

extern void abort (void);

int
main (void)
{
  int sum = 0, prod = 1;
#pragma omp parallel
  #pragma omp sections reduction (+:sum)
    {
    #pragma omp section
      sum += 2;
    #pragma omp section
      sum += 2;
    #pragma omp section
      sum += 2;
    }
  if (sum != 6)
    abort ();
  sum = 0;
#pragma omp parallel sections reduction (+:sum)
  {
  #pragma omp section
    sum += 2;
  #pragma omp section
    sum += 2;
  #pragma omp section
    sum += 2;
  }
  if (sum != 6)
    abort ();
  sum = 0;
#pragma omp parallel
  #pragma omp sections reduction (+:sum) reduction (*:prod)
    {
    #pragma omp section
      {
	sum += 2;
	prod *= 2;
      }
    #pragma omp section
      {
	sum += 2;
	prod *= 2;
      }
    #pragma omp section
      {
	sum += 2;
	prod *= 2;
      }
    }
  if (sum != 6 || prod != 8)
    abort ();
  sum = 0;
  prod = 1;
#pragma omp parallel sections reduction (+:sum) reduction (*:prod)
  {
  #pragma omp section
    {
      sum += 2;
      prod *= 2;
    }
  #pragma omp section
    {
      sum += 2;
      prod *= 2;
    }
  #pragma omp section
    {
      sum += 2;
      prod *= 2;
    }
  }
  if (sum != 6 || prod != 8)
    abort ();
  return 0;
}
