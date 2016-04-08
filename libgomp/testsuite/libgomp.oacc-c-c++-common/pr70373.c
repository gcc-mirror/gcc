#define N 32

int
foo (unsigned int sum)
{
#pragma acc parallel reduction (+:sum)
  {
    sum;
  }

  return sum;
}

int
main (void)
{
  unsigned int sum = 0;
  foo (sum);
  return 0;
}
