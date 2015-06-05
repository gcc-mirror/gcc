/* { dg-do run } */
/* { dg-options "-O2 -ftree-parallelize-loops=2" } */

unsigned int *a;

unsigned int __attribute__((noclone,noinline))
f (unsigned int n)
{
  int i;
  unsigned int sum = 1;

  for (i = 0; i < n; ++i)
    sum += a[i];

  return sum;
}

int
main (void)
{
  unsigned int res;
  unsigned int array[4000];
  int i;
  for (i = 0; i < 4000; ++i)
    array[i] = i % 7;
  a = &array[0];
  res = f (4000);
  return !(res == 11995);
}
