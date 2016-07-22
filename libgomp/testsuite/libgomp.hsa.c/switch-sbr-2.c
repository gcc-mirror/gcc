/* { dg-additional-options "-fno-tree-switch-conversion" } */

#pragma omp declare target
int
foo (unsigned a)
{
  switch (a)
    {
    case 1 ... 5:
      return 1;
    case 9 ... 11:
      return a + 3;
    case 12 ... 13:
      return a + 3;
    default:
      return 44;
    }
}
#pragma omp end declare target

#define s 100

void __attribute__((noinline, noclone))
verify(int *a)
{
  if (a[0] != 44)
    __builtin_abort ();
  
  for (int i = 1; i <= 5; i++)
    if (a[i] != 1)
      __builtin_abort ();

  for (int i = 6; i <= 8; i++)
    if (a[i] != 44)
      __builtin_abort ();

  for (int i = 9; i <= 13; i++)
    if (a[i] != i + 3)
      __builtin_abort ();

  for (int i = 14; i < s; i++)
    if (a[i] != 44)
      __builtin_abort ();
}

int main(int argc)
{
  int array[s];
#pragma omp target
  {
    for (int i = 0; i < s; i++)
      {
	int v = foo (i);
	array[i] = v;
      }
  }
  verify (array);
  return 0;
}
