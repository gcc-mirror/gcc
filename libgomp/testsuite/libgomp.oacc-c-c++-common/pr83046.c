/* { dg-do link } */

#define N 100

int
main ()
{
  int a[N];
  int i, x;
  int c;

  c = 1;
#pragma acc parallel loop
  for (i = 0; i < 100; i++)
    a[i] = 0;

  if (c)
    __builtin_unreachable ();

#pragma acc parallel loop
  for (i = 0; i < 100; i++)
    a[i] = 1;

  return 0;
}
