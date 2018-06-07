/* { dg-do link } */

int
main (void)
{
  #pragma acc parallel
  #pragma acc loop
  for (int i = 1; i < 10; i++)
    for (;;)
      ;

  return 0;
}
