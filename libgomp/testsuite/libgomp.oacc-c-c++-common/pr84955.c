/* { dg-do compile }  */

int
main (void)
{
  int i, j;

#pragma acc parallel loop tile(2,3)
  for (i = 1; i < 10; i++)
    for (j = 1; j < 10; j++)
      for (;;)
	;

  return i + j;
}
