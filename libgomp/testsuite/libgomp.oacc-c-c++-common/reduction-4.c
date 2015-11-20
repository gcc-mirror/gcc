/* { dg-do run { target { ! { hppa*-*-hpux* } } } } */

/* complex reductions.  */

#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <complex.h>

#define vl 32

int
main(void)
{
  const int n = 1000;
  int i;
  double _Complex vresult, result, array[n];
  bool lvresult, lresult;

  for (i = 0; i < n; i++)
    array[i] = i;

  result = 0;
  vresult = 0;

  /* '&&' reductions.  */
#pragma acc parallel vector_length (vl) copy(lresult)
#pragma acc loop reduction (&&:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult && (creal(result) > creal(array[i]));

  /* Verify the reduction.  */
  for (i = 0; i < n; i++)
    lvresult = lresult && (creal(result) > creal(array[i]));

  if (lresult != lvresult)
    abort ();

  result = 5;
  vresult = 5;

  lresult = false;
  lvresult = false;

  /* '||' reductions.  */
#pragma acc parallel vector_length (vl) copy(lresult)
#pragma acc loop reduction (||:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult || (creal(result) > creal(array[i]));

  /* Verify the reduction.  */
  for (i = 0; i < n; i++)
    lvresult = lresult || (creal(result) > creal(array[i]));

  if (lresult != lvresult)
    abort ();

  return 0;
}
