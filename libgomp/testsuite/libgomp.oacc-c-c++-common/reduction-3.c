/* { dg-do run } */

/* double reductions.  */

#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

#define vl 32

int
main(void)
{
  const int n = 1000;
  int i;
  double vresult, result, array[n];
  bool lvresult, lresult;

  for (i = 0; i < n; i++)
    array[i] = i;

  result = 0;
  vresult = 0;

  /* 'max' reductions.  */
#pragma acc parallel vector_length (vl) copy(result)
#pragma acc loop reduction (max:result)
  for (i = 0; i < n; i++)
    result = result > array[i] ? result : array[i];

  /* Verify the reduction.  */
  for (i = 0; i < n; i++)
    vresult = vresult > array[i] ? vresult : array[i];

  if (result != vresult)
    abort ();

  result = 0;
  vresult = 0;

  /* 'min' reductions.  */
#pragma acc parallel vector_length (vl) copy(result)
#pragma acc loop reduction (min:result)
  for (i = 0; i < n; i++)
    result = result < array[i] ? result : array[i];

  /* Verify the reduction.  */
  for (i = 0; i < n; i++)
    vresult = vresult < array[i] ? vresult : array[i];

  if (result != vresult)
    abort ();

  result = 5;
  vresult = 5;

  lresult = false;
  lvresult = false;

  /* '&&' reductions.  */
#pragma acc parallel vector_length (vl) copy(lresult)
#pragma acc loop reduction (&&:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult && (result > array[i]);

  /* Verify the reduction.  */
  for (i = 0; i < n; i++)
    lvresult = lresult && (result > array[i]);

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
    lresult = lresult || (result > array[i]);

  /* Verify the reduction.  */
  for (i = 0; i < n; i++)
    lvresult = lresult || (result > array[i]);

  if (lresult != lvresult)
    abort ();

  return 0;
}
