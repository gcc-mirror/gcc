/* { dg-xfail-run-if "unhandled case" { *-*-* } } */

/* Test reuse of loop index variables in kernels region.  */

#include <assert.h>

#define SIZE 16384

int
main (int argc, char* argv[])
{
  float arr[SIZE], arr_o[SIZE];
  int i, o;

  for (i = 0; i < SIZE; i++)
    arr[i] = i;

  i = 15;
  #pragma acc kernels
  {
    i *= 30;
    o = i;

    #pragma acc loop independent
    for (i = 0; i < SIZE; i++)
      arr_o[i] = arr[i] * 2;
  }

  assert (i == SIZE);
  assert (o == 450);

  for (i = 0; i < SIZE; i++)
    assert (arr_o[i] == arr[i] * 2);

  return 0;
}
