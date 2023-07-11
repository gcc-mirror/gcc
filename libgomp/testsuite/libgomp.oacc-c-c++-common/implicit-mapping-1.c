/* { dg-do run } */

#include <string.h>
#include <assert.h>

int main(void)
{
  int arr[100];

  memset (arr, 0, sizeof (int) * 100);

#pragma acc enter data copyin(arr[30:10])

#pragma acc serial
/* { dg-warning {using .vector_length \(32\)., ignoring 1} "" { target openacc_nvidia_accel_selected } .-1 } */
  {
    arr[33] = 66;
  }

#pragma acc exit data copyout(arr[30:10])

  assert (arr[33] == 66);

  return 0;
}
