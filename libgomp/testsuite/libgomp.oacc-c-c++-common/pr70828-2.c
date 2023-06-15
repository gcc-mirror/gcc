/* Subarray declared on data construct, accessed through pointer.  */

#include <assert.h>

void
s1 (int *arr, int c)
{
#pragma acc data copy(arr[5:c-10])
  {
#pragma acc parallel loop
    for (int i = 5; i < c - 5; i++)
      arr[i] = i;
  }
}

int
main (int argc, char* argv[])
{
  const int c = 100;
  int arr[c];

  for (int i = 0; i < c; i++)
    arr[i] = 0;

  s1 (arr, c);

  for (int i = 0; i < c; i++)
    if (i >= 5 && i < c - 5)
      assert (arr[i] == i);
    else
      assert (arr[i] == 0);

  return 0;
}
