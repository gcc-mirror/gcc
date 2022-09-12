/* { dg-do run } */

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define N 16

struct Z {
  int *ptr;
  int arr[N];
  int c;
};

int main (int argc, char *argv[])
{
  struct Z *myz;
  myz = (struct Z *) calloc (1, sizeof *myz);

#pragma omp target map(tofrom:myz->arr[0:N], myz->c)
  {
    for (int i = 0; i < N; i++)
      myz->arr[i]++;
    myz->c++;
  }

  for (int i = 0; i < N; i++)
    assert (myz->arr[i] == 1);
  assert (myz->c == 1);

  free (myz);

  return 0;
}

