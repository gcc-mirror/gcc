#include <assert.h>

int main (int argc, char *argv[])
{
  int y = 0;
  int *x = &y;

#pragma omp target map(*x)
  {
    (*x)++;
  }

  assert (y == 1);

  return 0;
}
