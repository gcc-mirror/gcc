/* { dg-do run } */

#include <cassert>

int foo (int &&x)
{
  int y;
#pragma omp target map(x, y)
  {
    x++;
    y = x;
  }
  return y;
}

int main (int argc, char *argv[])
{
  int y = 5;
  y = foo (y + 3);
  assert (y == 9);
  return 0;
}
