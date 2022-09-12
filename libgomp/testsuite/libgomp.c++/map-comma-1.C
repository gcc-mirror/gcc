/* { dg-do run } */

#include <cassert>

int main (int argc, char *argv[])
{
  int a = 5, b = 7;
#pragma omp target map((a, b))
  {
    a++;
    b++;
  }
  assert (a == 5 && b == 8);
  return 0;
}
