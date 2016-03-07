#include <assert.h>
#include <limits.h>

#define T unsigned int
#define BITSIZE CHAR_BIT * sizeof (T)

#define C1 123u

#pragma omp declare target
T
rotate (T value, T shift)
{
  T r = (value << shift) | (value >> (BITSIZE - shift));
  return (r >> shift) | (r << (BITSIZE - shift));
}
#pragma omp end declare target

int
main (int argc)
{
  T v1, v2, v3, v4, v5;

#pragma omp target map(to: v1, v2, v3, v4, v5)
  {
    v1 = rotate (C1, 10);
    v2 = rotate (C1, 2);
    v3 = rotate (C1, 5);
    v4 = rotate (C1, 16);
    v5 = rotate (C1, 32);
  }

  assert (v1 == C1);
  assert (v2 == C1);
  assert (v3 == C1);
  assert (v4 == C1);
  assert (v5 == C1);

  return 0;
}
