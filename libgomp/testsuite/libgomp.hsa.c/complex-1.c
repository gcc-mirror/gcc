#include <assert.h>
#include <complex.h>
#include <math.h>

#define uchar unsigned char
#define C 123

#define TEST(type)                                                             \
  type foo_##type (void)                                                       \
  {                                                                            \
    _Complex type a = C + 45I;                                                 \
    return __real__ a;                                                         \
  }

#pragma omp declare target
TEST (char)
TEST (uchar)
TEST (short)
TEST (int)

float
bar (float a, float b)
{
  _Complex float c = a + b * I;

  c += 11.f + 12.f * I;

  _Complex float d = 2.f + 4.44f * I;

  return __real__(crealf (c + d) + cimag (d) * I);
}

#pragma omp end declare target

int
main (void)
{
  int v = 0;
  float v2 = 0.0f;

#pragma omp target map(to: v)
  v = foo_char ();

  assert (v == C);

#pragma omp target map(to: v)
  v = foo_uchar ();

  assert (v == C);

#pragma omp target map(to: v)
  v = foo_short ();

  assert (v == C);

#pragma omp target map(to: v)
  v = foo_int ();

  assert (v == C);

#pragma omp target map(to: v2)
  v2 = bar (1.12f, 4.44f);

  assert (fabs (v2 - 14.12) < 0.0001f);
}
