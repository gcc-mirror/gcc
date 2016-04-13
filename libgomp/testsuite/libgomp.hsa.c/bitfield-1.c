#include <assert.h>

#define ASSIGN_SX(N)                                                           \
  s##N.a1 = 1;                                                                 \
  s##N.a2 = 2;                                                                 \
  s##N.a3 = 3;                                                                 \
  s##N.a4 = 4;                                                                 \
  s##N.a5 = 5;                                                                 \
  s##N.a6 = 6;                                                                 \
  s##N.a7 = 7;                                                                 \
  s##N.a8 = 8;                                                                 \
  s##N.a9 = 9;                                                                 \
  s##N.a10 = 10;

#define ASSERT_SX(N)                                                           \
  assert (s##N.a1 == 1); \
  assert (s##N.a2 == 2); \
  assert (s##N.a3 == 3); \
  assert (s##N.a4 == 4); \
  assert (s##N.a5 == 5); \
  assert (s##N.a6 == 6); \
  assert (s##N.a7 == 7); \
  assert (s##N.a8 == 8); \
  assert (s##N.a9 == 9); \
  assert (s##N.a10 == 10);

struct S1
{
  unsigned a : 10;
  unsigned b : 20;
};

struct S2
{
  unsigned a1 : 10;
  unsigned a2 : 10;
  unsigned a3 : 10;
  unsigned a4 : 10;
  unsigned a5 : 10;
  unsigned a6 : 10;
  unsigned a7 : 10;
  unsigned a8 : 10;
  unsigned a9 : 10;
  unsigned a10 : 10;
};

struct S3
{
  unsigned a1 : 10;
  unsigned a2 : 9;
  unsigned a3 : 8;
  unsigned a4 : 7;
  unsigned a5 : 6;
  unsigned a6 : 5;
  unsigned a7 : 6;
  unsigned a8 : 7;
  unsigned a9 : 8;
  unsigned a10 : 9;
};

struct S4
{
  unsigned a1 : 10;
  int a2 : 9;
  unsigned a3 : 8;
  int a4 : 7;
  unsigned a5 : 6;
  int a6 : 5;
  unsigned a7 : 6;
  int a8 : 7;
  unsigned a9 : 8;
  int a10 : 9;
};

struct S5
{
  unsigned a1 : 31;
  int a2 : 9;
  unsigned a3 : 17;
  int a4 : 7;
  unsigned a5 : 6;
  int a6 : 5;
  unsigned long a7 : 55;
  int a8 : 7;
  unsigned a9 : 8;
  int a10 : 9;
};

int
main ()
{
  struct S1 s1;

#pragma omp target map(to: s1)
  {
    s1.a = 2;
    s1.b = 3;
  }

  assert (s1.a == 2);
  assert (s1.b == 3);

  struct S2 s2;

#pragma omp target map(to: s2)
  {
    ASSIGN_SX (2)
  }

  ASSERT_SX (2)

  struct S3 s3;

#pragma omp target map(to: s3)
  {
    ASSIGN_SX (3)
  }

  ASSERT_SX (3)

  struct S4 s4;

#pragma omp target map(to: s4)
  {
    ASSIGN_SX (4)
  }

  ASSERT_SX (4)

  struct S4 s5;

  s5.a1 = 0;
  s5.a2 = 1;
  s5.a3 = 2;
  s5.a4 = 3;
  s5.a5 = 4;
  s5.a6 = 5;
  s5.a7 = 6;
  s5.a8 = 7;
  s5.a9 = 8;
  s5.a10 = 9;

#pragma omp target map(to: s5)
  {
    s5.a1++;
    s5.a2++;
    s5.a3++;
    s5.a4++;
    s5.a5++;
    s5.a6++;
    s5.a7++;
    s5.a8++;
    s5.a9++;
    s5.a10++;
  }

  ASSERT_SX (5)

  return 0;
}
