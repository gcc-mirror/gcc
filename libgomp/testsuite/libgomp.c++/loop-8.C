#include <omp.h>
#include <stdlib.h>
#include <string.h>

int
test1 ()
{
  short int buf[64], *p;
  int i;
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[10]; p < &buf[54]; p++)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[3]; p <= &buf[63]; p += 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[16]; p < &buf[51]; p = 4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[16]; p <= &buf[40]; p = p + 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[53]; p > &buf[9]; --p)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[63]; p >= &buf[3]; p -= 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[48]; p > &buf[15]; p = -4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for
  for (p = &buf[40]; p >= &buf[16]; p = p - 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  return 0;
}

int
test2 ()
{
  int buf[64], *p;
  int i;
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[10]; p < &buf[54]; p++)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[3]; p <= &buf[63]; p += 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[16]; p < &buf[51]; p = 4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[16]; p <= &buf[40]; p = p + 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[53]; p > &buf[9]; --p)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[63]; p >= &buf[3]; p -= 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[48]; p > &buf[15]; p = -4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (static, 3)
  for (p = &buf[40]; p >= &buf[16]; p = p - 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  return 0;
}

int
test3 ()
{
  int buf[64], *p;
  int i;
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[10]; p < &buf[54]; p++)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[3]; p <= &buf[63]; p += 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[16]; p < &buf[51]; p = 4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[16]; p <= &buf[40]; p = p + 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[53]; p > &buf[9]; --p)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[63]; p >= &buf[3]; p -= 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[48]; p > &buf[15]; p = -4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (dynamic, 3)
  for (p = &buf[40]; p >= &buf[16]; p = p - 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  return 0;
}

int
test4 ()
{
  int buf[64], *p;
  int i;
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[10]; p < &buf[54]; p++)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[3]; p <= &buf[63]; p += 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[16]; p < &buf[51]; p = 4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[16]; p <= &buf[40]; p = p + 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[53]; p > &buf[9]; --p)
    *p = 5;
  for (i = 0; i < 64; i++)
    if (buf[i] != 5 * (i >= 10 && i < 54))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[63]; p >= &buf[3]; p -= 2)
    p[-2] = 6;
  for (i = 0; i < 64; i++)
    if (buf[i] != 6 * ((i & 1) && i <= 61))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[48]; p > &buf[15]; p = -4 + p)
    p[2] = 7;
  for (i = 0; i < 64; i++)
    if (buf[i] != 7 * ((i & 3) == 2 && i >= 18 && i < 53))
      abort ();
  memset (buf, '\0', sizeof (buf));
#pragma omp parallel for schedule (runtime)
  for (p = &buf[40]; p >= &buf[16]; p = p - 4ULL)
    p[2] = -7;
  for (i = 0; i < 64; i++)
    if (buf[i] != -7 * ((i & 3) == 2 && i >= 18 && i <= 42))
      abort ();
  return 0;
}

int
main ()
{
  test1 ();
  test2 ();
  test3 ();
  omp_set_schedule (omp_sched_static, 0);
  test4 ();
  omp_set_schedule (omp_sched_static, 3);
  test4 ();
  omp_set_schedule (omp_sched_dynamic, 5);
  test4 ();
  omp_set_schedule (omp_sched_guided, 2);
  test4 ();
  return 0;
}
