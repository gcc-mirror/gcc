/* { dg-do run } */

/* Same as reduction-arrays-2.c test, but with non-constant subarray
   base indexes.  */

#define ARRAY_BODY(ARRAY, MIN, LEN)		\
  for (int i = 0; i < 10; i++)			\
    for (int j = MIN; j < MIN + LEN; j++)	\
      ARRAY[j] += 1;

int zero = 0;
int one = 1;
int two = 2;
int three = 3;
int four = 4;

int main (void)
{
  int o[6] = { 5, 1, 1, 5, 9, 9 };
  int a[6];

  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    a[i] = o[i];

  #pragma acc parallel
  #pragma acc loop reduction(+:a[one:2])
  ARRAY_BODY (a, one, 2)
  ARRAY_BODY (o, one, 2)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel
  #pragma acc loop gang reduction(+:a[one:2])
  ARRAY_BODY (a, one, 2)
  ARRAY_BODY (o, one, 2)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a[three:2])
  #pragma acc loop reduction(+:a[three:2])
  ARRAY_BODY (a, three, 2)
  ARRAY_BODY (o, three, 2)
  for (int i = 0; i < 6; i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a[three:2])
  #pragma acc loop worker reduction(+:a[three:2])
  ARRAY_BODY (a, three, 2)
  ARRAY_BODY (o, three, 2)
  for (int i = 0; i < 6; i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a)
  #pragma acc loop reduction(+:a[zero:5])
  ARRAY_BODY (a, zero, 5)
  ARRAY_BODY (o, zero, 5)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a)
  #pragma acc loop vector reduction(+:a[zero:5])
  ARRAY_BODY (a, zero, 5)
  ARRAY_BODY (o, zero, 5)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel
  #pragma acc loop reduction(+:a)
  ARRAY_BODY (a, four, 1)
  ARRAY_BODY (o, four, 1)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a)
  #pragma acc loop reduction(+:a)
  ARRAY_BODY (a, three, 3)
  ARRAY_BODY (o, three, 3)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

#if !defined(ACC_DEVICE_TYPE_host)

  #pragma acc parallel loop reduction(+:a)
  ARRAY_BODY (a, one, 3)
  ARRAY_BODY (o, one, 3)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel loop reduction(+:a[two:3])
  ARRAY_BODY (a, two, 3)
  ARRAY_BODY (o, two, 3)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel reduction(+:a[one:2])
  ARRAY_BODY (a, one, 2)
  ARRAY_BODY (o, one, 2)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

#endif
  return 0;
}
