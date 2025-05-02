/* { dg-do run } */

/* More array reduction tests, different combinations of parallel/loop
   construct, implied/explicit copy clauses, and subarrays. */

#define ARRAY_BODY(ARRAY, MIN, LEN)		\
  for (int i = 0; i < 10; i++)			\
    for (int j = MIN; j < MIN + LEN; j++)	\
      ARRAY[j] += 1;

int main (void)
{
  int o[6] = { 5, 1, 1, 5, 9, 9 };
  int a[6];

  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    a[i] = o[i];

  #pragma acc parallel
  #pragma acc loop reduction(+:a[1:2])
  ARRAY_BODY (a, 1, 2)
  ARRAY_BODY (o, 1, 2)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a[3:2])
  #pragma acc loop reduction(+:a[3:2])
  ARRAY_BODY (a, 3, 2)
  ARRAY_BODY (o, 3, 2)
  for (int i = 0; i < 6; i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a)
  #pragma acc loop reduction(+:a[0:5])
  ARRAY_BODY (a, 0, 5)
  ARRAY_BODY (o, 0, 5)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel
  #pragma acc loop reduction(+:a)
  ARRAY_BODY (a, 4, 1)
  ARRAY_BODY (o, 4, 1)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a)
  #pragma acc loop reduction(+:a)
  ARRAY_BODY (a, 3, 3)
  ARRAY_BODY (o, 3, 3)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

#if !defined(ACC_DEVICE_TYPE_host)

  #pragma acc parallel loop reduction(+:a)
  ARRAY_BODY (a, 1, 3)
  ARRAY_BODY (o, 1, 3)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel loop reduction(+:a[2:3])
  ARRAY_BODY (a, 2, 3)
  ARRAY_BODY (o, 2, 3)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel reduction(+:a)
  ARRAY_BODY (a, 3, 2)
  ARRAY_BODY (o, 3, 2)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel reduction(+:a[1:2])
  ARRAY_BODY (a, 1, 2)
  ARRAY_BODY (o, 1, 2)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

#endif
  return 0;
}
