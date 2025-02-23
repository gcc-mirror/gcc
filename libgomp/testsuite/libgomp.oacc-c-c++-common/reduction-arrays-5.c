/* { dg-do run } */

/* Same as reduction-arrays-4.c test, but reduced arrays are VLAs.  */

#define ARRAY_BODY(ARRAY, MIN, LEN)		\
  for (int i = 0; i < 10; i++)			\
    for (int j = MIN; j < MIN + LEN; j++)	\
      ARRAY[j] += 1;

int zero = 0;
int one = 1;
int two = 2;
int three = 3;
int four = 4;
int five = 5;
int six = 6;

int main (void)
{
  int init[6] = { 5, 1, 1, 5, 9, 9 };
  int o[six];
  int a[six];

  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    a[i] = o[i] = init[i];

  #pragma acc parallel
  #pragma acc loop reduction(+:a[one:two])
  ARRAY_BODY (a, one, two)
  ARRAY_BODY (o, one, two)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a[three:two])
  #pragma acc loop reduction(+:a[three:two])
  ARRAY_BODY (a, three, two)
  ARRAY_BODY (o, three, two)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a)
  #pragma acc loop reduction(+:a[zero:five])
  ARRAY_BODY (a, zero, five)
  ARRAY_BODY (o, zero, five)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel
  #pragma acc loop reduction(+:a)
  ARRAY_BODY (a, four, one)
  ARRAY_BODY (o, four, one)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel copy(a)
  #pragma acc loop reduction(+:a)
  ARRAY_BODY (a, three, three)
  ARRAY_BODY (o, three, three)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel loop reduction(+:a)
  ARRAY_BODY (a, one, three)
  ARRAY_BODY (o, one, three)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel loop reduction(+:a[two:three])
  ARRAY_BODY (a, two, three)
  ARRAY_BODY (o, two, three)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  #pragma acc parallel reduction(+:a[one:two])
  ARRAY_BODY (a, one, two)
  ARRAY_BODY (o, one, two)
  for (int i = 0; i < sizeof (a) / sizeof (int); i++)
    if (a[i] != o[i])
      __builtin_abort ();

  return 0;
}
