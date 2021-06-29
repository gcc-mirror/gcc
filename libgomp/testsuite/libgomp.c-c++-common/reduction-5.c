/* { dg-additional-options "-foffload-options=nvptx-none=-latomic" { target { offload_target_nvptx } } } */
/* C / C++'s logical AND and OR operators take any scalar argument
   which compares (un)equal to 0 - the result 1 or 0 and of type int.

   In this testcase, the int result is again converted to a floating-poing
   or complex type.

   While having a floating-point/complex array element with || and && can make
   sense, having a non-integer/non-bool reduction variable is odd but valid.

   Test: FP reduction variable + FP array - as reduction-1.c but with target  */

#define N 1024
_Complex float rcf[N];
_Complex double rcd[N];
float rf[N];
double rd[N];

int
reduction_or ()
{
  float orf = 0;
  double ord = 0;
  _Complex float orfc = 0;
  _Complex double ordc = 0;

  #pragma omp target parallel reduction(||: orf) map(orf)
  for (int i=0; i < N; ++i)
    orf = orf || rf[i];

  #pragma omp target parallel for reduction(||: ord) map(ord)
  for (int i=0; i < N; ++i)
    ord = ord || rcd[i];

  #pragma omp target parallel for simd reduction(||: orfc) map(orfc)
  for (int i=0; i < N; ++i)
    orfc = orfc || rcf[i];

  #pragma omp target parallel loop reduction(||: ordc) map(ordc)
  for (int i=0; i < N; ++i)
    ordc = ordc || rcd[i];

  return orf + ord + __real__ orfc + __real__ ordc;
}

int
reduction_or_teams ()
{
  float orf = 0;
  double ord = 0;
  _Complex float orfc = 0;
  _Complex double ordc = 0;

  #pragma omp target teams distribute parallel for reduction(||: orf) map(orf)
  for (int i=0; i < N; ++i)
    orf = orf || rf[i];

  #pragma omp target teams distribute parallel for simd reduction(||: ord) map(ord)
  for (int i=0; i < N; ++i)
    ord = ord || rcd[i];

  #pragma omp target teams distribute parallel for reduction(||: orfc) map(orfc)
  for (int i=0; i < N; ++i)
    orfc = orfc || rcf[i];

  #pragma omp target teams distribute parallel for simd reduction(||: ordc) map(ordc)
  for (int i=0; i < N; ++i)
    ordc = ordc || rcd[i];

  return orf + ord + __real__ orfc + __real__ ordc;
}

int
reduction_and ()
{
  float andf = 1;
  double andd = 1;
  _Complex float andfc = 1;
  _Complex double anddc = 1;

  #pragma omp target parallel reduction(&&: andf) map(andf)
  for (int i=0; i < N; ++i)
    andf = andf && rf[i];

  #pragma omp target parallel for reduction(&&: andd) map(andd)
  for (int i=0; i < N; ++i)
    andd = andd && rcd[i];

  #pragma omp target parallel for simd reduction(&&: andfc) map(andfc)
  for (int i=0; i < N; ++i)
    andfc = andfc && rcf[i];

  #pragma omp target parallel loop reduction(&&: anddc) map(anddc)
  for (int i=0; i < N; ++i)
    anddc = anddc && rcd[i];

  return andf + andd + __real__ andfc + __real__ anddc;
}

int
reduction_and_teams ()
{
  float andf = 1;
  double andd = 1;
  _Complex float andfc = 1;
  _Complex double anddc = 1;

  #pragma omp target teams distribute parallel for reduction(&&: andf) map(andf)
  for (int i=0; i < N; ++i)
    andf = andf && rf[i];

  #pragma omp target teams distribute parallel for simd reduction(&&: andd) map(andd)
  for (int i=0; i < N; ++i)
    andd = andd && rcd[i];

  #pragma omp target teams distribute parallel for reduction(&&: andfc) map(andfc)
  for (int i=0; i < N; ++i)
    andfc = andfc && rcf[i];

  #pragma omp target teams distribute parallel for simd reduction(&&: anddc) map(anddc)
  for (int i=0; i < N; ++i)
    anddc = anddc && rcd[i];

  return andf + andd + __real__ andfc + __real__ anddc;
}

int
main ()
{
  for (int i = 0; i < N; ++i)
    {
      rf[i] = 0;
      rd[i] = 0;
      rcf[i] = 0;
      rcd[i] = 0;
    }

  if (reduction_or () != 0)
    __builtin_abort ();
  if (reduction_or_teams () != 0)
    __builtin_abort ();
  if (reduction_and () != 0)
    __builtin_abort ();
  if (reduction_and_teams () != 0)
    __builtin_abort ();

  rf[10] = 1.0;
  rd[15] = 1.0;
  rcf[10] = 1.0;
  rcd[15] = 1.0i;

  if (reduction_or () != 4)
    __builtin_abort ();
  if (reduction_or_teams () != 4)
    __builtin_abort ();
  if (reduction_and () != 0)
    __builtin_abort ();
  if (reduction_and_teams () != 0)
    __builtin_abort ();

  for (int i = 0; i < N; ++i)
    {
      rf[i] = 1;
      rd[i] = 1;
      rcf[i] = 1;
      rcd[i] = 1;
    }

  if (reduction_or () != 4)
    __builtin_abort ();
  if (reduction_or_teams () != 4)
    __builtin_abort ();
  if (reduction_and () != 4)
    __builtin_abort ();
  if (reduction_and_teams () != 4)
    __builtin_abort ();

  rf[10] = 0.0;
  rd[15] = 0.0;
  rcf[10] = 0.0;
  rcd[15] = 0.0;

  if (reduction_or () != 4)
    __builtin_abort ();
  if (reduction_or_teams () != 4)
    __builtin_abort ();
  if (reduction_and () != 0)
    __builtin_abort ();
  if (reduction_and_teams () != 0)
    __builtin_abort ();

  return 0;
}
