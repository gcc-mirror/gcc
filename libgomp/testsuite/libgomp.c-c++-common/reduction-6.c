/* { dg-additional-options "-foffload-options=nvptx-none=-latomic" { target { offload_target_nvptx } } } */
/* C / C++'s logical AND and OR operators take any scalar argument
   which compares (un)equal to 0 - the result 1 or 0 and of type int.

   In this testcase, the int result is again converted to an integer complex
   type.

   While having a floating-point/complex array element with || and && can make
   sense, having a complex reduction variable is odd but valid.

   Test: int complex reduction variable + int complex array.
         as reduction-4.c but with target.  */

#define N 1024
_Complex char rcc[N];
_Complex short rcs[N];
_Complex int rci[N];
_Complex long long rcl[N];

int
reduction_or ()
{
  _Complex char orc = 0;
  _Complex short ors = 0;
  _Complex int ori = 0;
  _Complex long orl = 0;

  #pragma omp target parallel reduction(||: orc) map(orc)
  for (int i=0; i < N; ++i)
    orc = orc || rcl[i];

  #pragma omp target parallel for reduction(||: ors) map(ors)
  for (int i=0; i < N; ++i)
    ors = ors || rci[i];

  #pragma omp target parallel for simd reduction(||: ori) map(ori)
  for (int i=0; i < N; ++i)
    ori = ori || rcs[i];

  #pragma omp target parallel loop reduction(||: orl) map(orl)
  for (int i=0; i < N; ++i)
    orl = orl || rcc[i];

  return __real__ (orc + ors + ori + orl) + __imag__ (orc + ors + ori + orl);
}

int
reduction_or_teams ()
{
  _Complex char orc = 0;
  _Complex short ors = 0;
  _Complex int ori = 0;
  _Complex long orl = 0;

  #pragma omp target teams distribute parallel for reduction(||: orc) map(orc)
  for (int i=0; i < N; ++i)
    orc = orc || rcc[i];

  #pragma omp target teams distribute parallel for simd reduction(||: ors) map(ors)
  for (int i=0; i < N; ++i)
    ors = ors || rcs[i];

  #pragma omp target teams distribute parallel for reduction(||: ori) map(ori)
  for (int i=0; i < N; ++i)
    ori = ori || rci[i];

  #pragma omp target teams distribute parallel for simd reduction(||: orl) map(orl)
  for (int i=0; i < N; ++i)
    orl = orl || rcl[i];

  return __real__ (orc + ors + ori + orl) + __imag__ (orc + ors + ori + orl);
}

int
reduction_and ()
{
  _Complex char andc = 1;
  _Complex short ands = 1;
  _Complex int andi = 1;
  _Complex long andl = 1;

  #pragma omp target parallel reduction(&&: andc) map(andc)
  for (int i=0; i < N; ++i)
    andc = andc && rcc[i];

  #pragma omp target parallel for reduction(&&: ands) map(ands)
  for (int i=0; i < N; ++i)
    ands = ands && rcs[i];

  #pragma omp target parallel for simd reduction(&&: andi) map(andi)
  for (int i=0; i < N; ++i)
    andi = andi && rci[i];

  #pragma omp target parallel loop reduction(&&: andl) map(andl)
  for (int i=0; i < N; ++i)
    andl = andl && rcl[i];

  return __real__ (andc + ands + andi + andl)
	 + __imag__ (andc + ands + andi + andl);
}

int
reduction_and_teams ()
{
  _Complex char andc = 1;
  _Complex short ands = 1;
  _Complex int andi = 1;
  _Complex long andl = 1;

  #pragma omp target teams distribute parallel for reduction(&&: andc) map(andc)
  for (int i=0; i < N; ++i)
    andc = andc && rcl[i];

  #pragma omp target teams distribute parallel for simd reduction(&&: ands) map(ands)
  for (int i=0; i < N; ++i)
    ands = ands && rci[i];

  #pragma omp target teams distribute parallel for reduction(&&: andi) map(andi)
  for (int i=0; i < N; ++i)
    andi = andi && rcs[i];

  #pragma omp target teams distribute parallel for simd reduction(&&: andl) map(andl)
  for (int i=0; i < N; ++i)
    andl = andl && rcc[i];

  return __real__ (andc + ands + andi + andl)
	 + __imag__ (andc + ands + andi + andl);
}

int
main ()
{
  for (int i = 0; i < N; ++i)
    {
      rcc[i] = 0;
      rcs[i] = 0;
      rci[i] = 0;
      rcl[i] = 0;
    }

  if (reduction_or () != 0)
    __builtin_abort ();
  if (reduction_or_teams () != 0)
    __builtin_abort ();
  if (reduction_and () != 0)
    __builtin_abort ();
  if (reduction_and_teams () != 0)
    __builtin_abort ();

  rcc[10] = 1.0;
  rcs[15] = 1.0i;
  rci[10] = 1.0;
  rcl[15] = 1.0i;

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
      rcc[i] = 1;
      rcs[i] = 1i;
      rci[i] = 1;
      rcl[i] = 1 + 1i;
    }

  if (reduction_or () != 4)
    __builtin_abort ();
  if (reduction_or_teams () != 4)
    __builtin_abort ();
  if (reduction_and () != 4)
    __builtin_abort ();
  if (reduction_and_teams () != 4)
    __builtin_abort ();

  rcc[10] = 0.0;
  rcs[15] = 0.0;
  rci[10] = 0.0;
  rcl[15] = 0.0;

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
