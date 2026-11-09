/* { dg-additional-options "-foffload-options=nvptx-none=-latomic" { target { offload_target_nvptx } } } */

/* Complex-typed simd reductions in offloaded code.  SIMT targets combine
   these across the lanes of a warp or wavefront.  */

#define N 1024

_Complex double cd[N];
_Complex float cf[N];
_Complex int ci[N];

static void
init (void)
{
  for (int i = 0; i < N; i++)
    {
      cd[i] = i + (N - i) * 1.0i;
      cf[i] = i + (N - i) * 1.0fi;
      ci[i] = i + (N - i) * 1i;
    }
}

static int
plus (void)
{
  _Complex double sd = 0;
  _Complex float sf = 0;
  _Complex int si = 0;

  #pragma omp target simd reduction(+: sd) map(tofrom: sd)
  for (int i = 0; i < N; i++)
    sd += cd[i];

  #pragma omp target teams distribute parallel for simd reduction(+: sf) \
	  map(tofrom: sf)
  for (int i = 0; i < N; i++)
    sf += cf[i];

  #pragma omp target simd reduction(-: si) map(tofrom: si) /* { dg-warning "'-' operator for reductions deprecated" } */
  for (int i = 0; i < N; i++)
    si -= ci[i];

  /* Sum of i is N * (N - 1) / 2, and sum of N - i is N * (N + 1) / 2.  */
  if (__real__ sd != N * (N - 1) / 2 || __imag__ sd != N * (N + 1) / 2)
    return 1;
  if (__real__ sf != N * (N - 1) / 2 || __imag__ sf != N * (N + 1) / 2)
    return 2;
  if (__real__ si != -(N * (N - 1) / 2) || __imag__ si != -(N * (N + 1) / 2))
    return 3;
  return 0;
}

static int
mult (void)
{
  _Complex double pd = 1;
  _Complex float pf = 1;

  /* Eight factors of 2i: the product is 256, exactly.  Multiplying the
     real and imaginary parts separately would give 0.  */
  #pragma omp target simd reduction(*: pd) map(tofrom: pd)
  for (int i = 0; i < N; i++)
    pd *= (i % (N / 8) == 0) ? 2.0i : 1.0;

  #pragma omp target teams distribute parallel for simd reduction(*: pf) \
	  map(tofrom: pf)
  for (int i = 0; i < N; i++)
    pf *= (i % (N / 8) == 0) ? 2.0fi : 1.0f;

  if (__real__ pd != 256 || __imag__ pd != 0)
    return 4;
  if (__real__ pf != 256 || __imag__ pf != 0)
    return 5;
  return 0;
}

static int
logical (void)
{
  _Complex double ord = 0, andd = 1;
  _Complex float orf = 0, andf = 1;

  #pragma omp target simd reduction(||: ord) map(tofrom: ord)
  for (int i = 0; i < N; i++)
    ord = ord || (i == N - 3 ? cd[i] : 0);

  #pragma omp target simd reduction(&&: andd) map(tofrom: andd)
  for (int i = 0; i < N; i++)
    andd = andd && (i == 5 ? 0 : cd[i] + 1);

  #pragma omp target teams distribute parallel for simd reduction(||: orf) \
	  map(tofrom: orf)
  for (int i = 0; i < N; i++)
    orf = orf || (i == 7 ? 1.0fi : 0);

  #pragma omp target teams distribute parallel for simd reduction(&&: andf) \
	  map(tofrom: andf)
  for (int i = 0; i < N; i++)
    andf = andf && cf[i];

  if (__real__ ord != 1 || __imag__ ord != 0)
    return 6;
  if (__real__ andd != 0 || __imag__ andd != 0)
    return 7;
  if (__real__ orf != 1 || __imag__ orf != 0)
    return 8;
  if (__real__ andf != 1 || __imag__ andf != 0)
    return 9;
  return 0;
}

int
main (void)
{
  int r;

  init ();

  if ((r = plus ()) || (r = mult ()) || (r = logical ()))
    return r;
  return 0;
}
