/* Check that the SIMD versions of math routines give the same (or
   sufficiently close) results as their scalar equivalents.  */

/* { dg-do run { target { ! hppa*-*-hpux* } } } */
/* { dg-options "-O2 -ftree-vectorize -fno-math-errno" } */
/* { dg-additional-options -foffload-options=amdgcn-amdhsa=-mstack-size=3000000 { target offload_target_amdgcn } } */
/* { dg-additional-options "-DNONSTDFUNC=1" { target nonstandard_math_functions } } */

#undef PRINT_RESULT
#define VERBOSE 0
#define EARLY_EXIT 1

#include <math.h>
#include <stdlib.h>

#ifdef PRINT_RESULT
  #include <stdio.h>
  #define PRINTF printf
#else
  static void null_printf (const char *f, ...) { }

  #define PRINTF null_printf
#endif

#define N 512
#define EPSILON_float 1e-5
#define EPSILON_double 1e-10

static int xfail = 0;
static int failed = 0;

int deviation_float (float x, float y)
{
  union {
    float f;
    unsigned u;
  } u, v;

  u.f = x;
  v.f = y;

  unsigned mask = 0x80000000U;
  int i;

  for (i = 32; i > 0; i--)
    if ((u.u ^ v.u) & mask)
      break;
    else
      mask >>= 1;

  return i;
}

int deviation_double (double x, double y)
{
  union {
    double d;
    unsigned long long u;
  } u, v;

  u.d = x;
  v.d = y;

  unsigned long long mask = 0x8000000000000000ULL;
  int i;

  for (i = 64; i > 0; i--)
    if ((u.u ^ v.u) & mask)
      break;
    else
      mask >>= 1;

  return i;
}

#define TEST_FUN_XFAIL(TFLOAT, LOW, HIGH, FUN) \
  xfail = 1; \
  TEST_FUN (TFLOAT, LOW, HIGH, FUN); \
  xfail = 0;

#define TEST_FUN(TFLOAT, LOW, HIGH, FUN) \
__attribute__((optimize("no-tree-vectorize"))) \
__attribute__((optimize("no-unsafe-math-optimizations"))) \
void check_##FUN (TFLOAT res[N], TFLOAT a[N]) \
{ \
  for (int i = 0; i < N; i++) { \
    TFLOAT expected = FUN (a[i]); \
    TFLOAT diff = __builtin_fabs (expected - res[i]); \
    int deviation = deviation_##TFLOAT (expected, res[i]); \
    int fail = isnan (res[i]) != isnan (expected) \
	       || isinf (res[i]) != isinf (expected) \
	       || (diff > EPSILON_##TFLOAT && deviation > 10); \
    if (VERBOSE || fail) \
      PRINTF (#FUN "(%f) = %f, expected = %f, diff = %f, deviation = %d %s\n", \
	      a[i], res[i], expected, diff, deviation, fail ? "(!)" : ""); \
    failed |= (fail && !xfail); \
    if (EARLY_EXIT && failed) \
      exit (1); \
  } \
} \
void test_##FUN (void) \
{ \
  TFLOAT res[N], a[N]; \
  for (int i = 0; i < N; i++) \
    a[i] = LOW + ((HIGH - LOW) / N) * i; \
  _Pragma ("omp target parallel for simd map(to:a) map(from:res)") \
    for (int i = 0; i < N; i++) \
      res[i] = FUN (a[i]); \
  check_##FUN (res, a); \
}\
test_##FUN ();

#define TEST_FUN2(TFLOAT, LOW1, HIGH1, LOW2, HIGH2, FUN) \
__attribute__((optimize("no-tree-vectorize"))) \
__attribute__((optimize("no-unsafe-math-optimizations"))) \
void check_##FUN (TFLOAT res[N], TFLOAT a[N], TFLOAT b[N]) \
{ \
  int failed = 0; \
  for (int i = 0; i < N; i++) { \
    TFLOAT expected = FUN (a[i], b[i]); \
    TFLOAT diff = __builtin_fabs (expected - res[i]); \
    int deviation = deviation_##TFLOAT (expected, res[i]); \
    int fail = isnan (res[i]) != isnan (expected) \
	       || isinf (res[i]) != isinf (expected) \
	       || (diff > EPSILON_##TFLOAT && deviation > 10); \
    failed |= fail; \
    if (VERBOSE || fail) \
      PRINTF (#FUN "(%f,%f) = %f, expected = %f, diff = %f, deviation = %d %s\n", \
	      a[i], b[i], res[i], expected, diff, deviation, fail ? "(!)" : ""); \
    if (EARLY_EXIT && fail) \
      exit (1); \
  } \
} \
void test_##FUN (void) \
{ \
  TFLOAT res[N], a[N], b[N]; \
  for (int i = 0; i < N; i++) { \
    a[i] = LOW1 + ((HIGH1 - LOW1) / N) * i; \
    b[i] = LOW2 + ((HIGH2 - LOW2) / N) * i; \
  } \
  _Pragma ("omp target parallel for simd map(to:a) map(from:res)") \
    for (int i = 0; i < N; i++) \
      res[i] = FUN (a[i], b[i]); \
  check_##FUN (res, a, b); \
}\
test_##FUN ();

int main (void)
{
  TEST_FUN (float, -1.1, 1.1, acosf);
  TEST_FUN (float, -10, 10, acoshf);
  TEST_FUN (float, -1.1, 1.1, asinf);
  TEST_FUN (float, -10, 10, asinhf);
  TEST_FUN (float, -1.1, 1.1, atanf);
  TEST_FUN2 (float, -2.0, 2.0, 2.0, -2.0, atan2f);
  TEST_FUN (float, -2.0, 2.0, atanhf);
  TEST_FUN2 (float, -10.0, 10.0, 5.0, -15.0, copysignf);
  TEST_FUN (float, -3.14159265359, 3.14159265359, cosf);
  TEST_FUN (float, -3.14159265359, 3.14159265359, coshf);
  TEST_FUN (float, -10.0, 10.0, erff);
  TEST_FUN (float, -10.0, 10.0, expf);
  TEST_FUN (float, -10.0, 10.0, exp2f);
  TEST_FUN2 (float, -10.0, 10.0, 100.0, -25.0, fmodf);
#ifdef NONSTDFUNC
  TEST_FUN (float, -10.0, 10.0, gammaf);
#endif
  TEST_FUN2 (float, -10.0, 10.0, 15.0, -5.0,hypotf);
  TEST_FUN (float, -10.0, 10.0, lgammaf);
  TEST_FUN (float, -1.0, 50.0, logf);
  TEST_FUN (float, -1.0, 500.0, log10f);
  TEST_FUN (float, -1.0, 64.0, log2f);
  TEST_FUN2 (float, -100.0, 100.0, 100.0, -100.0, powf);
  TEST_FUN2 (float, -50.0, 100.0, -2.0, 40.0, remainderf);
  TEST_FUN (float, -50.0, 50.0, rintf);
#ifdef NONSTDFUNC
  TEST_FUN2 (float, -50.0, 50.0, -10.0, 32.0, __builtin_scalbf);
  TEST_FUN (float, -10.0, 10.0, __builtin_significandf);
#endif
  TEST_FUN (float, -3.14159265359, 3.14159265359, sinf);
  TEST_FUN (float, -3.14159265359, 3.14159265359, sinhf);
  TEST_FUN (float, -0.1, 10000.0, sqrtf);
  TEST_FUN (float, -5.0, 5.0, tanf);
  TEST_FUN (float, -3.14159265359, 3.14159265359, tanhf);
  /* Newlib's version of tgammaf is known to have poor accuracy.  */
  TEST_FUN_XFAIL (float, -10.0, 10.0, tgammaf);

  TEST_FUN (double, -1.1, 1.1, acos);
  TEST_FUN (double, -10, 10, acosh);
  TEST_FUN (double, -1.1, 1.1, asin);
  TEST_FUN (double, -10, 10, asinh);
  TEST_FUN (double, -1.1, 1.1, atan);
  TEST_FUN2 (double, -2.0, 2.0, 2.0, -2.0, atan2);
  TEST_FUN (double, -2.0, 2.0, atanh);
  TEST_FUN2 (double, -10.0, 10.0, 5.0, -15.0, copysign);
  TEST_FUN (double, -3.14159265359, 3.14159265359, cos);
  TEST_FUN (double, -3.14159265359, 3.14159265359, cosh);
  TEST_FUN (double, -10.0, 10.0, erf);
  TEST_FUN (double, -10.0, 10.0, exp);
  TEST_FUN (double, -10.0, 10.0, exp2);
  TEST_FUN2 (double, -10.0, 10.0, 100.0, -25.0, fmod);
#ifdef NONSTDFUNC
  TEST_FUN (double, -10.0, 10.0, gamma);
#endif
  TEST_FUN2 (double, -10.0, 10.0, 15.0, -5.0, hypot);
  TEST_FUN (double, -10.0, 10.0, lgamma);
  TEST_FUN (double, -1.0, 50.0, log);
  TEST_FUN (double, -1.0, 500.0, log10);
  TEST_FUN (double, -1.0, 64.0, log2);
  TEST_FUN2 (double, -100.0, 100.0, 100.0, -100.0, pow);
  TEST_FUN2 (double, -50.0, 100.0, -2.0, 40.0, remainder);
  TEST_FUN (double, -50.0, 50.0, rint);
#ifdef NONSTDFUNC
  TEST_FUN2 (double, -50.0, 50.0, -10.0, 32.0, __builtin_scalb);
  TEST_FUN (double, -10.0, 10.0, __builtin_significand);
#endif
  TEST_FUN (double, -3.14159265359, 3.14159265359, sin);
  TEST_FUN (double, -3.14159265359, 3.14159265359, sinh);
  TEST_FUN (double, -0.1, 10000.0, sqrt);
  TEST_FUN (double, -5.0, 5.0, tan);
  TEST_FUN (double, -3.14159265359, 3.14159265359, tanh);
  /* Newlib's version of tgamma is known to have poor accuracy.  */
  TEST_FUN_XFAIL (double, -10.0, 10.0, tgamma);

  return failed;
}
