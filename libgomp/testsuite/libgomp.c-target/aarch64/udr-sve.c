/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-march=armv8-a+sve -msve-vector-bits=256 -fopenmp -O2" } */

#include <arm_sve.h>

#pragma omp declare reduction (+:svint32_t: omp_out = svadd_s32_z (svptrue_b32(), omp_in, omp_out)) \
		    initializer (omp_priv = svindex_s32 (0, 0))

void __attribute__ ((noipa))
parallel_reduction ()
{
  int a[8] = {1, 1, 1, 1, 1, 1, 1, 1};
  int b[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  svint32_t va = svld1_s32 (svptrue_b32 (), b);
  int i = 0;
  int64_t res;

  #pragma omp parallel reduction (+:va, i)
    {
      va = svld1_s32 (svptrue_b32 (), a);
      i++;
    }

  res = svaddv_s32 (svptrue_b32 (), va);

  if (res != i * 8)
    __builtin_abort ();
}

void __attribute__ ((noipa))
for_reduction ()
{
  int a[8] = {1, 1, 1, 1, 1, 1, 1, 1};
  int b[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  svint32_t va = svld1_s32 (svptrue_b32 (), b);
  int j;
  int64_t res;

  #pragma omp parallel for reduction (+:va)
  for (j = 0; j < 8; j++)
    va += svld1_s32 (svptrue_b32 (), a);

  res = svaddv_s32 (svptrue_b32 (), va);

  if (res != 64)
    __builtin_abort ();
}

void __attribute__ ((noipa))
simd_reduction ()
{
  int a[8];
  svint32_t va = svindex_s32 (0, 0);
  int i = 0;
  int j;
  int64_t res = 0;

  for (j = 0; j < 8; j++)
    a[j] = 1;

  #pragma omp simd reduction (+:va)
  for (j = 0; j < 16; j++)
    va += svld1_s32 (svptrue_b32 (), a);

  res = svaddv_s32 (svptrue_b32 (), va);

  if (res != 128)
    __builtin_abort ();
}

void __attribute__ ((noipa))
inscan_reduction_incl ()
{
  svint32_t va = svindex_s32 (0, 0);
  int a[8] = {1, 1, 1, 1, 1, 1, 1, 1};
  int b[64] = { 0 };
  int j;
  int64_t res = 0;

  #pragma omp parallel for reduction (inscan, +:va)
  for (j = 0; j < 8; j++)
    {
      va += svld1_s32 (svptrue_b32 (), a);
      #pragma omp scan inclusive (va)
      svst1_s32 (svptrue_b32 (), b + j * 8, va);
    }

  res = svaddv_s32 (svptrue_b32 (), va);

  if (res != 64)
    __builtin_abort ();

  for (j = 0; j < 64; j+=8)
    if (b[j] != (j / 8 + 1))
      __builtin_abort ();
}

void __attribute__ ((noipa))
inscan_reduction_excl ()
{
  svint32_t va = svindex_s32 (0, 0);
  int a[8] = {1, 1, 1, 1, 1, 1, 1, 1};
  int b[64] = { 0 };
  int j;
  int64_t res = 0;

  #pragma omp parallel for reduction (inscan, +:va)
  for (j = 0; j < 8; j++)
    {
      svst1_s32 (svptrue_b32 (), b + j * 8, va);
      #pragma omp scan exclusive (va)
      va += svld1_s32 (svptrue_b32 (), a);
    }

  res = svaddv_s32 (svptrue_b32 (), va);

  if (res != 64)
    __builtin_abort ();

  for (j = 0; j < 64; j+=8)
    if (b[j] != j / 8)
      __builtin_abort ();
}


int
main ()
{
  parallel_reduction ();
  for_reduction ();
  simd_reduction ();
  inscan_reduction_incl ();
  inscan_reduction_excl ();
}
