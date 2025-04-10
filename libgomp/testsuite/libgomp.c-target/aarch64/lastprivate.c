/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#pragma GCC target "+sve"

#include <arm_sve.h>
#include <omp.h>

static svint32_t __attribute__ ((noipa))
foo (svint32_t *vb, svint32_t *vc, int tn)
{
  svint32_t temp = svindex_s32 (tn, 0);
  temp = svadd_s32_z (svptrue_b32 (), temp, *vb);
  return svadd_s32_z (svptrue_b32 (), temp, *vc);
}

void __attribute__ ((noipa))
lastprivate_sections ()
{
  int a[8], b[8], c[8];
  svint32_t va, vb, vc;
  int i;

#pragma omp parallel for
  for (i = 0; i < 8; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

#pragma omp parallel sections lastprivate (vb, vc) num_threads (2)
  {
    #pragma omp section
    vb = svld1_s32 (svptrue_b32 (), b);
    #pragma omp section
    vb = svld1_s32 (svptrue_b32 (), b);
    vc = svld1_s32 (svptrue_b32 (), c);
  }

  va = svadd_s32_z (svptrue_b32 (), vb, vc);
  svst1_s32 (svptrue_b32 (), a, va);

  for (i = 0; i < 8; i++)
    if (a[i] != b[i] + c[i])
      __builtin_abort ();
}

void __attribute__ ((noipa))
lastprivate_for ()
{
  int a[32], b[32], c[32];
  int aa[8], bb[8], cc[8];
  svint32_t va, vb, vc;
  int i, tn;

#pragma omp parallel for
  for (i = 0; i < 32; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

#pragma omp parallel for lastprivate (va, vb, vc, tn)
  for (i = 0; i < 4; i++)
    {
      vb = svld1_s32 (svptrue_b32 (), b + i * 8);
      vc = svld1_s32 (svptrue_b32 (), c + i * 8);
      tn = i;
      va = foo (&vb, &vc, tn);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }

  svst1_s32 (svptrue_b32 (), aa, va);
  svst1_s32 (svptrue_b32 (), bb, vb);
  svst1_s32 (svptrue_b32 (), cc, vc);

  for (i = 0; i < 8; i++)
    if (aa[i] != bb[i] + cc[i] + tn)
      __builtin_abort ();

  for (i = 0; i < 32; i++)
    if (a[i] != b[i] + c[i] + i / 8)
      __builtin_abort ();
}

void __attribute__ ((noipa))
lastprivate_simd ()
{

  int a[64], b[64], c[64];
  int aa[8], bb[8], cc[8];
  svint32_t va, vb, vc;
  int i;

#pragma omp parallel for
  for (i = 0; i < 64; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

#pragma omp simd lastprivate (va, vb, vc)
  for (i = 0; i < 8; i++)
    {
      vb = svld1_s32 (svptrue_b32 (), b + i * 8);
      vc = svld1_s32 (svptrue_b32 (), c + i * 8);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }

  svst1_s32 (svptrue_b32 (), aa, va);
  svst1_s32 (svptrue_b32 (), bb, vb);
  svst1_s32 (svptrue_b32 (), cc, vc);

  for (i = 0; i < 8; i++)
    if (aa[i] != bb[i] + cc[i])
      __builtin_abort ();

  for (i = 0; i < 64; i++)
    if (a[i] != b[i] + c[i])
      __builtin_abort ();
}

void __attribute__ ((noipa))
lastprivate_distribute ()
{

  int a[32], b[32], c[32];
  int aa[8], bb[8], cc[8];
  svint32_t va, vb, vc;
  int i, tn;

#pragma omp parallel for
  for (i = 0; i < 32; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

#pragma omp teams
#pragma omp distribute lastprivate (va, vb, vc, tn)
  for (i = 0; i < 4; i++)
    {
      vb = svld1_s32 (svptrue_b32 (), b + i * 8);
      vc = svld1_s32 (svptrue_b32 (), c + i * 8);
      tn = i;
      va = foo (&vb, &vc, tn);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }

  svst1_s32 (svptrue_b32 (), aa, va);
  svst1_s32 (svptrue_b32 (), bb, vb);
  svst1_s32 (svptrue_b32 (), cc, vc);

  for (i = 0; i < 8; i++)
    if (aa[i] != bb[i] + cc[i] + tn)
      __builtin_abort ();

  for (i = 0; i < 32; i++)
    if (a[i] != b[i] + c[i] + i / 8)
      __builtin_abort ();
}

int
main ()
{
  lastprivate_for ();
  lastprivate_sections ();
  lastprivate_simd ();
  lastprivate_distribute ();
}
