/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#pragma GCC target "+sve"

#include <arm_sve.h>
#include <omp.h>

static void __attribute__ ((noipa))
vec_compare (svint32_t *x, svint32_t y)
{
  svbool_t p = svnot_b_z (svptrue_b32 (), svcmpeq_s32 (svptrue_b32 (), *x, y));

  if (svptest_any (svptrue_b32 (), p))
    __builtin_abort ();
}

void __attribute__ ((noipa))
firstprivate_sections ()
{
  int b[8], c[8];
  svint32_t vb, vc;
  int i;

#pragma omp parallel for
  for (i = 0; i < 8; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

  vb = svld1_s32 (svptrue_b32 (), b);
  vc = svld1_s32 (svptrue_b32 (), c);

#pragma omp parallel sections firstprivate (vb, vc)
  {
    #pragma omp section
    vec_compare (&vb, svindex_s32 (0, 1));
    vec_compare (&vc, svindex_s32 (1, 1));

    #pragma omp section
    vec_compare (&vb, svindex_s32 (0, 1));
    vec_compare (&vc, svindex_s32 (1, 1));
  }

}

void __attribute__ ((noipa))
firstprivate_for ()
{

  int a[32], b[32], c[32];
  svint32_t va, vb, vc;
  int i;

#pragma omp parallel for
  for (i = 0; i < 32; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

  vb = svindex_s32 (1, 0);
  vc = svindex_s32 (0, 1);

#pragma omp parallel for firstprivate (vb, vc) private (va)
  for (i = 0; i < 4; i++)
    {
      svint32_t tb, tc;
      vec_compare (&vb, svindex_s32 (1, 0));
      vec_compare (&vc, svindex_s32 (0, 1));
      tb = svld1_s32 (svptrue_b32 (), b + i * 8);
      tc = svld1_s32 (svptrue_b32 (), c + i * 8);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      va = svadd_s32_z (svptrue_b32 (), va, tb);
      va = svadd_s32_z (svptrue_b32 (), va, tc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }

  for (i = 0; i < 32; i++)
    if (a[i] != b[i] + c[i] + vb[i % 8] + vc[i % 8])
      __builtin_abort ();
}

void __attribute__ ((noipa))
firstprivate_distribute ()
{

  int a[32], b[32], c[32];
  svint32_t va, vb, vc;
  int i;

#pragma omp parallel for
  for (i = 0; i < 32; i++)
    {
      b[i] = i;
      c[i] = i + 1;
    }

  vb = svindex_s32 (1, 0);
  vc = svindex_s32 (0, 1);

#pragma omp teams
#pragma omp distribute firstprivate (vb, vc) private (va)
  for (i = 0; i < 4; i++)
    {
      svint32_t tb, tc;
      vec_compare (&vb, svindex_s32 (1, 0));
      vec_compare (&vc, svindex_s32 (0, 1));
      tb = svld1_s32 (svptrue_b32 (), b + i * 8);
      tc = svld1_s32 (svptrue_b32 (), c + i * 8);
      va = svadd_s32_z (svptrue_b32 (), vb, vc);
      va = svadd_s32_z (svptrue_b32 (), va, tb);
      va = svadd_s32_z (svptrue_b32 (), va, tc);
      svst1_s32 (svptrue_b32 (), a + i * 8, va);
    }

  for (i = 0; i < 32; i++)
    if (a[i] != b[i] + c[i] + vb[i % 8] + vc[i % 8])
      __builtin_abort ();
}

int
main ()
{
  firstprivate_for ();
  firstprivate_sections ();
  firstprivate_distribute ();
}
