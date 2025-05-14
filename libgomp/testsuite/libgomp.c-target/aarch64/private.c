/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#pragma GCC target "+sve"

#include <arm_sve.h>
#include <omp.h>

static void __attribute__ ((noipa))
compare_vec (svint32_t *x, svint32_t y)
{
  svbool_t p = svnot_b_z (svptrue_b32 (), svcmpeq_s32 (svptrue_b32 (), *x, y));

  if (svptest_any (svptrue_b32 (), p))
    __builtin_abort ();
}

void __attribute__ ((noipa))
private ()
{
  svint32_t a;
#pragma omp parallel private (a) num_threads (10)
  {
    a = svindex_s32 (omp_get_thread_num (), 0);

#pragma omp barrier
    compare_vec (&a, svindex_s32 (omp_get_thread_num (), 0));
  }
}

void __attribute__ ((noipa))
firstprivate ()
{
  svint32_t a = svindex_s32 (1,1);
  svint32_t b;

#pragma omp parallel private (b) firstprivate (a) num_threads (12)
  {
    compare_vec (&a, svindex_s32 (1, 1));
    b = svindex_s32 (omp_get_thread_num (), 0);

#pragma omp barrier
    compare_vec (&a, svindex_s32 (1, 1));
    compare_vec (&b, svindex_s32 (omp_get_thread_num (), 0));
    if (omp_get_thread_num () == 5)
      {
	a = svindex_s32 (1, 2);
	b = svindex_s32 (10, 0);
      }

#pragma omp barrier
    if (omp_get_thread_num () == 5)
      {
	compare_vec (&a, svindex_s32 (1, 2));
	compare_vec (&b, svindex_s32 (10, 0));
      }
    else
      {
	compare_vec (&a, svindex_s32 (1, 1));
	compare_vec (&b, svindex_s32 (omp_get_thread_num (), 0));
      }
  }
}

void __attribute__ ((noipa))
lastprivate ()
{
  svint32_t a = svindex_s32 (1,1);
  svint32_t b;
  int i;

#pragma omp parallel for private (a) lastprivate (b)
  for (i = 0; i < 16; i++)
    {
      b = svindex_s32 (i, 0);

      compare_vec (&b, svindex_s32 (i, 0));
      if (i == 5)
	{
	  a = svindex_s32 (1, 2);
	  b = svindex_s32 (10, 0);
	}
      else
	a = svindex_s32 (1, 1);

      if (i == 5)
	{
	  compare_vec (&a, svindex_s32 (1, 2));
	  compare_vec (&b, svindex_s32 (10, 0));
	}
      else
	{
	  compare_vec (&a, svindex_s32 (1, 1));
	  compare_vec (&b, svindex_s32 (i, 0));
	}
    }

  compare_vec (&b, svindex_s32 (15, 0));
}

int
main ()
{
  private ();
  firstprivate ();
  lastprivate ();
}
