/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#pragma GCC target "+sve"

#include <arm_sve.h>
#include <stdlib.h>
#include <omp.h>

static void __attribute__ ((noipa))
compare_vec (svint32_t x, svint32_t y)
{
  svbool_t p = svnot_b_z (svptrue_b32 (), svcmpeq_s32 (svptrue_b32 (), x, y));

  if (svptest_any (svptrue_b32 (), p))
    __builtin_abort ();
}

static void __attribute__ ((noipa))
compare_vecb (svbool_t x, svbool_t y)
{
  svbool_t p = sveor_b_z (svptrue_b32 (), x, y);

  if (svptest_any (svptrue_b32 (), p))
    __builtin_abort ();
}

void __attribute__ ((noipa))
implicit_shared_default (svint32_t a, svint32_t b, svbool_t p)
{

#pragma omp parallel default (shared) num_threads (10)
  {
    /* 'a', 'b' and 'p' are implicitly shared.  */
    compare_vec (a, svindex_s32 (0, 1));
    compare_vec (b, svindex_s32 (8, 1));
    compare_vecb (p, svptrue_b32 ());

#pragma omp barrier
    if (omp_get_thread_num () == 2)
      a = svadd_s32_z (p, a, b);

#pragma omp barrier
    if (omp_get_thread_num () == 0)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svindex_s32 (8, 1));
	compare_vecb (p, svptrue_b32 ());
	b = svadd_s32_z (p, a, b);
      }

#pragma omp barrier
    compare_vec (a, svindex_s32 (8, 2));
    compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));

#pragma omp barrier
    if (omp_get_thread_num () == 0 || omp_get_thread_num () == 2)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));
      }
  }
}

void __attribute__ ((noipa))
explicit_shared (svint32_t a, svint32_t b, svbool_t p)
{

#pragma omp parallel shared (a, b, p) num_threads (12)
  {
    /* 'a', 'b' and 'p' are explicitly shared.  */
    compare_vec (a, svindex_s32 (0, 1));
    compare_vec (b, svindex_s32 (8, 1));
    compare_vecb (p, svptrue_b32 ());

#pragma omp barrier
    if (omp_get_thread_num () == 2)
      a = svadd_s32_z (p, a, b);

#pragma omp barrier
    if (omp_get_thread_num () == 0)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svindex_s32 (8, 1));
	compare_vecb (p, svptrue_b32 ());
	b = svadd_s32_z (p, a, b);
      }

#pragma omp barrier
    compare_vec (a, svindex_s32 (8, 2));
    compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));

#pragma omp barrier
    if (omp_get_thread_num () == 0 || omp_get_thread_num () == 2)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));
      }
  }
}

void __attribute__ ((noipa))
implicit_shared_no_default (svint32_t a, svint32_t b, svbool_t p)
{

#pragma omp parallel num_threads (16)
  {
    /* 'a', 'b' and 'p' are implicitly shared without default clause.  */
    compare_vec (a, svindex_s32 (0, 1));
    compare_vec (b, svindex_s32 (8, 1));
    compare_vecb (p, svptrue_b32 ());

#pragma omp barrier
    if (omp_get_thread_num () == 12)
      a = svadd_s32_z (p, a, b);

#pragma omp barrier
    if (omp_get_thread_num () == 15)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svindex_s32 (8, 1));
	compare_vecb (p, svptrue_b32 ());
	b = svadd_s32_z (p, a, b);
      }

#pragma omp barrier
    compare_vec (a, svindex_s32 (8, 2));
    compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));

#pragma omp barrier
    if (omp_get_thread_num () == 12 || omp_get_thread_num () == 15)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));
      }
  }

}

void __attribute__ ((noipa))
mix_shared (svint32_t b, svbool_t p)
{

  svint32_t a = svindex_s32 (0, 0);
  int *m = (int *) malloc (8 * sizeof (int));
  int i;

#pragma omp parallel for
  for (i = 0; i < 8; i++)
    m[i] = i;

#pragma omp parallel num_threads (16)
  {
    compare_vec (a, svindex_s32 (0, 0));
    compare_vec (b, svindex_s32 (8, 1));

#pragma omp barrier
    /* 'm' is predetermined shared here.  'a' is implicitly shared here.  */
    if (omp_get_thread_num () == 10)
      a = svld1_s32 (svptrue_b32 (), m);

#pragma omp barrier
    /* 'a', 'b' and 'p' are implicitly shared without default clause.  */
    compare_vec (a, svindex_s32 (0, 1));
    compare_vec (b, svindex_s32 (8, 1));
    compare_vecb (p, svptrue_b32 ());

#pragma omp barrier
    if (omp_get_thread_num () == 12)
      a = svadd_s32_z (p, a, b);

#pragma omp barrier
    if (omp_get_thread_num () == 15)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svindex_s32 (8, 1));
	compare_vecb (p, svptrue_b32 ());
	b = svadd_s32_z (p, a, b);
      }

#pragma omp barrier
    if (omp_get_thread_num () == 12 || omp_get_thread_num () == 15)
      {
	compare_vec (a, svindex_s32 (8, 2));
	compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));
      }

#pragma omp barrier
    compare_vec (a, svindex_s32 (8, 2));
    compare_vec (b, svadd_s32_z (p, svindex_s32 (8, 2), svindex_s32 (8, 1)));
  }
}

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__((arm_sve_vector_bits (N)))

typedef svint32_t v8si FIXED_ATTR;

void __attribute__ ((noipa))
predetermined_shared_static (int n)
{

  int *m = (int *) malloc (8 * sizeof (int));
  int i;

#pragma omp parallel for
  /* 'm' is predetermined shared here.  */
  for (i = 0; i < 8; i++)
    m[i] = i;

  static v8si a = { 0, 1, 2, 3, 4, 5, 6, 7 };

#pragma omp parallel num_threads (16)
  {
    /* 'a' is implicit shared here.  */
    if (n == 0)
      compare_vec (a, svindex_s32 (0, 1));

    if (n == 1)
      compare_vec (a, svindex_s32 (1, 1));

#pragma omp barrier
    if (omp_get_thread_num () == 12)
      {
	if (n == 0)
	  compare_vec (a, svindex_s32 (0, 1));

	if (n == 1)
	  compare_vec (a, svindex_s32 (1, 1));

	a = svadd_s32_z (svptrue_b32 (), a, svindex_s32 (1, 0));
      }

#pragma omp barrier
    if (n == 0)
      compare_vec (a, svindex_s32 (1, 1));

    if (n == 1)
      compare_vec (a, svindex_s32 (2, 1));
  }
}


int
main ()
{
  svint32_t x = svindex_s32 (0, 1);
  svint32_t y = svindex_s32 (8, 1);
  svbool_t p = svptrue_b32 ();

  /* Implicit shared.  */
  implicit_shared_default (x, y, p);

  /* Explicit shared.  */
  explicit_shared (x, y, p);

  /* Implicit shared with no default clause.  */
  implicit_shared_no_default (x, y, p);

  /* Mix shared.  */
  mix_shared (y, p);

  /* Predetermined and static shared.  */
  predetermined_shared_static (0);
  predetermined_shared_static (1);
}
