/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#pragma GCC target "+sve"

#include <arm_sve.h>
#include <stdint.h>

typedef __SVInt32_t v8si __attribute__ ((arm_sve_vector_bits(256)));

v8si vec1;
#pragma omp threadprivate (vec1)

void __attribute__ ((noipa))
foo ()
{
  int64_t res = 0;

  vec1 = svindex_s32 (1, 0);

#pragma omp parallel copyin (vec1) firstprivate (res) num_threads(10)
  {
    res = svaddv_s32 (svptrue_b32 (), vec1);

#pragma omp barrier
    if (res != 8LL)
      __builtin_abort ();
  }
}

int
main ()
{
  int64_t res = 0;

#pragma omp parallel firstprivate (res) num_threads(10)
  {
    vec1 = svindex_s32 (1, 0);
    res = svaddv_s32 (svptrue_b32 (), vec1);

#pragma omp barrier
    if (res != 8LL)
      __builtin_abort ();
  }

  foo ();
}
