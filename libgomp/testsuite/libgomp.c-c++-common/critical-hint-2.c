/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */
#include <omp.h>

void
example_criticial ()
{
  int a, b;
  #pragma omp parallel for
  for (int i = 0; i < 10; ++i)
    {
      #pragma omp critical hint(omp_sync_hint_none)
      a += i;
      #pragma omp critical (HASH1) hint(omp_sync_hint_none)
      a += i;
      #pragma omp critical (HASH2) hint(omp_sync_hint_uncontended)
      a += i;
      #pragma omp critical (HASH3) hint(omp_sync_hint_contended)
      a += i;
      #pragma omp critical (HASH4) hint(omp_sync_hint_speculative)
      a += i;
      #pragma omp critical (HASH5) hint(omp_sync_hint_nonspeculative)
      a += i;
      #pragma omp critical (HASH6) hint(omp_sync_hint_contended + omp_sync_hint_speculative)
      a += i;
      #pragma omp critical (HASH7) hint(omp_sync_hint_contended | omp_sync_hint_speculative)
      a += i;
    }
}

/* { dg-final { scan-tree-dump-times "omp critical \\(HASH1\\) hint\\(0\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp critical \\(HASH2\\) hint\\(1\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp critical \\(HASH3\\) hint\\(2\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp critical \\(HASH4\\) hint\\(8\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp critical \\(HASH5\\) hint\\(4\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp critical \\(HASH6\\) hint\\(10\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp critical \\(HASH7\\) hint\\(10\\)" 1 "original" } } */
