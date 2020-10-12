/* { dg-do compile } */

#include <omp.h>

void
example_criticial ()
{
  int a, b;
  #pragma omp parallel for
  for (int i = 0; i < 10; ++i)
    {
      #pragma omp critical hint(omp_sync_hint_none)  /* OK */
      a += i;
      #pragma omp critical (HASH) hint(omp_sync_hint_none)  /* OK */
      a += i;
      #pragma omp critical (HASH2) hint(omp_sync_hint_uncontended)  /* OK */
      a += i;
      #pragma omp critical (HASH3) hint(omp_sync_hint_contended)  /* OK */
      a += i;
      #pragma omp critical (HASH4) hint(omp_sync_hint_speculative)  /* OK */
      a += i;
      #pragma omp critical (HASH5) hint(omp_sync_hint_nonspeculative)  /* OK */
      a += i;
      #pragma omp critical (HASH6) hint(omp_sync_hint_contended + omp_sync_hint_speculative)  /* OK */
      a += i;
      #pragma omp critical (HASH6) hint(omp_sync_hint_contended | omp_sync_hint_speculative)  /* OK */
      a += i;

      /* Accepted but invalid: different hint for same name. */
      #pragma omp critical (HASH6) hint(omp_sync_hint_uncontended + omp_sync_hint_speculative)  
      a += i;
      /* Accepted but invalid: Some random integer expr. */
      #pragma omp critical (HASH) hint(omp_sync_hint_speculative + 1 + 2)
      a += i;

      #pragma omp critical (HASH) hint(-3)  /* { dg-error "expected constant integer expression" } */
      a += i;
      #pragma omp critical (HASH2) hint(b)  /* { dg-error "constant integer expression" } */
      a += i;
/*
  Fails with gcc as 'expected identifier' and
        with g++ as "clause requires a name, except when 'omp_sync_hint_none'"
      #pragma omp critical () hint(omp_sync_hint_speculative)
      a += i;
*/
      #pragma omp critical hint(omp_sync_hint_speculative)  /* { dg-error "with 'hint' clause requires a name, except when 'omp_sync_hint_none' is used" } */
      a += i;
    }
}
