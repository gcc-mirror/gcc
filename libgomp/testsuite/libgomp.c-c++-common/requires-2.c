/* { dg-do link { target offloading_enabled } } */
/* { dg-additional-options "-foffload=disable -flto" } */
/* { dg-additional-sources requires-2-aux.c } */

/* Check diagnostic by host's lto1.
   Other file does not have any 'omp requires'. */

#pragma omp requires unified_shared_memory

int a[10];
extern void foo (void);

int
main (void)
{
  #pragma omp target
  for (int i = 0; i < 10; i++)
    a[i] = 0;

  foo ();
  return 0;
}

/* { dg-error "OpenMP 'requires' directive with 'unified_shared_memory' specified only in some compilation units" "" { target *-*-* } 0 }
     { dg-note {requires-2\.c' has 'unified_shared_memory'} {} { target *-*-* } 0 }
     { dg-note {but '[^']*requires-2-aux\.c' has not} {} { target *-*-* } 0 } */
/* { dg-excess-errors "Ignore messages like: errors during merging of translation units|mkoffload returned 1 exit status" } */
