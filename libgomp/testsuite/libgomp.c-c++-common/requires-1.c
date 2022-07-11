/* { dg-do link { target offload_target_any } } */
/* { dg-additional-sources requires-1-aux.c } */

/* Check diagnostic by device-compiler's lto1.
   Other file uses: 'requires unified_address'.  */

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

/* { dg-error "OpenMP 'requires' directive with non-identical clauses in multiple compilation units: 'unified_shared_memory' vs. 'unified_address'" "" { target *-*-* } 0 }
     { dg-note {requires-1\.c' has 'unified_shared_memory'} {} { target *-*-* } 0 }
     { dg-note {requires-1-aux\.c' has 'unified_address'} {} { target *-*-* } 0 } */
/* { dg-excess-errors "Ignore messages like: errors during merging of translation units|mkoffload returned 1 exit status" } */
