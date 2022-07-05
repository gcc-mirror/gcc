/* { dg-do link { target offloading_enabled } } */
/* { dg-additional-options "-flto" } */
/* { dg-additional-sources requires-4-aux.c } */

/* Check diagnostic by device-compiler's or host compiler's lto1.
   Other file uses: 'requires reverse_offload', but that's inactive as
   there are no declare target directives, device constructs nor device routines  */

#pragma omp requires unified_address,unified_shared_memory

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
