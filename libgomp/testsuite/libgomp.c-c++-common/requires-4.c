/* { dg-additional-options "-flto" } */
/* { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } } */
/* { dg-additional-sources requires-4-aux.c } */

/* Check no diagnostic by device-compiler's or host compiler's lto1.
   Other file uses: 'requires reverse_offload', but that's inactive as
   there are no declare target directives, device constructs nor device routines  */

/* Depending on offload device capabilities, it may print something like the
   following (only) if GOMP_DEBUG=1:
   "devices present but 'omp requires unified_address, unified_shared_memory, reverse_offload' cannot be fulfilled"
   and in that case does host-fallback execution.

   No offload devices support USM at present, so we may verify host-fallback
   execution by presence of separate memory spaces.  */

#pragma omp requires unified_address,unified_shared_memory

int a[10] = { 0 };
extern void foo (void);

int
main (void)
{
  #pragma omp target map(to: a)
  for (int i = 0; i < 10; i++)
    a[i] = i;

  for (int i = 0; i < 10; i++)
    if (a[i] != i)
      __builtin_abort ();

  foo ();
  return 0;
}
