/* { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } } */
/* { dg-additional-sources requires-5-aux.c } */

/* Depending on offload device capabilities, it may print something like the
   following (only) if GOMP_DEBUG=1:
   "devices present but 'omp requires unified_address, unified_shared_memory, reverse_offload' cannot be fulfilled"
   and in that case does host-fallback execution.

   As no offload devices support USM at present, we may verify host-fallback
   execution by absence of separate memory spaces.  */

#pragma omp requires unified_shared_memory, unified_address, reverse_offload

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
