/* { dg-do run { target { offload_target_nvptx || offload_target_amdgcn } } } */
/* { dg-additional-sources requires-5-aux.c } */

#pragma omp requires unified_shared_memory, unified_address, reverse_offload

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

/* (Only) if GOMP_DEBUG=1, should print at runtime the following:
   "devices present but 'omp requires unified_address, unified_shared_memory, reverse_offload' cannot be fulfilled" */
