/* { dg-additional-options "-foffload-options=nvptx-none=-misa=sm_35" { target { offload_target_nvptx } } } */

#pragma omp requires unified_shared_memory, unified_address, reverse_offload

/* The requires line is not active as there is none of:
     declare target directives, device constructs or device routines.
   Thus, this code is expected to work everywhere.  */

int a[10];
extern void foo (void);

int
main (void)
{
  for (int i = 0; i < 10; i++)
    a[i] = 0;

  return 0;
}
