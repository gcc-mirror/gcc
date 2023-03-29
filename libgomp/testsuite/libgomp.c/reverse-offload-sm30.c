/* { dg-do link { target { offload_target_nvptx } } } */
/* { dg-additional-options "-foffload-options=nvptx-none=-march=sm_30 -foffload-options=nvptx-none=-mptx=_" } */

#pragma omp requires reverse_offload

int
main ()
{
  #pragma omp target
    {
    }
  return 0;
}

/* { dg-warning "'omp requires reverse_offload' requires at least 'sm_35' for '-foffload-options=nvptx-none=-march=' - disabling offload-code generation for this device type" "" { target *-*-* } 0 } */
