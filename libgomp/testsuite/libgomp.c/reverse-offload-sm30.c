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

/* The 'mkoffload's currently don't obey '-fno-diagnostics-color' etc., so use a different way to effect the same thing:
   { dg-set-compiler-env-var GCC_COLORS "" }
   ..., so that the following regexp doesn't have to deal with color code escape sequences.  */
/* { dg-warning "'omp requires reverse_offload' requires at least 'sm_35' for '-foffload-options=nvptx-none=-march=' - disabling offload-code generation for this device type" "" { target *-*-* } 0 } */
