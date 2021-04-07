// PR99555 "[OpenMP/nvptx] Execution-time hang for simple nested OpenMP 'target'/'parallel'/'task' constructs"

// { dg-additional-options "-O0" }

#include <unistd.h> // For 'alarm'.

#include "../libgomp.c-c++-common/on_device_arch.h"

int main (void)
{
  if (on_device_arch_nvptx ())
    alarm (4); /*TODO Until resolved, make sure that we exit quickly, with error status.
		 { dg-xfail-run-if "PR99555" { offload_device_nvptx } } */

#pragma omp target
#pragma omp parallel // num_threads(1)
#pragma omp task
  ;

  return 0;
}
