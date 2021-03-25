// PR99555 "[OpenMP/nvptx] Execution-time hang for simple nested OpenMP 'target'/'parallel'/'task' constructs"

// { dg-additional-options "-O0" }

// { dg-additional-sources "../lib/on_device_arch.c" }
extern int on_device_arch_nvptx ();

int main (void)
{
  if (on_device_arch_nvptx ())
    __builtin_abort (); //TODO Until resolved, skip, with error status.

#pragma omp target
#pragma omp parallel // num_threads(1)
#pragma omp task
  ;

  return 0;
}
