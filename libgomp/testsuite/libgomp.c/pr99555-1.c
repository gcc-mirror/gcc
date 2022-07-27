// PR99555 "[OpenMP/nvptx] Execution-time hang for simple nested OpenMP 'target'/'parallel'/'task' constructs"

// { dg-additional-options "-O0" }

int main (void)
{
#pragma omp target
#pragma omp parallel // num_threads(1)
#pragma omp task
  ;

  return 0;
}
