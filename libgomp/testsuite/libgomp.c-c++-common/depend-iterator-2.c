volatile int v;

__attribute__((noipa)) void
foo (int *p, int i)
{
  #pragma omp task depend (out: p[0])
  v++;
  #pragma omp task depend (in: p[0])
  v++;
  #pragma omp task depend (inout: p[0])
  v++;
  #pragma omp task depend (mutexinoutset: p[0])
  v++;
  #pragma omp task depend (out: p[0]) depend (in: p[1])
  v++;
  #pragma omp task depend (in: p[0]) depend (inout: p[1])
  v++;
  #pragma omp task depend (inout: p[0]) depend (mutexinoutset: p[1])
  v++;
  #pragma omp task depend (mutexinoutset: p[0]) depend (out: p[1])
  v++;
  #pragma omp task depend (iterator (j=0:2) , out : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:2) , in : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:2) , inout : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:2) , mutexinoutset : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:2) , out : p[j]) depend (iterator (j=0:2) , in : p[j + 2])
  v++;
  #pragma omp task depend (iterator (j=0:2) , in : p[j]) depend (iterator (j=0:2) , inout : p[j + 2])
  v++;
  #pragma omp task depend (iterator (j=0:2) , inout : p[j]) depend (iterator (j=0:2) , mutexinoutset : p[j + 2])
  v++;
  #pragma omp task depend (iterator (j=0:2) , mutexinoutset : p[j]) depend (iterator (j=0:2) , out : p[j + 2])
  v++;
  #pragma omp task depend (iterator (j=0:i) , out : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:i) , in : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:i) , inout : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:i) , mutexinoutset : p[j])
  v++;
  #pragma omp task depend (iterator (j=0:i) , out : p[j]) depend (iterator (j=0:i) , in : p[j + 2])
  v++;
  #pragma omp task depend (iterator (j=0:i) , in : p[j]) depend (iterator (j=0:i) , inout : p[j + 2])
  v++;
  #pragma omp task depend (iterator (j=0:i) , inout : p[j]) depend (iterator (j=0:i) , mutexinoutset : p[j + 2])
  v++;
  #pragma omp task depend (iterator (j=0:i) , mutexinoutset : p[j]) depend (iterator (j=0:i) , out : p[j + 2])
  v++;
}

int
main ()
{
  int p[4];
  foo (p, 2);
  foo (p, -1);
  return 0;
}
