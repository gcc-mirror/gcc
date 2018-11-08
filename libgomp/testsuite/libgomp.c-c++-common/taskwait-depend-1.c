#ifdef __cplusplus
extern "C"
#endif
void abort (void);

int
main ()
{
  int a, b, c, d;
  #pragma omp parallel num_threads (4)
  #pragma omp single
  {
    #pragma omp task depend(out : a)
    a = 6;
    #pragma omp task depend(out : b)
    b = 7;
    #pragma omp task depend(out : c)
    c = 8;
    #pragma omp taskwait depend(in : a, c)
    d = a + c;
    #pragma omp task depend(out : a)
    a = 9;
    #pragma omp task depend(out : c)
    c = 10;
  }
  if (a != 9 || b != 7 || c != 10 || d != 6 + 8)
    abort ();
  return 0;
}
