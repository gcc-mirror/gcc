int
main ()
{
  int a, b, c, d;
  #pragma omp parallel num_threads (6)
  #pragma omp single
  {
    #pragma omp task depend(out: c)
      c = 1;
    #pragma omp task depend(out: a)
      a = 2;
    #pragma omp task depend(out: b)
      b = 3;
    /* The above 3 tasks can be scheduled in any order.  */
    #pragma omp task depend(in: a) depend(mutexinoutset: c)
      c += a;
    #pragma omp task depend(in: b) depend(mutexinoutset: c)
      c += b;
    /* The above 2 tasks are mutually exclusive and need to wait
       for the first and second or first and third tasks respectively.  */
    #pragma omp task depend(in: c)
      d = c;
    /* The above task needs to wait for the mutexinoutset tasks.  */
  }
  if (d != 6)
    __builtin_abort ();
  return 0;
}
