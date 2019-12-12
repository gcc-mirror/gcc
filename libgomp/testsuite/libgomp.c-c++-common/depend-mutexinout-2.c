int
main ()
{
  int a, b, c = 0;
  #pragma omp parallel num_threads(2)
  {
    #pragma omp barrier
    #pragma omp single
    {
      #pragma omp task depend(out: a)
      {
	int i;
	a = 0;
	for (i = 0; i < 524288; ++i)
	  {
	    asm volatile ("" : "+g" (a));
	    a++;
	  }
      }
      #pragma omp task depend(out: b)
      {
	int i;
	b = 0;
	for (i = 0; i < 64; ++i)
	  {
	    asm volatile ("" : "+g" (b));
	    b++;
	  }
      }
      #pragma omp task depend(in: a) depend(mutexinoutset: c)
      {
	int i;
	int d = c;
	for (i = 0; i < 524288 + 64 - a; ++i)
	  {
	    asm volatile ("" : "+g" (d) : "g" (&a) : "memory");
	    d++;
	  }
	asm volatile ("" : "+g" (d), "+g" (c));
	c = d;
      }
      #pragma omp task depend(in: b) depend(mutexinoutset: c)
      {
	int i;
	int d = c;
	for (i = 0; i < 524288 + 64 - b; ++i)
	  {
	    asm volatile ("" : "+g" (d) : "g" (&b) : "memory");
	    d++;
	  }
	asm volatile ("" : "+g" (d), "+g" (c));
	c = d;
      }
    }
  }
  if (c != 524288 + 64)
    __builtin_abort ();
  return 0;
}
