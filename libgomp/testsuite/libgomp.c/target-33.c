extern void abort (void);

int
main ()
{
  int a = 1, b = 2, c = 4, d[7];
  #pragma omp taskgroup
  {
    #pragma omp target enter data nowait map (to: a, b, c) depend(out: d[0])
    #pragma omp target nowait map (alloc: a, b) depend(in: d[0]) depend(out: d[1])
    {
      #pragma omp atomic update
      a |= 4;
      #pragma omp atomic update
      b |= 8;
    }
    #pragma omp target nowait map (alloc: a, c) depend(in: d[0]) depend(out: d[2])
    {
      #pragma omp atomic update
      a |= 16;
      #pragma omp atomic update
      c |= 32;
    }
    #pragma omp target exit data nowait map (from: a, b, c) depend(in: d[1], d[2])
  }
  if (a != 21 || b != 10 || c != 36)
    abort ();
  #pragma omp target map (tofrom: a, b) nowait
  {
    a &= ~16;
    b &= ~2;
  }
  #pragma omp target map (tofrom: c) nowait
  {
    c |= 8;
  }
  #pragma omp barrier
  if (a != 5 || b != 8 || c != 44)
    abort ();
  #pragma omp target map (tofrom: a, b) nowait
  {
    a |= 32;
    b |= 4;
  }
  #pragma omp target map (tofrom: c) nowait
  {
    c &= ~4;
  }
  #pragma omp taskwait
  if (a != 37 || b != 12 || c != 40)
    abort ();
  #pragma omp target nowait map (tofrom: a, b) depend(out: d[3])
  {
    #pragma omp atomic update
    a = a + 9;
    b -= 8;
  }
  #pragma omp target nowait map (tofrom: a, c) depend(out: d[4])
  {
    #pragma omp atomic update
    a = a + 4;
    c >>= 1;
  }
  #pragma omp task if (0) depend (in: d[3], d[4]) shared (a, b, c)
  if (a != 50 || b != 4 || c != 20)
    abort ();
  #pragma omp task shared (a)
  a += 50;
  #pragma omp target nowait map (tofrom: b)
  b++;
  #pragma omp target map (tofrom: c) nowait
  c--;
  #pragma omp taskwait
  if (a != 100 || b != 5 || c != 19)
    abort ();
  #pragma omp target map (tofrom: a) nowait depend(out: d[5])
  a++;
  #pragma omp target map (tofrom: b) nowait depend(out: d[6])
  b++;
  #pragma omp target map (tofrom: a, b) depend(in: d[5], d[6])
  {
    if (a != 101 || b != 6)
      a = -9;
    else
      {
	a = 24;
	b = 38;
      }
  }
  if (a != 24 || b != 38)
    abort ();
  return 0;
}
