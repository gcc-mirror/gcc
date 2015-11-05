extern void abort (void);

__attribute__((noinline, noclone)) void
foo (int *p, int *q, int *r, int n, int m)
{
  int i, err, *s = r;
  int sep = 1;
  #pragma omp target map(to:sep)
  sep = 0;
  #pragma omp target data map(to:p[0:8])
  {
    /* For zero length array sections, p points to the start of
       already mapped range, q to the end of it (with nothing mapped
       after it), and r does not point to an mapped range.  */
    #pragma omp target map(alloc:p[:0]) map(to:q[:0]) map(from:r[:0]) private(i) map(from:err) firstprivate (s)
    {
      err = 0;
      for (i = 0; i < 8; i++)
	if (p[i] != i + 1)
	  err = 1;
      if (sep)
	{
	  if (q != (int *) 0 || r != (int *) 0)
	    err = 1;
	}
      else if (p + 8 != q || r != s)
	err = 1;
    }
    if (err)
      abort ();
    /* Implicit mapping of pointers behaves the same way.  */
    #pragma omp target private(i) map(from:err) firstprivate (s)
    {
      err = 0;
      for (i = 0; i < 8; i++)
	if (p[i] != i + 1)
	  err = 1;
      if (sep)
	{
	  if (q != (int *) 0 || r != (int *) 0)
	    err = 1;
	}
      else if (p + 8 != q || r != s)
	err = 1;
    }
    if (err)
      abort ();
    /* And zero-length array sections, though not known at compile
       time, behave the same.  */
    #pragma omp target map(p[:n]) map(tofrom:q[:n]) map(alloc:r[:n]) private(i) map(from:err) firstprivate (s)
    {
      err = 0;
      for (i = 0; i < 8; i++)
	if (p[i] != i + 1)
	  err = 1;
      if (sep)
	{
	  if (q != (int *) 0 || r != (int *) 0)
	    err = 1;
	}
      else if (p + 8 != q || r != s)
	err = 1;
    }
    if (err)
      abort ();
    /* Non-zero length array sections, though not known at compile,
       behave differently.  */
    #pragma omp target map(p[:m]) map(tofrom:q[:m]) map(to:r[:m]) private(i) map(from:err)
    {
      err = 0;
      for (i = 0; i < 8; i++)
	if (p[i] != i + 1)
	  err = 1;
      if (q[0] != 9 || r[0] != 10)
	err = 1;
    }
    if (err)
      abort ();
    #pragma omp target data map(to:q[0:1])
    {
      /* For zero length array sections, p points to the start of
	 already mapped range, q points to the start of another one,
	 and r to the end of the second one.  */
      #pragma omp target map(to:p[:0]) map(from:q[:0]) map(tofrom:r[:0]) private(i) map(from:err)
      {
	err = 0;
	for (i = 0; i < 8; i++)
	  if (p[i] != i + 1)
	    err = 1;
	if (q[0] != 9)
	  err = 1;
	else if (sep)
	  {
	    if (r != (int *) 0)
	      err = 1;
	  }
	else if (r != q + 1)
	  err = 1;
      }
      if (err)
	abort ();
      /* Implicit mapping of pointers behaves the same way.  */
      #pragma omp target private(i) map(from:err)
      {
	err = 0;
	for (i = 0; i < 8; i++)
	  if (p[i] != i + 1)
	    err = 1;
	if (q[0] != 9)
	  err = 1;
	else if (sep)
	  {
	    if (r != (int *) 0)
	      err = 1;
	  }
	else if (r != q + 1)
	  err = 1;
      }
      if (err)
	abort ();
      /* And zero-length array sections, though not known at compile
	 time, behave the same.  */
      #pragma omp target map(p[:n]) map(alloc:q[:n]) map(from:r[:n]) private(i) map(from:err)
      {
	err = 0;
	for (i = 0; i < 8; i++)
	  if (p[i] != i + 1)
	    err = 1;
	if (q[0] != 9)
	  err = 1;
	else if (sep)
	  {
	    if (r != (int *) 0)
	      err = 1;
	  }
	else if (r != q + 1)
	  err = 1;
      }
      if (err)
	abort ();
      /* Non-zero length array sections, though not known at compile,
	 behave differently.  */
      #pragma omp target map(p[:m]) map(alloc:q[:m]) map(tofrom:r[:m]) private(i) map(from:err)
      {
	err = 0;
	for (i = 0; i < 8; i++)
	  if (p[i] != i + 1)
	    err = 1;
	if (q[0] != 9 || r[0] != 10)
	  err = 1;
      }
      if (err)
	abort ();
    }
  }
}

int
main ()
{
  int a[32], i;
  for (i = 0; i < 32; i++)
    a[i] = i;
  foo (a + 1, a + 9, a + 10, 0, 1);
  return 0;
}
