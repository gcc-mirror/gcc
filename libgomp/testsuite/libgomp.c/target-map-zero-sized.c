/* { dg-do run } */
/* { dg-additional-options "-O0" }  */

/* Issue showed up in the real world when large data was distributed
   over multiple MPI progresses - such that for one process n == 0
   happend at run time.

   Before  map(var[:0])  and  map(var[:n])  with n > 0 was handled,
   this patch now also handles  map(var[:n]) with n == 0.

   Failed before with "libgomp: pointer target not mapped for attach".  */

/* Here, the base address is shifted - which should have no effect,
   but must work as well.  */
void
with_offset ()
{
  struct S {
     int *ptr1, *ptr2;
  };
  struct S s1, s2;
  int *a, *b, *c, *d;
  s1.ptr1 = (int *) 0L;
  s1.ptr2 = (int *) 0xdeedbeef;
  s2.ptr1 = (int *) 0L;
  s2.ptr2 = (int *) 0xdeedbeef;
  a = (int *) 0L;
  b = (int *) 0xdeedbeef;
  c = (int *) 0L;
  d = (int *) 0xdeedbeef;

  int n1, n2, n3, n4;
  n1 = n2 = n3 = n4 = 0;

  #pragma omp target enter data map(s1.ptr1[4:n1], s1.ptr2[6:n2], a[3:n3], b[2:n4])

  #pragma omp target map(s2.ptr1[4:n1], s2.ptr2[2:n2], c[6:n3], d[9:n4])
  {
    if (s2.ptr1 != (void *) 0L || s2.ptr2 != (void *) 0xdeedbeef
	|| c != (void *) 0L || d != (void *) 0xdeedbeef)
      __builtin_abort ();
  }

  #pragma omp target map(s1.ptr1[4:n1], s1.ptr2[6:n2], a[3:n3], b[2:n4])
  {
    if (s1.ptr1 != (void *) 0L || s1.ptr2 != (void *) 0xdeedbeef
	|| a != (void *) 0L || b != (void *) 0xdeedbeef)
      __builtin_abort ();
  }

  #pragma omp target
  {
    if (s1.ptr1 != (void *) 0L || s1.ptr2 != (void *) 0xdeedbeef
	|| a != (void *) 0L || b != (void *) 0xdeedbeef)
      __builtin_abort ();
  }

  #pragma omp target exit data map(s1.ptr1[4:n1], s1.ptr2[6:n2], a[3:n3], b[2:n4])
}

int
main ()
{
  struct S {
     int *ptr1, *ptr2;
  };
  struct S s1, s2;
  int *a, *b, *c, *d;
  s1.ptr1 = (int *) 0L;
  s1.ptr2 = (int *) 0xdeedbeef;
  s2.ptr1 = (int *) 0L;
  s2.ptr2 = (int *) 0xdeedbeef;
  a = (int *) 0L;
  b = (int *) 0xdeedbeef;
  c = (int *) 0L;
  d = (int *) 0xdeedbeef;

  int n1, n2, n3, n4;
  n1 = n2 = n3 = n4 = 0;

  #pragma omp target enter data map(s1.ptr1[:n1], s1.ptr2[:n2], a[:n3], b[:n4])

  #pragma omp target map(s2.ptr1[:n1], s2.ptr2[:n2], c[:n3], d[:n4])
  {
    if (s2.ptr1 != (void *) 0L || s2.ptr2 != (void *) 0xdeedbeef
	|| c != (void *) 0L || d != (void *) 0xdeedbeef)
      __builtin_abort ();
  }

  #pragma omp target map(s1.ptr1[:n1], s1.ptr2[:n2], a[:n3], b[:n4])
  {
    if (s1.ptr1 != (void *) 0L || s1.ptr2 != (void *) 0xdeedbeef
	|| a != (void *) 0L || b != (void *) 0xdeedbeef)
      __builtin_abort ();
  }

  #pragma omp target
  {
    if (s1.ptr1 != (void *) 0L || s1.ptr2 != (void *) 0xdeedbeef
	|| a != (void *) 0L || b != (void *) 0xdeedbeef)
      __builtin_abort ();
  }

  #pragma omp target exit data map(s1.ptr1[:n1], s1.ptr2[:n2], a[:n3], b[:n4])

  with_offset ();
}
