#include <omp.h>
#include <stdlib.h>

struct S { char p[64]; int a; int b[2]; long c[4]; int *d; char q[64]; };

__attribute__((noinline, noclone)) void
foo (struct S s)
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int sep = 1;

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  int err;
  #pragma omp target map(tofrom: s.a, s.b, s.c[1:2], s.d, s.d[-2:3]) map(to: sep) map(from: err)
  {
    err = s.a != 11 || s.b[0] != 12 || s.b[1] != 13;
    err |= s.c[1] != 15 || s.c[2] != 16 || s.d[-2] != 18 || s.d[-1] != 19 || s.d[0] != 20;
    s.a = 35; s.b[0] = 36; s.b[1] = 37;
    s.c[1] = 38; s.c[2] = 39; s.d[-2] = 40; s.d[-1] = 41; s.d[0] = 42;
    sep = 0;
  }
  if (err) abort ();
  err = s.a != 35 || s.b[0] != 36 || s.b[1] != 37;
  err |= s.c[1] != 38 || s.c[2] != 39 || s.d[-2] != 40 || s.d[-1] != 41 || s.d[0] != 42;
  if (err) abort ();
  s.a = 50; s.b[0] = 49; s.b[1] = 48;
  s.c[1] = 47; s.c[2] = 46; s.d[-2] = 45; s.d[-1] = 44; s.d[0] = 43;
  if (sep
      && (omp_target_is_present (&s.a, d)
	  || omp_target_is_present (s.b, d)
	  || omp_target_is_present (&s.c[1], d)
	  || omp_target_is_present (s.d, d)
	  || omp_target_is_present (&s.d[-2], d)))
    abort ();
  #pragma omp target data map(alloc: s.a, s.b, s.c[1:2], s.d, s.d[-2:3])
  {
    if (!omp_target_is_present (&s.a, d)
	|| !omp_target_is_present (s.b, d)
	|| !omp_target_is_present (&s.c[1], d)
	|| !omp_target_is_present (s.d, d)
	|| !omp_target_is_present (&s.d[-2], d))
      abort ();
    #pragma omp target update to(s.a, s.b, s.c[1:2], s.d, s.d[-2:3])
    #pragma omp target map(alloc: s.a, s.b, s.c[1:2], s.d, s.d[-2:3]) map(from: err)
    {
      err = s.a != 50 || s.b[0] != 49 || s.b[1] != 48;
      err |= s.c[1] != 47 || s.c[2] != 46 || s.d[-2] != 45 || s.d[-1] != 44 || s.d[0] != 43;
      s.a = 17; s.b[0] = 18; s.b[1] = 19;
      s.c[1] = 20; s.c[2] = 21; s.d[-2] = 22; s.d[-1] = 23; s.d[0] = 24;
    }
    #pragma omp target update from(s.a, s.b, s.c[1:2], s.d, s.d[-2:3])
  }
  if (sep
      && (omp_target_is_present (&s.a, d)
	  || omp_target_is_present (s.b, d)
	  || omp_target_is_present (&s.c[1], d)
	  || omp_target_is_present (s.d, d)
	  || omp_target_is_present (&s.d[-2], d)))
    abort ();
  if (err) abort ();
  err = s.a != 17 || s.b[0] != 18 || s.b[1] != 19;
  err |= s.c[1] != 20 || s.c[2] != 21 || s.d[-2] != 22 || s.d[-1] != 23 || s.d[0] != 24;
  if (err) abort ();
  s.a = 33; s.b[0] = 34; s.b[1] = 35;
  s.c[1] = 36; s.c[2] = 37; s.d[-2] = 38; s.d[-1] = 39; s.d[0] = 40;
  #pragma omp target enter data map(alloc: s.a, s.b, s.c[1:2], s.d, s.d[-2:3])
  if (!omp_target_is_present (&s.a, d)
      || !omp_target_is_present (s.b, d)
      || !omp_target_is_present (&s.c[1], d)
      || !omp_target_is_present (s.d, d)
      || !omp_target_is_present (&s.d[-2], d))
    abort ();
  #pragma omp target enter data map(always, to: s.a, s.b, s.c[1:2], s.d, s.d[-2:3])
  #pragma omp target map(alloc: s.a, s.b, s.c[1:2], s.d, s.d[-2:3]) map(from: err)
  {
    err = s.a != 33 || s.b[0] != 34 || s.b[1] != 35;
    err |= s.c[1] != 36 || s.c[2] != 37 || s.d[-2] != 38 || s.d[-1] != 39 || s.d[0] != 40;
    s.a = 49; s.b[0] = 48; s.b[1] = 47;
    s.c[1] = 46; s.c[2] = 45; s.d[-2] = 44; s.d[-1] = 43; s.d[0] = 42;
  }
  #pragma omp target exit data map(always, from: s.a, s.b, s.c[1:2], s.d, s.d[-2:3])
  if (!omp_target_is_present (&s.a, d)
      || !omp_target_is_present (s.b, d)
      || !omp_target_is_present (&s.c[1], d)
      || !omp_target_is_present (s.d, d)
      || !omp_target_is_present (&s.d[-2], d))
    abort ();
  #pragma omp target exit data map(release: s.a, s.b, s.c[1:2], s.d, s.d[-2:3])
  if (sep
      && (omp_target_is_present (&s.a, d)
	  || omp_target_is_present (s.b, d)
	  || omp_target_is_present (&s.c[1], d)
	  || omp_target_is_present (s.d, d)
	  || omp_target_is_present (&s.d[-2], d)))
    abort ();
  if (err) abort ();
  err = s.a != 49 || s.b[0] != 48 || s.b[1] != 47;
  err |= s.c[1] != 46 || s.c[2] != 45 || s.d[-2] != 44 || s.d[-1] != 43 || s.d[0] != 42;
  if (err) abort ();
}

int
main ()
{
  int d[3] = { 18, 19, 20 };
  struct S s = { {}, 11, { 12, 13 }, { 14, 15, 16, 17 }, d + 2, {} };
  foo (s);
  return 0;
}
