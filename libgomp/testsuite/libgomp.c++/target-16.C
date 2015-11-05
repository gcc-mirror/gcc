#include <omp.h>
#include <stdlib.h>

template <typename C, typename I, typename L, typename UC, typename SH>
struct S { C p[64]; I a; I b[2]; L c[4]; I *d; UC &e; C (&f)[2]; SH (&g)[4]; I *&h; C q[64]; };

template <typename C, typename I, typename L, typename UC, typename SH>
__attribute__((noinline, noclone)) void
foo (S<C, I, L, UC, SH> s)
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int sep = 1;

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  int err;
  #pragma omp target map(tofrom: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3]) map(to: sep) map(from: err)
  {
    err = s.a != 11 || s.b[0] != 12 || s.b[1] != 13;
    err |= s.c[1] != 15 || s.c[2] != 16 || s.d[-2] != 18 || s.d[-1] != 19 || s.d[0] != 20;
    err |= s.e != 21 || s.f[0] != 22 || s.f[1] != 23 || s.g[1] != 25 || s.g[2] != 26;
    err |= s.h[2] != 31 || s.h[3] != 32 || s.h[4] != 33;
    s.a = 35; s.b[0] = 36; s.b[1] = 37;
    s.c[1] = 38; s.c[2] = 39; s.d[-2] = 40; s.d[-1] = 41; s.d[0] = 42;
    s.e = 43; s.f[0] = 44; s.f[1] = 45; s.g[1] = 46; s.g[2] = 47;
    s.h[2] = 48; s.h[3] = 49; s.h[4] = 50;
    sep = 0;
  }
  if (err) abort ();
  err = s.a != 35 || s.b[0] != 36 || s.b[1] != 37;
  err |= s.c[1] != 38 || s.c[2] != 39 || s.d[-2] != 40 || s.d[-1] != 41 || s.d[0] != 42;
  err |= s.e != 43 || s.f[0] != 44 || s.f[1] != 45 || s.g[1] != 46 || s.g[2] != 47;
  err |= s.h[2] != 48 || s.h[3] != 49 || s.h[4] != 50;
  if (err) abort ();
  s.a = 50; s.b[0] = 49; s.b[1] = 48;
  s.c[1] = 47; s.c[2] = 46; s.d[-2] = 45; s.d[-1] = 44; s.d[0] = 43;
  s.e = 42; s.f[0] = 41; s.f[1] = 40; s.g[1] = 39; s.g[2] = 38;
  s.h[2] = 37; s.h[3] = 36; s.h[4] = 35;
  if (sep
      && (omp_target_is_present (&s.a, d)
	  || omp_target_is_present (s.b, d)
	  || omp_target_is_present (&s.c[1], d)
	  || omp_target_is_present (s.d, d)
	  || omp_target_is_present (&s.d[-2], d)
	  || omp_target_is_present (&s.e, d)
	  || omp_target_is_present (s.f, d)
	  || omp_target_is_present (&s.g[1], d)
	  || omp_target_is_present (&s.h, d)
	  || omp_target_is_present (&s.h[2], d)))
    abort ();
  #pragma omp target data map(alloc: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3])
  {
    if (!omp_target_is_present (&s.a, d)
	|| !omp_target_is_present (s.b, d)
	|| !omp_target_is_present (&s.c[1], d)
	|| !omp_target_is_present (s.d, d)
	|| !omp_target_is_present (&s.d[-2], d)
	|| !omp_target_is_present (&s.e, d)
	|| !omp_target_is_present (s.f, d)
	|| !omp_target_is_present (&s.g[1], d)
	|| !omp_target_is_present (&s.h, d)
	|| !omp_target_is_present (&s.h[2], d))
      abort ();
    #pragma omp target update to(s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3])
    #pragma omp target map(alloc: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3]) map(from: err)
    {
      err = s.a != 50 || s.b[0] != 49 || s.b[1] != 48;
      err |= s.c[1] != 47 || s.c[2] != 46 || s.d[-2] != 45 || s.d[-1] != 44 || s.d[0] != 43;
      err |= s.e != 42 || s.f[0] != 41 || s.f[1] != 40 || s.g[1] != 39 || s.g[2] != 38;
      err |= s.h[2] != 37 || s.h[3] != 36 || s.h[4] != 35;
      s.a = 17; s.b[0] = 18; s.b[1] = 19;
      s.c[1] = 20; s.c[2] = 21; s.d[-2] = 22; s.d[-1] = 23; s.d[0] = 24;
      s.e = 25; s.f[0] = 26; s.f[1] = 27; s.g[1] = 28; s.g[2] = 29;
      s.h[2] = 30; s.h[3] = 31; s.h[4] = 32;
    }
    #pragma omp target update from(s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3])
  }
  if (sep
      && (omp_target_is_present (&s.a, d)
	  || omp_target_is_present (s.b, d)
	  || omp_target_is_present (&s.c[1], d)
	  || omp_target_is_present (s.d, d)
	  || omp_target_is_present (&s.d[-2], d)
	  || omp_target_is_present (&s.e, d)
	  || omp_target_is_present (s.f, d)
	  || omp_target_is_present (&s.g[1], d)
	  || omp_target_is_present (&s.h, d)
	  || omp_target_is_present (&s.h[2], d)))
    abort ();
  if (err) abort ();
  err = s.a != 17 || s.b[0] != 18 || s.b[1] != 19;
  err |= s.c[1] != 20 || s.c[2] != 21 || s.d[-2] != 22 || s.d[-1] != 23 || s.d[0] != 24;
  err |= s.e != 25 || s.f[0] != 26 || s.f[1] != 27 || s.g[1] != 28 || s.g[2] != 29;
  err |= s.h[2] != 30 || s.h[3] != 31 || s.h[4] != 32;
  if (err) abort ();
  s.a = 33; s.b[0] = 34; s.b[1] = 35;
  s.c[1] = 36; s.c[2] = 37; s.d[-2] = 38; s.d[-1] = 39; s.d[0] = 40;
  s.e = 41; s.f[0] = 42; s.f[1] = 43; s.g[1] = 44; s.g[2] = 45;
  s.h[2] = 46; s.h[3] = 47; s.h[4] = 48;
  #pragma omp target enter data map(alloc: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3])
  if (!omp_target_is_present (&s.a, d)
      || !omp_target_is_present (s.b, d)
      || !omp_target_is_present (&s.c[1], d)
      || !omp_target_is_present (s.d, d)
      || !omp_target_is_present (&s.d[-2], d)
      || !omp_target_is_present (&s.e, d)
      || !omp_target_is_present (s.f, d)
      || !omp_target_is_present (&s.g[1], d)
      || !omp_target_is_present (&s.h, d)
      || !omp_target_is_present (&s.h[2], d))
    abort ();
  #pragma omp target enter data map(always, to: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3])
  #pragma omp target map(alloc: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3]) map(from: err)
  {
    err = s.a != 33 || s.b[0] != 34 || s.b[1] != 35;
    err |= s.c[1] != 36 || s.c[2] != 37 || s.d[-2] != 38 || s.d[-1] != 39 || s.d[0] != 40;
    err |= s.e != 41 || s.f[0] != 42 || s.f[1] != 43 || s.g[1] != 44 || s.g[2] != 45;
    err |= s.h[2] != 46 || s.h[3] != 47 || s.h[4] != 48;
    s.a = 49; s.b[0] = 48; s.b[1] = 47;
    s.c[1] = 46; s.c[2] = 45; s.d[-2] = 44; s.d[-1] = 43; s.d[0] = 42;
    s.e = 31; s.f[0] = 40; s.f[1] = 39; s.g[1] = 38; s.g[2] = 37;
    s.h[2] = 36; s.h[3] = 35; s.h[4] = 34;
  }
  #pragma omp target exit data map(always, from: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3])
  if (!omp_target_is_present (&s.a, d)
      || !omp_target_is_present (s.b, d)
      || !omp_target_is_present (&s.c[1], d)
      || !omp_target_is_present (s.d, d)
      || !omp_target_is_present (&s.d[-2], d)
      || !omp_target_is_present (&s.e, d)
      || !omp_target_is_present (s.f, d)
      || !omp_target_is_present (&s.g[1], d)
      || !omp_target_is_present (&s.h, d)
      || !omp_target_is_present (&s.h[2], d))
    abort ();
  #pragma omp target exit data map(release: s.a, s.b, s.c[1:2], s.d[-2:3], s.e, s.f, s.g[1:2], s.h[2:3])
  if (sep
      && (omp_target_is_present (&s.a, d)
	  || omp_target_is_present (s.b, d)
	  || omp_target_is_present (&s.c[1], d)
	  || omp_target_is_present (s.d, d)
	  || omp_target_is_present (&s.d[-2], d)
	  || omp_target_is_present (&s.e, d)
	  || omp_target_is_present (s.f, d)
	  || omp_target_is_present (&s.g[1], d)
	  || omp_target_is_present (&s.h, d)
	  || omp_target_is_present (&s.h[2], d)))
    abort ();
  if (err) abort ();
  err = s.a != 49 || s.b[0] != 48 || s.b[1] != 47;
  err |= s.c[1] != 46 || s.c[2] != 45 || s.d[-2] != 44 || s.d[-1] != 43 || s.d[0] != 42;
  err |= s.e != 31 || s.f[0] != 40 || s.f[1] != 39 || s.g[1] != 38 || s.g[2] != 37;
  err |= s.h[2] != 36 || s.h[3] != 35 || s.h[4] != 34;
  if (err) abort ();
}

int
main ()
{
  int d[3] = { 18, 19, 20 };
  unsigned char e = 21;
  char f[2] = { 22, 23 };
  short g[4] = { 24, 25, 26, 27 };
  int hb[7] = { 28, 29, 30, 31, 32, 33, 34 };
  int *h = hb + 1;
  S<char, int, long, unsigned char, short> s = { {}, 11, { 12, 13 }, { 14, 15, 16, 17 }, d + 2, e, f, g, h, {} };
  foo (s);
}
