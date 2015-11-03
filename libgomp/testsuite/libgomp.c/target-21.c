extern void abort (void);
union U { int x; long long y; };
struct T { int a; union U b; int c; };
struct S { int s; int u; struct T v; int x[10]; union U w; int y[10]; int z[10]; };
volatile int z;

int
main ()
{
  struct S s;
  s.s = 0;
  s.u = 1;
  s.v.a = 2;
  s.v.b.y = 3LL;
  s.v.c = 19;
  s.w.x = 4;
  s.x[0] = 7;
  s.x[1] = 8;
  s.y[3] = 9;
  s.y[4] = 10;
  s.y[5] = 11;
  int err = 0;
  #pragma omp target map (to:s.v.b, s.u, s.x[0:z + 2]) \
		     map (tofrom:s.y[3:3]) \
		     map (from: s.w, s.z[z + 1:z + 3], err)
  {
    err = 0;
    if (s.u != 1 || s.v.b.y != 3LL || s.x[0] != 7 || s.x[1] != 8
	|| s.y[3] != 9 || s.y[4] != 10 || s.y[5] != 11)
      err = 1;
    s.w.x = 6;
    s.y[3] = 12;
    s.y[4] = 13;
    s.y[5] = 14;
    s.z[1] = 15;
    s.z[2] = 16;
    s.z[3] = 17;
  }
  if (err || s.w.x != 6 || s.y[3] != 12 || s.y[4] != 13 || s.y[5] != 14
      || s.z[1] != 15 || s.z[2] != 16 || s.z[3] != 17)
    abort ();
  s.u++;
  s.v.a++;
  s.v.b.y++;
  s.w.x++;
  s.x[1] = 18;
  s.z[0] = 19;
  #pragma omp target data map (tofrom: s)
  #pragma omp target map (always to: s.w, s.x[1], err) map (alloc:s.u, s.v.b, s.z[z:z + 1])
  {
    err = 0;
    if (s.u != 2 || s.v.b.y != 4LL || s.w.x != 7 || s.x[1] != 18 || s.z[0] != 19)
      err = 1;
    s.w.x = 8;
    s.x[1] = 20;
    s.z[0] = 21;
  }
  if (err || s.w.x != 8 || s.x[1] != 20 || s.z[0] != 21)
    abort ();
  s.u++;
  s.v.a++;
  s.v.b.y++;
  s.w.x++;
  s.x[0] = 22;
  s.x[1] = 23;
  #pragma omp target data map (from: s.w, s.x[0:2]) map (to: s.v.b, s.u)
  #pragma omp target map (always to: s.w, s.x[0:2], err) map (alloc:s.u, s.v.b)
  {
    err = 0;
    if (s.u != 3 || s.v.b.y != 5LL || s.w.x != 9 || s.x[0] != 22 || s.x[1] != 23)
      err = 1;
    s.w.x = 11;
    s.x[0] = 24;
    s.x[1] = 25;
  }
  if (err || s.w.x != 11 || s.x[0] != 24 || s.x[1] != 25)
    abort ();
  return 0;
}
