/* { dg-do run } */
/* { dg-options "-O0" } */

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);
volatile int v;
#pragma omp declare target to (v)
typedef void (*fnp1) (void);
typedef fnp1 (*fnp2) (void);
void f1 (void) { v++; }
void f2 (void) { v += 4; }
void f3 (void) { v += 16; f1 (); }
fnp1 f4 (void) { v += 64; return f2; }
int a = 1;
int *b = &a;
int **c = &b;
fnp2 f5 (void) { f3 (); return f4; }
#pragma omp declare target to (c)

int
main ()
{
  int err = 0;
  #pragma omp target map(from:err)
  {
    volatile int xa;
    int *volatile xb;
    int **volatile xc;
    fnp2 xd;
    fnp1 xe;
    err = 0;
    xa = a;
    err |= xa != 1;
    xb = b;
    err |= xb != &a;
    xc = c;
    err |= xc != &b;
    xd = f5 ();
    err |= v != 17;
    xe = xd ();
    err |= v != 81;
    xe ();
    err |= v != 85;
  }
  if (err)
    abort ();
  return 0;
}
