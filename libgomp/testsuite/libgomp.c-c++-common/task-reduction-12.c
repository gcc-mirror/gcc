extern
#ifdef __cplusplus
"C"
#endif
void abort (void);
int a, b[3] = { 1, 1, 1 };
unsigned long int c[2] = { ~0UL, ~0UL };

void
bar (int i)
{
  #pragma omp task in_reduction (*: b[:3]) in_reduction (&: c[1:]) \
	      in_reduction (+: a)
  {
    a += 4;
    b[1] *= 4;
    c[1] &= ~(1UL << (i + 16));
  }
}

void
foo (int x)
{
  #pragma omp sections reduction (task, +: a) reduction (task, *: b) \
		       reduction (task, &: c[1:1])
  {
    {
      a++; b[0] *= 2; bar (2); b[2] *= 3; c[1] &= ~(1UL << 2);
    }
    #pragma omp section
    { b[0] *= 2; bar (4); b[2] *= 3; c[1] &= ~(1UL << 4); a++; }
    #pragma omp section
    { bar (6); b[2] *= 3; c[1] &= ~(1UL << 6); a++; b[0] *= 2; }
    #pragma omp section
    { b[2] *= 3; c[1] &= ~(1UL << 8); a++; b[0] *= 2; bar (8); }
    #pragma omp section
    { c[1] &= ~(1UL << 10); a++; b[0] *= 2; bar (10); b[2] *= 3; }
    #pragma omp section
    { a++; b[0] *= 2; b[2] *= 3; c[1] &= ~(1UL << 12); bar (12); }
    #pragma omp section
    if (x)
      {
	a++; b[0] *= 2; b[2] *= 3; bar (14); c[1] &= ~(1UL << 14);
      }
  }
}

int
main ()
{
  volatile int one = 1;
  foo (!one);
  if (a != 30 || b[0] != 64 || b[1] != (1 << 12) || b[2] != 3 * 3 * 3 * 3 * 3 * 3
      || c[0] != ~0UL || c[1] != ~0x15541554UL)
    abort ();
  a = 0;
  b[0] = 1;
  b[1] = 1;
  b[2] = 1;
  c[1] = ~0UL;
  #pragma omp parallel
  foo (one);
  if (a != 35 || b[0] != 128 || b[1] != (1 << 14) || b[2] != 3 * 3 * 3 * 3 * 3 * 3 * 3
      || c[0] != ~0UL || c[1] != ~0x55545554UL)
    abort ();
  return 0;
}
