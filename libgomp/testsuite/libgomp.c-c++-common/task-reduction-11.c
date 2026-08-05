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
foo (unsigned long long int x, unsigned long long int y, unsigned long long int z)
{
  unsigned long long int i;
  #pragma omp for schedule(runtime) reduction (task, +: a) \
		  reduction (task, *: b) reduction (task, &: c[1:1])
  for (i = x; i < y; i += z)
    {
      a++;
      b[0] *= 2;
      bar (i);
      b[2] *= 3;
      c[1] &= ~(1UL << i);
    }
}

int
main ()
{
  volatile int two = 2;
  foo (two, 7 * two, two);
  if (a != 30 || b[0] != 64 || b[1] != (1 << 12) || b[2] != 3 * 3 * 3 * 3 * 3 * 3
      || c[0] != ~0UL || c[1] != ~0x15541554UL)
    abort ();
  a = 0;
  b[0] = 1;
  b[1] = 1;
  b[2] = 1;
  c[1] = ~0UL;
  #pragma omp parallel
  foo (two, 8 * two, two);
  if (a != 35 || b[0] != 128 || b[1] != (1 << 14) || b[2] != 3 * 3 * 3 * 3 * 3 * 3 * 3
      || c[0] != ~0UL || c[1] != ~0x55545554UL)
    abort ();
  return 0;
}
