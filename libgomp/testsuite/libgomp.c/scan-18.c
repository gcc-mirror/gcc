/* { dg-require-effective-target size32plus } */
/* { dg-additional-options "-O2 -fopenmp -fdump-tree-vect-details" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump-times "vectorized \[2-6] loops" 2 "vect" { target sse2_runtime } } } */

extern void abort (void);
int r, a[1024], b[1024];
unsigned short r2, b2[1024];
unsigned char r3, b3[1024];

__attribute__((noipa)) void
foo (int *a, int *b, unsigned short *b2, unsigned char *b3)
{
  #pragma omp for simd reduction (inscan, +:r, r2, r3)
  for (int i = 0; i < 1024; i++)
    {
      {
	b[i] = r;
	b2[i] = r2;
	b3[i] = r3;
      }
      #pragma omp scan exclusive(r, r2, r3)
      { r += a[i]; r2 += a[i]; r3 += a[i]; }
    }
}

__attribute__((noipa)) int
bar (unsigned short *s2p, unsigned char *s3p)
{
  int s = 0;
  unsigned short s2 = 0;
  unsigned char s3 = 0;
  #pragma omp parallel
  #pragma omp for simd reduction (inscan, +:s, s2, s3)
  for (int i = 0; i < 1024; i++)
    {
      { b[i] = s; b2[i] = s2; b3[i] = s3; }
      #pragma omp scan exclusive(s, s2, s3)
      {
	s += 2 * a[i];
	s2 += 2 * a[i];
	s3 += 2 * a[i];
      }
    }
  *s2p = s2;
  *s3p = s3;
  return s;
}

__attribute__((noipa)) void
baz (int *a, int *b, unsigned short *b2, unsigned char *b3)
{
  #pragma omp parallel for simd reduction (inscan, +:r, r2, r3) if (simd: 0)
  for (int i = 0; i < 1024; i++)
    {
      {
	b[i] = r;
	b2[i] = r2;
	b3[i] = r3;
      }
      #pragma omp scan exclusive(r, r2, r3)
      {
	r += a[i];
	r2 += a[i];
	r3 += a[i];
      }
    }
}

__attribute__((noipa)) int
qux (unsigned short *s2p, unsigned char *s3p)
{
  int s = 0;
  unsigned short s2 = 0;
  unsigned char s3 = 0;
  #pragma omp parallel for simd simdlen (1) reduction (inscan, +:s, s2, s3)
  for (int i = 0; i < 1024; i++)
    {
      { b[i] = s; b2[i] = s2; b3[i] = s3; }
      #pragma omp scan exclusive(s, s2, s3)
      { s += 2 * a[i]; s2 += 2 * a[i]; s3 += 2 * a[i]; }
    }
  *s2p = s2;
  *s3p = s3;
  return s;
}

int
main ()
{
  int s = 0;
  unsigned short s2;
  unsigned char s3;
  for (int i = 0; i < 1024; ++i)
    {
      a[i] = i;
      b[i] = -1;
      b2[i] = -1;
      b3[i] = -1;
      asm ("" : "+g" (i));
    }
  #pragma omp parallel
  foo (a, b, b2, b3);
  if (r != 1024 * 1023 / 2
      || r2 != (unsigned short) r
      || r3 != (unsigned char) r)
    abort ();
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s
	  || b2[i] != (unsigned short) s
	  || b3[i] != (unsigned char) s)
	abort ();
      else
	{
	  b[i] = 25;
	  b2[i] = 24;
	  b3[i] = 26;
	}
      s += i;
    }
  if (bar (&s2, &s3) != 1024 * 1023)
    abort ();
  if (s2 != (unsigned short) (1024 * 1023)
      || s3 != (unsigned char) (1024 * 1023))
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s
	  || b2[i] != (unsigned short) s
	  || b3[i] != (unsigned char) s)
	abort ();
      else
	{
	  b[i] = -1;
	  b2[i] = -1;
	  b3[i] = -1;
	}
      s += 2 * i;
    }
  r = 0;
  r2 = 0;
  r3 = 0;
  baz (a, b, b2, b3);
  if (r != 1024 * 1023 / 2
      || r2 != (unsigned short) r
      || r3 != (unsigned char) r)
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s
	  || b2[i] != (unsigned short) s
	  || b3[i] != (unsigned char) s)
	abort ();
      else
	{
	  b[i] = 25;
	  b2[i] = 24;
	  b3[i] = 26;
	}
      s += i;
    }
  s2 = 0;
  s3 = 0;
  if (qux (&s2, &s3) != 1024 * 1023)
    abort ();
  if (s2 != (unsigned short) (1024 * 1023)
      || s3 != (unsigned char) (1024 * 1023))
    abort ();
  s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i] != s
	  || b2[i] != (unsigned short) s
	  || b3[i] != (unsigned char) s)
	abort ();
      s += 2 * i;
    }
  return 0;
}
