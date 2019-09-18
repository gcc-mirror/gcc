/* { dg-require-effective-target size32plus } */
/* { dg-additional-options "-O2 -fopenmp -fdump-tree-vect-details" } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-final { scan-tree-dump-times "vectorized \[2-6] loops" 2 "vect" { target sse2_runtime } } } */

extern void abort (void);
float r = 1.0f, a[1024], b[1024];

__attribute__((noipa)) void
foo (float *a, float *b)
{
  #pragma omp for simd reduction (inscan, *:r)
  for (int i = 0; i < 1024; i++)
    {
      r *= a[i];
      #pragma omp scan inclusive(r)
      b[i] = r;
    }
}

__attribute__((noipa)) float
bar (void)
{
  float s = -__builtin_inff ();
  #pragma omp parallel for simd reduction (inscan, max:s)
  for (int i = 0; i < 1024; i++)
    {
      s = s > a[i] ? s : a[i];
      #pragma omp scan inclusive(s)
      b[i] = s;
    }
  return s;
}

int
main ()
{
  float s = 1.0f;
  for (int i = 0; i < 1024; ++i)
    {
      if (i < 80)
	a[i] = (i & 1) ? 0.25f : 0.5f;
      else if (i < 200)
	a[i] = (i % 3) == 0 ? 2.0f : (i % 3) == 1 ? 4.0f : 1.0f;
      else if (i < 280)
	a[i] = (i & 1) ? 0.25f : 0.5f;
      else if (i < 380)
	a[i] = (i % 3) == 0 ? 2.0f : (i % 3) == 1 ? 4.0f : 1.0f;
      else
	switch (i % 6)
	  {
	  case 0: a[i] = 0.25f; break;
	  case 1: a[i] = 2.0f; break;
	  case 2: a[i] = -1.0f; break;
	  case 3: a[i] = -4.0f; break;
	  case 4: a[i] = 0.5f; break;
	  case 5: a[i] = 1.0f; break;
	  default: a[i] = 0.0f; break;
	  }
      b[i] = -19.0f;
      asm ("" : "+g" (i));
    }
  #pragma omp parallel
  foo (a, b);
  if (r * 16384.0f != 0.125f)
    abort ();
  float m = -175.25f;
  for (int i = 0; i < 1024; ++i)
    {
      s *= a[i];
      if (b[i] != s)
	abort ();
      else
	{
	  a[i] = m - ((i % 3) == 1 ? 2.0f : (i % 3) == 2 ? 4.0f : 0.0f);
	  b[i] = -231.75f;
	  m += 0.75f;
	}
    }
  if (bar () != 592.0f)
    abort ();
  s = -__builtin_inff ();
  for (int i = 0; i < 1024; ++i)
    {
      if (s < a[i])
	s = a[i];
      if (b[i] != s)
	abort ();
    }
  return 0;
}
