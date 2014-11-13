// { dg-do run }

#include <omp.h>

#define EPS 0.000001
#define N 1000

extern "C" void abort (void);

void init (float *a1, float *a2, int n)
{
  int s = -1;
  for (int i = 0; i < n; i++)
    {
      a1[i] = s * 0.01;
      a2[i] = i;
      s = -s;
    }
}

void check (float *a, float *b, int n)
{
  for (int i = 0; i < n; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}

void vec_mult_ref (float *&p, float *&v1, float *&v2, int n)
{
  for (int i = 0; i < n; i++)
    p[i] = v1[i] * v2[i];
}

void vec_mult (float *&p, float *&v1, float *&v2, int n)
{
  #pragma omp target map(to: v1[0:n], v2[:n]) map(from: p[0:n])
    #pragma omp parallel for
      for (int i = 0; i < n; i++)
	p[i] = v1[i] * v2[i];
}

int main ()
{
  float *p = new float [N];
  float *p1 = new float [N];
  float *v1 = new float [N];
  float *v2 = new float [N];

  init (v1, v2, N);

  vec_mult_ref (p, v1, v2, N);
  vec_mult (p1, v1, v2, N);

  check (p, p1, N);

  delete [] p;
  delete [] p1;
  delete [] v1;
  delete [] v2;

  return 0;
}
