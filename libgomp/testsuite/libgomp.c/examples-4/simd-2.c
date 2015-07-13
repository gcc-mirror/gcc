/* { dg-do run { target vect_simd_clones } } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 100
#define EPS 0.0000000000000001

#include <stdlib.h>

void init(double *a, double *a_ref, double *b, int n)
{
   int i;
   for ( i=0; i<N; i++ )
   {
      a[i] = i;
      a_ref[i] = i;
      b[i] = N-i;
   }
}

#pragma omp declare simd uniform(fact)
double add1(double a, double b, double fact)
{
   double c;
   c = a + b + fact;
   return c;
}

#pragma omp declare simd uniform(a,b,fact) linear(i:1)
double add2(double *a, double *b, int i, double fact)
{
   double c;
   c = a[i] + b[i] + fact;
   return c;
}

#pragma omp declare simd uniform(fact) linear(a,b:1)
double add3(double *a, double *b, double fact)
{
   double c;
   c = *a + *b + fact;
   return c;
}

void work( double *a, double *b, int n )
{
   int i;
   double tmp;
   #pragma omp simd private(tmp)
   for ( i = 0; i < n; i++ ) {
      tmp  = add1( a[i],  b[i], 1.0);
      a[i] = add2( a,     b, i, 1.0) + tmp;
      a[i] = add3(&a[i], &b[i], 1.0);
   }
}

void work_ref( double *a, double *b, int n )
{
   int i;
   double tmp;
   for ( i = 0; i < n; i++ ) {
      tmp  = add1( a[i],  b[i], 1.0);
      a[i] = add2( a,     b, i, 1.0) + tmp;
      a[i] = add3(&a[i], &b[i], 1.0);
   }
}

void check (double *a, double *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}


int main ()
{
   int i;
   double a[N], a_ref[N], b[N];

   init(a, a_ref, b, N);

   work(a, b, N );
   work_ref(a_ref, b, N );

   check(a, a_ref);

   return 0;
}
