/* { dg-do run { target vect_simd_clones } } */
/* { dg-additional-options "-msse2" { target sse2_runtime } } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#define N 100
#define EPS 0.000001

#include <stdlib.h>
#include <stdio.h>

void init(int *b, float *y, int n)
{
   int i, s = -1;
   for ( i=0; i<N; i++ )
   {
      b[i] = i*i*s;
      y[i] = b[i] * 0.1f;
      s = -s;
   }
}

#pragma omp declare simd linear(p:1) notinbranch
int foo(int *p){
  *p = *p + 10;
  return *p;
}

int myaddint(int *a, int *b, int n)
{
#pragma omp simd
  for (int i=0; i<n; i++){
      a[i]  = foo(&b[i]);  /* foo is not called under a condition */
  }
  return a[n-1];
}

int myaddint_ref(int *a, int *b, int n)
{
  for (int i=0; i<n; i++){
      a[i]  = foo(&b[i]);
  }
  return a[n-1];
}

#pragma omp declare simd linear(p:1) inbranch
float goo(float *p){
  *p = *p + 18.5f;
  return *p;
}

int myaddfloat(float *x, float *y, int n)
{
#pragma omp simd
  for (int i=0; i<n; i++){
     x[i] = (x[i] > y[i]) ? goo(&y[i]) : y[i];
       /* goo is called under the condition (or within a branch) */
  }
  return x[n-1];
}

int myaddfloat_ref(float *x, float *y, int n)
{
  for (int i=0; i<n; i++){
     x[i] = (x[i] > y[i]) ? goo(&y[i]) : y[i];
  }
  return x[n-1];
}

void check_addint (int *a, int *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] != b[i])
      abort ();
}

void check_addfloat (float *a, float *b)
{
  int i;
  for (i = 0; i < N; i++)
    if (a[i] - b[i] > EPS || b[i] - a[i] > EPS)
      abort ();
}

int main ()
{
   int i;
   int a[N], a_ref[N], b[N];
   float x[N], x_ref[N], y[N];

   init(a, x, N);
   init(b, y, N);
   myaddint(a, b, N);
   myaddfloat(x, y, N);

   init(a_ref, x_ref, N);
   init(b, y, N);
   myaddint_ref(a_ref, b, N);
   myaddfloat_ref(x_ref, y, N);

   check_addint(a, a_ref);
   check_addfloat(x, x_ref);

   return 0;
}
