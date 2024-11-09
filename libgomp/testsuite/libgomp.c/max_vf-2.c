/* Ensure that the default safelen is set correctly for the larger of the host
   and offload device, to prevent defeating the vectorizer.  */
 
/* { dg-require-effective-target offload_target_any } */

/* { dg-do link } */
/* { dg-options "-fopenmp -O2 -fdump-tree-omplower" } */

int f(float *a, float *b, int n)
{
  float sum = 0;
  #pragma omp target teams distribute parallel for simd map(tofrom: sum) reduction(+:sum)
  for (int i = 0; i < n; i++)
    sum += a[i] * b[i];
  return sum;
}

/* Make sure that the max_vf used is suitable for the offload device.
{ dg-final { scan-tree-dump-times {omp simd safelen\(64\)} 1 "omplower" { target offload_target_amdgcn } } } */

int main() {}
