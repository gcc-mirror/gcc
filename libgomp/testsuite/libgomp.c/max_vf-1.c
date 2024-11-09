/* Test that omp parallel simd schedule uses the correct max_vf for the
   host system, when target directives are present.  */

/* { dg-require-effective-target offload_target_any } */

/* { dg-do link } */
/* { dg-options "-fopenmp -O2 -fdump-tree-ompexp -foffload=-fdump-tree-optimized" } */

/* Fix a max_vf size so we can scan for it.
{ dg-additional-options "-msse2 -mno-avx" { target { x86_64-*-* i?86-*-* } } } */

#define N 1024
int a[N], b[N], c[N];

/* Test both static schedules and inline target directives.  */
void
f2 (void)
{
  int i;
  #pragma omp target parallel for simd schedule (simd: static, 7)
  for (i = 0; i < N; i++)
    a[i] = b[i] + c[i];
}

/* Test both dynamic schedules and declare target functions.  */
#pragma omp declare target
void
f3 (int *a, int *b, int *c)
{
  int i;
  #pragma omp parallel for simd schedule (simd : dynamic, 7)
  for (i = 0; i < N; i++)
    a[i] = b[i] + c[i];
}
#pragma omp end declare target

/* Make sure that the max_vf is used as an IFN.
{ dg-final { scan-tree-dump-times {GOMP_MAX_VF} 2 "ompexp" { target { x86_64-*-* i?86-*-* } } } } */

/* Make sure the max_vf is passed as a temporary variable.
{ dg-final { scan-tree-dump-times {__builtin_GOMP_parallel_loop_nonmonotonic_dynamic \(.*, D\.[0-9]*, 0\);} 1 "ompexp" { target { x86_64-*-* i?86-*-* } } } } */

/* Test SIMD offload devices
{ dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump-times {__builtin_GOMP_parallel_loop_nonmonotonic_dynamic \(.*, 64, 0\);} 1 "optimized" { target offload_target_amdgcn } } }
{ dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-times {__builtin_GOMP_parallel_loop_nonmonotonic_dynamic \(.*, 7, 0\);} 1 "optimized" { target offload_target_nvptx } } } */

int main() {}
