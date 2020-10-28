/* { dg-do link { target vect_simd_clones } } */
/* { dg-require-effective-target lto } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target shared } */
/* { dg-additional-options "-fdump-tree-gimple -fdump-tree-optimized -O2 -fPIC -shared -flto -flto-partition=one" } */
/* { dg-additional-options "-mno-sse3" { target { i?86-*-* x86_64-*-* } } } */

int
f01 (int a)
{
  asm volatile ("" : "+g" (a) : "g" (1) : "memory");
  return a;
}

int
f02 (int a)
{
  asm volatile ("" : "+g" (a) : "g" (2) : "memory");
  return a;
}

int
f03 (int a)
{
  asm volatile ("" : "+g" (a) : "g" (3) : "memory");
  return a;
}

#pragma omp declare variant (f01) match (device={isa("avx512f")}) /* 4 or 8 */
#pragma omp declare variant (f02) match (implementation={vendor(score(3):gnu)},device={kind(cpu)}) /* (1 or 2) + 3 */
#pragma omp declare variant (f03) match (implementation={vendor(score(5):gnu)},device={kind(host)}) /* (1 or 2) + 5 */
int
f04 (int a)
{
  asm volatile ("" : "+g" (a) : "g" (4) : "memory");
  return a;
}

#pragma omp declare simd
int
test1 (int x)
{
  /* At gimplification time, we can't decide yet which function to call.  */
  /* { dg-final { scan-tree-dump-times "f04 \\\(x" 2 "gimple" } } */
  /* After simd clones are created, the original non-clone test1 shall
     call f03 (score 6), the sse2/avx/avx2 clones too, but avx512f clones
     shall call f01 with score 8.  */
  /* { dg-final { scan-ltrans-tree-dump-not "f04 \\\(x" "optimized" } } */
  /* { dg-final { scan-ltrans-tree-dump-times "f03 \\\(x" 14 "optimized" } } */
  /* { dg-final { scan-ltrans-tree-dump-times "f01 \\\(x" 4 "optimized" } } */
  int a = f04 (x);
  int b = f04 (x);
  return a + b;
}
