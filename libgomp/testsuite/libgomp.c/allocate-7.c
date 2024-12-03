/* TODO: move to ../libgomp.c-c++-common once C++ is implemented. */
/* NOTE: { target c } is unsupported with with the C compiler.  */

/* { dg-do run } */

#include <omp.h>

int AAA [[omp::decl(allocate,allocator(omp_low_lat_mem_alloc),align(4096))]];

#ifndef __cplusplus
  _Static_assert (_Alignof(AAA) == _Alignof(int), "wrong alignment");
#elif __cplusplus >= 201103L
  static_assert (alignof(AAA) == _Alignof(int), "wrong alignment");
#endif


void test0 ()
{
  int A1[5], B1;
  #pragma omp allocate(A1, B1) align(512) allocator(omp_default_mem_alloc)

#ifndef __cplusplus
  _Static_assert (_Alignof(A1) == _Alignof(int[5]), "wrong alignment");
  _Static_assert (_Alignof(B1) == _Alignof(int), "wrong alignment");
#elif __cplusplus >= 201103L
  static_assert (alignof(A1) == alignof(int[5]), "wrong alignment");
  static_assert (alignof(B1) == alignof(int), "wrong alignment");
#endif

  if (((__UINTPTR_TYPE__) &A1 % 512) != 0)
    __builtin_abort ();
  if (((__UINTPTR_TYPE__) &B1 % 512) != 0)
    __builtin_abort ();
}

int main()
{
  static int BBB [[omp::decl(allocate,allocator(omp_low_lat_mem_alloc),align(4096))]];

#ifndef __cplusplus
  _Static_assert (_Alignof(AAA) == _Alignof(int), "wrong alignment");
#elif __cplusplus >= 201103L
  static_assert (alignof(AAA) == alignof(int), "wrong alignment");
#endif

  if (((__UINTPTR_TYPE__) &AAA % 4096) != 0)
    __builtin_abort ();
  if (((__UINTPTR_TYPE__) &BBB % 4096) != 0)
    __builtin_abort ();

  test0 ();

  return 0;
}
