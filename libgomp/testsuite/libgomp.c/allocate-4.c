/* TODO: move to ../libgomp.c-c++-common once C++ is implemented. */
/* NOTE: { target c } is unsupported with with the C compiler.  */

/* { dg-do run } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include <omp.h>
#include <stdint.h>

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(" 5 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(" 5 "gimple" } } */


int one ()
{
  int sum = 0;
  #pragma omp allocate(sum)
  /* { dg-final { scan-tree-dump-times "sum\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 4, 0B\\);" 1 "gimple" } } */
  /* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(sum\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */

  /* NOTE: Initializer cannot be omp_init_allocator - as 'A' is
     in the same scope and the auto-omp_free comes later than
     any omp_destroy_allocator.  */
  omp_allocator_handle_t my_allocator = omp_low_lat_mem_alloc;
  int n = 25;
  int A[n];
  #pragma omp allocate(A) align(128) allocator(my_allocator)
  /* { dg-final { scan-tree-dump-times "A\\.\[0-9\]+ = __builtin_GOMP_alloc \\(128, _\[0-9\]+, my_allocator\\);" 1 "gimple" } } */
  /* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(A\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */

  if (((intptr_t)A) % 128 != 0)
    __builtin_abort ();
  for (int i = 0; i < n; ++i)
    A[i] = i;

  omp_alloctrait_t traits[1] = { { omp_atk_alignment, 64 } };
  my_allocator = omp_init_allocator(omp_low_lat_mem_space,1,traits);
  {
    int B[n] = { };
    int C[5] = {1,2,3,4,5};
    #pragma omp allocate(B,C) allocator(my_allocator)
    /* { dg-final { scan-tree-dump-times "B\\.\[0-9\]+ = __builtin_GOMP_alloc \\(\[0-9\]+, _\[0-9\]+, my_allocator\\);" 1 "gimple" } } */
    /* { dg-final { scan-tree-dump-times "C\\.\[0-9\]+ = __builtin_GOMP_alloc \\(\[0-9\]+, 20, my_allocator\\);" 1 "gimple" } } */
    /* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(B\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */
    /* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(C\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */

    int D[5] = {11,22,33,44,55};
    #pragma omp allocate(D) align(256)
    /* { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_GOMP_alloc \\(256, 20, 0B\\);" 1 "gimple" } } */
    /* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(D\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */

    if (((intptr_t) B) % 64 != 0)
      __builtin_abort ();
    if (((intptr_t) C) % 64 != 0)
      __builtin_abort ();
    if (((intptr_t) D) % 64 != 0)
      __builtin_abort ();

    for (int i = 0; i < 5; ++i)
      {
	if (C[i] != i+1)
	  __builtin_abort ();
	if (D[i] != i+1 + 10*(i+1))
	  __builtin_abort ();
      }

    for (int i = 0; i < n; ++i)
      {
	if (B[i] != 0)
	  __builtin_abort ();
	sum += A[i]+B[i]+C[i%5]+D[i%5];
      }
  }
  omp_destroy_allocator (my_allocator);
  return sum;
}

int
main ()
{
  if (one () != 1200)
    __builtin_abort ();
  return 0;
}
