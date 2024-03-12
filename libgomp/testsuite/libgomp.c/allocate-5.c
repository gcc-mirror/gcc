/* TODO: move to ../libgomp.c-c++-common once C++ is implemented. */
/* NOTE: { target c } is unsupported with with the C compiler.  */

/* { dg-do run } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include <omp.h>
#include <stdint.h>

/* { dg-final { scan-tree-dump-not "__builtin_stack_save" "gimple" } } */
/* { dg-final { scan-tree-dump-not "__builtin_alloca" "gimple" } } */
/* { dg-final { scan-tree-dump-not "__builtin_stack_restore" "gimple" } } */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(" 5 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(" 5 "gimple" } } */

void
one ()
{
  int result = 0, n = 3;
  #pragma omp target map(tofrom: result) firstprivate(n)
    {
      int var = 5, var2[n];
      #pragma omp allocate(var,var2) align(128) allocator(omp_low_lat_mem_alloc)
/* { dg-final { scan-tree-dump-times "var\\.\[0-9\]+ = __builtin_GOMP_alloc \\(128, 4, 5\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "var2\\.\[0-9\]+ = __builtin_GOMP_alloc \\(128, D\\.\[0-9\]+, 5\\);" 1 "gimple" } } */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(var\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(var2\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */

      if ((intptr_t) &var % 128 != 0)
	__builtin_abort ();
      if ((intptr_t) var2 % 128 != 0)
	__builtin_abort ();
      if (var != 5)
	__builtin_abort ();

      #pragma omp parallel for
      for (int i = 0; i < n; ++i)
	var2[i] = (i+33);

      #pragma omp loop reduction(+:result)
      for (int i = 0; i < n; ++i)
	result += var + var2[i];
    }
  if (result != (3*5 + 33 + 34 + 35))
    __builtin_abort ();
}

void
two ()
{
  struct st {
    int a, b;
  };
  int scalar = 44, array[5] = {1,2,3,4,5};
  struct st s = {.a=11, .b=56};
  #pragma omp allocate(scalar, array, s)
/* { dg-final { scan-tree-dump-times "scalar\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 4, 0B\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "array\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 20, 0B\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "s\\.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 8, 0B\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(scalar\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(array\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(s\\.\[0-9\]+, 0B\\);" 1 "gimple" } } */

  #pragma omp parallel firstprivate(scalar) firstprivate(array) firstprivate(s)
  {
    if (scalar != 44)
      __builtin_abort ();
    scalar = 33;
    for (int i = 0; i < 5; ++i)
      if (array[i] != i+1)
	__builtin_abort ();
    for (int i = 0; i < 5; ++i)
      array[i] = 10*(i+1);
    if (s.a != 11 || s.b != 56)
      __builtin_abort ();
    s.a = 74;
    s.b = 674;
  }
  if (scalar != 44)
    __builtin_abort ();
  for (int i = 0; i < 5; ++i)
    if (array[i] != i+1)
      __builtin_abort ();
  if (s.a != 11 || s.b != 56)
    __builtin_abort ();

  #pragma omp target defaultmap(firstprivate : scalar) defaultmap(none : aggregate) defaultmap(none : pointer)
  {
    if (scalar != 44)
      __builtin_abort ();
    scalar = 33;
  }
  if (scalar != 44)
    __builtin_abort ();

  #pragma omp target defaultmap(none : scalar) defaultmap(firstprivate : aggregate) defaultmap(none : pointer)
  {
    for (int i = 0; i < 5; ++i)
      if (array[i] != i+1)
	__builtin_abort ();
    for (int i = 0; i < 5; ++i)
      array[i] = 10*(i+1);
  }
  for (int i = 0; i < 5; ++i)
    if (array[i] != i+1)
      __builtin_abort ();
  #pragma omp target defaultmap(none : scalar) defaultmap(firstprivate : aggregate) defaultmap(none : pointer)
  {
    if (s.a != 11 || s.b != 56)
      __builtin_abort ();
    s.a = 74;
    s.b = 674;
  }
  if (s.a != 11 || s.b != 56)
    __builtin_abort ();
}

int
main ()
{
  one ();
  two ();
  return 0;
}
