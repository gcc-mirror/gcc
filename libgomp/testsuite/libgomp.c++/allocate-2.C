/* { dg-do run } */
/* { dg-additional-options "-fdump-tree-omplower" } */

/* For the 4 vars in omp_parallel, 4 in omp_target and 1 of 2 in each of no_alloc{,2}_func.  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(" 10 "omplower" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(" 10 "omplower" } } */

#include <omp.h>


void
check_int (int *x, int y)
{
  if (*x != y)
    __builtin_abort ();
}

void
check_ptr (int **x, int *y)
{
  if (*x != y)
    __builtin_abort ();
}


template<typename t>
t
no_alloc_func ()
{
  /* There is no __builtin_GOMP_alloc / __builtin_GOMP_free as
     allocator == omp_default_mem_alloc (known at compile time. */
  t no_alloc, alloc_has_align = 3;
  #pragma omp allocate(no_alloc) allocator(omp_default_mem_alloc)
  /* But this one is allocated because of align. */
  #pragma omp allocate(alloc_has_align) allocator(omp_default_mem_alloc) align(sizeof(t))
  no_alloc = 7;
  return no_alloc + alloc_has_align;
}

template<typename t>
t
no_alloc2_func()
{
  /* There is no __builtin_GOMP_alloc / __builtin_GOMP_free as
     no_alloc2 is TREE_UNUSED.  But there is for is_alloc2.  */
  t no_alloc2, is_alloc2;
  #pragma omp allocate(no_alloc2, is_alloc2)
  is_alloc2 = 7;
  return is_alloc2;
}


template<typename t>
void
omp_parallel ()
{
  int n = 6;
  t iii = 5, jjj[5], kkk[n];
  t *ptr = (t *) 0x1234;
  #pragma omp allocate(iii, jjj, kkk, ptr)

  for (int i = 0; i < 5; i++)
    jjj[i] = 3*i;
  for (int i = 0; i < 6; i++)
    kkk[i] = 7*i;

  #pragma omp parallel default(none) firstprivate(iii, jjj, kkk, ptr) if(0)
  {
    if (iii != 5)
      __builtin_abort();
    iii = 7;
    check_int (&iii, 7);
    for (int i = 0; i < 5; i++)
      if (jjj[i] != 3*i)
	__builtin_abort ();
    for (int i = 0; i < 6; i++)
      if (kkk[i] != 7*i)
	__builtin_abort ();
    for (int i = 0; i < 5; i++)
      jjj[i] = 4*i;
    for (int i = 0; i < 6; i++)
      kkk[i] = 8*i;
    for (int i = 0; i < 5; i++)
      check_int (&jjj[i], 4*i);
    for (int i = 0; i < 6; i++)
      check_int (&kkk[i], 8*i);
    if (ptr != (int *) 0x1234)
      __builtin_abort ();
    ptr = (int *) 0xabcd;
    if (ptr != (int *) 0xabcd)
      __builtin_abort ();
    check_ptr (&ptr, (int *) 0xabcd);
  }
  if (iii != 5)
    __builtin_abort ();
  check_int (&iii, 5);
  for (int i = 0; i < 5; i++)
    {
      if (jjj[i] != 3*i)
	__builtin_abort ();
      check_int (&jjj[i], 3*i);
    }
  for (int i = 0; i < 6; i++)
    {
      if (kkk[i] != 7*i)
	__builtin_abort ();
      check_int (&kkk[i], 7*i);
    }
  if (ptr != (int *) 0x1234)
    __builtin_abort ();
  check_ptr (&ptr, (int *) 0x1234);

  #pragma omp parallel default(firstprivate) if(0)
  {
    if (iii != 5)
      __builtin_abort();
    iii = 7;
    check_int (&iii, 7);
    for (int i = 0; i < 5; i++)
      if (jjj[i] != 3*i)
	__builtin_abort ();
    for (int i = 0; i < 6; i++)
      if (kkk[i] != 7*i)
	__builtin_abort ();
    for (int i = 0; i < 5; i++)
      jjj[i] = 4*i;
    for (int i = 0; i < 6; i++)
      kkk[i] = 8*i;
    for (int i = 0; i < 5; i++)
      check_int (&jjj[i], 4*i);
    for (int i = 0; i < 6; i++)
      check_int (&kkk[i], 8*i);
    if (ptr != (int *) 0x1234)
      __builtin_abort ();
    ptr = (int *) 0xabcd;
    if (ptr != (int *) 0xabcd)
      __builtin_abort ();
    check_ptr (&ptr, (int *) 0xabcd);
  }
  if (iii != 5)
    __builtin_abort ();
  check_int (&iii, 5);
  for (int i = 0; i < 5; i++)
    {
      if (jjj[i] != 3*i)
	__builtin_abort ();
      check_int (&jjj[i], 3*i);
    }
  for (int i = 0; i < 6; i++)
    {
      if (kkk[i] != 7*i)
	__builtin_abort ();
      check_int (&kkk[i], 7*i);
    }
  if (ptr != (int *) 0x1234)
    __builtin_abort ();
  check_ptr (&ptr, (int *) 0x1234);
}


template<typename t>
void
omp_target ()
{
  int n = 6;
  t iii = 5, jjj[5], kkk[n];
  t *ptr = (int *) 0x1234;
  #pragma omp allocate(iii, jjj, kkk, ptr)

  for (int i = 0; i < 5; i++)
    jjj[i] = 3*i;
  for (int i = 0; i < 6; i++)
    kkk[i] = 7*i;

  #pragma omp target defaultmap(none) firstprivate(iii, jjj, kkk, ptr)
  {
    if (iii != 5)
      __builtin_abort();
    iii = 7;
    check_int (&iii, 7);
    for (int i = 0; i < 5; i++)
      if (jjj[i] != 3*i)
	__builtin_abort ();
    for (int i = 0; i < 6; i++)
      if (kkk[i] != 7*i)
	__builtin_abort ();
    for (int i = 0; i < 5; i++)
      jjj[i] = 4*i;
    for (int i = 0; i < 6; i++)
      kkk[i] = 8*i;
    for (int i = 0; i < 5; i++)
      check_int (&jjj[i], 4*i);
    for (int i = 0; i < 6; i++)
      check_int (&kkk[i], 8*i);
    if (ptr != (int *) 0x1234)
      __builtin_abort ();
    ptr = (int *) 0xabcd;
    if (ptr != (int *) 0xabcd)
      __builtin_abort ();
    check_ptr (&ptr, (int *) 0xabcd);
  }
  if (iii != 5)
    __builtin_abort ();
  check_int (&iii, 5);
  for (int i = 0; i < 5; i++)
    {
      if (jjj[i] != 3*i)
	__builtin_abort ();
      check_int (&jjj[i], 3*i);
    }
  for (int i = 0; i < 6; i++)
    {
      if (kkk[i] != 7*i)
	__builtin_abort ();
      check_int (&kkk[i], 7*i);
    }
  if (ptr != (int *) 0x1234)
    __builtin_abort ();
  check_ptr (&ptr, (int *) 0x1234);

  #pragma omp target defaultmap(firstprivate)
  {
    if (iii != 5)
      __builtin_abort();
    iii = 7;
    check_int (&iii, 7);
    for (int i = 0; i < 5; i++)
      if (jjj[i] != 3*i)
	__builtin_abort ();
    for (int i = 0; i < 6; i++)
      if (kkk[i] != 7*i)
	__builtin_abort ();
    for (int i = 0; i < 5; i++)
      jjj[i] = 4*i;
    for (int i = 0; i < 6; i++)
      kkk[i] = 8*i;
    for (int i = 0; i < 5; i++)
      check_int (&jjj[i], 4*i);
    for (int i = 0; i < 6; i++)
      check_int (&kkk[i], 8*i);
    if (ptr != (int *) 0x1234)
      __builtin_abort ();
    ptr = (int *) 0xabcd;
    if (ptr != (int *) 0xabcd)
      __builtin_abort ();
    check_ptr (&ptr, (int *) 0xabcd);
  }
  if (iii != 5)
    __builtin_abort ();
  check_int (&iii, 5);
  for (int i = 0; i < 5; i++)
    {
      if (jjj[i] != 3*i)
	__builtin_abort ();
      check_int (&jjj[i], 3*i);
    }
  for (int i = 0; i < 6; i++)
    {
      if (kkk[i] != 7*i)
	__builtin_abort ();
      check_int (&kkk[i], 7*i);
    }
  if (ptr != (int *) 0x1234)
    __builtin_abort ();
  check_ptr (&ptr, (int *) 0x1234);

  #pragma omp target defaultmap(tofrom)
  {
    if (iii != 5)
      __builtin_abort();
    iii = 7;
    check_int (&iii, 7);
    for (int i = 0; i < 5; i++)
      if (jjj[i] != 3*i)
	__builtin_abort ();
    for (int i = 0; i < 6; i++)
      if (kkk[i] != 7*i)
	__builtin_abort ();
    for (int i = 0; i < 5; i++)
      jjj[i] = 4*i;
    for (int i = 0; i < 6; i++)
      kkk[i] = 8*i;
    for (int i = 0; i < 5; i++)
      check_int (&jjj[i], 4*i);
    for (int i = 0; i < 6; i++)
      check_int (&kkk[i], 8*i);
    if (ptr != (int *) 0x1234)
      __builtin_abort ();
    ptr = (int *) 0xabcd;
    if (ptr != (int *) 0xabcd)
      __builtin_abort ();
    check_ptr (&ptr, (int *) 0xabcd);
  }

  if (iii != 7)
    __builtin_abort ();
  check_int (&iii, 7);
  for (int i = 0; i < 5; i++)
    {
      if (jjj[i] != 4*i)
	__builtin_abort ();
      check_int (&jjj[i], 4*i);
    }
  for (int i = 0; i < 6; i++)
    {
      if (kkk[i] != 8*i)
	__builtin_abort ();
      check_int (&kkk[i], 8*i);
    }
  if (ptr != (int *) 0xabcd)
    __builtin_abort ();
  check_ptr (&ptr, (int *) 0xabcd);
}

int
foo()
{
  return no_alloc_func<int>() + no_alloc2_func<int>();
}

int
main ()
{
  omp_parallel<int> ();
  omp_target<int> ();
  if (foo() != 10 + 7)
    __builtin_abort ();
  return 0;
}
