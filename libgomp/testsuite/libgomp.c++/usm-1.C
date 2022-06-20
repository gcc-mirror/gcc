/* { dg-do run } */
/* { dg-require-effective-target omp_usm } */
#include <stdint.h>

#pragma omp requires unified_shared_memory

int g1 = 0;

struct s1
{
  s1() { a = g1++;}
  ~s1() { g1--;}
  int a;
};

int
main ()
{
  s1 *p1 = new s1;
  s1 *p2 = new s1[10];

  if (!p1 || !p2 || p1->a != 0)
    __builtin_abort ();

  for (int i = 0; i < 10; i++)
    if (p2[i].a != i+1)
      __builtin_abort ();

  uintptr_t pp1 = (uintptr_t)p1;
  uintptr_t pp2 = (uintptr_t)p2;

#pragma omp target firstprivate(pp1, pp2)
    {
      s1 *t1 = (s1*)pp1;
      s1 *t2 = (s1*)pp2;
      if (t1->a != 0)
	__builtin_abort ();

      for (int i = 0; i < 10; i++)
	if (t2[i].a != i+1)
	  __builtin_abort ();

      t1->a = 42;
    }

  if (p1->a != 42)
    __builtin_abort ();

  delete [] p2;
  delete p1;
  if (g1 != 0)
    __builtin_abort ();
  return 0;
}
