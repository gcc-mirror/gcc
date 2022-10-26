/* { dg-do run } */
/* { dg-additional-options "-std=c++17" } */
/* { dg-require-effective-target omp_usm } */
#include <stdint.h>

#pragma omp requires unified_shared_memory

struct s1
{
  int a;
};

int
main ()
{
  s1 *p1 = new s1;
  s1 *p2 = new s1[10];

  if (!p1 || !p2)
    __builtin_abort ();

  uintptr_t pp1 = (uintptr_t)p1;
  uintptr_t pp2 = (uintptr_t)p2;
  if (pp1 & 0x7f != 0)
    __builtin_abort ();

  if (pp2 & 0x7f != 0)
    __builtin_abort ();

  delete [] p2;
  delete p1;
  return 0;
}
