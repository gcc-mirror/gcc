/* Verify OMP instances of variables with address space.  */

/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target offload_device_nonshared_as } */

#include <assert.h>

int __seg_fs a;

int
main (void)
{
  // a = 123; // SIGSEGV
  int b;
#pragma omp target map(alloc: a) map(from: b)
  {
    a = 321; // no SIGSEGV (given 'offload_device_nonshared_as')
    asm volatile ("" : : "g" (&a) : "memory");
    b = a;
  }
  assert (b == 321);

  return 0;
}
