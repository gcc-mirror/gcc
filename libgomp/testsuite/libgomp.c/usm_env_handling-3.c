/* { dg-do run }  */
/* { dg-set-target-env-var GOMP_RUNTIME_USM "enabled" } */

// PR libgomp/127186

/* Check that attaching to a static global variable works with
   GOMP_RUNTIME_USM="enabled", which otherwise implies self mapping.

   This testcase also works with 'requires self_maps', normal offloading,
   and host fallback. */

#include <stdint.h>
#include <omp.h>

#ifdef USE_SELF_MAPS
  #pragma omp requires self_maps
#endif


// 'declare target' global variables:
int *p = nullptr;
int *pa[10] = {};
int *p2 = nullptr;

struct S {
  int *p, *pa[10];
} s;

int Edev = 0;

#pragma omp declare target enter(p, pa, p2, s, Edev)


// This function is executed either on the host (fallback) or on the device.
void
check_addr (intptr_t s_p_addr, intptr_t p_addr, intptr_t pa_addr, intptr_t s_pa_addr)
{
#if 0
   __builtin_printf ("%p / %p / %p / %p / %p (%p) - device %s\n",
		     p, pa[1], s.p, s.pa[1], p2, (void*)&Edev,
		     __builtin_omp_is_initial_device() ? "host" : "nohost");
#endif

  if (p == nullptr
      || pa[1] == nullptr
      || s.p != (void*) s_p_addr
      || s.pa[1] == nullptr
      || p2 != &Edev)
    __builtin_abort ();

  if (p_addr != 0 && (void*) p_addr != p)
    __builtin_abort ();
  if (pa_addr != 0 && (void*) pa_addr != pa[1])
    __builtin_abort ();
  if (s_pa_addr != 0 && (void*) s_pa_addr != s.pa[1])
    __builtin_abort ();
}


int
main ()
{
  int A,B,C,D;
  p = &A;
  pa[1] = &B;
  s.p = &C;
  s.pa[1] = &D;
  p2 = &Edev;

  // Check whether self mapping is active
  bool self_mapping_p = false;
  #pragma omp target map(to: self_mapping_p)
    self_mapping_p = true;
  __builtin_printf ("DEBUG: self_mapping_p = %d, num_devs = %d\n",
		    self_mapping_p, omp_get_num_devices ());

#if 0
   __builtin_printf ("%p / %p / %p / %p / %p (%p) - host\n",
		     p, pa[1], s.p, s.pa[1], p2, (void*)&Edev);
#endif

  // Map A, B, C, D (via the pointer) - and attach the pointee
  // to the global variable:

  #pragma omp target enter data map(to: p, p[:1])
  #pragma omp target enter data map(to: pa[1][0])
  #pragma omp target enter data map(to: s.pa[1][0])
  #pragma omp target enter data map(to: p2[:0])

  #pragma omp target enter data map(to: C)
  // This one works for the wrong reason (i.e. always is the host address)
  #pragma omp target update to(s.p)

  // Check whether the pointer attachment was successful on the default device
  intptr_t s_p_addr = (intptr_t) s.p;

  intptr_t p_addr = self_mapping_p ? (intptr_t) &A : 0;
  intptr_t pa_addr = self_mapping_p ? (intptr_t) &B : 0;
  intptr_t s_pa_addr = self_mapping_p ? (intptr_t) &D : 0;

  #pragma omp target firstprivate(s_p_addr, p_addr, pa_addr, s_pa_addr)
    check_addr (s_p_addr, p_addr, pa_addr, s_pa_addr);
}
