/* Offloaded 'constructor' and 'destructor' functions, and C++ objects construction and destruction.  */

/* { dg-require-effective-target init_priority } */

/* { dg-additional-options -fdump-tree-optimized-raw-asmname }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw-asmname } */

#include <omp.h>
#include <vector>

#pragma omp declare target

struct S
{
  int x;

  S()
    : x(-1)
  {
    __builtin_printf("%s, %d, %d\n", __FUNCTION__, x, omp_is_initial_device());
  }
  S(int x)
    : x(x)
  {
    __builtin_printf("%s, %d, %d\n", __FUNCTION__, x, omp_is_initial_device());
  }
  ~S()
  {
    __builtin_printf("%s, %d, %d\n", __FUNCTION__, x, omp_is_initial_device());
  }
};

#pragma omp end declare target

S sH1 __attribute__((init_priority(1500))) (7);

#pragma omp declare target

S sHD1 __attribute__((init_priority(2000))) (5);

std::vector<S> svHD1 __attribute__((init_priority(1000))) (2);

static void
__attribute__((constructor(20000)))
initDH1()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

static void
__attribute__((destructor(20000)))
finiDH1()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

#pragma omp end declare target

S sH2 __attribute__((init_priority(500))) (3);

static void
__attribute__((constructor(10000)))
initH1()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

static void
__attribute__((destructor(10000)))
finiH1()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

int main()
{
  int c = 0;

  __builtin_printf("%s:%d, %d\n", __FUNCTION__, ++c, omp_is_initial_device());

#pragma omp target map(c)
  {
    __builtin_printf("%s:%d, %d\n", __FUNCTION__, ++c, omp_is_initial_device());
  }

#pragma omp target map(c)
  {
    __builtin_printf("%s:%d, %d\n", __FUNCTION__, ++c, omp_is_initial_device());
  }

  __builtin_printf("%s:%d, %d\n", __FUNCTION__, ++c, omp_is_initial_device());

  return 0;
}

/* Verify '__cxa_atexit' calls (or '__aeabi_atexit', per 'targetm.cxx.use_aeabi_atexit').

   For the host, there are four expected calls:
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_atexit, } 4 optimized { target { cxa_atexit && { ! arm_eabi } } } } }
     { dg-final { scan-tree-dump-times {gimple_call <__aeabi_atexit, } 4 optimized { target { cxa_atexit && arm_eabi } } } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_atexit, NULL, _ZN1SD1Ev, \&sH1, \&__dso_handle>} 1 optimized { target { cxa_atexit && { ! arm_eabi } } } } }
     { dg-final { scan-tree-dump-times {gimple_call <__aeabi_atexit, NULL, \&sH1, _ZN1SD1Ev, \&__dso_handle>} 1 optimized { target { cxa_atexit && arm_eabi } } } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_atexit, NULL, _ZN1SD1Ev, \&sHD1, \&__dso_handle>} 1 optimized { target { cxa_atexit && { ! arm_eabi } } } } }
     { dg-final { scan-tree-dump-times {gimple_call <__aeabi_atexit, NULL, \&sHD1, _ZN1SD1Ev, \&__dso_handle>} 1 optimized { target { cxa_atexit && arm_eabi } } } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_atexit, NULL, _ZNSt6vectorI1SSaIS0_EED1Ev, \&svHD1, \&__dso_handle>} 1 optimized { target { cxa_atexit && { ! arm_eabi } } } } }
     { dg-final { scan-tree-dump-times {gimple_call <__aeabi_atexit, NULL, \&svHD1, _ZNSt6vectorI1SSaIS0_EED1Ev, \&__dso_handle>} 1 optimized { target { cxa_atexit && arm_eabi } } } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_atexit, NULL, _ZN1SD1Ev, \&sH2, \&__dso_handle>} 1 optimized { target { cxa_atexit && { ! arm_eabi } } } } }
     { dg-final { scan-tree-dump-times {gimple_call <__aeabi_atexit, NULL, \&sH2, _ZN1SD1Ev, \&__dso_handle>} 1 optimized { target { cxa_atexit && arm_eabi } } } }

   For the device, there are two expected calls:
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_atexit, } 2 optimized { target cxa_atexit } } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_atexit, NULL, _ZN1SD1Ev, \&sHD1, \&__dso_handle>} 1 optimized { target cxa_atexit } } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_atexit, NULL, _ZNSt6vectorI1SSaIS0_EED1Ev, \&svHD1, \&__dso_handle>} 1 optimized { target cxa_atexit } } }
*/

/* Defined order in which 'constructor' functions, and 'destructor' functions are run, and C++ objects are constructed (..., and destructed in reverse order).
   { dg-output {S, 3, 1[\r\n]+} }
   { dg-output {S, -1, 1[\r\n]+} }
   { dg-output {S, -1, 1[\r\n]+} }
   { dg-output {S, 7, 1[\r\n]+} }
   { dg-output {S, 5, 1[\r\n]+} }
   { dg-output {initH1, 1[\r\n]+} }
   { dg-output {initDH1, 1[\r\n]+} }
   { dg-output {main:1, 1[\r\n]+} }
   { dg-output {S, -1, 0[\r\n]+} { target offload_device } }
   { dg-output {S, -1, 0[\r\n]+} { target offload_device } }
   { dg-output {S, 5, 0[\r\n]+} { target offload_device } }
   { dg-output {initDH1, 0[\r\n]+} { target offload_device } }
   { dg-output {main:2, 1[\r\n]+} { target  { ! offload_device } } }
   { dg-output {main:2, 0[\r\n]+} { target offload_device } }
   { dg-output {main:3, 1[\r\n]+} { target { ! offload_device } } }
   { dg-output {main:3, 0[\r\n]+} { target offload_device } }
   { dg-output {main:4, 1[\r\n]+} }
   { dg-output {~S, 5, 0[\r\n]+} { target offload_device } }
   { dg-output {~S, -1, 0[\r\n]+} { target offload_device } }
   { dg-output {~S, -1, 0[\r\n]+} { target offload_device } }
   { dg-output {finiDH1, 0[\r\n]+} { target offload_device } }
   { dg-output {~S, 5, 1[\r\n]+} }
   { dg-output {~S, 7, 1[\r\n]+} }
   { dg-output {~S, -1, 1[\r\n]+} }
   { dg-output {~S, -1, 1[\r\n]+} }
   { dg-output {~S, 3, 1[\r\n]+} }
   { dg-output {finiDH1, 1[\r\n]+} }
   { dg-output {finiH1, 1[\r\n]+} }
*/
