/* Offloaded 'constructor' and 'destructor' functions.  */

#include <omp.h>

#pragma omp declare target

static void
__attribute__((constructor))
initHD1()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

static void
__attribute__((constructor))
initHD2()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

static void
__attribute__((destructor))
finiHD1()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

static void
__attribute__((destructor))
finiHD2()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

#pragma omp end declare target

static void
__attribute__((constructor))
initH1()
{
  __builtin_printf("%s, %d\n", __FUNCTION__, omp_is_initial_device());
}

static void
__attribute__((destructor))
finiH2()
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

/* The order is undefined, in which same-priority 'constructor' functions, and 'destructor' functions are run.
   { dg-output {init[^,]+, 1[\r\n]+} }
   { dg-output {init[^,]+, 1[\r\n]+} }
   { dg-output {init[^,]+, 1[\r\n]+} }
   { dg-output {main:1, 1[\r\n]+} }
   { dg-output {initHD[^,]+, 0[\r\n]+} { target offload_device } }
   { dg-output {initHD[^,]+, 0[\r\n]+} { target offload_device } }
   { dg-output {main:2, 1[\r\n]+} { target { ! offload_device } } }
   { dg-output {main:2, 0[\r\n]+} { target offload_device } }
   { dg-output {main:3, 1[\r\n]+} { target  { ! offload_device } } }
   { dg-output {main:3, 0[\r\n]+} { target offload_device } }
   { dg-output {main:4, 1[\r\n]+} }
   { dg-output {finiHD[^,]+, 0[\r\n]+} { target offload_device } }
   { dg-output {finiHD[^,]+, 0[\r\n]+} { target offload_device } }
   { dg-output {fini[^,]+, 1[\r\n]+} }
   { dg-output {fini[^,]+, 1[\r\n]+} }
   { dg-output {fini[^,]+, 1[\r\n]+} }
*/
