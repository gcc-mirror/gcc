/* Test 'nohost' clause via 'acc_on_device'.

   With optimizations disabled, we currently don't expect that 'acc_on_device' "evaluates at compile time to a constant".
   { dg-skip-if "TODO PR82391" { *-*-* } { "-O0" } }
*/

/* { dg-additional-options "-fdump-tree-oaccloops" } */

/* { dg-additional-options "-fno-inline" } for stable results regarding OpenACC 'routine'.  */

#include <assert.h>
#include <openacc.h>

#pragma acc routine
static int fact(int n)
{
  if (n == 0 || n == 1)
    return 1;
  else
    return n * fact(n - 1);
}

#pragma acc routine nohost
static int fact_nohost(int n)
{
  /* Make sure this fails host compilation.  */
#if defined ACC_DEVICE_TYPE_host
  asm ("IT'S A TRAP");
#elif defined ACC_DEVICE_TYPE_nvidia
  asm ("{\n\t  .reg .u32 %tid_x;\n\t  mov.u32 %tid_x, %tid.x;\n\t}");
#elif defined ACC_DEVICE_TYPE_radeon
  asm ("s_nop 0");
#else
# error Not ported to this ACC_DEVICE_TYPE
#endif

  return fact(n);
}
/* { dg-final { scan-tree-dump-times {(?n)^OpenACC routine 'fact_nohost' has 'nohost' clause\.$} 1 oaccloops { target { c && offloading_enabled } } } }
   { dg-final { scan-tree-dump-times {(?n)^OpenACC routine 'fact_nohost\(int\)' has 'nohost' clause\.$} 1 oaccloops { target { c++ && offloading_enabled } } } }
   TODO See PR101551 for 'offloading_enabled' differences.  */

int main()
{
#define N 10
  int x[N];

#pragma acc parallel loop copyout(x)
  for (int i = 0; i < N; ++i)
    /*TODO PR82391: '(int) acc_device_*' cast to avoid the C++ 'acc_on_device' wrapper.  */
    x[i] = acc_on_device((int) acc_device_not_host) ? fact_nohost(i) : 0;

  for (int i = 0; i < N; ++i)
    {
      if (acc_get_device_type() == acc_device_host)
	assert(x[i] == 0);
      else
	assert(x[i] == fact(i));
    }

  return 0;
}
