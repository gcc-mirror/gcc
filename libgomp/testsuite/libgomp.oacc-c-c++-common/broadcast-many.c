/* To avoid 'error: shared-memory region overflow':
   { dg-additional-options "-foffload-options=amdgcn-amdhsa=-mgang-private-size=64" { target openacc_radeon_accel_selected } }
*/

#include <assert.h>
#include <stdio.h>

#if ACC_DEVICE_TYPE_nvidia
/* To avoid 'libgomp: The Nvidia accelerator has insufficient resources'.  */
#define NUM_WORKERS 28
#else
#define NUM_WORKERS 32
#endif

#define LOCAL(n) double n = input;
#define LOCALS(n) LOCAL(n##1) LOCAL(n##2) LOCAL(n##3) LOCAL(n##4) \
		  LOCAL(n##5) LOCAL(n##6) LOCAL(n##7) LOCAL(n##8)
#define LOCALS2(n) LOCALS(n##a) LOCALS(n##b) LOCALS(n##c) LOCALS(n##d) \
		   LOCALS(n##e) LOCALS(n##f) LOCALS(n##g) LOCALS(n##h)

#define USE(n) n
#define USES(n,OP) USE(n##1) OP USE(n##2) OP USE(n##3) OP USE (n##4) OP \
		   USE(n##5) OP USE(n##6) OP USE(n##7) OP USE (n##8)
#define USES2(n,OP) USES(n##a,OP) OP USES(n##b,OP) OP USES(n##c,OP) OP \
		    USES(n##d,OP) OP USES(n##e,OP) OP USES(n##f,OP) OP \
		    USES(n##g,OP) OP USES(n##h,OP)

int main (void)
{
  int ret;
  int input = 1;

  #pragma acc parallel num_gangs(1) num_workers(NUM_WORKERS) copyout(ret)
  {
    int w = 0;
    LOCALS2(h);

    #pragma acc loop worker reduction(+:w)
    for (int i = 0; i < 32; i++)
      {
	int u = USES2(h,+);
	w += u;
      }

    printf ("w=%d\n", w);
    /* { dg-output "w=2048(\n|\r\n|\r)" } */

    LOCALS2(i);

    #pragma acc loop worker reduction(+:w)
    for (int i = 0; i < 32; i++)
      {
	int u = USES2(i,+);
	w += u;
      }

    printf ("w=%d\n", w);
    /* { dg-output "w=4096(\n|\r\n|\r)" } */

    LOCALS2(j);
    LOCALS2(k);

    #pragma acc loop worker reduction(+:w)
    for (int i = 0; i < 32; i++)
      {
	int u = USES2(j,+);
	w += u;
      }

    printf ("w=%d\n", w);
    /* { dg-output "w=6144(\n|\r\n|\r)" } */

    #pragma acc loop worker reduction(+:w)
    for (int i = 0; i < 32; i++)
      {
	int u = USES2(k,+);
	w += u;
      }

    ret = (w == 64 * 32 * 4);
    printf ("w=%d\n", w);
    /* { dg-output "w=8192(\n|\r\n|\r)" } */
  }

  assert (ret);

  return 0;
}
