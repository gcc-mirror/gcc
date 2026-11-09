/* Check single-clause _Complex reductions on an AMD GCN device in "simt"
   offload mode, where each lane of a wavefront is a thread.  The update
   of a complex value uses a lock, which the lanes of a wavefront hold
   together, so they must merge one at a time.  Check full and partial
   wavefronts, parallel regions and worksharing loops.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 4096

static const int sizes[] = { 1, 64, 100, 1024 };

int
main (void)
{
  for (int k = 0; k < (int) (sizeof (sizes) / sizeof (sizes[0])); k++)
    {
      int nt = sizes[k];
      _Complex float sf = 0, pf = 1;
      _Complex double sd = 0, pd = 1;
      _Complex int si = 0;

      #pragma omp target teams num_teams(1) firstprivate(nt) \
		  map(tofrom: sf, pf, sd, pd, si)
      {
	#pragma omp parallel num_threads(nt) reduction(+: sf)
	sf += (omp_get_thread_num () + 1) + 2.0fi;

	#pragma omp parallel num_threads(nt) reduction(+: sd)
	sd += (omp_get_thread_num () + 1) + 2.0i;

	#pragma omp parallel for num_threads(nt) reduction(+: si)
	for (int i = 0; i < N; i++)
	  si += i + 1i;

	/* Eight factors of 2i: the product is 256, exactly.  */
	#pragma omp parallel for num_threads(nt) reduction(*: pf)
	for (int i = 0; i < N; i++)
	  pf *= (i % (N / 8) == 0) ? 2.0fi : 1.0f;

	#pragma omp parallel for num_threads(nt) reduction(*: pd)
	for (int i = 0; i < N; i++)
	  pd *= (i % (N / 8) == 0) ? 2.0i : 1.0;
      }

      if (__real__ sf != (float) nt * (nt + 1) / 2 || __imag__ sf != 2 * nt)
	abort ();
      if (__real__ sd != (double) nt * (nt + 1) / 2 || __imag__ sd != 2 * nt)
	abort ();
      if (__real__ si != N * (N - 1) / 2 || __imag__ si != N)
	abort ();
      if (__real__ pf != 256 || __imag__ pf != 0)
	abort ();
      if (__real__ pd != 256 || __imag__ pd != 0)
	abort ();
    }

  return 0;
}
