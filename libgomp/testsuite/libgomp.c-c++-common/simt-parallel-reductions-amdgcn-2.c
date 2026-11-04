/* Check the size of the private copies for "parallel" with task reductions
   nested inside a parallel region on an AMD GCN device in "simt" offload
   mode.  Each outer thread (a lane) is a team of one, so it needs one copy,
   not one per lane of its wavefront.  With 64 copies per lane the 1024
   threads would need 256 MiB, more than the default device heap.  Every
   lane allocates its hash table at once; this needs newlib's SIMT malloc.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define NT 1024
#define N 1024

int buf[NT][N];
#pragma omp declare target (buf)

int
main (void)
{
  int nthr = 0, bad = 0;

  #pragma omp target teams num_teams(1) map(tofrom: nthr, bad)
  #pragma omp parallel num_threads(NT)
  {
    int t = omp_get_thread_num ();
    int *p = buf[t];
    int b = 0;

    #pragma omp single
    nthr = omp_get_num_threads ();

    for (int i = 0; i < N; i++)
      p[i] = 1;

    #pragma omp parallel reduction(task, +: p[:N])
    for (int i = 0; i < N; i++)
      p[i] += t + i;

    for (int i = 0; i < N; i++)
      if (p[i] != 1 + t + i)
	b++;

    if (b)
      {
	#pragma omp atomic
	bad += b;
      }
  }

  if (nthr != NT || bad != 0)
    abort ();

  return 0;
}
