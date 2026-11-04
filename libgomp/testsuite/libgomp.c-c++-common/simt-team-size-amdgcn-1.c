/* Check teams whose size is not a multiple of the wavefront size on an AMD
   GCN device in "simt" offload mode, where each lane of a wavefront is a
   thread.  The last wavefront of such a team is partial, and only the team's
   threads may run the region.  Also check "if (0)", a "thread_limit" that
   is not a multiple of the wavefront size, and a nested region inside a
   team of one.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 1000
#define MAXT 256

static const int sizes[] = { 1, 2, 10, 63, 64, 65, 100, 129, 200, 256 };

int
main (void)
{
  for (int k = 0; k < (int) (sizeof (sizes) / sizeof (sizes[0])); k++)
    {
      int nt = sizes[k];
      int count = 0, nthr = 0, single = 0, bad = 0;
      int seen[MAXT] = { 0 }, iters[N] = { 0 }, iters2[N] = { 0 };
      long sum = 0;

      #pragma omp target teams num_teams(1) firstprivate(nt) \
		  map(tofrom: count, nthr, single, bad, seen, iters, iters2, \
			      sum)
      #pragma omp parallel num_threads(nt)
      {
	int t = omp_get_thread_num ();
	#pragma omp atomic
	count++;
	if (t < MAXT)
	  {
	    #pragma omp atomic
	    seen[t]++;
	  }
	if (omp_get_num_threads () != nt || omp_get_team_size (1) != nt)
	  {
	    #pragma omp atomic
	    bad++;
	  }
	if (t == 0)
	  nthr = omp_get_num_threads ();
	#pragma omp single
	single++;
	#pragma omp for
	for (int i = 0; i < N; i++)
	  {
	    #pragma omp atomic
	    iters[i]++;
	  }
	#pragma omp for schedule(static)
	for (int i = 0; i < N; i++)
	  {
	    #pragma omp atomic
	    iters2[i]++;
	  }
	#pragma omp for reduction(+: sum)
	for (int i = 0; i < N; i++)
	  sum += i;
      }

      if (count != nt || nthr != nt || single != 1 || bad
	  || sum != (long) N * (N - 1) / 2)
	abort ();
      for (int t = 0; t < MAXT; t++)
	if (seen[t] != (t < nt))
	  abort ();
      for (int i = 0; i < N; i++)
	if (iters[i] != 1 || iters2[i] != 1)
	  abort ();
    }

  /* "if (0)" runs one thread, and a nested region inside it is a team of
     one too.  */
  int count = 0, nthr = 0, level = 0, active = -1, ncount = 0, nnthr = 0;
  #pragma omp target teams num_teams(1) \
	      map(tofrom: count, nthr, level, active, ncount, nnthr)
  #pragma omp parallel if (0)
  {
    #pragma omp atomic
    count++;
    nthr = omp_get_num_threads ();
    active = omp_get_active_level ();
    #pragma omp parallel num_threads(4)
    {
      #pragma omp atomic
      ncount++;
      nnthr = omp_get_num_threads ();
      level = omp_get_level ();
    }
  }
  if (count != 1 || nthr != 1 || active != 0
      || ncount != 1 || nnthr != 1 || level != 2)
    abort ();

  /* "thread_limit" caps the default team size.  */
  count = 0;
  nthr = 0;
  #pragma omp target teams num_teams(1) thread_limit(100) \
	      map(tofrom: count, nthr)
  #pragma omp parallel
  {
    #pragma omp atomic
    count++;
    if (omp_get_thread_num () == 0)
      nthr = omp_get_num_threads ();
  }
  if (count != nthr || nthr < 1 || nthr > 100)
    abort ();

  /* Sections size the team by the number of sections.  */
  int s1 = 0, s2 = 0, s3 = 0;
  #pragma omp target teams num_teams(1) map(tofrom: s1, s2, s3)
  #pragma omp parallel sections
  {
    #pragma omp section
    { s1++; }
    #pragma omp section
    { s2++; }
    #pragma omp section
    { s3++; }
  }
  if (s1 != 1 || s2 != 1 || s3 != 1)
    abort ();

  return 0;
}
