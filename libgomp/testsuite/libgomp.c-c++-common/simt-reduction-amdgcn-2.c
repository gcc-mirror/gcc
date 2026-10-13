/* Companion to simt-reduction-amdgcn-1.c: check reductions on a
   "#pragma omp simd" loop nested inside team/thread parallelism
   ("#pragma omp target teams distribute parallel for"), which is the
   intended way to use SIMT lanes (one team's one thread == one 64-lane
   wavefront), as opposed to the bare "target" + "simd" with no enclosing
   teams/parallel exercised by simt-reduction-amdgcn-1.c.

   num_teams(1) num_threads(1) forces a single team and a single thread,
   i.e. a single wavefront, so this isolates the within-wavefront
   reduction tree from any cross-team/cross-thread combination -- the
   same thing simt-reduction-amdgcn-1.c checks, just reached through the
   nested construct instead of the bare one.

   As of this writing, this nested case does not even compile, for two
   independent reasons, triggered regardless of the num_teams/num_threads
   values since the limitation is structural rather than a function of
   the actual team/thread counts:

     - The GCN backend's lane-index translation for a SIMT loop inside
       team/thread parallelism is an unimplemented stub
       ("omp_simt_xchg_idx_<mode>" in gcn-valu.md, "sorry, placeholder
       ... simt_xchg_idx"), hit when generating the amdgcn offload
       image.

     - Separately, generating the host fallback version of the same
       region ICEs in expand_GOMP_SIMT_LANE (internal-fn.cc) with
       "'--param=offload-gpu-mode=simt-directed|simt' is incompatible
       with '-no-msimt'" -- the incompatibility is correctly diagnosed,
       but compilation does not stop there and crashes instead.  */

/* { dg-do link } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */
/* { dg-excess-errors "sorry, unimplemented: simt_xchg_idx; ICE in expand_GOMP_SIMT_LANE for the host fallback; lto1, mkoffload and lto-wrapper fatal errors" { target offload_target_amdgcn } } */

#include <stdlib.h>

#define FULL_N (64 * 5)		/* Multiple of 64: only full groups.  */
#define PART_N (64 * 3 + 17)	/* Last group has only 17 lanes live.  */

static void
test_sum (int n)
{
  long sum = 0, expected = 0;
  int i;

  #pragma omp target teams distribute parallel for simd \
    num_teams(1) num_threads(1) reduction(+:sum) map(tofrom: sum)
  for (i = 0; i < n; i++)
    sum += i;

  for (i = 0; i < n; i++)
    expected += i;

  if (sum != expected)
    abort ();
}

static void
test_minmax (int n)
{
  int rmin = n, rmax = -1;
  int i;

  #pragma omp target teams distribute parallel for simd \
    num_teams(1) num_threads(1) \
    reduction(min:rmin) reduction(max:rmax) map(tofrom: rmin, rmax)
  for (i = 0; i < n; i++)
    {
      if (i < rmin)
	rmin = i;
      if (i > rmax)
	rmax = i;
    }

  if (rmin != 0 || rmax != n - 1)
    abort ();
}

static void
test_xor (int n)
{
  unsigned int x = 0, expected = 0;
  int i;

  #pragma omp target teams distribute parallel for simd \
    num_teams(1) num_threads(1) reduction(^:x) map(tofrom: x)
  for (i = 0; i < n; i++)
    x ^= i;

  for (i = 0; i < n; i++)
    expected ^= i;

  if (x != expected)
    abort ();
}

int
main (void)
{
  test_sum (FULL_N);
  test_sum (PART_N);
  test_minmax (FULL_N);
  test_minmax (PART_N);
  test_xor (FULL_N);
  test_xor (PART_N);
  return 0;
}
