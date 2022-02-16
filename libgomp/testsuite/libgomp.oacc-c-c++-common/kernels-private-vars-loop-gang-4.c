/* Test of gang-private addressable variable declared on loop directive, with
   broadcasting to partitioned workers.  */

/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-omp-all" }
   { dg-additional-options "-foffload=-fopt-info-omp-all" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting:
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop_i 0 c_loop_j 0 c_loop_k 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#include <assert.h>

int
main (int argc, char* argv[])
{
  int x = 5, i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'x\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
  {
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
    #pragma acc loop gang(num:32) private(x) /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'x' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'p' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < 32; i++)
      {
        int *p = &x;

	x = i * 2;

	#pragma acc loop worker(num:32) /* { dg-line l_loop_j[incr c_loop_j] } */
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += x;

	(*p)--;
      }
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i / 32) * 2);

  return 0;
}
