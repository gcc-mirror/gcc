/* Test of worker-private variables declared in a local scope, broadcasting
   to vector-partitioned mode.  Back-to-back worker loops.  */

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
  int i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr) /* { dg-line l_compute[incr c_compute] } */
  /* [PR104784] For some reason, for C++, the OpenACC 'kernels' decomposition
     decides that a data region is needed for 'j', and subsequently requests it
     to be made addressable.
     { dg-note {OpenACC 'kernels' decomposition: variable 'j' declared in block requested to be made addressable} {} { target c++ } l_compute$c_compute }
     { dg-note {variable 'j' made addressable} {} { target c++ } l_compute$c_compute }
     { dg-note {variable 'j' declared in block is candidate for adjusting OpenACC privatization level} {} { target c++ } l_compute$c_compute } */
  {
    int j;

    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 } */
    #pragma acc loop gang(num:32) /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target c } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    for (i = 0; i < 32; i++)
      {
	#pragma acc loop worker(num:32) /* { dg-line l_loop_j[incr c_loop_j] } */
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-note {variable 'k' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-note {variable 'x' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int x = i ^ j * 3;

	    #pragma acc loop vector(length:32) /* { dg-line l_loop_k[incr c_loop_k] } */
	    /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k$c_loop_k } */
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }

	#pragma acc loop worker(num:32) /* { dg-line l_loop_j[incr c_loop_j] } */
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-note {variable 'k' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-note {variable 'x' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int x = i | j * 5;
	    
	    #pragma acc loop vector(length:32) /* { dg-line l_loop_k[incr c_loop_k] } */
	    /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k$c_loop_k } */
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}

  return 0;
}
