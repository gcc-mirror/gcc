/* Verify OpenACC 'declare' with VLAs.  */

/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-omp-all" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */


#include <assert.h>


void
f (void)
{
  int N = 1000;
  int i, A[N];
#pragma acc declare copy(A)

  for (i = 0; i < N; i++)
    A[i] = -i;

#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'N' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'N' made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { ! __OPTIMIZE__ } } l_compute$c_compute }
     { dg-optimized {assigned OpenACC gang loop parallelism} {} { target __OPTIMIZE__ } l_compute$c_compute } */
  /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
  for (i = 0; i < N; i++)
    A[i] = i;

#pragma acc update host(A)

  for (i = 0; i < N; i++)
    assert (A[i] == i);
}


/* The same as 'f' but everything contained in an OpenACC 'data' construct.  */

void
f_data (void)
{
#pragma acc data
  /* { dg-bogus {note: variable [^\n\r]+ candidate for adjusting OpenACC privatization level} {TODO 'data'} { xfail *-*-* } .-1 } */
  {
    int N = 1000;
    int i, A[N];
# pragma acc declare copy(A)

    for (i = 0; i < N; i++)
      A[i] = -i;

# pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'N' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'N' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { ! __OPTIMIZE__ } } l_compute$c_compute }
       { dg-optimized {assigned OpenACC gang loop parallelism} {} { target __OPTIMIZE__ } l_compute$c_compute } */
    /* { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    for (i = 0; i < N; i++)
      A[i] = i;

# pragma acc update host(A)

    for (i = 0; i < N; i++)
      assert (A[i] == i);
  }
}


int
main ()
{
  f ();

  f_data ();

  return 0;
}
