/* { dg-additional-options "--param=openacc-kernels=decompose" } */

/* { dg-additional-options "-fopt-info-all-omp" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
   { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop_i 0] }
   { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#include  <openacc.h>

int test_parallel ()
{
  int ok = 1;
  int val = 2;
  int ary[32];
  int ondev = 0;

  for (int i = 0; i < 32; i++)
    ary[i] = ~0;

  /* val defaults to firstprivate, ary defaults to copy.  */
#pragma acc parallel num_gangs (32) copy (ok) copy(ondev) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
  {
    ondev = acc_on_device (acc_device_not_host);
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
       ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
#pragma acc loop gang(static:1) /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized {assigned OpenACC gang loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
    for (unsigned i = 0; i < 32; i++)
      {
	if (val != 2)
	  ok = 0;
	val += i;
	ary[i] = val;
      }
  }

  if (ondev)
    {
      if (!ok)
	return 1;
      if (val != 2)
	return 1;

      for (int i = 0; i < 32; i++)
	if (ary[i] != 2 + i)
	  return 1;
    }
  
  return 0;
}

int test_kernels ()
{
  int val = 2;
  int ary[32];
  int ondev = 0;

  for (int i = 0; i < 32; i++)
    ary[i] = ~0;

  /* val defaults to copy, ary defaults to copy.  */
#pragma acc kernels copy(ondev) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {OpenACC 'kernels' decomposition: variable 'val' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'val' made addressable} {} { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'ondev\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
  {
    /* { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 } */
    ondev = acc_on_device (acc_device_not_host);
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
       ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */
    /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
    /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
    for (unsigned i = 0; i < 32; i++)
      {
	ary[i] = val;
	val++;
      }
  }

  if (ondev)
    {
      if (val != 2 + 32)
	return 1;

      for (int i = 0; i < 32; i++)
	if (ary[i] != 2 + i)
	  return 1;
    }
  
  return 0;
}

int main ()
{
  if (test_parallel ())
    return 1;

  if (test_kernels ())
    return 1;

  return 0;
}
