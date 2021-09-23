/* Tests for gang-private variables, 'atomic' access */

/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#include <assert.h>
#include <openacc.h>

int main (void)
{
  int ret;


  ret = 0;
  #pragma acc parallel num_gangs(1444) num_workers(32) reduction(+: ret) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'w' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'w' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'w' adjusted for OpenACC privatization level: 'gang'} "" { target { ! openacc_host_selected } } l_compute$c_compute } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  {
    int w = -22;

    #pragma acc loop worker /* { dg-line l_loop[incr c_loop] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    for (int i = 0; i < 2232; i++)
      {
	#pragma acc atomic update
	w++;
      }

    ret = (w == -22 + 2232);
  }
  if (acc_get_device_type () == acc_device_host)
    assert (ret == 1);
  else
    assert (ret == 1444);


  ret = 0;
  #pragma acc parallel num_gangs(1414) vector_length(32) reduction(+: ret) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'v' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'v' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'v' adjusted for OpenACC privatization level: 'gang'} "" { target { ! openacc_host_selected } } l_compute$c_compute } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  {
    int v = 10;

    #pragma acc loop vector /* { dg-line l_loop[incr c_loop] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    for (int i = 0; i < 3201; i++)
      {
	#pragma acc atomic update
	v++;
      }

    ret = (v == 10 + 3201);
  }
  if (acc_get_device_type () == acc_device_host)
    assert (ret == 1);
  else
    assert (ret == 1414);


  ret = 0;
#pragma acc parallel num_gangs(314) reduction(+: ret) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'v' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'v' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_compute$c_compute }
     { dg-note {variable 'v' adjusted for OpenACC privatization level: 'gang'} "" { target { ! openacc_host_selected } } l_compute$c_compute } */
  {
    int v = -222;

#pragma acc atomic update
    ++v;
#pragma acc atomic update
    ++v;
#pragma acc atomic update
    ++v;

    ret += (v == -222 + 3);
  }
  if (acc_get_device_type () == acc_device_host)
    assert (ret == 1);
  else
    assert (ret == 314);


  return 0;
}
