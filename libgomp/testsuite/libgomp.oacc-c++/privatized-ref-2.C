/* { dg-do run } */

/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" } */

/* { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" } */

/* { dg-additional-options "-Wuninitialized" } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop 0] }
   { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#include <stdlib.h>

void gangs (void)
{
  double res[65536];
  int i;

#pragma acc parallel copyout(res) num_gangs(64) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  {
    int i, j;
#pragma acc loop collapse(2) gang /* { dg-line l_loop[incr c_loop] } */
    /* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    /* { dg-note {variable 'tmpvar' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
       But, with optimizations enabled, per the '*.ssa' dump ('gcc/tree-ssa.c:execute_update_addresses_taken'):
           No longer having address taken: tmpvar
           Now a gimple register: tmpvar
       However, 'tmpvar' remains in the candidate set:
       { dg-note {variable 'tmpvar' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_loop$c_loop }
       Now, for GCN offloading, 'adjust_private_decl' does the privatization change right away:
       { dg-note {variable 'tmpvar' adjusted for OpenACC privatization level: 'gang'} "" { target openacc_radeon_accel_selected } l_loop$c_loop }
       For nvptx offloading however, we first mark up 'tmpvar', and then later apply the privatization change -- or, with optimizations enabled, don't, because we then don't actually call 'expand_var_decl'.
       { dg-note {variable 'tmpvar' adjusted for OpenACC privatization level: 'gang'} "" { target { openacc_nvidia_accel_selected && { ! __OPTIMIZE__ } } } l_loop$c_loop }
       { dg-bogus {note: variable 'tmpvar' adjusted for OpenACC privatization level: 'gang'} "" { target { openacc_nvidia_accel_selected && __OPTIMIZE__ } } l_loop$c_loop }
  */
    /* { dg-note {variable 'tmpref' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    for (i = 0; i < 256; i++)
      {
	for (j = 0; j < 256; j++)
	  {
	    int tmpvar;
	    int &tmpref = tmpvar;
	    tmpref = (i * 256 + j) * 97;
	    res[i * 256 + j] = tmpref;
	  }
      }
  }

  for (i = 0; i < 65536; i++)
    if (res[i] != i * 97)
      abort ();
}

void workers (void)
{
  double res[65536];
  int i;

#pragma acc parallel copyout(res) num_gangs(64) num_workers(64) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-warning "using .num_workers \\(32\\)., ignoring 64" "" { target openacc_nvidia_accel_selected } l_compute$c_compute } */
  {
    int i, j;
#pragma acc loop gang /* { dg-line l_loop[incr c_loop] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    for (i = 0; i < 256; i++)
      {
#pragma acc loop worker /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
	/* { dg-note {variable 'tmpvar' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
	   { dg-note {variable 'tmpvar' ought to be adjusted for OpenACC privatization level: 'worker'} "" { target *-*-* } l_loop$c_loop }
	   { dg-note {variable 'tmpvar' adjusted for OpenACC privatization level: 'worker'} "TODO" { target { ! openacc_host_selected } xfail *-*-* } l_loop$c_loop } */
	/* { dg-note {variable 'tmpref' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
	for (j = 0; j < 256; j++)
	  {
	    int tmpvar;
	    int &tmpref = tmpvar;
	    tmpref = (i * 256 + j) * 99;
	    res[i * 256 + j] = tmpref;
	  }
      }
  }

  for (i = 0; i < 65536; i++)
    if (res[i] != i * 99)
      abort ();
}

void vectors (void)
{
  double res[65536];
  int i;

#pragma acc parallel copyout(res) num_gangs(64) num_workers(64) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-warning "using .num_workers \\(32\\)., ignoring 64" "" { target openacc_nvidia_accel_selected } l_compute$c_compute } */
  {
    int i, j;
#pragma acc loop gang worker /* { dg-line l_loop[incr c_loop] } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    for (i = 0; i < 256; i++)
      {
#pragma acc loop vector /* { dg-line l_loop[incr c_loop] } */
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
	/* { dg-note {variable 'tmpvar' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
	   { dg-note {variable 'tmpvar' ought to be adjusted for OpenACC privatization level: 'vector'} "" { target *-*-* } l_loop$c_loop }
	   { dg-note {variable 'tmpvar' adjusted for OpenACC privatization level: 'vector'} "TODO" { target { ! openacc_host_selected } } l_loop$c_loop } */
	/* { dg-note {variable 'tmpref' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
	for (j = 0; j < 256; j++)
	  {
	    int tmpvar;
	    int &tmpref = tmpvar;
	    tmpref = (i * 256 + j) * 101;
	    res[i * 256 + j] = tmpref;
	  }
      }
  }

  for (i = 0; i < 65536; i++)
    if (res[i] != i * 101)
      abort ();
}

void gangs_workers_vectors (void)
{
  double res[65536];
  int i;

#pragma acc parallel copyout(res) num_gangs(64) num_workers(64) /* { dg-line l_compute[incr c_compute] } */
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute } */
  /* { dg-warning "using .num_workers \\(32\\)., ignoring 64" "" { target openacc_nvidia_accel_selected } l_compute$c_compute } */
  {
    int i, j;
#pragma acc loop collapse(2) gang worker vector /* { dg-line l_loop[incr c_loop] } */
    /* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    /* { dg-note {variable 'tmpvar' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
       { dg-note {variable 'tmpvar' ought to be adjusted for OpenACC privatization level: 'vector'} "" { target *-*-* } l_loop$c_loop }
       { dg-note {variable 'tmpvar' adjusted for OpenACC privatization level: 'vector'} "TODO" { target { ! openacc_host_selected } } l_loop$c_loop } */
    /* { dg-note {variable 'tmpref' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop } */
    for (i = 0; i < 256; i++)
      {
	for (j = 0; j < 256; j++)
	  {
	    int tmpvar;
	    int &tmpref = tmpvar;
	    tmpref = (i * 256 + j) * 103;
	    res[i * 256 + j] = tmpref;
	  }
      }
  }

  for (i = 0; i < 65536; i++)
    if (res[i] != i * 103)
      abort ();
}

int main (int argc, char *argv[])
{
  gangs ();
  workers ();
  vectors ();
  gangs_workers_vectors ();
  return 0;
}
