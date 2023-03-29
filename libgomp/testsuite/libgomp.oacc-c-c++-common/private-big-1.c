/* Test "big" private data.  */

/* { dg-additional-options -fno-inline } for stable results regarding OpenACC 'routine'.  */

/* { dg-additional-options -fopt-info-all-omp }
   { dg-additional-options --param=openacc-privatization=noisy }
   { dg-additional-options -foffload=-fopt-info-all-omp }
   { dg-additional-options -foffload=--param=openacc-privatization=noisy }
   for testing/documenting aspects of that functionality.  */

/* { dg-additional-options -Wopenacc-parallelism } for testing/documenting
   aspects of that functionality.  */

/* For GCN offloading compilation, we (expectedly) run into a
   'gang-private data-share memory exhausted' error: the default
   '-mgang-private-size' is too small.  Raise it so that 'uint32_t x[344]' plus
   some internal-use data fits in:
   { dg-additional-options -foffload-options=amdgcn-amdhsa=-mgang-private-size=1555 { target openacc_radeon_accel_selected } } */

/* It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
   passed to 'incr' may be unset, and in that case, it will be set to [...]",
   so to maintain compatibility with earlier Tcl releases, we manually
   initialize counter variables:
   { dg-line l_dummy[variable c_compute 0 c_loop 0] }
   { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

#include <assert.h>
#include <stdint.h>


/* Based on 'private-variables.c:loop_g_5'.  */

/* To demonstrate PR105421 "GCN offloading, raised '-mgang-private-size':
   'HSA_STATUS_ERROR_MEMORY_APERTURE_VIOLATION'", a 'struct' indirection, for
   example, has been necessary in combination with a separate routine.  */

struct data
{
  uint32_t *x;
  uint32_t *arr;
  uint32_t i;
};

#pragma acc routine worker
static void
loop_g_5_r(struct data *data)
{
  uint32_t *x = data->x;
  uint32_t *arr = data->arr;
  uint32_t i = data->i;

#pragma acc loop /* { dg-line l_loop[incr c_loop] } */
  /* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
  /* { dg-optimized {assigned OpenACC worker vector loop parallelism} {} { target *-*-* } l_loop$c_loop } */
  for (int j = 0; j < 320; j++)
    arr[i * 320 + j] += x[(i * 320 + j) % 344];
}

void loop_g_5()
{
  uint32_t x[344], i, arr[320 * 320];

  for (i = 0; i < 320 * 320; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr)
  {
    #pragma acc loop gang private(x) /* { dg-line l_loop[incr c_loop] } */
    /* { dg-note {variable 'x' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop$c_loop }
       { dg-note {variable 'x' ought to be adjusted for OpenACC privatization level: 'gang'} {} { target *-*-* } l_loop$c_loop }
       { dg-note {variable 'x' adjusted for OpenACC privatization level: 'gang'} {} { target { ! openacc_host_selected } } l_loop$c_loop } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
    /* { dg-note {variable 'data' declared in block is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop$c_loop }
       { dg-note {variable 'data' ought to be adjusted for OpenACC privatization level: 'gang'} {} { target *-*-* } l_loop$c_loop }
       { dg-note {variable 'data' adjusted for OpenACC privatization level: 'gang'} {} { target { ! openacc_host_selected } } l_loop$c_loop } */
    /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop } */
    /* { dg-optimized {assigned OpenACC gang loop parallelism} {} { target *-*-* } l_loop$c_loop } */
    for (i = 0; i < 320; i++)
      {
        for (int j = 0; j < 344; j++)
	  x[j] = j * (2 + i);

	struct data data = { x, arr, i };
	loop_g_5_r(&data); /* { dg-line l_compute[incr c_compute] } */
	/* { dg-optimized {assigned OpenACC worker vector loop parallelism} {} { target *-*-* } l_compute$c_compute } */
      }
  }

  for (i = 0; i < 320 * 320; i++)
    assert(arr[i] == i + (i % 344) * (2 + (i / 320)));
}


int main ()
{
  loop_g_5();

  return 0;
}
