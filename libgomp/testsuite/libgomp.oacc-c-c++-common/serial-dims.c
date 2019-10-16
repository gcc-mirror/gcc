/* OpenACC dimensions with the serial construct.  */

#include <limits.h>
#include <openacc.h>
#include <gomp-constants.h>

/* TODO: "(int) acc_device_*" casts because of the C++ acc_on_device wrapper
   not behaving as expected for -O0.  */
#pragma acc routine seq
static unsigned int __attribute__ ((optimize ("O2"))) acc_gang ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia)
	   || acc_on_device ((int) acc_device_gcn))
    return __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
  else
    __builtin_abort ();
}

#pragma acc routine seq
static unsigned int __attribute__ ((optimize ("O2"))) acc_worker ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia)
	   || acc_on_device ((int) acc_device_gcn))
    return __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
  else
    __builtin_abort ();
}

#pragma acc routine seq
static unsigned int __attribute__ ((optimize ("O2"))) acc_vector ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia)
	   || acc_on_device ((int) acc_device_gcn))
    return __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
  else
    __builtin_abort ();
}


int main ()
{
  acc_init (acc_device_default);

  /* Serial OpenACC constructs must get launched as 1 x 1 x 1.  */
  {
    int gangs_min, gangs_max;
    int workers_min, workers_max;
    int vectors_min, vectors_max;
    int gangs_actual, workers_actual, vectors_actual;
    int i, j, k;

    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
    gangs_actual = workers_actual = vectors_actual = 1;
#pragma acc serial
    /* { dg-warning "region contains gang partitioned code but is not gang partitioned" "" { target *-*-* } 61 } */
    /* { dg-warning "region contains worker partitioned code but is not worker partitioned" "" { target *-*-* } 61 } */
    /* { dg-warning "region contains vector partitioned code but is not vector partitioned" "" { target *-*-* } 61 } */
    /* { dg-warning "using vector_length \\(32\\), ignoring 1" "" { target openacc_nvidia_accel_selected } 61 } */
    {
      if (acc_on_device (acc_device_nvidia))
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  vectors_actual = 32;
	}
      else if (acc_on_device (acc_device_gcn))
	{
	  /* AMD GCN relies on the autovectorizer for the vector dimension:
	     the loop below isn't likely to be vectorized, so vectors_actual
	     is effectively 1.  */
	  vectors_actual = 1;
	}
      else if (!acc_on_device (acc_device_host))
	__builtin_abort ();
#pragma acc loop gang \
  reduction (min: gangs_min, workers_min, vectors_min) \
  reduction (max: gangs_max, workers_max, vectors_max)
      for (i = 100 * gangs_actual; i > -100 * gangs_actual; i--)
#pragma acc loop worker \
  reduction (min: gangs_min, workers_min, vectors_min) \
  reduction (max: gangs_max, workers_max, vectors_max)
	for (j = 100 * workers_actual; j > -100 * workers_actual; j--)
#pragma acc loop vector \
  reduction (min: gangs_min, workers_min, vectors_min) \
  reduction (max: gangs_max, workers_max, vectors_max)
	  for (k = 100 * vectors_actual; k > -100 * vectors_actual; k--)
	    {
	      gangs_min = gangs_max = acc_gang ();
	      workers_min = workers_max = acc_worker ();
	      vectors_min = vectors_max = acc_vector ();
	    }
      if (gangs_min != 0 || gangs_max != gangs_actual - 1
	  || workers_min != 0 || workers_max != workers_actual - 1
	  || vectors_min != 0 || vectors_max != vectors_actual - 1)
	__builtin_abort ();
    }
  }

  return 0;
}
