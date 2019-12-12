/* OpenACC parallelism dimensions clauses: num_gangs, num_workers,
   vector_length.  */

/* See also '../libgomp.oacc-fortran/parallel-dims.f90'.  */

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
  else if (acc_on_device ((int) acc_device_nvidia))
    return __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
  else
    __builtin_abort ();
}

#pragma acc routine seq
static unsigned int __attribute__ ((optimize ("O2"))) acc_worker ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia))
    return __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
  else
    __builtin_abort ();
}

#pragma acc routine seq
static unsigned int __attribute__ ((optimize ("O2"))) acc_vector ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia))
    return __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
  else
    __builtin_abort ();
}


int main ()
{
  acc_init (acc_device_default);

  /* OpenACC parallel construct.  */

  /* Non-positive value.  */

  /* GR, WS, VS.  */
  {
#define GANGS 0 /* { dg-warning "'num_gangs' value must be positive" "" { target c } } */
    int gangs_actual = GANGS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (gangs_actual) \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max) \
  num_gangs (GANGS) /* { dg-warning "'num_gangs' value must be positive" "" { target c++ } } */
    {
      /* We're actually executing with num_gangs (1).  */
      gangs_actual = 1;
      for (int i = 100 * gangs_actual; i > -100 * gangs_actual; --i)
	{
	  /* <https://gcc.gnu.org/PR80547>.  */
#if 0
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
#else
	  int gangs = acc_gang ();
	  gangs_min = (gangs_min < gangs) ? gangs_min : gangs;
	  gangs_max = (gangs_max > gangs) ? gangs_max : gangs;
	  int workers = acc_worker ();
	  workers_min = (workers_min < workers) ? workers_min : workers;
	  workers_max = (workers_max > workers) ? workers_max : workers;
	  int vectors = acc_vector ();
	  vectors_min = (vectors_min < vectors) ? vectors_min : vectors;
	  vectors_max = (vectors_max > vectors) ? vectors_max : vectors;
#endif
	}
    }
    if (gangs_actual != 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != gangs_actual - 1
	|| workers_min != 0 || workers_max != 0
	|| vectors_min != 0 || vectors_max != 0)
      __builtin_abort ();
#undef GANGS
  }

  /* GP, WS, VS.  */
  {
#define GANGS 0 /* { dg-warning "'num_gangs' value must be positive" "" { target c } } */
    int gangs_actual = GANGS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (gangs_actual) \
  num_gangs (GANGS) /* { dg-warning "'num_gangs' value must be positive" "" { target c++ } } */
    {
      /* We're actually executing with num_gangs (1).  */
      gangs_actual = 1;
#pragma acc loop gang reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * gangs_actual; i > -100 * gangs_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (gangs_actual != 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != gangs_actual - 1
	|| workers_min != 0 || workers_max != 0
	|| vectors_min != 0 || vectors_max != 0)
      __builtin_abort ();
#undef GANGS
  }

  /* GR, WP, VS.  */
  {
#define WORKERS 0 /* { dg-warning "'num_workers' value must be positive" "" { target c } } */
    int workers_actual = WORKERS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (workers_actual) \
  num_workers (WORKERS) /* { dg-warning "'num_workers' value must be positive" "" { target c++ } } */
    {
      /* We're actually executing with num_workers (1).  */
      workers_actual = 1;
#pragma acc loop worker reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * workers_actual; i > -100 * workers_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (workers_actual != 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != 0
	|| workers_min != 0 || workers_max != workers_actual - 1
	|| vectors_min != 0 || vectors_max != 0)
      __builtin_abort ();
#undef WORKERS
  }

  /* GR, WS, VP.  */
  {
#define VECTORS 0 /* { dg-warning "'vector_length' value must be positive" "" { target c } } */
    int vectors_actual = VECTORS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (vectors_actual) /* { dg-warning "using vector_length \\(32\\), ignoring 1" "" { target openacc_nvidia_accel_selected } } */ \
  vector_length (VECTORS) /* { dg-warning "'vector_length' value must be positive" "" { target c++ } } */
    {
      /* We're actually executing with vector_length (1), just the GCC nvptx
	 back end enforces vector_length (32).  */
      if (acc_on_device (acc_device_nvidia))
	vectors_actual = 32;
      else
	vectors_actual = 1;
#pragma acc loop vector reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * vectors_actual; i > -100 * vectors_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (acc_get_device_type () == acc_device_nvidia)
      {
	if (vectors_actual != 32)
	  __builtin_abort ();
      }
    else
      if (vectors_actual != 1)
	__builtin_abort ();
    if (gangs_min != 0 || gangs_max != 0
	|| workers_min != 0 || workers_max != 0
	|| vectors_min != 0 || vectors_max != vectors_actual - 1)
      __builtin_abort ();
#undef VECTORS
  }


  /* High value.  */
  
  /* GR, WS, VS.  */
  {
    /* There is no actual limit for the number of gangs, so we try with a
       rather high value.  */
    int gangs = 12345;
    int gangs_actual = gangs;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (gangs_actual) \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max) \
  num_gangs (gangs)
    {
      if (acc_on_device (acc_device_host))
	{
	  /* We're actually executing with num_gangs (1).  */
	  gangs_actual = 1;
	}
      /* As we're executing GR not GP, don't multiply with a "gangs_actual"
	 factor.  */
      for (int i = 100 /* * gangs_actual */; i > -100 /* * gangs_actual */; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (gangs_actual < 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != gangs_actual - 1
	|| workers_min != 0 || workers_max != 0
	|| vectors_min != 0 || vectors_max != 0)
      __builtin_abort ();
  }

  /* GP, WS, VS.  */
  {
    /* There is no actual limit for the number of gangs, so we try with a
       rather high value.  */
    int gangs = 12345;
    int gangs_actual = gangs;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (gangs_actual) \
  num_gangs (gangs)
    {
      if (acc_on_device (acc_device_host))
	{
	  /* We're actually executing with num_gangs (1).  */
	  gangs_actual = 1;
	}
#pragma acc loop gang reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * gangs_actual; i > -100 * gangs_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (gangs_actual < 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != gangs_actual - 1
	|| workers_min != 0 || workers_max != 0
	|| vectors_min != 0 || vectors_max != 0)
      __builtin_abort ();
  }

  /* GR, WP, VS.  */
  {
    /* We try with an outrageously large value. */
#define WORKERS 2 << 20
    int workers_actual = WORKERS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (workers_actual) /* { dg-warning "using num_workers \\(32\\), ignoring 2097152" "" { target openacc_nvidia_accel_selected } } */ \
  num_workers (WORKERS)
    {
      if (acc_on_device (acc_device_host))
	{
	  /* We're actually executing with num_workers (1).  */
	  workers_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	{
	  /* The GCC nvptx back end enforces num_workers (32).  */
	  workers_actual = 32;
	}
      else
	__builtin_abort ();
#pragma acc loop worker reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * workers_actual; i > -100 * workers_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (workers_actual < 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != 0
	|| workers_min != 0 || workers_max != workers_actual - 1
	|| vectors_min != 0 || vectors_max != 0)
      __builtin_abort ();
#undef WORKERS
  }

  /* GR, WP, VS.  */
  {
    /* We try with an outrageously large value. */
    int workers = 2 << 20;
    /* For nvptx offloading, this one will not result in "using num_workers
       (32), ignoring runtime setting", and will in fact try to launch with
       "num_workers (workers)", which will run into "libgomp: cuLaunchKernel
       error: invalid argument".  So, limit ourselves here.  */
    if (acc_get_device_type () == acc_device_nvidia)
      workers = 32;
    int workers_actual = workers;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (workers_actual) \
  num_workers (workers)
    {
      if (acc_on_device (acc_device_host))
	{
	  /* We're actually executing with num_workers (1).  */
	  workers_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	{
	  /* We're actually executing with num_workers (32).  */
	  /* workers_actual = 32; */
	}
      else
	__builtin_abort ();
#pragma acc loop worker reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * workers_actual; i > -100 * workers_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (workers_actual < 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != 0
	|| workers_min != 0 || workers_max != workers_actual - 1
	|| vectors_min != 0 || vectors_max != 0)
      __builtin_abort ();
  }

  /* GR, WS, VP.  */
  {
    /* We try with an outrageously large value. */
#define VECTORS 2 << 20
    int vectors_actual = VECTORS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (vectors_actual) /* { dg-warning "using vector_length \\(1024\\), ignoring 2097152" "" { target openacc_nvidia_accel_selected } } */ \
  vector_length (VECTORS)
    {
      if (acc_on_device (acc_device_host))
	{
	  /* We're actually executing with vector_length (1).  */
	  vectors_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  vectors_actual = 1024;
	}
      else
	__builtin_abort ();
#pragma acc loop vector reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * vectors_actual; i > -100 * vectors_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (vectors_actual < 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != 0
	|| workers_min != 0 || workers_max != 0
	|| vectors_min != 0 || vectors_max != vectors_actual - 1)
      __builtin_abort ();
#undef VECTORS
  }

  /* GR, WS, VP.  */
  {
    /* We try with an outrageously large value. */
    int vectors = 2 << 20;
    int vectors_actual = vectors;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (vectors_actual) /* { dg-warning "using vector_length \\(32\\), ignoring runtime setting" "" { target openacc_nvidia_accel_selected } } */ \
  vector_length (vectors)
    {
      if (acc_on_device (acc_device_host))
	{
	  /* We're actually executing with vector_length (1).  */
	  vectors_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  vectors_actual = 32;
	}
      else
	__builtin_abort ();
#pragma acc loop vector reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * vectors_actual; i > -100 * vectors_actual; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (vectors_actual < 1)
      __builtin_abort ();
    if (gangs_min != 0 || gangs_max != 0
	|| workers_min != 0 || workers_max != 0
	|| vectors_min != 0 || vectors_max != vectors_actual - 1)
      __builtin_abort ();
  }


  /* Composition of GP, WP, VP.  */
  {
    int gangs = 12345;
    /* With nvptx offloading, multi-level reductions apparently are very slow
       in the following case.  So, limit ourselves here.  */
    if (acc_get_device_type () == acc_device_nvidia)
      gangs = 3;
    int gangs_actual = gangs;
#define WORKERS 3
    int workers_actual = WORKERS;
#define VECTORS 11
    int vectors_actual = VECTORS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel copy (gangs_actual, workers_actual, vectors_actual) /* { dg-warning "using vector_length \\(32\\), ignoring 11" "" { target openacc_nvidia_accel_selected } } */ \
  num_gangs (gangs) \
  num_workers (WORKERS) \
  vector_length (VECTORS)
    {
      if (acc_on_device (acc_device_host))
	{
	  /* We're actually executing with num_gangs (1), num_workers (1),
	     vector_length (1).  */
	  gangs_actual = 1;
	  workers_actual = 1;
	  vectors_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  vectors_actual = 32;
	}
      else
	__builtin_abort ();
#pragma acc loop gang reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100 * gangs_actual; i > -100 * gangs_actual; --i)
#pragma acc loop worker reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	for (int j = 100 * workers_actual; j > -100 * workers_actual; --j)
#pragma acc loop vector reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	  for (int k = 100 * vectors_actual; k > -100 * vectors_actual; --k)
	    {
	      gangs_min = gangs_max = acc_gang ();
	      workers_min = workers_max = acc_worker ();
	      vectors_min = vectors_max = acc_vector ();
	    }
    }
    if (gangs_min != 0 || gangs_max != gangs_actual - 1
	|| workers_min != 0 || workers_max != workers_actual - 1
	|| vectors_min != 0 || vectors_max != vectors_actual - 1)
      __builtin_abort ();
#undef VECTORS
#undef WORKERS
  }


  /* OpenACC kernels construct.  */

  /* We can't test parallelized OpenACC kernels constructs in this way: use of
     the acc_gang, acc_worker, acc_vector functions will make the construct
     unparallelizable.  */


  /* Unparallelized OpenACC kernels constructs must get launched as 1 x 1 x 1
     kernels.  */
  {
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc kernels
    {
      /* This is to make the OpenACC kernels construct unparallelizable.  */
      asm volatile ("" : : : "memory");

#pragma acc loop reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100; i > -100; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (gangs_min != 0 || gangs_max != 1 - 1
	|| workers_min != 0 || workers_max != 1 - 1
	|| vectors_min != 0 || vectors_max != 1 - 1)
      __builtin_abort ();
  }


  /* Unparallelized OpenACC kernels constructs must get launched as 1 x 1 x 1
     kernels even when there are explicit num_gangs, num_workers, or
     vector_length clauses.  */
  {
    int gangs = 5;
#define WORKERS 5
#define VECTORS 13
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc kernels \
  num_gangs (gangs) \
  num_workers (WORKERS) \
  vector_length (VECTORS)
    {
      /* This is to make the OpenACC kernels construct unparallelizable.  */
      asm volatile ("" : : : "memory");

#pragma acc loop reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100; i > -100; --i)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (gangs_min != 0 || gangs_max != 1 - 1
	|| workers_min != 0 || workers_max != 1 - 1
	|| vectors_min != 0 || vectors_max != 1 - 1)
      __builtin_abort ();
#undef VECTORS
#undef WORKERS
  }


  /* OpenACC serial construct.  */

  /* GR, WS, VS.  */
  {
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc serial /* { dg-warning "using vector_length \\(32\\), ignoring 1" "" { target openacc_nvidia_accel_selected } } */ \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
    {
      for (int i = 100; i > -100; i--)
	{
	  gangs_min = gangs_max = acc_gang ();
	  workers_min = workers_max = acc_worker ();
	  vectors_min = vectors_max = acc_vector ();
	}
    }
    if (gangs_min != 0 || gangs_max != 1 - 1
	|| workers_min != 0 || workers_max != 1 - 1
	|| vectors_min != 0 || vectors_max != 1 - 1)
      __builtin_abort ();
  }

  /* Composition of GP, WP, VP.  */
  {
    int vectors_actual = 1;  /* Implicit 'vector_length (1)' clause.  */
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc serial copy (vectors_actual) /* { dg-warning "using vector_length \\(32\\), ignoring 1" "" { target openacc_nvidia_accel_selected } } */ \
  copy (gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max)
    {
      if (acc_on_device (acc_device_nvidia))
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  /* It's unclear if that's actually permissible here;
	     <https://github.com/OpenACC/openacc-spec/issues/238> "OpenACC
	     'serial' construct might not actually be serial".  */
	  vectors_actual = 32;
	}
#pragma acc loop gang reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      for (int i = 100; i > -100; i--)
#pragma acc loop worker reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	for (int j = 100; j > -100; j--)
#pragma acc loop vector reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	  for (int k = 100 * vectors_actual; k > -100 * vectors_actual; k--)
	    {
	      gangs_min = gangs_max = acc_gang ();
	      workers_min = workers_max = acc_worker ();
	      vectors_min = vectors_max = acc_vector ();
	    }
    }
    if (acc_get_device_type () == acc_device_nvidia)
      {
	if (vectors_actual != 32)
	  __builtin_abort ();
      }
    else
      if (vectors_actual != 1)
	__builtin_abort ();
    if (gangs_min != 0 || gangs_max != 1 - 1
	|| workers_min != 0 || workers_max != 1 - 1
	|| vectors_min != 0 || vectors_max != vectors_actual - 1)
      __builtin_abort ();
  }


  return 0;
}
