/* OpenACC parallelism dimensions clauses: num_gangs, num_workers,
   vector_length.  */

/* { dg-additional-options "-DEXPENSIVE" { target run_expensive_tests } } */

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
   { dg-line l_dummy[variable c_compute 0 c_loop_i 0 c_loop_j 0 c_loop_k 0] }
   { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
   "WARNING: dg-line var l_dummy defined, but not used".  */

/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

/* See also '../libgomp.oacc-fortran/parallel-dims.f90'.  */

#include <limits.h>
#include <openacc.h>
#include <gomp-constants.h>

#pragma acc routine seq
inline __attribute__ ((always_inline))
static int acc_gang ()
{
  return __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
}

#pragma acc routine seq
inline __attribute__ ((always_inline))
static int acc_worker ()
{
  return __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
}

#pragma acc routine seq
inline __attribute__ ((always_inline))
static int acc_vector ()
{
  return __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
}

#ifdef EXPENSIVE
#define N 100
#else
#define N 50
#endif

int main ()
{
  acc_init (acc_device_default);

  /* OpenACC parallel construct.  */

  /* Non-positive value.  */

  /* GR, WS, VS.  */
  {
#define GANGS 0
    /* { dg-warning {'num_gangs' value must be positive} {} { target c } .-1 } */
    int gangs_actual = GANGS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (gangs_actual) \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max) \
  num_gangs (GANGS)
    /* { dg-note {in expansion of macro 'GANGS'} {} { target c } .-1 } */
    /* { dg-warning {'num_gangs' value must be positive} {} { target c++ } .-2 } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    {
      /* We're actually executing with num_gangs (1).  */
      gangs_actual = 1;
      for (int i = N * gangs_actual; i > -N * gangs_actual; --i)
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

  /* GP, WS, VS.  */
  {
#define GANGS 0
    /* { dg-warning {'num_gangs' value must be positive} {} { target c } .-1 } */
    int gangs_actual = GANGS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (gangs_actual) \
  num_gangs (GANGS)
    /* { dg-note {in expansion of macro 'GANGS'} {} { target c } .-1 } */
    /* { dg-warning {'num_gangs' value must be positive} {} { target c++ } .-2 } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {region contains gang partitioned code but is not gang partitioned} {} { target *-*-* } l_compute$c_compute } */
    {
      /* We're actually executing with num_gangs (1).  */
      gangs_actual = 1;
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  gang \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC gang loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * gangs_actual; i > -N * gangs_actual; --i)
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
#define WORKERS 0
    /* { dg-warning {'num_workers' value must be positive} {} { target c } .-1 } */
    int workers_actual = WORKERS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (workers_actual) \
  num_workers (WORKERS)
    /* { dg-note {in expansion of macro 'WORKERS'} {} { target c } .-1 } */
    /* { dg-warning {'num_workers' value must be positive} {} { target c++ } .-2 } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {region contains worker partitioned code but is not worker partitioned} {} { target *-*-* } l_compute$c_compute } */
    {
      /* We're actually executing with num_workers (1).  */
      workers_actual = 1;
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  worker \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC worker loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * workers_actual; i > -N * workers_actual; --i)
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
#define VECTORS 0
    /* { dg-warning {'vector_length' value must be positive} {} { target c } .-1 } */
    int vectors_actual = VECTORS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (vectors_actual) \
  vector_length (VECTORS)
    /* { dg-note {in expansion of macro 'VECTORS'} {} { target c } .-1 } */
    /* { dg-warning {'vector_length' value must be positive} {} { target c++ } .-2 } */
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {region contains vector partitioned code but is not vector partitioned} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected } l_compute$c_compute } */
    {
      /* We're actually executing with vector_length (1), just the GCC nvptx
	 back end enforces vector_length (32).  */
      if (acc_on_device (acc_device_nvidia))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	vectors_actual = 32;
      else
	vectors_actual = 1;
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  vector \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * vectors_actual; i > -N * vectors_actual; --i)
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
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (gangs_actual) \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max) \
  num_gangs (gangs)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-bogus {warning: region is gang partitioned but does not contain gang partitioned code} {TODO 'reduction'} { xfail *-*-* } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_host))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with num_gangs (1).  */
	  gangs_actual = 1;
	}
      /* As we're executing GR not GP, don't multiply with a "gangs_actual"
	 factor.  */
      for (int i = N /* * gangs_actual */; i > -N /* * gangs_actual */; --i)
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
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (gangs_actual) \
  num_gangs (gangs)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_host))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with num_gangs (1).  */
	  gangs_actual = 1;
	}
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  gang \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC gang loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * gangs_actual; i > -N * gangs_actual; --i)
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
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (workers_actual) \
  num_workers (WORKERS)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {using 'num_workers \(32\)', ignoring 2097152} {} { target openacc_nvidia_accel_selected } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_host))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with num_workers (1).  */
	  workers_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC nvptx back end enforces num_workers (32).  */
	  workers_actual = 32;
	}
      else if (acc_on_device (acc_device_radeon))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC GCN back end is limited to num_workers (16).  */
	  workers_actual = 16;
	}
      else
	__builtin_abort ();
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  worker \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC worker loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * workers_actual; i > -N * workers_actual; --i)
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
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (workers_actual) \
  num_workers (workers)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_host))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with num_workers (1).  */
	  workers_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with num_workers (32).  */
	  /* workers_actual = 32; */
	}
      else if (acc_on_device (acc_device_radeon))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC GCN back end is limited to num_workers (16).  */
	  workers_actual = 16;
	}
      else
	__builtin_abort ();
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  worker \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC worker loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * workers_actual; i > -N * workers_actual; --i)
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
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (vectors_actual) \
  vector_length (VECTORS)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {using 'vector_length \(1024\)', ignoring 2097152} {} { target openacc_nvidia_accel_selected } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_host))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with vector_length (1).  */
	  vectors_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC nvptx back end reduces to vector_length (1024).  */
	  vectors_actual = 1024;
	}
      else if (acc_on_device (acc_device_radeon))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC GCN back end enforces vector_length (1): autovectorize. */
	  vectors_actual = 1;
	}
      else
	__builtin_abort ();
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  vector \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * vectors_actual; i > -N * vectors_actual; --i)
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
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (vectors_actual) \
  vector_length (vectors)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {using 'vector_length \(32\)', ignoring runtime setting} {} { target openacc_nvidia_accel_selected } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_host))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with vector_length (1).  */
	  vectors_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  vectors_actual = 32;
	}
      else if (acc_on_device (acc_device_radeon))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* Because of the way vectors are implemented for GCN, a vector loop
	     containing a seq routine call will not vectorize calls to that
	     routine.  Hence, we'll only get one "vector".  */
	  vectors_actual = 1;
	}
      else
	__builtin_abort ();
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  vector \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC vector loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * vectors_actual; i > -N * vectors_actual; --i)
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
    /* Similar appears to be true for GCN.  */
    if (acc_get_device_type () == acc_device_radeon)
      gangs = 3;
    int gangs_actual = gangs;
#define WORKERS 3
    int workers_actual = WORKERS;
#define VECTORS 11
    int vectors_actual = VECTORS;
    int gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max;
    gangs_min = workers_min = vectors_min = INT_MAX;
    gangs_max = workers_max = vectors_max = INT_MIN;
#pragma acc parallel /* { dg-line l_compute[incr c_compute] } */ \
  copy (gangs_actual, workers_actual, vectors_actual) \
  num_gangs (gangs) \
  num_workers (WORKERS) \
  vector_length (VECTORS)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {using 'vector_length \(32\)', ignoring 11} {} { target openacc_nvidia_accel_selected } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_host))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* We're actually executing with num_gangs (1), num_workers (1),
	     vector_length (1).  */
	  gangs_actual = 1;
	  workers_actual = 1;
	  vectors_actual = 1;
	}
      else if (acc_on_device (acc_device_nvidia))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  vectors_actual = 32;
	}
      else if (acc_on_device (acc_device_radeon))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* See above comments about GCN vectors_actual.  */
	  vectors_actual = 1;
	}
      else
	__builtin_abort ();
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  gang \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC gang loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N * gangs_actual; i > -N * gangs_actual; --i)
#pragma acc loop /* { dg-line l_loop_j[incr c_loop_j] } */ \
  worker \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-note {variable 'k' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-optimized {assigned OpenACC worker loop parallelism} {} { target *-*-* } l_loop_j$c_loop_j } */
	for (int j = N * workers_actual; j > -N * workers_actual; --j)
#pragma acc loop /* { dg-line l_loop_k[incr c_loop_k] } */ \
  vector \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	  /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k$c_loop_k } */
	  /* { dg-optimized {assigned OpenACC vector loop parallelism} {} { target *-*-* } l_loop_k$c_loop_k } */
	  for (int k = N * vectors_actual; k > -N * vectors_actual; --k)
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
#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'vectors_max' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'vectors_max' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'vectors_min' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'vectors_min' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'workers_max' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'workers_max' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'workers_min' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'workers_min' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'gangs_max' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'gangs_max' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'gangs_min' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'gangs_min' made addressable} {} { target *-*-* } l_compute$c_compute } */
    {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N; i > -N; --i)
	{
	  /* This is to make the loop unparallelizable.  */
	  asm volatile ("" : : : "memory");

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
#pragma acc kernels /* { dg-line l_compute[incr c_compute] } */ \
  num_gangs (gangs) \
  num_workers (WORKERS) \
  vector_length (VECTORS)
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'vectors_max' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'vectors_max' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'vectors_min' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'vectors_min' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'workers_max' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'workers_max' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'workers_min' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'workers_min' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'gangs_max' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'gangs_max' made addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-note {OpenACC 'kernels' decomposition: variable 'gangs_min' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
       { dg-note {variable 'gangs_min' made addressable} {} { target *-*-* } l_compute$c_compute } */
    {
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N; i > -N; --i)
	{
	  /* This is to make the loop unparallelizable.  */
	  asm volatile ("" : : : "memory");

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
#pragma acc serial /* { dg-line l_compute[incr c_compute] } */ \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-warning {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected } l_compute$c_compute } */
    {
      for (int i = N; i > -N; i--)
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
#pragma acc serial /* { dg-line l_compute[incr c_compute] } */ \
  copy (vectors_actual) \
  copy (gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max)
    /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute } */
    /* { dg-bogus {warning: region contains gang partitioned code but is not gang partitioned} {TODO 'serial'} { xfail *-*-* } l_compute$c_compute }
       { dg-bogus {warning: region contains worker partitioned code but is not worker partitioned} {TODO 'serial'} { xfail *-*-* } l_compute$c_compute }
       { dg-bogus {warning: region contains vector partitioned code but is not vector partitioned} {TODO 'serial'} { xfail *-*-* } l_compute$c_compute } */
    /* { dg-warning {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected } l_compute$c_compute } */
    {
      if (acc_on_device (acc_device_nvidia))
	/* { dg-optimized {assigned OpenACC seq loop parallelism} {} { target { c++ && { ! __OPTIMIZE__ } } } .-1 }
	   ..., as without optimizations, we're not inlining the C++ 'acc_on_device' wrapper.  */
	{
	  /* The GCC nvptx back end enforces vector_length (32).  */
	  /* It's unclear if that's actually permissible here;
	     <https://github.com/OpenACC/openacc-spec/issues/238> "OpenACC
	     'serial' construct might not actually be serial".  */
	  vectors_actual = 32;
	}
#pragma acc loop /* { dg-line l_loop_i[incr c_loop_i] } */ \
  gang \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
      /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i } */
      /* { dg-optimized {assigned OpenACC gang loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i } */
      for (int i = N; i > -N; i--)
#pragma acc loop /* { dg-line l_loop_j[incr c_loop_j] } */ \
  worker \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-note {variable 'k' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j } */
	/* { dg-optimized {assigned OpenACC worker loop parallelism} {} { target *-*-* } l_loop_j$c_loop_j } */
	for (int j = N; j > -N; j--)
#pragma acc loop /* { dg-line l_loop_k[incr c_loop_k] } */ \
  vector \
  reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
	  /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k$c_loop_k } */
	  /* { dg-optimized {assigned OpenACC vector loop parallelism} {} { target *-*-* } l_loop_k$c_loop_k } */
	  for (int k = N * vectors_actual; k > -N * vectors_actual; k--)
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
