/* OpenACC parallelism dimensions clauses: num_gangs, num_workers,
   vector_length.  */

/* Copied from '../libgomp.oacc-c-c++-common/parallel-dims.c'.  */

/* Used by 'parallel-dims.f90'.  */

#include <gomp-constants.h>

#pragma acc routine seq
/* static */ int acc_gang ()
{
  return __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
}

#pragma acc routine seq
/* static */ int acc_worker ()
{
  return __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
}

#pragma acc routine seq
/* static */ int acc_vector ()
{
  return __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
}
