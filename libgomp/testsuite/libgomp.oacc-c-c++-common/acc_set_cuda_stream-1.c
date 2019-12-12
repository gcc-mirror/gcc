/* Verify expected nvptx plugin behavior for "acc_set_cuda_stream" for
   "acc_async_sync".  */

/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-set-target-env-var GOMP_DEBUG "1" } */

#undef NDEBUG
#include <assert.h>
#include <openacc.h>

int main(void)
{
  int async = 42;

  /* Initialize.  */
#pragma acc parallel async(acc_async_sync)
      ;
#pragma acc parallel async(async)
      ;
#pragma acc wait

  void *cuda_stream_sync = acc_get_cuda_stream (acc_async_sync);
  assert (cuda_stream_sync == NULL);
  void *cuda_stream_async = acc_get_cuda_stream (async);
  assert (cuda_stream_async != NULL);
  int ret = acc_set_cuda_stream (acc_async_sync, cuda_stream_async);
  assert (ret == 0);
  void *cuda_stream_sync_ = acc_get_cuda_stream (acc_async_sync);
  assert (cuda_stream_sync_ == cuda_stream_sync);
  void *cuda_stream_async_ = acc_get_cuda_stream (async);
  assert (cuda_stream_async_ == cuda_stream_async);

#pragma acc parallel async(acc_async_sync)
      ;
#pragma acc parallel async(async)
      ;
#pragma acc wait

  return 0;
}

/* { dg-output "Refusing request to set CUDA stream associated with \"acc_async_sync\"" } */
