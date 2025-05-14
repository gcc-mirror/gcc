/* { dg-do run { target { offload_device_nvptx } } } */
/* { dg-do link { target { ! offload_device_nvptx } } } */

/* { dg-require-effective-target openacc_cuda } */
/* { dg-require-effective-target openacc_cudart } */
/* { dg-additional-options "-lcuda -lcudart" } */

/* NOTE: This file is also included by libgomp.c-c++-common/interop-cuda-libonly.c
   to test the fallback version, which defines USE_CUDA_FALLBACK_HEADER.  */

/* Minimal check whether CUDA works - by checking whether the API routines
   seem to work.  This includes a fallback if the header is not
   available.  */

#include <assert.h>
#include <omp.h>

#if __has_include(<cuda.h>) && __has_include(<cudaTypedefs.h>) && __has_include(<cuda_runtime.h>) && !defined(USE_CUDA_FALLBACK_HEADER)
  #include <cuda.h>
  #include <cudaTypedefs.h>
  #include <cuda_runtime.h>

#else
  /* Add a poor man's fallback declaration.  */
  #if USE_CUDA_FALLBACK_HEADER
    // Don't warn.
  #elif !__has_include(<cuda.h>)
    #warning "Using GCC's cuda.h as fallback for cuda.h"
  #elif !__has_include(<cudaTypedefs.h>)
    #warning "Using GCC's cuda.h as fallback for cudaTypedefs.h"
  #else
    #warning "Using GCC's cuda.h as fallback for cuda_runtime.h"
  #endif
  #include "../../../include/cuda/cuda.h"

  typedef int cudaError_t;
  typedef CUstream cudaStream_t;
  enum {
    cudaSuccess = 0
  };

  enum cudaDeviceAttr {
    cudaDevAttrClockRate = 13,
    cudaDevAttrMaxGridDimX = 5
  };

  cudaError_t cudaDeviceGetAttribute (int *, enum cudaDeviceAttr, int);
  cudaError_t cudaStreamQuery(cudaStream_t);
  CUresult cuCtxGetApiVersion(CUcontext, unsigned int *);
  CUresult cuStreamGetCtx (CUstream, CUcontext *);
#endif

int
main ()
{
  int ivar;
  unsigned uvar;
  omp_interop_rc_t res;
  omp_interop_t obj_cuda = omp_interop_none;
  omp_interop_t obj_cuda_driver = omp_interop_none;
  cudaError_t cuda_err;
  CUresult cu_err;

  #pragma omp interop init(target, targetsync, prefer_type("cuda") : obj_cuda) \
		      init(target, targetsync, prefer_type("cuda_driver") : obj_cuda_driver) \

  omp_interop_fr_t fr = (omp_interop_fr_t) omp_get_interop_int (obj_cuda, omp_ipr_fr_id, &res);
  assert (res == omp_irc_success);
  assert (fr == omp_ifr_cuda);

  fr = (omp_interop_fr_t) omp_get_interop_int (obj_cuda_driver, omp_ipr_fr_id, &res);
  assert (res == omp_irc_success);
  assert (fr == omp_ifr_cuda_driver);

  ivar = (int) omp_get_interop_int (obj_cuda, omp_ipr_vendor, &res);
  assert (res == omp_irc_success);
  assert (ivar == 11);

  ivar = (int) omp_get_interop_int (obj_cuda_driver, omp_ipr_vendor, &res);
  assert (res == omp_irc_success);
  assert (ivar == 11);


  /* Check whether the omp_ipr_device -> cudaDevice_t yields a valid device.  */

  CUdevice cu_dev = (int) omp_get_interop_int (obj_cuda_driver, omp_ipr_device, &res);
  assert (res == omp_irc_success);

  /* Assume a clock size is available and > 1 GHz; value is in kHz.  */
  cu_err = cuDeviceGetAttribute (&ivar, cudaDevAttrClockRate, cu_dev);
  assert (cu_err == CUDA_SUCCESS);
  assert (ivar > 1000000 /* kHz */);

  /* Assume that the MaxGridDimX is available and > 1024.  */
  cu_err = cuDeviceGetAttribute (&ivar, cudaDevAttrMaxGridDimX, cu_dev);
  assert (cu_err == CUDA_SUCCESS);
  assert (ivar > 1024);

  int cuda_dev = (int) omp_get_interop_int (obj_cuda, omp_ipr_device, &res);
  assert (res == omp_irc_success);
  assert (cuda_dev == (CUdevice) cu_dev); // Assume they are the same ...

  /* Assume a clock size is available and > 1 GHz; value is in kHz.  */
  cuda_err = cudaDeviceGetAttribute (&ivar, cudaDevAttrClockRate, cuda_dev);
  assert (cuda_err == cudaSuccess);
  assert (ivar > 1000000 /* kHz */);

  /* Assume that the MaxGridDimX is available and > 1024.  */
  cuda_err = cudaDeviceGetAttribute (&ivar, cudaDevAttrMaxGridDimX, cuda_dev);
  assert (cuda_err == cudaSuccess);
  assert (ivar > 1024);




  /* Check whether the omp_ipr_device_context -> CUcontext yields a context.  */

  CUcontext cu_ctx = (CUcontext) omp_get_interop_ptr (obj_cuda_driver, omp_ipr_device_context, &res);
  assert (res == omp_irc_success);

  /* Assume API Version > 0 for Nvidia, cudaErrorNotSupported for AMD.  */
  uvar = 99;
  cu_err = cuCtxGetApiVersion (cu_ctx, &uvar);
  assert (cu_err == CUDA_SUCCESS);
  assert (uvar > 0);


  /* Check whether the omp_ipr_targetsync -> cudaStream_t yields a stream.  */

  cudaStream_t cuda_sm = (cudaStream_t) omp_get_interop_ptr (obj_cuda, omp_ipr_targetsync, &res);
  assert (res == omp_irc_success);

  CUstream cu_sm = (cudaStream_t) omp_get_interop_ptr (obj_cuda_driver, omp_ipr_targetsync, &res);
  assert (res == omp_irc_success);

  assert ((void*) cu_sm != (void*) cuda_sm); // Type compatible but should have created two streams

  int dev_stream = 99;
#if CUDA_VERSION >= 12080
  cuda_err = cudaStreamGetDevice (cuda_sm, &dev_stream);
  assert (cuda_err == cudaSuccess);
#else
  cu_err = cuStreamGetCtx (cu_sm, &cu_ctx) != CUDA_SUCCESS;
  if (cu_err == CUDA_SUCCESS)
    cuda_err = cuCtxPushCurrent (cu_ctx) != CUDA_SUCCESS;
  if (cu_err == CUDA_SUCCESS)
    cuda_err = cuCtxGetDevice (&dev_stream) != CUDA_SUCCESS;
  if (cu_err == CUDA_SUCCESS)
    cu_err = cuCtxPopCurrent (&cu_ctx) != CUDA_SUCCESS;
  assert (cu_err == CUDA_SUCCESS);
#endif
  assert (dev_stream == cuda_dev);

  /* All jobs should have been completed (as there were none none)  */
  cuda_err = cudaStreamQuery (cuda_sm);
  assert (cuda_err == cudaSuccess);

  cu_err = cuStreamQuery (cu_sm);
  assert (cu_err == CUDA_SUCCESS);

  #pragma omp interop destroy(obj_cuda, obj_cuda_driver)
}
