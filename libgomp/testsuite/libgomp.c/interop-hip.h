/* Minimal check whether HIP works - by checking whether the API routines
   seem to work.  This includes various fallbacks if the header is not
   available.  */

#include <assert.h>
#include <omp.h>

#if !defined(__HIP_PLATFORM_AMD__) && !defined(__HIP_PLATFORM_NVIDIA__)
  #error "Either __HIP_PLATFORM_AMD__ or __HIP_PLATFORM_NVIDIA__ must be defined"
#endif

#if defined(__HIP_PLATFORM_AMD__) && defined(__HIP_PLATFORM_NVIDIA__)
  #error "Either __HIP_PLATFORM_AMD__ or __HIP_PLATFORM_NVIDIA__ must be defined"
#endif

#if __has_include(<hip/hip_runtime_api.h>) && !defined(USE_HIP_FALLBACK_HEADER)
  #include <hip/hip_runtime_api.h>

#elif defined(__HIP_PLATFORM_AMD__)
  /* Add a poor man's fallback declaration.  */
  #if !defined(USE_HIP_FALLBACK_HEADER)
    #warning "Using fallback declaration for <hip/hip_runtime_api.h> for __HIP_PLATFORM_AMD__"
  #endif

  typedef struct ihipStream_t* hipStream_t;
  typedef struct ihipCtx_t* hipCtx_t;
  typedef int hipError_t;
  typedef int hipDevice_t;
  enum {
    hipSuccess = 0,
    hipErrorNotSupported = 801
  };

  typedef enum hipDeviceAttribute_t {
    hipDeviceAttributeClockRate = 5,
    hipDeviceAttributeMaxGridDimX = 29
  } hipDeviceAttribute_t;

  hipError_t hipDeviceGetAttribute (int *, hipDeviceAttribute_t, hipDevice_t);
  hipError_t hipCtxGetApiVersion (hipCtx_t, int *);
  hipError_t hipStreamGetDevice (hipStream_t, hipDevice_t *);
  hipError_t hipStreamQuery (hipStream_t);

#elif defined(__HIP_PLATFORM_NVIDIA__)
  /* Add a poor man's fallback declaration.  */
  #if !defined(USE_HIP_FALLBACK_HEADER)
    #warning "Using fallback declaration for <hip/hip_runtime_api.h> for __HIP_PLATFORM_NVIDIA__"
  #endif

  #if __has_include(<cuda.h>) && __has_include(<cudaTypedefs.h>) && __has_include(<cuda_runtime.h>) && !defined(USE_CUDA_FALLBACK_HEADER)
    #include <cuda.h>
    #include <cudaTypedefs.h>
    #include <cuda_runtime.h>
  #else
    #if defined(USE_CUDA_FALLBACK_HEADER)
       // no warning
    #elif !__has_include(<cuda.h>)
      #warning "Using GCC's cuda.h as fallback for cuda.h"
    #elif !__has_include(<cudaTypedefs.h>)
      #warning "Using GCC's cuda.h as fallback for cudaTypedefs.h"
    #else
      #warning "Using GCC's cuda.h as fallback for cuda_runtime.h"
    #endif

    #include "../../../include/cuda/cuda.h"

    typedef int cudaError_t;
    enum {
      cudaSuccess = 0
    };

    enum cudaDeviceAttr {
      cudaDevAttrClockRate = 13,
      cudaDevAttrMaxGridDimX = 5
    };

    cudaError_t cudaDeviceGetAttribute (int *, enum cudaDeviceAttr, int);
    CUresult cuCtxGetApiVersion(CUcontext, unsigned int *);
    CUresult cuStreamGetCtx (CUstream, CUcontext *);
  #endif

  typedef CUstream hipStream_t;
  typedef CUcontext hipCtx_t;
  typedef CUdevice hipDevice_t;

  typedef int hipError_t;
  typedef int hipDevice_t;
  enum {
    hipSuccess = 0,
    hipErrorNotSupported = 801
  };


  typedef enum hipDeviceAttribute_t {
    hipDeviceAttributeClockRate = 5,
    hipDeviceAttributeMaxGridDimX = 29
  } hipDeviceAttribute_t;

  inline static hipError_t
  hipDeviceGetAttribute (int *ival, hipDeviceAttribute_t attr, hipDevice_t dev)
  {
    enum cudaDeviceAttr cuattr;
    switch (attr)
      {
      case hipDeviceAttributeClockRate:
	cuattr = cudaDevAttrClockRate;
	break;
      case hipDeviceAttributeMaxGridDimX:
	cuattr = cudaDevAttrMaxGridDimX;
	break;
      default:
	assert (0);
      }
    return cudaDeviceGetAttribute (ival, cuattr, dev) != cudaSuccess;
  }

  inline static hipError_t
  hipCtxGetApiVersion (hipCtx_t ctx, int *ver)
  {
    unsigned uver;
    hipError_t err;
    err = cuCtxGetApiVersion (ctx, &uver) != CUDA_SUCCESS;
    *ver = (int) uver;
    return err;
  }

  inline static hipError_t
  hipStreamGetDevice (hipStream_t stream, hipDevice_t *dev)
  {
#if CUDA_VERSION >= 12080
    return cudaStreamGetDevice (stream, dev);
#else
    hipError_t err;
    CUcontext ctx;
    err = cuStreamGetCtx (stream, &ctx) != CUDA_SUCCESS;
    if (err == hipSuccess)
      err = cuCtxPushCurrent (ctx) != CUDA_SUCCESS;
    if (err == hipSuccess)
      err = cuCtxGetDevice (dev) != CUDA_SUCCESS;
    if (err == hipSuccess)
      err = cuCtxPopCurrent (&ctx) != CUDA_SUCCESS;
    return err;
#endif
  }

  inline static hipError_t
  hipStreamQuery (hipStream_t stream)
  {
    return cuStreamQuery (stream) != CUDA_SUCCESS;
  }

#else
  #error "should be unreachable"
#endif

int
main ()
{
  int ivar;
  omp_interop_rc_t res;
  omp_interop_t obj = omp_interop_none;
  hipError_t hip_err;

  #pragma omp interop init(target, targetsync, prefer_type("hip") : obj)

  omp_interop_fr_t fr = (omp_interop_fr_t) omp_get_interop_int (obj, omp_ipr_fr_id, &res);
  assert (res == omp_irc_success);
  assert (fr == omp_ifr_hip);

  ivar = (int) omp_get_interop_int (obj, omp_ipr_vendor, &res);
  assert (res == omp_irc_success);
  int vendor_is_amd = ivar == 1;
  #if defined(__HIP_PLATFORM_AMD__)
    assert (ivar == 1);
  #elif defined(__HIP_PLATFORM_NVIDIA__)
    assert (ivar == 11);
  #else
    assert (0);
  #endif


  /* Check whether the omp_ipr_device -> hipDevice_t yields a valid device.  */

  hipDevice_t hip_dev = (int) omp_get_interop_int (obj, omp_ipr_device, &res);
  assert (res == omp_irc_success);

  /* Assume a clock size is available and > 1 GHz; value is in kHz.  */
  hip_err = hipDeviceGetAttribute (&ivar, hipDeviceAttributeClockRate, hip_dev);
  assert (hip_err == hipSuccess);
  assert (ivar > 1000000 /* kHz */);

  /* Assume that the MaxGridDimX is available and > 1024.  */
  hip_err = hipDeviceGetAttribute (&ivar, hipDeviceAttributeMaxGridDimX, hip_dev);
  assert (hip_err == hipSuccess);
  assert (ivar > 1024);


  /* Check whether the omp_ipr_device_context -> hipCtx_t yields a context.  */

  hipCtx_t hip_ctx = (hipCtx_t) omp_get_interop_ptr (obj, omp_ipr_device_context, &res);
  assert (res == omp_irc_success);

  /* Assume API Version > 0 for Nvidia, hipErrorNotSupported for AMD.  */
  ivar = -99;
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    hip_err = hipCtxGetApiVersion (hip_ctx, &ivar);
  #pragma GCC diagnostic pop

  if (vendor_is_amd)
    assert (hip_err == hipErrorNotSupported && ivar == -99);
  else
    {
      assert (hip_err == hipSuccess);
      assert (ivar > 0);
    }


  /* Check whether the omp_ipr_targetsync -> hipStream_t yields a stream.  */

  hipStream_t hip_sm = (hipStream_t) omp_get_interop_ptr (obj, omp_ipr_targetsync, &res);
  assert (res == omp_irc_success);

  hipDevice_t dev_stream = 99;
  hip_err = hipStreamGetDevice (hip_sm, &dev_stream);
  assert (hip_err == hipSuccess);
  assert (dev_stream == hip_dev);

  /* All jobs should have been completed (as there were none none)  */
  hip_err = hipStreamQuery (hip_sm);
  assert (hip_err == hipSuccess);

  #pragma omp interop destroy(obj)
}
