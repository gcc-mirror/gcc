/* Check whether hipBlas' daxpy works with an interop object.
     daxpy(N, DA, DX, INCX, DY, INCY)
   calculates (for DX = DY = 1):
     DY(1:N) =  DY(1:N) + DA * DX(1:N)
   and otherwise N array elements, taking every INCX-th or INCY-th one, repectively.

Based on the interop example in OpenMP's example document  */

/* Minimal check whether HIP works - by checking whether the API routines
   seem to work.  This includes a fallback if the header is not
   available.  */

#if !defined(__HIP_PLATFORM_AMD__) && !defined(__HIP_PLATFORM_NVIDIA__)
  #error "Either __HIP_PLATFORM_AMD__ or __HIP_PLATFORM_NVIDIA__ must be defined"
#endif

#if defined(__HIP_PLATFORM_AMD__) && defined(__HIP_PLATFORM_NVIDIA__)
  #error "Either __HIP_PLATFORM_AMD__ or __HIP_PLATFORM_NVIDIA__ must be defined"
#endif


#include <assert.h>
#include <omp.h>
#include "../libgomp.c-c++-common/on_device_arch.h"


#if __has_include(<hipblas/hipblas.h>) && (__has_include(<library_types.h>) || !defined(__HIP_PLATFORM_NVIDIA__)) && !defined(USE_HIP_FALLBACK_HEADER)
  #ifdef __HIP_PLATFORM_NVIDIA__
    /* There seems to be an issue with hip/library_types.h including
       CUDA's "library_types.h". Include CUDA's one explicitly here.
       Could possibly worked around by using -isystem vs. -I.  */
    #include <library_types.h>

    /* For some reasons, the following symbols do not seem to get
       mapped from HIP to CUDA, causing link errors.  */
    #define hipblasSetStream cublasSetStream_v2
    #define hipblasDaxpy cublasDaxpy_v2
    #define hipblasCreate cublasCreate_v2
  #endif
  #include <hipblas/hipblas.h>

#elif defined(__HIP_PLATFORM_AMD__)
  /* Add a poor man's fallback declaration.  */
  #if !defined(USE_HIP_FALLBACK_HEADER)
    #warning "Using fallback declaration for <hipblas/hipblas.h> for __HIP_PLATFORM_AMD__"
  #endif

  typedef enum
  {
    HIPBLAS_STATUS_SUCCESS = 0

  } hipblasStatus_t;

  typedef struct ihipStream_t* hipStream_t;
  typedef void* hipblasHandle_t;

  hipblasStatus_t hipblasCreate (hipblasHandle_t*);
  hipblasStatus_t hipblasSetStream (hipblasHandle_t, hipStream_t);
  hipblasStatus_t hipblasDaxpy (hipblasHandle_t, int, const double*, const double*, int, double*, int);

#else
  /* Add a poor man's fallback declaration.  */
  #if !defined(USE_HIP_FALLBACK_HEADER)
    #warning "Using fallback declaration for <hipblas/hipblas.h> for __HIP_PLATFORM_NVIDA__"
  #endif

  #if __has_include(<cuda.h>) && __has_include(<cudaTypedefs.h>) && __has_include(<cuda_runtime.h>) && __has_include(<cublas_v2.h>) && !defined(USE_CUDA_FALLBACK_HEADER)
    #include <cuda.h>
    #include <cudaTypedefs.h>
    #include <cuda_runtime.h>
    #include <cublas_v2.h>

  #else
    /* Add a poor man's fallback declaration.  */
    #if defined(USE_CUDA_FALLBACK_HEADER)
      // no warning
    #elif !__has_include(<cuda.h>)
      #warning "Using GCC's cuda.h as fallback for cuda.h"
    #elif !__has_include(<cudaTypedefs.h>)
      #warning "Using GCC's cuda.h as fallback for cudaTypedefs.h"
    #elif !__has_include(<cuda_runtime.h>)
      #warning "Using GCC's cuda.h as fallback for cuda_runtime.h"
    #else
      #warning "Using GCC's cuda.h as fallback for cublas_v2.h"
    #endif
    #include "../../../include/cuda/cuda.h"

    typedef enum {
      CUBLAS_STATUS_SUCCESS = 0,
    } cublasStatus_t;

    typedef CUstream cudaStream_t;
    typedef struct cublasContext* cublasHandle_t;

    #define cublasCreate cublasCreate_v2
    cublasStatus_t cublasCreate_v2 (cublasHandle_t *);

    #define cublasSetStream cublasSetStream_v2
    cublasStatus_t cublasSetStream_v2 (cublasHandle_t, cudaStream_t);

    #define cublasDaxpy cublasDaxpy_v2
    cublasStatus_t cublasDaxpy_v2(cublasHandle_t, int, const double*, const double*, int, double*, int);
  #endif

  #define HIPBLAS_STATUS_SUCCESS CUBLAS_STATUS_SUCCESS
  #define hipblasStatus_t cublasStatus_t
  #define hipStream_t cudaStream_t
  #define hipblasHandle_t cublasHandle_t
  #define hipblasCreate cublasCreate
  #define hipblasSetStream cublasSetStream
  #define hipblasDaxpy cublasDaxpy
#endif

static int used_variant = 0;

void
run_hipBlasdaxpy (int n, double da, const double *dx, int incx, double *dy, int incy, omp_interop_t obj)
{
  used_variant = 1;

  omp_interop_rc_t res;
  hipblasStatus_t stat;

  omp_intptr_t fr = omp_get_interop_int(obj, omp_ipr_fr_id, &res);
  assert (res == omp_irc_success && fr == omp_ifr_hip);

  hipStream_t stream = (hipStream_t) omp_get_interop_ptr (obj, omp_ipr_targetsync, &res);
  assert (res == omp_irc_success);

  hipblasHandle_t handle;
  stat = hipblasCreate (&handle);
  assert (stat == HIPBLAS_STATUS_SUCCESS);

  stat = hipblasSetStream (handle, stream);
  assert (stat == HIPBLAS_STATUS_SUCCESS);

  /* 'da' can be in host or device space, 'dx' and 'dy' must be in device space.  */
  stat = hipblasDaxpy (handle, n, &da, dx, 1, dy, 1) ;
  assert (stat == HIPBLAS_STATUS_SUCCESS);
}

#if defined(__HIP_PLATFORM_AMD__)
#pragma omp declare variant(run_hipBlasdaxpy) \
                       match(construct={dispatch}, target_device={kind(nohost), arch("amdgcn")}) \
                       adjust_args(need_device_ptr : dx, dy) \
                       append_args(interop(targetsync, prefer_type("hip")))
#elif defined(__HIP_PLATFORM_NVIDIA__) 
#pragma omp declare variant(run_hipBlasdaxpy) \
                       match(construct={dispatch}, target_device={kind(nohost), arch("nvptx")}) \
                       adjust_args(need_device_ptr : dx, dy) \
                       append_args(interop(targetsync, prefer_type("hip")))
#else
 #error "wrong platform"
#endif

void
run_daxpy (int n, double da, const double *dx, int incx, double *dy, int incy)
{
  used_variant = 2;

  if (incx == 1 && incy == 1)
    #pragma omp simd
    for (int i = 0; i < n; i++)
      dy[i] += da * dx[i];
  else
    {
      int ix = 0;
      int iy = 0;
      for (int i = 0; i < n; i++)
	{
	  dy[iy] += da * dx[ix];
	  ix += incx;
	  iy += incy;
	}
    }
}


void
run_test (int dev)
{
  constexpr int N = 1024;

  // A = {1,2,...,N}
  // B = {-1, -2, ..., N}
  // B' = daxpy (N, 3, A, incx=1, B, incy=1)
  //    = B + 3*A
  // -> B' = {0, 2, 4, 6, ... }

  double A[N], B[N];
  double factor = 3.0;
  for (int i = 0; i < N; i++)
    {
      A[i] = i;
      B[i] = -i;
    }

  if (dev != omp_initial_device && dev != omp_get_num_devices ())
    {
      #pragma omp target enter data device(dev) map(A, B)
    }

  used_variant = 99;
  #pragma omp dispatch device(dev)
    run_daxpy (N, factor, A, 1, B, 1);  

  if (dev != omp_initial_device && dev != omp_get_num_devices ())
    {
      #pragma omp target exit data device(dev) map(release: A) map(from: B)

      int tmp = omp_get_default_device ();
      omp_set_default_device (dev);
#if defined(__HIP_PLATFORM_AMD__)
      if (on_device_arch_gcn ())
#else
      if (on_device_arch_nvptx ())
#endif
	assert (used_variant == 1);
      else
	assert (used_variant == 2);
      omp_set_default_device (tmp);
    }
  else
    assert (used_variant == 2);

  for (int i = 0; i < N; i++)
    assert (B[i] == 2*i);
}

int   
main () 
{   
  int ndev = omp_get_num_devices ();

  for (int dev = 0; dev <= ndev; dev++)
    run_test (dev);
  run_test (omp_initial_device);  

  return 0;
}
