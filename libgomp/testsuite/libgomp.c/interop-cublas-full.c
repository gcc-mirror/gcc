/* { dg-require-effective-target openacc_cublas } */
/* { dg-additional-options "-lcublas" } */

/* NOTE: This file is also included by libgomp.c-c++-common/interop-cudablas-libonly.c
   to test the fallback version.  */

/* Check whether cuBlas' daxpy works with an interop object.
     daxpy(N, DA, DX, INCX, DY, INCY)
   calculates (for DX = DY = 1):
     DY(1:N) =  DY(1:N) + DA * DX(1:N)
   and otherwise N array elements, taking every INCX-th or INCY-th one, repectively.

Based on the interop example in OpenMP's example document  */

/* Minimal check whether CUDA works - by checking whether the API routines
   seem to work.  This includes a fallback if the header is not
   available.  */

#include <assert.h>
#include <omp.h>
#include "../libgomp.c-c++-common/on_device_arch.h"


#if __has_include(<cuda.h>) && __has_include(<cudaTypedefs.h>) && __has_include(<cuda_runtime.h>) && __has_include(<cublas_v2.h>) && !defined(USE_CUDA_FALLBACK_HEADER)
  #include <cuda.h>
  #include <cudaTypedefs.h>
  #include <cuda_runtime.h>
  #include <cublas_v2.h>

#else
  /* Add a poor man's fallback declaration.  */
  #if USE_CUDA_FALLBACK_HEADER
    // Don't warn.
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

static int used_variant = 0;

void
run_cuBlasdaxpy (int n, double da, const double *dx, int incx, double *dy, int incy, omp_interop_t obj)
{
  used_variant = 1;

  omp_interop_rc_t res;
  cublasStatus_t stat;

  omp_intptr_t fr = omp_get_interop_int(obj, omp_ipr_fr_id, &res);
  assert (res == omp_irc_success && fr == omp_ifr_cuda);

  cudaStream_t stream = (cudaStream_t) omp_get_interop_ptr (obj, omp_ipr_targetsync, &res);
  assert (res == omp_irc_success);

  cublasHandle_t handle;
  stat = cublasCreate (&handle);
  assert (stat == CUBLAS_STATUS_SUCCESS);

  stat = cublasSetStream (handle, stream);
  assert (stat == CUBLAS_STATUS_SUCCESS);

  /* 'da' can be in host or device space, 'dx' and 'dy' must be in device space.  */
  stat = cublasDaxpy (handle, n, &da, dx, 1, dy, 1) ;
  assert (stat == CUBLAS_STATUS_SUCCESS);
}


#pragma omp declare variant(run_cuBlasdaxpy) \
                       match(construct={dispatch}, target_device={kind(nohost), arch("nvptx")}) \
                       adjust_args(need_device_ptr : dx, dy) \
                       append_args(interop(targetsync, prefer_type("cuda")))

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
      if (on_device_arch_nvptx ())
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
