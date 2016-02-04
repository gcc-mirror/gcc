/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lcuda -lcublas -lcudart" } */

#include <stdlib.h>
#include <openacc.h>
#include <cuda.h>
#include <cuda_runtime_api.h>
#include <cublas_v2.h>

void
saxpy_host (int n, float a, float *x, float *y)
{
  int i;

  for (i = 0; i < n; i++)
    y[i] = y[i] + a * x[i];
}

#pragma acc routine
void
saxpy_target (int n, float a, float *x, float *y)
{
  int i;

  for (i = 0; i < n; i++)
    y[i] = y[i] + a * x[i];
}

int
main(int argc, char **argv)
{
#define N 8
  int i;
  float x_ref[N], y_ref[N];
  float x[N], y[N];
  cublasHandle_t h;
  float a = 2.0;

  for (i = 0; i < N; i++)
    {
      x[i] = x_ref[i] = 4.0 + i;
      y[i] = y_ref[i] = 3.0;
    }

  saxpy_host (N, a, x_ref, y_ref);

  cublasCreate (&h);

#pragma acc data copyin (x[0:N]) copy (y[0:N])
  {
#pragma acc host_data use_device (x, y)
    {
      cublasSaxpy (h, N, &a, x, 1, y, 1);
    }
  }

  for (i = 0; i < N; i++)
    {
      if (y[i] != y_ref[i])
        abort ();
    }

#pragma acc data create (x[0:N]) copyout (y[0:N])
  {
#pragma acc kernels
    for (i = 0; i < N; i++)
      y[i] = 3.0;

#pragma acc host_data use_device (x, y)
    {
      cublasSaxpy (h, N, &a, x, 1, y, 1);
    }
  }

  cublasDestroy (h);

  for (i = 0; i < N; i++)
    {
      if (y[i] != y_ref[i])
        abort ();
    }

  for (i = 0; i < N; i++)
    y[i] = 3.0;

  /* There's no need to use host_data here.  */
#pragma acc data copyin (x[0:N]) copyin (a) copy (y[0:N])
  {
#pragma acc parallel present (x[0:N]) pcopy (y[0:N]) present (a)
    saxpy_target (N, a, x, y);
  }

  for (i = 0; i < N; i++)
    {
      if (y[i] != y_ref[i])
        abort ();
    }

  return 0;
}
