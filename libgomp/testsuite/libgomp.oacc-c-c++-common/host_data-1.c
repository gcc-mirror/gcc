/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-lm -lcuda -lcublas -lcudart -Wall -Wextra" } */

#include <stdlib.h>
#include <math.h>
#include <openacc.h>
#include <cuda.h>
#include <cuda_runtime_api.h>
#include <cublas_v2.h>

#pragma acc routine
void
saxpy (int n, float a, float *x, float *y)
{
  int i;

  for (i = 0; i < n; i++)
    y[i] = y[i] + a * x[i];
}

void
validate_results (int n, float *a, float *b)
{
  int i;

  for (i = 0; i < n; i++)
    if (fabs (a[i] - b[i]) > .00001)
      abort ();
}

int
main()
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

  saxpy (N, a, x_ref, y_ref);

  cublasCreate (&h);

#pragma acc data copyin (x[0:N]) copy (y[0:N])
  {
#pragma acc host_data use_device (x, y)
    {
      cublasSaxpy (h, N, &a, x, 1, y, 1);
    }
  }

  validate_results (N, y, y_ref);

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

  validate_results (N, y, y_ref);

  for (i = 0; i < N; i++)
    y[i] = 3.0;

  /* There's no need to use host_data here.  */
#pragma acc data copyin (x[0:N]) copyin (a) copy (y[0:N])
  {
#pragma acc parallel present (x[0:N]) pcopy (y[0:N]) present (a)
    saxpy (N, a, x, y);
  }

  validate_results (N, y, y_ref);

  /* Exercise host_data with data transferred with acc enter data.  */

  for (i = 0; i < N; i++)
    y[i] = 3.0;

#pragma acc enter data copyin (x, a, y)
#pragma acc parallel present (x[0:N]) pcopy (y[0:N]) present (a)
  {
    saxpy (N, a, x, y);
  }
#pragma acc exit data delete (x, a) copyout (y)

  validate_results (N, y, y_ref);

  return 0;
}
