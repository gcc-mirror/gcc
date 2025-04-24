/* { dg-require-effective-target openacc_cublas } */
/* { dg-require-effective-target gomp_hip_header_nvidia } */
/* { dg-additional-options "-lcublas -Wno-deprecated-declarations" } */

#define __HIP_PLATFORM_NVIDIA__ 1

#include "interop-hipblas.h"
