/* { dg-require-effective-target openacc_libcublas } */
/* { dg-additional-options "-lcublas" } */

#define __HIP_PLATFORM_NVIDIA__ 1

#define USE_HIP_FALLBACK_HEADER 1
#define USE_CUDA_FALLBACK_HEADER 1

#include "interop-hipblas.h"
