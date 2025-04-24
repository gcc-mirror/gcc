/* { dg-require-effective-target gomp_hip_header_amd } */
/* { dg-require-effective-target gomp_libhipblas } */
/* { dg-additional-options "-lhipblas" } */

#define __HIP_PLATFORM_AMD__ 1

#include "interop-hipblas.h"
