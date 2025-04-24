/* { dg-require-effective-target gomp_libhipblas } */
/* { dg-additional-options "-lhipblas" } */

#define __HIP_PLATFORM_AMD__ 1

#define USE_HIP_FALLBACK_HEADER 1

#include "interop-hipblas.h"
