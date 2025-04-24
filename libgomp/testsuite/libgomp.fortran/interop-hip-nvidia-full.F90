! { dg-require-effective-target gomp_hipfort_module }
! { dg-require-effective-target openacc_cudart }
! { dg-require-effective-target openacc_cuda }
! { dg-additional-options "-lcuda -lcudart" }

#define HAVE_HIPFORT 1
#define USE_CUDA_NAMES 1

#include "interop-hip.h"
