! { dg-do run { target { offload_device_nvptx } } }
! { dg-do link { target { ! offload_device_nvptx } } }

! { dg-require-effective-target openacc_libcudart }
! { dg-require-effective-target openacc_libcuda }
! { dg-additional-options "-lcuda -lcudart" }

#define USE_CUDA_NAMES 1
#define USE_HIP_FALLBACK_MODULE 1

#include "interop-hip.h"
