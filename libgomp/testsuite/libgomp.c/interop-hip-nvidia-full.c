/* { dg-do run { target { offload_device_nvptx } } } */
/* { dg-do link { target { ! offload_device_nvptx } } } */

/* { dg-require-effective-target openacc_cudart } */
/* { dg-require-effective-target openacc_cuda } */
/* { dg-require-effective-target gomp_hip_header_nvidia } */
/* { dg-additional-options "-lcuda -lcudart -Wno-deprecated-declarations" } */

#define __HIP_PLATFORM_NVIDIA__ 1

#include "interop-hip.h"
