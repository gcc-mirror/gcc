/* { dg-do run { target { offload_device_nvptx } } } */
/* { dg-do link { target { ! offload_device_nvptx } } } */

/* { dg-require-effective-target openacc_libcudart } */
/* { dg-require-effective-target openacc_libcuda } */
/* { dg-additional-options "-lcuda -lcudart" } */

/* Same as interop-cuda-full.c, but also works if the header is not available. */

#define USE_CUDA_FALLBACK_HEADER 1
#include "interop-cuda-full.c"
