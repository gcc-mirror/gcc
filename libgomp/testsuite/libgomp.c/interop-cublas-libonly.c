/* { dg-require-effective-target openacc_libcublas } */
/* { dg-additional-options "-lcublas" } */

/* Same as interop-cudablas-full.c, but also works if the header is not available. */

#define USE_CUDA_FALLBACK_HEADER 1
#include "interop-cublas-full.c"
