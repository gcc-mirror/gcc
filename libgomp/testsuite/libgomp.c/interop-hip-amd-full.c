/* { dg-require-effective-target gomp_hip_header_amd } */
/* { dg-require-effective-target gomp_libamdhip64 } */
/* { dg-additional-options "-lamdhip64" } */

#define __HIP_PLATFORM_AMD__ 1

#include "interop-hip.h"
