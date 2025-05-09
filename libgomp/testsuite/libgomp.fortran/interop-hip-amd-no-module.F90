! { dg-do run { target { offload_device_gcn } } }
! { dg-do link { target { ! offload_device_gcn } } }

! { dg-require-effective-target gomp_libamdhip64 }
! { dg-additional-options "-lamdhip64" }

#define USE_HIP_FALLBACK_MODULE 1

#include "interop-hip.h"
