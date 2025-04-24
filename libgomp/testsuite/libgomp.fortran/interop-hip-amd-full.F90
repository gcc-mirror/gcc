! { dg-require-effective-target gomp_hipfort_module }
! { dg-require-effective-target gomp_libamdhip64 }
! { dg-additional-options "-lamdhip64" }

#define HAVE_HIPFORT 1

#include "interop-hip.h"
