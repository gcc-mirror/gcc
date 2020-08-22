! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

/* Nullify the 'finalize' clause.  */
#define finalize
#include "mdc-refcount-1-1-1.f90"
