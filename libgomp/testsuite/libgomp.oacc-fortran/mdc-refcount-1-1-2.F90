! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

/* Nullify the 'finalize' clause, which disturbs reference counting.  */
#define finalize
#include "mdc-refcount-1-1-1.f90"

! { dg-output ".*CheCKpOInT1(\n|\r\n|\r)" }
! { dg-output ".CheCKpOInT2(\n|\r\n|\r)" }
