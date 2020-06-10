! { dg-do run }

/* Nullify the 'finalize' clause, which disturbs reference counting.  */
#define finalize
#include "deep-copy-6.f90"

! { dg-output ".*CheCKpOInT1(\n|\r\n|\r)" }
! { dg-output ".CheCKpOInT2(\n|\r\n|\r)" }
