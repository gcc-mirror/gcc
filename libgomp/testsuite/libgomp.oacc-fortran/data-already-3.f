! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER I

!$ACC DATA PRESENT_OR_COPY (I)
      CALL ACC_COPYIN (I)
!$ACC END DATA

      END

! { dg-shouldfail "" }
! { dg-output "already mapped to" }
