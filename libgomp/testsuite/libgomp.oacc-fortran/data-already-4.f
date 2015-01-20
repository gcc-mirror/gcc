! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER I

      CALL ACC_PRESENT_OR_COPYIN (I)
      CALL ACC_COPYIN (I)

      END

! { dg-shouldfail "" }
! { dg-output "already mapped to" }
