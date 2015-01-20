! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER I

!$ACC ENTER DATA CREATE (I)
      CALL ACC_CREATE (I)

      END

! { dg-shouldfail "" }
! { dg-output "already mapped to" }
