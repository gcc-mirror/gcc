! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER I

      CALL ACC_COPYIN (I)

!$ACC DATA COPY (I)
      I = 0
!$ACC END DATA

      END

! { dg-shouldfail "" }
! { dg-output "Trying to map into device .* object when .* is already mapped" }
