! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER I

      CALL ACC_COPYIN (I)
      WRITE(0, *) "CheCKpOInT"
!$ACC DATA COPY (I)
      I = 0
!$ACC END DATA

      END

! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }
! { dg-output "Trying to map into device \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) object when \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) is already mapped" }
! { dg-shouldfail "" }
