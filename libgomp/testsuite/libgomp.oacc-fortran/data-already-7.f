! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER I

!$ACC ENTER DATA CREATE (I)
      WRITE(0, *) "CheCKpOInT"
      CALL ACC_CREATE (I)

      END

! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }
