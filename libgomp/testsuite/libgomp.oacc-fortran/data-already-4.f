! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE
      INCLUDE "openacc_lib.h"

      INTEGER I

      CALL ACC_PRESENT_OR_COPYIN (I)
      WRITE(0, *) "CheCKpOInT"
      CALL ACC_COPYIN (I)

      END

! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }
