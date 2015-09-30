! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE

      INTEGER I

!$ACC DATA PRESENT_OR_COPY (I)
      WRITE(0, *) "CheCKpOInT"
!$ACC DATA COPYOUT (I)
      I = 0
!$ACC END DATA
!$ACC END DATA

      END

! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }
! { dg-output "Trying to map into device \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) object when \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) is already mapped" }
! { dg-shouldfail "" }
