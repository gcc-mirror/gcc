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
