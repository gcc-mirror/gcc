! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE

      INTEGER I

!$ACC DATA CREATE (I)
      WRITE(0, *) "CheCKpOInT"
!$ACC PARALLEL COPYIN (I)
      I = 0
!$ACC END PARALLEL
!$ACC END DATA

      END

! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }
