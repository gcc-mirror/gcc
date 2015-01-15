! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE

      INTEGER I

!$ACC DATA CREATE (I)
!$ACC PARALLEL COPYIN (I)
      I = 0
!$ACC END PARALLEL
!$ACC END DATA

      END

! { dg-shouldfail "" }
! { dg-output "Trying to map into device .* object when .* is already mapped" }
