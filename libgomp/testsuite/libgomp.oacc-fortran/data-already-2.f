! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

      IMPLICIT NONE

      INTEGER I

!$ACC DATA PRESENT_OR_COPY (I)
!$ACC DATA COPYOUT (I)
      I = 0
!$ACC END DATA
!$ACC END DATA

      END

! { dg-shouldfail "" }
! { dg-output "Trying to map into device .* object when .* is already mapped" }
