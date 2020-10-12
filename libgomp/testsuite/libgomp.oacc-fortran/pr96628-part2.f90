! { dg-do compile  { target skip-all-targets } }
!
! Main file is pr96628-part1.f90

MODULE m
  IMPLICIT NONE
  REAL*8, ALLOCATABLE :: t(:)
CONTAINS
  SUBROUTINE run()
    use m2
    IMPLICIT NONE

    INTEGER :: i,j    ! loop indices
    !$acc data present(t)
    !$acc parallel
    !$acc loop gang
    DO j = 1,2
      !$acc loop vector
      DO i = 1,2
        CALL one(t(:))
        CALL two(t(:))
      END DO
   END DO
   !$acc end parallel
   !$acc end data
  END SUBROUTINE run
END MODULE m

use m
implicit none
integer :: i
t = [(3.0_8*i, i = 1, 100)]
!$acc data copy(t)
call run
!$acc end data
if (any (abs(t - [((300.0_8*i)/15150.0_8, i = 1, 100)]) < 10.0_8*epsilon(t))) stop 1
end
