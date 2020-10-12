! { dg-do run }
! { dg-additional-sources pr96628-part2.f90 }
! { dg-additional-options "-ftree-slp-vectorize" }
!
! This file is compiled first
module m2
  real*8 :: mysum
  !$acc declare device_resident(mysum)
contains
    SUBROUTINE one(t)
      !$acc routine
      REAL*8,  INTENT(IN)    :: t(:)
      mysum = sum(t)
    END SUBROUTINE one
    SUBROUTINE two(t)
      !$acc routine seq
      REAL*8, INTENT(INOUT) :: t(:)
      t = (100.0_8*t)/sum
    END SUBROUTINE two
end module m2
