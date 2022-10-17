! Test reductions on dummy arguments inside modules.

! { dg-do run }

! { dg-additional-options -Wuninitialized }

module prm
  implicit none

contains

subroutine param_reduction(var)
  implicit none
  integer(kind=8) :: var
  integer      :: j,k

!$acc parallel copy(var)
!$acc loop reduction(+ : var) gang
  ! { dg-bogus {'var\.[0-9]+' is used uninitialized} TODO { xfail *-*-* } .-1 }
  !   { dg-note {'var\.[0-9]+' was declared here} {} { target *-*-* } .-2 }
 do k=1,10
!$acc loop vector reduction(+ : var)
    ! { dg-bogus {'var\.[0-9]+' may be used uninitialized} TODO { xfail { ! __OPTIMIZE__ } } .-1 }
    !   { dg-note {'var\.[0-9]+' was declared here} {} { target { ! __OPTIMIZE__ } } .-2 }
    do j=1,100
     var = var + 1.0
    enddo
 enddo
!$acc end parallel
end subroutine param_reduction

end module prm

program test
  use prm
  implicit none

  integer(8) :: r

  r=10.0
  call param_reduction (r)

  if (r .ne. 1010) stop 1
end program test
