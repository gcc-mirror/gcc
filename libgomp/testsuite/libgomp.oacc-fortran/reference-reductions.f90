! Test reductions on dummy arguments inside modules.

! { dg-do run }

module prm
  implicit none

contains

subroutine param_reduction(var)
  implicit none
  integer(kind=8) :: var
  integer      :: j,k

!$acc parallel copy(var)
!$acc loop reduction(+ : var) gang
 do k=1,10
!$acc loop vector reduction(+ : var)
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

  if (r .ne. 1010) call abort ()
end program test
