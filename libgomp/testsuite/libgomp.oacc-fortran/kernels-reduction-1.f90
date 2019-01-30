! Test a simple acc loop reduction inside a kernels region. 

! { dg-do run }
! { dg-additional-options "-fopenacc-kernels=parloops" } as this is
! specifically testing "parloops" handling.

program reduction
  integer, parameter     :: n = 20
  integer                :: i, red

  red = 0

  !$acc kernels
  !$acc loop reduction (+:red)
  do i = 1, n
     red = red + 1
  end do
  !$acc end kernels

  if (red .ne. n) call abort
end program reduction
