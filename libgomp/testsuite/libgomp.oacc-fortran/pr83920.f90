! { dg-do run }

subroutine foo (BETA, C)
  real ::  C(100,100)
  integer :: i, j, l
  real, parameter :: one = 1.0
  real :: beta

  !$acc parallel copy(c(1:100,1:100)) num_gangs(2)
  !$acc loop gang
  do j = 1, 100
     if (beta /= one) then
        !$acc loop vector
        do i = 1, 100
           C(i,j) = 0.0
        end do
     end if
  end do
  !$acc end parallel
end subroutine foo

program test_foo
  real :: c(100,100), beta
  beta = 0.0
  c(:,:) = 1.0
  call foo (beta, c)
  if (c(1,1) /= 0.0) STOP 1
end program test_foo
