! Ensure that a non-scalar dummy arguments which are implicitly used inside
! offloaded regions are properly mapped using present_or_copy.

! { dg-do run }

program main
  implicit none

  integer,parameter :: size = 100
  integer :: array(size), i, n

  n = size

  !$acc data copy(array)
  call kernels(array, n)

  !$acc update host(array)

  do i = 1, n
     if (array(i) .ne. i) call abort
  end do

  call parallel(array, n)
  !$acc end data

  do i = 1, n
     if (array(i) .ne. i+i) call abort
  end do
end program main

subroutine kernels (array, n)
  integer, dimension (n) :: array
  integer :: n, i

  !$acc kernels
  do i = 1, n
     array(i) = i
  end do
  !$acc end kernels
end subroutine kernels


subroutine parallel (array, n)
  integer, dimension (n) :: array
  integer :: n, i

  !$acc parallel
  do i = 1, n
     array(i) = i+i
  end do
  !$acc end parallel
end subroutine parallel
