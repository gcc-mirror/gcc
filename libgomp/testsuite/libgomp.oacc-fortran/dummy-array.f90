! Ensure that dummy arrays are transferred to the accelerator
! via an implicit pcopy.

! { dg-do run } 

program main
  integer, parameter :: n = 1000
  integer :: a(n)
  integer :: i

  a(:) = -1

  call dummy_array (a, n)
  
  do i = 1, n
     if (a(i) .ne. i) call abort
  end do
end program main

subroutine dummy_array (a, n)
  integer a(n)

  !$acc parallel loop num_gangs (100) gang
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel loop
end subroutine
