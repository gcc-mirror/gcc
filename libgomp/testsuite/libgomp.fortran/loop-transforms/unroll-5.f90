! { dg-additional-options "-O0 -g" }
! { dg-additional-options "-fdump-tree-omp_transform_loops-details -fopt-info-optimized" }
! { dg-do run }

module test_functions
contains
  integer function compute_sum4 (step,n) result(sum)
    implicit none
    integer :: i, step, n

    sum = 0
    !$omp do
    !$omp unroll partial(5)
    do i = 1,n,step
       sum = sum + 1
    end do
  end function compute_sum4
end module test_functions

program test
  use test_functions
  implicit none

  integer :: result

  result = compute_sum4 (1, 100)
  write (*,*) result
  if (result .ne. 100) then
     call abort
  end if

  result = compute_sum4 (1, 9)
  write (*,*) result
  if (result .ne. 9) then
     call abort
  end if

  result = compute_sum4 (2, 96)
  write (*,*) result
  if (result .ne. 48) then
     call abort
  end if

  result = compute_sum4 (-2, -98)
  write (*,*) result
  if (result .ne. 50) then
     call abort
  end if

  result = compute_sum4 (-2, -100)
  write (*,*) result
  if (result .ne. 51) then
     call abort
  end if
end program
