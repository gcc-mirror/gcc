! Test lowering of the internal representation of "omp unroll" loops
! which are not unrolled.

! { dg-additional-options "-O0" }
! { dg-additional-options "--param=omp-unroll-full-max-iterations=0" }
! { dg-additional-options "-fdump-tree-omp_transform_loops-details -fopt-info-optimized" }
! { dg-do run }

module test_functions
contains
  integer function compute_sum1 () result(sum)
    implicit none

    integer :: i

    sum = 0
    !$omp unroll
    do i = 0,50
       sum = sum + 1
    end do
  end function compute_sum1

  integer function compute_sum3 (step,n) result(sum)
    implicit none
    integer :: i, step, n

    sum = 0
    do i = 0,n,step
       sum = sum + 1
    end do
  end function compute_sum3
end module test_functions

program test
  use test_functions
  implicit none

  integer :: result

  result = compute_sum1 ()
  if (result .ne. 51) then
     call abort
  end if

  result = compute_sum3 (1, 100)
  if (result .ne. 101) then
     call abort
  end if

  result = compute_sum3 (2, 100)
  if (result .ne. 51) then
     call abort
  end if

  result = compute_sum3 (-2, -100)
  if (result .ne. 51) then
     call abort
  end if
end program
