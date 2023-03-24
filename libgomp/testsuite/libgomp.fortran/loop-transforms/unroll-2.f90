! { dg-additional-options "-fdump-tree-original -g" }
! { dg-do run }

module test_functions
contains
  integer function compute_sum1 () result(sum)
    implicit none

    integer :: i

    sum = 0
    !$omp unroll full
    do i = 1,10,3
       sum = sum + 1
    end do
  end function compute_sum1

  integer function compute_sum2() result(sum)
    implicit none

    integer :: i

    sum = 0
    !$omp unroll full
    do i = -20,1,3
       sum = sum + 1
    end do
  end function compute_sum2


  integer function compute_sum3() result(sum)
    implicit none

    integer :: i

    sum = 0
    !$omp unroll full
    do i = 30,1,-3
       sum = sum + 1
    end do
  end function compute_sum3


  integer function compute_sum4() result(sum)
    implicit none

    integer :: i

    sum = 0
    !$omp unroll full
    do i = 50,-60,-10
       sum = sum + 1
    end do
  end function compute_sum4

end module test_functions

program test
  use test_functions
  implicit none

  integer :: result

  result = compute_sum1 ()
  write (*,*) result
  if (result .ne. 4) then
     call abort
  end if

  result = compute_sum2 ()
  write (*,*) result
  if (result .ne. 8) then
     call abort
  end if

  result = compute_sum3 ()
  write (*,*) result
  if (result .ne. 10) then
     call abort
  end if

  result = compute_sum4 ()
  write (*,*) result
  if (result .ne. 12) then
     call abort
  end if

end program
