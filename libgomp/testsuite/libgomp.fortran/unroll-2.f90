! { dg-do run }
! { dg-additional-options "-g" }

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
  if (result .ne. 4) then
    stop 1
  end if

  result = compute_sum2 ()
  if (result .ne. 8) then
    stop 2
  end if

  result = compute_sum3 ()
  if (result .ne. 10) then
    stop 3
  end if

  result = compute_sum4 ()
  if (result .ne. 12) then
    stop 4
  end if
end program
