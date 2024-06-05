! { dg-do run }
! { dg-additional-options "-g" }

module test_functions
contains
  integer function compute_sum1 () result(sum)
    implicit none
    integer :: i

    sum = 0
    !$omp unroll partial(2)
    do i = 1,50
      sum = sum + 1
    end do
  end function compute_sum1

  integer function compute_sum3 (step,n) result(sum)
    implicit none
    integer :: i, step, n

    sum = 0
    !$omp unroll partial(5)
    do i = 1,n,step
      sum = sum + 1
    end do
  end function compute_sum3
end module test_functions

program test
  use test_functions
  implicit none
  integer :: result

  result = compute_sum1 ()
  if (result .ne. 50) then
    stop 1
  end if

  result = compute_sum3 (1, 100)
  if (result .ne. 100) then
    stop 2
  end if

  result = compute_sum3 (1, 9)
  if (result .ne. 9) then
    stop 3
  end if

  result = compute_sum3 (2, 96)
  if (result .ne. 48) then
    stop 4
  end if

  result = compute_sum3 (-2, -98)
  if (result .ne. 50) then
    stop 5
  end if

  result = compute_sum3 (-2, -100)
  if (result .ne. 51) then
    stop 6
  end if
end program
