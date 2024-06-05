! { dg-do run }

module test_functions
  contains
  integer function compute_sum() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp parallel do reduction(+:sum) private(j)
    do i = 1,10,3
      !$omp unroll full
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function

  integer function compute_sum2() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp parallel do reduction(+:sum) private(i, j)
    !$omp unroll partial(2)
    do i = 1,10,3
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function
end module test_functions

program test
  use test_functions
  implicit none

  integer :: result

  result = compute_sum ()
  if (result .ne. 16) then
    stop 1
  end if

  result = compute_sum2 ()
  if (result .ne. 16) then
    stop 2
  end if
end program
