! { dg-do run }
! { dg-options "-fno-openmp -fopenmp-simd" }

module test_functions
  contains
  integer function compute_sum() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp simd reduction(+:sum)
    do i = 1,10,3
      !$omp unroll full
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function compute_sum
end module test_functions

program test
  use test_functions
  implicit none
  integer :: result

  result = compute_sum ()
  if (result .ne. 16) then
    stop 1
  end if
end program
