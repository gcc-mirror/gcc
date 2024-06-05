! { dg-do run }
! { dg-additional-options "-g" }

module test_functions
contains
  integer function compute_sum4 (step,n) result(sum)
    implicit none
    integer :: i, step, n

    sum = 0
    !$omp parallel do reduction(+:sum) lastprivate(i)
    !$omp unroll partial(5)
    do i = 1,n,step
       sum = sum + 1
    end do
  end function compute_sum4

  integer function compute_sum5 (step,n) result(sum)
    implicit none
    integer :: i, step, n

    sum = 0
    !$omp parallel do reduction(+:sum) lastprivate(i)
    !$omp unroll partial(5)
    !$omp unroll partial(10)
    do i = 1,n,step
      sum = sum + 1
    end do
  end function compute_sum5

  integer function compute_sum6 (step,n) result(sum)
    implicit none
    integer :: i, j, step, n

    sum = 0
    !$omp parallel do reduction(+:sum) lastprivate(i) &
    !$omp & private(j)
    do i = 1,n,step
      !$omp unroll full
      !$omp unroll partial(10)
      do j = 1, 1000
        sum = sum + 1
      end do
    end do
  end function compute_sum6
end module test_functions

program test
  use test_functions
  implicit none
  integer :: result

  result = compute_sum4 (1, 100)
  if (result .ne. 100) then
    stop 1
  end if

  result = compute_sum4 (1, 9)
  if (result .ne. 9) then
    stop 2
  end if

  result = compute_sum4 (2, 96)
  if (result .ne. 48) then
    stop 3
  end if

  result = compute_sum4 (-2, -98)
  if (result .ne. 50) then
    stop 4
  end if

  result = compute_sum4 (-2, -100)
  if (result .ne. 51) then
    stop 5
  end if

  result = compute_sum5 (1, 100)
  if (result .ne. 100) then
    stop 6
  end if

  result = compute_sum5 (1, 9)
  if (result .ne. 9) then
    stop 7
  end if

  result = compute_sum5 (2, 96)
  if (result .ne. 48) then
    stop 8
  end if

  result = compute_sum5 (-2, -98)
  if (result .ne. 50) then
    stop 9
  end if

  result = compute_sum5 (-2, -100)
  if (result .ne. 51) then
    stop 10
  end if
end program
