! { dg-do run }

module test_functions
  contains
  integer function compute_sum1() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp parallel do reduction(+:sum) private(j)
    do i = 1,10,3
      !$omp tile sizes(2)
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function

  integer function compute_sum2() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp parallel do reduction(+:sum) private(j)
    do i = 1,10,3
      !$omp tile sizes(16)
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function

  integer function compute_sum3() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp parallel do reduction(+:sum) private(j)
    do i = 1,10,3
      !$omp tile sizes(100)
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function

  integer function compute_sum4() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp parallel do reduction(+:sum) private(i, j)
    !$omp tile sizes(6,10)
    do i = 1,10,3
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
  end function

  integer function compute_sum5() result(sum)
    implicit none
    integer :: i,j

    sum = 0
    !$omp parallel do collapse(2) reduction(+:sum) private(i, j)
    !$omp tile sizes(6,10)
    do i = 1,10,3
      do j = 1,10,3
        sum = sum + 1
      end do
    end do
    !$omp end tile
    !$omp end parallel do
  end function
end module test_functions

program test
  use test_functions
  implicit none
  integer :: result

  result = compute_sum1 ()
  if (result .ne. 16) then
    stop 1
  end if

  result = compute_sum2 ()
  if (result .ne. 16) then
    stop 2
  end if

  result = compute_sum3 ()
  if (result .ne. 16) then
    stop 3
  end if

  result = compute_sum4 ()
  if (result .ne. 16) then
    stop 4
  end if

  result = compute_sum5 ()
  if (result .ne. 16) then
    stop 5
  end if
end program
