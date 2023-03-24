! { dg-additional-options "-fdump-tree-original" }
! { dg-do run }

module test_functions
  contains
  integer function compute_sum1() result(sum)
    implicit none

    integer :: i,j

    sum = 0
    !$omp do
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
    !$omp do
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
    !$omp do
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
    !$omp do
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
    !$omp parallel do collapse(2)
    !$omp tile sizes(6,10)
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

  result = compute_sum1 ()
  write (*,*) result
  if (result .ne. 16) then
     call abort
  end if

  result = compute_sum2 ()
  write (*,*) result
  if (result .ne. 16) then
     call abort
  end if

  result = compute_sum3 ()
  write (*,*) result
  if (result .ne. 16) then
     call abort
  end if

  result = compute_sum4 ()
  write (*,*) result
  if (result .ne. 16) then
     call abort
  end if

  result = compute_sum5 ()
  write (*,*) result
  if (result .ne. 16) then
     call abort
  end if
end program
