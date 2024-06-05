! { dg-do run }
! { dg-additional-options "-cpp" }

#ifndef UNROLL_FACTOR
#define UNROLL_FACTOR 1
#endif
module test_functions
contains
  subroutine copy (array1, array2)
    implicit none
    integer :: array1(:)
    integer :: array2(:)
    integer :: i

    !$omp parallel do private(i)
    !$omp unroll partial(UNROLL_FACTOR)
    do i = 1, 100
      array1(i) = array2(i)
    end do
  end subroutine

  subroutine copy2 (array1, array2)
    implicit none

    integer :: array1(100)
    integer :: array2(100)
    integer :: i

    !$omp parallel do private(i)
    !$omp unroll partial(UNROLL_FACTOR)
    do i = 0,99
      array1(i+1) = array2(i+1)
    end do
  end subroutine copy2

  subroutine copy3 (array1, array2)
    implicit none

    integer :: array1(100)
    integer :: array2(100)
    integer :: i

    !$omp parallel do lastprivate(i)
    !$omp unroll partial(UNROLL_FACTOR)
    do i = -49,50
      if (i < 0) then
        array1((-1)*i) = array2((-1)*i)
      else
        array1(50+i) = array2(50+i)
      endif
    end do
  end subroutine copy3

  subroutine copy4 (array1, array2)
    implicit none
    integer :: array1(:)
    integer :: array2(:)
    integer :: i

    !$omp parallel do private(i)
    !$omp unroll partial(UNROLL_FACTOR)
    do i = 2, 200, 2
       array1(i/2) = array2(i/2)
    end do
  end subroutine copy4

  subroutine copy5 (array1, array2)
    implicit none
    integer :: array1(:)
    integer :: array2(:)
    integer :: i

    !$omp parallel do private(i)
    !$omp unroll partial(UNROLL_FACTOR)
    do i = 200, 2, -2
       array1(i/2) = array2(i/2)
    end do
  end subroutine

  subroutine copy6 (array1, array2, lower, upper, step)
    implicit none
    integer :: array1(:)
    integer :: array2(:)
    integer :: lower, upper, step
    integer :: i

    !$omp parallel do private(i)
    !$omp unroll partial(UNROLL_FACTOR)
    do i = lower, upper, step
      array1 (i) = array2(i)
    end do
  end subroutine

  subroutine prepare (array1, array2)
    implicit none
    integer :: array1(:)
    integer :: array2(:)

    array1 = 2
    array2 = 0
  end subroutine

  subroutine check_equal (array1, array2)
    implicit none
    integer :: array1(:)
    integer :: array2(:)
    integer :: i

    do i=1,100
      if (array1(i) /= array2(i)) then
        stop 1
      end if
    end do
  end subroutine

  subroutine check_equal_at_steps (array1, array2, lower, upper, step)
    implicit none
    integer :: array1(:)
    integer :: array2(:)
    integer :: lower, upper, step
    integer :: i

    do i=lower, upper, step
      if (array1(i) /= array2(i)) then
        stop 2
      end if
    end do
  end subroutine

  subroutine check_unchanged_at_non_steps (array1, array2, lower, upper, step)
    implicit none
    integer :: array1(:)
    integer :: array2(:)
    integer :: lower, upper, step
    integer :: i, j

    do i=lower, upper,step
      do j=i,i+step-1
        if (array2(j) /= 0) then
          stop 3
        end if
      end do
    end do
  end subroutine
end module test_functions

program test
  use test_functions
  implicit none
  integer :: array1(100), array2(100)

  call prepare (array1, array2)
  call copy (array1, array2)
  call check_equal (array1, array2)

  call prepare (array1, array2)
  call copy2 (array1, array2)
  call check_equal (array1, array2)

  call prepare (array1, array2)
  call copy3 (array1, array2)
  call check_equal (array1, array2)

  call prepare (array1, array2)
  call copy4 (array1, array2)
  call check_equal (array1, array2)

  call prepare (array1, array2)
  call copy5 (array1, array2)
  call check_equal (array1, array2)

  call prepare (array1, array2)
  call copy6 (array1, array2, 1, 100, 5)
  call check_equal_at_steps (array1, array2, 1, 100, 5)
  call check_unchanged_at_non_steps (array1, array2, 1, 100, 5)

  call prepare (array1, array2)
  call copy6 (array1, array2, 1, 50, 5)
  call check_equal_at_steps (array1, array2, 1, 50, 5)
  call check_unchanged_at_non_steps (array1, array2, 1, 50, 5)

  call prepare (array1, array2)
  call copy6 (array1, array2, 3, 18, 7)
  call check_equal_at_steps (array1, array2, 3 , 18, 7)
  call check_unchanged_at_non_steps (array1, array2, 3, 18, 7)
end program
