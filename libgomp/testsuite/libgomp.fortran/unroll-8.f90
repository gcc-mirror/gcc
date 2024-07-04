! { dg-do run }
! { dg-additional-options "-g" }

module test_functions
contains
  subroutine copy (array1, array2, step, n)
    implicit none
    integer :: array1(n)
    integer :: array2(n)
    integer :: i, step, n

    call omp_set_num_threads (4)
    !$omp parallel do shared(array1) shared(array2) schedule(static, 4) &
    !$omp & private(i)
    !$omp unroll partial(2)
    do i = 1,n
      array1(i) = array2(i)
    end do
  end subroutine
end module test_functions

program test
  use test_functions
  implicit none
  integer :: array1(100), array2(100)
  integer :: i

  array1 = 2
  call copy(array1, array2, 1, 100)
  do i=1,100
    if (array1(i) /= array2(i)) then
      stop 1
    end if
  end do
end program
