! { dg-do run }

program main
  implicit none (type, external)
  integer :: j
  integer, allocatable :: A(:)
  character(len=:), allocatable :: my_str
  character(len=15), allocatable :: my_str15

  A = [(3*j, j=1, 10)]
  call foo (A, size(A))
  call bar (A)
  my_str = "1234567890"
  call foo_str(my_str)
  call bar_str(my_str)
  my_str15 = "123456789012345"
  call foobar (my_str15)
  deallocate (A, my_str, my_str15)
contains
  subroutine foo (array, nn)
    integer :: i, nn
    integer :: array(nn)

    !$acc parallel copyout(array)
    array = [(-i, i = 1, nn)]
    !$acc loop gang private(array)
    do i = 1, 10
      array(i) = i
    end do
    if (any (array /= [(-i, i = 1, nn)])) error stop 1
    !$acc end parallel
  end subroutine foo
  subroutine bar (array)
    integer :: i
    integer :: array(:)

    !$acc parallel copyout(array)
    array = [(-2*i, i = 1, size(array))]
    !$acc loop gang private(array)
    do i = 1, 10
      array(i) = 9*i
    end do
    if (any (array /= [(-2*i, i = 1, 10)])) error stop 2
    !$acc end parallel
  end subroutine bar
  subroutine foo_str(str)
    integer :: i
    character(len=*) :: str

    !$acc parallel copyout(str)
    str = "abcdefghij"
    !$acc loop gang private(str)
    do i = 1, 10
      str(i:i) = achar(ichar('A') + i)
    end do
    if (str /= "abcdefghij") error stop 3
    !$acc end parallel
  end
  subroutine bar_str(str)
    integer :: i
    character(len=:), allocatable :: str

! ***************************************
! FIXME: Fails due to PR middle-end/95499
! ***************************************
    !!$acc parallel copyout(str)
    str = "abcdefghij"
    !!$acc loop gang private(str)
    !do i = 1, 10
    !  str(i:i) = achar(ichar('A') + i)
    !end do
    if (str /= "abcdefghij") error stop 5
    !!$acc end parallel
  end
  subroutine foobar (scalar)
    integer :: i
    character(len=15), optional :: scalar

    !$acc parallel copyout(scalar)
    scalar = "abcdefghi-12345"
    !$acc loop gang private(scalar)
    do i = 1, 15
      scalar(i:i) = achar(ichar('A') + i)
    end do
    !$acc end parallel
    if (scalar /= "abcdefghi-12345") error stop 6
  end subroutine foobar
  subroutine foobar15 (scalar)
    integer :: i
    character(len=15), optional, allocatable :: scalar

    !$acc parallel copyout(scalar)
    scalar = "abcdefghi-12345"
    !$acc loop gang private(scalar)
    do i = 1, 15
      scalar(i:i) = achar(ichar('A') + i)
    end do
    !$acc end parallel
    if (scalar /= "abcdefghi-12345") error stop 1
  end subroutine foobar15
end
