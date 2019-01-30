! Test OpenACC declare directives with optional arguments.

! { dg-do run }

program test
  implicit none

  integer, parameter :: n = 64
  integer :: i
  integer :: a_int, b_int, c_int, res_int
  integer :: a_arr(n), b_arr(n), c_arr(n), res_arr(n)

  a_int = 7
  b_int = 3
  c_int = 11

  call test_int(res_int, a_int)
  if (res_int .ne. a_int) stop 1

  call test_int(res_int, a_int, b_int)
  if (res_int .ne. a_int * b_int) stop 2

  call test_int(res_int, a_int, b_int, c_int)
  if (res_int .ne. a_int * b_int + c_int) stop 3

  do i = 1, n
    a_arr(i) = i
    b_arr(i) = n - i + 1
    c_arr(i) = i * 3
  end do

  call test_array(res_arr, a_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i)) stop 4
  end do

  call test_array(res_arr, a_arr, b_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i) * b_arr(i)) stop 5
  end do

  call test_array(res_arr, a_arr, b_arr, c_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i) * b_arr(i) + c_arr(i)) stop 6
  end do
contains
  subroutine test_int(res, a, b, c)
    integer :: a
    integer, optional :: b, c
    !$acc declare present_or_copyin(a, b, c)
    integer :: res
    !$acc declare present_or_copyout(res)

    !$acc parallel
    res = a
    if (present(b)) res = res * b
    if (present(c)) res = res + c
    !$acc end parallel
  end subroutine test_int

  subroutine test_array(res, a, b, c)
    integer :: a(n)
    integer, optional :: b(n), c(n)
    !$acc declare present_or_copyin(a, b, c)
    integer :: res(n)
    !$acc declare present_or_copyout(res)

    !$acc parallel loop
    do i = 1, n
      res(i) = a(i)
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(b)) then
        res(i) = res(i) * b(i)
      end if
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(c)) then
        res(i) = res(i) + c(i)
      end if
    end do
  end subroutine test_array
end program test
