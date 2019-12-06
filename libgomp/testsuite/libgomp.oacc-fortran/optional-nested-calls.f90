! Test propagation of optional arguments from within an OpenACC parallel region.

! { dg-do run }

program test
  implicit none

  integer, parameter :: n = 64
  integer :: i
  integer :: res_int
  integer :: a_arr(n), b_arr(n), res_arr(n)
  integer, allocatable :: a_alloc(:), b_alloc(:), res_alloc(:)

  call test_int_caller(res_int, 5)
  if (res_int .ne. 10) stop 1

  call test_int_caller(res_int, 2, 3)
  if (res_int .ne. 11) stop 2

  do i = 1, n
    a_arr(i) = i
    b_arr(i) = n - i + 1
  end do

  call test_array_caller(res_arr, a_arr)
  do i = 1, n
    if (res_arr(i) .ne. 2 * a_arr(i)) stop 3
  end do

  call test_array_caller(res_arr, a_arr, b_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i) * b_arr(i) + a_arr(i) + b_arr(i)) stop 4
  end do

  allocate(a_alloc(n))
  allocate(b_alloc(n))
  allocate(res_alloc(n))

  do i = 1, n
    a_alloc(i) = i
    b_alloc(i) = n - i + 1
  end do

  call test_array_caller(res_arr, a_arr)
  do i = 1, n
    if (res_arr(i) .ne. 2 * a_alloc(i)) stop 5
  end do

  call test_array_caller(res_arr, a_arr, b_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i) * b_alloc(i) + a_alloc(i) + b_alloc(i)) stop 6
  end do

  deallocate(a_alloc)
  deallocate(b_alloc)
  deallocate(res_alloc)
contains
  subroutine test_int_caller(res, a, b)
    integer :: res, a
    integer, optional :: b

    !$acc data copyin(a, b) copyout (res)
    !$acc parallel
    res = a
    if (present(b)) res = res * b
    call test_int_callee(res, a, b)
    !$acc end parallel
    !$acc end data
  end subroutine test_int_caller

  subroutine test_int_callee(res, a, b)
    !$acc routine seq
    integer :: res, a
    integer, optional :: b

    res = res + a
    if (present(b)) res = res + b
  end subroutine test_int_callee

  subroutine test_array_caller(res, a, b)
    integer :: res(n), a(n), i
    integer, optional :: b(n)

    !$acc data copyin(a, b) copyout(res)
    !$acc parallel
    !$acc loop seq
    do i = 1, n
      res(i) = a(i)
      if (present(b)) res(i) = res(i) * b(i)
    end do
    call test_array_callee(res, a, b)
    !$acc end parallel
    !$acc end data
  end subroutine test_array_caller

  subroutine test_array_callee(res, a, b)
    !$acc routine seq
    integer :: res(n), a(n), i
    integer, optional :: b(n)

    do i = 1, n
      res(i) = res(i) + a(i)
      if (present(b)) res(i) = res(i) + b(i)
    end do
  end subroutine test_array_callee

  subroutine test_allocatable_caller(res, a, b)
    integer :: i
    integer, allocatable :: res(:), a(:)
    integer, allocatable, optional :: b(:)

    !$acc data copyin(a, b) copyout(res)
    !$acc parallel
    !$acc loop seq
    do i = 1, n
      res(i) = a(i)
      if (present(b)) res(i) = res(i) * b(i)
    end do
    call test_array_callee(res, a, b)
    !$acc end parallel
    !$acc end data
  end subroutine test_allocatable_caller

  subroutine test_allocatable_callee(res, a, b)
    !$acc routine seq
    integer :: i
    integer, allocatable :: res(:), a(:)
    integer, allocatable, optional :: b(:)

    do i = 1, n
      res(i) = res(i) + a(i)
      if (present(b)) res(i) = res(i) + b(i)
    end do
  end subroutine test_allocatable_callee
end program test
