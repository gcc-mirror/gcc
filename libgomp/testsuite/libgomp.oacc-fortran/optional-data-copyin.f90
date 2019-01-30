! Test OpenACC data regions with a copy-in of optional arguments.

! { dg-do run }

program test
  implicit none

  integer, parameter :: n = 64
  integer :: i
  integer :: a_int, b_int, c_int, res_int
  integer :: a_arr(n), b_arr(n), c_arr(n), res_arr(n)
  integer, allocatable :: a_alloc(:), b_alloc(:), c_alloc(:), res_alloc(:)

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

  allocate (a_alloc(n))
  allocate (b_alloc(n))
  allocate (c_alloc(n))
  allocate (res_alloc(n))

  do i = 1, n
    a_alloc(i) = i
    b_alloc(i) = n - i + 1
    c_alloc(i) = i * 3
  end do

  call test_allocatable(res_alloc, a_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. a_alloc(i)) stop 7
  end do

  call test_allocatable(res_alloc, a_alloc, b_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. a_alloc(i) * b_alloc(i)) stop 8
  end do

  call test_allocatable(res_alloc, a_alloc, b_alloc, c_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. a_alloc(i) * b_alloc(i) + c_alloc(i)) stop 9
  end do

  deallocate (a_alloc)
  deallocate (b_alloc)
  deallocate (c_alloc)
  deallocate (res_alloc)
contains
  subroutine test_int(res, a, b, c)
    integer :: res
    integer :: a
    integer, optional :: b, c

    !$acc data copyin(a, b, c) copyout(res)
    !$acc parallel
    res = a

    if (present(b)) res = res * b

    if (present(c)) res = res + c
    !$acc end parallel
    !$acc end data
  end subroutine test_int

  subroutine test_array(res, a, b, c)
    integer :: res(n)
    integer :: a(n)
    integer, optional :: b(n), c(n)

    !$acc data copyin(a, b, c) copyout(res)
    !$acc parallel loop
    do i = 1, n
      res(i) = a(i)
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(b)) res(i) = res(i) * b(i)
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(c)) res(i) = res(i) + c(i)
    end do
    !$acc end data
  end subroutine test_array

  subroutine test_allocatable(res, a, b, c)
    integer, allocatable :: res(:)
    integer, allocatable  :: a(:)
    integer, allocatable, optional :: b(:), c(:)

    !$acc data copyin(a, b, c) copyout(res)
    !$acc parallel loop
    do i = 1, n
      res(i) = a(i)
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(b)) res(i) = res(i) * b(i)
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(c)) res(i) = res(i) + c(i)
    end do
    !$acc end data
  end subroutine test_allocatable
end program test
