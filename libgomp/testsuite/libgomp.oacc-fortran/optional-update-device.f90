! Test OpenACC update to device with an optional argument.

! { dg-do run }

program optional_update_device
  implicit none

  integer, parameter :: n = 64
  integer :: i
  integer :: a_int, b_int, res_int
  integer :: a_arr(n), b_arr(n), res_arr(n)
  integer, allocatable :: a_alloc(:), b_alloc(:), res_alloc(:)

  a_int = 5
  b_int = 11

  call test_int(res_int, a_int)
  if (res_int .ne. a_int) stop 1

  call test_int(res_int, a_int, b_int)
  if (res_int .ne. a_int * b_int) stop 2

  res_arr(:) = 0
  do i = 1, n
    a_arr(i) = i
    b_arr(i) = n - i + 1
  end do

  call test_array(res_arr, a_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i)) stop 3
  end do

  call test_array(res_arr, a_arr, b_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i) * b_arr(i)) stop 4
  end do

  allocate (a_alloc(n))
  allocate (b_alloc(n))
  allocate (res_alloc(n))

  res_alloc(:) = 0
  do i = 1, n
    a_alloc(i) = i
    b_alloc(i) = n - i + 1
  end do

  call test_allocatable(res_alloc, a_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. a_alloc(i)) stop 5
  end do

  call test_allocatable(res_alloc, a_alloc, b_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. a_alloc(i) * b_alloc(i)) stop 6
  end do

  deallocate (a_alloc)
  deallocate (b_alloc)
  deallocate (res_alloc)
contains
  subroutine test_int(res, a, b)
    integer :: res
    integer :: a
    integer, optional :: b

    !$acc data create(a, b, res)
    !$acc update device(a, b)
    !$acc parallel
    res = a
    if (present(b)) res = res * b
    !$acc end parallel
    !$acc update self(res)
    !$acc end data
  end subroutine test_int

  subroutine test_array(res, a, b)
    integer :: res(n)
    integer :: a(n)
    integer, optional :: b(n)

    !$acc data create(a, b, res)
    !$acc update device(a, b)
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
    !$acc update self(res)
    !$acc end data
  end subroutine test_array

  subroutine test_allocatable(res, a, b)
    integer, allocatable :: res(:)
    integer, allocatable :: a(:)
    integer, allocatable, optional :: b(:)

    !$acc data create(a, b, res)
    !$acc update device(a, b)
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
    !$acc update self(res)
    !$acc end data
  end subroutine test_allocatable
end program optional_update_device
