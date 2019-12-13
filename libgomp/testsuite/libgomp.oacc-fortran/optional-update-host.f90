! Test OpenACC update to host with an optional argument.

! { dg-do run }

program optional_update_host
  implicit none

  integer, parameter :: n = 64
  integer :: i
  integer :: a_int, b_int, res_int
  integer :: a_arr(n), b_arr(n), res_arr(n)
  integer, allocatable :: a_alloc(:), b_alloc(:), res_alloc(:)

  a_int = 5
  b_int = 11
  res_int = 0

  call test_int(a_int, b_int)
  if (res_int .ne. 0) stop 1

  call test_int(a_int, b_int, res_int)
  if (res_int .ne. a_int * b_int) stop 2

  res_arr(:) = 0
  do i = 1, n
    a_arr(i) = i
    b_arr(i) = n - i + 1
  end do

  call test_array(a_arr, b_arr)
  do i = 1, n
    if (res_arr(i) .ne. 0) stop 3
  end do

  call test_array(a_arr, b_arr, res_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i) * b_arr(i)) stop 4
  end do

  allocate(a_alloc(n))
  allocate(b_alloc(n))
  allocate(res_alloc(n))

  res_alloc(:) = 0
  do i = 1, n
    a_alloc(i) = i
    b_alloc(i) = n - i + 1
  end do

  call test_allocatable(a_alloc, b_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. 0) stop 5
  end do

  call test_allocatable(a_alloc, b_alloc, res_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. a_alloc(i) * b_alloc(i)) stop 6
  end do

  deallocate(a_alloc)
  deallocate(b_alloc)
  deallocate(res_alloc)
contains
  subroutine test_int(a, b, res)
    integer :: a, b
    integer, optional :: res

    !$acc data create(a, b, res)
    !$acc update device(a, b)
    !$acc parallel
    if (present(res)) res = a
    if (present(res)) res = res * b
    !$acc end parallel
    !$acc update self(res)
    !$acc end data
  end subroutine test_int

  subroutine test_array(a, b, res)
    integer :: a(n), b(n)
    integer, optional :: res(n)

    !$acc data create(a, b, res)
    !$acc update device(a, b)
    !$acc parallel loop
    do i = 1, n
      if (present(res)) res(i) = a(i)
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(res)) res(i) = res(i) * b(i)
    end do
    !$acc update self(res)
    !$acc end data
  end subroutine test_array

  subroutine test_allocatable(a, b, res)
    integer, allocatable :: a(:), b(:)
    integer, allocatable, optional :: res(:)

    !$acc data create(a, b, res)
    !$acc update device(a, b)
    !$acc parallel loop
    do i = 1, n
      if (present(res)) res(i) = a(i)
    end do

    !$acc parallel loop
    do i = 1, n
      if (present(res)) res(i) = res(i) * b(i)
    end do
    !$acc update self(res)
    !$acc end data
  end subroutine test_allocatable
end program optional_update_host 
