! Test that optional arguments work in firstprivate clauses.  The effect of
! non-present arguments in firstprivate clauses is undefined, and is not
! tested for.

! { dg-do run }

program test_firstprivate
  implicit none
  integer, parameter :: n = 64

  integer :: i, j
  integer :: a_int, b_int, c_int, res_int
  integer :: a_arr(n), b_arr(n), c_arr(n), res_arr(n)
  integer, allocatable :: a_alloc(:), b_alloc(:), c_alloc(:), res_alloc(:)

  a_int = 14
  b_int = 5
  c_int = 12

  call test_int(res_int, a_int, b_int, c_int)
  if (res_int .ne. a_int * b_int + c_int) stop 1

  do i = 1, n
    a_arr(i) = i
    b_arr(i) = n - i + 1
    c_arr(i) = i * 3
  end do

  call test_array(res_arr, a_arr, b_arr, c_arr)
  do i = 1, n
    if (res_arr(i) .ne. a_arr(i) * b_arr(i) + c_arr(i)) stop 2
  end do

  allocate(a_alloc(n))
  allocate(b_alloc(n))
  allocate(c_alloc(n))
  allocate(res_alloc(n))

  do i = 1, n
    a_arr(i) = i
    b_arr(i) = n - i + 1
    c_arr(i) = i * 3
  end do

  call test_allocatable(res_alloc, a_alloc, b_alloc, c_alloc)
  do i = 1, n
    if (res_alloc(i) .ne. a_alloc(i) * b_alloc(i) + c_alloc(i)) stop 3
  end do

  deallocate(a_alloc)
  deallocate(b_alloc)
  deallocate(c_alloc)
  deallocate(res_alloc)
contains
  subroutine test_int(res, a, b, c)
    integer :: a
    integer, optional :: b, c
    integer :: res

    !$acc parallel firstprivate(a, b, c) copyout(res)
    res = a
    if (present(b)) res = res * b
    if (present(c)) res = res + c
    !$acc end parallel
  end subroutine test_int

  subroutine test_array(res, a, b, c)
    integer :: a(n)
    integer, optional :: b(n), c(n)
    integer :: res(n)

    !$acc data copyin(a, b, c) copyout(res)
    !$acc parallel loop firstprivate(a)
    do i = 1, n
      res(i) = a(i)
    end do

    !$acc parallel loop firstprivate(b)
    do i = 1, n
      if (present(b)) res(i) = res(i) * b(i)
    end do

    !$acc parallel loop firstprivate(c)
    do i = 1, n
      if (present(c)) res(i) = res(i) + c(i)
    end do
    !$acc end data
  end subroutine test_array

  subroutine test_allocatable(res, a, b, c)
    integer, allocatable :: a(:)
    integer, allocatable, optional :: b(:), c(:)
    integer, allocatable :: res(:)

    !$acc data copyin(a, b, c) copyout(res)
    !$acc parallel loop firstprivate(a)
    do i = 1, n
      res(i) = a(i)
    end do

    !$acc parallel loop firstprivate(b)
    do i = 1, n
      if (present(b)) res(i) = res(i) * b(i)
    end do

    !$acc parallel loop firstprivate(c)
    do i = 1, n
      if (present(c)) res(i) = res(i) + c(i)
    end do
    !$acc end data
  end subroutine test_allocatable
end program test_firstprivate
