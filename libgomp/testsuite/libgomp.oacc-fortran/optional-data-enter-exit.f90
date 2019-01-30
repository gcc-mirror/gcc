! Test OpenACC unstructured enter data/exit data regions with optional
! arguments.

! { dg-do run }

program test
  implicit none

  integer, parameter :: n = 64
  integer :: a(n), b(n), c(n), res(n)
  integer :: x, y, z, r, i

  do i = 1, n
    a(i) = i
    b(i) = n - i + 1
    c(i) = i * 3
  end do

  res = test_array(a)
  do i = 1, n
    if (res(i) .ne. a(i)) stop 1
  end do

  res = test_array(a, b)
  do i = 1, n
    if (res(i) .ne. a(i) * b(i)) stop 2
  end do

  res = test_array(a, b, c)
  do i = 1, n
    if (res(i) .ne. a(i) * b(i) + c(i)) stop 3
  end do

  x = 7
  y = 3
  z = 11

  r = test_int(x)
  if (r .ne. x) stop 4

  r = test_int(x, y)
  if (r .ne. x * y) stop 5

  r = test_int(x, y, z)
  if (r .ne. x * y + z) stop 6
contains
  function test_array(a, b, c)
    integer :: a(n)
    integer, optional :: b(n), c(n)
    integer :: test_array(n), res(n)

    !$acc enter data copyin(a, b, c) create(res)
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
    !$acc exit data copyout(res) delete(a, b, c)

    test_array = res
  end function test_array

  function test_int(a, b, c)
    integer :: a
    integer, optional :: b, c
    integer :: test_int, res

    !$acc enter data copyin(a, b, c) create(res)
    !$acc parallel present(a, b, c, res)
    res = a
    if (present(b)) res = res * b
    if (present(c)) res = res + c
    !$acc end parallel
    !$acc exit data copyout(res) delete(a, b, c)

    test_int = res
  end function test_int
end program test
