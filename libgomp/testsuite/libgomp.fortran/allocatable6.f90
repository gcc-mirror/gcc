! PR fortran/46874
! { dg-do run }

  interface
    subroutine sub (a, b, c, d, n)
      integer :: n
      integer, allocatable :: a(:), b(:), c(:), d(:)
    end subroutine
  end interface

  integer, allocatable :: a(:), b(:), c(:), d(:)
  integer :: i, j
  allocate (a(50), b(50), c(50), d(50))
  do i = 1, 50
    a(i) = 2 + modulo (i, 7)
    b(i) = 179 - modulo (i, 11)
  end do
  c = 0
  d = 2147483647
  call sub (a, b, c, d, 50)
  do i = 1, 50
    j = 0
    if (i .eq. 3) then
      j = 8
    else if (i .gt. 1 .and. i .lt. 9) then
      j = 7
    end if
    if (c(i) .ne. j) stop 1
    j = 179 - modulo (i, 11)
    if (i .gt. 1 .and. i .lt. 9) j = i
    if (d(i) .ne. j) stop 2
  end do
  deallocate (a, b, c, d)
end

subroutine sub (a, b, c, d, n)
  integer :: n
  integer, allocatable :: a(:), b(:), c(:), d(:)
!$omp parallel do shared(a, b) reduction(+:c) reduction(min:d)
  do i = 1, n
    c(a(i)) = c(a(i)) + 1
    d(i) = min(d(i), b(i))
    d(a(i)) = min(d(a(i)), a(i))
  end do
end
