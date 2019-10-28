! Test the copy, copyin, copyout, pcopy, pcopyin, pcopyout, and pcreate
! clauses on kernels constructs.

! { dg-do run }

program map
  integer, parameter     :: n = 20, c = 10
  integer                :: i, a(n), b(n), d(n)

  a(:) = 0
  b(:) = 0

  ! COPY

  !$acc kernels copy (a)
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end kernels

  do i = 1, n
     b(i) = i
  end do

  call check (a, b, n)

  ! COPYOUT

  a(:) = 0

  !$acc kernels copyout (a)
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end kernels

  do i = 1, n
     if (a(i) .ne. b(i)) stop 1
  end do
  call check (a, b, n)

  ! COPYIN

  a(:) = 0

  !$acc kernels copyout (a) copyin (b)
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end kernels

  call check (a, b, n)

  ! PRESENT_OR_COPY

  !$acc kernels pcopy (a)
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end kernels

  call check (a, b, n)

  ! PRESENT_OR_COPYOUT

  a(:) = 0

  !$acc kernels pcopyout (a)
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end kernels

  call check (a, b, n)

  ! PRESENT_OR_COPYIN

  a(:) = 0

  !$acc kernels pcopyout (a) pcopyin (b)
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end kernels

  call check (a, b, n)

  ! PRESENT_OR_CREATE

  a(:) = 0

  !$acc kernels pcopyout (a) pcreate (d)
  !$acc loop
  do i = 1, n
     d(i) = i
     a(i) = d(i)
  end do
  !$acc end kernels

  call check (a, b, n)
end program map

subroutine check (a, b, n)
  integer :: n, a(n), b(n)
  integer :: i

  do i = 1, n
     if (a(i) .ne. b(i)) stop 2
  end do
end subroutine check
