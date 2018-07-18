program subarrays
  integer, parameter     :: n = 20, c = 10
  integer                :: i, a(n), b(n)

  a(:) = 0
  b(:) = 0

  ! COPY

  !$acc parallel copy (a(1:n))
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  do i = 1, n
     b(i) = i
  end do

  call check (a, b, n)

  ! COPYOUT

  a(:) = 0

  !$acc parallel copyout (a(1:n))
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  do i = 1, n
     if (a(i) .ne. b(i)) STOP 1
  end do
  call check (a, b, n)

  ! COPYIN

  a(:) = 0

  !$acc parallel copyout (a(1:n)) copyin (b(1:n))
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)

  ! PRESENT_OR_COPY

  !$acc parallel pcopy (a(1:n))
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)

  ! PRESENT_OR_COPYOUT

  a(:) = 0

  !$acc parallel pcopyout (a(1:n))
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)

  ! PRESENT_OR_COPYIN

  a(:) = 0

  !$acc parallel pcopyout (a(1:n)) pcopyin (b(1:n))
  !$acc loop
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)
end program subarrays

subroutine check (a, b, n)
  integer :: n, a(n), b(n)
  integer :: i

  do i = 1, n
     if (a(i) .ne. b(i)) STOP 2
  end do
end subroutine check
