program subarrays
  integer, parameter     :: n = 20, c = 10, low = 5, high = 10
  integer                :: i, a(n), b(n)

  a(:) = 0
  b(:) = 0

  ! COPY

  !$acc parallel copy (a(low:high))
  !$acc loop
  do i = low, high
     a(i) = i
  end do
  !$acc end parallel

  do i = low, high
     b(i) = i
  end do

  call check (a, b, n)

  ! COPYOUT

  a(:) = 0

  !$acc parallel copyout (a(low:high))
  !$acc loop
  do i = low, high
     a(i) = i
  end do
  !$acc end parallel

  do i = low, high
     if (a(i) .ne. b(i)) STOP 1
  end do
  call check (a, b, n)

  ! COPYIN

  a(:) = 0

  !$acc parallel copyout (a(low:high)) copyin (b(low:high))
  !$acc loop
  do i = low, high
     a(i) = b(i)
  end do
  !$acc end parallel

  call check (a, b, n)

  ! PRESENT_OR_COPY

  a(:) = 0
  
  !$acc parallel pcopy (a(low:high))
  !$acc loop
  do i = low, high
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)

  ! PRESENT_OR_COPYOUT

  a(:) = 0

  !$acc parallel pcopyout (a(low:high))
  !$acc loop
  do i = low, high
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)

  ! PRESENT_OR_COPYIN

  a(:) = 0

  !$acc parallel pcopyout (a(low:high)) &
  !$acc & pcopyin (b(low:high))
  !$acc loop
  do i = low, high
     a(i) = b(i)
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
