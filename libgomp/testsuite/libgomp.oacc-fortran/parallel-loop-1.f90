! Exercise the auto, independent, seq and tile loop clauses inside
! parallel regions. 

! { dg-do run }

program loops
  integer, parameter     :: n = 20, c = 10
  integer                :: i, a(n), b(n)

  a(:) = 0
  b(:) = 0

  ! COPY

  !$acc parallel copy (a)
  !$acc loop auto
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

  !$acc parallel copyout (a)
  !$acc loop independent
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  do i = 1, n
     if (a(i) .ne. b(i)) call abort
  end do
  call check (a, b, n)

  ! COPYIN

  a(:) = 0

  !$acc parallel copyout (a) copyin (b)
  !$acc loop seq
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)

  ! PRESENT_OR_COPY

  !$acc parallel pcopy (a)
  !$acc loop tile (*)
  do i = 1, n
     a(i) = i
  end do
  !$acc end parallel

  call check (a, b, n)

end program loops

subroutine check (a, b, n)
  integer :: n, a(n), b(n)
  integer :: i

  do i = 1, n
     if (a(i) .ne. b(i)) call abort
  end do
end subroutine check
