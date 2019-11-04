! Exercise the auto, independent, seq and tile loop clauses inside
! kernels regions. 

! { dg-do run }

program loops
  integer, parameter     :: n = 20
  integer                :: i, a(n), b(n)

  a(:) = 0
  b(:) = 0

  ! COPY

  !$acc kernels copy (a)
  !$acc loop auto
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
  !$acc loop independent
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
  !$acc loop seq
  do i = 1, n
     a(i) = b(i)
  end do
  !$acc end kernels

  call check (a, b, n)

end program loops

subroutine check (a, b, n)
  integer :: n, a(n), b(n)
  integer :: i

  do i = 1, n
     if (a(i) .ne. b(i)) stop 2
  end do
end subroutine check
