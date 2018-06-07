! { dg-do run }
! { dg-options "-fno-inline" }

  integer, parameter :: n = 10
  integer :: a(n), i
  do i = 1, n
     a(i) = i
  end do
  !$acc parallel
  !$acc loop
  do i = 1, n
     call incr(a(i))
  end do
  !$acc end parallel
  do i = 1, n
     if (a(i) .ne. (i + 1)) STOP 1
  end do
end
subroutine incr (x)
  !$acc routine
  integer, intent(inout) :: x
  x = x + 1
end subroutine incr
