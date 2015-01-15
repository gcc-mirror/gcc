! { dg-do run }
! { dg-options "-fno-inline" }

  module m1
    contains
    recursive function fact (x) result (res)
      !$acc routine
      integer, intent(in) :: x
      integer :: res
      if (x < 1) then
         res = 1
      else
         res = x * fact (x - 1)
      end if
    end function fact
  end module m1
  use m1
  integer, parameter :: n = 10
  integer :: a(n), i
  !$acc parallel
  !$acc loop
  do i = 1, n
     a(i) = fact (i)
  end do
  !$acc end parallel
  do i = 1, n
     if (a(i) .ne. fact(i)) call abort
  end do
end
